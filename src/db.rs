pub mod build_options;
mod timing;

use std::{
    cell::{Ref, RefCell},
    cmp,
    collections::hash_map::Entry,
    fs, io,
    path::{Path, PathBuf},
    rc::Rc,
};

use anyhow::{bail, Result};
use path_absolutize::Absolutize;
use ustr::Ustr;

use crate::{
    common::{new_key_type, IndexVec, QPath},
    db::{
        build_options::{BuildOptions, EmitOption},
        timing::Timings,
    },
    diagnostics::Diagnostics,
    hir::{ExprId, HirMap},
    span::{Source, SourceId, Sources, Span},
    ty::{
        coerce::{Coercion, Coercions},
        IntTy, Ty, TyKind, Typed,
    },
};

#[derive(Debug)]
pub struct Db {
    pub time: Timings,
    pub sources: Rc<RefCell<Sources>>,
    pub modules: IndexVec<ModuleId, ModuleInfo>,
    pub defs: IndexVec<DefId, Def>,
    pub types: CommonTypes,
    pub coercions: HirMap<Coercions>,
    pub diagnostics: Diagnostics,
    build_options: BuildOptions,
    root_dir: PathBuf,
    main_source: SourceId,
    main_module: Option<ModuleId>,
    main_fun: Option<DefId>,
}

impl Db {
    pub fn new(build_options: BuildOptions, root_file: &Path) -> Result<Self> {
        let absolute_path = root_file.absolutize()?;

        if !absolute_path.is_file() {
            bail!("provided path `{}` in not a file", absolute_path.display());
        }

        let root_dir = absolute_path.parent().expect("to have a parent directory").to_path_buf();

        let mut sources = Sources::new();
        let main_source = sources.load_file(absolute_path.to_path_buf())?;

        let sources = Rc::new(RefCell::new(sources));

        Ok(Self {
            time: Timings::new(build_options.timings),
            build_options,
            diagnostics: Diagnostics::new(sources.clone()),
            sources,
            modules: IndexVec::new(),
            defs: IndexVec::new(),
            types: CommonTypes::new(),
            coercions: HirMap::new(),
            root_dir,
            main_source,
            main_module: None,
            main_fun: None,
        })
    }

    pub fn build_options(&self) -> &BuildOptions {
        &self.build_options
    }

    pub fn output_path(&self) -> PathBuf {
        let binding = self.main_source();
        let main_path = binding.path();
        let file_name = main_path.file_stem().expect("main source to be a file");
        let src_dir = main_path.parent().expect("to have a parent directory");
        src_dir.join(self.build_options.output_dir.clone()).join(file_name)
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub fn main_source_id(&self) -> SourceId {
        self.main_source
    }

    pub fn main_source(&self) -> Ref<'_, Source> {
        Ref::map(self.sources.borrow(), |s| {
            s.get(self.main_source).expect("to always have a main source")
        })
    }

    pub fn main_module_id(&self) -> Option<ModuleId> {
        self.main_module
    }

    pub fn main_module(&self) -> Option<&ModuleInfo> {
        self.main_module.and_then(|id| self.modules.get(id))
    }

    #[allow(unused)]
    pub fn main_function_id(&self) -> Option<DefId> {
        self.main_fun
    }

    pub fn main_function(&self) -> Option<&Def> {
        self.main_fun.and_then(|id| self.defs.get(id))
    }

    pub fn set_main_fun(&mut self, id: DefId) {
        self.main_fun = Some(id);
    }

    pub fn push_coercion(&mut self, expr_id: ExprId, c: Coercion) {
        match self.coercions.entry(expr_id) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().push(c);
            }
            Entry::Vacant(entry) => {
                entry.insert(Coercions::one(c));
            }
        }
    }

    pub fn emit_file(
        &self,
        opt: EmitOption,
        f: impl Fn(&Self, &mut fs::File) -> io::Result<()>,
    ) -> io::Result<()> {
        if self.build_options.should_emit(opt) {
            let mut file = fs::File::create(self.output_path().with_extension(opt.as_extension()))?;
            f(self, &mut file)
        } else {
            Ok(())
        }
    }
}

macro_rules! new_db_key {
    ($key: ident -> $collection: ident : $type: ident) => {
        new_key_type!($key);

        #[allow(unused)]
        impl std::ops::Index<$key> for Db {
            type Output = $type;

            fn index(&self, index: $key) -> &Self::Output {
                &self.$collection[index]
            }
        }

        #[allow(unused)]
        impl std::ops::IndexMut<$key> for Db {
            fn index_mut(&mut self, index: $key) -> &mut Self::Output {
                &mut self.$collection[index]
            }
        }
    };
}

new_db_key!(ModuleId -> modules : ModuleInfo);
new_db_key!(DefId -> defs : Def);

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    #[allow(unused)]
    pub id: ModuleId,
    pub source_id: SourceId,
    pub name: QPath,
    #[allow(unused)]
    pub is_main: bool,
}

impl ModuleInfo {
    pub fn alloc(db: &mut Db, source_id: SourceId, name: QPath, is_main: bool) -> ModuleId {
        let id = db.modules.push_with_key(|id| Self { id, source_id, name, is_main });

        if is_main {
            assert!(db.main_module.is_none());
            db.main_module = Some(id);
        }

        id
    }
}

#[derive(Debug, Clone)]
pub struct Def {
    pub id: DefId,
    pub name: Ustr,
    pub qpath: QPath,
    pub scope: ScopeInfo,
    pub kind: Box<DefKind>,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ScopeInfo {
    pub module_id: ModuleId,
    pub level: ScopeLevel,
    pub vis: Vis,
}

#[derive(Debug, Clone)]
pub enum DefKind {
    Fn(FnInfo),
    Ty(Ty),
    Variable,
}

impl DefKind {
    #[must_use]
    pub fn is_fn(&self) -> bool {
        matches!(self, Self::Fn(..))
    }

    #[must_use]
    pub fn as_ty(&self) -> Option<&Ty> {
        if let Self::Ty(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl Def {
    pub fn alloc(
        db: &mut Db,
        qpath: QPath,
        scope: ScopeInfo,
        kind: DefKind,
        ty: Ty,
        span: Span,
    ) -> DefId {
        db.defs.push_with_key(|id| Self {
            id,
            name: qpath.name(),
            qpath,
            scope,
            kind: Box::new(kind),
            ty,
            span,
        })
    }
}

impl Typed for Def {
    fn ty(&self) -> Ty {
        self.ty
    }

    fn ty_mut(&mut self) -> &mut Ty {
        &mut self.ty
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Private,
    Public,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeLevel {
    Global,
    Local(usize),
}

impl ScopeLevel {
    /// Returns `true` if the scope level is [`Global`].
    ///
    /// [`Global`]: ScopeLevel::Global
    #[must_use]
    pub fn is_global(&self) -> bool {
        matches!(self, Self::Global)
    }

    /// Returns `true` if the scope level is [`Local`].
    ///
    /// [`Local`]: ScopeLevel::Local
    #[must_use]
    pub fn is_local(&self) -> bool {
        matches!(self, Self::Local(..))
    }
}

impl PartialOrd for ScopeLevel {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ScopeLevel {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Self::Global, Self::Global) => cmp::Ordering::Equal,
            (Self::Global, Self::Local(..)) => cmp::Ordering::Less,
            (Self::Local(..), Self::Global) => cmp::Ordering::Greater,
            (Self::Local(a), Self::Local(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FnInfo {
    Bare,
}

#[derive(Debug)]
pub struct CommonTypes {
    pub int: Ty,
    pub bool: Ty,
    pub unit: Ty,
    pub never: Ty,
    pub typ: Ty,
    pub unknown: Ty,
}

impl Default for CommonTypes {
    fn default() -> Self {
        Self::new()
    }
}

impl CommonTypes {
    pub fn new() -> Self {
        Self {
            int: Ty::new(TyKind::Int(IntTy::Int)),
            bool: Ty::new(TyKind::Bool),
            unit: Ty::new(TyKind::Unit),
            never: Ty::new(TyKind::Never),
            typ: Ty::new(TyKind::Type),
            unknown: Ty::new(TyKind::Unknown),
        }
    }
}
