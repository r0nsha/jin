pub mod build_options;
mod timing;

use core::fmt;
use std::{
    cell::{Ref, RefCell},
    cmp,
    collections::hash_map::Entry,
    fs, io,
    path::Path,
    rc::Rc,
};

use anyhow::{bail, Result};
use camino::{Utf8Path, Utf8PathBuf};
use path_absolutize::Absolutize;
use rustc_hash::FxHashSet;
use ustr::Ustr;

use crate::{
    db::{
        build_options::{BuildOptions, EmitOption},
        timing::Timings,
    },
    diagnostics::Diagnostics,
    hir::{const_eval::ConstStorage, ExprId, HirMap},
    index_vec::{new_key_type, IndexVec, IndexVecExt},
    qpath::QPath,
    span::{Source, SourceId, Sources, Span},
    target::{TargetMetrics, TargetPlatform},
    ty::{
        coerce::{Coercion, Coercions},
        FloatTy, IntTy, Ty, TyKind, Typed, UintTy,
    },
    word::Word,
};

#[derive(Debug)]
pub struct Db {
    pub time: Timings,
    pub sources: Rc<RefCell<Sources>>,
    pub modules: IndexVec<ModuleId, ModuleInfo>,
    pub defs: IndexVec<DefId, DefInfo>,
    pub structs: IndexVec<StructId, StructInfo>,
    pub types: CommonTypes,
    pub coercions: HirMap<Coercions>,
    pub const_storage: ConstStorage,
    pub extern_libs: FxHashSet<ExternLib>,
    pub diagnostics: Diagnostics,
    build_options: BuildOptions,
    root_dir: Utf8PathBuf,
    main_source: SourceId,
    main_module: Option<ModuleId>,
    main_fun: Option<DefId>,
}

impl Db {
    pub fn new(build_options: BuildOptions, root_file: &Utf8Path) -> Result<Self> {
        let absolute_path: Utf8PathBuf =
            root_file.as_std_path().absolutize()?.into_owned().try_into()?;

        if !absolute_path.is_file() {
            bail!("provided path `{}` in not a file", absolute_path);
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
            structs: IndexVec::new(),
            types: CommonTypes::new(),
            coercions: HirMap::default(),
            const_storage: ConstStorage::new(),
            extern_libs: FxHashSet::default(),
            root_dir,
            main_source,
            main_module: None,
            main_fun: None,
        })
    }

    pub fn build_options(&self) -> &BuildOptions {
        &self.build_options
    }

    pub fn target_platform(&self) -> &TargetPlatform {
        &self.build_options.target_platform
    }

    pub fn target_metrics(&self) -> &TargetMetrics {
        &self.build_options.target_metrics
    }

    pub fn output_path(&self) -> Utf8PathBuf {
        let binding = self.main_source();
        let main_path = binding.path();
        let file_name = main_path.file_stem().expect("main source to be a file");
        let src_dir = main_path.parent().expect("to have a parent directory");
        src_dir.join(self.build_options.output_dir.clone()).join(file_name)
    }

    pub fn output_dir(&self) -> Utf8PathBuf {
        self.output_path()
            .parent()
            .expect("expected to have a parent directory for output_path")
            .to_owned()
    }

    pub fn root_dir(&self) -> &Utf8Path {
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

    pub fn main_function(&self) -> Option<&DefInfo> {
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
new_db_key!(DefId -> defs : DefInfo);
new_db_key!(StructId -> structs : StructInfo);

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
pub struct DefInfo {
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
    ExternGlobal,
    Global,
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

impl DefInfo {
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

impl Typed for DefInfo {
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
    Extern,
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub id: StructId,
    pub def_id: DefId,
    pub fields: Vec<StructField>,
    pub is_extern: bool,
    pub fn_ty: Ty,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Word,
    pub ty: Ty,
}

#[derive(Debug)]
pub struct CommonTypes {
    pub i8: Ty,
    pub i16: Ty,
    pub i32: Ty,
    pub i64: Ty,
    pub int: Ty,

    pub u8: Ty,
    pub u16: Ty,
    pub u32: Ty,
    pub u64: Ty,
    pub uint: Ty,

    pub f32: Ty,
    pub f64: Ty,

    pub str: Ty,
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
            i8: Ty::new(TyKind::Int(IntTy::I8)),
            i16: Ty::new(TyKind::Int(IntTy::I16)),
            i32: Ty::new(TyKind::Int(IntTy::I32)),
            i64: Ty::new(TyKind::Int(IntTy::I64)),
            int: Ty::new(TyKind::Int(IntTy::Int)),

            u8: Ty::new(TyKind::Uint(UintTy::U8)),
            u16: Ty::new(TyKind::Uint(UintTy::U16)),
            u32: Ty::new(TyKind::Uint(UintTy::U32)),
            u64: Ty::new(TyKind::Uint(UintTy::U64)),
            uint: Ty::new(TyKind::Uint(UintTy::Uint)),

            f32: Ty::new(TyKind::Float(FloatTy::F32)),
            f64: Ty::new(TyKind::Float(FloatTy::F64)),

            str: Ty::new(TyKind::Str),
            bool: Ty::new(TyKind::Bool),
            unit: Ty::new(TyKind::Unit),
            never: Ty::new(TyKind::Never),
            typ: Ty::new(TyKind::Type),
            unknown: Ty::new(TyKind::Unknown),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum ExternLib {
    Sys(String),
    Path { search_path: Utf8PathBuf, name: String },
}

impl fmt::Display for ExternLib {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExternLib::Sys(name) | ExternLib::Path { name, .. } => f.write_str(name),
        }
    }
}

impl ExternLib {
    pub fn try_from_str(name: &str, relative_to: impl AsRef<Utf8Path>) -> Option<Self> {
        let path = Path::new(name);

        if name.starts_with(['.', '/', '\\']) || path.has_root() || path.extension().is_some() {
            path.absolutize_from(relative_to.as_ref().as_std_path()).ok().map(|p| {
                let p: Utf8PathBuf = p.into_owned().try_into().unwrap();
                let search_path = p.parent().unwrap().to_path_buf();
                let name = p.file_name().unwrap().to_string();
                Self::Path { search_path, name }
            })
        } else {
            Some(Self::Sys(name.to_string()))
        }
    }
}
