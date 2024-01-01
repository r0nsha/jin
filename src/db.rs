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
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::{ustr, Ustr};

use crate::{
    data_structures::index_vec::{new_key_type, IndexVec, IndexVecExt},
    db::{
        build_options::{BuildOptions, EmitOption},
        timing::Timings,
    },
    diagnostics::Diagnostics,
    hir::{ExprId, HirMap},
    middle::{Mutability, Vis},
    qpath::QPath,
    span::{Source, SourceId, Sources, Span},
    target::{TargetMetrics, TargetPlatform},
    ty::{
        coerce::{Coercion, Coercions},
        FloatTy, FnTy, FnTyParam, IntTy, Ty, TyKind, Typed, UintTy,
    },
    word::Word,
};

#[derive(Debug)]
pub struct Db {
    pub sources: Rc<RefCell<Sources>>,
    pub packages: FxHashMap<Ustr, Package>,
    pub modules: IndexVec<ModuleId, ModuleInfo>,
    pub defs: IndexVec<DefId, Def>,
    pub adts: IndexVec<AdtId, Adt>,
    pub types: CommonTypes,
    pub coercions: HirMap<Coercions>,
    pub extern_libs: FxHashSet<ExternLib>,
    pub diagnostics: Diagnostics,
    timings: Timings,
    build_options: BuildOptions,
    main_package_name: Ustr,
    main_source: SourceId,
    main_module: Option<ModuleId>,
    main_fun: Option<DefId>,
}

impl Db {
    pub fn new(
        build_options: BuildOptions,
        root_file: &Utf8Path,
    ) -> Result<Self> {
        let absolute_path: Utf8PathBuf =
            root_file.as_std_path().absolutize()?.into_owned().try_into()?;

        if !absolute_path.is_file() {
            bail!("provided path `{}` in not a file", absolute_path);
        }

        let mut sources = Sources::new();
        let main_source = sources.load_file(absolute_path.to_path_buf())?;

        let main_package_name =
            ustr(&sources.get(main_source).unwrap().file_name());

        let packages: FxHashMap<_, _> = {
            let main_package_root_path = absolute_path
                .parent()
                .expect("to have a parent directory")
                .to_path_buf();

            [(
                main_package_name,
                Package::new(
                    main_package_name,
                    main_package_root_path.clone(),
                    main_source,
                ),
            )]
            .into_iter()
            .collect()
        };

        let sources = Rc::new(RefCell::new(sources));

        Ok(Self {
            timings: Timings::new(),
            build_options,
            diagnostics: Diagnostics::new(sources.clone()),
            sources,
            packages,
            modules: IndexVec::new(),
            defs: IndexVec::new(),
            adts: IndexVec::new(),
            types: CommonTypes::new(),
            coercions: HirMap::default(),
            extern_libs: FxHashSet::default(),
            main_package_name,
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
        let file_name =
            main_path.file_stem().expect("main source to be a file");
        let src_dir = main_path.parent().expect("to have a parent directory");
        src_dir.join(self.build_options.output_dir.clone()).join(file_name)
    }

    pub fn output_dir(&self) -> Utf8PathBuf {
        self.output_path()
            .parent()
            .expect("expected to have a parent directory for output_path")
            .to_owned()
    }

    pub fn main_package(&self) -> &Package {
        &self.packages[&self.main_package_name]
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

    pub fn find_module_by_source_id(
        &self,
        id: SourceId,
    ) -> Option<&ModuleInfo> {
        self.modules.iter().find(|m| m.source_id == id)
    }

    pub fn find_module_by_path(&self, path: &Utf8Path) -> Option<&ModuleInfo> {
        let sources = &self.sources.borrow();

        self.modules.iter().find(
            |m| matches!(sources.get(m.source_id), Some(s) if s.path() == path),
        )
    }

    pub fn find_module_by_qpath(&self, qpath: &QPath) -> Option<&ModuleInfo> {
        self.modules.iter().find(|m| &m.qpath == qpath)
    }

    pub fn find_package_by_source_id(&self, id: SourceId) -> Option<&Package> {
        self.find_module_by_source_id(id)
            .and_then(|m| self.packages.get(&m.package))
    }

    pub fn adt_def(&self, adt_id: AdtId) -> Option<&Def> {
        self.adts.get(adt_id).and_then(|s| self.defs.get(s.def_id))
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
            let mut file = fs::File::create(
                self.output_path().with_extension(opt.as_extension()),
            )?;
            f(self, &mut file)
        } else {
            Ok(())
        }
    }

    pub fn time<R>(
        &mut self,
        name: impl Into<String>,
        mut f: impl FnMut(&mut Self) -> R,
    ) -> R {
        if self.build_options().timings {
            self.timings.start(name);
            let result = f(self);
            self.timings.stop();
            result
        } else {
            f(self)
        }
    }

    pub fn print_timings(&self) {
        if self.build_options().timings {
            self.timings.print();
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
new_db_key!(AdtId -> adts : Adt);

#[derive(Debug, Clone)]
pub struct Package {
    pub name: Ustr,
    pub root_path: Utf8PathBuf,
    pub main_source_id: SourceId,
}

impl Package {
    pub fn new(
        name: Ustr,
        root_path: Utf8PathBuf,
        main_source_id: SourceId,
    ) -> Self {
        Self { name, root_path, main_source_id }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub id: ModuleId,
    pub package: Ustr,
    pub source_id: SourceId,
    pub qpath: QPath,
    #[allow(unused)]
    pub is_main: bool,
}

impl ModuleInfo {
    pub fn alloc(
        db: &mut Db,
        package: Ustr,
        source_id: SourceId,
        name: QPath,
        is_main: bool,
    ) -> ModuleId {
        let id = db.modules.push_with_key(|id| Self {
            id,
            package,
            source_id,
            qpath: name,
            is_main,
        });

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
    pub mutability: Mutability,
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
    Adt(AdtId),
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

impl Def {
    pub fn alloc(
        db: &mut Db,
        qpath: QPath,
        scope: ScopeInfo,
        kind: DefKind,
        mutability: Mutability,
        ty: Ty,
        span: Span,
    ) -> DefId {
        db.defs.push_with_key(|id| Self {
            id,
            name: qpath.name(),
            qpath,
            scope,
            kind: Box::new(kind),
            mutability,
            ty,
            span,
        })
    }

    pub fn word(&self) -> Word {
        Word::new(self.name, self.span)
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
pub struct Adt {
    pub id: AdtId,
    pub def_id: DefId,
    pub name: Word,
    pub kind: AdtKind,
}

impl Adt {
    #[must_use]
    pub fn is_ref(&self) -> bool {
        match &self.kind {
            AdtKind::Struct(s) => s.kind.is_ref(),
        }
    }

    #[must_use]
    pub fn as_struct(&self) -> Option<&StructDef> {
        self.kind.as_struct()
    }

    #[must_use]
    pub fn as_struct_mut(&mut self) -> Option<&mut StructDef> {
        self.kind.as_struct_mut()
    }

    #[must_use]
    pub fn is_infinitely_sized(&self) -> Option<&StructField> {
        match &self.kind {
            AdtKind::Struct(s) => s.is_infinitely_sized(),
        }
    }

    pub fn ty(&self) -> Ty {
        TyKind::Adt(self.id, todo!("fill with the adt's ty params")).into()
    }
}

#[derive(Debug, Clone)]
pub enum AdtKind {
    Struct(StructDef),
}

impl AdtKind {
    #[allow(clippy::unnecessary_wraps)]
    #[must_use]
    pub fn as_struct(&self) -> Option<&StructDef> {
        match self {
            Self::Struct(v) => Some(v),
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    #[must_use]
    pub fn as_struct_mut(&mut self) -> Option<&mut StructDef> {
        match self {
            Self::Struct(v) => Some(v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub id: AdtId,
    pub fields: Vec<StructField>,
    pub kind: StructKind,
    pub ctor_ty: Ty,
    pub ctor_vis: Vis,
}

#[derive(Debug, Clone, Copy)]
pub enum StructKind {
    Ref,
    Extern,
}

impl StructKind {
    /// Returns `true` if the struct kind is [`Ref`].
    ///
    /// [`Ref`]: StructKind::Ref
    #[must_use]
    pub fn is_ref(self) -> bool {
        matches!(self, Self::Ref)
    }
}

impl StructDef {
    pub fn new(
        id: AdtId,
        fields: Vec<StructField>,
        kind: StructKind,
        ctor_ty: Ty,
    ) -> Self {
        let ctor_vis = if fields.iter().any(|f| f.vis == Vis::Private) {
            Vis::Private
        } else {
            Vis::Public
        };

        Self { id, fields, kind, ctor_ty, ctor_vis }
    }

    pub fn field_by_name(&self, name: &str) -> Option<&StructField> {
        self.fields.iter().find(|f| f.name.name() == name)
    }

    pub fn fill_ctor_ty(&mut self, ret: Ty) {
        self.ctor_ty = Ty::new(TyKind::Fn(FnTy {
            params: self
                .fields
                .iter()
                .map(|f| FnTyParam { name: Some(f.name.name()), ty: f.ty })
                .collect(),
            ret,
            is_c_variadic: false,
        }));
    }

    pub fn is_infinitely_sized(&self) -> Option<&StructField> {
        fn contains_struct(ty: Ty, adt_id: AdtId) -> bool {
            match ty.kind() {
                TyKind::Fn(fun) => {
                    if fun.params.iter().any(|p| contains_struct(p.ty, adt_id))
                    {
                        return false;
                    }

                    contains_struct(fun.ret, adt_id)
                }
                TyKind::Adt(adt_id2, _) if *adt_id2 == adt_id => true,
                _ => false,
            }
        }

        if let StructKind::Extern = self.kind {
            self.fields.iter().find(|f| contains_struct(f.ty, self.id))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Word,
    pub vis: Vis,
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
            ExternLib::Sys(name) | ExternLib::Path { name, .. } => {
                f.write_str(name)
            }
        }
    }
}

impl ExternLib {
    pub fn try_from_str(
        name: &str,
        relative_to: impl AsRef<Utf8Path>,
    ) -> Option<Self> {
        let path = Path::new(name);

        if name.starts_with(['.', '/', '\\'])
            || path.has_root()
            || path.extension().is_some()
        {
            path.absolutize_from(relative_to.as_ref().as_std_path()).ok().map(
                |p| {
                    let p: Utf8PathBuf = p.into_owned().try_into().unwrap();
                    let search_path = p.parent().unwrap().to_path_buf();
                    let name = p.file_name().unwrap().to_string();
                    Self::Path { search_path, name }
                },
            )
        } else {
            Some(Self::Sys(name.to_string()))
        }
    }
}
