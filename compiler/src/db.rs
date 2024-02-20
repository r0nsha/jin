pub mod build_options;
mod timing;

use core::fmt;
use std::{cmp, fs, io, path::Path};

use camino::{Utf8Path, Utf8PathBuf};
use compiler_data_structures::{
    index_vec::{IndexVec, IndexVecExt},
    new_key_type,
    once::Once,
};
use path_absolutize::Absolutize;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::Ustr;

use crate::{
    db::{
        build_options::{BuildOptions, EmitOption},
        timing::Timings,
    },
    diagnostics::Diagnostics,
    middle::{CallConv, Mutability, TyParam, Vis},
    qpath::QPath,
    span::{Source, SourceId, Sources, Span, Spanned},
    target::{TargetMetrics, TargetPlatform},
    ty::{FloatTy, FnTy, FnTyFlags, FnTyParam, Instantiation, IntTy, Ty, TyKind, UintTy},
    word::Word,
};

#[derive(Debug)]
pub struct Db {
    pub sources: Sources,
    pub packages: FxHashMap<Ustr, Package>,
    pub modules: IndexVec<ModuleId, ModuleInfo>,
    pub defs: IndexVec<DefId, Def>,
    pub adts: IndexVec<AdtId, Adt>,
    pub variants: IndexVec<VariantId, Variant>,
    pub types: CommonTypes,
    pub builtins: FxHashMap<DefId, Builtin>,
    pub extern_libs: FxHashSet<ExternLib>,
    pub diagnostics: Diagnostics,

    timings: Timings,
    build_options: BuildOptions,

    pub std_package_name: Once<Ustr>,
    pub main_package_name: Once<Ustr>,
    pub main_source: Once<SourceId>,
    pub main_module: Once<ModuleId>,
}

impl Db {
    pub fn new(build_options: BuildOptions) -> Self {
        Self {
            timings: Timings::new(),
            build_options,
            sources: Sources::new(),
            packages: FxHashMap::default(),
            modules: IndexVec::new(),
            defs: IndexVec::new(),
            adts: IndexVec::new(),
            variants: IndexVec::new(),
            types: CommonTypes::new(),
            builtins: FxHashMap::default(),
            extern_libs: FxHashSet::default(),
            diagnostics: Diagnostics::new(),
            std_package_name: Once::new(),
            main_package_name: Once::new(),
            main_source: Once::new(),
            main_module: Once::new(),
        }
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
        let root_dir = main_path.parent().expect("to have a parent directory");

        let target_dir = if let Some(dir) = &self.build_options.output_dir {
            root_dir.join(dir)
        } else {
            root_dir.to_path_buf()
        };

        target_dir.join(file_name)
    }

    pub fn output_dir(&self) -> Utf8PathBuf {
        self.output_path()
            .parent()
            .expect("expected to have a parent directory for output_path")
            .to_owned()
    }

    pub fn main_package(&self) -> &Package {
        &self.packages[&self.main_package_name.unwrap()]
    }

    pub fn set_main_package(&mut self, name: Ustr) {
        self.main_package_name.set(name);
        self.main_source.set(self.packages[&name].main_source_id);
    }

    pub fn main_source_id(&self) -> SourceId {
        self.main_source.unwrap()
    }

    pub fn main_source(&self) -> &Source {
        self.sources.get(self.main_source.unwrap()).expect("to always have a main source")
    }

    #[inline]
    #[track_caller]
    pub fn main_module(&self) -> &ModuleInfo {
        &self.modules[self.main_module.unwrap()]
    }

    pub fn find_module_by_source_id(&self, id: SourceId) -> Option<&ModuleInfo> {
        self.modules.iter().find(|m| m.source_id == id)
    }

    pub fn find_module_by_path(&self, path: &Utf8Path) -> Option<&ModuleInfo> {
        self.modules
            .iter()
            .find(|m| matches!(self.sources.get(m.source_id), Some(s) if s.path() == path))
    }

    pub fn find_module_by_qpath<'a>(
        &self,
        package: &str,
        path: impl IntoIterator<Item = &'a str>,
    ) -> Option<&ModuleInfo> {
        let qpath = QPath::from_iter(path);
        self.modules.iter().find(|m| m.package == package && m.qpath == qpath)
    }

    pub fn package(&self, package: Ustr) -> &Package {
        &self.packages[&package]
    }

    pub fn find_package_by_path(&self, path: &Utf8Path) -> Option<&Package> {
        self.packages.values().find(|p| path.starts_with(&p.root_path))
    }

    pub fn find_package_by_source_id(&self, id: SourceId) -> Option<&Package> {
        self.find_module_by_source_id(id).and_then(|m| self.packages.get(&m.package))
    }

    pub fn is_main_package(&self, package: Ustr) -> bool {
        self.main_package_name.get().map_or(false, |n| n == package)
    }

    pub fn create_package(
        &mut self,
        name: Ustr,
        root_file: &Utf8Path,
    ) -> anyhow::Result<(Ustr, SourceId)> {
        let absolute_path: Utf8PathBuf =
            root_file.as_std_path().absolutize()?.into_owned().try_into()?;

        if !absolute_path.is_file() {
            anyhow::bail!("`{}` in not a file", absolute_path);
        }

        let source_id = self.sources.load_file(absolute_path.to_path_buf())?;
        let root_path = absolute_path.parent().expect("to have a parent directory").to_path_buf();
        self.packages.insert(name, Package::new(name, root_path, source_id));

        Ok((name, source_id))
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

    pub fn time<R>(&mut self, name: impl Into<String>, f: impl FnOnce(&mut Self) -> R) -> R {
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

    pub fn print_diagnostics(self) {
        self.diagnostics.print(&self.sources)
    }
}

macro_rules! new_db_key {
    ($key: ident -> $collection: ident : $type: ident) => {
        new_key_type! {
            pub struct $key;
        }

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
new_db_key!(VariantId -> variants : Variant);

#[derive(Debug, Clone)]
pub struct Package {
    pub name: Ustr,
    pub root_path: Utf8PathBuf,
    pub main_source_id: SourceId,
}

impl Package {
    pub fn new(name: Ustr, root_path: Utf8PathBuf, main_source_id: SourceId) -> Self {
        Self { name, root_path, main_source_id }
    }

    pub fn is_std(&self, db: &Db) -> bool {
        db.std_package_name == self.name
    }
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    pub id: ModuleId,
    pub package: Ustr,
    pub source_id: SourceId,
    pub qpath: QPath,
    pub path: String,
    #[allow(unused)]
    pub is_main: bool,
}

impl ModuleInfo {
    pub fn alloc(
        db: &mut Db,
        package: Ustr,
        source_id: SourceId,
        qpath: QPath,
        is_main: bool,
    ) -> ModuleId {
        let id = db.modules.push_with_key(|id| Self {
            id,
            package,
            source_id,
            path: qpath.join(),
            qpath,
            is_main,
        });

        if is_main {
            db.main_module.set(id);
        }

        id
    }

    pub fn span(&self) -> Span {
        Span::uniform(self.source_id, 0)
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
    BuiltinTy(Ty),
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
        if let Self::BuiltinTy(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Fn(fi) => match fi {
                FnInfo::Bare => "function",
                FnInfo::Extern => "extern function",
            },
            Self::BuiltinTy(_) => "builtin type",
            Self::Adt(_) => "type",
            Self::ExternGlobal => "extern let",
            Self::Global => "let",
            Self::Variable => "variable",
        }
    }
}

impl fmt::Display for DefKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Def {
    pub fn alloc(
        db: &mut Db,
        qpath: QPath,
        scope: ScopeInfo,
        kind: DefKind,
        mutability: Mutability,
        span: Span,
    ) -> DefId {
        db.defs.push_with_key(|id| Self {
            id,
            name: qpath.name(),
            qpath,
            scope,
            kind: Box::new(kind),
            mutability,
            span,
        })
    }

    pub fn word(&self) -> Word {
        Word::new(self.name, self.span)
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
    pub ty_params: Vec<TyParam>,
    pub kind: AdtKind,
}

impl Adt {
    #[must_use]
    pub fn is_ref(&self) -> bool {
        match &self.kind {
            AdtKind::Struct(s) => s.kind.is_ref(),
            AdtKind::Union(u) => u.kind.is_ref(),
        }
    }

    #[must_use]
    pub fn is_value_struct(&self) -> bool {
        match &self.kind {
            AdtKind::Struct(s) => s.kind.is_value(),
            AdtKind::Union(_) => false,
        }
    }

    #[must_use]
    pub fn is_value_type(&self) -> bool {
        match &self.kind {
            AdtKind::Struct(s) => s.kind.is_value(),
            AdtKind::Union(u) => u.kind.is_value(),
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
    pub fn as_union(&self) -> Option<&UnionDef> {
        self.kind.as_union()
    }

    #[must_use]
    pub fn as_union_mut(&mut self) -> Option<&mut UnionDef> {
        self.kind.as_union_mut()
    }

    #[must_use]
    pub fn is_infinitely_sized<'a>(&'a self, db: &'a Db) -> Option<&AdtField> {
        match &self.kind {
            AdtKind::Struct(s) => s.is_infinitely_sized(db),
            AdtKind::Union(u) => u.is_infinitely_sized(db),
        }
    }

    pub fn ty(&self) -> Ty {
        TyKind::Adt(self.id, self.ty_params.iter().map(|tp| tp.ty).collect()).into()
    }

    pub fn instantiation(&self, targs: &[Ty]) -> Instantiation {
        debug_assert!(targs.len() == self.ty_params.len());

        self.ty_params
            .iter()
            .zip(targs)
            .map(|(tp, ty)| (tp.ty.as_param().unwrap().var, *ty))
            .collect()
    }

    pub fn fields<'a>(&'a self, db: &'a Db) -> Box<dyn Iterator<Item = &AdtField> + '_> {
        match &self.kind {
            AdtKind::Struct(s) => Box::new(s.fields.iter()),
            AdtKind::Union(u) => Box::new(u.variants(db).flat_map(|v| v.fields.iter())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AdtKind {
    Struct(StructDef),
    Union(UnionDef),
}

impl AdtKind {
    #[must_use]
    pub fn as_struct(&self) -> Option<&StructDef> {
        if let Self::Struct(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_struct_mut(&mut self) -> Option<&mut StructDef> {
        if let Self::Struct(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_union(&self) -> Option<&UnionDef> {
        if let Self::Union(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub fn as_union_mut(&mut self) -> Option<&mut UnionDef> {
        if let Self::Union(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub id: AdtId,
    pub fields: Vec<AdtField>,
    pub kind: StructKind,
    pub ctor_ty: Ty,
    pub ctor_vis: Vis,
}

#[derive(Debug, Clone, Copy)]
pub enum StructKind {
    Ref,
    Value,
}

impl StructKind {
    /// Returns `true` if the struct kind is [`Ref`].
    ///
    /// [`Ref`]: StructKind::Ref
    #[must_use]
    pub fn is_ref(self) -> bool {
        matches!(self, Self::Ref)
    }

    /// Returns `true` if the struct kind is [`Value`].
    ///
    /// [`Value`]: StructKind::Value
    #[must_use]
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value)
    }
}

impl StructDef {
    pub fn new(id: AdtId, fields: Vec<AdtField>, kind: StructKind, ctor_ty: Ty) -> Self {
        Self { id, fields, kind, ctor_ty, ctor_vis: Vis::Module }
    }

    pub fn field_by_name(&self, name: &str) -> Option<&AdtField> {
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
            callconv: CallConv::default(),
            flags: FnTyFlags::empty(),
        }));
    }

    pub fn fill_ctor_vis(&mut self) {
        self.ctor_vis = self.fields.iter().map(|f| f.vis).min().unwrap_or(Vis::Export);
    }

    pub fn is_infinitely_sized(&self, db: &Db) -> Option<&AdtField> {
        if let StructKind::Value = self.kind {
            self.fields.iter().find(|f| f.ty.infinitely_contains_adt(db, self.id))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnionDef {
    pub id: AdtId,
    pub kind: UnionKind,
    pub variants: Vec<VariantId>,
}

impl UnionDef {
    pub fn new(id: AdtId, kind: UnionKind) -> Self {
        Self { id, kind, variants: vec![] }
    }

    pub fn variants<'a>(&'a self, db: &'a Db) -> impl Iterator<Item = &Variant> {
        self.variants.iter().map(|&id| &db[id])
    }

    pub fn is_infinitely_sized<'a>(&'a self, db: &'a Db) -> Option<&AdtField> {
        if let UnionKind::Value = self.kind {
            self.variants(db)
                .find_map(|v| v.fields.iter().find(|f| f.ty.infinitely_contains_adt(db, self.id)))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnionKind {
    Ref,
    Value,
}

impl UnionKind {
    /// Returns `true` if the union kind is [`Ref`].
    ///
    /// [`Ref`]: UnionKind::Ref
    #[must_use]
    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Ref)
    }

    /// Returns `true` if the union kind is [`Value`].
    ///
    /// [`Value`]: UnionKind::Value
    #[must_use]
    pub fn is_value(&self) -> bool {
        matches!(self, Self::Value)
    }
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub id: VariantId,
    pub adt_id: AdtId,
    pub index: usize,
    pub name: Word,
    pub fields: Vec<AdtField>,
    pub ctor_ty: Ty,
}

impl Variant {
    pub fn field_by_name(&self, name: &str) -> Option<&AdtField> {
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
            callconv: CallConv::default(),
            flags: FnTyFlags::empty(),
        }));
    }

    pub fn full_name(&self, db: &Db) -> String {
        format!("{}.{}", db[self.adt_id].name, self.name)
    }
}

#[derive(Debug, Clone)]
pub struct AdtField {
    pub name: Word,
    pub vis: Vis,
    pub ty: Ty,
}

impl Spanned for AdtField {
    fn span(&self) -> Span {
        self.name.span()
    }
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

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    SliceGrow,
    Forget,
}

impl<'a> TryFrom<&'a str> for Builtin {
    type Error = ();

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match value {
            "slice_grow" => Ok(Self::SliceGrow),
            "forget" => Ok(Self::Forget),
            _ => Err(()),
        }
    }
}
