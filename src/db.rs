use std::{
    cmp, io,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::{
    common::{
        new_key_type,
        target::{TargetMetrics, TargetPlatform},
        IndexVec, QualifiedName,
    },
    diagnostics::Diagnostics,
    span::{Source, SourceId, Sources, Span},
    ty::Type,
};

#[derive(Debug)]
pub struct Database {
    build_options: BuildOptions,

    pub sources: Sources,
    pub modules: IndexVec<ModuleId, ModuleInfo>,
    pub symbols: IndexVec<SymbolId, SymbolInfo>,
    pub types: IndexVec<TypeId, Type>,

    pub diagnostics: Diagnostics,

    root_dir: PathBuf,
    main_source: SourceId,
    main_module: Option<ModuleId>,
    main_fun: Option<SymbolId>,
}

impl Database {
    pub fn new(build_options: BuildOptions, root_file: &Path) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut sources = Sources::new();
        let main_source = sources.add_file(absolute_path.to_path_buf())?;

        Ok(Self {
            build_options,

            sources,
            modules: IndexVec::new(),
            symbols: IndexVec::new(),
            types: IndexVec::new(),

            diagnostics: Diagnostics::new(),

            root_dir: absolute_path.parent().unwrap().to_path_buf(),
            main_source,
            main_module: None,
            main_fun: None,
        })
    }

    pub fn build_options(&self) -> &BuildOptions {
        &self.build_options
    }

    pub fn output_path(&self) -> PathBuf {
        self.main_source().path().with_extension("")
    }

    pub fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    #[allow(unused)]
    pub fn main_source_id(&self) -> SourceId {
        self.main_source
    }

    pub fn main_source(&self) -> &Source {
        self.sources.get(self.main_source).unwrap()
    }

    pub fn main_module_id(&self) -> Option<ModuleId> {
        self.main_module
    }

    pub fn main_module(&self) -> Option<&ModuleInfo> {
        self.main_module.and_then(|id| self.modules.get(id))
    }

    #[allow(unused)]
    pub fn main_function_id(&self) -> Option<SymbolId> {
        self.main_fun
    }

    pub fn main_function(&self) -> Option<&SymbolInfo> {
        self.main_fun.and_then(|id| self.symbols.get(id))
    }

    pub fn set_main_fun(&mut self, id: SymbolId) {
        self.main_fun = Some(id);
    }

    pub fn print_diagnostics(&self) {
        self.diagnostics.print(&self.sources).expect("printing diagnostis to work");
    }

    pub fn alloc_ty(&mut self, ty: Type) -> TypeId {
        self.types.push(ty)
    }
}

macro_rules! new_db_key {
    ($key: ident -> $collection: ident : $type: ident) => {
        new_key_type!($key);

        #[allow(unused)]
        impl std::ops::Index<$key> for Database {
            type Output = $type;

            fn index(&self, index: $key) -> &Self::Output {
                &self.$collection[index]
            }
        }

        #[allow(unused)]
        impl std::ops::IndexMut<$key> for Database {
            fn index_mut(&mut self, index: $key) -> &mut Self::Output {
                &mut self.$collection[index]
            }
        }
    };
}

new_db_key!(ModuleId -> modules : ModuleInfo);
new_db_key!(SymbolId -> symbols : SymbolInfo);
new_db_key!(TypeId -> types : Type);

#[derive(Debug)]
#[allow(clippy::struct_excessive_bools)]
pub struct BuildOptions {
    pub print_times: bool,
    pub print_ast: bool,
    pub print_hir: bool,
    pub print_mir: bool,
    pub print_llvm_ir: bool,
    pub target_platform: TargetPlatform,
    pub target_metrics: TargetMetrics,
}

impl BuildOptions {
    #[allow(clippy::fn_params_excessive_bools)]
    pub fn new(
        print_times: bool,
        print_ast: bool,
        print_hir: bool,
        print_mir: bool,
        print_llvm_ir: bool,
        target_platform: TargetPlatform,
    ) -> Self {
        let target_metrics = target_platform.metrics();
        Self {
            print_times,
            print_ast,
            print_hir,
            print_mir,
            print_llvm_ir,
            target_platform,
            target_metrics,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleInfo {
    #[allow(unused)]
    pub id: ModuleId,
    pub source_id: SourceId,
    pub name: QualifiedName,
    #[allow(unused)]
    pub is_main: bool,
}

impl ModuleInfo {
    pub fn alloc(
        db: &mut Database,
        source_id: SourceId,
        name: QualifiedName,
        is_main: bool,
    ) -> ModuleId {
        let id = db.modules.push_with_key(|id| Self { id, source_id, name, is_main });

        if is_main {
            assert!(db.main_module.is_none());
            db.main_module = Some(id);
        }

        id
    }
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub id: SymbolId,
    pub module_id: ModuleId,
    pub qualified_name: QualifiedName,
    pub scope_level: ScopeLevel,
    pub kind: Box<SymbolInfoKind>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SymbolInfoKind {
    Function(FunctionInfo),
    Variable,
}

impl SymbolInfo {
    pub fn alloc(
        db: &mut Database,
        module_id: ModuleId,
        qualified_name: QualifiedName,
        scope_level: ScopeLevel,
        kind: SymbolInfoKind,
        ty: TypeId,
        span: Span,
    ) -> SymbolId {
        db.symbols.push_with_key(|id| Self {
            id,
            module_id,
            qualified_name,
            scope_level,
            kind: Box::new(kind),
            ty,
            span,
        })
    }

    #[allow(unused)]
    pub fn vis(&self) -> Vis {
        match &self.scope_level {
            ScopeLevel::Global(vis) => *vis,
            ScopeLevel::Local(_) => Vis::Private,
        }
    }

    pub fn set_ty(&mut self, ty: TypeId) {
        if self.ty.is_null() {
            self.ty = ty;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Private,
    Public,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeLevel {
    Global(Vis),
    Local(usize),
}

impl PartialOrd for ScopeLevel {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ScopeLevel {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Self::Global(_), Self::Global(_)) => cmp::Ordering::Equal,
            (Self::Global(_), Self::Local(_)) => cmp::Ordering::Less,
            (Self::Local(_), Self::Global(_)) => cmp::Ordering::Greater,
            (Self::Local(a), Self::Local(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionInfo {
    Orphan,
}
