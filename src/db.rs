use std::{
    cmp, io,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::{
    common::{new_key_type, IndexVec, QualifiedName},
    diagnostics::Diagnostics,
    span::{Source, SourceId, Sources, Span},
    ty::Ty,
};

#[derive(Debug)]
pub struct Database {
    build_options: BuildOptions,

    pub sources: Sources,
    pub modules: IndexVec<ModuleId, ModuleInfo>,
    pub definitions: IndexVec<DefinitionId, DefinitionInfo>,
    // TODO: split this into TypeCx?
    pub types: IndexVec<TyId, Ty>,

    pub diagnostics: Diagnostics,

    root_dir: PathBuf,
    main_source: SourceId,
    main_module: Option<ModuleId>,
    main_fun: Option<DefinitionId>,
}

impl Database {
    pub fn new(
        build_options: BuildOptions,
        root_file: &Path,
    ) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut sources = Sources::new();
        let main_source = sources.add_file(absolute_path.to_path_buf())?;

        Ok(Self {
            build_options,

            sources,
            modules: IndexVec::new(),
            definitions: IndexVec::new(),
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
    pub fn main_function_id(&self) -> Option<DefinitionId> {
        self.main_fun
    }

    pub fn main_function(&self) -> Option<&DefinitionInfo> {
        self.main_fun.and_then(|id| self.definitions.get(id))
    }

    pub fn set_main_fun(&mut self, id: DefinitionId) {
        self.main_fun = Some(id);
    }

    pub fn print_diagnostics(&self) {
        self.diagnostics
            .print(&self.sources)
            .expect("printing diagnostis to work");
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
new_db_key!(DefinitionId -> definitions : DefinitionInfo);
new_db_key!(TyId -> types : Ty);

#[derive(Debug)]
pub struct BuildOptions {
    pub print_times: bool,
    pub print_ast: bool,
    pub print_hir: bool,
    pub print_mir: bool,
}

impl Ty {
    pub fn alloc(db: &mut Database, ty: Self) -> TyId {
        db.types.push(ty)
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
        let id = db.modules.push_with_key(|id| Self {
            id,
            source_id,
            name,
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
pub struct DefinitionInfo {
    pub id: DefinitionId,
    pub module_id: ModuleId,
    pub qualified_name: QualifiedName,
    pub scope_level: ScopeLevel,
    pub kind: Box<DefinitionInfoKind>,
    pub ty: TyId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum DefinitionInfoKind {
    Function(FunctionInfo),
    Parameter,
}

impl DefinitionInfo {
    pub fn alloc(
        db: &mut Database,
        module_id: ModuleId,
        qualified_name: QualifiedName,
        scope_level: ScopeLevel,
        kind: DefinitionInfoKind,
        ty: TyId,
        span: Span,
    ) -> DefinitionId {
        db.definitions.push_with_key(|id| Self {
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
    pub const fn vis(&self) -> Vis {
        match &self.scope_level {
            ScopeLevel::Global(vis) => *vis,
            ScopeLevel::Scope(_) => Vis::Private,
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
    #[allow(unused)]
    Scope(usize),
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
            (Self::Global(_), Self::Scope(_)) => cmp::Ordering::Less,
            (Self::Scope(_), Self::Global(_)) => cmp::Ordering::Greater,
            (Self::Scope(a), Self::Scope(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionInfo {
    Orphan,
}
