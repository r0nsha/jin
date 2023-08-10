use std::{
    cmp, io,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::{
    common::{new_id_type, IdVec, QualifiedName},
    diagnostics::Diagnostics,
    span::{Source, SourceId, Sources, Span},
    ty::Ty,
};

#[derive(Debug)]
pub(crate) struct Database {
    build_options: BuildOptions,

    pub(crate) sources: Sources,
    pub(crate) modules: IdVec<ModuleId, Module>,
    pub(crate) symbols: IdVec<SymbolId, Symbol>,
    pub(crate) functions: IdVec<FunctionId, Function>,
    pub(crate) types: IdVec<TyId, Ty>,

    pub(crate) diagnostics: Diagnostics,

    root_dir: PathBuf,
    main_source: SourceId,
    main_module: Option<ModuleId>,
    main_fun: Option<FunctionId>,
}

impl Database {
    pub(crate) fn new(
        build_options: BuildOptions,
        root_file: PathBuf,
    ) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut sources = Sources::new();
        let main_source = sources.add_file(absolute_path.to_path_buf())?;

        Ok(Self {
            build_options,

            sources,
            modules: IdVec::new(),
            symbols: IdVec::new(),
            functions: IdVec::new(),
            types: IdVec::new(),

            diagnostics: Diagnostics::new(),

            root_dir: absolute_path.parent().unwrap().to_path_buf(),
            main_source,
            main_module: None,
            main_fun: None,
        })
    }

    pub(crate) fn build_options(&self) -> &BuildOptions {
        &self.build_options
    }

    pub(crate) fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub(crate) fn main_source_id(&self) -> SourceId {
        self.main_source
    }

    pub(crate) fn main_source(&self) -> &Source {
        self.sources.get(self.main_source).unwrap()
    }

    pub(crate) fn main_module_id(&self) -> Option<ModuleId> {
        self.main_module
    }

    pub(crate) fn main_module(&self) -> Option<&Module> {
        self.main_module.and_then(|id| self.modules.get(id))
    }

    pub(crate) fn main_fun_id(&self) -> Option<FunctionId> {
        self.main_fun
    }

    pub(crate) fn main_fun(&self) -> Option<&Function> {
        self.main_fun.and_then(|id| self.functions.get(id))
    }

    pub(crate) fn set_main_fun(&mut self, id: FunctionId) {
        self.main_fun = Some(id);
    }
}

#[derive(Debug)]
pub(crate) struct BuildOptions {
    pub(crate) print_times: bool,
    pub(crate) print_ast: bool,
    pub(crate) print_hir: bool,
    pub(crate) print_mir: bool,
}

new_id_type!(ModuleId);

impl ModuleId {
    pub(crate) fn get(self, db: &Database) -> &Module {
        &db.modules[self]
    }

    pub(crate) fn get_mut(self, db: &mut Database) -> &mut Module {
        &mut db.modules[self]
    }
}

new_id_type!(SymbolId);

impl SymbolId {
    pub(crate) fn get(self, db: &Database) -> &Symbol {
        &db.symbols[self]
    }

    pub(crate) fn get_mut(self, db: &mut Database) -> &mut Symbol {
        &mut db.symbols[self]
    }
}

new_id_type!(FunctionId);

impl FunctionId {
    pub(crate) fn get(self, db: &Database) -> &Function {
        &db.functions[self]
    }

    pub(crate) fn get_mut(self, db: &mut Database) -> &mut Function {
        &mut db.functions[self]
    }
}

new_id_type!(TyId);

impl Ty {
    pub(crate) fn alloc(db: &mut Database, ty: Ty) -> TyId {
        db.types.push(ty)
    }
}

impl TyId {
    pub(crate) fn get(self, db: &Database) -> &Ty {
        &db.types[self]
    }

    pub(crate) fn get_mut(self, db: &mut Database) -> &mut Ty {
        &mut db.types[self]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) id: ModuleId,
    pub(crate) source_id: SourceId,
    pub(crate) name: QualifiedName,
    pub(crate) is_main: bool,
}

impl Module {
    pub(crate) fn alloc(
        db: &mut Database,
        source_id: SourceId,
        name: QualifiedName,
        is_main: bool,
    ) -> ModuleId {
        let id = db.modules.push_with_id(|id| Module {
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
pub(crate) struct Symbol {
    pub(crate) id: SymbolId,
    pub(crate) module_id: ModuleId,
    pub(crate) qualified_name: QualifiedName,
    pub(crate) vis: Vis,
    pub(crate) scope_level: ScopeLevel,
    pub(crate) kind: SymbolKind,
    pub(crate) ty: TyId,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum SymbolKind {
    Function(FunctionId),
}

impl Symbol {
    pub(crate) fn alloc(
        db: &mut Database,
        module_id: ModuleId,
        qualified_name: QualifiedName,
        vis: Vis,
        scope_level: ScopeLevel,
        kind: SymbolKind,
        ty: TyId,
        span: Span,
    ) -> SymbolId {
        db.symbols.push_with_id(|id| Symbol {
            id,
            module_id,
            qualified_name,
            vis,
            scope_level,
            kind,
            ty,
            span,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Vis {
    Private,
    Public,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ScopeLevel {
    Global,
    Scope(usize),
}

impl ScopeLevel {
    pub(crate) fn next(self) -> Self {
        use ScopeLevel::*;

        match self {
            Global => Scope(1),
            Scope(n) => Scope(n + 1),
        }
    }

    pub(crate) fn prev(self) -> Self {
        use ScopeLevel::*;

        match self {
            Global => panic!("SymbolLevel::Global has no previous scope"),
            Scope(n) if n == 1 => Global,
            Scope(n) => Scope(n - 1),
        }
    }
}

impl From<usize> for ScopeLevel {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::Global,
            n => Self::Scope(n),
        }
    }
}

impl PartialOrd for ScopeLevel {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ScopeLevel {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        use ScopeLevel::*;

        match (self, other) {
            (Global, Global) => cmp::Ordering::Equal,
            (Global, Scope(_)) => cmp::Ordering::Less,
            (Scope(_), Global) => cmp::Ordering::Greater,
            (Scope(a), Scope(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub(crate) id: FunctionId,
    pub(crate) module_id: ModuleId,
    pub(crate) name: QualifiedName,
    pub(crate) kind: FunKind,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

impl Function {
    pub(crate) fn alloc(
        db: &mut Database,
        module_id: ModuleId,
        name: QualifiedName,
        kind: FunKind,
        span: Span,
        ty: TyId,
    ) -> FunctionId {
        db.functions.push_with_id(|id| Function {
            id,
            module_id,
            name,
            kind,
            span,
            ty,
        })
    }
}

#[derive(Debug, Clone)]
pub(crate) enum FunKind {
    Orphan,
}
