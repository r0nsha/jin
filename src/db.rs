use std::{
    cmp, io,
    path::{Path, PathBuf},
};

use path_absolutize::Absolutize;

use crate::{
    ast::{self, Vis},
    common::{new_id_type, IdVec, QualifiedName},
    diagnostics::Diagnostics,
    span::{Source, SourceId, Sources, Span},
    ty::Type,
};

#[derive(Debug)]
pub(crate) struct Database {
    build_options: BuildOptions,

    pub(crate) sources: Sources,
    pub(crate) modules: IdVec<ModuleId, Module>,
    pub(crate) bindings: IdVec<BindingId, Binding>,
    pub(crate) funs: IdVec<FunId, Fun>,
    pub(crate) types: IdVec<TypeId, Type>,

    pub(crate) diagnostics: Diagnostics,

    root_dir: PathBuf,
    main_source: SourceId,
    main_module: ModuleId,
    main_fun: Option<BindingId>,
}

impl Database {
    pub(crate) fn new(build_options: BuildOptions, root_file: PathBuf) -> io::Result<Self> {
        let absolute_path = root_file.absolutize().unwrap();

        let mut sources = Sources::new();
        let main_source = sources.add_file(absolute_path.to_path_buf())?;

        Ok(Self {
            build_options,

            sources,
            modules: IdVec::new(),
            bindings: IdVec::new(),
            funs: IdVec::new(),
            types: IdVec::new(),

            diagnostics: Diagnostics::new(),

            root_dir: absolute_path.parent().unwrap().to_path_buf(),
            main_source,
            main_module: ModuleId::null(),
            main_fun: None,
        })
    }

    pub(crate) fn build_options(&self) -> &BuildOptions {
        &self.build_options
    }

    pub(crate) fn root_dir(&self) -> &Path {
        &self.root_dir
    }

    pub(crate) fn main_source(&self) -> &Source {
        self.sources.get(self.main_source).unwrap()
    }

    pub(crate) fn main_module(&self) -> &Module {
        assert!(!self.main_module.is_null());
        self.modules.get(self.main_module).unwrap()
    }

    pub(crate) fn main_fun(&self) {
        todo!()
    }
}

#[derive(Debug)]
pub(crate) struct BuildOptions {
    pub(crate) print_times: bool,
    pub(crate) print_ast: bool,
    pub(crate) print_hir: bool,
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

new_id_type!(BindingId);

impl BindingId {
    pub(crate) fn get(self, db: &Database) -> &Binding {
        &db.bindings[self]
    }

    pub(crate) fn get_mut(self, db: &mut Database) -> &mut Binding {
        &mut db.bindings[self]
    }
}

new_id_type!(FunId);

impl FunId {
    pub(crate) fn get(self, db: &Database) -> &Fun {
        &db.funs[self]
    }

    pub(crate) fn get_mut(self, db: &mut Database) -> &mut Fun {
        &mut db.funs[self]
    }
}

new_id_type!(TypeId);

impl TypeId {
    pub(crate) fn get(self, db: &Database) -> &Type {
        &db.types[self]
    }

    pub(crate) fn get_mut(self, db: &mut Database) -> &mut Type {
        &mut db.types[self]
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Module {
    id: ModuleId,
    source_id: SourceId,
    name: QualifiedName,
    is_root: bool,
}

impl From<&ast::Module> for Module {
    fn from(module: &ast::Module) -> Self {
        Self {
            id: ModuleId::null(),
            source_id: module.source_id,
            name: module.name.clone(),
            is_root: module.is_root,
        }
    }
}

impl Module {
    pub(crate) fn id(&self) -> ModuleId {
        self.id
    }

    pub(crate) fn source_id(&self) -> SourceId {
        self.source_id
    }

    pub(crate) fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub(crate) fn is_root(&self) -> bool {
        self.is_root
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub(crate) id: BindingId,
    pub(crate) module_id: ModuleId,
    pub(crate) qualified_name: QualifiedName,
    pub(crate) vis: Vis,
    pub(crate) scope_level: ScopeLevel,
    pub(crate) uses: usize,
    pub(crate) kind: BindingKind,
    pub(crate) ty: TypeId,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum BindingKind {
    Fun(FunId),
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
            Global => panic!("BindingScope::Global has no previous scope"),
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
pub(crate) struct Fun {
    pub(crate) kind: FunKind,
    pub(crate) span: Span,
    pub(crate) ty: TypeId,
}

#[derive(Debug, Clone)]
pub(crate) enum FunKind {
    Orphan,
}

// #[derive(Debug, Clone, EnumAsInner)]
// pub(crate) enum FunKind {
//     Orphan {
//         //     params: Vec<FunParam>,
//         body: Block,
//     },
//     // Extern {
//     //     lib: Option<ExternLib>,
//     //     dylib: Option<ExternLib>,
//     //     link_name: Ustr,
//     // },
//     // Intrinsic(Intrinsic),
// }
