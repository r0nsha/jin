use std::cmp;

use crate::{
    ast::{self, Vis},
    common::{new_id_type, IdVec, QualifiedName},
    span::{SourceId, Span},
    ty::Ty,
};

#[derive(Debug)]
pub(crate) struct Database {
    pub(crate) modules: IdVec<ModuleId, Module>,

    main_module: ModuleId,
    main_fun: Option<BindingId>,
}

impl Database {
    pub(crate) fn new() -> Self {
        Self {
            modules: IdVec::new(),
            main_module: ModuleId::null(),
            main_fun: None,
        }
    }

    pub(crate) fn main_module(&self) -> &Module {
        assert!(!self.main_module.is_null());
        self.modules.get(self.main_module).unwrap()
    }

    pub(crate) fn main_fun(&self) {
        todo!()
    }
}

new_id_type!(ModuleId);

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

new_id_type!(BindingId);

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub(crate) id: BindingId,
    pub(crate) module_id: ModuleId,
    pub(crate) qualified_name: QualifiedName,
    pub(crate) vis: Vis,
    pub(crate) scope_level: ScopeLevel,
    pub(crate) uses: usize,
    pub(crate) kind: BindingKind,
    pub(crate) ty: Ty, // TODO: store TyId
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

new_id_type!(FunId);

#[derive(Debug, Clone)]
pub(crate) struct Fun {
    pub(crate) kind: FunKind,
    pub(crate) span: Span,
    pub(crate) ty: Ty, // TODO: use TyId
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
