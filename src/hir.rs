use enum_as_inner::EnumAsInner;
use slotmap::{Key, SlotMap};

use crate::{
    ast::{self, QualifiedName, Vis},
    span::{SourceId, Span, Spanned},
    ty::Typed,
};

#[derive(Debug)]
pub struct Cache {
    modules: SlotMap<ModuleId, ResolvedModule>,
    root_module_id: ModuleId,
    binding_infos: SlotMap<BindingId, BindingInfo>,
    bindings: SlotMap<BindingId, Binding>,
    funs: SlotMap<FunId, Fun>,
    entry_point_fun_id: Option<FunId>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            modules: SlotMap::with_key(),
            root_module_id: ModuleId::null(),
            binding_infos: SlotMap::with_key(),
            bindings: SlotMap::with_key(),
            funs: SlotMap::with_key(),
            entry_point_fun_id: None,
        }
    }

    pub fn get_module(&self, id: ModuleId) -> Option<&ResolvedModule> {
        self.modules.get(id)
    }

    pub fn insert_module(&mut self, mut module: ResolvedModule) -> ModuleId {
        self.modules.insert_with_key(|key| {
            module.id = key;

            if module.is_root {
                self.root_module_id = module.id;
            }

            module
        })
    }

    pub fn get_binding_info(&mut self, id: BindingId) -> Option<&BindingInfo> {
        self.binding_infos.get(id)
    }

    pub fn insert_binding_info(&mut self, mut binding: BindingInfo) -> BindingId {
        self.binding_infos.insert_with_key(|key| {
            binding.id = key;
            binding
        })
    }

    pub fn root_module(&self) -> &ResolvedModule {
        assert!(!self.root_module_id.is_null());
        self.get_module(self.root_module_id).unwrap()
    }

    pub fn modules(&self) -> impl Iterator<Item = &ResolvedModule> {
        self.modules.values()
    }

    pub fn binding_infos(&self) -> impl Iterator<Item = &BindingInfo> {
        self.binding_infos.values()
    }

    pub fn funs(&self) -> impl Iterator<Item = &Fun> {
        self.funs.values()
    }

    pub fn entry_point_fun(&self) -> Option<&Fun> {
        self.entry_point_fun_id.map(|id| self.funs.get(id))
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    id: ModuleId,
    source_id: SourceId,
    name: QualifiedName,
    is_root: bool,
}

impl From<ast::Module> for ResolvedModule {
    fn from(module: ast::Module) -> Self {
        Self {
            id: ModuleId::null(),
            source_id: module.source_id,
            name: module.name,
            is_root: module.is_root,
        }
    }
}

impl ResolvedModule {
    pub fn id(&self) -> ModuleId {
        self.id
    }

    pub fn source_id(&self) -> SourceId {
        self.source_id
    }

    pub fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub fn is_root(&self) -> bool {
        self.is_root
    }
}

slotmap::new_key_type! {
    pub struct BindingId;
    pub struct ModuleId;
}

#[derive(Debug, Clone)]
pub struct BindingInfo {
    pub id: BindingId,
    pub qualified_name: QualifiedName,
    pub vis: Vis,
    pub scope: BindingScope,
    pub uses: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingScope {
    Global,
    Scope(usize),
}

impl BindingScope {
    pub fn next(self) -> Self {
        match self {
            BindingScope::Global => BindingScope::Scope(1),
            BindingScope::Scope(n) => BindingScope::Scope(n + 1),
        }
    }

    pub fn prev(self) -> Self {
        match self {
            BindingScope::Global => panic!("BindingScope::Global has no previous scope"),
            BindingScope::Scope(n) if n == 1 => BindingScope::Global,
            BindingScope::Scope(n) => BindingScope::Scope(n - 1),
        }
    }
}

slotmap::new_key_type! {
    pub struct FunId;
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Hir {}

impl Spanned for Hir {
    fn span(&self) -> Span {
        todo!()
    }
}

impl Typed for Hir {
    fn ty(&self) -> &crate::ty::Ty {
        todo!()
    }
}

macro_rules! define_hir {
    ($name: ident, $($element: ident: $ty: ty),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            $(pub $element: $ty),*,
            pub span: Span,
            pub ty: Ty,
        }
    };
}
