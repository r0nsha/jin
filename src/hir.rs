mod pretty_print;

use std::{cmp, io};

use enum_as_inner::EnumAsInner;
use slotmap::{Key, SecondaryMap, SlotMap};

use crate::{
    ast::{self, QualifiedName, Vis},
    span::{SourceId, Span, Spanned},
    ty::{Ty, Typed},
};

#[derive(Debug)]
pub struct Cache {
    pub modules: SlotMap<ModuleId, Module>,
    root_module_id: ModuleId,
    binding_infos: SlotMap<BindingId, BindingInfo>,
    global_bindings: SecondaryMap<BindingId, Binding>,
    entry_point_id: Option<BindingId>,
}

impl Cache {
    pub fn new() -> Self {
        Self {
            modules: SlotMap::with_key(),
            root_module_id: ModuleId::null(),
            binding_infos: SlotMap::with_key(),
            global_bindings: SecondaryMap::new(),
            entry_point_id: None,
        }
    }

    pub fn get_module(&self, id: ModuleId) -> Option<&Module> {
        self.modules.get(id)
    }

    pub fn insert_module(&mut self, mut module: Module) -> ModuleId {
        self.modules.insert_with_key(|key| {
            module.id = key;

            if module.is_root {
                self.root_module_id = module.id;
            }

            module
        })
    }

    pub fn get_binding_info(&self, id: BindingId) -> Option<&BindingInfo> {
        self.binding_infos.get(id)
    }

    pub fn insert_binding_info(&mut self, mut binding: BindingInfo) -> BindingId {
        self.binding_infos.insert_with_key(|key| {
            binding.id = key;
            binding
        })
    }

    pub fn get_global_binding(&self, id: BindingId) -> Option<&Binding> {
        self.global_bindings.get(id)
    }

    pub fn insert_global_binding(&mut self, mut binding: Binding) {
        assert!(!binding.id.is_null());
        self.global_bindings.insert(binding.id, binding);
    }

    pub fn get_root_module(&self) -> &Module {
        assert!(!self.root_module_id.is_null());
        self.get_module(self.root_module_id).unwrap()
    }

    pub fn get_entry_point_info(&self) -> Option<&BindingInfo> {
        self.entry_point_id.and_then(|id| self.get_binding_info(id))
    }

    pub fn get_entry_point(&self) -> Option<&Binding> {
        self.entry_point_id
            .and_then(|id| self.get_global_binding(id))
    }

    pub fn pretty_print(&self) -> io::Result<()> {
        pretty_print::print_hir(self)
    }
}

#[derive(Debug, Clone)]
pub struct Module {
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
    pub module_id: ModuleId,
    pub id: BindingId,
    pub qualified_name: QualifiedName,
    pub vis: Vis,
    pub scope: BindingScope,
    pub uses: usize,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingScope {
    Global,
    Scope(usize),
}

impl BindingScope {
    pub fn next(self) -> Self {
        use BindingScope::*;

        match self {
            Global => Scope(1),
            Scope(n) => Scope(n + 1),
        }
    }

    pub fn prev(self) -> Self {
        use BindingScope::*;

        match self {
            Global => panic!("BindingScope::Global has no previous scope"),
            Scope(n) if n == 1 => Global,
            Scope(n) => Scope(n - 1),
        }
    }
}

impl From<usize> for BindingScope {
    fn from(value: usize) -> Self {
        match value {
            0 => Self::Global,
            n => Self::Scope(n),
        }
    }
}

impl PartialOrd for BindingScope {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BindingScope {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        use BindingScope::*;

        match (self, other) {
            (Global, Global) => cmp::Ordering::Equal,
            (Global, Scope(_)) => cmp::Ordering::Less,
            (Scope(_), Global) => cmp::Ordering::Greater,
            (Scope(a), Scope(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Hir {
    // Name(Name),
    Binding(Binding),
    Ret(Ret),
    Const(Const),
}

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

define_hir!(Binding, id: BindingId, kind: BindingKind);

#[derive(Debug, Clone, EnumAsInner)]
pub enum BindingKind {
    Value(Box<Hir>),
    Fun(Box<Fun>),
}

impl Spanned for BindingKind {
    fn span(&self) -> Span {
        match self {
            BindingKind::Value(v) => v.span(),
            BindingKind::Fun(f) => f.span,
        }
    }
}

impl Typed for BindingKind {
    fn ty(&self) -> &Ty {
        match self {
            BindingKind::Value(v) => v.ty(),
            BindingKind::Fun(f) => &f.ty,
        }
    }
}

define_hir!(Fun, kind: FunKind);

#[derive(Debug, Clone, EnumAsInner)]
pub enum FunKind {
    Orphan {
        //     params: Vec<FunParam>,
        body: Block,
    },
    // Extern {
    //     lib: Option<ExternLib>,
    //     dylib: Option<ExternLib>,
    //     link_name: Ustr,
    // },
    // Intrinsic(Intrinsic),
}

define_hir!(Block, statements: Vec<Hir>);
define_hir!(Name, id: BindingId);
define_hir!(Ret, value: Option<Box<Hir>>);
define_hir!(Const, kind: ConstKind);

#[derive(Debug, Clone)]
pub enum ConstKind {
    Int(usize),
    Unit,
}
