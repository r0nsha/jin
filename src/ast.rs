mod gen;
mod pretty_print;

pub use gen::gen;

use std::{io, path::Path};

use enum_as_inner::EnumAsInner;
use slotmap::{Key, SlotMap};
use ustr::{ustr, Ustr};

use crate::{
    span::{SourceId, Span, Spanned},
    ty::Ty,
};

#[derive(Debug, Clone)]
pub struct Module {
    source_id: SourceId,
    name: QualifiedName,
    is_root: bool,
    pub bindings: Vec<Binding>,
}

impl Module {
    pub fn new(source_id: SourceId, name: QualifiedName, is_root: bool) -> Self {
        Self {
            source_id,
            name,
            is_root,
            bindings: vec![],
        }
    }

    pub fn pretty_print(&self) -> io::Result<()> {
        pretty_print::print_module(self)
    }
}

#[derive(Debug)]
pub struct ResolvedModules {
    modules: SlotMap<ModuleId, ResolvedModule>,
    root_module_id: ModuleId,
}

impl ResolvedModules {
    pub fn new() -> Self {
        Self {
            modules: SlotMap::with_key(),
            root_module_id: ModuleId::null(),
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

    pub fn root_module(&self) -> &ResolvedModule {
        assert!(!self.root_module_id.is_null());
        self.get_module(self.root_module_id).unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = &ResolvedModule> {
        self.modules.values()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut ResolvedModule> {
        self.modules.values_mut()
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    id: ModuleId,
    source_id: SourceId,
    name: QualifiedName,
    is_root: bool,
    pub bindings: Vec<Binding>,
    pub binding_infos: SlotMap<BindingId, BindingInfo>,
}

impl From<Module> for ResolvedModule {
    fn from(module: Module) -> Self {
        Self {
            id: ModuleId::null(),
            source_id: module.source_id,
            name: module.name,
            is_root: module.is_root,
            bindings: module.bindings,
            binding_infos: SlotMap::with_key(),
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

    pub fn add_binding_info(&mut self, mut binding: BindingInfo) -> BindingId {
        self.binding_infos.insert_with_key(|key| {
            binding.id = key;
            binding
        })
    }

    pub fn get_binding_info(&mut self, id: BindingId) -> Option<&BindingInfo> {
        self.binding_infos.get(id)
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

#[derive(Debug, Clone)]
pub struct QualifiedName(Vec<Ustr>);

impl QualifiedName {
    pub fn new(full_name: Vec<Ustr>) -> Self {
        assert!(!full_name.is_empty());
        Self(full_name)
    }

    pub fn from_path(root: &Path, target: &Path) -> Option<Self> {
        let target = target.with_extension("");
        let stripped = target.strip_prefix(root).ok()?;

        Some(Self(
            stripped
                .iter()
                .map(|component| ustr(component.to_string_lossy().as_ref()))
                .collect(),
        ))
    }

    pub fn name(&self) -> Ustr {
        *self.0.last().unwrap()
    }

    pub fn full_name(&self, separator: &str) -> String {
        self.0
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }

    pub fn standard_full_name(&self) -> String {
        self.full_name(".")
    }

    pub fn child(mut self, name: Ustr) -> Self {
        self.0.push(name);
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Private,
    Public,
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

#[derive(Debug, Clone, EnumAsInner)]
pub enum Ast {
    Binding(Binding),
    Fun(Fun),
    Ret(Ret),
    Lit(Lit),
}

impl Ast {
    pub fn ty(&self) -> Option<&Ty> {
        match self {
            Self::Binding(binding) => binding.ty.as_ref(),
            Self::Fun(fun) => fun.ty.as_ref(),
            Self::Ret(ret) => ret.ty.as_ref(),
            Self::Lit(lit) => lit.ty.as_ref(),
        }
    }

    pub fn set_ty(&mut self, ty: Ty) {
        match self {
            Self::Binding(binding) => binding.set_ty(ty),
            Self::Fun(fun) => fun.set_ty(ty),
            Self::Ret(ret) => ret.set_ty(ty),
            Self::Lit(lit) => lit.set_ty(ty),
        }
    }

    pub fn ty_cloned(&self) -> Ty {
        self.ty().unwrap().clone()
    }
}

impl Spanned for Ast {
    fn span(&self) -> Span {
        match self {
            Self::Binding(binding) => binding.span,
            Self::Fun(fun) => fun.span,
            Self::Ret(ret) => ret.span,
            Self::Lit(lit) => lit.span,
        }
    }
}

macro_rules! define_ast {
    ($name: ident, $($element: ident: $ty: ty),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            $(pub $element: $ty),*,
            pub span: Span,
            pub ty: Option<Ty>,
        }

        impl $name {
            pub fn ty_cloned(&self) -> Ty {
                self.ty.as_ref().unwrap().clone()
            }

            pub fn set_ty(&mut self, ty: Ty) {
                self.ty = Some(ty);
            }
        }
    };
}

define_ast!(Binding, kind: BindingKind);

impl Binding {
    pub fn get_actual_ty(&self) -> Option<&Ty> {
        match &self.kind {
            BindingKind::Fun { fun, .. } => fun.ty.as_ref(),
        }
    }

    pub fn name(&self) -> Ustr {
        match &self.kind {
            BindingKind::Fun { name, .. } => *name,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BindingKind {
    Fun { name: Ustr, fun: Box<Fun> },
}

define_ast!(Fun, body: Box<Ast>);
define_ast!(Ret, value: Box<Option<Ast>>);
define_ast!(Lit, kind: LitKind);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
}
