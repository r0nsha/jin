mod gen;

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

    pub fn get_binding(&mut self, name: Ustr) -> Option<&Binding> {
        self.bindings.iter().find(|b| b.name() == name)
    }

    pub fn pretty_print(&self) -> io::Result<()> {
        let mut p = PrettyPrint {
            builder: ptree::TreeBuilder::new(self.name.standard_full_name()),
        };

        for binding in &self.bindings {
            p.print_binding(binding);
        }

        let tree = p.builder.build();
        ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
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
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    id: ModuleId,
    source_id: SourceId,
    name: QualifiedName,
    bindings: Vec<Binding>,
    binding_infos: SlotMap<BindingId, BindingInfo>,
    is_root: bool,
}

impl From<Module> for ResolvedModule {
    fn from(module: Module) -> Self {
        Self {
            id: ModuleId::null(),
            source_id: module.source_id,
            name: module.name,
            bindings: module.bindings,
            binding_infos: SlotMap::with_key(),
            is_root: module.is_root,
        }
    }
}

impl ResolvedModule {
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

struct PrettyPrint {
    builder: ptree::TreeBuilder,
}

impl PrettyPrint {
    fn print_ast(&mut self, ast: &Ast) {
        match ast {
            Ast::Binding(fun) => self.print_binding(fun),
            Ast::Fun(fun) => self.print_fun(fun),
            Ast::Ret(ret) => self.print_ret(ret),
            Ast::Lit(lit) => self.print_lit(lit),
        }
    }

    fn print_binding(&mut self, binding: &Binding) {
        match &binding.kind {
            BindingKind::Fun { name, fun } => {
                self.builder.begin_child(format!(
                    "fn {} {}",
                    name,
                    Self::print_ty(fun.ty.as_ref())
                ));

                self.print_fun_body(fun);
            }
        }
    }

    fn print_fun(&mut self, fun: &Fun) {
        self.builder
            .begin_child(format!("fn {}", Self::print_ty(fun.ty.as_ref())));

        self.print_fun_body(fun);
    }

    fn print_ret(&mut self, ret: &Ret) -> () {
        self.builder.begin_child("return".to_string());

        if let Some(value) = ret.value.as_ref() {
            self.print_ast(value);
        }

        self.builder.end_child();
    }

    fn print_lit(&mut self, lit: &Lit) {
        match lit.kind {
            LitKind::Int(value) => {
                self.builder
                    .add_empty_child(format!("int: {value} {}", Self::print_ty(lit.ty.as_ref())));
            }
        }
    }

    fn print_ty(ty: Option<&Ty>) -> String {
        format!(
            "(ty: {})",
            match ty {
                Some(ty) => ty.to_string(),
                None => "?".to_string(),
            }
        )
    }

    fn print_fun_body(&mut self, fun: &Fun) {
        self.builder.begin_child("body".to_string());

        self.print_ast(&fun.body);

        self.builder.end_child();
        self.builder.end_child();
    }
}
