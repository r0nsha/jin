use std::io;

use enum_as_inner::EnumAsInner;
use ustr::Ustr;

use crate::{span::Span, ty::Ty};

#[derive(Debug, Clone)]
pub struct Module {
    pub bindings: Vec<Binding>,
}

impl Module {
    pub fn pretty_print(&self) -> io::Result<()> {
        let mut p = PrettyPrint {
            builder: ptree::TreeBuilder::new("ast".to_string()),
        };

        for binding in &self.bindings {
            p.visit_binding(binding);
        }

        let tree = p.builder.build();
        ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
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
    pub fn span(&self) -> Span {
        match self {
            Self::Binding(binding) => binding.span,
            Self::Fun(fun) => fun.span,
            Self::Ret(ret) => ret.span,
            Self::Lit(lit) => lit.span,
        }
    }

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

    pub fn pretty_print(&self) -> io::Result<()> {
        let mut p = PrettyPrint {
            builder: ptree::TreeBuilder::new("ast".to_string()),
        };

        p.visit(self);

        let tree = p.builder.build();
        ptree::print_tree_with(&tree, &ptree::PrintConfig::default())
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

pub trait AstVisitor<T> {
    fn visit(&mut self, ast: &Ast) -> T {
        match ast {
            Ast::Binding(fun) => self.visit_binding(fun),
            Ast::Fun(fun) => self.visit_fun(fun),
            Ast::Ret(ret) => self.visit_ret(ret),
            Ast::Lit(lit) => self.visit_lit(lit),
        }
    }

    fn visit_binding(&mut self, binding: &Binding) -> T;
    fn visit_fun(&mut self, fun: &Fun) -> T;
    fn visit_ret(&mut self, ret: &Ret) -> T;
    fn visit_lit(&mut self, lit: &Lit) -> T;
}

struct PrettyPrint {
    builder: ptree::TreeBuilder,
}

impl AstVisitor<()> for PrettyPrint {
    fn visit_binding(&mut self, binding: &Binding) {
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

    fn visit_fun(&mut self, fun: &Fun) {
        self.builder
            .begin_child(format!("fn {}", Self::print_ty(fun.ty.as_ref())));

        self.print_fun_body(fun);
    }

    fn visit_ret(&mut self, ret: &Ret) -> () {
        self.builder.begin_child("return".to_string());

        if let Some(value) = ret.value.as_ref() {
            self.visit(value);
        }

        self.builder.end_child();
    }

    fn visit_lit(&mut self, lit: &Lit) {
        match lit.kind {
            LitKind::Int(value) => {
                self.builder
                    .add_empty_child(format!("int: {value} {}", Self::print_ty(lit.ty.as_ref())));
            }
        }
    }
}

impl PrettyPrint {
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

        self.visit(&fun.body);

        self.builder.end_child();
        self.builder.end_child();
    }
}
