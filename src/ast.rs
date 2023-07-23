use std::io;

use ustr::{ustr, Ustr};

use crate::{span::Span, ty::Ty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Fun(Fun),
    Ret(Ret),
    Lit(Lit),
}

impl Ast {
    // TODO: remove these functions after we implement the parser
    pub fn fun(name: &str, body: Self, span: Span, ty: Option<Ty>) -> Self {
        Self::Fun(Fun {
            name: ustr(name),
            body: Box::new(body),
            span,
            ty,
        })
    }

    pub fn ret(value: Option<Ast>, span: Span, ty: Option<Ty>) -> Self {
        Self::Ret(Ret {
            value: Box::new(value),
            span,
            ty,
        })
    }

    pub fn int(value: usize, span: Span, ty: Option<Ty>) -> Self {
        Self::Lit(Lit {
            kind: LitKind::Int(value),
            span,
            ty,
        })
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Fun(fun) => fun.span,
            Self::Ret(ret) => ret.span,
            Self::Lit(lit) => lit.span,
        }
    }

    pub fn ty(&self) -> Option<&Ty> {
        match self {
            Self::Fun(fun) => fun.ty.as_ref(),
            Self::Ret(ret) => ret.ty.as_ref(),
            Self::Lit(lit) => lit.ty.as_ref(),
        }
    }

    pub fn ty_mut(&mut self) -> &mut Option<Ty> {
        match self {
            Self::Fun(fun) => &mut fun.ty,
            Self::Ret(ret) => &mut ret.ty,
            Self::Lit(lit) => &mut lit.ty,
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
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $name {
            $(pub $element: $ty),*,
            pub span: Span,
            pub ty: Option<Ty>,
        }
    };
}

define_ast!(Fun, name: Ustr, body: Box<Ast>);
define_ast!(Ret, value: Box<Option<Ast>>);
define_ast!(Lit, kind: LitKind);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
}

pub trait AstVisitor<T> {
    fn visit(&mut self, ast: &Ast) -> T {
        match ast {
            Ast::Fun(fun) => self.visit_fun(fun),
            Ast::Ret(ret) => self.visit_ret(ret),
            Ast::Lit(lit) => self.visit_lit(lit),
        }
    }

    fn visit_fun(&mut self, fun: &Fun) -> T;
    fn visit_ret(&mut self, ret: &Ret) -> T;
    fn visit_lit(&mut self, lit: &Lit) -> T;
}

struct PrettyPrint {
    builder: ptree::TreeBuilder,
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
}

impl AstVisitor<()> for PrettyPrint {
    fn visit_fun(&mut self, fun: &Fun) {
        self.builder.begin_child(format!(
            "fn {} {}",
            fun.name,
            Self::print_ty(fun.ty.as_ref())
        ));

        self.builder.begin_child("body".to_string());

        self.visit(&fun.body);

        self.builder.end_child();
        self.builder.end_child();
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
