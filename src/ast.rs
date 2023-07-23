use ustr::{ustr, Ustr};

use crate::{span::Span, ty::Ty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Fun(Fun),
    Lit(Lit),
}

impl Ast {
    // TODO: remove these functions...
    pub fn fun(name: &str, body: Self, span: Span, ty: Option<Ty>) -> Self {
        Self::Fun(Fun {
            name: ustr(name),
            body: Box::new(body),
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
            Self::Lit(lit) => lit.span,
        }
    }

    pub fn ty(&self) -> Option<&Ty> {
        match self {
            Self::Fun(fun) => fun.ty.as_ref(),
            Self::Lit(lit) => lit.ty.as_ref(),
        }
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

define_ast!(Fun, name: Ustr, body: Box<Ast>,);
define_ast!(Lit, kind: LitKind,);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
}
