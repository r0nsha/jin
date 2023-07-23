use ustr::{ustr, Ustr};

use crate::{span::Span, ty::Ty};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ast {
    Fun(Fun),
    Lit(Lit),
}

impl Ast {
    pub fn fun(name: &str, body: Self, span: Span) -> Self {
        Self::Fun(Fun {
            name: ustr(name),
            body: Box::new(body),
            span,
            ty: None,
        })
    }

    pub fn int(value: usize, span: Span) -> Self {
        Self::Lit(Lit {
            kind: LitKind::Int(value),
            span,
            ty: None,
        })
    }
}

macro_rules! define_ast {
    ($name: ident, $($element: ident: $ty: ty),* $(,)?) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        pub struct $name {
            $($element: $ty),*,
            span: Span,
            ty: Option<Ty>,
        }
    };
}

define_ast!(Fun, name: Ustr, body: Box<Ast>,);
define_ast!(Lit, kind: LitKind,);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
}
