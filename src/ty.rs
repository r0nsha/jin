use ena::unify::{EqUnifyValue, UnifyKey};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    kind: TyKind,
    span: Span,
}

impl Ty {
    pub fn var(inner: u32, span: Span) -> Self {
        Self {
            kind: TyKind::Var(TyVar(inner)),
            span,
        }
    }

    pub fn int(span: Span) -> Self {
        Self {
            kind: TyKind::Int(IntTy::Int),
            span,
        }
    }

    pub fn fun(return_ty: Ty, span: Span) -> Self {
        Self {
            kind: TyKind::Fun(FunTy {
                return_ty: Box::new(return_ty),
            }),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Var(TyVar),
    Int(IntTy),
    Fun(FunTy),
}

impl EqUnifyValue for TyKind {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TyVar(u32);

impl UnifyKey for TyVar {
    type Value = Option<TyKind>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntTy {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunTy {
    return_ty: Box<Ty>,
}
