use std::fmt;

use ena::unify::{EqUnifyValue, UnifyKey};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    pub kind: TyKind,
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
                ret: Box::new(return_ty),
            }),
            span,
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn occurs_check(&self, var: TyVar) -> Result<(), Self> {
        match &self.kind {
            TyKind::Fun(fun) => {
                // fun.arg.occurs_check(var).map_err(|_| self.clone())?;
                fun.ret.occurs_check(var).map_err(|_| self.clone())
            }
            TyKind::Var(v) => {
                if *v == var {
                    Err(Ty {
                        kind: TyKind::Var(*v),
                        span: self.span,
                    })
                } else {
                    Ok(())
                }
            }
            TyKind::Int(_) => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TyKind {
    Var(TyVar),
    Int(IntTy),
    Fun(FunTy),
}

impl EqUnifyValue for Ty {}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TyVar(u32);

impl UnifyKey for TyVar {
    type Value = Option<Ty>;

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
    pub ret: Box<Ty>,
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TyKind::Var(var) => write!(f, "@{}", var.0),
            TyKind::Int(int) => match int {
                IntTy::Int => f.write_str("int"),
            },
            TyKind::Fun(fun) => {
                f.write_str("fn() ")?;
                fun.ret.fmt(f)
            }
        }
    }
}
