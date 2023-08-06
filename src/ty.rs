use std::fmt;

use ena::unify::{EqUnifyValue, UnifyKey};
use enum_as_inner::EnumAsInner;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Ty {
    pub(crate) kind: TyKind,
    pub(crate) span: Span,
}

impl Ty {
    pub(crate) fn var(var: TyVar, span: Span) -> Self {
        Self {
            kind: TyKind::Var(var),
            span,
        }
    }

    pub(crate) fn int(span: Span) -> Self {
        Self {
            kind: TyKind::Int(IntTy::Int),
            span,
        }
    }

    pub(crate) fn fun(ret: Ty, span: Span) -> Self {
        Self {
            kind: TyKind::Fun(FunTy { ret: Box::new(ret) }),
            span,
        }
    }

    pub(crate) fn never(span: Span) -> Self {
        Self {
            kind: TyKind::Never,
            span,
        }
    }

    pub(crate) fn unit(span: Span) -> Self {
        Self {
            kind: TyKind::Unit,
            span,
        }
    }

    pub(crate) fn occurs_check(&self, var: TyVar) -> Result<(), Self> {
        match &self.kind {
            TyKind::Fun(fun) => fun.ret.occurs_check(var).map_err(|_| self.clone()),
            TyKind::Var(v) => {
                if *v == var {
                    Err(self.clone())
                } else {
                    Ok(())
                }
            }
            TyKind::Int(_) | TyKind::Unit | TyKind::Never => Ok(()),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TyKind::Fun(fun) => {
                f.write_str("fn() ")?;
                fun.ret.fmt(f)
            }
            TyKind::Var(var) => write!(f, "${}", var.0),
            TyKind::Int(int) => match int {
                IntTy::Int => f.write_str("int"),
            },
            TyKind::Never => f.write_str("never"),
            TyKind::Unit => f.write_str("()"),
        }
    }
}

impl EqUnifyValue for Ty {}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub(crate) enum TyKind {
    Var(TyVar),
    Int(IntTy),
    Fun(FunTy),
    Unit, // TODO: when we implement tuples, this should just be an empty tuple
    Never,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TyVar(u32);

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
pub(crate) enum IntTy {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunTy {
    pub(crate) ret: Box<Ty>,
}
