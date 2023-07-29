use std::fmt;

use ena::unify::{EqUnifyValue, UnifyKey};
use enum_as_inner::EnumAsInner;

use crate::span::Span;

slotmap::new_key_type! {
    pub struct TyId;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

impl Ty {
    pub fn var(var: TyVar, span: Span) -> Self {
        Self {
            kind: TyKind::Var(var),
            span,
        }
    }

    pub fn int(span: Span) -> Self {
        Self {
            kind: TyKind::Int(IntTy::Int),
            span,
        }
    }

    pub fn fun(ret: Ty, span: Span) -> Self {
        Self {
            kind: TyKind::Fun(FunTy { ret: Box::new(ret) }),
            span,
        }
    }

    pub fn never(span: Span) -> Self {
        Self {
            kind: TyKind::Never,
            span,
        }
    }

    pub fn unit(span: Span) -> Self {
        Self {
            kind: TyKind::Unit,
            span,
        }
    }

    pub fn occurs_check(&self, var: TyVar) -> Result<(), Self> {
        match &self.kind {
            TyKind::Fun(fun) => {
                // fun.arg.occurs_check(var).map_err(|_| self.clone())?;
                fun.ret.occurs_check(var).map_err(|_| self.clone())
            }
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
            TyKind::Var(var) => write!(f, "@{}", var.0),
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
pub enum TyKind {
    Var(TyVar),
    Int(IntTy),
    Fun(FunTy),
    Never,
    Unit, // TODO: when implementing tuples, this should just be an empty tuple
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

pub trait Typed {
    fn ty(&self) -> &Ty;
}
