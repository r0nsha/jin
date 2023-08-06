use std::fmt;

use ena::unify::{EqUnifyValue, UnifyKey};
use enum_as_inner::EnumAsInner;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Ty {
    pub(crate) kind: TypeKind,
    pub(crate) span: Span,
}

impl Ty {
    pub(crate) fn var(var: TypeVar, span: Span) -> Self {
        Self {
            kind: TypeKind::Var(var),
            span,
        }
    }

    pub(crate) fn int(span: Span) -> Self {
        Self {
            kind: TypeKind::Int(IntType::Int),
            span,
        }
    }

    pub(crate) fn fun(ret: Ty, span: Span) -> Self {
        Self {
            kind: TypeKind::Fun(FunType { ret: Box::new(ret) }),
            span,
        }
    }

    pub(crate) fn never(span: Span) -> Self {
        Self {
            kind: TypeKind::Never,
            span,
        }
    }

    pub(crate) fn unit(span: Span) -> Self {
        Self {
            kind: TypeKind::Unit,
            span,
        }
    }

    pub(crate) fn occurs_check(&self, var: TypeVar) -> Result<(), Self> {
        match &self.kind {
            TypeKind::Fun(fun) => {
                // fun.arg.occurs_check(var).map_err(|_| self.clone())?;
                fun.ret.occurs_check(var).map_err(|_| self.clone())
            }
            TypeKind::Var(v) => {
                if *v == var {
                    Err(self.clone())
                } else {
                    Ok(())
                }
            }
            TypeKind::Int(_) | TypeKind::Unit | TypeKind::Never => Ok(()),
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TypeKind::Fun(fun) => {
                f.write_str("fn() ")?;
                fun.ret.fmt(f)
            }
            TypeKind::Var(var) => write!(f, "${}", var.0),
            TypeKind::Int(int) => match int {
                IntType::Int => f.write_str("int"),
            },
            TypeKind::Never => f.write_str("never"),
            TypeKind::Unit => f.write_str("()"),
        }
    }
}

impl EqUnifyValue for Ty {}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub(crate) enum TypeKind {
    Var(TypeVar),
    Int(IntType),
    Fun(FunType),
    Unit, // TODO: when implementing tuples, this should just be an empty tuple
    Never,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub(crate) struct TypeVar(u32);

impl UnifyKey for TypeVar {
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
pub(crate) enum IntType {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FunType {
    pub(crate) ret: Box<Ty>,
}
