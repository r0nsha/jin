use std::fmt;

use ena::unify::{EqUnifyValue, UnifyKey};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Var(TyVar),
    Int(IntTy),
    Fun(FunTy),
    Never,
}

impl Ty {
    pub fn var(inner: u32) -> Self {
        Ty::Var(TyVar(inner))
    }

    pub fn int() -> Self {
        Ty::Int(IntTy::Int)
    }

    pub fn fun(return_ty: Ty) -> Self {
        Ty::Fun(FunTy {
            ret: Box::new(return_ty),
        })
    }

    pub fn as_fun(&self) -> &FunTy {
        if let Self::Fun(f) = self {
            f
        } else {
            panic!("expected Fun, got {self}")
        }
    }

    pub fn occurs_check(&self, var: TyVar) -> Result<(), Self> {
        match self {
            Ty::Fun(fun) => {
                // fun.arg.occurs_check(var).map_err(|_| self.clone())?;
                fun.ret.occurs_check(var).map_err(|_| self.clone())
            }
            Ty::Var(v) => {
                if *v == var {
                    Err(Ty::Var(*v))
                } else {
                    Ok(())
                }
            }
            Ty::Int(_) | Ty::Never => Ok(()),
        }
    }
}

impl EqUnifyValue for Ty {}

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

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Fun(fun) => {
                f.write_str("fn() ")?;
                fun.ret.fmt(f)
            }
            Ty::Var(var) => write!(f, "@{}", var.0),
            Ty::Int(int) => match int {
                IntTy::Int => f.write_str("int"),
            },
            Ty::Never => f.write_str("never"),
        }
    }
}
