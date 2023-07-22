use ena::unify::{EqUnifyValue, UnifyKey};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Var(TyVar),
    Int(IntTy),
    Fun(FunTy),
}

impl EqUnifyValue for Ty {}

impl Ty {
    pub fn var(inner: u32) -> Self {
        Self::Var(TyVar(inner))
    }

    pub fn int() -> Self {
        Self::Int(IntTy::Int)
    }
}

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
    return_ty: Box<Ty>,
}
