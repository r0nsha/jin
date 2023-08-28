mod printer;
pub mod tcx;

use std::ops::Deref;

use derive_more::{From, Into};
use enum_as_inner::EnumAsInner;
use internment::Intern;
use ustr::Ustr;

use crate::{db::Db, ty::printer::TyPrinter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ty(Intern<TyKind>);

impl Ty {
    pub fn new(kind: TyKind) -> Self {
        Self(Intern::new(kind))
    }

    pub fn from_ref(kind: &TyKind) -> Self {
        Self(Intern::from_ref(kind))
    }

    pub fn occurs_check(self, var: TyVar) -> Result<(), Self> {
        match self.as_ref() {
            TyKind::Function(fun) => fun.ret.occurs_check(var).map_err(|_| self),
            TyKind::Infer(InferTy::TyVar(v)) => {
                if *v == var {
                    Err(self)
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }
}

impl Deref for Ty {
    type Target = TyKind;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<TyKind> for Ty {
    fn as_ref(&self) -> &TyKind {
        &self.0
    }
}

impl From<TyKind> for Ty {
    fn from(value: TyKind) -> Self {
        Self::new(value)
    }
}

impl From<&TyKind> for Ty {
    fn from(value: &TyKind) -> Self {
        Self::from_ref(value)
    }
}

impl From<&TyKind> for TyKind {
    fn from(value: &TyKind) -> Self {
        value.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum TyKind {
    Function(FunctionTy),
    Int(IntTy),
    Bool,
    // TODO: when we implement tuples, Unit should become Tuple([])
    Unit,
    Never,
    Infer(InferTy),
    Unknown,
}

impl TyKind {
    pub const DEFAULT_INT: Self = Self::Int(IntTy::Int);

    pub fn display<'db>(&'db self, db: &'db Db) -> TyPrinter<'db> {
        TyPrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Db) -> String {
        self.display(db).to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct TyVar(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, From, Into)]
pub struct IntVar(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntVarValue {
    Int(IntTy),
}

impl From<IntVarValue> for TyKind {
    fn from(value: IntVarValue) -> Self {
        match value {
            IntVarValue::Int(ty) => Self::Int(ty),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntTy {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTy {
    pub ret: Ty,
    pub params: Vec<FunctionTyParam>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionTyParam {
    // TODO: This shouldn't be optional...
    pub name: Option<Ustr>,
    pub ty: Ty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InferTy {
    TyVar(TyVar),
    IntVar(IntVar),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GeneralizedTy {
    Mono(Ty),
    Poly(PolyTy),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PolyTy {
    pub ty: Ty,
    pub vars: Vec<(Ustr, TyVar)>,
}

pub trait Typed {
    fn ty(&self) -> Ty;
    fn ty_mut(&mut self) -> &mut Ty;

    fn set_ty(&mut self, ty: Ty) {
        *self.ty_mut() = ty;
    }
}
