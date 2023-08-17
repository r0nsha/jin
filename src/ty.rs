mod printer;

use derive_more::{From, Into};
use enum_as_inner::EnumAsInner;

use crate::db::{Database, TyId};
use crate::span::Span;
use crate::ty::printer::TypePrinter;

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Ty {
    Int(IntTy, Span),
    Function(FunctionTy),
    // TODO: when we implement tuples, Unit should become Tuple([])
    Unit(Span),
    Never(Span),
    Infer(InferTy, Span),
}

impl Ty {
    pub fn default_int(span: Span) -> Self {
        Self::Int(IntTy::Int, span)
    }

    pub fn occurs_check(&self, var: TyVar) -> Result<(), Self> {
        match self {
            Self::Function(fun) => fun.ret.occurs_check(var).map_err(|_| self.clone()),
            Self::Infer(InferTy::TyVar(v), _) => {
                if *v == var {
                    Err(self.clone())
                } else {
                    Ok(())
                }
            }
            _ => Ok(()),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Int(_, span)
            | Self::Function(FunctionTy { span, .. })
            | Self::Unit(span)
            | Self::Never(span)
            | Self::Infer(_, span) => *span,
        }
    }

    pub fn display<'db>(&'db self, db: &'db Database) -> TypePrinter<'db> {
        TypePrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Database) -> String {
        self.display(db).to_string()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, Into)]
pub struct TyVar(u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, From, Into)]
pub struct IntVar(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntVarValue {
    Int(IntTy, Span),
}

impl From<IntVarValue> for Ty {
    fn from(value: IntVarValue) -> Self {
        match value {
            IntVarValue::Int(ty, span) => Self::Int(ty, span),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IntTy {
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionTy {
    pub ret: Box<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferTy {
    TyVar(TyVar),
    IntVar(IntVar),
}

pub trait Typed {
    fn ty(&self) -> TyId;
    fn ty_mut(&mut self) -> &mut TyId;

    fn set_ty(&mut self, ty: TyId) {
        *self.ty_mut() = ty;
    }
}
