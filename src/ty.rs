mod printer;

use enum_as_inner::EnumAsInner;

use crate::db::Database;
use crate::span::Span;
use crate::ty::printer::TypePrinter;

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Ty {
    Var(TyVar, Span),
    Int(IntTy, Span),
    Function(FunctionTy),
    // TODO: when we implement tuples, Unit should become Tuple([])
    Unit(Span),
    Never(Span),
}

impl Ty {
    pub fn occurs_check(&self, var: TyVar) -> Result<(), Self> {
        match self {
            Self::Function(fun) => {
                fun.ret.occurs_check(var).map_err(|_| self.clone())
            }
            Self::Var(v, _) => {
                if *v == var {
                    Err(self.clone())
                } else {
                    Ok(())
                }
            }
            Self::Int(..) | Self::Unit(_) | Self::Never(_) => Ok(()),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Var(_, span)
            | Self::Int(_, span)
            | Self::Function(FunctionTy { span, .. })
            | Self::Unit(span)
            | Self::Never(span) => *span,
        }
    }

    pub fn display<'db>(&'db self, db: &'db Database) -> TypePrinter<'db> {
        TypePrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Database) -> String {
        self.display(db).to_string()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TyVar(u32);

impl From<TyVar> for u32 {
    fn from(value: TyVar) -> Self {
        value.0
    }
}

impl From<u32> for TyVar {
    fn from(value: u32) -> Self {
        Self(value)
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
