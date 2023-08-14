mod printer;

use enum_as_inner::EnumAsInner;

use crate::db::Database;
use crate::span::Span;
use crate::ty::printer::TypePrinter;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

impl Ty {
    pub fn var(var: TyVar, span: Span) -> Self {
        Self { kind: TyKind::Var(var), span }
    }

    pub fn int(span: Span) -> Self {
        Self { kind: TyKind::Int(IntTy::Int), span }
    }

    pub fn fun(ret: Self, span: Span) -> Self {
        Self { kind: TyKind::Function(FunctionTy { ret: Box::new(ret) }), span }
    }

    pub fn never(span: Span) -> Self {
        Self { kind: TyKind::Never, span }
    }

    pub fn unit(span: Span) -> Self {
        Self { kind: TyKind::Unit, span }
    }

    pub fn occurs_check(&self, var: TyVar) -> Result<(), Self> {
        match &self.kind {
            TyKind::Function(fun) => {
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

    pub fn display<'db>(&'db self, db: &'db Database) -> TypePrinter<'db> {
        TypePrinter::new(db, self)
    }

    pub fn to_string(&self, db: &Database) -> String {
        self.display(db).to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner)]
pub enum TyKind {
    Var(TyVar),
    Int(IntTy),
    Function(FunctionTy),
    Unit, // TODO: when we implement tuples, this should just be an empty tuple
    Never,
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
}
