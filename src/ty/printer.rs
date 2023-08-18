use std::fmt::{Display, Formatter, Result};

use crate::{
    db::Database,
    ty::{InferTy, IntTy, Ty},
};

pub struct TypePrinter<'db> {
    _db: &'db Database,
    ty: &'db Ty,
}

impl Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        TypePrinter::fmt_ty(f, self.ty)
    }
}

impl<'db> TypePrinter<'db> {
    pub fn new(db: &'db Database, ty: &'db Ty) -> Self {
        Self { _db: db, ty }
    }

    fn fmt_ty(f: &mut Formatter, ty: &Ty) -> Result {
        match ty {
            Ty::Function(fun) => {
                f.write_str("fn() ")?;
                Self::fmt_ty(f, &fun.ret)
            }
            Ty::Int(int, _) => match int {
                IntTy::Int => f.write_str("int"),
            },
            Ty::Bool(_) => f.write_str("bool"),
            Ty::Unit(_) => f.write_str("()"),
            Ty::Never(_) => f.write_str("!"),
            Ty::Infer(InferTy::TyVar(v), _) => write!(f, "?{}", v.0),
            Ty::Infer(InferTy::IntVar(_), _) => f.write_str("{int}"),
        }
    }
}
