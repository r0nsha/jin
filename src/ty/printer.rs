use std::fmt::{Display, Formatter, Result};

use crate::{
    db::Db,
    ty::{InferTy, IntTy, TyKind},
};

pub struct TyPrinter<'db> {
    _db: &'db Db,
    ty: &'db TyKind,
}

impl Display for TyPrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        TyPrinter::fmt_type(f, self.ty)
    }
}

impl<'db> TyPrinter<'db> {
    pub fn new(db: &'db Db, ty: &'db TyKind) -> Self {
        Self { _db: db, ty }
    }

    fn fmt_type(f: &mut Formatter, ty: &TyKind) -> Result {
        match ty {
            TyKind::Function(fun) => {
                f.write_str("fn() ")?;
                Self::fmt_type(f, &fun.ret)
            }
            TyKind::Int(int) => match int {
                IntTy::Int => f.write_str("int"),
            },
            TyKind::Bool => f.write_str("bool"),
            TyKind::Unit => f.write_str("()"),
            TyKind::Never => f.write_str("!"),
            TyKind::Infer(InferTy::TyVar(v)) => write!(f, "?{}", v.0),
            TyKind::Infer(InferTy::IntVar(_)) => f.write_str("{int}"),
            TyKind::Unknown => f.write_str("{unknown}"),
        }
    }
}
