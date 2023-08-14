use std::fmt::{Display, Formatter, Result};

use crate::db::Database;
use crate::ty::{IntTy, Ty, TyKind};

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
        match &ty.kind {
            TyKind::Function(fun) => {
                f.write_str("fn() ")?;
                Self::fmt_ty(f, &fun.ret)
            }
            TyKind::Var(var) => write!(f, "${}", var.0),
            TyKind::Int(int) => match int {
                IntTy::Int => f.write_str("int"),
            },
            TyKind::Never => f.write_str("!"),
            TyKind::Unit => f.write_str("()"),
        }
    }
}
