use std::fmt::{Display, Formatter, Result};

use crate::db::Database;
use crate::ty::*;

pub(crate) struct TypePrinter<'db> {
    db: &'db Database,
    ty: &'db Ty,
}

impl Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.fmt_ty(f, &self.ty)
    }
}

impl<'db> TypePrinter<'db> {
    pub(crate) fn new(db: &'db Database, ty: &'db Ty) -> Self {
        Self { db, ty }
    }

    fn fmt_ty(&self, f: &mut Formatter, ty: &Ty) -> Result {
        match &ty.kind {
            TyKind::Function(fun) => {
                f.write_str("fn() ")?;
                self.fmt_ty(f, &fun.ret)
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
