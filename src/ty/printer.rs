use std::fmt::{Display, Formatter, Result};

use crate::db::Database;
use crate::ty::*;

pub(crate) struct TypePrinter<'db> {
    db: &'db Database,
    ty: &'db Ty,
}

impl Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.ty.kind {
            TyKind::Function(fun) => {
                f.write_str("fn() ")?;
                fun.ret.fmt(f)
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

impl TypePrinter<'_> {
    pub(crate) fn new(db: &Database, ty: &Ty) -> Self {
        Self { db, ty }
    }
}
