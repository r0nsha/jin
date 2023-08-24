use std::fmt::{Display, Formatter, Result};

use crate::{
    db::Db,
    ty::{InferType, IntType, TypeKind},
};

pub struct TypePrinter<'db> {
    _db: &'db Db,
    ty: &'db TypeKind,
}

impl Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        TypePrinter::fmt_type(f, self.ty)
    }
}

impl<'db> TypePrinter<'db> {
    pub fn new(db: &'db Db, ty: &'db TypeKind) -> Self {
        Self { _db: db, ty }
    }

    fn fmt_type(f: &mut Formatter, ty: &TypeKind) -> Result {
        match ty {
            TypeKind::Function(fun) => {
                f.write_str("fn() ")?;
                Self::fmt_type(f, &fun.ret)
            }
            TypeKind::Int(int) => match int {
                IntType::Int => f.write_str("int"),
            },
            TypeKind::Bool => f.write_str("bool"),
            TypeKind::Unit => f.write_str("()"),
            TypeKind::Never => f.write_str("!"),
            TypeKind::Infer(InferType::TypeVar(v)) => write!(f, "?{}", v.0),
            TypeKind::Infer(InferType::IntVar(_)) => f.write_str("{int}"),
            TypeKind::Unknown => f.write_str("{unknown}"),
        }
    }
}
