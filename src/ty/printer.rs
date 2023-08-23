use std::fmt::{Display, Formatter, Result};

use crate::{
    db::Db,
    ty::{InferType, IntType, Type},
};

pub struct TypePrinter<'db> {
    _db: &'db Db,
    ty: &'db Type,
}

impl Display for TypePrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        TypePrinter::fmt_type(f, self.ty)
    }
}

impl<'db> TypePrinter<'db> {
    pub fn new(db: &'db Db, ty: &'db Type) -> Self {
        Self { _db: db, ty }
    }

    fn fmt_type(f: &mut Formatter, ty: &Type) -> Result {
        match ty {
            Type::Function(fun) => {
                f.write_str("fn() ")?;
                Self::fmt_type(f, &fun.ret)
            }
            Type::Int(int, _) => match int {
                IntType::Int => f.write_str("int"),
            },
            Type::Bool(_) => f.write_str("bool"),
            Type::Unit(_) => f.write_str("()"),
            Type::Never(_) => f.write_str("!"),
            Type::Infer(InferType::TypeVar(v), _) => write!(f, "?{}", v.0),
            Type::Infer(InferType::IntVar(_), _) => f.write_str("{int}"),
        }
    }
}
