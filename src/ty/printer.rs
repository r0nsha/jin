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
            TyKind::Fn(fun) => {
                f.write_str("fn")?;

                if !fun.ty_params.is_empty() {
                    f.write_str("[")?;
                    for (i, param) in fun.ty_params.iter().enumerate() {
                        f.write_str(param.name.as_str())?;

                        if i != fun.ty_params.len() - 1 {
                            f.write_str(", ")?;
                        }
                    }
                    f.write_str("]")?;
                }

                f.write_str("(")?;
                for (i, param) in fun.params.iter().enumerate() {
                    if let Some(name) = param.name {
                        f.write_str(name.as_str())?;
                        f.write_str(" ")?;
                    }

                    Self::fmt_type(f, &param.ty)?;

                    if i != fun.params.len() - 1 {
                        f.write_str(", ")?;
                    }
                }

                f.write_str(") ")?;
                Self::fmt_type(f, &fun.ret)
            }
            TyKind::Int(int) => match int {
                IntTy::Int => f.write_str("int"),
            },
            TyKind::Bool => f.write_str("bool"),
            TyKind::Unit => f.write_str("()"),
            TyKind::Never => f.write_str("!"),
            TyKind::Infer(InferTy::TyVar(v)) => write!(f, "?{}", v.0),
            TyKind::Infer(InferTy::IntVar(..)) => f.write_str("{int}"),
            TyKind::Unknown => f.write_str("{unknown}"),
        }
    }
}
