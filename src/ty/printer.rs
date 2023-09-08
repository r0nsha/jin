use std::fmt::{Display, Formatter, Result};

use crate::{
    db::Db,
    ty::{InferTy, IntTy, TyKind, UintTy},
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

    #[allow(clippy::match_same_arms)]
    fn fmt_type(f: &mut Formatter, ty: &TyKind) -> Result {
        match ty {
            TyKind::Fn(fun) => {
                f.write_str("fn")?;

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
            TyKind::Int(ity) => f.write_str(match ity {
                IntTy::I8 => "i8",
                IntTy::I16 => "i16",
                IntTy::I32 => "i32",
                IntTy::I64 => "i64",
                IntTy::Int => "int",
            }),
            TyKind::Uint(uty) => f.write_str(match uty {
                UintTy::U8 => "u8",
                UintTy::U16 => "u16",
                UintTy::U32 => "u32",
                UintTy::U64 => "u64",
                UintTy::Uint => "uint",
            }),
            TyKind::Bool => f.write_str("bool"),
            TyKind::Unit => f.write_str("()"),
            TyKind::Never => f.write_str("!"),
            TyKind::Param(p) => f.write_str(p.name.as_str()),
            // TyKind::Infer(InferTy::TyVar(_)) => f.write_str("{unknown}"),
            TyKind::Infer(InferTy::TyVar(v)) => write!(f, "?{}", v.0),
            TyKind::Infer(InferTy::IntVar(..)) => f.write_str("{int}"),
            TyKind::Type => f.write_str("{type}"),
            TyKind::Unknown => f.write_str("{unknown}"),
        }
    }
}
