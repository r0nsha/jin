use std::fmt::{Display, Formatter, Result};

use crate::{
    db::Db,
    ty::{GeneralizedTy, InferTy, IntTy, TyKind},
};

pub struct TyPrinter<'db> {
    _db: &'db Db,
    ty: &'db TyKind,
}

impl Display for TyPrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        TyPrinter::fmt_ty(f, self.ty)
    }
}

impl<'db> TyPrinter<'db> {
    pub fn new(db: &'db Db, ty: &'db TyKind) -> Self {
        Self { _db: db, ty }
    }

    fn fmt_ty(f: &mut Formatter, ty: &TyKind) -> Result {
        match ty {
            TyKind::Function(fun) => {
                f.write_str("fn(")?;

                for (i, param) in fun.params.iter().enumerate() {
                    f.write_str(param.name.map_or("_", |s| s.as_str()))?;
                    f.write_str(" ")?;
                    Self::fmt_ty(f, &param.ty)?;

                    if i != fun.params.len() - 1 {
                        f.write_str(", ")?;
                    }
                }

                f.write_str(") ")?;
                Self::fmt_ty(f, &fun.ret)
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

pub struct GeneralizedTyPrinter<'db> {
    db: &'db Db,
    ty: &'db GeneralizedTy,
}

impl Display for GeneralizedTyPrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.fmt_ty(f, self.ty)
    }
}

impl<'db> GeneralizedTyPrinter<'db> {
    pub fn new(db: &'db Db, ty: &'db GeneralizedTy) -> Self {
        Self { db, ty }
    }

    fn fmt_ty(&self, f: &mut Formatter, ty: &GeneralizedTy) -> Result {
        match ty {
            GeneralizedTy::Mono(ty) => TyPrinter::new(self.db, &ty).fmt(f),
            GeneralizedTy::Poly(pty) => {
                // TODO: print polymorphic params
                f.write_str("poly!!!")?;
                TyPrinter::new(self.db, &pty.ty).fmt(f)
            }
        }
    }
}
