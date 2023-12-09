use std::fmt::{Display, Formatter, Result};

use crate::{
    db::Db,
    sym,
    ty::{FloatTy, InferTy, IntTy, TyKind, UintTy},
};

pub struct TyPrinter<'db> {
    db: &'db Db,
    ty: &'db TyKind,
}

impl Display for TyPrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.fmt_type(f, self.ty)
    }
}

impl<'db> TyPrinter<'db> {
    pub fn new(db: &'db Db, ty: &'db TyKind) -> Self {
        Self { db, ty }
    }

    fn fmt_type(&self, f: &mut Formatter, ty: &TyKind) -> Result {
        match ty {
            TyKind::Fn(fun) => {
                f.write_str("fn")?;

                f.write_str("(")?;
                for (i, param) in fun.params.iter().enumerate() {
                    if let Some(name) = param.name {
                        f.write_str(name.as_str())?;
                        f.write_str(" ")?;
                    }

                    self.fmt_type(f, &param.ty)?;

                    if i != fun.params.len() - 1 {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(") -> ")?;

                self.fmt_type(f, &fun.ret)
            }
            TyKind::Struct(sid) => f.write_str(self.db.structs[*sid].name.as_str()),
            TyKind::RawPtr(pointee) => {
                f.write_str("*")?;
                self.fmt_type(f, pointee)
            }
            TyKind::Int(ity) => f.write_str(match ity {
                IntTy::I8 => sym::I8,
                IntTy::I16 => sym::I16,
                IntTy::I32 => sym::I32,
                IntTy::I64 => sym::I64,
                IntTy::Int => sym::INT,
            }),
            TyKind::Uint(uty) => f.write_str(match uty {
                UintTy::U8 => sym::U8,
                UintTy::U16 => sym::U16,
                UintTy::U32 => sym::U32,
                UintTy::U64 => sym::U64,
                UintTy::Uint => sym::UINT,
            }),
            TyKind::Float(fty) => f.write_str(match fty {
                FloatTy::F32 => sym::F32,
                FloatTy::F64 => sym::F64,
            }),
            TyKind::Str => f.write_str(sym::STR),
            TyKind::Bool => f.write_str(sym::BOOL),
            TyKind::Unit => f.write_str("{}"),
            TyKind::Never => f.write_str(sym::NEVER),
            TyKind::Param(p) => f.write_str(p.name.as_str()),

            #[cfg(debug_assertions)]
            TyKind::Infer(InferTy::Ty(v)) => write!(f, "?{}", v.0),
            #[cfg(not(debug_assertions))]
            TyKind::Infer(InferTy::Ty(_)) => f.write_str("{unknown}"),

            #[cfg(debug_assertions)]
            TyKind::Infer(InferTy::Int(v)) => write!(f, "i?{}", v.0),
            #[cfg(not(debug_assertions))]
            TyKind::Infer(InferTy::Int(_)) => f.write_str("(untyped int)"),

            #[cfg(debug_assertions)]
            TyKind::Infer(InferTy::Float(v)) => write!(f, "f?{}", v.0),
            #[cfg(not(debug_assertions))]
            TyKind::Infer(InferTy::Float(_)) => f.write_str("(untyped float)"),

            #[cfg(debug_assertions)]
            TyKind::Type(ty) => write!(f, "type({})", ty.display(self.db)),
            #[cfg(not(debug_assertions))]
            TyKind::Type(_) => f.write_str("(type)"),

            #[cfg(debug_assertions)]
            TyKind::Module(id) => write!(f, "module({id})"),
            #[cfg(not(debug_assertions))]
            TyKind::Module(_) => f.write_str("(module)"),

            TyKind::Unknown => f.write_str("(unknown)"),
        }
    }
}
