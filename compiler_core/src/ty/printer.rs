use core::fmt::{Display, Formatter, Result, Write};

use ustr::Ustr;

use crate::{
    db::Db,
    middle::{CallConv, Mutability},
    sym,
    ty::{FloatTy, FnTyFlags, FnTyParam, InferTy, IntTy, Ty, TyKind, UintTy},
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
                write!(f, "{}", fun.display(self.db, None))
            }
            TyKind::Adt(adt_id, targs) => {
                f.write_str(self.db[*adt_id].name.as_str())?;

                if !targs.is_empty() {
                    f.write_char('[')?;

                    let last_idx = targs.len() - 1;
                    for (idx, ty) in targs.iter().enumerate() {
                        write!(f, "{}", ty.display(self.db))?;

                        if idx != last_idx {
                            f.write_str(", ")?;
                        }
                    }

                    f.write_char(']')?;
                }

                Ok(())
            }
            TyKind::Slice(elem) => {
                f.write_char('[')?;
                self.fmt_type(f, elem)?;
                f.write_char(']')
            }
            TyKind::Ref(inner, mutability) => {
                f.write_str(match mutability {
                    Mutability::Imm => "&",
                    Mutability::Mut => "&mut ",
                })?;
                self.fmt_type(f, inner)
            }
            TyKind::RawPtr(pointee) => {
                f.write_str("*")?;
                self.fmt_type(f, pointee)
            }
            TyKind::Int(ity) => f.write_str(match ity {
                IntTy::I8 => sym::ty::I8,
                IntTy::I16 => sym::ty::I16,
                IntTy::I32 => sym::ty::I32,
                IntTy::I64 => sym::ty::I64,
                IntTy::Int => sym::ty::INT,
            }),
            TyKind::Uint(uty) => f.write_str(match uty {
                UintTy::U8 => sym::ty::U8,
                UintTy::U16 => sym::ty::U16,
                UintTy::U32 => sym::ty::U32,
                UintTy::U64 => sym::ty::U64,
                UintTy::Uint => sym::ty::UINT,
            }),
            TyKind::Float(fty) => f.write_str(match fty {
                FloatTy::F32 => sym::ty::F32,
                FloatTy::F64 => sym::ty::F64,
            }),
            TyKind::Str => f.write_str(sym::ty::STR),
            TyKind::Char => f.write_str(sym::ty::CHAR),
            TyKind::Bool => f.write_str(sym::ty::BOOL),
            TyKind::Unit => f.write_str("()"),
            TyKind::Never => f.write_str(sym::ty::NEVER),
            TyKind::Param(p) => f.write_str(p.name.as_str()),

            #[cfg(debug_assertions)]
            TyKind::Infer(InferTy::Ty(v)) => write!(f, "(ty:{})", v.0),
            #[cfg(not(debug_assertions))]
            TyKind::Infer(InferTy::Ty(_)) => f.write_str("_"),

            #[cfg(debug_assertions)]
            TyKind::Infer(InferTy::Int(v)) => write!(f, "(int:{})", v.0),
            #[cfg(not(debug_assertions))]
            TyKind::Infer(InferTy::Int(_)) => f.write_str("(untyped int)"),

            #[cfg(debug_assertions)]
            TyKind::Infer(InferTy::Float(v)) => write!(f, "(float:{})", v.0),
            #[cfg(not(debug_assertions))]
            TyKind::Infer(InferTy::Float(_)) => f.write_str("(untyped float)"),

            #[cfg(debug_assertions)]
            TyKind::Type(ty) => write!(f, "(type:{})", ty.display(self.db)),
            #[cfg(not(debug_assertions))]
            TyKind::Type(_) => f.write_str("(type)"),

            #[cfg(debug_assertions)]
            TyKind::Module(id) => write!(f, "(module:{id})"),
            #[cfg(not(debug_assertions))]
            TyKind::Module(_) => f.write_str("(module)"),

            TyKind::Unknown => f.write_str("(unknown)"),
        }
    }
}

pub struct FnTyPrinter<'db> {
    pub db: &'db Db,
    pub name: Option<Ustr>,
    pub params: &'db [FnTyParam],
    pub ret: Option<Ty>,
    pub callconv: CallConv,
    pub flags: FnTyFlags,
}

impl Display for FnTyPrinter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str("fn")?;

        if self.flags.contains(FnTyFlags::EXTERN) {
            write!(f, " extern \"{}\"", self.callconv)?;
        }

        if let Some(name) = self.name {
            write!(f, " {name}")?;
        }

        f.write_str("(")?;

        if !self.params.is_empty() {
            let last_idx = self.params.len() - 1;

            for (idx, param) in self.params.iter().enumerate() {
                if let Some(name) = param.name {
                    write!(f, "{name}: ")?;
                }

                write!(f, "{}", param.ty.display(self.db))?;

                if idx != last_idx {
                    f.write_str(", ")?;
                }
            }
        }

        if self.flags.contains(FnTyFlags::C_VARIADIC) {
            f.write_str("..")?;
        }

        f.write_str(")")?;

        if let Some(ret) = &self.ret {
            write!(f, " {}", ret.display(self.db))?;
        }

        Ok(())
    }
}
