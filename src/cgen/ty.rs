use std::fmt;

use pretty::RcDoc;

use crate::{
    cgen::generate::Generator,
    sym,
    ty::{FnTy, IntTy, TyKind, UintTy},
};

pub trait CTy<'db, 'a>
where
    Self: fmt::Debug,
{
    fn cty(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a>;
}

impl<'db, 'a> CTy<'db, 'a> for TyKind {
    fn cty(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a> {
        match self {
            Self::Int(ity) => ity.cty(cx),
            Self::Uint(uty) => uty.cty(cx),
            Self::Fn(fty) => fty.cty(cx),
            Self::Str => RcDoc::text(sym::STR),
            Self::RawPtr(ty) => ty.cty(cx).append(RcDoc::text("*")),
            Self::Bool => RcDoc::text(sym::BOOL),
            Self::Unit => RcDoc::text(sym::UNIT),
            Self::Never => RcDoc::text(sym::NEVER),
            _ => panic!("unexpected type {self:?}"),
        }
    }
}

impl<'db, 'a> CTy<'db, 'a> for IntTy {
    fn cty(&self, _: &Generator<'db, 'a>) -> RcDoc<'a> {
        RcDoc::text(match self {
            Self::I8 => sym::I8,
            Self::I16 => sym::I16,
            Self::I32 => sym::I32,
            Self::I64 => sym::I64,
            Self::Int => sym::INT,
        })
    }
}

impl<'db, 'a> CTy<'db, 'a> for UintTy {
    fn cty(&self, _: &Generator<'db, 'a>) -> RcDoc<'a> {
        RcDoc::text(match self {
            Self::U8 => sym::U8,
            Self::U16 => sym::U16,
            Self::U32 => sym::U32,
            Self::U64 => sym::U64,
            Self::Uint => sym::UINT,
        })
    }
}

impl<'db, 'a> CTy<'db, 'a> for FnTy {
    fn cty(&self, _: &Generator<'db, 'a>) -> RcDoc<'a> {
        todo!("c fn types");
    }
}
