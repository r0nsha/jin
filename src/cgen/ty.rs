use std::fmt;

use pretty::RcDoc;

use crate::{
    cgen::generate::Generator,
    ty::{FnTy, IntTy, TyKind, UintTy},
};

pub trait CTy<'db, 'a>
where
    Self: fmt::Debug,
{
    fn cty(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a>;

    fn cpointee(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a> {
        panic!("{self:?} has no pointee");
    }
}

impl<'db, 'a> CTy<'db, 'a> for TyKind {
    fn cty(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a> {
        match self {
            Self::Int(ity) => ity.cty(cx).into(),
            Self::Uint(uty) => uty.cty(cx).into(),
            Self::Fn(_) | Self::Str | Self::RawPtr(_) => {
                todo!()
                // cx.context.ptr_type(AddressSpace::default()).into()
            }
            Self::Bool => todo!(), //cx.context.bool_type().into(),
            Self::Unit | Self::Never => todo!(), // cx.layout.unit_ty.into(),
            _ => panic!("unexpected type {self:?}"),
        }
    }

    fn cpointee(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a> {
        match self {
            Self::Str => todo!(),             // cx.layout.str_ty.into(),
            Self::RawPtr(pointee) => todo!(), //pointee.cty(cx),
            _ => panic!("unexpected type: {self:?}"),
        }
    }
}

impl<'db, 'a> CTy<'db, 'a> for IntTy {
    fn cty(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a> {
        todo!()
        // match self {
        //     Self::I8 => cx.context.i8_type(),
        //     Self::I16 => cx.context.i16_type(),
        //     Self::I32 => cx.context.i32_type(),
        //     Self::I64 => cx.context.i64_type(),
        //     Self::Int => cx.layout.int_ty,
        // }
    }
}

impl<'db, 'a> CTy<'db, 'a> for UintTy {
    fn cty(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a> {
        todo!()
        // match self {
        //     Self::U8 => cx.context.i8_type(),
        //     Self::U16 => cx.context.i16_type(),
        //     Self::U32 => cx.context.i32_type(),
        //     Self::U64 => cx.context.i64_type(),
        //     Self::Uint => cx.layout.int_ty,
        // }
    }
}

impl<'db, 'a> CTy<'db, 'a> for FnTy {
    fn cty(&self, cx: &Generator<'db, 'a>) -> RcDoc<'a> {
        todo!("c fn types");
        // let param_tys: Vec<_> = self.params.iter().map(|p| p.ty.cty(cx).into()).collect();
        // self.ret.cty(cx).fn_type(&param_tys, false)
    }
}
