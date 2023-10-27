use std::fmt;

use ustr::Ustr;

use crate::{
    cgen::generate::Generator,
    ty::{FnTy, IntTy, TyKind, UintTy},
};

pub trait CTy<'db>
where
    Self: fmt::Debug,
{
    fn cty(&self, cx: &Generator<'db>) -> Ustr;

    fn cpointee(&self, cx: &Generator<'db>) -> Ustr {
        panic!("{self:?} has no pointee");
    }
}

impl<'db> CTy<'db> for TyKind {
    fn cty(&self, cx: &Generator<'db>) -> Ustr {
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

    fn cpointee(&self, cx: &Generator<'db>) -> Ustr {
        match self {
            Self::Str => todo!(),             // cx.layout.str_ty.into(),
            Self::RawPtr(pointee) => todo!(), //pointee.cty(cx),
            _ => panic!("unexpected type: {self:?}"),
        }
    }
}

impl<'db> CTy<'db> for IntTy {
    fn cty(&self, cx: &Generator<'db>) -> Ustr {
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

impl<'db> CTy<'db> for UintTy {
    fn cty(&self, cx: &Generator<'db>) -> Ustr {
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

impl<'db> CTy<'db> for FnTy {
    fn cty(&self, cx: &Generator<'db>) -> Ustr {
        todo!("c fn types");
        // let param_tys: Vec<_> = self.params.iter().map(|p| p.ty.cty(cx).into()).collect();
        // self.ret.cty(cx).fn_type(&param_tys, false)
    }
}
