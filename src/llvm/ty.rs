use inkwell::{
    types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    AddressSpace,
};

use crate::{
    llvm::{generate::Generator, inkwell_ext::ContextExt},
    ty::{FnTy, IntTy, TyKind, UintTy},
};

impl<'db, 'cx> Generator<'db, 'cx> {
    #[inline]
    pub fn str_ty(&self) -> StructType<'cx> {
        self.context.struct_type(
            &[self.context.ptr_type(AddressSpace::default()).into(), self.int_ty.into()],
            false,
        )
    }

    #[inline]
    pub fn unit_ty(&self) -> StructType<'cx> {
        self.unit_ty
    }

    #[inline]
    pub fn never_ty(&self) -> StructType<'cx> {
        self.unit_ty
    }
}

pub trait LlvmTy<'db, 'cx, T> {
    fn llty(&self, cx: &Generator<'db, 'cx>) -> T;
    fn llpointee(&self, cx: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx>;
}

impl<'db, 'cx> LlvmTy<'db, 'cx, BasicTypeEnum<'cx>> for TyKind {
    fn llty(&self, cx: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        match self {
            Self::Int(ity) => ity.llty(cx).into(),
            Self::Uint(uty) => uty.llty(cx).into(),
            Self::Fn(_) | Self::Str | Self::RawPtr(_) => {
                cx.context.ptr_type(AddressSpace::default()).into()
            }
            Self::Bool => cx.context.bool_type().into(),
            Self::Unit => cx.unit_ty().into(),
            Self::Never => cx.never_ty().into(),
            _ => panic!("unexpected type {self:?}"),
        }
    }

    fn llpointee(&self, cx: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        match self {
            Self::Str => cx.str_ty().into(),
            Self::RawPtr(pointee) => pointee.llty(cx),
            _ => panic!("unexpected type: {self:?}"),
        }
    }
}

impl<'db, 'cx> LlvmTy<'db, 'cx, IntType<'cx>> for IntTy {
    fn llty(&self, cx: &Generator<'db, 'cx>) -> IntType<'cx> {
        match self {
            Self::I8 => cx.context.i8_type(),
            Self::I16 => cx.context.i16_type(),
            Self::I32 => cx.context.i32_type(),
            Self::I64 => cx.context.i64_type(),
            Self::Int => cx.int_ty,
        }
    }

    fn llpointee(&self, _: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        panic!("{self:?} has no pointee");
    }
}

impl<'db, 'cx> LlvmTy<'db, 'cx, IntType<'cx>> for UintTy {
    fn llty(&self, cx: &Generator<'db, 'cx>) -> IntType<'cx> {
        match self {
            Self::U8 => cx.context.i8_type(),
            Self::U16 => cx.context.i16_type(),
            Self::U32 => cx.context.i32_type(),
            Self::U64 => cx.context.i64_type(),
            Self::Uint => cx.int_ty,
        }
    }

    fn llpointee(&self, _: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        panic!("{self:?} has no pointee");
    }
}

impl<'db, 'cx> LlvmTy<'db, 'cx, FunctionType<'cx>> for FnTy {
    fn llty(&self, cx: &Generator<'db, 'cx>) -> FunctionType<'cx> {
        let param_tys: Vec<_> = self.params.iter().map(|p| p.ty.llty(cx).into()).collect();
        self.ret.llty(cx).fn_type(&param_tys, false)
    }

    fn llpointee(&self, _: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        panic!("{self:?} has no pointee");
    }
}
