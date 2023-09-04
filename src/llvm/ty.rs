use inkwell::{
    types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    AddressSpace,
};

use crate::{
    llvm::{generate::Generator, inkwell_ext::ContextExt},
    ty::{FnTy, IntTy, TyKind},
};

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn unit_ty(&self) -> StructType<'cx> {
        self.context.struct_type(&[], false)
    }

    pub fn never_ty(&self) -> StructType<'cx> {
        self.unit_ty()
    }
}

pub trait LlvmTy<'db, 'cx, T> {
    fn llvm_ty(&self, cx: &Generator<'db, 'cx>) -> T;
}

impl<'db, 'cx> LlvmTy<'db, 'cx, BasicTypeEnum<'cx>> for TyKind {
    fn llvm_ty(&self, cx: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        match self {
            Self::Int(inner) => inner.llvm_ty(cx).into(),
            Self::Fn(_) => cx.context.ptr_type(AddressSpace::default()).into(),
            Self::Bool => cx.context.bool_type().into(),
            Self::Unit => cx.unit_ty().into(),
            Self::Never => cx.never_ty().into(),
            _ => panic!("unexpected type {self:?}"),
        }
    }
}

impl<'db, 'cx> LlvmTy<'db, 'cx, IntType<'cx>> for IntTy {
    fn llvm_ty(&self, cx: &Generator<'db, 'cx>) -> IntType<'cx> {
        match self {
            Self::I8 => cx.context.i8_type(),
            Self::I16 => cx.context.i16_type(),
            Self::I32 => cx.context.i32_type(),
            Self::I64 => cx.context.i64_type(),
            Self::Int => cx.isize_ty,
        }
    }
}

impl<'db, 'cx> LlvmTy<'db, 'cx, FunctionType<'cx>> for FnTy {
    fn llvm_ty(&self, cx: &Generator<'db, 'cx>) -> FunctionType<'cx> {
        let param_tys: Vec<_> = self.params.iter().map(|p| p.ty.llvm_ty(cx).into()).collect();
        self.ret.llvm_ty(cx).fn_type(&param_tys, false)
    }
}
