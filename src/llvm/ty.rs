use inkwell::{
    types::{BasicType, BasicTypeEnum, FunctionType, IntType, StructType},
    AddressSpace,
};

use crate::{
    db::TyId,
    llvm::generate::Generator,
    ty::{FunctionTy, IntTy, Ty},
};

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn unit_ty(&self) -> StructType<'cx> {
        self.context.struct_type(&[], false)
    }

    pub fn never_ty(&self) -> StructType<'cx> {
        self.unit_ty()
    }
}

pub trait LlvmType<'db, 'cx, T> {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> T;
}

impl<'db, 'cx> LlvmType<'db, 'cx, BasicTypeEnum<'cx>> for TyId {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        cx.db[*self].llvm_type(cx)
    }
}

impl<'db, 'cx> LlvmType<'db, 'cx, BasicTypeEnum<'cx>> for Ty {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> BasicTypeEnum<'cx> {
        match self {
            Self::Int(inner, _) => inner.llvm_type(cx).into(),
            Self::Function(inner) => inner.llvm_type(cx).ptr_type(AddressSpace::default()).into(),
            Self::Unit(_) => cx.unit_ty().into(),
            Self::Never(_) => cx.never_ty().into(),
            Self::Infer(_, _) => panic!("unexpected infer type {}", self.display(cx.db)),
        }
    }
}

impl<'db, 'cx> LlvmType<'db, 'cx, IntType<'cx>> for IntTy {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> IntType<'cx> {
        match self {
            Self::Int => cx.isize_ty,
        }
    }
}

impl<'db, 'cx> LlvmType<'db, 'cx, FunctionType<'cx>> for FunctionTy {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> FunctionType<'cx> {
        self.ret.llvm_type(cx).fn_type(&[], false)
    }
}
