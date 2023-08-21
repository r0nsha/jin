use inkwell::{
    types::{self as ll, BasicType},
    AddressSpace,
};

use crate::{
    db::TypeId,
    llvm::generate::Generator,
    ty::{FunctionType, IntType, Type},
};

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn unit_ty(&self) -> ll::StructType<'cx> {
        self.context.struct_type(&[], false)
    }

    pub fn never_ty(&self) -> ll::StructType<'cx> {
        self.unit_ty()
    }
}

pub trait LlvmType<'db, 'cx, T> {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> T;
}

impl<'db, 'cx> LlvmType<'db, 'cx, ll::BasicTypeEnum<'cx>> for TypeId {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> ll::BasicTypeEnum<'cx> {
        cx.db[*self].llvm_type(cx)
    }
}

impl<'db, 'cx> LlvmType<'db, 'cx, ll::BasicTypeEnum<'cx>> for Type {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> ll::BasicTypeEnum<'cx> {
        match self {
            Self::Int(inner, _) => inner.llvm_type(cx).into(),
            Self::Function(inner) => inner.llvm_type(cx).ptr_type(AddressSpace::default()).into(),
            Self::Bool(_) => cx.context.bool_type().into(),
            Self::Unit(_) => cx.unit_ty().into(),
            Self::Never(_) => cx.never_ty().into(),
            Self::Infer(_, _) => panic!("unexpected infer type {}", self.display(cx.db)),
        }
    }
}

impl<'db, 'cx> LlvmType<'db, 'cx, ll::IntType<'cx>> for IntType {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> ll::IntType<'cx> {
        match self {
            Self::Int => cx.isize_ty,
        }
    }
}

impl<'db, 'cx> LlvmType<'db, 'cx, ll::FunctionType<'cx>> for FunctionType {
    fn llvm_type(&self, cx: &Generator<'db, 'cx>) -> ll::FunctionType<'cx> {
        let param_tys: Vec<_> = self.params.iter().map(|p| p.ty.llvm_type(cx).into()).collect();
        self.ret.llvm_type(cx).fn_type(&param_tys, false)
    }
}
