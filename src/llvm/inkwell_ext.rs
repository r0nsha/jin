use std::mem;

use inkwell::{
    context::{AsContextRef, Context},
    types::{AsTypeRef, PointerType},
    AddressSpace,
};
use llvm_sys::prelude::*;

extern "C" {
    pub fn LLVMPointerTypeIsOpaque(Ty: LLVMTypeRef) -> LLVMBool;
    pub fn LLVMPointerTypeInContext(C: LLVMContextRef, A: libc::c_uint) -> LLVMTypeRef;
}

pub trait ContextExt<'ctx> {
    fn ptr_type(&self, address_space: AddressSpace) -> PointerType<'ctx>;
}

impl<'ctx> ContextExt<'ctx> for Context {
    fn ptr_type(&self, address_space: AddressSpace) -> PointerType<'ctx> {
        unsafe {
            PointerType::new(LLVMPointerTypeInContext(
                self.as_ctx_ref(),
                mem::transmute(address_space),
            ))
        }
    }
}

pub trait PointerTypeExt<'ctx> {
    fn is_opaque(&self) -> bool;
}

impl<'ctx> PointerTypeExt<'ctx> for PointerType<'ctx> {
    fn is_opaque(&self) -> bool {
        unsafe { LLVMPointerTypeIsOpaque(self.as_type_ref()) == 1 }
    }
}
