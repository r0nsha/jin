use inkwell::{
    context::Context,
    targets::TargetMachine,
    types::{IntType, StructType},
    AddressSpace,
};

use crate::llvm::inkwell_ext::ContextExt;

pub struct Layout<'cx> {
    pub int_ty: IntType<'cx>,
    pub unit_ty: StructType<'cx>,
    pub str_ty: StructType<'cx>,
}

impl<'cx> Layout<'cx> {
    pub fn new(context: &'cx Context, target_machine: &TargetMachine) -> Self {
        let unit_ty = context.opaque_struct_type("unit");
        unit_ty.set_body(&[], false);

        let int_ty = context.ptr_sized_int_type(&target_machine.get_target_data(), None);

        let str_ty = context.opaque_struct_type("str");
        str_ty.set_body(&[context.ptr_type(AddressSpace::default()).into(), int_ty.into()], false);

        Self { int_ty, unit_ty, str_ty }
    }
}
