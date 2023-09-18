use inkwell::{
    context::Context,
    targets::TargetMachine,
    types::{IntType, StructType},
};

pub struct Layout<'cx> {
    pub int_ty: IntType<'cx>,
    pub unit_ty: StructType<'cx>,
}

impl<'cx> Layout<'cx> {
    pub fn new(context: &'cx Context, target_machine: &TargetMachine) -> Self {
        let unit_ty = context.opaque_struct_type("unit");
        unit_ty.set_body(&[], false);

        Self {
            int_ty: context.ptr_sized_int_type(&target_machine.get_target_data(), None),
            unit_ty,
        }
    }
}
