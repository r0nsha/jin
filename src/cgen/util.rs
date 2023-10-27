use ustr::Ustr;

use crate::cgen::generate::{FnState, Generator};

impl<'db, 'a> Generator<'db, 'a> {
    // #[inline]
    // pub fn append_block(&self, state: &FnState<'db, 'cx>, name: &str) -> BasicBlock<'cx> {
    //     self.context.append_basic_block(state.function_value, name)
    // }

    // #[inline]
    // pub fn current_block(&self) -> BasicBlock<'cx> {
    //     self.bx.get_insert_block().unwrap()
    // }

    // #[inline]
    // pub fn start_block(&self, state: &mut FnState<'db, 'cx>, bb: BasicBlock<'cx>) {
    //     state.current_block = bb;
    //     self.bx.position_at_end(bb);
    // }

    // #[inline]
    // pub fn unit_value(&self) -> StructValue<'cx> {
    //     self.layout.unit_ty.const_named_struct(&[])
    // }

    // #[inline]
    // pub fn bool_value(&self, value: bool) -> IntValue<'cx> {
    //     self.context.bool_type().const_int(u64::from(value), false)
    // }

    // pub fn build_static_str(&mut self, value: Ustr) -> PointerValue<'cx> {
    //     let static_str =
    //         self.module.add_global(self.context.i8_type().array_type(value.len() as u32), None, "");
    //     static_str.set_initializer(&self.context.const_string(value.as_bytes(), false));
    //     let ptr = static_str.as_pointer_value();
    //     self.static_strs.insert(value, ptr);
    //     ptr
    // }

    // pub fn build_static_str_slice(
    //     &mut self,
    //     value: impl Into<Ustr>,
    //     name: &str,
    // ) -> PointerValue<'cx> {
    //     let value = value.into();
    //
    //     if let Some(slice) = self.static_str_slices.get(&value) {
    //         *slice
    //     } else {
    //         let static_str = self.build_static_str(value);
    //
    //         let len = self.layout.int_ty.const_int(value.len() as u64, false);
    //         let slice = self.const_slice(static_str, len);
    //         let static_slice = self.module.add_global(slice.get_type(), None, name);
    //         static_slice.set_initializer(&slice);
    //
    //         self.static_str_slices.insert(value, static_slice.as_pointer_value());
    //
    //         static_slice.as_pointer_value()
    //     }
    // }

    // #[inline]
    // pub fn const_slice(&self, ptr: PointerValue<'cx>, len: IntValue<'cx>) -> StructValue<'cx> {
    //     self.const_struct(&[ptr.as_basic_value_enum(), len.as_basic_value_enum()])
    // }

    // #[inline]
    // pub fn const_struct(&self, values: &[BasicValueEnum<'cx>]) -> StructValue<'cx> {
    //     self.context.const_struct(values, false)
    // }

    // pub fn current_block_is_terminating(&self) -> bool {
    //     self.current_block().get_terminator().is_some()
    // }

    // pub fn build_unreachable(&self) {
    //     if !self.current_block_is_terminating() {
    //         self.bx.build_unreachable();
    //     }
    // }

    // pub fn undef_value(ty: BasicTypeEnum<'cx>) -> BasicValueEnum<'cx> {
    //     match ty {
    //         BasicTypeEnum::ArrayType(array) => array.get_undef().into(),
    //         BasicTypeEnum::FloatType(float) => float.get_undef().into(),
    //         BasicTypeEnum::IntType(int) => int.get_undef().into(),
    //         BasicTypeEnum::PointerType(pointer) => pointer.get_undef().into(),
    //         BasicTypeEnum::StructType(tuple) => tuple.get_undef().into(),
    //         BasicTypeEnum::VectorType(vector) => vector.get_undef().into(),
    //     }
    // }

    // pub fn build_stack_alloc(
    //     &self,
    //     state: &FnState<'db, 'cx>,
    //     ty: BasicTypeEnum<'cx>,
    //     name: &str,
    // ) -> PointerValue<'cx> {
    //     self.bx.position_at_end(state.prologue_block);
    //     let ptr = self.bx.build_alloca(ty, name);
    //     self.bx.position_at_end(state.current_block);
    //     ptr
    // }
}
