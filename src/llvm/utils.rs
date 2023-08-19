use inkwell::{
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicValueEnum, StructValue},
};

use crate::llvm::generate::{FunctionState, Generator};

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn current_block(&self) -> BasicBlock<'cx> {
        self.builder.get_insert_block().unwrap()
    }

    pub fn start_block(&self, state: &mut FunctionState<'cx>, bb: BasicBlock<'cx>) {
        state.current_block = bb;
        self.builder.position_at_end(bb);
    }

    pub fn unit_value(&self) -> StructValue<'cx> {
        self.context.const_struct(&[], false)
    }

    pub fn current_block_is_terminating(&self) -> bool {
        self.current_block().get_terminator().is_some()
    }

    pub fn undef_value(typ: BasicTypeEnum<'cx>) -> BasicValueEnum<'cx> {
        match typ {
            BasicTypeEnum::ArrayType(array) => array.get_undef().into(),
            BasicTypeEnum::FloatType(float) => float.get_undef().into(),
            BasicTypeEnum::IntType(int) => int.get_undef().into(),
            BasicTypeEnum::PointerType(pointer) => pointer.get_undef().into(),
            BasicTypeEnum::StructType(tuple) => tuple.get_undef().into(),
            BasicTypeEnum::VectorType(vector) => vector.get_undef().into(),
        }
    }

    #[allow(unused)]
    pub fn print_current_state(&self, state: &FunctionState<'cx>) {
        let current_block = self.current_block();
        println!(
            "function: {}\n\tblock: {}\n\tterminated: {}",
            state.function_value.get_name().to_str().unwrap(),
            current_block.get_name().to_str().unwrap(),
            current_block.get_terminator().is_some()
        );
    }
}
