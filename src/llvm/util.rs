use inkwell::{
    basic_block::BasicBlock,
    types::BasicTypeEnum,
    values::{BasicValueEnum, StructValue},
};

use crate::llvm::generate::{FunctionState, Generator};

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn current_block(&self) -> BasicBlock<'cx> {
        self.bx.get_insert_block().unwrap()
    }

    pub fn start_block(&self, state: &mut FunctionState<'cx>, bb: BasicBlock<'cx>) {
        state.current_block = bb;
        self.bx.position_at_end(bb);
    }

    pub fn unit_value(&self) -> StructValue<'cx> {
        self.context.const_struct(&[], false)
    }

    pub fn current_block_is_terminating(&self) -> bool {
        self.current_block().get_terminator().is_some()
    }

    pub fn build_unreachable(&self) {
        if !self.current_block_is_terminating() {
            self.bx.build_unreachable();
        }
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

    // #[allow(unused)]
    // fn gep_at_index(
    //     &mut self,
    //     load: BasicValueEnum<'cx>,
    //     index: u32,
    //     name: &str,
    // ) -> BasicValueEnum<'cx> {
    //     let instruction = load.as_instruction_value().unwrap();
    //     assert_eq!(instruction.get_opcode(), InstructionOpcode::Load);
    //
    //     let pointer = instruction.get_operand(0).unwrap().left().unwrap().into_pointer_value();
    //
    //     let gep = self.builder.build_struct_gep(pointer, index, name).unwrap();
    //     self.builder.build_load(gep, name)
    // }

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
