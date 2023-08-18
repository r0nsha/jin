use inkwell::basic_block::BasicBlock;

use crate::llvm::generate::{FunctionState, Generator};

impl<'db, 'cx> Generator<'db, 'cx> {
    pub fn current_block(&self) -> BasicBlock<'cx> {
        self.builder.get_insert_block().unwrap()
    }

    pub fn start_block(&self, state: &mut FunctionState<'cx>, block: BasicBlock<'cx>) {
        state.current_block = block;
        self.builder.position_at_end(block);
    }

    #[allow(unused)]
    pub fn print_current_state(&self, state: &FunctionState<'cx>) {
        let current_block = self.current_block();
        println!(
            "function: {}\n\tblock: {}\n\tterminated: {}",
            state.function.get_name().to_str().unwrap(),
            current_block.get_name().to_str().unwrap(),
            current_block.get_terminator().is_some()
        );
    }
}
