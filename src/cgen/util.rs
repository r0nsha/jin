use pretty::RcDoc;
use ustr::Ustr;

use crate::cgen::generate::{FnState, Generator};

impl<'db, 'a> Generator<'db, 'a> {
    // pub fn append_block(&self, state: &FnState<'db, 'cx>, name: &str) -> BasicBlock<'cx> {
    //     self.context.append_basic_block(state.function_value, name)
    // }

    // pub fn current_block(&self) -> BasicBlock<'cx> {
    //     self.bx.get_insert_block().unwrap()
    // }

    // pub fn start_block(&self, state: &mut FnState<'db, 'cx>, bb: BasicBlock<'cx>) {
    //     state.current_block = bb;
    //     self.bx.position_at_end(bb);
    // }

    pub fn unit_value(&self) -> RcDoc<'a> {
        RcDoc::text("{0}")
    }

    pub fn bool_value(&self, value: bool) -> RcDoc<'a> {
        RcDoc::text(if value { "true" } else { "false" })
    }

    pub fn str_value(&self, value: &str) -> RcDoc<'a> {
        RcDoc::text("{")
            .append(RcDoc::text(".data = ").append(self.str_lit(value)))
            .append(RcDoc::text(format!(".len = {}", value.len())))
            .append(RcDoc::text("}"))
    }

    pub fn str_lit(&self, value: &str) -> RcDoc<'a> {
        RcDoc::text(format!("\"{value}\""))
    }

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
