use crate::db::FunctionId;

use super::*;

pub(crate) struct FunctionBuilder {
    f: Function,
}

impl FunctionBuilder {
    pub(crate) fn new(id: FunctionId) -> Self {
        Self {
            f: Function::new(id),
        }
    }

    pub(crate) fn add_register(&mut self, reg: Register) -> RegisterId {
        self.f.registers.push(reg)
    }

    pub(crate) fn add_parameter(&mut self, reg_id: RegisterId) -> usize {
        self.f.parameters.push(reg_id);
        self.f.parameters.len() - 1
    }

    pub(crate) fn create_block(&mut self) -> &Block {
        self.f.cfg.blocks.push(Block::new());
        self.f.cfg.blocks.as_slice().last().unwrap()
    }

    pub(crate) fn finish(self) -> Result<Function, String> {
        // TODO: validate that the function is built correctly
        Ok(self.f)
    }
}
