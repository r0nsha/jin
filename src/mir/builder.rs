use std::collections::HashSet;

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

    #[inline]
    pub(crate) fn block(&self, id: BlockId) -> &Block {
        &self.f.cfg.blocks[id]
    }

    #[inline]
    pub(crate) fn block_mut(&mut self, id: BlockId) -> &mut Block {
        &mut self.f.cfg.blocks[id]
    }

    #[inline]
    pub(crate) fn start_block(&self) -> &Block {
        self.block(BlockId(0))
    }

    #[inline]
    pub(crate) fn start_block_mut(&mut self) -> &mut Block {
        self.block_mut(BlockId(0))
    }

    pub(crate) fn reachable_blocks(&self) -> HashSet<BlockId> {
        let mut reachable = HashSet::new();

        for blk in self.f.cfg.blocks.iter() {
            if !blk.predecessors.is_empty() {
                reachable.insert(blk.id);
            }
        }

        reachable
    }

    pub(crate) fn create_register(&mut self, reg: Register) -> RegisterId {
        self.f.registers.push(reg)
    }

    pub(crate) fn create_parameter(&mut self, reg: Register) -> usize {
        let reg_id = self.create_register(reg);
        self.f.parameters.push(reg_id);
        self.f.parameters.len() - 1
    }

    pub(crate) fn create_block(&mut self) -> &Block {
        self.f.cfg.blocks.push_with_id(|id| Block::new(id));
        self.f.cfg.blocks.as_slice().last().unwrap()
    }

    pub(crate) fn create_edge(&mut self, source: BlockId, target: BlockId) {
        self.f.cfg.blocks[target].predecessors.push(source);
        self.f.cfg.blocks[source].successors.push(target);
    }

    pub(crate) fn finish(self) -> Result<Function, String> {
        // TODO: validation:
        //      - blocks with index > 0 have predecessors
        //      - blocks with index < len - 1 have successors
        Ok(self.f)
    }
}
