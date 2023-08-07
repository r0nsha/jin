use std::collections::HashSet;

use crate::db::FunctionId;

use super::*;

pub(crate) struct FunctionBuilder {
    f: Function,
    current_block: BlockId,
}

impl FunctionBuilder {
    pub(crate) fn new(id: FunctionId) -> Self {
        let mut s = Self {
            f: Function::new(id),
            current_block: BlockId(0),
        };

        s.create_block();

        s
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

    #[inline]
    pub(crate) fn current_block(&self) -> &Block {
        self.block(self.current_block)
    }

    #[inline]
    pub(crate) fn current_block_mut(&mut self) -> &mut Block {
        self.block_mut(self.current_block)
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

    pub(crate) fn create_block(&mut self) -> BlockId {
        self.f.cfg.blocks.push_with_id(|id| Block::new(id))
    }

    pub(crate) fn create_edge(&mut self, source: BlockId, target: BlockId) {
        self.f.cfg.blocks[target].predecessors.push(source);
        self.f.cfg.blocks[source].successors.push(target);
    }

    pub(crate) fn finish(self) -> Result<Function, String> {
        if self.f.blocks().is_empty() {
            return Err("Function has 0 blocks".to_string());
        }

        let mut is_terminating = false;

        {
            let blocks = self.f.blocks();

            for (i, blk) in blocks.iter().enumerate() {
                if i > 0 && blk.predecessors.is_empty() {
                    return Err(format!(
                        "Non-starting block &{i} is unreachable (has no predecessors)"
                    ));
                }

                let blk_is_terminating = blk
                    .instructions
                    .iter()
                    .any(|inst| matches!(inst, Instruction::Return(_)));

                if i < blocks.len() - 1 {
                    if blk.successors.is_empty() && !blk_is_terminating {
                        return Err(format!(
                            "Intermediate block &{i} leads nowhere (has no successors and isn't terminating)"
                        ));
                    }
                }

                is_terminating = is_terminating || blk_is_terminating;
            }
        }

        if !is_terminating {
            return Err("Function never terminates".to_string());
        }

        Ok(self.f)
    }
}
