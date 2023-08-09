use std::collections::HashSet;

use crate::{db::FunctionId, span::Span};

use super::*;

pub(crate) struct FunctionBuilder {
    f: Function,
    current_block: BlockId,
}

impl FunctionBuilder {
    pub(crate) fn new(id: FunctionId) -> Self {
        Self {
            f: Function::new(id),
            current_block: BlockId(0),
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

    #[inline]
    pub(crate) fn create_register(&mut self, ty: TyId) -> RegisterId {
        self.f.registers.push(Register { ty })
    }

    pub(crate) fn create_parameter(&mut self, ty: TyId) -> usize {
        let reg_id = self.create_register(ty);
        self.f.parameters.push(reg_id);
        self.f.parameters.len() - 1
    }

    #[inline]
    pub(crate) fn create_block(&mut self, name: impl AsRef<str>) -> BlockId {
        self.f.cfg.blocks.push_with_id(|id| Block::new(id, name))
    }

    #[inline]
    pub(crate) fn position_at(&mut self, id: BlockId) {
        self.current_block = id;
    }

    pub(crate) fn create_edge(&mut self, source: BlockId, target: BlockId) {
        self.f.cfg.blocks[target].predecessors.push(source);
        self.f.cfg.blocks[source].successors.push(target);
    }

    pub(crate) fn build_unit_lit(&mut self, reg: RegisterId, span: Span) {
        self.current_block_mut()
            .add_instruction(Instruction::UnitLit(UnitLit {
                register: reg,
                span,
            }));
    }

    pub(crate) fn build_int_lit(&mut self, reg: RegisterId, value: usize, span: Span) {
        self.current_block_mut()
            .add_instruction(Instruction::IntLit(IntLit {
                register: reg,
                value,
                span,
            }));
    }

    pub(crate) fn build_return(&mut self, reg: RegisterId, span: Span) {
        self.current_block_mut()
            .add_instruction(Instruction::Return(Return {
                register: reg,
                span,
            }));
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
                        "Non-starting block @{i} is unreachable (has no predecessors)"
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

impl Block {
    #[inline]
    pub(crate) fn add_instruction(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }
}
