use std::collections::HashSet;

use super::{Block, BlockId, Call, Function, Inst, IntLit, Return, TyId, UnitLit, Value, ValueId};
use crate::{
    ast::BinaryOp,
    db::DefId,
    mir::{Binary, BoolLit, Br, BrIf, LoadGlobal, Phi, PhiValue},
    span::Span,
};

pub struct FunctionBuilder {
    f: Function,
    current_block: BlockId,
}

impl FunctionBuilder {
    pub fn new(id: DefId) -> Self {
        Self { f: Function::new(id), current_block: BlockId::first() }
    }

    #[inline]
    pub fn blocks(&self) -> &[Block] {
        self.f.cfg.blocks.as_slice()
    }

    #[inline]
    pub fn block(&self, id: BlockId) -> &Block {
        &self.f.cfg.blocks[id]
    }

    #[inline]
    pub fn block_mut(&mut self, id: BlockId) -> &mut Block {
        &mut self.f.cfg.blocks[id]
    }

    #[inline]
    pub fn current_block(&self) -> &Block {
        self.block(self.current_block)
    }

    #[inline]
    pub fn current_block_mut(&mut self) -> &mut Block {
        self.block_mut(self.current_block)
    }

    pub fn is_terminating(&self) -> bool {
        return self.blocks().iter().any(Block::is_terminating);
    }

    #[allow(unused)]
    pub fn reachable_blocks(&self) -> HashSet<BlockId> {
        let mut reachable = HashSet::new();

        for blk in self.f.cfg.blocks.iter() {
            if !blk.predecessors.is_empty() {
                reachable.insert(blk.id);
            }
        }

        reachable
    }

    #[inline]
    pub fn create_value(&mut self, ty: TyId) -> ValueId {
        self.f.values.push(Value { ty })
    }

    #[allow(unused)]
    pub fn create_parameter(&mut self, ty: TyId) -> usize {
        let value = self.create_value(ty);
        self.f.parameters.push(value);
        self.f.parameters.len() - 1
    }

    #[inline]
    pub fn create_block(&mut self, name: impl AsRef<str>) -> BlockId {
        self.f.cfg.blocks.push_with_key(|id| Block::new(id, name))
    }

    #[inline]
    pub fn position_at(&mut self, id: BlockId) {
        self.current_block = id;
    }

    pub fn create_edge(&mut self, source: BlockId, target: BlockId) {
        self.f.cfg.blocks[target].predecessors.push(source);
        self.f.cfg.blocks[source].successors.push(target);
    }

    pub fn build_int_lit(&mut self, vid: ValueId, lit: usize, span: Span) {
        self.current_block_mut().add_inst(Inst::IntLit(IntLit { value: vid, lit, span }));
    }

    pub fn build_bool_lit(&mut self, value: ValueId, lit: bool, span: Span) {
        self.current_block_mut().add_inst(Inst::BoolLit(BoolLit { value, lit, span }));
    }

    pub fn build_unit_lit(&mut self, value: ValueId, span: Span) {
        self.current_block_mut().add_inst(Inst::UnitLit(UnitLit { value, span }));
    }

    pub fn build_return(&mut self, value: ValueId, span: Span) {
        self.current_block_mut().add_inst(Inst::Return(Return { value, span }));
    }

    pub fn build_br(&mut self, target: BlockId, span: Span) {
        self.create_edge(self.current_block().id, target);
        self.current_block_mut().add_inst(Inst::Br(Br { target, span }));
    }

    pub fn build_brif(&mut self, cond: ValueId, b1: BlockId, b2: BlockId, span: Span) {
        self.create_edge(self.current_block().id, b1);
        self.create_edge(self.current_block().id, b2);
        self.current_block_mut().add_inst(Inst::BrIf(BrIf { cond, b1, b2, span }));
    }

    pub fn build_phi(&mut self, value: ValueId, phi_values: Box<[PhiValue]>, span: Span) {
        self.current_block_mut().add_inst(Inst::Phi(Phi { value, phi_values, span }));
    }

    pub fn build_call(&mut self, value: ValueId, callee: ValueId, span: Span) {
        self.current_block_mut().add_inst(Inst::Call(Call { value, callee, span }));
    }

    pub fn build_load_global(&mut self, value: ValueId, id: DefId, span: Span) {
        self.current_block_mut().add_inst(Inst::LoadGlobal(LoadGlobal { value, id, span }));
    }

    pub fn build_binary(
        &mut self,
        value: ValueId,
        op: BinaryOp,
        lhs: ValueId,
        rhs: ValueId,
        span: Span,
    ) {
        self.current_block_mut().add_inst(Inst::Binary(Binary { value, op, lhs, rhs, span }));
    }

    pub fn finish(self) -> Result<Function, String> {
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

                let blk_is_terminating = blk.is_terminating();

                if i < blocks.len() - 1 && blk.successors.is_empty() && !blk_is_terminating {
                    return Err(format!(
                        "Intermediate block &{i} leads nowhere (has no successors and isn't terminating)"
                    ));
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
    pub fn add_inst(&mut self, inst: Inst) {
        self.instructions.push(inst);
    }
}
