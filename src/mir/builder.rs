use std::collections::HashSet;

use anyhow::{bail, Result};

use crate::{
    ast::BinOp,
    db::SymbolId,
    mir::{
        Bin, Block, BlockId, BoolLit, Br, BrIf, Call, Function, FunctionParam, Inst, IntLit, Load,
        Phi, PhiValue, Return, UnitLit, Unreachable, Value, ValueId,
    },
    span::Span,
    ty::Type,
};

pub struct FunctionBuilder {
    f: Function,
    current_block: BlockId,
}

impl FunctionBuilder {
    pub fn new(id: SymbolId) -> Self {
        Self { f: Function::new(id), current_block: BlockId::first() }
    }

    #[inline]
    #[allow(unused)]
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
    pub fn create_value(&mut self, ty: Type) -> ValueId {
        self.f.values.push(Value { ty })
    }

    #[inline]
    pub fn create_param(&mut self, id: SymbolId) {
        self.f.params.push(FunctionParam { id });
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

    pub fn build_int_lit(&mut self, ty: Type, lit: usize, span: Span) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::IntLit(IntLit { value, lit, span }));
        value
    }

    pub fn build_bool_lit(&mut self, ty: Type, lit: bool, span: Span) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::BoolLit(BoolLit { value, lit, span }));
        value
    }

    pub fn build_unit_lit(&mut self, ty: Type, span: Span) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::UnitLit(UnitLit { value, span }));
        value
    }

    pub fn build_return(&mut self, value: ValueId, span: Span) {
        self.current_block_mut().add_inst(Inst::Return(Return { value, span }));
    }

    pub fn build_unreachable(&mut self, ty: Type, span: Span) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::Unreachable(Unreachable { value, span }));
        value
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

    pub fn build_phi(&mut self, ty: Type, phi_values: Box<[PhiValue]>, span: Span) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::Phi(Phi { value, phi_values, span }));
        value
    }

    pub fn build_call(
        &mut self,
        ty: Type,
        callee: ValueId,
        args: Vec<ValueId>,
        span: Span,
    ) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::Call(Call { value, callee, args, span }));
        value
    }

    pub fn build_load(&mut self, ty: Type, id: SymbolId, span: Span) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::Load(Load { value, id, span }));
        value
    }

    pub fn build_bin(
        &mut self,
        ty: Type,
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
        span: Span,
    ) -> ValueId {
        let value = self.create_value(ty);
        self.current_block_mut().add_inst(Inst::Bin(Bin { value, op, lhs, rhs, span }));
        value
    }

    pub fn finish(self) -> Result<Function> {
        if self.f.blocks().is_empty() {
            bail!("Function has 0 blocks");
        }

        let mut is_terminating = false;

        {
            let blocks = self.f.blocks();

            for (i, blk) in blocks.iter().enumerate() {
                if i > 0 && blk.predecessors.is_empty() {
                    bail!("Non-starting block @{i} is unreachable (has no predecessors)");
                }

                let blk_is_terminating = blk.is_terminating();

                if i < blocks.len() - 1 && blk.successors.is_empty() && !blk_is_terminating {
                    bail!("Intermediate block &{i} leads nowhere (has no successors and isn't terminating)");
                }

                is_terminating = is_terminating || blk_is_terminating;
            }
        }

        if !is_terminating {
            bail!("Function never terminates");
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
