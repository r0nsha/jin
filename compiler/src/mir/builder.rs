use crate::{
    mir::{BlockId, Body, Inst, ValueId},
    span::Span,
};

pub struct InstBuilder<'a> {
    body: &'a mut Body,
    block: BlockId,
}

impl<'a> InstBuilder<'a> {
    pub fn new(body: &'a mut Body, block: BlockId) -> Self {
        Self { body, block }
    }

    pub fn br(&mut self, target: BlockId) -> &mut Self {
        self.body.create_edge(self.block, target);
        self.inst(Inst::Br { target });
        self
    }

    pub fn brif(
        &mut self,
        cond: ValueId,
        then: BlockId,
        otherwise: Option<BlockId>,
    ) -> &mut Self {
        self.body.create_edge(self.block, then);

        if let Some(otherwise) = otherwise {
            self.body.create_edge(self.block, otherwise);
        }

        self.inst(Inst::BrIf { cond, then, otherwise });
        self
    }

    pub fn switch(&mut self, cond: ValueId, blocks: Vec<BlockId>) -> &mut Self {
        for &target in &blocks {
            self.body.create_edge(self.block, target);
        }

        self.inst(Inst::Switch { cond, blocks });
        self
    }

    pub fn free(&mut self, value: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::Free { value, span });
        self
    }

    fn inst(&mut self, inst: Inst) {
        self.body.block_mut(self.block).push_inst(inst);
    }
}
