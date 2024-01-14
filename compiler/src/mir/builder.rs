use crate::{
    middle::BinOp,
    mir::{BlockId, Body, Inst, ValueId},
    span::Span,
};

pub struct InstBuilder<'a> {
    body: &'a mut Body,
    block: BlockId,
}

#[allow(unused)]
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

    pub fn binary(
        &mut self,
        value: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        op: BinOp,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::Binary { value, lhs, rhs, op, span });
        self
    }

    pub fn call(
        &mut self,
        value: ValueId,
        callee: ValueId,
        args: Vec<ValueId>,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::Call { value, callee, args, span });
        self
    }

    pub fn ret(&mut self, value: ValueId) -> &mut Self {
        self.inst(Inst::Return { value });
        self
    }

    pub fn store(&mut self, value: ValueId, target: ValueId) -> &mut Self {
        self.inst(Inst::Store { value, target });
        self
    }

    pub fn incref(&mut self, value: ValueId) -> &mut Self {
        self.inst(Inst::IncRef { value });
        self
    }

    pub fn decref(&mut self, value: ValueId) -> &mut Self {
        self.inst(Inst::DecRef { value });
        self
    }

    pub fn stackalloc(&mut self, value: ValueId, init: ValueId) -> &mut Self {
        self.inst(Inst::StackAlloc { value, init: Some(init) });
        self
    }

    pub fn stackalloc_uninit(&mut self, value: ValueId) -> &mut Self {
        self.inst(Inst::StackAlloc { value, init: None });
        self
    }

    pub fn alloc(&mut self, value: ValueId) -> &mut Self {
        self.inst(Inst::Alloc { value });
        self
    }

    pub fn destroy(
        &mut self,
        value: ValueId,
        destroy_glue: bool,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::Destroy { value, destroy_glue, span });
        self
    }

    pub fn free(&mut self, value: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::Free { value, span });
        self
    }

    pub fn inst(&mut self, inst: Inst) {
        self.body.block_mut(self.block).push_inst(inst);
    }
}
