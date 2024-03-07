use compiler_core::{middle::BinOp, span::Span};

use crate::{BlockId, Body, Inst, RtCallKind, ValueId};

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
        span: Span,
    ) -> &mut Self {
        self.body.create_edge(self.block, then);

        if let Some(otherwise) = otherwise {
            self.body.create_edge(self.block, otherwise);
        }

        self.inst(Inst::BrIf { cond, then, otherwise, span });
        self
    }

    pub fn switch(&mut self, cond: ValueId, blocks: Vec<BlockId>, span: Span) -> &mut Self {
        for &target in &blocks {
            self.body.create_edge(self.block, target);
        }

        self.inst(Inst::Switch { cond, blocks, span });
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
        self.inst(Inst::Binary { value, lhs, rhs, op, span, checked: true });
        self
    }

    pub fn binary_unchecked(
        &mut self,
        value: ValueId,
        lhs: ValueId,
        rhs: ValueId,
        op: BinOp,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::Binary { value, lhs, rhs, op, span, checked: false });
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

    pub fn rtcall(&mut self, value: ValueId, kind: RtCallKind, span: Span) -> &mut Self {
        self.inst(Inst::RtCall { value, kind, span });
        self
    }

    pub fn ret(&mut self, value: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::Return { value, span });
        self
    }

    pub fn store(&mut self, value: ValueId, target: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::Store { value, target, span });
        self
    }

    pub fn slice_index(
        &mut self,
        value: ValueId,
        slice: ValueId,
        index: ValueId,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::SliceIndex { value, slice, index, span, checked: true });
        self
    }

    pub fn slice_index_unchecked(
        &mut self,
        value: ValueId,
        slice: ValueId,
        index: ValueId,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::SliceIndex { value, slice, index, span, checked: false });
        self
    }

    pub fn slice_slice(
        &mut self,
        value: ValueId,
        slice: ValueId,
        low: ValueId,
        high: ValueId,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::SliceSlice { value, slice, low, high, span });
        self
    }

    pub fn slice_store(
        &mut self,
        slice: ValueId,
        index: ValueId,
        value: ValueId,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::SliceStore { slice, index, value, span, checked: true });
        self
    }

    pub fn slice_store_unchecked(
        &mut self,
        slice: ValueId,
        index: ValueId,
        value: ValueId,
        span: Span,
    ) -> &mut Self {
        self.inst(Inst::SliceStore { slice, index, value, span, checked: false });
        self
    }

    pub fn incref(&mut self, value: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::IncRef { value, span });
        self
    }

    pub fn decref(&mut self, value: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::DecRef { value, span });
        self
    }

    pub fn stackalloc(&mut self, value: ValueId, init: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::StackAlloc { value, init: Some(init), span });
        self
    }

    pub fn stackalloc_uninit(&mut self, value: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::StackAlloc { value, init: None, span });
        self
    }

    pub fn alloc_slice(&mut self, value: ValueId, cap: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::SliceAlloc { value, cap, span });
        self
    }

    pub fn alloc(&mut self, value: ValueId, span: Span) -> &mut Self {
        self.inst(Inst::Alloc { value, span });
        self
    }

    pub fn destroy(&mut self, value: ValueId, destroy_glue: bool, span: Span) -> &mut Self {
        self.inst(Inst::Destroy { value, destroy_glue, span });
        self
    }

    pub fn free(&mut self, value: ValueId, traced: bool, span: Span) -> &mut Self {
        self.inst(Inst::Free { value, traced, span });
        self
    }

    pub fn inst(&mut self, inst: Inst) {
        self.body.block_mut(self.block).push_inst(inst);
    }
}
