use std::collections::HashSet;

use super::{
    Block, BlockId, Call, Function, Instruction, IntLit, Register, RegisterId, Return, TyId,
    UnitLit, Value,
};
use crate::{
    ast::BinaryOp,
    db::DefinitionId,
    mir::{Binary, BoolLit, Jmp, Jnz, Phi, PhiValue},
    span::Span,
};

pub struct FunctionBuilder {
    f: Function,
    current_block: BlockId,
}

impl FunctionBuilder {
    pub fn new(id: DefinitionId) -> Self {
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
    pub fn create_register(&mut self, ty: TyId) -> RegisterId {
        self.f.registers.push(Register { ty })
    }

    #[allow(unused)]
    pub fn create_parameter(&mut self, ty: TyId) -> usize {
        let reg_id = self.create_register(ty);
        self.f.parameters.push(reg_id);
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

    pub fn build_int_lit(&mut self, reg: RegisterId, value: usize, span: Span) {
        self.current_block_mut().add_instruction(Instruction::IntLit(IntLit {
            register: reg,
            value,
            span,
        }));
    }

    pub fn build_bool_lit(&mut self, reg: RegisterId, value: bool, span: Span) {
        self.current_block_mut().add_instruction(Instruction::BoolLit(BoolLit {
            register: reg,
            value,
            span,
        }));
    }

    pub fn build_unit_lit(&mut self, reg: RegisterId, span: Span) {
        self.current_block_mut()
            .add_instruction(Instruction::UnitLit(UnitLit { register: reg, span }));
    }

    pub fn build_return(&mut self, value: Value, span: Span) {
        self.current_block_mut().add_instruction(Instruction::Return(Return { value, span }));
    }

    pub fn build_jmp(&mut self, target: BlockId, span: Span) {
        // TODO: do we need this early return? does it not mess up codegen?
        // if self.current_block().is_terminating() {
        //     return;
        // }

        self.create_edge(self.current_block().id, target);
        self.current_block_mut().add_instruction(Instruction::Jmp(Jmp { target, span }));
    }

    pub fn build_jnz(&mut self, cond: Value, b1: BlockId, b2: BlockId, span: Span) {
        // TODO: do we need this early return? does it not mess up codegen?
        // if self.current_block().is_terminating() {
        //     return;
        // }

        self.create_edge(self.current_block().id, b1);
        self.create_edge(self.current_block().id, b2);
        self.current_block_mut().add_instruction(Instruction::Jnz(Jnz { cond, b1, b2, span }));
    }

    pub fn build_phi(&mut self, register: RegisterId, values: Box<[PhiValue]>, span: Span) {
        self.current_block_mut().add_instruction(Instruction::Phi(Phi { register, values, span }));
    }

    pub fn build_call(&mut self, register: RegisterId, callee: Value, span: Span) {
        self.current_block_mut().add_instruction(Instruction::Call(Call {
            register,
            callee,
            span,
        }));
    }

    pub fn build_binary(
        &mut self,
        register: RegisterId,
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
        span: Span,
    ) {
        self.current_block_mut().add_instruction(Instruction::Binary(Binary {
            register,
            op,
            lhs,
            rhs,
            span,
        }));
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
    pub fn add_instruction(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }
}
