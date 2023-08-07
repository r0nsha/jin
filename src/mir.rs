mod builder;
mod lower;

pub(crate) use lower::lower;

use crate::{
    common::{new_id_type, IdVec},
    db::{FunctionId, TyId},
};

pub(crate) struct Mir {
    pub(crate) functions: Vec<Function>,
}

impl Mir {
    pub(crate) fn new() -> Self {
        Self { functions: vec![] }
    }

    pub(crate) fn add_function(&mut self, function: Function) {
        self.functions.push(function);
    }
}

pub(crate) struct Function {
    id: FunctionId,
    registers: IdVec<RegisterId, Register>,
    parameters: Vec<RegisterId>,
    cfg: Cfg,
}

impl Function {
    fn new(id: FunctionId) -> Self {
        Self {
            id,
            registers: IdVec::new(),
            parameters: vec![],
            cfg: Cfg::new(),
        }
    }

    pub(crate) fn id(&self) -> FunctionId {
        self.id
    }

    pub(crate) fn register(&self, id: RegisterId) -> Option<&Register> {
        self.registers.get(id)
    }

    pub(crate) fn parameter(&self, index: usize) -> Option<RegisterId> {
        self.parameters.get(index).copied()
    }

    pub(crate) fn parameters(&self) -> &[RegisterId] {
        &self.parameters
    }

    pub(crate) fn block(&self, id: BlockId) -> Option<&Block> {
        self.cfg.blocks.get(id)
    }

    pub(crate) fn blocks(&self) -> &[Block] {
        self.cfg.blocks.as_slice()
    }
}

pub(crate) struct Cfg {
    pub(crate) blocks: IdVec<BlockId, Block>,
}

impl Cfg {
    fn new() -> Self {
        Self {
            blocks: IdVec::new(),
        }
    }
}

new_id_type!(RegisterId);

pub(crate) struct Register {
    pub(crate) ty: TyId,
}

new_id_type!(BlockId);

pub(crate) struct Block {
    pub(crate) id: BlockId,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) predecessors: Vec<BlockId>,
    pub(crate) successors: Vec<BlockId>,
}

impl Block {
    fn new(id: BlockId) -> Self {
        Self {
            id,
            instructions: vec![],
            predecessors: vec![],
            successors: vec![],
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Instruction {
    Return(Return),
    IntLit(IntLit),
    UnitLit(UnitLit),
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub(crate) register: RegisterId,
}

#[derive(Debug, Clone)]
pub(crate) struct IntLit {
    pub(crate) register: RegisterId,
    pub(crate) value: usize,
}

#[derive(Debug, Clone)]
pub(crate) struct UnitLit {
    pub(crate) register: RegisterId,
}
