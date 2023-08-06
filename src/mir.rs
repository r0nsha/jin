mod builder;

pub(crate) use builder::FunctionBuilder;

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
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) predecessors: Vec<BlockId>,
    pub(crate) successors: Vec<BlockId>,
}

impl Block {
    fn new(
        instructions: Vec<Instruction>,
        predecessors: Vec<BlockId>,
        successors: Vec<BlockId>,
    ) -> Self {
        Self {
            instructions: vec![],
            predecessors: vec![],
            successors: vec![],
        }
    }
}

pub(crate) enum Instruction {}
