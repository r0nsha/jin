mod builder;
mod lower;
mod pretty_print;

pub(crate) use lower::lower;
use ustr::{ustr, Ustr};

use crate::db::SymbolId;
use crate::{
    common::{new_id_type, IdVec},
    db::{Database, FunctionId, TyId},
    span::Span,
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

    pub(crate) fn pretty_print(&self, db: &Database) {
        pretty_print::print(db, self)
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
        Self { blocks: IdVec::new() }
    }
}

new_id_type!(RegisterId);

pub(crate) struct Register {
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Symbol(SymbolId),
    Register(RegisterId),
}

new_id_type!(BlockId);

pub(crate) struct Block {
    pub(crate) id: BlockId,
    pub(crate) name: Ustr,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) predecessors: Vec<BlockId>,
    pub(crate) successors: Vec<BlockId>,
}

impl Block {
    fn new(id: BlockId, name: impl AsRef<str>) -> Self {
        Self {
            id,
            name: ustr(name.as_ref()),
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
    pub(crate) value: Value,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct IntLit {
    pub(crate) register: RegisterId,
    pub(crate) value: usize,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct UnitLit {
    pub(crate) register: RegisterId,
    pub(crate) span: Span,
}
