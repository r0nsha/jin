mod builder;
mod lower;
mod pretty_print;

pub(crate) use lower::lower;
use ustr::{ustr, Ustr};

use crate::db::SymbolId;
use crate::{
    common::{new_key_type, IndexVec},
    db::{Database, TyId},
    span::Span,
};

#[derive(Debug)]
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

#[derive(Debug)]
pub(crate) struct Function {
    id: SymbolId,
    registers: IndexVec<RegisterId, Register>,
    parameters: Vec<RegisterId>,
    cfg: Cfg,
}

impl Function {
    fn new(id: SymbolId) -> Self {
        Self {
            id,
            registers: IndexVec::new(),
            parameters: vec![],
            cfg: Cfg::new(),
        }
    }

    pub(crate) fn id(&self) -> SymbolId {
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

#[derive(Debug)]
pub(crate) struct Cfg {
    pub(crate) blocks: IndexVec<BlockId, Block>,
}

impl Cfg {
    fn new() -> Self {
        Self { blocks: IndexVec::new() }
    }
}

new_key_type!(RegisterId);

#[derive(Debug, Clone, Copy)]
pub(crate) struct Register {
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Value {
    Symbol(SymbolId),
    Register(RegisterId),
}

impl From<SymbolId> for Value {
    fn from(value: SymbolId) -> Self {
        Self::Symbol(value)
    }
}

impl From<RegisterId> for Value {
    fn from(value: RegisterId) -> Self {
        Self::Register(value)
    }
}

new_key_type!(BlockId);

#[derive(Debug)]
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
    Call(Call),
    IntLit(IntLit),
    UnitLit(UnitLit),
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub(crate) value: Value,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Call {
    pub(crate) register: RegisterId,
    pub(crate) callee: Value,
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
