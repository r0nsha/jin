mod builder;
mod lower;
mod pretty_print;

pub use lower::lower;
use ustr::{ustr, Ustr};

use crate::{
    common::{new_key_type, IndexVec},
    db::{Database, DefinitionId, TyId},
    span::Span,
};

#[derive(Debug)]
pub struct Mir {
    // TODO: turn into a HashMap
    pub functions: Vec<Function>,
}

impl Mir {
    pub fn new() -> Self {
        Self { functions: vec![] }
    }

    pub fn add_function(&mut self, function: Function) {
        self.functions.push(function);
    }

    #[allow(unused)]
    pub fn main_function(&self, db: &Database) -> Option<&Function> {
        db.main_function_id().and_then(|id| self.functions.iter().find(|f| f.id() == id))
    }

    pub fn pretty_print(&self, db: &Database) {
        pretty_print::print(db, self);
    }
}

#[derive(Debug)]
pub struct Function {
    id: DefinitionId,
    registers: IndexVec<RegisterId, Register>,
    parameters: Vec<RegisterId>,
    cfg: Cfg,
}

impl Function {
    fn new(id: DefinitionId) -> Self {
        Self { id, registers: IndexVec::new(), parameters: vec![], cfg: Cfg::new() }
    }

    pub fn id(&self) -> DefinitionId {
        self.id
    }

    pub fn register(&self, id: RegisterId) -> Option<&Register> {
        self.registers.get(id)
    }

    #[allow(unused)]
    pub fn parameter(&self, index: usize) -> Option<RegisterId> {
        self.parameters.get(index).copied()
    }

    #[allow(unused)]
    pub fn parameters(&self) -> &[RegisterId] {
        &self.parameters
    }

    #[allow(unused)]
    pub fn block(&self, id: BlockId) -> Option<&Block> {
        self.cfg.blocks.get(id)
    }

    pub fn blocks(&self) -> &[Block] {
        self.cfg.blocks.as_slice()
    }
}

#[derive(Debug)]
pub struct Cfg {
    pub blocks: IndexVec<BlockId, Block>,
}

impl Cfg {
    fn new() -> Self {
        Self { blocks: IndexVec::new() }
    }
}

new_key_type!(RegisterId);

#[derive(Debug, Clone, Copy)]
pub struct Register {
    pub ty: TyId,
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Definition(DefinitionId),
    Register(RegisterId),
}

impl From<DefinitionId> for Value {
    fn from(value: DefinitionId) -> Self {
        Self::Definition(value)
    }
}

impl From<RegisterId> for Value {
    fn from(value: RegisterId) -> Self {
        Self::Register(value)
    }
}

new_key_type!(BlockId);

impl BlockId {
    #[inline]
    pub fn first() -> Self {
        Self(0)
    }
}

#[derive(Debug)]
pub struct Block {
    pub id: BlockId,
    pub name: Ustr,
    pub instructions: Vec<Instruction>,
    pub predecessors: Vec<BlockId>,
    pub successors: Vec<BlockId>,
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

    fn is_terminating(&self) -> bool {
        self.instructions.iter().any(|inst| matches!(inst, Instruction::Return(_)))
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Return(Return),
    Call(Call),
    IAdd(Binary),
    IntLit(IntLit),
    UnitLit(UnitLit),
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: Value,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub register: RegisterId,
    pub callee: Value,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub register: RegisterId,
    pub left: Value,
    pub right: Value,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IntLit {
    pub register: RegisterId,
    pub value: usize,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnitLit {
    pub register: RegisterId,
    #[allow(unused)]
    pub span: Span,
}
