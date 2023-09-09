mod builder;
mod lower;
mod pretty_print;

use std::{collections::HashMap, io};

pub use lower::lower;
use ustr::{ustr, Ustr};

use crate::{
    ast::BinOpKind,
    common::{new_key_type, IndexVec},
    db::{Db, DefId},
    span::Span,
    ty::Ty,
};

#[derive(Debug)]
pub struct Mir {
    pub functions: HashMap<DefId, Function>,
}

impl Mir {
    pub fn new() -> Self {
        Self { functions: HashMap::default() }
    }

    pub fn add_function(&mut self, function: Function) {
        self.functions.insert(function.id(), function);
    }

    #[allow(unused)]
    pub fn main_function(&self, db: &Db) -> Option<&Function> {
        db.main_function_id().and_then(|id| self.functions.get(&id))
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty_print::print(db, self, w)
    }
}

#[derive(Debug)]
pub struct Function {
    id: DefId,
    params: Vec<FunctionParam>,
    values: IndexVec<ValueId, Value>,
    cfg: Cfg,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionParam {
    id: DefId,
}

impl FunctionParam {
    pub fn id(self) -> DefId {
        self.id
    }
}

impl Function {
    fn new(id: DefId) -> Self {
        Self { id, params: vec![], values: IndexVec::new(), cfg: Cfg::new() }
    }

    pub fn id(&self) -> DefId {
        self.id
    }

    pub fn value(&self, id: ValueId) -> Option<&Value> {
        self.values.get(id)
    }

    #[allow(unused)]
    pub fn param(&self, index: usize) -> Option<FunctionParam> {
        self.params.get(index).copied()
    }

    pub fn params(&self) -> &[FunctionParam] {
        &self.params
    }

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

new_key_type!(ValueId);

#[derive(Debug, Clone, Copy)]
pub struct Value {
    pub ty: Ty,
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
    pub instructions: Vec<Inst>,
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
        self.instructions
            .iter()
            .any(|inst| matches!(inst, Inst::Return(..) | Inst::Unreachable(..)))
    }
}

#[derive(Debug, Clone)]
pub enum Inst {
    Return(Return),
    Br(Br),
    BrIf(BrIf),
    Phi(Phi),
    Call(Call),
    Cast(Cast),
    StackAlloc(StackAlloc),
    Load(Load),
    Neg(Neg),
    Not(Not),
    BinOp(BinOp),
    IntLit(IntLit),
    BoolLit(BoolLit),
    UnitLit(UnitLit),
    Unreachable(Unreachable),
}

#[derive(Debug, Clone)]
pub struct Return {
    pub value: ValueId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Br {
    pub target: BlockId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BrIf {
    pub cond: ValueId,
    pub b1: BlockId,
    pub b2: BlockId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Phi {
    pub value: ValueId,
    pub phi_values: Box<[PhiValue]>,
    #[allow(unused)]
    pub span: Span,
}

pub type PhiValue = (BlockId, ValueId);

#[derive(Debug, Clone)]
pub struct Call {
    pub value: ValueId,
    pub callee: ValueId,
    pub args: Vec<ValueId>,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub value: ValueId,
    pub operand: ValueId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct StackAlloc {
    pub id: DefId,
    pub operand: ValueId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Load {
    pub value: ValueId,
    pub id: DefId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Neg {
    pub value: ValueId,
    pub operand: ValueId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Not {
    pub value: ValueId,
    pub operand: ValueId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub value: ValueId,
    pub op: BinOpKind,
    pub lhs: ValueId,
    pub rhs: ValueId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct IntLit {
    pub value: ValueId,
    pub lit: usize,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BoolLit {
    pub value: ValueId,
    pub lit: bool,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct UnitLit {
    pub value: ValueId,
    #[allow(unused)]
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Unreachable {
    pub value: ValueId,
    #[allow(unused)]
    pub span: Span,
}
