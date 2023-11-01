mod lower;
mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;
pub use lower::lower;
use ustr::Ustr;

use crate::{
    db::{Db, DefId},
    hir::const_eval::Const,
    index_vec::{new_key_type, IndexSlice, IndexVec},
    middle::{BinOp, UnOp},
    ty::Ty,
};

new_key_type!(FnSigId);
new_key_type!(GlobalId);
new_key_type!(BlockId);
new_key_type!(ValueId);

#[derive(Debug)]
pub struct Mir {
    pub fn_sigs: IndexVec<FnSigId, FnSig>,
    pub fns: Vec<Fn>,
    pub extern_fns: Vec<ExternFn>,
    pub globals: IndexVec<GlobalId, Global>,
    pub main_fn: Option<FnSigId>,
}

impl Mir {
    pub fn new() -> Self {
        Self {
            fn_sigs: IndexVec::new(),
            fns: vec![],
            extern_fns: vec![],
            globals: IndexVec::new(),
            main_fn: None,
        }
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty_print::print(db, self, w)
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub def_id: DefId,
    pub sig: FnSigId,
    pub value: BlockId,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub struct ExternFn {
    pub def_id: DefId,
    pub sig: FnSigId,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub def_id: DefId,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub id: FnSigId,
    pub name: Ustr,
    pub params: Vec<FnParam>,
    pub ret: Ty,
    pub ty: Ty,
    pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub id: GlobalId,
    pub def_id: DefId,
    pub name: Ustr,
    pub ty: Ty,
    pub kind: GlobalKind,
}

#[derive(Debug, Clone)]
pub enum GlobalKind {
    Bare { value: BlockId, body: Body },
    Extern,
}

#[derive(Debug, Clone)]
pub struct Body {
    blocks: Blocks,
    values: Values,
}

pub type Blocks = IndexVec<BlockId, Block>;
pub type Values = IndexVec<ValueId, Value>;

impl Body {
    pub fn new() -> Self {
        Self { blocks: IndexVec::new(), values: IndexVec::new() }
    }

    #[inline]
    pub fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id]
    }

    #[inline]
    pub fn value(&self, id: ValueId) -> &Value {
        &self.values[id]
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    insts: Vec<Inst>,
}

impl Block {
    pub fn new() -> Self {
        Self { insts: vec![] }
    }

    pub fn insts(&self) -> &[Inst] {
        &self.insts
    }

    pub fn push_inst(&mut self, inst: Inst) {
        self.insts.push(inst);
    }
}

#[derive(Debug, Clone)]
pub enum Inst {
    // StackAlloc { id: LocalId, def_id: DefId, value: ValueId },
    // If { cond: ValueId, then: ValueId, otherwise: ValueId },
    // Return { value: ValueId },
    // Call { callee: ValueId, args: Vec<ValueId> },
    // Binary { lhs: ValueId, rhs: ValueId, op: BinOp },
    // Unary { value: ValueId, op: UnOp },
    // Cast { value: ValueId, target: Ty },
    // Index { value: ValueId, index: usize },
    // Const(Const),
}

#[derive(Debug, Clone)]
pub struct Value {
    pub id: ValueId,
    pub ty: Ty,
}
