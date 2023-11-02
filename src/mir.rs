mod lower;
mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;
pub use lower::lower;
use rustc_hash::FxHashMap;
use typed_index_collections::TiSlice;
use ustr::Ustr;

use crate::{
    db::{Db, DefId},
    hir::const_eval::Const,
    index_vec::{new_key_type, IndexSlice, IndexVec, IndexVecExt},
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
    Bare { value: ValueId, body: Body },
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
    pub fn start_block(&self) -> &Block {
        &self.blocks[BlockId::from(0)]
    }

    #[inline]
    pub fn blocks(&self) -> &TiSlice<BlockId, Block> {
        &self.blocks
    }

    pub fn push_block(&mut self, name: impl Into<String>) -> BlockId {
        self.blocks.push_with_key(|id| Block::new(id, name.into()))
    }

    pub fn last_block(&self) -> &Block {
        self.blocks.last().unwrap()
    }

    pub fn last_block_mut(&mut self) -> &mut Block {
        self.blocks.last_mut().unwrap()
    }

    #[inline]
    pub fn value(&self, id: ValueId) -> &Value {
        &self.values[id]
    }

    #[inline]
    pub fn value_mut(&mut self, id: ValueId) -> &mut Value {
        &mut self.values[id]
    }

    pub fn push_value(&mut self, ty: Ty) -> ValueId {
        self.values.push_with_key(|id| Value { id, ty })
    }

    pub fn is_terminating(&self) -> bool {
        matches!(self.last_block().insts().last(), Some(Inst::Return { .. }))
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    id: BlockId,
    name: String,
    insts: Vec<Inst>,
}

impl Block {
    pub fn new(id: BlockId, name: String) -> Self {
        Self { id, name, insts: vec![] }
    }

    pub fn id(&self) -> BlockId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
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
    Return { value: ValueId },
    Call { value: ValueId, callee: ValueId, args: Vec<ValueId> },
    // Binary { value: ValueId, lhs: ValueId, rhs: ValueId, op: BinOp },
    // Unary { value: ValueId, inner: ValueId, op: UnOp },
    // Cast { value: ValueId, inner: ValueId, target: Ty },
    Member { value: ValueId, inner: ValueId, member: Ustr },
    Load { value: ValueId, kind: LoadKind },
    StrLit { value: ValueId, lit: Ustr },
    IntLit { value: ValueId, lit: i128 },
    BoolLit { value: ValueId, lit: bool },
    UnitLit { value: ValueId },
}

#[derive(Debug, Clone)]
pub struct Value {
    pub id: ValueId,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum LoadKind {
    Fn(FnSigId),
    Param(DefId),
}
