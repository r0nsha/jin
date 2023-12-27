mod lower;
mod pretty_print;
mod specialize;
pub mod subst;

use std::io;

use derive_more::From;
use enum_as_inner::EnumAsInner;
pub use lower::lower;
use rustc_hash::{FxHashMap, FxHashSet};
pub use specialize::specialize;
use ustr::Ustr;

use crate::{
    data_structures::{
        id_map::IdMap,
        index_vec::{new_key_type, IndexSlice, IndexVec, IndexVecExt},
    },
    db::{Db, DefId, StructId},
    middle::{BinOp, Pat, UnOp},
    span::Span,
    ty::{Instantiation, Ty},
};

new_key_type!(FnSigId);
new_key_type!(GlobalId);
new_key_type!(BlockId);
new_key_type!(ValueId);

#[derive(Debug, Clone)]
pub struct Mir {
    pub fn_sigs: IdMap<FnSigId, FnSig>,
    pub fns: FxHashMap<FnSigId, Fn>,
    pub globals: IdMap<GlobalId, Global>,
    pub struct_ctors: FxHashMap<StructId, FnSigId>,
    pub main_fn: Option<FnSigId>,
}

impl Mir {
    pub fn new() -> Self {
        Self {
            fn_sigs: IdMap::new(),
            fns: FxHashMap::default(),
            globals: IdMap::new(),
            struct_ctors: FxHashMap::default(),
            main_fn: None,
        }
    }

    pub fn pretty_print(
        &self,
        db: &Db,
        w: &mut impl io::Write,
    ) -> io::Result<()> {
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
    pub pat: Pat,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub id: FnSigId,
    pub name: Ustr,
    pub params: Vec<FnParam>,
    pub ty: Ty,
    pub is_extern: bool,
    pub is_c_variadic: bool,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub id: GlobalId,
    pub def_id: DefId,
    pub name: Ustr,
    pub ty: Ty,
    pub kind: GlobalKind,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum GlobalKind {
    Static(StaticGlobal),
    Extern,
}

#[derive(Debug, Clone)]
pub struct StaticGlobal {
    pub body: Body,
    pub result: ValueId,
}

#[derive(Debug, Clone)]
pub struct Body {
    blocks: Blocks,
    values: Values,

    // A mapping for values to their instantation, used for specialization
    instantations: FxHashMap<ValueId, Instantiation>,

    // A mapping of values to their destroy flags
    destroy_flags: FxHashMap<ValueId, ValueId>,
}

pub type Blocks = IndexVec<BlockId, Block>;
pub type Values = IndexVec<ValueId, Value>;

impl BlockId {
    #[inline]
    pub fn start() -> Self {
        Self::from(0)
    }
}

impl Body {
    pub fn new() -> Self {
        Self {
            blocks: IndexVec::new(),
            values: IndexVec::new(),
            instantations: FxHashMap::default(),
            destroy_flags: FxHashMap::default(),
        }
    }

    #[inline]
    pub fn block(&self, id: BlockId) -> &Block {
        &self.blocks[id]
    }

    #[inline]
    pub fn block_mut(&mut self, id: BlockId) -> &mut Block {
        &mut self.blocks[id]
    }

    #[inline]
    pub fn blocks(&self) -> &IndexSlice<BlockId, Block> {
        &self.blocks
    }

    #[inline]
    pub fn blocks_mut(&mut self) -> &mut IndexSlice<BlockId, Block> {
        &mut self.blocks
    }

    pub fn create_block(&mut self, name: impl Into<String>) -> BlockId {
        self.blocks.push_with_key(|id| Block::new(id, name.into()))
    }

    pub fn create_edge(&mut self, source: BlockId, target: BlockId) {
        self.block_mut(source).successors.insert(target);
        self.block_mut(target).predecessors.insert(source);
    }

    #[inline]
    pub fn value(&self, id: ValueId) -> &Value {
        &self.values[id]
    }

    #[inline]
    pub fn value_mut(&mut self, id: ValueId) -> &mut Value {
        &mut self.values[id]
    }

    #[inline]
    pub fn values(&self) -> &IndexSlice<ValueId, Value> {
        &self.values
    }

    #[inline]
    pub fn values_mut(&mut self) -> &mut IndexSlice<ValueId, Value> {
        &mut self.values
    }

    pub fn create_value(&mut self, ty: Ty, kind: ValueKind) -> ValueId {
        self.values.push_with_key(|id| Value { id, ty, kind })
    }

    pub fn instantation(&self, value: ValueId) -> Option<&Instantiation> {
        self.instantations.get(&value)
    }

    #[allow(unused)]
    pub fn instantations(&self) -> &FxHashMap<ValueId, Instantiation> {
        &self.instantations
    }

    pub fn instantations_mut(
        &mut self,
    ) -> &mut FxHashMap<ValueId, Instantiation> {
        &mut self.instantations
    }

    pub fn create_instantation(
        &mut self,
        value: ValueId,
        instantation: Instantiation,
    ) {
        self.instantations.insert(value, instantation);
    }

    pub fn is_terminating(&self) -> bool {
        self.blocks()
            .last()
            .map(|blk| matches!(blk.insts.last(), Some(Inst::Return { .. })))
            .unwrap_or_default()
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId,
    pub name: String,
    pub insts: Vec<Inst>,
    pub predecessors: FxHashSet<BlockId>,
    pub successors: FxHashSet<BlockId>,
}

impl Block {
    pub fn new(id: BlockId, name: String) -> Self {
        Self {
            id,
            name,
            insts: vec![],
            predecessors: FxHashSet::default(),
            successors: FxHashSet::default(),
        }
    }

    pub fn display_name(&self) -> String {
        format!("{}_{}", self.name, self.id.0)
    }

    pub fn push_inst(&mut self, inst: Inst) {
        self.insts.push(inst);
    }

    pub fn is_connected(&self) -> bool {
        self.id == BlockId::start() || !self.predecessors.is_empty()
    }
}

#[derive(Debug, Clone)]
pub enum Inst {
    Local { value: ValueId, init: ValueId },
    Store { value: ValueId, target: ValueId },
    Free { value: ValueId, destroy_flag: Option<ValueId>, span: Span },
    Br { target: BlockId },
    BrIf { cond: ValueId, then: BlockId, otherwise: Option<BlockId> },
    If { value: ValueId, cond: ValueId, then: ValueId, otherwise: ValueId },
    Return { value: ValueId },
    Call { value: ValueId, callee: ValueId, args: Vec<ValueId> },
    Binary { value: ValueId, lhs: ValueId, rhs: ValueId, op: BinOp, span: Span },
    Unary { value: ValueId, inner: ValueId, op: UnOp },
    Cast { value: ValueId, inner: ValueId, target: Ty, span: Span },
    StrLit { value: ValueId, lit: Ustr },
}

#[derive(Debug, Clone)]
pub struct Value {
    pub id: ValueId,
    pub ty: Ty,
    pub kind: ValueKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueKind {
    // A temporary or unnamed value, with a possible alias
    Register(Option<Ustr>),

    // A local variable or function parameter
    Local(DefId),

    // A global variable
    Global(GlobalId),

    // A function signature
    Fn(FnSigId),

    // A constant value
    Const(Const),

    // A field of a value
    Field(ValueId, Ustr),
}

#[derive(Debug, Clone, PartialEq, From)]
pub enum Const {
    Str(Ustr),
    Int(i128),
    Float(f64),
    Bool(bool),
    Unit,
}
