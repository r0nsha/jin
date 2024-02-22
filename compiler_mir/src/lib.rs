mod builder;
mod lower;
mod monomorphize;
mod ownck;
mod pmatch;
mod pretty;
pub mod subst;

use std::{io, iter};

use compiler_data_structures::{
    id_map::IdMap,
    index_vec::{IndexSlice, IndexVec, IndexVecExt},
    new_key_type,
};
use indexmap::{indexset, IndexSet};
pub use lower::lower;
pub use monomorphize::monomorphize;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::Ustr;

use compiler_core::{
    db::{AdtId, Db, DefId, VariantId},
    middle::{BinOp, Pat, UnOp},
    span::Span,
    ty::{Instantiation, Ty},
};

use crate::builder::InstBuilder;

new_key_type! {
    pub struct FnSigId;
    pub struct GlobalId;
    pub struct BlockId;
    pub struct ValueId;
}

#[derive(Debug, Clone)]
pub struct Mir {
    pub fn_sigs: IdMap<FnSigId, FnSig>,
    pub fns: FxHashMap<FnSigId, Fn>,
    pub globals: IdMap<GlobalId, Global>,
    pub main_fn: Option<FnSigId>,
}

impl Default for Mir {
    fn default() -> Self {
        Self::new()
    }
}

impl Mir {
    pub fn new() -> Self {
        Self {
            fn_sigs: IdMap::new(),
            fns: FxHashMap::default(),
            globals: IdMap::new(),
            main_fn: None,
        }
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty::print(db, self, w)
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub sig: FnSigId,
    pub body: Body,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub pat: Pat,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub id: FnSigId,
    pub def_id: DefId,
    pub mangled_name: Ustr,
    pub display_name: Ustr,
    pub params: Vec<FnParam>,
    pub ty: Ty,
    pub is_inline: bool,
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

#[derive(Debug, Clone)]
pub enum GlobalKind {
    Const(Const),
    Static(StaticGlobal),
    Extern,
}

impl GlobalKind {
    #[must_use]
    pub fn as_static_mut(&mut self) -> Option<&mut StaticGlobal> {
        if let Self::Static(v) = self {
            Some(v)
        } else {
            None
        }
    }
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

impl Default for Body {
    fn default() -> Self {
        Self::new()
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

    pub fn clone_block(&mut self, block: BlockId) -> BlockId {
        let name = self.block(block).name.clone();
        self.blocks.push_with_key(|id| Block::new(id, name))
    }

    pub fn create_blocks(&mut self, base: impl core::fmt::Display, len: usize) -> Vec<BlockId> {
        (0..len).map(|i| self.create_block(format!("{base}{i}"))).collect()
    }

    pub fn create_edge(&mut self, source: BlockId, target: BlockId) {
        if source == target {
            // Prevent cycles between a block and itself
            return;
        }

        self.block_mut(source).successors.insert(target);
        self.block_mut(target).predecessors.insert(source);
    }

    pub fn connect_blocks(&mut self, blocks: &[BlockId]) {
        for (&curr, &next) in blocks.iter().zip(blocks[1..].iter()) {
            self.create_edge(curr, next);
        }
    }

    pub fn parent(&self, value: ValueId) -> Option<ValueId> {
        match self.value(value).kind {
            ValueKind::Field(parent, _) | ValueKind::Variant(parent, _) => Some(parent),
            _ => None,
        }
    }

    #[inline]
    pub fn value(&self, value: ValueId) -> &Value {
        &self.values[value]
    }

    #[inline]
    pub fn value_mut(&mut self, value: ValueId) -> &mut Value {
        &mut self.values[value]
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

    pub fn create_register(&mut self, ty: Ty) -> ValueId {
        self.create_value(ty, ValueKind::Register(None))
    }

    pub fn instantation(&self, value: ValueId) -> Option<&Instantiation> {
        self.instantations.get(&value)
    }

    #[allow(unused)]
    pub fn instantations(&self) -> &FxHashMap<ValueId, Instantiation> {
        &self.instantations
    }

    pub fn instantations_mut(&mut self) -> &mut FxHashMap<ValueId, Instantiation> {
        &mut self.instantations
    }

    pub fn create_instantation(&mut self, value: ValueId, instantation: Instantiation) {
        self.instantations.insert(value, instantation);
    }

    pub fn ins(&mut self, block: BlockId) -> InstBuilder {
        InstBuilder::new(self, block)
    }

    pub fn last_inst_is_return(&self) -> bool {
        self.blocks()
            .last()
            .map(|b| matches!(b.insts.last(), Some(Inst::Return { .. })))
            .unwrap_or_default()
    }

    pub fn cleanup(&mut self) {
        self.connect_implicit_successors();
        self.remove_unused_blocks();
    }

    fn remove_unused_blocks(&mut self) {
        let blocks = &self.blocks;
        let mut new_blocks = IndexVec::<BlockId, Block>::new();
        let mut id_map: IndexVec<_, _> =
            iter::repeat(BlockId::start()).take(blocks.len()).collect();
        let mut valid = Vec::with_capacity(blocks.len());

        for (id, block) in blocks.iter_enumerated() {
            if block.insts.is_empty() || !block.is_connected() {
                continue;
            }

            id_map[id] = BlockId(new_blocks.len());
            valid.push((id, block));
            new_blocks.push_with_key(|id| Block::new(id, block.name.clone()));
        }

        for (id, block) in valid {
            let block_id = id_map[id];

            new_blocks[block_id].insts = block.insts.clone();

            let (last_inst, insts) = new_blocks[block_id].insts.split_last_mut().unwrap();

            for inst in insts {
                update_block_ids(blocks, &id_map, inst);
            }

            update_block_ids(blocks, &id_map, last_inst);

            let successors = match last_inst {
                Inst::Br { target } => {
                    indexset! { *target }
                }
                Inst::BrIf { cond: _, then, otherwise } => {
                    if let Some(otherwise) = otherwise {
                        indexset! { *then, *otherwise }
                    } else {
                        indexset! { *then }
                    }
                }
                Inst::Switch { cond: _, blocks } => blocks.iter().copied().collect::<IndexSet<_>>(),
                _ => continue,
            };

            for &succ in &successors {
                new_blocks[succ].predecessors.insert(block_id);
            }

            new_blocks[block_id].successors = successors;
        }

        self.blocks = new_blocks;
    }

    fn connect_implicit_successors(&mut self) {
        for block in self.blocks_mut() {
            match block.insts.last() {
                Some(
                    Inst::Br { .. } | Inst::BrIf { .. } | Inst::Switch { .. } | Inst::Return { .. },
                ) => (),
                _ if block.successors.len() == 1 => {
                    block.push_inst(Inst::Br { target: block.successors[0] });
                }
                _ => (),
            }
        }
    }
}

fn update_block_ids(
    blocks: &IndexSlice<BlockId, Block>,
    id_map: &IndexSlice<BlockId, BlockId>,
    inst: &mut Inst,
) {
    match inst {
        Inst::Br { target } => {
            *target = id_map[find_successor(blocks, *target)];
        }
        Inst::BrIf { cond: _, then, otherwise } => {
            *then = id_map[find_successor(blocks, *then)];

            if let Some(otherwise) = otherwise {
                *otherwise = id_map[find_successor(blocks, *otherwise)];
            }
        }
        Inst::Switch { cond: _, blocks: targets } => {
            for target in targets {
                *target = id_map[find_successor(blocks, *target)];
            }
        }
        _ => (),
    }
}

fn find_successor(blocks: &IndexSlice<BlockId, Block>, old_id: BlockId) -> BlockId {
    let mut id = old_id;

    loop {
        let block = &blocks[id];

        if !block.insts.is_empty() {
            break;
        }

        id = block.successors[0];
    }

    id
}

#[derive(Debug, Clone)]
pub struct Block {
    pub id: BlockId,
    pub name: String,
    pub insts: Vec<Inst>,
    pub predecessors: IndexSet<BlockId>,
    pub successors: IndexSet<BlockId>,
}

impl Block {
    pub fn new(id: BlockId, name: String) -> Self {
        Self {
            id,
            name,
            insts: vec![],
            predecessors: IndexSet::default(),
            successors: IndexSet::default(),
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
    StackAlloc { value: ValueId, init: Option<ValueId> },
    Store { value: ValueId, target: ValueId },
    Alloc { value: ValueId },
    SliceAlloc { value: ValueId, cap: ValueId },
    SliceIndex { value: ValueId, slice: ValueId, index: ValueId, span: Option<Span> },
    SliceSlice { value: ValueId, slice: ValueId, low: ValueId, high: ValueId, span: Span },
    SliceStore { slice: ValueId, index: ValueId, value: ValueId, span: Option<Span> },
    Destroy { value: ValueId, destroy_glue: bool, span: Span },
    Free { value: ValueId, traced: bool, span: Span },
    IncRef { value: ValueId },
    DecRef { value: ValueId },
    Br { target: BlockId },
    BrIf { cond: ValueId, then: BlockId, otherwise: Option<BlockId> },
    Switch { cond: ValueId, blocks: Vec<BlockId> },
    Return { value: ValueId },
    Call { value: ValueId, callee: ValueId, args: Vec<ValueId>, span: Span },
    RtCall { value: ValueId, kind: RtCallKind, span: Span },
    Binary { value: ValueId, lhs: ValueId, rhs: ValueId, op: BinOp, span: Option<Span> },
    Unary { value: ValueId, inner: ValueId, op: UnOp },
    Cast { value: ValueId, source: ValueId, target: Ty, span: Span },
    Transmute { value: ValueId, source: ValueId, target: Ty },
    StrLit { value: ValueId, lit: Ustr },
}

#[derive(Debug, Clone, Copy)]
pub enum RtCallKind {
    SliceGrow { slice: ValueId, new_cap: ValueId },
}

impl RtCallKind {
    pub fn traced(self) -> bool {
        match self {
            RtCallKind::SliceGrow { .. } => true,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            RtCallKind::SliceGrow { .. } => "jinrt_slice_grow",
        }
    }
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

    // A function parameter
    Param(DefId, usize),

    // A local variable
    Local(DefId),

    // A global variable
    Global(GlobalId),

    // A function signature
    Fn(FnSigId),

    // A constant value
    Const(Const),

    // A field of a value
    Field(ValueId, Ustr),

    // A variant of a type union
    Variant(ValueId, VariantId),

    // A dereferenced pointer
    Deref(ValueId),
}

impl ValueKind {
    /// Returns `true` if the value kind is [`Register`].
    ///
    /// [`Register`]: ValueKind::Register
    #[must_use]
    #[allow(unused)]
    pub fn is_register(&self) -> bool {
        matches!(self, Self::Register(..))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Str(Ustr),
    Int(i128),
    Float(f64),
    Bool(bool),
    Unit,
}
