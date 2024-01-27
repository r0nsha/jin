mod pretty_print;
pub mod subst;

use std::{collections::hash_map::Entry, io};

use data_structures::{index_vec::IndexVec, new_key_type};
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    db::{AdtId, Db, DefId, ModuleId, VariantId},
    middle::{BinOp, Pat, TyParam, UnOp},
    span::{Span, Spanned},
    ty::{coerce::Coercions, Instantiation, Ty, Typed},
    word::Word,
};

#[derive(Debug, Clone)]
pub struct Hir {
    pub fns: IndexVec<FnId, Fn>,
    pub lets: IndexVec<LetId, Let>,
    pub extern_lets: IndexVec<ExternLetId, ExternLet>,
    pub coercions: FxHashMap<ExprId, Coercions>,
}

impl Hir {
    pub fn new() -> Self {
        Self {
            fns: IndexVec::new(),
            lets: IndexVec::new(),
            extern_lets: IndexVec::new(),
            coercions: FxHashMap::default(),
        }
    }

    pub fn push_coercions(&mut self, expr_id: ExprId, c: Coercions) {
        match self.coercions.entry(expr_id) {
            Entry::Occupied(mut entry) => {
                entry.get_mut().extend(c);
            }
            Entry::Vacant(entry) => {
                entry.insert(c);
            }
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

new_key_type! {
    pub struct ExprId;
    pub struct FnId;
    pub struct ExternLetId;
    pub struct LetId;
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Let(Let),
    Assign(Assign),
    Swap(Swap),
    Match(Match),
    Loop(Loop),
    Break,
    Block(Block),
    Return(Return),
    Call(Call),
    Unary(Unary),
    Binary(Binary),
    Cast(Cast),
    Field(Field),
    Index(Index),
    Slice(Slice),
    Name(Name),
    Variant(Variant),
    SliceLit(SliceLit),
    BoolLit(bool),
    IntLit(u128),
    FloatLit(f64),
    StrLit(Ustr),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: FnId,
    pub module_id: ModuleId,
    pub def_id: DefId,
    pub sig: FnSig,
    pub kind: FnKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum FnKind {
    Bare { body: Expr },
    Extern { is_c_variadic: bool },
}

impl FnKind {
    /// Returns `true` if the fn kind is [`Extern`].
    ///
    /// [`Extern`]: FnKind::Extern
    #[must_use]
    pub fn is_extern(&self) -> bool {
        matches!(self, Self::Extern { .. })
    }
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub word: Word,
    pub ty_params: Vec<TyParam>,
    pub params: Vec<FnParam>,
    pub ret: Ty,
    pub ret_span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub pat: Pat,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub id: LetId,
    pub module_id: ModuleId,
    pub pat: Pat,
    pub value: Box<Expr>,
    pub ty: Ty,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExternLet {
    pub module_id: ModuleId,
    pub id: DefId,
    pub word: Word,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: Option<BinOp>,
}

#[derive(Debug, Clone)]
pub struct Swap {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub expr: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pat: MatchPat,
    pub guard: Option<Box<Expr>>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum MatchPat {
    Name(DefId, Ty, Span),
    Wildcard(Span),
    Unit(Span),
    Bool(bool, Span),
    Int(i128, Span),
    Str(Ustr, Span),
    Adt(AdtId, Vec<Self>, Span),
    Variant(VariantId, Vec<Self>, Span),
    Or(Vec<Self>, Span),
}

impl Spanned for MatchPat {
    fn span(&self) -> Span {
        match self {
            Self::Name(_, _, span)
            | Self::Wildcard(span)
            | Self::Unit(span)
            | Self::Bool(_, span)
            | Self::Int(_, span)
            | Self::Str(_, span)
            | Self::Adt(_, _, span)
            | Self::Variant(_, _, span)
            | Self::Or(_, span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub cond: Option<Box<Expr>>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<CallArg>,
}

#[derive(Debug, Clone)]
pub struct CallArg {
    pub name: Option<Word>,
    pub expr: Expr,
    pub index: Option<usize>,
}

impl Typed for CallArg {
    fn ty(&self) -> Ty {
        self.expr.ty
    }

    fn ty_mut(&mut self) -> &mut Ty {
        &mut self.expr.ty
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub expr: Box<Expr>,
    pub op: UnOp,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOp,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub expr: Box<Expr>,
    pub target: Ty,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub expr: Box<Expr>,
    pub field: Word,
}

#[derive(Debug, Clone)]
pub struct Index {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Slice {
    pub expr: Box<Expr>,
    pub low: Option<Box<Expr>>,
    pub high: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: DefId,
    pub word: Word,
    pub instantiation: Instantiation,
}

#[derive(Debug, Clone)]
pub struct Variant {
    pub id: VariantId,
    pub instantiation: Instantiation,
}

#[derive(Debug, Clone)]
pub struct SliceLit {
    pub exprs: Vec<Expr>,
    pub cap: Option<Box<Expr>>,
}
