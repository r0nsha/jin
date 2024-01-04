mod pretty_print;
pub mod subst;

use std::{collections::hash_map::Entry, io};

use data_structures::{index_vec::IndexVec, new_key_type};
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    db::{Db, DefId, ModuleId},
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

impl Expr {
    pub fn walk(&self, mut f: impl FnMut(&Expr)) {
        self.walk_(&mut f);
    }

    fn walk_(&self, f: &mut impl FnMut(&Expr)) {
        match &self.kind {
            ExprKind::Let(let_) => let_.value.walk_(f),
            ExprKind::Assign(assign) => {
                assign.lhs.walk_(f);
                assign.rhs.walk_(f);
            }
            ExprKind::If(if_) => {
                if_.cond.walk_(f);
                if_.then.walk_(f);
                if_.otherwise.walk_(f);
            }
            ExprKind::Match(match_) => {
                match_.expr.walk_(f);

                for case in &match_.cases {
                    case.expr.walk_(f);
                }
            }
            ExprKind::Loop(loop_) => {
                loop_.expr.walk_(f);
            }
            ExprKind::Block(blk) => {
                for expr in &blk.exprs {
                    expr.walk_(f);
                }
            }
            ExprKind::Return(ret) => ret.expr.walk_(f),
            ExprKind::Call(call) => {
                call.callee.walk_(f);

                for arg in &call.args {
                    arg.expr.walk_(f);
                }
            }
            ExprKind::Unary(un) => un.expr.walk_(f),
            ExprKind::Binary(bin) => {
                bin.lhs.walk_(f);
                bin.rhs.walk_(f);
            }
            ExprKind::Cast(cast) => cast.expr.walk_(f),
            ExprKind::Field(access) => access.expr.walk_(f),
            ExprKind::Break | ExprKind::Name(_) | ExprKind::Lit(_) => (),
        }

        f(self);
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Let(Let),
    Assign(Assign),
    If(If),
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
    Name(Name),
    Lit(Lit),
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
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub expr: Box<Expr>,
    pub cases: Vec<MatchCase>,
}

#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pat: MatchPat,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum MatchPat {
    Name(DefId, Span),
    Wildcard(Span),
    Bool(bool, Span),
}

impl Spanned for MatchPat {
    fn span(&self) -> Span {
        match self {
            Self::Name(_, span)
            | Self::Wildcard(span)
            | Self::Bool(_, span) => *span,
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
pub struct Name {
    pub id: DefId,
    pub word: Word,
    pub instantiation: Instantiation,
}

#[derive(Debug, Clone)]
pub enum Lit {
    Bool(bool),
    Int(u128),
    Float(f64),
    Str(Ustr),
}
