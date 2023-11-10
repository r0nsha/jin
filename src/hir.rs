pub mod const_eval;
mod pretty_print;

use std::{fmt, io};

use enum_as_inner::EnumAsInner;
use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    db::{Db, DefId, ModuleId},
    index_vec::{new_key_type, IndexVec},
    middle::{BinOp, UnOp},
    span::Span,
    ty::{Instantiation, Ty, Typed},
    word::Word,
};

#[derive(Debug, Clone)]
pub struct Hir {
    pub fns: IndexVec<FnId, Fn>,
    pub extern_lets: IndexVec<ExternLetId, ExternLet>,
    pub lets: IndexVec<LetId, Let>,
}

impl Hir {
    pub fn new() -> Self {
        Self { fns: IndexVec::new(), extern_lets: IndexVec::new(), lets: IndexVec::new() }
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty_print::print(db, self, w)
    }
}

new_key_type!(ExprId);
new_key_type!(FnId);
new_key_type!(ExternLetId);
new_key_type!(LetId);

pub type HirMap<T> = FxHashMap<ExprId, T>;

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
            ExprKind::Member(access) => access.expr.walk_(f),
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
    Loop(Loop),
    Break,
    Block(Block),
    Return(Return),
    Call(Call),
    Unary(Unary),
    Binary(Binary),
    Cast(Cast),
    Member(Member),
    Name(Name),
    Lit(Lit),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub module_id: ModuleId,
    pub id: DefId,
    pub sig: FnSig,
    pub kind: FnKind,
    pub span: Span,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FnKind {
    Bare { body: Expr },
    Extern { is_c_variadic: bool },
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
pub struct TyParam {
    pub id: DefId,
    pub word: Word,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub id: DefId,
    pub name: Word,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Let {
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
pub enum Pat {
    Name(NamePat),
    Discard(Span),
}

impl Pat {
    pub fn walk(&self, mut f: impl FnMut(&NamePat)) {
        self.walk_(&mut f);
    }

    fn walk_(&self, f: &mut impl FnMut(&NamePat)) {
        match self {
            Self::Name(n) => f(n),
            Self::Discard(_) => (),
        }
    }

    pub fn any(&self, mut f: impl FnMut(&NamePat) -> bool) -> bool {
        self.any_(&mut f)
    }

    fn any_(&self, f: &mut impl FnMut(&NamePat) -> bool) -> bool {
        match self {
            Self::Name(n) => f(n),
            Self::Discard(_) => false,
        }
    }

    pub fn ids(&self) -> Vec<DefId> {
        let mut ids = vec![];
        self.walk(|name| ids.push(name.id));
        ids
    }
}

#[derive(Debug, Clone)]
pub struct NamePat {
    pub id: DefId,
    pub word: Word,
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pat::Name(n) => n.word.fmt(f),
            Pat::Discard(_) => f.write_str("_"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Box<Expr>,
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
pub struct Member {
    pub expr: Box<Expr>,
    pub member: Word,
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
