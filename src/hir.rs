pub mod const_eval;
mod lower;
mod pretty_print;

use std::{collections::HashMap, fmt, io};

use enum_as_inner::EnumAsInner;
pub use lower::lower;
use ustr::Ustr;

use crate::{
    ast::{AttrKind, BinOp, UnOp},
    common::{new_key_type, Word},
    db::{Db, DefId, ModuleId},
    span::{Span, Spanned},
    ty::{Instantiation, Ty, Typed},
};

#[derive(Debug, Clone)]
pub struct Hir {
    pub fns: Vec<Fn>,
    pub extern_lets: Vec<ExternLet>,
    pub lets: Vec<Let>,
}

impl Hir {
    pub fn new() -> Self {
        Self { fns: vec![], extern_lets: vec![], lets: vec![] }
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty_print::print(db, self, w)
    }
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
            ExprKind::If(if_) => {
                if_.cond.walk_(f);
                if_.then.walk_(f);
                if let Some(otherwise) = &if_.otherwise {
                    otherwise.walk_(f);
                }
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
            ExprKind::Name(_) | ExprKind::Lit(_) => (),
        }

        f(self);
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ExprKind {
    Let(Let),
    If(If),
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
    pub attrs: Attrs,
    pub sig: FnSig,
    pub kind: FnKind,
    pub span: Span,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum FnKind {
    Bare { body: Expr },
    Extern,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub ty_params: Vec<TyParam>,
    pub params: Vec<FnParam>,
    pub ret: Option<TyExpr>,
}

#[derive(Debug, Clone)]
pub struct TyParam {
    pub id: DefId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub id: DefId,
    pub ty_annot: TyExpr,
    pub span: Span,
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub module_id: ModuleId,
    pub attrs: Attrs,
    pub pat: Pat,
    pub ty_annot: Option<TyExpr>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ExternLet {
    pub module_id: ModuleId,
    pub id: DefId,
    pub attrs: Attrs,
    pub word: Word,
    pub ty_annot: TyExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pat {
    Name(NamePat),
    Discard(Span),
}

impl Pat {
    #[allow(unused)]
    pub fn walk(&self, mut f: impl FnMut(&NamePat)) {
        self.walk_(&mut f);
    }

    #[allow(unused)]
    fn walk_(&self, f: &mut impl FnMut(&NamePat)) {
        match self {
            Pat::Name(n) => f(n),
            Pat::Discard(_) => (),
        }
    }

    pub fn any(&self, mut f: impl FnMut(&NamePat) -> bool) -> bool {
        self.any_(&mut f)
    }

    fn any_(&self, f: &mut impl FnMut(&NamePat) -> bool) -> bool {
        match self {
            Pat::Name(n) => f(n),
            Pat::Discard(_) => false,
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
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Option<Box<Expr>>,
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
    pub target: TyExpr,
}

#[derive(Debug, Clone)]
pub struct Member {
    pub expr: Box<Expr>,
    pub member: Word,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: DefId,
    pub args: Option<Vec<TyExpr>>,
    pub instantiation: Instantiation,
}

#[derive(Debug, Clone)]
pub enum Lit {
    Str(Ustr),
    Int(u128),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub enum TyExpr {
    RawPtr(Box<TyExpr>, Span),
    Name(TyName),
    Unit(Span),
    Hole(Span),
}

#[derive(Debug, Clone)]
pub struct TyName {
    pub id: DefId,
    pub args: Vec<TyExpr>,
    pub span: Span,
}

impl Spanned for TyExpr {
    fn span(&self) -> Span {
        match self {
            TyExpr::Name(n) => n.span,
            Self::RawPtr(_, span) | TyExpr::Unit(span) | TyExpr::Hole(span) => *span,
        }
    }
}

new_key_type!(ExprId);

pub type HirMap<T> = HashMap<ExprId, T>;

pub type Attrs = Vec<Attr>;

#[derive(Debug, Clone)]
pub struct Attr {
    pub kind: AttrKind,
    pub value: Option<Expr>,
    pub span: Span,
}
