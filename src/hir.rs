mod lower;
mod pretty_print;

use std::{collections::HashMap, fmt, io};

use enum_as_inner::EnumAsInner;
pub use lower::lower;

use crate::{
    ast::{BinOp, UnOp},
    common::{new_key_type, Word},
    db::{Db, DefId},
    span::{Span, Spanned},
    ty::{self, Typed},
};

#[derive(Debug, Clone)]
pub struct Hir {
    pub items: Vec<Item>,
}

impl Hir {
    pub fn new() -> Self {
        Self { items: vec![] }
    }

    pub fn pretty_print(&self, db: &Db, w: &mut impl io::Write) -> io::Result<()> {
        pretty_print::print(db, self, w)
    }
}

#[derive(Debug, Clone)]
pub struct Item {
    pub kind: ItemKind,
    pub ty: ty::Ty,
}

impl Spanned for Item {
    fn span(&self) -> Span {
        self.kind.span()
    }

    fn span_mut(&mut self) -> &mut Span {
        self.kind.span_mut()
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ItemKind {
    Fn(Fn),
    Let(Let),
}

impl Spanned for ItemKind {
    fn span(&self) -> Span {
        match self {
            Self::Fn(x) => x.span,
            Self::Let(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Fn(x) => &mut x.span,
            Self::Let(x) => &mut x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
    pub span: Span,
    pub ty: ty::Ty,
}

impl Expr {
    pub fn rewrite(self, mut f: impl FnMut(Self) -> Self) -> Self {
        self.rewrite_(&mut f)
    }

    fn rewrite_(self, f: &mut impl FnMut(Self) -> Self) -> Self {
        let kind = match self.kind {
            ExprKind::Let(let_) => ExprKind::Let(Let {
                pat: let_.pat.clone(),
                ty_annot: let_.ty_annot.clone(),
                value: Box::new(let_.value.rewrite_(f)),
                span: let_.span,
            }),
            ExprKind::If(if_) => ExprKind::If(If {
                cond: Box::new(if_.cond.rewrite_(f)),
                then: Box::new(if_.then.rewrite_(f)),
                otherwise: if_.otherwise.map(|o| Box::new(o.rewrite_(f))),
            }),
            ExprKind::Block(blk) => ExprKind::Block(Block {
                exprs: blk.exprs.into_iter().map(|expr| expr.rewrite_(f)).collect(),
            }),
            ExprKind::Return(ret) => {
                ExprKind::Return(Return { expr: Box::new(ret.expr.rewrite_(f)) })
            }
            ExprKind::Call(call) => ExprKind::Call(Call {
                callee: Box::new(call.callee.rewrite_(f)),
                args: call
                    .args
                    .into_iter()
                    .map(|arg| CallArg {
                        name: arg.name,
                        expr: arg.expr.rewrite_(f),
                        index: arg.index,
                    })
                    .collect(),
            }),
            ExprKind::Unary(un) => {
                ExprKind::Unary(Unary { expr: Box::new(un.expr.rewrite_(f)), op: un.op })
            }
            ExprKind::Binary(bin) => ExprKind::Binary(Binary {
                lhs: Box::new(bin.lhs.rewrite_(f)),
                rhs: Box::new(bin.rhs.rewrite_(f)),
                op: bin.op,
            }),
            ExprKind::Cast(cast) => {
                ExprKind::Cast(Cast { expr: Box::new(cast.expr.rewrite_(f)), ty: cast.ty.clone() })
            }
            kind => kind.clone(),
        };

        f(Expr { id: self.id, kind, span: self.span, ty: self.ty })
    }

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
            ExprKind::Name(_) | ExprKind::Lit(_) => (),
        }

        f(self);
    }

    pub fn walk_mut(&mut self, mut f: impl FnMut(&mut Expr)) {
        self.walk_mut_(&mut f);
    }

    fn walk_mut_(&mut self, f: &mut impl FnMut(&mut Expr)) {
        match &mut self.kind {
            ExprKind::Let(let_) => let_.value.walk_mut_(f),
            ExprKind::If(if_) => {
                if_.cond.walk_mut_(f);
                if_.then.walk_mut_(f);
                if let Some(otherwise) = &mut if_.otherwise {
                    otherwise.walk_mut_(f);
                }
            }
            ExprKind::Block(blk) => {
                for expr in &mut blk.exprs {
                    expr.walk_mut_(f);
                }
            }
            ExprKind::Return(ret) => ret.expr.walk_mut_(f),
            ExprKind::Call(call) => {
                call.callee.walk_mut_(f);

                for arg in &mut call.args {
                    arg.expr.walk_mut_(f);
                }
            }
            ExprKind::Unary(un) => un.expr.walk_mut_(f),
            ExprKind::Binary(bin) => {
                bin.lhs.walk_mut_(f);
                bin.rhs.walk_mut_(f);
            }
            ExprKind::Cast(cast) => cast.expr.walk_mut_(f),
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
    Name(Name),
    Lit(Lit),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: DefId,
    pub sig: FnSig,
    pub body: Expr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub ty_params: Vec<TyParam>,
    pub params: Vec<FnParam>,
    pub ret: Option<Ty>,
}

#[derive(Debug, Clone)]
pub struct TyParam {
    pub id: DefId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub id: DefId,
    pub ty_annot: Ty,
    pub span: Span,
    pub ty: ty::Ty, // TODO: Remove
}

#[derive(Debug, Clone)]
pub struct Let {
    pub pat: Pat,
    pub ty_annot: Option<Ty>,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Pat {
    Name(NamePat),
    Ignore(Span),
}

impl Pat {
    pub fn walk(&self, mut f: impl FnMut(&NamePat)) {
        self.walk_(&mut f);
    }

    fn walk_(&self, f: &mut impl FnMut(&NamePat)) {
        match self {
            Pat::Name(n) => f(n),
            Pat::Ignore(_) => (),
        }
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
            Pat::Ignore(_) => f.write_str("_"),
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
    fn ty(&self) -> ty::Ty {
        self.expr.ty
    }

    fn ty_mut(&mut self) -> &mut ty::Ty {
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
    pub ty: Ty,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: DefId,
    pub args: Option<Vec<Ty>>,
    pub instantiation: ty::Instantiation, // TODO: Turn into an Option<_>
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
}

#[derive(Debug, Clone)]
pub enum LitKind {
    Int(usize),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub enum Ty {
    Name(TyName),
    Unit(Span),
    Infer(Span),
}

#[derive(Debug, Clone)]
pub struct TyName {
    pub id: DefId,
    pub args: Vec<Ty>,
    pub span: Span,
}

impl Spanned for Ty {
    fn span(&self) -> Span {
        match self {
            Ty::Name(n) => n.span,
            Ty::Unit(span) | Ty::Infer(span) => *span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Ty::Name(n) => &mut n.span,
            Ty::Unit(span) | Ty::Infer(span) => span,
        }
    }
}

new_key_type!(ExprId);

pub type HirMap<T> = HashMap<ExprId, T>;
