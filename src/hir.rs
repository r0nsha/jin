mod lower;
mod pretty_print;
pub mod visit;

use std::{collections::HashMap, io};

use enum_as_inner::EnumAsInner;
pub use lower::lower;

use crate::{
    ast::BinOpKind,
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

    pub fn pretty_print(&self, db: &Db) -> io::Result<()> {
        println!();
        pretty_print::print(db, self)?;
        println!();
        Ok(())
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

impl Typed for Item {
    fn ty(&self) -> ty::Ty {
        self.kind.ty()
    }

    fn ty_mut(&mut self) -> &mut ty::Ty {
        self.kind.ty_mut()
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ItemKind {
    Fn(Fn),
}

impl Spanned for ItemKind {
    fn span(&self) -> Span {
        match self {
            Self::Fn(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Fn(x) => &mut x.span,
        }
    }
}

impl Typed for ItemKind {
    fn ty(&self) -> ty::Ty {
        match self {
            Self::Fn(x) => x.ty,
        }
    }

    fn ty_mut(&mut self) -> &mut ty::Ty {
        match self {
            Self::Fn(x) => &mut x.ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum ExprKind {
    If(If),
    Block(Block),
    Return(Return),
    Call(Call),
    Bin(BinOp),
    Name(Name),
    Lit(Lit),
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: DefId,
    pub sig: FnSig,
    pub body: Expr,
    pub span: Span,
    pub ty: ty::Ty, // TODO: Remove
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
pub struct BinOp {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOpKind,
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
    Never(Span),
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
            Ty::Unit(span) | Ty::Never(span) | Ty::Infer(span) => *span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Ty::Name(n) => &mut n.span,
            Ty::Unit(span) | Ty::Never(span) | Ty::Infer(span) => span,
        }
    }
}

new_key_type!(HirId);

pub type HirMap<T> = HashMap<HirId, T>;
