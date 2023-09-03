mod lower;
mod pretty_print;
pub mod visit;

use std::io;

use enum_as_inner::EnumAsInner;
pub use lower::lower;

use crate::{
    ast::BinOpKind,
    common::{new_key_type, IndexVec, Word},
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

#[derive(Debug, Clone, EnumAsInner)]
pub enum Expr {
    If(If),
    Block(Block),
    Return(Return),
    Call(Call),
    Bin(BinOp),
    Name(Name),
    Lit(Lit),
}

impl Typed for Expr {
    fn ty(&self) -> ty::Ty {
        match self {
            Self::If(x) => x.ty,
            Self::Block(x) => x.ty,
            Self::Return(x) => x.ty,
            Self::Call(x) => x.ty,
            Self::Bin(x) => x.ty,
            Self::Name(x) => x.ty,
            Self::Lit(x) => x.ty,
        }
    }

    fn ty_mut(&mut self) -> &mut ty::Ty {
        match self {
            Self::If(x) => &mut x.ty,
            Self::Block(x) => &mut x.ty,
            Self::Return(x) => &mut x.ty,
            Self::Call(x) => &mut x.ty,
            Self::Bin(x) => &mut x.ty,
            Self::Name(x) => &mut x.ty,
            Self::Lit(x) => &mut x.ty,
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::If(x) => x.span,
            Self::Block(x) => x.span,
            Self::Return(x) => x.span,
            Self::Call(x) => x.span,
            Self::Bin(x) => x.span,
            Self::Name(x) => x.span,
            Self::Lit(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::If(x) => &mut x.span,
            Self::Block(x) => &mut x.span,
            Self::Return(x) => &mut x.span,
            Self::Call(x) => &mut x.span,
            Self::Bin(x) => &mut x.span,
            Self::Name(x) => &mut x.span,
            Self::Lit(x) => &mut x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub id: DefId,
    pub sig: FnSig,
    pub body: Expr,
    pub span: Span,
    pub ty: ty::Ty,
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
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Option<Box<Expr>>,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Box<Expr>,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub args: Vec<CallArg>,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct CallArg {
    pub name: Option<Word>,
    pub expr: Expr,
    pub index: Option<usize>,
}

impl Typed for CallArg {
    fn ty(&self) -> ty::Ty {
        self.expr.ty()
    }

    fn ty_mut(&mut self) -> &mut ty::Ty {
        self.expr.ty_mut()
    }
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOpKind,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: DefId,
    pub args: Vec<Ty>,
    pub instantiation: ty::Instantiation,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
    pub ty: ty::Ty,
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

pub type HirMap<T> = IndexVec<HirId, T>;
