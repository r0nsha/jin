mod lower;
mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;
pub use lower::lower;

use crate::{
    ast::BinOp,
    common::Word,
    db::{Db, DefId},
    span::{Span, Spanned},
    ty::{self, Typed},
};

#[derive(Debug, Clone)]
pub struct Hir {
    pub items: Vec<Item>,
}

impl Hir {
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
    Item(Item),
    If(If),
    Block(Block),
    Return(Return),
    Call(Call),
    Bin(Bin),
    Name(Name),
    Lit(Lit),
}

impl Typed for Expr {
    fn ty(&self) -> ty::Ty {
        match self {
            Self::Item(x) => x.ty(),
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
            Self::Item(x) => x.ty_mut(),
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
            Self::Item(x) => x.span(),
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
            Self::Item(x) => x.span_mut(),
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
    pub body: Block,
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
pub struct Bin {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOp,
    pub span: Span,
    pub ty: ty::Ty,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: DefId,
    pub args: Vec<ty::Ty>,
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

pub trait HirVisitor: Sized {
    fn visit_item(&mut self, item: &Item) {
        noop_visit_item(self, item);
    }

    fn visit_fn(&mut self, f: &Fn) {
        noop_visit_fn(self, f);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        noop_visit_expr(self, expr);
    }

    fn visit_if(&mut self, if_: &If) {
        noop_visit_if(self, if_);
    }

    fn visit_block(&mut self, blk: &Block) {
        noop_visit_block(self, blk);
    }

    fn visit_return(&mut self, ret: &Return) {
        noop_visit_return(self, ret);
    }

    fn visit_call(&mut self, call: &Call) {
        noop_visit_call(self, call);
    }

    fn visit_bin(&mut self, bin: &Bin) {
        noop_visit_bin(self, bin);
    }

    fn visit_name(&mut self, name: &Name) {
        noop_visit_name(self, name);
    }

    fn visit_lit(&mut self, lit: &Lit) {
        noop_visit_lit(self, lit);
    }
}

pub fn noop_visit_item(vis: &mut impl HirVisitor, item: &Item) {
    match &item.kind {
        ItemKind::Fn(f) => vis.visit_fn(f),
    }
}

pub fn noop_visit_fn(vis: &mut impl HirVisitor, f: &Fn) {
    vis.visit_block(&f.body);
}

pub fn noop_visit_expr(vis: &mut impl HirVisitor, expr: &Expr) {
    match expr {
        Expr::Item(x) => vis.visit_item(x),
        Expr::If(x) => vis.visit_if(x),
        Expr::Block(x) => vis.visit_block(x),
        Expr::Return(x) => vis.visit_return(x),
        Expr::Call(x) => vis.visit_call(x),
        Expr::Bin(x) => vis.visit_bin(x),
        Expr::Name(x) => vis.visit_name(x),
        Expr::Lit(x) => vis.visit_lit(x),
    }
}

pub fn noop_visit_if(vis: &mut impl HirVisitor, if_: &If) {
    vis.visit_expr(&if_.cond);
    vis.visit_expr(&if_.then);
    if let Some(o) = &if_.otherwise {
        vis.visit_expr(o);
    }
}

pub fn noop_visit_block(vis: &mut impl HirVisitor, blk: &Block) {
    for expr in &blk.exprs {
        vis.visit_expr(expr);
    }
}

pub fn noop_visit_return(vis: &mut impl HirVisitor, ret: &Return) {
    vis.visit_expr(&ret.expr);
}

pub fn noop_visit_call(vis: &mut impl HirVisitor, call: &Call) {
    vis.visit_expr(&call.callee);

    for arg in &call.args {
        vis.visit_expr(&arg.expr);
    }
}

pub fn noop_visit_bin(vis: &mut impl HirVisitor, bin: &Bin) {
    vis.visit_expr(&bin.lhs);
    vis.visit_expr(&bin.rhs);
}

pub fn noop_visit_name(_: &mut impl HirVisitor, _: &Name) {}

pub fn noop_visit_lit(_: &mut impl HirVisitor, _: &Lit) {}
