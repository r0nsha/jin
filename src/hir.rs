mod lower;
mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
pub use lower::lower;
use ustr::Ustr;

use crate::{
    ast::BinaryOp,
    db::{Database, DefId, ModuleId, TyId},
    span::{Span, Spanned},
};

#[derive(Debug, Clone)]
pub struct Hir {
    pub modules: Vec<Module>,
}

impl Hir {
    pub fn pretty_print(&self, db: &Database) -> io::Result<()> {
        println!();
        println!("HIR:");
        println!();

        for module in &self.modules {
            pretty_print::print_module(db, module)?;
        }

        println!();

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub id: ModuleId,
    pub definitions: Vec<Def>,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Expr {
    #[allow(unused)]
    Function(Function),
    If(If),
    Block(Block),
    Return(Return),
    Call(Call),
    Binary(Binary),
    Name(Name),
    Lit(Lit),
}

impl Expr {
    pub fn ty(&self) -> TyId {
        match self {
            Self::Function(x) => x.ty,
            Self::If(x) => x.ty,
            Self::Block(x) => x.ty,
            Self::Return(x) => x.ty,
            Self::Call(x) => x.ty,
            Self::Binary(x) => x.ty,
            Self::Name(x) => x.ty,
            Self::Lit(x) => x.ty,
        }
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Function(x) => x.span,
            Self::If(x) => x.span,
            Self::Block(x) => x.span,
            Self::Return(x) => x.span,
            Self::Call(x) => x.span,
            Self::Binary(x) => x.span,
            Self::Name(x) => x.span,
            Self::Lit(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Function(x) => &mut x.span,
            Self::If(x) => &mut x.span,
            Self::Block(x) => &mut x.span,
            Self::Return(x) => &mut x.span,
            Self::Call(x) => &mut x.span,
            Self::Binary(x) => &mut x.span,
            Self::Name(x) => &mut x.span,
            Self::Lit(x) => &mut x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Def {
    pub id: Option<DefId>,
    pub name: Ustr,
    pub kind: DefKind,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub enum DefKind {
    Function(Function),
}

impl DefKind {
    pub fn ty(&self) -> TyId {
        match self {
            Self::Function(x) => x.ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: Option<DefId>,
    pub name: Ustr,
    pub body: Block,
    pub params: IndexMap<Ustr, FunctionParam>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub id: Option<DefId>,
    pub name: Ustr,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Option<Box<Expr>>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Expr>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Box<Expr>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinaryOp,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: Option<DefId>,
    pub name: Ustr,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub enum LitKind {
    Int(usize),
    Bool(bool),
    Unit,
}
