mod lower;
mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
pub use lower::lower;
use ustr::Ustr;

use crate::{
    db::{Database, DefinitionId, ModuleId, TyId},
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
    pub definitions: Vec<Definition>,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Node {
    #[allow(unused)]
    Function(Function),
    Block(Block),
    Return(Return),
    Call(Call),
    Name(Name),
    Lit(Lit),
}

impl Node {
    pub fn ty(&self) -> TyId {
        match self {
            Self::Function(x) => x.ty,
            Self::Block(x) => x.ty,
            Self::Return(x) => x.ty,
            Self::Call(x) => x.ty,
            Self::Name(x) => x.ty,
            Self::Lit(x) => x.ty,
        }
    }
}

impl Spanned for Node {
    fn span(&self) -> Span {
        match self {
            Self::Function(x) => x.span,
            Self::Block(x) => x.span,
            Self::Return(x) => x.span,
            Self::Call(x) => x.span,
            Self::Name(x) => x.span,
            Self::Lit(x) => x.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Function(x) => &mut x.span,
            Self::Block(x) => &mut x.span,
            Self::Return(x) => &mut x.span,
            Self::Call(x) => &mut x.span,
            Self::Name(x) => &mut x.span,
            Self::Lit(x) => &mut x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Definition {
    pub id: Option<DefinitionId>,
    pub name: Ustr,
    pub kind: DefinitionKind,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub enum DefinitionKind {
    Function(Function),
}

impl DefinitionKind {
    pub fn ty(&self) -> TyId {
        match self {
            Self::Function(x) => x.ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub id: Option<DefinitionId>,
    pub name: Ustr,
    pub body: Block,
    pub params: IndexMap<Ustr, FunctionParam>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub id: Option<DefinitionId>,
    pub name: Ustr,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub exprs: Vec<Node>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Box<Node>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Node>,
    pub span: Span,
    pub ty: TyId,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub id: Option<DefinitionId>,
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
    Unit,
}
