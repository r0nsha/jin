mod pretty_print;
pub mod token;

use std::io;

use ustr::Ustr;

use crate::{
    common::QualifiedName,
    span::{SourceId, Span, Spanned},
};

#[derive(Debug, Clone)]
pub struct Library {
    name: Ustr,
    is_main: bool,
    pub modules: Vec<Module>,
}

impl Library {
    pub fn new(name: Ustr, is_main: bool, modules: Vec<Module>) -> Self {
        Self { name, is_main, modules }
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }

    pub fn name(&self) -> Ustr {
        self.name
    }

    pub fn pretty_print(&self) -> io::Result<()> {
        println!();
        println!("{}:", self.name());
        println!();

        for module in &self.modules {
            pretty_print::print_module(module)?;
        }

        println!();

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub source: SourceId,
    pub name: QualifiedName,
    pub top_levels: Vec<TopLevel>,
    is_main: bool,
}

impl Module {
    pub fn new(
        source_id: SourceId,
        name: QualifiedName,
        is_main: bool,
    ) -> Self {
        Self { source: source_id, name, is_main, top_levels: vec![] }
    }

    pub fn pretty_print(&self) -> io::Result<()> {
        pretty_print::print_module(self)
    }

    pub fn is_main(&self) -> bool {
        self.is_main
    }
}

#[derive(Debug, Clone)]
pub enum Ast {
    Block(Block),
    Call(Call),
    Name(Name),
    Lit(Lit),
}

impl Spanned for Ast {
    fn span(&self) -> Span {
        match self {
            Self::Block(x) => x.span,
            Self::Call(x) => x.span,
            Self::Name(x) => x.span,
            Self::Lit(x) => x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ustr,
    pub body: Box<Ast>,
    pub params: Vec<FunctionParam>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionParam {
    pub name: Ustr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Return),
    Expr(Ast),
}

#[derive(Debug, Clone)]
pub struct Return {
    pub expr: Option<Box<Ast>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Name {
    pub name: Ustr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub kind: LitKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LitKind {
    Int(usize),
    Unit,
}
