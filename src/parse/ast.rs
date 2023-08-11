use std::io;

use ustr::Ustr;

use crate::{
    common::QualifiedName,
    span::{SourceId, Span, Spanned},
};

use super::pretty_print;

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) source: SourceId,
    pub(crate) name: QualifiedName,
    pub(crate) is_main: bool,
    pub(crate) top_levels: Vec<TopLevel>,
}

impl Module {
    pub(crate) fn new(
        source_id: SourceId,
        name: QualifiedName,
        is_main: bool,
    ) -> Self {
        Self { source: source_id, name, is_main, top_levels: vec![] }
    }

    pub(crate) fn pretty_print(&self) -> io::Result<()> {
        pretty_print::print_module(self)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Ast {
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
pub(crate) enum TopLevel {
    Function(Function),
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub(crate) name: Ustr,
    pub(crate) body: Box<Ast>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) stmts: Vec<Statement>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    Return(Return),
    Expr(Ast),
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub(crate) expr: Option<Box<Ast>>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Call {
    pub(crate) callee: Box<Ast>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Name {
    pub(crate) name: Ustr,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Lit {
    pub(crate) kind: LitKind,
    pub(crate) span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LitKind {
    Int(usize),
    Unit,
}
