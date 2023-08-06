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
    pub(crate) bindings: Vec<Binding>,
}

impl Module {
    pub(crate) fn new(source_id: SourceId, name: QualifiedName, is_main: bool) -> Self {
        Self {
            source: source_id,
            name,
            is_main,
            bindings: vec![],
        }
    }

    pub(crate) fn pretty_print(&self) -> io::Result<()> {
        pretty_print::print_module(self)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Ast {
    Block(Block),
    Ret(Ret),
    Lit(Lit),
}

impl Spanned for Ast {
    fn span(&self) -> Span {
        match self {
            Self::Block(x) => x.span,
            Self::Ret(x) => x.span,
            Self::Lit(x) => x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub(crate) kind: BindingKind,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) enum BindingKind {
    Function(Function),
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub(crate) name: Ustr,
    // TODO: FunKind
    pub(crate) body: Box<Ast>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) exprs: Vec<Ast>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Ret {
    pub(crate) expr: Option<Box<Ast>>,
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
