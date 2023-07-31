mod pretty_print;

use std::{io, path::Path};

use enum_as_inner::EnumAsInner;
use ustr::{ustr, Ustr};

use crate::{
    common::QualifiedName,
    span::{SourceId, Span, Spanned},
};

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) source_id: SourceId,
    pub(crate) name: QualifiedName,
    pub(crate) is_root: bool,
    pub(crate) bindings: Vec<Binding>,
}

impl Module {
    pub(crate) fn new(source_id: SourceId, name: QualifiedName, is_root: bool) -> Self {
        Self {
            source_id,
            name,
            is_root,
            bindings: vec![],
        }
    }

    pub(crate) fn pretty_print(&self) -> io::Result<()> {
        pretty_print::print_module(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Vis {
    Private,
    Public,
}

#[derive(Debug, Clone, EnumAsInner)]
pub(crate) enum Ast {
    Binding(Binding),
    Fun(Fun),
    Ret(Ret),
    Lit(Lit),
}

impl Spanned for Ast {
    fn span(&self) -> Span {
        match self {
            Self::Binding(binding) => binding.span,
            Self::Fun(fun) => fun.span,
            Self::Ret(ret) => ret.span,
            Self::Lit(lit) => lit.span,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub(crate) kind: BindingKind,
    pub(crate) span: Span,
}

impl Binding {
    // TODO: remove when we get patterns
    pub(crate) fn name(&self) -> Ustr {
        match &self.kind {
            BindingKind::Fun { name, .. } => *name,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum BindingKind {
    Fun { name: Ustr, fun: Box<Fun> },
}

#[derive(Debug, Clone)]
pub(crate) struct Fun {
    pub(crate) body: Box<Ast>,
    pub(crate) span: Span,
}

#[derive(Debug, Clone)]
pub(crate) struct Ret {
    pub(crate) value: Option<Box<Ast>>,
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
}
