mod pretty_print;

use std::{io, path::Path};

use enum_as_inner::EnumAsInner;
use ustr::{ustr, Ustr};

use crate::span::{SourceId, Span, Spanned};

#[derive(Debug, Clone)]
pub struct Module {
    pub source_id: SourceId,
    pub name: QualifiedName,
    pub is_root: bool,
    pub bindings: Vec<Binding>,
}

impl Module {
    pub fn new(source_id: SourceId, name: QualifiedName, is_root: bool) -> Self {
        Self {
            source_id,
            name,
            is_root,
            bindings: vec![],
        }
    }

    pub fn pretty_print(&self) -> io::Result<()> {
        pretty_print::print_module(self)
    }
}

#[derive(Debug, Clone)]
pub struct QualifiedName(Vec<Ustr>);

impl QualifiedName {
    pub fn new(full_name: Vec<Ustr>) -> Self {
        assert!(!full_name.is_empty());
        Self(full_name)
    }

    pub fn from_path(root: &Path, target: &Path) -> Option<Self> {
        let target = target.with_extension("");
        let stripped = target.strip_prefix(root).ok()?;

        Some(Self(
            stripped
                .iter()
                .map(|component| ustr(component.to_string_lossy().as_ref()))
                .collect(),
        ))
    }

    pub fn name(&self) -> Ustr {
        *self.0.last().unwrap()
    }

    pub fn full_name(&self, separator: &str) -> String {
        self.0
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<_>>()
            .join(separator)
    }

    pub fn standard_full_name(&self) -> String {
        self.full_name(".")
    }

    pub fn child(mut self, name: Ustr) -> Self {
        self.0.push(name);
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Vis {
    Private,
    Public,
}

#[derive(Debug, Clone, EnumAsInner)]
pub enum Ast {
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
pub struct Binding {
    pub kind: BindingKind,
    pub span: Span,
}

impl Binding {
    // TODO: remove when we get patterns
    pub fn name(&self) -> Ustr {
        match &self.kind {
            BindingKind::Fun { name, .. } => *name,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BindingKind {
    Fun { name: Ustr, fun: Box<Fun> },
}

#[derive(Debug, Clone)]
pub struct Fun {
    pub body: Box<Ast>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Ret {
    pub value: Option<Box<Ast>>,
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
}
