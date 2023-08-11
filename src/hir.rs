mod lower;
mod pretty_print;

use enum_as_inner::EnumAsInner;
pub(crate) use lower::lower;
use ustr::Ustr;

use crate::{
    db::{Database, ModuleId, SymbolId, TyId},
    span::{Span, Spanned},
};

#[derive(Debug, Clone)]
pub(crate) struct Hir {
    pub(crate) modules: Vec<Module>,
}

impl Hir {
    pub(crate) fn pretty_print(&self, db: &Database) {
        for module in &self.modules {
            pretty_print::print_module(db, module);
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) id: ModuleId,
    pub(crate) definitions: Vec<Definition>,
}

#[derive(Debug, Clone, EnumAsInner)]
pub(crate) enum Node {
    Function(Function),
    Block(Block),
    Return(Return),
    Name(Name),
    Lit(Lit),
}

impl Node {
    pub(crate) fn ty(&self) -> TyId {
        match self {
            Node::Function(x) => x.ty,
            Node::Block(x) => x.ty,
            Node::Return(x) => x.ty,
            Node::Name(x) => x.ty,
            Node::Lit(x) => x.ty,
        }
    }
}

impl Spanned for Node {
    fn span(&self) -> Span {
        match self {
            Node::Function(x) => x.span,
            Node::Block(x) => x.span,
            Node::Return(x) => x.span,
            Node::Name(x) => x.span,
            Node::Lit(x) => x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Definition {
    pub(crate) id: Option<SymbolId>,
    pub(crate) name: Ustr,
    pub(crate) kind: DefinitionKind,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) enum DefinitionKind {
    Function(Function),
}

impl DefinitionKind {
    pub(crate) fn ty(&self) -> TyId {
        match self {
            DefinitionKind::Function(x) => x.ty,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    pub(crate) id: Option<SymbolId>,
    pub(crate) name: Ustr,
    pub(crate) body: Block,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) exprs: Vec<Node>,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub(crate) expr: Option<Box<Node>>,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) struct Name {
    pub(crate) id: Option<SymbolId>,
    pub(crate) name: Ustr,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) struct Lit {
    pub(crate) kind: LitKind,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) enum LitKind {
    Int(usize),
    Unit,
}
