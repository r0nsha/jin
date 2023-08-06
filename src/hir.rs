mod lower;
mod pretty_print;

use enum_as_inner::EnumAsInner;
pub(crate) use lower::lower;
use ustr::Ustr;

use crate::{
    db::{Database, FunctionId, ModuleId, SymbolId, TyId},
    span::{Span, Spanned},
};

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) id: ModuleId,
    pub(crate) definitions: Vec<Definition>,
}

impl Module {
    pub(crate) fn pretty_print(&self, db: &Database) {
        pretty_print::print_module(db, self)
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub(crate) enum Hir {
    Function(Function),
    Block(Block),
    Return(Return),
    Lit(Lit),
}

impl Hir {
    pub(crate) fn ty(&self) -> TyId {
        match self {
            Hir::Function(x) => x.ty,
            Hir::Block(x) => x.ty,
            Hir::Return(x) => x.ty,
            Hir::Lit(x) => x.ty,
        }
    }
}

impl Spanned for Hir {
    fn span(&self) -> Span {
        match self {
            Hir::Function(x) => x.span,
            Hir::Block(x) => x.span,
            Hir::Return(x) => x.span,
            Hir::Lit(x) => x.span,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Definition {
    pub(crate) id: SymbolId,
    pub(crate) name: Ustr, // TODO: remove?
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
    pub(crate) id: FunctionId,
    pub(crate) name: Ustr, // TODO: remove?
    pub(crate) body: Block,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) exprs: Vec<Hir>,
    pub(crate) span: Span,
    pub(crate) ty: TyId,
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub(crate) expr: Option<Box<Hir>>,
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
