mod lower;
mod pretty_print;

pub(crate) use lower::lower;
use ustr::Ustr;

use std::io;

use crate::{
    db::{Database, FunId, ModuleId, SymbolId, TypeId},
    span::{Span, Spanned},
};

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) id: ModuleId,
    pub(crate) bindings: Vec<Binding>,
}

impl Module {
    pub(crate) fn pretty_print(&self, db: &Database) -> io::Result<()> {
        pretty_print::print_module(db, self)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Hir {
    Fun(Fun),
    Ret(Ret),
    Lit(Lit),
}

impl Hir {
    pub(crate) fn ty(&self) -> TypeId {
        match self {
            Hir::Fun(x) => x.ty,
            Hir::Ret(x) => x.ty,
            Hir::Lit(x) => x.ty,
        }
    }
}

impl Spanned for Hir {
    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Binding {
    pub(crate) id: SymbolId,
    pub(crate) name: Ustr,
    pub(crate) expr: Box<Hir>,
    pub(crate) span: Span,
    pub(crate) ty: TypeId,
}

#[derive(Debug, Clone)]
pub(crate) struct Fun {
    pub(crate) id: FunId,
    pub(crate) name: Ustr,
    pub(crate) body: Block,
    pub(crate) span: Span,
    pub(crate) ty: TypeId,
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Hir>,
    pub(crate) span: Span,
    pub(crate) ty: TypeId,
}

#[derive(Debug, Clone)]
pub(crate) struct Ret {
    pub(crate) expr: Option<Box<Hir>>,
    pub(crate) span: Span,
    pub(crate) ty: TypeId,
}

#[derive(Debug, Clone)]
pub(crate) struct Lit {
    pub(crate) kind: LitKind,
    pub(crate) span: Span,
    pub(crate) ty: TypeId,
}

#[derive(Debug, Clone)]
pub(crate) enum LitKind {
    Int(usize),
    Unit,
}
