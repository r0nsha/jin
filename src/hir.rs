mod pretty_print;

use std::io;

use enum_as_inner::EnumAsInner;

use crate::{
    ast::{self, Vis},
    database::ModuleId,
    span::{Span, Spanned},
    ty::{Ty, Typed},
};

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) id: ModuleId,
}

impl From<&ast::Module> for Module {
    fn from(module: &ast::Module) -> Self {
        Self {
            id: ModuleId::null(),
        }
    }
}

#[derive(Debug, Clone, EnumAsInner)]
pub(crate) enum Hir {
    Ret(Ret),
    Const(Const),
}

impl Hir {
    pub(crate) fn pretty_print(&self) -> io::Result<()> {
        todo!()
        // pretty_print::print_hir(self)
    }
}

impl Spanned for Hir {
    fn span(&self) -> Span {
        todo!()
    }
}

impl Typed for Hir {
    fn ty(&self) -> &crate::ty::Ty {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Block {
    pub(crate) statements: Vec<Hir>,
    pub(crate) span: Span,
    pub(crate) ty: Ty,
}

#[derive(Debug, Clone)]
pub(crate) struct Ret {
    pub(crate) value: Option<Box<Hir>>,
    pub(crate) span: Span,
    pub(crate) ty: Ty,
}

#[derive(Debug, Clone)]
pub(crate) struct Const {
    pub(crate) kind: ConstKind,
    pub(crate) span: Span,
    pub(crate) ty: Ty,
}

#[derive(Debug, Clone)]
pub(crate) enum ConstKind {
    Int(usize),
    Unit,
}
