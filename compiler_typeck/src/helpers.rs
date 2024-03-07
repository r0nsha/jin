use compiler_ast as ast;
use compiler_core::hir;

pub(crate) fn trans_let_kind(kind: &ast::LetKind) -> hir::LetKind {
    match kind {
        ast::LetKind::Let => hir::LetKind::Let,
        ast::LetKind::Const => hir::LetKind::Const,
    }
}
