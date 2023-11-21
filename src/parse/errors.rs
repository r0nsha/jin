use crate::{
    ast::token::TokenKind,
    diagnostics::{Diagnostic, Label},
    span::Span,
};

pub fn path_not_found(path: &str, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("path `{path}` not found"))
        .with_label(Label::primary(span).with_message("not found"))
}

pub fn unexpected_token_err(expected: &str, found: TokenKind, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("expected {expected}, found {found}"))
        .with_label(Label::primary(span).with_message("found here"))
}
