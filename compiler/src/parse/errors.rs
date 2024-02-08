use crate::{
    diagnostics::{Diagnostic, Label},
    parse::token::TokenKind,
    span::Span,
};

pub fn path_not_found(path: &str, span: Span) -> Diagnostic {
    Diagnostic::error(format!("path `{path}` not found"))
        .with_label(Label::primary(span, "not found"))
}

pub fn unexpected_token_err(expected: &str, found: TokenKind, span: Span) -> Diagnostic {
    Diagnostic::error(format!("expected {expected}, found {found}"))
        .with_label(Label::primary(span, "found here"))
}

pub fn invalid_c_variadic(span: Span) -> Diagnostic {
    Diagnostic::error("non extern function cannot use c varargs")
        .with_label(Label::primary(span, "here"))
}
