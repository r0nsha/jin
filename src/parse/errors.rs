use crate::{
    diagnostics::{Diagnostic, Label},
    span::Span,
};

pub fn path_not_found(path: &str, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("path `{path}` not found"))
        .with_label(Label::primary(span).with_message("not found"))
}
