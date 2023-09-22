use ustr::Ustr;

use crate::{
    common::Word,
    db::Db,
    diagnostics::{Diagnostic, Label},
    span::{Span, Spanned},
};

#[derive(Debug)]
pub(super) enum ResolveError {
    MultipleItems { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleParams { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleTyParams { name: Ustr, prev_span: Span, dup_span: Span },
    NameNotFound(Word),
    InvalidInferTy(Span),
}

impl ResolveError {
    pub fn into_diagnostic(self, _db: &Db) -> Diagnostic {
        match self {
            Self::MultipleItems { name, prev_span, dup_span } => {
                Diagnostic::error("check::multiple_items")
                    .with_message(format!("the item `{name}` is defined multiple times"))
                    .with_label(
                        Label::primary(dup_span)
                            .with_message(format!("`{name}` defined again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span)
                            .with_message(format!("first definition of `{name}`")),
                    )
                    .with_help("you can only define items once in a module")
            }
            Self::MultipleTyParams { name, prev_span, dup_span } => {
                Diagnostic::error("check::multiple_type_params")
                    .with_message(format!(
                        "the name `{name}` is already used as a type parameter name"
                    ))
                    .with_label(
                        Label::primary(dup_span).with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span).with_message(format!("first use of `{name}`")),
                    )
            }
            Self::MultipleParams { name, prev_span, dup_span } => {
                Diagnostic::error("check::multiple_params")
                    .with_message(format!("the name `{name}` is already used as a parameter name"))
                    .with_label(
                        Label::primary(dup_span).with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span).with_message(format!("first use of `{name}`")),
                    )
            }
            Self::NameNotFound(name) => Diagnostic::error("check::name_not_found")
                .with_message(format!("cannot find `{name}` in this scope"))
                .with_label(Label::primary(name.span()).with_message("not found in this scope")),
            Self::InvalidInferTy(span) => Diagnostic::error("check::invalid_infer_type")
                .with_message("cannot use a _ type in a function's signature")
                .with_label(Label::primary(span)),
        }
    }
}
