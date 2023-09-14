use ustr::Ustr;

use crate::{
    common::Word,
    diagnostics::{Diagnostic, Label},
    span::{Span, Spanned},
};

pub(super) enum ResolveError {
    MultipleItems { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleParams { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleTyParams { name: Ustr, prev_span: Span, dup_span: Span },
    NameNotFound(Word),
    InvalidReturn(Span),
    InvalidInferTy(Span),
}

impl From<ResolveError> for Diagnostic {
    fn from(err: ResolveError) -> Self {
        match err {
            ResolveError::MultipleItems { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_items")
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
            ResolveError::MultipleTyParams { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_type_params")
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
            ResolveError::MultipleParams { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_params")
                    .with_message(format!("the name `{name}` is already used as a parameter name"))
                    .with_label(
                        Label::primary(dup_span).with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span).with_message(format!("first use of `{name}`")),
                    )
            }
            ResolveError::NameNotFound(name) => Self::error("resolve::name_not_found")
                .with_message(format!("cannot find `{name}` in this scope"))
                .with_label(Label::primary(name.span()).with_message("not found in this scope")),
            ResolveError::InvalidReturn(span) => Self::error("resolve::invalid_return")
                .with_message("cannot return outside of function scope")
                .with_label(Label::primary(span)),
            ResolveError::InvalidInferTy(span) => Self::error("resolve::invalid_infer_type")
                .with_message("cannot use a _ type in a function's signature")
                .with_label(Label::primary(span)),
        }
    }
}
