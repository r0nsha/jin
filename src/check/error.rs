use ustr::Ustr;

use crate::{
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::{Ty, TyVar},
};

pub(super) enum CheckError {
    TyNotEq {
        expected: TypeId,
        actual: TypeId,
    },
    InfiniteTy {
        ty: TypeId,
        var: TyVar,
    },
    MisplacedReturn {
        span: Span,
    },
    DuplicateName {
        name: Ustr,
        prev_span: Span,
        dup_span: Span,
    },
}

impl From<CheckError> for Diagnostic {
    fn from(err: CheckError) -> Self {
        match err {
            CheckError::TyNotEq { expected, actual } => {
                Diagnostic::error("check::incompatible_types")
                    .with_message(format!("expected `{expected}`, got `{actual}` instead"))
                    .with_label(
                        Label::primary(expected.span)
                            .with_message("expected type `{expected}` originates here"),
                    )
                    .with_label(
                        Label::secondary(actual.span).with_message("found type `{actual}` here"),
                    )
            }
            CheckError::InfiniteTy { ty, .. } => Diagnostic::error("check::infinite_type")
                .with_message(format!("type `{ty}` has an infinite size"))
                .with_label(Label::primary(ty.span)),
            CheckError::MisplacedReturn { span } => Diagnostic::error("check::misplaced_return")
                .with_message("cannot return outside of function scope")
                .with_label(Label::primary(span)),
            CheckError::DuplicateName {
                name,
                prev_span,
                dup_span,
            } => Diagnostic::error("check::duplicate_names")
                .with_message(format!("the name `{name}` is defined multiple times"))
                .with_label(
                    Label::secondary(prev_span)
                        .with_message(format!("previous definition of `{name}` is here")),
                )
                .with_label(
                    Label::primary(dup_span).with_message(format!("`{name}` is redefined here")),
                )
                .with_help("you can only define names once in a module"),
        }
    }
}
