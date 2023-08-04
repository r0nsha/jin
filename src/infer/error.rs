use crate::{
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::{Type, TypeVar},
};

pub(crate) enum InferError {
    TypesNotEq { expected: Type, actual: Type },
    InfiniteType { ty: Type, var: TypeVar },
    MisplacedReturn { span: Span }, // TODO: move to another pass
}

impl From<InferError> for Diagnostic {
    fn from(err: InferError) -> Self {
        match err {
            InferError::TypesNotEq { expected, actual } => {
                Diagnostic::error("infer::incompatible_types")
                    .with_message(format!("expected `{expected}`, got `{actual}` instead"))
                    .with_label(
                        Label::primary(expected.span)
                            .with_message(format!("expected type `{expected}` originates here")),
                    )
                    .with_label(
                        Label::secondary(actual.span)
                            .with_message(format!("found type `{actual}` here")),
                    )
            }
            InferError::InfiniteType { ty, .. } => Diagnostic::error("infer::infinite_type")
                .with_message(format!("type `{ty}` has an infinite size"))
                .with_label(Label::primary(ty.span)),
            InferError::MisplacedReturn { span } => Diagnostic::error("infer::misplaced_return")
                .with_message("cannot return outside of function scope")
                .with_label(Label::primary(span)),
        }
    }
}
