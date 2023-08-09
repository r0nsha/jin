use crate::{
    diagnostics::{Diagnostic, Label},
    ty::{Ty, TyVar},
};

pub(crate) enum InferError {
    TypesNotEq { expected: Ty, actual: Ty },
    InfiniteType { ty: Ty, var: TyVar },
}

impl From<InferError> for Diagnostic {
    fn from(err: InferError) -> Self {
        match err {
            InferError::TypesNotEq { expected, actual } => {
                Diagnostic::error("infer::incompatible_types")
                    .with_message(format!(
                        "expected `{expected}`, got `{actual}` instead"
                    ))
                    .with_label(Label::primary(expected.span).with_message(
                        format!("expected type `{expected}` originates here"),
                    ))
                    .with_label(
                        Label::secondary(actual.span).with_message(format!(
                            "found type `{actual}` here"
                        )),
                    )
            }
            InferError::InfiniteType { ty, .. } => {
                Diagnostic::error("infer::infinite_type")
                    .with_message(format!("type `{ty}` has an infinite size"))
                    .with_label(Label::primary(ty.span))
            }
        }
    }
}
