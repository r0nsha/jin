use miette::Diagnostic;
use thiserror::Error;
use ustr::Ustr;

use crate::{
    span::{Span, Spanned},
    ty::{Ty, TyVar},
};

#[derive(Error, Diagnostic, Debug)]
pub(super) enum CheckError {
    #[error("expected `{expected}`, got `{actual}` instead")]
    #[diagnostic(code(check::incompatible_types))]
    TyNotEq {
        #[label("expected type `{expected}` originates here")]
        expected: Ty,
        #[label("found type `{actual}` here")]
        actual: Ty,
    },
    #[error("type `{ty}` has an infinite size")]
    #[diagnostic(code(check::infinite_type))]
    InfiniteTy {
        #[label]
        ty: Ty,
        var: TyVar,
    },
    #[error("cannot return outside of function scope")]
    #[diagnostic(code(check::infinite_type))]
    MisplacedReturn {
        #[label]
        span: Span,
    },
    #[error("the name `{name}` is defined multiple times")]
    #[diagnostic(
        code(resolve::duplicate_names),
        help("you can only define the names once in this module")
    )]
    DuplicateName {
        name: Ustr,
        #[label("`{name}` is already defined here")]
        prev_span: Span,
        #[label("`{name}` is defined again here")]
        dup_span: Span,
    },
}

impl Spanned for CheckError {
    fn span(&self) -> Span {
        match self {
            CheckError::TyNotEq { expected, .. } => expected.span,
            CheckError::InfiniteTy { ty, .. } => ty.span,
            CheckError::MisplacedReturn { span } => *span,
            CheckError::DuplicateName { dup_span, .. } => *dup_span,
        }
    }
}
