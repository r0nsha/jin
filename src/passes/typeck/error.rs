use ustr::Ustr;

use crate::{
    common::Word,
    db::Db,
    diagnostics::{Diagnostic, Label},
    passes::typeck::unify::{Obligation, ObligationKind},
    span::{Span, Spanned},
    ty::Ty,
};

pub enum InferError {
    TyMismatch { expected: Ty, found: Ty, obligation: Obligation },
    InfiniteTy { ty: Ty, obligation: Obligation },
    ArgMismatch { expected: usize, found: usize, span: Span },
    NamedParamNotFound { word: Word },
    MultipleNamedArgs { name: Ustr, prev: Span, dup: Span },
}

impl InferError {
    pub fn into_diagnostic(self, db: &Db) -> Diagnostic {
        match self {
            Self::TyMismatch { expected, found, obligation } => {
                let expected_ty = expected.display(db).to_string();
                let found_ty = found.display(db).to_string();

                let msg = format!("expected `{expected_ty}`, found `{found_ty}`");

                let mut diag = Diagnostic::error("typeck::type_mismatch")
                    .with_message(msg.clone())
                    .with_label(Label::primary(obligation.span()).with_message(msg));

                match *obligation.kind() {
                    ObligationKind::Obvious => (),
                    ObligationKind::Exprs(expected_span, found_span) => diag.push_labels([
                        Label::secondary(expected_span)
                            .with_message(format!("expected `{expected_ty}`")),
                        Label::secondary(found_span).with_message(format!("found `{found_ty}`")),
                    ]),
                    ObligationKind::ReturnTy(return_ty_span) => {
                        diag.push_label(Label::secondary(return_ty_span).with_message(format!(
                            "expected `{expected_ty}` because of return type"
                        )));
                    }
                }

                diag
            }
            Self::InfiniteTy { ty, obligation } => Diagnostic::error("typeck::infinite_type")
                .with_message(format!("type `{}` is an infinite type", ty.display(db)))
                .with_label(Label::primary(obligation.span())),
            Self::ArgMismatch { expected, found, span } => {
                Diagnostic::error("typeck::arg_mismatch")
                    .with_message(format!(
                        "this function takes {expected} arguments, but {found} were supplied"
                    ))
                    .with_label(
                        Label::primary(span)
                            .with_message(format!("expected {expected} arguments, found {found}")),
                    )
            }
            Self::NamedParamNotFound { word } => Diagnostic::error("typeck::named_param_not_found")
                .with_message(format!("cannot find parameter with the name `{}`", word.name()))
                .with_label(
                    Label::primary(word.span())
                        .with_message(format!("tried to pass `{}` here", word.name())),
                ),
            Self::MultipleNamedArgs { name, prev, dup } => {
                Diagnostic::error("resolve::multiple_named_args")
                    .with_message(format!("argument `{name}` is passed multiple times"))
                    .with_label(
                        Label::primary(dup).with_message(format!("`{name}` is passed again here")),
                    )
                    .with_label(
                        Label::secondary(prev)
                            .with_message(format!("`{name}` is already passed here")),
                    )
            }
        }
    }
}
