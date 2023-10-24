use ustr::Ustr;

use crate::{
    ast::AttrKind,
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir::const_eval::ConstEvalError,
    sema::unify::{Obligation, ObligationKind},
    span::{Span, Spanned},
    ty::Ty,
    word::Word,
};

#[derive(Debug)]
pub enum CheckError {
    MultipleItems { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleParams { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleTyParams { name: Ustr, prev_span: Span, dup_span: Span },
    NameNotFound(Word),
    InvalidInferTy(Span),
    TyMismatch { expected: Ty, found: Ty, obligation: Obligation },
    InfiniteTy { ty: Ty, obligation: Obligation },
    ArgMismatch { expected: usize, found: usize, span: Span },
    TyArgMismatch { expected: usize, found: usize, span: Span },
    NamedParamNotFound { word: Word },
    MultipleNamedArgs { name: Ustr, prev: Span, dup: Span, is_named: bool },
    UncallableTy { ty: Ty, span: Span },
    ExpectedTy { ty: Ty, span: Span },
    CannotInfer { ty: Ty, span: Span },
    InvalidReturn(Span),
    CyclicItems { origin_span: Span, reference_span: Span },
    ConstEval(ConstEvalError, Span),
    InvalidMember { ty: Ty, member: Word },
    NonConstAttrValue { ty: Ty, span: Span },
    PathNotFound { path: Ustr, span: Span },
    InvalidAttrPlacement { kind: AttrKind, span: Span },
    NonConstGlobalLet { span: Span },
}

impl CheckError {
    pub fn into_diagnostic(self, db: &Db) -> Diagnostic {
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
            Self::TyMismatch { expected, found, obligation } => {
                let expected_ty = expected.display(db).to_string();
                let found_ty = found.display(db).to_string();

                let msg = format!("expected type `{expected_ty}`, found `{found_ty}`");

                let mut diag =
                    Diagnostic::error("check::type_mismatch").with_message(msg.clone()).with_label(
                        Label::primary(obligation.span())
                            .with_message(format!("expected `{expected_ty}` here")),
                    );

                match *obligation.kind() {
                    ObligationKind::Obvious => (),
                    ObligationKind::Exprs(expected_span, found_span) => diag.push_labels([
                        Label::secondary(expected_span).with_message(expected_ty.to_string()),
                        Label::secondary(found_span).with_message(found_ty.to_string()),
                    ]),
                    ObligationKind::ReturnTy(return_ty_span) => {
                        diag.push_label(
                            Label::secondary(return_ty_span).with_message("because of return type"),
                        );
                    }
                }

                diag
            }
            Self::InfiniteTy { ty, obligation } => Diagnostic::error("check::infinite_type")
                .with_message(format!("type `{}` is an infinite type", ty.display(db)))
                .with_label(Label::primary(obligation.span())),
            Self::ArgMismatch { expected, found, span } => Diagnostic::error("check::arg_mismatch")
                .with_message(format!(
                    "this function takes {expected} argument(s), but {found} were supplied"
                ))
                .with_label(
                    Label::primary(span)
                        .with_message(format!("expected {expected} arguments, found {found}")),
                ),
            Self::TyArgMismatch { expected, found, span } => {
                Diagnostic::error("check::type_arg_mismatch")
                    .with_message(format!(
                        "expected {expected} type argument(s), but {found} were supplied"
                    ))
                    .with_label(
                        Label::primary(span).with_message(format!(
                            "expected {expected} type arguments, found {found}"
                        )),
                    )
            }
            Self::NamedParamNotFound { word } => Diagnostic::error("check::named_param_not_found")
                .with_message(format!("cannot find parameter with the name `{}`", word.name()))
                .with_label(
                    Label::primary(word.span())
                        .with_message(format!("found argument `{}` here", word.name())),
                ),
            Self::MultipleNamedArgs { name, prev, dup, is_named } => {
                Diagnostic::error("check::multiple_named_args")
                    .with_message(if is_named {
                        format!("argument `{name}` is passed multiple times")
                    } else {
                        format!("argument `{name}` is already passed positionally")
                    })
                    .with_label(
                        Label::primary(dup).with_message(format!("`{name}` is passed again here")),
                    )
                    .with_label(
                        Label::secondary(prev)
                            .with_message(format!("`{name}` is already passed here")),
                    )
            }
            Self::UncallableTy { ty, span } => Diagnostic::error("check::uncallable_type")
                .with_message(format!("expected a function, found `{}`", ty.display(db)))
                .with_label(Label::primary(span).with_message("expected a function")),
            Self::ExpectedTy { ty, span } => Diagnostic::error("check::expected_ty")
                .with_message(format!("expected a type, found value of type `{}`", ty.display(db)))
                .with_label(Label::primary(span).with_message("expected a type")),
            Self::CannotInfer { ty, span } => Diagnostic::error("check::cannot_infer")
                .with_message(format!("type annotations needed for `{}`", ty.display(db)))
                .with_label(Label::primary(span).with_message("cannot infer type")),
            Self::InvalidReturn(span) => Diagnostic::error("check::invalid_return")
                .with_message("cannot return outside of function scope")
                .with_label(Label::primary(span)),
            Self::CyclicItems { origin_span, reference_span } => {
                Diagnostic::error("check::cyclic_items")
                    .with_message("cycle detected while checking definition")
                    .with_label(Label::primary(origin_span).with_message("definition here"))
                    .with_label(
                        Label::secondary(reference_span).with_message("cyclic reference here"),
                    )
            }
            Self::ConstEval(err, span) => {
                let msg = match err {
                    ConstEvalError::DivByZero => "division by zero",
                    ConstEvalError::RemByZero => "reminder by zero",
                    ConstEvalError::Overflow => "integer overflow",
                };
                Diagnostic::error("check::const_eval_error")
                    .with_message(format!("const evaluation failed: {msg}"))
                    .with_label(Label::primary(span).with_message(format!("caught {msg}")))
            }
            Self::InvalidMember { ty, member } => Diagnostic::error("check::invalid_member")
                .with_message(format!("type `{}` has no member `{}`", ty.display(db), member))
                .with_label(Label::primary(member.span()).with_message("unknown member")),
            Self::NonConstAttrValue { ty, span } => {
                Diagnostic::error("check::non_const_attr_value")
                    .with_message(format!(
                "value of type `{}` must resolve to a const, because it is passed to an attribute",
                ty.display(db),
            ))
                    .with_label(Label::primary(span).with_message("not const"))
            }
            Self::PathNotFound { path, span } => Diagnostic::error("check::path_not_found")
                .with_message(format!("path `{path}` not found"))
                .with_label(Label::primary(span).with_message("not found")),
            Self::InvalidAttrPlacement { kind, span } => {
                Diagnostic::error("check::invalid_attr_placement")
                    .with_message(format!("attribute `{kind}` cannot be placed here"))
                    .with_label(Label::primary(span).with_message("invalid attribute"))
            }
            Self::NonConstGlobalLet { span } => Diagnostic::error("check::non_const_global_let")
                .with_message("global variable must resolve to a const value")
                .with_label(Label::primary(span).with_message("not a const value")),
        }
    }
}
