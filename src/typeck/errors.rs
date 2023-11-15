use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    span::{Span, Spanned},
    ty::Ty,
    word::Word,
};

pub fn invalid_member(db: &Db, expr_ty: Ty, expr_span: Span, member: Word) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("no member `{}` on type `{}`", member, expr_ty.display(db)))
        .with_label(Label::primary(member.span()).with_message("unknown member"))
        .with_label(
            Label::secondary(expr_span)
                .with_message(format!("expression has type `{}`", expr_ty.display(db))),
        )
}

pub fn named_param_not_found(name: Word) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("cannot find parameter with the name `{}`", name.name()))
        .with_label(
            Label::primary(name.span())
                .with_message(format!("found argument `{}` here", name.name())),
        )
}

pub fn arg_mismatch(expected: usize, found: usize, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("function takes {expected} argument(s), but {found} were supplied"))
        .with_label(
            Label::primary(span)
                .with_message(format!("expected {expected} arguments, found {found}")),
        )
}

pub fn ty_mismatch(expected: &str, found: &str, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("expected type `{expected}`, found `{found}`"))
        .with_label(Label::primary(span).with_message(format!("expected `{expected}` here")))
}
