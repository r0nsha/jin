use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    span::{Span, Spanned},
    ty::Ty,
    word::Word,
};

pub fn invalid_member(db: &Db, ty: Ty, member: Word) -> Diagnostic {
    Diagnostic::error("check::invalid_member")
        .with_message(format!("type `{}` has no member `{}`", ty.display(db), member))
        .with_label(Label::primary(member.span()).with_message("unknown member"))
}

pub fn named_param_not_found(name: Word) -> Diagnostic {
    Diagnostic::error("check::named_param_not_found")
        .with_message(format!("cannot find parameter with the name `{}`", name.name()))
        .with_label(
            Label::primary(name.span())
                .with_message(format!("found argument `{}` here", name.name())),
        )
}

pub fn arg_mismatch(expected: usize, found: usize, span: Span) -> Diagnostic {
    Diagnostic::error("check::arg_mismatch")
        .with_message(format!(
            "function takes {expected} argument(s), but {found} were supplied"
        ))
        .with_label(
            Label::primary(span)
                .with_message(format!("expected {expected} arguments, found {found}")),
        )
}
