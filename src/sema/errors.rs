use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    span::Spanned,
    ty::Ty,
    word::Word,
};

pub fn invalid_member(db: &Db, ty: Ty, member: Word) -> Diagnostic {
    Diagnostic::error("check::invalid_member")
        .with_message(format!("type `{}` has no member `{}`", ty.display(db), member))
        .with_label(Label::primary(member.span()).with_message("unknown member"))
}
