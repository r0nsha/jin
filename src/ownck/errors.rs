use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir,
    span::Span,
};

pub fn use_after_move(
    db: &Db,
    item: hir::DestroyGlueItem,
    moved_to: Span,
    already_moved_to: Span,
) -> Diagnostic {
    let value_name = match item {
        hir::DestroyGlueItem::Expr(_) => "temporary value".to_string(),
        hir::DestroyGlueItem::Def(id) => {
            format!("value `{}`", db[id].name)
        }
    };

    let short_value_name = match item {
        hir::DestroyGlueItem::Expr(_) => value_name.clone(),
        hir::DestroyGlueItem::Def(id) => {
            format!("`{}`", db[id].name)
        }
    };

    Diagnostic::error()
        .with_message(format!("use of moved {value_name}"))
        .with_label(Label::primary(moved_to).with_message(format!(
            "{short_value_name} used here after being moved"
        )))
        .with_label(
            Label::secondary(already_moved_to)
                .with_message(format!("{short_value_name} already moved here")),
        )
}
