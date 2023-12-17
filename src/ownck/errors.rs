use crate::{
    db::{Db, DefId},
    diagnostics::{Diagnostic, Label},
    hir,
    span::{Span, Spanned},
    word::Word,
};

pub fn already_moved(
    db: &Db,
    item: hir::DestroyGlueItem,
    moved_to: Span,
    already_moved_to: Span,
) -> Diagnostic {
    let (name, short_name) = get_item_names(db, item);

    Diagnostic::error()
        .with_message(format!("use of moved {name}"))
        .with_label(
            Label::primary(moved_to)
                .with_message(format!("{short_name} used here after move")),
        )
        .with_label(
            Label::secondary(already_moved_to)
                .with_message(format!("{short_name} already moved here")),
        )
}

pub fn already_partially_moved(
    db: &Db,
    item: hir::DestroyGlueItem,
    moved_to: Span,
    already_moved_member: Word,
) -> Diagnostic {
    let (name, short_name) = get_item_names(db, item);

    Diagnostic::error()
        .with_message(format!(
            "use of partially moved member `{already_moved_member}` of {name}"
        ))
        .with_label(
            Label::primary(moved_to).with_message(format!(
                "{short_name} used here after partial move"
            )),
        )
        .with_label(
            Label::secondary(already_moved_member.span())
                .with_message(format!("{short_name} partially moved here")),
        )
}

pub fn move_after_partially_moved(
    db: &Db,
    item: hir::DestroyGlueItem,
    moved_to: Span,
    already_moved_to: Vec<Span>,
) -> Diagnostic {
    let (name, short_name) = get_item_names(db, item);

    Diagnostic::error()
        .with_message(format!("use of partially moved {name}"))
        .with_label(
            Label::primary(moved_to).with_message(format!(
                "{short_name} used here after partial move"
            )),
        )
        .with_labels(already_moved_to.into_iter().map(|moved_to| {
            Label::secondary(moved_to).with_message(format!(
                "{short_name} already partially moved here"
            ))
        }))
}

pub fn move_global_item(db: &Db, id: DefId, moved_to: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!(
            "cannot move out of global item `{}`",
            db[id].qpath
        ))
        .with_label(
            Label::primary(moved_to).with_message("global item moved here"),
        )
}

pub fn move_into_loop(
    db: &Db,
    item: hir::DestroyGlueItem,
    moved_to: Span,
    value_span: Span,
    loop_span: Span,
) -> Diagnostic {
    let (name, short_name) = get_item_names(db, item);

    Diagnostic::error()
        .with_message(format!("use of moved {name}"))
        .with_label(Label::primary(moved_to).with_message(format!(
            "{short_name} moved here, in the previous loop iteration"
        )))
        .with_label(
            Label::secondary(loop_span).with_message("inside this loop"),
        )
        .with_label(
            Label::secondary(value_span)
                .with_message(format!("{short_name} defined here")),
        )
}

fn get_item_names(db: &Db, item: hir::DestroyGlueItem) -> (String, String) {
    let name = match item {
        hir::DestroyGlueItem::Expr(_) => "temporary value".to_string(),
        hir::DestroyGlueItem::Def(id) => {
            format!("value `{}`", db[id].name)
        }
    };

    let short_name = match item {
        hir::DestroyGlueItem::Expr(_) => name.clone(),
        hir::DestroyGlueItem::Def(id) => {
            format!("`{}`", db[id].name)
        }
    };

    (name, short_name)
}
