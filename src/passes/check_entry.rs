use crate::{
    db::{self, Db, DefKind},
    diagnostics::{Diagnostic, Label},
    hir::{Hir, ItemKind},
    span::Span,
    ty::TyKind,
};

pub fn check_entry(db: &mut Db, hir: &Hir) -> Result<(), Diagnostic> {
    check_entry_exists(db)?;
    check_entry_ty(db, hir)
}

fn check_entry_exists(db: &mut Db) -> Result<(), Diagnostic> {
    let main_module_id = db.main_module_id().unwrap();

    let main_fun_id = if let Some(main_fun) = db.defs.iter().find(|def| {
        def.scope.module_id == main_module_id
            && matches!(def.kind.as_ref(), DefKind::Fn(db::FnInfo::Bare))
            && def.qpath.name() == "main"
    }) {
        main_fun.id
    } else {
        return Err(Diagnostic::error("typeck::no_main")
            .with_message("`main` function not found")
            .with_label(
                Label::primary(Span::uniform(
                    db.main_source_id(),
                    (db.main_source().contents().len() - 1) as u32,
                ))
                .with_message("consider adding a main function here"),
            ));
    };

    db.set_main_fun(main_fun_id);

    Ok(())
}

fn check_entry_ty(db: &Db, hir: &Hir) -> Result<(), Diagnostic> {
    let main_fun = db.main_function().expect("to exist");
    let fun_ty = main_fun.ty.kind();

    if is_main_fun_ty(fun_ty) {
        for item in &hir.items {
            match &item.kind {
                ItemKind::Fn(fun) if fun.id == main_fun.id => {
                    let tp = &fun.sig.ty_params;

                    if !tp.is_empty() {
                        let span = tp[0].span.merge(tp.last().unwrap().span);

                        return Err(Diagnostic::error("typeck::main_with_type_params")
                            .with_message("type parameters in `main` function are not allowed")
                            .with_label(Label::primary(span).with_message("not allowed")));
                    }

                    break;
                }
                ItemKind::Fn(_) => (),
            }
        }

        Ok(())
    } else {
        Err(Diagnostic::error("typeck::wrong_main_type")
            .with_message("`main` function's type must be `fn() ()`")
            .with_label(
                Label::primary(main_fun.span)
                    .with_message(format!("found type `{}`", fun_ty.display(db))),
            ))
    }
}

fn is_main_fun_ty(ty: &TyKind) -> bool {
    matches!(ty, TyKind::Fn(f) if f.params.is_empty() && f.ret.is_unit())
}
