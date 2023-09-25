use crate::{
    db::{Db, DefKind, FnInfo},
    diagnostics::{Diagnostic, Label},
    hir::Hir,
    span::Span,
    ty::TyKind,
};

pub fn check_entry(db: &mut Db, hir: &Hir) {
    if check_entry_exists(db) {
        check_entry_ty(db, hir);
    }
}

fn check_entry_exists(db: &mut Db) -> bool {
    let main_module_id = db.main_module_id().unwrap();

    let main_id = db.defs.iter().find_map(|def| {
        (def.scope.module_id == main_module_id
            && matches!(def.kind.as_ref(), DefKind::Fn(FnInfo::Bare))
            && def.qpath.name() == "main")
            .then_some(def.id)
    });

    if let Some(main_id) = main_id {
        db.set_main_fun(main_id);
        true
    } else {
        let source_end_span =
            Span::uniform(db.main_source_id(), (db.main_source().contents().len() - 1) as u32);

        db.diagnostics.emit(
            Diagnostic::error("analysis::no_entry_point")
                .with_message("`main` function not found")
                .with_label(
                    Label::primary(source_end_span)
                        .with_message("consider adding a main function here"),
                ),
        );

        false
    }
}

fn check_entry_ty(db: &mut Db, hir: &Hir) {
    let main_fun = db.main_function().expect("to exist");
    let fty = main_fun.ty.kind();

    if is_main_fun_ty(fty) {
        for fun in &hir.fns {
            if fun.id == main_fun.id {
                let tp = &fun.sig.ty_params;

                if !tp.is_empty() {
                    let tp_span = tp[0].span.merge(tp.last().unwrap().span);

                    db.diagnostics.emit(
                        Diagnostic::error("analysis::entry_point_with_type_params")
                            .with_message("type parameters in `main` function are not allowed")
                            .with_label(Label::primary(tp_span).with_message("not allowed")),
                    );
                }

                break;
            }
        }
    } else {
        db.diagnostics.emit(
            Diagnostic::error("analysis::wrong_entry_point_type")
                .with_message("`main` function's type must be `fn() ()`")
                .with_label(
                    Label::primary(main_fun.span)
                        .with_message(format!("found type `{}`", main_fun.ty.display(db))),
                ),
        );
    }
}

fn is_main_fun_ty(ty: &TyKind) -> bool {
    matches!(ty, TyKind::Fn(f) if f.params.is_empty() && f.ret.is_unit())
}
