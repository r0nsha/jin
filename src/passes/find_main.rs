use crate::{
    db::Database,
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::Type,
};

pub(crate) fn find_main(db: &mut Database) {
    let main_module_id = db.main_module_id().unwrap();

    let main_fun_id = if let Some(main_fun) = db
        .funs
        .iter()
        .find(|f| f.module_id == main_module_id && f.name == "main")
    {
        let fun_ty = main_fun.ty.get(db);

        if !is_main_fun_ty(fun_ty) {
            db.diagnostics.add(
                Diagnostic::error("invalid_main")
                    .with_message("the `main` function's type must be `fn() ()`")
                    .with_label(
                        Label::primary(main_fun.span)
                            .with_message(format!("found type `{fun_ty}` instead")),
                    ),
            )
        }

        Some(main_fun.id)
    } else {
        db.diagnostics.add(
            Diagnostic::error("no_main")
                .with_message("couldn't find a function named `main` in the main module")
                .with_label(Label::primary(Span::initial(
                    db.main_module().unwrap().source_id,
                ))),
        );

        None
    };

    if let Some(id) = main_fun_id {
        db.set_main_fun(id);
    }
}

fn is_main_fun_ty(ty: &Type) -> bool {
    let fun_ty = ty.kind.as_fun().unwrap();
    fun_ty.ret.kind.is_unit()
}
