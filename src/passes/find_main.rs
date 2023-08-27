use crate::{
    db::{self, Db, SymbolInfoKind},
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::TyKind,
};

pub fn find_main(db: &mut Db) {
    let main_module_id = db.main_module_id().unwrap();

    let main_fun_id = if let Some(main_fun) = db.symbols.iter().find(|sym| {
        sym.scope.module_id == main_module_id
            && matches!(sym.kind.as_ref(), SymbolInfoKind::Function(db::FunctionInfo::Orphan))
            && sym.qpath.name() == "main"
    }) {
        let fun_ty = main_fun.ty.as_ref();

        if !is_main_fun_ty(fun_ty) {
            db.diagnostics.add(
                Diagnostic::error("invalid_main")
                    .with_message("the `main` function's type must be `fn() ()`")
                    .with_label(
                        Label::primary(main_fun.span)
                            .with_message(format!("found type `{}`", fun_ty.display(db))),
                    ),
            );
        }

        Some(main_fun.id)
    } else {
        db.diagnostics.add(
            Diagnostic::error("typeck::no_main")
                .with_message("`main` function not found")
                .with_label(
                    Label::primary(Span::uniform(
                        db.main_source_id(),
                        (db.main_source().contents().len() - 1) as u32,
                    ))
                    .with_message("consider adding a main function here"),
                ),
        );

        None
    };

    if let Some(id) = main_fun_id {
        db.set_main_fun(id);
    }
}

fn is_main_fun_ty(ty: &TyKind) -> bool {
    let fun_ty = ty.as_function().unwrap();
    fun_ty.ret.is_unit()
}
