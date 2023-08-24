use crate::{
    db::{self, Db, SymbolInfoKind},
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::TypeKind,
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
            Diagnostic::error("no_main")
                .with_message("couldn't find a function named `main` in the main module")
                .with_label(Label::primary(Span::initial(db.main_module().unwrap().source_id))),
        );

        None
    };

    if let Some(id) = main_fun_id {
        db.set_main_fun(id);
    }
}

fn is_main_fun_ty(ty: &TypeKind) -> bool {
    let fun_ty = ty.as_function().unwrap();
    fun_ty.ret.is_unit()
}
