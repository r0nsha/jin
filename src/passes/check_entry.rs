use crate::{
    db::{self, Db, SymbolInfoKind},
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::TyKind,
};

pub fn check_entry(db: &mut Db) -> Result<(), Diagnostic> {
    check_entry_exists(db)?;
    check_entry_ty(db)
}

fn check_entry_exists(db: &mut Db) -> Result<(), Diagnostic> {
    let main_module_id = db.main_module_id().unwrap();

    let main_fun_id = if let Some(main_fun) = db.symbols.iter().find(|sym| {
        sym.scope.module_id == main_module_id
            && matches!(sym.kind.as_ref(), SymbolInfoKind::Function(db::FunctionInfo::Orphan))
            && sym.qpath.name() == "main"
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

fn check_entry_ty(db: &Db) -> Result<(), Diagnostic> {
    let main_fun = db.main_function().expect("to exist");
    let fun_ty = main_fun.ty.as_ref();

    if is_main_fun_ty(fun_ty) {
        Ok(())
    } else {
        Err(Diagnostic::error("typeck::wrong_main_type")
            .with_message("the `main` function's type must be `fn() ()`")
            .with_label(
                Label::primary(main_fun.span)
                    .with_message(format!("found type `{}`", fun_ty.display(db))),
            ))
    }
}

fn is_main_fun_ty(ty: &TyKind) -> bool {
    matches!(ty, TyKind::Function(f) if f.params.is_empty() && f.ret.is_unit())
}
