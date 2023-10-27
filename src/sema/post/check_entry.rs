use crate::{
    db::{Db, DefKind, FnInfo},
    diagnostics::{Diagnostic, Label},
    hir::Hir,
    span::{Span, Spanned},
    ty::TyKind,
};

pub fn check_entry(db: &mut Db, hir: &Hir) {
    CheckEntry { db }.run(hir);
}

struct CheckEntry<'db> {
    db: &'db mut Db,
}

impl<'db> CheckEntry<'db> {
    fn run(&mut self, hir: &Hir) {
        if self.check_entry_exists() {
            self.check_entry_ty(hir);
        }
    }

    fn check_entry_exists(&mut self) -> bool {
        let main_module_id = self.db.main_module_id().unwrap();

        let main_id = self.db.defs.iter().find_map(|def| {
            (def.scope.module_id == main_module_id
                && matches!(def.kind.as_ref(), DefKind::Fn(FnInfo::Bare))
                && def.qpath.name() == "main")
                .then_some(def.id)
        });

        if let Some(main_id) = main_id {
            self.db.set_main_fun(main_id);
            true
        } else {
            let source_end_span = Span::uniform(
                self.db.main_source_id(),
                (self.db.main_source().contents().len() - 1) as u32,
            );

            self.db.diagnostics.emit(
                Diagnostic::error("check::no_entry_point")
                    .with_message("`main` function not found")
                    .with_label(
                        Label::primary(source_end_span)
                            .with_message("consider adding a main function here"),
                    ),
            );

            false
        }
    }

    fn check_entry_ty(&mut self, hir: &Hir) {
        let main_fun = self.db.main_function().expect("to exist");
        let fty = main_fun.ty.kind();

        if is_main_fun_ty(fty) {
            for fun in &hir.fns {
                if fun.id == main_fun.id {
                    let tp = &fun.sig.ty_params;

                    if !tp.is_empty() {
                        let tp_span = tp[0].word.span().merge(tp.last().unwrap().word.span());

                        self.db.diagnostics.emit(
                            Diagnostic::error("check::entry_point_with_type_params")
                                .with_message("type parameters in `main` function are not allowed")
                                .with_label(Label::primary(tp_span).with_message("not allowed")),
                        );
                    }

                    break;
                }
            }
        } else {
            self.db.diagnostics.emit(
                Diagnostic::error("check::wrong_entry_point_type")
                    .with_message("`main` function's type must be `fn() ()`")
                    .with_label(
                        Label::primary(main_fun.span)
                            .with_message(format!("found type `{}`", main_fun.ty.display(self.db))),
                    ),
            );
        }
    }
}

fn is_main_fun_ty(ty: &TyKind) -> bool {
    matches!(ty, TyKind::Fn(f) if f.params.is_empty() && f.ret.is_unit())
}
