use compiler_core::{
    db::{Db, DefKind, FnInfo},
    diagnostics::{Diagnostic, Label},
    hir::Hir,
    span::{Span, Spanned},
    ty::TyKind,
};

pub fn check_main(db: &mut Db, hir: &mut Hir) {
    CheckMain { db }.run(hir);
}

struct CheckMain<'db> {
    db: &'db mut Db,
}

impl<'db> CheckMain<'db> {
    fn run(&mut self, hir: &mut Hir) {
        if self.exists(hir) {
            self.check_ty(hir);
        }
    }

    fn exists(&mut self, hir: &mut Hir) -> bool {
        let main_module_id = self.db.main_module.unwrap();

        let main_def_id = self.db.defs.iter().find_map(|def| {
            (def.scope.module_id == main_module_id
                && matches!(&def.kind, DefKind::Fn(FnInfo::Bare))
                && def.qpath.name() == "main")
                .then_some(def.id)
        });

        if let Some(main_def_id) = main_def_id {
            let main_fn = hir.fns.iter().find(|f| f.def_id == main_def_id).unwrap();
            hir.main_fn.set(main_fn.id);
            true
        } else {
            let main_source_end =
                self.db.main_source().contents().len().checked_sub(1).unwrap_or_default();
            let source_end_span = Span::uniform(self.db.main_source_id(), main_source_end as u32);

            self.db.diagnostics.add(Diagnostic::error("`main` function not found").with_label(
                Label::primary(source_end_span, "consider adding a main function here"),
            ));

            false
        }
    }

    fn check_ty(&mut self, hir: &Hir) {
        let main_fn = &hir.fns[hir.main_fn.unwrap()];
        let fty = main_fn.sig.ty.kind();

        if is_main_ty(fty) {
            let tp = &main_fn.sig.tparams;

            if !tp.is_empty() {
                let tp_span = tp[0].word.span().merge(tp.last().unwrap().word.span());

                self.db.diagnostics.add(
                    Diagnostic::error("type parameters in `main` function are not allowed")
                        .with_label(Label::primary(tp_span, "not allowed")),
                );
            }
        } else {
            self.db.diagnostics.add(
                Diagnostic::error("`main` function's type must be `fn() ()`").with_label(
                    Label::primary(main_fn.span, format!("found type `{}`", fty.display(self.db))),
                ),
            );
        }
    }
}

fn is_main_ty(ty: &TyKind) -> bool {
    matches!(ty, TyKind::Fn(f) if f.params.is_empty() && f.ret.is_unit())
}
