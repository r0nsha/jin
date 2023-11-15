use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir::{Expr, ExprKind, FnKind, Hir},
    ty::{Ty, TyKind},
};

pub fn check_bodies(db: &mut Db, hir: &Hir) {
    CheckBodies { db }.run(hir);
}

struct CheckBodies<'db> {
    db: &'db mut Db,
}

impl CheckBodies<'_> {
    fn run(&mut self, hir: &Hir) {
        for f in &hir.fns {
            match &f.kind {
                FnKind::Bare { body } => self.expr(body),
                FnKind::Extern { .. } => (),
            }
        }

        for let_ in &hir.lets {
            self.expr(&let_.value);
        }
    }

    fn expr(&mut self, expr: &Expr) {
        expr.walk(|expr| {
            if let ExprKind::Cast(cast) = &expr.kind {
                let source = cast.expr.ty;
                let target = expr.ty;

                if !is_valid_cast(source, target) {
                    let source = source.display(self.db);
                    let target = target.display(self.db);

                    self.db.diagnostics.emit(
                        Diagnostic::error()
                            .with_message(format!("cannot cast `{source}` to `{target}`"))
                            .with_label(Label::primary(expr.span).with_message("invalid cast")),
                    );
                }
            }
        });
    }
}

fn is_valid_cast(source: Ty, target: Ty) -> bool {
    matches!(
        (source.kind(), target.kind()),
        (
            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_),
            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
        )
    )
}
