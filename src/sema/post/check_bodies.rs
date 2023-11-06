use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir::{const_eval::Const, Expr, ExprKind, FnKind, Hir},
    span::Span,
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
                FnKind::Bare { body } => self.analyze_expr(body),
                FnKind::Extern { .. } => (),
            }
        }

        for let_ in &hir.lets {
            self.analyze_expr(&let_.value);
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        expr.walk(|expr| {
            if let ExprKind::Cast(cast) = &expr.kind {
                let source = cast.expr.ty;
                let target = expr.ty;

                if !is_valid_cast(source, target) {
                    let source = source.display(self.db);
                    let target = target.display(self.db);

                    self.db.diagnostics.emit(
                        Diagnostic::error("check::invalid_cast")
                            .with_message(format!("cannot cast `{source}` to `{target}`"))
                            .with_label(Label::primary(expr.span).with_message("invalid cast")),
                    );
                }
            }
        });

        if let Some(Const::Int(value)) = self.db.const_storage.expr(expr.id) {
            self.check_const_int_range(*value, expr.ty, expr.span);
        }
    }

    fn check_const_int_range(&mut self, value: i128, ty: Ty, span: Span) {
        match ty.kind() {
            TyKind::Int(ity) => {
                if !ity.contains(value) {
                    self.int_out_of_range_err(value, ty, span);
                }
            }
            TyKind::Uint(uty) => {
                if !uty.contains(value) {
                    self.int_out_of_range_err(value, ty, span);
                }
            }
            _ => (),
        }
    }

    fn int_out_of_range_err(&mut self, value: i128, ty: Ty, span: Span) {
        self.db.diagnostics.emit(
            Diagnostic::error("check::int_out_of_range")
                .with_message(format!(
                    "integer {} is of range of its type `{}`",
                    value,
                    ty.display(self.db)
                ))
                .with_label(Label::primary(span).with_message("integer overflows its type")),
        );
    }
}

fn is_valid_cast(source: Ty, target: Ty) -> bool {
    matches!(
        (source.kind(), target.kind()),
        (TyKind::Int(_) | TyKind::Uint(_), TyKind::Int(_) | TyKind::Uint(_))
    )
}
