pub mod check_entry;

pub use check_entry::check_entry;

use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir::{const_eval::Const, Expr, ExprKind, FnKind, Hir},
    span::Span,
    ty::{Ty, TyKind},
};

pub fn analyze(db: &mut Db, hir: &Hir) {
    Context { db }.analyze(hir);
}

struct Context<'db> {
    db: &'db mut Db,
}

impl Context<'_> {
    fn analyze(&mut self, hir: &Hir) {
        for f in &hir.fns {
            match &f.kind {
                FnKind::Bare { body } => self.analyze_expr(body),
                FnKind::Extern => (),
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
                    self.emit(AnalysisError::InvalidCast { source, target, span: expr.span });
                }
            }
        });

        if let Some(value) = self.db.const_storage.expr(expr.id) {
            if let Err(err) = check_const_value_range(value, expr.ty, expr.span) {
                self.emit(err);
            }
        }
    }

    fn emit(&mut self, err: AnalysisError) {
        self.db.diagnostics.emit(err.into_diagnostic(self.db));
    }
}

fn check_const_value_range(value: &Const, ty: Ty, span: Span) -> Result<(), AnalysisError> {
    match value {
        Const::Int(value) => match ty.kind() {
            TyKind::Int(ity) => {
                if ity.contains(*value) {
                    Ok(())
                } else {
                    Err(AnalysisError::IntOutOfRange { value: *value, ty, span })
                }
            }
            TyKind::Uint(uty) => {
                if uty.contains(*value) {
                    Ok(())
                } else {
                    Err(AnalysisError::IntOutOfRange { value: *value, ty, span })
                }
            }
            _ => Ok(()),
        },
        _ => Ok(()),
    }
}

#[derive(Debug)]
pub enum AnalysisError {
    InvalidCast { source: Ty, target: Ty, span: Span },
    IntOutOfRange { value: i128, ty: Ty, span: Span },
}

impl AnalysisError {
    pub fn into_diagnostic(self, db: &Db) -> Diagnostic {
        match self {
            Self::InvalidCast { source, target, span } => {
                let source = source.display(db);
                let target = target.display(db);

                Diagnostic::error("analysis::invalid_cast")
                    .with_message(format!("cannot cast `{source}` to `{target}`"))
                    .with_label(Label::primary(span).with_message("invalid cast"))
            }
            Self::IntOutOfRange { value, ty, span } => {
                Diagnostic::error("analysis::int_out_of_range")
                    .with_message(format!(
                        "integer {} is of range of its type `{}`",
                        value,
                        ty.display(db)
                    ))
                    .with_label(Label::primary(span).with_message("integer overflows its type"))
            }
        }
    }
}

fn is_valid_cast(source: Ty, target: Ty) -> bool {
    matches!(
        (source.kind(), target.kind()),
        (TyKind::Int(_) | TyKind::Uint(_), TyKind::Int(_) | TyKind::Uint(_))
    )
}
