use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir::{ExprKind, Hir, ItemKind},
    span::Span,
    ty::{Ty, TyKind},
};

pub fn analysis(db: &mut Db, hir: &Hir) {
    Context { db }.analyze(hir);
}

struct Context<'db> {
    db: &'db mut Db,
}

impl Context<'_> {
    fn analyze(&mut self, hir: &Hir) {
        for item in &hir.items {
            match &item.kind {
                ItemKind::Fn(f) => f.body.walk(|expr| {
                    if let ExprKind::Cast(cast) = &expr.kind {
                        let source = cast.expr.ty;
                        let target = expr.ty;

                        if !is_valid_cast(source, target) {
                            self.emit(AnalysisError::InvalidCast {
                                source,
                                target,
                                span: expr.span,
                            });
                        }
                    }
                }),
            }
        }
    }

    fn emit(&mut self, err: AnalysisError) {
        self.db.diagnostics.emit(err.into_diagnostic(self.db));
    }
}

#[derive(Debug)]
pub enum AnalysisError {
    InvalidCast { source: Ty, target: Ty, span: Span },
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
        }
    }
}

fn is_valid_cast(source: Ty, target: Ty) -> bool {
    #[allow(clippy::match_like_matches_macro)]
    match (source.kind(), target.kind()) {
        (TyKind::Int(_), TyKind::Int(_)) => true,
        _ => false,
    }
}
