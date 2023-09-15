use crate::{
    db::{Db, DefKind, FnInfo},
    diagnostics::{Diagnostic, Label},
    hir::{Expr, ExprKind, Hir},
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
            self.analyze_expr(&f.body);
        }

        for let_ in &hir.lets {
            self.analyze_expr(&let_.value);
        }

        self.check_entry(hir);
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
    }

    fn check_entry(&mut self, hir: &Hir) {
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
            self.emit(AnalysisError::NoEntryPoint);
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
                        let span = tp[0].span.merge(tp.last().unwrap().span);
                        self.emit(AnalysisError::EntryPointWithTyParams { span });
                    }

                    break;
                }
            }
        } else {
            self.emit(AnalysisError::WrongEntryPointTy { ty: main_fun.ty, span: main_fun.span });
        }
    }

    fn emit(&mut self, err: AnalysisError) {
        self.db.diagnostics.emit(err.into_diagnostic(self.db));
    }
}

fn is_main_fun_ty(ty: &TyKind) -> bool {
    matches!(ty, TyKind::Fn(f) if f.params.is_empty() && f.ret.is_unit())
}

#[derive(Debug)]
pub enum AnalysisError {
    InvalidCast { source: Ty, target: Ty, span: Span },
    NoEntryPoint,
    WrongEntryPointTy { ty: Ty, span: Span },
    EntryPointWithTyParams { span: Span },
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
            Self::NoEntryPoint => Diagnostic::error("analysis::no_entry_point")
                .with_message("`main` function not found")
                .with_label(
                    Label::primary(Span::uniform(
                        db.main_source_id(),
                        (db.main_source().contents().len() - 1) as u32,
                    ))
                    .with_message("consider adding a main function here"),
                ),
            Self::WrongEntryPointTy { ty, span } => {
                Diagnostic::error("analysis::wrong_entry_point_type")
                    .with_message("`main` function's type must be `fn() ()`")
                    .with_label(
                        Label::primary(span)
                            .with_message(format!("found type `{}`", ty.display(db))),
                    )
            }
            Self::EntryPointWithTyParams { span } => {
                Diagnostic::error("analysis::entry_point_with_type_params")
                    .with_message("type parameters in `main` function are not allowed")
                    .with_label(Label::primary(span).with_message("not allowed"))
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
