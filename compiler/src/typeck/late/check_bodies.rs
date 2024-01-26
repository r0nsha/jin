use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir,
    hir::{Expr, ExprKind, FnKind, Hir},
    middle::UnOp,
    ty::{Ty, TyKind},
    typeck::errors,
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
        match &expr.kind {
            ExprKind::Let(let_) => self.expr(&let_.value),
            ExprKind::Assign(hir::Assign { lhs, rhs, .. })
            | ExprKind::Swap(hir::Swap { lhs, rhs })
            | ExprKind::Binary(hir::Binary { lhs, rhs, .. }) => {
                self.expr(lhs);
                self.expr(rhs);
            }
            ExprKind::Match(match_) => {
                self.expr(&match_.expr);

                for arm in &match_.arms {
                    if let Some(guard) = &arm.guard {
                        self.expr(guard);
                    }

                    self.expr(&arm.expr);
                }
            }
            ExprKind::Loop(loop_) => {
                if let Some(cond) = &loop_.cond {
                    self.expr(cond);
                }

                self.expr(&loop_.expr);
            }
            ExprKind::Block(block) => {
                for expr in &block.exprs {
                    self.expr(expr);
                }
            }
            ExprKind::SliceLit(lit) => {
                for expr in &lit.exprs {
                    self.expr(expr);
                }

                if let Some(cap) = &lit.cap {
                    self.expr(cap);
                }
            }
            ExprKind::Return(ret) => self.expr(&ret.expr),
            ExprKind::Call(call) => {
                self.expr(&call.callee);

                for arg in &call.args {
                    self.expr(&arg.expr);
                }
            }
            ExprKind::Index(index) => {
                self.expr(&index.expr);
                self.expr(&index.index);
            }
            ExprKind::Cast(cast) => {
                let source = cast.expr.ty;
                let target = expr.ty;

                if !is_valid_cast(source, target) {
                    let source = source.display(self.db);
                    let target = target.display(self.db);

                    self.db.diagnostics.emit(
                        Diagnostic::error()
                            .with_message(format!(
                                "cannot cast `{source}` to `{target}`"
                            ))
                            .with_label(
                                Label::primary(expr.span)
                                    .with_message("invalid cast"),
                            ),
                    );
                }

                self.expr(&cast.expr);
            }
            ExprKind::Unary(un) => {
                if un.op == UnOp::Neg && un.expr.ty.is_uint() {
                    self.db.diagnostics.emit(errors::invalid_un_op(
                        self.db, un.op, un.expr.ty, expr.span,
                    ));
                }

                self.expr(&un.expr);
            }
            ExprKind::Field(field) => self.expr(&field.expr),
            ExprKind::Name(_)
            | ExprKind::Break
            | ExprKind::Variant(_)
            | ExprKind::BoolLit(_)
            | ExprKind::IntLit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::StrLit(_) => (),
        }
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
