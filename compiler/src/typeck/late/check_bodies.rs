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
                match &call.callee.kind {
                    ExprKind::Name(_) => (),
                    _ => self.expr(&call.callee),
                }

                for arg in &call.args {
                    self.expr(&arg.expr);
                }
            }
            ExprKind::Index(index) => {
                self.expr(&index.expr);
                self.expr(&index.index);
            }
            ExprKind::Slice(slice) => {
                self.expr(&slice.expr);

                if let Some(low) = &slice.low {
                    self.expr(low);
                }

                if let Some(high) = &slice.high {
                    self.expr(high);
                }
            }
            ExprKind::Deref(deref) => {
                self.expr(&deref.expr);
            }
            ExprKind::Cast(cast) => {
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

                self.expr(&cast.expr);
            }
            ExprKind::Transmute(trans) => {
                let source_size = trans.expr.ty.size(self.db);
                let target_size = trans.target.size(self.db);

                if source_size != target_size {
                    self.db.diagnostics.emit(
                        Diagnostic::error()
                            .with_message("cannot transmute between types of different sizes")
                            .with_label(Label::primary(expr.span).with_message("invalid transmute"))
                            .with_note(format!(
                                "source type: {} ({} bits)",
                                trans.expr.ty.display(self.db),
                                source_size
                            ))
                            .with_note(format!(
                                "target type: {} ({} bits)",
                                trans.target.display(self.db),
                                target_size
                            )),
                    );
                }
            }
            ExprKind::Unary(un) => {
                if un.op == UnOp::Neg && un.expr.ty.is_uint() {
                    self.db
                        .diagnostics
                        .emit(errors::invalid_un_op(self.db, un.op, un.expr.ty, expr.span));
                }

                self.expr(&un.expr);
            }
            ExprKind::Field(field) => self.expr(&field.expr),
            ExprKind::Name(name) => {
                if self.db.intrinsics.contains_key(&name.id) {
                    self.db.diagnostics.emit(
                        Diagnostic::error()
                            .with_message(format!(
                                "intrinsic `{}` must be called",
                                self.db[name.id].name
                            ))
                            .with_label(Label::primary(expr.span).with_message("must be called")),
                    );
                }
            }
            ExprKind::Break
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
        ) | (
            TyKind::RawPtr(_) | TyKind::Int(_) | TyKind::Uint(_),
            TyKind::RawPtr(_) | TyKind::Int(_) | TyKind::Uint(_)
        )
    )
}
