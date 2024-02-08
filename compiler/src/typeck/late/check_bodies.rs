use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir,
    hir::{Expr, ExprKind, FnKind, Hir},
    middle::UnOp,
    span::Span,
    ty::{Ty, TyKind},
    typeck::errors,
};

pub fn check_bodies(db: &mut Db, hir: &Hir) {
    let mut cx = CheckBodies { db, diagnostics: vec![] };
    cx.run(hir);
    db.diagnostics.emit_many(cx.diagnostics);
}

struct CheckBodies<'db> {
    db: &'db Db,
    diagnostics: Vec<Diagnostic>,
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
        CheckBody::new(self).expr(expr)
    }
}

struct CheckBody<'db, 'cx> {
    cx: &'cx mut CheckBodies<'db>,
    in_unsafe_cx: bool,
}

impl<'db, 'cx> CheckBody<'db, 'cx> {
    fn new(cx: &'cx mut CheckBodies<'db>) -> Self {
        Self { cx, in_unsafe_cx: false }
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
            ExprKind::Unsafe(uns) => {
                let prev = self.in_unsafe_cx;
                self.in_unsafe_cx = true;
                self.expr(&uns.expr);
                self.in_unsafe_cx = prev;
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
                    let source = source.display(self.cx.db);
                    let target = target.display(self.cx.db);

                    self.cx.diagnostics.push(
                        Diagnostic::error(format!("cannot cast `{source}` to `{target}`"))
                            .with_label(Label::primary(expr.span, "invalid cast")),
                    );
                }

                self.expr(&cast.expr);
            }
            ExprKind::Transmute(trans) => {
                let source_size = trans.expr.ty.size(self.cx.db);
                let target_size = trans.target.size(self.cx.db);

                if source_size != target_size {
                    self.cx.diagnostics.push(
                        Diagnostic::error("cannot transmute between types of different sizes")
                            .with_label(Label::primary(expr.span, "invalid transmute"))
                            .with_note(format!(
                                "source type: {} ({} bits)",
                                trans.expr.ty.display(self.cx.db),
                                source_size
                            ))
                            .with_note(format!(
                                "target type: {} ({} bits)",
                                trans.target.display(self.cx.db),
                                target_size
                            )),
                    );
                }

                self.expect_unsafe_cx("transmute", expr.span);
            }
            ExprKind::Unary(un) => {
                if un.op == UnOp::Neg && un.expr.ty.is_uint() {
                    self.cx
                        .diagnostics
                        .push(errors::invalid_un_op(self.cx.db, un.op, un.expr.ty, expr.span));
                }

                self.expr(&un.expr);
            }
            ExprKind::Field(field) => self.expr(&field.expr),
            ExprKind::Name(name) => {
                if self.cx.db.intrinsics.contains_key(&name.id) {
                    self.cx.diagnostics.push(
                        Diagnostic::error(format!(
                            "intrinsic `{}` must be called",
                            self.cx.db[name.id].name
                        ))
                        .with_label(Label::primary(expr.span, "must be called")),
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

    fn expect_unsafe_cx(&mut self, action: &str, span: Span) {
        if !self.in_unsafe_cx {
            self.cx.diagnostics.push(
                Diagnostic::error(format!("{action} is unsafe and requires an `unsafe` keyword"))
                    .with_label(Label::primary(span, format!("unsafe {action}"))),
            )
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
