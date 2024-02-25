use compiler_core::ty::{IntTy, UintTy};
use compiler_core::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    hir::{
        visit::{self, Visitor},
        Assign, Binary, Call, Cast, Convert, Deref, Expr, ExprKind, FnKind, Hir, Name, Swap, Unary,
        Unsafe,
    },
    middle::UnOp,
    span::Span,
    ty::{Ty, TyKind},
};

use crate::errors;

pub fn check_bodies(db: &mut Db, hir: &Hir) {
    let mut cx = CheckBodies { db, diagnostics: vec![] };
    cx.check(hir);
    db.diagnostics.extend(cx.diagnostics);
}

struct CheckBodies<'db> {
    db: &'db Db,
    diagnostics: Vec<Diagnostic>,
}

impl CheckBodies<'_> {
    fn check(&mut self, hir: &Hir) {
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
        CheckBody::new(self).visit_expr(expr)
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

    fn check_assign_lhs(&mut self, lhs: &Expr, action: &str) {
        if let ExprKind::Field(f) = &lhs.kind {
            match f.expr.ty.auto_deref().kind() {
                TyKind::Slice(_) | TyKind::Str => self.expect_unsafe_cx(
                    &format!("{} of builtin field `{}`", action, f.field),
                    lhs.span,
                ),
                _ => (),
            }
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

impl<'db, 'cx> Visitor for CheckBody<'db, 'cx> {
    fn visit_assign(&mut self, _: &Expr, assign: &Assign) {
        self.check_assign_lhs(&assign.lhs, "assign");
        visit::walk_assign(self, assign);
    }

    fn visit_swap(&mut self, _: &Expr, swap: &Swap) {
        self.check_assign_lhs(&swap.lhs, "swap");
        visit::walk_swap(self, swap);
    }

    fn visit_binary(&mut self, expr: &Expr, binary: &Binary) {
        if binary.lhs.ty.is_raw_ptr() || binary.rhs.ty.is_raw_ptr() {
            self.expect_unsafe_cx("pointer arithmetic", expr.span);
        }
    }

    fn visit_unsafe(&mut self, _: &Expr, unsafe_: &Unsafe) {
        let prev = self.in_unsafe_cx;
        self.in_unsafe_cx = true;
        visit::walk_unsafe(self, unsafe_);
        self.in_unsafe_cx = prev;
    }

    fn visit_call(&mut self, expr: &Expr, call: &Call) {
        if call.callee.ty.as_fn().unwrap().is_extern() {
            self.expect_unsafe_cx("extern function call", expr.span);
        }

        match &call.callee.kind {
            ExprKind::Name(_) => (),
            _ => self.visit_expr(&call.callee),
        }

        visit::walk_call_args(self, &call.args);
    }

    fn visit_deref(&mut self, expr: &Expr, deref: &Deref) {
        self.expect_unsafe_cx("dereference of raw pointer", expr.span);
        visit::walk_deref(self, deref);
    }

    fn visit_convert(&mut self, expr: &Expr, convert: &Convert) {
        let source = convert.expr.ty;
        let target = expr.ty;

        if !is_valid_conversion(source, target) {
            let source = source.display(self.cx.db);
            let target = target.display(self.cx.db);

            self.cx.diagnostics.push(
                Diagnostic::error(format!("cannot convert `{source}` to `{target}`"))
                    .with_label(Label::primary(expr.span, "invalid conversion")),
            );
        }

        if source.is_raw_ptr() || target.is_raw_ptr() {
            self.expect_unsafe_cx("raw pointer conversion", expr.span);
        }

        visit::walk_convert(self, convert);
    }

    fn visit_cast(&mut self, expr: &Expr, cast: &Cast) {
        self.expect_unsafe_cx("cast", expr.span);

        let source_size = cast.expr.ty.size(self.cx.db);
        let target_size = cast.target.size(self.cx.db);

        if source_size != target_size {
            self.cx.diagnostics.push(
                Diagnostic::error("cannot cast between types of different sizes")
                    .with_label(Label::primary(expr.span, "invalid cast"))
                    .with_note(format!(
                        "source type: {} ({} bits)",
                        cast.expr.ty.display(self.cx.db),
                        source_size
                    ))
                    .with_note(format!(
                        "target type: {} ({} bits)",
                        cast.target.display(self.cx.db),
                        target_size
                    )),
            );
        }

        visit::walk_cast(self, cast);
    }

    fn visit_unary(&mut self, expr: &Expr, unary: &Unary) {
        if unary.op == UnOp::Neg && unary.expr.ty.is_uint() {
            self.cx.diagnostics.push(errors::invalid_un_op(
                self.cx.db,
                unary.op,
                unary.expr.ty,
                expr.span,
            ));
        }

        visit::walk_unary(self, unary);
    }

    fn visit_name(&mut self, expr: &Expr, name: &Name) {
        if self.cx.db.builtins.contains_key(&name.id) {
            self.cx.diagnostics.push(
                Diagnostic::error(format!(
                    "builtin function `{}` must be called",
                    self.cx.db[name.id].name
                ))
                .with_label(Label::primary(expr.span, "must be called")),
            );
        }
    }
}

fn is_valid_conversion(source: Ty, target: Ty) -> bool {
    matches!(
        (source.kind(), target.kind()),
        (
            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_),
            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_)
        ) | (
            TyKind::RawPtr(_) | TyKind::Int(_) | TyKind::Uint(_),
            TyKind::RawPtr(_) | TyKind::Int(_) | TyKind::Uint(_)
        ) | (
            TyKind::Int(IntTy::I8)
                | TyKind::Uint(UintTy::U8)
                | TyKind::Int(IntTy::I16)
                | TyKind::Uint(UintTy::U16)
                | TyKind::Int(IntTy::I32)
                | TyKind::Uint(UintTy::U32),
            TyKind::Char
        ) | (TyKind::Char, TyKind::Int(IntTy::I32) | TyKind::Uint(UintTy::U32))
    )
}
