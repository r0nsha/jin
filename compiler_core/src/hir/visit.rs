use super::*;

pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Let(x) => self.visit_let(expr, x),
            ExprKind::Assign(x) => self.visit_assign(expr, x),
            ExprKind::Swap(x) => self.visit_swap(expr, x),
            ExprKind::Match(x) => self.visit_match(expr, x),
            ExprKind::Loop(x) => self.visit_loop(expr, x),
            ExprKind::Break => self.visit_break(expr),
            ExprKind::Block(x) => self.visit_block(expr, x),
            ExprKind::Unsafe(x) => self.visit_unsafe(expr, x),
            ExprKind::Return(x) => self.visit_return(expr, x),
            ExprKind::Call(x) => self.visit_call(expr, x),
            ExprKind::Unary(x) => self.visit_unary(expr, x),
            ExprKind::Binary(x) => self.visit_binary(expr, x),
            ExprKind::Deref(x) => self.visit_deref(expr, x),
            ExprKind::Cast(x) => self.visit_cast(expr, x),
            ExprKind::Transmute(x) => self.visit_transmute(expr, x),
            ExprKind::Field(x) => self.visit_field(expr, x),
            ExprKind::Index(x) => self.visit_index(expr, x),
            ExprKind::Slice(x) => self.visit_slice(expr, x),
            ExprKind::Name(x) => self.visit_name(expr, x),
            ExprKind::Variant(x) => self.visit_variant(expr, x),
            ExprKind::SliceLit(x) => self.visit_slice_lit(expr, x),
            ExprKind::BoolLit(x) => self.visit_bool_lit(expr, *x),
            ExprKind::IntLit(x) => self.visit_int_lit(expr, *x),
            ExprKind::FloatLit(x) => self.visit_float_lit(expr, *x),
            ExprKind::StrLit(x) => self.visit_str_lit(expr, *x),
            ExprKind::CharLit(x) => self.visit_char_lit(expr, *x),
        }
    }

    fn visit_let(&mut self, _: &Expr, let_: &Let) {
        walk_let(self, let_);
    }

    fn visit_assign(&mut self, _: &Expr, assign: &Assign) {
        walk_assign(self, assign);
    }

    fn visit_swap(&mut self, _: &Expr, swap: &Swap) {
        walk_swap(self, swap);
    }

    fn visit_match(&mut self, _: &Expr, match_: &Match) {
        walk_match(self, match_);
    }

    fn visit_loop(&mut self, _: &Expr, loop_: &Loop) {
        walk_loop(self, loop_);
    }

    fn visit_block(&mut self, _: &Expr, block: &Block) {
        walk_block(self, block);
    }

    fn visit_unsafe(&mut self, _: &Expr, unsafe_: &Unsafe) {
        walk_unsafe(self, unsafe_);
    }

    fn visit_return(&mut self, _: &Expr, ret: &Return) {
        walk_return(self, ret);
    }

    fn visit_call(&mut self, _: &Expr, call: &Call) {
        walk_call(self, call);
    }

    fn visit_unary(&mut self, _: &Expr, unary: &Unary) {
        walk_unary(self, unary);
    }

    fn visit_binary(&mut self, _: &Expr, binary: &Binary) {
        walk_binary(self, binary);
    }

    fn visit_deref(&mut self, _: &Expr, deref: &Deref) {
        walk_deref(self, deref);
    }

    fn visit_cast(&mut self, _: &Expr, cast: &Cast) {
        walk_cast(self, cast);
    }

    fn visit_transmute(&mut self, _: &Expr, transmute: &Transmute) {
        walk_transmute(self, transmute);
    }

    fn visit_field(&mut self, _: &Expr, field: &Field) {
        walk_field(self, field);
    }

    fn visit_index(&mut self, _: &Expr, index: &Index) {
        walk_index(self, index);
    }

    fn visit_slice(&mut self, _: &Expr, slice: &Slice) {
        walk_slice(self, slice);
    }

    fn visit_slice_lit(&mut self, _: &Expr, lit: &SliceLit) {
        walk_slice_lit(self, lit);
    }

    fn visit_break(&mut self, _: &Expr) {}
    fn visit_name(&mut self, _: &Expr, _: &Name) {}
    fn visit_variant(&mut self, _: &Expr, _: &Variant) {}
    fn visit_bool_lit(&mut self, _: &Expr, _: bool) {}
    fn visit_int_lit(&mut self, _: &Expr, _: u128) {}
    fn visit_float_lit(&mut self, _: &Expr, _: f64) {}
    fn visit_str_lit(&mut self, _: &Expr, _: Ustr) {}
    fn visit_char_lit(&mut self, _: &Expr, _: char) {}
}

pub fn walk_let(v: &mut impl Visitor, let_: &Let) {
    v.visit_expr(&let_.value);
}

pub fn walk_assign(v: &mut impl Visitor, assign: &Assign) {
    v.visit_expr(&assign.lhs);
    v.visit_expr(&assign.rhs);
}

pub fn walk_swap(v: &mut impl Visitor, swap: &Swap) {
    v.visit_expr(&swap.lhs);
    v.visit_expr(&swap.rhs);
}

pub fn walk_match(v: &mut impl Visitor, match_: &Match) {
    v.visit_expr(&match_.expr);

    for arm in &match_.arms {
        if let Some(guard) = &arm.guard {
            v.visit_expr(guard);
        }

        v.visit_expr(&arm.expr);
    }
}

pub fn walk_loop(v: &mut impl Visitor, loop_: &Loop) {
    if let Some(cond) = &loop_.cond {
        v.visit_expr(cond);
    }

    v.visit_expr(&loop_.expr);
}

pub fn walk_block(v: &mut impl Visitor, block: &Block) {
    for expr in &block.exprs {
        v.visit_expr(expr);
    }
}

pub fn walk_unsafe(v: &mut impl Visitor, unsafe_: &Unsafe) {
    v.visit_expr(&unsafe_.expr);
}

pub fn walk_return(v: &mut impl Visitor, ret: &Return) {
    v.visit_expr(&ret.expr);
}

pub fn walk_call(v: &mut impl Visitor, call: &Call) {
    v.visit_expr(&call.callee);
    walk_call_args(v, &call.args);
}

pub fn walk_call_args(v: &mut impl Visitor, args: &[CallArg]) {
    for arg in args {
        v.visit_expr(&arg.expr);
    }
}

pub fn walk_unary(v: &mut impl Visitor, unary: &Unary) {
    v.visit_expr(&unary.expr);
}

pub fn walk_binary(v: &mut impl Visitor, binary: &Binary) {
    v.visit_expr(&binary.lhs);
    v.visit_expr(&binary.rhs);
}

pub fn walk_deref(v: &mut impl Visitor, deref: &Deref) {
    v.visit_expr(&deref.expr);
}

pub fn walk_cast(v: &mut impl Visitor, cast: &Cast) {
    v.visit_expr(&cast.expr);
}

pub fn walk_transmute(v: &mut impl Visitor, transmute: &Transmute) {
    v.visit_expr(&transmute.expr);
}

pub fn walk_field(v: &mut impl Visitor, field: &Field) {
    v.visit_expr(&field.expr);
}

pub fn walk_index(v: &mut impl Visitor, index: &Index) {
    v.visit_expr(&index.expr);
    v.visit_expr(&index.index);
}

pub fn walk_slice(v: &mut impl Visitor, slice: &Slice) {
    v.visit_expr(&slice.expr);

    if let Some(low) = &slice.low {
        v.visit_expr(low);
    }

    if let Some(high) = &slice.high {
        v.visit_expr(high);
    }
}

pub fn walk_slice_lit(v: &mut impl Visitor, lit: &SliceLit) {
    for expr in &lit.exprs {
        v.visit_expr(expr);
    }

    if let Some(cap) = &lit.cap {
        v.visit_expr(cap);
    }
}
