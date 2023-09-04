use super::*;

pub trait HirVisitor: Sized {
    fn visit_item(&mut self, item: &Item) {
        noop_visit_item(self, item);
    }

    fn visit_fn(&mut self, f: &Fn) {
        noop_visit_fn(self, f);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        noop_visit_expr(self, expr);
    }

    fn visit_if(&mut self, if_: &If) {
        noop_visit_if(self, if_);
    }

    fn visit_block(&mut self, blk: &Block) {
        noop_visit_block(self, blk);
    }

    fn visit_return(&mut self, ret: &Return) {
        noop_visit_return(self, ret);
    }

    fn visit_call(&mut self, call: &Call) {
        noop_visit_call(self, call);
    }

    fn visit_bin(&mut self, bin: &BinOp) {
        noop_visit_bin(self, bin);
    }

    fn visit_name(&mut self, name: &Name) {
        noop_visit_name(self, name);
    }

    fn visit_lit(&mut self, lit: &Lit) {
        noop_visit_lit(self, lit);
    }
}

pub fn noop_visit_item(vis: &mut impl HirVisitor, item: &Item) {
    match &item.kind {
        ItemKind::Fn(f) => vis.visit_fn(f),
    }
}

pub fn noop_visit_fn(vis: &mut impl HirVisitor, f: &Fn) {
    vis.visit_expr(&f.body);
}

pub fn noop_visit_expr(vis: &mut impl HirVisitor, expr: &Expr) {
    match &expr.kind {
        ExprKind::If(x) => vis.visit_if(x),
        ExprKind::Block(x) => vis.visit_block(x),
        ExprKind::Return(x) => vis.visit_return(x),
        ExprKind::Call(x) => vis.visit_call(x),
        ExprKind::Bin(x) => vis.visit_bin(x),
        ExprKind::Name(x) => vis.visit_name(x),
        ExprKind::Lit(x) => vis.visit_lit(x),
    }
}

pub fn noop_visit_if(vis: &mut impl HirVisitor, if_: &If) {
    vis.visit_expr(&if_.cond);
    vis.visit_expr(&if_.then);
    if let Some(o) = &if_.otherwise {
        vis.visit_expr(o);
    }
}

pub fn noop_visit_block(vis: &mut impl HirVisitor, blk: &Block) {
    for expr in &blk.exprs {
        vis.visit_expr(expr);
    }
}

pub fn noop_visit_return(vis: &mut impl HirVisitor, ret: &Return) {
    vis.visit_expr(&ret.expr);
}

pub fn noop_visit_call(vis: &mut impl HirVisitor, call: &Call) {
    vis.visit_expr(&call.callee);

    for arg in &call.args {
        vis.visit_expr(&arg.expr);
    }
}

pub fn noop_visit_bin(vis: &mut impl HirVisitor, bin: &BinOp) {
    vis.visit_expr(&bin.lhs);
    vis.visit_expr(&bin.rhs);
}

pub fn noop_visit_name(_: &mut impl HirVisitor, _: &Name) {}

pub fn noop_visit_lit(_: &mut impl HirVisitor, _: &Lit) {}
