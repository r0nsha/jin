use crate::{
    db::Db,
    hir::{Expr, ExprId, ExprKind, Fn, Hir, ItemKind},
    passes::typeck::unify::EqResult,
    ty::{coerce::Coercions, Ty},
};

trait CoerceExt<T> {
    fn or_coerce(&self, expr_id: ExprId, source: Ty, target: Ty) -> EqResult<T>;
}

impl<T> CoerceExt<T> for EqResult<T> {
    fn or_coerce(&self, expr_id: ExprId, source: Ty, target: Ty) -> EqResult<T> {
        todo!()
    }
}

pub fn apply_coercions(db: &mut Db, hir: &mut Hir) {
    let mut cx = Context { db };

    for item in &mut hir.items {
        match &mut item.kind {
            ItemKind::Fn(f) => f.apply_adj(&mut cx),
        }
    }
}

struct Context<'db> {
    db: &'db mut Db,
}

trait ApplyAdjustments<'db> {
    fn apply_adj(&mut self, cx: &mut Context<'db>);
}

impl ApplyAdjustments<'_> for Fn {
    fn apply_adj(&mut self, cx: &mut Context<'_>) {
        self.body.apply_adj(cx);
    }
}

impl ApplyAdjustments<'_> for Expr {
    fn apply_adj(&mut self, cx: &mut Context<'_>) {
        todo!()
        // match &mut self.kind {
        //     ExprKind::If(if_) => todo!(),
        //     ExprKind::Block(blk) => todo!(),
        //     ExprKind::Return(ret) => todo!(),
        //     ExprKind::Call(call) => todo!(),
        //     ExprKind::Bin(bin) => todo!(),
        //     ExprKind::Name(name) => todo!(),
        //     ExprKind::Lit(_) => (),
        // }
        //
        // apply_adj_to_expr(self, cx);
    }
}

fn apply_adj_to_expr(expr: &mut Expr, cx: &Context<'_>) {
    todo!()
    // let adjustments = cx.tcx.adjustments[expr.id];

    // if !adjustments.is_empty() {
    //     todo!("apply here...");
    // }
}
