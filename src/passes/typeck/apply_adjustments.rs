use crate::{
    db::Db,
    hir::{Expr, Fn, Hir, ItemKind},
    ty::{adjust::Adjustments, tcx::TyCtxt},
};

pub fn apply_adjustments(db: &mut Db, tcx: &mut TyCtxt, hir: &mut Hir) {
    let mut cx = Context { db, tcx };
    for item in &mut hir.items {
        match &mut item.kind {
            ItemKind::Fn(f) => f.apply_adj(&mut cx),
        }
    }
}

struct Context<'db> {
    db: &'db mut Db,
    tcx: &'db mut TyCtxt,
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
        match self {
            Expr::If(if_) => todo!(),
            Expr::Block(blk) => todo!(),
            Expr::Return(ret) => todo!(),
            Expr::Call(call) => todo!(),
            Expr::Bin(bin) => todo!(),
            Expr::Name(name) => todo!(),
            Expr::Lit(_) => (),
        }

        apply_adj_to_expr(self, cx);
    }
}

fn apply_adj_to_expr(expr: &mut Expr, cx: &Context<'_>) {
    todo!()
    // let adjustments = cx.tcx.adjustments[expr.id];

    // if !adjustments.is_empty() {
    //     todo!("apply here...");
    // }
}
