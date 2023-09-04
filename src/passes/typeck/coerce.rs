use crate::{
    db::Db,
    hir::{Expr, ExprId, ExprKind, Fn, Hir, ItemKind},
    passes::typeck::unify::{EqError, EqResult},
    ty::{
        coerce::{Coercion, CoercionKind, Coercions},
        Ty, TyKind,
    },
};

trait CoerceExt {
    fn or_coerce(self, db: &mut Db, expr_id: ExprId) -> Self;
}

impl CoerceExt for EqResult<()> {
    fn or_coerce(self, db: &mut Db, expr_id: ExprId) -> Self {
        match self {
            Ok(res) => Ok(res),
            Err(err) => {
                let source = err.found;
                let target = err.expected;

                match (source.kind(), target.kind()) {
                    (TyKind::Never, _) => {
                        db.push_coercion(
                            expr_id,
                            Coercion { kind: CoercionKind::NeverToAny, target },
                        );
                        Ok(())
                    }
                    (_, _) => Err(err),
                }
            }
        }
    }
}

pub fn apply_coercions(db: &mut Db, hir: &mut Hir) {
    // let mut cx = Context { db };

    for item in &mut hir.items {
        match &mut item.kind {
            ItemKind::Fn(f) => {
                f.body = f.body.clone().rewrite(|expr| {
                    if let Some(coercions) = db.coercions.get(&expr.id) {
                        coercions.apply(expr)
                    } else {
                        expr
                    }
                });
            }
        }
    }
}

struct Context<'db> {
    db: &'db mut Db,
}

trait ApplyCoercions<'db> {
    fn apply_coercions(&mut self, cx: &mut Context<'db>);
}

impl ApplyCoercions<'_> for Fn {
    fn apply_coercions(&mut self, cx: &mut Context<'_>) {
        self.body.apply_coercions(cx);
    }
}

impl ApplyCoercions<'_> for Expr {
    fn apply_coercions(&mut self, cx: &mut Context<'_>) {
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
    // let coercions = cx.tcx.coercions[expr.id];

    // if !adjustments.is_empty() {
    //     todo!("apply here...");
    // }
}
