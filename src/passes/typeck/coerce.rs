use crate::{
    db::Db,
    hir::{ExprId, Hir, ItemKind},
    passes::typeck::unify::EqResult,
    ty::{
        coerce::{Coercion, CoercionKind},
        TyKind,
    },
};

pub trait CoerceExt {
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

pub fn apply_coercions(db: &Db, hir: &mut Hir) {
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
