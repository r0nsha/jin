use crate::{
    db::Db,
    hir::{ExprId, Hir, ItemKind},
    passes::typeck::{infcx::InferCtxt, normalize::NormalizeTy, unify::EqResult},
    ty::{
        coerce::{Coercion, CoercionKind},
        TyKind,
    },
};

pub trait CoerceExt<'db> {
    fn or_coerce(self, infcx: &mut InferCtxt<'db>, expr_id: ExprId) -> Self;
}

impl CoerceExt<'_> for EqResult<()> {
    fn or_coerce(self, infcx: &mut InferCtxt, expr_id: ExprId) -> Self {
        let inner = &mut infcx.inner.borrow_mut();

        match self {
            Ok(res) => Ok(res),
            Err(err) => {
                let source = err.found.normalize(inner);
                let target = err.expected.normalize(inner);

                match (source.kind(), target.kind()) {
                    (TyKind::Never, _) => {
                        infcx.db.push_coercion(
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
                        dbg!(&expr, coercions);
                        coercions.apply(expr)
                    } else {
                        expr
                    }
                });
            }
        }
    }
}
