use crate::{
    hir::ExprId,
    sema::{normalize::NormalizeTy, unify::EqResult, Resolver},
    ty::{
        coerce::{Coercion, CoercionKind},
        TyKind,
    },
};

pub trait CoerceExt<'db> {
    fn or_coerce(self, cx: &mut Resolver<'db>, expr_id: ExprId) -> Self;
}

impl CoerceExt<'_> for EqResult<()> {
    fn or_coerce(self, cx: &mut Resolver, expr_id: ExprId) -> Self {
        let storage = &mut cx.storage.borrow_mut();
        let target_metrics = cx.db.target_metrics();

        match self {
            Ok(res) => Ok(res),
            Err(err) => {
                let source = err.found.normalize(storage);
                let target = err.expected.normalize(storage);

                match (source.kind(), target.kind()) {
                    (TyKind::Never, _) => {
                        cx.db.push_coercion(
                            expr_id,
                            Coercion { kind: CoercionKind::NeverToAny, target },
                        );
                        Ok(())
                    }
                    (TyKind::Int(isource), TyKind::Int(itarget))
                        if itarget.size(target_metrics) >= isource.size(target_metrics) =>
                    {
                        cx.db.push_coercion(
                            expr_id,
                            Coercion { kind: CoercionKind::IntPromotion, target },
                        );
                        Ok(())
                    }
                    (_, _) => Err(err),
                }
            }
        }
    }
}
