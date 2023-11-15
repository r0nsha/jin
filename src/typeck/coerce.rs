use crate::{
    hir::ExprId,
    typeck::{normalize::NormalizeTy, unify::EqResult, Typeck},
    ty::{
        coerce::{Coercion, CoercionKind},
        TyKind,
    },
};

pub trait CoerceExt<'db> {
    fn or_coerce(self, cx: &mut Typeck<'db>, expr_id: ExprId) -> Self;
}

impl CoerceExt<'_> for EqResult<()> {
    fn or_coerce(self, cx: &mut Typeck, expr_id: ExprId) -> Self {
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
