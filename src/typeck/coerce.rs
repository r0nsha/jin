use crate::{
    hir::ExprId,
    ty::{
        coerce::{Coercion, CoercionKind},
        TyKind,
    },
    typeck::{normalize::NormalizeTy, tcx::TyCtxt, unify::EqResult},
};

pub trait CoerceExt<'db> {
    fn or_coerce(self, tcx: &mut TyCtxt<'db>, expr_id: ExprId) -> Self;
}

impl CoerceExt<'_> for EqResult<()> {
    fn or_coerce(self, tcx: &mut TyCtxt, expr_id: ExprId) -> Self {
        let storage = &mut tcx.storage.borrow_mut();
        let target_metrics = tcx.db.target_metrics();

        match self {
            Ok(res) => Ok(res),
            Err(err) => {
                let source = err.found.normalize(storage);
                let target = err.expected.normalize(storage);

                match (source.kind(), target.kind()) {
                    (TyKind::Never, _) => {
                        tcx.db.push_coercion(
                            expr_id,
                            Coercion { kind: CoercionKind::NeverToAny, target },
                        );
                        Ok(())
                    }
                    (TyKind::Int(isource), TyKind::Int(itarget))
                        if itarget.size(target_metrics) >= isource.size(target_metrics) =>
                    {
                        tcx.db.push_coercion(
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
