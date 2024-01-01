use crate::{
    hir::ExprId,
    middle::Mutability,
    ty::{
        coerce::{Coercion, CoercionKind},
        Ty, TyKind,
    },
    typeck::{
        normalize::NormalizeTy,
        unify::{EqResult, UnifyOptions},
        Typeck,
    },
};

pub trait CoerceExt<'db> {
    fn or_coerce(self, cx: &mut Typeck<'db>, expr_id: ExprId) -> Self;
}

impl CoerceExt<'_> for EqResult {
    fn or_coerce(self, cx: &mut Typeck, expr_id: ExprId) -> Self {
        match self {
            Ok(res) => Ok(res),
            Err(err) => {
                let (source, target) = {
                    let storage = &mut cx.storage.borrow_mut();
                    let source = err.found.normalize(storage);
                    let target = err.expected.normalize(storage);
                    (source, target)
                };

                if let Some(kind) = source.coerce(&target, cx) {
                    cx.db.push_coercion(expr_id, Coercion { kind, target });
                    Ok(())
                } else {
                    Err(err)
                }
            }
        }
    }
}

pub trait Coerce<'a> {
    fn coerce(&self, target: &Self, cx: &Typeck<'a>) -> Option<CoercionKind>;

    fn can_coerce(&self, target: &Self, cx: &Typeck<'a>) -> bool {
        self.coerce(target, cx).is_some()
    }
}

impl<'a> Coerce<'a> for Ty {
    fn coerce(&self, target: &Self, cx: &Typeck<'a>) -> Option<CoercionKind> {
        let target_metrics = cx.db.target_metrics();

        match (self.kind(), target.kind()) {
            (TyKind::Never, _) | (TyKind::Unit, TyKind::Never) => {
                Some(CoercionKind::NeverToAny)
            }
            (TyKind::Int(a), TyKind::Int(b))
                if b.size(target_metrics) >= a.size(target_metrics) =>
            {
                Some(CoercionKind::IntPromotion)
            }
            (
                TyKind::Ref(a, Mutability::Mut),
                TyKind::Ref(b, Mutability::Imm),
            ) => {
                if a.can_unify(*b, cx, UnifyOptions { unify_param_tys: false })
                    .is_ok()
                {
                    Some(CoercionKind::MutRefToImm)
                } else {
                    None
                }
            }
            (_, TyKind::Ref(b, _)) => {
                if self
                    .can_unify(*b, cx, UnifyOptions { unify_param_tys: false })
                    .is_ok()
                {
                    Some(CoercionKind::OwnedToRef)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
