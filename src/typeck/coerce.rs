use crate::{
    hir::ExprId,
    middle::Mutability,
    ty::{
        coerce::{Coercion, CoercionKind, Coercions},
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

                if let Some(coercions) = source.coerce(&target, cx) {
                    cx.db.push_coercions(expr_id, coercions);
                    Ok(())
                } else {
                    Err(err)
                }
            }
        }
    }
}

pub trait Coerce<'a> {
    fn coerce_ex(
        &self,
        target: &Self,
        cx: &Typeck<'a>,
        options: UnifyOptions,
    ) -> Option<Coercions>;

    fn coerce(&self, target: &Self, cx: &Typeck<'a>) -> Option<Coercions> {
        self.coerce_ex(target, cx, UnifyOptions::default())
    }

    fn can_coerce(
        &self,
        target: &Self,
        cx: &Typeck<'a>,
        options: UnifyOptions,
    ) -> bool {
        self.coerce_ex(target, cx, options).is_some()
    }
}

impl<'a> Coerce<'a> for Ty {
    fn coerce_ex(
        &self,
        target: &Self,
        cx: &Typeck<'a>,
        options: UnifyOptions,
    ) -> Option<Coercions> {
        let mut coercions = Coercions::new();

        if coerce_tys(*self, *target, cx, &mut coercions, options) {
            Some(coercions)
        } else {
            None
        }
    }
}

fn coerce_tys(
    source: Ty,
    target: Ty,
    cx: &Typeck,
    coercions: &mut Coercions,
    options: UnifyOptions,
) -> bool {
    let target_metrics = cx.db.target_metrics();

    match (source.kind(), target.kind()) {
        (TyKind::Never, _) | (TyKind::Unit, TyKind::Never) => {
            coercions.push(Coercion { kind: CoercionKind::NeverToAny, target });
            true
        }
        (TyKind::Int(a), TyKind::Int(b))
            if b.size(target_metrics) >= a.size(target_metrics) =>
        {
            coercions
                .push(Coercion { kind: CoercionKind::IntPromotion, target });
            true
        }
        (TyKind::Ref(a, Mutability::Mut), TyKind::Ref(b, Mutability::Imm)) => {
            if can_unify_or_coerce(*a, *b, cx, coercions, options) {
                coercions
                    .push(Coercion { kind: CoercionKind::MutRefToImm, target });
                true
            } else {
                false
            }
        }
        (_, TyKind::Ref(b, _)) => {
            if can_unify_or_coerce(source, *b, cx, coercions, options) {
                coercions
                    .push(Coercion { kind: CoercionKind::OwnedToRef, target });
                true
            } else {
                false
            }
        }
        _ => false,
    }
}

fn can_unify_or_coerce(
    source: Ty,
    target: Ty,
    cx: &Typeck,
    coercions: &mut Coercions,
    options: UnifyOptions,
) -> bool {
    source.can_unify(target, cx, options).is_ok()
        || coerce_tys(source, target, cx, coercions, options)
}
