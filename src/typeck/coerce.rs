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
        options: CoerceOptions,
    ) -> Option<Coercions>;

    fn coerce(&self, target: &Self, cx: &Typeck<'a>) -> Option<Coercions> {
        self.coerce_ex(target, cx, CoerceOptions::default())
    }

    fn can_coerce(
        &self,
        target: &Self,
        cx: &Typeck<'a>,
        options: CoerceOptions,
    ) -> bool {
        self.coerce_ex(target, cx, options).is_some()
    }
}

impl<'a> Coerce<'a> for Ty {
    fn coerce_ex(
        &self,
        target: &Self,
        cx: &Typeck<'a>,
        options: CoerceOptions,
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
    options: CoerceOptions,
) -> bool {
    let target_metrics = cx.db.target_metrics();

    match (source.kind(), target.kind()) {
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
        (TyKind::Ref(a, Mutability::Imm), TyKind::Ref(b, Mutability::Imm)) => {
            coerce_tys(*a, *b, cx, coercions, options)
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
        (TyKind::Never, _) | (TyKind::Unit, TyKind::Never) => {
            coercions.push(Coercion { kind: CoercionKind::NeverToAny, target });
            true
        }
        _ => source.unify(target, cx, options.unify_options).is_ok(),
    }
}

fn can_unify_or_coerce(
    source: Ty,
    target: Ty,
    cx: &Typeck,
    coercions: &mut Coercions,
    options: CoerceOptions,
) -> bool {
    let unify_result = if options.rollback_unifications {
        source.can_unify(target, cx, options.unify_options)
    } else {
        source.unify(target, cx, options.unify_options)
    };

    unify_result.is_ok() || coerce_tys(source, target, cx, coercions, options)
}

#[derive(Debug, Clone, Copy)]
pub struct CoerceOptions {
    pub unify_options: UnifyOptions,
    pub rollback_unifications: bool,
}

impl Default for CoerceOptions {
    fn default() -> Self {
        Self::new()
    }
}

impl CoerceOptions {
    pub fn new() -> Self {
        Self {
            unify_options: UnifyOptions::default(),
            rollback_unifications: false,
        }
    }
}
