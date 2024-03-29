use compiler_core::{
    hir::ExprId,
    middle::Mutability,
    ty::{
        coerce::{Coercion, CoercionKind, Coercions},
        Ty, TyKind,
    },
};

use crate::unify::TyUnifyExt as _;
use crate::{
    unify::{EqResult, UnifyOptions, UnifyResult},
    Typeck,
};

pub trait CoerceExt<'db>
where
    Self: Sized,
{
    fn or_coerce_ex(self, cx: &mut Typeck<'db>, expr_id: ExprId, options: CoerceOptions) -> Self;
    fn or_coerce(self, cx: &mut Typeck<'db>, expr_id: ExprId) -> Self {
        self.or_coerce_ex(cx, expr_id, CoerceOptions::default())
    }
}

impl CoerceExt<'_> for EqResult {
    fn or_coerce_ex(self, cx: &mut Typeck, expr_id: ExprId, options: CoerceOptions) -> Self {
        match self {
            Ok(res) => Ok(res),
            Err(err) => {
                let (source, target) = (cx.normalize(err.found), cx.normalize(err.expected));

                if let Some(coercions) = source.coerce(&target, cx, options) {
                    cx.hir.push_coercions(expr_id, coercions);
                    Ok(())
                } else {
                    Err(err)
                }
            }
        }
    }
}

pub trait Coerce<'a> {
    fn coerce(&self, target: &Self, cx: &Typeck<'a>, options: CoerceOptions) -> Option<Coercions>;

    fn can_coerce(&self, target: &Self, cx: &Typeck<'a>, options: CoerceOptions) -> bool {
        self.coerce(target, cx, options).is_some()
    }
}

impl<'a> Coerce<'a> for Ty {
    fn coerce(&self, target: &Self, cx: &Typeck<'a>, options: CoerceOptions) -> Option<Coercions> {
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
        (TyKind::Int(a), TyKind::Int(b)) if b.size(target_metrics) >= a.size(target_metrics) => {
            coercions.push(Coercion { kind: CoercionKind::IntPromotion, target });
            true
        }
        (TyKind::Uint(a), TyKind::Uint(b)) if b.size(target_metrics) >= a.size(target_metrics) => {
            coercions.push(Coercion { kind: CoercionKind::IntPromotion, target });
            true
        }
        (TyKind::Never, _) | (TyKind::Unit, TyKind::Never) => {
            coercions.push(Coercion { kind: CoercionKind::NeverToAny, target });
            true
        }
        (TyKind::Ref(a, Mutability::Mut), TyKind::Ref(b, Mutability::Imm)) => {
            if can_unify_or_coerce(*a, *b, cx, coercions, options) {
                coercions.push(Coercion { kind: CoercionKind::MutRefToImm, target });
                true
            } else {
                false
            }
        }
        (TyKind::Ref(a, Mutability::Imm), TyKind::Ref(b, Mutability::Imm)) => {
            coerce_tys(*a, *b, cx, coercions, options)
        }
        (_, TyKind::Unit) => {
            coercions.push(Coercion { kind: CoercionKind::AnyToUnit, target });
            true
        }
        (_, TyKind::Never) => {
            coercions.push(Coercion { kind: CoercionKind::AnyToNever, target });
            true
        }
        (_, TyKind::Ref(b, _))
            if options.allow_owned_to_ref
                && can_unify_or_coerce(source, *b, cx, coercions, options) =>
        {
            coercions.push(Coercion { kind: CoercionKind::OwnedToRef, target });
            true
        }
        (TyKind::Ref(a, _), _)
            if !target.is_ref()
                && !target.is_move(cx.db)
                && unify_with_options(*a, target, cx, options).is_ok() =>
        {
            coercions.push(Coercion { kind: CoercionKind::RefToOwned, target });
            true
        }
        _ => unify_with_options(source, target, cx, options).is_ok(),
    }
}

fn can_unify_or_coerce(
    source: Ty,
    target: Ty,
    cx: &Typeck,
    coercions: &mut Coercions,
    options: CoerceOptions,
) -> bool {
    let unify_result = unify_with_options(source, target, cx, options);
    unify_result.is_ok() || coerce_tys(source, target, cx, coercions, options)
}

fn unify_with_options(source: Ty, target: Ty, cx: &Typeck, options: CoerceOptions) -> UnifyResult {
    if options.rollback_unifications {
        source.can_unify(target, cx, options.unify_options)
    } else {
        source.unify(target, cx, options.unify_options)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CoerceOptions {
    pub unify_options: UnifyOptions,
    pub rollback_unifications: bool,
    pub allow_owned_to_ref: bool,
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
            allow_owned_to_ref: false,
        }
    }
}
