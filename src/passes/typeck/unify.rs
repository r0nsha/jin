use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{
    passes::typeck::{error::InferError, infcx::InferCtxt, normalize::NormalizeTy},
    span::Span,
    ty::{InferTy, IntTy, IntVar, IntVarValue, Ty, TyKind, TyVar},
};

impl<'db> InferCtxt<'db> {
    #[inline]
    #[must_use]
    pub fn at(&self, obligation: Obligation) -> At<'_, '_> {
        At { infcx: self, obligation }
    }
}

pub struct At<'db, 'a> {
    infcx: &'a InferCtxt<'db>,
    obligation: Obligation,
}

impl At<'_, '_> {
    pub fn eq(&self, expected: Ty, found: Ty) -> Result<(), InferError> {
        UnifyCtxt { infcx: self.infcx }.unify_ty_ty(expected, found).map_err(|err| {
            let mut infcx = self.infcx.inner.borrow_mut();

            match err {
                UnifyError::TyMismatch { .. } => InferError::TyMismatch {
                    expected: expected.normalize(&mut infcx),
                    found: found.normalize(&mut infcx),
                    obligation: self.obligation,
                },
                UnifyError::InfiniteTy { ty } => InferError::InfiniteTy {
                    ty: ty.normalize(&mut infcx),
                    obligation: Obligation::obvious(self.obligation.span()),
                },
            }
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Obligation {
    span: Span,
    kind: ObligationKind,
}

impl Obligation {
    pub fn new(span: Span, kind: ObligationKind) -> Self {
        Self { span, kind }
    }

    pub fn obvious(span: Span) -> Self {
        Self::new(span, ObligationKind::Obvious)
    }

    pub fn exprs(span: Span, expected: Span, found: Span) -> Self {
        Self::new(span, ObligationKind::Exprs(expected, found))
    }

    pub fn return_ty(span: Span, return_ty_span: Span) -> Self {
        Self::new(span, ObligationKind::ReturnTy(return_ty_span))
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> &ObligationKind {
        &self.kind
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ObligationKind {
    /// Should be obvious from the span
    Obvious,
    /// Two expressions which are expected to have the same type
    Exprs(Span, Span),
    /// An expression which was expected to be equal to the return type
    ReturnTy(Span),
}

struct UnifyCtxt<'db, 'a> {
    infcx: &'a InferCtxt<'db>,
}

impl UnifyCtxt<'_, '_> {
    fn unify_ty_ty(&mut self, a: Ty, b: Ty) -> Result<(), UnifyError> {
        let (a, b) = {
            let mut infcx = self.infcx.inner.borrow_mut();
            let a = a.normalize(&mut infcx);
            let b = b.normalize(&mut infcx);
            (a, b)
        };

        match (a.kind(), b.kind()) {
            (TyKind::Never, _)
            | (_, TyKind::Never)
            | (TyKind::Bool, TyKind::Bool)
            | (TyKind::Unit, TyKind::Unit)
            | (TyKind::Int(IntTy::Int), TyKind::Int(IntTy::Int)) => Ok(()),

            (TyKind::Fn(ref fex), TyKind::Fn(ref fact)) => {
                self.unify_ty_ty(fex.ret, fact.ret)?;

                if fex.params.len() == fact.params.len() {
                    for (p1, p2) in fex.params.iter().zip(fact.params.iter()) {
                        self.unify_ty_ty(p1.ty, p2.ty)?;
                    }

                    Ok(())
                } else {
                    Err(UnifyError::TyMismatch { a, b })
                }
            }

            // Unify ?T1 ~ ?T2
            (TyKind::Infer(InferTy::TyVar(expected)), TyKind::Infer(InferTy::TyVar(found))) => {
                self.infcx
                    .inner
                    .borrow_mut()
                    .ty_unification_table
                    .unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ ?int
            (TyKind::Infer(InferTy::IntVar(expected)), TyKind::Infer(InferTy::IntVar(found))) => {
                self.infcx
                    .inner
                    .borrow_mut()
                    .int_unification_table
                    .unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ int
            (TyKind::Int(ity), TyKind::Infer(InferTy::IntVar(var)))
            | (TyKind::Infer(InferTy::IntVar(var)), TyKind::Int(ity)) => {
                self.infcx
                    .inner
                    .borrow_mut()
                    .int_unification_table
                    .unify_var_value(*var, Some(IntVarValue::Int(*ity)))?;
                Ok(())
            }

            // Unify ?T ~ T
            (TyKind::Infer(InferTy::TyVar(var)), _) => self.unify_ty_var(b, *var),

            // Unify T ~ ?T
            (_, TyKind::Infer(InferTy::TyVar(var))) => self.unify_ty_var(a, *var),

            (_, _) => Err(UnifyError::TyMismatch { a, b }),
        }
    }

    fn unify_ty_var(&mut self, ty: Ty, var: TyVar) -> Result<(), UnifyError> {
        ty.occurs_check(var).map_err(|ty| UnifyError::InfiniteTy { ty })?;
        self.infcx.inner.borrow_mut().ty_unification_table.unify_var_value(var, Some(ty))?;
        Ok(())
    }
}

impl UnifyKey for TyVar {
    type Value = Option<Ty>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "TyVar"
    }
}

impl EqUnifyValue for Ty {}

impl UnifyKey for IntVar {
    type Value = Option<IntVarValue>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "IntTy"
    }
}

impl EqUnifyValue for IntVarValue {}

pub enum UnifyError {
    TyMismatch { a: Ty, b: Ty },
    InfiniteTy { ty: Ty },
}

impl From<(Ty, Ty)> for UnifyError {
    fn from((a, b): (Ty, Ty)) -> Self {
        Self::TyMismatch { a, b }
    }
}

impl From<(IntVarValue, IntVarValue)> for UnifyError {
    fn from((a, b): (IntVarValue, IntVarValue)) -> Self {
        Self::TyMismatch { a: Ty::new(a.into()), b: Ty::new(b.into()) }
    }
}
