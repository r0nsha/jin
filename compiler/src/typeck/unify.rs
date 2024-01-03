use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{
    diagnostics::{Diagnostic, Label},
    span::Span,
    ty::{FloatTy, FloatVar, InferTy, IntVar, IntVarValue, Ty, TyKind, TyVar},
    typeck::{errors, Typeck},
};

impl<'db> Typeck<'db> {
    #[inline]
    #[must_use]
    pub fn at(&self, obligation: Obligation) -> At<'_, '_> {
        At { cx: self, obligation }
    }
}

pub struct At<'db, 'a> {
    cx: &'a Typeck<'db>,
    obligation: Obligation,
}

impl At<'_, '_> {
    pub fn eq(&self, expected: Ty, found: Ty) -> EqResult {
        expected.unify(found, self.cx, UnifyOptions::default()).map_err(|err| {
            let diagnostic = match err {
                UnifyError::TyMismatch { .. } => {
                    let expected = self.cx.normalize(expected);
                    let found = self.cx.normalize(found);

                    let expected_ty = expected.display(self.cx.db).to_string();
                    let found_ty = found.display(self.cx.db).to_string();

                    let mut diag = errors::ty_mismatch(
                        &expected_ty,
                        &found_ty,
                        self.obligation.span(),
                    );

                    match *self.obligation.kind() {
                        ObligationKind::Obvious => (),
                        ObligationKind::Exprs(expected_span, found_span) => {
                            diag.push_labels([
                                Label::secondary(expected_span)
                                    .with_message(expected_ty.to_string()),
                                Label::secondary(found_span)
                                    .with_message(found_ty.to_string()),
                            ]);
                        }
                        ObligationKind::ReturnTy(return_ty_span) => {
                            diag.push_label(
                                Label::secondary(return_ty_span)
                                    .with_message("because of return type"),
                            );
                        }
                    }

                    diag
                }
                UnifyError::InfiniteTy { ty } => {
                    let ty = self.cx.normalize(ty);
                    let obligation =
                        Obligation::obvious(self.obligation.span());

                    Diagnostic::error()
                        .with_message(format!(
                            "type `{}` is an infinite type",
                            ty.display(self.cx.db)
                        ))
                        .with_label(Label::primary(obligation.span()))
                }
            };

            EqError { expected, found, diagnostic }
        })
    }
}

impl Ty {
    pub fn unify(
        self,
        other: Ty,
        cx: &Typeck,
        options: UnifyOptions,
    ) -> UnifyResult {
        UnifyCx { cx, options }.unify_ty_ty(self, other)
    }

    pub fn can_unify(
        self,
        other: Ty,
        cx: &Typeck,
        options: UnifyOptions,
    ) -> UnifyResult {
        let snapshot = cx.storage.borrow_mut().snapshot();
        let result = self.unify(other, cx, options);
        cx.storage.borrow_mut().rollback_to(snapshot);
        result
    }
}

#[derive(Debug, Clone, Copy)]
pub struct UnifyOptions {
    pub unify_param_tys: bool,
}

impl Default for UnifyOptions {
    fn default() -> Self {
        Self::new()
    }
}

impl UnifyOptions {
    pub fn new() -> Self {
        Self { unify_param_tys: false }
    }
}

pub type EqResult = Result<(), EqError>;

#[derive(Debug)]
pub struct EqError {
    pub expected: Ty,
    pub found: Ty,
    pub diagnostic: Diagnostic,
}

impl From<EqError> for Diagnostic {
    fn from(value: EqError) -> Self {
        value.diagnostic
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

struct UnifyCx<'db, 'a> {
    cx: &'a Typeck<'db>,
    options: UnifyOptions,
}

impl UnifyCx<'_, '_> {
    fn unify_ty_ty(&mut self, a: Ty, b: Ty) -> UnifyResult {
        let (a, b) = (self.cx.normalize(a), self.cx.normalize(b));

        match (a.kind(), b.kind()) {
            (TyKind::Bool, TyKind::Bool)
            | (TyKind::Unit, TyKind::Unit)
            | (TyKind::Str, TyKind::Str) => Ok(()),

            (TyKind::Uint(a), TyKind::Uint(b)) if a == b => Ok(()),
            (TyKind::Int(a), TyKind::Int(b)) if a == b => Ok(()),
            (TyKind::Float(a), TyKind::Float(b)) if a == b => Ok(()),

            (TyKind::Fn(ref fex), TyKind::Fn(ref fact)) => {
                self.unify_ty_ty(fex.ret, fact.ret)?;

                if fex.params.len() == fact.params.len() {
                    for (p1, p2) in fex.params.iter().zip(fact.params.iter()) {
                        self.unify_ty_ty(p1.ty, p2.ty)?;
                    }

                    if fex.is_c_variadic == fact.is_c_variadic {
                        Ok(())
                    } else {
                        Err(UnifyError::TyMismatch { a, b })
                    }
                } else {
                    Err(UnifyError::TyMismatch { a, b })
                }
            }

            (TyKind::Adt(id_a, targs_a), TyKind::Adt(id_b, targs_b)) => {
                if id_a == id_b && targs_a.len() == targs_b.len() {
                    for (t1, t2) in targs_a.iter().zip(targs_b.iter()) {
                        self.unify_ty_ty(*t1, *t2)?;
                    }

                    Ok(())
                } else {
                    Err(UnifyError::TyMismatch { a, b })
                }
            }

            (TyKind::Ref(a, ma), TyKind::Ref(b, mb)) if *ma == *mb => {
                self.unify_ty_ty(*a, *b)
            }

            (TyKind::RawPtr(a), TyKind::RawPtr(b)) => self.unify_ty_ty(*a, *b),

            // Unify ?T1 ~ ?T2
            (TyKind::Infer(InferTy::Ty(a)), TyKind::Infer(InferTy::Ty(b))) => {
                self.cx.storage.borrow_mut().ty.unify_var_var(*a, *b)?;
                Ok(())
            }

            // Unify ?int ~ ?int
            (
                TyKind::Infer(InferTy::Int(a)),
                TyKind::Infer(InferTy::Int(b)),
            ) => {
                self.cx.storage.borrow_mut().int.unify_var_var(*a, *b)?;
                Ok(())
            }

            // Unify ?int ~ int
            (TyKind::Int(ity), TyKind::Infer(InferTy::Int(var)))
            | (TyKind::Infer(InferTy::Int(var)), TyKind::Int(ity)) => {
                self.cx
                    .storage
                    .borrow_mut()
                    .int
                    .unify_var_value(*var, Some(IntVarValue::Int(*ity)))?;
                Ok(())
            }

            // Unify ?int ~ uint
            (TyKind::Uint(uty), TyKind::Infer(InferTy::Int(var)))
            | (TyKind::Infer(InferTy::Int(var)), TyKind::Uint(uty)) => {
                self.cx
                    .storage
                    .borrow_mut()
                    .int
                    .unify_var_value(*var, Some(IntVarValue::Uint(*uty)))?;
                Ok(())
            }

            // Unify ?float ~ ?float
            (
                TyKind::Infer(InferTy::Float(a)),
                TyKind::Infer(InferTy::Float(b)),
            ) => {
                self.cx.storage.borrow_mut().float.unify_var_var(*a, *b)?;
                Ok(())
            }

            // Unify ?float ~ float
            (TyKind::Float(fty), TyKind::Infer(InferTy::Float(var)))
            | (TyKind::Infer(InferTy::Float(var)), TyKind::Float(fty)) => {
                self.cx
                    .storage
                    .borrow_mut()
                    .float
                    .unify_var_value(*var, Some(*fty))?;
                Ok(())
            }

            // Unify ?T ~ T
            (TyKind::Infer(InferTy::Ty(var)), _) => self.unify_ty_var(b, *var),

            // Unify T ~ ?T
            (_, TyKind::Infer(InferTy::Ty(var))) => self.unify_ty_var(a, *var),

            (TyKind::Param(p1), TyKind::Param(p2)) if p1.var == p2.var => {
                if p1.var == p2.var {
                    Ok(())
                } else {
                    Err(UnifyError::TyMismatch { a, b })
                }
            }

            (TyKind::Param(_), _) | (_, TyKind::Param(_))
                if self.options.unify_param_tys =>
            {
                Ok(())
            }

            (_, _) => Err(UnifyError::TyMismatch { a, b }),
        }
    }

    fn unify_ty_var(&mut self, ty: Ty, var: TyVar) -> UnifyResult {
        ty.occurs_check(var).map_err(|ty| UnifyError::InfiniteTy { ty })?;
        self.cx.storage.borrow_mut().ty.unify_var_value(var, Some(ty))?;
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
        "IntVar"
    }
}

impl EqUnifyValue for IntVarValue {}

impl UnifyKey for FloatVar {
    type Value = Option<FloatTy>;

    fn index(&self) -> u32 {
        (*self).into()
    }

    fn from_index(u: u32) -> Self {
        Self::from(u)
    }

    fn tag() -> &'static str {
        "FloatVar"
    }
}

impl EqUnifyValue for FloatTy {}

pub type UnifyResult = Result<(), UnifyError>;

pub enum UnifyError {
    TyMismatch { a: Ty, b: Ty },
    InfiniteTy { ty: Ty },
}

impl From<(Ty, Ty)> for UnifyError {
    fn from((a, b): (Ty, Ty)) -> Self {
        Self::TyMismatch { a, b }
    }
}

impl<A, B> From<(A, B)> for UnifyError
where
    A: Into<TyKind>,
    B: Into<TyKind>,
{
    fn from((a, b): (A, B)) -> Self {
        Self::TyMismatch { a: Ty::new(a.into()), b: Ty::new(b.into()) }
    }
}
