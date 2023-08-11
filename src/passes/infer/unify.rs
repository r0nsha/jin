use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{
    diagnostics::{Diagnostic, Label},
    ty::*,
    ty::{Ty, TyVar},
};

use super::{constraint::Constraint, InferCx};

impl<'db> InferCx<'db> {
    pub(crate) fn unification(&mut self) -> Result<(), InferError> {
        let mut constraints = self.constraints.clone();
        constraints.sort();

        for constraint in constraints.iter() {
            match constraint {
                Constraint::Eq { expected, actual } => self.unify_ty_ty(
                    &expected.get(self.db).clone(),
                    &actual.get(self.db).clone(),
                )?,
                Constraint::Callable { callee } => {
                    self.unify_callable(callee.get(self.db).clone())?
                }
            }
        }

        Ok(())
    }

    fn unify_callable(&mut self, callee: Ty) -> Result<(), InferError> {
        let callee = self.normalize_ty(callee);
        dbg!(&self.tcx.unification_table);
        dbg!(&callee);

        match &callee.kind {
            TyKind::Function(_) => Ok(()),
            _ => Err(InferError::NotCallable { ty: callee }),
        }
    }

    fn unify_ty_ty(
        &mut self,
        expected: &Ty,
        actual: &Ty,
    ) -> Result<(), InferError> {
        let expected = self.normalize_ty(expected.clone());
        let actual = self.normalize_ty(actual.clone());

        match (&expected.kind, &actual.kind) {
            (TyKind::Function(expected), TyKind::Function(actual)) => {
                self.unify_ty_ty(&expected.ret, &actual.ret)
            }

            (TyKind::Var(expected), TyKind::Var(actual)) => self
                .tcx
                .unification_table
                .unify_var_var(*expected, *actual)
                .map_err(|(expected, actual)| InferError::TypesNotEq {
                    expected,
                    actual,
                }),

            (TyKind::Var(var), _) => {
                actual
                    .occurs_check(*var)
                    .map_err(|ty| InferError::InfiniteType { var: *var, ty })?;

                self.tcx
                    .unification_table
                    .unify_var_value(*var, Some(actual))
                    .map_err(|(expected, actual)| InferError::TypesNotEq {
                        expected,
                        actual,
                    })
            }

            (_, TyKind::Var(var)) => {
                expected
                    .occurs_check(*var)
                    .map_err(|ty| InferError::InfiniteType { var: *var, ty })?;

                self.tcx
                    .unification_table
                    .unify_var_value(*var, Some(expected))
                    .map_err(|(expected, actual)| InferError::TypesNotEq {
                        expected,
                        actual,
                    })
            }

            (TyKind::Never, _)
            | (_, TyKind::Never)
            | (TyKind::Int(IntTy::Int), TyKind::Int(IntTy::Int)) => Ok(()),

            (_, _) => Err(InferError::TypesNotEq { expected, actual }),
        }
    }

    fn normalize_ty(&mut self, ty: Ty) -> Ty {
        match ty.kind {
            TyKind::Function(fun) => {
                let ret = self.normalize_ty(*fun.ret);
                Ty::fun(ret, ty.span)
            }
            TyKind::Var(var) => {
                match self.tcx.unification_table.probe_value(var) {
                    Some(ty) => self.normalize_ty(ty),
                    None => ty,
                }
            }
            TyKind::Int(_) | TyKind::Unit | TyKind::Never => ty,
        }
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

pub(crate) enum InferError {
    TypesNotEq { expected: Ty, actual: Ty },
    InfiniteType { ty: Ty, var: TyVar },
    NotCallable { ty: Ty },
}

impl From<InferError> for Diagnostic {
    fn from(err: InferError) -> Self {
        match err {
            InferError::TypesNotEq { expected, actual } => {
                Diagnostic::error("infer::incompatible_types")
                    .with_message(format!(
                        "expected `{expected}`, got `{actual}` instead"
                    ))
                    .with_label(Label::primary(expected.span).with_message(
                        format!("expected type `{expected}` originates here"),
                    ))
                    .with_label(
                        Label::secondary(actual.span).with_message(format!(
                            "found type `{actual}` here"
                        )),
                    )
            }
            InferError::InfiniteType { ty, .. } => {
                Diagnostic::error("infer::infinite_type")
                    .with_message(format!("type `{ty}` has an infinite size"))
                    .with_label(Label::primary(ty.span))
            }
            InferError::NotCallable { ty } => {
                Diagnostic::error("infer::not_callable")
                    .with_message(format!(
                        "type `{ty}` is neither a function nor a struct"
                    ))
                    .with_label(
                        Label::primary(ty.span).with_message("not callable"),
                    )
            }
        }
    }
}
