use ena::unify::{EqUnifyValue, UnifyKey};

use crate::db::Database;
use crate::passes::infer::normalize::NormalizeTy;
use crate::{
    diagnostics::{Diagnostic, Label},
    ty::{IntTy, TyKind},
    ty::{Ty, TyVar},
};

use super::{constraint::Constraint, InferCx};

impl<'db> InferCx<'db> {
    pub fn unification(&mut self) -> Result<(), InferError> {
        // PERF: can I remove this clone?
        let constraints = self.constraints.clone();

        for constraint in constraints.iter() {
            match constraint {
                Constraint::Eq { expected, actual } => self.unify_ty_ty(
                    &self.db[*expected].clone(),
                    &self.db[*actual].clone(),
                )?,
            }
        }

        Ok(())
    }

    fn unify_ty_ty(
        &mut self,
        expected: &Ty,
        actual: &Ty,
    ) -> Result<(), InferError> {
        let expected = expected.clone().normalize(&mut self.tcx);
        let actual = actual.clone().normalize(&mut self.tcx);

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

            (TyKind::Never, _)
            | (_, TyKind::Never)
            | (TyKind::Unit, TyKind::Unit)
            | (TyKind::Int(IntTy::Int), TyKind::Int(IntTy::Int)) => Ok(()),

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

            (_, _) => Err(InferError::TypesNotEq { expected, actual }),
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

pub enum InferError {
    TypesNotEq {
        expected: Ty,
        actual: Ty,
    },
    InfiniteType {
        ty: Ty,
        #[allow(unused)]
        var: TyVar,
    },
}

impl InferError {
    pub fn into_diagnostic(self, db: &Database) -> Diagnostic {
        match self {
            InferError::TypesNotEq { expected, actual } => {
                Diagnostic::error("infer::incompatible_types")
                    .with_message(format!(
                        "expected `{}`, got `{}` instead",
                        expected.display(db),
                        actual.display(db),
                    ))
                    .with_label(Label::primary(expected.span).with_message(
                        format!(
                            "expected type `{}` originates here",
                            expected.display(db)
                        ),
                    ))
                    .with_label(Label::secondary(actual.span).with_message(
                        format!("found type `{}` here", actual.display(db)),
                    ))
            }
            InferError::InfiniteType { ty, .. } => {
                Diagnostic::error("infer::infinite_type")
                    .with_message(format!(
                        "type `{}` is an infinite type",
                        ty.display(db)
                    ))
                    .with_label(Label::primary(ty.span))
            }
        }
    }
}
