use ena::unify::{EqUnifyValue, UnifyKey};

use crate::db::Database;
use crate::passes::infer::normalize::NormalizeTy;
use crate::{
    diagnostics::{Diagnostic, Label},
    ty::IntTy,
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

        match (expected, actual) {
            (Ty::Function(expected), Ty::Function(actual)) => {
                self.unify_ty_ty(&expected.ret, &actual.ret)
            }

            (Ty::Var(expected, _), Ty::Var(actual, _)) => self
                .tcx
                .unification_table
                .unify_var_var(expected, actual)
                .map_err(|(expected, actual)| InferError::TypesNotEq {
                    expected,
                    actual,
                }),

            (Ty::Never(_), _)
            | (_, Ty::Never(_))
            | (Ty::Unit(_), Ty::Unit(_))
            | (Ty::Int(IntTy::Int, _), Ty::Int(IntTy::Int, _)) => Ok(()),

            (Ty::Var(var, _), actual) => self.unify_ty_var(actual, var),
            (expected, Ty::Var(var, _)) => self.unify_ty_var(expected, var),

            (expected, actual) => {
                Err(InferError::TypesNotEq { expected, actual })
            }
        }
    }

    fn unify_ty_var(
        &mut self,
        expected: Ty,
        var: TyVar,
    ) -> Result<(), InferError> {
        expected
            .occurs_check(var)
            .map_err(|ty| InferError::InfiniteType { var, ty })?;

        self.tcx.unification_table.unify_var_value(var, Some(expected)).map_err(
            |(expected, actual)| InferError::TypesNotEq { expected, actual },
        )
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
            Self::TypesNotEq { expected, actual } => {
                Diagnostic::error("infer::incompatible_types")
                    .with_message(format!(
                        "expected `{}`, got `{}` instead",
                        expected.display(db),
                        actual.display(db),
                    ))
                    .with_label(Label::primary(expected.span()).with_message(
                        format!(
                            "expected type `{}` originates here",
                            expected.display(db)
                        ),
                    ))
                    .with_label(Label::secondary(actual.span()).with_message(
                        format!("found type `{}` here", actual.display(db)),
                    ))
            }
            Self::InfiniteType { ty, .. } => {
                Diagnostic::error("infer::infinite_type")
                    .with_message(format!(
                        "type `{}` is an infinite type",
                        ty.display(db)
                    ))
                    .with_label(Label::primary(ty.span()))
            }
        }
    }
}
