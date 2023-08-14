use crate::db::Database;
use crate::passes::infer::normalize::NormalizeTy;
use crate::ty::{InferTy, IntVarValue};
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
            (Ty::Never(_), _)
            | (_, Ty::Never(_))
            | (Ty::Unit(_), Ty::Unit(_))
            | (Ty::Int(IntTy::Int, _), Ty::Int(IntTy::Int, _)) => Ok(()),

            (Ty::Function(expected), Ty::Function(actual)) => {
                self.unify_ty_ty(&expected.ret, &actual.ret)
            }

            (
                Ty::Infer(InferTy::TyVar(expected), _),
                Ty::Infer(InferTy::TyVar(actual), _),
            ) => {
                self.tcx
                    .ty_unification_table
                    .unify_var_var(expected, actual)?;
                Ok(())
            }

            (
                Ty::Infer(InferTy::IntVar(expected), _),
                Ty::Infer(InferTy::IntVar(actual), _),
            ) => {
                self.tcx
                    .int_unification_table
                    .unify_var_var(expected, actual)?;
                Ok(())
            }

            (Ty::Int(ity, span), Ty::Infer(InferTy::IntVar(var), _))
            | (Ty::Infer(InferTy::IntVar(var), _), Ty::Int(ity, span)) => {
                self.tcx
                    .int_unification_table
                    .unify_var_value(var, Some(IntVarValue::Int(ity, span)))?;
                Ok(())
            }

            (Ty::Infer(InferTy::TyVar(var), _), actual) => {
                self.unify_ty_var(actual, var)
            }
            (expected, Ty::Infer(InferTy::TyVar(var), _)) => {
                self.unify_ty_var(expected, var)
            }

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

        self.tcx.ty_unification_table.unify_var_value(var, Some(expected))?;

        Ok(())
    }
}

impl From<(Ty, Ty)> for InferError {
    fn from((expected, actual): (Ty, Ty)) -> Self {
        Self::TypesNotEq { expected, actual }
    }
}

impl From<(IntVarValue, IntVarValue)> for InferError {
    fn from((expected, actual): (IntVarValue, IntVarValue)) -> Self {
        Self::TypesNotEq { expected: expected.into(), actual: actual.into() }
    }
}

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
