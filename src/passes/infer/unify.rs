use ena::unify::UnifyKey;

use crate::ty::*;

use super::{constraint::Constraint, error::InferError, InferCx};

impl<'db> InferCx<'db> {
    pub(crate) fn unification(&mut self) -> Result<(), InferError> {
        for constraint in self.constraints.clone().iter() {
            match constraint {
                Constraint::Eq { expected, actual } => self.unify_ty_ty(
                    &expected.get(&self.db).clone(),
                    &actual.get(&self.db).clone(),
                )?,
            }
        }
        Ok(())
    }

    fn unify_ty_ty(&mut self, expected: &Ty, actual: &Ty) -> Result<(), InferError> {
        let expected = self.normalize_ty(expected.clone());
        let actual = self.normalize_ty(actual.clone());

        match (&expected.kind, &actual.kind) {
            (TyKind::Function(expected), TyKind::Function(actual)) => {
                self.unify_ty_ty(&expected.ret, &actual.ret)
            }

            (TyKind::Var(expected), TyKind::Var(actual)) => self
                .typecx
                .unification_table
                .unify_var_var(*expected, *actual)
                .map_err(|(expected, actual)| InferError::TypesNotEq { expected, actual }),

            (TyKind::Var(var), _) => {
                actual
                    .occurs_check(*var)
                    .map_err(|ty| InferError::InfiniteType { var: *var, ty })?;

                self.typecx
                    .unification_table
                    .unify_var_value(*var, Some(actual))
                    .map_err(|(expected, actual)| InferError::TypesNotEq { expected, actual })
            }

            (_, TyKind::Var(var)) => {
                expected
                    .occurs_check(*var)
                    .map_err(|ty| InferError::InfiniteType { var: *var, ty })?;

                self.typecx
                    .unification_table
                    .unify_var_value(*var, Some(expected))
                    .map_err(|(expected, actual)| InferError::TypesNotEq { expected, actual })
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
            TyKind::Var(var) => match self.typecx.unification_table.probe_value(var) {
                Some(ty) => self.normalize_ty(ty),
                None => ty,
            },
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
