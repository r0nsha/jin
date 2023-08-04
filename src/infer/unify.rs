use crate::ty::{IntType, Type, TypeKind};

use super::{
    constraint::{Constraint, Constraints},
    error::InferError,
    InferCx,
};

impl<'a> InferCx<'a> {
    pub(crate) fn unification(&mut self, constraints: &Constraints) -> Result<(), InferError> {
        for constraint in constraints.iter() {
            match constraint {
                Constraint::TypeEq { expected, actual } => self.unify_ty_ty(
                    &expected.get(&self.db).clone(),
                    &actual.get(&self.db).clone(),
                )?,
            }
        }
        Ok(())
    }

    fn unify_ty_ty(&mut self, expected: &Type, actual: &Type) -> Result<(), InferError> {
        let expected = self.normalize_ty(expected.clone());
        let actual = self.normalize_ty(actual.clone());

        match (&expected.kind, &actual.kind) {
            (TypeKind::Fun(expected), TypeKind::Fun(actual)) => {
                self.unify_ty_ty(&expected.ret, &actual.ret)
            }

            (TypeKind::Var(expected), TypeKind::Var(actual)) => self
                .typecx
                .unification_table
                .unify_var_var(*expected, *actual)
                .map_err(|(expected, actual)| InferError::TypesNotEq { expected, actual }),

            (TypeKind::Var(var), _) => {
                actual
                    .occurs_check(*var)
                    .map_err(|ty| InferError::InfiniteType { var: *var, ty })?;

                self.typecx
                    .unification_table
                    .unify_var_value(*var, Some(actual))
                    .map_err(|(expected, actual)| InferError::TypesNotEq { expected, actual })
            }

            (_, TypeKind::Var(var)) => {
                expected
                    .occurs_check(*var)
                    .map_err(|ty| InferError::InfiniteType { var: *var, ty })?;

                self.typecx
                    .unification_table
                    .unify_var_value(*var, Some(expected))
                    .map_err(|(expected, actual)| InferError::TypesNotEq { expected, actual })
            }

            (TypeKind::Int(IntType::Int), TypeKind::Int(IntType::Int)) => Ok(()),

            (_, _) => Err(InferError::TypesNotEq { expected, actual }),
        }
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty.kind {
            TypeKind::Fun(fun) => {
                let ret = self.normalize_ty(*fun.ret);
                Type::fun(ret, ty.span)
            }
            TypeKind::Var(var) => match self.typecx.unification_table.probe_value(var) {
                Some(ty) => self.normalize_ty(ty),
                None => ty,
            },
            TypeKind::Int(_) | TypeKind::Unit | TypeKind::Never => ty,
        }
    }
}
