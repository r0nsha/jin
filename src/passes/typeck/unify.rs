use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    passes::typeck::{constraint::Constraint, infcx::InferCtxt, normalize::NormalizeTy},
    ty::{InferType, IntType, IntVar, IntVarValue, TypeKind, TypeVar},
};

impl<'db> InferCtxt<'db> {
    pub fn unification(&mut self) -> Result<(), InferError> {
        let constraints = self.constraints.clone();

        for constraint in constraints.iter() {
            match constraint {
                Constraint::Eq { expected, actual } => {
                    self.unify_ty_ty(&self.db[*expected].clone(), &self.db[*actual].clone())?;
                }
            }
        }

        Ok(())
    }

    fn unify_ty_ty(&mut self, expected: &TypeKind, actual: &TypeKind) -> Result<(), InferError> {
        let expected = expected.clone().normalize(self);
        let actual = actual.clone().normalize(self);

        match (expected, actual) {
            (TypeKind::Never(_), _)
            | (_, TypeKind::Never(_))
            | (TypeKind::Bool(_), TypeKind::Bool(_))
            | (TypeKind::Unit(_), TypeKind::Unit(_))
            | (TypeKind::Int(IntType::Int, _), TypeKind::Int(IntType::Int, _)) => Ok(()),

            (ref expected @ TypeKind::Function(ref fex), ref actual @ TypeKind::Function(ref fact)) => {
                self.unify_ty_ty(&fex.ret, &fact.ret)?;

                if fex.params.len() == fact.params.len() {
                    for (p1, p2) in fex.params.iter().zip(fact.params.iter()) {
                        self.unify_ty_ty(&p1.ty, &p2.ty)?;
                    }

                    Ok(())
                } else {
                    Err(InferError::TypesNotEq {
                        expected: expected.clone(),
                        actual: actual.clone(),
                    })
                }
            }

            // Unify ?X ~ ?Y
            (
                TypeKind::Infer(InferType::TypeVar(expected), _),
                TypeKind::Infer(InferType::TypeVar(actual), _),
            ) => {
                self.ty_unification_table.unify_var_var(expected, actual)?;
                Ok(())
            }

            // Unify ?int ~ ?int
            (
                TypeKind::Infer(InferType::IntVar(expected), _),
                TypeKind::Infer(InferType::IntVar(actual), _),
            ) => {
                self.int_unification_table.unify_var_var(expected, actual)?;
                Ok(())
            }

            // Unify ?int ~ int
            (TypeKind::Int(ity, span), TypeKind::Infer(InferType::IntVar(var), _))
            | (TypeKind::Infer(InferType::IntVar(var), _), TypeKind::Int(ity, span)) => {
                self.int_unification_table
                    .unify_var_value(var, Some(IntVarValue::Int(ity, span)))?;
                Ok(())
            }

            // Unify ?N ~ any
            (TypeKind::Infer(InferType::TypeVar(var), _), actual) => self.unify_ty_var(actual, var),
            (expected, TypeKind::Infer(InferType::TypeVar(var), _)) => self.unify_ty_var(expected, var),

            (expected, actual) => Err(InferError::TypesNotEq { expected, actual }),
        }
    }

    fn unify_ty_var(&mut self, expected: TypeKind, var: TypeVar) -> Result<(), InferError> {
        expected.occurs_check(var).map_err(|ty| InferError::InfiniteType { var, ty })?;
        self.ty_unification_table.unify_var_value(var, Some(expected))?;
        Ok(())
    }
}

impl From<(TypeKind, TypeKind)> for InferError {
    fn from((expected, actual): (TypeKind, TypeKind)) -> Self {
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
        expected: TypeKind,
        actual: TypeKind,
    },
    InfiniteType {
        ty: TypeKind,
        #[allow(unused)]
        var: TypeVar,
    },
}

impl InferError {
    pub fn into_diagnostic(self, db: &Db) -> Diagnostic {
        match self {
            Self::TypesNotEq { expected, actual } => Diagnostic::error("infer::incompatible_types")
                .with_message(format!(
                    "expected `{}`, got `{}` instead",
                    expected.display(db),
                    actual.display(db),
                ))
                .with_label(Label::primary(expected.span()).with_message(format!(
                    "expected type `{}` originates here",
                    expected.display(db)
                )))
                .with_label(
                    Label::secondary(actual.span())
                        .with_message(format!("found type `{}` here", actual.display(db))),
                ),
            Self::InfiniteType { ty, .. } => Diagnostic::error("infer::infinite_type")
                .with_message(format!("type `{}` is an infinite type", ty.display(db)))
                .with_label(Label::primary(ty.span())),
        }
    }
}

impl UnifyKey for TypeVar {
    type Value = Option<TypeKind>;

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

impl EqUnifyValue for TypeKind {}

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
