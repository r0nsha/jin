use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{
    db::Database,
    diagnostics::{Diagnostic, Label},
    passes::typeck::{constraint::Constraint, infcx::InferCtxt, normalize::NormalizeTy},
    ty::{InferType, IntType, IntVar, IntVarValue, Type, TypeVar},
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

    fn unify_ty_ty(&mut self, expected: &Type, actual: &Type) -> Result<(), InferError> {
        let expected = expected.clone().normalize(self);
        let actual = actual.clone().normalize(self);

        match (expected, actual) {
            (Type::Never(_), _)
            | (_, Type::Never(_))
            | (Type::Bool(_), Type::Bool(_))
            | (Type::Unit(_), Type::Unit(_))
            | (Type::Int(IntType::Int, _), Type::Int(IntType::Int, _)) => Ok(()),

            (ref expected @ Type::Function(ref fex), ref actual @ Type::Function(ref fact)) => {
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
                Type::Infer(InferType::TypeVar(expected), _),
                Type::Infer(InferType::TypeVar(actual), _),
            ) => {
                self.ty_unification_table.unify_var_var(expected, actual)?;
                Ok(())
            }

            // Unify ?int ~ ?int
            (
                Type::Infer(InferType::IntVar(expected), _),
                Type::Infer(InferType::IntVar(actual), _),
            ) => {
                self.int_unification_table.unify_var_var(expected, actual)?;
                Ok(())
            }

            // Unify ?int ~ int
            (Type::Int(ity, span), Type::Infer(InferType::IntVar(var), _))
            | (Type::Infer(InferType::IntVar(var), _), Type::Int(ity, span)) => {
                self.int_unification_table
                    .unify_var_value(var, Some(IntVarValue::Int(ity, span)))?;
                Ok(())
            }

            // Unify ?N ~ any
            (Type::Infer(InferType::TypeVar(var), _), actual) => self.unify_ty_var(actual, var),
            (expected, Type::Infer(InferType::TypeVar(var), _)) => self.unify_ty_var(expected, var),

            (expected, actual) => Err(InferError::TypesNotEq { expected, actual }),
        }
    }

    fn unify_ty_var(&mut self, expected: Type, var: TypeVar) -> Result<(), InferError> {
        expected.occurs_check(var).map_err(|ty| InferError::InfiniteType { var, ty })?;
        self.ty_unification_table.unify_var_value(var, Some(expected))?;
        Ok(())
    }
}

impl From<(Type, Type)> for InferError {
    fn from((expected, actual): (Type, Type)) -> Self {
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
        expected: Type,
        actual: Type,
    },
    InfiniteType {
        ty: Type,
        #[allow(unused)]
        var: TypeVar,
    },
}

impl InferError {
    pub fn into_diagnostic(self, db: &Database) -> Diagnostic {
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
    type Value = Option<Type>;

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

impl EqUnifyValue for Type {}

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
