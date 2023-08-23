use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    passes::typeck::{constraint::Constraint, infcx::InferCtxt, normalize::NormalizeTy},
    ty::{InferType, IntType, IntVar, IntVarValue, Type, TypeKind, TypeVar},
};

impl<'db> InferCtxt<'db> {
    pub fn unification(&'db mut self) -> Result<(), InferError> {
        let constraints = self.constraints.clone();

        for constraint in constraints.iter() {
            match constraint {
                Constraint::Eq { expected, found } => {
                    UnifyCtxt { infcx: self, a: *expected, b: *found, a_is_expected: true }
                        .unify_ty_ty(*expected, *found)?;
                }
            }
        }

        Ok(())
    }

    fn unify_ty_ty(&mut self, expected: Type, found: Type) -> Result<(), InferError> {
        let expected = expected.normalize(self);
        let found = found.normalize(self);

        match (expected.as_ref(), found.as_ref()) {
            (TypeKind::Never(_), _)
            | (_, TypeKind::Never(_))
            | (TypeKind::Bool(_), TypeKind::Bool(_))
            | (TypeKind::Unit(_), TypeKind::Unit(_))
            | (TypeKind::Int(IntType::Int, _), TypeKind::Int(IntType::Int, _)) => Ok(()),

            (TypeKind::Function(ref fex), TypeKind::Function(ref fact)) => {
                self.unify_ty_ty(fex.ret, fact.ret)?;

                if fex.params.len() == fact.params.len() {
                    for (p1, p2) in fex.params.iter().zip(fact.params.iter()) {
                        self.unify_ty_ty(p1.ty, p2.ty)?;
                    }

                    Ok(())
                } else {
                    Err(InferError::TypesNotEq { expected, found })
                }
            }

            // Unify ?T1 ~ ?T2
            (
                TypeKind::Infer(InferType::TypeVar(expected), _),
                TypeKind::Infer(InferType::TypeVar(found), _),
            ) => {
                self.ty_unification_table.unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ ?int
            (
                TypeKind::Infer(InferType::IntVar(expected), _),
                TypeKind::Infer(InferType::IntVar(found), _),
            ) => {
                self.int_unification_table.unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ int
            (TypeKind::Int(ity, span), TypeKind::Infer(InferType::IntVar(var), _))
            | (TypeKind::Infer(InferType::IntVar(var), _), TypeKind::Int(ity, span)) => {
                self.int_unification_table
                    .unify_var_value(*var, Some(IntVarValue::Int(*ity, *span)))?;
                Ok(())
            }

            // Unify ?T ~ T
            (TypeKind::Infer(InferType::TypeVar(var), _), found) => {
                self.unify_ty_var(Type::from(found), *var)
            }

            // Unify T ~ ?T
            (expected, TypeKind::Infer(InferType::TypeVar(var), _)) => {
                self.unify_ty_var(Type::from(expected), *var)
            }

            (expected, found) => Err(InferError::TypesNotEq {
                expected: Type::from(expected),
                found: Type::from(found),
            }),
        }
    }

    fn unify_ty_var(&mut self, expected: Type, var: TypeVar) -> Result<(), InferError> {
        expected.occurs_check(var).map_err(|ty| InferError::InfiniteType { var, ty })?;
        self.ty_unification_table.unify_var_value(var, Some(expected))?;
        Ok(())
    }
}

struct UnifyCtxt<'db, 'cx> {
    infcx: &'cx mut InferCtxt<'db>,
    a: Type,
    b: Type,
    a_is_expected: bool,
}

impl<'db, 'cx> UnifyCtxt<'db, 'cx> {
    fn unify_ty_ty(&mut self, a: Type, b: Type) -> Result<(), InferError> {
        let a = a.normalize(self.infcx);
        let b = b.normalize(self.infcx);

        match (a.as_ref(), b.as_ref()) {
            (TypeKind::Never(_), _)
            | (_, TypeKind::Never(_))
            | (TypeKind::Bool(_), TypeKind::Bool(_))
            | (TypeKind::Unit(_), TypeKind::Unit(_))
            | (TypeKind::Int(IntType::Int, _), TypeKind::Int(IntType::Int, _)) => Ok(()),

            (TypeKind::Function(ref fex), TypeKind::Function(ref fact)) => {
                self.unify_ty_ty(fex.ret, fact.ret)?;

                if fex.params.len() == fact.params.len() {
                    for (p1, p2) in fex.params.iter().zip(fact.params.iter()) {
                        self.unify_ty_ty(p1.ty, p2.ty)?;
                    }

                    Ok(())
                } else {
                    Err(self.ty_unification_err())
                }
            }

            // Unify ?T1 ~ ?T2
            (
                TypeKind::Infer(InferType::TypeVar(expected), _),
                TypeKind::Infer(InferType::TypeVar(found), _),
            ) => {
                self.infcx.ty_unification_table.unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ ?int
            (
                TypeKind::Infer(InferType::IntVar(expected), _),
                TypeKind::Infer(InferType::IntVar(found), _),
            ) => {
                self.infcx.int_unification_table.unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ int
            (TypeKind::Int(ity, span), TypeKind::Infer(InferType::IntVar(var), _))
            | (TypeKind::Infer(InferType::IntVar(var), _), TypeKind::Int(ity, span)) => {
                self.infcx
                    .int_unification_table
                    .unify_var_value(*var, Some(IntVarValue::Int(*ity, *span)))?;
                Ok(())
            }

            // Unify ?T ~ T
            (TypeKind::Infer(InferType::TypeVar(var), _), _) => self.unify_ty_var(b, *var),

            // Unify T ~ ?T
            (_, TypeKind::Infer(InferType::TypeVar(var), _)) => self.unify_ty_var(a, *var),

            (_, _) => Err(self.ty_unification_err()),
        }
    }

    fn unify_ty_var(&mut self, expected: Type, var: TypeVar) -> Result<(), InferError> {
        expected.occurs_check(var).map_err(|ty| InferError::InfiniteType { var, ty })?;
        self.infcx.ty_unification_table.unify_var_value(var, Some(expected))?;
        Ok(())
    }

    fn ty_unification_err(&self) -> InferError {
        let ExpectedFound { expected, found } =
            ExpectedFound::new(self.a_is_expected, self.a, self.b);
        InferError::TypesNotEq { expected, found }
    }
}

impl From<(Type, Type)> for InferError {
    fn from((expected, found): (Type, Type)) -> Self {
        Self::TypesNotEq { expected, found }
    }
}

impl From<(IntVarValue, IntVarValue)> for InferError {
    fn from((expected, found): (IntVarValue, IntVarValue)) -> Self {
        Self::TypesNotEq { expected: Type::new(expected.into()), found: Type::new(found.into()) }
    }
}

pub enum InferError {
    TypesNotEq {
        expected: Type,
        found: Type,
    },
    InfiniteType {
        ty: Type,
        #[allow(unused)]
        var: TypeVar,
    },
}

impl InferError {
    pub fn into_diagnostic(self, db: &Db) -> Diagnostic {
        match self {
            Self::TypesNotEq { expected, found } => Diagnostic::error("infer::incompatible_types")
                .with_message(format!(
                    "expected `{}`, got `{}` instead",
                    expected.display(db),
                    found.display(db),
                ))
                .with_label(Label::primary(expected.span()).with_message(format!(
                    "expected type `{}` originates here",
                    expected.display(db)
                )))
                .with_label(
                    Label::secondary(found.span())
                        .with_message(format!("found type `{}` here", found.display(db))),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpectedFound<T> {
    pub expected: T,
    pub found: T,
}

impl<T> ExpectedFound<T> {
    pub fn new(a_is_expected: bool, a: T, b: T) -> Self {
        if a_is_expected {
            ExpectedFound { expected: a, found: b }
        } else {
            ExpectedFound { expected: b, found: a }
        }
    }
}
