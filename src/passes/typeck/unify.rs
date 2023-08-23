use ena::unify::{EqUnifyValue, UnifyKey};

use crate::{
    db::Db,
    diagnostics::{Diagnostic, Label},
    passes::typeck::{infcx::InferCtxt, normalize::NormalizeTy},
    span::Span,
    ty::{InferType, IntType, IntVar, IntVarValue, Type, TypeKind, TypeVar},
};

impl<'db> InferCtxt<'db> {
    #[inline]
    #[must_use]
    pub fn at(&mut self, span: Span) -> At<'db, '_> {
        At { infcx: self, span }
    }
}

pub struct At<'db, 'icx> {
    infcx: &'icx mut InferCtxt<'db>,
    span: Span,
}

impl At<'_, '_> {
    pub fn eq(&mut self, expected: Type, found: Type) -> Result<(), InferError> {
        UnifyCtxt { infcx: self.infcx }.unify_ty_ty(expected, found).map_err(|err| match err {
            UnifyError::TypeMismatch { .. } => InferError::TypeMismatch { expected, found },
            UnifyError::InfiniteType { ty } => InferError::InfiniteType { ty },
        })
    }
}

struct UnifyCtxt<'db, 'icx> {
    infcx: &'icx mut InferCtxt<'db>,
}

impl<'db, 'icx> UnifyCtxt<'db, 'icx> {
    fn unify_ty_ty(&mut self, a: Type, b: Type) -> Result<(), UnifyError> {
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
                    Err(UnifyError::TypeMismatch { a, b })
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

            (_, _) => Err(UnifyError::TypeMismatch { a, b }),
        }
    }

    fn unify_ty_var(&mut self, ty: Type, var: TypeVar) -> Result<(), UnifyError> {
        ty.occurs_check(var).map_err(|ty| UnifyError::InfiniteType { ty })?;
        self.infcx.ty_unification_table.unify_var_value(var, Some(ty))?;
        Ok(())
    }
}

impl From<(Type, Type)> for UnifyError {
    fn from((a, b): (Type, Type)) -> Self {
        Self::TypeMismatch { a, b }
    }
}

impl From<(IntVarValue, IntVarValue)> for UnifyError {
    fn from((a, b): (IntVarValue, IntVarValue)) -> Self {
        Self::TypeMismatch { a: Type::new(a.into()), b: Type::new(b.into()) }
    }
}

pub enum UnifyError {
    TypeMismatch { a: Type, b: Type },
    InfiniteType { ty: Type },
}

pub enum InferError {
    TypeMismatch { expected: Type, found: Type },
    InfiniteType { ty: Type },
}

impl InferError {
    pub fn into_diagnostic(self, db: &Db) -> Diagnostic {
        match self {
            Self::TypeMismatch { expected, found } => {
                Diagnostic::error("infer::incompatible_types")
                    .with_message(format!(
                        "expected `{}`, found `{}` instead",
                        expected.display(db),
                        found.display(db),
                    ))
                    .with_label(
                        Label::primary(found.span())
                            .with_message(format!("found type `{}` here", found.display(db))),
                    )
                // .with_label(Label::secondary(expected.span()).with_message(format!(
                //     "expected type `{}` originates here",
                //     expected.display(db)
                // )))
            }
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
