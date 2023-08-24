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
    pub fn at(&mut self, cause: Cause) -> At<'db, '_> {
        At { infcx: self, cause }
    }
}

pub struct At<'db, 'icx> {
    infcx: &'icx mut InferCtxt<'db>,
    cause: Cause,
}

impl At<'_, '_> {
    pub fn eq(&mut self, expected: Type, found: Type) -> Result<(), InferError> {
        UnifyCtxt { infcx: self.infcx }.unify_ty_ty(expected, found).map_err(|err| match err {
            UnifyError::TypeMismatch { .. } => InferError::TypeMismatch {
                expected: expected.normalize(self.infcx),
                found: found.normalize(self.infcx),
                cause: self.cause,
            },
            UnifyError::InfiniteType { ty } => InferError::InfiniteType {
                ty: ty.normalize(self.infcx),
                cause: Cause::obvious(self.cause.span()),
            },
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Cause {
    span: Span,
    kind: CauseKind,
}

impl Cause {
    pub fn new(span: Span, kind: CauseKind) -> Self {
        Self { span, kind }
    }

    pub fn obvious(span: Span) -> Self {
        Self::new(span, CauseKind::Obvious)
    }

    pub fn exprs(span: Span, expected: Span, found: Span) -> Self {
        Self::new(span, CauseKind::Exprs(expected, found))
    }

    pub fn return_ty(span: Span, return_ty_span: Span) -> Self {
        Self::new(span, CauseKind::ReturnTy(return_ty_span))
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> &CauseKind {
        &self.kind
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CauseKind {
    /// Should be obvious from the span
    Obvious,
    /// Two expressions which are expected to have the same type
    Exprs(Span, Span),
    /// An expression which was expected to be equal to the return type
    ReturnTy(Span),
}

struct UnifyCtxt<'db, 'icx> {
    infcx: &'icx mut InferCtxt<'db>,
}

impl<'db, 'icx> UnifyCtxt<'db, 'icx> {
    fn unify_ty_ty(&mut self, a: Type, b: Type) -> Result<(), UnifyError> {
        let a = a.normalize(self.infcx);
        let b = b.normalize(self.infcx);

        match (a.as_ref(), b.as_ref()) {
            (TypeKind::Never, _)
            | (_, TypeKind::Never)
            | (TypeKind::Bool, TypeKind::Bool)
            | (TypeKind::Unit, TypeKind::Unit)
            | (TypeKind::Int(IntType::Int), TypeKind::Int(IntType::Int)) => Ok(()),

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
                TypeKind::Infer(InferType::TypeVar(expected)),
                TypeKind::Infer(InferType::TypeVar(found)),
            ) => {
                self.infcx.ty_unification_table.unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ ?int
            (
                TypeKind::Infer(InferType::IntVar(expected)),
                TypeKind::Infer(InferType::IntVar(found)),
            ) => {
                self.infcx.int_unification_table.unify_var_var(*expected, *found)?;
                Ok(())
            }

            // Unify ?int ~ int
            (TypeKind::Int(ity), TypeKind::Infer(InferType::IntVar(var)))
            | (TypeKind::Infer(InferType::IntVar(var)), TypeKind::Int(ity)) => {
                self.infcx
                    .int_unification_table
                    .unify_var_value(*var, Some(IntVarValue::Int(*ity)))?;
                Ok(())
            }

            // Unify ?T ~ T
            (TypeKind::Infer(InferType::TypeVar(var)), _) => self.unify_ty_var(b, *var),

            // Unify T ~ ?T
            (_, TypeKind::Infer(InferType::TypeVar(var))) => self.unify_ty_var(a, *var),

            (_, _) => Err(UnifyError::TypeMismatch { a, b }),
        }
    }

    fn unify_ty_var(&mut self, ty: Type, var: TypeVar) -> Result<(), UnifyError> {
        ty.occurs_check(var).map_err(|ty| UnifyError::InfiniteType { ty })?;
        self.infcx.ty_unification_table.unify_var_value(var, Some(ty))?;
        Ok(())
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

pub enum UnifyError {
    TypeMismatch { a: Type, b: Type },
    InfiniteType { ty: Type },
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

pub enum InferError {
    TypeMismatch { expected: Type, found: Type, cause: Cause },
    InfiniteType { ty: Type, cause: Cause },
}

impl InferError {
    pub fn into_diagnostic(self, db: &Db) -> Diagnostic {
        match self {
            Self::TypeMismatch { expected, found, cause } => {
                let expected_ty = expected.display(db).to_string();
                let found_ty = found.display(db).to_string();

                let msg = format!("expected `{expected_ty}`, found `{found_ty}`");

                let mut diag = Diagnostic::error("infer::type_mismatch")
                    .with_message(msg.clone())
                    .with_label(Label::primary(cause.span()).with_message(msg));

                match *cause.kind() {
                    CauseKind::Obvious => (),
                    CauseKind::Exprs(expected_span, found_span) => diag.push_labels([
                        Label::secondary(expected_span)
                            .with_message(format!("expected `{expected_ty}`")),
                        Label::secondary(found_span).with_message(format!("found `{found_ty}`")),
                    ]),
                    CauseKind::ReturnTy(return_ty_span) => {
                        diag.push_label(Label::secondary(return_ty_span).with_message(format!(
                            "expected `{expected_ty}` because of return type"
                        )));
                    }
                }

                diag
            }
            Self::InfiniteType { ty, cause } => Diagnostic::error("infer::infinite_type")
                .with_message(format!("type `{}` is an infinite type", ty.display(db)))
                .with_label(Label::primary(cause.span())),
        }
    }
}
