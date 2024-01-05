use crate::{
    ast,
    db::DefKind,
    hir,
    middle::Vis,
    span::{Span, Spanned as _},
    ty::{InferTy, Ty, TyKind},
    typeck::{
        coerce::CoerceExt as _,
        env::{Env, ScopeKind},
        errors,
        unify::Obligation,
        Typeck, TypeckResult,
    },
};

impl<'db> Typeck<'db> {
    pub(super) fn check_match(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        arms: &[ast::MatchArm],
        span: Span,
        expected_ty: Option<Ty>,
    ) -> TypeckResult<hir::Expr> {
        let expr = self.check_expr(env, expr, None)?;
        let expr_ty = self.normalize(expr.ty);

        let mut new_arms = Vec::<hir::MatchArm>::new();
        let mut result_ty: Option<Ty> = expected_ty;
        let mut last_case_span: Option<Span> = None;

        for arm in arms {
            let new_arm = self.check_match_arm(
                env,
                arm,
                expr_ty,
                expr.span,
                expected_ty,
            )?;

            if let Some(result_ty) = result_ty {
                self.at(if let Some(last_case_span) = last_case_span {
                    Obligation::exprs(span, last_case_span, new_arm.expr.span)
                } else {
                    Obligation::obvious(new_arm.expr.span)
                })
                .eq(result_ty, new_arm.expr.ty)
                .or_coerce(self, new_arm.expr.id)?;
            } else {
                result_ty = Some(new_arm.expr.ty);
            }

            last_case_span = Some(new_arm.expr.span);

            new_arms.push(new_arm);
        }

        Ok(self.expr(
            hir::ExprKind::Match(hir::Match {
                expr: Box::new(expr),
                arms: new_arms,
            }),
            result_ty.unwrap_or(self.db.types.unit),
            span,
        ))
    }

    fn check_match_arm(
        &mut self,
        env: &mut Env,
        case: &ast::MatchArm,
        expr_ty: Ty,
        expr_span: Span,
        expected_ty: Option<Ty>,
    ) -> TypeckResult<hir::MatchArm> {
        env.with_anon_scope(ScopeKind::Block, |env| {
            let pat =
                self.check_match_pat(env, &case.pat, expr_ty, expr_span)?;
            let expr = self.check_expr(env, &case.expr, expected_ty)?;
            Ok(hir::MatchArm { pat, expr: Box::new(expr) })
        })
    }

    fn check_match_pat(
        &mut self,
        env: &mut Env,
        pat: &ast::MatchPat,
        expr_ty: Ty,
        expr_span: Span,
    ) -> TypeckResult<hir::MatchPat> {
        match pat {
            ast::MatchPat::Name(word, mutability) => {
                let id = self.define_def(
                    env,
                    Vis::Private,
                    DefKind::Variable,
                    *word,
                    *mutability,
                    expr_ty,
                )?;

                Ok(hir::MatchPat::Name(id, word.span()))
            }
            ast::MatchPat::Wildcard(span) => Ok(hir::MatchPat::Wildcard(*span)),
            ast::MatchPat::Unit(span) => {
                self.at(Obligation::exprs(*span, *span, expr_span))
                    .eq(self.db.types.unit, expr_ty)?;
                Ok(hir::MatchPat::Unit(*span))
            }
            ast::MatchPat::Bool(value, span) => {
                self.at(Obligation::exprs(*span, *span, expr_span))
                    .eq(self.db.types.bool, expr_ty)?;
                Ok(hir::MatchPat::Bool(*value, *span))
            }
            ast::MatchPat::Int(value, span) => {
                if *value < 0 {
                    match expr_ty.kind() {
                        TyKind::Int(_) | TyKind::Infer(InferTy::Int(_)) => (),
                        _ => {
                            return Err(errors::ty_mismatch(
                                "int",
                                &expr_ty.to_string(self.db),
                                *span,
                            ))
                        }
                    }
                } else {
                    self.at(Obligation::exprs(*span, *span, expr_span))
                        .eq(self.fresh_int_var(), expr_ty)?;
                }

                Ok(hir::MatchPat::Int(*value, *span))
            }
            ast::MatchPat::Str(value, span) => {
                self.at(Obligation::exprs(*span, *span, expr_span))
                    .eq(self.db.types.str, expr_ty)?;
                Ok(hir::MatchPat::Str(*value, *span))
            }
        }
    }
}
