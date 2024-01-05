use crate::{
    ast,
    db::DefKind,
    diagnostics::{Diagnostic, Label},
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
        pat_ty: Ty,
        parent_span: Span,
    ) -> TypeckResult<hir::MatchPat> {
        match pat {
            ast::MatchPat::Name(word, mutability) => {
                let id = self.define_def(
                    env,
                    Vis::Private,
                    DefKind::Variable,
                    *word,
                    *mutability,
                    pat_ty,
                )?;

                Ok(hir::MatchPat::Name(id, word.span()))
            }
            ast::MatchPat::Wildcard(span) => Ok(hir::MatchPat::Wildcard(*span)),
            ast::MatchPat::Unit(span) => {
                self.at(Obligation::exprs(*span, *span, parent_span))
                    .eq(self.db.types.unit, pat_ty)?;
                Ok(hir::MatchPat::Unit(*span))
            }
            ast::MatchPat::Bool(value, span) => {
                self.at(Obligation::exprs(*span, *span, parent_span))
                    .eq(self.db.types.bool, pat_ty)?;
                Ok(hir::MatchPat::Bool(*value, *span))
            }
            ast::MatchPat::Int(value, span) => {
                if *value < 0 {
                    match pat_ty.kind() {
                        TyKind::Int(_) | TyKind::Infer(InferTy::Int(_)) => (),
                        _ => {
                            return Err(errors::ty_mismatch(
                                "int",
                                &pat_ty.to_string(self.db),
                                *span,
                            ))
                        }
                    }
                } else {
                    self.at(Obligation::exprs(*span, *span, parent_span))
                        .eq(self.fresh_int_var(), pat_ty)?;
                }

                Ok(hir::MatchPat::Int(*value, *span))
            }
            ast::MatchPat::Str(value, span) => {
                self.at(Obligation::exprs(*span, *span, parent_span))
                    .eq(self.db.types.str, pat_ty)?;
                Ok(hir::MatchPat::Str(*value, *span))
            }
            ast::MatchPat::Adt(path, subpats, span) => {
                let id = self.path_lookup(env, path)?;
                let def = &self.db[id];

                match def.kind.as_ref() {
                    &DefKind::Adt(adt_id) => {
                        self.at(Obligation::exprs(*span, parent_span, *span))
                            .eq(pat_ty, self.db[adt_id].ty())?;

                        let fields =
                            self.db[adt_id].as_struct().unwrap().fields.clone();

                        let mut new_subpats =
                            vec![hir::MatchPat::Wildcard(*span); fields.len()];

                        for (idx, subpat) in subpats.iter().enumerate() {
                            match subpat {
                                ast::Subpat::Positional(subpat) => {
                                    if let Some(field) = fields.get(idx) {
                                        let new_subpat = self.check_match_pat(
                                            env,
                                            subpat,
                                            field.ty,
                                            field.span(),
                                        )?;

                                        new_subpats[idx] = new_subpat;
                                    } else {
                                        return Err(Diagnostic::error()
                                            .with_message(format!(
                                                "expected at most {} patterns \
                                                 for type `{}`",
                                                fields.len(),
                                                self.db[adt_id].name
                                            ))
                                            .with_label(
                                                Label::primary(subpat.span())
                                                    .with_message(
                                                        "pattern doesn't map \
                                                         to any field",
                                                    ),
                                            ));
                                    }
                                }
                            }
                        }

                        // TODO: report missing fields

                        Ok(hir::MatchPat::Adt(adt_id, new_subpats, *span))
                    }
                    _ => Err(Diagnostic::error()
                        .with_message(format!(
                            "expected a named type, found value of type `{}`",
                            def.ty.display(self.db)
                        ))
                        .with_label(
                            Label::primary(*span)
                                .with_message("expected a named type"),
                        )),
                }
            }
        }
    }
}
