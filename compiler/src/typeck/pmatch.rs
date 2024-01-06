use itertools::Itertools;
use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    db::{AdtField, Def, DefId, DefKind},
    diagnostics::{Diagnostic, Label},
    hir,
    middle::Vis,
    span::{Span, Spanned as _},
    ty::{InferTy, Ty, TyKind},
    typeck::{
        coerce::CoerceExt as _,
        env::{Env, ScopeKind},
        errors,
        errors::field_not_found,
        unify::Obligation,
        Typeck, TypeckResult,
    },
    word::Word,
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
            let mut names = UstrMap::default();
            let pat = self.check_match_pat(
                env, &case.pat, expr_ty, expr_span, &mut names,
            )?;
            self.check_match_pat_name_bound_once(&pat)?;
            let expr = self.check_expr(env, &case.expr, expected_ty)?;
            Ok(hir::MatchArm { pat, expr: Box::new(expr) })
        })
    }

    #[allow(clippy::too_many_lines)]
    fn check_match_pat(
        &mut self,
        env: &mut Env,
        pat: &ast::MatchPat,
        pat_ty: Ty,
        parent_span: Span,
        pat_names: &mut UstrMap<DefId>,
    ) -> TypeckResult<hir::MatchPat> {
        let derefed_ty = pat_ty.auto_deref();

        match pat {
            ast::MatchPat::Name(word, mutability) => {
                let id = if let Some(id) = pat_names.get(&word.name()) {
                    *id
                } else {
                    let id = self.define_def(
                        env,
                        Vis::Private,
                        DefKind::Variable,
                        *word,
                        *mutability,
                        pat_ty,
                    )?;

                    pat_names.insert(word.name(), id);

                    id
                };

                Ok(hir::MatchPat::Name(id, word.span()))
            }
            ast::MatchPat::Wildcard(span) => Ok(hir::MatchPat::Wildcard(*span)),
            ast::MatchPat::Unit(span) => {
                self.at(Obligation::exprs(*span, *span, parent_span))
                    .eq(self.db.types.unit, derefed_ty)?;
                Ok(hir::MatchPat::Unit(*span))
            }
            ast::MatchPat::Bool(value, span) => {
                self.at(Obligation::exprs(*span, *span, parent_span))
                    .eq(self.db.types.bool, derefed_ty)?;
                Ok(hir::MatchPat::Bool(*value, *span))
            }
            ast::MatchPat::Int(value, span) => {
                if *value < 0 {
                    match derefed_ty.kind() {
                        TyKind::Int(_) | TyKind::Infer(InferTy::Int(_)) => (),
                        _ => {
                            return Err(errors::ty_mismatch(
                                "int",
                                &derefed_ty.to_string(self.db),
                                *span,
                            ))
                        }
                    }
                } else {
                    self.at(Obligation::exprs(*span, *span, parent_span))
                        .eq(self.fresh_int_var(), derefed_ty)?;
                }

                Ok(hir::MatchPat::Int(*value, *span))
            }
            ast::MatchPat::Str(value, span) => {
                self.at(Obligation::exprs(*span, *span, parent_span))
                    .eq(self.db.types.str, derefed_ty)?;
                Ok(hir::MatchPat::Str(*value, *span))
            }
            ast::MatchPat::Adt(path, subpats, is_exhaustive, span) => self
                .check_match_pat_adt(
                    env,
                    path,
                    subpats,
                    *is_exhaustive,
                    *span,
                    pat_ty,
                    parent_span,
                    pat_names,
                ),
            ast::MatchPat::Or(pats, span) => {
                let pats: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        self.check_match_pat(env, pat, pat_ty, *span, pat_names)
                    })
                    .try_collect()?;

                let all_bound: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let mut bound = UstrMap::default();
                        self.collect_bound_names_in_pat(pat, &mut bound);
                        bound
                    })
                    .collect();

                let mut name_counts: UstrMap<usize> = all_bound
                    .iter()
                    .flat_map(|b| b.keys().map(|name| (*name, 0)))
                    .collect();

                for bound in all_bound {
                    for name in bound.into_keys() {
                        *name_counts.get_mut(&name).unwrap() += 1;
                    }
                }

                for (name, count) in name_counts {
                    if count < pats.len() {
                        return Err(Diagnostic::error()
                            .with_message(format!(
                                "identifier `{name}` is not bound in all \
                                 alternatives"
                            ))
                            .with_label(
                                Label::primary(*span)
                                    .with_message("in these patterns"),
                            ));
                    }
                }

                Ok(hir::MatchPat::Or(pats, *span))
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    #[allow(clippy::too_many_lines)]
    fn check_match_pat_adt(
        &mut self,
        env: &mut Env,
        path: &[Word],
        subpats: &[ast::Subpat],
        is_exhaustive: bool,
        span: Span,
        pat_ty: Ty,
        parent_span: Span,
        names: &mut UstrMap<DefId>,
    ) -> TypeckResult<hir::MatchPat> {
        let id = self.path_lookup(env, path)?;
        let def = &self.db[id];

        match def.kind.as_ref() {
            &DefKind::Adt(adt_id) => {
                let adt_name = self.db[adt_id].name.name();
                let adt_ty = self.db[adt_id].ty();
                let instantiation =
                    self.fresh_instantiation(env, adt_ty.collect_params());

                self.at(Obligation::exprs(span, parent_span, span))
                    .eq(pat_ty.auto_deref(), instantiation.fold(adt_ty))?;

                let fields =
                    self.db[adt_id].as_struct().unwrap().fields.clone();

                let mut used_fields = UstrMap::default();
                let mut use_field = |name: Ustr, span: Span| {
                    if let Some(prev_span) = used_fields.insert(name, span) {
                        let dup_span = span;

                        Err(Diagnostic::error()
                            .with_message(format!(
                                "field `{name}` has already been matched"
                            ))
                            .with_label(Label::primary(dup_span).with_message(
                                format!("`{name}` matched again here"),
                            ))
                            .with_label(
                                Label::secondary(prev_span).with_message(
                                    format!("first match of `{name}`"),
                                ),
                            ))
                    } else {
                        Ok(())
                    }
                };

                let mut new_subpats =
                    vec![hir::MatchPat::Wildcard(span); fields.len()];

                for (idx, subpat) in subpats.iter().enumerate() {
                    let (field_idx, field, subpat, field_use_span) =
                        match subpat {
                            ast::Subpat::Positional(subpat) => {
                                if let Some(field) = fields.get(idx) {
                                    (idx, field, subpat, subpat.span())
                                } else {
                                    return Err(Diagnostic::error()
                                        .with_message(format!(
                                            "expected at most {} patterns for \
                                             type `{}`",
                                            fields.len(),
                                            adt_name
                                        ))
                                        .with_label(
                                            Label::primary(subpat.span())
                                                .with_message(
                                                    "pattern doesn't map to \
                                                     any field",
                                                ),
                                        ));
                                }
                            }
                            ast::Subpat::Named(name, subpat) => {
                                if let Some((field_idx, field)) = fields
                                    .iter()
                                    .enumerate()
                                    .find(|(_, f)| f.name.name() == name.name())
                                {
                                    (field_idx, field, subpat, name.span())
                                } else {
                                    return Err(field_not_found(
                                        self.db,
                                        self.db[adt_id].ty(),
                                        span,
                                        *name,
                                    ));
                                }
                            }
                        };

                    use_field(field.name.name(), field_use_span)?;

                    self.check_field_access(
                        env,
                        &self.db[adt_id],
                        field,
                        field_use_span,
                    )?;

                    let field_ty = instantiation.fold(field.ty);
                    let field_ty = match pat_ty.kind() {
                        TyKind::Ref(_, mutability) => {
                            // If the parent pattern's type is a reference, the field's type is
                            // implicitly a reference too.
                            field_ty.create_ref(*mutability)
                        }
                        _ => field_ty,
                    };

                    let new_subpat = self.check_match_pat(
                        env,
                        subpat,
                        field_ty,
                        field.span(),
                        names,
                    )?;

                    new_subpats[field_idx] = new_subpat;
                }

                if is_exhaustive {
                    Self::check_match_pat_adt_missing_fields(
                        adt_name,
                        &fields,
                        &used_fields,
                        span,
                    )?;
                }

                Ok(hir::MatchPat::Adt(adt_id, new_subpats, span))
            }
            _ => Err(Diagnostic::error()
                .with_message(format!(
                    "expected a named type, found value of type `{}`",
                    def.ty.display(self.db)
                ))
                .with_label(
                    Label::primary(span).with_message("expected a named type"),
                )),
        }
    }

    // TODO: WordMap
    fn check_match_pat_adt_missing_fields(
        adt_name: Ustr,
        fields: &[AdtField],
        used_fields: &UstrMap<Span>,
        span: Span,
    ) -> TypeckResult<()> {
        let missing_fields: Vec<_> = fields
            .iter()
            .filter(|f| !used_fields.contains_key(&f.name.name()))
            .collect();

        if missing_fields.is_empty() {
            Ok(())
        } else {
            Err(Diagnostic::error()
                .with_message(format!(
                    "missing {} field(s) in `{}` pattern: {}",
                    missing_fields.len(),
                    adt_name,
                    missing_fields
                        .into_iter()
                        .map(|f| format!("`{}`", f.name))
                        .join(", ")
                ))
                .with_label(
                    Label::primary(span)
                        .with_message("pattern is not exhaustive"),
                )
                .with_note(
                    "if this is intentional, use `..` at the end of the \
                     pattern",
                ))
        }
    }

    // TODO: WordMap
    fn collect_bound_names_in_pat(
        &self,
        pat: &hir::MatchPat,
        bound: &mut UstrMap<Vec<Span>>,
    ) {
        match pat {
            hir::MatchPat::Name(id, _) => {
                let Def { name, span, .. } = self.db[*id];
                bound.entry(name).or_default().push(span);
            }
            hir::MatchPat::Adt(_, subpats, _) => {
                for pat in subpats {
                    self.collect_bound_names_in_pat(pat, bound);
                }
            }
            hir::MatchPat::Or(pats, _) => {
                for pat in pats {
                    self.collect_bound_names_in_pat(pat, bound);
                }
            }
            _ => (),
        }
    }

    fn check_match_pat_name_bound_once(
        &self,
        pat: &hir::MatchPat,
    ) -> TypeckResult<()> {
        let mut bound = UstrMap::default();
        self.collect_bound_names_in_pat(pat, &mut bound);

        for (name, spans) in bound {
            if spans.len() > 1 {
                return Err(Diagnostic::error()
                    .with_message(format!(
                        "identifier `{name}` is bound more than once in the \
                         same pattern"
                    ))
                    .with_labels(
                        spans
                            .into_iter()
                            .map(|s| Label::primary(s).with_message("here")),
                    ));
            }
        }

        Ok(())
    }
}
