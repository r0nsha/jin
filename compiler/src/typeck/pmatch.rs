use itertools::Itertools;
use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    db::{AdtField, AdtId, AdtKind, DefId, DefKind, VariantId},
    diagnostics::{Diagnostic, Label},
    hir,
    middle::Vis,
    span::{Span, Spanned as _},
    ty::{InferTy, Instantiation, Ty, TyKind},
    typeck::{
        coerce::CoerceExt as _,
        env::{Env, PathLookup, ScopeKind},
        errors,
        errors::field_not_found,
        unify::Obligation,
        Typeck, TypeckResult,
    },
    word::WordMap,
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
            let new_arm = self.check_match_arm(env, arm, expr_ty, expr.span, expected_ty)?;

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
            hir::ExprKind::Match(hir::Match { expr: Box::new(expr), arms: new_arms }),
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

            let pat = self.check_match_pat(env, &case.pat, expr_ty, expr_span, &mut names)?;
            self.check_match_pat_name_bound_once(&pat)?;

            let guard = if let Some(guard) = &case.guard {
                let bool_ty = self.db.types.bool;
                let guard = self.check_expr(env, guard, Some(bool_ty))?;
                self.at(Obligation::obvious(guard.span))
                    .eq(bool_ty, guard.ty)
                    .or_coerce(self, guard.id)?;
                Some(Box::new(guard))
            } else {
                None
            };

            let expr = self.check_expr(env, &case.expr, expected_ty)?;

            Ok(hir::MatchArm { pat, guard, expr: Box::new(expr) })
        })
    }

    #[allow(clippy::too_many_lines)]
    fn check_match_pat(
        &mut self,
        env: &mut Env,
        pat: &ast::MatchPat,
        pat_ty: Ty,
        parent_span: Span,
        names: &mut UstrMap<DefId>,
    ) -> TypeckResult<hir::MatchPat> {
        let derefed_ty = pat_ty.auto_deref();

        match pat {
            ast::MatchPat::Name(word, mutability) => {
                let id = if let Some(id) = names.get(&word.name()) {
                    // We make sure that names in all alternatives are bound to the same type
                    let expected_ty = self.def_ty(*id);

                    if let Err(err) = self
                        .at(Obligation::exprs(word.span(), self.db[*id].span, word.span()))
                        .eq(expected_ty, pat_ty)
                    {
                        return Err(Diagnostic::error(format!(
                            "in the same arm, the identifier `{word}` must have the same type in \
                             all alternatives",
                        ))
                        .with_label(Label::primary(
                            word.span(),
                            format!("has type `{}` here", err.found.display(self.db)),
                        ))
                        .with_label(Label::secondary(
                            self.db[*id].span,
                            format!(
                                "previously introduce with type `{}`",
                                err.expected.display(self.db)
                            ),
                        )));
                    }

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

                    names.insert(word.name(), id);

                    id
                };

                Ok(hir::MatchPat::Name(id, self.def_ty(id), word.span()))
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
            ast::MatchPat::Adt(pat) => {
                self.check_match_pat_adt(env, pat, pat_ty, parent_span, names)
            }
            ast::MatchPat::Or(pats, span) => {
                let pats: Vec<_> = pats
                    .iter()
                    .map(|pat| self.check_match_pat(env, pat, pat_ty, *span, names))
                    .try_collect()?;

                let all_bound: Vec<_> = pats
                    .iter()
                    .map(|pat| {
                        let mut bound = UstrMap::default();
                        self.collect_bound_names_in_pat(pat, &mut bound);
                        bound
                    })
                    .collect();

                let mut name_counts: UstrMap<usize> =
                    all_bound.iter().flat_map(|b| b.keys().map(|name| (*name, 0))).collect();

                for bound in all_bound {
                    for name in bound.into_keys() {
                        *name_counts.get_mut(&name).unwrap() += 1;
                    }
                }

                for (name, count) in name_counts {
                    if count < pats.len() {
                        return Err(Diagnostic::error(format!(
                            "identifier `{name}` is not bound in all alternatives"
                        ))
                        .with_label(Label::primary(*span, "in these patterns")));
                    }
                }

                Ok(hir::MatchPat::Or(pats, *span))
            }
        }
    }

    fn check_match_pat_adt(
        &mut self,
        env: &mut Env,
        pat: &ast::MatchPatAdt,
        pat_ty: Ty,
        parent_span: Span,
        names: &mut UstrMap<DefId>,
    ) -> TypeckResult<hir::MatchPat> {
        if pat.is_inferred_variant {
            let pat_ty_deref = pat_ty.auto_deref();
            let union_def = pat_ty_deref
                .as_union(self.db)
                .ok_or_else(|| errors::expected_union_ty(self.db, pat_ty, pat.span))?;

            debug_assert!(pat.path.len() == 1);
            let variant = self.lookup_variant_in_union(union_def, pat.path[0], pat.span)?;

            return self.check_match_pat_variant(env, pat, pat_ty, parent_span, names, variant.id);
        }

        match self.path_lookup(env, &pat.path)? {
            PathLookup::Def(id) => {
                let def = &self.db[id];

                match def.kind.as_ref() {
                    &DefKind::Adt(adt_id) => {
                        self.check_match_pat_struct(env, pat, pat_ty, parent_span, names, adt_id)
                    }
                    _ => Err(errors::expected_named_ty(self.def_ty(id).display(self.db), pat.span)),
                }
            }
            PathLookup::Variant(variant_id) => {
                self.check_match_pat_variant(env, pat, pat_ty, parent_span, names, variant_id)
            }
        }
    }

    fn check_match_pat_struct(
        &mut self,
        env: &mut Env,
        pat: &ast::MatchPatAdt,
        pat_ty: Ty,
        parent_span: Span,
        names: &mut UstrMap<DefId>,
        adt_id: AdtId,
    ) -> TypeckResult<hir::MatchPat> {
        let instantiation =
            self.check_match_pat_adt_ty(env, pat.span, pat_ty, parent_span, adt_id)?;

        let fields = match &self.db[adt_id].kind {
            AdtKind::Struct(s) => s.fields.clone(),
            AdtKind::Union(_) => {
                return Err(Diagnostic::error(format!(
                    "expected a struct, but `{}` is a union type",
                    pat_ty.display(self.db)
                ))
                .with_label(Label::primary(pat.span, "expected a struct type")))
            }
        };

        let new_subpats =
            self.check_match_pat_subpats(env, pat, pat_ty, names, adt_id, &fields, &instantiation)?;

        Ok(hir::MatchPat::Adt(adt_id, new_subpats, pat.span))
    }

    fn check_match_pat_variant(
        &mut self,
        env: &mut Env,
        pat: &ast::MatchPatAdt,
        pat_ty: Ty,
        parent_span: Span,
        names: &mut UstrMap<DefId>,
        variant_id: VariantId,
    ) -> TypeckResult<hir::MatchPat> {
        let adt_id = self.db[variant_id].adt_id;

        let instantiation =
            self.check_match_pat_adt_ty(env, pat.span, pat_ty, parent_span, adt_id)?;

        let new_subpats = self.check_match_pat_subpats(
            env,
            pat,
            pat_ty,
            names,
            adt_id,
            &self.db[variant_id].fields.clone(),
            &instantiation,
        )?;

        Ok(hir::MatchPat::Variant(variant_id, new_subpats, pat.span))
    }

    fn check_match_pat_adt_ty(
        &mut self,
        env: &mut Env,
        span: Span,
        pat_ty: Ty,
        parent_span: Span,
        adt_id: AdtId,
    ) -> TypeckResult<Instantiation> {
        let adt_ty = self.db[adt_id].ty();
        let instantiation = self.fresh_instantiation(env, adt_ty.collect_params());

        self.at(Obligation::exprs(span, parent_span, span))
            .eq(pat_ty.auto_deref(), instantiation.fold(adt_ty))?;

        Ok(instantiation)
    }

    #[allow(clippy::too_many_lines)]
    #[allow(clippy::too_many_arguments)]
    fn check_match_pat_subpats(
        &mut self,
        env: &mut Env,
        pat: &ast::MatchPatAdt,
        pat_ty: Ty,
        names: &mut UstrMap<DefId>,
        adt_id: AdtId,
        fields: &[AdtField],
        instantiation: &Instantiation,
    ) -> TypeckResult<Vec<hir::MatchPat>> {
        let adt_name = self.db[adt_id].name.name();

        let mut used_fields = WordMap::default();
        let mut use_field = |name: Ustr, span: Span| {
            if let Some(prev_span) = used_fields.insert_split(name, span) {
                let dup_span = span;

                Err(Diagnostic::error(format!("field `{name}` has already been matched"))
                    .with_label(Label::primary(dup_span, format!("`{name}` matched again here")))
                    .with_label(Label::secondary(prev_span, format!("first match of `{name}`"))))
            } else {
                Ok(())
            }
        };

        let mut new_subpats = vec![hir::MatchPat::Wildcard(pat.span); fields.len()];

        for (idx, subpat) in pat.subpats.iter().enumerate() {
            let (field_idx, field, subpat, field_use_span) = match subpat {
                ast::Subpat::Positional(subpat) => {
                    if let Some(field) = fields.get(idx) {
                        (idx, field, subpat, subpat.span())
                    } else {
                        return Err(Diagnostic::error(format!(
                            "expected at most {} patterns for type `{}`",
                            fields.len(),
                            adt_name
                        ))
                        .with_label(Label::primary(
                            subpat.span(),
                            "pattern doesn't map to any field",
                        )));
                    }
                }
                ast::Subpat::Named(name, subpat) => {
                    if let Some((field_idx, field)) =
                        fields.iter().enumerate().find(|(_, f)| f.name.name() == name.name())
                    {
                        (field_idx, field, subpat, name.span())
                    } else {
                        return Err(field_not_found(
                            self.db,
                            self.db[adt_id].ty(),
                            pat.span,
                            *name,
                        ));
                    }
                }
            };

            use_field(field.name.name(), field_use_span)?;

            self.check_field_access(env, &self.db[adt_id], field, field_use_span)?;

            let field_ty = instantiation.fold(field.ty);
            let field_ty = match pat_ty.kind() {
                TyKind::Ref(_, mutability) => {
                    // If the parent pattern's type is a reference, the field's type is
                    // implicitly a reference too.
                    field_ty.create_ref(*mutability)
                }
                _ => field_ty,
            };

            let new_subpat = self.check_match_pat(env, subpat, field_ty, field.span(), names)?;

            new_subpats[field_idx] = new_subpat;
        }

        if pat.is_exhaustive {
            Self::check_match_pat_adt_missing_fields(adt_name, fields, &used_fields, pat.span)?;
        }

        Ok(new_subpats)
    }

    fn check_match_pat_adt_missing_fields(
        adt_name: Ustr,
        fields: &[AdtField],
        used_fields: &WordMap,
        span: Span,
    ) -> TypeckResult<()> {
        let missing_fields: Vec<_> =
            fields.iter().filter(|f| !used_fields.contains(f.name)).collect();

        if missing_fields.is_empty() {
            Ok(())
        } else {
            Err(Diagnostic::error(format!(
                "missing {} field(s) in `{}` pattern: {}",
                missing_fields.len(),
                adt_name,
                missing_fields.into_iter().map(|f| format!("`{}`", f.name)).join(", ")
            ))
            .with_label(Label::primary(span, "pattern is not exhaustive"))
            .with_note("if this is intentional, use `..` at the end of the pattern"))
        }
    }

    fn collect_bound_names_in_pat(&self, pat: &hir::MatchPat, bound: &mut UstrMap<Vec<Span>>) {
        match pat {
            hir::MatchPat::Name(id, _, span) => {
                bound.entry(self.db[*id].name).or_default().push(*span);
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

    fn check_match_pat_name_bound_once(&self, pat: &hir::MatchPat) -> TypeckResult<()> {
        let mut bound = UstrMap::default();
        self.collect_bound_names_in_pat(pat, &mut bound);

        for (name, spans) in bound {
            if spans.len() > 1 {
                return Err(Diagnostic::error(format!(
                    "identifier `{name}` is bound more than once in the same pattern"
                ))
                .with_labels(spans.into_iter().map(|s| Label::primary(s, "here"))));
            }
        }

        Ok(())
    }
}
