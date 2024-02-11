use itertools::Itertools;
use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    db::{AdtField, AdtId, AdtKind, DefId, DefKind, VariantId},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    hir,
    middle::Mutability,
    span::{Span, Spanned as _},
    ty::{InferTy, Instantiation, Ty, TyKind},
    typeck::{
        coerce::CoerceExt as _,
        errors,
        errors::field_not_found,
        exprs,
        lookup::PathLookup,
        ns::{Env, ScopeKind},
        types,
        unify::Obligation,
        Typeck,
    },
    word::{Word, WordMap},
};

pub(super) fn check(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    expr: &ast::Expr,
    arms: &[ast::MatchArm],
    span: Span,
    expected_ty: Option<Ty>,
) -> DiagnosticResult<hir::Expr> {
    let expr = exprs::check_expr(cx, env, expr, None)?;
    let expr_ty = cx.normalize(expr.ty);

    let mut new_arms = Vec::<hir::MatchArm>::new();
    let mut result_ty: Option<Ty> = expected_ty;
    let mut last_case_span: Option<Span> = None;

    for arm in arms {
        let new_arm = check_match_arm(cx, env, arm, expr_ty, expr.span, expected_ty)?;

        if let Some(result_ty) = result_ty {
            cx.at(if let Some(last_case_span) = last_case_span {
                Obligation::exprs(span, last_case_span, new_arm.expr.span)
            } else {
                Obligation::obvious(new_arm.expr.span)
            })
            .eq(result_ty, new_arm.expr.ty)
            .or_coerce(cx, new_arm.expr.id)?;
        } else {
            result_ty = Some(new_arm.expr.ty);
        }

        last_case_span = Some(new_arm.expr.span);

        new_arms.push(new_arm);
    }

    Ok(cx.expr(
        hir::ExprKind::Match(hir::Match { expr: Box::new(expr), arms: new_arms }),
        result_ty.unwrap_or(cx.db.types.unit),
        span,
    ))
}

fn check_match_arm(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    case: &ast::MatchArm,
    expr_ty: Ty,
    expr_span: Span,
    expected_ty: Option<Ty>,
) -> DiagnosticResult<hir::MatchArm> {
    env.with_anon_scope(ScopeKind::Block, |env| {
        let mut names = UstrMap::default();

        let pat = check_match_pat(cx, env, &case.pat, expr_ty, expr_span, &mut names)?;
        check_match_pat_name_bound_once(cx, &pat)?;

        let guard = if let Some(guard) = &case.guard {
            let bool_ty = cx.db.types.bool;
            let guard = exprs::check_expr(cx, env, guard, Some(bool_ty))?;
            cx.at(Obligation::obvious(guard.span)).eq(bool_ty, guard.ty).or_coerce(cx, guard.id)?;
            Some(Box::new(guard))
        } else {
            None
        };

        let expr = exprs::check_expr(cx, env, &case.expr, expected_ty)?;

        Ok(hir::MatchArm { pat, guard, expr: Box::new(expr) })
    })
}

#[allow(clippy::too_many_lines)]
fn check_match_pat(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    pat: &ast::MatchPat,
    pat_ty: Ty,
    parent_span: Span,
    names: &mut UstrMap<DefId>,
) -> DiagnosticResult<hir::MatchPat> {
    let pat_ty_deref = cx.normalize(pat_ty).auto_deref();

    match pat {
        ast::MatchPat::Name(word, mutability) => {
            check_match_pat_name(cx, env, *word, *mutability, pat_ty, parent_span, names)
        }
        ast::MatchPat::Wildcard(span) => Ok(hir::MatchPat::Wildcard(*span)),
        ast::MatchPat::Unit(span) => {
            cx.at(Obligation::exprs(*span, *span, parent_span))
                .eq(cx.db.types.unit, pat_ty_deref)?;
            Ok(hir::MatchPat::Unit(*span))
        }
        ast::MatchPat::Bool(value, span) => {
            cx.at(Obligation::exprs(*span, *span, parent_span))
                .eq(cx.db.types.bool, pat_ty_deref)?;
            Ok(hir::MatchPat::Bool(*value, *span))
        }
        ast::MatchPat::Int(value, span) => {
            if *value < 0 {
                match pat_ty_deref.kind() {
                    TyKind::Int(_) | TyKind::Infer(InferTy::Int(_)) => (),
                    _ => {
                        return Err(errors::ty_mismatch(
                            "int",
                            &pat_ty_deref.to_string(cx.db),
                            *span,
                        ))
                    }
                }
            } else {
                cx.at(Obligation::exprs(*span, *span, parent_span))
                    .eq(cx.fresh_int_var(), pat_ty_deref)?;
            }

            Ok(hir::MatchPat::Int(*value, *span))
        }
        ast::MatchPat::Str(value, span) => {
            cx.at(Obligation::exprs(*span, *span, parent_span))
                .eq(cx.db.types.str, pat_ty_deref)?;
            Ok(hir::MatchPat::Str(*value, *span))
        }
        ast::MatchPat::Adt(pat) => check_match_pat_adt(cx, env, pat, pat_ty, parent_span, names),
        ast::MatchPat::Or(pats, span) => check_match_pat_or(cx, env, pats, *span, pat_ty, names),
    }
}

fn check_match_pat_name(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    word: Word,
    mutability: Mutability,
    pat_ty: Ty,
    parent_span: Span,
    names: &mut UstrMap<DefId>,
) -> DiagnosticResult<hir::MatchPat> {
    if let Some(res) = maybe_check_match_pat_inferred_variant(
        cx,
        env,
        &ast::MatchPatAdt {
            path: vec![word],
            subpats: vec![],
            is_exhaustive: true,
            span: word.span(),
        },
        pat_ty,
        parent_span,
        names,
    ) {
        return res;
    }

    let id = if let Some(id) = names.get(&word.name()) {
        // We make sure that names in all alternatives are bound to the same type
        let expected_ty = cx.def_ty(*id);

        if let Err(err) = cx
            .at(Obligation::exprs(word.span(), cx.db[*id].span, word.span()))
            .eq(expected_ty, pat_ty)
        {
            return Err(Diagnostic::error(format!(
                "in the same arm, the identifier `{word}` must have the same type in all \
                 alternatives",
            ))
            .with_label(Label::primary(
                word.span(),
                format!("has type `{}` here", err.found.display(cx.db)),
            ))
            .with_label(Label::secondary(
                cx.db[*id].span,
                format!("previously introduce with type `{}`", err.expected.display(cx.db)),
            )));
        }

        *id
    } else {
        let id = cx.define().new_local(env, DefKind::Variable, word, mutability, pat_ty);
        names.insert(word.name(), id);
        id
    };

    Ok(hir::MatchPat::Name(id, cx.def_ty(id), word.span()))
}

fn maybe_check_match_pat_inferred_variant(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    pat: &ast::MatchPatAdt,
    pat_ty: Ty,
    parent_span: Span,
    names: &mut UstrMap<DefId>,
) -> Option<DiagnosticResult<hir::MatchPat>> {
    let pat_ty_deref = cx.normalize(pat_ty).auto_deref();

    if let Some(union_def) = pat_ty_deref.as_union(cx.db) {
        let word = pat.path[0];
        if let Some(variant) = cx.lookup().maybe_variant_in_union(union_def, word) {
            return Some(check_match_pat_variant(
                cx,
                env,
                pat,
                pat_ty,
                parent_span,
                names,
                variant.id,
            ));
        }
    }

    None
}

fn check_match_pat_adt(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    pat: &ast::MatchPatAdt,
    pat_ty: Ty,
    parent_span: Span,
    names: &mut UstrMap<DefId>,
) -> DiagnosticResult<hir::MatchPat> {
    if pat.path.len() == 1 {
        if let Some(res) =
            maybe_check_match_pat_inferred_variant(cx, env, pat, pat_ty, parent_span, names)
        {
            return res;
        }
    }

    match cx.lookup().path(env.module_id(), &pat.path)? {
        PathLookup::Def(id) => {
            let def = &cx.db[id];

            match def.kind.as_ref() {
                &DefKind::Adt(adt_id) => {
                    check_match_pat_struct(cx, env, pat, pat_ty, parent_span, names, adt_id)
                }
                _ => Err(errors::expected_named_ty(cx.def_ty(id).display(cx.db), pat.span)),
            }
        }
        PathLookup::Variant(variant_id) => {
            check_match_pat_variant(cx, env, pat, pat_ty, parent_span, names, variant_id)
        }
    }
}

fn check_match_pat_struct(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    pat: &ast::MatchPatAdt,
    pat_ty: Ty,
    parent_span: Span,
    names: &mut UstrMap<DefId>,
    adt_id: AdtId,
) -> DiagnosticResult<hir::MatchPat> {
    let instantiation = check_match_pat_adt_ty(cx, env, pat.span, pat_ty, parent_span, adt_id)?;

    let fields = match &cx.db[adt_id].kind {
        AdtKind::Struct(s) => s.fields.clone(),
        AdtKind::Union(_) => {
            return Err(Diagnostic::error(format!(
                "expected a struct, but `{}` is a union type",
                pat_ty.display(cx.db)
            ))
            .with_label(Label::primary(pat.span, "expected a struct type")))
        }
    };

    let new_subpats =
        check_match_pat_subpats(cx, env, pat, pat_ty, names, adt_id, &fields, &instantiation)?;

    Ok(hir::MatchPat::Adt(adt_id, new_subpats, pat.span))
}

fn check_match_pat_variant(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    pat: &ast::MatchPatAdt,
    pat_ty: Ty,
    parent_span: Span,
    names: &mut UstrMap<DefId>,
    variant_id: VariantId,
) -> DiagnosticResult<hir::MatchPat> {
    let adt_id = cx.db[variant_id].adt_id;
    let instantiation = check_match_pat_adt_ty(cx, env, pat.span, pat_ty, parent_span, adt_id)?;

    let new_subpats = check_match_pat_subpats(
        cx,
        env,
        pat,
        pat_ty,
        names,
        adt_id,
        &cx.db[variant_id].fields.clone(),
        &instantiation,
    )?;

    Ok(hir::MatchPat::Variant(variant_id, new_subpats, pat.span))
}

fn check_match_pat_adt_ty(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    span: Span,
    pat_ty: Ty,
    parent_span: Span,
    adt_id: AdtId,
) -> DiagnosticResult<Instantiation> {
    let adt_ty = cx.db[adt_id].ty();
    let instantiation = types::fresh_instantiation(cx, env, adt_ty.collect_params());

    cx.at(Obligation::exprs(span, parent_span, span))
        .eq(pat_ty.auto_deref(), instantiation.fold(adt_ty))?;

    Ok(instantiation)
}

#[allow(clippy::too_many_lines)]
#[allow(clippy::too_many_arguments)]
fn check_match_pat_subpats(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    pat: &ast::MatchPatAdt,
    pat_ty: Ty,
    names: &mut UstrMap<DefId>,
    adt_id: AdtId,
    fields: &[AdtField],
    instantiation: &Instantiation,
) -> DiagnosticResult<Vec<hir::MatchPat>> {
    let adt_name = cx.db[adt_id].name.name();

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
            ast::MatchSubpat::Positional(subpat) => {
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
            ast::MatchSubpat::Named(name, subpat) => {
                if let Some((field_idx, field)) =
                    fields.iter().enumerate().find(|(_, f)| f.name.name() == name.name())
                {
                    (field_idx, field, subpat, name.span())
                } else {
                    return Err(field_not_found(cx.db, cx.db[adt_id].ty(), pat.span, *name));
                }
            }
        };

        use_field(field.name.name(), field_use_span)?;

        exprs::check_field_access(cx, env, &cx.db[adt_id], field, field_use_span)?;

        let field_ty = instantiation.fold(field.ty);
        let field_ty = match pat_ty.kind() {
            TyKind::Ref(_, mutability) => {
                // If the parent pattern's type is a reference, the field's type is
                // implicitly a reference too.
                field_ty.create_ref(*mutability)
            }
            _ => field_ty,
        };

        let new_subpat = check_match_pat(cx, env, subpat, field_ty, field.span(), names)?;

        new_subpats[field_idx] = new_subpat;
    }

    if pat.is_exhaustive {
        check_match_pat_adt_missing_fields(adt_name, fields, &used_fields, pat.span)?;
    }

    Ok(new_subpats)
}

fn check_match_pat_adt_missing_fields(
    adt_name: Ustr,
    fields: &[AdtField],
    used_fields: &WordMap,
    span: Span,
) -> DiagnosticResult<()> {
    let missing_fields: Vec<_> = fields.iter().filter(|f| !used_fields.contains(f.name)).collect();

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

fn check_match_pat_or(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    pats: &[ast::MatchPat],
    span: Span,
    pat_ty: Ty,
    names: &mut UstrMap<DefId>,
) -> DiagnosticResult<hir::MatchPat> {
    let pats: Vec<_> =
        pats.iter().map(|pat| check_match_pat(cx, env, pat, pat_ty, span, names)).try_collect()?;

    let all_bound: Vec<_> = pats
        .iter()
        .map(|pat| {
            let mut bound = UstrMap::default();
            collect_bound_names_in_pat(cx, pat, &mut bound);
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
            .with_label(Label::primary(span, "in these patterns")));
        }
    }

    Ok(hir::MatchPat::Or(pats, span))
}

fn collect_bound_names_in_pat(
    cx: &Typeck<'_>,
    pat: &hir::MatchPat,
    bound: &mut UstrMap<Vec<Span>>,
) {
    match pat {
        hir::MatchPat::Name(id, _, span) => {
            bound.entry(cx.db[*id].name).or_default().push(*span);
        }
        hir::MatchPat::Adt(_, subpats, _) => {
            for pat in subpats {
                collect_bound_names_in_pat(cx, pat, bound);
            }
        }
        hir::MatchPat::Or(pats, _) => {
            for pat in pats {
                collect_bound_names_in_pat(cx, pat, bound);
            }
        }
        _ => (),
    }
}

fn check_match_pat_name_bound_once(cx: &Typeck<'_>, pat: &hir::MatchPat) -> DiagnosticResult<()> {
    let mut bound = UstrMap::default();
    collect_bound_names_in_pat(cx, pat, &mut bound);

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
