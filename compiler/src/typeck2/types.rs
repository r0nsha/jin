use crate::{
    ast,
    ast::Ast,
    db::{AdtField, AdtId, DefId, DefKind, ModuleId, VariantId},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{Mutability, TyParam, Vis},
    span::{Span, Spanned as _},
    ty::{Instantiation, ParamTy, Ty, TyKind},
    typeck2::{
        errors,
        ns::{AssocTy, Env, ScopeKind},
        tyexpr,
        tyexpr::AllowTyHole,
        ResolutionMap, Typeck,
    },
    word::WordMap,
};

pub(super) fn check(
    cx: &mut Typeck,
    res_map: &mut ResolutionMap,
    ast: &Ast,
) -> DiagnosticResult<()> {
    for (module, item, id) in ast.items_with_id() {
        if let ast::Item::Type(tydef) = item {
            check_tydef(cx, res_map, module.id, id, tydef)?;
        }
    }

    Ok(())
}

fn check_tydef(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    tydef: &ast::TyDef,
) -> DiagnosticResult<()> {
    let mut env = Env::new(module_id);
    let adt_id = res_map.item_to_adt.remove(&item_id).expect("to be defined");

    env.with_anon_scope(ScopeKind::TyDef, |env| -> DiagnosticResult<()> {
        for tp in &cx.db[adt_id].ty_params {
            env.insert(tp.word.name(), tp.id);
        }

        match &tydef.kind {
            ast::TyDefKind::Struct(struct_def) => check_struct(cx, env, adt_id, struct_def),
            ast::TyDefKind::Union(union_def) => check_union(cx, env, adt_id, union_def),
        }
    })
}

fn check_struct(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    adt_id: AdtId,
    struct_def: &ast::StructTyDef,
) -> DiagnosticResult<()> {
    let mut fields = vec![];
    let mut defined_fields = WordMap::default();

    for field in &struct_def.fields {
        if let Some(prev_span) = defined_fields.insert(field.name) {
            return Err(errors::name_defined_twice("field", field.name, prev_span));
        }

        let ty = tyexpr::check(cx, env, &field.ty_expr, AllowTyHole::No)?;
        fields.push(AdtField { name: field.name, vis: field.vis, ty });
    }

    let adt_ty = cx.db[adt_id].ty();
    let struct_def = cx.db[adt_id].as_struct_mut().unwrap();
    struct_def.fields = fields;
    struct_def.fill_ctor_ty(adt_ty);
    struct_def.fill_ctor_vis();

    let adt = &cx.db[adt_id];

    if let Some(field) = adt.is_infinitely_sized(cx.db) {
        return Err(errors::infinitely_sized_adt(adt, field));
    }

    if adt.is_value_struct() {
        let move_field =
            cx.db[adt_id].as_struct().unwrap().fields.iter().find(|f| f.ty.is_move(cx.db));

        if let Some(field) = move_field {
            return Err(Diagnostic::error(format!(
                "value type `{}` cannot contain fields with move semantics",
                adt.name
            ))
            .with_label(Label::primary(
                field.span(),
                format!("has type `{}`", field.ty.display(cx.db)),
            ))
            .with_label(Label::secondary(adt.name.span(), "type defined here")));
        }
    }

    Ok(())
}

fn check_union(
    cx: &mut Typeck<'_>,
    env: &mut Env,
    adt_id: AdtId,
    union_def: &ast::UnionTyDef,
) -> DiagnosticResult<()> {
    let adt_ty = cx.db[adt_id].ty();

    for (idx, variant) in union_def.variants.iter().enumerate() {
        let id = cx.db[adt_id].as_union().unwrap().variants[idx];
        check_variant(cx, env, adt_ty, variant, id)?;
    }

    let adt = &cx.db[adt_id];
    if let Some(field) = adt.is_infinitely_sized(cx.db) {
        return Err(errors::infinitely_sized_adt(adt, field));
    }

    Ok(())
}

fn check_variant(
    cx: &mut Typeck<'_>,
    env: &Env,
    adt_ty: Ty,
    variant: &ast::UnionVariant,
    variant_id: VariantId,
) -> DiagnosticResult<()> {
    let mut fields = vec![];
    let mut defined_fields = WordMap::default();

    for field in &variant.fields {
        if let Some(prev_span) = defined_fields.insert(field.name) {
            return Err(errors::name_defined_twice("field", field.name, prev_span));
        }

        let ty = tyexpr::check(cx, env, &field.ty_expr, AllowTyHole::No)?;
        fields.push(AdtField { name: field.name, vis: Vis::Public, ty });
    }

    let variant = &mut cx.db[variant_id];
    variant.fields = fields;
    variant.fill_ctor_ty(adt_ty);

    Ok(())
}

pub(super) fn define_ty_params(
    cx: &mut Typeck,
    env: &mut Env,
    ty_params: &[ast::TyParam],
) -> DiagnosticResult<Vec<TyParam>> {
    let mut new_ty_params = vec![];
    let mut defined_ty_params = WordMap::default();

    for tp in ty_params {
        let ty = Ty::new(TyKind::Param(ParamTy { name: tp.word.name(), var: cx.fresh_var() }));

        let id = cx.define().new_local(
            env,
            DefKind::Ty(ty),
            tp.word,
            Mutability::Imm,
            TyKind::Type(ty).into(),
        );

        if let Some(prev_span) = defined_ty_params.insert(tp.word) {
            return Err(errors::name_defined_twice("type parameter", tp.word, prev_span));
        }

        new_ty_params.push(TyParam { id, word: tp.word, ty });
    }

    Ok(new_ty_params)
}

pub(super) fn fresh_instantiation(
    cx: &Typeck<'_>,
    env: &Env,
    ty_params: Vec<ParamTy>,
) -> Instantiation {
    let env_fn_ty_params = env.fn_id().map_or(vec![], |id| cx.def_ty(id).collect_params());

    ty_params
        .into_iter()
        .map(|param| {
            (
                param.var,
                // If the type param is one of the current function's type
                // params, we don't want to instantiate it
                if env_fn_ty_params.iter().any(|p| p.var == param.var) {
                    Ty::new(TyKind::Param(param))
                } else {
                    cx.fresh_ty_var()
                },
            )
        })
        .collect()
}

pub(super) fn try_extract_assoc_ty(cx: &Typeck<'_>, id: DefId) -> Option<AssocTy> {
    let def = &cx.db[id];

    match def.kind.as_ref() {
        DefKind::Adt(adt_id) => Some(AssocTy::Adt(*adt_id)),
        DefKind::Ty(ty) => Some(AssocTy::BuiltinTy(*ty)),
        _ => None,
    }
}

pub(super) fn apply_targs_to_ty(
    cx: &Typeck<'_>,
    env: &Env,
    ty: Ty,
    targs: Option<&[Ty]>,
    span: Span,
) -> DiagnosticResult<(Ty, Instantiation)> {
    let mut ty_params = ty.collect_params();

    // NOTE: map type params that are part of the current polymorphic function to
    // themselves, so that we don't instantiate them. that's quite ugly though.
    if let Some(fn_id) = env.fn_id() {
        let fn_ty_params = cx.def_ty(fn_id).collect_params();
        for ftp in fn_ty_params {
            if let Some(tp) = ty_params.iter_mut().find(|p| p.var == ftp.var) {
                *tp = ftp.clone();
            }
        }
    }

    let instantiation: Instantiation = match &targs {
        Some(args) if args.len() == ty_params.len() => {
            ty_params.into_iter().zip(args.iter()).map(|(param, arg)| (param.var, *arg)).collect()
        }
        Some(args) => {
            return Err(errors::ty_arg_mismatch(ty_params.len(), args.len(), span));
        }
        _ => fresh_instantiation(cx, env, ty_params),
    };

    Ok((instantiation.fold(ty), instantiation))
}
