use compiler_ast::{self as ast, Ast};
use compiler_core::{
    db::{AdtField, AdtId, DefId, DefKind, ModuleId, VariantId},
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{Mutability, TyParam, Vis},
    span::{Span, Spanned as _},
    ty::{Instantiation, ParamTy, Ty, TyKind},
    word::{Word, WordMap},
};

use crate::{
    errors,
    ns::{AssocTy, Env, ScopeKind},
    tyexpr,
    tyexpr::AllowTyHole,
    Typeck,
};

pub(crate) fn check(cx: &mut Typeck, ast: &Ast) {
    for (module, item, id) in ast.items_with_id() {
        if let ast::Item::Type(tydef) = item {
            if let Err(diagnostic) = check_tydef(cx, module.id, id, tydef) {
                cx.db.diagnostics.add(diagnostic);
            }
        }
    }
}

fn check_tydef(
    cx: &mut Typeck<'_>,
    module_id: ModuleId,
    item_id: ast::GlobalItemId,
    tydef: &ast::TyDef,
) -> DiagnosticResult<()> {
    let mut env = Env::new(module_id);

    env.with_anon_scope(ScopeKind::TyDef, |env| -> DiagnosticResult<()> {
        match &tydef.kind {
            ast::TyDefKind::Struct(struct_def) => {
                let adt_id = check_adt_prologue(cx, env, item_id);
                check_struct(cx, env, adt_id, struct_def)
            }
            ast::TyDefKind::Union(union_def) => {
                let adt_id = check_adt_prologue(cx, env, item_id);
                check_union(cx, env, adt_id, union_def)
            }
            ast::TyDefKind::Alias(_) => {
                // Aliases are checked during tyexpr::check
                Ok(())
            }
        }
    })
}

fn check_adt_prologue(cx: &mut Typeck<'_>, env: &mut Env, item_id: ast::GlobalItemId) -> AdtId {
    let adt_id = cx.res_map.item_to_adt.remove(&item_id).expect("to be defined");
    for tp in &cx.db[adt_id].tparams {
        env.insert(tp.word.name(), tp.id);
    }
    adt_id
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

pub(crate) fn define_tparams(
    cx: &mut Typeck,
    env: &mut Env,
    tparams: &[ast::TyParam],
) -> Vec<TyParam> {
    let mut new_tparams = vec![];
    let mut defined_tparams = WordMap::default();

    for tp in tparams {
        let (id, ty) = define_tparam(cx, env, tp.word);

        if let Some(prev_span) = defined_tparams.insert(tp.word) {
            cx.db.diagnostics.add(errors::name_defined_twice("type parameter", tp.word, prev_span));
        }

        new_tparams.push(TyParam { id, word: tp.word, ty });
    }

    new_tparams
}

pub(crate) fn define_tparam(cx: &mut Typeck, env: &mut Env, word: Word) -> (DefId, Ty) {
    let ty = Ty::new(TyKind::Param(ParamTy { name: word.name(), var: cx.fresh_var() }));
    let id = cx.define().new_local(
        env,
        DefKind::BuiltinTy(ty),
        word,
        Mutability::Imm,
        TyKind::Type(ty).into(),
    );
    (id, ty)
}

pub(crate) fn fresh_instantiation(
    cx: &Typeck<'_>,
    env: &Env,
    tparams: Vec<ParamTy>,
) -> Instantiation {
    let env_fn_tparams = env.fn_id().map_or(vec![], |id| cx.def_ty(id).collect_params());

    tparams
        .into_iter()
        .map(|param| {
            (
                param.var,
                // If the type param is one of the current function's type
                // params, we don't want to instantiate it
                if env_fn_tparams.iter().any(|p| p.var == param.var) {
                    Ty::new(TyKind::Param(param))
                } else {
                    cx.fresh_ty_var()
                },
            )
        })
        .collect()
}

pub(crate) fn try_extract_assoc_ty(cx: &Typeck<'_>, id: DefId) -> Option<AssocTy> {
    let def = &cx.db[id];

    match &def.kind {
        DefKind::Adt(adt_id) => Some(AssocTy::Adt(*adt_id)),
        DefKind::BuiltinTy(ty) => Some(AssocTy::BuiltinTy(*ty)),
        _ => None,
    }
}

pub(crate) fn apply_targs_to_ty(
    cx: &Typeck<'_>,
    env: &Env,
    ty: Ty,
    targs: Option<&[Ty]>,
    span: Span,
) -> DiagnosticResult<(Ty, Instantiation)> {
    let mut tparams = ty.collect_params();

    // NOTE: map type params that are part of the current polymorphic function to
    // themselves, so that we don't instantiate them. that's quite ugly though.
    if let Some(fn_id) = env.fn_id() {
        let fn_tparams = cx.def_ty(fn_id).collect_params();
        for ftp in fn_tparams {
            if let Some(tp) = tparams.iter_mut().find(|p| p.var == ftp.var) {
                *tp = ftp.clone();
            }
        }
    }

    let instantiation: Instantiation = match &targs {
        Some(args) if args.len() == tparams.len() => {
            tparams.into_iter().zip(args.iter()).map(|(param, arg)| (param.var, *arg)).collect()
        }
        Some(args) => {
            return Err(errors::targ_mismatch(tparams.len(), args.len(), span));
        }
        _ => fresh_instantiation(cx, env, tparams),
    };

    Ok((instantiation.fold(ty), instantiation))
}
