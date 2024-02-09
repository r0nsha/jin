use data_structures::index_vec::{IndexVecExt as _, Key as _};

use crate::{
    ast,
    ast::{Ast, ItemId},
    db::{
        AdtField, AdtId, AdtKind, DefId, DefKind, ModuleId, StructDef, UnionDef, Variant, VariantId,
    },
    diagnostics::{Diagnostic, DiagnosticResult, Label},
    middle::{Mutability, TyParam},
    span::Spanned as _,
    ty::{ParamTy, Ty, TyKind},
    typeck2::{
        attrs, errors,
        ns::{Env, ScopeKind},
        tyexpr,
        tyexpr::AllowTyHole,
        ResolutionMap, Typeck,
    },
    word::WordMap,
};

pub(super) fn define(
    cx: &mut Typeck,
    res_map: &mut ResolutionMap,
    ast: &Ast,
) -> DiagnosticResult<()> {
    for (module, item, id) in ast.items_with_id() {
        if let ast::Item::Type(tydef) = item {
            define_tydef(cx, res_map, module.id, id, tydef)?;
        }
    }

    Ok(())
}

fn define_tydef(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ItemId,
    tydef: &ast::TyDef,
) -> DiagnosticResult<()> {
    let (adt_id, _) = match &tydef.kind {
        ast::TyDefKind::Struct(struct_def) => {
            attrs::validate(&tydef.attrs, attrs::Placement::Struct)?;
            let unknown = cx.db.types.unknown;
            cx.define().adt(module_id, tydef, |id| {
                AdtKind::Struct(StructDef::new(
                    id,
                    vec![],
                    struct_def.kind,
                    unknown, // Will be filled later
                ))
            })?
        }
        ast::TyDefKind::Union(union_def) => {
            attrs::validate(&tydef.attrs, attrs::Placement::Union)?;
            let variants = define_variants(cx, union_def)?;
            cx.define().adt(module_id, tydef, |id| {
                AdtKind::Union(UnionDef::new(id, union_def.kind, variants))
            })?
        }
    };

    res_map.item_to_adt.insert(ast::GlobalItemId::new(module_id, item_id), adt_id);

    Ok(())
}

fn define_variants(
    cx: &mut Typeck<'_>,
    union_def: &ast::UnionTyDef,
) -> DiagnosticResult<Vec<VariantId>> {
    let mut variants = vec![];
    let mut defined_variants = WordMap::default();

    for (index, variant) in union_def.variants.iter().enumerate() {
        if let Some(prev_span) = defined_variants.insert(variant.name) {
            return Err(errors::name_defined_twice("variant", variant.name, prev_span));
        }

        let unknown = cx.db.types.unknown;
        let id = cx.db.variants.push_with_key(|id| Variant {
            id,
            adt_id: AdtId::null(),
            index,
            name: variant.name,
            fields: vec![],
            ctor_ty: unknown,
        });

        variants.push(id);
    }

    Ok(variants)
}

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
    item_id: ItemId,
    tydef: &ast::TyDef,
) -> DiagnosticResult<()> {
    match &tydef.kind {
        ast::TyDefKind::Struct(struct_def) => {
            check_struct(cx, res_map, module_id, item_id, tydef, struct_def)
        }
        ast::TyDefKind::Union(union_def) => {
            check_union(cx, res_map, module_id, item_id, tydef, union_def)
        }
    }
}

fn check_struct(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ItemId,
    tydef: &ast::TyDef,
    struct_def: &ast::StructTyDef,
) -> DiagnosticResult<()> {
    let mut env = Env::new(module_id);
    let adt_id = res_map
        .item_to_adt
        .remove(&ast::GlobalItemId::new(module_id, item_id))
        .expect("to be defined");

    let mut fields = vec![];

    env.with_anon_scope(ScopeKind::TyDef, |env| -> DiagnosticResult<()> {
        check_adt_ty_params(cx, env, tydef, adt_id)?;

        let mut defined_fields = WordMap::default();

        for field in &struct_def.fields {
            if let Some(prev_span) = defined_fields.insert(field.name) {
                return Err(errors::name_defined_twice("field", field.name, prev_span));
            }

            let ty = tyexpr::check(cx, module_id, &field.ty_expr, AllowTyHole::No)?;
            fields.push(AdtField { name: field.name, vis: field.vis, ty });
        }

        Ok(())
    })?;

    let adt_ty = cx.db[adt_id].ty();
    let struct_def = cx.db[adt_id].as_struct_mut().unwrap();
    struct_def.fields = fields;
    struct_def.fill_ctor_ty(adt_ty);

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
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ItemId,
    tydef: &ast::TyDef,
    union_def: &ast::UnionTyDef,
) -> DiagnosticResult<()> {
    // TODO: create Env
    // TODO: typarams
    // TODO: typarams: dup
    // TODO: variant: field: ty
    // TODO: variant: field: dup
    // TODO: variant: field: add
    // TODO: variant: set ctor ty
    // TODO: check infinitely sized
    // todo!()
    Ok(())
}

fn check_adt_ty_params(
    cx: &mut Typeck,
    env: &mut Env,
    tydef: &ast::TyDef,
    adt_id: AdtId,
) -> DiagnosticResult<()> {
    let ty_params = check_ty_params(cx, env, &tydef.ty_params)?;
    cx.db[adt_id].ty_params = ty_params;
    let adt = &cx.db[adt_id];
    cx.def_to_ty.insert(adt.def_id, TyKind::Type(adt.ty()).into());
    Ok(())
}

pub(super) fn check_ty_params(
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
