use data_structures::index_vec::{IndexVecExt as _, Key as _};
use ustr::ustr;

use crate::{
    ast,
    ast::{Ast, ItemId},
    db::{
        AdtId, AdtKind, Def, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, StructDef, UnionDef,
        Variant, VariantId,
    },
    diagnostics::DiagnosticResult,
    middle::{Mutability, NamePat, Pat},
    qpath::QPath,
    span::Spanned as _,
    typeck2::{
        attrs, errors,
        ns::{AssocTy, Env},
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
        match item {
            ast::Item::Let(let_) => define_let(cx, res_map, module.id, id, let_)?,
            ast::Item::ExternLet(let_) => define_extern_let(cx, res_map, module.id, id, let_)?,
            ast::Item::Fn(fun) => define_fn(cx, res_map, module.id, id, fun, None)?,
            ast::Item::Type(tydef) => define_tydef(cx, res_map, module.id, id, tydef)?,
            _ => (),
        }
    }

    Ok(())
}

fn define_let(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ItemId,
    let_: &ast::Let,
) -> DiagnosticResult<()> {
    attrs::validate(&let_.attrs, attrs::Placement::Let)?;

    let pat = match &let_.pat {
        Pat::Name(name) => {
            let id = cx.define().new_global(
                module_id,
                name.vis,
                DefKind::Global,
                name.word,
                name.mutability,
            )?;
            Pat::Name(NamePat { id, ty: cx.db.types.unknown, ..name.clone() })
        }
        Pat::Discard(span) => Pat::Discard(*span),
    };

    res_map.item_to_pat.insert(ast::GlobalItemId::new(module_id, item_id), pat);

    Ok(())
}

fn define_extern_let(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ItemId,
    let_: &ast::ExternLet,
) -> DiagnosticResult<()> {
    attrs::validate(&let_.attrs, attrs::Placement::ExternLet)?;

    let id = cx.define().new_global(
        module_id,
        let_.vis,
        DefKind::ExternGlobal,
        let_.word,
        let_.mutability,
    )?;

    res_map.item_to_def.insert(ast::GlobalItemId::new(module_id, item_id), id);

    Ok(())
}

fn define_fn(
    cx: &mut Typeck<'_>,
    res_map: &mut ResolutionMap,
    module_id: ModuleId,
    item_id: ItemId,
    fun: &ast::Fn,
    assoc_ty: Option<AssocTy>,
) -> DiagnosticResult<()> {
    attrs::validate(
        &fun.attrs,
        match &fun.kind {
            ast::FnKind::Bare { .. } => attrs::Placement::Fn,
            ast::FnKind::Extern { .. } => attrs::Placement::ExternFn,
        },
    )?;

    let name = fun.sig.word.name();

    let id = match &fun.kind {
        ast::FnKind::Bare { .. } => {
            let base_qpath = if let Some(assoc_ty) = assoc_ty {
                match assoc_ty {
                    AssocTy::Adt(adt_id) => cx.db[cx.db[adt_id].def_id].qpath.clone(),
                    AssocTy::BuiltinTy(ty) => QPath::from(ustr(&ty.display(cx.db).to_string())),
                }
            } else {
                cx.db[module_id].qpath.clone()
            };

            let qpath = base_qpath.child(name);

            let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis: fun.vis };

            if let Some(def) = cx.global_env.module(module_id).ns.contains_def(name) {
                return Err(errors::multiple_item_def_err(def.span, fun.sig.word));
            }

            let id = Def::alloc(
                cx.db,
                qpath,
                scope,
                DefKind::Fn(FnInfo::Bare),
                Mutability::Imm,
                fun.sig.word.span(),
            );

            cx.global_env.module_mut(module_id).ns.defined_fns.entry(name).or_default().push(id);

            id
        }
        ast::FnKind::Extern { .. } => cx.define().new_global(
            module_id,
            fun.vis,
            DefKind::Fn(FnInfo::Extern),
            fun.sig.word,
            Mutability::Imm,
        )?,
    };

    res_map.item_to_def.insert(ast::GlobalItemId::new(module_id, item_id), id);

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
