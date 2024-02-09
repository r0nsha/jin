use data_structures::index_vec::{IndexVecExt as _, Key as _};

use crate::{
    ast,
    ast::{Ast, ItemId},
    db::{AdtId, AdtKind, ModuleId, StructDef, UnionDef, Variant, VariantId},
    diagnostics::DiagnosticResult,
    typeck2::{attrs, errors, Typeck},
    word::WordMap,
};

pub(super) fn define_types(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item, id) in ast.items_with_id() {
        if let ast::Item::Type(tydef) = item {
            define_tydef(cx, module.id, id, tydef)?;
        }
    }

    Ok(())
}

fn define_tydef(
    cx: &mut Typeck<'_>,
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

    cx.item_map.item_to_adt.insert(ast::GlobalItemId::new(module_id, item_id), adt_id);

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
