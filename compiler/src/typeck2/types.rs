use crate::{
    ast,
    ast::{Ast, ItemId},
    db::{AdtKind, ModuleId, StructDef, UnionDef},
    diagnostics::DiagnosticResult,
    typeck2::{attrs, Typeck},
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
            cx.define().adt(module_id, tydef, |id| {
                AdtKind::Union(UnionDef::new(id, union_def.kind, vec![]))
            })?
        }
    };

    cx.item_map.item_to_adt.insert(ast::GlobalItemId::new(module_id, item_id), adt_id);

    Ok(())
}
