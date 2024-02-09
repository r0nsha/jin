use crate::{
    ast,
    ast::{Ast, ItemId},
    db::{DefKind, ModuleId},
    diagnostics::DiagnosticResult,
    middle::{NamePat, Pat},
    typeck2::{attrs, ResolutionMap, Typeck},
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
