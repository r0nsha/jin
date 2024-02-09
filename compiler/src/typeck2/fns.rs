use ustr::ustr;

use crate::{
    ast,
    ast::{Ast, ItemId},
    db::{Def, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel},
    diagnostics::DiagnosticResult,
    middle::Mutability,
    qpath::QPath,
    span::Spanned as _,
    typeck2::{attrs, errors, AssocTy, ResolutionMap, Typeck},
};

pub(super) fn define(
    cx: &mut Typeck,
    res_map: &mut ResolutionMap,
    ast: &Ast,
) -> DiagnosticResult<()> {
    for (module, item, id) in ast.items_with_id() {
        if let ast::Item::Fn(fun) = item {
            define_fn(cx, res_map, module.id, id, fun, None)?
        }
    }

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
            cx.global_env.module_mut(module_id).ns.fn_names.insert(name);

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

            Def::alloc(
                cx.db,
                qpath,
                scope,
                DefKind::Fn(FnInfo::Bare),
                Mutability::Imm,
                fun.sig.word.span(),
            )
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
