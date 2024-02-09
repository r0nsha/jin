use crate::{
    ast,
    ast::Ast,
    db::ModuleId,
    diagnostics::DiagnosticResult,
    typeck2::{attrs, Typeck},
};

pub(super) fn define_extern_imports(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (_, item) in ast.items() {
        if let ast::Item::ExternImport(import) = item {
            attrs::validate(&import.attrs, attrs::Placement::ExternImport)?;
            cx.db.extern_libs.insert(import.lib.clone());
        }
    }

    Ok(())
}

pub(super) fn define_imports(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item, id) in ast.items_with_id() {
        if let ast::Item::Import(import) = item {
            define_import(cx, module.id, id, import)?;
        }
    }

    Ok(())
}

fn define_import(
    cx: &mut Typeck,
    module_id: ModuleId,
    item_id: ast::ItemId,
    import: &ast::Import,
) -> DiagnosticResult<()> {
    attrs::validate(&import.attrs, attrs::Placement::Import)?;
    todo!()
}
