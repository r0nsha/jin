use crate::{
    ast,
    ast::Ast,
    db::{DefKind, ModuleId},
    diagnostics::DiagnosticResult,
    middle::Mutability,
    ty::{Ty, TyKind},
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

pub(super) fn define_qualified_imports(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item) in ast.items() {
        if let ast::Item::Import(import) = item {
            if let ast::ImportKind::Qualified(alias, vis) = &import.kind {
                attrs::validate(&import.attrs, attrs::Placement::Import)?;
                let name = alias.unwrap_or(*import.path.last().unwrap());
                let id = cx.define().new_global(
                    module.id,
                    *vis,
                    DefKind::Variable,
                    name,
                    Mutability::Imm,
                )?;
                cx.def_to_ty.insert(id, Ty::new(TyKind::Module(module.id)));
            }
        }
    }

    Ok(())
}

pub(super) fn define_unqualified_imports(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item) in ast.items() {
        if let ast::Item::Import(import) = item {
            if let ast::ImportKind::Unqualified(imports) = &import.kind {
                attrs::validate(&import.attrs, attrs::Placement::Import)?;
                dbg!(&imports);
                todo!()
            }
        }
    }

    Ok(())
}

fn define_unqualified_import(
    cx: &mut Typeck,
    module_id: ModuleId,
    item_id: ast::ItemId,
    import: &ast::Import,
) -> DiagnosticResult<()> {
    attrs::validate(&import.attrs, attrs::Placement::Import)?;
    todo!()
}
