use crate::{
    ast,
    ast::Ast,
    db::{DefKind, ModuleId},
    diagnostics::DiagnosticResult,
    middle::{IsUfcs, Mutability},
    span::Spanned as _,
    ty::{Ty, TyKind},
    typeck2::{attrs, lookup::Query, Typeck},
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
                let in_module = module.id;
                let target_module_id = import_prologue(cx, in_module, import)?;
                let name = alias.unwrap_or(*import.path.last().unwrap());
                let id = cx.define().new_global(
                    in_module,
                    *vis,
                    DefKind::Variable,
                    name,
                    Mutability::Imm,
                )?;
                cx.def_to_ty.insert(id, Ty::new(TyKind::Module(target_module_id)));
            }
        }
    }

    Ok(())
}

pub(super) fn define_unqualified_imports(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item) in ast.items() {
        if let ast::Item::Import(import) = item {
            if let ast::ImportKind::Unqualified(imports) = &import.kind {
                let in_module = module.id;
                let target_module_id = import_prologue(cx, in_module, import)?;

                for uim in imports {
                    match uim {
                        ast::UnqualifiedImport::Name(name, alias, vis) => {
                            let results = cx.lookup().import(in_module, target_module_id, *name)?;
                            cx.insert_import_lookup_results(
                                in_module,
                                alias.unwrap_or(*name),
                                *vis,
                                results,
                            )?;
                        }
                        ast::UnqualifiedImport::Glob(is_ufcs, _) => {
                            insert_glob_target(cx, in_module, target_module_id, *is_ufcs);
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

fn import_prologue(
    cx: &mut Typeck,
    in_module: ModuleId,
    import: &ast::Import,
) -> DiagnosticResult<ModuleId> {
    attrs::validate(&import.attrs, attrs::Placement::Import)?;
    let target_module_id = resolve_import_path(cx, in_module, import)?;

    // Insert imported modules as UFCS targets implicitly.
    // This is done because always adding `?` whenever we import any type is
    // really annoying... I couldn't find any issues with this yet, but if
    // some do pop up, we'll need to find a more clever solution for this
    // redundancy.
    insert_glob_target(cx, in_module, target_module_id, IsUfcs::Yes);

    Ok(target_module_id)
}

fn resolve_import_path(
    cx: &Typeck,
    from_module: ModuleId,
    import: &ast::Import,
) -> DiagnosticResult<ModuleId> {
    let module_info = cx.db.find_module_by_path(&import.module_path).unwrap();

    let mut target_module_id = module_info.id;

    // We skip the first part since it is the import root module name
    for &part in import.path.iter().skip(1) {
        let id = cx.lookup().query(from_module, target_module_id, &Query::Name(part))?;
        target_module_id = cx.is_module_def(id, part.span())?;
    }

    Ok(target_module_id)
}

fn insert_glob_target(
    cx: &mut Typeck,
    in_module: ModuleId,
    target_module_id: ModuleId,
    is_ufcs: IsUfcs,
) {
    cx.global_env.module_mut(in_module).globs.insert(target_module_id, is_ufcs);
}
