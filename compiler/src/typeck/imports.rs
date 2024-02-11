use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    ast,
    ast::Ast,
    db::{DefId, DefKind, ModuleId},
    diagnostics::DiagnosticResult,
    middle::{IsUfcs, Mutability},
    span::Spanned as _,
    ty::{Ty, TyKind},
    typeck::{
        attrs,
        lookup::{ImportLookupResult, Query},
        Typeck,
    },
};

pub(super) fn define_qualified(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
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

pub(super) fn define_unqualified(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<ImportedFns> {
    let mut imported_fns = ImportedFns::default();

    for (module, item) in ast.items() {
        if let ast::Item::Import(import) = item {
            if let ast::ImportKind::Unqualified(imports) = &import.kind {
                let imported_fns_entry = imported_fns.entry(module.id).or_default();
                let in_module = module.id;
                let target_module_id = import_prologue(cx, in_module, import)?;

                for uim in imports {
                    match uim {
                        ast::UnqualifiedImport::Name(name, alias, vis) => {
                            let results = cx.lookup().import(in_module, target_module_id, *name)?;
                            let alias = alias.unwrap_or(*name);

                            for res in results {
                                match res {
                                    ImportLookupResult::Def(id) => {
                                        cx.define().global(in_module, alias, id, *vis)?;
                                    }
                                    ImportLookupResult::Fn(id) => {
                                        imported_fns_entry.push(ImportedFn {
                                            id,
                                            name: name.name(),
                                            alias: alias.name(),
                                        });
                                    }
                                }
                            }
                        }
                        ast::UnqualifiedImport::Glob(is_ufcs, _) => {
                            insert_glob_target(cx, in_module, target_module_id, *is_ufcs);
                        }
                    }
                }
            }
        }
    }

    Ok(imported_fns)
}

pub(super) fn fill_imported_fn_candidates(
    cx: &mut Typeck,
    imported_fns: ImportedFns,
) -> DiagnosticResult<()> {
    for (module_id, fns) in imported_fns {
        for f in fns {
            let from_module = cx.db[f.id].scope.module_id;
            let from_set = cx
                .global_env
                .module(from_module)
                .ns
                .fns
                .get(&f.name)
                .expect("to have a defined set");
            let candidate =
                from_set.iter().find(|c| c.id == f.id).expect("candidate to be defined").clone();
            let set = cx.global_env.module_mut(module_id).ns.fns.entry(f.alias).or_default();
            set.try_insert(candidate).map_err(|err| err.into_diagnostic(cx.db))?;
        }
    }

    Ok(())
}

pub(super) type ImportedFns = FxHashMap<ModuleId, Vec<ImportedFn>>;

pub(super) struct ImportedFn {
    pub(super) id: DefId,
    pub(super) name: Ustr,
    pub(super) alias: Ustr,
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
