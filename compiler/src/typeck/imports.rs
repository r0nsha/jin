use rustc_hash::FxHashMap;
use ustr::Ustr;

use crate::{
    ast,
    ast::Ast,
    db::{DefId, DefKind, ModuleId},
    diagnostics::DiagnosticResult,
    middle::{IsUfcs, Mutability, Vis},
    span::{Span, Spanned as _},
    ty::{Ty, TyKind},
    typeck::{
        attrs, errors,
        lookup::{ImportLookupResult, Query},
        ns::NsDef,
        ImportNode, ItemMap, Typeck,
    },
    word::Word,
};

pub(super) fn build_graph(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    build_defined_graph_nodes(cx)?;
    build_import_graph_nodes(cx, ast)?;
    println!("{:?}", petgraph::dot::Dot::new(cx.res_map.import_graph.graph()));
    Ok(())
}

fn build_defined_graph_nodes(cx: &mut Typeck) -> DiagnosticResult<()> {
    for (module_id, env) in &cx.global_env.modules {
        for def in env.ns.defs.values() {
            cx.res_map.import_graph.add_def(def.data);
        }

        for name in env.ns.defined_fns.keys() {
            cx.res_map.import_graph.add_fn(*module_id, *name);
        }
    }

    Ok(())
}

fn build_import_graph_nodes(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item) in ast.items() {
        let ast::Item::Import(import) = item else { continue };

        match &import.kind {
            ast::ImportKind::Qualified { alias, vis } => {
                let name = alias.unwrap_or(*import.path.last().unwrap());
                cx.res_map.import_graph.add_import(NsDef::new(
                    name.name(),
                    module.id,
                    *vis,
                    name.span(),
                ));
            }
            ast::ImportKind::Unqualified { imports } => {
                for uim in imports {
                    match uim {
                        ast::UnqualifiedImport::Name(name, alias, vis) => {
                            let name = alias.unwrap_or(*name);
                            cx.res_map.import_graph.add_import(NsDef::new(
                                name.name(),
                                module.id,
                                *vis,
                                name.span(),
                            ));
                        }
                        ast::UnqualifiedImport::Glob(_, _) => (),
                    }
                }
            }
        }
    }

    Ok(())
}

pub(super) fn define_qualified_names(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item) in ast.items() {
        let ast::Item::Import(import) = item else { continue };
        let ast::ImportKind::Qualified { alias, vis } = &import.kind else { continue };

        if import.path.len() == 1 {
            define_qualified_helper(cx, module.id, import, *alias, *vis)?;
        }
    }

    Ok(())
}

pub(super) fn define_qualified_paths(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (module, item) in ast.items() {
        let ast::Item::Import(import) = item else { continue };
        let ast::ImportKind::Qualified { alias, vis } = &import.kind else { continue };

        if import.path.len() > 1 {
            define_qualified_helper(cx, module.id, import, *alias, *vis)?;
        }
    }

    Ok(())
}

pub(super) fn define_qualified_helper(
    cx: &mut Typeck,
    in_module: ModuleId,
    import: &ast::Import,
    alias: Option<Word>,
    vis: Vis,
) -> DiagnosticResult<()> {
    let target_module_id = import_prologue(cx, in_module, import)?;
    let name = alias.unwrap_or(*import.path.last().unwrap());
    let id = cx.define().new_global(in_module, vis, DefKind::Variable, name, Mutability::Imm)?;
    cx.def_to_ty.insert(id, Ty::new(TyKind::Module(target_module_id)));
    Ok(())
}

pub(super) fn define_unqualified(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<ImportedFns> {
    let item_to_module_id = define_globs(cx, ast)?;
    define_unqualified_names(cx, ast, item_to_module_id)
}

fn define_globs(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<ItemMap<ModuleId>> {
    let mut item_to_module_id = ItemMap::<ModuleId>::default();

    for (module, item, item_id) in ast.items_with_id() {
        let ast::Item::Import(import) = item else { continue };
        let ast::ImportKind::Unqualified { imports } = &import.kind else { continue };

        let in_module = module.id;
        let target_module_id = import_prologue(cx, in_module, import)?;
        item_to_module_id.insert(item_id, target_module_id);

        for uim in imports {
            let ast::UnqualifiedImport::Glob(is_ufcs, _) = uim else { continue };
            insert_glob_target(cx, in_module, target_module_id, *is_ufcs);
        }
    }

    Ok(item_to_module_id)
}

fn define_unqualified_names(
    cx: &mut Typeck<'_>,
    ast: &Ast,
    item_to_module_id: ItemMap<ModuleId>,
) -> DiagnosticResult<ImportedFns> {
    let mut imported_fns = ImportedFns::default();

    for (module, item, item_id) in ast.items_with_id() {
        let ast::Item::Import(import) = item else { continue };
        let ast::ImportKind::Unqualified { imports } = &import.kind else { continue };

        let imported_fns_entry = imported_fns.entry(module.id).or_default();
        let in_module = module.id;
        let target_module_id = item_to_module_id[&item_id];

        for uim in imports {
            let ast::UnqualifiedImport::Name(name, alias, vis) = uim else { continue };
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
    let mut target_module_id = cx.db.find_module_by_path(&import.module_path).unwrap().id;

    // We skip the first part since it is the import root module name
    for &part in import.path.iter().skip(1) {
        let id = cx.lookup().query(from_module, target_module_id, &Query::Name(part))?;

        if !cx.def_to_ty.contains_key(&id) {
            return Err(errors::expected_module(format!("found {}", cx.db[id].kind), part.span()));
        }

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
