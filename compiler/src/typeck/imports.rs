use std::iter;

use itertools::{Itertools as _, Position};
use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    ast::Ast,
    db::{DefId, DefKind, FnInfo, ModuleId},
    diagnostics::{Diagnostic, DiagnosticResult},
    middle::{IsUfcs, Mutability, Vis},
    span::{Span, Spanned as _},
    ty::{Ty, TyKind},
    typeck::{attrs, errors, ns::NsDef, Typeck},
    word::Word,
};

pub(super) fn define(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<ImportedFns> {
    let imports = build_imports_map(cx, ast)?;
    let mut define = Define::new(cx);
    define.define_imports(imports)?;
    Ok(define.imported_fns)
}

fn build_imports_map(cx: &Typeck, ast: &Ast) -> DiagnosticResult<ImportsMap> {
    fn insert_import(map: &mut Imports, name: Word, imp: Import) -> DiagnosticResult<()> {
        if let Some(prev) = map.imports.insert(name.name(), imp) {
            Err(errors::multiple_item_def_err(prev.span(), name))
        } else {
            Ok(())
        }
    }

    let mut map = ImportsMap::default();

    for (module, item) in ast.items() {
        let ast::Item::Import(import) = item else { continue };

        attrs::validate(&import.attrs, attrs::Placement::Import)?;
        let entry = map.entry(module.id).or_default();

        let root_module_id = cx.db.find_module_by_path(&import.module_path).unwrap().id;

        match &import.kind {
            ast::ImportKind::Qualified { alias, vis } => {
                let alias = alias.unwrap_or_else(|| *import.path.last().unwrap());
                insert_import(
                    entry,
                    alias,
                    Import {
                        root_module_id,
                        path: import.path.clone(),
                        alias,
                        module_id: module.id,
                        vis: *vis,
                    },
                )?;
            }
            ast::ImportKind::Unqualified { imports } => {
                for uim in imports {
                    match uim {
                        ast::UnqualifiedImport::Name(name, alias, vis) => {
                            let alias = alias.unwrap_or(*name);
                            insert_import(
                                entry,
                                alias,
                                Import {
                                    root_module_id,
                                    path: import
                                        .path
                                        .iter()
                                        .copied()
                                        .chain(iter::once(*name))
                                        .collect(),
                                    alias,
                                    module_id: module.id,
                                    vis: *vis,
                                },
                            )?;
                        }
                        ast::UnqualifiedImport::Glob(is_ufcs, span) => {
                            entry.glob_imports.push(GlobImport {
                                root_module_id,
                                path: import.path.clone(),
                                is_ufcs: *is_ufcs,
                                module_id: module.id,
                                vis: Vis::Private,
                                span: *span,
                            });
                        }
                    }
                }
            }
        }
    }

    Ok(map)
}

struct Define<'db, 'cx> {
    cx: &'cx mut Typeck<'db>,
    resolved: ResolvedMap,
    imported_fns: ImportedFns,
}

impl<'db, 'cx> Define<'db, 'cx> {
    fn new(cx: &'cx mut Typeck<'db>) -> Self {
        Self { cx, resolved: ResolvedMap::default(), imported_fns: ImportedFns::default() }
    }

    fn define_imports(&mut self, map: ImportsMap) -> DiagnosticResult<()> {
        for imports in map.values() {
            for imp in imports.imports.values() {
                self.define(&map, imp)?;
            }

            for imp in &imports.glob_imports {
                self.define_glob(&map, imp)?;
            }
        }

        Ok(())
    }

    fn define(&mut self, map: &ImportsMap, imp: &Import) -> DiagnosticResult<Resolved> {
        if let Some(res) = self.get_resolved_import_path(imp) {
            return Ok(*res);
        }

        // Resolve the entire import path, recursing if needed
        let in_module = imp.module_id;
        let resolved =
            self.resolve_import_path(map, imp.module_id, imp.root_module_id, &imp.path)?;

        // Insert imported modules as UFCS targets implicitly.
        // This is done because always adding `?` whenever we import any type is
        // really annoying... I couldn't find any issues with this yet, but if
        // some do pop up, we'll need to find a more clever solution for this
        // redundancy.
        let (resolved, res_module_id) = match resolved {
            Resolved::Module(module_id) => {
                let id = self.import_module(imp, module_id)?;
                (Resolved::Def(NsDef::from_def_id(self.cx.db, id)), module_id)
            }
            Resolved::Def(def) => {
                self.import_def(imp, def.id)?;
                (resolved, self.cx.db[def.id].scope.module_id)
            }
            Resolved::Fn(target_module_id, name) => {
                self.import_fns(in_module, target_module_id, name, imp.alias.name());
                (resolved, target_module_id)
            }
        };
        self.insert_glob_target(in_module, res_module_id, IsUfcs::Yes);

        self.resolved.entry(in_module).or_default().insert(imp.alias.name(), resolved);

        Ok(resolved)
    }

    fn define_glob(&mut self, map: &ImportsMap, imp: &GlobImport) -> DiagnosticResult<()> {
        let in_module = imp.module_id;
        let resolved =
            self.resolve_import_path(map, imp.module_id, imp.root_module_id, &imp.path)?;

        let res_module_id = match resolved {
            Resolved::Def(def) => self.cx.expect_module_def(def.id, imp.span)?,
            Resolved::Module(module_id) => module_id,
            Resolved::Fn(_, _) => return Err(Self::expected_module_found_fn(imp.span)),
        };
        self.insert_glob_target(in_module, res_module_id, imp.is_ufcs);

        Ok(())
    }

    fn resolve_import_path(
        &mut self,
        map: &ImportsMap,
        imp_module_id: ModuleId,
        root_module_id: ModuleId,
        path: &[Word],
    ) -> DiagnosticResult<Resolved> {
        let mut curr_module_id = root_module_id;

        // We skip the first part since it has already been used to specify `root_module_id`
        for (pos, &part) in path.iter().skip(1).with_position() {
            let env = self.cx.global_env.module(curr_module_id);
            let name = part.name();
            let span = part.span();

            let resolved = if let Some(def) = env.ns.defs.get(&name) {
                Resolved::Def(*def)
            } else if env.ns.defined_fns.get(&name).is_some() {
                Resolved::Fn(curr_module_id, name)
            } else if let Some(target_imp) =
                map.get(&curr_module_id).and_then(|m| m.imports.get(&name))
            {
                self.define(map, target_imp)?
            } else {
                return Err(errors::name_not_found(
                    self.cx.db,
                    imp_module_id,
                    curr_module_id,
                    part,
                ));
            };

            self.check_resolved_access(imp_module_id, &resolved, span)?;

            match pos {
                Position::First | Position::Middle => match resolved {
                    Resolved::Module(_) => unreachable!(),
                    Resolved::Def(def) => {
                        if !self.cx.def_to_ty.contains_key(&def.id) {
                            return Err(errors::expected_module(
                                format!("found {}", self.cx.db[def.id].kind),
                                span,
                            ));
                        }

                        curr_module_id = self.cx.expect_module_def(def.id, span)?;
                    }
                    Resolved::Fn(_, _) => return Err(Self::expected_module_found_fn(span)),
                },
                Position::Last | Position::Only => return Ok(resolved),
            }
        }

        Ok(Resolved::Module(root_module_id))
    }

    fn check_resolved_access(
        &mut self,
        from_module: ModuleId,
        resolved: &Resolved,
        span: Span,
    ) -> DiagnosticResult<()> {
        match resolved {
            Resolved::Module(_) => unreachable!(),
            Resolved::Def(def) => def.check_access(self.cx, from_module, span),
            Resolved::Fn(module_id, name) => {
                let defined_fns = self
                    .cx
                    .global_env
                    .module(*module_id)
                    .ns
                    .defined_fns
                    .get(name)
                    .expect("to be defined");

                if defined_fns.len() == 1 {
                    self.cx.check_access_def(from_module, defined_fns[0], span)
                } else {
                    Ok(())
                }
            }
        }
    }

    fn import_module(&mut self, imp: &Import, module_id: ModuleId) -> DiagnosticResult<DefId> {
        let id = self.cx.define().new_global(
            imp.module_id,
            imp.vis,
            DefKind::Global,
            imp.alias,
            Mutability::Imm,
        )?;
        self.cx.def_to_ty.insert(id, Ty::new(TyKind::Module(module_id)));
        Ok(id)
    }

    fn import_def(&mut self, imp: &Import, id: DefId) -> DiagnosticResult<()> {
        self.cx.define().global(imp.module_id, imp.alias, id, imp.vis)?;
        Ok(())
    }

    fn import_fns(
        &mut self,
        in_module: ModuleId,
        target_module_id: ModuleId,
        name: Ustr,
        alias: Ustr,
    ) {
        let defined_fns = self
            .cx
            .global_env
            .module(target_module_id)
            .ns
            .defined_fns
            .get(&name)
            .expect("to be defined");

        for &id in defined_fns {
            self.imported_fns.entry(in_module).or_default().push(ImportedFn { id, name, alias });
        }
    }

    fn get_resolved_import_path(&self, imp: &Import) -> Option<&Resolved> {
        self.resolved.get(&imp.module_id).and_then(|m| m.get(&imp.alias.name()))
    }

    fn insert_glob_target(
        &mut self,
        in_module: ModuleId,
        target_module_id: ModuleId,
        is_ufcs: IsUfcs,
    ) {
        self.cx.global_env.module_mut(in_module).globs.insert(target_module_id, is_ufcs);
    }

    fn expected_module_found_fn(span: Span) -> Diagnostic {
        errors::expected_module(format!("found {}", DefKind::Fn(FnInfo::Bare)), span)
    }
}

type ImportsMap = FxHashMap<ModuleId, Imports>;

#[derive(Debug, Clone)]
struct Imports {
    imports: UstrMap<Import>,
    glob_imports: Vec<GlobImport>,
}

impl Imports {
    fn new() -> Self {
        Self { imports: UstrMap::default(), glob_imports: vec![] }
    }
}

impl Default for Imports {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct Import {
    root_module_id: ModuleId,
    path: Vec<Word>,
    alias: Word,
    module_id: ModuleId,
    vis: Vis,
}

impl Import {
    fn span(&self) -> Span {
        self.alias.span()
    }
}

#[derive(Debug, Clone)]
struct GlobImport {
    root_module_id: ModuleId,
    path: Vec<Word>,
    is_ufcs: IsUfcs,
    module_id: ModuleId,
    vis: Vis,
    span: Span,
}

type ResolvedMap = FxHashMap<ModuleId, UstrMap<Resolved>>;

#[derive(Debug, Clone, Copy)]
enum Resolved {
    Module(ModuleId),
    Def(NsDef),
    Fn(ModuleId, Ustr),
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
