use compiler_core::{
    ast,
    ast::Ast,
    db::{DefId, DefKind, FnInfo, ModuleId},
    diagnostics::{Diagnostic, DiagnosticResult},
    middle::{IsUfcs, Mutability, Vis},
    span::{Span, Spanned as _},
    ty::{Ty, TyKind},
    word::Word,
};
use itertools::{Itertools as _, Position};
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::{Ustr, UstrMap};

use crate::{
    attrs, errors,
    ns::{self, NsDef},
    Typeck,
};

pub(crate) fn define(cx: &mut Typeck, ast: &Ast) -> ImportedFns {
    let imports = build_imports_map(cx, ast);
    let mut define = Define::new(cx);
    define.define_imports(imports);
    define.imported_fns
}

fn build_imports_map(cx: &mut Typeck, ast: &Ast) -> ImportsMap {
    let mut map = ImportsMap::default();

    for (module, item) in ast.items() {
        let ast::Item::Import(import) = item else { continue };
        attrs::validate(cx, &import.attrs, attrs::Placement::Import);

        let entry = map.entry(module.id).or_default();
        let root_module_id = cx.db.find_module_by_path(&import.module_path).unwrap().id;
        BuildImportsMap::new(cx, entry, root_module_id, module.id, import.vis).build(&import.tree);
    }

    map
}

struct BuildImportsMap<'a, 'db> {
    cx: &'a mut Typeck<'db>,
    entry: &'a mut Imports,
    root_module_id: ModuleId,
    module_id: ModuleId,
    vis: Vis,
}

impl<'a, 'db> BuildImportsMap<'a, 'db> {
    fn new(
        cx: &'a mut Typeck<'db>,
        entry: &'a mut Imports,
        root_module_id: ModuleId,
        module_id: ModuleId,
        vis: Vis,
    ) -> Self {
        Self { cx, entry, root_module_id, module_id, vis }
    }

    fn build(&mut self, tree: &ast::ImportTree) {
        self.build_helper(vec![], tree);
    }

    fn build_helper(&mut self, path: Vec<Word>, tree: &ast::ImportTree) {
        match tree {
            ast::ImportTree::Group(imports) => {
                for import in imports {
                    self.build_helper(path.clone(), import);
                }
            }
            ast::ImportTree::Path(name, next) => {
                let mut new_path = path.clone();
                new_path.push(*name);
                self.build_helper(new_path, next);
            }
            ast::ImportTree::Name(name, alias) => self.insert_name(path, *name, *alias),
            ast::ImportTree::Glob(is_ufcs, span) => {
                self.insert_glob(path, *is_ufcs, *span);
            }
        }
    }

    fn insert_name(&mut self, path: Vec<Word>, name: Word, alias: Option<Word>) {
        let mut new_path = path.clone();
        new_path.push(name);

        let alias = alias.unwrap_or(name);
        let import = Import {
            root_module_id: self.root_module_id,
            path: new_path,
            alias,
            module_id: self.module_id,
            vis: self.vis,
        };

        if let Some(prev) = self.entry.imports.insert(alias.name(), import) {
            self.cx.db.diagnostics.add(errors::multiple_item_def_err(prev.span(), name))
        }
    }

    fn insert_glob(&mut self, path: Vec<Word>, is_ufcs: IsUfcs, span: Span) {
        self.entry.glob_imports.push(GlobImport {
            root_module_id: self.root_module_id,
            path,
            is_ufcs,
            module_id: self.module_id,
            vis: self.vis,
            span,
        });
    }
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

    fn define_imports(&mut self, map: ImportsMap) {
        for imports in map.values() {
            for imp in imports.imports.values() {
                if let Err(diagnostic) = self.define(&map, imp) {
                    self.cx.db.diagnostics.add(diagnostic);
                }
            }

            for imp in &imports.glob_imports {
                if let Err(diagnostic) = self.define_glob(&map, imp) {
                    self.cx.db.diagnostics.add(diagnostic);
                }
            }
        }
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
                let id = self.import_module(imp, module_id);
                (Resolved::Def(NsDef::from_def_id(self.cx.db, id)), module_id)
            }
            Resolved::Def(def) => {
                self.import_def(imp, def.id)?;
                (resolved, self.cx.db[def.id].scope.module_id)
            }
            Resolved::Fn(target_module_id, name) => {
                self.import_fns(in_module, target_module_id, name, imp.alias);
                (resolved, target_module_id)
            }
        };
        self.insert_glob_import(
            in_module,
            res_module_id,
            ns::GlobImport { is_ufcs: IsUfcs::Yes, vis: Vis::Module },
        );

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

        self.insert_glob_import(
            in_module,
            res_module_id,
            ns::GlobImport { is_ufcs: imp.is_ufcs, vis: imp.vis },
        );

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

    fn import_module(&mut self, imp: &Import, module_id: ModuleId) -> DefId {
        let id = self.cx.define().new_global(
            imp.module_id,
            imp.vis,
            DefKind::Global,
            imp.alias,
            Mutability::Imm,
        );
        self.cx.def_to_ty.insert(id, Ty::new(TyKind::Module(module_id)));
        id
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
        alias: Word,
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

    fn insert_glob_import(
        &mut self,
        in_module: ModuleId,
        target_module_id: ModuleId,
        imp: ns::GlobImport,
    ) {
        let entry = self.cx.global_env.module_mut(in_module).globs.entry(target_module_id);
        let entry = entry.or_insert(imp);

        // Regular glob imports are considered `stronger` than UFCS imports.
        // When glob importing of the same module multiple times, we always want to keep
        // `IsUfcs::No` so that it doesn't become a UFCS import by accident.
        if entry.is_ufcs == IsUfcs::Yes {
            entry.is_ufcs = imp.is_ufcs;
        }

        // When two glob imports of the same module have different visibilities,
        // we want to keep the most public (aka accessible) of the two.
        entry.vis = entry.vis.max(imp.vis);
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

pub(crate) fn fill_imported_fn_candidates(cx: &mut Typeck, imported_fns: ImportedFns) {
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

            let mut candidate =
                from_set.iter().find(|c| c.id == f.id).expect("candidate to be defined").clone();
            candidate.word = f.alias;

            let set = cx.global_env.module_mut(module_id).ns.fns.entry(f.alias.name()).or_default();

            if let Err(err) = set.try_insert(candidate) {
                cx.db.diagnostics.add(err.into_diagnostic(cx.db));
            }
        }
    }
}

pub(crate) type ImportedFns = FxHashMap<ModuleId, Vec<ImportedFn>>;

pub(crate) struct ImportedFn {
    pub(crate) id: DefId,
    pub(crate) name: Ustr,
    pub(crate) alias: Word,
}

// Iterate all accessible globs which are reached by other glob imports, creating a transitive
// chain of glob imports which are then collected and flattened in each module's `ModuleEnv::globs`
pub(crate) fn define_transitive_globs(cx: &mut Typeck) {
    let mut trans = TransitiveGlobs::default();

    // Collect
    for &module_id in cx.global_env.modules.keys() {
        CollectTransitiveGlobs::new(cx, &mut trans, module_id).traverse_root(module_id);
    }

    // Insert
    for (module_id, pairs) in trans {
        let module_globs = &mut cx.global_env.module_mut(module_id).globs;
        for (glob_module_id, imp) in pairs {
            module_globs.insert(glob_module_id, imp);
        }
    }
}

type TransitiveGlobs = FxHashMap<ModuleId, Vec<(ModuleId, ns::GlobImport)>>;

struct CollectTransitiveGlobs<'cx, 'db> {
    cx: &'cx Typeck<'db>,
    trans: &'cx mut TransitiveGlobs,
    module_id: ModuleId,
    visited: FxHashSet<ModuleId>,
}

impl<'cx, 'db> CollectTransitiveGlobs<'cx, 'db> {
    fn new(cx: &'cx Typeck<'db>, trans: &'cx mut TransitiveGlobs, module_id: ModuleId) -> Self {
        trans.insert(module_id, vec![]);
        Self { cx, trans, module_id, visited: FxHashSet::default() }
    }

    fn traverse_root(&mut self, module_id: ModuleId) {
        for (&glob_module_id, imp) in &self.cx.global_env.module(module_id).globs {
            self.traverse(glob_module_id, imp);
        }
    }

    fn traverse(&mut self, module_id: ModuleId, parent_imp: &ns::GlobImport) {
        if !self.visited.insert(module_id) {
            return;
        }

        for (&glob_module_id, imp) in &self.cx.global_env.module(module_id).globs {
            self.collect(module_id, glob_module_id, parent_imp, imp);
        }
    }

    fn collect(
        &mut self,
        in_module: ModuleId,
        glob_module_id: ModuleId,
        parent_imp: &ns::GlobImport,
        imp: &ns::GlobImport,
    ) {
        if self.module_id != in_module && !self.cx.can_access(in_module, glob_module_id, imp.vis) {
            return;
        }

        let trans_imp = parent_imp.merge_transitive(imp);
        self.traverse(glob_module_id, &trans_imp);

        if glob_module_id != self.module_id {
            self.insert(glob_module_id, trans_imp);
        }
    }

    fn insert(&mut self, glob_module_id: ModuleId, imp: ns::GlobImport) {
        self.trans.get_mut(&self.module_id).unwrap().push((glob_module_id, imp));
    }
}

pub(crate) fn insert_prelude(cx: &mut Typeck) {
    let prelude_module_id =
        cx.db.find_module_by_qpath("std", ["prelude"]).expect("std.prelude to exist").id;

    for (&module_id, env) in &mut cx.global_env.modules {
        // Don't insert the prelude for modules in package `std`
        if ns::in_std(cx.db, module_id) {
            continue;
        }

        // Don't insert the prelude for modules which already imported it
        env.globs
            .entry(prelude_module_id)
            .or_insert(ns::GlobImport { is_ufcs: IsUfcs::No, vis: Vis::Module });
    }
}
