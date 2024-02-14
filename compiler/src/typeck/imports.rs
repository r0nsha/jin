use core::fmt;
use std::iter;

use itertools::{Itertools as _, Position};
use petgraph::prelude::*;
use rustc_hash::{FxHashMap, FxHashSet};
use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    ast::Ast,
    db::{DefId, DefKind, FnInfo, ModuleId},
    diagnostics::{Diagnostic, DiagnosticResult},
    middle::{IsUfcs, Mutability, Vis},
    span::{Span, Spanned as _},
    ty::{Ty, TyKind},
    typeck::{
        attrs, errors,
        lookup::{ImportLookupResult, Query},
        ItemMap, Typeck,
    },
    word::Word,
};

type NodeMapping = FxHashMap<ModuleId, UstrMap<NodeIndex>>;

pub(super) fn build_graph(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    let mut graph = ImportGraph::new();
    let node_mapping = build_graph_nodes(cx, &mut graph, ast)?;
    build_graph_edges(cx, &mut graph, node_mapping)?;
    Ok(())
}

fn build_graph_nodes(
    cx: &mut Typeck,
    graph: &mut ImportGraph,
    ast: &Ast,
) -> DiagnosticResult<NodeMapping> {
    let mut node_mapping = FxHashMap::<ModuleId, UstrMap<NodeIndex>>::default();

    // Build ImportNodeKind::{Def, Fn}
    for (&module_id, env) in &cx.global_env.modules {
        let map = node_mapping.entry(module_id).or_default();

        for def in env.ns.defs.values() {
            let idx = graph.add_def(def.data);
            map.insert(cx.db[def.data].name, idx);
        }

        for &name in env.ns.defined_fns.keys() {
            let idx = graph.add_fn(module_id, name);
            map.insert(name, idx);
        }
    }

    // Build ImportNodeKind::Import
    for (module, item) in ast.items() {
        let ast::Item::Import(import) = item else { continue };
        let map = node_mapping.entry(module.id).or_default();
        let root_module_id = cx.db.find_module_by_path(&import.module_path).unwrap().id;

        match &import.kind {
            ast::ImportKind::Qualified { alias, vis } => {
                let node = ImportNode {
                    root_module_id,
                    path: import.path.clone(),
                    alias: *alias,
                    module_id: module.id,
                    vis: *vis,
                    span: import.span,
                };
                let name = node.name().name();
                let idx = graph.add_import(node);
                map.insert(name, idx);
            }
            ast::ImportKind::Unqualified { imports } => {
                for uim in imports {
                    match uim {
                        ast::UnqualifiedImport::Name(name, alias, vis) => {
                            let node = ImportNode {
                                root_module_id,
                                path: import
                                    .path
                                    .iter()
                                    .copied()
                                    .chain(iter::once(*name))
                                    .collect(),
                                alias: *alias,
                                module_id: module.id,
                                vis: *vis,
                                span: import.span,
                            };
                            let name = node.name().name();
                            let idx = graph.add_import(node);
                            map.insert(name, idx);
                        }
                        ast::UnqualifiedImport::Glob(_, _) => (),
                    }
                }
            }
        }
    }

    Ok(node_mapping)
}

fn build_graph_edges(
    cx: &mut Typeck,
    graph: &mut ImportGraph,
    node_mapping: NodeMapping,
) -> DiagnosticResult<()> {
    let mut bge = BuildGraphEdges::new(cx, node_mapping);

    let import_indices: Vec<_> = graph.import_indices().collect();
    for idx in import_indices {
        bge.resolve_import_node(graph, idx)?;
    }

    println!("{:?}", petgraph::dot::Dot::new(&graph.0));
    todo!();
    Ok(())
}

struct BuildGraphEdges<'db> {
    cx: &'db Typeck<'db>,
    mapping: NodeMapping,
    visited: FxHashSet<NodeIndex>,
}

impl<'db> BuildGraphEdges<'db> {
    fn new(cx: &'db Typeck<'db>, mapping: NodeMapping) -> Self {
        Self { cx, mapping, visited: FxHashSet::default() }
    }

    fn resolve_node(
        &mut self,
        graph: &mut ImportGraph,
        parent_idx: NodeIndex,
        idx: NodeIndex,
    ) -> DiagnosticResult<()> {
        match &graph.0[idx] {
            ImportGraphNode::Def(_) | ImportGraphNode::Fn(_, _) => {
                assert_ne!(parent_idx, idx);
                graph.add_edge(parent_idx, idx);
                Ok(())
            }
            ImportGraphNode::Import(_) => self.resolve_import_node(graph, idx),
        }
    }

    fn resolve_import_node(
        &mut self,
        graph: &mut ImportGraph,
        idx: NodeIndex,
    ) -> DiagnosticResult<()> {
        let ImportGraphNode::Import(node) = graph.0[idx].clone() else { unreachable!() };
        let curr_module_id = node.root_module_id;

        for &part in node.path.iter().skip(1) {
            let Some(&part_node_idx) = self.mapping[&curr_module_id].get(&part.name()) else {
                return Err(errors::name_not_found(
                    self.cx.db,
                    node.module_id,
                    curr_module_id,
                    part,
                ));
            };

            self.resolve_node(graph, idx, part_node_idx)?;
        }

        Ok(())
    }
}

pub(super) fn define(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<ImportedFns> {
    let imports = build_imports_map(cx, ast)?;
    let mut define = Define::new(cx);
    define.define_imports(imports)?;
    Ok(define.imported_fns)
}

fn build_imports_map(cx: &Typeck, ast: &Ast) -> DiagnosticResult<ImportsMap> {
    let mut imports = ImportsMap::default();

    for (module, item) in ast.items() {
        let ast::Item::Import(import) = item else { continue };

        attrs::validate(&import.attrs, attrs::Placement::Import)?;
        let entry = imports.entry(module.id).or_default();

        let root_module_id = cx.db.find_module_by_path(&import.module_path).unwrap().id;

        let mut insert = |name: Word, imp: Import| -> DiagnosticResult<()> {
            if let Some(prev) = entry.imports.insert(name.name(), imp) {
                Err(errors::multiple_item_def_err(prev.span(), name))
            } else {
                Ok(())
            }
        };

        match &import.kind {
            ast::ImportKind::Qualified { alias, vis } => {
                let alias = alias.unwrap_or_else(|| *import.path.last().unwrap());
                insert(
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
                            insert(
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
                        ast::UnqualifiedImport::Glob(_, _) => (),
                    }
                }
            }
        }
    }

    Ok(imports)
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
                (Resolved::Def(id), module_id)
            }
            Resolved::Def(id) => {
                self.import_def(imp, id)?;
                (resolved, self.cx.db[id].scope.module_id)
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
        // Resolve the entire import path, recursing if needed
        let in_module = imp.module_id;
        let resolved =
            self.resolve_import_path(map, imp.module_id, imp.root_module_id, &imp.path)?;

        let res_module_id = match resolved {
            Resolved::Def(id) => self.cx.expect_module_def(id, imp.span)?,
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
        // imp: &Import,
    ) -> DiagnosticResult<Resolved> {
        let mut curr_module_id = root_module_id;

        // We skip the first part since it has already been used to specify `root_module_id`
        for (pos, &part) in path.iter().skip(1).with_position() {
            let env = self.cx.global_env.module(curr_module_id);
            let name = part.name();

            let resolved = if let Some(def) = env.ns.defs.get(&name) {
                Resolved::Def(def.data)
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

            match pos {
                Position::First | Position::Middle => match resolved {
                    Resolved::Module(_) => unreachable!(),
                    Resolved::Def(id) => {
                        if !self.cx.def_to_ty.contains_key(&id) {
                            return Err(errors::expected_module(
                                format!("found {}", self.cx.db[id].kind),
                                part.span(),
                            ));
                        }

                        curr_module_id = self.cx.expect_module_def(id, part.span())?;
                    }
                    Resolved::Fn(_, _) => return Err(Self::expected_module_found_fn(part.span())),
                },
                Position::Last | Position::Only => return Ok(resolved),
            }
        }

        Ok(Resolved::Module(root_module_id))
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
    Def(DefId),
    Fn(ModuleId, Ustr),
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

        target_module_id = cx.expect_module_def(id, part.span())?;
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

#[derive(Debug)]
struct ImportGraph(Graph<ImportGraphNode, ()>);

impl ImportGraph {
    fn new() -> Self {
        Self(Graph::new())
    }

    fn node(&self, idx: NodeIndex) -> &ImportGraphNode {
        &self.0.raw_nodes()[idx.index()].weight
    }

    fn import_indices(&self) -> impl Iterator<Item = NodeIndex> + '_ {
        self.0.node_indices().filter(|idx| matches!(self.0[*idx], ImportGraphNode::Import(_)))
    }

    fn add_def(&mut self, id: DefId) -> NodeIndex {
        self.0.add_node(ImportGraphNode::Def(id))
    }

    fn add_fn(&mut self, module_id: ModuleId, name: Ustr) -> NodeIndex {
        self.0.add_node(ImportGraphNode::Fn(module_id, name))
    }

    fn add_import(&mut self, imp: ImportNode) -> NodeIndex {
        self.0.add_node(ImportGraphNode::Import(imp))
    }

    fn add_edge(&mut self, a: NodeIndex, b: NodeIndex) {
        self.0.add_edge(a, b, ());
    }
}

#[derive(Clone)]
enum ImportGraphNode {
    Def(DefId),
    Fn(ModuleId, Ustr),
    Import(ImportNode),
}

#[derive(Debug, Clone)]
struct ImportNode {
    root_module_id: ModuleId,
    path: Vec<Word>,
    alias: Option<Word>,
    module_id: ModuleId,
    vis: Vis,
    span: Span,
}

impl ImportNode {
    fn name(&self) -> Word {
        self.alias.unwrap_or_else(|| *self.path.last().unwrap())
    }
}

impl fmt::Debug for ImportGraphNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Def(id) => write!(f, "Def({id:?})"),
            Self::Fn(mid, name) => write!(f, "Fn({mid:?}, {name})"),
            Self::Import(imp) => {
                write!(
                    f,
                    "Import({:?}, {}, alias: {:?})",
                    imp.module_id,
                    imp.path.iter().map(ToString::to_string).join(", "),
                    imp.alias.as_ref().map(ToString::to_string)
                )
            }
        }
    }
}
