mod attrs;
mod builtins;
mod define;
mod errors;
mod fns;
mod imports;
mod lets;
mod types;

use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap, UstrSet};

use crate::{
    ast,
    ast::Ast,
    db::{AdtId, Db, DefId, ModuleId},
    diagnostics::DiagnosticResult,
    hir::Hir,
    middle::{Pat, Vis},
    span::Span,
    ty::{Ty, TyKind},
    typeck2::builtins::BuiltinTys,
};

pub fn typeck(db: &mut Db, ast: Ast) -> DiagnosticResult<Hir> {
    let mut cx = Typeck::new(db);
    let mut res_map = ResolutionMap::new();
    cx.init_global_env(&ast);
    imports::define_extern_imports(&mut cx, &ast)?;
    types::define(&mut cx, &mut res_map, &ast)?;
    lets::define(&mut cx, &mut res_map, &ast)?;
    fns::define(&mut cx, &mut res_map, &ast)?;
    Ok(cx.hir)
}

pub(super) struct Typeck<'db> {
    db: &'db mut Db,

    /// The Hir being constructed
    hir: Hir,

    /// The global namespace, mapped by module
    global_env: GlobalEnv,

    /// A mapping from definitions to their resolved type
    def_to_ty: FxHashMap<DefId, Ty>,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db) -> Self {
        let mut def_to_ty = FxHashMap::default();
        let builtin_tys = BuiltinTys::new(db, &mut def_to_ty);
        Self { db, hir: Hir::new(), global_env: GlobalEnv::new(builtin_tys), def_to_ty }
    }

    fn init_global_env(&mut self, ast: &Ast) {
        for module in &ast.modules {
            self.global_env.insert_module(module.id);
        }
    }
}

/// Various mappings and resolutions from the `define_*` passes
pub(super) struct ResolutionMap {
    pub(super) item_to_def: FxHashMap<ast::GlobalItemId, DefId>,
    pub(super) item_to_adt: FxHashMap<ast::GlobalItemId, AdtId>,
    pub(super) item_to_pat: FxHashMap<ast::GlobalItemId, Pat>,
}

impl ResolutionMap {
    pub(super) fn new() -> Self {
        Self {
            item_to_def: FxHashMap::default(),
            item_to_adt: FxHashMap::default(),
            item_to_pat: FxHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct GlobalEnv {
    modules: FxHashMap<ModuleId, ModuleEnv>,
    builtin_tys: BuiltinTys,
}

impl GlobalEnv {
    pub fn new(builtin_tys: BuiltinTys) -> Self {
        Self { modules: FxHashMap::default(), builtin_tys }
    }

    #[track_caller]
    pub fn module(&self, id: ModuleId) -> &ModuleEnv {
        self.modules.get(&id).unwrap()
    }

    #[track_caller]
    pub fn module_mut(&mut self, id: ModuleId) -> &mut ModuleEnv {
        self.modules.get_mut(&id).unwrap()
    }

    fn insert_module(&mut self, id: ModuleId) {
        self.modules.insert(id, ModuleEnv::new(id));
    }
}

#[derive(Debug)]
pub struct ModuleEnv {
    pub module_id: ModuleId,
    pub ns: Namespace,
    pub assoc_ns: FxHashMap<AssocTy, Namespace>,
}

impl ModuleEnv {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, ns: Namespace::new(module_id), assoc_ns: FxHashMap::default() }
    }
}

#[derive(Debug)]
pub struct Namespace {
    pub module_id: ModuleId,
    defs: UstrMap<NamespaceDef>,
    // pub fns: UstrMap<FnCandidateSet>,
    pub fn_names: UstrSet,
}

impl Namespace {
    pub fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            defs: UstrMap::default(),
            // fns: FxHashMap::default()
            fn_names: UstrSet::default(),
        }
    }

    pub fn insert_def(&mut self, name: Ustr, def: NamespaceDef) -> Option<NamespaceDef> {
        self.defs.insert(name, def)
    }

    pub fn get_def(&self, from_module: ModuleId, name: Ustr) -> Option<DefId> {
        self.defs
            .get(&name)
            .filter(|def| def.vis == Vis::Public || from_module == self.module_id)
            .map(|def| def.id)
    }

    pub fn contains_def(&self, name: Ustr) -> Option<&NamespaceDef> {
        self.defs.get(&name)
    }
}

#[derive(Debug)]
pub struct NamespaceDef {
    pub id: DefId,
    pub vis: Vis,
    pub span: Span,
}

impl NamespaceDef {
    pub fn new(id: DefId, vis: Vis, span: Span) -> Self {
        Self { id, vis, span }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub(super) enum AssocTy {
    Adt(AdtId),
    BuiltinTy(Ty),
}

impl AssocTy {
    pub(super) fn ty(self, db: &Db) -> Ty {
        match self {
            Self::Adt(adt_id) => db[adt_id].ty(),
            Self::BuiltinTy(ty) => ty,
        }
    }
}

impl From<Ty> for AssocTy {
    fn from(value: Ty) -> Self {
        match value.kind() {
            TyKind::Adt(adt_id, _) => Self::Adt(*adt_id),
            _ => Self::BuiltinTy(value),
        }
    }
}
