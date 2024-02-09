mod attrs;
mod builtins;
mod define;
mod errors;
mod imports;
mod types;

use rustc_hash::FxHashMap;

use crate::{
    ast,
    ast::Ast,
    db::{AdtId, Db, DefId},
    diagnostics::DiagnosticResult,
    hir::Hir,
    ty::Ty,
    typeck2::builtins::BuiltinTys,
};

pub fn typeck(db: &mut Db, ast: Ast) -> DiagnosticResult<Hir> {
    let mut cx = Typeck::new(db);
    imports::define_extern_imports(&mut cx, &ast)?;
    types::define_types(&mut cx, &ast)?;
    Ok(cx.hir)
}

pub(super) struct Typeck<'db> {
    db: &'db mut Db,

    /// The Hir being constructed
    hir: Hir,

    /// Various mappings and resolutions from the `define_*` passes
    res_map: ResolutionMap,

    /// The set of builtin types with their definitions
    builtin_tys: BuiltinTys,

    /// A mapping from definitions to their resolved type
    def_to_ty: FxHashMap<DefId, Ty>,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db) -> Self {
        let mut def_to_ty = FxHashMap::default();
        let builtin_tys = BuiltinTys::new(db, &mut def_to_ty);
        Self { db, hir: Hir::new(), res_map: ResolutionMap::new(), builtin_tys, def_to_ty }
    }
}

pub(super) struct ResolutionMap {
    pub(super) item_to_adt: FxHashMap<ast::GlobalItemId, AdtId>,
}

impl ResolutionMap {
    pub(super) fn new() -> Self {
        Self { item_to_adt: FxHashMap::default() }
    }
}
