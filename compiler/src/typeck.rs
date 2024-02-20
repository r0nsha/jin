mod attrs;
mod builtins;
mod coerce;
mod define;
mod errors;
mod exprs;
mod fns;
mod imports;
mod items;
mod late;
mod lookup;
mod normalize;
mod ns;
mod pmatch;
mod subst;
mod tyexpr;
mod types;
mod unify;

use std::cell::RefCell;

use ena::unify::{InPlace, InPlaceUnificationTable, Snapshot};
use rustc_hash::FxHashMap;

use crate::{
    ast,
    ast::Ast,
    counter::Counter,
    db::{AdtId, Db, DefId, ModuleId},
    diagnostics::DiagnosticResult,
    hir,
    hir::Hir,
    middle::{Pat, Vis},
    span::Span,
    ty::{FloatVar, InferTy, IntVar, Ty, TyKind, TyVar},
    typeck::{builtins::BuiltinTys, ns::GlobalEnv},
};

pub fn typeck(db: &mut Db, ast: Ast) -> Hir {
    let mut cx = Typeck::new(db);

    if let Err(diagnostic) = typeck_inner(&mut cx, ast) {
        cx.db.diagnostics.add(diagnostic);
    }

    cx.hir
}

fn typeck_inner(cx: &mut Typeck, ast: Ast) -> DiagnosticResult<()> {
    cx.init_global_env(&ast);

    // Define
    items::define(cx, &ast);
    let imported_fns = imports::define(cx, &ast)?;
    imports::insert_prelude(cx);
    imports::define_transitive_globs(cx);

    if cx.db.diagnostics.any_errors() {
        return Ok(());
    }

    // Check top-level types and signatures
    types::check(cx, &ast)?;
    items::check_sigs(cx, &ast)?;
    imports::fill_imported_fn_candidates(cx, imported_fns)?;

    // Check bodies & expressions
    items::check_bodies(cx, &ast)?;

    // Finalization steps & checks
    subst::subst(cx);
    late::checks(cx.db, &mut cx.hir);

    Ok(())
}

pub(super) struct Typeck<'db> {
    pub(super) db: &'db mut Db,

    /// The Hir being constructed
    pub(super) hir: Hir,

    /// The global namespace, mapped by module
    pub(super) global_env: GlobalEnv,

    /// Mappings used for name and item resolution
    pub(super) res_map: ResMap,

    /// Stores type unification tables
    pub(super) storage: RefCell<TyStorage>,

    /// A mapping from definitions to their resolved type
    pub(super) def_to_ty: FxHashMap<DefId, Ty>,

    /// Counter for generating hir::ExprId's
    pub(super) expr_id: Counter<hir::ExprId>,
}

#[derive(Debug)]
pub struct TyStorage {
    pub ty: InPlaceUnificationTable<TyVar>,
    pub int: InPlaceUnificationTable<IntVar>,
    pub float: InPlaceUnificationTable<FloatVar>,
}

impl TyStorage {
    pub fn new() -> Self {
        Self {
            ty: InPlaceUnificationTable::new(),
            int: InPlaceUnificationTable::new(),
            float: InPlaceUnificationTable::new(),
        }
    }

    pub fn snapshot(&mut self) -> TyStorageSnapshot {
        TyStorageSnapshot {
            ty: self.ty.snapshot(),
            int: self.int.snapshot(),
            float: self.float.snapshot(),
        }
    }

    pub fn rollback_to(&mut self, to: TyStorageSnapshot) {
        self.ty.rollback_to(to.ty);
        self.int.rollback_to(to.int);
        self.float.rollback_to(to.float);
    }
}

pub struct TyStorageSnapshot {
    ty: Snapshot<InPlace<TyVar>>,
    int: Snapshot<InPlace<IntVar>>,
    float: Snapshot<InPlace<FloatVar>>,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db) -> Self {
        let mut def_to_ty = FxHashMap::default();
        let builtin_tys = BuiltinTys::new(db, &mut def_to_ty);
        Self {
            db,
            hir: Hir::new(),
            global_env: GlobalEnv::new(builtin_tys),
            res_map: ResMap::new(),
            storage: RefCell::new(TyStorage::new()),
            def_to_ty,
            expr_id: Counter::new(),
        }
    }

    fn init_global_env(&mut self, ast: &Ast) {
        for module in &ast.modules {
            self.global_env.insert_module(module.id);
        }
    }

    fn expect_module_def(&self, def_id: DefId, span: Span) -> DiagnosticResult<ModuleId> {
        match self.normalize(self.def_ty(def_id)).kind() {
            TyKind::Module(module_id) => Ok(*module_id),
            ty => Err(errors::expected_module(format!("type `{}`", &ty.to_string(self.db)), span)),
        }
    }

    fn expr(&mut self, kind: hir::ExprKind, ty: Ty, span: Span) -> hir::Expr {
        hir::Expr { id: self.expr_id.increment(), kind, ty, span }
    }

    fn expr_or_block(&mut self, expr: hir::Expr) -> hir::Expr {
        if let hir::ExprKind::Block(_) = &expr.kind {
            return expr;
        }

        let ty = expr.ty;
        let span = expr.span;

        self.expr(hir::ExprKind::Block(hir::Block { exprs: vec![expr] }), ty, span)
    }

    fn unit_expr(&mut self, span: Span) -> hir::Expr {
        self.expr(hir::ExprKind::Block(hir::Block { exprs: vec![] }), self.db.types.unit, span)
    }

    #[inline]
    pub fn fresh_ty_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Ty(self.fresh_var())))
    }

    #[inline]
    pub fn fresh_var(&self) -> TyVar {
        self.storage.borrow_mut().ty.new_key(None)
    }

    #[inline]
    pub fn fresh_int_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Int(self.storage.borrow_mut().int.new_key(None))))
    }

    #[inline]
    pub fn fresh_float_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Float(self.storage.borrow_mut().float.new_key(None))))
    }

    #[track_caller]
    fn def_ty(&self, id: DefId) -> Ty {
        self.def_to_ty[&id]
    }

    #[inline]
    pub(super) fn check_access_def(
        &self,
        from_module: ModuleId,
        accessed: DefId,
        span: Span,
    ) -> DiagnosticResult<()> {
        self.can_access_def(from_module, accessed).then_some(()).ok_or_else(|| {
            let def = &self.db[accessed];
            errors::private_access_violation(self.db, def.scope.module_id, def.name, span)
        })
    }

    #[inline]
    pub(super) fn can_access_def(&self, from_module: ModuleId, accessed: DefId) -> bool {
        let def = &self.db[accessed];
        self.can_access(from_module, def.scope.module_id, def.scope.vis)
    }

    #[inline]
    pub(super) fn can_access(&self, from_module: ModuleId, in_module: ModuleId, vis: Vis) -> bool {
        match vis {
            Vis::Export => true,
            Vis::Package => self.db[from_module].package == self.db[in_module].package,
            Vis::Module => from_module == in_module,
        }
    }
}

pub(super) type ItemMap<T> = FxHashMap<ast::GlobalItemId, T>;

/// Various mappings and resolutions from the `define_*` passes
pub(super) struct ResMap {
    pub(super) item_to_def: ItemMap<DefId>,
    pub(super) item_to_adt: ItemMap<AdtId>,
    pub(super) item_to_pat: ItemMap<Pat>,
    pub(super) item_to_ty: ItemMap<Ty>,
    pub(super) item_to_sig: ItemMap<hir::FnSig>,
}

impl ResMap {
    pub(super) fn new() -> Self {
        Self {
            item_to_def: FxHashMap::default(),
            item_to_adt: FxHashMap::default(),
            item_to_pat: FxHashMap::default(),
            item_to_ty: FxHashMap::default(),
            item_to_sig: FxHashMap::default(),
        }
    }
}
