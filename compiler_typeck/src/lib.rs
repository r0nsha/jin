mod attrs;
mod builtins;
mod define;
mod errors;
mod exprs;
mod fns;
mod hooks;
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
use std::rc::Rc;

use compiler_ast::{self as ast, Ast};
use compiler_core::{
    counter::Counter,
    db::{AdtId, Db, DefId, ModuleId},
    diagnostics::DiagnosticResult,
    hir::{self, Hir},
    middle::{Pat, TyExpr, TyParam, Vis},
    span::Span,
    ty::{FloatVar, InferTy, Instantiation, IntVar, Ty, TyKind, TyVar},
};
use ena::unify::{InPlace, InPlaceUnificationTable, Snapshot};
use rustc_hash::FxHashMap;

use crate::ns::GlobalEnv;

pub fn typeck(db: &mut Db, ast: Ast) -> Hir {
    let mut cx = Typeck::new(db);
    typeck_inner(&mut cx, ast);
    cx.hir
}

fn typeck_inner(cx: &mut Typeck, ast: Ast) {
    cx.init_global_env(&ast);

    // Define
    items::define(cx, &ast);
    let imported_fns = imports::define(cx, &ast);
    imports::insert_prelude(cx);
    imports::define_transitive_globs(cx);

    if cx.db.diagnostics.any_errors() {
        return;
    }

    // Check top-level types and signatures
    types::check(cx, &ast);
    items::check_sigs(cx, &ast);
    imports::fill_imported_fn_candidates(cx, imported_fns);

    if cx.db.diagnostics.any_errors() {
        return;
    }

    // Check bodies & expressions
    items::check_bodies(cx, &ast);

    if cx.db.diagnostics.any_errors() {
        return;
    }

    // Finalization steps & checks
    subst::subst(cx);
    late::checks(cx.db, &mut cx.hir);
}

pub(crate) struct Typeck<'db> {
    pub(crate) db: &'db mut Db,

    /// The Hir being constructed
    pub(crate) hir: Hir,

    /// The global namespace, mapped by module
    pub(crate) global_env: GlobalEnv,

    /// Mappings used for name and item resolution
    pub(crate) res_map: ResMap,

    /// Stores type unification tables
    pub(crate) storage: RefCell<TyStorage>,

    /// A mapping from definitions to their resolved type
    pub(crate) ty_aliases: FxHashMap<DefId, TyAlias>,

    /// A mapping from definitions to their resolved type
    pub(crate) def_to_ty: FxHashMap<DefId, Ty>,

    /// Counter for generating hir::ExprId's
    pub(crate) expr_id: Counter<hir::ExprId>,
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

impl Default for TyStorage {
    fn default() -> Self {
        Self::new()
    }
}

pub struct TyStorageSnapshot {
    ty: Snapshot<InPlace<TyVar>>,
    int: Snapshot<InPlace<IntVar>>,
    float: Snapshot<InPlace<FloatVar>>,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self {
            db,
            hir: Hir::new(),
            global_env: GlobalEnv::new(),
            res_map: ResMap::new(),
            storage: RefCell::new(TyStorage::new()),
            ty_aliases: FxHashMap::default(),
            def_to_ty: FxHashMap::default(),
            expr_id: Counter::new(),
        }
    }

    fn init_global_env(&mut self, ast: &Ast) {
        for module in &ast.modules {
            self.global_env.insert_module(module.id);
        }

        builtins::define_all(self);
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
    pub(crate) fn check_access_def(
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
    pub(crate) fn can_access_def(&self, from_module: ModuleId, accessed: DefId) -> bool {
        let def = &self.db[accessed];
        self.can_access(from_module, def.scope.module_id, def.scope.vis)
    }

    #[inline]
    pub(crate) fn can_access(&self, from_module: ModuleId, in_module: ModuleId, vis: Vis) -> bool {
        match vis {
            Vis::Public => true,
            Vis::Private => {
                from_module == in_module || self.db[from_module].is_submodule(self.db, in_module)
            }
        }
    }
}

pub(crate) type ItemMap<T> = FxHashMap<ast::GlobalItemId, T>;

/// Various mappings and resolutions from the `define_*` passes
pub(crate) struct ResMap {
    pub(crate) item_to_def: ItemMap<DefId>,
    pub(crate) item_to_adt: ItemMap<AdtId>,
    pub(crate) item_to_pat: ItemMap<Pat>,
    pub(crate) item_to_ty: ItemMap<Ty>,
    pub(crate) item_to_sig: ItemMap<hir::FnSig>,
}

impl ResMap {
    pub(crate) fn new() -> Self {
        Self {
            item_to_def: FxHashMap::default(),
            item_to_adt: FxHashMap::default(),
            item_to_pat: FxHashMap::default(),
            item_to_ty: FxHashMap::default(),
            item_to_sig: FxHashMap::default(),
        }
    }
}

pub(crate) struct TyAlias {
    pub(crate) tyexpr: Option<Rc<TyExpr>>,
    pub(crate) ty: Option<Ty>,
    pub(crate) tparams: Vec<TyParam>,
}

impl TyAlias {
    pub fn instantiation(&self, targs: &[Ty]) -> Instantiation {
        Instantiation::from((self.tparams.as_slice(), targs))
    }
}

pub(crate) fn trans_let_kind(kind: &ast::LetKind) -> hir::LetKind {
    match kind {
        ast::LetKind::Let => hir::LetKind::Let,
        ast::LetKind::Const => hir::LetKind::Const,
    }
}
