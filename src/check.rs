mod analysis;
mod attrs;
mod coerce;
mod env;
mod error;
mod instantiate;
mod normalize;
mod subst;
mod unify;

use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;
use enum_as_inner::EnumAsInner;
use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    ast::{self, Ast},
    check::{
        env::{BuiltinTys, Env, GlobalScope, ScopeKind},
        error::CheckError,
        normalize::NormalizeTy,
    },
    common::{Counter, Word},
    db::{Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, Vis},
    diagnostics::Diagnostic,
    hir,
    hir::{ExprId, Hir},
    macros::create_bool_enum,
    span::{Span, Spanned},
    ty::{InferTy, Instantiation, IntVar, Ty, TyKind, TyVar},
};

pub type CheckResult<T> = Result<T, CheckError>;

pub fn check(db: &mut Db, ast: &Ast) -> Result<Hir, Diagnostic> {
    CheckCx::new(db, ast).run().map_err(|err| err.into_diagnostic(db))
}

pub struct CheckCx<'db> {
    db: &'db mut Db,
    ast: &'db Ast,
    hir: Hir,
    storage: RefCell<TyStorage>,
    global_scope: GlobalScope,
    builtin_tys: BuiltinTys,
    item_statuses: FxHashMap<ast::ItemId, ItemStatus>,
    expr_id: Counter<ExprId>,
}

#[derive(Debug)]
pub struct TyStorage {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
}

impl TyStorage {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumAsInner)]
pub enum ItemStatus {
    Unresolved,
    InProgress,
    Complete,
}

impl<'db> CheckCx<'db> {
    fn new(db: &'db mut Db, ast: &'db Ast) -> Self {
        Self {
            builtin_tys: BuiltinTys::new(db),
            global_scope: GlobalScope::new(ast),
            db,
            ast,
            hir: Hir::new(),
            storage: RefCell::new(TyStorage::new()),
            item_statuses: FxHashMap::default(),
            expr_id: Counter::new(),
        }
    }

    fn run(mut self) -> CheckResult<Hir> {
        for module in &self.ast.modules {
            for (item_id, item) in module.items.iter().enumerate() {
                if self.item_status(item_id.into()).is_unresolved() {
                    let mut env = Env::new(module.id.expect("ModuleId to be resolved"));
                    self.check_item(&mut env, item)?;
                }
            }
        }

        self.subst(hir);

        Ok(self.hir)
    }

    #[inline]
    pub fn item_status(&self, id: ast::ItemId) -> ItemStatus {
        self.item_statuses.get(&id).copied().unwrap_or(ItemStatus::Unresolved)
    }

    #[inline]
    pub fn set_item_status(&self, id: ast::ItemId, status: ItemStatus) {
        self.item_statuses.insert(id, status);
    }

    #[inline]
    pub fn fresh_ty_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::TyVar(self.fresh_var())))
    }

    #[inline]
    pub fn fresh_var(&self) -> TyVar {
        self.storage.borrow_mut().ty_unification_table.new_key(None)
    }

    #[inline]
    pub fn fresh_int_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::IntVar(
            self.storage.borrow_mut().int_unification_table.new_key(None),
        )))
    }

    #[inline]
    pub fn normalize(&self, ty: Ty) -> Ty {
        ty.normalize(&mut self.storage.borrow_mut())
    }

    fn lookup_and_check_global_item(
        &mut self,
        module_id: ModuleId,
        name: Ustr,
    ) -> CheckResult<Option<DefId>> {
        if let Some(item_id) = self.global_scope.get_item(module_id, name) {
            let item = &self.ast.modules[module_id].items[item_id];

            self.set_item_status(item_id, ItemStatus::InProgress);

            let mut env = Env::new(module_id);
            self.check_item(&mut env, item)?;

            let id = self.global_scope.get_def(module_id, name).expect("global item to be defined");

            self.set_item_status(item_id, ItemStatus::Complete);

            Ok(Some(id))
        } else {
            Ok(None)
        }
    }

    fn define_global_def(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
        ty: Ty,
    ) -> CheckResult<DefId> {
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let qpath = self.db[module_id].name.clone().child(name.name());

        let id = DefInfo::alloc(self.db, qpath, scope, kind, ty, name.span());

        if let Some(prev_id) = self.global_scope.insert_def(module_id, name.name(), id) {
            let def = &self.db[prev_id];

            return Err(CheckError::MultipleItems {
                name: def.qpath.name(),
                prev_span: def.span,
                dup_span: name.span(),
            });
        }

        Ok(id)
    }

    fn define_local_def(&mut self, env: &mut Env, kind: DefKind, name: Word, ty: Ty) -> DefId {
        let id = DefInfo::alloc(
            self.db,
            env.scope_path(self.db).child(name.name()),
            ScopeInfo { module_id: env.module_id(), level: env.scope_level(), vis: Vis::Private },
            kind,
            ty,
            name.span(),
        );

        env.current_mut().insert(name.name(), id);

        id
    }

    fn define_def(
        &mut self,
        env: &mut Env,
        vis: Vis,
        kind: DefKind,
        name: Word,
        ty: Ty,
    ) -> CheckResult<DefId> {
        if env.in_global_scope() {
            self.define_global_def(env.module_id(), vis, kind, name, ty)
        } else {
            Ok(self.define_local_def(env, kind, name, ty))
        }
    }

    fn define_pat(
        &mut self,
        env: &mut Env,
        vis: Vis,
        kind: DefKind,
        pat: &ast::Pat,
        ty: Ty,
    ) -> CheckResult<hir::Pat> {
        match pat {
            ast::Pat::Name(name) => Ok(hir::Pat::Name(hir::NamePat {
                id: self.define_def(env, vis, kind, name.word, ty)?,
                word: name.word,
            })),
            ast::Pat::Discard(span) => Ok(hir::Pat::Discard(*span)),
        }
    }

    fn lookup_def(&mut self, env: &Env, word: Word) -> CheckResult<DefId> {
        let module_id = env.module_id();
        let name = word.name();

        if let Some(id) = env.lookup(name).copied() {
            Ok(id)
        } else if let Some(id) = self.global_scope.get_def(module_id, name) {
            Ok(id)
        } else if let Some(id) = self.lookup_and_check_global_item(module_id, name)? {
            Ok(id)
        } else if let Some(id) = self.builtin_tys.get(name) {
            Ok(id)
        } else {
            Err(CheckError::NameNotFound(word))
        }
    }

    fn check_item(&mut self, env: &mut Env, item: &ast::Item) -> CheckResult<()> {
        match item {
            ast::Item::Fn(fun) => {
                let f = self.check_fn(env, fun)?;
                self.hir.fns.push(f);
            }
            ast::Item::Let(let_) => {
                let let_ = self.check_let(env, let_)?;
                self.hir.lets.push(let_);
            }
            ast::Item::ExternLet(let_) => {
                let let_ = self.check_extern_let(env, let_)?;
                self.hir.extern_lets.push(let_);
            }
        }

        Ok(())
    }

    fn check_fn(&mut self, env: &mut Env, fun: &ast::Fn) -> CheckResult<hir::Fn> {
        let sig = env
            .with_scope(fun.sig.word.name(), ScopeKind::Fn, |env| self.check_sig(env, &fun.sig))?;

        let id = self.define_def(env, Vis::Private, DefKind::Fn(FnInfo::Bare), fun.sig.word)?;

        let attrs = self.check_attrs(env, &fun.attrs)?;

        let (sig, kind) = env.with_scope(fun.sig.word.name(), ScopeKind::Fn, |env| {
            let sig = self.check_sig(env, &fun.sig)?;

            let kind = match &fun.kind {
                ast::FnKind::Bare { body } => {
                    hir::FnKind::Bare { body: self.check_expr(env, body)? }
                }
                ast::FnKind::Extern => hir::FnKind::Extern,
            };

            Ok((sig, kind))
        })?;

        Ok(hir::Fn { module_id: env.module_id(), id, attrs, sig, kind, span: fun.span })
    }

    fn check_sig(&mut self, env: &mut Env, sig: &ast::FnSig) -> CheckResult<hir::FnSig> {
        assert!(env.in_kind(ScopeKind::Fn), "FnSig must be resolved inside a ScopeKind::Fn");

        let ty_params = self.check_ty_params(env, &sig.ty_params)?;

        let mut params = vec![];
        let mut defined_params = UstrMap::<Span>::default();

        for p in &sig.params {
            let id = self.define_local_def(env, DefKind::Variable, p.name);

            let ty_annot = self.check_ty_expr(env, &p.ty_annot, AllowTyHole::No)?;

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                return Err(CheckError::MultipleParams {
                    name: p.name.name(),
                    prev_span,
                    dup_span: p.name.span(),
                });
            }

            params.push(hir::FnParam { id, ty_annot, span: p.span, ty: self.db.types.unknown });
        }

        let ret = sig
            .ret
            .as_ref()
            .map(|ret| self.check_ty_expr(env, ret, AllowTyHole::No))
            .transpose()?;

        Ok(hir::FnSig { ty_params, params, ret })
    }

    fn check_let(&mut self, env: &mut Env, let_: &ast::Let) -> CheckResult<hir::Let> {
        let pat = self.define_pat(env, Vis::Private, DefKind::Variable, &let_.pat)?;

        env.with_anon_scope(ScopeKind::Initializer, |env| {
            if let Some(ty) = &let_.ty_annot {
                self.check_ty_expr(env, ty, AllowTyHole::Yes)?;
            }

            self.check_expr(env, &let_.value)
        })?;

        let attrs = self.check_attrs(env, &let_.attrs)?;

        let ty_annot = let_
            .ty_annot
            .as_ref()
            .map(|t| self.check_ty_expr(env, t, AllowTyHole::Yes))
            .transpose()?;

        let value = self.check_expr(env, &let_.value)?;

        Ok(hir::Let {
            module_id: env.module_id(),
            attrs,
            pat,
            ty_annot,
            value: Box::new(value),
            span: let_.span,
        })
    }

    fn check_extern_let(
        &mut self,
        env: &mut Env,
        let_: &ast::ExternLet,
    ) -> CheckResult<hir::ExternLet> {
        let id = self.define_def(env, Vis::Private, DefKind::ExternGlobal, let_.word)?;

        let attrs = self.check_attrs(env, &let_.attrs)?;
        let ty_annot = self.check_ty_expr(env, &let_.ty_annot, AllowTyHole::No)?;

        Ok(hir::ExternLet {
            module_id: env.module_id(),
            id,
            attrs,
            word: let_.word,
            ty_annot,
            span: let_.span,
        })
    }

    fn check_attrs(&mut self, env: &mut Env, attrs: &ast::Attrs) -> CheckResult<hir::Attrs> {
        attrs
            .iter()
            .map(|attr| {
                Ok(hir::Attr {
                    kind: attr.kind,
                    value: attr.value.as_ref().map(|v| self.check_expr(env, v)).transpose()?,
                    span: attr.span,
                })
            })
            .try_collect()
    }

    fn check_expr(&mut self, env: &mut Env, expr: &ast::Expr) -> CheckResult<hir::Expr> {
        match expr {
            ast::Expr::Item(item) => {
                let span = item.span();

                match item {
                    ast::Item::Let(let_) => {
                        let let_ = self.check_let(env, let_)?;
                        Ok(self.expr(hir::ExprKind::Let(let_), span))
                    }
                    ast::Item::Fn(_) | ast::Item::ExternLet(_) => {
                        self.check_item(env, item)?;
                        Ok(self.unit(span))
                    }
                }
            }
            ast::Expr::Return { expr, span } => {
                let expr = if let Some(expr) = expr {
                    self.check_expr(env, expr)?
                } else {
                    self.unit(*span)
                };

                Ok(self.expr(hir::ExprKind::Return(hir::Return { expr: Box::new(expr) }), *span))
            }
            ast::Expr::If { cond, then, otherwise, span } => {
                let cond = self.check_expr(env, cond)?;
                let then = self.check_expr(env, then)?;

                let otherwise = if let Some(otherwise) = otherwise {
                    Some(Box::new(self.check_expr(env, otherwise)?))
                } else {
                    None
                };

                Ok(self.expr(
                    hir::ExprKind::If(hir::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        otherwise,
                    }),
                    *span,
                ))
            }
            ast::Expr::Block { exprs, span } => env.with_anon_scope(ScopeKind::Block, |env| {
                let exprs = exprs.iter().map(|e| self.check_expr(env, e)).try_collect()?;
                Ok(self.expr(hir::ExprKind::Block(hir::Block { exprs }), *span))
            }),
            ast::Expr::Call { callee, args, span } => {
                let callee = self.check_expr(env, callee)?;

                let mut new_args = vec![];

                for arg in args {
                    new_args.push(match arg {
                        ast::CallArg::Named(name, expr) => hir::CallArg {
                            name: Some(*name),
                            expr: self.check_expr(env, expr)?,
                            index: None,
                        },
                        ast::CallArg::Positional(expr) => hir::CallArg {
                            name: None,
                            expr: self.check_expr(env, expr)?,
                            index: None,
                        },
                    });
                }

                Ok(self.expr(
                    hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args: new_args }),
                    *span,
                ))
            }
            ast::Expr::Unary { expr, op, span } => {
                let expr = self.check_expr(env, expr)?;

                Ok(self.expr(
                    hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                    *span,
                ))
            }
            ast::Expr::Member { expr, member, span } => {
                let expr = self.check_expr(env, expr)?;

                Ok(self.expr(
                    hir::ExprKind::Member(hir::Member { expr: Box::new(expr), member: *member }),
                    *span,
                ))
            }
            ast::Expr::Binary { lhs, rhs, op, span } => {
                let lhs = self.check_expr(env, lhs)?;
                let rhs = self.check_expr(env, rhs)?;

                Ok(self.expr(
                    hir::ExprKind::Binary(hir::Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: *op,
                    }),
                    *span,
                ))
            }
            ast::Expr::Cast { expr, ty, span } => {
                let expr = self.check_expr(env, expr)?;
                let target = self.check_ty_expr(env, ty, AllowTyHole::Yes)?;

                Ok(self
                    .expr(hir::ExprKind::Cast(hir::Cast { expr: Box::new(expr), target }), *span))
            }
            ast::Expr::Name { word, args, span } => {
                let id = self.lookup_def(env, *word)?;

                let args = if let Some(args) = args {
                    let mut new_args = vec![];

                    for arg in args {
                        new_args.push(self.check_ty_expr(env, arg, AllowTyHole::Yes)?);
                    }

                    Some(new_args)
                } else {
                    None
                };

                Ok(self.expr(
                    hir::ExprKind::Name(hir::Name {
                        id,
                        args,
                        instantiation: Instantiation::default(),
                    }),
                    *span,
                ))
            }
            ast::Expr::Group { expr, span } => {
                let mut expr = self.check_expr(env, expr)?;
                expr.span = *span;
                Ok(expr)
            }
            ast::Expr::Lit { kind, span } => Ok(self.expr(
                hir::ExprKind::Lit(match kind {
                    ast::LitKind::Str(v) => hir::Lit::Str(*v),
                    ast::LitKind::Int(v) => hir::Lit::Int(*v),
                    ast::LitKind::Bool(v) => hir::Lit::Bool(*v),
                    ast::LitKind::Unit => hir::Lit::Unit,
                }),
                *span,
            )),
        }
    }

    fn check_ty_params(
        &mut self,
        env: &mut Env,
        ty_params: &[ast::TyParam],
    ) -> CheckResult<Vec<hir::TyParam>> {
        let mut new_ty_params = vec![];
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty = self.db.types.unknown;
            let id = self.define_local_def(env, DefKind::Ty(ty), tp.name);

            if let Some(prev_span) = defined_ty_params.insert(tp.name.name(), tp.name.span()) {
                return Err(CheckError::MultipleTyParams {
                    name: tp.name.name(),
                    prev_span,
                    dup_span: tp.name.span(),
                });
            }

            new_ty_params.push(hir::TyParam { id, span: tp.name.span() });
        }

        Ok(new_ty_params)
    }

    fn check_ty_expr(
        &mut self,
        env: &Env,
        ty: &ast::TyExpr,
        allow_hole: AllowTyHole,
    ) -> CheckResult<hir::TyExpr> {
        match ty {
            ast::TyExpr::RawPtr(pointee, span) => {
                let pointee = self.check_ty_expr(env, pointee, allow_hole)?;
                Ok(hir::TyExpr::RawPtr(Box::new(pointee), *span))
            }
            ast::TyExpr::Name(name) => {
                let id = self.lookup_def(env, name.word)?;

                let args = name
                    .args
                    .iter()
                    .map(|a| self.check_ty_expr(env, a, AllowTyHole::Yes))
                    .try_collect()?;

                Ok(hir::TyExpr::Name(hir::TyName { id, args, span: name.span }))
            }
            ast::TyExpr::Hole(span) => {
                if allow_hole.into() {
                    Ok(hir::TyExpr::Hole(*span))
                } else {
                    Err(CheckError::InvalidInferTy(*span))
                }
            }
            ast::TyExpr::Unit(span) => Ok(hir::TyExpr::Unit(*span)),
        }
    }

    fn expr(&mut self, kind: hir::ExprKind, span: Span) -> hir::Expr {
        hir::Expr { id: self.expr_id.next(), kind, span, ty: self.db.types.unknown }
    }

    fn unit(&mut self, span: Span) -> hir::Expr {
        self.expr(hir::ExprKind::Lit(hir::Lit::Unit), span)
    }
}

enum ItemResult {
    Let(hir::Let),
    Unit(DefId),
}

create_bool_enum!(AllowTyHole);
