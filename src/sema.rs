mod coerce;
mod env;
mod error;
mod instantiate;
mod normalize;
mod passes;
mod subst;
mod unify;

use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;
use rustc_hash::FxHashMap;
use ustr::{Ustr, UstrMap};

use crate::{
    ast::{self, Ast, AttrKind, BinOp, UnOp},
    common::{Counter, Word},
    db::{Db, DefId, DefInfo, DefKind, ExternLib, FnInfo, ModuleId, ScopeInfo, ScopeLevel, Vis},
    diagnostics::Diagnostic,
    hir,
    hir::{const_eval::Const, ExprId, Hir},
    macros::create_bool_enum,
    sema::{
        coerce::CoerceExt,
        env::{BuiltinTys, Env, GlobalScope, ScopeKind},
        error::ResolveError,
        instantiate::instantiate,
        normalize::NormalizeTy,
        unify::Obligation,
    },
    span::{Span, Spanned},
    sym,
    ty::{FnTy, FnTyParam, InferTy, Instantiation, IntVar, ParamTy, Ty, TyKind, TyVar},
};

pub type ResolveResult<T> = Result<T, ResolveError>;

pub fn resolve(db: &mut Db, ast: &Ast) -> Result<Hir, Diagnostic> {
    Resolver::new(db, ast).run().map_err(|err| err.into_diagnostic(db))
}

pub struct Resolver<'db> {
    db: &'db mut Db,
    ast: &'db Ast,
    hir: Hir,
    global_scope: GlobalScope,
    builtin_tys: BuiltinTys,
    item_statuses: FxHashMap<ast::ItemId, ItemStatus>,
    storage: RefCell<TyStorage>,
    expr_id: Counter<ExprId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ItemStatus {
    Unresolved,
    InProgress,
    Complete,
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

impl<'db> Resolver<'db> {
    fn new(db: &'db mut Db, ast: &'db Ast) -> Self {
        Self {
            builtin_tys: BuiltinTys::new(db),
            global_scope: GlobalScope::new(ast),
            db,
            ast,
            hir: Hir::new(),
            item_statuses: FxHashMap::default(),
            storage: RefCell::new(TyStorage::new()),
            expr_id: Counter::new(),
        }
    }

    fn run(mut self) -> ResolveResult<Hir> {
        for module in &self.ast.modules {
            let mut env = Env::new(module.id.expect("ModuleId to be resolved"));

            for (idx, item) in module.items.iter().enumerate() {
                let item_id = ast::ItemId::from(idx);

                if let ItemStatus::Unresolved = self.item_status(item_id) {
                    self.resolve_global_item(&mut env, item_id, item)?;
                }
            }
        }

        self.subst_hir();

        passes::check_bodies(self.db, &self.hir);
        passes::check_entry(self.db, &self.hir);

        Ok(self.hir)
    }

    #[inline]
    fn item_status(&self, id: ast::ItemId) -> ItemStatus {
        self.item_statuses.get(&id).copied().unwrap_or(ItemStatus::Unresolved)
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

    fn find_and_resolve_global_item(
        &mut self,
        module_id: ModuleId,
        name: Ustr,
    ) -> ResolveResult<Option<DefId>> {
        if let Some(item_id) = self.global_scope.get_item(module_id, name) {
            let item = &self.ast.modules[module_id].items[item_id];
            self.resolve_global_item(&mut Env::new(module_id), item_id, item)?;
            let id = self.global_scope.get_def(module_id, name).expect("global def to be defined");
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
    ) -> ResolveResult<DefId> {
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let qpath = self.db[module_id].name.clone().child(name.name());

        let id = DefInfo::alloc(self.db, qpath, scope, kind, ty, name.span());

        if let Some(prev_id) = self.global_scope.insert_def(module_id, name.name(), id) {
            let def = &self.db[prev_id];
            return Err(ResolveError::MultipleItems {
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
    ) -> ResolveResult<DefId> {
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
    ) -> ResolveResult<hir::Pat> {
        match pat {
            ast::Pat::Name(name) => Ok(hir::Pat::Name(hir::NamePat {
                id: self.define_def(env, vis, kind, name.word, ty)?,
                word: name.word,
            })),
            ast::Pat::Discard(span) => Ok(hir::Pat::Discard(*span)),
        }
    }

    fn lookup_def(&mut self, env: &Env, word: Word) -> ResolveResult<DefId> {
        let module_id = env.module_id();
        let name = word.name();

        if let Some(id) = env.lookup(name).copied() {
            Ok(id)
        } else if let Some(id) = self.global_scope.get_def(module_id, name) {
            Ok(id)
        } else if let Some(id) = self.find_and_resolve_global_item(module_id, name)? {
            Ok(id)
        } else if let Some(id) = self.builtin_tys.get(name) {
            Ok(id)
        } else {
            Err(ResolveError::NameNotFound(word))
        }
    }

    fn resolve_global_item(
        &mut self,
        env: &mut Env,
        item_id: ast::ItemId,
        item: &ast::Item,
    ) -> ResolveResult<()> {
        self.item_statuses.insert(item_id, ItemStatus::InProgress);
        self.resolve_item(env, item)?;
        self.item_statuses.insert(item_id, ItemStatus::Complete);
        Ok(())
    }

    fn resolve_item(&mut self, env: &mut Env, item: &ast::Item) -> ResolveResult<()> {
        match item {
            ast::Item::Fn(fun) => {
                let f = self.resolve_fn(env, fun)?;
                self.hir.fns.push(f);
            }
            ast::Item::Let(let_) => {
                let let_ = self.resolve_let(env, let_)?;
                self.hir.lets.push(let_);
            }
            ast::Item::ExternLet(let_) => {
                let let_ = self.resolve_extern_let(env, let_)?;
                self.hir.extern_lets.push(let_);
            }
        }

        Ok(())
    }

    fn resolve_fn(&mut self, env: &mut Env, fun: &ast::Fn) -> ResolveResult<hir::Fn> {
        let mut sig = env.with_scope(
            fun.sig.word.name(),
            ScopeKind::FnSig,
            |env| -> Result<_, ResolveError> { self.resolve_sig(env, &fun.sig) },
        )?;

        let id = self.define_def(
            env,
            Vis::Private,
            DefKind::Fn(match fun.kind {
                ast::FnKind::Bare { .. } => FnInfo::Bare,
                ast::FnKind::Extern => FnInfo::Extern,
            }),
            fun.sig.word,
            sig.ty,
        )?;

        self.resolve_attrs(
            env,
            &fun.attrs,
            match &fun.kind {
                ast::FnKind::Bare { .. } => AttrsPlacement::Fn,
                ast::FnKind::Extern => AttrsPlacement::ExternFn,
            },
        )?;

        let kind = env.with_scope(
            fun.sig.word.name(),
            ScopeKind::Fn(id),
            |env| -> Result<_, ResolveError> {
                for p in &mut sig.params {
                    p.id = self.define_local_def(env, DefKind::Variable, p.name, p.ty);
                }

                match &fun.kind {
                    ast::FnKind::Bare { body } => {
                        let ret_ty = sig.ty.as_fn().unwrap().ret;

                        let body = self.resolve_expr(env, body, Some(ret_ty))?;

                        let unify_body_res = self
                            .at(Obligation::return_ty(
                                body.span,
                                sig.ret.as_ref().map_or(self.db[id].span, Spanned::span),
                            ))
                            .eq(ret_ty, body.ty)
                            .or_coerce(self, body.id);

                        // If the function's return type is `()`, we want to let the user end the body with
                        // whatever expression they want, so that they don't need to end it with a `()`
                        if !self.normalize(ret_ty).is_unit() {
                            unify_body_res?;
                        }

                        Ok(hir::FnKind::Bare { body })
                    }
                    ast::FnKind::Extern => Ok(hir::FnKind::Extern),
                }
            },
        )?;

        Ok(hir::Fn { module_id: env.module_id(), id, attrs: vec![], sig, kind, span: fun.span })
    }

    fn resolve_sig(&mut self, env: &mut Env, sig: &ast::FnSig) -> ResolveResult<hir::FnSig> {
        let ty_params = self.resolve_ty_params(env, &sig.ty_params)?;

        let mut params = vec![];
        let mut defined_params = UstrMap::<Span>::default();

        for p in &sig.params {
            let ty_annot = self.resolve_ty_expr(env, &p.ty_annot, AllowTyHole::No)?;
            let ty = self.check_ty_expr(&ty_annot)?;

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                return Err(ResolveError::MultipleParams {
                    name: p.name.name(),
                    prev_span,
                    dup_span: p.name.span(),
                });
            }

            params.push(hir::FnParam { id: DefId::INVALID, name: p.name, ty_annot, ty });
        }

        let (ret, ret_ty) = if let Some(ret) = &sig.ret {
            let ret = self.resolve_ty_expr(env, ret, AllowTyHole::No)?;
            let ret_ty = self.check_ty_expr(&ret)?;
            (Some(ret), ret_ty)
        } else {
            (None, self.db.types.unit)
        };

        let ty = Ty::new(TyKind::Fn(FnTy {
            params: params
                .iter()
                .map(|p| FnTyParam { name: Some(p.name.name()), ty: p.ty })
                .collect(),
            ret: ret_ty,
        }));

        Ok(hir::FnSig { ty_params, params, ret, ty })
    }

    fn resolve_let(&mut self, env: &mut Env, let_: &ast::Let) -> ResolveResult<hir::Let> {
        let (ty_annot, ty) = if let Some(ty_annot) = &let_.ty_annot {
            let ty_annot = self.resolve_ty_expr(env, ty_annot, AllowTyHole::Yes)?;
            let ty = self.check_ty_expr(&ty_annot)?;
            (Some(ty_annot), ty)
        } else {
            (None, self.fresh_ty_var())
        };

        let pat = self.define_pat(env, Vis::Private, DefKind::Variable, &let_.pat, ty)?;
        self.resolve_attrs(env, &let_.attrs, AttrsPlacement::Let)?;

        let value = self.resolve_expr(env, &let_.value, Some(ty))?;

        self.at(Obligation::obvious(value.span)).eq(ty, value.ty).or_coerce(self, value.id)?;

        match &pat {
            hir::Pat::Name(name) => {
                self.db[name.id].ty = ty;

                if let Some(value) = self.db.const_storage.expr(value.id) {
                    self.db.const_storage.insert_def(name.id, value.clone());
                }
            }
            hir::Pat::Discard(_) => (),
        }

        Ok(hir::Let {
            module_id: env.module_id(),
            attrs: vec![],
            pat,
            ty_annot,
            value: Box::new(value),
            span: let_.span,
        })
    }

    fn resolve_extern_let(
        &mut self,
        env: &mut Env,
        let_: &ast::ExternLet,
    ) -> ResolveResult<hir::ExternLet> {
        let ty_annot = self.resolve_ty_expr(env, &let_.ty_annot, AllowTyHole::No)?;
        let ty = self.check_ty_expr(&ty_annot)?;
        self.resolve_attrs(env, &let_.attrs, AttrsPlacement::ExternLet)?;
        let id = self.define_def(env, Vis::Private, DefKind::ExternGlobal, let_.word, ty)?;

        Ok(hir::ExternLet {
            module_id: env.module_id(),
            id,
            attrs: vec![],
            word: let_.word,
            ty_annot,
            span: let_.span,
        })
    }

    fn resolve_attrs(
        &mut self,
        env: &mut Env,
        attrs: &ast::Attrs,
        placement: AttrsPlacement,
    ) -> ResolveResult<()> {
        for attr in attrs {
            let (value, value_ty, value_span) = if let Some(value) = &attr.value {
                let value = self.resolve_expr(&mut Env::new(env.module_id()), value, None)?;

                let const_ =
                    self.db.const_storage.expr(value.id).cloned().ok_or(
                        ResolveError::NonConstAttrValue { ty: value.ty, span: value.span },
                    )?;

                (const_, value.ty, value.span)
            } else {
                (Const::Bool(true), self.db.types.bool, attr.span)
            };

            match attr.kind {
                AttrKind::Link => {
                    self.at(Obligation::obvious(value_span)).eq(self.db.types.str, value_ty)?;

                    let lib = {
                        let path = *value.as_str().unwrap();
                        let sources = &self.db.sources.borrow();
                        let relative_to =
                            sources[self.db[env.module_id()].source_id].path().parent().unwrap();

                        ExternLib::try_from_str(&path, relative_to)
                            .ok_or(ResolveError::PathNotFound { path, span: value_span })?
                    };

                    self.db.extern_libs.insert(lib);
                }
            }
        }

        for attr in attrs {
            match (attr.kind, placement) {
                (AttrKind::Link, AttrsPlacement::ExternFn | AttrsPlacement::ExternLet) => (),
                (kind, _) => {
                    return Err(ResolveError::InvalidAttrPlacement { kind, span: attr.span })
                }
            }
        }

        Ok(())
    }

    fn resolve_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        expected_ty: Option<Ty>,
    ) -> ResolveResult<hir::Expr> {
        let expr: ResolveResult<hir::Expr> = match expr {
            ast::Expr::Item(item) => {
                let span = item.span();

                match item {
                    ast::Item::Fn(_) | ast::Item::ExternLet(_) => {
                        self.resolve_item(env, item)?;
                        Ok(self.unit(span))
                    }
                    ast::Item::Let(let_) => {
                        let let_ = self.resolve_let(env, let_)?;
                        Ok(self.expr(hir::ExprKind::Let(let_), self.db.types.unit, span))
                    }
                }
            }
            ast::Expr::Return { expr, span } => {
                if let Some(fn_id) = env.fn_id() {
                    let ret_ty = self.db[fn_id].ty.as_fn().unwrap().ret;

                    let expr = if let Some(expr) = expr {
                        self.resolve_expr(env, expr, Some(ret_ty))?
                    } else {
                        self.unit(*span)
                    };

                    self.at(Obligation::return_ty(expr.span, self.db[fn_id].span))
                        .eq(ret_ty, expr.ty)
                        .or_coerce(self, expr.id)?;

                    Ok(self.expr(
                        hir::ExprKind::Return(hir::Return { expr: Box::new(expr) }),
                        self.db.types.never,
                        *span,
                    ))
                } else {
                    return Err(ResolveError::InvalidReturn(*span));
                }
            }
            ast::Expr::If { cond, then, otherwise, span } => {
                let cond = self.resolve_expr(env, cond, Some(self.db.types.bool))?;

                self.at(Obligation::obvious(cond.span))
                    .eq(self.db.types.bool, cond.ty)
                    .or_coerce(self, cond.id)?;

                let mut then = self.resolve_expr(env, then, expected_ty)?;

                // let otherwise = if let Some(otherwise) = otherwise {
                //     Some(Box::new(self.resolve_expr(env, otherwise)?))
                // } else {
                //     None
                // };

                let otherwise = if let Some(otherwise) = otherwise.as_ref() {
                    let otherwise = self.resolve_expr(env, otherwise, Some(then.ty))?;

                    self.at(Obligation::exprs(*span, then.span, otherwise.span))
                        .eq(then.ty, otherwise.ty)
                        .or_coerce(self, otherwise.id)?;

                    otherwise
                } else {
                    // NOTE: We don't unify here since, since we allow non-unit blocks to
                    // _become_ unit blocks, meaning that a block that doesn't return a unit value,
                    // but is expected to - is assumed to return it anyways.
                    then.ty = self.db.types.unit;
                    self.unit(*span)
                };

                let ty = then.ty;

                Ok(self.expr(
                    hir::ExprKind::If(hir::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        otherwise: Some(Box::new(otherwise)),
                    }),
                    ty,
                    *span,
                ))
            }
            ast::Expr::Block { exprs, span } => env.with_anon_scope(ScopeKind::Block, |env| {
                if exprs.is_empty() {
                    Ok(self.unit(*span))
                } else {
                    let mut new_exprs = vec![];
                    let last = exprs.len() - 1;

                    for (i, expr) in exprs.iter().enumerate() {
                        let expected_ty =
                            if i == last { expected_ty } else { Some(self.db.types.unit) };
                        new_exprs.push(self.resolve_expr(env, expr, expected_ty)?);
                    }

                    let ty = new_exprs.last().unwrap().ty;

                    Ok(self.expr(hir::ExprKind::Block(hir::Block { exprs: new_exprs }), ty, *span))
                }
            }),
            ast::Expr::Call { callee, args, span } => {
                let callee = self.resolve_expr(env, callee, None)?;

                let mut new_args = vec![];

                for arg in args {
                    new_args.push(match arg {
                        ast::CallArg::Named(name, expr) => hir::CallArg {
                            name: Some(*name),
                            expr: self.resolve_expr(env, expr, None)?,
                            index: None,
                        },
                        ast::CallArg::Positional(expr) => hir::CallArg {
                            name: None,
                            expr: self.resolve_expr(env, expr, None)?,
                            index: None,
                        },
                    });
                }

                if let TyKind::Fn(fun_ty) = callee.ty.kind() {
                    #[derive(Debug)]
                    struct PassedArg {
                        is_named: bool,
                        span: Span,
                    }

                    if new_args.len() != fun_ty.params.len() {
                        return Err(ResolveError::ArgMismatch {
                            expected: fun_ty.params.len(),
                            found: new_args.len(),
                            span: *span,
                        });
                    }

                    let mut already_passed_args = UstrMap::<PassedArg>::default();

                    // Resolve positional arg indices
                    for (idx, arg) in new_args.iter_mut().enumerate() {
                        if arg.name.is_none() {
                            arg.index = Some(idx);
                            already_passed_args.insert(
                                fun_ty.params[idx].name.expect("to have a name"),
                                PassedArg { is_named: false, span: arg.expr.span },
                            );
                        }
                    }

                    // Resolve named arg indices
                    for arg in &mut new_args {
                        if let Some(arg_name) = &arg.name {
                            let name = arg_name.name();

                            let idx = fun_ty
                                .params
                                .iter()
                                .enumerate()
                                .find_map(
                                    |(i, p)| if p.name == Some(name) { Some(i) } else { None },
                                )
                                .ok_or(ResolveError::NamedParamNotFound { word: *arg_name })?;

                            // Report named arguments that are passed twice
                            if let Some(passed_arg) = already_passed_args.insert(
                                arg_name.name(),
                                PassedArg { is_named: true, span: arg_name.span() },
                            ) {
                                return Err(ResolveError::MultipleNamedArgs {
                                    name: arg_name.name(),
                                    prev: passed_arg.span,
                                    dup: arg_name.span(),
                                    is_named: passed_arg.is_named,
                                });
                            }

                            arg.index = Some(idx);
                        }
                    }

                    // Unify all args with their corresponding param type
                    for arg in &new_args {
                        let idx = arg.index.expect("arg index to be resolved");
                        self.at(Obligation::obvious(arg.expr.span))
                            .eq(fun_ty.params[idx].ty, arg.expr.ty)
                            .or_coerce(self, arg.expr.id)?;
                    }

                    let ty = fun_ty.ret;

                    Ok(self.expr(
                        hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args: new_args }),
                        ty,
                        *span,
                    ))
                } else {
                    return Err(ResolveError::UncallableTy {
                        ty: self.normalize(callee.ty),
                        span: callee.span,
                    });
                }
            }
            ast::Expr::Unary { expr, op, span } => {
                let expr = self.resolve_expr(env, expr, None)?;

                match op {
                    UnOp::Neg => {
                        // TODO: Only allow signed integers (need traits)
                        self.at(Obligation::obvious(expr.span))
                            .eq(self.fresh_int_var(), expr.ty)
                            .or_coerce(self, expr.id)?;
                    }
                    UnOp::Not => {
                        // TODO: Allow bitnot (integers, need traits)
                        self.at(Obligation::obvious(expr.span))
                            .eq(self.db.types.bool, expr.ty)
                            .or_coerce(self, expr.id)?;
                    }
                }

                let ty = expr.ty;

                Ok(self.expr(
                    hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                    ty,
                    *span,
                ))
            }
            ast::Expr::Binary { lhs, rhs, op, span } => {
                let lhs = self.resolve_expr(env, lhs, None)?;
                let rhs = self.resolve_expr(env, rhs, Some(lhs.ty))?;

                self.at(Obligation::exprs(*span, lhs.span, rhs.span))
                    .eq(lhs.ty, rhs.ty)
                    .or_coerce(self, rhs.id)?;

                match op {
                    BinOp::And | BinOp::Or => {
                        self.at(Obligation::obvious(lhs.span))
                            .eq(self.db.types.bool, lhs.ty)
                            .or_coerce(self, lhs.id)?;

                        self.at(Obligation::obvious(rhs.span))
                            .eq(self.db.types.bool, rhs.ty)
                            .or_coerce(self, rhs.id)?;
                    }
                    _ => {
                        // TODO: type check arithmetic operations (traits)
                        // TODO: type check cmp operations (traits)
                    }
                }

                let ty = match op {
                    BinOp::Cmp(..) => self.db.types.bool,
                    _ => lhs.ty,
                };

                Ok(self.expr(
                    hir::ExprKind::Binary(hir::Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: *op,
                    }),
                    ty,
                    *span,
                ))
            }
            ast::Expr::Cast { expr, ty, span } => {
                let expr = self.resolve_expr(env, expr, None)?;
                let target = self.resolve_ty_expr(env, ty, AllowTyHole::Yes)?;
                let ty = self.check_ty_expr(&target)?;

                Ok(self.expr(
                    hir::ExprKind::Cast(hir::Cast { expr: Box::new(expr), target }),
                    ty,
                    *span,
                ))
            }
            ast::Expr::Member { expr, member, span } => {
                let expr = self.resolve_expr(env, expr, None)?;

                let ty = self.normalize(expr.ty);

                let res_ty = match ty.kind() {
                    TyKind::Str if member.name() == sym::PTR => {
                        Ty::new(TyKind::RawPtr(self.db.types.u8))
                    }
                    TyKind::Str if member.name() == sym::LEN => self.db.types.uint,
                    _ => return Err(ResolveError::InvalidMember { ty, member: *member }),
                };

                Ok(self.expr(
                    hir::ExprKind::Member(hir::Member { expr: Box::new(expr), member: *member }),
                    res_ty,
                    *span,
                ))
            }
            ast::Expr::Name { word, args, span } => {
                let id = self.lookup_def(env, *word)?;

                let def_ty = self.normalize(self.db[id].ty);
                let ty_params = def_ty.collect_params();

                let args = if let Some(args) = args {
                    let mut new_args = vec![];

                    for arg in args {
                        new_args.push(self.resolve_ty_expr(env, arg, AllowTyHole::Yes)?);
                    }

                    Some(new_args)
                } else {
                    None
                };

                let instantiation: Instantiation = match &args {
                    Some(args) if args.len() == ty_params.len() => {
                        let arg_tys: Vec<Ty> =
                            args.iter().map(|arg| self.check_ty_expr(arg)).try_collect()?;

                        ty_params
                            .into_iter()
                            .zip(arg_tys)
                            .map(|(param, arg)| (param.var, arg))
                            .collect()
                    }
                    Some(args) => {
                        return Err(ResolveError::TyArgMismatch {
                            expected: ty_params.len(),
                            found: args.len(),
                            span: *span,
                        });
                    }
                    _ => ty_params
                        .into_iter()
                        .map(|param| (param.var, self.fresh_ty_var()))
                        .collect(),
                };

                let ty = instantiate(def_ty, instantiation.clone());

                Ok(self.expr(hir::ExprKind::Name(hir::Name { id, args, instantiation }), ty, *span))
            }
            ast::Expr::Group { expr, span } => {
                let mut expr = self.resolve_expr(env, expr, expected_ty)?;
                expr.span = *span;
                Ok(expr)
            }
            ast::Expr::Lit { kind, span } => {
                let (kind, ty) = match kind {
                    ast::LitKind::Str(v) => (hir::Lit::Str(*v), self.db.types.str),
                    ast::LitKind::Int(v) => (hir::Lit::Int(*v), self.fresh_int_var()),
                    ast::LitKind::Bool(v) => (hir::Lit::Bool(*v), self.db.types.bool),
                    ast::LitKind::Unit => (hir::Lit::Unit, self.db.types.unit),
                };

                Ok(self.expr(hir::ExprKind::Lit(kind), ty, *span))
            }
        };

        let expr = expr?;

        self.db
            .const_storage
            .eval_expr(&expr)
            .map_err(|e| ResolveError::ConstEval(e, expr.span))?;

        Ok(expr)
    }

    fn resolve_ty_params(
        &mut self,
        env: &mut Env,
        ty_params: &[ast::TyParam],
    ) -> ResolveResult<Vec<hir::TyParam>> {
        let mut new_ty_params = vec![];
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty =
                Ty::new(TyKind::Param(ParamTy { name: tp.name.name(), var: self.fresh_var() }));

            let id = self.define_local_def(env, DefKind::Ty(ty), tp.name, self.db.types.typ);

            if let Some(prev_span) = defined_ty_params.insert(tp.name.name(), tp.name.span()) {
                return Err(ResolveError::MultipleTyParams {
                    name: tp.name.name(),
                    prev_span,
                    dup_span: tp.name.span(),
                });
            }

            new_ty_params.push(hir::TyParam { id, span: tp.name.span() });
        }

        Ok(new_ty_params)
    }

    fn resolve_ty_expr(
        &mut self,
        env: &Env,
        ty: &ast::TyExpr,
        allow_hole: AllowTyHole,
    ) -> ResolveResult<hir::TyExpr> {
        match ty {
            ast::TyExpr::RawPtr(pointee, span) => {
                let pointee = self.resolve_ty_expr(env, pointee, allow_hole)?;
                Ok(hir::TyExpr::RawPtr(Box::new(pointee), *span))
            }
            ast::TyExpr::Name(name) => {
                let id = self.lookup_def(env, name.word)?;

                let args = name
                    .args
                    .iter()
                    .map(|a| self.resolve_ty_expr(env, a, AllowTyHole::Yes))
                    .try_collect()?;

                Ok(hir::TyExpr::Name(hir::TyName { id, args, span: name.span }))
            }
            ast::TyExpr::Hole(span) => {
                if allow_hole.into() {
                    Ok(hir::TyExpr::Hole(*span))
                } else {
                    Err(ResolveError::InvalidInferTy(*span))
                }
            }
            ast::TyExpr::Unit(span) => Ok(hir::TyExpr::Unit(*span)),
        }
    }

    fn expr(&mut self, kind: hir::ExprKind, ty: Ty, span: Span) -> hir::Expr {
        hir::Expr { id: self.expr_id.next(), kind, ty, span }
    }

    fn unit(&mut self, span: Span) -> hir::Expr {
        self.expr(hir::ExprKind::Lit(hir::Lit::Unit), self.db.types.unit, span)
    }

    fn check_ty_expr(&mut self, ty: &hir::TyExpr) -> ResolveResult<Ty> {
        match ty {
            hir::TyExpr::RawPtr(pointee, _) => {
                Ok(Ty::new(TyKind::RawPtr(self.check_ty_expr(pointee)?)))
            }
            hir::TyExpr::Name(name) => {
                let def = &self.db[name.id];

                match def.kind.as_ref() {
                    DefKind::Ty(ty) => Ok(*ty),
                    _ => Err(ResolveError::ExpectedTy { ty: def.ty, span: name.span }),
                }
            }
            hir::TyExpr::Unit(_) => Ok(self.db.types.unit),
            hir::TyExpr::Hole(_) => Ok(self.fresh_ty_var()),
        }
    }
}

create_bool_enum!(AllowTyHole);

#[derive(Debug, Clone, Copy)]
enum AttrsPlacement {
    Fn,
    ExternFn,
    Let,
    ExternLet,
}
