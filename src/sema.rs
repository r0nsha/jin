mod attrs;
mod coerce;
mod env;
mod error;
mod instantiate;
mod item_state;
mod normalize;
mod post;
mod subst;
mod unify;

use std::cell::RefCell;

use ena::unify::InPlaceUnificationTable;
use ustr::UstrMap;

use crate::{
    ast::{self, Ast},
    counter::Counter,
    db::{Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, Vis},
    diagnostics::Diagnostic,
    hir,
    hir::{ExprId, Hir},
    macros::create_bool_enum,
    middle::{BinOp, TyExpr, UnOp},
    sema::{
        attrs::AttrsPlacement,
        coerce::CoerceExt,
        env::{BuiltinTys, Env, GlobalScope, ScopeKind, Symbol},
        error::CheckError,
        instantiate::instantiate,
        item_state::{ItemState, ItemStatus},
        normalize::NormalizeTy,
        unify::Obligation,
    },
    span::{Span, Spanned},
    sym,
    ty::{FnTy, FnTyParam, InferTy, Instantiation, IntVar, ParamTy, Ty, TyKind, TyVar},
    word::Word,
};

pub type CheckResult<T> = Result<T, CheckError>;

pub fn check(db: &mut Db, ast: &Ast) -> Result<Hir, Diagnostic> {
    Sema::new(db, ast).run().map_err(|err| err.into_diagnostic(db))
}

pub struct Sema<'db> {
    db: &'db mut Db,
    ast: &'db Ast,
    hir: Hir,
    global_scope: GlobalScope,
    builtin_tys: BuiltinTys,
    item_state: ItemState,
    storage: RefCell<TyStorage>,
    expr_id: Counter<ExprId>,
    checking_global_items: bool,
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

impl<'db> Sema<'db> {
    fn new(db: &'db mut Db, ast: &'db Ast) -> Self {
        Self {
            builtin_tys: BuiltinTys::new(db),
            global_scope: GlobalScope::new(ast),
            db,
            ast,
            hir: Hir::new(),
            item_state: ItemState::new(),
            storage: RefCell::new(TyStorage::new()),
            expr_id: Counter::new(),
            checking_global_items: true,
        }
    }

    fn run(mut self) -> CheckResult<Hir> {
        self.check_items()?;
        self.checking_global_items = false;

        // for module in &self.ast.modules {
        //     let mut env = Env::new(module.id);
        //
        //     for (item_id, item) in module.items.iter_enumerated() {
        //         let global_item_id = ast::GlobalItemId::new(module.id, item_id);
        //
        //         if let ItemStatus::Unresolved = self.item_state.get_status(&global_item_id) {
        //             self.check_global_item(&mut env, global_item_id, item)?;
        //         }
        //     }
        // }

        self.subst();

        post::check_bodies(self.db, &self.hir);
        post::check_entry(self.db, &self.hir);

        Ok(self.hir)
    }

    fn check_items(&mut self) -> CheckResult<()> {
        for module in &self.ast.modules {
            let mut env = Env::new(module.id);

            for (item_id, item) in module.items.iter_enumerated() {
                let global_item_id = ast::GlobalItemId::new(module.id, item_id);

                if let ItemStatus::Unresolved = self.item_state.get_status(&global_item_id) {
                    self.check_global_item(&mut env, global_item_id, item)?;
                }
            }
        }

        Ok(())
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

        if let Some(prev_id) = self.global_scope.insert_def(Symbol::new(module_id, name.name()), id)
        {
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
        value_id: ExprId,
    ) -> CheckResult<hir::Pat> {
        match pat {
            ast::Pat::Name(name) => {
                let id = self.define_def(env, vis, kind, name.word, ty)?;

                if let Some(value) = self.db.const_storage.expr(value_id) {
                    self.db.const_storage.insert_def(id, value.clone());
                }

                Ok(hir::Pat::Name(hir::NamePat { id, word: name.word }))
            }
            ast::Pat::Discard(span) => Ok(hir::Pat::Discard(*span)),
        }
    }

    fn lookup_def(&mut self, env: &Env, word: Word) -> CheckResult<DefId> {
        let name = word.name();

        if let Some(id) = env.lookup(name).copied() {
            return Ok(id);
        }

        let symbol = Symbol::new(env.module_id(), name);

        if let Some(id) = self.global_scope.get_def(&symbol) {
            return Ok(id);
        }

        if self.checking_global_items {
            if let Some(id) = self.find_and_check_global_item(&symbol)? {
                return Ok(id);
            }
        }

        if let Some(id) = self.builtin_tys.get(name) {
            return Ok(id);
        }

        Err(CheckError::NameNotFound(word))
    }

    fn find_and_check_global_item(&mut self, symbol: &Symbol) -> CheckResult<Option<DefId>> {
        if let Some(item_id) = self.global_scope.get_item(symbol) {
            let item = &self.ast.modules[symbol.module_id].items[item_id];
            self.check_global_item(
                &mut Env::new(symbol.module_id),
                ast::GlobalItemId::new(symbol.module_id, item_id),
                item,
            )?;
            let id = self.global_scope.get_def(symbol).expect("global def to be defined");
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }

    fn check_global_item(
        &mut self,
        env: &mut Env,
        item_id: ast::GlobalItemId,
        item: &ast::Item,
    ) -> CheckResult<()> {
        self.item_state.mark_as_in_progress(item_id).map_err(|err| CheckError::CyclicItems {
            origin_span: item.span(),
            reference_span: self.ast.find_item(err.causee).expect("item to exist").span(),
        })?;

        self.check_item(env, item)?;
        self.item_state.mark_as_resolved(item_id);

        Ok(())
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
        self.check_attrs(
            env.module_id(),
            &fun.attrs,
            match &fun.kind {
                ast::FnKind::Bare { .. } => AttrsPlacement::Fn,
                ast::FnKind::Extern => AttrsPlacement::ExternFn,
            },
        )?;

        let mut sig = env.with_scope(
            fun.sig.word.name(),
            ScopeKind::FnSig,
            |env| -> Result<_, CheckError> { self.check_fn_sig(env, &fun.sig) },
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

        let kind = env.with_scope(
            fun.sig.word.name(),
            ScopeKind::Fn(id),
            |env| -> Result<_, CheckError> {
                for p in &mut sig.params {
                    p.id = self.define_local_def(env, DefKind::Variable, p.name, p.ty);
                }

                match &fun.kind {
                    ast::FnKind::Bare { body } => {
                        let ret_ty = sig.ty.as_fn().unwrap().ret;

                        let body = self.check_expr(env, body, Some(ret_ty))?;

                        let unify_body_res = self
                            .at(Obligation::return_ty(
                                body.span,
                                fun.sig.ret.as_ref().map_or(self.db[id].span, Spanned::span),
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

        Ok(hir::Fn { module_id: env.module_id(), id, sig, kind, span: fun.span })
    }

    fn check_fn_sig(&mut self, env: &mut Env, sig: &ast::FnSig) -> CheckResult<hir::FnSig> {
        let ty_params = self.check_ty_params(env, &sig.ty_params)?;

        let mut params = vec![];
        let mut defined_params = UstrMap::<Span>::default();

        for p in &sig.params {
            let ty = self.check_ty_expr(env, &p.ty_expr, AllowTyHole::No)?;

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                return Err(CheckError::MultipleParams {
                    name: p.name.name(),
                    prev_span,
                    dup_span: p.name.span(),
                });
            }

            params.push(hir::FnParam { id: DefId::INVALID, name: p.name, ty });
        }

        let ret = if let Some(ret) = &sig.ret {
            self.check_ty_expr(env, ret, AllowTyHole::No)?
        } else {
            self.db.types.unit
        };

        let ty = Ty::new(TyKind::Fn(FnTy {
            params: params
                .iter()
                .map(|p| FnTyParam { name: Some(p.name.name()), ty: p.ty })
                .collect(),
            ret,
        }));

        Ok(hir::FnSig { word: sig.word, ty_params, params, ret, ty })
    }

    fn check_let(&mut self, env: &mut Env, let_: &ast::Let) -> CheckResult<hir::Let> {
        self.check_attrs(env.module_id(), &let_.attrs, AttrsPlacement::Let)?;

        let ty = if let Some(ty_expr) = &let_.ty_expr {
            self.check_ty_expr(env, ty_expr, AllowTyHole::Yes)?
        } else {
            self.fresh_ty_var()
        };

        let value = self.check_expr(env, &let_.value, Some(ty))?;

        if env.in_global_scope() && self.db.const_storage.expr(value.id).is_none() {
            return Err(CheckError::NonConstGlobalLet { span: value.span });
        }

        self.at(Obligation::obvious(value.span)).eq(ty, value.ty).or_coerce(self, value.id)?;

        let pat = self.define_pat(env, Vis::Private, DefKind::Variable, &let_.pat, ty, value.id)?;

        Ok(hir::Let { module_id: env.module_id(), pat, value: Box::new(value), span: let_.span })
    }

    fn check_extern_let(
        &mut self,
        env: &mut Env,
        let_: &ast::ExternLet,
    ) -> CheckResult<hir::ExternLet> {
        self.check_attrs(env.module_id(), &let_.attrs, AttrsPlacement::ExternLet)?;

        let ty = self.check_ty_expr(env, &let_.ty_expr, AllowTyHole::No)?;
        let id = self.define_def(env, Vis::Private, DefKind::ExternGlobal, let_.word, ty)?;

        Ok(hir::ExternLet { module_id: env.module_id(), id, word: let_.word, span: let_.span })
    }

    fn check_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        expected_ty: Option<Ty>,
    ) -> CheckResult<hir::Expr> {
        let expr = match expr {
            ast::Expr::Item(item) => {
                let span = item.span();

                match item {
                    ast::Item::Fn(_) | ast::Item::ExternLet(_) => {
                        self.check_item(env, item)?;
                        self.unit_expr(span)
                    }
                    ast::Item::Let(let_) => {
                        let let_ = self.check_let(env, let_)?;
                        self.expr(hir::ExprKind::Let(let_), self.db.types.unit, span)
                    }
                }
            }
            ast::Expr::Return { expr, span } => {
                if let Some(fn_id) = env.fn_id() {
                    let ret_ty = self.db[fn_id].ty.as_fn().unwrap().ret;

                    let expr = if let Some(expr) = expr {
                        self.check_expr(env, expr, Some(ret_ty))?
                    } else {
                        self.unit_expr(*span)
                    };

                    self.at(Obligation::return_ty(expr.span, self.db[fn_id].span))
                        .eq(ret_ty, expr.ty)
                        .or_coerce(self, expr.id)?;

                    self.expr(
                        hir::ExprKind::Return(hir::Return { expr: Box::new(expr) }),
                        self.db.types.never,
                        *span,
                    )
                } else {
                    return Err(CheckError::InvalidReturn(*span));
                }
            }
            ast::Expr::If { cond, then, otherwise, span } => {
                let cond = self.check_expr(env, cond, Some(self.db.types.bool))?;

                self.at(Obligation::obvious(cond.span))
                    .eq(self.db.types.bool, cond.ty)
                    .or_coerce(self, cond.id)?;

                let mut then = self.check_expr(env, then, expected_ty)?;

                let otherwise = if let Some(otherwise) = otherwise.as_ref() {
                    let otherwise = self.check_expr(env, otherwise, Some(then.ty))?;

                    self.at(Obligation::exprs(*span, then.span, otherwise.span))
                        .eq(then.ty, otherwise.ty)
                        .or_coerce(self, otherwise.id)?;

                    otherwise
                } else {
                    // NOTE: We don't unify here since, since we allow non-unit blocks to
                    // _become_ unit blocks, meaning that a block that doesn't return a unit value,
                    // but is expected to - is assumed to return it anyways.
                    then.ty = self.db.types.unit;
                    self.unit_expr(*span)
                };

                let ty = then.ty;

                self.expr(
                    hir::ExprKind::If(hir::If {
                        cond: Box::new(cond),
                        then: Box::new(then),
                        otherwise: Box::new(otherwise),
                    }),
                    ty,
                    *span,
                )
            }
            ast::Expr::Block { exprs, span } => {
                env.with_anon_scope(ScopeKind::Block, |env| -> CheckResult<hir::Expr> {
                    let (exprs, ty) = if exprs.is_empty() {
                        (vec![], self.db.types.unit)
                    } else {
                        let mut new_exprs = vec![];
                        let last = exprs.len() - 1;

                        for (i, expr) in exprs.iter().enumerate() {
                            let expected_ty =
                                if i == last { expected_ty } else { Some(self.db.types.unit) };
                            new_exprs.push(self.check_expr(env, expr, expected_ty)?);
                        }

                        let ty = new_exprs.last().unwrap().ty;

                        (new_exprs, ty)
                    };

                    Ok(self.expr(hir::ExprKind::Block(hir::Block { exprs }), ty, *span))
                })?
            }
            ast::Expr::Call { callee, args, span } => {
                let callee = self.check_expr(env, callee, None)?;

                let mut new_args = vec![];

                for arg in args {
                    new_args.push(match arg {
                        ast::CallArg::Named(name, expr) => hir::CallArg {
                            name: Some(*name),
                            expr: self.check_expr(env, expr, None)?,
                            index: None,
                        },
                        ast::CallArg::Positional(expr) => hir::CallArg {
                            name: None,
                            expr: self.check_expr(env, expr, None)?,
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
                        return Err(CheckError::ArgMismatch {
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
                                .ok_or(CheckError::NamedParamNotFound { word: *arg_name })?;

                            // Report named arguments that are passed twice
                            if let Some(passed_arg) = already_passed_args.insert(
                                arg_name.name(),
                                PassedArg { is_named: true, span: arg_name.span() },
                            ) {
                                return Err(CheckError::MultipleNamedArgs {
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

                    self.expr(
                        hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args: new_args }),
                        ty,
                        *span,
                    )
                } else {
                    return Err(CheckError::UncallableTy {
                        ty: self.normalize(callee.ty),
                        span: callee.span,
                    });
                }
            }
            ast::Expr::Unary { expr, op, span } => {
                let expr = self.check_expr(env, expr, None)?;

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

                self.expr(
                    hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                    ty,
                    *span,
                )
            }
            ast::Expr::Binary { lhs, rhs, op, span } => {
                let lhs = self.check_expr(env, lhs, None)?;
                let rhs = self.check_expr(env, rhs, Some(lhs.ty))?;

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

                self.expr(
                    hir::ExprKind::Binary(hir::Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: *op,
                    }),
                    ty,
                    *span,
                )
            }
            ast::Expr::Cast { expr, ty_expr: ty, span } => {
                let expr = self.check_expr(env, expr, None)?;
                let target = self.check_ty_expr(env, ty, AllowTyHole::Yes)?;

                self.expr(
                    hir::ExprKind::Cast(hir::Cast { expr: Box::new(expr), target }),
                    target,
                    *span,
                )
            }
            ast::Expr::Member { expr, member, span } => {
                let expr = self.check_expr(env, expr, None)?;

                let ty = self.normalize(expr.ty);

                let res_ty = match ty.kind() {
                    TyKind::Str if member.name() == sym::PTR => {
                        Ty::new(TyKind::RawPtr(self.db.types.u8))
                    }
                    TyKind::Str if member.name() == sym::LEN => self.db.types.uint,
                    _ => return Err(CheckError::InvalidMember { ty, member: *member }),
                };

                self.expr(
                    hir::ExprKind::Member(hir::Member { expr: Box::new(expr), member: *member }),
                    res_ty,
                    *span,
                )
            }
            ast::Expr::Name { word, args, span } => {
                let id = self.lookup_def(env, *word)?;

                let def_ty = self.normalize(self.db[id].ty);
                let ty_params = def_ty.collect_params();

                let args = if let Some(args) = args {
                    let args: Vec<Ty> = args
                        .iter()
                        .map(|arg| self.check_ty_expr(env, arg, AllowTyHole::Yes))
                        .try_collect()?;

                    Some(args)
                } else {
                    None
                };

                let instantiation: Instantiation = match &args {
                    Some(args) if args.len() == ty_params.len() => ty_params
                        .into_iter()
                        .zip(args)
                        .map(|(param, arg)| (param.var, *arg))
                        .collect(),
                    Some(args) => {
                        return Err(CheckError::TyArgMismatch {
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

                self.expr(
                    hir::ExprKind::Name(hir::Name { id, word: *word, instantiation }),
                    ty,
                    *span,
                )
            }
            ast::Expr::Lit { kind, span } => {
                let (kind, ty) = match kind {
                    ast::LitKind::Str(v) => (hir::Lit::Str(*v), self.db.types.str),
                    ast::LitKind::Int(v) => (hir::Lit::Int(*v), self.fresh_int_var()),
                    ast::LitKind::Bool(v) => (hir::Lit::Bool(*v), self.db.types.bool),
                };

                self.expr(hir::ExprKind::Lit(kind), ty, *span)
            }
        };

        self.db.const_storage.eval_expr(&expr).map_err(|e| CheckError::ConstEval(e, expr.span))?;

        Ok(expr)
    }

    fn check_ty_params(
        &mut self,
        env: &mut Env,
        ty_params: &[ast::TyParam],
    ) -> CheckResult<Vec<hir::TyParam>> {
        let mut new_ty_params = vec![];
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty =
                Ty::new(TyKind::Param(ParamTy { name: tp.name.name(), var: self.fresh_var() }));

            let id = self.define_local_def(env, DefKind::Ty(ty), tp.name, self.db.types.typ);

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
        ty: &TyExpr,
        allow_hole: AllowTyHole,
    ) -> CheckResult<Ty> {
        match ty {
            TyExpr::RawPtr(pointee, _) => {
                let pointee = self.check_ty_expr(env, pointee, allow_hole)?;
                Ok(Ty::new(TyKind::RawPtr(pointee)))
            }
            TyExpr::Name(name) => {
                let id = self.lookup_def(env, name.word)?;

                // TODO: use args when we implement polymorphic types
                // let args: Vec<Ty> = name
                //     .args
                //     .iter()
                //     .map(|a| self.check_ty_expr(env, a, allow_hole))
                //     .try_collect()?;

                let def = &self.db[id];

                match def.kind.as_ref() {
                    DefKind::Ty(ty) => Ok(*ty),
                    _ => Err(CheckError::ExpectedTy { ty: def.ty, span: name.span }),
                }
            }
            TyExpr::Hole(span) => {
                if allow_hole == AllowTyHole::Yes {
                    Ok(self.fresh_ty_var())
                } else {
                    Err(CheckError::InvalidInferTy(*span))
                }
            }
        }
    }

    fn expr(&mut self, kind: hir::ExprKind, ty: Ty, span: Span) -> hir::Expr {
        hir::Expr { id: self.expr_id.next(), kind, ty, span }
    }

    fn unit_expr(&mut self, span: Span) -> hir::Expr {
        self.expr(hir::ExprKind::Block(hir::Block { exprs: vec![] }), self.db.types.unit, span)
    }
}

create_bool_enum!(AllowTyHole);
