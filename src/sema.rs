mod attrs;
mod coerce;
mod env;
mod errors;
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
    db::{
        Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, StructField,
        StructInfo,
    },
    diagnostics::{Diagnostic, Label},
    hir,
    hir::{const_eval::ConstEvalError, ExprId, Hir},
    index_vec::IndexVecExt,
    macros::create_bool_enum,
    middle::{BinOp, Mutability, TyExpr, UnOp, Vis},
    sema::{
        attrs::AttrsPlacement,
        coerce::CoerceExt,
        env::{BuiltinTys, Env, GlobalScope, ScopeKind, Symbol},
        instantiate::instantiate,
        item_state::{ItemState, ItemStatus, ResolvedFnSig},
        normalize::NormalizeTy,
        unify::Obligation,
    },
    span::{Span, Spanned},
    sym,
    ty::{FloatVar, FnTy, FnTyParam, InferTy, Instantiation, IntVar, ParamTy, Ty, TyKind, TyVar},
    word::Word,
};

pub type CheckResult<T> = Result<T, Diagnostic>;

pub fn check(db: &mut Db, ast: &Ast) -> CheckResult<Hir> {
    Sema::new(db, ast).run()
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
    checking_items: bool,
}

#[derive(Debug)]
pub struct TyStorage {
    pub ty_unification_table: InPlaceUnificationTable<TyVar>,
    pub int_unification_table: InPlaceUnificationTable<IntVar>,
    pub float_unification_table: InPlaceUnificationTable<FloatVar>,
}

impl TyStorage {
    pub fn new() -> Self {
        Self {
            ty_unification_table: InPlaceUnificationTable::new(),
            int_unification_table: InPlaceUnificationTable::new(),
            float_unification_table: InPlaceUnificationTable::new(),
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
            checking_items: true,
        }
    }

    fn run(mut self) -> CheckResult<Hir> {
        self.check_items()?;
        self.checking_items = false;
        self.check_fn_bodies()?;

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
                    self.check_item(&mut env, item, global_item_id)?;
                }
            }
        }

        Ok(())
    }

    fn check_fn_bodies(&mut self) -> CheckResult<()> {
        for module in &self.ast.modules {
            let mut env = Env::new(module.id);

            for (item_id, item) in module.items.iter_enumerated() {
                if let ast::Item::Fn(fun) = item {
                    let global_item_id = ast::GlobalItemId::new(module.id, item_id);
                    let ResolvedFnSig { id, sig } = self
                        .item_state
                        .get_resolved_fn_sig(global_item_id)
                        .expect("fn to be resolved")
                        .clone();
                    let f = self.check_fn_body(&mut env, fun, sig, id)?;
                    self.hir.fns.push(f);
                }
            }
        }

        Ok(())
    }

    fn define_global_def(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
        ty: Ty,
    ) -> CheckResult<DefId> {
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let qpath = self.db[module_id].name.clone().child(name.name());

        let id = DefInfo::alloc(self.db, qpath, scope, kind, mutability, ty, name.span());

        if let Some(prev_id) = self.global_scope.insert_def(Symbol::new(module_id, name.name()), id)
        {
            let def = &self.db[prev_id];

            let dup_span = name.span();
            let name = def.qpath.name();
            let prev_span = def.span;

            return Err(Diagnostic::error("check::multiple_items")
                .with_message(format!("the item `{name}` is defined multiple times"))
                .with_label(
                    Label::primary(dup_span).with_message(format!("`{name}` defined again here")),
                )
                .with_label(
                    Label::secondary(prev_span)
                        .with_message(format!("first definition of `{name}`")),
                )
                .with_help("you can only define items once in a module"));
        }

        Ok(id)
    }

    fn define_local_def(
        &mut self,
        env: &mut Env,
        kind: DefKind,
        name: Word,
        mutability: Mutability,
        ty: Ty,
    ) -> DefId {
        let id = DefInfo::alloc(
            self.db,
            env.scope_path(self.db).child(name.name()),
            ScopeInfo { module_id: env.module_id(), level: env.scope_level(), vis: Vis::Private },
            kind,
            mutability,
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
        mutability: Mutability,
        ty: Ty,
    ) -> CheckResult<DefId> {
        if env.in_global_scope() {
            self.define_global_def(env.module_id(), vis, kind, name, mutability, ty)
        } else {
            Ok(self.define_local_def(env, kind, name, mutability, ty))
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
                let id = self.define_def(env, vis, kind, name.word, name.mutability, ty)?;

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

        if self.checking_items {
            if let Some(id) = self.find_and_check_item(&symbol)? {
                return Ok(id);
            }
        }

        if let Some(id) = self.builtin_tys.get(name) {
            return Ok(id);
        }

        Err(Diagnostic::error("check::name_not_found")
            .with_message(format!("cannot find `{word}` in this scope"))
            .with_label(Label::primary(word.span()).with_message("not found in this scope")))
    }

    fn find_and_check_item(&mut self, symbol: &Symbol) -> CheckResult<Option<DefId>> {
        if let Some(item_id) = self.global_scope.get_item(symbol) {
            let item = &self.ast.modules[symbol.module_id].items[item_id];
            self.check_item(
                &mut Env::new(symbol.module_id),
                item,
                ast::GlobalItemId::new(symbol.module_id, item_id),
            )?;
            let id = self.global_scope.get_def(symbol).expect("global def to be defined");
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }

    fn check_item(
        &mut self,
        env: &mut Env,
        item: &ast::Item,
        item_id: ast::GlobalItemId,
    ) -> CheckResult<()> {
        self.item_state.mark_as_in_progress(item_id).map_err(|err| {
            let origin_span = item.span();
            let reference_span = self.ast.find_item(err.causee).expect("item to exist").span();

            Diagnostic::error("check::cyclic_items")
                .with_message("cycle detected while checking definition")
                .with_label(Label::primary(origin_span).with_message("definition here"))
                .with_label(Label::secondary(reference_span).with_message("cyclic reference here"))
        })?;

        match item {
            ast::Item::Fn(fun) => {
                self.check_fn_item(env, fun, item_id)?;
                // Fn will be added to Hir after `check_fn_bodies`
            }
            ast::Item::Let(let_) => {
                let let_ = self.check_let(env, let_)?;
                self.hir.lets.push(let_);
            }
            ast::Item::Type(tydef) => {
                self.check_ty_def(env, tydef)?;
            }
            ast::Item::ExternLet(let_) => {
                let let_ = self.check_extern_let(env, let_)?;
                self.hir.extern_lets.push(let_);
            }
            ast::Item::ExternImport(import) => {
                self.db.extern_libs.insert(import.lib.clone());
            }
        }

        self.item_state.mark_as_resolved(item_id);

        Ok(())
    }

    fn check_fn_body(
        &mut self,
        env: &mut Env,
        fun: &ast::Fn,
        mut sig: hir::FnSig,
        id: DefId,
    ) -> CheckResult<hir::Fn> {
        let kind =
            env.with_scope(fun.sig.word.name(), ScopeKind::Fn(id), |env| -> CheckResult<_> {
                for tp in &sig.ty_params {
                    env.insert(tp.word.name(), tp.id);
                }

                for p in &mut sig.params {
                    p.id = self.define_local_def(
                        env,
                        DefKind::Variable,
                        p.name,
                        Mutability::Imm,
                        p.ty,
                    );
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
                    ast::FnKind::Extern { is_c_variadic } => {
                        Ok(hir::FnKind::Extern { is_c_variadic: *is_c_variadic })
                    }
                }
            })?;

        Ok(hir::Fn { module_id: env.module_id(), id, sig, kind, span: fun.span })
    }

    fn check_fn_item(
        &mut self,
        env: &mut Env,
        fun: &ast::Fn,
        item_id: ast::GlobalItemId,
    ) -> CheckResult<()> {
        self.check_attrs(
            env.module_id(),
            &fun.attrs,
            match &fun.kind {
                ast::FnKind::Bare { .. } => AttrsPlacement::Fn,
                ast::FnKind::Extern { .. } => AttrsPlacement::ExternFn,
            },
        )?;

        let is_c_variadic = match &fun.kind {
            ast::FnKind::Bare { .. } => false,
            ast::FnKind::Extern { is_c_variadic } => *is_c_variadic,
        };

        let sig = env.with_scope(
            fun.sig.word.name(),
            ScopeKind::Fn(DefId::INVALID),
            |env| -> CheckResult<_> { self.check_fn_sig(env, &fun.sig, is_c_variadic) },
        )?;

        let id = self.define_def(
            env,
            Vis::Private,
            DefKind::Fn(match &fun.kind {
                ast::FnKind::Bare { .. } => FnInfo::Bare,
                ast::FnKind::Extern { .. } => FnInfo::Extern,
            }),
            sig.word,
            Mutability::Imm,
            sig.ty,
        )?;

        self.item_state.insert_resolved_fn_sig(item_id, ResolvedFnSig { id, sig });

        Ok(())
    }

    fn check_fn_sig(
        &mut self,
        env: &mut Env,
        sig: &ast::FnSig,
        is_c_variadic: bool,
    ) -> CheckResult<hir::FnSig> {
        let ty_params = self.check_ty_params(env, &sig.ty_params)?;

        let mut params = vec![];
        let mut defined_params = UstrMap::<Span>::default();

        for p in &sig.params {
            let ty = self.check_ty_expr(env, &p.ty_expr, AllowTyHole::No)?;

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                let name = p.name.name();
                let dup_span = p.name.span();

                return Err(Diagnostic::error("check::multiple_params")
                    .with_message(format!("the name `{name}` is already used as a parameter name"))
                    .with_label(
                        Label::primary(dup_span).with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span).with_message(format!("first use of `{name}`")),
                    ));
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
            is_c_variadic,
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

        self.at(Obligation::obvious(value.span)).eq(ty, value.ty).or_coerce(self, value.id)?;

        let def_kind = if env.in_global_scope() { DefKind::Global } else { DefKind::Variable };
        let pat = self.define_pat(env, Vis::Private, def_kind, &let_.pat, ty, value.id)?;

        Ok(hir::Let {
            module_id: env.module_id(),
            pat,
            value: Box::new(value),
            ty,
            span: let_.span,
        })
    }

    fn check_ty_def(&mut self, env: &mut Env, tydef: &ast::TyDef) -> CheckResult<()> {
        self.check_attrs(env.module_id(), &tydef.attrs, AttrsPlacement::ExternLet)?;

        match &tydef.kind {
            ast::TyDefKind::Struct(struct_def) => {
                let mut fields = vec![];
                let mut defined_fields = UstrMap::<Span>::default();

                for field in &struct_def.fields {
                    if let Some(prev_span) =
                        defined_fields.insert(field.name.name(), field.name.span())
                    {
                        let name = field.name.name();
                        let dup_span = field.name.span();

                        return Err(Diagnostic::error("check::multiple_fields")
                            .with_message(format!(
                                "the name `{name}` is already used as a field name"
                            ))
                            .with_label(
                                Label::primary(dup_span)
                                    .with_message(format!("`{name}` used again here")),
                            )
                            .with_label(
                                Label::secondary(prev_span)
                                    .with_message(format!("first use of `{name}`")),
                            ));
                    }

                    fields.push(StructField { name: field.name, ty: self.db.types.unknown });
                }

                let struct_id = self.db.structs.push_with_key(|id| StructInfo {
                    id,
                    def_id: DefId::INVALID,
                    name: tydef.word,
                    fields,
                    is_extern: struct_def.is_extern,
                    ctor_ty: self.db.types.unknown,
                });

                let def_id = self.define_def(
                    env,
                    Vis::Private,
                    DefKind::Struct(struct_id),
                    tydef.word,
                    Mutability::Imm,
                    self.db.types.typ,
                )?;

                self.db.structs[struct_id].def_id = def_id;

                for (idx, field) in struct_def.fields.iter().enumerate() {
                    let ty = self.check_ty_expr(env, &field.ty_expr, AllowTyHole::No)?;
                    self.db.structs[struct_id].fields[idx].ty = ty;
                }

                self.db.structs[struct_id].fill_ctor_ty();

                let struct_info = &self.db.structs[struct_id];

                if let Some(field) = self.db.structs[struct_id].is_infinitely_sized() {
                    return Err(Diagnostic::error("check::infinitely_sized_type")
                        .with_message(format!("type `{}` is infinitely sized", struct_info.name))
                        .with_label(
                            Label::primary(struct_info.name.span()).with_message("defined here"),
                        )
                        .with_label(Label::secondary(field.name.span()).with_message(format!(
                            "field has type `{}` without indirection",
                            struct_info.name
                        ))));
                }
            }
        }

        Ok(())
    }

    fn check_extern_let(
        &mut self,
        env: &mut Env,
        let_: &ast::ExternLet,
    ) -> CheckResult<hir::ExternLet> {
        self.check_attrs(env.module_id(), &let_.attrs, AttrsPlacement::ExternLet)?;

        let ty = self.check_ty_expr(env, &let_.ty_expr, AllowTyHole::No)?;
        let id = self.define_def(
            env,
            Vis::Private,
            DefKind::ExternGlobal,
            let_.word,
            let_.mutability,
            ty,
        )?;

        Ok(hir::ExternLet { module_id: env.module_id(), id, word: let_.word, span: let_.span })
    }

    fn check_expr(
        &mut self,
        env: &mut Env,
        expr: &ast::Expr,
        expected_ty: Option<Ty>,
    ) -> CheckResult<hir::Expr> {
        let expr = match expr {
            ast::Expr::Let(let_) => {
                let span = let_.span;
                let let_ = self.check_let(env, let_)?;
                self.expr(hir::ExprKind::Let(let_), self.db.types.unit, span)
            }
            ast::Expr::Assign { lhs, rhs, span } => {
                let lhs = self.check_expr(env, lhs, None)?;
                self.check_assign_lhs(&lhs)?;

                let rhs = self.check_expr(env, rhs, Some(lhs.ty))?;

                self.at(Obligation::exprs(*span, lhs.span, lhs.span))
                    .eq(lhs.ty, lhs.ty)
                    .or_coerce(self, lhs.id)?;

                self.expr(
                    hir::ExprKind::Assign(hir::Assign { lhs: Box::new(lhs), rhs: Box::new(rhs) }),
                    self.db.types.unit,
                    *span,
                )
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
                    return Err(Diagnostic::error("check::invalid_return")
                        .with_message("cannot return outside of function scope")
                        .with_label(Label::primary(*span)));
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
            ast::Expr::Loop { cond, expr, span } => {
                let cond = if let Some(cond) = cond.as_ref() {
                    let cond = self.check_expr(env, cond, Some(self.db.types.bool))?;

                    self.at(Obligation::obvious(cond.span))
                        .eq(self.db.types.bool, cond.ty)
                        .or_coerce(self, cond.id)?;

                    Some(Box::new(cond))
                } else {
                    None
                };

                let expr = env.with_anon_scope(ScopeKind::Loop, |env| {
                    self.check_expr(env, expr, Some(self.db.types.never))
                })?;

                // NOTE: expected & actual types are flipped here so that all types are accepted
                self.at(Obligation::obvious(expr.span))
                    .eq(expr.ty, self.db.types.never)
                    .or_coerce(self, expr.id)?;

                self.expr(
                    hir::ExprKind::Loop(hir::Loop { cond, expr: Box::new(expr) }),
                    self.db.types.never,
                    *span,
                )
            }
            ast::Expr::Break { span } => {
                if env.in_scope_kind(&ScopeKind::Loop) {
                    self.expr(hir::ExprKind::Break, self.db.types.never, *span)
                } else {
                    return Err(Diagnostic::error("check::invalid_break")
                        .with_message("cannot break outside of a loop")
                        .with_label(Label::primary(*span).with_message("break outside of loop")));
                }
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

                let callee_ty = self.normalize(callee.ty);

                if let TyKind::Fn(fun_ty) = callee_ty.kind() {
                    #[derive(Debug)]
                    struct PassedArg {
                        is_named: bool,
                        span: Span,
                    }

                    if !fun_ty.is_c_variadic && new_args.len() != fun_ty.params.len() {
                        let expected = fun_ty.params.len();
                        let found = new_args.len();

                        return Err(Diagnostic::error("check::arg_mismatch")
                            .with_message(format!("this function takes {expected} argument(s), but {found} were supplied"))
                            .with_label(Label::primary(*span).with_message(format!(
                                "expected {expected} arguments, found {found}"
                            ))));
                    }

                    let mut already_passed_args = UstrMap::<PassedArg>::default();

                    // Resolve positional arg indices
                    for (idx, arg) in new_args.iter_mut().enumerate() {
                        if arg.name.is_none() {
                            arg.index = Some(idx);

                            if let Some(param_name) = fun_ty.params.get(idx).and_then(|p| p.name) {
                                already_passed_args.insert(
                                    param_name,
                                    PassedArg { is_named: false, span: arg.expr.span },
                                );
                            }
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
                                .ok_or_else(|| {
                                    Diagnostic::error("check::named_param_not_found")
                                        .with_message(format!(
                                            "cannot find parameter with the name `{}`",
                                            arg_name.name()
                                        ))
                                        .with_label(Label::primary(arg_name.span()).with_message(
                                            format!("found argument `{}` here", arg_name.name()),
                                        ))
                                })?;

                            // Report named arguments that are passed twice
                            if let Some(passed_arg) = already_passed_args.insert(
                                arg_name.name(),
                                PassedArg { is_named: true, span: arg_name.span() },
                            ) {
                                let name = arg_name.name();
                                let prev = passed_arg.span;
                                let dup = arg_name.span();
                                let is_named = passed_arg.is_named;

                                return Err(Diagnostic::error("check::multiple_named_args")
                                    .with_message(if is_named {
                                        format!("argument `{name}` is passed multiple times")
                                    } else {
                                        format!("argument `{name}` is already passed positionally")
                                    })
                                    .with_label(
                                        Label::primary(dup)
                                            .with_message(format!("`{name}` is passed again here")),
                                    )
                                    .with_label(Label::secondary(prev).with_message(format!(
                                        "`{name}` is already passed here"
                                    ))));
                            }

                            arg.index = Some(idx);
                        }
                    }

                    // Unify all args with their corresponding param type
                    for arg in &new_args {
                        let idx = arg.index.expect("arg index to be resolved");

                        if let Some(param) = fun_ty.params.get(idx) {
                            self.at(Obligation::obvious(arg.expr.span))
                                .eq(param.ty, arg.expr.ty)
                                .or_coerce(self, arg.expr.id)?;
                        }
                    }

                    self.expr(
                        hir::ExprKind::Call(hir::Call { callee: Box::new(callee), args: new_args }),
                        fun_ty.ret,
                        *span,
                    )
                } else {
                    let ty = self.normalize(callee.ty);
                    let span = callee.span;

                    return Err(Diagnostic::error("check::uncallable_type")
                        .with_message(format!(
                            "expected a function, found `{}`",
                            ty.display(self.db)
                        ))
                        .with_label(Label::primary(span).with_message("expected a function")));
                }
            }
            ast::Expr::Unary { expr, op, span } => {
                let expr = self.check_expr(env, expr, None)?;

                let ty = self.normalize(expr.ty);

                match op {
                    UnOp::Neg => {
                        if !ty.is_any_int() && !ty.is_any_float() {
                            return Err(Diagnostic::error("check::invalid_neg")
                                .with_message(format!(
                                    "cannot use `{}` on `{}`",
                                    op,
                                    ty.display(self.db)
                                ))
                                .with_label(
                                    Label::primary(expr.span)
                                        .with_message(format!("invalid use of `{op}`")),
                                ));
                        }
                    }
                    UnOp::Not => {
                        if !ty.is_any_int() && !ty.is_bool() {
                            return Err(Diagnostic::error("check::invalid_not")
                                .with_message(format!(
                                    "cannot use `{}` on `{}`",
                                    op,
                                    ty.display(self.db)
                                ))
                                .with_label(
                                    Label::primary(expr.span)
                                        .with_message(format!("invalid use of `{op}`")),
                                ));
                        }
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
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Rem => {
                        let ty = self.normalize(lhs.ty);

                        if !ty.is_any_int() && !ty.is_any_float() {
                            return Err(Diagnostic::error("check::invalid_binary_op")
                                .with_message(format!(
                                    "cannot use `{}` on `{}`",
                                    op,
                                    ty.display(self.db)
                                ))
                                .with_label(
                                    Label::primary(lhs.span)
                                        .with_message(format!("invalid use of `{op}`")),
                                ));
                        }
                    }
                    BinOp::Shl | BinOp::Shr | BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => {
                        let ty = self.normalize(lhs.ty);

                        if !ty.is_any_int() {
                            return Err(Diagnostic::error("check::invalid_binary_op")
                                .with_message(format!(
                                    "cannot use `{}` on `{}`",
                                    op,
                                    ty.display(self.db)
                                ))
                                .with_label(
                                    Label::primary(lhs.span)
                                        .with_message(format!("invalid use of `{op}`")),
                                ));
                        }
                    }
                    BinOp::Cmp(_) => (),
                }

                let result_ty = match op {
                    BinOp::Cmp(..) => self.db.types.bool,
                    _ => lhs.ty,
                };

                self.expr(
                    hir::ExprKind::Binary(hir::Binary {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        op: *op,
                    }),
                    result_ty,
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
                    TyKind::Struct(sid) => {
                        let struct_info = &self.db[*sid];

                        if let Some(field) = struct_info.field_by_name(member.name().as_str()) {
                            field.ty
                        } else {
                            return Err(errors::invalid_member(self.db, ty, *member));
                        }
                    }
                    TyKind::Str if member.name() == sym::PTR => {
                        Ty::new(TyKind::RawPtr(self.db.types.u8))
                    }
                    TyKind::Str if member.name() == sym::LEN => self.db.types.uint,
                    _ => {
                        return Err(Diagnostic::error("check::invalid_member")
                            .with_message(format!(
                                "type `{}` has no member `{}`",
                                ty.display(self.db),
                                member
                            ))
                            .with_label(
                                Label::primary(member.span()).with_message("unknown member"),
                            ))
                    }
                };

                self.expr(
                    hir::ExprKind::Member(hir::Member { expr: Box::new(expr), member: *member }),
                    res_ty,
                    *span,
                )
            }
            ast::Expr::Name { word, args, span } => {
                let id = self.lookup_def(env, *word)?;

                if let DefKind::Struct(sid) = self.db[id].kind.as_ref() {
                    // NOTE: if the named definition is a struct, we want to return its
                    // constructor function's type
                    self.expr(
                        hir::ExprKind::Name(hir::Name {
                            id,
                            word: *word,
                            instantiation: Instantiation::default(),
                        }),
                        self.db[*sid].ctor_ty,
                        *span,
                    )
                } else {
                    let def_ty = self.normalize(self.db[id].ty);
                    let mut ty_params = def_ty.collect_params();

                    // NOTE: map type params that are part of the current polymorphic function to themselves, so
                    // that we don't instantiate them. that's quite ugly though.
                    if let Some(fn_id) = env.fn_id() {
                        let fn_ty_params = self.db[fn_id].ty.collect_params();
                        for ftp in fn_ty_params {
                            if let Some(tp) = ty_params.iter_mut().find(|p| p.var == ftp.var) {
                                *tp = ftp.clone();
                            }
                        }
                    }

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
                            let expected = ty_params.len();
                            let found = args.len();

                            return Err(Diagnostic::error("check::type_arg_mismatch")
                                                    .with_message(format!(
                                                "expected {expected} type argument(s), but {found} were supplied"
                                            ))
                                                    .with_label(Label::primary(*span).with_message(format!(
                                                        "expected {expected} type arguments, found {found}"
                                                    ))));
                        }
                        _ => {
                            let fn_ty_params =
                                env.fn_id().map_or(vec![], |id| self.db[id].ty.collect_params());

                            ty_params
                                .into_iter()
                                .map(|param| {
                                    (
                                        param.var,
                                        // If the type param is one of the current function's type
                                        // params, we don't want to instantiate it
                                        if fn_ty_params.iter().any(|p| p.var == param.var) {
                                            Ty::new(TyKind::Param(param))
                                        } else {
                                            self.fresh_ty_var()
                                        },
                                    )
                                })
                                .collect()
                        }
                    };

                    let ty = instantiate(def_ty, instantiation.clone());

                    self.expr(
                        hir::ExprKind::Name(hir::Name { id, word: *word, instantiation }),
                        ty,
                        *span,
                    )
                }
            }
            ast::Expr::Lit { kind, span } => {
                let (kind, ty) = match kind {
                    ast::LitKind::Str(v) => (hir::Lit::Str(*v), self.db.types.str),
                    ast::LitKind::Int(v) => (hir::Lit::Int(*v), self.fresh_int_var()),
                    ast::LitKind::Float(v) => (hir::Lit::Float(*v), self.fresh_float_var()),
                    ast::LitKind::Bool(v) => (hir::Lit::Bool(*v), self.db.types.bool),
                };

                self.expr(hir::ExprKind::Lit(kind), ty, *span)
            }
        };

        self.db.const_storage.eval_expr(&expr).map_err(|err| {
            let msg = match err {
                ConstEvalError::DivByZero => "division by zero",
                ConstEvalError::RemByZero => "reminder by zero",
                ConstEvalError::Overflow => "integer overflow",
            };

            Diagnostic::error("check::const_eval_error")
                .with_message(format!("const evaluation failed: {msg}"))
                .with_label(Label::primary(expr.span).with_message(format!("caught {msg}")))
        })?;

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
                Ty::new(TyKind::Param(ParamTy { name: tp.word.name(), var: self.fresh_var() }));

            let id = self.define_local_def(
                env,
                DefKind::Ty(ty),
                tp.word,
                Mutability::Imm,
                self.db.types.typ,
            );

            if let Some(prev_span) = defined_ty_params.insert(tp.word.name(), tp.word.span()) {
                let name = tp.word.name();
                let dup_span = tp.word.span();

                return Err(Diagnostic::error("check::multiple_type_params")
                    .with_message(format!(
                        "the name `{name}` is already used as a type parameter name"
                    ))
                    .with_label(
                        Label::primary(dup_span).with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span).with_message(format!("first use of `{name}`")),
                    ));
            }

            new_ty_params.push(hir::TyParam { id, word: tp.word });
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
            TyExpr::Fn(fn_ty) => {
                let params = fn_ty
                    .params
                    .iter()
                    .map(|ty| {
                        self.check_ty_expr(env, ty, allow_hole)
                            .map(|ty| FnTyParam { name: None, ty })
                    })
                    .try_collect()?;

                let ret = if let Some(ret) = &fn_ty.ret {
                    self.check_ty_expr(env, ret, allow_hole)?
                } else {
                    self.db.types.unit
                };

                Ok(Ty::new(TyKind::Fn(FnTy { params, ret, is_c_variadic: fn_ty.is_c_variadic })))
            }
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
                    DefKind::Struct(sid) => Ok(Ty::new(TyKind::Struct(*sid))),
                    _ => Err(Diagnostic::error("check::expected_ty")
                        .with_message(format!(
                            "expected a type, found value of type `{}`",
                            def.ty.display(self.db)
                        ))
                        .with_label(Label::primary(name.span).with_message("expected a type"))),
                }
            }
            TyExpr::Unit(_) => Ok(self.db.types.unit),
            TyExpr::Hole(span) => {
                if allow_hole == AllowTyHole::Yes {
                    Ok(self.fresh_ty_var())
                } else {
                    Err(Diagnostic::error("check::invalid_infer_type")
                        .with_message("cannot use a _ type in a function's signature")
                        .with_label(Label::primary(*span)))
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

    #[inline]
    pub fn fresh_ty_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Ty(self.fresh_var())))
    }

    #[inline]
    pub fn fresh_var(&self) -> TyVar {
        self.storage.borrow_mut().ty_unification_table.new_key(None)
    }

    #[inline]
    pub fn fresh_int_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Int(
            self.storage.borrow_mut().int_unification_table.new_key(None),
        )))
    }

    #[inline]
    pub fn fresh_float_var(&self) -> Ty {
        Ty::new(TyKind::Infer(InferTy::Float(
            self.storage.borrow_mut().float_unification_table.new_key(None),
        )))
    }

    #[inline]
    pub fn normalize(&self, ty: Ty) -> Ty {
        ty.normalize(&mut self.storage.borrow_mut())
    }

    fn check_assign_lhs(&self, expr: &hir::Expr) -> CheckResult<()> {
        match &expr.kind {
            hir::ExprKind::Member(access) => self.check_assign_lhs_inner(&access.expr, expr.span),
            hir::ExprKind::Name(name) => self.check_assign_lhs_name(name, expr.span),
            _ => Err(Diagnostic::error("check::invalid_assign")
                .with_message("invalid left-hand side of assignment")
                .with_label(
                    Label::primary(expr.span).with_message("cannot assign to this expression"),
                )),
        }
    }

    fn check_assign_lhs_inner(&self, expr: &hir::Expr, origin_span: Span) -> CheckResult<()> {
        match &expr.kind {
            hir::ExprKind::Name(name) => self.check_assign_lhs_name(name, origin_span),
            _ => Ok(()),
        }
    }

    fn check_assign_lhs_name(&self, name: &hir::Name, origin_span: Span) -> CheckResult<()> {
        let def = &self.db[name.id];

        if def.mutability.is_mut() {
            Ok(())
        } else {
            Err(Diagnostic::error("check::immutable_assign")
                .with_message(format!("cannot assign twice to immutable value `{}`", def.name))
                .with_label(
                    Label::primary(origin_span)
                        .with_message(format!("`{}` is immutable", def.name)),
                ))
        }
    }
}

create_bool_enum!(AllowTyHole);
