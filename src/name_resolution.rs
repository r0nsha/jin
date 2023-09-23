mod env;
mod error;

use ustr::{ustr, UstrMap};

use crate::{
    ast::{self, Ast},
    common::{Counter, QPath, Word},
    db::{Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, Vis},
    diagnostics::Diagnostic,
    hir,
    hir::{ExprId, Hir},
    macros::create_bool_enum,
    name_resolution::{
        env::{Env, EnvKind, GlobalScope, ScopeKind},
        error::ResolveError,
    },
    span::{Span, Spanned},
    sym, ty,
    ty::Instantiation,
};

pub fn resolve(db: &mut Db, ast: Ast) -> Result<Hir, Diagnostic> {
    fn inner(db: &mut Db, mut ast: Ast) -> Result<Hir, ResolveError> {
        let mut cx = Resolver::new(db);

        cx.define_builtin_tys();
        cx.define_global_items(&mut ast)?;
        cx.resolve_all(&mut ast)?;

        Ok(cx.hir)
    }

    inner(db, ast).map_err(|err| err.into_diagnostic(db))
}

struct Resolver<'db> {
    db: &'db mut Db,
    hir: Hir,
    global_scope: GlobalScope,
    builtins: UstrMap<DefId>,
    expr_id: Counter<ExprId>,
}

impl<'db> Resolver<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self {
            db,
            hir: Hir::new(),
            global_scope: GlobalScope::new(),
            builtins: UstrMap::default(),
            expr_id: Counter::new(),
        }
    }

    fn define_builtin_tys(&mut self) {
        let mut mk = |name: &str, ty: &dyn std::ops::Fn(&Db) -> ty::Ty| -> Option<DefId> {
            let name = ustr(name);
            let scope_info = ScopeInfo {
                module_id: self.db.main_module_id().expect("to be resolved"),
                level: ScopeLevel::Global,
                vis: Vis::Public,
            };

            self.builtins.insert(
                name,
                DefInfo::alloc(
                    self.db,
                    QPath::from(name),
                    scope_info,
                    DefKind::Ty(ty(self.db)),
                    self.db.types.typ,
                    Span::unknown(),
                ),
            )
        };

        mk(sym::I8, &|db| db.types.i8);
        mk(sym::I16, &|db| db.types.i16);
        mk(sym::I32, &|db| db.types.i32);
        mk(sym::I64, &|db| db.types.i64);
        mk(sym::INT, &|db| db.types.int);

        mk(sym::U8, &|db| db.types.u8);
        mk(sym::U16, &|db| db.types.u16);
        mk(sym::U32, &|db| db.types.u32);
        mk(sym::U64, &|db| db.types.u64);
        mk(sym::UINT, &|db| db.types.uint);

        mk(sym::STR, &|db| db.types.str);
        mk(sym::BOOL, &|db| db.types.bool);
        mk(sym::NEVER, &|db| db.types.never);
    }

    fn define_global_items(&mut self, ast: &mut Ast) -> Result<(), ResolveError> {
        for module in &mut ast.modules {
            for item in &mut module.items {
                self.define_global_item(module.id.expect("to be resolved"), item)?;
            }
        }

        Ok(())
    }

    fn define_global_item(
        &mut self,
        module_id: ModuleId,
        item: &mut ast::Item,
    ) -> Result<(), ResolveError> {
        match item {
            ast::Item::Fn(fun) => {
                let id = self.define_def(
                    EnvKind::Global(module_id, Vis::Public),
                    DefKind::Fn(match &fun.kind {
                        ast::FnKind::Bare { .. } => FnInfo::Bare,
                        ast::FnKind::Extern => FnInfo::Extern,
                    }),
                    fun.sig.name,
                )?;

                fun.id = Some(id);

                Ok(())
            }
            ast::Item::ExternLet(let_) => {
                let id = self.define_def(
                    EnvKind::Global(module_id, Vis::Public),
                    DefKind::ExternGlobal,
                    let_.word,
                )?;

                let_.id = Some(id);

                Ok(())
            }
            ast::Item::Let(let_) => self.define_pat(
                EnvKind::Global(module_id, Vis::Public),
                DefKind::Global,
                &mut let_.pat,
            ),
        }
    }

    fn define_global_def(
        &mut self,
        module_id: ModuleId,
        vis: Vis,
        kind: DefKind,
        name: Word,
    ) -> Result<DefId, ResolveError> {
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let qpath = self.db[module_id].name.clone().child(name.name());

        let id = DefInfo::alloc(self.db, qpath, scope, kind, self.db.types.unknown, name.span());

        if let Some(prev_id) = self.global_scope.insert(module_id, name.name(), id) {
            let def = &self.db[prev_id];
            return Err(ResolveError::MultipleItems {
                name: def.qpath.name(),
                prev_span: def.span,
                dup_span: name.span(),
            });
        }

        Ok(id)
    }

    fn define_local_def(&mut self, env: &mut Env, kind: DefKind, name: Word) -> DefId {
        let id = DefInfo::alloc(
            self.db,
            env.scope_path(self.db).child(name.name()),
            ScopeInfo { module_id: env.module_id(), level: env.scope_level(), vis: Vis::Private },
            kind,
            self.db.types.unknown,
            name.span(),
        );

        env.current_mut().insert(name.name(), id);

        id
    }

    fn define_def(
        &mut self,
        env: EnvKind,
        kind: DefKind,
        name: Word,
    ) -> Result<DefId, ResolveError> {
        match env {
            EnvKind::Global(module_id, vis) => self.define_global_def(module_id, vis, kind, name),
            EnvKind::Local(env) => Ok(self.define_local_def(env, kind, name)),
        }
    }

    fn define_pat(
        &mut self,
        env: EnvKind,
        kind: DefKind,
        pat: &mut ast::Pat,
    ) -> Result<(), ResolveError> {
        match pat {
            ast::Pat::Name(name) => {
                let id = self.define_def(env, kind, name.word)?;
                name.id = Some(id);
            }
            ast::Pat::Discard(_) => (),
        }

        Ok(())
    }

    fn lookup(&self, env: &Env, word: Word) -> Result<DefId, ResolveError> {
        let name = word.name();
        env.lookup(name)
            .copied()
            .or_else(|| self.global_scope.lookup(env.module_id(), name))
            .or_else(|| self.builtins.get(&name).copied())
            .ok_or(ResolveError::NameNotFound(word))
    }

    fn resolve_all(&mut self, ast: &mut Ast) -> Result<(), ResolveError> {
        for module in &mut ast.modules {
            let mut env = Env::new(module.id.expect("ModuleId to be resolved"));

            for item in &mut module.items {
                match self.resolve_item(item, &mut env)? {
                    ItemResult::Let(let_) => self.hir.lets.push(let_),
                    ItemResult::Unit => (),
                }
            }
        }

        Ok(())
    }

    fn resolve_item(
        &mut self,
        item: &mut ast::Item,
        env: &mut Env,
    ) -> Result<ItemResult, ResolveError> {
        match item {
            ast::Item::Fn(fun) => {
                let f = self.resolve_fn(env, fun)?;
                self.hir.fns.push(f);
                Ok(ItemResult::Unit)
            }
            ast::Item::Let(let_) => {
                let let_ = self.resolve_let(env, let_)?;
                Ok(ItemResult::Let(let_))
            }
            ast::Item::ExternLet(let_) => {
                let let_ = self.resolve_extern_let(env, let_)?;
                self.hir.extern_lets.push(let_);
                Ok(ItemResult::Unit)
            }
        }
    }

    fn resolve_fn(&mut self, env: &mut Env, fun: &mut ast::Fn) -> Result<hir::Fn, ResolveError> {
        // TODO: instead of checking global scope here, pass the already defined id? or smth else
        if !env.in_global_scope() {
            fun.id = Some(self.define_def(
                EnvKind::Local(env),
                DefKind::Fn(FnInfo::Bare),
                fun.sig.name,
            )?);
        }

        for attr in &mut fun.attrs {
            if let Some(value) = &mut attr.value {
                self.resolve_expr(env, value)?;
            }
        }

        let attrs = self.resolve_attrs(env, &mut fun.attrs)?;

        let (sig, kind) = env.with_scope(fun.sig.name.name(), ScopeKind::Fn, |env| {
            let sig = self.resolve_sig(env, &mut fun.sig)?;

            let kind = match &mut fun.kind {
                ast::FnKind::Bare { body } => {
                    hir::FnKind::Bare { body: self.resolve_expr(env, body)? }
                }
                ast::FnKind::Extern => hir::FnKind::Extern,
            };

            Ok((sig, kind))
        })?;

        Ok(hir::Fn {
            module_id: env.module_id(),
            id: fun.id.expect("to be resolved"),
            attrs,
            sig,
            kind,
            span: fun.span,
        })
    }

    fn resolve_sig(
        &mut self,
        env: &mut Env,
        sig: &mut ast::FnSig,
    ) -> Result<hir::FnSig, ResolveError> {
        assert!(env.in_kind(ScopeKind::Fn), "FnSig must be resolved inside a ScopeKind::Fn");

        let ty_params = self.resolve_ty_params(env, &mut sig.ty_params)?;

        let mut params = vec![];
        let mut defined_params = UstrMap::<Span>::default();

        for p in &mut sig.params {
            let id = self.define_local_def(env, DefKind::Variable, p.name);

            let ty_annot = self.resolve_ty_expr(env, &mut p.ty_annot, AllowTyHole::No)?;

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                return Err(ResolveError::MultipleParams {
                    name: p.name.name(),
                    prev_span,
                    dup_span: p.name.span(),
                });
            }

            params.push(hir::FnParam { id, ty_annot, span: p.span, ty: self.db.types.unknown });
        }

        let ret = sig
            .ret
            .as_mut()
            .map(|ret| self.resolve_ty_expr(env, ret, AllowTyHole::No))
            .transpose()?;

        Ok(hir::FnSig { ty_params, params, ret })
    }

    fn resolve_let(
        &mut self,
        env: &mut Env,
        let_: &mut ast::Let,
    ) -> Result<hir::Let, ResolveError> {
        if !env.in_global_scope() {
            self.define_pat(EnvKind::Local(env), DefKind::Variable, &mut let_.pat)?;

            let_.pat.walk(|pat| {
                env.insert(pat.word.name(), pat.id.unwrap());
            });
        }

        env.with_anon_scope(ScopeKind::Initializer, |env| {
            if let Some(ty) = &mut let_.ty_annot {
                self.resolve_ty_expr(env, ty, AllowTyHole::Yes)?;
            }

            self.resolve_expr(env, &mut let_.value)
        })?;

        let attrs = self.resolve_attrs(env, &mut let_.attrs)?;

        let ty_annot = let_
            .ty_annot
            .as_mut()
            .map(|t| self.resolve_ty_expr(env, t, AllowTyHole::Yes))
            .transpose()?;

        let value = self.resolve_expr(env, &mut let_.value)?;

        Ok(hir::Let {
            module_id: env.module_id(),
            attrs,
            // TODO: move to function that both defines and lowers this Pat
            pat: match &let_.pat {
                ast::Pat::Name(name) => hir::Pat::Name(hir::NamePat {
                    id: name.id.expect("to be resolved"),
                    word: name.word,
                }),
                ast::Pat::Discard(span) => hir::Pat::Discard(*span),
            },
            ty_annot,
            value: Box::new(value),
            span: let_.span,
        })
    }

    fn resolve_extern_let(
        &mut self,
        env: &mut Env,
        let_: &mut ast::ExternLet,
    ) -> Result<hir::ExternLet, ResolveError> {
        if !env.in_global_scope() {
            let id = self.define_def(EnvKind::Local(env), DefKind::ExternGlobal, let_.word)?;
            let_.id = Some(id);
        }

        let attrs = self.resolve_attrs(env, &mut let_.attrs)?;

        let ty_annot = self.resolve_ty_expr(env, &mut let_.ty_annot, AllowTyHole::No)?;

        Ok(hir::ExternLet {
            module_id: env.module_id(),
            id: let_.id.expect("to be resolved"),
            attrs,
            word: let_.word,
            ty_annot,
            span: let_.span,
        })
    }

    fn resolve_attrs(
        &mut self,
        env: &mut Env,
        attrs: &mut ast::Attrs,
    ) -> Result<hir::Attrs, ResolveError> {
        attrs
            .iter_mut()
            .map(|attr| {
                Ok(hir::Attr {
                    kind: attr.kind,
                    value: attr.value.as_mut().map(|v| self.resolve_expr(env, v)).transpose()?,
                    span: attr.span,
                })
            })
            .try_collect()
    }

    fn resolve_expr(
        &mut self,
        env: &mut Env,
        expr: &mut ast::Expr,
    ) -> Result<hir::Expr, ResolveError> {
        match expr {
            ast::Expr::Item(item) => match self.resolve_item(item, env)? {
                ItemResult::Let(let_) => Ok(self.expr(hir::ExprKind::Let(let_), item.span())),
                ItemResult::Unit => Ok(self.unit(item.span())),
            },
            ast::Expr::Return { expr, span } => {
                let expr = if let Some(expr) = expr {
                    self.resolve_expr(env, expr)?
                } else {
                    self.unit(*span)
                };

                Ok(self.expr(hir::ExprKind::Return(hir::Return { expr: Box::new(expr) }), *span))
            }
            ast::Expr::If { cond, then, otherwise, span } => {
                let cond = self.resolve_expr(env, cond)?;
                let then = self.resolve_expr(env, then)?;

                let otherwise = if let Some(otherwise) = otherwise {
                    Some(Box::new(self.resolve_expr(env, otherwise)?))
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
                let exprs = exprs.iter_mut().map(|e| self.resolve_expr(env, e)).try_collect()?;
                Ok(self.expr(hir::ExprKind::Block(hir::Block { exprs }), *span))
            }),
            ast::Expr::Call { callee, args, span } => {
                let callee = self.resolve_expr(env, callee)?;

                let mut new_args = vec![];

                for arg in &mut *args {
                    new_args.push(match arg {
                        ast::CallArg::Named(name, expr) => hir::CallArg {
                            name: Some(*name),
                            expr: self.resolve_expr(env, expr)?,
                            index: None,
                        },
                        ast::CallArg::Positional(expr) => hir::CallArg {
                            name: None,
                            expr: self.resolve_expr(env, expr)?,
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
                let expr = self.resolve_expr(env, expr)?;

                Ok(self.expr(
                    hir::ExprKind::Unary(hir::Unary { expr: Box::new(expr), op: *op }),
                    *span,
                ))
            }
            ast::Expr::Member { expr, member, span } => {
                let expr = self.resolve_expr(env, expr)?;

                Ok(self.expr(
                    hir::ExprKind::Member(hir::Member { expr: Box::new(expr), member: *member }),
                    *span,
                ))
            }
            ast::Expr::Binary { lhs, rhs, op, span } => {
                let lhs = self.resolve_expr(env, lhs)?;
                let rhs = self.resolve_expr(env, rhs)?;

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
                let expr = self.resolve_expr(env, expr)?;
                let target = self.resolve_ty_expr(env, ty, AllowTyHole::Yes)?;

                Ok(self
                    .expr(hir::ExprKind::Cast(hir::Cast { expr: Box::new(expr), target }), *span))
            }
            ast::Expr::Name { word, args, span } => {
                let id = self.lookup(env, *word)?;

                let args = if let Some(args) = args {
                    let mut new_args = vec![];

                    for arg in args {
                        new_args.push(self.resolve_ty_expr(env, arg, AllowTyHole::Yes)?);
                    }

                    Some(new_args)
                } else {
                    None
                };

                Ok(self.expr(
                    hir::ExprKind::Name(hir::Name {
                        id,
                        args,
                        instantiation: Instantiation::new(),
                    }),
                    *span,
                ))
            }
            ast::Expr::Group { expr, span } => {
                let mut expr = self.resolve_expr(env, expr)?;
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

    fn resolve_ty_params(
        &mut self,
        env: &mut Env,
        ty_params: &mut [ast::TyParam],
    ) -> Result<Vec<hir::TyParam>, ResolveError> {
        let mut new_ty_params = vec![];
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty = self.db.types.unknown;
            let id = self.define_local_def(env, DefKind::Ty(ty), tp.name);

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
        ty: &mut ast::TyExpr,
        allow_hole: AllowTyHole,
    ) -> Result<hir::TyExpr, ResolveError> {
        match ty {
            ast::TyExpr::RawPtr(pointee, span) => {
                let pointee = self.resolve_ty_expr(env, pointee, allow_hole)?;
                Ok(hir::TyExpr::RawPtr(Box::new(pointee), *span))
            }
            ast::TyExpr::Name(name) => {
                let id = self.lookup(env, name.word)?;
                let args = name
                    .args
                    .iter_mut()
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

    fn expr(&mut self, kind: hir::ExprKind, span: Span) -> hir::Expr {
        hir::Expr { id: self.expr_id.next(), kind, span, ty: self.db.types.unknown }
    }

    fn unit(&mut self, span: Span) -> hir::Expr {
        self.expr(hir::ExprKind::Lit(hir::Lit::Unit), span)
    }
}

enum ItemResult {
    Let(hir::Let),
    Unit,
}

create_bool_enum!(AllowTyHole);
