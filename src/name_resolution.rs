mod env;
mod error;

use ustr::{ustr, UstrMap};

use crate::{
    ast::{self, Ast},
    common::{QPath, Word},
    db::{Db, DefId, DefInfo, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, Vis},
    diagnostics::Diagnostic,
    hir,
    hir::Hir,
    macros::create_bool_enum,
    name_resolution::{
        env::{Env, EnvKind, GlobalScope, ScopeKind},
        error::ResolveError,
    },
    span::{Span, Spanned},
    sym, ty,
};

pub fn resolve(db: &mut Db, ast: &mut Ast) -> Result<(), Diagnostic> {
    fn inner(db: &mut Db, ast: &mut Ast) -> Result<(), ResolveError> {
        let mut cx = Resolver::new(db);

        cx.define_builtin_tys();
        cx.define_global_items(ast)?;
        cx.resolve_all(ast)?;

        Ok(())
    }

    inner(db, ast).map_err(|err| err.into_diagnostic(db))
}

struct Resolver<'db> {
    db: &'db mut Db,
    hir: Hir,
    global_scope: GlobalScope,
    builtins: UstrMap<DefId>,
}

impl<'db> Resolver<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, hir: Hir::new(), global_scope: GlobalScope::new(), builtins: UstrMap::default() }
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
                self.resolve_item(item, &mut env)?;
            }
        }

        Ok(())
    }

    fn resolve_item(&mut self, item: &mut ast::Item, env: &mut Env) -> Result<(), ResolveError> {
        match item {
            ast::Item::Fn(fun) => self.resolve_fn(env, fun),
            ast::Item::Let(let_) => self.resolve_let(env, let_),
            ast::Item::ExternLet(let_) => self.resolve_extern_let(env, let_),
        }
    }

    fn resolve_fn(&mut self, env: &mut Env, fun: &mut ast::Fn) -> Result<(), ResolveError> {
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

        env.with_scope(fun.sig.name.name(), ScopeKind::Fn, |env| {
            self.resolve_sig(env, &mut fun.sig)?;

            match &mut fun.kind {
                ast::FnKind::Bare { body } => self.resolve_expr(env, body),
                ast::FnKind::Extern => Ok(()),
            }
        })?;

        Ok(())
    }

    fn resolve_sig(&mut self, env: &mut Env, sig: &mut ast::FnSig) -> Result<(), ResolveError> {
        assert!(env.in_kind(ScopeKind::Fn), "FnSig must be resolved inside a ScopeKind::Fn");

        self.resolve_ty_params(env, &mut sig.ty_params)?;

        let mut defined_params = UstrMap::<Span>::default();

        for p in &mut sig.params {
            p.id = Some(self.define_local_def(env, DefKind::Variable, p.name));
            self.resolve_ty(env, &mut p.ty, AllowTyHole::No)?;

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                return Err(ResolveError::MultipleParams {
                    name: p.name.name(),
                    prev_span,
                    dup_span: p.name.span(),
                });
            }
        }

        if let Some(ret) = sig.ret.as_mut() {
            self.resolve_ty(env, ret, AllowTyHole::No)?;
        }

        Ok(())
    }

    fn resolve_let(&mut self, env: &mut Env, let_: &mut ast::Let) -> Result<(), ResolveError> {
        if !env.in_global_scope() {
            self.define_pat(EnvKind::Local(env), DefKind::Variable, &mut let_.pat)?;

            let_.pat.walk(|pat| {
                env.insert(pat.word.name(), pat.id.unwrap());
            });
        }

        env.with_anon_scope(ScopeKind::Initializer, |env| {
            if let Some(ty) = &mut let_.ty_annot {
                self.resolve_ty(env, ty, AllowTyHole::Yes)?;
            }

            self.resolve_expr(env, &mut let_.value)
        })?;

        Ok(())
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

        self.resolve_ty(env, &mut let_.ty_annot, AllowTyHole::No)?;

        Ok(hir::ExternLet {
            module_id: env.module_id(),
            id: let_.id.expect("to be resolved"),
            attrs: let_.attrs.lower(cx),
            word: let_.word,
            ty_annot: let_.ty_annot.lower(cx),
            span: let_.span,
        })
    }

    fn resolve_attrs(
        &mut self,
        env: &mut Env,
        attrs: &ast::Attrs,
    ) -> Result<hir::Attrs, ResolveError> {
        attrs
            .into_iter()
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
            ast::Expr::Item(item) => {
                self.resolve_item(item, env)?;
            }
            ast::Expr::Return { expr, .. } => {
                if let Some(expr) = expr {
                    self.resolve_expr(env, expr)?;
                }
            }
            ast::Expr::If { cond, then, otherwise, .. } => {
                self.resolve_expr(env, cond)?;
                self.resolve_expr(env, then)?;

                if let Some(otherwise) = otherwise {
                    self.resolve_expr(env, otherwise)?;
                }
            }
            ast::Expr::Block { exprs, .. } => {
                env.with_anon_scope(ScopeKind::Block, |env| {
                    for expr in &mut *exprs {
                        self.resolve_expr(env, expr)?;
                    }

                    Ok(())
                })?;
            }
            ast::Expr::Call { callee, args, .. } => {
                self.resolve_expr(env, callee)?;

                for arg in args {
                    match arg {
                        ast::CallArg::Named(_, expr) | ast::CallArg::Positional(expr) => {
                            self.resolve_expr(env, expr)?;
                        }
                    }
                }
            }
            ast::Expr::Unary { expr, .. } | ast::Expr::Member { expr, .. } => {
                self.resolve_expr(env, expr)?;
            }
            ast::Expr::Binary { lhs, rhs, .. } => {
                self.resolve_expr(env, lhs)?;
                self.resolve_expr(env, rhs)?;
            }
            ast::Expr::Cast { expr, ty, .. } => {
                self.resolve_expr(env, expr)?;
                self.resolve_ty(env, ty, AllowTyHole::Yes)?;
            }
            ast::Expr::Name { id, word, args, .. } => {
                *id = Some(self.lookup(env, *word)?);

                if let Some(args) = args {
                    for arg in args {
                        self.resolve_ty(env, arg, AllowTyHole::Yes)?;
                    }
                }
            }
            ast::Expr::Group { expr, span: _ } => {
                self.resolve_expr(env, expr)?;
            }
            ast::Expr::Lit { .. } => (),
        }

        Ok(())
    }

    fn resolve_ty_params(
        &mut self,
        env: &mut Env,
        ty_params: &mut [ast::TyParam],
    ) -> Result<(), ResolveError> {
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty = self.db.types.unknown;
            tp.id = Some(self.define_local_def(env, DefKind::Ty(ty), tp.name));

            if let Some(prev_span) = defined_ty_params.insert(tp.name.name(), tp.name.span()) {
                return Err(ResolveError::MultipleTyParams {
                    name: tp.name.name(),
                    prev_span,
                    dup_span: tp.name.span(),
                });
            }
        }

        Ok(())
    }

    fn resolve_ty(
        &mut self,
        env: &Env,
        ty: &mut ast::TyExpr,
        allow_hole: AllowTyHole,
    ) -> Result<(), ResolveError> {
        match ty {
            ast::TyExpr::RawPtr(pointee, _) => self.resolve_ty(env, pointee, allow_hole),
            ast::TyExpr::Name(name) => {
                name.id = Some(self.lookup(env, name.word)?);
                Ok(())
            }
            ast::TyExpr::Hole(span) => {
                if allow_hole.into() {
                    Ok(())
                } else {
                    Err(ResolveError::InvalidInferTy(*span))
                }
            }
            ast::TyExpr::Unit(_) => Ok(()),
        }
    }
}

create_bool_enum!(AllowTyHole);
