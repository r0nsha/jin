mod env;
mod error;

use ustr::{ustr, UstrMap};

use crate::{
    ast::{Ast, Block, CallArg, Expr, ExternLet, Fn, FnKind, FnSig, Item, Let, Pat, Ty, TyParam},
    common::{QPath, Word},
    db::{Db, Def, DefId, DefKind, FnInfo, ModuleId, ScopeInfo, ScopeLevel, Vis},
    name_resolution::{
        env::{Env, EnvKind, GlobalScope, ScopeKind},
        error::ResolveError,
    },
    span::{Span, Spanned},
    sym, ty,
};

pub fn resolve(db: &mut Db, ast: &mut Ast) {
    let mut cx = Resolver::new(db);

    cx.define_builtin_tys();
    cx.define_global_items(ast);
    cx.resolve_all(ast);

    if !cx.errors.is_empty() {
        let errors = cx.errors;
        db.diagnostics.emit_many(errors);
    }
}

struct Resolver<'db> {
    db: &'db mut Db,
    errors: Vec<ResolveError>,
    global_scope: GlobalScope,
    builtins: UstrMap<DefId>,
}

impl<'db> Resolver<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, errors: vec![], global_scope: GlobalScope::new(), builtins: UstrMap::default() }
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
                Def::alloc(
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

        mk(sym::BOOL, &|db| db.types.bool);
        mk(sym::NEVER, &|db| db.types.never);
    }

    fn define_global_items(&mut self, ast: &mut Ast) {
        for module in &mut ast.modules {
            for item in &mut module.items {
                self.define_global_item(module.id.expect("to be resolved"), item);
            }
        }
    }

    fn define_global_item(&mut self, module_id: ModuleId, item: &mut Item) {
        match item {
            Item::Fn(fun) => {
                fun.id = Some(self.define_def(
                    EnvKind::Global(module_id, Vis::Public),
                    DefKind::Fn(match &fun.kind {
                        FnKind::Bare { .. } => FnInfo::Bare,
                        FnKind::Extern => FnInfo::Extern,
                    }),
                    fun.sig.name,
                ));
            }
            Item::ExternLet(let_) => {
                let_.id = Some(self.define_def(
                    EnvKind::Global(module_id, Vis::Public),
                    DefKind::ExternGlobal,
                    let_.word,
                ));
            }
            Item::Let(let_) => self.define_pat(
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
    ) -> DefId {
        let scope = ScopeInfo { module_id, level: ScopeLevel::Global, vis };
        let qpath = self.db[module_id].name.clone().child(name.name());

        let id = Def::alloc(self.db, qpath, scope, kind, self.db.types.unknown, name.span());

        if let Some(prev_id) = self.global_scope.insert(module_id, name.name(), id) {
            let def = &self.db[prev_id];
            self.errors.push(ResolveError::MultipleItems {
                name: def.qpath.name(),
                prev_span: def.span,
                dup_span: name.span(),
            });
        }

        id
    }

    fn define_local_def(&mut self, env: &mut Env, kind: DefKind, name: Word) -> DefId {
        let id = Def::alloc(
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

    fn define_def(&mut self, env: EnvKind, kind: DefKind, name: Word) -> DefId {
        match env {
            EnvKind::Global(module_id, vis) => self.define_global_def(module_id, vis, kind, name),
            EnvKind::Local(env) => self.define_local_def(env, kind, name),
        }
    }

    fn define_pat(&mut self, env: EnvKind, kind: DefKind, pat: &mut Pat) {
        match pat {
            Pat::Name(name) => {
                name.id = Some(self.define_def(env, kind, name.word));
            }
            Pat::Discard(_) => (),
        }
    }

    fn lookup(&self, env: &Env, word: Word) -> Result<DefId, ResolveError> {
        let name = word.name();
        env.lookup(name)
            .copied()
            .or_else(|| self.global_scope.lookup(env.module_id(), name))
            .or_else(|| self.builtins.get(&name).copied())
            .ok_or(ResolveError::NameNotFound(word))
    }

    fn resolve_all(&mut self, ast: &mut Ast) {
        for module in &mut ast.modules {
            let mut env = Env::new(module.id.expect("ModuleId to be resolved"));

            for item in &mut module.items {
                self.resolve_item(item, &mut env);
            }
        }
    }

    fn resolve_item(&mut self, item: &mut Item, env: &mut Env) {
        match item {
            Item::Fn(fun) => self.resolve_fn(env, fun),
            Item::Let(let_) => self.resolve_let(env, let_),
            Item::ExternLet(let_) => self.resolve_extern_let(env, let_),
        }
    }

    fn resolve_fn(&mut self, env: &mut Env, fun: &mut Fn) {
        if !env.in_global_scope() {
            fun.id =
                Some(self.define_def(EnvKind::Local(env), DefKind::Fn(FnInfo::Bare), fun.sig.name));
        }

        for attr in &mut fun.attrs {
            if let Some(value) = &mut attr.value {
                self.resolve_expr(env, value);
            }
        }

        env.with_scope(fun.sig.name.name(), ScopeKind::Fn, |env| {
            self.resolve_sig(env, &mut fun.sig);

            match &mut fun.kind {
                FnKind::Bare { body } => self.resolve_expr(env, body),
                FnKind::Extern => (),
            }
        });
    }

    fn resolve_sig(&mut self, env: &mut Env, sig: &mut FnSig) {
        assert!(env.in_kind(ScopeKind::Fn), "FnSig must be resolved inside a ScopeKind::Fn");

        self.resolve_ty_params(env, &mut sig.ty_params);

        let mut defined_params = UstrMap::<Span>::default();

        for p in &mut sig.params {
            p.id = Some(self.define_local_def(env, DefKind::Variable, p.name));
            self.resolve_ty(env, &mut p.ty);

            if let Some(prev_span) = defined_params.insert(p.name.name(), p.name.span()) {
                self.errors.push(ResolveError::MultipleParams {
                    name: p.name.name(),
                    prev_span,
                    dup_span: p.name.span(),
                });
            }
        }

        if let Some(ret) = sig.ret.as_mut() {
            self.resolve_ty(env, ret);
        }
    }

    fn resolve_let(&mut self, env: &mut Env, let_: &mut Let) {
        if !env.in_global_scope() {
            self.define_pat(EnvKind::Local(env), DefKind::Variable, &mut let_.pat);

            let_.pat.walk(|pat| {
                env.insert(pat.word.name(), pat.id.unwrap());
            });
        }

        env.with_anon_scope(ScopeKind::Initializer, |env| {
            if let Some(ty) = &mut let_.ty_annot {
                self.resolve_ty(env, ty);
            }

            self.resolve_expr(env, &mut let_.value);
        });
    }

    fn resolve_extern_let(&mut self, env: &mut Env, let_: &mut ExternLet) {
        if !env.in_global_scope() {
            let_.id = Some(self.define_def(EnvKind::Local(env), DefKind::ExternGlobal, let_.word));
        }

        self.resolve_ty(env, &mut let_.ty_annot);
    }

    fn resolve_block(&mut self, env: &mut Env, blk: &mut Block) {
        env.with_anon_scope(ScopeKind::Block, |env| {
            for expr in &mut blk.exprs {
                self.resolve_expr(env, expr);
            }
        });
    }

    fn resolve_expr(&mut self, env: &mut Env, expr: &mut Expr) {
        match expr {
            Expr::Item(item) => self.resolve_item(item, env),
            Expr::Return(ret) => {
                if let Some(expr) = ret.expr.as_mut() {
                    self.resolve_expr(env, expr);
                }
            }
            Expr::If(if_) => {
                self.resolve_expr(env, &mut if_.cond);
                self.resolve_expr(env, &mut if_.then);

                if let Some(otherwise) = &mut if_.otherwise {
                    self.resolve_expr(env, otherwise);
                }
            }
            Expr::Block(blk) => self.resolve_block(env, blk),
            Expr::Call(call) => {
                self.resolve_expr(env, &mut call.callee);

                for arg in &mut call.args {
                    match arg {
                        CallArg::Named(_, expr) | CallArg::Positional(expr) => {
                            self.resolve_expr(env, expr);
                        }
                    }
                }
            }
            Expr::Unary(un) => {
                self.resolve_expr(env, &mut un.expr);
            }
            Expr::Binary(bin) => {
                self.resolve_expr(env, &mut bin.lhs);
                self.resolve_expr(env, &mut bin.rhs);
            }
            Expr::Cast(cast) => {
                self.resolve_expr(env, &mut cast.expr);
                self.resolve_ty(env, &mut cast.ty);
            }
            Expr::MemberAccess(access) => {
                self.resolve_expr(env, &mut access.expr);
            }
            Expr::Name(name) => {
                match self.lookup(env, name.word) {
                    Ok(id) => name.id = Some(id),
                    Err(err) => self.errors.push(err),
                }

                if let Some(args) = &mut name.args {
                    for arg in args {
                        self.resolve_ty(env, arg);
                    }
                }
            }
            Expr::Lit(_) => (),
        }
    }

    fn resolve_ty_params(&mut self, env: &mut Env, ty_params: &mut [TyParam]) {
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty = self.db.types.unknown;
            tp.id = Some(self.define_local_def(env, DefKind::Ty(ty), tp.name));

            if let Some(prev_span) = defined_ty_params.insert(tp.name.name(), tp.name.span()) {
                self.errors.push(ResolveError::MultipleTyParams {
                    name: tp.name.name(),
                    prev_span,
                    dup_span: tp.name.span(),
                });
            }
        }
    }

    fn resolve_ty(&mut self, env: &Env, ty: &mut Ty) {
        match ty {
            Ty::RawPtr(pointee, _) => self.resolve_ty(env, pointee),
            Ty::Name(name) => match self.lookup(env, name.word) {
                Ok(id) => name.id = Some(id),
                Err(err) => self.errors.push(err),
            },
            Ty::Hole(span) if env.current().kind == ScopeKind::Fn => {
                // TODO: pass a `allow_infer_ty: AllowInferTy::{Yes/No}` instead of scope kind
                self.errors.push(ResolveError::InvalidInferTy(*span));
            }
            _ => (),
        }
    }
}
