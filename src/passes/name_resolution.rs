mod env;
mod error;

use ustr::{ustr, UstrMap};

use crate::{
    ast::{
        Ast, Binary, Block, Call, CallArg, Cast, Expr, Fn, FnSig, If, Item, Let, Name, Pat, Return,
        Ty, TyName, TyParam, Unary,
    },
    common::{QPath, Word},
    db::{Db, Def, DefId, DefKind, FnInfo, ModuleId, ModuleInfo, ScopeInfo, ScopeLevel, Vis},
    passes::name_resolution::{
        env::{Env, GlobalScope, ScopeKind},
        error::ResolveError,
    },
    span::{Span, Spanned},
    sym, ty,
};

pub fn name_resolution(db: &mut Db, ast: &mut Ast) {
    let mut cx = Resolver::new(db);

    cx.define_modules(ast);
    cx.define_builtins();
    cx.define_global_items(ast);
    cx.resolve_all(ast);
    cx.report_cyclic_global_variables();

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

    fn define_builtins(&mut self) {
        let mut mk = |name: &str, ty: &dyn std::ops::Fn(&Db) -> ty::Ty| -> Option<DefId> {
            let name = ustr(name);
            self.builtins.insert(
                name,
                Def::alloc(
                    self.db,
                    QPath::from(name),
                    ScopeInfo {
                        module_id: self.db.main_module_id().expect("to be resolved"),
                        level: ScopeLevel::Global,
                        vis: Vis::Public,
                    },
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

    fn define_modules(&mut self, ast: &mut Ast) {
        for module in &mut ast.modules {
            module.id = Some(ModuleInfo::alloc(
                self.db,
                module.source,
                module.name.clone(),
                module.is_main(),
            ));
        }
    }

    fn define_global_items(&mut self, ast: &mut Ast) {
        for module in &mut ast.modules {
            for item in &mut module.items {
                self.define_global_item(module.id.expect("to be resolved"), item);
            }
        }
    }

    fn resolve_all(&mut self, ast: &mut Ast) {
        for module in &mut ast.modules {
            let mut env = Env::new(module.id.expect("ModuleId to be resolved"));

            for item in &mut module.items {
                item.resolve(self, &mut env);
            }
        }
    }

    fn define_global_item(&mut self, module_id: ModuleId, item: &mut Item) {
        match item {
            Item::Fn(fun) => {
                fun.id = Some(self.define_global_def(
                    module_id,
                    Vis::Public,
                    DefKind::Fn(FnInfo::Bare),
                    fun.sig.name,
                ));
            }
            Item::Let(let_) => match &mut let_.pat {
                Pat::Name(name) => {
                    name.id = Some(self.define_global_def(
                        module_id,
                        Vis::Public,
                        DefKind::Global,
                        name.word,
                    ));
                }
                Pat::Ignore(_) => (),
            },
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

    fn define_def(&mut self, env: &mut Env, kind: DefKind, name: Word) -> DefId {
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

    fn define_pat(&mut self, env: &mut Env, kind: DefKind, pat: &mut Pat) {
        match pat {
            Pat::Name(name) => {
                name.id = Some(self.define_def(env, kind, name.word));
            }
            Pat::Ignore(_) => (),
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

    fn resolve_ty_params(&mut self, env: &mut Env, ty_params: &mut [TyParam]) {
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty = self.db.types.unknown;
            tp.id = Some(self.define_def(env, DefKind::Ty(ty), tp.name));

            if let Some(prev_span) = defined_ty_params.insert(tp.name.name(), tp.name.span()) {
                self.errors.push(ResolveError::MultipleTyParams {
                    name: tp.name.name(),
                    prev_span,
                    dup_span: tp.name.span(),
                });
            }
        }
    }

    fn report_cyclic_global_variables(&mut self) {
        // TODO:
    }
}

trait Resolve<'db> {
    fn resolve(&mut self, cx: &mut Resolver<'db>, env: &mut Env);
}

impl Resolve<'_> for Item {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        match self {
            Item::Fn(fun) => fun.resolve(cx, env),
            Item::Let(let_) => let_.resolve(cx, env),
        }
    }
}

impl Resolve<'_> for Expr {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        match self {
            Self::Item(x) => x.resolve(cx, env),
            Self::If(x) => x.resolve(cx, env),
            Self::Block(x) => x.resolve(cx, env),
            Self::Return(x) => x.resolve(cx, env),
            Self::Call(x) => x.resolve(cx, env),
            Self::Unary(x) => x.resolve(cx, env),
            Self::Binary(x) => x.resolve(cx, env),
            Self::Cast(x) => x.resolve(cx, env),
            Self::Name(x) => x.resolve(cx, env),
            Self::Lit(..) => (),
        }
    }
}

impl Resolve<'_> for Fn {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        if !env.in_global_scope() {
            let id = cx.define_def(env, DefKind::Fn(FnInfo::Bare), self.sig.name);
            self.id = Some(id);
        }

        env.push(self.sig.name.name(), ScopeKind::Fn);
        self.sig.resolve(cx, env);
        self.body.resolve(cx, env);
        env.pop();
    }
}

impl Resolve<'_> for Let {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        if !env.in_global_scope() {
            cx.define_pat(env, DefKind::Variable, &mut self.pat);

            self.pat.walk(|pat| {
                env.insert(pat.word.name(), pat.id.unwrap());
            });
        }

        env.push(ustr("_"), ScopeKind::Initializer);

        if let Some(ty) = &mut self.ty_annot {
            ty.resolve(cx, env);
        }

        self.value.resolve(cx, env);

        env.pop();
    }
}

impl Resolve<'_> for FnSig {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        assert!(env.in_kind(ScopeKind::Fn), "FnSig must be resolved inside a ScopeKind::Fn");

        cx.resolve_ty_params(env, &mut self.ty_params);

        let mut defined_params = UstrMap::<Span>::default();

        for param in &mut self.params {
            param.id = Some(cx.define_def(env, DefKind::Variable, param.name));
            param.ty.resolve(cx, env);

            if let Some(prev_span) = defined_params.insert(param.name.name(), param.name.span()) {
                cx.errors.push(ResolveError::MultipleParams {
                    name: param.name.name(),
                    prev_span,
                    dup_span: param.name.span(),
                });
            }
        }

        if let Some(ret) = self.ret.as_mut() {
            ret.resolve(cx, env);
        }
    }
}

impl Resolve<'_> for If {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        self.cond.resolve(cx, env);
        self.then.resolve(cx, env);

        if let Some(otherwise) = &mut self.otherwise {
            otherwise.resolve(cx, env);
        }
    }
}

impl Resolve<'_> for Block {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        env.push(ustr("_"), ScopeKind::Block);

        for expr in &mut self.exprs {
            expr.resolve(cx, env);
        }

        env.pop();
    }
}

impl Resolve<'_> for Return {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        if let Some(expr) = self.expr.as_mut() {
            expr.resolve(cx, env);
        }
    }
}

impl Resolve<'_> for Call {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        self.callee.resolve(cx, env);

        for arg in &mut self.args {
            match arg {
                CallArg::Named(_, expr) | CallArg::Positional(expr) => expr.resolve(cx, env),
            }
        }
    }
}

impl Resolve<'_> for Unary {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        self.expr.resolve(cx, env);
    }
}

impl Resolve<'_> for Binary {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        self.lhs.resolve(cx, env);
        self.rhs.resolve(cx, env);
    }
}

impl Resolve<'_> for Cast {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        self.expr.resolve(cx, env);
        self.ty.resolve(cx, env);
    }
}

impl Resolve<'_> for Name {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        if let Ok(id) = cx.lookup(env, self.name) {
            self.id = Some(id);
        } else {
            cx.errors.push(ResolveError::NameNotFound(self.name));
        }

        if let Some(args) = &mut self.args {
            for arg in args {
                arg.resolve(cx, env);
            }
        }
    }
}

impl Resolve<'_> for Ty {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        match self {
            Self::Name(name) => name.resolve(cx, env),
            Self::Infer(span) if env.current().kind == ScopeKind::Fn => {
                cx.errors.push(ResolveError::InvalidInferTy(*span));
            }
            _ => (),
        }
    }
}

impl Resolve<'_> for TyName {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        if let Ok(id) = cx.lookup(env, self.name) {
            self.id = Some(id);
        } else {
            cx.errors.push(ResolveError::NameNotFound(self.name));
        }
    }
}
