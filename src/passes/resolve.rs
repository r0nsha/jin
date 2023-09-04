use std::collections::HashMap;

use ustr::{ustr, Ustr, UstrMap};

use crate::{
    ast::{
        Ast, BinOp, Block, Call, CallArg, Expr, Fn, FnSig, If, Item, Module, Name, Return, Ty,
        TyName, TyParam,
    },
    common::{QPath, Word},
    db::{Db, Def, DefId, DefKind, FnInfo, ModuleId, ModuleInfo, ScopeInfo, ScopeLevel, Vis},
    diagnostics::{Diagnostic, Label},
    span::{Span, Spanned},
    ty,
};

pub fn resolve(db: &mut Db, ast: &mut Ast) {
    let mut cx = Resolver::new(db);

    cx.resolve_modules(&mut ast.modules);
    cx.declare_builtin_defs();
    cx.resolve_global_items(&mut ast.modules);
    cx.resolve_all(&mut ast.modules);

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

    fn declare_builtin_defs(&mut self) {
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

        mk("int", &|db| db.types.int);
        mk("bool", &|db| db.types.bool);
    }

    fn resolve_modules(&mut self, modules: &mut [Module]) {
        for module in modules {
            module.id = Some(ModuleInfo::alloc(
                self.db,
                module.source,
                module.name.clone(),
                module.is_main(),
            ));
        }
    }

    fn resolve_global_items(&mut self, modules: &mut [Module]) {
        for module in modules {
            for item in &mut module.items {
                self.declare_global_item(module.id.expect("to be resolved"), item);
            }
        }
    }

    fn resolve_all(&mut self, modules: &mut [Module]) {
        for module in modules {
            let mut env = Env::new(module.id.expect("ModuleId to be resolved"));

            for item in &mut module.items {
                item.resolve(self, &mut env);
            }
        }
    }

    fn declare_global_item(&mut self, module_id: ModuleId, item: &mut Item) -> DefId {
        match item {
            Item::Fn(fun) => {
                let id = self.declare_global_def(
                    module_id,
                    Vis::Public,
                    DefKind::Fn(FnInfo::Bare),
                    fun.sig.name,
                );

                fun.id = Some(id);

                id
            }
        }
    }

    fn declare_global_def(
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

    fn declare_item(&mut self, env: &mut Env, item: &mut Item) -> DefId {
        match item {
            Item::Fn(fun) => {
                let id = self.declare_def(env, DefKind::Fn(FnInfo::Bare), fun.sig.name);

                fun.id = Some(id);

                id
            }
        }
    }

    fn declare_def(&mut self, env: &mut Env, kind: DefKind, name: Word) -> DefId {
        let id = Def::alloc(
            self.db,
            env.scope_path(self.db).child(name.name()),
            ScopeInfo { module_id: env.module_id, level: env.scope_level(), vis: Vis::Private },
            kind,
            self.db.types.unknown,
            name.span(),
        );

        env.current_mut().insert(name.name(), id);

        id
    }

    fn lookup(&self, env: &Env, word: Word) -> Result<DefId, ResolveError> {
        let name = word.name();
        env.lookup(name)
            .copied()
            .or_else(|| self.global_scope.lookup(env.module_id, name))
            .or_else(|| self.builtins.get(&name).copied())
            .ok_or(ResolveError::NameNotFound(word))
    }

    fn resolve_ty_params(&mut self, env: &mut Env, ty_params: &mut [TyParam]) {
        let mut defined_ty_params = UstrMap::<Span>::default();

        for tp in ty_params {
            let ty = self.db.types.unknown;
            tp.id = Some(self.declare_def(env, DefKind::Ty(ty), tp.name));

            if let Some(prev_span) = defined_ty_params.insert(tp.name.name(), tp.name.span()) {
                self.errors.push(ResolveError::MultipleTyParams {
                    name: tp.name.name(),
                    prev_span,
                    dup_span: tp.name.span(),
                });
            }
        }
    }
}

trait Resolve<'db> {
    fn resolve(&mut self, cx: &mut Resolver<'db>, env: &mut Env);
}

impl Resolve<'_> for Item {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        if !env.in_global_scope() {
            cx.declare_item(env, self);
        }

        match self {
            Item::Fn(fun) => fun.resolve(cx, env),
        }
    }
}

impl Resolve<'_> for Expr {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        match self {
            Self::Item(inner) => inner.resolve(cx, env),
            Self::If(inner) => inner.resolve(cx, env),
            Self::Block(inner) => inner.resolve(cx, env),
            Self::Return(inner) => inner.resolve(cx, env),
            Self::Call(inner) => inner.resolve(cx, env),
            Self::BinOp(inner) => inner.resolve(cx, env),
            Self::Name(inner) => inner.resolve(cx, env),
            Self::Lit(..) => (),
        }
    }
}

impl Resolve<'_> for Fn {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        env.push(self.sig.name.name(), ScopeKind::Fn);
        self.sig.resolve(cx, env);
        self.body.resolve(cx, env);
        env.pop();
    }
}

impl Resolve<'_> for FnSig {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        assert!(env.in_kind(ScopeKind::Fn), "FnSig must be resolved inside a ScopeKind::Fn");

        cx.resolve_ty_params(env, &mut self.ty_params);

        let mut defined_params = UstrMap::<Span>::default();

        for param in &mut self.params {
            param.id = Some(cx.declare_def(env, DefKind::Variable, param.name));
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
        if !env.in_kind(ScopeKind::Fn) {
            cx.errors.push(ResolveError::InvalidReturn(self.span));
        }

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

impl Resolve<'_> for BinOp {
    fn resolve(&mut self, cx: &mut Resolver<'_>, env: &mut Env) {
        self.lhs.resolve(cx, env);
        self.rhs.resolve(cx, env);
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
                cx.errors.push(ResolveError::InvalidPlaceholderTy(*span));
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

#[derive(Debug)]
pub struct GlobalScope {
    modules: HashMap<(ModuleId, Ustr), DefId>,
}

impl GlobalScope {
    pub fn new() -> Self {
        Self { modules: HashMap::new() }
    }

    pub fn lookup(&self, module_id: ModuleId, name: Ustr) -> Option<DefId> {
        self.modules.get(&(module_id, name)).copied()
    }

    pub fn insert(&mut self, module_id: ModuleId, name: Ustr, id: DefId) -> Option<DefId> {
        self.modules.insert((module_id, name), id)
    }
}

#[derive(Debug)]
pub struct Env {
    module_id: ModuleId,
    scopes: Vec<Scope>,
}

#[allow(unused)]
impl Env {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, scopes: vec![] }
    }

    pub fn push(&mut self, name: Ustr, kind: ScopeKind) {
        self.scopes.push(Scope { kind, name, defs: UstrMap::default() });
    }

    pub fn pop(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    pub fn current(&self) -> &Scope {
        self.scopes.last().expect("to have a scope")
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("to have a scope")
    }

    pub fn insert(&mut self, k: Ustr, v: DefId) {
        self.scopes.last_mut().unwrap().defs.insert(k, v);
    }

    pub fn lookup(&self, k: Ustr) -> Option<&DefId> {
        self.lookup_depth(k).map(|r| r.1)
    }

    #[allow(unused)]
    pub fn lookup_mut(&mut self, k: Ustr) -> Option<&mut DefId> {
        self.lookup_depth_mut(k).map(|r| r.1)
    }

    #[allow(unused)]
    pub fn lookup_depth(&self, k: Ustr) -> Option<(usize, &DefId)> {
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(value) = scope.defs.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    #[allow(unused)]
    pub fn lookup_depth_mut(&mut self, k: Ustr) -> Option<(usize, &mut DefId)> {
        for (depth, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(value) = scope.defs.get_mut(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    pub fn scope_level(&self) -> ScopeLevel {
        match self.depth() {
            0 => unreachable!("expected to have at least one scope"),
            n => ScopeLevel::Local(n),
        }
    }

    pub fn scope_path(&self, db: &Db) -> QPath {
        let mut qpath = db[self.module_id].name.clone();
        qpath.extend(self.scopes.iter().map(|s| s.name));
        qpath
    }

    #[inline]
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    #[inline]
    pub fn in_global_scope(&self) -> bool {
        self.depth() == 0
    }

    fn in_kind(&self, kind: ScopeKind) -> bool {
        self.scopes.iter().any(|s| s.kind == kind)
    }
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub name: Ustr,
    pub defs: UstrMap<DefId>,
}

impl Scope {
    fn insert(&mut self, name: Ustr, id: DefId) {
        self.defs.insert(name, id);
    }

    #[allow(unused)]
    fn get(&mut self, name: Ustr) -> Option<DefId> {
        self.defs.get(&name).copied()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ScopeKind {
    Fn,
    Block,
}

pub(super) enum ResolveError {
    MultipleItems { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleParams { name: Ustr, prev_span: Span, dup_span: Span },
    MultipleTyParams { name: Ustr, prev_span: Span, dup_span: Span },
    NameNotFound(Word),
    InvalidReturn(Span),
    InvalidPlaceholderTy(Span),
}

impl From<ResolveError> for Diagnostic {
    fn from(err: ResolveError) -> Self {
        match err {
            ResolveError::MultipleItems { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_items")
                    .with_message(format!("the item `{name}` is defined multiple times"))
                    .with_label(
                        Label::primary(dup_span)
                            .with_message(format!("`{name}` defined again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span)
                            .with_message(format!("first definition of `{name}`")),
                    )
                    .with_help("you can only define items once in a module")
            }
            ResolveError::MultipleTyParams { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_type_params")
                    .with_message(format!(
                        "the name `{name}` is already used as a type parameter name"
                    ))
                    .with_label(
                        Label::primary(dup_span).with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span).with_message(format!("first use of `{name}`")),
                    )
            }
            ResolveError::MultipleParams { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_params")
                    .with_message(format!("the name `{name}` is already used as a parameter name"))
                    .with_label(
                        Label::primary(dup_span).with_message(format!("`{name}` used again here")),
                    )
                    .with_label(
                        Label::secondary(prev_span).with_message(format!("first use of `{name}`")),
                    )
            }
            ResolveError::NameNotFound(name) => Self::error("resolve::name_not_found")
                .with_message(format!("cannot find `{name}` in this scope"))
                .with_label(Label::primary(name.span()).with_message("not found in this scope")),
            ResolveError::InvalidReturn(span) => Self::error("resolve::invalid_return")
                .with_message("cannot return outside of function scope")
                .with_label(Label::primary(span)),
            ResolveError::InvalidPlaceholderTy(span) => {
                Self::error("resolve::invalid_placeholder_type")
                    .with_message("cannot use a placeholder type _ in a function's signature")
                    .with_label(Label::primary(span))
            }
        }
    }
}
