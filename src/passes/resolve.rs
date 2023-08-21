use std::collections::HashMap;

use ustr::{ustr, Ustr, UstrMap};

use crate::{
    common::{QualifiedName, Word},
    db::{
        Database, FunctionInfo, ModuleId, ScopeLevel, SymbolId, SymbolInfo, SymbolInfoKind, TypeId,
        Vis,
    },
    diagnostics::{Diagnostic, Label},
    hir::{
        Binary, Block, Call, CallArg, Expr, Function, Hir, If, Item, ItemKind, Module, Name, Return,
    },
    span::Span,
};

pub fn resolve(db: &mut Database, hir: &mut Hir) {
    let mut cx = ResolveCx::new(db);

    cx.resolve_global_items(&mut hir.modules);
    cx.resolve_all(&mut hir.modules);

    if !cx.errors.is_empty() {
        let errors = cx.errors;
        db.diagnostics.extend(errors);
    }
}

struct ResolveCx<'db> {
    db: &'db mut Database,
    errors: Vec<ResolveError>,
    global_scope: GlobalScope,
}

impl<'db> ResolveCx<'db> {
    fn new(db: &'db mut Database) -> Self {
        Self { db, errors: vec![], global_scope: GlobalScope::new() }
    }

    fn resolve_global_items(&mut self, modules: &mut [Module]) {
        for module in modules {
            let mut env = Env::new(module.id);

            env.scopes.push(ustr(""), ScopeKind::Global);

            for item in &mut module.items {
                self.declare_item(&mut env, item);
            }

            let symbols = env.scopes.pop().expect("to have a single scope").symbols;
            self.global_scope.insert_module(module.id, symbols);
        }
    }

    fn resolve_all(&mut self, modules: &mut [Module]) {
        for module in modules {
            let mut env = Env::new(module.id);

            for item in &mut module.items {
                item.resolve(self, &mut env);
            }
        }
    }

    fn declare_item(&mut self, env: &mut Env, item: &mut Item) -> SymbolId {
        match &mut item.kind {
            ItemKind::Function(fun) => {
                let id = self.declare_symbol(
                    env,
                    SymbolInfoKind::Function(FunctionInfo::Orphan),
                    fun.name,
                );

                fun.id = Some(id);

                id
            }
        }
    }

    fn declare_symbol(&mut self, env: &mut Env, kind: SymbolInfoKind, name: Word) -> SymbolId {
        let module_id = env.module_id;
        let scope_level = env.scope_level(Vis::Public);
        dbg!(scope_level);
        let qname = env.scope_name(self.db).child(name.name());

        let id = SymbolInfo::alloc(
            self.db,
            module_id,
            qname,
            scope_level,
            kind,
            TypeId::null(),
            name.span(),
        );

        match env.scopes.current_mut() {
            Some(scope) => scope.insert(name.name(), id),
            None => {
                if let Some(prev_id) = self.global_scope.insert(env.module_id, name.name(), id) {
                    let sym = &self.db[prev_id];
                    self.errors.push(ResolveError::MultipleItems {
                        name: sym.qualified_name.name(),
                        prev_span: sym.span,
                        dup_span: name.span(),
                    });
                }
            }
        }

        id
    }
}

trait Resolve<'db> {
    fn resolve(&mut self, cx: &mut ResolveCx<'db>, env: &mut Env);
}

impl Resolve<'_> for Item {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        if !env.is_global_scope() {
            cx.declare_item(env, self);
        }

        match &mut self.kind {
            ItemKind::Function(fun) => fun.resolve(cx, env),
        }
    }
}

impl Resolve<'_> for Expr {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        match self {
            Self::Item(inner) => inner.resolve(cx, env),
            Self::If(inner) => inner.resolve(cx, env),
            Self::Block(inner) => inner.resolve(cx, env),
            Self::Return(inner) => inner.resolve(cx, env),
            Self::Call(inner) => inner.resolve(cx, env),
            Self::Binary(inner) => inner.resolve(cx, env),
            Self::Name(inner) => inner.resolve(cx, env),
            Self::Lit(_) => (),
        }
    }
}

impl Resolve<'_> for Function {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        env.scopes.push(self.name.name(), ScopeKind::Fun);

        for param in &mut self.params {
            param.id = Some(cx.declare_symbol(env, SymbolInfoKind::Variable, param.name));
        }

        self.body.resolve(cx, env);
        env.scopes.pop();
    }
}

impl Resolve<'_> for If {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        self.cond.resolve(cx, env);
        self.then.resolve(cx, env);

        if let Some(otherwise) = &mut self.otherwise {
            otherwise.resolve(cx, env);
        }
    }
}

impl Resolve<'_> for Block {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        env.scopes.push(ustr("_"), ScopeKind::Block);

        for expr in &mut self.exprs {
            expr.resolve(cx, env);
        }

        env.scopes.pop();
    }
}

impl Resolve<'_> for Return {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        self.expr.resolve(cx, env);

        if !env.scopes.in_kind(ScopeKind::Fun) {
            cx.errors.push(ResolveError::InvalidReturn { span: self.span });
        }
    }
}

impl Resolve<'_> for Call {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        self.callee.resolve(cx, env);

        let mut already_passed = UstrMap::<Span>::default();

        for arg in &mut self.args {
            match arg {
                CallArg::Named(name, expr) => {
                    if let Some(prev_span) = already_passed.insert(name.name(), name.span()) {
                        cx.errors.push(ResolveError::MultipleNamedArgs {
                            prev: Word::new(name.name(), prev_span),
                            dup: *name,
                        });
                    }

                    expr.resolve(cx, env);
                }
                CallArg::Positional(expr) => expr.resolve(cx, env),
            }
        }
    }
}

impl Resolve<'_> for Binary {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        self.lhs.resolve(cx, env);
        self.rhs.resolve(cx, env);
    }
}

impl Resolve<'_> for Name {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        self.id = env
            .scopes
            .lookup(self.name.name())
            .copied()
            .or_else(|| cx.global_scope.lookup(env.module_id, self.name.name()));

        if self.id.is_none() {
            cx.errors.push(ResolveError::NameNotFound(self.name));
        }
    }
}

#[derive(Debug)]
pub struct GlobalScope(HashMap<ModuleId, UstrMap<SymbolId>>);

impl GlobalScope {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn lookup(&self, module_id: ModuleId, name: Ustr) -> Option<SymbolId> {
        self.0.get(&module_id).and_then(|syms| syms.get(&name)).copied()
    }

    pub fn insert_module(
        &mut self,
        module_id: ModuleId,
        symbols: UstrMap<SymbolId>,
    ) -> Option<UstrMap<SymbolId>> {
        self.0.insert(module_id, symbols)
    }

    pub fn insert(&mut self, module_id: ModuleId, name: Ustr, id: SymbolId) -> Option<SymbolId> {
        self.0.get_mut(&module_id).and_then(|syms| syms.insert(name, id))
    }
}

#[derive(Debug)]
pub struct Env {
    module_id: ModuleId,
    scopes: Scopes,
}

impl Env {
    pub fn new(module_id: ModuleId) -> Self {
        Self { module_id, scopes: Scopes::new() }
    }

    pub fn scope_level(&self, vis: Vis) -> ScopeLevel {
        match self.scopes.depth() {
            0 => ScopeLevel::Global(vis),
            n => ScopeLevel::Local(n),
        }
    }

    pub fn scope_name(&self, db: &Database) -> QualifiedName {
        let module_name = &db[self.module_id].name;
        self.scopes.current().map_or_else(
            || module_name.clone(),
            |scope| module_name.clone().child(scope.name.clone()),
        )
    }

    pub fn is_global_scope(&self) -> bool {
        self.scopes.depth() == 0
    }
}

#[derive(Debug)]
struct Scopes(Vec<Scope>);

impl Scopes {
    fn new() -> Self {
        Self(vec![])
    }

    fn push(&mut self, name: Ustr, kind: ScopeKind) {
        let name = self
            .current()
            .map_or_else(|| QualifiedName::from(name), |curr| curr.name.clone().child(name));

        self.0.push(Scope { kind, name, symbols: UstrMap::default() });
    }

    fn pop(&mut self) -> Option<Scope> {
        self.0.pop()
    }

    fn current(&self) -> Option<&Scope> {
        self.0.last()
    }

    #[allow(unused)]
    fn current_mut(&mut self) -> Option<&mut Scope> {
        self.0.last_mut()
    }

    #[allow(unused)]
    fn insert(&mut self, k: Ustr, v: SymbolId) {
        self.0.last_mut().unwrap().symbols.insert(k, v);
    }

    #[allow(unused)]
    fn lookup_depth(&self, k: Ustr) -> Option<(usize, &SymbolId)> {
        for (depth, scope) in self.0.iter().enumerate().rev() {
            if let Some(value) = scope.symbols.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    #[allow(unused)]
    fn lookup_depth_mut(&mut self, k: Ustr) -> Option<(usize, &mut SymbolId)> {
        for (depth, scope) in self.0.iter_mut().enumerate().rev() {
            if let Some(value) = scope.symbols.get_mut(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    fn lookup(&self, k: Ustr) -> Option<&SymbolId> {
        self.lookup_depth(k).map(|r| r.1)
    }

    #[allow(unused)]
    fn lookup_mut(&mut self, k: Ustr) -> Option<&mut SymbolId> {
        self.lookup_depth_mut(k).map(|r| r.1)
    }

    #[allow(unused)]
    fn depth(&self) -> usize {
        self.0.len()
    }

    fn in_kind(&self, kind: ScopeKind) -> bool {
        self.0.iter().any(|s| s.kind == kind)
    }
}

#[derive(Debug)]
struct Scope {
    pub kind: ScopeKind,
    pub name: QualifiedName,
    pub symbols: UstrMap<SymbolId>,
}

impl Scope {
    fn insert(&mut self, name: Ustr, id: SymbolId) {
        self.symbols.insert(name, id);
    }

    #[allow(unused)]
    fn get(&mut self, name: Ustr) -> Option<SymbolId> {
        self.symbols.get(&name).copied()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ScopeKind {
    Global,
    Fun,
    Block,
}

pub(super) enum ResolveError {
    MultipleItems { name: Ustr, prev_span: Span, dup_span: Span },
    NameNotFound(Word),
    MultipleNamedArgs { prev: Word, dup: Word },
    InvalidReturn { span: Span },
}

impl From<ResolveError> for Diagnostic {
    fn from(err: ResolveError) -> Self {
        match err {
            ResolveError::MultipleItems { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_items")
                    .with_message(format!("the item `{name}` is defined multiple times"))
                    .with_label(
                        Label::primary(dup_span)
                            .with_message(format!("`{name}` is redefined here")),
                    )
                    .with_label(
                        Label::secondary(prev_span)
                            .with_message(format!("previous item `{name}` defined here")),
                    )
                    .with_help("you can only define items once in a module")
            }
            ResolveError::MultipleNamedArgs { prev, dup } => {
                Self::error("resolve::multiple_named_args")
                    .with_message(format!("argument `{prev}` is passed multiple times"))
                    .with_label(
                        Label::primary(dup.span())
                            .with_message(format!("`{dup}` is redefined here")),
                    )
                    .with_label(
                        Label::secondary(prev.span())
                            .with_message(format!("argument `{prev}` is already passed here")),
                    )
            }
            ResolveError::NameNotFound(name) => Self::error("resolve::name_not_found")
                .with_message(format!("cannot find value `{name}` in this scope"))
                .with_label(Label::primary(name.span()).with_message("not found in this scope")),
            ResolveError::InvalidReturn { span } => Self::error("resolve::invalid_return")
                .with_message("cannot return outside of function scope")
                .with_label(Label::primary(span)),
        }
    }
}
