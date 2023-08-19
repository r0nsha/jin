use std::collections::HashMap;

use ustr::{ustr, Ustr, UstrMap};

use crate::{
    common::QualifiedName,
    db::{Database, DefId, DefInfo, DefInfoKind, FunctionInfo, ModuleId, ScopeLevel, TyId, Vis},
    diagnostics::{Diagnostic, Label},
    hir::{Binary, Block, Call, Def, DefKind, Expr, Function, Hir, If, Module, Name, Return},
    span::Span,
};

pub fn resolve(db: &mut Database, hir: &mut Hir) {
    let mut cx = ResolveCx::new(db);

    cx.create_modules_and_resolve_globals(&mut hir.modules);
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

    fn create_modules_and_resolve_globals(&mut self, modules: &mut [Module]) {
        for module in modules {
            let mut scope_definitions = UstrMap::<DefId>::default();
            let mut already_defined = UstrMap::<Span>::default();

            for def in &mut module.definitions {
                if let Some(&prev_span) = already_defined.get(&def.name) {
                    self.errors.push(ResolveError::MultipleDefs {
                        name: def.name,
                        prev_span,
                        dup_span: def.span,
                    });
                } else {
                    already_defined.insert(def.name, def.span);
                }

                let id = self.declare_def(
                    def,
                    module.id,
                    self.db[module.id].name.clone(),
                    ScopeLevel::Global(Vis::Public),
                );

                scope_definitions.insert(def.name, id);
            }

            self.global_scope.0.insert(module.id, scope_definitions);
        }
    }

    fn resolve_all(&mut self, modules: &mut [Module]) {
        for module in modules {
            let mut env = Env::new(module.id);

            for def in &mut module.definitions {
                def.resolve(self, &mut env);
            }
        }
    }

    fn declare_def(
        &mut self,
        def: &mut Def,
        module_id: ModuleId,
        prefix: QualifiedName,
        scope_level: ScopeLevel,
    ) -> DefId {
        let kind = match &def.kind {
            DefKind::Function(_) => DefInfoKind::Function(FunctionInfo::Orphan),
        };

        let id = DefInfo::alloc(
            self.db,
            module_id,
            prefix.child(def.name),
            scope_level,
            kind,
            TyId::null(),
            def.span,
        );

        def.id = Some(id);

        match &mut def.kind {
            DefKind::Function(fun) => fun.id = def.id,
        }

        id
    }
}

trait Resolve<'db> {
    fn resolve(&mut self, cx: &mut ResolveCx<'db>, env: &mut Env);
}

impl Resolve<'_> for Def {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        let scope_level = env.scope_level(Vis::Private);

        if let Some(scope) = env.scopes.current_mut() {
            let id = cx.declare_def(self, env.module_id, scope.name.clone(), scope_level);
            scope.insert(self.name, id);
        }

        match &mut self.kind {
            DefKind::Function(fun) => fun.resolve(cx, env),
        }
    }
}

impl Resolve<'_> for Expr {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        match self {
            Self::Def(inner) => inner.resolve(cx, env),
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
        env.scopes.push_scope(self.name, ScopeKind::Fun);

        for param in self.params.values_mut() {
            let id = DefInfo::alloc(
                cx.db,
                env.module_id,
                param.name.into(),
                env.scope_level(Vis::Private),
                DefInfoKind::Variable,
                TyId::null(),
                param.span,
            );

            param.id = Some(id);
            env.scopes.insert(param.name, id);
        }

        self.body.resolve(cx, env);
        env.scopes.pop_scope();
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
        env.scopes.push_scope(ustr("_"), ScopeKind::Block);

        for expr in &mut self.exprs {
            expr.resolve(cx, env);
        }

        env.scopes.pop_scope();
    }
}

impl Resolve<'_> for Return {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        self.expr.resolve(cx, env);

        if !env.scopes.in_kind(&ScopeKind::Fun) {
            cx.errors.push(ResolveError::InvalidReturn { span: self.span });
        }
    }
}

impl Resolve<'_> for Call {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        self.callee.resolve(cx, env);
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
            .get_value(self.name)
            .copied()
            .or_else(|| cx.global_scope.find_definition(env.module_id, self.name));

        if self.id.is_none() {
            cx.errors.push(ResolveError::NameNotFound { name: self.name, span: self.span });
        }
    }
}

#[derive(Debug)]
pub struct GlobalScope(HashMap<ModuleId, UstrMap<DefId>>);

impl GlobalScope {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn find_definition(&self, module_id: ModuleId, name: Ustr) -> Option<DefId> {
        self.0.get(&module_id).and_then(|defs| defs.get(&name)).copied()
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
}

#[derive(Debug)]
struct Scopes(Vec<Scope>);

impl Scopes {
    fn new() -> Self {
        Self(vec![])
    }

    fn push_scope(&mut self, name: Ustr, kind: ScopeKind) {
        let name = self
            .current()
            .map_or_else(|| QualifiedName::from(name), |curr| curr.name.clone().child(name));

        self.0.push(Scope { kind, name, definitions: UstrMap::default() });
    }

    fn pop_scope(&mut self) {
        self.0.pop();
    }

    fn current(&self) -> Option<&Scope> {
        self.0.last()
    }

    #[allow(unused)]
    fn current_mut(&mut self) -> Option<&mut Scope> {
        self.0.last_mut()
    }

    #[allow(unused)]
    fn insert(&mut self, k: Ustr, v: DefId) {
        self.0.last_mut().unwrap().definitions.insert(k, v);
    }

    #[allow(unused)]
    fn get(&self, k: Ustr) -> Option<(usize, &DefId)> {
        for (depth, scope) in self.0.iter().enumerate().rev() {
            if let Some(value) = scope.definitions.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    #[allow(unused)]
    fn get_mut(&mut self, k: Ustr) -> Option<(usize, &mut DefId)> {
        for (depth, scope) in self.0.iter_mut().enumerate().rev() {
            if let Some(value) = scope.definitions.get_mut(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    #[allow(unused)]
    fn get_value(&self, k: Ustr) -> Option<&DefId> {
        self.get(k).map(|r| r.1)
    }

    #[allow(unused)]
    fn get_value_mut(&mut self, k: Ustr) -> Option<&mut DefId> {
        self.get_mut(k).map(|r| r.1)
    }

    #[allow(unused)]
    fn depth(&self) -> usize {
        self.0.len()
    }

    fn in_kind(&self, kind: &ScopeKind) -> bool {
        self.0.iter().any(|s| &s.kind == kind)
    }
}

#[derive(Debug)]
struct Scope {
    kind: ScopeKind,
    name: QualifiedName,
    definitions: UstrMap<DefId>,
}

impl Scope {
    fn insert(&mut self, name: Ustr, id: DefId) {
        self.definitions.insert(name, id);
    }

    #[allow(unused)]
    fn get(&mut self, name: Ustr) -> Option<DefId> {
        self.definitions.get(&name).copied()
    }
}

#[derive(Debug, PartialEq, Eq)]
enum ScopeKind {
    Fun,
    Block,
}

pub(super) enum ResolveError {
    MultipleDefs { name: Ustr, prev_span: Span, dup_span: Span },
    NameNotFound { name: Ustr, span: Span },
    InvalidReturn { span: Span },
}

impl From<ResolveError> for Diagnostic {
    fn from(err: ResolveError) -> Self {
        match err {
            ResolveError::MultipleDefs { name, prev_span, dup_span } => {
                Self::error("resolve::multiple_definitions")
                    .with_message(format!("the name `{name}` is defined multiple times"))
                    .with_label(
                        Label::primary(dup_span)
                            .with_message(format!("`{name}` is redefined here")),
                    )
                    .with_label(
                        Label::secondary(prev_span)
                            .with_message(format!("previous definition of `{name}` is here")),
                    )
                    .with_help("you can only define names once in a module")
            }
            ResolveError::NameNotFound { name, span } => Self::error("resolve::name_not_found")
                .with_message(format!("cannot find value `{name}` in this scope"))
                .with_label(Label::primary(span).with_message("not found in this scope")),
            ResolveError::InvalidReturn { span } => Self::error("resolve::invalid_return")
                .with_message("cannot return outside of function scope")
                .with_label(Label::primary(span)),
        }
    }
}
