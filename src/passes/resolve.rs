use std::collections::HashMap;

use ustr::{Ustr, UstrMap};

use crate::{
    db::{
        Database, DefinitionId, DefinitionInfo, DefinitionInfoKind, FunctionInfo, ModuleId,
        ScopeLevel, TyId, Vis,
    },
    diagnostics::{Diagnostic, Label},
    hir::{
        Binary, Block, Call, Definition, DefinitionKind, Expr, Function, Hir, If, Module, Name,
        Return,
    },
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
            let mut scope_definitions = UstrMap::<DefinitionId>::default();
            let mut already_defined = UstrMap::<Span>::default();

            for def in &mut module.definitions {
                if let Some(&prev_span) = already_defined.get(&def.name) {
                    self.errors.push(ResolveError::MultipleDefinitions {
                        name: def.name,
                        prev_span,
                        dup_span: def.span,
                    });
                } else {
                    already_defined.insert(def.name, def.span);
                }

                let qualified_name = self.db[module.id].name.clone().child(def.name);

                let kind = match &def.kind {
                    DefinitionKind::Function(_) => {
                        DefinitionInfoKind::Function(FunctionInfo::Orphan)
                    }
                };

                let id = DefinitionInfo::alloc(
                    self.db,
                    module.id,
                    qualified_name,
                    ScopeLevel::Global(Vis::Public),
                    kind,
                    TyId::null(),
                    def.span,
                );

                match &mut def.kind {
                    DefinitionKind::Function(fun) => fun.id = Some(id),
                }

                def.id = Some(id);

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
}

trait Resolve<'db> {
    fn resolve(&mut self, cx: &mut ResolveCx<'db>, env: &mut Env);
}

impl Resolve<'_> for Definition {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        if self.id.is_some() {
            match &mut self.kind {
                DefinitionKind::Function(fun) => fun.resolve(cx, env),
            }
        } else {
            todo!("local defs");
        }
    }
}

impl Resolve<'_> for Expr {
    fn resolve(&mut self, cx: &mut ResolveCx<'_>, env: &mut Env) {
        match self {
            Self::Function(inner) => inner.resolve(cx, env),
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
        env.scopes.push_scope(ScopeKind::Fun);
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
        env.scopes.push_scope(ScopeKind::Block);

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
        if let Some(id) = cx.global_scope.find_definition(env.module_id, self.name) {
            self.id = Some(id);
        } else {
            cx.errors.push(ResolveError::NameNotFound { name: self.name, span: self.span });
        }
    }
}

#[derive(Debug)]
pub struct GlobalScope(HashMap<ModuleId, UstrMap<DefinitionId>>);

impl GlobalScope {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn find_definition(&self, module_id: ModuleId, name: Ustr) -> Option<DefinitionId> {
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
}

#[derive(Debug)]
struct Scopes(Vec<Scope>);

impl Scopes {
    fn new() -> Self {
        Self(vec![])
    }

    fn push_scope(&mut self, kind: ScopeKind) {
        self.0.push(Scope { kind, definitions: UstrMap::default() });
    }

    fn pop_scope(&mut self) {
        self.0.pop();
    }

    #[allow(unused)]
    fn insert(&mut self, k: Ustr, v: DefinitionId) {
        self.0.last_mut().unwrap().definitions.insert(k, v);
    }

    #[allow(unused)]
    fn get(&self, k: Ustr) -> Option<(usize, &DefinitionId)> {
        for (depth, scope) in self.0.iter().enumerate().rev() {
            if let Some(value) = scope.definitions.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    #[allow(unused)]
    fn get_mut(&mut self, k: Ustr) -> Option<(usize, &mut DefinitionId)> {
        for (depth, scope) in self.0.iter_mut().enumerate().rev() {
            if let Some(value) = scope.definitions.get_mut(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    #[allow(unused)]
    fn get_value(&self, k: Ustr) -> Option<&DefinitionId> {
        self.get(k).map(|r| r.1)
    }

    #[allow(unused)]
    fn get_value_mut(&mut self, k: Ustr) -> Option<&mut DefinitionId> {
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
    definitions: UstrMap<DefinitionId>,
}

#[derive(Debug, PartialEq, Eq)]
enum ScopeKind {
    Fun,
    Block,
}

pub(super) enum ResolveError {
    MultipleDefinitions { name: Ustr, prev_span: Span, dup_span: Span },
    NameNotFound { name: Ustr, span: Span },
    InvalidReturn { span: Span },
}

impl From<ResolveError> for Diagnostic {
    fn from(err: ResolveError) -> Self {
        match err {
            ResolveError::MultipleDefinitions { name, prev_span, dup_span } => {
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
