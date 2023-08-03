use std::collections::HashMap;

use ustr::{Ustr, UstrMap};

use crate::{
    common::Scopes,
    db::{
        self, Database, FunKind, ModuleId, ScopeLevel, Symbol, SymbolId, SymbolKind, TypeId, Vis,
    },
    diagnostics::{Diagnostic, Label},
    hir::*,
    span::Span,
};

pub(super) fn resolve(db: &mut Database, modules: &mut [Module]) {
    let mut cx = Resolve::new(db);

    cx.create_modules_and_global_scope(modules);

    if !cx.errors.is_empty() {
        let errors = cx.errors;
        db.diagnostics.extend(errors);
    }
}

struct Resolve<'a> {
    db: &'a mut Database,
    errors: Vec<ResolveError>,
    global_scope: GlobalScope,
}

impl<'a> Resolve<'a> {
    fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            errors: vec![],
            global_scope: GlobalScope::new(),
        }
    }

    fn create_modules_and_global_scope(&mut self, modules: &mut [Module]) {
        for module in modules {
            let mut scope_symbols = UstrMap::<SymbolId>::default();
            let mut defined_symbols = UstrMap::<Span>::default();

            for binding in &mut module.bindings {
                let kind = match &binding.kind {
                    BindingKind::Fun(fun) => SymbolKind::Fun(db::Fun::alloc(
                        &mut self.db,
                        FunKind::Orphan,
                        fun.span,
                        fun.ty,
                    )),
                };

                if let Some(&prev_span) = defined_symbols.get(&binding.name) {
                    self.errors.push(ResolveError::DuplicateSymbol {
                        name: binding.name,
                        prev_span,
                        dup_span: binding.span,
                    });
                } else {
                    defined_symbols.insert(binding.name, binding.span);
                }

                let qualified_name = module.id.get(&mut self.db).name.clone().child(binding.name);

                binding.id = Symbol::alloc(
                    &mut self.db,
                    module.id,
                    qualified_name,
                    Vis::Public,
                    ScopeLevel::Global,
                    kind,
                    TypeId::null(),
                    binding.span,
                );

                scope_symbols.insert(binding.name, binding.id);
            }

            self.global_scope.0.insert(module.id, scope_symbols);
        }
    }
}

#[derive(Debug)]
pub(crate) struct GlobalScope(HashMap<ModuleId, UstrMap<SymbolId>>);

impl GlobalScope {
    pub(crate) fn new() -> Self {
        Self(HashMap::new())
    }

    pub(crate) fn find_binding(&self, module_id: ModuleId, name: Ustr) -> Option<SymbolId> {
        self.0
            .get(&module_id)
            .and_then(|bindings| bindings.get(&name))
            .copied()
    }
}

pub(crate) struct Env {
    module_id: ModuleId,
    pub(crate) scopes: Scopes<Ustr, SymbolId>,
}

impl Env {
    pub(crate) fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            scopes: Scopes::new(),
        }
    }

    pub(crate) fn module_id(&self) -> ModuleId {
        self.module_id
    }
}

pub(super) enum ResolveError {
    DuplicateSymbol {
        name: Ustr,
        prev_span: Span,
        dup_span: Span,
    },
}

impl From<ResolveError> for Diagnostic {
    fn from(err: ResolveError) -> Self {
        match err {
            ResolveError::DuplicateSymbol {
                name,
                prev_span,
                dup_span,
            } => Diagnostic::error("check::duplicate_names")
                .with_message(format!("the name `{name}` is defined multiple times"))
                .with_label(
                    Label::secondary(prev_span)
                        .with_message(format!("previous definition of `{name}` is here")),
                )
                .with_label(
                    Label::primary(dup_span).with_message(format!("`{name}` is redefined here")),
                )
                .with_help("you can only define names once in a module"),
        }
    }
}
