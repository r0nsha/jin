use ustr::{Ustr, UstrMap};

use crate::{
    common::{IdVec, Scopes},
    db::{
        self, Database, FunKind, ModuleId, ScopeLevel, Symbol, SymbolId, SymbolKind, TypeId, Vis,
    },
    diagnostics::{Diagnostic, Label},
    hir::*,
    span::Span,
};

pub(super) fn resolve(db: &mut Database, modules: &[Module]) {
    if let Err(err) = create_modules_and_global_scope(db, modules) {
        db.diagnostics.add(err.into());
    }
}

fn create_modules_and_global_scope(
    db: &mut Database,
    modules: &[Module],
) -> Result<(), ResolveError> {
    for module in modules {
        let mut global_bindings = UstrMap::default();
        let mut defined_names = UstrMap::<Span>::default();

        for binding in &module.bindings {
            let kind = match &binding.kind {
                BindingKind::Fun(fun) => {
                    SymbolKind::Fun(db::Fun::alloc(db, FunKind::Orphan, fun.span, fun.ty))
                }
            };

            let name = binding.name;
            let span = binding.span;

            if let Some(prev_span) = defined_names.insert(name, span) {
                return Err(ResolveError::DuplicateSymbol {
                    name,
                    prev_span,
                    dup_span: span,
                });
            } else {
                let binding_id = Symbol::alloc(
                    db,
                    module.id,
                    module.id.get(db).name.clone().child(binding.name),
                    Vis::Public,
                    ScopeLevel::Global,
                    kind,
                    TypeId::null(),
                    binding.span,
                );

                global_bindings.insert(name, binding_id);
            }
        }
    }

    Ok(())
}

#[derive(Debug)]
pub(crate) struct GlobalScope(IdVec<ModuleId, UstrMap<SymbolId>>);

impl GlobalScope {
    pub(crate) fn new() -> Self {
        Self(IdVec::new())
    }

    pub(crate) fn find_binding(&self, module_id: ModuleId, name: Ustr) -> Option<SymbolId> {
        self.0
            .get(module_id)
            .and_then(|bindings| bindings.get(&name))
            .copied()
    }
}

pub(crate) struct Env {
    module_id: ModuleId,
    pub(crate) scopes: Scopes<Ustr, SymbolId>,
    pub(crate) fun_scopes: FunScopes,
}

impl Env {
    pub(crate) fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            scopes: Scopes::new(),
            fun_scopes: FunScopes::new(),
        }
    }

    pub(crate) fn module_id(&self) -> ModuleId {
        self.module_id
    }
}

#[derive(Debug)]
pub(crate) struct FunScopes(Vec<FunScope>);

impl FunScopes {
    pub(crate) fn new() -> Self {
        Self(vec![])
    }

    pub(crate) fn push(&mut self, new_scope: FunScope) {
        self.0.push(new_scope);
    }

    pub(crate) fn pop(&mut self) {
        self.0.pop();
    }

    pub(crate) fn current(&self) -> Option<&FunScope> {
        self.0.last()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FunScope {
    pub(crate) ret_ty: TypeId,
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
