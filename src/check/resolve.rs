use miette::Diagnostic;
use slotmap::{Key, SecondaryMap};
use thiserror::Error;
use ustr::{Ustr, UstrMap};

use crate::{
    ast,
    hir::{self, BindingId, ModuleId},
    span::{Span, Spanned},
    CompilerResult,
};

use super::CheckContext;

pub(super) fn create_modules(
    cx: &mut CheckContext,
    modules: Vec<ast::Module>,
) -> ResolveResult<()> {
    for module in modules {
        let module_id = cx.cache.insert_module(hir::Module::from(&module));
        let module_full_name = cx.cache.get_module(module_id).unwrap().name().clone();

        let mut global_bindings = UstrMap::default();
        let mut defined_names = UstrMap::<Span>::default();

        for binding in module.bindings {
            let qualified_name = module_full_name.clone().child(binding.name());

            let ty = cx.typecx.fresh_var(binding.span);

            let binding_id = cx.cache.insert_binding_info(hir::BindingInfo {
                module_id,
                id: BindingId::null(),
                qualified_name,
                vis: ast::Vis::Public,
                scope: hir::BindingScope::Global,
                uses: 0,
                ty,
                span: binding.span,
            });

            let name = binding.name();
            let span = binding.span;

            if let Some(prev_span) = defined_names.insert(name, span) {
                return Err(ResolveError::DuplicateName {
                    name,
                    prev_span,
                    dup_span: span,
                });
            } else {
                global_bindings.insert(name, binding_id);
            }
        }

        cx.global_scope.0.insert(module_id, global_bindings);
    }

    Ok(())
}

pub(super) type ResolveResult<T> = CompilerResult<T, ResolveError>;

#[derive(Error, Diagnostic, Debug)]
pub(super) enum ResolveError {
    #[error("the name `{name}` is defined multiple times")]
    #[diagnostic(
        code(resolve::duplicate_names),
        help("you can only define `{name}` once in this module")
    )]
    DuplicateName {
        name: Ustr,
        #[label("`{name}` is already defined here")]
        prev_span: Span,
        #[label("`{name}` is defined again here")]
        dup_span: Span,
    },
}

impl Spanned for ResolveError {
    fn span(&self) -> Span {
        match self {
            ResolveError::DuplicateName { dup_span, .. } => *dup_span,
        }
    }
}

#[derive(Debug)]
pub struct GlobalScope(SecondaryMap<ModuleId, UstrMap<BindingId>>);

impl GlobalScope {
    pub fn new() -> Self {
        Self(SecondaryMap::new())
    }

    pub fn find_binding(&self, module_id: ModuleId, name: Ustr) -> Option<BindingId> {
        self.0
            .get(module_id)
            .and_then(|bindings| bindings.get(&name))
            .copied()
    }
}
