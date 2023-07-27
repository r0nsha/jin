use miette::Diagnostic;
use thiserror::Error;

use crate::{
    ast::*,
    span::{Span, Spanned},
    state::State,
    util::ErrExt,
    CompilerResult,
};

pub fn resolve(state: &State, modules: Vec<Module>) -> CompilerResult<ResolvedModules> {
    Resolver::new()
        .resolve(modules)
        .map_err(|err| err.with_source_code(state))
}

#[derive(Debug)]
struct Resolver {
    resolved_modules: ResolvedModules,
}

impl Resolver {
    fn new() -> Self {
        Self {
            resolved_modules: ResolvedModules::new(),
        }
    }

    fn resolve(mut self, modules: Vec<Module>) -> ResolveResult<ResolvedModules> {
        for module in modules {
            self.resolved_modules
                .insert_module(ResolvedModule::from(module));
        }

        for module in self.resolved_modules.iter_mut() {
            // TODO: add all global names to binding_infos
        }

        // TODO: resolve all names
        for module in self.resolved_modules.iter_mut() {
            // TODO: add all global names to binding_infos
            //     for binding in &mut module.bindings {
            //         self.resolve_binding(binding);
            //     }
        }

        Ok(self.resolved_modules)
    }

    fn resolve_ast(&mut self, ast: &mut Ast) {
        match ast {
            Ast::Binding(binding) => self.resolve_binding(binding),
            Ast::Fun(fun) => self.resolve_fun(fun),
            Ast::Ret(ret) => {
                if let Some(value) = ret.value.as_mut() {
                    self.resolve_ast(value);
                }
            }
            _ => (),
        }
    }

    fn resolve_binding(&mut self, binding: &mut Binding) {
        match &mut binding.kind {
            BindingKind::Fun { name, fun } => self.resolve_fun(fun),
        }
    }

    fn resolve_fun(&mut self, fun: &mut Fun) {
        todo!()
    }
}

type ResolveResult<T> = CompilerResult<T, ResolveError>;

#[derive(Error, Diagnostic, Debug)]
enum ResolveError {
    // #[error("expected `{expected}`, got `{actual}` instead")]
    // #[diagnostic(code(parse::expected_token))]
    // ExpectedToken {
    //     expected: TokenKind,
    //     actual: TokenKind,
    //     #[label("found `{actual}` here")]
    //     span: Span,
    // },
}

impl Spanned for ResolveError {
    fn span(&self) -> Span {
        match self {
            _ => todo!(),
        }
    }
}
