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
    Resolver::new(modules)
        .resolve()
        .map_err(|err| err.with_source_code(state))
}

#[derive(Debug)]
struct Resolver {
    modules: Vec<Module>,
    resolved_modules: ResolvedModules,
}

impl Resolver {
    fn new(modules: Vec<Module>) -> Self {
        Self {
            modules,
            resolved_modules: ResolvedModules::new(),
        }
    }
}

impl Resolver {
    fn resolve(mut self) -> ResolveResult<ResolvedModules> {
        Ok(self.resolved_modules)
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
