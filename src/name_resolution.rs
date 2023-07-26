use miette::Diagnostic;
use thiserror::Error;

use crate::{
    ast::*,
    span::{Span, Spanned},
    state::State,
    util::ErrExt,
    CompilerResult,
};

pub fn resolve(state: &State, module: Module) -> CompilerResult<Module> {
    Resolver::new(module)
        .resolve()
        .map_err(|err| err.with_source_code(state))
}

#[derive(Debug)]
struct Resolver {
    module: Module,
}

impl Resolver {
    fn new(module: Module) -> Self {
        Self { module }
    }
}

impl Resolver {
    fn resolve(mut self) -> ResolveResult<Module> {
        Ok(self.module)
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
