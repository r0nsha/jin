use miette::Diagnostic;
use thiserror::Error;

use crate::{
    ast::*,
    span::{Span, Spanned},
    state::State,
    util::ErrExt,
    CompilerResult,
};

pub fn resolve(state: &State, modules: Vec<Module>) -> CompilerResult<Module> {
    Resolver::new(modules)
        .resolve()
        .map_err(|err| err.with_source_code(state))
}

#[derive(Debug)]
struct Resolver {
    modules: Vec<Module>,
}

impl Resolver {
    fn new(module: Vec<Module>) -> Self {
        Self { modules: module }
    }
}

impl Resolver {
    fn resolve(mut self) -> ResolveResult<Module> {
        Ok(self.modules)
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
