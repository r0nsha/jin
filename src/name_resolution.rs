use miette::Diagnostic;
use slotmap::SlotMap;
use thiserror::Error;

use crate::{
    ast::*,
    span::{Span, Spanned},
    state::State,
    util::ErrExt,
    CompilerResult,
};

pub fn resolve(
    state: &State,
    modules: Vec<Module>,
) -> CompilerResult<SlotMap<ModuleId, ResolvedModule>> {
    Resolver::new(modules)
        .resolve()
        .map_err(|err| err.with_source_code(state))
}

#[derive(Debug)]
struct Resolver {
    modules: Vec<Module>,
    resolved_modules: SlotMap<ModuleId, ResolvedModule>,
}

impl Resolver {
    fn new(modules: Vec<Module>) -> Self {
        Self {
            modules,
            resolved_modules: SlotMap::with_key(),
        }
    }
}

impl Resolver {
    fn resolve(mut self) -> ResolveResult<SlotMap<ModuleId, ResolvedModule>> {
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
