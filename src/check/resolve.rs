use miette::Diagnostic;
use thiserror::Error;

use crate::{
    ast, hir,
    span::{Span, Spanned},
    CompilerResult,
};

pub fn create_modules(cache: &mut hir::Cache, modules: Vec<ast::Module>) {
    for module in modules {
        cache.insert_module(hir::Module::from(module));
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
