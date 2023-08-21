use super::{lexer, parser};
use crate::{
    ast::{Ast, Module},
    db::Database,
    diagnostics::Diagnostic,
    span::Source,
};

pub fn parse_modules(db: &mut Database) -> Ast {
    let mut ast = Ast::new();

    match parse_module(db, db.main_source()) {
        Ok(module) => ast.modules.push(module),
        Err(diag) => db.diagnostics.add(diag),
    }

    ast
}

fn parse_module(db: &Database, source: &Source) -> Result<Module, Diagnostic> {
    let tokens = lexer::tokenize(source)?;
    let module = parser::parse(db, source, tokens)?;
    Ok(module)
}
