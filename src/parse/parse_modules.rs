use crate::{db::Database, diagnostics::Diagnostic, span::Source};

use super::{ast::Module, parser, tokenize::tokenize};

pub(crate) fn parse_modules(db: &mut Database) -> Vec<Module> {
    let mut modules = vec![];

    match parse_module(db, db.main_source()) {
        Ok(module) => modules.push(module),
        Err(diag) => db.diagnostics.add(diag),
    }

    modules
}

fn parse_module(db: &Database, source: &Source) -> Result<Module, Diagnostic> {
    let tokens = tokenize(source)?;
    let module = parser::parse(&db, source, tokens)?;
    Ok(module)
}
