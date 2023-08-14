use crate::{
    ast::{Library, Module},
    db::Database,
    diagnostics::Diagnostic,
    span::Source,
};

use super::{lexer, parser};

pub(crate) fn parse_modules(db: &mut Database) -> Library {
    let mut modules = vec![];

    match parse_module(db, db.main_source()) {
        Ok(module) => modules.push(module),
        Err(diag) => db.diagnostics.add(diag),
    }

    let lib_name = modules
        .iter()
        .find(|m| m.is_main())
        .expect("to have a main module")
        .name
        .name();

    Library::new(lib_name, true, modules)
}

fn parse_module(db: &Database, source: &Source) -> Result<Module, Diagnostic> {
    let tokens = lexer::tokenize(source)?;
    let module = parser::parse(db, source, tokens)?;
    Ok(module)
}
