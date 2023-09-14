use std::cell::Ref;

use super::{lexer, parser};
use crate::{
    ast::{Ast, Module},
    db::Db,
    diagnostics::Diagnostic,
    span::SourceId,
};

pub fn parse_module_tree(db: &mut Db) -> Ast {
    let mut ast = Ast::new();

    match parse_module(db, db.main_source_id()) {
        Ok(module) => ast.modules.push(module),
        Err(diag) => db.diagnostics.emit(diag),
    }

    ast
}

fn parse_module(db: &Db, source_id: SourceId) -> Result<Module, Diagnostic> {
    let source = &Ref::map(db.sources.borrow(), |s| s.get(source_id).unwrap());
    let tokens = lexer::tokenize(source)?;
    let module = parser::parse(db, source, tokens)?;
    Ok(module)
}
