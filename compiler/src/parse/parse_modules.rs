use std::cell::Ref;

use camino::Utf8PathBuf;
use rustc_hash::FxHashSet;
use ustr::Ustr;

use super::{lexer, parser};
use crate::{
    ast::{Ast, Module},
    db::{Db, ModuleInfo},
    diagnostics::DiagnosticResult,
    span::SourceId,
};

pub fn parse_module_tree(db: &mut Db) -> Ast {
    let mut ast = Ast::new();
    parse_module(db, db.main_package().name, &mut ast, db.main_source_id());
    ast
}

fn parse_module(
    db: &mut Db,
    package: Ustr,
    ast: &mut Ast,
    source_id: SourceId,
) {
    match parse_module_inner(db, package, source_id) {
        Ok((module, imported_module_paths)) => {
            ast.modules.push(module);

            for path in imported_module_paths {
                parse_module_from_path(db, path, ast);
            }
        }
        Err(diag) => db.diagnostics.emit(diag),
    }
}

fn parse_module_inner(
    db: &mut Db,
    package: Ustr,
    source_id: SourceId,
) -> DiagnosticResult<(Module, FxHashSet<Utf8PathBuf>)> {
    let (mut module, paths) = {
        let source =
            &Ref::map(db.sources.borrow(), |s| s.get(source_id).unwrap());
        let tokens = lexer::tokenize(source)?;
        parser::parse(db, package, source, tokens)?
    };

    module.id = ModuleInfo::alloc(
        db,
        package,
        module.source,
        module.name.clone(),
        module.is_main(),
    );

    Ok((module, paths))
}

fn parse_module_from_path(db: &mut Db, path: Utf8PathBuf, ast: &mut Ast) {
    if db.sources.borrow().find_by_path(&path).is_none() {
        let source_id = db
            .sources
            .borrow_mut()
            .load_file(path)
            .expect("import path to exist");

        let package = db
            .find_package_by_source_id(source_id)
            .expect("to be part of a package");

        parse_module(db, package.name, ast, source_id);
    }
}
