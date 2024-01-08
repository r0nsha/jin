mod errors;
mod import;
mod lexer;
mod parser;
mod tyexpr;

use std::cell::Ref;

use camino::{Utf8Path, Utf8PathBuf};
use rustc_hash::FxHashSet;
use ustr::{ustr, Ustr};

use crate::{
    ast::{Ast, Module},
    db::{Db, ModuleInfo},
    diagnostics::DiagnosticResult,
    span::SourceId,
};

pub fn parse_module_tree(
    db: &mut Db,
    root_file: &Utf8Path,
) -> anyhow::Result<Ast> {
    let mut ast = Ast::new();

    // Std
    let root_std_file = Utf8PathBuf::try_from(
        std::env::current_dir().unwrap().join("std/std.jin"),
    )
    .unwrap();
    let (std_package, _) = db.create_package(ustr("std"), &root_std_file)?;
    parse_package(db, &mut ast, std_package);

    // Main package
    let (main_package, _) =
        db.create_package(ustr(root_file.file_name().unwrap()), root_file)?;
    db.set_main_package(main_package);
    parse_package(db, &mut ast, main_package);

    Ok(ast)
}

fn parse_package(db: &mut Db, ast: &mut Ast, package: Ustr) {
    parse_module(db, package, ast, db.package(package).main_source_id);
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
        let package =
            db.find_package_by_path(&path).expect("to be part of a package");

        let source_id = db
            .sources
            .borrow_mut()
            .load_file(path)
            .expect("import path to exist");

        parse_module(db, package.name, ast, source_id);
    }
}
