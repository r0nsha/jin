mod define_extern_imports;

use crate::{
    ast::Ast, db::Db, diagnostics::DiagnosticResult, hir::Hir,
    typeck2::define_extern_imports::define_extern_imports,
};

pub fn typeck(db: &mut Db, ast: Ast) -> DiagnosticResult<Hir> {
    let mut cx = Typeck::new(db, ast);
    define_extern_imports(&mut cx);
    Ok(cx.hir)
}

pub(super) struct Typeck<'db> {
    db: &'db mut Db,
    ast: Ast,
    hir: Hir,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db, ast: Ast) -> Self {
        Self { db, ast, hir: Hir::new() }
    }
}
