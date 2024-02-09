use crate::{ast::Ast, db::Db, diagnostics::DiagnosticResult, hir::Hir};

pub fn typeck(db: &mut Db, ast: Ast) -> DiagnosticResult<Hir> {
    let mut cx = Typeck::new(db, ast);
    todo!();
    Ok(cx.hir)
}

struct Typeck<'db> {
    db: &'db mut Db,
    ast: Ast,
    hir: Hir,
}

impl<'db> Typeck<'db> {
    fn new(db: &'db mut Db, ast: Ast) -> Self {
        Self { db, ast, hir: Hir::new() }
    }
}
