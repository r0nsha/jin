use crate::{
    ast,
    ast::Ast,
    diagnostics::DiagnosticResult,
    typeck2::{attrs, Typeck},
};

pub(super) fn define_extern_imports(cx: &mut Typeck, ast: &Ast) -> DiagnosticResult<()> {
    for (_, item) in ast.items() {
        if let ast::Item::ExternImport(import) = item {
            attrs::validate(&import.attrs, attrs::Placement::ExternImport)?;
            cx.db.extern_libs.insert(import.lib.clone());
        }
    }

    Ok(())
}
