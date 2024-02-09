use crate::{ast, typeck2::Typeck};

pub(super) fn define_extern_imports(cx: &mut Typeck) {
    for module in &cx.ast.modules {
        for item in &module.items {
            if let ast::Item::ExternImport(import) = item {
                cx.db.extern_libs.insert(import.lib.clone());
            }
        }
    }
}
