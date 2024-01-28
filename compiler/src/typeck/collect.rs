use crate::{
    ast,
    db::ModuleId,
    typeck::{env::Symbol, Typeck},
    word::Word,
};

impl<'db> Typeck<'db> {
    pub(super) fn collect_items(&mut self) {
        for module in &self.ast.modules {
            for (item_id, item) in module.items.iter_enumerated() {
                self.collect_item(module.id, item, item_id);
            }
        }
    }

    fn collect_item(&mut self, module_id: ModuleId, item: &ast::Item, item_id: ast::ItemId) {
        match item {
            ast::Item::Fn(fun) => {
                self.insert_item(module_id, fun.sig.word, item_id);
            }
            ast::Item::Let(let_) => {
                let_.pat.walk(|p| {
                    self.insert_item(module_id, p.word, item_id);
                });
            }
            ast::Item::Type(tydef) => {
                self.insert_item(module_id, tydef.word, item_id);
            }
            ast::Item::Import(import) => match &import.kind {
                ast::ImportKind::Qualified(alias, _) => {
                    self.collect_name_or_alias(
                        module_id,
                        item_id,
                        *import.path.last().unwrap(),
                        *alias,
                    );
                }
                ast::ImportKind::Unqualified(imports) => {
                    for import in imports {
                        if let ast::UnqualifiedImport::Name(name, alias, _) = import {
                            self.collect_name_or_alias(module_id, item_id, *name, *alias);
                        }
                    }
                }
            },
            ast::Item::ExternLet(let_) => {
                self.insert_item(module_id, let_.word, item_id);
            }
            ast::Item::ExternImport(_) => (),
            ast::Item::Assoc(_, item) => {
                self.collect_item(module_id, item, item_id);
            }
        }
    }

    fn collect_name_or_alias(
        &mut self,
        module_id: ModuleId,
        item_id: ast::ItemId,
        name: Word,
        alias: Option<Word>,
    ) {
        let name = alias.unwrap_or(name);
        self.insert_item(module_id, name, item_id);
    }

    fn insert_item(&mut self, module_id: ModuleId, word: Word, item_id: ast::ItemId) {
        self.global_scope
            .symbol_to_item
            .entry(Symbol::new(module_id, word.name()))
            .or_default()
            .push(item_id);
    }
}
