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
                match item {
                    ast::Item::Fn(fun) => self.collect_item(module.id, fun.sig.word, item_id),
                    ast::Item::Let(let_) => {
                        let_.pat.walk(|p| self.collect_item(module.id, p.word, item_id));
                    }
                    ast::Item::Type(tydef) => self.collect_item(module.id, tydef.word, item_id),
                    ast::Item::Import(import) => {
                        self.collect_import_name(module.id, item_id, &import.root);
                    }
                    ast::Item::ExternLet(let_) => self.collect_item(module.id, let_.word, item_id),
                    ast::Item::ExternImport(_) => (),
                }
            }
        }
    }

    fn collect_import_name(
        &mut self,
        module_id: ModuleId,
        item_id: ast::ItemId,
        name: &ast::ImportName,
    ) {
        match &name.node {
            Some(node) => self.collect_import_node(module_id, item_id, node),
            None => self.collect_item(module_id, name.name(), item_id),
        }
    }

    fn collect_import_node(
        &mut self,
        module_id: ModuleId,
        item_id: ast::ItemId,
        node: &ast::ImportNode,
    ) {
        match node {
            ast::ImportNode::Name(name) => self.collect_import_name(module_id, item_id, name),
            ast::ImportNode::Group(nodes) => {
                for node in nodes {
                    self.collect_import_node(module_id, item_id, node);
                }
            }
            ast::ImportNode::Glob(_) => (),
        }
    }

    fn collect_item(&mut self, module_id: ModuleId, word: Word, item_id: ast::ItemId) {
        self.global_scope
            .symbol_to_item
            .entry(Symbol::new(module_id, word.name()))
            .or_default()
            .push(item_id);
    }
}
