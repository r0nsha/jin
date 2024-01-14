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

    fn collect_item(
        &mut self,
        module_id: ModuleId,
        item: &ast::Item,
        item_id: ast::ItemId,
    ) {
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
            ast::Item::Import(import) => {
                self.collect_import_name(module_id, item_id, &import.root);
            }
            ast::Item::ExternLet(let_) => {
                self.insert_item(module_id, let_.word, item_id);
            }
            ast::Item::ExternImport(_) | ast::Item::Associated(_, _) => (),
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
            None => self.insert_item(module_id, name.name(), item_id),
        }
    }

    fn collect_import_node(
        &mut self,
        module_id: ModuleId,
        item_id: ast::ItemId,
        node: &ast::ImportNode,
    ) {
        match node {
            ast::ImportNode::Name(name) => {
                self.collect_import_name(module_id, item_id, name);
            }
            ast::ImportNode::Group(nodes) => {
                for node in nodes {
                    self.collect_import_node(module_id, item_id, node);
                }
            }
            ast::ImportNode::Glob(..) => (),
        }
    }

    fn insert_item(
        &mut self,
        module_id: ModuleId,
        word: Word,
        item_id: ast::ItemId,
    ) {
        self.global_scope
            .symbol_to_item
            .entry(Symbol::new(module_id, word.name()))
            .or_default()
            .push(item_id);
    }
}
