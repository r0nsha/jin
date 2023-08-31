use crate::{
    db::{Db, DefId},
    hir::{Fn, Hir, Item, ItemKind},
    ty::Ty,
};

// TODO: find polymorphic function uses in roots, recursively
// TODO: collect Vec<MonoItem>
// TODO: place generated mono defs where needed
pub fn monomorphize(db: &mut Db, hir: &Hir) -> Vec<MonoItem> {
    let mut cx = Context::new(db);
    cx.monomorphize(hir);
    cx.mono_items
}

struct Context<'db> {
    db: &'db mut Db,
    mono_items: Vec<MonoItem>,
}

#[derive(Debug)]
pub struct MonoItem {
    id: DefId,
    args: Vec<Ty>,
}

impl<'db> Context<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, mono_items: vec![] }
    }

    fn monomorphize(&mut self, hir: &Hir) {
        for item in &hir.items {
            self.monomorphize_item(item);
        }
    }

    fn monomorphize_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Fn(f) => {
                if f.ty.is_polymorphic() {
                    self.monomorphize_fn(f);
                }
            }
        }
    }

    fn monomorphize_fn(&self, f: &Fn) {}
}
