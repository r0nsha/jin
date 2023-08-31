use crate::{
    db::{Db, DefId},
    hir::{Fn, Hir, ItemKind},
    ty::Ty,
};

// TODO: find polymorphic function uses in roots, recursively
// TODO: collect Vec<MonoItem>
// TODO: place generated mono defs where needed
pub fn monomorphize(db: &mut Db, hir: &Hir) {
    Context::new(db).monomorphize(hir);
}

struct Context<'db> {
    db: &'db mut Db,
    mono_items: Vec<MonoItem>,
}

struct MonoItem {
    id: DefId,
    args: Vec<Ty>,
}

impl<'db> Context<'db> {
    fn new(db: &'db mut Db) -> Self {
        Self { db, mono_items: vec![] }
    }

    fn monomorphize(&mut self, hir: &Hir) {
        for item in &hir.items {
            match &item.kind {
                ItemKind::Fn(f) => {
                    if f.ty.is_polymorphic() {
                        self.monomorphize_root_fn(f);
                    }
                }
            }
        }
    }

    fn monomorphize_root_fn(&self, f: &Fn) {}
}
