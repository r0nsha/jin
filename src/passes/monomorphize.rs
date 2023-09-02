use std::collections::HashSet;

use crate::{
    db::{Db, DefId},
    hir::{visit::*, Fn, Hir, Item, ItemKind, Name},
    passes::subst::ParamFolder,
    ty::{fold::TyFolder, Instantiation, Ty},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonoItem {
    pub id: DefId,
    pub ty: Ty,
}

pub fn monomorphize(db: &mut Db, hir: &Hir) -> HashSet<MonoItem> {
    Collector::new(db, hir).collect_roots().mono_items
}

struct Collector<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    mono_items: HashSet<MonoItem>,
}

impl<'db> Collector<'db> {
    fn new(db: &'db mut Db, hir: &'db Hir) -> Self {
        Self { db, hir, mono_items: HashSet::new() }
    }

    fn collect_roots(mut self) -> Self {
        for item in &self.hir.items {
            match &item.kind {
                ItemKind::Fn(f) => {
                    if !f.ty.is_polymorphic() {
                        PolyCollector { root: &mut self, instantiation: Instantiation::new() }
                            .visit_fn(f);
                    }
                }
            }
        }

        self
    }

    fn collect_poly_item(&mut self, id: DefId, instantiation: Instantiation) {
        // TODO: this doesn't work with local items
        let item = self
            .hir
            .items
            .iter()
            .find(|item| match &item.kind {
                ItemKind::Fn(f) => f.id == id,
            })
            .expect("item to exist");

        PolyCollector { root: self, instantiation }.visit_item(item);
    }

    fn collect_def_use(&mut self, id: DefId, ty: Ty) {
        self.mono_items.insert(MonoItem { id, ty });
    }
}

struct PolyCollector<'db, 'a> {
    root: &'a mut Collector<'db>,
    instantiation: Instantiation,
}

impl HirVisitor for PolyCollector<'_, '_> {
    fn visit_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Fn(f) => {
                if !f.ty.is_polymorphic() {
                    PolyCollector { root: self.root, instantiation: Instantiation::new() }
                        .visit_fn(f);
                }
            }
        }
    }

    fn visit_fn(&mut self, f: &Fn) {
        noop_visit_fn(self, f);

        for p in &f.sig.params {
            if p.ty.is_polymorphic() {
                dbg!(&p.ty);
                dbg!(&self.instantiation);
                let ty =
                    ParamFolder { db: self.root.db, instantiation: &self.instantiation }.fold(p.ty);

                self.root.collect_def_use(p.id, ty);
            }
        }
    }

    fn visit_name(&mut self, name: &Name) {
        noop_visit_name(self, name);

        if !name.instantiation.is_empty() {
            let mut folder = ParamFolder { db: self.root.db, instantiation: &self.instantiation };
            let ty = folder.fold(name.ty);
            let args: Instantiation =
                name.instantiation.iter().map(|(var, ty)| (*var, folder.fold(*ty))).collect();
            self.root.collect_def_use(name.id, ty);
            self.root.collect_poly_item(name.id, args);
        }
    }
}
