use std::collections::HashSet;

use crate::{
    db::{Db, DefId},
    hir::{Expr, ExprKind, Fn, Hir, Item, ItemKind},
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
            self.collect_uses_in_item(item, &Instantiation::new());
        }

        self
    }

    fn collect_poly_item(&mut self, id: DefId, instantiation: &Instantiation) {
        let item = self
            .hir
            .items
            .iter()
            .find(|item| match &item.kind {
                ItemKind::Fn(f) => f.id == id,
            })
            .expect("item to exist");

        self.collect_uses_in_item(item, instantiation);
    }

    fn collect_def_use(&mut self, id: DefId, ty: Ty) {
        self.mono_items.insert(MonoItem { id, ty });
    }

    fn collect_uses_in_item(&mut self, item: &Item, instantiation: &Instantiation) {
        match &item.kind {
            ItemKind::Fn(f) => {
                if !f.ty.is_polymorphic() {
                    self.collect_uses_in_fn(f, instantiation);
                }
            }
        }
    }

    fn collect_uses_in_fn(&mut self, f: &Fn, instantiation: &Instantiation) {
        for p in &f.sig.params {
            if p.ty.is_polymorphic() {
                let ty = ParamFolder { db: self.db, instantiation }.fold(p.ty);
                self.collect_def_use(p.id, ty);
            }
        }

        self.collect_uses(&f.body, instantiation);
    }

    fn collect_uses(&mut self, expr: &Expr, instantiation: &Instantiation) {
        expr.walk(|expr| {
            if let ExprKind::Name(name) = &expr.kind {
                if !name.instantiation.is_empty() {
                    let mut folder = ParamFolder { db: self.db, instantiation };
                    let ty = folder.fold(expr.ty);
                    let args: Instantiation = name
                        .instantiation
                        .iter()
                        .map(|(var, ty)| (*var, folder.fold(*ty)))
                        .collect();
                    self.collect_def_use(name.id, ty);
                    self.collect_poly_item(name.id, &args);
                }
            }
        });
    }
}
