use std::{collections::HashSet, hash::Hash};

use crate::{
    db::{Db, DefId},
    hir::{visit::*, Fn, Hir, ItemKind, Name},
    passes::subst::ParamFolder,
    ty::{fold::TyFolder, Ty},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MonoItem {
    pub id: DefId,
    pub ty: Ty,        // Used for identifying a polymorphic item use, paired with `id`
    pub args: Vec<Ty>, // Used to propogate the ty args downwards
}

impl Hash for MonoItem {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.ty.kind().hash(state);
    }
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
                        PolyCollector { root: &mut self, args: vec![] }.visit_fn(f);
                    }
                }
            }
        }

        self
    }

    fn collect_poly_item(&mut self, id: DefId, args: Vec<Ty>) {
        // TODO: this doesn't work with local items
        let item = self.hir.items.iter().find(|item| match &item.kind {
            ItemKind::Fn(f) => f.id == id,
        });

        if let Some(item) = item {
            PolyCollector { root: self, args }.visit_item(item);
        }
    }

    fn collect_def_use(&mut self, id: DefId, args: Vec<Ty>, ty: Ty) {
        self.mono_items.insert(MonoItem { id, ty, args });
    }
}

struct PolyCollector<'db, 'a> {
    root: &'a mut Collector<'db>,
    args: Vec<Ty>,
}

impl HirVisitor for PolyCollector<'_, '_> {
    fn visit_fn(&mut self, f: &Fn) {
        noop_visit_fn(self, f);

        for p in &f.sig.params {
            if p.ty.is_polymorphic() {
                let ty = ParamFolder { db: self.root.db, args: &self.args }.fold(p.ty);
                self.root.collect_def_use(p.id, vec![], ty);
            }
        }
    }

    fn visit_name(&mut self, name: &Name) {
        noop_visit_name(self, name);

        if !name.args.is_empty() {
            let mut folder = ParamFolder { db: self.root.db, args: &self.args };
            let ty = folder.fold(name.ty);
            let args: Vec<_> = name.args.iter().map(|arg| folder.fold(*arg)).collect();
            self.root.collect_def_use(name.id, args.clone(), ty);
            self.root.collect_poly_item(name.id, args);
        }
    }
}
