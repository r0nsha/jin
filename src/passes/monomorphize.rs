use std::collections::HashSet;

use crate::{
    db::{Db, DefId},
    hir::{Fn, Hir, HirVisitor, Item, ItemKind, Name},
    passes::subst::SubstTy,
    span::Span,
    ty::{fold::TyFolder, Ty, TyKind},
};

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MonoItem {
    pub id: DefId,
    pub args: Vec<Ty>,
}

pub fn monomorphize(db: &mut Db, hir: &Hir) -> HashSet<MonoItem> {
    Collector::new(db, hir).collect().mono_items
}

struct Collector<'db> {
    db: &'db mut Db,
    hir: &'db Hir,
    mono_items: HashSet<MonoItem>,
}

impl<'db> Collector<'db> {
    fn new(db: &'db mut Db, hir: &Hir) -> Self {
        Self { db, hir, mono_items: HashSet::new() }
    }

    fn collect(mut self) -> Self {
        for item in &self.hir.items {
            self.collect_root_item(item);
        }

        self
    }

    fn collect_root_item(&mut self, item: &Item) {
        match &item.kind {
            ItemKind::Fn(fun) => {
                if !fun.ty.is_polymorphic() {
                    self.collect_root_fn(fun);
                }
            }
        }
    }

    fn collect_poly_item(&mut self, id: DefId) {
        let item = self.hir.items.iter().find(|item| match &item.kind {
            ItemKind::Fn(f) => f.id == id,
        });

        match &item.kind {
            ItemKind::Fn(fun) => {
                if !fun.ty.is_polymorphic() {
                    self.collect_root_fn(fun);
                }
            }
        }
    }

    fn collect_root_fn(&mut self, fun: &Fn) {
        for param in &fun.sig.params {
            self.collect_def_use(param.id, vec![]);
        }
    }

    fn collect_def_use(&mut self, id: DefId, args: Vec<Ty>) {
        self.mono_items.insert(MonoItem { args, id });
    }
}

impl HirVisitor for Collector<'_> {
    fn visit_fn(&mut self, f: &Fn) {
        for p in &f.sig.params {
            if p.ty.is_polymorphic() {
                self.collect_def_use(p.id, vec![]);
            }
        }
    }

    fn visit_name(&mut self, name: &Name) {
        if !name.args.is_empty() {
            self.collect_def_use(name.id, name.args.clone());
        }
    }
}

pub struct ParamFolder<'db, 'a> {
    db: &'db mut Db,
    args: &'a [Ty],
}

impl SubstTy for ParamFolder<'_, '_> {
    fn subst_ty(&mut self, ty: Ty, _: Span) -> Ty {
        self.fold(ty)
    }

    fn db(&mut self) -> &mut Db {
        self.db
    }
}

impl TyFolder for ParamFolder<'_, '_> {
    fn fold(&mut self, ty: Ty) -> Ty {
        match ty.kind() {
            TyKind::Param(p) => self.args[p.index],
            _ => self.super_fold(ty),
        }
    }
}
