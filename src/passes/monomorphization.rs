use crate::{
    db::{Db, DefId},
    hir::{Hir, ItemKind},
    ty::Ty,
};

pub fn monomorphization(db: &mut Db, hir: &mut Hir) {
    // TODO: collect mono items
    // TODO: find polymorphic function uses in mono items, recursively, marking visited items to
    //       not visit them twice
    // TODO: generate the appropriate definition for each use
    // TODO: place generated mono defs where needed
}

fn collect_mono_items(hir: &Hir) -> Vec<DefId> {
    let mut defs = vec![];

    for item in &hir.items {
        match &item.kind {
            ItemKind::Fn(f) => if f.ty.is_polymorphic() {},
        }
    }

    defs
}

struct Collector<'db> {
    db: &'db mut Db,
    defs: Vec<DefId>,
}
