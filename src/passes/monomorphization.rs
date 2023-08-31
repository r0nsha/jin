use crate::{db::Db, hir::Hir};

pub fn monomorphization(db: &mut Db, hir: &mut Hir) {
    // TODO: collect mono items
    // TODO: find polymorphic function uses in mono items
    // TODO: generate the appropriate "hir",
}

