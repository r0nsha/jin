use crate::{db::Db, hir::Hir};

pub fn monomorphization(db: &mut Db, hir: &mut Hir) {
    // TODO: collect mono items
    // TODO: find polymorphic function uses in mono items, recursively, marking visited items to
    //       not visit them twice
    // TODO: generate the appropriate definition for each use
    // TODO: replace the used places with the new definition
}

