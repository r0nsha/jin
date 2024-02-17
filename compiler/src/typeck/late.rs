mod check_bodies;
mod check_main;
mod cyclic_globals;
mod leaky_items;

use crate::{db::Db, hir::Hir};

pub fn checks(db: &mut Db, hir: &mut Hir) {
    cyclic_globals::cyclic_globals(db, hir);
    leaky_items::leaky_items(db, hir);
    check_main::check_main(db, hir);
    check_bodies::check_bodies(db, hir);
}
