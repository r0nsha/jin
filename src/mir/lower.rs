use crate::{db::Database, hir};

use super::*;

pub(crate) fn lower(db: &Database, hir: Vec<hir::Module>) -> Mir {
    let mut mir = Mir::new();

    for module in hir {}

    mir
}
