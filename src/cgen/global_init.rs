use rustc_hash::FxHashMap;

use crate::{cgen::generate::Generator, mir::GlobalId};

impl<'db> Generator<'db> {
    pub fn get_global_init_order(&self) -> Vec<GlobalId> {
        GlobalInitOrder { cx: self, states: FxHashMap::default(), order: vec![] }.get()
    }
}

struct GlobalInitOrder<'db, 'a> {
    cx: &'a Generator<'db>,
    states: FxHashMap<GlobalId, GlobalInitState>,
    order: Vec<GlobalId>,
}

#[derive(Debug)]
struct GlobalInitState {
    pub fn_name: String,
    pub was_init: bool,
}

impl GlobalInitOrder<'_, '_> {
    fn get(mut self) -> Vec<GlobalId> {
        self.order
    }
}
