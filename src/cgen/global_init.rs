use crate::{
    cgen::generate::Generator,
    mir::{Global, GlobalId, GlobalKind, ValueKind},
};

impl<'db> Generator<'db> {
    pub fn get_global_init_order(&self) -> Vec<GlobalId> {
        GlobalInitOrder { cx: self, order: vec![] }.get()
    }
}

struct GlobalInitOrder<'db, 'a> {
    cx: &'a Generator<'db>,
    order: Vec<GlobalId>,
}

impl GlobalInitOrder<'_, '_> {
    fn get(mut self) -> Vec<GlobalId> {
        for glob in self.cx.mir.globals.values() {
            self.search_global(glob);
            if let GlobalKind::Static(..) = &glob.kind {
                self.add_global(glob.id);
            }
        }

        self.order
    }

    fn search_global(&mut self, glob: &Global) {
        if let GlobalKind::Static(body, _) = &glob.kind {
            for value in body.values() {
                if let ValueKind::Global(gid) = &value.kind {
                    if !self.add_global(*gid) {
                        self.search_global_by_id(*gid);
                    }
                }
            }
        }
    }

    fn search_global_by_id(&mut self, id: GlobalId) {
        self.search_global(&self.cx.mir.globals[id]);
    }

    fn add_global(&mut self, id: GlobalId) -> bool {
        if self.order.contains(&id) {
            false
        } else {
            self.order.push(id);
            true
        }
    }
}
