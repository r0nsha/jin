use crate::{
    cgen::generate::Generator,
    mir::{Global, GlobalId, GlobalKind, Inst, LoadKind},
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
        for glob in &self.cx.mir.globals {
            self.search_global(glob);
            self.add_global(glob.id);
        }

        self.order
    }

    fn search_global(&mut self, glob: &Global) {
        if let GlobalKind::Static(body, _) = &glob.kind {
            for blk in body.blocks() {
                for inst in blk.insts() {
                    if let Inst::Load { kind: LoadKind::Global(gid), .. } = inst {
                        if !self.add_global(*gid) {
                            self.search_global_by_id(*gid);
                        }
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
