use compiler_mir::{Global, GlobalId, GlobalKind, ValueKind};

use crate::generate::Generator;

impl<'db> Generator<'db> {
    pub fn get_global_init_order(&self) -> Vec<GlobalId> {
        let mut order: Vec<GlobalId> = vec![];

        for glob in self.mir.globals.values() {
            search_global(self, &mut order, glob);
            if matches!(&glob.kind, GlobalKind::Static { .. }) {
                add_global(&mut order, glob.id);
            }
        }

        order
    }
}

fn search_global(cx: &Generator, order: &mut Vec<GlobalId>, glob: &Global) {
    let GlobalKind::Static(body) = &glob.kind else { return };

    for value in body.values() {
        let ValueKind::Global(gid) = &value.kind else { continue };
        if !add_global(order, *gid) {
            search_global_by_id(cx, order, *gid);
        }
    }
}

fn search_global_by_id(cx: &Generator, order: &mut Vec<GlobalId>, id: GlobalId) {
    search_global(cx, order, &cx.mir.globals[id]);
}

fn add_global(order: &mut Vec<GlobalId>, id: GlobalId) -> bool {
    if order.contains(&id) {
        return false;
    }

    order.push(id);
    true
}
