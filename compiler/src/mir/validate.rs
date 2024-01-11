use crate::mir::{Body, GlobalKind, Inst, Mir, StaticGlobal};

impl Mir {
    pub fn validate(&self) {
        for fun in self.fns.values() {
            validate_body(&self.fn_sigs[fun.sig].name, &fun.body);
        }

        for global in self.globals.values() {
            if let GlobalKind::Static(StaticGlobal { body, .. }) = &global.kind
            {
                validate_body(&global.name, body);
            }
        }
    }
}

fn validate_body(name: &str, body: &Body) {
    validate_refcnt_balance(name, body);
}

fn validate_refcnt_balance(name: &str, body: &Body) {
    let mut increfs = 0;
    let mut decrefs = 0;

    for block in body.blocks() {
        for inst in &block.insts {
            match inst {
                Inst::IncRef { .. } => increfs += 1,
                Inst::DecRef { .. } => decrefs += 1,
                _ => (),
            }
        }
    }

    let balance = increfs - decrefs;
    if balance != 0 {
        eprintln!(
            "{name}: unbalanced refcounts - {increfs}/{decrefs} ({balance})"
        );
    }
}
