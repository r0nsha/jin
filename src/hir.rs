use slotmap::SlotMap;

pub struct Cache {
    pub funs: SlotMap<FunId, Fun>,
}

slotmap::new_key_type! {
    pub struct FunId;
}

impl Cache {
    pub fn new() -> Self {
        Self {
            funs: SlotMap::with_key(),
        }
    }
}
