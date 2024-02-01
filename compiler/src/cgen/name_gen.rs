use rustc_hash::FxHashMap;
use ustr::{ustr, Ustr, UstrMap};

use crate::{counter::Counter, db::DefId};

#[derive(Debug, Clone)]
pub struct LocalNames {
    names: FxHashMap<DefId, Ustr>,
    gen: UniqueNameGenerator,
}

impl LocalNames {
    pub fn new() -> Self {
        Self { names: FxHashMap::default(), gen: UniqueNameGenerator::new() }
    }

    pub fn insert_unique(&mut self, id: DefId, name: Ustr) -> Ustr {
        let unique_name = self.gen.generate(name);
        self.names.insert(id, unique_name);
        unique_name
    }

    pub fn get(&self, id: DefId) -> Option<Ustr> {
        self.names.get(&id).copied()
    }
}

#[derive(Debug, Clone)]
pub struct UniqueNameGenerator {
    counters: UstrMap<Counter<usize>>,
}

impl UniqueNameGenerator {
    pub fn new() -> Self {
        Self { counters: UstrMap::default() }
    }

    pub fn generate(&mut self, name: Ustr) -> Ustr {
        let counter = self.counters.entry(name).or_default();
        let unique_name = format!("{}${}", name, counter.value());
        counter.increment();
        ustr(&unique_name)
    }
}
