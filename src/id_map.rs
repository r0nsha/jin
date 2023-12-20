use std::hash::Hash;

use rustc_hash::FxHashMap;

use crate::counter::Counter;

pub struct IdMap<K, V> {
    map: FxHashMap<K, V>,
    counter: Counter<K>,
}

impl<K: Eq + Hash, V> IdMap<K, V> {
    pub fn get(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.map.get_mut(key)
    }
}

impl<K: From<usize> + Copy + Eq + Hash, V> IdMap<K, V> {
    pub fn insert(&mut self, value: V) -> K {
        let key = self.counter.next();
        self.map.insert(key, value);
        key
    }
}
