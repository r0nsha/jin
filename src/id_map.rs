use std::hash::Hash;

use rustc_hash::FxHashMap;

use crate::counter::Counter;

pub struct IdMap<K, V> {
    map: FxHashMap<K, V>,
    counter: Counter<K>,
}

impl<K, V> IdMap<K, V> {
    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.map.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&K, &mut V)> {
        self.map.iter_mut()
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.map.keys()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.map.values()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.map.values_mut()
    }
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
