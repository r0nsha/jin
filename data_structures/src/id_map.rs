use std::{collections::hash_map, hash::Hash, ops};

use rustc_hash::FxHashMap;

use crate::counter::Counter;

#[derive(Debug, Clone)]
pub struct IdMap<K, V> {
    map: FxHashMap<K, V>,
    counter: Counter<K>,
}

impl<K, V> IdMap<K, V> {
    #[must_use]
    pub fn new() -> Self {
        Self { map: FxHashMap::default(), counter: Counter::default() }
    }

    #[must_use]
    pub fn new_with_counter(counter: Counter<K>) -> Self {
        Self { map: FxHashMap::default(), counter }
    }

    #[must_use]
    pub fn inner(&self) -> &FxHashMap<K, V> {
        &self.map
    }

    #[must_use]
    pub fn inner_mut(&mut self) -> &mut FxHashMap<K, V> {
        &mut self.map
    }

    #[must_use]
    pub fn counter(&self) -> &Counter<K> {
        &self.counter
    }

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

    #[must_use]
    pub fn len(&self) -> usize {
        self.map.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}

impl<K: Eq + Hash, V> IdMap<K, V> {
    pub fn contains_key(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.map.get(key)
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        self.map.get_mut(key)
    }
}

impl<K: From<usize> + Copy + Eq + Hash, V> IdMap<K, V> {
    pub fn insert(&mut self, value: V) -> K {
        let key = self.counter.increment();
        self.map.insert(key, value);
        key
    }

    pub fn insert_with_key(&mut self, f: impl FnOnce(K) -> V) -> K {
        let key = self.counter.increment();
        let value = f(key);
        self.map.insert(key, value);
        key
    }

    pub fn extend(&mut self, iter: impl IntoIterator<Item = (K, V)>) {
        let prev_len = self.map.len();
        self.map.extend(iter);
        self.counter.increment_by(self.map.len() - prev_len);
    }
}

impl<K, V> Default for IdMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash, V> ops::Index<K> for IdMap<K, V> {
    type Output = V;

    #[inline]
    fn index(&self, index: K) -> &Self::Output {
        &self.map[&index]
    }
}

impl<K: Eq + Hash, V> ops::IndexMut<K> for IdMap<K, V> {
    #[track_caller]
    #[inline]
    fn index_mut(&mut self, index: K) -> &mut Self::Output {
        self.map.get_mut(&index).unwrap()
    }
}

impl<K, V> IntoIterator for IdMap<K, V> {
    type Item = (K, V);
    type IntoIter = hash_map::IntoIter<K, V>;

    fn into_iter(self) -> hash_map::IntoIter<K, V> {
        self.map.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a IdMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = hash_map::Iter<'a, K, V>;

    #[allow(clippy::into_iter_on_ref)]
    fn into_iter(self) -> hash_map::Iter<'a, K, V> {
        self.map.iter()
    }
}

impl<'a, K, V> IntoIterator for &'a mut IdMap<K, V> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = hash_map::IterMut<'a, K, V>;

    #[allow(clippy::into_iter_on_ref)]
    fn into_iter(self) -> hash_map::IterMut<'a, K, V> {
        self.map.iter_mut()
    }
}
