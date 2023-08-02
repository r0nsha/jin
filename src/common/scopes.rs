use std::{collections::HashMap, hash::Hash};

// PERF: use UstrMap
#[derive(Debug, Clone)]
pub(crate) struct Scopes<K: Hash + Eq, V>(Vec<HashMap<K, V>>);

impl<K: Hash + Eq, V> Scopes<K, V> {
    pub(crate) fn new() -> Self {
        Self(vec![])
    }

    pub(crate) fn push_scope(&mut self) {
        self.0.push(HashMap::default());
    }

    pub(crate) fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub(crate) fn insert(&mut self, k: K, v: V) {
        self.0.last_mut().unwrap().insert(k, v);
    }

    pub(crate) fn get(&self, k: K) -> Option<(usize, &V)> {
        for (depth, scope) in self.0.iter().enumerate().rev() {
            if let Some(value) = scope.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    pub(crate) fn get_mut(&mut self, k: K) -> Option<(usize, &mut V)> {
        for (depth, scope) in self.0.iter_mut().enumerate().rev() {
            if let Some(value) = scope.get_mut(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    pub(crate) fn get_value(&self, k: K) -> Option<&V> {
        self.get(k).map(|r| r.1)
    }

    pub(crate) fn get_value_mut(&mut self, k: K) -> Option<&mut V> {
        self.get_mut(k).map(|r| r.1)
    }

    pub(crate) fn depth(&self) -> usize {
        self.0.len()
    }
}
