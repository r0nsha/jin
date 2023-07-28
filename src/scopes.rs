use std::{collections::HashMap, hash::Hash};

// PERF: use UstrMap
#[derive(Debug, Clone)]
pub struct Scopes<K: Hash + Eq, V>(Vec<HashMap<K, V>>);

impl<K: Hash + Eq, V> Scopes<K, V> {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn push_scope(&mut self) {
        self.0.push(HashMap::default());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.0.last_mut().unwrap().insert(k, v);
    }

    pub fn get(&self, k: K) -> Option<(usize, &V)> {
        for (depth, scope) in self.0.iter().enumerate().rev() {
            if let Some(value) = scope.get(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    pub fn get_mut(&mut self, k: K) -> Option<(usize, &mut V)> {
        for (depth, scope) in self.0.iter_mut().enumerate().rev() {
            if let Some(value) = scope.get_mut(&k) {
                return Some((depth + 1, value));
            }
        }
        None
    }

    pub fn get_value(&self, k: K) -> Option<&V> {
        self.get(k).map(|r| r.1)
    }

    pub fn get_value_mut(&mut self, k: K) -> Option<&mut V> {
        self.get_mut(k).map(|r| r.1)
    }

    pub fn depth(&self) -> usize {
        self.0.len()
    }
}
