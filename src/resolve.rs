use std::{collections::HashMap, hash::Hash};

#[derive(Debug)]
pub struct Resolver<K, V> {
    scopes: Vec<HashMap<K, V>>,
}

#[derive(Debug)]
pub enum ResolverError {
    DuplicateItem,
}

impl<K: Hash + Eq, V> Resolver<K, V> {
    pub fn new() -> Self {
        Self { scopes: vec![HashMap::new()] }
    }

    // TODO: Make this with_scope rather than enter and exit scope... Better with error
    // handling
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop().expect("cannot remove last scope");
    }

    pub fn resolve(&self, key: &K) -> Option<&V> {
        self.scopes.iter().rev().find_map(|scope| scope.get(key))
    }

    pub fn define(&mut self, key: K, val: V) -> Option<V> {
        let current_scope = self.scopes.last_mut().expect("must have at least 1 scope");
        current_scope.insert(key, val)
    }
}
