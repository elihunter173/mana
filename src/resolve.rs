use std::collections::{hash_map, HashMap};

use crate::intern::Symbol;

#[derive(Debug)]
pub struct Resolver<T> {
    scopes: Vec<HashMap<Symbol, T>>,
}

#[derive(Debug)]
pub enum ResolverError {
    DuplicateItem,
}

impl<T> Resolver<T> {
    pub fn new() -> Self {
        Self { scopes: vec![HashMap::new()] }
    }

    // TODO: Think about having a with_scope rather than enter and exit scope... Better with error
    // handling probably
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop().expect("cannot remove last scope");
    }

    pub fn resolve(&self, sym: Symbol) -> Option<&T> {
        self.scopes.iter().rev().find_map(|scope| scope.get(&sym))
    }

    pub fn define(&mut self, sym: Symbol, val: T) -> Result<(), ResolverError> {
        let current_scope = self.scopes.last_mut().expect("must have at least 1 scope");
        match current_scope.entry(sym) {
            hash_map::Entry::Vacant(slot) => {
                slot.insert(val);
                Ok(())
            }
            hash_map::Entry::Occupied(_) => Err(ResolverError::DuplicateItem),
        }
    }
}
