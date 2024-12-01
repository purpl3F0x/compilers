use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

use super::symbol_table_entries::{FunctionEntry, LValueEntry};

#[derive(Debug, Clone)]
pub struct InsertError<T> {
    pub entry: T,
}

pub type Scope<K, T> = HashMap<K, T>;

#[derive(Clone, Debug)]
pub struct Scopes<K, T>
where
    K: Default + Hash + Eq + Clone,
    T: Clone,
{
    pub scopes: Vec<Scope<K, T>>,
}

impl<K, T> Scopes<K, T>
where
    K: Default + Hash + Eq + Clone,
    T: Clone,
{
    pub fn new() -> Scopes<K, T> {
        Scopes { scopes: vec![Scope::new()] }
    }

    pub fn try_insert(&mut self, s: K, val: T) -> Result<(), InsertError<T>> {
        let entry = self.scopes.last_mut().unwrap().entry(s);
        match entry {
            Entry::Occupied(entry) => Err(InsertError { entry: entry.get().to_owned() }),
            Entry::Vacant(v) => {
                v.insert(val);
                Ok(())
            }
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(Scope::new())
    }

    pub fn pop(&mut self) -> Option<Scope<K, T>> {
        self.scopes.pop()
    }

    pub fn get_upper_scope(&self) -> Option<&Scope<K, T>> {
        self.scopes.get(self.scopes.len() - 2)
    }
}

impl<'ctx, K> Scopes<K, LValueEntry<'ctx>>
where
    K: Default + Hash + Eq + Clone,
{
    pub fn get_from_last(&mut self, s: K) -> Option<&LValueEntry<'ctx>> {
        let entry = self.scopes.last_mut()?.get_mut(&s);
        if let Some(entry) = entry {
            entry.is_used = true;
            return Some(entry);
        } else {
            return None;
        }
    }
}

impl<'ctx, K> Scopes<K, FunctionEntry<'ctx>>
where
    K: Default + Hash + Eq + Clone,
{
    pub fn get(&mut self, s: K) -> Option<FunctionEntry<'ctx>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(res) = scope.get_mut(&s) {
                res.is_used = true;
                return Some(res.clone());
            }
        }
        None
    }

    pub fn get_as_ref(&mut self, s: K) -> Option<&FunctionEntry<'ctx>> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(res) = scope.get_mut(&s) {
                res.is_used = true;
                return Some(res);
            }
        }
        None
    }
}
