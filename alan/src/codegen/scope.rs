use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;

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

    pub fn get(&self, s: K) -> Option<T> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.get(&s) {
                return Some(res.clone());
            }
        }
        None
    }

    pub fn get_from_last(&self, s: K) -> Option<T> {
        self.scopes.last()?.get(&s).cloned()
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

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn get_upper_scope(&self) -> Option<&Scope<K, T>> {
        self.scopes.get(self.scopes.len() - 2)
    }
}
