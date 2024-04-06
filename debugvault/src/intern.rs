use std::hash::{BuildHasherDefault, Hash};
use std::sync::Arc;
use dashmap::DashMap;
use rustc_hash::FxHasher;

pub struct InternMap<K, V: ?Sized> {
    map: DashMap<K, Arc<V>, BuildHasherDefault<FxHasher>>,
}

impl<K: Hash + Eq, V: ?Sized> InternMap<K, V> {
    pub fn new() -> Self {
        Self {
            map: DashMap::with_hasher(BuildHasherDefault::default()),
        }
    }

    pub fn add(&self, key: K, value: &V) -> Arc<V>
    where
        for<'a> &'a V: Into<Arc<V>>,
    {
        let value = value.into();
        self.map.insert(key, Arc::clone(&value));
        value
    }

    pub fn get(&self, key: &K) -> Option<Arc<V>> {
        self.map.get(key).map(|v| v.clone())
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}
