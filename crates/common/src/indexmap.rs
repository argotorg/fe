use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
};

use rustc_hash::FxBuildHasher;
use salsa::SalsaValue;

type OrderMap<K, V> = ordermap::OrderMap<K, V, FxBuildHasher>;
type OrderSet<V> = ordermap::OrderSet<V, FxBuildHasher>;

#[derive(Debug, Clone, Hash, PartialEq, Eq, SalsaValue)]
pub struct IndexMap<K, V>(
    // SAFETY: An `OrderMap` owns its keys and values, and its hasher holds no data.
    #[salsa_value(unsafe(prove(K: SalsaValue, V: SalsaValue)))] OrderMap<K, V>,
);

impl<K, V> IndexMap<K, V> {
    pub fn new() -> Self {
        Self(OrderMap::default())
    }

    pub fn with_capacity(n: usize) -> Self {
        Self(OrderMap::with_capacity_and_hasher(n, FxBuildHasher {}))
    }
}

impl<K, V> Default for IndexMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> IntoIterator for IndexMap<K, V> {
    type Item = <OrderMap<K, V> as IntoIterator>::Item;
    type IntoIter = <OrderMap<K, V> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a IndexMap<K, V> {
    type Item = <&'a OrderMap<K, V> as IntoIterator>::Item;
    type IntoIter = <&'a OrderMap<K, V> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a, K, V> IntoIterator for &'a mut IndexMap<K, V> {
    type Item = <&'a mut OrderMap<K, V> as IntoIterator>::Item;
    type IntoIter = <&'a mut OrderMap<K, V> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl<K, V> FromIterator<(K, V)> for IndexMap<K, V>
where
    K: Hash + Eq,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        Self(OrderMap::from_iter(iter))
    }
}

impl<K, V> Deref for IndexMap<K, V> {
    type Target = OrderMap<K, V>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> DerefMut for IndexMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Arc-wrapped IndexMap for use as a salsa input field.
/// Clone is O(1) (Arc refcount bump). Mutations use Arc::make_mut.
#[derive(Debug, Clone, SalsaValue)]
pub struct ArcIndexMap<K, V>(pub std::sync::Arc<IndexMap<K, V>>);

impl<K, V> ArcIndexMap<K, V> {
    pub fn new() -> Self {
        Self(std::sync::Arc::new(IndexMap::new()))
    }

    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: std::borrow::Borrow<Q> + Eq + Hash,
        Q: Eq + Hash + ?Sized,
    {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: K, value: V)
    where
        K: Eq + Hash + Clone,
        V: Clone,
    {
        std::sync::Arc::make_mut(&mut self.0).insert(key, value);
    }

    pub fn remove<Q>(&mut self, key: &Q)
    where
        K: std::borrow::Borrow<Q> + Eq + Hash + Clone,
        V: Clone,
        Q: Eq + Hash + ?Sized,
    {
        std::sync::Arc::make_mut(&mut self.0).remove(key);
    }
}

impl<K, V> Default for ArcIndexMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> PartialEq for ArcIndexMap<K, V>
where
    K: Eq + Hash,
    V: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        std::sync::Arc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}

impl<K, V> Eq for ArcIndexMap<K, V>
where
    K: Eq + Hash,
    V: Eq,
{
}

impl<K, V> Hash for ArcIndexMap<K, V>
where
    K: Hash + Eq,
    V: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (k, v) in self.0.as_ref() {
            k.hash(state);
            v.hash(state);
        }
    }
}

#[derive(Debug, Clone, SalsaValue)]
pub struct IndexSet<V>(
    // SAFETY: An `OrderSet` owns its values, and its hasher holds no data.
    #[salsa_value(unsafe(prove(V: SalsaValue)))] OrderSet<V>,
);

impl<V> IndexSet<V> {
    pub fn new() -> Self {
        Self(OrderSet::default())
    }

    pub fn with_capacity(n: usize) -> Self {
        Self(OrderSet::with_capacity_and_hasher(n, FxBuildHasher {}))
    }
}

impl<V> Default for IndexSet<V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<V> IntoIterator for IndexSet<V> {
    type Item = <OrderSet<V> as IntoIterator>::Item;
    type IntoIter = <OrderSet<V> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, V> IntoIterator for &'a IndexSet<V> {
    type Item = <&'a OrderSet<V> as IntoIterator>::Item;
    type IntoIter = <&'a OrderSet<V> as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<V> PartialEq for IndexSet<V>
where
    V: Hash + Eq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<V> Eq for IndexSet<V> where V: Eq + Hash {}

impl<V> Hash for IndexSet<V>
where
    V: Hash + Eq,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<V> FromIterator<V> for IndexSet<V>
where
    V: Hash + Eq,
{
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        Self(OrderSet::from_iter(iter))
    }
}

impl<V> Deref for IndexSet<V> {
    type Target = OrderSet<V>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V> DerefMut for IndexSet<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
