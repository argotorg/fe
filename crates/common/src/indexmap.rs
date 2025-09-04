use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
};

use rustc_hash::FxBuildHasher;
use salsa::Update;

type OrderMap<K, V> = ordermap::OrderMap<K, V, FxBuildHasher>;
type OrderSet<V> = ordermap::OrderSet<V, FxBuildHasher>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IndexMap<K, V>(OrderMap<K, V>);

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

unsafe impl<K, V> Update for IndexMap<K, V>
where
    K: Update + Eq + Hash,
    V: Update,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_map: Self) -> bool {
        unsafe {
            let old_map = &mut *old_pointer;

            // Check if the keys in both maps are the same w.r.t the key order.
            let is_key_same = old_map.len() == new_map.len()
                && old_map
                    .keys()
                    .zip(new_map.keys())
                    .all(|(old, new)| old == new);

            // If the keys are different, update entire map.
            if !is_key_same {
                old_map.clear();
                old_map.0.extend(new_map.0);
                return true;
            }

            // Update values if it's different.
            let mut changed = false;
            for (i, new_value) in new_map.0.into_values().enumerate() {
                let old_value = &mut old_map[i];
                changed |= V::maybe_update(old_value, new_value);
            }

            changed
        }
    }
}

#[derive(Debug, Clone)]
pub struct IndexSet<V>(OrderSet<V>);

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

unsafe impl<V> Update for IndexSet<V>
where
    V: Update + Eq + Hash,
{
    unsafe fn maybe_update(old_pointer: *mut Self, new_set: Self) -> bool {
        let old_set = unsafe { &mut *old_pointer };
        if old_set == &new_set {
            false
        } else {
            old_set.clear();
            old_set.0.extend(new_set.0);
            true
        }
    }
}
