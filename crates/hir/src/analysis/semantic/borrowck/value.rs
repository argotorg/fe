use std::{collections::BTreeMap, hash::Hash, marker::PhantomData};

use rustc_hash::FxHashMap;

use crate::analysis::{HirAnalysisDb, semantic::VariantIndex};

use super::{
    guard::{ChoiceKey, Guard, IndexExpr, IndexSubst, ValueIndexId, ValueScope},
    shape::{FieldKey, ShapeChildren, ShapeId, SlotPath, SlotProjection},
};

pub(crate) trait IndexPayload: Clone + Eq + Ord + Hash {
    fn substitute_indices(&self, subst: &IndexSubst) -> Self;
}

#[derive(Debug)]
pub(crate) struct ValueId<'db, P> {
    raw: u32,
    marker: PhantomData<fn(&'db ()) -> P>,
}

impl<P> Clone for ValueId<'_, P> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<P> Copy for ValueId<'_, P> {}

impl<P> PartialEq for ValueId<'_, P> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<P> Eq for ValueId<'_, P> {}

impl<P> PartialOrd for ValueId<'_, P> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<P> Ord for ValueId<'_, P> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl<P> Hash for ValueId<'_, P> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Guarded<P> {
    guard: Guard,
    payload: P,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GuardedSet<P> {
    entries: Vec<Guarded<P>>,
}

impl<P> Default for GuardedSet<P> {
    fn default() -> Self {
        Self {
            entries: Vec::new(),
        }
    }
}

impl<P: IndexPayload> GuardedSet<P> {
    fn singleton(payload: P) -> Self {
        Self {
            entries: vec![Guarded {
                guard: Guard::always(),
                payload,
            }],
        }
    }

    fn union(&self, other: &Self) -> Self {
        let mut entries = self.entries.clone();
        entries.extend(other.entries.iter().cloned());
        Self::normalize(entries)
    }

    fn with_guard(&self, guard: &Guard) -> Self {
        Self::normalize(
            self.entries
                .iter()
                .filter_map(|entry| {
                    Some(Guarded {
                        guard: entry.guard.and(guard)?,
                        payload: entry.payload.clone(),
                    })
                })
                .collect(),
        )
    }

    fn substitute(&self, subst: &IndexSubst) -> Self {
        Self::normalize(
            self.entries
                .iter()
                .filter_map(|entry| {
                    Some(Guarded {
                        guard: entry.guard.substitute(subst)?,
                        payload: entry.payload.substitute_indices(subst),
                    })
                })
                .collect(),
        )
    }

    fn scoped(&self, scope: ValueScope) -> Self {
        Self::normalize(
            self.entries
                .iter()
                .map(|entry| Guarded {
                    guard: entry.guard.scoped(scope),
                    payload: entry.payload.clone(),
                })
                .collect(),
        )
    }

    fn normalize(mut entries: Vec<Guarded<P>>) -> Self {
        entries.sort_unstable_by(|lhs, rhs| {
            lhs.payload
                .cmp(&rhs.payload)
                .then_with(|| lhs.guard.cmp(&rhs.guard))
        });
        entries.dedup();

        let mut normalized: Vec<Guarded<P>> = Vec::new();
        for entry in entries {
            if normalized.iter().any(|existing| {
                existing.payload == entry.payload && entry.guard.implies(&existing.guard)
            }) {
                continue;
            }
            normalized.retain(|existing| {
                existing.payload != entry.payload || !existing.guard.implies(&entry.guard)
            });
            normalized.push(entry);
        }
        normalized.sort_unstable_by(|lhs, rhs| {
            lhs.payload
                .cmp(&rhs.payload)
                .then_with(|| lhs.guard.cmp(&rhs.guard))
        });
        Self {
            entries: normalized,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct StructuredValue<'db, P> {
    shape: ShapeId<'db>,
    direct: GuardedSet<P>,
    children: ValueChildren<'db, P>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ValueChildren<'db, P> {
    None,
    Product {
        fields: Box<[(FieldKey, ValueId<'db, P>)]>,
    },
    Sum {
        variants: Box<[(VariantIndex, ValueId<'db, P>)]>,
    },
    Array {
        binder: ValueIndexId,
        default: ValueId<'db, P>,
        exact: BTreeMap<usize, ValueId<'db, P>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GuardedLeaf<P> {
    pub(crate) path: SlotPath<IndexExpr>,
    pub(crate) guard: Guard,
    pub(crate) payload_guard: Guard,
    pub(crate) payload: P,
}

pub(crate) struct ValueInterner<'db, P> {
    db: &'db dyn HirAnalysisDb,
    nodes: Vec<StructuredValue<'db, P>>,
    interned: FxHashMap<StructuredValue<'db, P>, ValueId<'db, P>>,
    empty: FxHashMap<ShapeId<'db>, ValueId<'db, P>>,
    next_binder: u32,
}

impl<'db, P: IndexPayload> ValueInterner<'db, P> {
    pub(crate) fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            db,
            nodes: Vec::new(),
            interned: FxHashMap::default(),
            empty: FxHashMap::default(),
            next_binder: 0,
        }
    }

    pub(crate) fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    pub(crate) fn empty(&mut self, shape: ShapeId<'db>) -> ValueId<'db, P> {
        if let Some(value) = self.empty.get(&shape) {
            return *value;
        }
        let children = match &shape.data(self.db).children {
            ShapeChildren::None => ValueChildren::None,
            ShapeChildren::Product { fields } => ValueChildren::Product {
                fields: fields
                    .iter()
                    .map(|(field, shape)| (*field, self.empty(*shape)))
                    .collect(),
            },
            ShapeChildren::Sum { variants } => ValueChildren::Sum {
                variants: variants
                    .iter()
                    .map(|(variant, shape)| (*variant, self.empty(*shape)))
                    .collect(),
            },
            ShapeChildren::Array { elem, .. } => ValueChildren::Array {
                binder: self.fresh_binder(),
                default: self.empty(*elem),
                exact: BTreeMap::new(),
            },
        };
        let value = self.intern(StructuredValue {
            shape,
            direct: GuardedSet::default(),
            children,
        });
        self.empty.insert(shape, value);
        value
    }

    pub(crate) fn with_direct(&mut self, value: ValueId<'db, P>, payload: P) -> ValueId<'db, P> {
        let mut node = self.node(value).clone();
        node.direct = GuardedSet::singleton(payload);
        self.intern(node)
    }

    pub(crate) fn product(
        &mut self,
        shape: ShapeId<'db>,
        fields: impl IntoIterator<Item = (FieldKey, ValueId<'db, P>)>,
    ) -> ValueId<'db, P> {
        let fields = fields.into_iter().collect::<Vec<_>>().into_boxed_slice();
        debug_assert!(matches!(
            &shape.data(self.db).children,
            ShapeChildren::Product { fields: expected }
                if expected.iter().map(|(field, _)| field).eq(fields.iter().map(|(field, _)| field))
        ));
        self.intern(StructuredValue {
            shape,
            direct: GuardedSet::default(),
            children: ValueChildren::Product { fields },
        })
    }

    pub(crate) fn sum_variant(
        &mut self,
        shape: ShapeId<'db>,
        variant: VariantIndex,
        value: ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        debug_assert!(matches!(
            &shape.data(self.db).children,
            ShapeChildren::Sum { variants }
                if variants.iter().any(|(candidate, _)| *candidate == variant)
        ));
        self.intern(StructuredValue {
            shape,
            direct: GuardedSet::default(),
            children: ValueChildren::Sum {
                variants: vec![(variant, value)].into_boxed_slice(),
            },
        })
    }

    pub(crate) fn array_repeat(
        &mut self,
        shape: ShapeId<'db>,
        default: ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        debug_assert!(matches!(
            &shape.data(self.db).children,
            ShapeChildren::Array { elem, .. } if *elem == self.node(default).shape
        ));
        let binder = self.fresh_binder();
        self.intern(StructuredValue {
            shape,
            direct: GuardedSet::default(),
            children: ValueChildren::Array {
                binder,
                default,
                exact: BTreeMap::new(),
            },
        })
    }

    pub(crate) fn array_exact(
        &mut self,
        shape: ShapeId<'db>,
        exact: impl IntoIterator<Item = (usize, ValueId<'db, P>)>,
    ) -> ValueId<'db, P> {
        let ShapeChildren::Array { len, elem } = &shape.data(self.db).children else {
            panic!("array value requires an array shape");
        };
        let exact = exact
            .into_iter()
            .filter(|(index, value)| *index < *len && self.node(*value).shape == *elem)
            .collect();
        let default = self.empty(*elem);
        let binder = self.fresh_binder();
        self.intern(StructuredValue {
            shape,
            direct: GuardedSet::default(),
            children: ValueChildren::Array {
                binder,
                default,
                exact,
            },
        })
    }

    pub(crate) fn join(&mut self, lhs: ValueId<'db, P>, rhs: ValueId<'db, P>) -> ValueId<'db, P> {
        if lhs == rhs {
            return lhs;
        }
        let lhs_node = self.node(lhs).clone();
        let mut rhs_node = self.node(rhs).clone();
        assert_eq!(lhs_node.shape, rhs_node.shape, "value join shape mismatch");
        let direct = lhs_node.direct.union(&rhs_node.direct);
        let children = match (lhs_node.children, &mut rhs_node.children) {
            (ValueChildren::None, ValueChildren::None) => ValueChildren::None,
            (ValueChildren::Product { fields: lhs }, ValueChildren::Product { fields: rhs }) => {
                ValueChildren::Product {
                    fields: lhs
                        .iter()
                        .zip(rhs.iter())
                        .map(|((lhs_key, lhs), (rhs_key, rhs))| {
                            assert_eq!(lhs_key, rhs_key, "product join field mismatch");
                            (*lhs_key, self.join(*lhs, *rhs))
                        })
                        .collect(),
                }
            }
            (ValueChildren::Sum { variants: lhs }, ValueChildren::Sum { variants: rhs }) => {
                let lhs = lhs.into_iter().collect::<BTreeMap<_, _>>();
                let rhs = rhs.iter().copied().collect::<BTreeMap<_, _>>();
                let variants = lhs
                    .keys()
                    .chain(rhs.keys())
                    .copied()
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .filter_map(|variant| match (lhs.get(&variant), rhs.get(&variant)) {
                        (Some(lhs), Some(rhs)) => Some((variant, self.join(*lhs, *rhs))),
                        (Some(value), None) | (None, Some(value)) => Some((variant, *value)),
                        (None, None) => None,
                    })
                    .collect();
                ValueChildren::Sum { variants }
            }
            (
                ValueChildren::Array {
                    binder: lhs_binder,
                    default: lhs_default,
                    exact: lhs_exact,
                },
                ValueChildren::Array {
                    binder: rhs_binder,
                    default: rhs_default,
                    exact: rhs_exact,
                },
            ) => {
                let subst = IndexSubst::from_pair(
                    IndexExpr::ValueParam(*rhs_binder),
                    IndexExpr::ValueParam(lhs_binder),
                );
                let rhs_default = self.substitute(*rhs_default, &subst);
                let rhs_exact = rhs_exact
                    .iter()
                    .map(|(index, value)| (*index, self.substitute(*value, &subst)))
                    .collect::<BTreeMap<_, _>>();
                let default = self.join(lhs_default, rhs_default);
                let keys = lhs_exact
                    .keys()
                    .chain(rhs_exact.keys())
                    .copied()
                    .collect::<std::collections::BTreeSet<_>>();
                let mut exact = BTreeMap::new();
                for index in keys {
                    let lhs = lhs_exact
                        .get(&index)
                        .copied()
                        .unwrap_or_else(|| self.specialize_default(lhs_default, lhs_binder, index));
                    let rhs = rhs_exact
                        .get(&index)
                        .copied()
                        .unwrap_or_else(|| self.specialize_default(rhs_default, lhs_binder, index));
                    let joined = self.join(lhs, rhs);
                    if joined != self.specialize_default(default, lhs_binder, index) {
                        exact.insert(index, joined);
                    }
                }
                ValueChildren::Array {
                    binder: lhs_binder,
                    default,
                    exact,
                }
            }
            _ => panic!("value join structure mismatch"),
        };
        self.intern(StructuredValue {
            shape: lhs_node.shape,
            direct,
            children,
        })
    }

    pub(crate) fn project(
        &mut self,
        value: ValueId<'db, P>,
        path: &SlotPath<IndexExpr>,
        scope: ValueScope,
    ) -> ValueId<'db, P> {
        self.project_from(value, path.as_slice(), scope, &mut SlotPath::new())
    }

    fn project_from(
        &mut self,
        value: ValueId<'db, P>,
        path: &[SlotProjection<IndexExpr>],
        scope: ValueScope,
        traversed: &mut SlotPath<IndexExpr>,
    ) -> ValueId<'db, P> {
        let Some((projection, suffix)) = path.split_first() else {
            return value;
        };
        let node = self.node(value).clone();
        let selected = match (projection, node.children) {
            (SlotProjection::Field(field), ValueChildren::Product { fields }) => fields
                .iter()
                .find_map(|(candidate, value)| (*candidate == *field).then_some(*value))
                .expect("projected field must exist"),
            (SlotProjection::VariantField { variant, field }, ValueChildren::Sum { variants }) => {
                let variant_value = variants
                    .iter()
                    .find_map(|(candidate, value)| (*candidate == *variant).then_some(*value))
                    .unwrap_or_else(|| {
                        let ShapeChildren::Sum { variants } = &node.shape.data(self.db).children
                        else {
                            unreachable!()
                        };
                        let shape = variants
                            .iter()
                            .find_map(|(candidate, shape)| {
                                (*candidate == *variant).then_some(*shape)
                            })
                            .expect("projected variant must exist");
                        self.empty(shape)
                    });
                let ValueChildren::Product { fields } = &self.node(variant_value).children else {
                    panic!("enum variant value must be a product");
                };
                let selected = fields
                    .iter()
                    .find_map(|(candidate, value)| (candidate.index() == *field).then_some(*value))
                    .expect("projected variant field must exist");
                let guard = Guard::always()
                    .with_variant(
                        ChoiceKey::relative(traversed.clone()).scoped(scope),
                        *variant,
                    )
                    .expect("one variant selection is satisfiable");
                self.with_guard(selected, &guard)
            }
            (
                SlotProjection::Index(index),
                ValueChildren::Array {
                    binder,
                    default,
                    exact,
                },
            ) => self.project_array(node.shape, binder, default, &exact, *index),
            _ => panic!("value projection does not match shape"),
        };
        traversed.push(projection.clone());
        let projected = self.project_from(selected, suffix, scope, traversed);
        traversed.pop();
        projected
    }

    fn project_array(
        &mut self,
        shape: ShapeId<'db>,
        binder: ValueIndexId,
        default: ValueId<'db, P>,
        exact: &BTreeMap<usize, ValueId<'db, P>>,
        index: IndexExpr,
    ) -> ValueId<'db, P> {
        let ShapeChildren::Array { len, .. } = shape.data(self.db).children else {
            unreachable!()
        };
        if let IndexExpr::Const(index) = index {
            assert!(index < len, "array projection is in bounds");
            return exact
                .get(&index)
                .copied()
                .unwrap_or_else(|| self.specialize_default(default, binder, index));
        }

        let mut alternatives = Vec::new();
        for (exact_index, value) in exact {
            if let Some(guard) = Guard::equal(index, IndexExpr::Const(*exact_index)) {
                alternatives.push(self.with_guard(*value, &guard));
            }
        }
        let subst = IndexSubst::from_pair(IndexExpr::ValueParam(binder), index);
        if let Some(fallback_guard) = exact.keys().try_fold(
            Guard::bounded(index, len).expect("symbolic bound is satisfiable"),
            |guard, exact_index| guard.with_disequality(index, IndexExpr::Const(*exact_index)),
        ) {
            let fallback = self.substitute(default, &subst);
            alternatives.push(self.with_guard(fallback, &fallback_guard));
        }
        alternatives
            .into_iter()
            .reduce(|lhs, rhs| self.join(lhs, rhs))
            .expect("array projection has a fallback")
    }

    pub(crate) fn replace(
        &mut self,
        value: ValueId<'db, P>,
        path: &SlotPath<IndexExpr>,
        replacement: ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        self.replace_from(value, path.as_slice(), replacement)
    }

    fn replace_from(
        &mut self,
        value: ValueId<'db, P>,
        path: &[SlotProjection<IndexExpr>],
        replacement: ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        let Some((projection, suffix)) = path.split_first() else {
            assert_eq!(
                self.node(value).shape,
                self.node(replacement).shape,
                "replacement shape mismatch"
            );
            return replacement;
        };
        let mut node = self.node(value).clone();
        node.children = match (projection, node.children) {
            (SlotProjection::Field(field), ValueChildren::Product { mut fields }) => {
                let (_, selected) = fields
                    .iter_mut()
                    .find(|(candidate, _)| candidate == field)
                    .expect("replaced field must exist");
                *selected = self.replace_from(*selected, suffix, replacement);
                ValueChildren::Product { fields }
            }
            (SlotProjection::VariantField { variant, field }, ValueChildren::Sum { variants }) => {
                let shape_variants = match &node.shape.data(self.db).children {
                    ShapeChildren::Sum { variants } => variants,
                    _ => unreachable!(),
                };
                let mut variants = variants.into_vec();
                let position = variants
                    .iter()
                    .position(|(candidate, _)| candidate == variant);
                let variant_value = position.map_or_else(
                    || {
                        let shape = shape_variants
                            .iter()
                            .find_map(|(candidate, shape)| (candidate == variant).then_some(*shape))
                            .expect("replaced variant must exist");
                        self.empty(shape)
                    },
                    |position| variants[position].1,
                );
                let mut variant_node = self.node(variant_value).clone();
                let ValueChildren::Product { mut fields } = variant_node.children else {
                    panic!("enum variant value must be a product");
                };
                let (_, selected) = fields
                    .iter_mut()
                    .find(|(candidate, _)| candidate.index() == *field)
                    .expect("replaced variant field must exist");
                *selected = self.replace_from(*selected, suffix, replacement);
                variant_node.children = ValueChildren::Product { fields };
                let variant_value = self.intern(variant_node);
                if let Some(position) = position {
                    variants[position].1 = variant_value;
                } else {
                    variants.push((*variant, variant_value));
                    variants.sort_unstable_by_key(|(variant, _)| *variant);
                }
                ValueChildren::Sum {
                    variants: variants.into_boxed_slice(),
                }
            }
            (SlotProjection::Index(index), children @ ValueChildren::Array { .. }) => {
                self.replace_array(node.shape, children, *index, suffix, replacement)
            }
            _ => panic!("value replacement does not match shape"),
        };
        self.intern(node)
    }

    fn replace_array(
        &mut self,
        shape: ShapeId<'db>,
        children: ValueChildren<'db, P>,
        index: IndexExpr,
        suffix: &[SlotProjection<IndexExpr>],
        replacement: ValueId<'db, P>,
    ) -> ValueChildren<'db, P> {
        let ValueChildren::Array {
            binder,
            default,
            mut exact,
        } = children
        else {
            unreachable!()
        };
        let ShapeChildren::Array { len, .. } = shape.data(self.db).children else {
            unreachable!()
        };
        if let IndexExpr::Const(index) = index {
            if index >= len {
                return ValueChildren::Array {
                    binder,
                    default,
                    exact,
                };
            }
            let current = exact
                .get(&index)
                .copied()
                .unwrap_or_else(|| self.specialize_default(default, binder, index));
            let replacement = self.replace_from(current, suffix, replacement);
            let specialized_default = self.specialize_default(default, binder, index);
            if replacement == specialized_default {
                exact.remove(&index);
            } else {
                exact.insert(index, replacement);
            }
            return ValueChildren::Array {
                binder,
                default,
                exact,
            };
        }

        let default_replacement = self.replace_from(default, suffix, replacement);
        let old_guard = Guard::not_equal(IndexExpr::ValueParam(binder), index)
            .expect("symbolic array indices may differ");
        let new_guard = Guard::equal(IndexExpr::ValueParam(binder), index)
            .expect("symbolic array indices may be equal");
        let default = {
            let old = self.with_guard(default, &old_guard);
            let new = self.with_guard(default_replacement, &new_guard);
            self.join(old, new)
        };
        for (exact_index, value) in &mut exact {
            let current = *value;
            let updated = self.replace_from(current, suffix, replacement);
            let old_guard = Guard::not_equal(index, IndexExpr::Const(*exact_index))
                .expect("symbolic and exact index may differ");
            let new_guard = Guard::equal(index, IndexExpr::Const(*exact_index))
                .expect("symbolic and exact index may match");
            let old = self.with_guard(current, &old_guard);
            let new = self.with_guard(updated, &new_guard);
            *value = self.join(old, new);
        }
        ValueChildren::Array {
            binder,
            default,
            exact,
        }
    }

    pub(crate) fn with_guard(&mut self, value: ValueId<'db, P>, guard: &Guard) -> ValueId<'db, P> {
        let node = self.node(value).clone();
        let children = match node.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product { fields } => ValueChildren::Product {
                fields: fields
                    .iter()
                    .map(|(field, value)| (*field, self.with_guard(*value, guard)))
                    .collect(),
            },
            ValueChildren::Sum { variants } => ValueChildren::Sum {
                variants: variants
                    .iter()
                    .map(|(variant, value)| (*variant, self.with_guard(*value, guard)))
                    .collect(),
            },
            ValueChildren::Array {
                binder,
                default,
                exact,
            } => ValueChildren::Array {
                binder,
                default: self.with_guard(default, guard),
                exact: exact
                    .into_iter()
                    .map(|(index, value)| (index, self.with_guard(value, guard)))
                    .collect(),
            },
        };
        self.intern(StructuredValue {
            shape: node.shape,
            direct: node.direct.with_guard(guard),
            children,
        })
    }

    pub(crate) fn substitute(
        &mut self,
        value: ValueId<'db, P>,
        subst: &IndexSubst,
    ) -> ValueId<'db, P> {
        if subst.is_empty() {
            return value;
        }
        let node = self.node(value).clone();
        let children = match node.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product { fields } => ValueChildren::Product {
                fields: fields
                    .iter()
                    .map(|(field, value)| (*field, self.substitute(*value, subst)))
                    .collect(),
            },
            ValueChildren::Sum { variants } => ValueChildren::Sum {
                variants: variants
                    .iter()
                    .map(|(variant, value)| (*variant, self.substitute(*value, subst)))
                    .collect(),
            },
            ValueChildren::Array {
                binder,
                default,
                exact,
            } => ValueChildren::Array {
                binder,
                default: self.substitute(default, subst),
                exact: exact
                    .into_iter()
                    .map(|(index, value)| (index, self.substitute(value, subst)))
                    .collect(),
            },
        };
        self.intern(StructuredValue {
            shape: node.shape,
            direct: node.direct.substitute(subst),
            children,
        })
    }

    pub(crate) fn enumerate_leaves(
        &self,
        value: ValueId<'db, P>,
        scope: ValueScope,
    ) -> Vec<GuardedLeaf<P>> {
        let mut leaves = Vec::new();
        self.enumerate_from(
            value,
            scope,
            &mut SlotPath::new(),
            &Guard::always(),
            &mut leaves,
        );
        leaves.sort_unstable_by(|lhs, rhs| {
            lhs.path
                .cmp(&rhs.path)
                .then_with(|| lhs.payload.cmp(&rhs.payload))
                .then_with(|| lhs.guard.cmp(&rhs.guard))
        });
        leaves
    }

    pub(crate) fn reconstruct(
        &mut self,
        shape: ShapeId<'db>,
        leaves: &[GuardedLeaf<P>],
    ) -> Option<ValueId<'db, P>> {
        self.build_from_leaves(shape, leaves, &IndexSubst::new())
    }

    fn build_from_leaves(
        &mut self,
        shape: ShapeId<'db>,
        leaves: &[GuardedLeaf<P>],
        subst: &IndexSubst,
    ) -> Option<ValueId<'db, P>> {
        let direct = GuardedSet::normalize(
            leaves
                .iter()
                .filter(|leaf| leaf.path.is_empty())
                .filter_map(|leaf| {
                    Some(Guarded {
                        guard: leaf.payload_guard.substitute(subst)?,
                        payload: leaf.payload.substitute_indices(subst),
                    })
                })
                .collect(),
        );
        let children = match &shape.data(self.db).children {
            ShapeChildren::None => ValueChildren::None,
            ShapeChildren::Product { fields } => ValueChildren::Product {
                fields: fields
                    .iter()
                    .map(|(field, child_shape)| {
                        let children = strip_matching_leaves(leaves, |projection| {
                            matches!(projection, SlotProjection::Field(candidate) if candidate == field)
                        });
                        Some((*field, self.build_from_leaves(*child_shape, &children, subst)?))
                    })
                    .collect::<Option<Box<_>>>()?,
            },
            ShapeChildren::Sum { variants } => ValueChildren::Sum {
                variants: variants
                    .iter()
                    .map(|(variant, variant_shape)| {
                        let ShapeChildren::Product { fields } =
                            &variant_shape.data(self.db).children
                        else {
                            return None;
                        };
                        let variant_leaves = leaves
                            .iter()
                            .filter_map(|leaf| {
                                let (projection, suffix) = leaf.path.as_slice().split_first()?;
                                let SlotProjection::VariantField {
                                    variant: candidate,
                                    field,
                                } = projection
                                else {
                                    return None;
                                };
                                (*candidate == *variant).then(|| {
                                    let key = fields
                                        .iter()
                                        .find_map(|(key, _)| {
                                            (key.index() == *field).then_some(*key)
                                        })
                                        .expect("summary variant field must match its shape");
                                    let mut path = SlotPath::new();
                                    path.push(SlotProjection::Field(key));
                                    for projection in suffix {
                                        path.push(projection.clone());
                                    }
                                    GuardedLeaf {
                                        path,
                                        guard: leaf.guard.clone(),
                                        payload_guard: leaf.payload_guard.clone(),
                                        payload: leaf.payload.clone(),
                                    }
                                })
                            })
                            .collect::<Vec<_>>();
                        Some((
                            *variant,
                            self.build_from_leaves(*variant_shape, &variant_leaves, subst)?,
                        ))
                    })
                    .collect::<Option<Box<_>>>()?,
            },
            ShapeChildren::Array { elem, .. } => {
                let symbolic = leaves
                    .iter()
                    .filter_map(|leaf| match leaf.path.as_slice().first() {
                        Some(SlotProjection::Index(index))
                            if !matches!(index, IndexExpr::Const(_)) =>
                        {
                            Some(*index)
                        }
                        _ => None,
                    })
                    .collect::<std::collections::BTreeSet<_>>();
                if symbolic.len() > 1 {
                    return None;
                }
                let binder = self.fresh_binder();
                let default = if let Some(index) = symbolic.first().copied() {
                    let children = strip_matching_leaves(leaves, |projection| {
                        matches!(projection, SlotProjection::Index(candidate) if *candidate == index)
                    });
                    let mut default_subst = subst.clone();
                    default_subst.insert(index, IndexExpr::ValueParam(binder));
                    self.build_from_leaves(*elem, &children, &default_subst)?
                } else {
                    self.empty(*elem)
                };
                let exact = leaves
                    .iter()
                    .filter_map(|leaf| match leaf.path.as_slice().first() {
                        Some(SlotProjection::Index(IndexExpr::Const(index))) => Some(*index),
                        _ => None,
                    })
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .map(|index| {
                        let children = strip_matching_leaves(leaves, |projection| {
                            matches!(projection, SlotProjection::Index(IndexExpr::Const(candidate)) if *candidate == index)
                        });
                        Some((index, self.build_from_leaves(*elem, &children, subst)?))
                    })
                    .collect::<Option<BTreeMap<_, _>>>()?;
                ValueChildren::Array {
                    binder,
                    default,
                    exact,
                }
            }
        };
        Some(self.intern(StructuredValue {
            shape,
            direct,
            children,
        }))
    }

    fn enumerate_from(
        &self,
        value: ValueId<'db, P>,
        scope: ValueScope,
        path: &mut SlotPath<IndexExpr>,
        inherited_guard: &Guard,
        out: &mut Vec<GuardedLeaf<P>>,
    ) {
        let node = self.node(value);
        for entry in &node.direct.scoped(scope).entries {
            if let Some(guard) = inherited_guard.and(&entry.guard) {
                out.push(GuardedLeaf {
                    path: path.clone(),
                    guard,
                    payload_guard: entry.guard.clone(),
                    payload: entry.payload.clone(),
                });
            }
        }
        match &node.children {
            ValueChildren::None => {}
            ValueChildren::Product { fields } => {
                for (field, value) in fields {
                    path.push(SlotProjection::Field(*field));
                    self.enumerate_from(*value, scope, path, inherited_guard, out);
                    path.pop();
                }
            }
            ValueChildren::Sum { variants } => {
                let choice = ChoiceKey::relative(path.clone()).scoped(scope);
                for (variant, value) in variants {
                    let Some(guard) = inherited_guard.with_variant(choice.clone(), *variant) else {
                        continue;
                    };
                    let ValueChildren::Product { fields } = &self.node(*value).children else {
                        continue;
                    };
                    for (field, value) in fields {
                        path.push(SlotProjection::VariantField {
                            variant: *variant,
                            field: field.index(),
                        });
                        self.enumerate_from(*value, scope, path, &guard, out);
                        path.pop();
                    }
                }
            }
            ValueChildren::Array {
                binder,
                default,
                exact,
            } => {
                let ShapeChildren::Array { len, .. } = node.shape.data(self.db).children else {
                    unreachable!()
                };
                let index = IndexExpr::ValueParam(*binder);
                let guard = exact.keys().try_fold(
                    inherited_guard
                        .with_bound(index, len)
                        .expect("array binder bound is satisfiable"),
                    |guard, exact_index| {
                        guard.with_disequality(index, IndexExpr::Const(*exact_index))
                    },
                );
                if let Some(guard) = guard {
                    path.push(SlotProjection::Index(index));
                    self.enumerate_from(*default, scope, path, &guard, out);
                    path.pop();
                }
                for (exact_index, value) in exact {
                    path.push(SlotProjection::Index(IndexExpr::Const(*exact_index)));
                    self.enumerate_from(*value, scope, path, inherited_guard, out);
                    path.pop();
                }
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub(crate) fn is_empty(&self, value: ValueId<'db, P>) -> bool {
        self.enumerate_leaves(value, ValueScope::Relative)
            .is_empty()
    }

    pub(crate) fn shape(&self, value: ValueId<'db, P>) -> ShapeId<'db> {
        self.node(value).shape
    }

    fn specialize_default(
        &mut self,
        default: ValueId<'db, P>,
        binder: ValueIndexId,
        index: usize,
    ) -> ValueId<'db, P> {
        self.substitute(
            default,
            &IndexSubst::from_pair(IndexExpr::ValueParam(binder), IndexExpr::Const(index)),
        )
    }

    fn fresh_binder(&mut self) -> ValueIndexId {
        let binder = ValueIndexId(self.next_binder);
        self.next_binder = self
            .next_binder
            .checked_add(1)
            .expect("structural value binder space exhausted");
        binder
    }

    fn node(&self, value: ValueId<'db, P>) -> &StructuredValue<'db, P> {
        &self.nodes[value.raw as usize]
    }

    fn intern(&mut self, value: StructuredValue<'db, P>) -> ValueId<'db, P> {
        if let Some(id) = self.interned.get(&value) {
            return *id;
        }
        let id = ValueId {
            raw: u32::try_from(self.nodes.len()).expect("structural value id space exhausted"),
            marker: PhantomData,
        };
        self.nodes.push(value.clone());
        self.interned.insert(value, id);
        id
    }
}

fn strip_matching_leaves<P: Clone>(
    leaves: &[GuardedLeaf<P>],
    predicate: impl Fn(&SlotProjection<IndexExpr>) -> bool,
) -> Vec<GuardedLeaf<P>> {
    leaves
        .iter()
        .filter_map(|leaf| {
            let (projection, suffix) = leaf.path.as_slice().split_first()?;
            predicate(projection).then(|| GuardedLeaf {
                path: SlotPath::from_steps(suffix.iter().cloned()),
                guard: leaf.guard.clone(),
                payload_guard: leaf.payload_guard.clone(),
                payload: leaf.payload.clone(),
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::{analysis::ty::ty_def::BorrowKind, test_db::HirAnalysisTestDb};

    use super::super::shape::{CapabilityLeafKind, CapabilityShape};

    #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Payload(u8);

    impl IndexPayload for Payload {
        fn substitute_indices(&self, _subst: &IndexSubst) -> Self {
            *self
        }
    }

    fn leaf_shape<'db>(db: &'db HirAnalysisTestDb) -> ShapeId<'db> {
        ShapeId::new(
            db,
            CapabilityShape {
                direct: Some(CapabilityLeafKind::Borrow(BorrowKind::Mut)),
                children: ShapeChildren::None,
            },
        )
    }

    fn array_shape<'db>(
        db: &'db HirAnalysisTestDb,
        elem: ShapeId<'db>,
        len: usize,
    ) -> ShapeId<'db> {
        ShapeId::new(
            db,
            CapabilityShape {
                direct: None,
                children: ShapeChildren::Array { len, elem },
            },
        )
    }

    fn index_path(index: IndexExpr) -> SlotPath<IndexExpr> {
        SlotPath::from_steps([SlotProjection::Index(index)])
    }

    #[test]
    fn exact_array_replacement_shadows_only_that_member() {
        let db = HirAnalysisTestDb::default();
        let leaf = leaf_shape(&db);
        let array = array_shape(&db, leaf, 1_000_000);
        let mut values = ValueInterner::new(&db);
        let empty = values.empty(leaf);
        let old = values.with_direct(empty, Payload(0));
        let replacement = values.with_direct(empty, Payload(1));
        let original = values.array_repeat(array, old);
        let replaced = values.replace(original, &index_path(IndexExpr::Const(0)), replacement);

        let scope = ValueScope::Local(crate::analysis::semantic::SLocalId::from_u32(0));
        let first = values.project(replaced, &index_path(IndexExpr::Const(0)), scope);
        assert_eq!(
            values
                .enumerate_leaves(first, scope)
                .into_iter()
                .map(|leaf| leaf.payload)
                .collect::<Vec<_>>(),
            vec![Payload(1)]
        );
        let second = values.project(replaced, &index_path(IndexExpr::Const(1)), scope);
        assert_eq!(
            values
                .enumerate_leaves(second, scope)
                .into_iter()
                .map(|leaf| leaf.payload)
                .collect::<Vec<_>>(),
            vec![Payload(0)]
        );
        assert!(values.node_count() < 32);
    }

    #[test]
    fn array_join_restores_branch_local_old_member() {
        let db = HirAnalysisTestDb::default();
        let leaf = leaf_shape(&db);
        let array = array_shape(&db, leaf, 2);
        let mut values = ValueInterner::new(&db);
        let empty = values.empty(leaf);
        let old = values.with_direct(empty, Payload(0));
        let replacement = values.with_direct(empty, Payload(1));
        let original = values.array_repeat(array, old);
        let replaced = values.replace(original, &index_path(IndexExpr::Const(0)), replacement);
        let joined = values.join(original, replaced);
        let selected = values.project(
            joined,
            &index_path(IndexExpr::Const(0)),
            ValueScope::Local(crate::analysis::semantic::SLocalId::from_u32(0)),
        );
        let payloads = values
            .enumerate_leaves(
                selected,
                ValueScope::Local(crate::analysis::semantic::SLocalId::from_u32(0)),
            )
            .into_iter()
            .map(|leaf| leaf.payload)
            .collect::<Vec<_>>();

        assert_eq!(payloads, vec![Payload(0), Payload(1)]);
    }

    #[test]
    fn out_of_bounds_array_replacement_is_unreachable() {
        let db = HirAnalysisTestDb::default();
        let leaf = leaf_shape(&db);
        let array = array_shape(&db, leaf, 2);
        let mut values = ValueInterner::new(&db);
        let empty = values.empty(leaf);
        let old = values.with_direct(empty, Payload(0));
        let original = values.array_repeat(array, old);
        let replacement = values.with_direct(empty, Payload(1));

        assert_eq!(
            values.replace(
                original,
                &index_path(IndexExpr::Const(usize::MAX)),
                replacement,
            ),
            original
        );
    }

    #[test]
    fn structurally_equal_values_are_interned() {
        let db = HirAnalysisTestDb::default();
        let leaf = leaf_shape(&db);
        let mut values = ValueInterner::new(&db);
        let empty = values.empty(leaf);

        assert_eq!(
            values.with_direct(empty, Payload(7)),
            values.with_direct(empty, Payload(7))
        );
    }

    #[test]
    fn dynamic_array_projection_matches_each_concrete_index() {
        let db = HirAnalysisTestDb::default();
        let leaf = leaf_shape(&db);
        let array = array_shape(&db, leaf, 3);
        let mut values = ValueInterner::new(&db);
        let empty = values.empty(leaf);
        let old = values.with_direct(empty, Payload(0));
        let replacement = values.with_direct(empty, Payload(1));
        let original = values.array_repeat(array, old);
        let replaced = values.replace(original, &index_path(IndexExpr::Const(0)), replacement);
        let index = IndexExpr::Runtime(crate::analysis::semantic::SLocalId::from_u32(1));
        let projected = values.project(
            replaced,
            &index_path(index),
            ValueScope::Local(crate::analysis::semantic::SLocalId::from_u32(0)),
        );
        let leaves = values.enumerate_leaves(
            projected,
            ValueScope::Local(crate::analysis::semantic::SLocalId::from_u32(0)),
        );

        for concrete in 0..3 {
            let expected = if concrete == 0 {
                Payload(1)
            } else {
                Payload(0)
            };
            let actual = leaves
                .iter()
                .filter(|leaf| {
                    leaf.guard
                        .with_equality(index, IndexExpr::Const(concrete))
                        .is_some()
                })
                .map(|leaf| leaf.payload)
                .collect::<BTreeSet<_>>();
            assert_eq!(actual, BTreeSet::from([expected]));
        }
    }

    #[test]
    fn join_is_idempotent_commutative_and_associative() {
        let db = HirAnalysisTestDb::default();
        let leaf = leaf_shape(&db);
        let array = array_shape(&db, leaf, 3);
        let mut values = ValueInterner::new(&db);
        let empty = values.empty(leaf);
        let original = values.with_direct(empty, Payload(0));
        let first = values.with_direct(empty, Payload(1));
        let second = values.with_direct(empty, Payload(2));
        let base = values.array_repeat(array, original);
        let left = values.replace(base, &index_path(IndexExpr::Const(0)), first);
        let right = values.replace(base, &index_path(IndexExpr::Const(1)), second);

        assert_eq!(values.join(left, left), left);
        let left_right = values.join(left, right);
        let right_left = values.join(right, left);
        assert_eq!(left_right, right_left);
        let base_left = values.join(base, left);
        let left_associative = values.join(base_left, right);
        let right_associative = values.join(base, left_right);
        assert_eq!(left_associative, right_associative);
    }
}
