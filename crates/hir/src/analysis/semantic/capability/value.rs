//! Shape-checked, hash-consed structural values shared by local state and summaries.
use super::{
    guard::{ChoiceKey, Guard, ValueOccurrence},
    index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
    path::{Projection, StructuralPath},
    semantics::CapabilityClass,
    shape::{ShapeChildren, ShapeId},
};
use crate::analysis::{
    HirAnalysisDb,
    semantic::{FieldIndex, VariantIndex},
};
use rustc_hash::FxHashMap;
use std::{
    collections::{BTreeMap, BTreeSet},
    hash::Hash,
    sync::Arc,
};

pub trait IndexPayload: Clone + Eq + Ord + Hash {
    fn class(&self) -> CapabilityClass;
    fn indices(&self) -> impl Iterator<Item = IndexExpr>;
    fn substitute(&self, substitution: &IndexSubst) -> Self;
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Guarded<P> {
    pub guard: Guard,
    pub payload: P,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueId<'db, P>(Arc<StructuredValue<'db, P>>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct StructuredValue<'db, P> {
    shape: ShapeId<'db>,
    scope: BinderScope,
    direct: Vec<Guarded<P>>,
    children: ValueChildren<'db, P>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum ValueChildren<'db, P> {
    None,
    Product(Box<[(FieldIndex, ValueId<'db, P>)]>),
    Sum(Box<[(VariantIndex, ValueId<'db, P>)]>),
    Array {
        default: ValueId<'db, P>,
        exact: BTreeMap<usize, ValueId<'db, P>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GuardedLeaf<P> {
    pub path: StructuralPath<IndexExpr>,
    pub guard: Guard,
    pub payload: P,
}

/// Widening is explicit, separate from the associative join operation.
#[derive(Clone, Copy, Debug)]
pub struct ValueLimits {
    pub guarded_alternatives: usize,
    pub guard_indices: usize,
    pub guard_nodes: usize,
    pub exact_members: usize,
    pub interned_nodes: usize,
}

impl Default for ValueLimits {
    fn default() -> Self {
        Self {
            guarded_alternatives: 64,
            guard_indices: 32,
            guard_nodes: 4096,
            exact_members: 64,
            interned_nodes: 16_384,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ValueMetrics {
    pub nodes_created: usize,
    pub interner_evictions: usize,
    pub widened_nodes: usize,
}

pub struct ValueInterner<'db, P> {
    db: &'db dyn HirAnalysisDb,
    nodes: FxHashMap<StructuredValue<'db, P>, ValueId<'db, P>>,
    limits: ValueLimits,
    metrics: ValueMetrics,
}

impl<'db, P: IndexPayload> ValueId<'db, P> {
    pub fn shape(&self) -> ShapeId<'db> {
        self.0.shape
    }
    pub fn scope(&self) -> &BinderScope {
        &self.0.scope
    }
    pub fn is_empty(&self) -> bool {
        self.0.direct.is_empty()
            && match &self.0.children {
                ValueChildren::None => true,
                ValueChildren::Product(fields) => fields.iter().all(|(_, child)| child.is_empty()),
                ValueChildren::Sum(variants) => variants.iter().all(|(_, child)| child.is_empty()),
                ValueChildren::Array { default, exact } => {
                    default.is_empty() && exact.values().all(Self::is_empty)
                }
            }
    }
}

impl<'db, P: IndexPayload> ValueInterner<'db, P> {
    pub fn new(db: &'db dyn HirAnalysisDb, limits: ValueLimits) -> Self {
        Self {
            db,
            nodes: FxHashMap::default(),
            limits,
            metrics: ValueMetrics::default(),
        }
    }

    pub fn metrics(&self) -> ValueMetrics {
        self.metrics
    }

    pub fn empty(&mut self, shape: ShapeId<'db>, scope: &BinderScope) -> ValueId<'db, P> {
        let children = match &shape.data(self.db).children {
            ShapeChildren::None => ValueChildren::None,
            ShapeChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, shape)| (*field, self.empty(*shape, scope)))
                    .collect(),
            ),
            ShapeChildren::Sum(_) => ValueChildren::Sum(Box::new([])),
            ShapeChildren::Array { element, .. } => {
                let (scope, _) = scope.bind(IndexNamespace::Value);
                ValueChildren::Array {
                    default: self.empty(*element, &scope),
                    exact: BTreeMap::new(),
                }
            }
        };
        self.intern(StructuredValue {
            shape,
            scope: scope.clone(),
            direct: Vec::new(),
            children,
        })
    }

    pub fn with_direct(
        &mut self,
        value: &ValueId<'db, P>,
        direct: Vec<Guarded<P>>,
    ) -> ValueId<'db, P> {
        let mut node = (*value.0).clone();
        node.direct = direct;
        self.intern(node)
    }

    pub fn product(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        fields: impl IntoIterator<Item = (FieldIndex, ValueId<'db, P>)>,
    ) -> ValueId<'db, P> {
        let ShapeChildren::Product(expected) = &shape.data(self.db).children else {
            panic!("product constructor requires a product shape");
        };
        let mut supplied = BTreeMap::new();
        for (field, value) in fields {
            assert!(
                expected.iter().any(|(key, _)| *key == field),
                "unknown product field"
            );
            assert!(
                supplied.insert(field, value).is_none(),
                "duplicate product field"
            );
        }
        let fields = expected
            .iter()
            .map(|(field, shape)| {
                let value = supplied
                    .remove(field)
                    .unwrap_or_else(|| self.empty(*shape, scope));
                self.check_child(&value, *shape, scope);
                (*field, value)
            })
            .collect();
        self.intern(StructuredValue {
            shape,
            scope: scope.clone(),
            direct: Vec::new(),
            children: ValueChildren::Product(fields),
        })
    }

    pub fn sum(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        variants: impl IntoIterator<Item = (VariantIndex, ValueId<'db, P>)>,
    ) -> ValueId<'db, P> {
        let ShapeChildren::Sum(expected) = &shape.data(self.db).children else {
            panic!("sum constructor requires an enum shape");
        };
        let mut supplied = BTreeMap::new();
        for (variant, value) in variants {
            let child_shape = expected
                .iter()
                .find(|(key, _)| *key == variant)
                .expect("unknown enum variant")
                .1;
            self.check_child(&value, child_shape, scope);
            assert!(
                supplied.insert(variant, value).is_none(),
                "duplicate enum variant"
            );
        }
        supplied.retain(|_, value| !value.is_empty());
        self.intern(StructuredValue {
            shape,
            scope: scope.clone(),
            direct: Vec::new(),
            children: ValueChildren::Sum(supplied.into_iter().collect()),
        })
    }

    pub fn array(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        build: impl FnOnce(&mut Self, &BinderScope, IndexExpr) -> ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        let (nested, binder) = scope.bind(IndexNamespace::Value);
        let default = build(self, &nested, binder);
        self.array_parts(shape, scope, default, BTreeMap::new(), Vec::new())
    }

    pub fn array_repeat(
        &mut self,
        shape: ShapeId<'db>,
        value: &ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        let scope = value.scope().clone();
        self.array(shape, &scope, |this, nested, _| this.lift(value, nested))
    }

    pub fn join(&mut self, lhs: &ValueId<'db, P>, rhs: &ValueId<'db, P>) -> ValueId<'db, P> {
        self.check_child(rhs, lhs.shape(), lhs.scope());
        if lhs == rhs {
            return lhs.clone();
        }
        let mut direct = lhs.0.direct.clone();
        direct.extend(rhs.0.direct.iter().cloned());
        let children = match (&lhs.0.children, &rhs.0.children) {
            (ValueChildren::None, ValueChildren::None) => ValueChildren::None,
            (ValueChildren::Product(left), ValueChildren::Product(right)) => {
                ValueChildren::Product(
                    left.iter()
                        .zip(right)
                        .map(|((field, left), (_, right))| (*field, self.join(left, right)))
                        .collect(),
                )
            }
            (ValueChildren::Sum(left), ValueChildren::Sum(right)) => {
                let mut variants: BTreeMap<_, _> = left.iter().cloned().collect();
                for (variant, value) in right {
                    let joined = variants
                        .get(variant)
                        .map_or_else(|| value.clone(), |left| self.join(left, value));
                    variants.insert(*variant, joined);
                }
                ValueChildren::Sum(variants.into_iter().collect())
            }
            (
                ValueChildren::Array {
                    default: left,
                    exact: left_exact,
                },
                ValueChildren::Array {
                    default: right,
                    exact: right_exact,
                },
            ) => {
                let default = self.join(left, right);
                let keys: BTreeSet<_> = left_exact
                    .keys()
                    .chain(right_exact.keys())
                    .copied()
                    .collect();
                let mut exact = BTreeMap::new();
                for key in keys {
                    let left = self.array_member(lhs, IndexExpr::Const(key));
                    let right = self.array_member(rhs, IndexExpr::Const(key));
                    exact.insert(key, self.join(&left, &right));
                }
                return self.array_parts(lhs.shape(), lhs.scope(), default, exact, direct);
            }
            _ => unreachable!("equal shapes have equal structural node kinds"),
        };
        self.intern(StructuredValue {
            shape: lhs.shape(),
            scope: lhs.scope().clone(),
            direct,
            children,
        })
    }

    pub fn with_guard(&mut self, value: &ValueId<'db, P>, guard: &Guard) -> ValueId<'db, P> {
        assert_eq!(
            value.scope(),
            guard.scope(),
            "guard scope must match its value"
        );
        let direct = value
            .0
            .direct
            .iter()
            .filter_map(|entry| {
                Some(Guarded {
                    guard: entry.guard.and(guard)?,
                    payload: entry.payload.clone(),
                })
            })
            .collect();
        let children = match &value.0.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, child)| (*field, self.with_guard(child, guard)))
                    .collect(),
            ),
            ValueChildren::Sum(variants) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| (*variant, self.with_guard(child, guard)))
                    .filter(|(_, child)| !child.is_empty())
                    .collect(),
            ),
            ValueChildren::Array { default, exact } => {
                let subst =
                    IndexSubst::new(value.scope(), default.scope(), []).expect("nested scope");
                let nested_guard = guard
                    .substitute(&subst)
                    .expect("scope extension preserves guard");
                let default = self.with_guard(default, &nested_guard);
                let exact = exact
                    .iter()
                    .map(|(key, child)| (*key, self.with_guard(child, guard)))
                    .collect();
                return self.array_parts(value.shape(), value.scope(), default, exact, direct);
            }
        };
        self.intern(StructuredValue {
            shape: value.shape(),
            scope: value.scope().clone(),
            direct,
            children,
        })
    }

    pub fn substitute(&mut self, value: &ValueId<'db, P>, subst: &IndexSubst) -> ValueId<'db, P> {
        assert_eq!(
            value.scope(),
            subst.source(),
            "substitution source scope must match"
        );
        let direct = value
            .0
            .direct
            .iter()
            .filter_map(|entry| {
                Some(Guarded {
                    guard: entry.guard.substitute(subst)?,
                    payload: entry.payload.substitute(subst),
                })
            })
            .collect();
        let children = match &value.0.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, child)| (*field, self.substitute(child, subst)))
                    .collect(),
            ),
            ValueChildren::Sum(variants) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| (*variant, self.substitute(child, subst)))
                    .filter(|(_, child)| !child.is_empty())
                    .collect(),
            ),
            ValueChildren::Array { default, exact } => {
                let default = self.substitute(default, &subst.under_binder(IndexNamespace::Value));
                let exact = exact
                    .iter()
                    .map(|(key, child)| (*key, self.substitute(child, subst)))
                    .collect();
                return self.array_parts(
                    value.shape(),
                    subst.destination(),
                    default,
                    exact,
                    direct,
                );
            }
        };
        self.intern(StructuredValue {
            shape: value.shape(),
            scope: subst.destination().clone(),
            direct,
            children,
        })
    }

    pub fn project(
        &mut self,
        value: &ValueId<'db, P>,
        path: &StructuralPath<IndexExpr>,
        occurrence: ValueOccurrence,
    ) -> ValueId<'db, P> {
        let mut current = value.clone();
        let mut prefix = StructuralPath::default();
        for step in path.as_slice() {
            current = match (step, &current.0.children) {
                (Projection::Field(field), ValueChildren::Product(fields)) => fields
                    .iter()
                    .find(|(key, _)| key == field)
                    .expect("invalid product projection")
                    .1
                    .clone(),
                (Projection::VariantField { variant, field }, ValueChildren::Sum(variants)) => {
                    let ShapeChildren::Sum(shapes) = &current.shape().data(self.db).children else {
                        unreachable!()
                    };
                    let shape = shapes
                        .iter()
                        .find(|(key, _)| key == variant)
                        .expect("invalid enum projection")
                        .1;
                    let child = variants
                        .iter()
                        .find(|(key, _)| key == variant)
                        .map(|(_, child)| child.clone())
                        .unwrap_or_else(|| self.empty(shape, current.scope()));
                    let ValueChildren::Product(fields) = &child.0.children else {
                        unreachable!()
                    };
                    let selected = &fields
                        .iter()
                        .find(|(key, _)| key == field)
                        .expect("invalid variant field")
                        .1;
                    let guard = Guard::always(current.scope())
                        .with_variant(ChoiceKey::new(occurrence, prefix.clone()), *variant)
                        .expect("one variant constraint");
                    self.with_guard(selected, &guard)
                }
                (Projection::Index(index), ValueChildren::Array { .. }) => {
                    self.array_member(&current, *index)
                }
                _ => panic!("structural path does not match capability shape"),
            };
            prefix = prefix.appended(*step);
        }
        current
    }

    pub fn replace(
        &mut self,
        value: &ValueId<'db, P>,
        path: &StructuralPath<IndexExpr>,
        replacement: &ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        self.replace_steps(value, path.as_slice(), replacement)
    }

    fn replace_steps(
        &mut self,
        value: &ValueId<'db, P>,
        steps: &[Projection<IndexExpr>],
        replacement: &ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        assert_eq!(
            value.scope(),
            replacement.scope(),
            "replacement scopes must match"
        );
        let Some((step, rest)) = steps.split_first() else {
            self.check_child(replacement, value.shape(), value.scope());
            return replacement.clone();
        };
        let mut node = (*value.0).clone();
        match (step, &mut node.children) {
            (Projection::Field(field), ValueChildren::Product(fields)) => {
                let child = &mut fields
                    .iter_mut()
                    .find(|(key, _)| key == field)
                    .expect("invalid replacement field")
                    .1;
                *child = self.replace_steps(child, rest, replacement);
            }
            (Projection::VariantField { variant, field }, ValueChildren::Sum(variants)) => {
                let ShapeChildren::Sum(shapes) = &value.shape().data(self.db).children else {
                    unreachable!()
                };
                let shape = shapes
                    .iter()
                    .find(|(key, _)| key == variant)
                    .expect("invalid replacement variant")
                    .1;
                let mut updated: BTreeMap<_, _> = variants.iter().cloned().collect();
                let child = updated
                    .entry(*variant)
                    .or_insert_with(|| self.empty(shape, value.scope()));
                let mut path = vec![Projection::Field(*field)];
                path.extend_from_slice(rest);
                *child = self.replace_steps(child, &path, replacement);
                updated.retain(|_, child| !child.is_empty());
                *variants = updated.into_iter().collect();
            }
            (Projection::Index(index), ValueChildren::Array { default, exact }) => {
                let ShapeChildren::Array { len, .. } = value.shape().data(self.db).children else {
                    unreachable!()
                };
                value
                    .scope()
                    .validate(*index)
                    .expect("free replacement index");
                if let IndexExpr::Const(key) = index {
                    assert!(*key < len, "constant array replacement is out of bounds");
                    let old = self.array_member(value, *index);
                    exact.insert(*key, self.replace_steps(&old, rest, replacement));
                } else {
                    let lifted = self.lift(replacement, default.scope());
                    let (_, binder) = value.scope().bind(IndexNamespace::Value);
                    let changed = self.replace_steps(default, rest, &lifted);
                    *default = self.select_update(default, &changed, binder, *index);
                    for (key, old) in exact.iter_mut() {
                        let changed = self.replace_steps(old, rest, replacement);
                        *old = self.select_update(old, &changed, IndexExpr::Const(*key), *index);
                    }
                }
                return self.array_parts(
                    value.shape(),
                    value.scope(),
                    default.clone(),
                    exact.clone(),
                    node.direct,
                );
            }
            _ => panic!("replacement path does not match capability shape"),
        }
        self.intern(node)
    }

    fn select_update(
        &mut self,
        old: &ValueId<'db, P>,
        changed: &ValueId<'db, P>,
        member: IndexExpr,
        selector: IndexExpr,
    ) -> ValueId<'db, P> {
        let scope = old.scope();
        let kept = Guard::always(scope)
            .with_disequality(member, selector)
            .map(|guard| self.with_guard(old, &guard))
            .unwrap_or_else(|| self.empty(old.shape(), scope));
        let changed = Guard::always(scope)
            .with_equality(member, selector)
            .map(|guard| self.with_guard(changed, &guard))
            .unwrap_or_else(|| self.empty(old.shape(), scope));
        self.join(&kept, &changed)
    }

    fn array_member(&mut self, value: &ValueId<'db, P>, index: IndexExpr) -> ValueId<'db, P> {
        let ShapeChildren::Array { len, element } = value.shape().data(self.db).children else {
            panic!("array required")
        };
        let ValueChildren::Array { default, exact } = &value.0.children else {
            unreachable!()
        };
        value.scope().validate(index).expect("free array index");
        if let IndexExpr::Const(key) = index {
            assert!(key < len, "constant array projection is out of bounds");
            if let Some(exact) = exact.get(&key) {
                return exact.clone();
            }
            return self.specialize(default, value.scope(), index);
        }
        let mut result = self.empty(element, value.scope());
        let mut default_guard = Guard::always(value.scope()).with_bound(index, len);
        for (key, child) in exact {
            let guard = Guard::always(value.scope())
                .with_equality(index, IndexExpr::Const(*key))
                .expect("runtime index can equal exact member");
            let child = self.with_guard(child, &guard);
            result = self.join(&result, &child);
            default_guard = default_guard
                .and_then(|guard| guard.with_disequality(index, IndexExpr::Const(*key)));
        }
        if let Some(guard) = default_guard {
            let child = self.specialize(default, value.scope(), index);
            let child = self.with_guard(&child, &guard);
            result = self.join(&result, &child);
        }
        result
    }

    fn specialize(
        &mut self,
        value: &ValueId<'db, P>,
        scope: &BinderScope,
        index: IndexExpr,
    ) -> ValueId<'db, P> {
        let (nested, binder) = scope.bind(IndexNamespace::Value);
        let subst =
            IndexSubst::new(&nested, scope, [(binder, index)]).expect("valid array specialization");
        self.substitute(value, &subst)
    }

    fn lift(&mut self, value: &ValueId<'db, P>, scope: &BinderScope) -> ValueId<'db, P> {
        let subst = IndexSubst::new(value.scope(), scope, []).expect("scope extension");
        self.substitute(value, &subst)
    }

    fn array_parts(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        default: ValueId<'db, P>,
        mut exact: BTreeMap<usize, ValueId<'db, P>>,
        direct: Vec<Guarded<P>>,
    ) -> ValueId<'db, P> {
        let ShapeChildren::Array { len, element } = shape.data(self.db).children else {
            panic!("array shape required")
        };
        let (nested, _) = scope.bind(IndexNamespace::Value);
        self.check_child(&default, element, &nested);
        exact.retain(|key, child| {
            assert!(*key < len, "exact member is out of bounds");
            self.check_child(child, element, scope);
            *child != self.specialize(&default, scope, IndexExpr::Const(*key))
        });
        self.intern(StructuredValue {
            shape,
            scope: scope.clone(),
            direct,
            children: ValueChildren::Array { default, exact },
        })
    }

    pub fn leaves(
        &self,
        value: &ValueId<'db, P>,
        occurrence: ValueOccurrence,
    ) -> Vec<GuardedLeaf<P>> {
        let mut leaves = Vec::new();
        self.collect_leaves(
            value,
            occurrence,
            &StructuralPath::default(),
            &Guard::always(value.scope()),
            &mut leaves,
        );
        leaves
    }

    fn collect_leaves(
        &self,
        value: &ValueId<'db, P>,
        occurrence: ValueOccurrence,
        path: &StructuralPath<IndexExpr>,
        guard: &Guard,
        leaves: &mut Vec<GuardedLeaf<P>>,
    ) {
        for entry in &value.0.direct {
            if let Some(guard) = entry.guard.and(guard) {
                leaves.push(GuardedLeaf {
                    path: path.clone(),
                    guard,
                    payload: entry.payload.clone(),
                });
            }
        }
        match &value.0.children {
            ValueChildren::None => {}
            ValueChildren::Product(fields) => {
                for (field, child) in fields {
                    self.collect_leaves(
                        child,
                        occurrence,
                        &path.appended(Projection::Field(*field)),
                        guard,
                        leaves,
                    );
                }
            }
            ValueChildren::Sum(variants) => {
                for (variant, child) in variants {
                    if let Some(guard) =
                        guard.with_variant(ChoiceKey::new(occurrence, path.clone()), *variant)
                    {
                        let ValueChildren::Product(fields) = &child.0.children else {
                            unreachable!()
                        };
                        for (field, child) in fields {
                            self.collect_leaves(
                                child,
                                occurrence,
                                &path.appended(Projection::VariantField {
                                    variant: *variant,
                                    field: *field,
                                }),
                                &guard,
                                leaves,
                            );
                        }
                    }
                }
            }
            ValueChildren::Array { default, exact } => {
                let ShapeChildren::Array { len, .. } = value.shape().data(self.db).children else {
                    unreachable!()
                };
                let (nested, binder) = value.scope().bind(IndexNamespace::Value);
                let subst = IndexSubst::new(value.scope(), &nested, []).expect("scope extension");
                let mut default_guard = guard
                    .substitute(&subst)
                    .and_then(|guard| guard.with_bound(binder, len));
                for (key, child) in exact {
                    self.collect_leaves(
                        child,
                        occurrence,
                        &path.appended(Projection::Index(IndexExpr::Const(*key))),
                        guard,
                        leaves,
                    );
                    default_guard = default_guard
                        .and_then(|guard| guard.with_disequality(binder, IndexExpr::Const(*key)));
                }
                if let Some(guard) = default_guard {
                    self.collect_leaves(
                        default,
                        occurrence,
                        &path.appended(Projection::Index(binder)),
                        &guard,
                        leaves,
                    );
                }
            }
        }
    }

    pub fn map_payloads<Q: IndexPayload>(
        &self,
        value: &ValueId<'db, P>,
        destination: &mut ValueInterner<'db, Q>,
        mut map: impl FnMut(&Guarded<P>) -> Vec<Guarded<Q>>,
    ) -> ValueId<'db, Q> {
        Self::map_node(value, destination, &mut map)
    }

    fn map_node<Q: IndexPayload>(
        value: &ValueId<'db, P>,
        destination: &mut ValueInterner<'db, Q>,
        map: &mut impl FnMut(&Guarded<P>) -> Vec<Guarded<Q>>,
    ) -> ValueId<'db, Q> {
        let direct = value.0.direct.iter().flat_map(&mut *map).collect();
        let children = match &value.0.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, child)| (*field, Self::map_node(child, destination, map)))
                    .collect(),
            ),
            ValueChildren::Sum(variants) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| (*variant, Self::map_node(child, destination, map)))
                    .filter(|(_, child)| !child.is_empty())
                    .collect(),
            ),
            ValueChildren::Array { default, exact } => {
                let default = Self::map_node(default, destination, map);
                let exact = exact
                    .iter()
                    .map(|(key, child)| (*key, Self::map_node(child, destination, map)))
                    .collect();
                return destination.array_parts(
                    value.shape(),
                    value.scope(),
                    default,
                    exact,
                    direct,
                );
            }
        };
        destination.intern(StructuredValue {
            shape: value.shape(),
            scope: value.scope().clone(),
            direct,
            children,
        })
    }

    /// Sound widening retains every payload, dropping guards and sparse partitions when
    /// limits are exceeded. It may introduce aliases, but cannot remove an existing one.
    pub fn widen(&mut self, value: &ValueId<'db, P>) -> ValueId<'db, P> {
        self.widen_node(value, false)
    }

    fn widen_node(&mut self, value: &ValueId<'db, P>, force: bool) -> ValueId<'db, P> {
        let weaken = force
            || value.0.direct.len() > self.limits.guarded_alternatives
            || value.0.direct.iter().any(|entry| {
                entry.guard.indices().len() > self.limits.guard_indices
                    || entry.guard.node_count() > self.limits.guard_nodes
            });
        let direct = value
            .0
            .direct
            .iter()
            .map(|entry| Guarded {
                guard: if weaken {
                    Guard::always(value.scope())
                } else {
                    entry.guard.clone()
                },
                payload: entry.payload.clone(),
            })
            .collect();
        let children = match &value.0.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, child)| (*field, self.widen_node(child, force)))
                    .collect(),
            ),
            ValueChildren::Sum(variants) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| (*variant, self.widen_node(child, force)))
                    .collect(),
            ),
            ValueChildren::Array { default, exact } => {
                let collapse = force || exact.len() > self.limits.exact_members;
                let mut default = self.widen_node(default, collapse);
                let mut updated = BTreeMap::new();
                for (key, child) in exact {
                    let child = self.widen_node(child, collapse);
                    if collapse {
                        let child = self.lift(&child, default.scope());
                        default = self.join(&default, &child);
                    } else {
                        updated.insert(*key, child);
                    }
                }
                let result =
                    self.array_parts(value.shape(), value.scope(), default, updated, direct);
                if &result != value {
                    self.metrics.widened_nodes += 1;
                }
                return result;
            }
        };
        let result = self.intern(StructuredValue {
            shape: value.shape(),
            scope: value.scope().clone(),
            direct,
            children,
        });
        if &result != value {
            self.metrics.widened_nodes += 1;
        }
        result
    }

    fn check_child(&self, value: &ValueId<'db, P>, shape: ShapeId<'db>, scope: &BinderScope) {
        assert_eq!(value.shape(), shape, "structural child shape mismatch");
        assert_eq!(
            value.scope(),
            scope,
            "structural child binder scope mismatch"
        );
    }

    fn intern(&mut self, mut node: StructuredValue<'db, P>) -> ValueId<'db, P> {
        for entry in &node.direct {
            assert_eq!(
                entry.guard.scope(),
                &node.scope,
                "payload guard scope mismatch"
            );
            assert_eq!(
                node.shape.direct(self.db).map(|semantics| semantics.class),
                Some(entry.payload.class()),
                "payload capability class mismatch"
            );
            for index in entry.payload.indices() {
                node.scope.validate(index).expect("free payload binder");
            }
        }
        let mut canonical = BTreeMap::<P, Guard>::new();
        for entry in node.direct {
            canonical
                .entry(entry.payload)
                .and_modify(|guard| *guard = guard.or(&entry.guard))
                .or_insert(entry.guard);
        }
        node.direct = canonical
            .into_iter()
            .map(|(payload, guard)| Guarded { guard, payload })
            .collect();
        if let Some(value) = self.nodes.get(&node) {
            return value.clone();
        }
        // IDs have structural equality, so cache eviction never changes domain equality.
        // Live values keep their nodes alive independently of the interning cache.
        if self.nodes.len() >= self.limits.interned_nodes {
            self.nodes.clear();
            self.metrics.interner_evictions += 1;
        }
        let value = ValueId(Arc::new(node.clone()));
        self.nodes.insert(node, value.clone());
        self.metrics.nodes_created += 1;
        value
    }
}
