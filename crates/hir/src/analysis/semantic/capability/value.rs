//! Shape-checked, hash-consed structural values shared by local state and summaries.
use super::{
    guard::{ChoiceKey, Guard, ValueOccurrence},
    index::{BinderScope, IndexError, IndexExpr, IndexNamespace, IndexSubst},
    path::{Projection, StructuralPath},
    semantics::{CapabilityClass, CapabilitySemantics},
    shape::{ShapeChildren, ShapeId},
};
use crate::analysis::{
    HirAnalysisDb,
    semantic::{FieldIndex, VariantIndex},
};
use rustc_hash::FxHashMap;
use std::{
    collections::{BTreeMap, BTreeSet, btree_map::Entry},
    hash::Hash,
    sync::Arc,
};

pub trait IndexPayload<'db>: Clone + Eq + Ord + Hash {
    fn accepts_class(&self, class: CapabilityClass) -> bool;
    fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>>;
    fn substitute(&self, db: &'db dyn HirAnalysisDb, substitution: &IndexSubst<'db>) -> Self;
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Guarded<'db, P> {
    pub guard: Guard<'db>,
    pub payload: P,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ValueId<'db, P>(Arc<StructuredValue<'db, P>>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct StructuredValue<'db, P> {
    shape: ShapeId<'db>,
    scope: BinderScope,
    direct: Vec<Guarded<'db, P>>,
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
pub struct GuardedLeaf<'db, P> {
    pub semantics: CapabilitySemantics<'db>,
    pub path: StructuralPath<IndexExpr<'db>>,
    pub guard: Guard<'db>,
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
    pub(super) db: &'db dyn HirAnalysisDb,
    nodes: FxHashMap<StructuredValue<'db, P>, ValueId<'db, P>>,
    limits: ValueLimits,
    metrics: ValueMetrics,
}

impl<'db, P: IndexPayload<'db>> ValueId<'db, P> {
    pub fn direct(&self) -> &[Guarded<'db, P>] {
        &self.0.direct
    }

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

impl<'db, P: IndexPayload<'db>> ValueInterner<'db, P> {
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
            ShapeChildren::None | ShapeChildren::EmptyArray => ValueChildren::None,
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

    /// Build every capability slot, retaining a single lexical family for each array.
    pub fn from_shape(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        mut leaf: impl FnMut(
            CapabilitySemantics<'db>,
            &StructuralPath<IndexExpr<'db>>,
            &BinderScope,
        ) -> Vec<Guarded<'db, P>>,
    ) -> ValueId<'db, P> {
        self.build_shape(shape, scope, &StructuralPath::default(), &mut leaf)
    }

    fn build_shape(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        path: &StructuralPath<IndexExpr<'db>>,
        leaf: &mut impl FnMut(
            CapabilitySemantics<'db>,
            &StructuralPath<IndexExpr<'db>>,
            &BinderScope,
        ) -> Vec<Guarded<'db, P>>,
    ) -> ValueId<'db, P> {
        let direct = shape
            .direct(self.db)
            .map_or_else(Vec::new, |semantics| leaf(semantics, path, scope));
        let children = match &shape.data(self.db).children {
            ShapeChildren::None | ShapeChildren::EmptyArray => ValueChildren::None,
            ShapeChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, shape)| {
                        (
                            *field,
                            self.build_shape(
                                *shape,
                                scope,
                                &path.appended(Projection::Field(*field)),
                                leaf,
                            ),
                        )
                    })
                    .collect(),
            ),
            ShapeChildren::Sum(variants) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, shape)| {
                        let ShapeChildren::Product(fields) = &shape.data(self.db).children else {
                            unreachable!("enum variant shape")
                        };
                        let children = ValueChildren::Product(
                            fields
                                .iter()
                                .map(|(field, child)| {
                                    (
                                        *field,
                                        self.build_shape(
                                            *child,
                                            scope,
                                            &path.appended(Projection::VariantField {
                                                variant: *variant,
                                                field: *field,
                                            }),
                                            leaf,
                                        ),
                                    )
                                })
                                .collect(),
                        );
                        (
                            *variant,
                            self.intern(StructuredValue {
                                shape: *shape,
                                scope: scope.clone(),
                                direct: Vec::new(),
                                children,
                            }),
                        )
                    })
                    .filter(|(_, child)| !child.is_empty())
                    .collect(),
            ),
            ShapeChildren::Array { element, .. } => {
                let (nested, index) = scope.bind(IndexNamespace::Value);
                ValueChildren::Array {
                    default: self.build_shape(
                        *element,
                        &nested,
                        &path.appended(Projection::Index(index)),
                        leaf,
                    ),
                    exact: BTreeMap::new(),
                }
            }
        };
        self.intern(StructuredValue {
            shape,
            scope: scope.clone(),
            direct,
            children,
        })
    }

    /// Retype a verifier-approved structural repack. Array mappings describe the
    /// element shape; they never limit transfer to the representative member zero.
    pub fn repack(&mut self, value: &ValueId<'db, P>, shape: ShapeId<'db>) -> ValueId<'db, P> {
        self.repack_with(value, shape, &mut |payload, _, _| payload.clone())
    }

    pub fn repack_with(
        &mut self,
        value: &ValueId<'db, P>,
        shape: ShapeId<'db>,
        map: &mut impl FnMut(&P, CapabilitySemantics<'db>, CapabilitySemantics<'db>) -> P,
    ) -> ValueId<'db, P> {
        if value.shape() == shape {
            return value.clone();
        }
        let direct = value
            .direct()
            .iter()
            .map(|entry| Guarded {
                guard: entry.guard.clone(),
                payload: map(
                    &entry.payload,
                    value
                        .shape()
                        .direct(self.db)
                        .expect("source capability semantics"),
                    shape.direct(self.db).expect("target capability semantics"),
                ),
            })
            .collect();
        let children = match (&value.0.children, &shape.data(self.db).children) {
            (ValueChildren::None, ShapeChildren::None | ShapeChildren::EmptyArray) => {
                ValueChildren::None
            }
            (ValueChildren::Product(fields), ShapeChildren::Product(shapes)) => {
                assert_eq!(fields.len(), shapes.len(), "repack product shape mismatch");
                ValueChildren::Product(
                    fields
                        .iter()
                        .zip(shapes)
                        .map(|((field, child), (expected, shape))| {
                            assert_eq!(field, expected, "repack field mismatch");
                            (*field, self.repack_with(child, *shape, map))
                        })
                        .collect(),
                )
            }
            (ValueChildren::Sum(variants), ShapeChildren::Sum(shapes)) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| {
                        let shape = shapes
                            .iter()
                            .find(|(key, _)| key == variant)
                            .expect("repack variant mismatch")
                            .1;
                        (*variant, self.repack_with(child, shape, map))
                    })
                    .collect(),
            ),
            (ValueChildren::Array { default, exact }, ShapeChildren::Array { len, element }) => {
                let ShapeChildren::Array {
                    len: source_len, ..
                } = value.shape().children(self.db)
                else {
                    unreachable!()
                };
                assert_eq!(source_len, len, "repack array length mismatch");
                let default = self.repack_with(default, *element, map);
                let exact = exact
                    .iter()
                    .map(|(index, child)| (*index, self.repack_with(child, *element, map)))
                    .collect();
                return self.array_parts(shape, value.scope(), default, exact, direct);
            }
            _ => panic!("repack structural shape mismatch"),
        };
        self.intern(StructuredValue {
            shape,
            scope: value.scope().clone(),
            direct,
            children,
        })
    }

    pub fn with_direct(
        &mut self,
        value: &ValueId<'db, P>,
        direct: Vec<Guarded<'db, P>>,
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
        build: impl FnOnce(&mut Self, &BinderScope, IndexExpr<'db>) -> ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        if matches!(shape.children(self.db), ShapeChildren::EmptyArray) {
            return self.empty(shape, scope);
        }
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

    pub fn with_guard(&mut self, value: &ValueId<'db, P>, guard: &Guard<'db>) -> ValueId<'db, P> {
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
                    guard: entry.guard.and(&guard.in_scope(entry.guard.scope()))?,
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

    pub fn substitute(
        &mut self,
        value: &ValueId<'db, P>,
        subst: &IndexSubst<'db>,
    ) -> ValueId<'db, P> {
        assert_eq!(
            value.scope(),
            subst.source(),
            "substitution source scope must match"
        );
        let shape = value.shape().substitute(self.db, subst);
        let direct = value
            .0
            .direct
            .iter()
            .filter_map(|entry| {
                let subst = subst.under_existentials(entry.guard.scope());
                Some(Guarded {
                    guard: entry.guard.substitute(&subst)?,
                    payload: entry.payload.substitute(self.db, &subst),
                })
            })
            .collect();
        let children = match &value.0.children {
            ValueChildren::Array { .. }
                if matches!(shape.children(self.db), ShapeChildren::EmptyArray) =>
            {
                ValueChildren::None
            }
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
                    .filter(|(key, _)| match shape.children(self.db) {
                        ShapeChildren::Array { len, .. } => {
                            len.known().is_none_or(|len| **key < len)
                        }
                        _ => unreachable!(),
                    })
                    .map(|(key, child)| (*key, self.substitute(child, subst)))
                    .collect();
                return self.array_parts(shape, subst.destination(), default, exact, direct);
            }
        };
        self.intern(StructuredValue {
            shape,
            scope: subst.destination().clone(),
            direct,
            children,
        })
    }

    /// Close witnesses introduced while resolving one source clause. The node
    /// keeps its surrounding shape scope; each direct alternative owns its extra
    /// existential variables, including under array member binders.
    pub fn close_existentials(
        &mut self,
        value: &ValueId<'db, P>,
        scope: &BinderScope,
    ) -> ValueId<'db, P> {
        value
            .scope()
            .existential_extension_of(scope)
            .expect("closing owned witnesses");
        let children = match &value.0.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, child)| (*field, self.close_existentials(child, scope)))
                    .collect(),
            ),
            ValueChildren::Sum(variants) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| (*variant, self.close_existentials(child, scope)))
                    .collect(),
            ),
            ValueChildren::Array { default, exact } => {
                let (nested, _) = scope.bind(IndexNamespace::Value);
                let default = self.close_existentials(default, &nested);
                let exact = exact
                    .iter()
                    .map(|(index, child)| (*index, self.close_existentials(child, scope)))
                    .collect();
                return self.array_parts(
                    value.shape(),
                    scope,
                    default,
                    exact,
                    value.0.direct.clone(),
                );
            }
        };
        self.intern(StructuredValue {
            shape: value.shape(),
            scope: scope.clone(),
            direct: value.0.direct.clone(),
            children,
        })
    }

    pub fn map_guards(
        &mut self,
        value: &ValueId<'db, P>,
        mut map: impl FnMut(&Guard<'db>) -> Option<Guard<'db>>,
    ) -> ValueId<'db, P> {
        Self::map_node(
            value,
            &StructuralPath::default(),
            &Guard::always(value.scope()),
            self,
            &mut |_, _, entry, _| {
                map(&entry.guard)
                    .map(|guard| Guarded {
                        guard,
                        payload: entry.payload.clone(),
                    })
                    .into_iter()
                    .collect()
            },
        )
    }

    /// A statically out-of-bounds selection has no reachable result.
    pub fn project(
        &mut self,
        value: &ValueId<'db, P>,
        path: &StructuralPath<IndexExpr<'db>>,
        occurrence: ValueOccurrence,
    ) -> Option<ValueId<'db, P>> {
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
                    if let ShapeChildren::Array { len, .. } = current.shape().children(self.db)
                        && let IndexExpr::Const(index) = index
                        && len.known().is_some_and(|len| *index >= len)
                    {
                        return None;
                    }
                    self.array_member(&current, *index)
                }
                (Projection::Index(_), ValueChildren::None)
                    if matches!(current.shape().children(self.db), ShapeChildren::EmptyArray) =>
                {
                    return None;
                }
                _ => panic!("structural path does not match capability shape"),
            };
            prefix = prefix.appended(*step);
        }
        Some(current)
    }

    pub fn replace(
        &mut self,
        value: &ValueId<'db, P>,
        path: &StructuralPath<IndexExpr<'db>>,
        replacement: &ValueId<'db, P>,
    ) -> ValueId<'db, P> {
        assert_eq!(
            value.scope(),
            replacement.scope(),
            "replacement scopes must match"
        );
        self.replace_family(
            value,
            path,
            replacement,
            &Guard::always(replacement.scope()),
            &Guarded {
                guard: Guard::always(value.scope()),
                payload: value
                    .scope()
                    .variables()
                    .map(|index| (index, index))
                    .collect(),
            },
        )
        .expect("an ordinary replacement introduces no free family binders")
    }

    /// Replace a guarded family without enumerating its members. Source binders
    /// selected by the destination path bind to the destination array's lexical
    /// members. Bindings supplied by a referent root stay distinct from those
    /// introduced by array traversal. Every source binder must be accounted for.
    pub fn replace_family(
        &mut self,
        value: &ValueId<'db, P>,
        path: &StructuralPath<IndexExpr<'db>>,
        replacement: &ValueId<'db, P>,
        guard: &Guard<'db>,
        context: &Guarded<'db, BTreeMap<IndexExpr<'db>, IndexExpr<'db>>>,
    ) -> Result<ValueId<'db, P>, IndexError<'db>> {
        assert_eq!(
            replacement.scope(),
            guard.scope(),
            "replacement guard scope mismatch"
        );
        assert_eq!(
            value.scope(),
            context.guard.scope(),
            "destination guard scope mismatch"
        );
        for index in path.indices() {
            replacement.scope().validate(index)?;
        }
        for (source, destination) in &context.payload {
            replacement.scope().validate(*source)?;
            value.scope().validate(*destination)?;
        }
        self.replace_steps(value, path.as_slice(), replacement, guard, context)
    }

    fn replace_steps(
        &mut self,
        value: &ValueId<'db, P>,
        steps: &[Projection<IndexExpr<'db>>],
        replacement: &ValueId<'db, P>,
        guard: &Guard<'db>,
        context: &Guarded<'db, BTreeMap<IndexExpr<'db>, IndexExpr<'db>>>,
    ) -> Result<ValueId<'db, P>, IndexError<'db>> {
        let Some((step, rest)) = steps.split_first() else {
            let mut scope = value.scope().clone();
            let mut bindings = context.payload.clone();
            for index in replacement.scope().variables() {
                if let Entry::Vacant(entry) = bindings.entry(index) {
                    if index.bound_namespace() != Some(IndexNamespace::Existential) {
                        return Err(IndexError::FreeBinder(index));
                    }
                    let (nested, witness) = scope.bind(IndexNamespace::Existential);
                    scope = nested;
                    entry.insert(witness);
                }
            }
            let substitution = IndexSubst::new(replacement.scope(), &scope, bindings)?;
            let replacement = self.substitute(replacement, &substitution);
            assert_eq!(
                replacement.shape(),
                value.shape(),
                "replacement shape mismatch: replacement={:?}; destination={:?}",
                replacement.shape().data(self.db),
                value.shape().data(self.db),
            );
            let Some(guard) = guard
                .substitute(&substitution)
                .and_then(|guard| guard.and(&context.guard.in_scope(&scope)))
            else {
                return Ok(value.clone());
            };
            let kept = if scope != *value.scope() {
                value.clone()
            } else {
                Guard::always(value.scope())
                    .difference(&guard)
                    .map(|guard| self.with_guard(value, &guard))
                    .unwrap_or_else(|| self.empty(value.shape(), value.scope()))
            };
            let changed = self.with_guard(&replacement, &guard);
            let changed = self.close_existentials(&changed, value.scope());
            return Ok(self.join(&kept, &changed));
        };
        let mut node = (*value.0).clone();
        match (step, &mut node.children) {
            (Projection::Field(field), ValueChildren::Product(fields)) => {
                let child = &mut fields
                    .iter_mut()
                    .find(|(key, _)| key == field)
                    .expect("invalid replacement field")
                    .1;
                *child = self.replace_steps(child, rest, replacement, guard, context)?;
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
                *child = self.replace_steps(child, &path, replacement, guard, context)?;
                updated.retain(|_, child| !child.is_empty());
                *variants = updated.into_iter().collect();
            }
            (Projection::Index(index), ValueChildren::Array { default, exact }) => {
                let ShapeChildren::Array { len, .. } = value.shape().data(self.db).children else {
                    unreachable!()
                };
                let selector = context.payload.get(index).copied().unwrap_or(*index);
                let binds_member =
                    matches!(index, IndexExpr::Bound(_)) && !context.payload.contains_key(index);
                if let IndexExpr::Const(key) = selector {
                    if len.known().is_some_and(|len| key >= len) {
                        return Ok(value.clone());
                    }
                    let old = self.array_member(value, selector);
                    exact.insert(
                        key,
                        self.replace_steps(&old, rest, replacement, guard, context)?,
                    );
                } else {
                    let (_, binder) = value.scope().bind(IndexNamespace::Value);
                    let lift = IndexSubst::new(value.scope(), default.scope(), [])?;
                    let nested_guard = context.guard.substitute(&lift).expect("scope extension");
                    let mut nested = Guarded {
                        guard: nested_guard,
                        payload: context.payload.clone(),
                    };
                    if binds_member {
                        nested.payload.insert(*index, binder);
                    } else if let Some(condition) = nested.guard.with_equality(binder, selector) {
                        nested.guard = condition;
                    } else {
                        return Ok(value.clone());
                    }
                    *default = self.replace_steps(default, rest, replacement, guard, &nested)?;
                    for (key, old) in exact.iter_mut() {
                        let mut selected = context.clone();
                        if binds_member {
                            selected.payload.insert(*index, IndexExpr::Const(*key));
                        } else if let Some(condition) = selected
                            .guard
                            .with_equality(IndexExpr::Const(*key), selector)
                        {
                            selected.guard = condition;
                        } else {
                            continue;
                        }
                        *old = self.replace_steps(old, rest, replacement, guard, &selected)?;
                    }
                }
                return Ok(self.array_parts(
                    value.shape(),
                    value.scope(),
                    default.clone(),
                    exact.clone(),
                    node.direct,
                ));
            }
            (Projection::Index(_), ValueChildren::None)
                if matches!(value.shape().children(self.db), ShapeChildren::EmptyArray) =>
            {
                return Ok(value.clone());
            }
            _ => panic!("replacement path does not match capability shape"),
        }
        Ok(self.intern(node))
    }

    fn array_member(&mut self, value: &ValueId<'db, P>, index: IndexExpr<'db>) -> ValueId<'db, P> {
        let ShapeChildren::Array { len, element } = value.shape().data(self.db).children else {
            panic!("array required")
        };
        let ValueChildren::Array { default, exact } = &value.0.children else {
            unreachable!()
        };
        value.scope().validate(index).expect("free array index");
        if let IndexExpr::Const(key) = index {
            assert!(
                len.known().is_none_or(|len| key < len),
                "constant array projection is out of bounds"
            );
            let selected = exact
                .get(&key)
                .cloned()
                .unwrap_or_else(|| self.specialize(default, value.scope(), index));
            return Guard::always(value.scope())
                .with_bound(index, len)
                .map(|guard| self.with_guard(&selected, &guard))
                .unwrap_or_else(|| self.empty(element, value.scope()));
        }
        let mut result = self.empty(element, value.scope());
        let mut default_guard = Guard::always(value.scope()).with_bound(index, len);
        for (key, child) in exact {
            if let Some(guard) = Guard::always(value.scope())
                .with_bound(index, len)
                .and_then(|guard| guard.with_equality(index, IndexExpr::Const(*key)))
            {
                let child = self.with_guard(child, &guard);
                result = self.join(&result, &child);
            }
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
        index: IndexExpr<'db>,
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
        direct: Vec<Guarded<'db, P>>,
    ) -> ValueId<'db, P> {
        let ShapeChildren::Array { len, element } = shape.data(self.db).children else {
            panic!("array shape required")
        };
        let (nested, _) = scope.bind(IndexNamespace::Value);
        self.check_child(&default, element, &nested);
        exact.retain(|key, child| {
            assert!(
                len.known().is_none_or(|len| *key < len),
                "exact member is out of bounds"
            );
            self.check_child(child, element, scope);
            let index = IndexExpr::Const(*key);
            let guard = Guard::always(scope)
                .with_bound(index, len)
                .expect("reachable exact member");
            *child = self.with_guard(child, &guard);
            let fallback = self.specialize(&default, scope, index);
            let fallback = self.with_guard(&fallback, &guard);
            *child != fallback
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
    ) -> Vec<GuardedLeaf<'db, P>> {
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
        path: &StructuralPath<IndexExpr<'db>>,
        guard: &Guard<'db>,
        leaves: &mut Vec<GuardedLeaf<'db, P>>,
    ) {
        for entry in &value.0.direct {
            if let Some(guard) = entry.guard.and(&guard.in_scope(entry.guard.scope())) {
                leaves.push(GuardedLeaf {
                    semantics: value
                        .shape()
                        .direct(self.db)
                        .expect("capability leaf shape"),
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
                    if let Some(guard) = guard.with_bound(IndexExpr::Const(*key), len) {
                        self.collect_leaves(
                            child,
                            occurrence,
                            &path.appended(Projection::Index(IndexExpr::Const(*key))),
                            &guard,
                            leaves,
                        );
                    }
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

    /// Preserve structural partitions while transforming semantic payloads. The
    /// shape supplies the capability class; summary sources need not duplicate it.
    pub fn map_payloads<Q: IndexPayload<'db>>(
        &self,
        value: &ValueId<'db, P>,
        destination: &mut ValueInterner<'db, Q>,
        mut map: impl FnMut(
            CapabilitySemantics<'db>,
            &StructuralPath<IndexExpr<'db>>,
            &Guarded<'db, P>,
            &Guard<'db>,
        ) -> Vec<Guarded<'db, Q>>,
    ) -> ValueId<'db, Q> {
        Self::map_node(
            value,
            &StructuralPath::default(),
            &Guard::always(value.scope()),
            destination,
            &mut map,
        )
    }

    fn map_node<Q: IndexPayload<'db>>(
        value: &ValueId<'db, P>,
        path: &StructuralPath<IndexExpr<'db>>,
        domain: &Guard<'db>,
        destination: &mut ValueInterner<'db, Q>,
        map: &mut impl FnMut(
            CapabilitySemantics<'db>,
            &StructuralPath<IndexExpr<'db>>,
            &Guarded<'db, P>,
            &Guard<'db>,
        ) -> Vec<Guarded<'db, Q>>,
    ) -> ValueId<'db, Q> {
        let direct = value
            .shape()
            .direct(destination.db)
            .map_or_else(Vec::new, |semantics| {
                value
                    .0
                    .direct
                    .iter()
                    .flat_map(|entry| {
                        entry
                            .guard
                            .and(&domain.in_scope(entry.guard.scope()))
                            .map(|domain| map(semantics, path, entry, &domain))
                            .unwrap_or_default()
                    })
                    .collect()
            });
        let children = match &value.0.children {
            ValueChildren::None => ValueChildren::None,
            ValueChildren::Product(fields) => ValueChildren::Product(
                fields
                    .iter()
                    .map(|(field, child)| {
                        (
                            *field,
                            Self::map_node(
                                child,
                                &path.appended(Projection::Field(*field)),
                                domain,
                                destination,
                                map,
                            ),
                        )
                    })
                    .collect(),
            ),
            ValueChildren::Sum(variants) => ValueChildren::Sum(
                variants
                    .iter()
                    .map(|(variant, child)| {
                        let ValueChildren::Product(fields) = &child.0.children else {
                            unreachable!("enum variant shape")
                        };
                        let fields = fields
                            .iter()
                            .map(|(field, child)| {
                                (
                                    *field,
                                    Self::map_node(
                                        child,
                                        &path.appended(Projection::VariantField {
                                            variant: *variant,
                                            field: *field,
                                        }),
                                        domain,
                                        destination,
                                        map,
                                    ),
                                )
                            })
                            .collect();
                        (
                            *variant,
                            destination.intern(StructuredValue {
                                shape: child.shape(),
                                scope: child.scope().clone(),
                                direct: Vec::new(),
                                children: ValueChildren::Product(fields),
                            }),
                        )
                    })
                    .filter(|(_, child)| !child.is_empty())
                    .collect(),
            ),
            ValueChildren::Array { default, exact } => {
                let ShapeChildren::Array { len, .. } = value.shape().children(destination.db)
                else {
                    unreachable!()
                };
                let (_, binder) = value.scope().bind(IndexNamespace::Value);
                let mut default_domain = domain
                    .in_scope(default.scope())
                    .with_bound(binder, len.index());
                for key in exact.keys() {
                    default_domain = default_domain
                        .and_then(|guard| guard.with_disequality(binder, IndexExpr::Const(*key)));
                }
                let default = if let Some(domain) = default_domain {
                    Self::map_node(
                        default,
                        &path.appended(Projection::Index(binder)),
                        &domain,
                        destination,
                        map,
                    )
                } else {
                    destination.empty(default.shape(), default.scope())
                };
                let exact = exact
                    .iter()
                    .map(|(key, child)| {
                        let child = if let Some(domain) =
                            domain.with_bound(IndexExpr::Const(*key), len.index())
                        {
                            Self::map_node(
                                child,
                                &path.appended(Projection::Index(IndexExpr::Const(*key))),
                                &domain,
                                destination,
                                map,
                            )
                        } else {
                            destination.empty(child.shape(), child.scope())
                        };
                        (*key, child)
                    })
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
                    Guard::always(entry.guard.scope())
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
        for entry in &mut node.direct {
            let subst = entry.guard.scope().canonical_existentials(
                &node.scope,
                entry
                    .guard
                    .indices()
                    .into_iter()
                    .chain(entry.payload.indices()),
            );
            entry.guard = entry
                .guard
                .substitute(&subst)
                .expect("clause alpha normalization");
            entry.payload = entry.payload.substitute(self.db, &subst);
            assert!(
                node.shape
                    .direct(self.db)
                    .is_some_and(|semantics| entry.payload.accepts_class(semantics.class)),
                "payload capability class mismatch"
            );
            for index in entry.payload.indices() {
                entry
                    .guard
                    .scope()
                    .validate(index)
                    .expect("free payload binder");
            }
        }
        let mut canonical = BTreeMap::<(BinderScope, P), Guard<'db>>::new();
        for entry in node.direct {
            canonical
                .entry((entry.guard.scope().clone(), entry.payload))
                .and_modify(|guard| *guard = guard.or(&entry.guard))
                .or_insert(entry.guard);
        }
        node.direct = canonical
            .into_iter()
            .map(|((_, payload), guard)| Guarded { guard, payload })
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
