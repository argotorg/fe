use std::collections::BTreeSet;

use super::{
    guard::{ChoiceKey, Guard, ValueOccurrence},
    index::{BinderScope, IndexError, IndexExpr, IndexNamespace, IndexSubst},
    path::{Projection, StructuralPath},
    semantics::{CapabilityClass, CapabilitySemantics, StorageClass},
    shape::{CapabilityShape, ShapeChildren, ShapeId, capability_shape},
    value::{Guarded, IndexPayload, ValueId, ValueInterner, ValueLimits},
};
use crate::{
    analysis::{
        semantic::{
            FieldIndex, VariantIndex, get_or_build_semantic_instance,
            identity_semantic_instance_key,
            normalized::{NValueDefinition, NValueId, normalize_semantic_body},
        },
        ty::{
            provider::ProviderTransport,
            ty_check::BodyOwner,
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::ItemKind,
    test_db::HirAnalysisTestDb,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Payload {
    tag: u8,
    indices: Vec<IndexExpr>,
}
impl IndexPayload for Payload {
    fn class(&self) -> CapabilityClass {
        CapabilityClass::Borrow(BorrowKind::Mut)
    }
    fn indices(&self) -> impl Iterator<Item = IndexExpr> {
        self.indices.iter().copied()
    }
    fn substitute(&self, subst: &IndexSubst) -> Self {
        Self {
            tag: self.tag,
            indices: self
                .indices
                .iter()
                .map(|index| subst.apply(*index))
                .collect(),
        }
    }
}

fn runtime(index: u32) -> IndexExpr {
    IndexExpr::Runtime(NValueId::from_u32(index))
}
fn scope() -> BinderScope {
    BinderScope::default()
}
fn path(index: IndexExpr) -> StructuralPath<IndexExpr> {
    StructuralPath::new([Projection::Index(index)])
}

fn leaf_shape(db: &HirAnalysisTestDb) -> ShapeId<'_> {
    let ty = TyId::u256(db);
    ShapeId::new(
        db,
        CapabilityShape {
            direct: Some(CapabilitySemantics {
                class: CapabilityClass::Borrow(BorrowKind::Mut),
                target_ty: ty,
                representation_ty: TyId::borrow_mut_of(db, ty),
                transport: ProviderTransport::ByValue,
                storage: StorageClass::Borrowed,
            }),
            children: ShapeChildren::None,
        },
    )
}

fn array_shape<'db>(db: &'db HirAnalysisTestDb, element: ShapeId<'db>, len: usize) -> ShapeId<'db> {
    ShapeId::new(
        db,
        CapabilityShape {
            direct: None,
            children: if len == 0 {
                ShapeChildren::None
            } else {
                ShapeChildren::Array { len, element }
            },
        },
    )
}

fn leaf<'db>(
    values: &mut ValueInterner<'db, Payload>,
    shape: ShapeId<'db>,
    scope: &BinderScope,
    tag: u8,
    indices: Vec<IndexExpr>,
) -> ValueId<'db, Payload> {
    let empty = values.empty(shape, scope);
    values.with_direct(
        &empty,
        vec![Guarded {
            guard: Guard::always(scope),
            payload: Payload { tag, indices },
        }],
    )
}

#[test]
fn lexical_binders_are_canonical_scoped_and_namespaced() {
    let (left, value) = scope().bind(IndexNamespace::Value);
    let (right, same) = scope().bind(IndexNamespace::Value);
    assert_eq!(left, right);
    assert_eq!(value, same);
    assert_eq!(scope().validate(value), Err(IndexError::FreeBinder(value)));
    let (both, loan) = left.bind(IndexNamespace::Loan);
    assert_ne!(value, loan);
    assert!(both.validate(value).is_ok());
    assert!(both.validate(loan).is_ok());
    assert!(IndexSubst::new(&left, &scope(), []).is_err());
    assert!(IndexSubst::new(&left, &scope(), [(value, IndexExpr::Const(0))]).is_ok());
    assert!(IndexSubst::new(&scope(), &scope(), [(IndexExpr::Const(0), runtime(0))]).is_err());
}

#[test]
fn substitution_is_simultaneous_and_composes_without_capture() {
    let first = IndexSubst::new(
        &scope(),
        &scope(),
        [(runtime(0), runtime(1)), (runtime(1), runtime(0))],
    )
    .unwrap();
    assert_eq!(first.apply(runtime(0)), runtime(1));
    assert_eq!(first.apply(runtime(1)), runtime(0));
    let second = IndexSubst::new(&scope(), &scope(), [(runtime(1), IndexExpr::Const(3))]).unwrap();
    let composed = first.then(&second).unwrap();
    let guard = Guard::always(&scope())
        .with_disequality(runtime(0), runtime(1))
        .unwrap();
    assert_eq!(
        guard.substitute(&first).and_then(|g| g.substitute(&second)),
        guard.substitute(&composed)
    );
    let (nested, binder) = scope().bind(IndexNamespace::Value);
    let remove = IndexSubst::new(&nested, &scope(), [(binder, runtime(0))]).unwrap();
    let lifted = remove.under_binder(IndexNamespace::Value);
    let (_, inner) = nested.bind(IndexNamespace::Value);
    assert_eq!(lifted.apply(inner), binder);
    assert_eq!(lifted.apply(binder), runtime(0));
}

#[test]
fn variant_collisions_are_checked_after_substitution_and_equality() {
    let choice = |index| ChoiceKey::new(ValueOccurrence::Argument(0), path(index));
    let first = Guard::always(&scope())
        .with_variant(choice(runtime(0)), VariantIndex(0))
        .unwrap();
    let different = first
        .with_variant(choice(runtime(1)), VariantIndex(1))
        .unwrap();
    let same = first
        .with_variant(choice(runtime(1)), VariantIndex(0))
        .unwrap();
    let subst = IndexSubst::new(
        &scope(),
        &scope(),
        [
            (runtime(0), IndexExpr::Const(0)),
            (runtime(1), IndexExpr::Const(0)),
        ],
    )
    .unwrap();
    assert!(different.substitute(&subst).is_none());
    assert_eq!(same.substitute(&subst), first.substitute(&subst));
    assert!(different.with_equality(runtime(0), runtime(1)).is_none());
    let independent = Guard::always(&scope())
        .with_variant(
            ChoiceKey::new(ValueOccurrence::Argument(1), path(runtime(0))),
            VariantIndex(1),
        )
        .unwrap();
    assert!(first.and(&independent).is_some());
}

#[test]
fn guard_conjunction_obeys_lattice_laws_and_concrete_models() {
    let mut guards = vec![Guard::always(&scope())];
    for left in [
        runtime(0),
        runtime(1),
        IndexExpr::Const(0),
        IndexExpr::Const(1),
    ] {
        for right in [
            runtime(0),
            runtime(1),
            IndexExpr::Const(0),
            IndexExpr::Const(1),
        ] {
            guards.extend(Guard::always(&scope()).with_equality(left, right));
            guards.extend(Guard::always(&scope()).with_disequality(left, right));
        }
        guards.extend(Guard::always(&scope()).with_bound(left, 2));
    }
    guards.sort();
    guards.dedup();
    for first in &guards {
        assert_eq!(first.and(first).as_ref(), Some(first));
        for second in &guards {
            assert_eq!(first.and(second), second.and(first));
            for third in &guards {
                assert_eq!(
                    first.and(second).and_then(|guard| guard.and(third)),
                    second.and(third).and_then(|guard| first.and(&guard))
                );
            }
            for left in [0, 1, 2] {
                for right in [0, 1, 2] {
                    let subst = IndexSubst::new(
                        &scope(),
                        &scope(),
                        [
                            (runtime(0), IndexExpr::Const(left)),
                            (runtime(1), IndexExpr::Const(right)),
                        ],
                    )
                    .unwrap();
                    let a = first.substitute(&subst).is_some();
                    let b = second.substitute(&subst).is_some();
                    assert_eq!(
                        first
                            .and(second)
                            .and_then(|g| g.substitute(&subst))
                            .is_some(),
                        a && b
                    );
                    if first.implies(second) {
                        assert!(!a || b);
                    }
                }
            }
        }
    }
}

#[test]
fn independently_built_and_nested_arrays_are_alpha_canonical() {
    let db = HirAnalysisTestDb::default();
    let leaf_shape = leaf_shape(&db);
    let inner_shape = array_shape(&db, leaf_shape, 3);
    let outer_shape = array_shape(&db, inner_shape, 1_000_000);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let mut build = |tag| {
        values.array(outer_shape, &scope(), |values, outer, i| {
            values.array(inner_shape, outer, |values, inner, j| {
                leaf(values, leaf_shape, inner, tag, vec![i, j])
            })
        })
    };
    let left = build(1);
    let right = build(2);
    let same = build(1);
    assert_eq!(left, same);
    assert_eq!(values.join(&left, &right), values.join(&right, &left));
    let selected = values.project(
        &left,
        &StructuralPath::new([
            Projection::Index(IndexExpr::Const(9)),
            Projection::Index(IndexExpr::Const(2)),
        ]),
        ValueOccurrence::Argument(0),
    );
    let leaves = values.leaves(&selected, ValueOccurrence::Argument(0));
    assert_eq!(
        leaves[0].payload.indices,
        [IndexExpr::Const(9), IndexExpr::Const(2)]
    );
    assert_eq!(values.leaves(&left, ValueOccurrence::Argument(0)).len(), 1);
    assert!(values.metrics().nodes_created < 25);
}

#[test]
fn structural_join_laws_hold_for_independently_constructed_sparse_arrays() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let shape = array_shape(&db, element, 3);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let mut operands = vec![values.empty(shape, &scope())];
    for tag in [1, 2, 3] {
        let item = leaf(&mut values, element, &scope(), tag, vec![]);
        let array = values.array_repeat(shape, &item);
        operands.push(array.clone());
        let changed = leaf(&mut values, element, &scope(), 4, vec![]);
        operands.push(values.replace(&array, &path(IndexExpr::Const(0)), &changed));
        operands.push(values.replace(&array, &path(runtime(0)), &changed));
    }
    for left in &operands {
        assert_eq!(values.join(left, left), *left);
        for right in &operands {
            let joined = values.join(left, right);
            assert_eq!(joined, values.join(right, left));
            for third in &operands {
                let left_join = values.join(&joined, third);
                let right_join = values.join(right, third);
                assert_eq!(left_join, values.join(left, &right_join));
            }
            for index in [
                IndexExpr::Const(0),
                IndexExpr::Const(1),
                runtime(0),
                runtime(1),
            ] {
                let l = values.project(left, &path(index), ValueOccurrence::Argument(0));
                let r = values.project(right, &path(index), ValueOccurrence::Argument(0));
                let projected_join =
                    values.project(&joined, &path(index), ValueOccurrence::Argument(0));
                assert_eq!(projected_join, values.join(&l, &r));
            }
        }
    }
}

#[test]
fn exact_updates_preserve_product_siblings_and_sparse_remainders() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let product = ShapeId::new(
        &db,
        CapabilityShape {
            direct: None,
            children: ShapeChildren::Product(
                [(FieldIndex(0), element), (FieldIndex(1), element)].into(),
            ),
        },
    );
    let array = array_shape(&db, product, 1_000_000);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let left = leaf(&mut values, element, &scope(), 1, vec![]);
    let right = leaf(&mut values, element, &scope(), 2, vec![]);
    let pair = values.product(
        product,
        &scope(),
        [
            (FieldIndex(0), left.clone()),
            (FieldIndex(1), right.clone()),
        ],
    );
    let initial = values.array_repeat(array, &pair);
    let replaced = StructuralPath::new([
        Projection::Index(IndexExpr::Const(4)),
        Projection::Field(FieldIndex(0)),
    ]);
    let updated = values.replace(&initial, &replaced, &right);
    assert_eq!(
        values.project(&updated, &replaced, ValueOccurrence::Argument(0)),
        right
    );
    let sibling = StructuralPath::new([
        Projection::Index(IndexExpr::Const(4)),
        Projection::Field(FieldIndex(1)),
    ]);
    assert_eq!(
        values.project(&updated, &sibling, ValueOccurrence::Argument(0)),
        right
    );
    let untouched = StructuralPath::new([
        Projection::Index(IndexExpr::Const(5)),
        Projection::Field(FieldIndex(0)),
    ]);
    assert_eq!(
        values.project(&updated, &untouched, ValueOccurrence::Argument(0)),
        left
    );
    assert_eq!(
        values.replace(&initial, &path(IndexExpr::Const(4)), &pair),
        initial
    );
    assert_eq!(
        values.leaves(&updated, ValueOccurrence::Argument(0)).len(),
        4
    );
}

#[test]
fn enum_exclusivity_is_scoped_to_a_value_occurrence() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let variant = ShapeId::new(
        &db,
        CapabilityShape {
            direct: None,
            children: ShapeChildren::Product([(FieldIndex(0), element)].into()),
        },
    );
    let shape = ShapeId::new(
        &db,
        CapabilityShape {
            direct: None,
            children: ShapeChildren::Sum(
                [(VariantIndex(0), variant), (VariantIndex(1), variant)].into(),
            ),
        },
    );
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let item = leaf(&mut values, element, &scope(), 1, vec![]);
    let fields = values.product(variant, &scope(), [(FieldIndex(0), item)]);
    let sum = values.sum(
        shape,
        &scope(),
        [(VariantIndex(0), fields.clone()), (VariantIndex(1), fields)],
    );
    let one = values.leaves(&sum, ValueOccurrence::Argument(0));
    assert!(one[0].guard.and(&one[1].guard).is_none());
    let two = values.leaves(&sum, ValueOccurrence::Argument(1));
    assert!(one[0].guard.and(&two[1].guard).is_some());
}

#[derive(Clone, Debug)]
enum Action {
    Exact(usize),
    Dynamic(usize),
    Conditional,
    Join,
}

fn evaluate<'db>(
    values: &mut ValueInterner<'db, Payload>,
    value: &ValueId<'db, Payload>,
    valuation: &[usize; 3],
) -> BTreeSet<u8> {
    let subst = IndexSubst::new(
        &scope(),
        &scope(),
        valuation
            .iter()
            .enumerate()
            .map(|(index, value)| (runtime(index as u32), IndexExpr::Const(*value))),
    )
    .unwrap();
    let evaluated = values.substitute(value, &subst);
    values
        .leaves(&evaluated, ValueOccurrence::Argument(0))
        .into_iter()
        .map(|leaf| {
            assert_eq!(leaf.guard, Guard::always(&scope()));
            leaf.payload.tag
        })
        .collect()
}

#[test]
fn sparse_arrays_match_concrete_execution_for_small_lengths_and_selector_valuations() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    for len in [0, 1, 2, 3, 4] {
        let shape = array_shape(&db, element, len);
        let limits = ValueLimits {
            guarded_alternatives: 1,
            guard_indices: 0,
            guard_nodes: 4096,
            exact_members: 0,
            interned_nodes: 256,
        };
        let mut values = ValueInterner::new(&db, limits);
        let initial = leaf(&mut values, element, &scope(), 1, vec![]);
        if len == 0 {
            let empty = values.empty(shape, &scope());
            assert!(
                values
                    .leaves(&empty, ValueOccurrence::Argument(0))
                    .is_empty()
            );
            assert_eq!(values.join(&empty, &empty), empty);
            continue;
        }
        let initial = values.array_repeat(shape, &initial);
        let valuations: Vec<_> = (0..len)
            .flat_map(|left| (0..len).flat_map(move |right| [0, 1].map(|cond| [left, right, cond])))
            .collect();
        let concrete = vec![vec![BTreeSet::from([1]); len]; valuations.len()];
        let mut states = vec![(initial, concrete)];
        let actions: Vec<_> = (0..len)
            .map(Action::Exact)
            .chain([
                Action::Dynamic(0),
                Action::Dynamic(1),
                Action::Conditional,
                Action::Join,
            ])
            .collect();
        for tag in [2, 3] {
            let mut next_states = Vec::new();
            for (old, model) in &states {
                for action in &actions {
                    let replacement = leaf(&mut values, element, &scope(), tag, vec![]);
                    let next = match action {
                        Action::Exact(key) => {
                            values.replace(old, &path(IndexExpr::Const(*key)), &replacement)
                        }
                        Action::Dynamic(index) => {
                            values.replace(old, &path(runtime(*index as u32)), &replacement)
                        }
                        Action::Conditional => {
                            let update = values.replace(old, &path(runtime(0)), &replacement);
                            let yes = Guard::always(&scope())
                                .with_equality(runtime(2), IndexExpr::Const(1))
                                .unwrap();
                            let no = Guard::always(&scope())
                                .with_disequality(runtime(2), IndexExpr::Const(1))
                                .unwrap();
                            let update = values.with_guard(&update, &yes);
                            let retained = values.with_guard(old, &no);
                            values.join(&update, &retained)
                        }
                        Action::Join => {
                            let alternative = values.array_repeat(shape, &replacement);
                            values.join(old, &alternative)
                        }
                    };
                    let mut expected = model.clone();
                    let widened = values.widen(&next);
                    for (valuation, concrete) in valuations.iter().zip(&mut expected) {
                        match action {
                            Action::Exact(key) => concrete[*key] = BTreeSet::from([tag]),
                            Action::Dynamic(index) => {
                                concrete[valuation[*index]] = BTreeSet::from([tag])
                            }
                            Action::Conditional if valuation[2] == 1 => {
                                concrete[valuation[0]] = BTreeSet::from([tag])
                            }
                            Action::Conditional => {}
                            Action::Join => {
                                for member in concrete.iter_mut() {
                                    member.insert(tag);
                                }
                            }
                        }
                        for (index, member) in concrete.iter().enumerate() {
                            let actual = values.project(
                                &next,
                                &path(IndexExpr::Const(index)),
                                ValueOccurrence::Argument(0),
                            );
                            assert_eq!(
                                evaluate(&mut values, &actual, valuation),
                                *member,
                                "len={len} action={action:?} selectors={valuation:?}"
                            );
                            let wide = values.project(
                                &widened,
                                &path(IndexExpr::Const(index)),
                                ValueOccurrence::Argument(0),
                            );
                            assert!(
                                member.is_subset(&evaluate(&mut values, &wide, valuation)),
                                "widening removed a concrete possibility"
                            );
                        }
                        for (selector, index) in valuation.iter().take(2).enumerate() {
                            let actual = values.project(
                                &next,
                                &path(runtime(selector as u32)),
                                ValueOccurrence::Argument(0),
                            );
                            assert_eq!(evaluate(&mut values, &actual, valuation), concrete[*index]);
                        }
                    }
                    next_states.push((next, expected));
                }
            }
            states = next_states;
        }
        assert!(values.metrics().widened_nodes > 0);
    }
}

#[test]
fn payload_mapping_preserves_structure_and_uses_checked_substitution() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let array = array_shape(&db, element, 10);
    let mut source = ValueInterner::new(&db, ValueLimits::default());
    let value = source.array(array, &scope(), |values, scope, binder| {
        leaf(values, element, scope, 1, vec![binder])
    });
    let mut destination = ValueInterner::new(&db, ValueLimits::default());
    let mapped = source.map_payloads(&value, &mut destination, |entry| {
        let mut entry = entry.clone();
        entry.payload.tag = 2;
        vec![entry]
    });
    let projected = destination.project(
        &mapped,
        &path(IndexExpr::Const(5)),
        ValueOccurrence::Summary,
    );
    let leaves = destination.leaves(&projected, ValueOccurrence::Summary);
    assert_eq!(
        leaves[0].payload,
        Payload {
            tag: 2,
            indices: vec![IndexExpr::Const(5)]
        }
    );
}

#[test]
fn semantic_shapes_separate_borrow_targets_from_view_contents() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "capability_shapes.fe".into(),
        r#"
struct Pair { left: mut u256, right: mut u256 }
fn inspect(pair: mut Pair, items: [mut u256; 1000000], empty: [mut u256; 0]) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func) => Some(*func),
            _ => None,
        })
        .unwrap();
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let artifacts = normalize_semantic_body(&db, instance).unwrap();
    let entries: Vec<_> = artifacts
        .body
        .values
        .iter()
        .filter(|value| matches!(value.definition, NValueDefinition::EntryParam { .. }))
        .collect();
    let shapes: Vec<_> = entries
        .iter()
        .map(|value| {
            capability_shape(&db, func.scope(), instance.assumptions(&db), value.ty).unwrap()
        })
        .collect();
    assert!(matches!(shapes[0].data(&db).children, ShapeChildren::None));
    assert_eq!(
        shapes[0].direct(&db).unwrap().class,
        CapabilityClass::Borrow(BorrowKind::Mut)
    );
    let target = shapes[0].direct(&db).unwrap().target_ty;
    let referent = capability_shape(&db, func.scope(), instance.assumptions(&db), target).unwrap();
    let ShapeChildren::Product(fields) = &referent.data(&db).children else {
        panic!("borrow referent retains structural fields")
    };
    assert_eq!(fields.len(), 2);
    assert!(
        fields
            .iter()
            .all(|(_, shape)| shape.contains_capability(&db))
    );
    assert!(matches!(
        shapes[1].data(&db).children,
        ShapeChildren::Array { len: 1_000_000, .. }
    ));
    assert!(matches!(shapes[2].data(&db).children, ShapeChildren::None));
    assert_eq!(
        shapes[1],
        capability_shape(&db, func.scope(), instance.assumptions(&db), entries[1].ty).unwrap()
    );
}

#[test]
fn complementary_guard_partitions_have_one_canonical_union() {
    let always = Guard::always(&scope());
    let bounded = always.with_bound(runtime(0), 3).unwrap();
    let zero = always
        .with_equality(runtime(0), IndexExpr::Const(0))
        .unwrap();
    let remainder = bounded
        .with_disequality(runtime(0), IndexExpr::Const(0))
        .unwrap();
    assert_eq!(zero.or(&remainder), bounded);
    assert_eq!(remainder.or(&zero), bounded);

    let db = HirAnalysisTestDb::default();
    let shape = leaf_shape(&db);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let empty = values.empty(shape, &scope());
    let payload = Payload {
        tag: 1,
        indices: vec![],
    };
    let split = values.with_direct(
        &empty,
        vec![
            Guarded {
                guard: zero,
                payload: payload.clone(),
            },
            Guarded {
                guard: remainder,
                payload: payload.clone(),
            },
        ],
    );
    let combined = values.with_direct(
        &empty,
        vec![Guarded {
            guard: bounded,
            payload,
        }],
    );
    assert_eq!(split, combined);
}

#[test]
fn guard_unions_obey_boolean_laws_and_substitution() {
    let always = Guard::always(&scope());
    let a = always
        .with_equality(runtime(0), IndexExpr::Const(0))
        .unwrap();
    let not_a = always
        .with_disequality(runtime(0), IndexExpr::Const(0))
        .unwrap();
    let b = always
        .with_equality(runtime(1), IndexExpr::Const(1))
        .unwrap();
    let c = always
        .with_equality(runtime(2), IndexExpr::Const(2))
        .unwrap();
    let bounds = always.with_bound(runtime(0), 3).unwrap();
    let symbolic = always.with_equality(runtime(0), runtime(1)).unwrap();
    assert_eq!(a.or(&not_a), always);
    // Consensus is a union property, not just a pairwise implication or array rule.
    let branches = a.and(&b).unwrap().or(&not_a.and(&c).unwrap());
    assert_eq!(branches.or(&b.and(&c).unwrap()), branches);
    let guards = [always, a, not_a, b, c, bounds, symbolic, branches];
    let subst = IndexSubst::new(
        &scope(),
        &scope(),
        [
            (runtime(0), runtime(2)),
            (runtime(1), runtime(2)),
            (runtime(2), runtime(0)),
        ],
    )
    .unwrap();
    for left in &guards {
        assert_eq!(left.or(left), *left);
        for right in &guards {
            let union = left.or(right);
            assert_eq!(union, right.or(left));
            let substituted = match (left.substitute(&subst), right.substitute(&subst)) {
                (Some(left), Some(right)) => Some(left.or(&right)),
                (left, right) => left.or(right),
            };
            assert_eq!(union.substitute(&subst), substituted);
            for third in &guards {
                assert_eq!(union.or(third), left.or(&right.or(third)));
                let distributed = match (left.and(third), right.and(third)) {
                    (Some(left), Some(right)) => Some(left.or(&right)),
                    (left, right) => left.or(right),
                };
                assert_eq!(union.and(third), distributed);
            }
        }
    }
}

#[test]
fn enum_union_substitution_reorders_and_identifies_choice_decisions() {
    let choice = |index| ChoiceKey::new(ValueOccurrence::Argument(0), path(index));
    let first = Guard::always(&scope())
        .with_variant(choice(runtime(0)), VariantIndex(0))
        .unwrap();
    let second = Guard::always(&scope())
        .with_variant(choice(runtime(1)), VariantIndex(1))
        .unwrap();
    let swapped = IndexSubst::new(
        &scope(),
        &scope(),
        [(runtime(0), runtime(1)), (runtime(1), runtime(0))],
    )
    .unwrap();
    assert_eq!(
        first.or(&second).substitute(&swapped),
        Some(
            first
                .substitute(&swapped)
                .unwrap()
                .or(&second.substitute(&swapped).unwrap())
        )
    );
    let identified = IndexSubst::new(&scope(), &scope(), [(runtime(1), runtime(0))]).unwrap();
    assert!(
        first
            .and(&second)
            .unwrap()
            .substitute(&identified)
            .is_none()
    );
    assert!(first.or(&second).substitute(&identified).is_some());
}

#[test]
fn indexed_enum_guards_recombine_across_equality_partitions() {
    let choice = |index| ChoiceKey::new(ValueOccurrence::Argument(0), path(index));
    let whole = Guard::always(&scope())
        .with_variant(choice(runtime(1)), VariantIndex(0))
        .unwrap();
    let equal = whole.with_equality(runtime(0), runtime(1)).unwrap();
    let distinct = whole.with_disequality(runtime(0), runtime(1)).unwrap();
    assert_eq!(equal.or(&distinct), whole);
    assert_eq!(distinct.or(&equal), whole);
    let selected_by_other_index = Guard::always(&scope())
        .with_equality(runtime(0), runtime(1))
        .unwrap()
        .with_variant(choice(runtime(0)), VariantIndex(0))
        .unwrap();
    assert_eq!(equal, selected_by_other_index);
}

#[test]
fn indexed_enum_guard_laws_are_independent_of_construction_order() {
    let choice = |index| ChoiceKey::new(ValueOccurrence::Argument(0), path(index));
    let always = Guard::always(&scope());
    let first = always
        .with_variant(choice(runtime(0)), VariantIndex(0))
        .unwrap();
    let second = always
        .with_variant(choice(runtime(1)), VariantIndex(0))
        .unwrap();
    let other = always
        .with_variant(choice(runtime(1)), VariantIndex(1))
        .unwrap();
    let equal = always.with_equality(runtime(0), runtime(1)).unwrap();
    let distinct = always.with_disequality(runtime(0), runtime(1)).unwrap();
    let guards = [first, second, other, equal, distinct];
    for (left_index, left) in guards.iter().enumerate() {
        assert_eq!(left.or(left), *left);
        assert_eq!(left.and(left).as_ref(), Some(left));
        for (right_index, right) in guards.iter().enumerate() {
            let union = left.or(right);
            assert_eq!(union, right.or(left));
            assert_eq!(left.and(right), right.and(left));
            assert_eq!(left.and(&union).as_ref(), Some(left));
            for (third_index, third) in guards.iter().enumerate() {
                assert!(
                    union.or(third) == left.or(&right.or(third)),
                    "union associativity: {left_index}, {right_index}, {third_index}"
                );
                assert!(
                    left.and(right).and_then(|guard| guard.and(third))
                        == right.and(third).and_then(|guard| left.and(&guard)),
                    "conjunction associativity: {left_index}, {right_index}, {third_index}"
                );
                let distributed = match (left.and(third), right.and(third)) {
                    (Some(left), Some(right)) => Some(left.or(&right)),
                    (left, right) => left.or(right),
                };
                assert!(
                    union.and(third) == distributed,
                    "distribution: {left_index}, {right_index}, {third_index}"
                );
            }
        }
    }
}
