use crate::analysis::semantic::capability::test_roots;
use std::{
    collections::{BTreeMap, BTreeSet},
    iter::empty,
};

use super::{
    birth::AllocationBirth,
    external::{ExternalOrigin, ExternalSource, ReferentContract},
    guard::{ChoiceKey, Guard, ValueOccurrence},
    handle::{
        AddressOccurrence, HandleAddressSpace, OpaqueHandleContract, OpaqueHandleRef,
        OpaqueWriteSite,
    },
    index::{BinderScope, IndexError, IndexExpr, IndexNamespace, IndexSubst},
    loan::{CapabilityRef, LoanDef, LoanId, LoanRef},
    opaque::OpaqueWrite,
    path::{Projection, RegionPath, StructuralPath},
    region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace},
    repack::ReferentRepackId,
    semantics::{CapabilityClass, CapabilitySemantics, StorageClass, TransportClass},
    shape::{ArrayLength, CapabilityShape, ShapeChildren, ShapeId, capability_shape},
    source::{InputSource, SourceExpr},
    state::BorrowState,
    value::{Guarded, IndexPayload, ValueId, ValueInterner, ValueLimits},
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BorrowActivation, FieldIndex, SemOrigin, VariantIndex, get_or_build_semantic_instance,
            identity_semantic_instance_key,
            normalized::{NRootId, NValueDefinition, NValueId, normalize_semantic_body},
        },
        ty::{
            provider::ProviderAddressSpace,
            ty_check::BodyOwner,
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::ItemKind,
    test_db::{HirAnalysisTestDb, find_func},
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Payload<'db> {
    tag: u8,
    indices: Vec<IndexExpr<'db>>,
}
impl<'db> IndexPayload<'db> for Payload<'db> {
    fn accepts_class(&self, class: CapabilityClass) -> bool {
        class == CapabilityClass::Borrow(BorrowKind::Mut)
    }
    fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> {
        self.indices.iter().copied()
    }
    fn substitute(&self, _: &'db dyn HirAnalysisDb, subst: &IndexSubst<'db>) -> Self {
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

fn runtime<'db>(index: u32) -> IndexExpr<'db> {
    IndexExpr::Runtime(NValueId::from_u32(index))
}
fn scope() -> BinderScope {
    BinderScope::default()
}
fn path<'db>(index: IndexExpr<'db>) -> StructuralPath<IndexExpr<'db>> {
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
                transport: TransportClass::MemoryBorrow,
                storage: StorageClass::Borrowed,
            }),
            children: ShapeChildren::None,
        },
    )
}

#[test]
fn physical_offsets_do_not_prove_disjoint_wide_accesses() {
    let db = HirAnalysisTestDb::default();
    let base = ExternalSource::input(
        InputSource::slot(0, StructuralPath::default()),
        ReferentContract::new(
            &db,
            TyId::u8(&db),
            HandleAddressSpace::Known(ProviderAddressSpace::Memory),
        ),
        false,
    );
    let region = |offset| {
        RegionSet::singleton(
            &scope(),
            RegionRoot::External(ExternalSource::memory(
                &db,
                SourceExpr {
                    invalidated: false,
                    source: base.clone(),
                    path: RegionPath::default(),
                    views: Default::default(),
                },
                TyId::u256(&db),
                Some((TyId::u8(&db), IndexExpr::Const(offset))),
            )),
            RegionPath::default(),
        )
    };
    assert_ne!(region(1).overlap(&db, &region(2)), OverlapResult::Disjoint);
}

fn array_shape<'db>(db: &'db HirAnalysisTestDb, element: ShapeId<'db>, len: usize) -> ShapeId<'db> {
    ShapeId::new(
        db,
        CapabilityShape {
            direct: None,
            children: if len == 0 {
                ShapeChildren::EmptyArray
            } else {
                ShapeChildren::Array {
                    len: ArrayLength::Known(len),
                    element,
                }
            },
        },
    )
}

fn leaf<'db>(
    values: &mut ValueInterner<'db, Payload<'db>>,
    shape: ShapeId<'db>,
    scope: &BinderScope,
    tag: u8,
    indices: Vec<IndexExpr<'db>>,
) -> ValueId<'db, Payload<'db>> {
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
    let selected = values
        .project(
            &left,
            &StructuralPath::new([
                Projection::Index(IndexExpr::Const(9)),
                Projection::Index(IndexExpr::Const(2)),
            ]),
            ValueOccurrence::Argument(0),
        )
        .expect("in-bounds projection");
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
                let l = values
                    .project(left, &path(index), ValueOccurrence::Argument(0))
                    .expect("in-bounds projection");
                let r = values
                    .project(right, &path(index), ValueOccurrence::Argument(0))
                    .expect("in-bounds projection");
                let projected_join = values
                    .project(&joined, &path(index), ValueOccurrence::Argument(0))
                    .expect("in-bounds projection");
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
        values
            .project(&updated, &replaced, ValueOccurrence::Argument(0))
            .expect("in-bounds projection"),
        right
    );
    let sibling = StructuralPath::new([
        Projection::Index(IndexExpr::Const(4)),
        Projection::Field(FieldIndex(1)),
    ]);
    assert_eq!(
        values
            .project(&updated, &sibling, ValueOccurrence::Argument(0))
            .expect("in-bounds projection"),
        right
    );
    let untouched = StructuralPath::new([
        Projection::Index(IndexExpr::Const(5)),
        Projection::Field(FieldIndex(0)),
    ]);
    assert_eq!(
        values
            .project(&updated, &untouched, ValueOccurrence::Argument(0))
            .expect("in-bounds projection"),
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
    values: &mut ValueInterner<'db, Payload<'db>>,
    value: &ValueId<'db, Payload<'db>>,
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
                            let actual = values
                                .project(
                                    &next,
                                    &path(IndexExpr::Const(index)),
                                    ValueOccurrence::Argument(0),
                                )
                                .expect("in-bounds projection");
                            assert_eq!(
                                evaluate(&mut values, &actual, valuation),
                                *member,
                                "len={len} action={action:?} selectors={valuation:?}"
                            );
                            let wide = values
                                .project(
                                    &widened,
                                    &path(IndexExpr::Const(index)),
                                    ValueOccurrence::Argument(0),
                                )
                                .expect("in-bounds projection");
                            assert!(
                                member.is_subset(&evaluate(&mut values, &wide, valuation)),
                                "widening removed a concrete possibility"
                            );
                        }
                        for (selector, index) in valuation.iter().take(2).enumerate() {
                            let actual = values
                                .project(
                                    &next,
                                    &path(runtime(selector as u32)),
                                    ValueOccurrence::Argument(0),
                                )
                                .expect("in-bounds projection");
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
    let mapped = source.map_payloads(&value, &mut destination, |_, _, entry, _| {
        let mut entry = entry.clone();
        entry.payload.tag = 2;
        vec![entry]
    });
    let projected = destination
        .project(
            &mapped,
            &path(IndexExpr::Const(5)),
            ValueOccurrence::Summary,
        )
        .expect("in-bounds projection");
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
        ShapeChildren::Array {
            len: ArrayLength::Known(1_000_000),
            ..
        }
    ));
    assert!(matches!(
        shapes[2].data(&db).children,
        ShapeChildren::EmptyArray
    ));
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

#[test]
fn admitted_generic_array_parameters_have_capability_shapes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "generic_capability_shape.fe".into(),
        "fn inspect<const N: usize>(_ values: own [mut u256; N]) {}",
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
    let artifacts =
        normalize_semantic_body(&db, instance).expect("generic array body must be admitted");
    let parameter = artifacts
        .body
        .values
        .iter()
        .find(|value| matches!(value.definition, NValueDefinition::EntryParam { param: 0 }))
        .expect("generic array parameter");
    assert!(
        parameter.ty.is_array(&db),
        "expected the actual aggregate value shape"
    );
    let shape = capability_shape(&db, func.scope(), instance.assumptions(&db), parameter.ty)
        .expect("an admitted generic array must retain its nested mutable capability family");
    assert!(shape.contains_capability(&db));
}

fn generic_array_shapes(db: &mut HirAnalysisTestDb) -> (&HirAnalysisTestDb, Vec<ShapeId<'_>>) {
    let file = db.new_stand_alone(
        "generic_array_algebra.fe".into(),
        "fn inspect<const N: usize, const M: usize>(_ first: own [mut u256; N], _ second: own [mut u256; M], _ nested: own [[mut u256; M]; N], borrowed: mut [mut u256; N]) {}",
    );
    let (top_mod, _) = db.top_mod(file);
    let func = top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func) => Some(*func),
            _ => None,
        })
        .unwrap();
    let instance = get_or_build_semantic_instance(
        db,
        identity_semantic_instance_key(db, BodyOwner::Func(func)),
    );
    let artifacts = normalize_semantic_body(db, instance).expect("generic body admission");
    let shapes = artifacts
        .body
        .values
        .iter()
        .filter(|value| matches!(value.definition, NValueDefinition::EntryParam { .. }))
        .map(|value| {
            capability_shape(db, func.scope(), instance.assumptions(db), value.ty).unwrap()
        })
        .collect();
    (db, shapes)
}

#[test]
fn symbolic_bounds_preserve_const_identity_and_specialize_as_unsigned_comparisons() {
    let mut db = HirAnalysisTestDb::default();
    let (db, shapes) = generic_array_shapes(&mut db);
    let lengths: Vec<_> = shapes[..2]
        .iter()
        .map(|shape| {
            let ShapeChildren::Array { len, .. } = shape.children(db) else {
                panic!("generic array")
            };
            len.index()
        })
        .collect();
    let [n, m] = lengths[..] else { unreachable!() };
    assert_ne!(n, m);
    assert!(matches!(n, IndexExpr::TypeConst(_)));
    assert_eq!(
        IndexSubst::new(&scope(), &scope(), [(n, runtime(0))]),
        Err(IndexError::InvalidConstSubstitution)
    );
    let always = Guard::always(&scope());
    let n_bound = always.with_bound(runtime(0), n).unwrap();
    let m_bound = always.with_bound(runtime(0), m).unwrap();
    let ordered = always.with_bound(n, m).unwrap();
    assert!(always.with_bound(n, n).is_none());
    assert!(ordered.with_bound(m, n).is_none());
    assert!(ordered.with_equality(n, m).is_none());
    assert_eq!(n_bound.or(&m_bound), m_bound.or(&n_bound));
    for n_value in [0, 1, 2, 4, usize::MAX] {
        for m_value in [0, 1, 3, usize::MAX] {
            for index in [0, 1, 2, 4, usize::MAX] {
                let subst = IndexSubst::new(
                    &scope(),
                    &scope(),
                    [
                        (n, n_value.into()),
                        (m, m_value.into()),
                        (runtime(0), index.into()),
                    ],
                )
                .unwrap();
                assert_eq!(
                    n_bound.substitute(&subst),
                    (index < n_value).then(|| always.clone())
                );
                assert_eq!(
                    ordered.substitute(&subst),
                    (n_value < m_value).then(|| always.clone())
                );
                assert_eq!(
                    n_bound.or(&m_bound).substitute(&subst),
                    (index < n_value || index < m_value).then(|| always.clone())
                );
                assert_eq!(
                    n_bound.and(&m_bound).and_then(|g| g.substitute(&subst)),
                    (index < n_value && index < m_value).then(|| always.clone())
                );
            }
        }
    }
}

#[test]
fn symbolic_array_updates_and_projections_commute_with_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let (db, shapes) = generic_array_shapes(&mut db);
    let shape = shapes[0];
    let ShapeChildren::Array { len, element } = *shape.children(db) else {
        panic!("array")
    };
    let mut values = ValueInterner::new(db, ValueLimits::default());
    let initial = leaf(&mut values, element, &scope(), 1, vec![]);
    let exact = leaf(&mut values, element, &scope(), 2, vec![]);
    let dynamic = leaf(&mut values, element, &scope(), 3, vec![]);
    let family = values.array_repeat(shape, &initial);
    let family = values.replace(&family, &path(2.into()), &exact);
    let family = values.replace(&family, &path(runtime(0)), &dynamic);
    let selected = values
        .project(&family, &path(runtime(1)), ValueOccurrence::Summary)
        .expect("in-bounds projection");
    let selected_zero = values
        .project(&family, &path(0.into()), ValueOccurrence::Summary)
        .expect("in-bounds projection");
    for length in 0..=4 {
        let subst = IndexSubst::new(&scope(), &scope(), [(len.index(), length.into())]).unwrap();
        let specialized = values.substitute(&family, &subst);
        assert_eq!(specialized.shape(), array_shape(db, element, length));
        if length == 0 {
            assert!(specialized.is_empty());
            assert!(values.substitute(&selected_zero, &subst).is_empty());
        } else {
            let mut concrete = values.array_repeat(specialized.shape(), &initial);
            if length > 2 {
                concrete = values.replace(&concrete, &path(2.into()), &exact);
            }
            concrete = values.replace(&concrete, &path(runtime(0)), &dynamic);
            assert_eq!(specialized, concrete);
        }
        for write in 0..=4 {
            for read in 0..=4 {
                let subst = IndexSubst::new(
                    &scope(),
                    &scope(),
                    [
                        (len.index(), length.into()),
                        (runtime(0), write.into()),
                        (runtime(1), read.into()),
                    ],
                )
                .unwrap();
                let actual = values.substitute(&selected, &subst);
                let expected = if read >= length {
                    BTreeSet::new()
                } else {
                    BTreeSet::from([if read == write {
                        3
                    } else if read == 2 {
                        2
                    } else {
                        1
                    }])
                };
                assert_eq!(
                    evaluate(&mut values, &actual, &[0, 0, 0]),
                    expected,
                    "length={length}, write={write}, read={read}"
                );
            }
        }
    }
}

#[test]
fn nested_generic_families_specialize_shapes_and_capability_types_together() {
    let mut db = HirAnalysisTestDb::default();
    let (db, shapes) = generic_array_shapes(&mut db);
    let ShapeChildren::Array {
        len: outer_len,
        element: inner,
    } = *shapes[2].children(db)
    else {
        panic!("outer array")
    };
    let ShapeChildren::Array {
        len: inner_len,
        element,
    } = *inner.children(db)
    else {
        panic!("inner array")
    };
    let mut values = ValueInterner::new(db, ValueLimits::default());
    let family = values.from_shape(shapes[2], &scope(), |_, path, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: Payload {
                tag: 1,
                indices: path.indices().collect(),
            },
        }]
    });
    assert_eq!(values.leaves(&family, ValueOccurrence::Summary).len(), 1);
    for outer in 0..=3 {
        for inner in 0..=3 {
            let subst = IndexSubst::new(
                &scope(),
                &scope(),
                [
                    (outer_len.index(), outer.into()),
                    (inner_len.index(), inner.into()),
                ],
            )
            .unwrap();
            let specialized = values.substitute(&family, &subst);
            let shape = array_shape(db, array_shape(db, element, inner), outer);
            assert_eq!(specialized.shape(), shape);
            let concrete = values.from_shape(shape, &scope(), |_, path, scope| {
                vec![Guarded {
                    guard: Guard::always(scope),
                    payload: Payload {
                        tag: 1,
                        indices: path.indices().collect(),
                    },
                }]
            });
            assert_eq!(specialized, concrete);
            assert_eq!(specialized.is_empty(), outer == 0 || inner == 0);
            let borrow = shapes[3].substitute(db, &subst).direct(db).unwrap();
            assert_eq!(borrow.target_ty.array_len(db), Some(outer));
            assert_eq!(
                borrow.representation_ty.as_borrow(db).unwrap().1,
                borrow.target_ty
            );
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ViewPayload;

impl<'db> IndexPayload<'db> for ViewPayload {
    fn accepts_class(&self, class: CapabilityClass) -> bool {
        class == CapabilityClass::View
    }
    fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> {
        empty()
    }
    fn substitute(&self, _: &'db dyn HirAnalysisDb, _: &IndexSubst<'db>) -> Self {
        self.clone()
    }
}

#[test]
fn specializing_an_empty_array_keeps_its_direct_view_capability() {
    let mut db = HirAnalysisTestDb::default();
    let (db, shapes) = generic_array_shapes(&mut db);
    let target_ty = shapes[3].direct(db).unwrap().target_ty;
    let shape = ShapeId::new(
        db,
        CapabilityShape {
            direct: Some(CapabilitySemantics {
                class: CapabilityClass::View,
                target_ty,
                representation_ty: TyId::view_of(db, target_ty),
                transport: TransportClass::ReadOnly,
                storage: StorageClass::Borrowed,
            }),
            children: shapes[0].children(db).clone(),
        },
    );
    let ShapeChildren::Array { len, .. } = shape.children(db) else {
        panic!("array")
    };
    let mut values = ValueInterner::new(db, ValueLimits::default());
    let empty = values.empty(shape, &scope());
    let view = values.with_direct(
        &empty,
        vec![Guarded {
            guard: Guard::always(&scope()),
            payload: ViewPayload,
        }],
    );
    let subst = IndexSubst::new(&scope(), &scope(), [(len.index(), 0.into())]).unwrap();
    let specialized = values.substitute(&view, &subst);
    assert!(matches!(
        specialized.shape().children(db),
        ShapeChildren::EmptyArray
    ));
    assert_eq!(
        specialized
            .shape()
            .direct(db)
            .unwrap()
            .target_ty
            .array_len(db),
        Some(0)
    );
    assert_eq!(
        values.leaves(&specialized, ValueOccurrence::Summary).len(),
        1
    );
    assert_eq!(specialized.direct(), view.direct());
}

#[test]
fn structural_summary_mapping_retains_nested_sources_and_exact_array_overrides() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let array = array_shape(&db, element, 1_000_000);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let value = values.array(array, &scope(), |values, scope, binder| {
        leaf(values, element, scope, 0, vec![binder])
    });
    let replacement = leaf(&mut values, element, &scope(), 1, vec![IndexExpr::Const(9)]);
    let value = values.replace(&value, &path(IndexExpr::Const(3)), &replacement);
    let mut summaries = ValueInterner::new(&db, ValueLimits::default());
    let summary = values.map_payloads(&value, &mut summaries, |semantics, slot, entry, _| {
        assert_eq!(semantics.class, CapabilityClass::Borrow(BorrowKind::Mut));
        assert_eq!(slot.as_slice().len(), 1);
        vec![Guarded {
            guard: entry.guard.clone(),
            payload: SourceExpr {
                invalidated: false,
                views: Default::default(),
                source: test_roots::input(
                    &db,
                    InputSource::slot(u32::from(entry.payload.tag), StructuralPath::default())
                        .follow(RegionPath::new([Projection::Field(FieldIndex(0))]))
                        .follow(RegionPath::new([Projection::Index(
                            entry.payload.indices[0],
                        )])),
                ),
                path: RegionPath::new([Projection::Field(FieldIndex(1))]),
            },
        }]
    });
    assert_eq!(summary.shape(), value.shape());
    assert!(summaries.metrics().nodes_created < 20);
    for (member, param, selected) in [(0, 0, 0), (3, 1, 9), (999_999, 0, 999_999)] {
        let projected = summaries
            .project(
                &summary,
                &path(IndexExpr::Const(member)),
                ValueOccurrence::Summary,
            )
            .expect("in-bounds projection");
        let leaves = summaries.leaves(&projected, ValueOccurrence::Summary);
        assert_eq!(leaves.len(), 1);
        assert_eq!(
            leaves[0].payload,
            SourceExpr {
                invalidated: false,
                views: Default::default(),
                source: test_roots::input(
                    &db,
                    InputSource::slot(param, StructuralPath::default())
                        .follow(RegionPath::new([Projection::Field(FieldIndex(0))]))
                        .follow(RegionPath::new([Projection::Index(IndexExpr::Const(
                            selected
                        ))]))
                ),
                path: RegionPath::new([Projection::Field(FieldIndex(1))]),
            }
        );
    }
    let mut restored = ValueInterner::new(&db, ValueLimits::default());
    let roundtrip = summaries.map_payloads(&summary, &mut restored, |semantics, _, entry, _| {
        assert_eq!(semantics.target_ty, TyId::u256(&db));
        let source = &entry.payload.source;
        vec![Guarded {
            guard: entry.guard.clone(),
            payload: Payload {
                tag: u8::try_from(source.param().unwrap()).unwrap(),
                indices: source.indices().collect(),
            },
        }]
    });
    assert_eq!(roundtrip, value);
}

#[test]
fn contextual_payload_mapping_reports_variant_fields_and_nested_array_binders() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let inner = array_shape(&db, element, 3);
    let fields = ShapeId::new(
        &db,
        CapabilityShape {
            direct: None,
            children: ShapeChildren::Product(vec![(FieldIndex(0), inner)].into()),
        },
    );
    let sum = ShapeId::new(
        &db,
        CapabilityShape {
            direct: None,
            children: ShapeChildren::Sum(
                vec![(VariantIndex(0), fields), (VariantIndex(1), fields)].into(),
            ),
        },
    );
    let outer = array_shape(&db, sum, 4);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let value = values.from_shape(outer, &scope(), |_, path, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: Payload {
                tag: 0,
                indices: path.indices().collect(),
            },
        }]
    });
    let mut summaries = ValueInterner::new(&db, ValueLimits::default());
    let summary = values.map_payloads(&value, &mut summaries, |_, slot, entry, _| {
        assert!(matches!(
            slot.as_slice(),
            [
                Projection::Index(_),
                Projection::VariantField {
                    field: FieldIndex(0),
                    ..
                },
                Projection::Index(_)
            ]
        ));
        assert_eq!(slot.indices().collect::<Vec<_>>(), entry.payload.indices);
        vec![Guarded {
            guard: entry.guard.clone(),
            payload: SourceExpr {
                invalidated: false,
                views: Default::default(),
                source: test_roots::input(&db, InputSource::slot(0, slot.clone())),
                path: RegionPath::default(),
            },
        }]
    });
    for leaf in summaries.leaves(&summary, ValueOccurrence::Summary) {
        let source = leaf.payload.source;
        assert_eq!(
            source,
            test_roots::input(&db, InputSource::slot(0, leaf.path))
        );
    }
}

#[test]
fn summary_alternatives_own_unknown_indices_under_array_members() {
    let db = HirAnalysisTestDb::default();
    let leaf = leaf_shape(&db);
    let shape = array_shape(&db, leaf, 3);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let unknown = values.from_shape(shape, &scope(), |_, _, scope| {
        let (witness_scope, selected) = scope.bind(IndexNamespace::Existential);
        vec![Guarded {
            guard: Guard::always(&witness_scope)
                .with_bound(selected, IndexExpr::Const(3))
                .unwrap(),
            payload: Payload {
                tag: 1,
                indices: vec![selected],
            },
        }]
    });
    let first = values
        .project(
            &unknown,
            &path(IndexExpr::Const(0)),
            ValueOccurrence::Summary,
        )
        .expect("in-bounds projection");
    let last = values
        .project(
            &unknown,
            &path(IndexExpr::Const(2)),
            ValueOccurrence::Summary,
        )
        .expect("in-bounds projection");
    assert_eq!(
        first, last,
        "an opaque result does not assert pointwise input/output correlation"
    );
    assert_eq!(first.scope(), &scope());
    assert_eq!(
        first.direct()[0]
            .guard
            .scope()
            .existential_extension_of(&scope()),
        Some(1)
    );
    assert!(
        scope()
            .validate(first.direct()[0].payload.indices[0])
            .is_err()
    );
    let (outer, binder) = scope().bind(IndexNamespace::Result);
    let lifted = values.substitute(&unknown, &IndexSubst::new(&scope(), &outer, []).unwrap());
    let selected = values
        .project(&lifted, &path(binder), ValueOccurrence::Summary)
        .expect("in-bounds projection");
    assert_eq!(
        selected.direct()[0]
            .guard
            .scope()
            .existential_extension_of(&outer),
        Some(1)
    );
}

#[test]
fn existential_region_witnesses_are_fresh_for_independent_enum_selections() {
    let db = HirAnalysisTestDb::default();
    let base = scope();
    let (witness_scope, selected) = base.bind(IndexNamespace::Existential);
    let root = test_roots::local(&db, NRootId::from_u32(0));
    let guarded = |variant| {
        RegionSet::new(
            &base,
            [Guarded {
                guard: Guard::always(&witness_scope)
                    .with_bound(selected, IndexExpr::Const(2))
                    .unwrap()
                    .with_variant(
                        ChoiceKey::new(ValueOccurrence::Argument(0), path(selected)),
                        VariantIndex(variant),
                    )
                    .unwrap(),
                payload: SymbolicPlace {
                    views: Default::default(),
                    root: root.clone(),
                    path: RegionPath::default(),
                },
            }],
        )
    };
    let left = guarded(0);
    let right = guarded(1);
    assert!(
        !matches!(left.overlap(&db, &right), OverlapResult::Disjoint),
        "different selected elements can have different variants while holding the same target"
    );
    let whole = RegionSet::singleton(&base, root, RegionPath::default());
    assert!(whole.provably_covers(&left));
    assert!(!left.provably_covers(&whole));
    assert_eq!(
        whole.remove_covered(&left),
        whole,
        "a possible witness is not a definite write"
    );
}

#[test]
fn occurrence_substitution_rechecks_colliding_enum_choices() {
    let a = ValueOccurrence::SummaryChoice(0);
    let b = ValueOccurrence::SummaryChoice(1);
    let guard = Guard::always(&scope())
        .with_variant(
            ChoiceKey::new(a, StructuralPath::default()),
            VariantIndex(0),
        )
        .unwrap()
        .with_variant(
            ChoiceKey::new(b, StructuralPath::default()),
            VariantIndex(1),
        )
        .unwrap();
    assert!(
        guard
            .map_occurrences(|_| ValueOccurrence::Argument(0))
            .is_none()
    );
    let renamed = guard
        .map_occurrences(|occurrence| match occurrence {
            ValueOccurrence::SummaryChoice(choice) => ValueOccurrence::CallChoice {
                result: NValueId::from_u32(9),
                choice,
            },
            other => other,
        })
        .unwrap();
    assert!(
        renamed
            .map_occurrences(|occurrence| match occurrence {
                ValueOccurrence::CallChoice { choice, .. } =>
                    ValueOccurrence::SummaryChoice(choice),
                other => other,
            })
            .is_some_and(|restored| restored == guard)
    );
}

#[test]
fn payload_mapping_masks_overwritten_default_members_before_observing_sources() {
    let db = HirAnalysisTestDb::default();
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let shape = array_shape(&db, leaf_shape(&db), 3);
    let original = values.from_shape(shape, &scope(), |_, path, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: Payload {
                tag: 0,
                indices: path.indices().collect(),
            },
        }]
    });
    let replacement = values.from_shape(leaf_shape(&db), &scope(), |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: Payload {
                tag: 1,
                indices: Vec::new(),
            },
        }]
    });
    let updated = values.replace(&original, &path(IndexExpr::Const(0)), &replacement);
    let mut destination = ValueInterner::new(&db, ValueLimits::default());
    let mut visited_default = false;
    values.map_payloads(&updated, &mut destination, |_, _, entry, domain| {
        if entry.payload.tag == 0 {
            visited_default = true;
            let index = entry.payload.indices[0];
            assert!(domain.with_equality(index, IndexExpr::Const(0)).is_none());
            assert!(domain.with_equality(index, IndexExpr::Const(1)).is_some());
            assert!(domain.with_equality(index, IndexExpr::Const(3)).is_none());
        }
        vec![entry.clone()]
    });
    assert!(visited_default);
}

#[test]
fn opaque_handle_origins_preserve_copies_but_never_imply_fresh_storage() {
    let db = HirAnalysisTestDb::default();
    let scope = scope();
    let contract = OpaqueHandleContract {
        handle_ty: TyId::u256(&db),
        target_ty: TyId::u256(&db),
        address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
    };
    let first = RegionRoot::External(ExternalSource::opaque(
        &db,
        OpaqueHandleRef {
            contract,
            occurrence: AddressOccurrence::Summary(0),
            arguments: Box::new([]),
        },
    ));
    let second = RegionRoot::External(ExternalSource::opaque(
        &db,
        OpaqueHandleRef {
            contract,
            occurrence: AddressOccurrence::Summary(1),
            arguments: Box::new([]),
        },
    ));
    let left = RegionSet::singleton(
        &scope,
        first.clone(),
        RegionPath::new([Projection::Field(FieldIndex(0))]),
    );
    let same = left.clone();
    let sibling = RegionSet::singleton(
        &scope,
        first,
        RegionPath::new([Projection::Field(FieldIndex(1))]),
    );
    let unknown = RegionSet::singleton(
        &scope,
        second,
        RegionPath::new([Projection::Field(FieldIndex(1))]),
    );
    assert!(left.provably_covers(&same));
    assert_eq!(left.overlap(&db, &sibling), OverlapResult::Disjoint);
    assert_eq!(left.overlap(&db, &unknown), OverlapResult::Unknown);
    assert!(!left.provably_covers(&unknown));
    let local = RegionSet::singleton(
        &scope,
        test_roots::local(&db, NRootId::from_u32(0)),
        RegionPath::default(),
    );
    assert_eq!(left.overlap(&db, &local), OverlapResult::Unknown);
    let storage = RegionSet::singleton(
        &scope,
        RegionRoot::External(ExternalSource::opaque(
            &db,
            OpaqueHandleRef {
                contract: OpaqueHandleContract {
                    address_space: HandleAddressSpace::Known(ProviderAddressSpace::Storage),
                    ..contract
                },
                occurrence: AddressOccurrence::Summary(0),
                arguments: Box::new([]),
            },
        )),
        RegionPath::default(),
    );
    assert_eq!(left.overlap(&db, &storage), OverlapResult::Disjoint);
    assert_eq!(local.overlap(&db, &storage), OverlapResult::Disjoint);
}

#[test]
fn out_of_bounds_array_operations_have_no_reachable_capability_effect() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let original = leaf(&mut values, element, &scope(), 1, vec![]);
    let replacement = leaf(&mut values, element, &scope(), 2, vec![]);
    for len in [0, 2] {
        let shape = array_shape(&db, element, len);
        let array = values.array_repeat(shape, &original);
        for index in [len, usize::MAX] {
            let selected = path(IndexExpr::Const(index));
            assert!(
                values
                    .project(&array, &selected, ValueOccurrence::Summary)
                    .is_none()
            );
            assert_eq!(values.replace(&array, &selected, &replacement), array);
        }
        if len != 0 {
            assert_eq!(
                values.project(&array, &path(0.into()), ValueOccurrence::Summary),
                Some(original.clone())
            );
        }
    }
}

#[test]
fn opaque_handle_specialization_keeps_payload_and_shape_contracts_aligned() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "opaque_handle_specialization.fe".into(),
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}
struct Ptr<T> { raw: *T }
impl<T> EffectHandle for Ptr<T> {
    type Target = T
    type Raw = *T
    const SPACE: AddressSpace = AddressSpace::Memory
    fn raw(self) -> *T { self.raw }
}
fn inspect<const N: usize>(_ ptr: own Ptr<[u256; N]>) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "inspect");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let artifacts = normalize_semantic_body(&db, instance).expect("generic handle admission");
    let ty = artifacts
        .body
        .values
        .iter()
        .find(|value| matches!(value.definition, NValueDefinition::EntryParam { param: 0 }))
        .expect("handle parameter")
        .ty;
    let contract = OpaqueHandleContract::for_ty(&db, func.scope(), instance.assumptions(&db), ty)
        .unwrap()
        .expect("declared handle contract");
    let shape = capability_shape(&db, func.scope(), instance.assumptions(&db), ty).unwrap();
    let target = capability_shape(
        &db,
        func.scope(),
        instance.assumptions(&db),
        contract.target_ty,
    )
    .unwrap();
    let ShapeChildren::Array { len, .. } = target.children(&db) else {
        panic!("array target")
    };
    let origin = OpaqueHandleRef {
        contract,
        occurrence: AddressOccurrence::Summary(4),
        arguments: vec![runtime(2)].into_boxed_slice(),
    };
    let region = RegionSet::singleton(
        &scope(),
        RegionRoot::External(ExternalSource::opaque(&db, origin)),
        RegionPath::default(),
    );
    let source = SourceExpr::from_place(&region.clauses()[0].payload).unwrap();
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let value = values.from_shape(shape, &scope(), |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: CapabilityRef::Address(region.clone()),
        }]
    });
    for length in [0, 3] {
        let subst = IndexSubst::new(
            &scope(),
            &scope(),
            [
                (len.index(), IndexExpr::Const(length)),
                (runtime(2), IndexExpr::Const(5)),
            ],
        )
        .unwrap();
        let specialized = values.substitute(&value, &subst);
        let semantics = specialized.shape().direct(&db).unwrap();
        assert_eq!(semantics.target_ty.array_len(&db), Some(length));
        let selected = specialized.direct()[0].payload.region(&db, &[], &scope());
        let RegionRoot::External(ExternalSource {
            origin: ExternalOrigin::OpaqueHandle(origin),
            ..
        }) = &selected.clauses()[0].payload.root
        else {
            panic!("specialization must preserve the opaque origin")
        };
        assert_eq!(origin.contract.handle_ty, semantics.representation_ty);
        assert_eq!(origin.contract.target_ty, semantics.target_ty);
        assert_eq!(
            origin.contract.address_space,
            HandleAddressSpace::Known(ProviderAddressSpace::Memory)
        );
        assert_eq!(origin.occurrence, AddressOccurrence::Summary(4));
        assert_eq!(origin.arguments.as_ref(), &[IndexExpr::Const(5)]);
        assert_eq!(
            source.substitute(&db, &subst),
            SourceExpr::from_place(&selected.clauses()[0].payload).unwrap()
        );
    }
}

#[test]
fn referent_views_preserve_projected_storage_authority_and_nested_handle_origins() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "referent_views.fe".into(),
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}
struct Cell<const ID: u256> { value: u256 }
struct Ptr<T> { raw: *T }
impl<T> EffectHandle for Ptr<T> {
    type Target = T
    type Raw = *T
    const SPACE: AddressSpace = AddressSpace::Memory
    fn raw(self) -> *T { self.raw }
}
struct Holder<T> { ptr: Ptr<T> }
fn inspect(
    _ first: own [Holder<Cell<1>>; 2],
    _ second: own [Holder<Cell<2>>; 2],
    _ third: own [Holder<Cell<3>>; 2],
) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let func = find_func(&db, top_mod, "inspect");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    let artifacts = normalize_semantic_body(&db, instance).unwrap();
    let types: Vec<_> = artifacts
        .body
        .values
        .iter()
        .filter_map(|value| {
            matches!(value.definition, NValueDefinition::EntryParam { .. }).then_some(value.ty)
        })
        .collect();
    let [source_ty, target_ty, third_ty]: [TyId<'_>; 3] = types.try_into().unwrap();
    let shape = |ty| capability_shape(&db, func.scope(), instance.assumptions(&db), ty).unwrap();
    let proof = |source, target| {
        ReferentRepackId::new(&db, source, target, func.scope(), instance.assumptions(&db))
    };
    let root = |index| test_roots::local(&db, NRootId::from_u32(index));
    let region = |index| RegionSet::singleton(&scope(), root(index), RegionPath::default());
    let mut values = ValueInterner::new(&db, ValueLimits::default());
    let mut contents = |ty, index| {
        values.from_shape(shape(ty), &scope(), |_, _, scope| {
            vec![Guarded {
                guard: Guard::always(scope),
                payload: CapabilityRef::Address(RegionSet::singleton(
                    scope,
                    root(index),
                    RegionPath::default(),
                )),
            }]
        })
    };
    let original = contents(source_ty, 10);
    let replacement = contents(target_ty, 11);
    let mut state = BorrowState::new(&mut values, [], [(root(0), original.clone())]);
    let conversion = proof(source_ty, target_ty);
    let converted = region(0).repack(&db, conversion);
    assert!(matches!(
        converted.overlap(&db, &region(0)),
        OverlapResult::Overlap(_)
    ));
    assert_eq!(converted.repack(&db, conversion.inverse(&db)), region(0));
    assert_eq!(
        converted
            .repack(&db, proof(target_ty, third_ty))
            .repack(&db, proof(third_ty, source_ty)),
        region(0),
        "a conversion cycle must not grow the fixed-point state",
    );
    let selected = RegionPath::new([
        Projection::Index(IndexExpr::Const(1)),
        Projection::Field(FieldIndex(0)),
    ]);
    let read = state
        .read_region(
            &db,
            &mut values,
            &converted,
            shape(target_ty),
            ValueOccurrence::Summary,
        )
        .unwrap();
    let handle = values
        .project(
            &read,
            &StructuralPath::new(selected.as_slice()),
            ValueOccurrence::Summary,
        )
        .unwrap();
    let target = handle.direct()[0].payload.region(&db, &[], &scope());
    assert!(matches!(
        target.overlap(&db, &region(10)),
        OverlapResult::Overlap(_)
    ));
    assert_ne!(
        target,
        region(10),
        "nested handle keeps its referent conversion"
    );
    let projected_read = state
        .read_region(
            &db,
            &mut values,
            &converted.project(&selected),
            handle.shape(),
            ValueOccurrence::Summary,
        )
        .unwrap();
    assert_eq!(projected_read, handle, "projection and conversion commute");

    let new_handle = values
        .project(
            &replacement,
            &StructuralPath::new(selected.as_slice()),
            ValueOccurrence::Summary,
        )
        .unwrap();
    state
        .write_region(
            OpaqueWrite {
                site: OpaqueWriteSite::Summary(0),
                scope: func.scope(),
                assumptions: instance.assumptions(&db),
            },
            &mut values,
            &converted.project(&selected),
            &new_handle,
        )
        .unwrap();
    let reread = state
        .read_region(
            &db,
            &mut values,
            &converted.project(&selected),
            new_handle.shape(),
            ValueOccurrence::Summary,
        )
        .unwrap();
    assert_eq!(
        reread, new_handle,
        "inverse writeback and forward load preserve identity"
    );
    let original_handle = values
        .project(
            &original,
            &StructuralPath::new(selected.as_slice()),
            ValueOccurrence::Summary,
        )
        .unwrap();
    let physical = state
        .read_region(
            &db,
            &mut values,
            &region(0).project(&selected),
            original_handle.shape(),
            ValueOccurrence::Summary,
        )
        .unwrap();
    let physical_target = physical.direct()[0].payload.region(&db, &[], &scope());
    assert!(matches!(
        physical_target.overlap(&db, &region(11)),
        OverlapResult::Overlap(_)
    ));
    assert_ne!(
        physical_target,
        region(11),
        "physical storage keeps the inverse view"
    );
    let sibling = RegionPath::new([
        Projection::Index(IndexExpr::Const(0)),
        Projection::Field(FieldIndex(0)),
    ]);
    assert_eq!(
        state
            .read_region(
                &db,
                &mut values,
                &region(0).project(&sibling),
                original_handle.shape(),
                ValueOccurrence::Summary
            )
            .unwrap(),
        original_handle
    );

    let (mut loan, _, abstraction) = LoanDef::new(
        BorrowKind::Mut,
        BorrowActivation::Immediate,
        SemOrigin::Synthetic,
        &scope(),
    );
    loan.extend(&region(0).substitute(&db, &abstraction), []);
    let borrow = CapabilityRef::borrow(
        BorrowKind::Mut,
        LoanRef {
            id: LoanId(0),
            args: Box::new([]),
        },
    );
    let borrowed = values.from_shape(
        shape(TyId::borrow_mut_of(&db, source_ty)),
        &scope(),
        |_, _, scope| {
            vec![Guarded {
                guard: Guard::always(scope),
                payload: borrow.clone(),
            }]
        },
    );
    let borrowed_conversion = proof(
        TyId::borrow_mut_of(&db, source_ty),
        TyId::borrow_mut_of(&db, target_ty),
    );
    let converted_borrow = borrowed_conversion
        .apply(&db, &mut values, &borrowed, &[])
        .unwrap();
    let converted_payload = &converted_borrow.direct()[0].payload;
    assert_eq!(converted_payload.loan(), borrow.loan());
    assert_eq!(
        converted_payload.authority(&Guard::always(&scope())),
        borrow.authority(&Guard::always(&scope()))
    );
    assert_eq!(converted_payload.region(&db, &[loan], &scope()), converted);
    assert_eq!(
        borrowed_conversion
            .inverse(&db)
            .apply(&db, &mut values, &converted_borrow, &[])
            .unwrap(),
        borrowed
    );

    // Summary paths are relative to an input; their views must move with that
    // input when a call receives an already projected region.
    let input = RegionSet::singleton(
        &scope(),
        RegionRoot::External(test_roots::input(&db, InputSource::place(0))),
        RegionPath::default(),
    );
    let source = SourceExpr::from_place(
        &input.repack(&db, conversion).project(&selected).clauses()[0].payload,
    )
    .unwrap();
    let SourceExpr { path, views, .. } = source;
    let caller_prefix = RegionPath::new([Projection::Field(FieldIndex(7))]);
    let caller = region(0).project(&caller_prefix);
    assert_eq!(
        caller
            .project(&path)
            .with_relative_views(&db, &views, path.as_slice().len()),
        caller.repack(&db, conversion).project(&path)
    );
}

#[test]
fn symbolic_lengths_do_not_hide_capability_structure() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "symbolic_referents.fe".into(),
        r#"
struct Pointer<T> { pointer: *T }
struct Inline<T> { value: T }
fn inspect<T, const N: usize>(
    _ bytes: own [u8; N],
    _ generic: own [T; N],
    _ text: own String<N>,
    _ pointer: own Pointer<T>,
    _ borrowed: ref T,
    _ inline: own Inline<T>,
) {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "inspect"))),
    );
    let artifacts = normalize_semantic_body(&db, instance).unwrap();
    let abstract_inputs: Vec<_> = artifacts
        .body
        .values
        .iter()
        .filter(|value| matches!(value.definition, NValueDefinition::EntryParam { .. }))
        .map(|value| {
            ReferentContract::new(
                &db,
                value.ty,
                HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            )
            .is_abstract(&db)
        })
        .collect();
    assert_eq!(abstract_inputs, [false, true, false, false, false, true]);
}

#[test]
fn allocation_birth_selects_guarded_full_families_and_only_their_own_bytes() {
    let db = HirAnalysisTestDb::default();
    let word = TyId::u256(&db);
    let (template_scope, parameter) = scope().bind(IndexNamespace::Existential);
    let condition = runtime(20);
    let guard = Guard::always(&template_scope)
        .with_equality(condition, IndexExpr::Const(1))
        .unwrap();
    let birth = AllocationBirth {
        allocation: OpaqueHandleRef {
            contract: OpaqueHandleContract {
                handle_ty: TyId::ptr_to(&db, word),
                target_ty: word,
                address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            },
            occurrence: AddressOccurrence::Summary(0),
            arguments: Box::new([IndexExpr::Const(3), parameter, runtime(4)]),
        },
        guard,
    };
    let mut candidate = birth.allocation.clone();
    candidate.arguments = Box::new([IndexExpr::Const(3), IndexExpr::Const(99), runtime(4)]);
    let source = ExternalSource::allocation(&db, candidate.clone());
    let root = RegionRoot::External(source.clone());
    let selected = birth.selector(&root, &scope()).unwrap();
    assert!(selected.proves_equal(condition, IndexExpr::Const(1)));
    assert!(
        selected
            .with_equality(condition, IndexExpr::Const(0))
            .is_none()
    );
    let viewed = ExternalSource::memory(
        &db,
        SourceExpr {
            source: source.clone(),
            path: RegionPath::default(),
            views: Default::default(),
            invalidated: false,
        },
        TyId::u8(&db),
        Some((TyId::u8(&db), IndexExpr::Const(1))),
    );
    assert_eq!(
        birth.selector(&RegionRoot::External(viewed), &scope()),
        Some(selected.clone())
    );
    let followed = source.follow(RegionPath::default(), source.contract, false);
    assert!(
        birth
            .selector(&RegionRoot::External(followed), &scope())
            .is_none()
    );
    // Neither another allocation choice nor a different outer family member is
    // reset just because its final (current invocation) argument matches.
    candidate.occurrence = AddressOccurrence::Summary(1);
    assert!(
        birth
            .selector(
                &RegionRoot::External(ExternalSource::allocation(&db, candidate.clone())),
                &scope()
            )
            .is_none()
    );
    candidate.occurrence = birth.allocation.occurrence;
    candidate.arguments[0] = IndexExpr::Const(4);
    assert!(
        birth
            .selector(
                &RegionRoot::External(ExternalSource::allocation(&db, candidate)),
                &scope()
            )
            .is_none()
    );
    let unknown =
        ExternalSource::unknown(source.contract, AddressOccurrence::Summary(0), Box::new([]));
    assert!(AllocationBirth::from_source(&unknown, Guard::always(&scope())).is_none());
    assert!(
        birth
            .selector(&RegionRoot::External(unknown), &scope())
            .is_none()
    );
}

#[test]
fn region_projection_keeps_witnesses_observed_by_indexed_choices() {
    let (nested, witness) = scope().bind(IndexNamespace::Existential);
    let guard = Guard::always(&nested)
        .with_variant(
            ChoiceKey::new(ValueOccurrence::Summary, path(witness)),
            VariantIndex(0),
        )
        .unwrap()
        .with_disequality(witness, runtime(0))
        .unwrap();
    assert_eq!(guard.project_witnesses(|index| index == witness), guard);
    let scalar = Guard::always(&nested)
        .with_disequality(witness, runtime(0))
        .unwrap();
    assert_eq!(
        scalar.project_witnesses(|index| index == witness),
        Guard::always(&nested)
    );
}

#[test]
fn allocation_birth_selection_commutes_with_index_substitution() {
    let db = HirAnalysisTestDb::default();
    let word = TyId::u256(&db);
    let (template_scope, parameter) = scope().bind(IndexNamespace::Existential);
    let (candidate_scope, candidate_index) = template_scope.bind(IndexNamespace::Existential);
    let template = AllocationBirth {
        allocation: OpaqueHandleRef {
            contract: OpaqueHandleContract {
                handle_ty: TyId::ptr_to(&db, word),
                target_ty: word,
                address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            },
            occurrence: AddressOccurrence::Summary(0),
            arguments: Box::new([runtime(0), parameter]),
        },
        guard: Guard::always(&template_scope)
            .with_equality(runtime(2), IndexExpr::Const(1))
            .unwrap()
            .with_variant(
                ChoiceKey::new(ValueOccurrence::Summary, path(parameter)),
                VariantIndex(0),
            )
            .unwrap(),
    };
    for born in 0..3 {
        for observed in 0..3 {
            for enabled in 0..2 {
                let entries = [
                    (runtime(0), IndexExpr::Const(born)),
                    (runtime(1), IndexExpr::Const(observed)),
                    (runtime(2), IndexExpr::Const(enabled)),
                ];
                let template_subst =
                    IndexSubst::new(&template_scope, &template_scope, entries).unwrap();
                let candidate_subst =
                    IndexSubst::new(&candidate_scope, &candidate_scope, entries).unwrap();
                let substituted = template
                    .guard
                    .substitute(&template_subst)
                    .and_then(|guard| {
                        AllocationBirth::from_source(
                            &ExternalSource::allocation(&db, template.allocation.clone())
                                .substitute(&db, &template_subst),
                            guard,
                        )
                    });
                for choice in 0..2 {
                    let mut allocation = template.allocation.clone();
                    allocation.occurrence = AddressOccurrence::Summary(choice);
                    allocation.arguments = Box::new([runtime(1), candidate_index]);
                    let direct = ExternalSource::allocation(&db, allocation);
                    let viewed = ExternalSource::memory(
                        &db,
                        SourceExpr {
                            source: direct.clone(),
                            path: RegionPath::default(),
                            views: Default::default(),
                            invalidated: false,
                        },
                        TyId::u8(&db),
                        Some((TyId::u8(&db), IndexExpr::Const(1))),
                    );
                    let followed = direct.follow(RegionPath::default(), direct.contract, false);
                    for source in [direct, viewed, followed] {
                        let root = RegionRoot::External(source);
                        let before = template
                            .selector(&root, &candidate_scope)
                            .and_then(|guard| guard.substitute(&candidate_subst));
                        let after = substituted.as_ref().and_then(|birth| {
                            birth
                                .selector(&root.substitute(&db, &candidate_subst), &candidate_scope)
                        });
                        assert_eq!(
                            before, after,
                            "born {born}, observed {observed}, enabled {enabled}, root {root:?}"
                        );
                        if let Some(selected) = after {
                            assert!(
                                selected.indices().contains(&candidate_index),
                                "indexed enum witness must survive selection and substitution"
                            );
                        }
                    }
                }
            }
        }
    }
}

#[test]
fn guard_projection_agrees_with_finite_witness_enumeration() {
    let (nested, hidden) = scope().bind(IndexNamespace::Existential);
    let (nested, indexed) = nested.bind(IndexNamespace::Existential);
    for length in 1..=4 {
        for excluded in [
            runtime(0),
            IndexExpr::Const(0),
            IndexExpr::Const(1),
            IndexExpr::Const(3),
        ] {
            for variant in 0..2 {
                let guard = Guard::always(&nested)
                    .with_variant(
                        ChoiceKey::new(ValueOccurrence::Summary, path(indexed)),
                        VariantIndex(variant),
                    )
                    .unwrap()
                    .with_bound(hidden, IndexExpr::Const(length))
                    .unwrap()
                    .with_disequality(hidden, excluded);
                let Some(guard) = guard else { continue };
                let enumerated = (0..length)
                    .filter_map(|value| {
                        guard.substitute(
                            &IndexSubst::new(&nested, &nested, [(hidden, IndexExpr::Const(value))])
                                .unwrap(),
                        )
                    })
                    .reduce(|left, right| left.or(&right))
                    .unwrap();
                let projected =
                    guard.project_witnesses(|index| matches!(index, IndexExpr::Bound(_)));
                assert_eq!(projected, enumerated);
                assert!(projected.indices().contains(&indexed));
                assert!(!projected.indices().contains(&hidden));
            }
        }
    }
}

#[test]
fn independent_enum_choice_unions_have_linear_size() {
    let scope = scope();
    // Both different roots and different fields within one root are independent.
    for fields in [false, true] {
        let choice = |index| {
            if fields {
                ChoiceKey::new(
                    ValueOccurrence::Argument(0),
                    StructuralPath::new([Projection::Field(FieldIndex(index as u16))]),
                )
            } else {
                ChoiceKey::new(ValueOccurrence::Argument(index), StructuralPath::default())
            }
        };
        let alternatives: Vec<_> = (0..10)
            .map(|index| {
                Guard::always(&scope)
                    .with_variant(choice(index), VariantIndex(1))
                    .unwrap()
            })
            .collect();
        let union = alternatives
            .iter()
            .cloned()
            .reduce(|left, right| left.or(&right))
            .unwrap();
        assert!(
            union.node_count() <= alternatives.len() * 32,
            "{} nodes for {} choices",
            union.node_count(),
            alternatives.len()
        );
        for alternative in &alternatives {
            assert!(alternative.implies(&union));
        }
        let excluded = (0..10).fold(Guard::always(&scope), |guard, index| {
            guard.with_variant(choice(index), VariantIndex(2)).unwrap()
        });
        assert!(union.and(&excluded).is_none());
    }
}

#[test]
fn grouped_enum_choices_preserve_index_aliases_and_full_tags() {
    let scope = scope();
    let choice = |field, index| {
        ChoiceKey::new(
            ValueOccurrence::Argument(0),
            StructuralPath::new([
                Projection::Field(FieldIndex(field)),
                Projection::Index(index),
            ]),
        )
    };
    for (left_tag, right_tag) in [(0, 1), (0, 32768), (1, u16::MAX)] {
        let left = Guard::always(&scope)
            .with_variant(choice(0, runtime(0)), VariantIndex(left_tag))
            .unwrap();
        let right = Guard::always(&scope)
            .with_variant(choice(0, runtime(1)), VariantIndex(right_tag))
            .unwrap();
        let pair = left.and(&right).unwrap();
        assert!(pair.with_equality(runtime(0), runtime(1)).is_none());
        assert!(pair.with_disequality(runtime(0), runtime(1)).is_some());
        let separate_field = Guard::always(&scope)
            .with_variant(choice(1, runtime(1)), VariantIndex(right_tag))
            .unwrap();
        assert!(
            left.and(&separate_field)
                .unwrap()
                .with_equality(runtime(0), runtime(1))
                .is_some()
        );
        let partition = left.with_equality(runtime(0), runtime(1)).unwrap();
        assert_eq!(
            partition.or(&left.with_disequality(runtime(0), runtime(1)).unwrap()),
            left
        );
    }
}

#[test]
fn identity_substitution_reuses_nested_values_without_interning() {
    let db = HirAnalysisTestDb::default();
    let element = leaf_shape(&db);
    let shape = array_shape(&db, array_shape(&db, element, 3), 1_000_000);
    let mut values = ValueInterner::new(
        &db,
        ValueLimits {
            interned_nodes: 0,
            ..ValueLimits::default()
        },
    );
    let ShapeChildren::Array { element: inner, .. } = shape.children(&db) else {
        panic!("array")
    };
    let value = values.array(shape, &scope(), |values, outer, i| {
        values.array(*inner, outer, |values, inner, j| {
            leaf(values, element, inner, 1, vec![i, j, runtime(0)])
        })
    });
    let identity = IndexSubst::new(&scope(), &scope(), [(runtime(0), runtime(0))]).unwrap();
    let before = values.metrics();
    assert_eq!(values.substitute(&value, &identity), value);
    assert_eq!(values.metrics(), before);

    let (extended, _) = scope().bind(IndexNamespace::Result);
    let extension = IndexSubst::new(&scope(), &extended, []).unwrap();
    let lifted = values.substitute(&value, &extension);
    assert_eq!(lifted.scope(), &extended);
    let changed = IndexSubst::new(&scope(), &scope(), [(runtime(0), 7.into())]).unwrap();
    let changed = values.substitute(&value, &changed);
    let leaves = values.leaves(&changed, ValueOccurrence::Argument(0));
    assert_eq!(leaves[0].payload.indices[2], IndexExpr::Const(7));
}

#[test]
fn caller_and_callee_choice_unions_stay_compact_through_summary_renaming() {
    let scope = scope();
    let alternatives: Vec<_> = (0..10)
        .map(|index| {
            Guard::always(&scope)
                .with_variant(
                    ChoiceKey::new(
                        ValueOccurrence::Value(NValueId::from_u32(index * 2)),
                        StructuralPath::default(),
                    ),
                    VariantIndex(1),
                )
                .unwrap()
                .with_variant(
                    ChoiceKey::new(
                        ValueOccurrence::CallChoice {
                            result: NValueId::from_u32(index * 2 + 1),
                            choice: 0,
                        },
                        StructuralPath::default(),
                    ),
                    VariantIndex(1),
                )
                .unwrap()
        })
        .collect();
    let union = alternatives
        .iter()
        .cloned()
        .reduce(|left, right| left.or(&right))
        .unwrap();
    assert!(
        union.node_count() <= alternatives.len() * 64,
        "{} caller/callee nodes",
        union.node_count()
    );
    for alternative in &alternatives {
        assert!(alternative.implies(&union));
    }
    let mut choices = BTreeMap::new();
    let summary = union
        .map_occurrences(|occurrence| {
            let next = choices.len() as u32;
            ValueOccurrence::SummaryChoice(*choices.entry(occurrence).or_insert(next))
        })
        .unwrap();
    assert!(
        summary.node_count() <= alternatives.len() * 64,
        "{} summary nodes",
        summary.node_count()
    );
    let instantiated = summary
        .map_occurrences(|occurrence| {
            let ValueOccurrence::SummaryChoice(choice) = occurrence else {
                panic!("summary choice")
            };
            ValueOccurrence::CallChoice {
                result: NValueId::from_u32(100),
                choice,
            }
        })
        .unwrap();
    assert!(
        instantiated.node_count() <= alternatives.len() * 64,
        "{} instantiated nodes",
        instantiated.node_count()
    );
}

#[test]
fn guard_identity_operations_preserve_indexed_choice_constraints() {
    let always = Guard::always(&scope());
    let choice = |index| {
        ChoiceKey::new(
            ValueOccurrence::Argument(0),
            StructuralPath::new([Projection::Index(index)]),
        )
    };
    let guard = always
        .with_variant(choice(runtime(0)), VariantIndex(0))
        .unwrap()
        .with_variant(choice(runtime(1)), VariantIndex(u16::MAX))
        .unwrap();
    assert_eq!(always.and(&guard), Some(guard.clone()));
    assert_eq!(guard.and(&always), Some(guard.clone()));
    assert_eq!(guard.and(&guard), Some(guard.clone()));
    assert_eq!(guard.or(&always), always);
    assert_eq!(always.or(&guard), always);
    assert_eq!(guard.or(&guard), guard);
    let mut observed = Vec::new();
    let identity = guard
        .map_occurrences(|occurrence| {
            observed.push(occurrence);
            occurrence
        })
        .unwrap();
    assert_eq!(observed, [ValueOccurrence::Argument(0)]);
    assert_eq!(identity, guard);
    assert!(identity.with_equality(runtime(0), runtime(1)).is_none());
}
