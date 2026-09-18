use super::*;
use crate::{
    analysis::{
        semantic::{
            BorrowActivation, FieldIndex, SemOrigin,
            capability::{
                index::IndexNamespace,
                loan::{LoanId, LoanRef},
                path::Projection,
                semantics::{CapabilityClass, CapabilitySemantics, StorageClass},
                shape::{ArrayLength, CapabilityShape, ShapeChildren},
                source::InputSource,
                value::{Guarded, ValueLimits},
            },
            normalized::NRootId,
        },
        ty::{
            provider::ProviderTransport,
            ty_def::{BorrowKind, TyId},
        },
    },
    test_db::HirAnalysisTestDb,
};

struct Shapes<'db> {
    scalar: ShapeId<'db>,
    handle: ShapeId<'db>,
    pair: ShapeId<'db>,
    array: ShapeId<'db>,
}

impl<'db> Shapes<'db> {
    fn new(db: &'db HirAnalysisTestDb) -> Self {
        let scalar = ShapeId::new(
            db,
            CapabilityShape {
                direct: None,
                children: ShapeChildren::None,
            },
        );
        let ty = TyId::u256(db);
        let handle = ShapeId::new(
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
        );
        let pair = ShapeId::new(
            db,
            CapabilityShape {
                direct: None,
                children: ShapeChildren::Product(
                    [(FieldIndex(0), handle), (FieldIndex(1), handle)].into(),
                ),
            },
        );
        let array = ShapeId::new(
            db,
            CapabilityShape {
                direct: None,
                children: ShapeChildren::Array {
                    len: ArrayLength::Known(3),
                    element: handle,
                },
            },
        );
        Self {
            scalar,
            handle,
            pair,
            array,
        }
    }
}

fn root<'db>(index: u32) -> RegionRoot<'db> {
    RegionRoot::Root(NRootId::from_u32(index))
}

fn region<'db>(root: RegionRoot<'db>) -> RegionSet<'db> {
    RegionSet::singleton(&BinderScope::default(), root, RegionPath::default())
}

fn handle<'db>(
    values: &mut CapabilityValues<'db>,
    shape: ShapeId<'db>,
    id: usize,
) -> CapabilityValue<'db> {
    values.from_shape(shape, &BinderScope::default(), |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: CapabilityRef::Mutable(LoanRef {
                id: LoanId(id),
                args: Box::new([]),
            }),
        }]
    })
}

fn loan<'db>(region: &RegionSet<'db>) -> LoanDef<'db> {
    let (mut loan, _, abstraction) = LoanDef::new(
        BorrowKind::Mut,
        BorrowActivation::Immediate,
        SemOrigin::Synthetic,
        region.scope(),
    );
    loan.extend(&region.substitute(&abstraction), []);
    loan
}

fn read<'db>(
    db: &'db HirAnalysisTestDb,
    values: &mut CapabilityValues<'db>,
    state: &BorrowState<'db>,
    region: &RegionSet<'db>,
    shape: ShapeId<'db>,
) -> CapabilityValue<'db> {
    state
        .read_region(
            db,
            values,
            region,
            shape,
            ValueOccurrence::Value(NValueId::from_u32(99)),
        )
        .unwrap()
}

#[test]
fn outer_borrow_tracks_contents_without_conflating_handle_slot_and_referent() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let first = handle(&mut values, shapes.handle, 0);
    let second = handle(&mut values, shapes.handle, 1);
    let outer = handle(&mut values, shapes.handle, 2);
    let pair = values.product(
        shapes.pair,
        &scope,
        [
            (FieldIndex(0), first.clone()),
            (FieldIndex(1), first.clone()),
        ],
    );
    let mut state = BorrowState::new(
        &mut values,
        [
            (NValueId::from_u32(0), shapes.handle),
            (NValueId::from_u32(1), shapes.handle),
        ],
        [(root(0), pair)],
    );
    state.set_value(NValueId::from_u32(0), outer.clone());
    let loans = [
        loan(&region(root(1))),
        loan(&region(root(2))),
        loan(&region(root(0))),
    ];
    let field = RegionPath::new([Projection::Field(FieldIndex(0))]);
    let slot = state.referent_region(NValueId::from_u32(0), &field, &loans);
    let loaded = read(&db, &mut values, &state, &slot, shapes.handle);
    state.set_value(NValueId::from_u32(1), loaded);
    assert_eq!(
        state.referent_region(NValueId::from_u32(1), &RegionPath::default(), &loans),
        region(root(1))
    );
    assert!(slot.intersection(&region(root(1))).is_empty());
    state.write_region(&mut values, &slot, &second).unwrap();
    assert_eq!(state.value(NValueId::from_u32(0)), &outer);
    assert_eq!(
        state.value(NValueId::from_u32(1)),
        &first,
        "an existing load keeps its old referent"
    );
    assert_eq!(read(&db, &mut values, &state, &slot, shapes.handle), second);
    let sibling = region(root(0)).project(&RegionPath::new([Projection::Field(FieldIndex(1))]));
    assert_eq!(
        read(&db, &mut values, &state, &sibling, shapes.handle),
        first
    );
}

#[test]
fn dynamic_stores_partition_array_members_and_exact_overwrites_remove_old_handles() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let old = handle(&mut values, shapes.handle, 0);
    let new = handle(&mut values, shapes.handle, 1);
    let array = values.array_repeat(shapes.array, &old);
    let mut state = BorrowState::new(&mut values, [], [(root(0), array)]);
    let index = IndexExpr::Runtime(NValueId::from_u32(0));
    let selected = region(root(0)).project(&RegionPath::new([Projection::Index(index)]));
    state.write_region(&mut values, &selected, &new).unwrap();
    let loaded = read(&db, &mut values, &state, &selected, shapes.handle);
    assert_eq!(loaded.direct().len(), 1);
    assert_eq!(loaded.direct()[0].payload, new.direct()[0].payload);
    let zero = region(root(0)).project(&RegionPath::new([Projection::Index(IndexExpr::Const(0))]));
    let loaded = read(&db, &mut values, &state, &zero, shapes.handle);
    let entries = loaded.direct();
    assert_eq!(entries.len(), 2);
    assert!(
        entries
            .iter()
            .find(|entry| entry.payload == new.direct()[0].payload)
            .unwrap()
            .guard
            .proves_equal(index, IndexExpr::Const(0))
    );
    let absent = values.empty(shapes.handle, &scope);
    state.write_region(&mut values, &zero, &absent).unwrap();
    assert!(read(&db, &mut values, &state, &zero, shapes.handle).is_empty());
    assert!(!read(&db, &mut values, &state, &region(root(0)), shapes.array).is_empty());
}

#[test]
fn symbolic_external_referents_preserve_member_identity_and_followed_handle_identity() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let (family_scope, member) = scope.bind(IndexNamespace::InputSlot);
    let source = InputSource::slot(0, StructuralPath::new([Projection::Index(member)]))
        .follow(RegionPath::new([Projection::Field(FieldIndex(0))]));
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let initial = values.from_shape(shapes.handle, &family_scope, |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: CapabilityRef::Mutable(LoanRef {
                id: LoanId(0),
                args: [member].into(),
            }),
        }]
    });
    let mut state = BorrowState::new(
        &mut values,
        [],
        [(RegionRoot::Input(source.clone()), initial)],
    );
    let index = IndexExpr::Runtime(NValueId::from_u32(0));
    let instantiate = |index| {
        let subst = IndexSubst::new(&family_scope, &scope, [(member, index)]).unwrap();
        region(RegionRoot::Input(source.substitute(&subst)))
    };
    let selected = instantiate(index);
    let loaded = read(&db, &mut values, &state, &selected, shapes.handle);
    assert_eq!(
        loaded.direct()[0].payload.loan().unwrap().args.as_ref(),
        &[index]
    );
    let replacement = handle(&mut values, shapes.handle, 1);
    state
        .write_region(&mut values, &selected, &replacement)
        .unwrap();
    assert_eq!(
        read(&db, &mut values, &state, &selected, shapes.handle),
        replacement
    );
    let zero = read(
        &db,
        &mut values,
        &state,
        &instantiate(IndexExpr::Const(0)),
        shapes.handle,
    );
    assert_eq!(zero.direct().len(), 2);
    assert!(
        zero.direct()
            .iter()
            .find(|entry| entry.payload == replacement.direct()[0].payload)
            .unwrap()
            .guard
            .proves_equal(index, IndexExpr::Const(0))
    );
    let slot = region(RegionRoot::Input(InputSource::slot(
        0,
        StructuralPath::new([Projection::Index(index)]),
    )));
    assert!(matches!(
        state.read_region(
            &db,
            &mut values,
            &slot,
            shapes.handle,
            ValueOccurrence::Summary
        ),
        Err(StateError::MissingStorage(_))
    ));
}

#[test]
fn conditional_and_ambiguous_stores_keep_unwritten_contents() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let old = handle(&mut values, shapes.handle, 0);
    let new = handle(&mut values, shapes.handle, 1);
    let mut state = BorrowState::new(
        &mut values,
        [],
        [(root(0), old.clone()), (root(1), old.clone())],
    );
    let selector = IndexExpr::Runtime(NValueId::from_u32(0));
    let condition = Guard::always(&scope)
        .with_equality(selector, IndexExpr::Const(0))
        .unwrap();
    state
        .write_region(&mut values, &region(root(0)).with_guard(&condition), &new)
        .unwrap();
    let loaded = read(&db, &mut values, &state, &region(root(0)), shapes.handle);
    assert_eq!(loaded.direct().len(), 2);
    assert!(
        loaded
            .direct()
            .iter()
            .find(|entry| entry.payload == new.direct()[0].payload)
            .unwrap()
            .guard
            .implies(&condition)
    );
    let ambiguous = region(root(0)).union(&region(root(1)));
    state.write_region(&mut values, &ambiguous, &new).unwrap();
    assert_eq!(
        read(&db, &mut values, &state, &region(root(1)), shapes.handle),
        values.join(&old, &new)
    );
}

#[test]
fn missing_capability_storage_is_an_error_and_failed_writes_are_atomic() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let old = handle(&mut values, shapes.handle, 0);
    let new = handle(&mut values, shapes.handle, 1);
    let mut state = BorrowState::new(&mut values, [], [(root(0), old)]);
    let before = state.clone();
    let destination = region(root(0)).union(&region(root(1)));
    assert_eq!(
        state.write_region(&mut values, &destination, &new),
        Err(StateError::MissingStorage(root(1)))
    );
    assert_eq!(state, before);
    assert_eq!(
        state.read_region(
            &db,
            &mut values,
            &region(root(1)),
            shapes.handle,
            ValueOccurrence::Summary
        ),
        Err(StateError::MissingStorage(root(1)))
    );
    assert_eq!(
        read(&db, &mut values, &state, &region(root(1)), shapes.scalar),
        values.empty(shapes.scalar, &scope)
    );

    let exact_source = RegionRoot::Input(InputSource::slot(
        0,
        StructuralPath::new([Projection::Index(IndexExpr::Const(0))]),
    ));
    let exact = BorrowState::new(&mut values, [], [(exact_source, new)]);
    let unknown = region(RegionRoot::Input(InputSource::slot(
        0,
        StructuralPath::new([Projection::Index(IndexExpr::Runtime(NValueId::from_u32(0)))]),
    )));
    assert!(
        matches!(
            exact.read_region(
                &db,
                &mut values,
                &unknown,
                shapes.handle,
                ValueOccurrence::Summary
            ),
            Err(StateError::MissingStorage(_))
        ),
        "one exact member does not cover an unknown member"
    );
}

#[test]
fn joins_include_storage_contents_and_are_independent_of_predecessor_order() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let first = handle(&mut values, shapes.handle, 0);
    let second = handle(&mut values, shapes.handle, 1);
    let holder = NValueId::from_u32(0);
    let mut left = BorrowState::new(
        &mut values,
        [(holder, shapes.handle)],
        [(root(0), first.clone())],
    );
    left.set_value(holder, first);
    let mut right = left.clone();
    right.set_value(holder, second.clone());
    right
        .write_region(&mut values, &region(root(0)), &second)
        .unwrap();
    let mut left_first = left.clone();
    assert!(left_first.join(&right, &mut values));
    let mut right_first = right;
    assert!(right_first.join(&left, &mut values));
    assert_eq!(left_first, right_first);
    assert!(!left_first.join(&right_first, &mut values));
    assert_eq!(
        read(
            &db,
            &mut values,
            &left_first,
            &region(root(0)),
            shapes.handle
        ),
        *left_first.value(holder)
    );
}

#[test]
fn symbolic_array_writes_are_pointwise_and_can_select_a_diagonal() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let old = handle(&mut values, shapes.handle, 0);
    let row = values.array_repeat(shapes.array, &old);
    let matrix_shape = ShapeId::new(
        &db,
        CapabilityShape {
            direct: None,
            children: ShapeChildren::Array {
                len: ArrayLength::Known(3),
                element: shapes.array,
            },
        },
    );
    let matrix = values.array_repeat(matrix_shape, &row);
    let mut state = BorrowState::new(&mut values, [], [(root(0), matrix)]);
    let (write_scope, member) = scope.bind(IndexNamespace::Result);
    let replacement = values.from_shape(shapes.handle, &write_scope, |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: CapabilityRef::Mutable(LoanRef {
                id: LoanId(1),
                args: [member].into(),
            }),
        }]
    });
    let diagonal = RegionSet::singleton(
        &write_scope,
        root(0),
        RegionPath::new([Projection::Index(member), Projection::Index(member)]),
    );
    state
        .write_region(&mut values, &diagonal, &replacement)
        .unwrap();
    for row in [0, 1, 2] {
        for column in [0, 1, 2] {
            let element = region(root(0)).project(&RegionPath::new([
                Projection::Index(IndexExpr::Const(row)),
                Projection::Index(IndexExpr::Const(column)),
            ]));
            let loaded = read(&db, &mut values, &state, &element, shapes.handle);
            assert_eq!(loaded.direct().len(), 1);
            let reference = loaded.direct()[0].payload.loan().unwrap();
            if row == column {
                assert_eq!(reference.id, LoanId(1));
                assert_eq!(reference.args.as_ref(), &[IndexExpr::Const(row)]);
            } else {
                assert_eq!(reference.id, LoanId(0));
            }
        }
    }
}

#[test]
fn family_writes_keep_input_slot_and_destination_array_binders_independent() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let (storage_scope, slot) = scope.bind(IndexNamespace::InputSlot);
    let source = InputSource::slot(0, StructuralPath::new([Projection::Index(slot)]));
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let initial = values.empty(shapes.array, &storage_scope);
    let mut state = BorrowState::new(
        &mut values,
        [],
        [(RegionRoot::Input(source.clone()), initial)],
    );
    let (write_scope, input_member) = scope.bind(IndexNamespace::Value);
    let (write_scope, result_member) = write_scope.bind(IndexNamespace::Result);
    let write_source = source.substitute(
        &IndexSubst::new(&storage_scope, &write_scope, [(slot, input_member)]).unwrap(),
    );
    let replacement = values.from_shape(shapes.handle, &write_scope, |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: CapabilityRef::Mutable(LoanRef {
                id: LoanId(0),
                args: [input_member, result_member].into(),
            }),
        }]
    });
    let destination = RegionSet::singleton(
        &write_scope,
        RegionRoot::Input(write_source),
        RegionPath::new([Projection::Index(result_member)]),
    );
    state
        .write_region(&mut values, &destination, &replacement)
        .unwrap();
    let selected_source = source.substitute(
        &IndexSubst::new(&storage_scope, &scope, [(slot, IndexExpr::Const(7))]).unwrap(),
    );
    let element = region(RegionRoot::Input(selected_source))
        .project(&RegionPath::new([Projection::Index(IndexExpr::Const(2))]));
    let loaded = read(&db, &mut values, &state, &element, shapes.handle);
    assert_eq!(loaded.direct().len(), 1);
    assert_eq!(
        loaded.direct()[0].payload.loan().unwrap().args.as_ref(),
        &[IndexExpr::Const(7), IndexExpr::Const(2)]
    );
}

#[test]
fn a_family_write_cannot_capture_an_unbound_source_index() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let initial = values.empty(shapes.array, &scope);
    let mut state = BorrowState::new(&mut values, [], [(root(0), initial)]);
    let (write_scope, unrelated) = scope.bind(IndexNamespace::Value);
    let replacement = values.from_shape(shapes.handle, &write_scope, |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: CapabilityRef::Mutable(LoanRef {
                id: LoanId(0),
                args: [unrelated].into(),
            }),
        }]
    });
    let index = IndexExpr::Runtime(NValueId::from_u32(0));
    let destination = RegionSet::singleton(
        &write_scope,
        root(0),
        RegionPath::new([Projection::Index(index)]),
    );
    let before = state.clone();
    assert_eq!(
        state.write_region(&mut values, &destination, &replacement),
        Err(StateError::UnrepresentableWrite(root(0)))
    );
    assert_eq!(state, before);
}

#[test]
fn a_guarded_family_write_specializes_an_exact_input_referent() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let scope = BinderScope::default();
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let exact = RegionRoot::Input(InputSource::slot(
        0,
        StructuralPath::new([Projection::Index(IndexExpr::Const(0))]),
    ));
    let initial = values.empty(shapes.handle, &scope);
    let mut state = BorrowState::new(&mut values, [], [(exact.clone(), initial)]);
    let (write_scope, member) = scope.bind(IndexNamespace::Result);
    let replacement = values.from_shape(shapes.handle, &write_scope, |_, _, scope| {
        vec![Guarded {
            guard: Guard::always(scope),
            payload: CapabilityRef::Mutable(LoanRef {
                id: LoanId(0),
                args: [member].into(),
            }),
        }]
    });
    let source = RegionRoot::Input(InputSource::slot(
        0,
        StructuralPath::new([Projection::Index(member)]),
    ));
    let guard = Guard::always(&write_scope)
        .with_equality(member, IndexExpr::Const(0))
        .unwrap();
    let destination =
        RegionSet::singleton(&write_scope, source, RegionPath::default()).with_guard(&guard);
    state
        .write_region(&mut values, &destination, &replacement)
        .unwrap();
    let loaded = read(&db, &mut values, &state, &region(exact), shapes.handle);
    assert_eq!(loaded.direct().len(), 1);
    assert_eq!(
        loaded.direct()[0].payload.loan().unwrap().args.as_ref(),
        &[IndexExpr::Const(0)]
    );
}

#[test]
#[should_panic(expected = "storage binders must occur in its root")]
fn storage_families_cannot_own_unrelated_binders() {
    let db = HirAnalysisTestDb::default();
    let shapes = Shapes::new(&db);
    let mut values = CapabilityValues::new(&db, ValueLimits::default());
    let (scope, _) = BinderScope::default().bind(IndexNamespace::InputSlot);
    let initial = values.empty(shapes.handle, &scope);
    BorrowState::new(
        &mut values,
        [],
        [(RegionRoot::Input(InputSource::place(0)), initial)],
    );
}
