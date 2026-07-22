#[path = "support/layout.rs"]
mod layout_test_support;

use fe_hir::{
    analysis::{
        initialize_analysis_pass,
        semantic::{
            EffectProviderSubst, GenericSubst, ImplEnv, NExpr, NSStmtKind, SemanticInstanceKey,
            get_or_build_semantic_instance, identity_semantic_instance_key,
            normalize_semantic_body,
        },
        ty::{
            LayoutBundleComponentId, LayoutBundleComponentKey, LayoutBundleComponentTransport,
            LayoutBundleInterfaceError, LayoutBundlePathStep, LayoutBundleSchemaError,
            LayoutEvidencePathStep, ProviderAddressSpace,
            const_ty::CallableInputLayoutHoleOrigin,
            ty_check::{
                BodyOwner, ReturnProjectionStep, ReturnProvenance, ReturnSource,
                check_contract_init_body, check_func_body,
            },
            ty_def::TyId,
            ty_lower::{
                CallableInputLayoutBackingSource, callable_input_layout_backing_index_lengths,
                callable_input_layout_backing_sources, callable_input_layout_bundle_schema,
                callable_layout_bundle_signature,
            },
        },
    },
    core::semantic::{
        AllocatedContractStorageLayout, AllocationUnitId, AssignedRootValue, ContractFieldId,
        ContractLayoutError, EnumOverlayGroup, FieldStorageLayout, LayoutBinding,
        LayoutBindingTarget, LayoutInvariantError, LayoutProjection, LayoutRootFamilyId,
        LayoutViewKind, PlaceStep, RootRole, StoragePlace, validate_allocated_contract_layout,
    },
    hir_def::{CallableDef, Contract, Expr, IdentId, ItemKind, Partial},
    test_db::{HirAnalysisTestDb, find_contract, format_diagnostics},
};
use layout_test_support::{parse_module, parse_ok};

fn field<'db>(
    db: &'db HirAnalysisTestDb,
    contract: Contract<'db>,
    name: &str,
) -> &'db FieldStorageLayout<'db> {
    let layout = contract.storage_layout(db);
    layout
        .field(&IdentId::new(db, name.to_string()))
        .unwrap_or_else(|| panic!("missing allocated field `{name}`: {layout:#?}"))
}

fn mutated_layout_error<'db>(
    db: &'db HirAnalysisTestDb,
    layout: &AllocatedContractStorageLayout<'db>,
    mutate: impl FnOnce(&mut AllocatedContractStorageLayout<'db>),
) -> LayoutInvariantError<'db> {
    let mut invalid = layout.clone();
    mutate(&mut invalid);
    validate_allocated_contract_layout(db, &invalid).expect_err("mutated layout should be invalid")
}

#[test]
fn field_and_type_parameter_landings_are_distinct_and_shape_stable() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Pair<T> { left: T, right: T }

contract C {
    mut first: Pair<Slot>,
    mut second: Pair<Slot>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let first = field(&db, contract, "first");
    let second = field(&db, contract, "second");

    assert_eq!(first.cells.len(), 2);
    assert_eq!(second.cells.len(), 2);
    assert_eq!(first.slot_count, 2);
    assert_eq!(second.slot_offset, 2);
    assert_eq!(first.target.shape_key(&db), second.target.shape_key(&db));
    assert_ne!(first.cells[0].root, first.cells[1].root);
    assert!(
        first
            .cells
            .iter()
            .all(|cell| cell.role == RootRole::Counted && cell.allocation.is_some())
    );
    assert!(first.declared.all_roots_classified(&db));
    assert!(first.target.all_roots_classified(&db));
    assert!(first.slot_basis.all_roots_classified(&db));
    for (field_idx, expected) in [(0, "Slot<0>"), (1, "Slot<1>")] {
        let selection = first
            .selection_for_projections(
                &db,
                LayoutViewKind::Target,
                &[LayoutProjection::Field(field_idx)],
            )
            .unwrap();
        assert_eq!(
            first
                .projected_concrete_ty(&db, LayoutViewKind::Target, &selection)
                .unwrap()
                .pretty_print(&db)
                .to_string(),
            expected
        );
    }
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();
}

#[test]
fn inferred_roots_skip_explicit_contract_reservations() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

contract C {
    x: StorageMap<u256, u256, 1>,
    y: StorageMap<u256, u256>,
    z: StorageMap<u256, u256>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();
    let x = field(&db, contract, "x");
    let y = field(&db, contract, "y");
    let z = field(&db, contract, "z");

    assert_eq!(x.slot_count, 0);
    assert_eq!(x.concrete_occurrences.len(), 1);
    assert_eq!(x.concrete_occurrences[0].value.data(&db).to_string(), "1");
    assert_eq!(y.cells[0].allocation.unwrap().slot, 0);
    assert_eq!(z.cells[0].allocation.unwrap().slot, 2);
    assert_eq!(layout.explicit_reservations.len(), 1);
    assert_eq!(
        layout.explicit_reservations[0].value.data(&db).to_string(),
        "1"
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn concrete_root_expressions_evaluate_after_generic_substitution() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct UsizeSlot<const ROOT: usize = _> {}
type Offset<const ROOT: u256> = Slot<{ ROOT + 1 }>
type UsizeOffset<const ROOT: usize> = UsizeSlot<{ ROOT + 1 }>
struct Holder<const ROOT: u256> { value: Slot<{ ROOT + 2 }> }
const THREE: u256 = 3

contract C {
    mut alias: Offset<0>,
    mut usize_alias: UsizeOffset<0>,
    mut holder: Holder<0>,
    mut named: Slot<THREE>,
    mut first: Slot,
    mut second: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let result = contract.storage_layout(&db);
    let layout = result
        .allocated
        .as_ref()
        .unwrap_or_else(|| panic!("missing allocation: {result:#?}"));
    let reservations = layout
        .explicit_reservations
        .iter()
        .map(|reservation| reservation.value.data(&db).to_string())
        .collect::<Vec<_>>();

    assert_eq!(reservations, ["1", "2", "3"]);
    assert_eq!(layout.explicit_reservations[0].occurrences.len(), 2);
    assert_eq!(field(&db, contract, "alias").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "usize_alias")
            .concrete_occurrences
            .len(),
        1
    );
    assert_eq!(field(&db, contract, "holder").concrete_occurrences.len(), 1);
    assert_eq!(field(&db, contract, "named").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "second").cells[0]
            .allocation
            .unwrap()
            .slot,
        4
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn provider_root_expressions_evaluate_after_impl_and_assoc_substitution() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Slot<const ROOT: u256 = _> {}
struct Direct<const ROOT: u256> { raw: u256 }
impl<const ROOT: u256> EffectHandle for Direct<ROOT> {
    type Target = Slot<{ ROOT + 1 }>
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

trait HasTarget { type Target }
struct Source<const ROOT: u256> {}
impl<const ROOT: u256> HasTarget for Source<ROOT> {
    type Target = Slot<{ ROOT + 1 }>
}
struct Indirect<T> { raw: u256 }
impl<T> EffectHandle for Indirect<T> where T: HasTarget {
    type Target = T::Target
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

contract C {
    mut direct: Direct<0>,
    mut indirect: Indirect<Source<0>>,
    mut first: Slot,
    mut second: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(field(&db, contract, "direct").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "indirect").concrete_occurrences.len(),
        1
    );
    assert_eq!(layout.explicit_reservations.len(), 1);
    assert_eq!(layout.explicit_reservations[0].occurrences.len(), 2);
    assert_eq!(
        field(&db, contract, "first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn nested_provider_targets_are_first_class_graph_edges() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Root<const ROOT: u256 = _> {}
struct Handle<T> { raw: u256 }
impl<T> EffectHandle for Handle<T> {
    type Target = T
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

struct Holder { value: Handle<Root> }
type Alias = Handle<Root>
struct AliasHolder { value: Alias }
enum Choice {
    Handle(Handle<Root>),
    Direct(Root),
}

contract C {
    mut holder: Holder,
    mut tuple: (u256, Handle<Root>),
    mut choice: Choice,
    mut array: [Handle<Root>; 2],
    mut holder_array: [Holder; 2],
    mut alias: AliasHolder,
    mut nested: Handle<Handle<Root>>,
    mut nested_array: [Handle<Handle<Root>>; 2],
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    for (name, inline_span, slot_count) in [
        ("holder", 1, 2),
        ("tuple", 2, 3),
        ("choice", 2, 3),
        ("array", 2, 4),
        ("holder_array", 2, 4),
        ("alias", 1, 2),
        ("nested", 1, 2),
        ("nested_array", 2, 4),
    ] {
        let field = field(&db, contract, name);
        assert_eq!(field.inline_span, inline_span, "{name}");
        assert_eq!(field.slot_count, slot_count, "{name}");
    }

    for name in ["holder", "tuple", "alias"] {
        let field = field(&db, contract, name);
        assert_eq!(field.cells.len(), 1, "{name}");
        assert!(
            field.occurrences[0]
                .place
                .steps
                .contains(&PlaceStep::ProviderTarget)
        );
    }
    let array = field(&db, contract, "array");
    assert_eq!(array.families.len(), 1);
    assert_eq!(array.families[0].dimensions.len(), 1);
    assert_eq!(array.families[0].dimensions[0].len, 2);
    assert!(
        array.occurrences[0]
            .place
            .steps
            .contains(&PlaceStep::ProviderTarget)
    );
    let holder_array = field(&db, contract, "holder_array");
    let [family] = holder_array.families.as_slice() else {
        panic!("nested provider array should have one root family")
    };
    let assigned = holder_array
        .root_value_for_visible_projections(
            LayoutViewKind::Target,
            family.lane,
            TyId::u256(&db),
            &[
                LayoutProjection::Index(None),
                LayoutProjection::Field(0),
                LayoutProjection::ConstParam(0),
            ],
        )
        .unwrap();
    let AssignedRootValue::Indexed { dimensions, .. } = assigned else {
        panic!("dynamic nested-provider projection should retain its root family")
    };
    assert_eq!(
        dimensions
            .iter()
            .map(|dimension| dimension.len)
            .collect::<Vec<_>>(),
        [2]
    );
    let assigned = holder_array
        .root_value_for_visible_projections(
            LayoutViewKind::Target,
            family.lane,
            TyId::u256(&db),
            &[
                LayoutProjection::Index(Some(1)),
                LayoutProjection::Field(0),
                LayoutProjection::ConstParam(0),
            ],
        )
        .unwrap();
    assert_eq!(
        assigned,
        AssignedRootValue::Literal {
            space: family.space,
            slot: family.slot_for_indices(&[1]).unwrap(),
            ty: TyId::u256(&db),
        }
    );
    let choice = field(&db, contract, "choice");
    assert_eq!(choice.cells.len(), 2);
    assert_eq!(choice.overlay_groups.len(), 1);
    assert!(
        choice
            .occurrences
            .iter()
            .any(|occurrence| occurrence.place.steps.contains(&PlaceStep::ProviderTarget))
    );
    let nested = field(&db, contract, "nested");
    assert_eq!(nested.cells.len(), 1);
    assert_eq!(
        nested.occurrences[0]
            .place
            .steps
            .iter()
            .filter(|step| **step == PlaceStep::ProviderTarget)
            .count(),
        2
    );
    let nested_array = field(&db, contract, "nested_array");
    let [nested_family] = nested_array.families.as_slice() else {
        panic!("doubly nested provider array should have one root family")
    };
    assert_eq!(
        nested_array.occurrences[0]
            .place
            .steps
            .iter()
            .filter(|step| **step == PlaceStep::ProviderTarget)
            .count(),
        2
    );
    assert!(matches!(
        nested_array.root_value_for_visible_projections(
            LayoutViewKind::Target,
            nested_family.lane,
            TyId::u256(&db),
            &[
                LayoutProjection::Index(None),
                LayoutProjection::ConstParam(0),
            ],
        ),
        Ok(AssignedRootValue::Indexed { .. })
    ));
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn nested_provider_targets_use_their_own_space_and_reservations() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Root<const ROOT: u256 = _> {}
struct TransientHandle<T> { raw: u256 }
impl<T> EffectHandle for TransientHandle<T> {
    type Target = T
    const SPACE: AddressSpace = AddressSpace::TransientStorage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}
struct Holder<T> { value: T }

contract C {
    mut explicit: Holder<TransientHandle<Root<1>>>,
    mut inferred: Holder<TransientHandle<Root>>,
    mut top: TransientHandle<Root>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();
    let explicit = field(&db, contract, "explicit");
    let inferred = field(&db, contract, "inferred");
    let top = field(&db, contract, "top");

    assert_eq!(explicit.inline_span, 1);
    assert_eq!(explicit.slot_count, 1);
    assert_eq!(explicit.concrete_occurrences.len(), 1);
    assert_eq!(
        explicit.concrete_occurrences[0].space,
        ProviderAddressSpace::Transient
    );
    assert_eq!(inferred.inline_span, 1);
    assert_eq!(inferred.slot_count, 1);
    assert_eq!(
        inferred.cells[0].allocation.unwrap().space,
        ProviderAddressSpace::Transient
    );
    assert_eq!(inferred.cells[0].allocation.unwrap().slot, 0);
    assert_eq!(top.address_space, ProviderAddressSpace::Transient);
    assert_eq!(top.cells[0].allocation.unwrap().slot, 2);
    assert_eq!(layout.explicit_reservations.len(), 1);
    assert_eq!(
        layout.explicit_reservations[0].space,
        ProviderAddressSpace::Transient
    );
    assert_eq!(
        layout.explicit_reservations[0].value.data(&db).to_string(),
        "1"
    );
    assert_eq!(
        layout
            .high_water_by_address_space
            .get(&ProviderAddressSpace::Storage),
        Some(&2)
    );
    assert_eq!(
        layout
            .high_water_by_address_space
            .get(&ProviderAddressSpace::Transient),
        Some(&3)
    );
    validate_allocated_contract_layout(&db, layout).unwrap();

    let mut invalid = layout.clone();
    let invalid_field = invalid
        .fields
        .get_mut(&IdentId::new(&db, "inferred".to_string()))
        .unwrap();
    invalid_field.occurrences[0].space = ProviderAddressSpace::Storage;
    invalid_field.cells[0].space = ProviderAddressSpace::Storage;
    invalid_field.cells[0].allocation.as_mut().unwrap().space = ProviderAddressSpace::Storage;
    assert!(matches!(
        validate_allocated_contract_layout(&db, &invalid),
        Err(LayoutInvariantError::InvalidOccurrenceGraph { .. })
    ));
}

#[test]
fn unreachable_nested_provider_types_do_not_reserve_roots() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Root<const ROOT: u256 = _> {}
struct Handle<T> { raw: u256 }
impl<T> EffectHandle for Handle<T> {
    type Target = T
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}
struct Phantom<T> { value: u256 }

contract C {
    mut phantom: Phantom<Handle<Root>>,
    mut zero: [Handle<Root>; 0],
    mut live: Root,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert!(field(&db, contract, "phantom").occurrences.is_empty());
    assert!(field(&db, contract, "zero").occurrences.is_empty());
    assert_eq!(
        field(&db, contract, "live").cells[0]
            .allocation
            .unwrap()
            .slot,
        1
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn recursive_provider_target_edges_reach_a_finite_fixed_point() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Root<const ROOT: u256 = _> {}
struct Recursive { raw: u256 }
impl EffectHandle for Recursive {
    type Target = (Recursive, Root)
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}
struct Holder { value: Recursive }

contract C {
    mut holder: Holder,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();
    let holder = field(&db, contract, "holder");

    assert_eq!(holder.inline_span, 1);
    assert_eq!(holder.slot_count, 2);
    assert_eq!(holder.cells.len(), 1);
    assert_eq!(
        holder.occurrences[0]
            .place
            .steps
            .iter()
            .filter(|step| **step == PlaceStep::ProviderTarget)
            .count(),
        1
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_alias_roots_survive_alias_erasure() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf<const ROOT: u256> {}
type Rooted<const ROOT: u256 = _> = Leaf<ROOT>

contract C {
    x: Rooted<1>,
    y: Rooted,
    z: Rooted,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(field(&db, contract, "x").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "y").cells[0].allocation.unwrap().slot,
        0
    );
    assert_eq!(
        field(&db, contract, "z").cells[0].allocation.unwrap().slot,
        2
    );
    assert_eq!(layout.explicit_reservations.len(), 1);
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_roots_survive_nested_fixed_aliases() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf<const ROOT: u256> {}
type Rooted<const ROOT: u256 = _> = Leaf<ROOT>
type Fixed = Rooted<1>
struct Slot<const ROOT: u256 = _> {}

contract C {
    mut fixed: Fixed,
    mut first: Slot,
    mut second: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(field(&db, contract, "fixed").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_alias_roots_preserve_every_concrete_owner_space() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, StaticSlot}

struct Plain<const ROOT: u256> {}
struct Routed<const SPACE: AddressSpace, const ROOT: u256> {}
impl<const SPACE: AddressSpace, const ROOT: u256> StaticSlot for Routed<SPACE, ROOT> {
    const SPACE: AddressSpace = SPACE
}
type Both<const ROOT: u256 = _> = (
    Plain<ROOT>,
    Routed<AddressSpace::TransientStorage, ROOT>,
)
struct StorageSlot<const ROOT: u256 = _> {}
struct TransientSlot<const ROOT: u256 = _> {}
impl<const ROOT: u256> StaticSlot for TransientSlot<ROOT> {
    const SPACE: AddressSpace = AddressSpace::TransientStorage
}

contract C {
    mut explicit: Both<1>,
    mut storage_first: StorageSlot,
    mut storage_second: StorageSlot,
    mut transient_first: TransientSlot,
    mut transient_second: TransientSlot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(layout.explicit_reservations.len(), 2);
    assert_eq!(
        field(&db, contract, "storage_first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "storage_second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    assert_eq!(
        field(&db, contract, "transient_first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "transient_second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn provider_concrete_roots_retain_every_nested_owner_space() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle, StaticSlot}

struct Plain<const ROOT: u256> {}
struct Routed<const ROOT: u256> {}
impl<const ROOT: u256> StaticSlot for Routed<ROOT> {
    const SPACE: AddressSpace = AddressSpace::TransientStorage
}

struct Wrapper<const ROOT: u256 = _> { raw: u256 }
impl<const ROOT: u256> EffectHandle for Wrapper<ROOT> {
    type Target = (Plain<ROOT>, Routed<ROOT>)
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

struct StorageSlot<const ROOT: u256 = _> {}
struct TransientSlot<const ROOT: u256 = _> {}
impl<const ROOT: u256> StaticSlot for TransientSlot<ROOT> {
    const SPACE: AddressSpace = AddressSpace::TransientStorage
}

contract C {
    mut explicit: Wrapper<1>,
    mut storage_first: StorageSlot,
    mut storage_second: StorageSlot,
    mut transient_first: TransientSlot,
    mut transient_second: TransientSlot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();
    let explicit = field(&db, contract, "explicit");

    assert_eq!(explicit.concrete_occurrences.len(), 3);
    assert_eq!(layout.explicit_reservations.len(), 2);
    for (name, expected_space, expected_slot) in [
        ("storage_first", ProviderAddressSpace::Storage, 0),
        ("storage_second", ProviderAddressSpace::Storage, 2),
        ("transient_first", ProviderAddressSpace::Transient, 0),
        ("transient_second", ProviderAddressSpace::Transient, 2),
    ] {
        let allocation = field(&db, contract, name).cells[0].allocation.unwrap();
        assert_eq!(allocation.space, expected_space);
        assert_eq!(allocation.slot, expected_slot);
    }
    assert_eq!(
        explicit
            .concrete_occurrences
            .iter()
            .map(|occurrence| occurrence.owner.pretty_print(&db).to_string())
            .collect::<Vec<_>>(),
        ["Plain<1>", "Routed<1>", "Wrapper<1>"]
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn concrete_alias_roots_follow_physical_reachability() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf<const ROOT: u256> {}
type Rooted<const ROOT: u256 = _> = Leaf<ROOT>
struct Phantom<T> { value: u256 }
struct Slot<const ROOT: u256 = _> {}

contract C {
    mut zero: [Rooted<1>; 0],
    mut phantom: Phantom<Rooted<2>>,
    mut live: Rooted<3>,
    mut first: Slot,
    mut second: Slot,
    mut third: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert!(field(&db, contract, "zero").concrete_occurrences.is_empty());
    assert!(
        field(&db, contract, "phantom")
            .concrete_occurrences
            .is_empty()
    );
    assert_eq!(field(&db, contract, "live").concrete_occurrences.len(), 1);
    assert_eq!(layout.explicit_reservations.len(), 1);
    assert_eq!(
        layout.explicit_reservations[0].value.data(&db).to_string(),
        "3"
    );
    for (name, slot) in [("first", 1), ("second", 2), ("third", 4)] {
        assert_eq!(
            field(&db, contract, name).cells[0].allocation.unwrap().slot,
            slot
        );
    }
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_alias_roots_survive_adt_field_instantiation() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf<const ROOT: u256> {}
type Rooted<const ROOT: u256 = _> = Leaf<ROOT>
struct Holder<const ROOT: u256> { value: Rooted<ROOT> }

contract C {
    x: Holder<1>,
    y: Rooted,
    z: Rooted,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(field(&db, contract, "x").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "y").cells[0].allocation.unwrap().slot,
        0
    );
    assert_eq!(
        field(&db, contract, "z").cells[0].allocation.unwrap().slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_alias_roots_survive_provider_target_normalization() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Leaf<const ROOT: u256> {}
type Rooted<const ROOT: u256 = _> = Leaf<ROOT>
struct Wrapper<const ROOT: u256> {}

impl<const ROOT: u256> EffectHandle for Wrapper<ROOT> {
    type Target = Rooted<ROOT>
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self {} }
    fn raw(self) -> u256 { 0 }
}

contract C {
    mut x: Wrapper<1>,
    mut y: Rooted,
    mut z: Rooted,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(field(&db, contract, "x").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "y").cells[0].allocation.unwrap().slot,
        0
    );
    assert_eq!(
        field(&db, contract, "z").cells[0].allocation.unwrap().slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_provider_roots_preserve_declared_and_target_uses() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Leaf<const ROOT: u256> {}
struct Wrapper<const ROOT: u256 = _> {}
struct Slot<const ROOT: u256 = _> {}

impl<const ROOT: u256> EffectHandle for Wrapper<ROOT> {
    type Target = Leaf<ROOT>
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self {} }
    fn raw(self) -> u256 { 0 }
}

contract C {
    mut explicit: Wrapper<1>,
    mut first: Slot,
    mut second: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(
        field(&db, contract, "explicit").concrete_occurrences.len(),
        2
    );
    assert_eq!(layout.explicit_reservations.len(), 1);
    assert_eq!(layout.explicit_reservations[0].occurrences.len(), 2);
    assert_eq!(
        field(&db, contract, "first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_roots_survive_nested_associated_type_normalization() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

trait HasTarget { type Target }
struct Leaf<const ROOT: u256> {}
type Rooted<const ROOT: u256 = _> = Leaf<ROOT>
struct Source {}
struct Wrapper<T> {}

impl HasTarget for Source { type Target = Rooted<1> }
impl<T> EffectHandle for Wrapper<T>
    where T: HasTarget
{
    type Target = T::Target
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self {} }
    fn raw(self) -> u256 { 0 }
}

contract C {
    mut x: Wrapper<Source>,
    mut y: Rooted,
    mut z: Rooted,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(field(&db, contract, "x").concrete_occurrences.len(), 1);
    assert_eq!(
        field(&db, contract, "y").cells[0].allocation.unwrap().slot,
        0
    );
    assert_eq!(
        field(&db, contract, "z").cells[0].allocation.unwrap().slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_roots_survive_inherited_associated_type_defaults() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Leaf<const ROOT: u256> {}
type Rooted<const ROOT: u256 = _> = Leaf<ROOT>
trait HasTarget { type Target = Rooted<1> }
struct Source {}
struct Wrapper<T> {}

impl HasTarget for Source {}
impl<T> EffectHandle for Wrapper<T>
    where T: HasTarget
{
    type Target = T::Target
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self {} }
    fn raw(self) -> u256 { 0 }
}

contract C {
    mut explicit: Wrapper<Source>,
    mut first: Rooted,
    mut second: Rooted,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(
        field(&db, contract, "explicit").concrete_occurrences.len(),
        1
    );
    assert_eq!(
        field(&db, contract, "first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn later_explicit_roots_reserve_before_earlier_inference() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}

contract C {
    mut first: Slot,
    mut reserved: Slot<1>,
    mut second: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");

    assert_eq!(
        field(&db, contract, "first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();
}

#[test]
fn explicit_roots_displace_inline_fields_and_whole_field_blocks() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Pair<T> { left: T, right: T }

contract C {
    mut inline: u256,
    mut reserved: Slot<0>,
    mut pair: Pair<Slot>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let inline = field(&db, contract, "inline");
    let pair = field(&db, contract, "pair");

    assert_eq!(inline.slot_offset, 1);
    assert_eq!(inline.slot_count, 1);
    assert_eq!(pair.slot_offset, 2);
    assert_eq!(pair.slot_count, 2);
    assert_eq!(pair.cells[0].allocation.unwrap().slot, 2);
    assert_eq!(pair.cells[1].allocation.unwrap().slot, 3);
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();
}

#[test]
fn duplicate_explicit_roots_share_one_sparse_reservation() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}

contract C {
    mut left: Slot<1>,
    mut right: Slot<1>,
    mut first: Slot,
    mut second: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(layout.explicit_reservations.len(), 1);
    assert_eq!(layout.explicit_reservations[0].occurrences.len(), 2);
    assert_eq!(
        field(&db, contract, "first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn indexed_families_move_as_one_contiguous_region() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}

contract C {
    mut reserved: Slot<1>,
    mut values: [Slot; 3],
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let values = field(&db, contract, "values");

    assert_eq!(values.slot_offset, 2);
    assert_eq!(values.slot_count, 3);
    assert_eq!(values.families[0].allocation.unwrap().slot, 2);
    assert_eq!(values.families[0].extent, 3);
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();
}

#[test]
fn explicit_reservations_are_sparse_and_address_space_local() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, StaticSlot}

struct Slot<const ROOT: u256 = _> {}
struct ParamSlot<const SPACE: AddressSpace, const ROOT: u256 = _> {}
impl<const SPACE: AddressSpace, const ROOT: u256> StaticSlot for ParamSlot<SPACE, ROOT> {
    const SPACE: AddressSpace = SPACE
}

contract C {
    mut huge: Slot<0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff>,
    mut transient_reserved: ParamSlot<AddressSpace::TransientStorage, 1>,
    mut storage: Slot,
    mut transient_first: ParamSlot<AddressSpace::TransientStorage>,
    mut transient_second: ParamSlot<AddressSpace::TransientStorage>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert_eq!(
        field(&db, contract, "storage").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "transient_first").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    assert_eq!(
        field(&db, contract, "transient_second").cells[0]
            .allocation
            .unwrap()
            .slot,
        2
    );
    assert_eq!(
        layout.high_water_by_address_space[&ProviderAddressSpace::Storage],
        1
    );
    assert_eq!(
        layout.high_water_by_address_space[&ProviderAddressSpace::Transient],
        3
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn explicit_arrays_share_while_enum_variants_reserve_every_value() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
enum Choice { A(Slot<1>), B(Slot<4>) }

contract C {
    mut repeated: [Slot<1>; 3],
    mut choice: Choice,
    mut inferred: [Slot; 2],
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();
    let repeated = field(&db, contract, "repeated");
    let inferred = field(&db, contract, "inferred");

    assert_eq!(repeated.concrete_occurrences.len(), 1);
    assert_eq!(layout.explicit_reservations.len(), 2);
    assert_eq!(inferred.families[0].allocation.unwrap().slot, 2);
    assert_eq!(inferred.families[0].extent, 2);
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn layout_shape_keys_preserve_root_equality_partitions() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct At<const ROOT: u256> {}
type Shared<const ROOT: u256 = _> = (At<ROOT>, At<ROOT>)

contract C {
    mut shared: Shared,
    mut distinct: (Slot, Slot),
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let shared = field(&db, contract, "shared");
    let distinct = field(&db, contract, "distinct");

    assert_ne!(shared.target.shape_key(&db), distinct.target.shape_key(&db));
    assert_eq!(shared.cells.len(), 1);
    assert_eq!(distinct.cells.len(), 2);
}

#[test]
fn const_fanout_shares_while_type_fanout_replicates() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256> {}
struct DefaultSlot<const ROOT: u256 = _> {}
struct Two<const ROOT: u256 = _> { left: Slot<ROOT>, right: Slot<ROOT> }
struct Pair<T> { left: T, right: T }

contract C {
    mut shared: Two,
    mut split: Pair<DefaultSlot>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let shared = field(&db, contract, "shared");
    let split = field(&db, contract, "split");

    assert_eq!(shared.cells.len(), 1);
    assert_eq!(shared.cells[0].occurrences.len(), 2);
    assert_eq!(split.cells.len(), 2);
    assert_eq!(shared.slot_count, 1);
    assert_eq!(split.slot_count, 2);
    assert!(shared.target_concrete_ty(&db, &Default::default()).is_ok());
}

#[test]
fn aliases_use_the_same_landing_rules_as_adts() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct At<const ROOT: u256> {}
struct Pair<T> { left: T, right: T }
type TwiceAt<const ROOT: u256 = _> = (At<ROOT>, At<ROOT>)
type AliasSlot = Slot

contract C {
    mut shared: TwiceAt,
    mut split: Pair<AliasSlot>,
    mut first: AliasSlot,
    mut second: AliasSlot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");

    assert_eq!(field(&db, contract, "shared").cells.len(), 1);
    assert_eq!(field(&db, contract, "split").cells.len(), 2);
    assert_ne!(
        field(&db, contract, "first").cells[0].root,
        field(&db, contract, "second").cells[0].root
    );
}

#[test]
fn provider_target_fanout_is_an_explicit_multi_leaf_binding() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Slot<const ROOT: u256 = _> {}
struct Wrapper<T> { raw: u256 }

impl<T> EffectHandle for Wrapper<T> {
    type Target = (T, T)
    const SPACE: AddressSpace = AddressSpace::Storage

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

contract C { mut value: Wrapper<Slot> }
"#,
    );
    let layout = field(&db, find_contract(&db, top_mod, "C"), "value");
    assert!(layout.is_provider);
    assert_eq!(layout.cells.len(), 2);
    assert_eq!(layout.slot_count, 2);

    let binding = layout
        .declared
        .bindings
        .values()
        .next()
        .expect("declared root must be classified");
    let LayoutBinding::Bound(leaves) = binding else {
        panic!("declared provider root unexpectedly non-physical");
    };
    assert_eq!(leaves.len(), 2);
    assert!(
        leaves
            .iter()
            .all(|leaf| matches!(leaf.target, LayoutBindingTarget::Scalar(_)))
    );
    assert!(
        layout
            .declared_concrete_ty(&db, &Default::default())
            .is_err()
    );
}

#[test]
fn fixed_arrays_use_symbolic_families_and_checked_row_major_projection() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Pair<T> { left: T, right: T }

contract C {
    mut zero: [Slot; 0],
    mut one: [Slot; 1],
    mut three: [Slot; 3],
    mut pair: [Pair<Slot>; 3],
    mut nested: [[Slot; 2]; 3],
    mut concrete: [Slot<7>; 3],
    mut huge: [Slot; 1000000],
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");

    assert!(field(&db, contract, "zero").cells.is_empty());
    assert!(field(&db, contract, "zero").families.is_empty());
    assert_eq!(field(&db, contract, "one").cells.len(), 1);
    assert_eq!(field(&db, contract, "three").families.len(), 1);
    assert_eq!(field(&db, contract, "three").families[0].extent, 3);
    assert_eq!(field(&db, contract, "pair").families.len(), 2);
    let nested = &field(&db, contract, "nested").families[0];
    let allocation = nested.allocation.unwrap();
    assert_eq!(nested.extent, 6);
    assert_eq!(nested.strides, [2, 1]);
    assert_eq!(nested.slot_for_indices(&[2, 1]), Some(allocation.slot + 5));
    assert_eq!(field(&db, contract, "huge").families.len(), 1);
    assert_eq!(field(&db, contract, "huge").occurrences.len(), 1);
    assert!(field(&db, contract, "concrete").cells.is_empty());
    assert!(field(&db, contract, "concrete").families.is_empty());
}

#[test]
fn nested_array_family_extent_overflow_rejects_the_contract() {
    parse_module!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}

contract C {
    mut huge: [[Slot; 4294967296]; 4294967296],
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let result = contract.storage_layout(&db);
    let errors = result
        .field_errors(&IdentId::new(&db, "huge".to_string()))
        .expect("overflowing family extent must reject the field");

    assert!(
        errors
            .iter()
            .any(|error| matches!(error, ContractLayoutError::LayoutExtentOverflow))
    );
    assert!(result.allocated.is_none());
}

#[test]
fn cross_space_code_byte_extent_overflow_rejects_without_panicking() {
    parse_module!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, StaticSlot}

struct CodeSlot<const ROOT: u256 = _> {}
impl<const ROOT: u256> StaticSlot for CodeSlot<ROOT> {
    const SPACE: AddressSpace = AddressSpace::Code
}

contract C {
    mut huge: [CodeSlot; 576460752303423488],
}
"#,
    );
    let rendered = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));
    assert!(
        rendered.contains("contract-field layout extent overflowed"),
        "{rendered}"
    );
    let contract = find_contract(&db, top_mod, "C");
    let result = contract.storage_layout(&db);

    assert!(result.allocated.is_none());
    assert_eq!(
        result.field_errors(&IdentId::new(&db, "huge".to_string())),
        Some([ContractLayoutError::LayoutExtentOverflow].as_slice())
    );
}

#[test]
fn concrete_param_dependent_static_slot_routes_the_concrete_occurrence() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, StaticSlot}

struct ParamSlot<const SPACE: AddressSpace, const ROOT: u256 = _> {}
impl<const SPACE: AddressSpace, const ROOT: u256> StaticSlot for ParamSlot<SPACE, ROOT> {
    const SPACE: AddressSpace = SPACE
}

contract C {
    lock: ParamSlot<AddressSpace::TransientStorage>,
    mut value: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let lock = field(&db, contract, "lock");
    assert_eq!(lock.cells[0].space, ProviderAddressSpace::Transient);
    assert_eq!(lock.cells[0].allocation.unwrap().slot, 0);
    assert_eq!(lock.slot_count, 0);
    assert_eq!(field(&db, contract, "value").slot_offset, 0);
}

#[test]
fn invalid_field_prevents_every_provisional_allocation() {
    parse_module!(
        db,
        top_mod,
        r#"
struct Good<const ROOT: u256 = _> {}
struct Bad<const ROOT: u8 = _> {}

contract C {
    mut before: Good,
    mut bad: Bad,
    mut after: Good,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let result = contract.storage_layout(&db);
    assert!(result.allocated.is_none());
    assert!(matches!(
        result
            .field_errors(&IdentId::new(&db, "bad".to_string()))
            .and_then(|errors| errors.first()),
        Some(ContractLayoutError::NonSlotContractLayoutHole { .. })
    ));
    assert!(
        result
            .field(&IdentId::new(&db, "before".to_string()))
            .is_none()
    );
    assert!(
        result
            .field(&IdentId::new(&db, "after".to_string()))
            .is_none()
    );
}

#[test]
fn duplicate_contract_field_names_block_layout_without_panicking() {
    parse_module!(
        db,
        top_mod,
        r#"
contract DuplicateFields {
    mut value: u256,
    mut value: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "DuplicateFields");
    let result = contract.storage_layout(&db);

    assert!(result.allocated.is_none());
    assert_eq!(result.field_results.len(), 2);
    for index in 0..2 {
        assert!(matches!(
            result.field_errors_for_id(ContractFieldId { contract, index }),
            Some([ContractLayoutError::InvalidFieldType])
        ));
    }

    let rendered = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));
    assert!(
        rendered.contains("duplicate field name in contract `DuplicateFields`"),
        "unexpected diagnostics:\n{rendered}"
    );
}

#[test]
fn explicit_non_slot_defaults_remain_ordinary_concrete_consts() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Ordinary<const VALUE: u8 = _> {}
struct Slot<const ROOT: u256 = _> {}

contract C {
    mut ordinary: Ordinary<1>,
    mut inferred: Slot,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();

    assert!(
        field(&db, contract, "ordinary")
            .concrete_occurrences
            .is_empty()
    );
    assert!(layout.explicit_reservations.is_empty());
    assert_eq!(
        field(&db, contract, "inferred").cells[0]
            .allocation
            .unwrap()
            .slot,
        0
    );
    validate_allocated_contract_layout(&db, layout).unwrap();
}

#[test]
fn non_slot_default_holes_do_not_enter_the_layout_evidence_abi() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Ordinary<const VALUE: u8 = _> {}

fn pass<const VALUE: u8>(value: Ordinary<VALUE>) -> Ordinary<VALUE> {
    value
}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "pass") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing pass function");
    let signature = callable_layout_bundle_signature(&db, func);

    assert!(signature.inputs.is_empty());
    assert!(signature.output.schema.components.is_empty());
}

#[test]
fn root_bearing_arrays_require_a_known_length_before_allocation() {
    parse_module!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
type Slots<const LEN: usize = _> = [Slot; LEN]

contract C { mut values: Slots }
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let errors = contract
        .storage_layout(&db)
        .field_errors(&IdentId::new(&db, "values".to_string()))
        .expect("unknown root-bearing array length must reject the field");

    assert!(errors.iter().any(|error| matches!(
        error,
        ContractLayoutError::UnknownArrayLengthWithLayoutRoots { .. }
    )));
    assert!(contract.storage_layout(&db).allocated.is_none());
}

#[test]
fn one_source_can_bind_scalar_and_multiple_indexed_landings() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256> {}
struct Mixed<const ROOT: u256 = _> {
    scalar: Slot<ROOT>,
    first: [Slot<ROOT>; 3],
    second: [Slot<ROOT>; 3],
}

contract C { mut value: Mixed }
"#,
    );
    let layout = field(&db, find_contract(&db, top_mod, "C"), "value");
    assert_eq!(layout.cells.len(), 1);
    assert_eq!(layout.families.len(), 2);
    assert_eq!(layout.slot_count, 7);
    let LayoutBinding::Bound(leaves) = layout
        .target
        .bindings
        .values()
        .next()
        .expect("source root should be classified")
    else {
        panic!("source root unexpectedly non-physical");
    };
    assert_eq!(leaves.len(), 3);
    assert_eq!(
        leaves
            .iter()
            .filter(|leaf| matches!(leaf.target, LayoutBindingTarget::Scalar(_)))
            .count(),
        1
    );
    assert_eq!(
        leaves
            .iter()
            .filter(|leaf| matches!(leaf.target, LayoutBindingTarget::Indexed(_)))
            .count(),
        2
    );
}

#[test]
fn phantom_and_wrapper_only_roots_are_classified_honestly() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Slot<const ROOT: u256 = _> {}
struct Phantom<T> { raw: u256 }
struct Wrapper<const ROOT: u256 = _> { raw: u256 }

impl<const ROOT: u256> EffectHandle for Wrapper<ROOT> {
    type Target = u256
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

contract C {
    mut phantom: Phantom<Slot>,
    mut wrapper: Wrapper,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let phantom = field(&db, contract, "phantom");
    assert!(phantom.cells.is_empty());
    assert_eq!(phantom.slot_count, 1);
    assert!(matches!(
        phantom.target.bindings.values().next(),
        Some(LayoutBinding::NonPhysical)
    ));

    let wrapper = field(&db, contract, "wrapper");
    assert_eq!(wrapper.inline_span, 1);
    assert_eq!(wrapper.cells.len(), 1);
    assert_eq!(wrapper.cells[0].role, RootRole::MaterializeOnly);
    assert_eq!(wrapper.slot_count, 2);
    assert_eq!(
        wrapper.cells[0].allocation.unwrap().slot,
        wrapper.slot_offset + 1
    );
}

#[test]
fn wrapper_only_array_roots_form_materialize_only_families() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Slot<const ROOT: u256> {}
struct Wrapper<const ROOT: u256 = _> {
    slots: [Slot<ROOT>; 3],
    raw: u256,
}

impl<const ROOT: u256> EffectHandle for Wrapper<ROOT> {
    type Target = u256
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self {
        Self { slots: [Slot {}, Slot {}, Slot {}], raw }
    }
    fn raw(self) -> u256 { self.raw }
}

contract C { mut value: Wrapper }
"#,
    );
    let layout = field(&db, find_contract(&db, top_mod, "C"), "value");

    assert_eq!(layout.inline_span, 1);
    assert!(layout.cells.is_empty());
    assert_eq!(layout.families.len(), 1);
    assert_eq!(layout.families[0].role, RootRole::MaterializeOnly);
    assert_eq!(layout.families[0].extent, 3);
    assert_eq!(layout.slot_count, 4);
    assert!(layout.declared.all_roots_classified(&db));
    assert!(layout.target.all_roots_classified(&db));
    assert!(layout.slot_basis.all_roots_classified(&db));
}

#[test]
fn wrapper_only_roots_advance_only_their_concrete_owner_space() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle, StaticSlot}

struct Wrapper<const ROOT: u256 = _> { raw: u256 }

impl<const ROOT: u256> EffectHandle for Wrapper<ROOT> {
    type Target = u256
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

impl<const ROOT: u256> StaticSlot for Wrapper<ROOT> {
    const SPACE: AddressSpace = AddressSpace::TransientStorage
}

contract C {
    mut before: u256,
    mut wrapper: Wrapper,
    mut after: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let wrapper = field(&db, contract, "wrapper");

    assert_eq!(wrapper.address_space, ProviderAddressSpace::Storage);
    assert_eq!(wrapper.inline_span, 1);
    assert_eq!(wrapper.slot_offset, 1);
    assert_eq!(wrapper.slot_count, 1);
    assert_eq!(wrapper.cells.len(), 1);
    assert_eq!(wrapper.cells[0].role, RootRole::MaterializeOnly);
    assert_eq!(wrapper.cells[0].space, ProviderAddressSpace::Transient);
    assert_eq!(wrapper.cells[0].allocation.unwrap().slot, 0);
    assert_eq!(field(&db, contract, "after").slot_offset, 2);
    assert_eq!(
        contract
            .storage_layout(&db)
            .allocated
            .as_ref()
            .unwrap()
            .high_water_by_address_space
            .get(&ProviderAddressSpace::Transient),
        Some(&1)
    );
}

#[test]
fn provider_normalization_lands_new_associated_type_roots_per_field() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

trait HasTarget { type Target }
struct Rooted<const ROOT: u256 = _> {}
struct Wrapper<T> { raw: u256 }

impl HasTarget for u256 { type Target = Rooted }

impl<T> EffectHandle for Wrapper<T>
    where T: HasTarget
{
    type Target = T::Target
    const SPACE: AddressSpace = AddressSpace::Storage
    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

contract C {
    mut first: Wrapper<u256>,
    mut second: Wrapper<u256>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let first = field(&db, contract, "first");
    let second = field(&db, contract, "second");

    assert_eq!(first.cells.len(), 1);
    assert_eq!(second.cells.len(), 1);
    assert_ne!(first.cells[0].root, second.cells[0].root);
}

#[test]
fn enum_overlay_groups_preserve_identity_and_reserve_max_family_extent() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
enum Choice {
    Two([Slot; 2]),
    Three([Slot; 3]),
}

contract C {
    mut choice: Choice,
    mut after: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let choice = field(&db, contract, "choice");
    assert_eq!(choice.inline_span, 1);
    assert_eq!(choice.families.len(), 2);
    assert_ne!(choice.families[0].lane, choice.families[1].lane);
    assert_eq!(choice.overlay_groups.len(), 1);
    assert_eq!(choice.overlay_groups[0].reserved_extent, 3);
    assert_eq!(
        choice.families[0].allocation.unwrap().slot,
        choice.families[1].allocation.unwrap().slot
    );
    assert_eq!(choice.slot_count, 4);
    assert_eq!(field(&db, contract, "after").slot_offset, 4);
}

#[test]
fn array_of_enums_overlays_root_families_per_element() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
enum Choice {
    Scalar(Slot),
    Family([Slot; 3]),
}
enum Reverse {
    Family([Slot; 3]),
    Scalar(Slot),
}

contract C {
    mut values: [Choice; 2],
    mut after: u256,
}
contract D {
    mut values: [Reverse; 2],
    mut after: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let values = field(&db, contract, "values");

    assert_eq!(values.inline_span, 2);
    assert_eq!(values.families.len(), 2);
    assert_eq!(values.overlay_groups.len(), 1);
    let scalar = values
        .families
        .iter()
        .find(|family| family.dimensions.len() == 1)
        .unwrap();
    let family = values
        .families
        .iter()
        .find(|family| family.dimensions.len() == 2)
        .unwrap();

    assert_eq!(
        scalar.allocation.unwrap().slot,
        family.allocation.unwrap().slot
    );
    assert_eq!(scalar.slot_for_indices(&[0]), Some(2));
    assert_eq!(scalar.slot_for_indices(&[1]), Some(5));
    assert_eq!(family.slot_for_indices(&[0, 1]), Some(3));
    assert_eq!(family.slot_for_indices(&[1, 0]), Some(5));
    assert_eq!(values.overlay_groups[0].reserved_extent, 6);
    assert_eq!(values.slot_count, 8);
    assert_eq!(field(&db, contract, "after").slot_offset, 8);
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();

    let reverse_contract = find_contract(&db, top_mod, "D");
    let reverse = field(&db, reverse_contract, "values");
    let reverse_scalar = reverse
        .families
        .iter()
        .find(|family| family.dimensions.len() == 1)
        .unwrap();
    let reverse_family = reverse
        .families
        .iter()
        .find(|family| family.dimensions.len() == 2)
        .unwrap();
    assert_eq!(reverse_scalar.strides, [3]);
    assert_eq!(reverse_family.strides, [3, 1]);
    assert_eq!(reverse_scalar.slot_for_indices(&[1]), Some(5));
    assert_eq!(reverse_family.slot_for_indices(&[0, 1]), Some(3));
    assert_eq!(field(&db, reverse_contract, "after").slot_offset, 8);

    let mut invalid = contract
        .storage_layout(&db)
        .allocated
        .as_ref()
        .unwrap()
        .clone();
    let values = invalid
        .fields
        .get_mut(&IdentId::new(&db, "values".to_string()))
        .unwrap();
    values
        .families
        .iter_mut()
        .find(|family| family.dimensions.len() == 1)
        .unwrap()
        .strides[0] = 1;
    assert!(matches!(
        validate_allocated_contract_layout(&db, &invalid),
        Err(LayoutInvariantError::InvalidFamilyRegion { .. })
    ));

    let mut invalid = contract
        .storage_layout(&db)
        .allocated
        .as_ref()
        .unwrap()
        .clone();
    invalid
        .fields
        .get_mut(&IdentId::new(&db, "values".to_string()))
        .unwrap()
        .overlay_groups[0]
        .reserved_extent -= 1;
    assert!(matches!(
        validate_allocated_contract_layout(&db, &invalid),
        Err(LayoutInvariantError::InvalidOverlayGroup { .. })
    ));
}

#[test]
fn nested_array_enum_overlays_compose_inner_and_outer_strides() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
enum Inner {
    Two([Slot; 2]),
    Three([Slot; 3]),
}
enum Outer {
    Nested([Inner; 2]),
    Eight([Slot; 8]),
}

contract C {
    mut values: [Outer; 2],
    mut after: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let values = field(&db, contract, "values");
    let family = |dimensions: &[usize]| {
        values
            .families
            .iter()
            .find(|family| {
                family
                    .dimensions
                    .iter()
                    .map(|dimension| dimension.len)
                    .eq(dimensions.iter().copied())
            })
            .unwrap()
    };
    let two = family(&[2, 2, 2]);
    let three = family(&[2, 2, 3]);
    let eight = family(&[2, 8]);

    assert_eq!(values.inline_span, 6);
    assert_eq!(values.overlay_groups.len(), 2);
    assert_eq!(two.strides, [8, 3, 1]);
    assert_eq!(two.extent, 13);
    assert_eq!(three.strides, [8, 3, 1]);
    assert_eq!(three.extent, 14);
    assert_eq!(eight.strides, [8, 1]);
    assert_eq!(eight.extent, 16);
    assert_eq!(two.allocation.unwrap().slot, eight.allocation.unwrap().slot);
    assert_eq!(
        three.allocation.unwrap().slot,
        eight.allocation.unwrap().slot
    );
    let mut overlay_extents = values
        .overlay_groups
        .iter()
        .map(|group| group.reserved_extent)
        .collect::<Vec<_>>();
    overlay_extents.sort_unstable();
    assert_eq!(overlay_extents, [14, 16]);
    assert_eq!(two.slot_for_indices(&[1, 0, 0]), Some(14));
    assert_eq!(three.slot_for_indices(&[0, 1, 2]), Some(11));
    assert_eq!(eight.slot_for_indices(&[1, 0]), Some(14));
    assert_eq!(values.slot_count, 22);
    assert_eq!(field(&db, contract, "after").slot_offset, 22);
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();
}

#[test]
fn nested_enum_overlays_retain_each_explicit_enum_relation() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
enum Inner {
    Two([Slot; 2]),
    Three([Slot; 3]),
}
enum Outer {
    Nested(Inner),
    Four([Slot; 4]),
}

contract C { mut value: Outer }
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = field(&db, contract, "value");

    assert_eq!(layout.families.len(), 3);
    assert_eq!(layout.overlay_groups.len(), 2);
    let inner = layout
        .overlay_groups
        .iter()
        .find(|group| !group.enum_place.steps.is_empty())
        .expect("nested enum overlay must retain its own place");
    assert_eq!(
        inner.enum_place.steps,
        [PlaceStep::EnumVariant(0), PlaceStep::EnumPayloadField(0)]
    );
    assert_eq!(inner.members.len(), 2);
    assert_eq!(inner.reserved_extent, 3);
    let outer = layout
        .overlay_groups
        .iter()
        .find(|group| group.enum_place.steps.is_empty())
        .expect("outer enum overlay must be explicit");
    assert_eq!(outer.members.len(), 3);
    assert_eq!(outer.reserved_extent, 4);
    assert_eq!(layout.slot_count, 6);
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();
}

#[test]
fn enum_overlay_never_aliases_roots_that_coexist_in_any_variant() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
enum Choice<const SHARED: u256 = _, const OTHER: u256 = _> {
    One(Slot<SHARED>),
    Two(Slot<OTHER>, Slot<SHARED>),
}

contract C {
    mut choice: Choice,
    mut after: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let choice = field(&db, contract, "choice");
    let root = StoragePlace::root(choice.field);
    let shared_one = choice
        .root_target_for_place(
            &root
                .with_step(PlaceStep::EnumVariant(0))
                .with_step(PlaceStep::EnumPayloadField(0)),
        )
        .unwrap();
    let other = choice
        .root_target_for_place(
            &root
                .with_step(PlaceStep::EnumVariant(1))
                .with_step(PlaceStep::EnumPayloadField(0)),
        )
        .unwrap();
    let shared_two = choice
        .root_target_for_place(
            &root
                .with_step(PlaceStep::EnumVariant(1))
                .with_step(PlaceStep::EnumPayloadField(1)),
        )
        .unwrap();

    assert_eq!(shared_one, shared_two);
    assert_ne!(shared_one, other);
    let slot = |target| match target {
        LayoutBindingTarget::Scalar(cell) => choice.cells[cell.0 as usize].allocation.unwrap().slot,
        LayoutBindingTarget::Indexed(_) => panic!("enum roots should be scalar"),
    };
    assert_ne!(slot(shared_one), slot(other));
    assert!(choice.overlay_groups.is_empty());
    assert_eq!(choice.slot_count, 3);
    assert_eq!(field(&db, contract, "after").slot_offset, 3);
    validate_allocated_contract_layout(
        &db,
        contract.storage_layout(&db).allocated.as_ref().unwrap(),
    )
    .unwrap();

    let mut invalid = contract
        .storage_layout(&db)
        .allocated
        .as_ref()
        .unwrap()
        .clone();
    let choice = invalid
        .fields
        .get_mut(&IdentId::new(&db, "choice".to_string()))
        .unwrap();
    let (LayoutBindingTarget::Scalar(shared), LayoutBindingTarget::Scalar(other)) =
        (shared_one, other)
    else {
        unreachable!()
    };
    choice.cells[other.0 as usize]
        .allocation
        .as_mut()
        .unwrap()
        .slot = choice.cells[shared.0 as usize].allocation.unwrap().slot;
    choice.overlay_groups.push(EnumOverlayGroup {
        enum_place: root,
        lane: 0,
        members: vec![
            AllocationUnitId::Scalar(shared),
            AllocationUnitId::Scalar(other),
        ],
        space: ProviderAddressSpace::Storage,
        reserved_extent: 1,
    });
    assert!(matches!(
        validate_allocated_contract_layout(&db, &invalid),
        Err(LayoutInvariantError::InvalidOverlayGroup { .. })
    ));
}

#[test]
fn completed_layout_validator_rejects_denormalized_graphs() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, StaticSlot}

struct Slot<const ROOT: u256 = _> {}
struct Routed<const ROOT: u256 = _> {}
impl<const ROOT: u256> StaticSlot for Routed<ROOT> {
    const SPACE: AddressSpace = AddressSpace::TransientStorage
}
enum Choice {
    Two([Slot; 2]),
    Three([Slot; 3]),
}

contract C {
    mut scalar: Slot,
    mut family: [Slot; 2],
    mut choice: Choice,
    mut explicit: Routed<5>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db).allocated.as_ref().unwrap();
    validate_allocated_contract_layout(&db, layout).unwrap();
    let scalar = IdentId::new(&db, "scalar".to_string());
    let family = IdentId::new(&db, "family".to_string());
    let choice = IdentId::new(&db, "choice".to_string());
    let explicit = IdentId::new(&db, "explicit".to_string());

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.fields.get_mut(&scalar).unwrap().cells[0].space =
                ProviderAddressSpace::Transient;
        }),
        LayoutInvariantError::InvalidOccurrenceGraph { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.fields.get_mut(&scalar).unwrap().cells[0]
                .allocation
                .as_mut()
                .unwrap()
                .space = ProviderAddressSpace::Transient;
        }),
        LayoutInvariantError::InvalidScalarCell { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.fields.get_mut(&scalar).unwrap().cells[0].role = RootRole::MaterializeOnly;
        }),
        LayoutInvariantError::InvalidScalarCell { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.fields.get_mut(&family).unwrap().families[0].space =
                ProviderAddressSpace::Transient;
        }),
        LayoutInvariantError::InvalidOccurrenceGraph { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.fields.get_mut(&family).unwrap().families[0].id = LayoutRootFamilyId(u32::MAX);
        }),
        LayoutInvariantError::InvalidFamilyRegion { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid
                .fields
                .get_mut(&explicit)
                .unwrap()
                .concrete_occurrences[0]
                .space = ProviderAddressSpace::Storage;
        }),
        LayoutInvariantError::InvalidConcreteOccurrence { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid
                .fields
                .get_mut(&scalar)
                .unwrap()
                .root_bindings
                .clear();
        }),
        LayoutInvariantError::InvalidRootBinding { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid
                .fields
                .get_mut(&scalar)
                .unwrap()
                .declared
                .bindings
                .clear();
        }),
        LayoutInvariantError::UnclassifiedViewRoot { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.fields.get_mut(&scalar).unwrap().place_roots.clear();
        }),
        LayoutInvariantError::InvalidPlaceBinding { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.fields.get_mut(&choice).unwrap().overlay_groups[0].members[0] =
                AllocationUnitId::Indexed(LayoutRootFamilyId(u32::MAX));
        }),
        LayoutInvariantError::InvalidOverlayGroup { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            invalid.explicit_reservations[0].occurrences.clear();
        }),
        LayoutInvariantError::InvalidExplicitReservation { .. }
    ));

    assert!(matches!(
        mutated_layout_error(&db, layout, |invalid| {
            *invalid
                .high_water_by_address_space
                .get_mut(&ProviderAddressSpace::Storage)
                .unwrap() += 1;
        }),
        LayoutInvariantError::InvalidAddressSpaceHighWater { .. }
    ));
}

#[test]
fn one_root_observed_in_conflicting_spaces_rejects_the_contract() {
    parse_module!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, StaticSlot}

struct Slot<const ROOT: u256> {}
struct ParamSlot<const SPACE: AddressSpace, const ROOT: u256> {}
impl<const SPACE: AddressSpace, const ROOT: u256> StaticSlot for ParamSlot<SPACE, ROOT> {
    const SPACE: AddressSpace = SPACE
}
struct Mixed<const ROOT: u256 = _> {
    ordinary: Slot<ROOT>,
    transient: ParamSlot<AddressSpace::TransientStorage, ROOT>,
}

contract C { mut value: Mixed }
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let errors = contract
        .storage_layout(&db)
        .field_errors(&IdentId::new(&db, "value".to_string()))
        .expect("field should be rejected");
    assert!(
        matches!(
            errors.first(),
            Some(ContractLayoutError::ConflictingLayoutRootSpaces { .. })
        ),
        "unexpected layout errors: {errors:#?}"
    );
    assert!(contract.storage_layout(&db).allocated.is_none());
}

#[test]
fn contract_init_aggregate_constructor_inherits_the_assigned_field_view() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> { value: u256 }

contract C {
    rooted: Rooted,

    init() uses (mut rooted) {
        rooted = Rooted { value: 11 }
    }
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let body = contract.init(&db).unwrap().body(&db);
    let typed = &check_contract_init_body(&db, contract).1;
    let (lhs, rhs) = body
        .exprs(&db)
        .values()
        .find_map(|expr| match expr {
            Partial::Present(Expr::Assign(lhs, rhs)) => Some((*lhs, *rhs)),
            _ => None,
        })
        .expect("missing field assignment");
    let concrete = field(&db, contract, "rooted")
        .target_concrete_ty(&db, &Default::default())
        .unwrap();

    assert_eq!(typed.expr_ty(&db, lhs), concrete);
    assert_eq!(typed.expr_ty(&db, rhs), concrete);
}

#[test]
fn callable_projection_sources_retain_nested_array_dimensions() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}

fn get_slot<const ROOT: u256>(values: [[Slot<ROOT>; 3]; 2], row: usize, col: usize) {
    let value = values[row][col]
}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "get_slot") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing get_slot function");
    let expected_path = vec![
        LayoutBundlePathStep::Index,
        LayoutBundlePathStep::Index,
        LayoutBundlePathStep::ConstParam(0),
    ];
    let sources = (0..CallableDef::Func(func).params(&db).len())
        .flat_map(|param_idx| callable_input_layout_backing_sources(&db, func, param_idx))
        .collect::<Vec<_>>();
    assert_eq!(
        sources,
        [CallableInputLayoutBackingSource {
            origin: CallableInputLayoutHoleOrigin::ValueParam(0),
            projection: expected_path,
        }]
    );

    assert_eq!(
        callable_input_layout_backing_index_lengths(&db, func, &sources[0]),
        Some(vec![2, 3])
    );
    let schema = callable_input_layout_bundle_schema(
        &db,
        func,
        CallableInputLayoutHoleOrigin::ValueParam(0),
    )
    .expect("missing nested-array evidence schema");
    assert_eq!(schema.components.len(), 1);
    assert_eq!(schema.components[0].rank(), 2);
    assert_eq!(
        schema.components[0].port.value_path,
        [LayoutEvidencePathStep::Index, LayoutEvidencePathStep::Index]
    );
    assert_eq!(schema.components[0].dimensions, [2, 3]);
}

#[test]
fn callable_layout_bundle_signature_is_declared_for_inputs_and_outputs() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn identity<const ROOT: u256>(
    map: StorageMap<u256, u256, ROOT>,
) -> StorageMap<u256, u256, ROOT> {
    map
}

fn concrete(map: StorageMap<u256, u256, 7>) {}

fn concrete_array(maps: [StorageMap<u256, u256, 7>; 2]) {}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "identity") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing identity function");
    let signature = callable_layout_bundle_signature(&db, func);

    assert_eq!(signature.inputs.len(), 1);
    assert_eq!(
        signature.inputs[0].origin,
        CallableInputLayoutHoleOrigin::ValueParam(0)
    );
    assert_eq!(signature.inputs[0].interface.schema.components.len(), 1);
    assert_eq!(signature.output.schema.components.len(), 1);
    assert!(matches!(
        signature.inputs[0].interface.schema.components[0].representative,
        Some(LayoutBundleComponentKey::Param(_))
    ));
    assert_eq!(
        signature.inputs[0].interface.schema.components[0].representative,
        signature.output.schema.components[0].representative
    );
    assert_eq!(signature.inputs[0].interface.schema.components[0].rank(), 0);
    assert_eq!(signature.output.schema.components[0].rank(), 0);
    assert_eq!(signature.inputs[0].interface.runtime_descriptor_count(), 1);
    assert_eq!(signature.output.runtime_descriptor_count(), 1);
    assert_eq!(signature.output.schema.components[0].port.value_path, []);
    let mut invalid = signature.inputs[0].interface.clone();
    invalid.transport.components[0] = LayoutBundleComponentTransport::CompileTime;
    assert!(matches!(
        invalid.validate(),
        Err(LayoutBundleInterfaceError::InvalidCompileTimeComponent { .. })
    ));
    let mut invalid = signature.inputs[0].interface.clone();
    invalid.transport.components.pop();
    assert!(matches!(
        invalid.validate(),
        Err(LayoutBundleInterfaceError::ComponentCount {
            expected: 1,
            actual: 0,
        })
    ));

    let concrete = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "concrete") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing concrete function");
    let concrete_ty = *concrete.arg_tys(&db)[0].skip_binder();
    let concrete_ty = concrete_ty
        .as_capability(&db)
        .map_or(concrete_ty, |(_, inner)| inner);
    let root = *concrete_ty
        .generic_args(&db)
        .last()
        .expect("missing explicit root argument");
    let key = SemanticInstanceKey::new(
        &db,
        BodyOwner::Func(func),
        GenericSubst::new(&db, vec![root]),
        EffectProviderSubst::empty(&db),
        ImplEnv::empty(&db, func.scope()),
    );
    let specialized = key.layout_bundle_signature(&db);
    assert!(matches!(
        specialized.inputs[0].interface.schema.components[0].representative,
        Some(LayoutBundleComponentKey::Static(value)) if value == root
    ));
    assert!(matches!(
        specialized.output.schema.components[0].representative,
        Some(LayoutBundleComponentKey::Static(value)) if value == root
    ));
    assert_eq!(
        specialized.inputs[0]
            .interface
            .transport
            .component(LayoutBundleComponentId(0)),
        Some(LayoutBundleComponentTransport::Runtime)
    );
    assert_eq!(
        specialized
            .output
            .transport
            .component(LayoutBundleComponentId(0)),
        Some(LayoutBundleComponentTransport::Runtime)
    );
    assert_eq!(
        specialized.inputs[0].interface.runtime_descriptor_count(),
        1
    );
    assert_eq!(specialized.output.runtime_descriptor_count(), 1);

    let concrete_array = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "concrete_array") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing concrete_array function");
    let array_signature = callable_layout_bundle_signature(&db, concrete_array);
    assert_eq!(array_signature.inputs.len(), 1);
    assert_eq!(
        array_signature.inputs[0].interface.schema.components.len(),
        1
    );
    assert_eq!(
        array_signature.inputs[0].interface.schema.components[0].rank(),
        1
    );
    assert_eq!(
        array_signature.inputs[0]
            .interface
            .runtime_descriptor_count(),
        0
    );
    let mut invalid = array_signature.inputs[0].interface.schema.clone();
    invalid.components[0].dimensions[0] = 0;
    assert!(matches!(
        invalid.validate(),
        Err(LayoutBundleSchemaError::EmptyDimension {
            component: LayoutBundleComponentId(0),
            axis: 0,
        })
    ));
}

#[test]
fn specialized_layout_signatures_expand_opaque_type_parameters() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}
impl<const ROOT: u256> Copy for Rooted<ROOT> {}

struct Wrapper<T, const LOCK: u256 = _> {
    value: T,
    lock: Rooted<LOCK>,
}

impl<T, const LOCK: u256> Wrapper<T, LOCK> {
    fn get(self) -> T
    where
        T: Copy,
    {
        self.value
    }
}

fn call(value: Wrapper<Rooted<7>, 9>) -> Rooted<7> {
    value.get()
}
"#,
    );
    let call = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "call") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing call function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(call)),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let signature = normalized
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|statement| {
            let NSStmtKind::Assign {
                expr: NExpr::Call { callee, .. },
                ..
            } = &statement.kind
            else {
                return None;
            };
            let BodyOwner::Func(func) = callee.key.owner(&db) else {
                return None;
            };
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "get")
                .then(|| callee.key.layout_bundle_signature(&db))
        })
        .expect("call must resolve Wrapper::get");

    assert_eq!(signature.inputs.len(), 1);
    assert_eq!(signature.inputs[0].interface.schema.components.len(), 2);
    assert_eq!(signature.output.schema.components.len(), 1);
    assert_eq!(
        signature.inputs[0].interface.schema.components[0]
            .port
            .value_path,
        [LayoutEvidencePathStep::Field(0)]
    );
    assert_eq!(
        signature.inputs[0].interface.schema.components[1]
            .port
            .value_path,
        [LayoutEvidencePathStep::Field(1)]
    );
    assert_eq!(signature.output.schema.components[0].port.value_path, []);
    assert_eq!(
        signature.inputs[0].interface.runtime_components().count(),
        2
    );
    assert!(signature.output.is_runtime(LayoutBundleComponentId(0)));
}

#[test]
fn opaque_specialization_does_not_coalesce_equal_occurrence_ports() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf<const ROOT: u256 = _> {}

struct Outer<const WRAPPER: u256 = _, const TARGET: u256 = _> {
    inner: Leaf<TARGET>,
}

fn consume<T>(value: T) {}

fn call<const ROOT: u256>(value: Outer<ROOT, ROOT>) {
    consume(value: value)
}
"#,
    );
    let call = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "call") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing call function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(call)),
    );
    let normalized = normalize_semantic_body(&db, instance).expect("normalization failed");
    let signature = normalized
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|statement| {
            let NSStmtKind::Assign {
                expr: NExpr::Call { callee, .. },
                ..
            } = &statement.kind
            else {
                return None;
            };
            let BodyOwner::Func(func) = callee.key.owner(&db) else {
                return None;
            };
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "consume")
                .then(|| callee.key.layout_bundle_signature(&db))
        })
        .expect("call must resolve consume");

    let components = &signature.inputs[0].interface.schema.components;
    assert_eq!(components.len(), 2, "{components:#?}");
    assert!(
        components
            .iter()
            .any(|component| component.port.value_path.is_empty())
    );
    assert!(
        components
            .iter()
            .any(|component| { component.port.value_path == [LayoutEvidencePathStep::Field(0)] })
    );
}

#[test]
fn layout_bundle_specialization_preserves_occurrence_ports() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

enum Choice<T> {
    First(T),
    Second(T),
}

impl Choice<StorageMap<u256, u256>> {
    fn consume(self) {}
}

struct Independent<const FIRST: u256, const SECOND: u256> {
    first: StorageMap<u256, u256, FIRST>,
    second: StorageMap<u256, u256, SECOND>,
}

struct Leaf<const ROOT: u256 = _> {}

struct Outer<const WRAPPER: u256 = _, const TARGET: u256 = _> {
    inner: Leaf<TARGET>,
}

fn consume_independent<const FIRST: u256, const SECOND: u256>(
    value: Independent<FIRST, SECOND>,
) {}

fn consume_outer<const WRAPPER: u256, const TARGET: u256>(
    value: Outer<WRAPPER, TARGET>,
) {}
"#,
    );

    let consume = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Impl(impl_) => impl_.funcs(&db).find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "consume")
            }),
            _ => None,
        })
        .expect("missing consume method");
    let landing_schema = identity_semantic_instance_key(&db, BodyOwner::Func(consume))
        .layout_bundle_signature(&db)
        .inputs
        .into_iter()
        .find(|input| input.origin == CallableInputLayoutHoleOrigin::Receiver)
        .expect("missing receiver layout schema")
        .interface
        .schema;
    assert_eq!(landing_schema.components.len(), 2);
    assert_ne!(
        landing_schema.components[0].port,
        landing_schema.components[1].port
    );

    let consume_independent = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "consume_independent") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing consume_independent function");
    let params = CallableDef::Func(consume_independent).params(&db);
    assert_eq!(params.len(), 2);
    let value_schema = SemanticInstanceKey::new(
        &db,
        BodyOwner::Func(consume_independent),
        GenericSubst::new(&db, vec![params[1], params[1]]),
        EffectProviderSubst::empty(&db),
        ImplEnv::empty(&db, consume_independent.scope()),
    )
    .layout_bundle_signature(&db)
    .inputs
    .into_iter()
    .find(|input| input.origin == CallableInputLayoutHoleOrigin::ValueParam(0))
    .expect("missing value layout schema")
    .interface
    .schema;
    assert_eq!(value_schema.components.len(), 2);
    assert_eq!(
        value_schema.components[0].representative,
        value_schema.components[1].representative
    );
    assert_ne!(
        value_schema.components[0].port,
        value_schema.components[1].port
    );

    let consume_outer = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "consume_outer") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing consume_outer function");
    let params = CallableDef::Func(consume_outer).params(&db);
    let outer_schema = SemanticInstanceKey::new(
        &db,
        BodyOwner::Func(consume_outer),
        GenericSubst::new(&db, vec![params[1], params[1]]),
        EffectProviderSubst::empty(&db),
        ImplEnv::empty(&db, consume_outer.scope()),
    )
    .layout_bundle_signature(&db)
    .inputs
    .into_iter()
    .find(|input| input.origin == CallableInputLayoutHoleOrigin::ValueParam(0))
    .expect("missing outer value layout schema")
    .interface
    .schema;
    assert_eq!(outer_schema.components.len(), 2, "{outer_schema:#?}");
    assert_eq!(outer_schema.components[0].port.value_path, []);
    assert_eq!(
        outer_schema.components[1].port.value_path,
        [LayoutEvidencePathStep::Field(0)]
    );
    assert_eq!(
        outer_schema.components[0].representative,
        outer_schema.components[1].representative
    );
}

#[test]
fn callable_layout_bundle_groups_array_base_and_indexed_landing() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn read(maps: [StorageMap<u256, u256>; 2], lane: usize, key: u256) -> u256 {
    maps[lane].get(key: key)
}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing read function");
    let schema = callable_input_layout_bundle_schema(
        &db,
        func,
        CallableInputLayoutHoleOrigin::ValueParam(0),
    )
    .expect("missing array input layout bundle");
    assert_eq!(schema.components.len(), 1);
    assert!(schema.components[0].supplied_const_params.len() >= 2);
    assert_eq!(schema.components[0].rank(), 1);
    assert_eq!(schema.components[0].dimensions, [2]);
    assert_eq!(
        schema.components[0].port.value_path,
        [LayoutEvidencePathStep::Index]
    );
}

#[test]
fn zero_length_callable_arrays_have_no_layout_evidence() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}

fn ignore(values: [Slot; 0]) {}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "ignore") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing ignore function");
    let schema = callable_input_layout_bundle_schema(
        &db,
        func,
        CallableInputLayoutHoleOrigin::ValueParam(0),
    )
    .expect("missing zero-length input schema");

    assert!(schema.components.is_empty());
    assert!(
        callable_layout_bundle_signature(&db, func)
            .inputs
            .is_empty()
    );
}

#[test]
fn callable_layout_bundle_splits_sibling_field_landings() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Pair<T> {
    left: T,
    right: T,
}

impl Pair<StorageMap<u256, u256>> {
    fn read_left(self, key: u256) -> u256 {
        self.left.get(key: key)
    }
}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Impl(impl_) => impl_.funcs(&db).find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read_left")
            }),
            _ => None,
        })
        .expect("missing read_left method");
    let schema =
        callable_input_layout_bundle_schema(&db, func, CallableInputLayoutHoleOrigin::Receiver)
            .expect("missing receiver layout bundle");
    assert_eq!(schema.components.len(), 2);
    for field in [0, 1] {
        assert!(schema.components.iter().any(|component| {
            component
                .port
                .value_path
                .contains(&LayoutEvidencePathStep::Field(field))
        }));
    }
}

#[test]
fn return_provenance_retains_enum_variant_projection() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

enum Wrapped<const ROOT: u256> { Map(StorageMap<u256, u256, ROOT>) }

fn unwrap<const ROOT: u256>(
    wrapped: Wrapped<ROOT>,
) -> StorageMap<u256, u256, ROOT> {
    let Wrapped::Map(map) = wrapped
    map
}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "unwrap") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing unwrap function");
    let typed_body = &check_func_body(&db, func).1;

    assert_eq!(
        typed_body.return_provenance(&db),
        ReturnProvenance::Forwarded(vec![ReturnSource {
            result_projection: Vec::new(),
            origin: CallableInputLayoutHoleOrigin::ValueParam(0),
            projection: vec![ReturnProjectionStep::VariantField {
                variant: 0,
                field: 0,
            }],
        }])
    );
}

#[test]
fn forwarded_return_sources_retain_partial_enum_transport() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

enum MaybeMap<const ROOT: u256> {
    Some(StorageMap<u256, u256, ROOT>),
    None,
}

fn maybe_map<const ROOT: u256>(
    map: StorageMap<u256, u256, ROOT>,
    empty: bool,
) -> MaybeMap<ROOT> {
    if empty {
        return MaybeMap::None
    }
    MaybeMap::Some(map)
}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "maybe_map") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing maybe_map function");
    let typed_body = &check_func_body(&db, func).1;
    let expected = vec![
        ReturnSource {
            result_projection: Vec::new(),
            origin: CallableInputLayoutHoleOrigin::ValueParam(0),
            projection: Vec::new(),
        },
        ReturnSource {
            result_projection: vec![ReturnProjectionStep::VariantField {
                variant: 0,
                field: 0,
            }],
            origin: CallableInputLayoutHoleOrigin::ValueParam(0),
            projection: Vec::new(),
        },
    ];

    assert_eq!(
        typed_body.return_provenance(&db),
        ReturnProvenance::Forwarded(expected.clone()),
        "the empty variant still carries the callable's type-level ROOT identity",
    );
    assert_eq!(typed_body.forwarded_return_sources(&db), expected);
}

#[test]
fn forwarded_return_sources_trace_explicit_borrows() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Holder<T> {
    value: T,
}

impl<T> Holder<T> {
    fn maybe_value(mut self, empty: bool) -> Option<mut T> {
        if empty {
            return Option::None
        }
        Option::Some(mut self.value)
    }
}
"#,
    );
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "maybe_value")
        })
        .expect("missing Holder::maybe_value function");
    let typed_body = &check_func_body(&db, func).1;

    assert_eq!(typed_body.return_provenance(&db), ReturnProvenance::Fresh);
    assert_eq!(
        typed_body.forwarded_return_sources(&db),
        [ReturnSource {
            result_projection: vec![ReturnProjectionStep::VariantField {
                variant: 0,
                field: 0,
            }],
            origin: CallableInputLayoutHoleOrigin::Receiver,
            projection: vec![ReturnProjectionStep::Field(0)],
        }]
    );
}

#[test]
fn aggregate_effect_inputs_bind_each_projected_layout_root() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Store { primary: Slot, secondary: Slot }

fn use_store() uses (store: Store) {}
"#,
    );
    let func = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "use_store") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing use_store function");
    let sources = (0..CallableDef::Func(func).params(&db).len())
        .flat_map(|param_idx| {
            callable_input_layout_backing_sources(&db, func, param_idx)
                .into_iter()
                .map(move |source| (param_idx, source))
        })
        .filter(|(_, source)| source.origin == CallableInputLayoutHoleOrigin::Effect(0))
        .collect::<Vec<_>>();
    assert_eq!(
        sources.len(),
        2,
        "direct const selectors with physical descendants are aliases, not ABI sources"
    );
    let primary = sources
        .iter()
        .find(|(_, source)| {
            source.projection
                == [
                    LayoutBundlePathStep::Field(0),
                    LayoutBundlePathStep::ConstParam(0),
                ]
        })
        .expect("missing primary effect-field root source");
    let secondary = sources
        .iter()
        .find(|(_, source)| {
            source.projection
                == [
                    LayoutBundlePathStep::Field(1),
                    LayoutBundlePathStep::ConstParam(0),
                ]
        })
        .expect("missing secondary effect-field root source");

    assert_ne!(primary.0, secondary.0);
}

#[test]
fn terminal_callable_sources_ignore_shape_only_ancestors() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

enum Choice<T> { Primary(T), Secondary(T) }

impl Choice<StorageMap<u256, u256>> {
    fn set(mut self, key: u256, value: u256) {
        match self {
            Choice::Primary(mut map) => map.set(key: key, value: value),
            Choice::Secondary(mut map) => map.set(key: key, value: value),
        }
    }
}

fn get_lane(
    maps: [[StorageMap<u256, u256>; 3]; 2],
    row: usize,
    col: usize,
    key: u256,
) -> u256 {
    maps[row][col].get(key: key)
}
"#,
    );
}

#[test]
fn control_flow_selected_local_layout_backing_sources_are_accepted() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn read_direct<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let map = if lane == 0 { maps[0] } else { maps[1] }
    map.get(key: key)
}

fn read_same_landing<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let map = if lane == 0 { maps[0] } else { maps[0] }
    map.get(key: key)
}
"#,
    );
}

#[test]
fn control_flow_selected_return_layout_backing_sources_are_accepted() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

fn choose<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    lane: usize,
) -> StorageMap<u256, u256, ROOT> {
    if lane == 0 { maps[0] } else { maps[1] }
}

msg Msg {
    #[selector = 1]
    Get { lane: usize, key: u256 } -> u256,
}

pub contract C {
    mut maps: [StorageMap<u256, u256>; 2],

    recv Msg {
        Get { lane, key } -> u256 uses (maps) {
            choose(maps: maps, lane: lane).get(key: key)
        }
    }
}
"#,
    );
}

#[test]
fn constructed_aggregate_layout_backing_sources_are_selected_by_result_field() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Pair<const LEFT: u256, const RIGHT: u256> {
    left: StorageMap<u256, u256, LEFT>,
    right: StorageMap<u256, u256, RIGHT>,
}

fn make_pair<const LEFT: u256, const RIGHT: u256>(
    left: StorageMap<u256, u256, LEFT>,
    right: StorageMap<u256, u256, RIGHT>,
) -> Pair<LEFT, RIGHT> {
    Pair { left, right }
}

fn read_left<const LEFT: u256, const RIGHT: u256>(
    pair: Pair<LEFT, RIGHT>,
    key: u256,
) -> u256 {
    pair.left.get(key: key)
}

fn valid<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let pair = make_pair(left: maps[lane], right: maps[0])
    read_left(pair: pair, key: key)
}
"#,
    );
}

#[test]
fn control_flow_selected_aggregate_field_is_accepted() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Pair<const LEFT: u256, const RIGHT: u256> {
    left: StorageMap<u256, u256, LEFT>,
    right: StorageMap<u256, u256, RIGHT>,
}

fn read_left<const LEFT: u256, const RIGHT: u256>(
    pair: Pair<LEFT, RIGHT>,
    key: u256,
) -> u256 {
    pair.left.get(key: key)
}

fn ambiguous<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let left = if lane == 0 { maps[0] } else { maps[1] }
    let pair = Pair { left, right: maps[0] }
    read_left(pair: pair, key: key)
}
"#,
    );
}

#[test]
fn reassigned_layout_backing_sources_follow_the_current_value() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Pair<const LEFT: u256, const RIGHT: u256> {
    left: StorageMap<u256, u256, LEFT>,
    right: StorageMap<u256, u256, RIGHT>,
}

fn invalid<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    key: u256,
) -> u256 {
    let mut map = maps[0]
    map = maps[1]
    map.get(key: key)
}

fn valid<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    key: u256,
) -> u256 {
    let mut map = maps[0]
    map = maps[0]
    map.get(key: key)
}

fn invalid_partial<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    key: u256,
) -> u256 {
    let mut pair = Pair { left: maps[0], right: maps[0] }
    pair.left = maps[1]
    pair.left.get(key: key)
}
"#,
    );
}

#[test]
fn legacy_trailing_layout_args_are_rejected_at_declared_arity() {
    parse_module!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Wrapper { slot: Slot }

impl Wrapper {
    fn invalid(self: Wrapper<1>) {}
}
"#,
    );
    let rendered = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));

    assert!(
        rendered.contains("incorrect number of generic arguments for `Wrapper`"),
        "legacy trailing layout args must fail declared-arity checking:\n{rendered}"
    );
}

#[test]
fn trait_dispatch_transports_control_flow_selected_layout_evidence() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Reader {}

trait Read {
    fn read<const ROOT: u256>(
        self,
        map: StorageMap<u256, u256, ROOT>,
        key: u256,
    ) -> u256
}

impl Read for Reader {
    fn read<const ROOT: u256>(
        self,
        map: StorageMap<u256, u256, ROOT>,
        key: u256,
    ) -> u256 {
        map.get(key: key)
    }
}

fn invalid<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let map = if lane == 0 { maps[0] } else { maps[1] }
    Reader {}.read(map: map, key: key)
}
"#,
    );
}

#[test]
fn trait_dispatch_uses_the_selected_impl_return_provenance() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct Reader {}

trait Identity {
    fn identity<const ROOT: u256>(
        self,
        map: StorageMap<u256, u256, ROOT>,
    ) -> StorageMap<u256, u256, ROOT>
}

impl Identity for Reader {
    fn identity<const ROOT: u256>(
        self,
        map: StorageMap<u256, u256, ROOT>,
    ) -> StorageMap<u256, u256, ROOT> {
        map
    }
}

fn valid<const ROOT: u256>(
    maps: [StorageMap<u256, u256, ROOT>; 2],
    key: u256,
) -> u256 {
    Reader {}.identity(map: maps[0]).get(key: key)
}
"#,
    );
}

#[test]
fn overloaded_calls_transport_control_flow_selected_layout_evidence() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::ops::{Add, AddAssign, Neg}
use std::evm::StorageMap

struct Rooted<const ROOT: u256> {
    map: StorageMap<u256, u256, ROOT>,
}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

impl<const ROOT: u256> Add<u256> for Rooted<ROOT> {
    type Output = u256

    fn add(own self, _ key: own u256) -> u256 {
        self.map.get(key: key)
    }
}

impl<const ROOT: u256> Neg for Rooted<ROOT> {
    type Output = u256

    fn neg(own self) -> u256 {
        self.map.get(key: 0)
    }
}

impl<const ROOT: u256> AddAssign<u256> for Rooted<ROOT> {
    fn add_assign(mut self, _ key: own u256) {
        let _value = self.map.get(key: key)
    }
}

fn invalid_bin<const ROOT: u256>(
    values: [Rooted<ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let rooted = if lane == 0 { values[0] } else { values[1] }
    rooted + key
}

fn invalid_unary<const ROOT: u256>(
    values: [Rooted<ROOT>; 2],
    lane: usize,
) -> u256 {
    let rooted = if lane == 0 { values[0] } else { values[1] }
    (-rooted)
}

fn invalid_aug_assign<const ROOT: u256>(
    values: [Rooted<ROOT>; 2],
    lane: usize,
    key: u256,
) {
    let mut rooted = if lane == 0 { values[0] } else { values[1] }
    rooted += key
}
"#,
    );
}

#[test]
fn overloaded_index_return_provenance_uses_the_selected_impl() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::ops::Index
use std::evm::StorageMap

struct Rooted<const ROOT: u256> {
    map: StorageMap<u256, u256, ROOT>,
}

impl<const ROOT: u256> Copy for Rooted<ROOT> {}

impl<const ROOT: u256> Index<usize> for Rooted<ROOT> {
    type Output = StorageMap<u256, u256, ROOT>

    fn index(self, _ index: usize) -> StorageMap<u256, u256, ROOT> {
        self.map
    }
}

fn valid<const ROOT: u256>(rooted: Rooted<ROOT>, key: u256) -> u256 {
    rooted[0].get(key: key)
}

fn through_index<const ROOT: u256>(
    rooted: Rooted<ROOT>,
) -> StorageMap<u256, u256, ROOT> {
    rooted[0]
}

fn valid_helper<const ROOT: u256>(rooted: Rooted<ROOT>, key: u256) -> u256 {
    through_index(rooted: rooted).get(key: key)
}

fn invalid<const ROOT: u256>(
    values: [Rooted<ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let rooted = if lane == 0 { values[0] } else { values[1] }
    rooted[0].get(key: key)
}

fn invalid_helper<const ROOT: u256>(
    values: [Rooted<ROOT>; 2],
    lane: usize,
    key: u256,
) -> u256 {
    let rooted = if lane == 0 { values[0] } else { values[1] }
    through_index(rooted: rooted).get(key: key)
}
"#,
    );
}

#[test]
fn for_loop_calls_transport_control_flow_selected_layout_evidence() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::Seq
use std::evm::StorageMap

struct RootedSeq<const ROOT: u256> {
    map: StorageMap<u256, u256, ROOT>,
}

impl<const ROOT: u256> Copy for RootedSeq<ROOT> {}

impl<const ROOT: u256> Seq for RootedSeq<ROOT> {
    type Item = u256

    fn len(self) -> usize {
        1
    }

    fn get(self, _ index: usize) -> u256 {
        self.map.get(key: 0)
    }
}

fn invalid<const ROOT: u256>(
    values: [RootedSeq<ROOT>; 2],
    lane: usize,
) -> u256 {
    let seq = if lane == 0 { values[0] } else { values[1] }
    let mut result = 0
    for value in seq {
        result = value
    }
    result
}
"#,
    );
}

#[test]
fn fresh_returns_and_const_fanout_preserve_one_callable_root_source() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Rooted<const ROOT: u256 = _> {}

struct Two<const ROOT: u256 = _> {
    left: Rooted<ROOT>,
    right: Rooted<ROOT>,
}

struct Independent<const FIRST: u256 = _, const SECOND: u256 = _> {}

impl<const FIRST: u256, const SECOND: u256> Independent<FIRST, SECOND> {
    fn first(self) -> u256 {
        FIRST
    }

    fn second(self) -> u256 {
        SECOND
    }
}

impl<const ROOT: u256> Two<ROOT> {
    fn root(self) -> u256 {
        ROOT
    }
}

fn rebuild<const ROOT: u256>(rooted: Rooted<ROOT>) -> Rooted<ROOT> {
    Rooted {}
}

fn consume_rebuilt<const ROOT: u256>(rooted: Rooted<ROOT>) -> u256 {
    let rebuilt = rebuild(rooted: rooted)
    let two = Two { left: rebuilt, right: rebuilt }
    two.root()
}

fn consume_independent<const FIRST: u256, const SECOND: u256>(
    roots: Independent<FIRST, SECOND>,
) -> (u256, u256) {
    (roots.first(), roots.second())
}
"#,
    );
    let rebuild = top_mod
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "rebuild") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("missing rebuild function");

    assert_eq!(
        check_func_body(&db, rebuild).1.return_provenance(&db),
        ReturnProvenance::Forwarded(vec![ReturnSource {
            result_projection: Vec::new(),
            origin: CallableInputLayoutHoleOrigin::ValueParam(0),
            projection: Vec::new(),
        }])
    );
}

#[test]
fn nested_enum_families_forward_each_callable_root() {
    parse_module!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}

impl<const ROOT: u256> Slot<ROOT> {
    fn root(self) -> u256 { ROOT }
}

enum Choice {
    Small([Slot; 2]),
    Large([Slot; 3]),
}

impl Choice {
    fn root(self, lane: usize) -> u256 {
        match self {
            Choice::Small(slots) => slots[lane].root(),
            Choice::Large(slots) => slots[lane].root(),
        }
    }
}

msg Msg {
    #[selector = 1]
    Get { lane: usize } -> u256,
}

contract C {
    mut choice: Choice,

    recv Msg {
        Get { lane } -> u256 uses (choice) {
            choice.root(lane: lane)
        }
    }
}
"#,
    );
    let rendered = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));
    assert!(rendered.is_empty(), "unexpected diagnostics:\n{rendered}");
}

#[test]
fn shared_roots_forward_through_every_enum_overlay_shape() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256> {}

impl<const ROOT: u256> Slot<ROOT> {
    fn root(self) -> u256 { ROOT }
}

enum ScalarFirst<const ROOT: u256 = _> {
    Scalar(Slot<ROOT>),
    Family([Slot<ROOT>; 3]),
}

impl<const ROOT: u256> ScalarFirst<ROOT> {
    fn root(self, lane: usize) -> u256 {
        match self {
            ScalarFirst::Scalar(slot) => slot.root(),
            ScalarFirst::Family(slots) => slots[lane].root(),
        }
    }
}

enum FamilyFirst<const ROOT: u256 = _> {
    Family([Slot<ROOT>; 3]),
    Scalar(Slot<ROOT>),
}

impl<const ROOT: u256> FamilyFirst<ROOT> {
    fn root(self, lane: usize) -> u256 {
        match self {
            FamilyFirst::Family(slots) => slots[lane].root(),
            FamilyFirst::Scalar(slot) => slot.root(),
        }
    }
}

enum Unequal<const ROOT: u256 = _> {
    Two([Slot<ROOT>; 2]),
    Three([Slot<ROOT>; 3]),
}

impl<const ROOT: u256> Unequal<ROOT> {
    fn root(self, lane: usize) -> u256 {
        match self {
            Unequal::Two(slots) => slots[lane].root(),
            Unequal::Three(slots) => slots[lane].root(),
        }
    }
}

enum Inner<const ROOT: u256> {
    Scalar(Slot<ROOT>),
    Two([Slot<ROOT>; 2]),
}

impl<const ROOT: u256> Inner<ROOT> {
    fn root(self, lane: usize) -> u256 {
        match self {
            Inner::Scalar(slot) => slot.root(),
            Inner::Two(slots) => slots[lane].root(),
        }
    }
}

enum Nested<const ROOT: u256 = _> {
    Inner(Inner<ROOT>),
    Three([Slot<ROOT>; 3]),
}

impl<const ROOT: u256> Nested<ROOT> {
    fn root(self, lane: usize) -> u256 {
        match self {
            Nested::Inner(inner) => inner.root(lane: lane),
            Nested::Three(slots) => slots[lane].root(),
        }
    }
}

msg Msg {
    #[selector = 1]
    ScalarFirst { lane: usize } -> u256,
    #[selector = 2]
    FamilyFirst { lane: usize } -> u256,
    #[selector = 3]
    Unequal { lane: usize } -> u256,
    #[selector = 4]
    Nested { lane: usize } -> u256,
}

contract C {
    mut scalar_first: ScalarFirst,
    mut family_first: FamilyFirst,
    mut unequal: Unequal,
    mut nested: Nested,

    recv Msg {
        ScalarFirst { lane } -> u256 uses (scalar_first) {
            scalar_first.root(lane: lane)
        }
        FamilyFirst { lane } -> u256 uses (family_first) {
            family_first.root(lane: lane)
        }
        Unequal { lane } -> u256 uses (unequal) {
            unequal.root(lane: lane)
        }
        Nested { lane } -> u256 uses (nested) {
            nested.root(lane: lane)
        }
    }
}
"#,
    );
}

#[test]
fn coexisting_scalar_and_family_landings_remain_distinct() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256> {}

impl<const ROOT: u256> Slot<ROOT> {
    fn root(self) -> u256 { ROOT }
}

struct Both<const ROOT: u256 = _> {
    scalar: Slot<ROOT>,
    family: [Slot<ROOT>; 2],
}

impl<const ROOT: u256> Both<ROOT> {
    fn root(self, lane: usize) -> u256 {
        if lane == 0 {
            self.scalar.root()
        } else {
            self.family[lane - 1].root()
        }
    }
}

msg Msg {
    #[selector = 1]
    Root { lane: usize } -> u256,
}

contract C {
    mut both: Both,

    recv Msg {
        Root { lane } -> u256 uses (both) {
            both.root(lane: lane)
        }
    }
}
"#,
    );
}

#[test]
fn provider_target_callable_sources_exclude_declared_wrapper_roots() {
    parse_module!(
        db,
        top_mod,
        r#"
use core::effect_ref::{AddressSpace, EffectHandle}

struct Slot<const ROOT: u256 = _> {}

impl<const ROOT: u256> Slot<ROOT> {
    fn root(self) -> u256 { ROOT }
}

struct Payload {
    slots: [Slot; 2],
}

impl Payload {
    fn root(self, lane: usize) -> u256 {
        self.slots[lane].root()
    }
}

struct Wrapper<const WRAPPER_ROOT: u256 = _> {
    raw: u256,
}

impl<const WRAPPER_ROOT: u256> EffectHandle for Wrapper<WRAPPER_ROOT> {
    type Target = Payload
    const SPACE: AddressSpace = AddressSpace::Storage

    fn from_raw(_ raw: u256) -> Self { Self { raw } }
    fn raw(self) -> u256 { self.raw }
}

msg Msg {
    #[selector = 1]
    Get { lane: usize } -> u256,
}

contract C {
    mut payload: Wrapper,

    recv Msg {
        Get { lane } -> u256 uses (payload) {
            payload.root(lane: lane)
        }
    }
}
"#,
    );
    let rendered = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));
    assert!(rendered.is_empty(), "unexpected diagnostics:\n{rendered}");
}
