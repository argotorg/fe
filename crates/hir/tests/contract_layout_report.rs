#[path = "support/layout.rs"]
mod layout_test_support;

use fe_hir::{
    analysis::ty::ProviderAddressSpace,
    core::semantic::{
        ContractLayoutEntry, ContractLayoutEntryKind, ContractLayoutParameterOrigin,
        ContractLayoutValue,
    },
    test_db::{HirAnalysisTestDb, find_contract},
};
use layout_test_support::parse_ok;

fn entry<'a, 'db>(
    db: &'db HirAnalysisTestDb,
    entries: &'a [ContractLayoutEntry<'db>],
    path: &str,
) -> &'a ContractLayoutEntry<'db> {
    entries
        .iter()
        .find(|entry| entry.path.display(db) == path)
        .unwrap_or_else(|| panic!("missing layout entry `{path}`: {entries:#?}"))
}

fn scalar_value(db: &HirAnalysisTestDb, entry: &ContractLayoutEntry<'_>) -> String {
    let ContractLayoutValue::Scalar(value) = entry.value else {
        panic!("expected scalar layout value: {entry:#?}");
    };
    value.data(db).to_string()
}

#[test]
fn report_distinguishes_inline_fields_from_explicit_and_inferred_parameters() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::StorageMap

struct CounterStore {
    global: u256,
    counts: StorageMap<u256, u256>,
}
struct Slot<const ROOT: u256 = _> {}
struct UsizeSlot<const ROOT: usize = _> {}

pub contract Counter {
    mut store: CounterStore,
    mut foo: u256,
    mut baz: StorageMap<u256, u256, 0>,
    mut usize_root: UsizeSlot,
    mut fixed: [Slot<7>; 2],
}
"#,
    );
    let contract = find_contract(&db, top_mod, "Counter");
    let report = contract.layout_report(&db).unwrap();
    let paths = report
        .entries
        .iter()
        .map(|entry| entry.path.display(&db))
        .collect::<Vec<_>>();
    assert_eq!(
        paths,
        [
            "baz.SALT",
            "store.global",
            "store.counts.SALT",
            "foo",
            "usize_root.ROOT",
            "fixed[i0].ROOT",
        ]
    );

    let expected = [
        (
            "baz.SALT",
            "0",
            ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Explicit),
        ),
        ("store.global", "1", ContractLayoutEntryKind::InlineField),
        (
            "store.counts.SALT",
            "2",
            ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Inferred),
        ),
        ("foo", "3", ContractLayoutEntryKind::InlineField),
        (
            "usize_root.ROOT",
            "4",
            ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Inferred),
        ),
        (
            "fixed[i0].ROOT",
            "7",
            ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Explicit),
        ),
    ];
    for (path, value, kind) in expected {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(scalar_value(&db, entry), value, "{path}");
        assert_eq!(entry.kind, kind, "{path}");
        assert_eq!(entry.address_space, ProviderAddressSpace::Storage, "{path}");
        let ty = if path == "usize_root.ROOT" {
            "usize"
        } else {
            "u256"
        };
        assert_eq!(entry.ty.pretty_print(&db).to_string(), ty, "{path}");
    }
    assert_eq!(
        entry(&db, &report.entries, "fixed[i0].ROOT")
            .path
            .index_dimensions()
            .collect::<Vec<_>>(),
        [(0, 2)]
    );
}

#[test]
fn report_preserves_array_geometry_and_enum_overlays() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf { left: u256, right: u8 }
struct Slot<const ROOT: u256 = _> {}
enum Choice { Pair(Leaf), Unit }

contract C {
    mut values: [Leaf; 2],
    mut roots: [[Slot; 3]; 2],
    mut choice: Choice,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();

    for (path, base) in [("values[i0].left", "0"), ("values[i0].right", "1")] {
        let entry = entry(&db, &report.entries, path);
        let ContractLayoutValue::Indexed {
            base: actual,
            dimensions,
            strides,
            extent,
        } = &entry.value
        else {
            panic!("expected indexed inline entry: {entry:#?}");
        };
        assert_eq!(actual.data(&db).to_string(), base);
        assert_eq!(dimensions, &[2]);
        assert_eq!(strides, &[2]);
        assert_eq!(*extent, 3);
    }

    let roots = entry(&db, &report.entries, "roots[i0][i1].ROOT");
    let ContractLayoutValue::Indexed {
        base,
        dimensions,
        strides,
        extent,
    } = &roots.value
    else {
        panic!("expected indexed parameter entry: {roots:#?}");
    };
    assert_eq!(base.data(&db).to_string(), "4");
    assert_eq!(dimensions, &[2, 3]);
    assert_eq!(strides, &[3, 1]);
    assert_eq!(*extent, 6);
    assert_eq!(
        roots.kind,
        ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Inferred)
    );

    let tag = entry(&db, &report.entries, "choice.<tag>");
    assert_eq!(scalar_value(&db, tag), "10");
    assert_eq!(tag.kind, ContractLayoutEntryKind::EnumTag);
    assert_eq!(
        scalar_value(&db, entry(&db, &report.entries, "choice::Pair.0.left")),
        "11"
    );
    assert_eq!(
        scalar_value(&db, entry(&db, &report.entries, "choice::Pair.0.right")),
        "12"
    );
}

#[test]
fn report_labels_shared_parameters_and_mutually_exclusive_variants() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Slot<const ROOT: u256 = _> {}
struct Shared<const ROOT: u256 = _> {
    left: Slot<ROOT>,
    right: Slot<ROOT>,
}
struct Explicit<const LEFT: u256 = _, const RIGHT: u256 = _> {}
enum Choice {
    Named { value: Slot },
    Tuple(Slot),
}

contract C {
    mut shared: Shared,
    mut choice: Choice,
    mut explicit: Explicit<4, 4>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();

    for path in ["shared.left.ROOT", "shared.right.ROOT"] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(scalar_value(&db, entry), "0", "{path}");
        assert_eq!(
            entry.kind,
            ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Inferred)
        );
    }
    assert_eq!(
        scalar_value(&db, entry(&db, &report.entries, "choice.<tag>")),
        "1"
    );
    for path in ["choice::Named.value.ROOT", "choice::Tuple.0.ROOT"] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(scalar_value(&db, entry), "2", "{path}");
        assert_eq!(
            entry.kind,
            ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Inferred)
        );
    }
    for path in ["explicit.LEFT", "explicit.RIGHT"] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(scalar_value(&db, entry), "4", "{path}");
        assert_eq!(
            entry.kind,
            ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Explicit)
        );
    }
}

#[test]
fn report_keeps_address_spaces_independent() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::TStorPtr
use core::effect_ref::{AddressSpace, StaticSlot}

struct Slot<const ROOT: u256 = _> {}
struct Holder { ptr: TStorPtr<Slot> }
struct Plain<const ROOT: u256> {}
struct Routed<const SPACE: AddressSpace, const ROOT: u256> {}
impl<const SPACE: AddressSpace, const ROOT: u256> StaticSlot for Routed<SPACE, ROOT> {
    const SPACE: AddressSpace = SPACE
}
type Both<const ROOT: u256 = _> = (
    Plain<ROOT>,
    Routed<AddressSpace::TransientStorage, ROOT>,
)

contract C {
    mut stored: u256,
    mut temporary: TStorPtr<u256>,
    mut holder: Holder,
    mut explicit: Both<7>,
    immutable: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();
    for (path, space, value) in [
        ("stored", ProviderAddressSpace::Storage, "0"),
        ("temporary", ProviderAddressSpace::Transient, "0"),
        ("holder.ptr.slot", ProviderAddressSpace::Storage, "1"),
        ("holder.ptr.ROOT", ProviderAddressSpace::Transient, "1"),
        ("explicit.0.ROOT", ProviderAddressSpace::Storage, "7"),
        ("explicit.1.ROOT", ProviderAddressSpace::Transient, "7"),
        ("immutable", ProviderAddressSpace::Code, "0"),
    ] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(entry.address_space, space, "{path}");
        assert_eq!(scalar_value(&db, entry), value, "{path}");
    }
}
