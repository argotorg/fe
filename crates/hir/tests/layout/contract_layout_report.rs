use crate::layout_test_support::{parse_module, parse_ok};
use fe_hir::{
    analysis::{initialize_analysis_pass, ty::AddressSpace},
    core::semantic::{
        ContractFieldId, ContractLayoutEntry, ContractLayoutEntryKind, ContractLayoutError,
        ContractLayoutValue,
    },
    hir_def::IdentId,
    test_db::{HirAnalysisTestDb, find_contract, format_diagnostics},
};

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
fn report_preserves_inline_array_geometry_and_enum_overlays() {
    parse_ok!(
        db,
        top_mod,
        r#"
struct Leaf { left: u256, right: u8 }
enum Choice { Pair(Leaf), Unit }

contract C {
    mut values: [Leaf; 2],
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

    let tag = entry(&db, &report.entries, "choice.<tag>");
    assert_eq!(scalar_value(&db, tag), "4");
    assert_eq!(tag.kind, ContractLayoutEntryKind::EnumTag);
    assert_eq!(
        scalar_value(&db, entry(&db, &report.entries, "choice::Pair.0.left")),
        "5"
    );
    assert_eq!(
        scalar_value(&db, entry(&db, &report.entries, "choice::Pair.0.right")),
        "6"
    );
}

#[test]
fn report_numbers_storage_and_transient_fields_apart() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::TSlot

contract C {
    mut stored: u256,
    mut temporary: TSlot<u256>,
    mut later: u256,
    immutable: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();
    for (path, space, value) in [
        ("stored", AddressSpace::Storage, "0"),
        // A transient cell is numbered among transient fields, so it leaves
        // the storage numbering alone.
        ("temporary", AddressSpace::Transient, "0"),
        ("later", AddressSpace::Storage, "1"),
        ("immutable", AddressSpace::Code, "0"),
    ] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(entry.address_space, space, "{path}");
        assert_eq!(scalar_value(&db, entry), value, "{path}");
    }
}

#[test]
fn report_places_explicit_slot_fields() {
    parse_ok!(
        db,
        top_mod,
        r#"
use core::keccak
use std::evm::TSlot

struct Pair { left: u256, right: u256 }

contract C {
    mut first: u256,
    #[slot(1)]
    mut placed: u256,
    mut pair: Pair,
    #[slot(keccak(keccak("example.main") - 1) & !(0xff as u256))]
    mut main: Pair,
    #[slot(1)]
    mut flag: TSlot<bool>,
    mut after: TSlot<bool>,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let report = contract.layout_report(&db).unwrap();
    // `pair` skips slot 1, which `placed` takes. `main` is at ERC-7201's
    // slot for the namespace `example.main`. Transient slot 1 is free for
    // `flag`, and `after` takes the transient slot before it.
    for (path, space, value) in [
        ("first", AddressSpace::Storage, "0"),
        ("placed", AddressSpace::Storage, "1"),
        ("pair.left", AddressSpace::Storage, "2"),
        ("pair.right", AddressSpace::Storage, "3"),
        (
            "main.left",
            AddressSpace::Storage,
            "10958655983261152271848436692291137275443024275653522991983264966744321209600",
        ),
        (
            "main.right",
            AddressSpace::Storage,
            "10958655983261152271848436692291137275443024275653522991983264966744321209601",
        ),
        ("flag", AddressSpace::Transient, "1"),
        ("after", AddressSpace::Transient, "0"),
    ] {
        let entry = entry(&db, &report.entries, path);
        assert_eq!(entry.address_space, space, "{path}");
        assert_eq!(scalar_value(&db, entry), value, "{path}");
    }
}

#[test]
fn overlapping_explicit_slots_reject_the_later_field() {
    parse_module!(
        db,
        top_mod,
        r#"
struct Pair { left: u256, right: u256 }

contract C {
    #[slot(4)]
    mut a: Pair,
    #[slot(5)]
    mut b: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let result = contract.storage_layout(&db);
    assert!(result.allocated.is_none());
    assert_eq!(
        result.field_errors(&IdentId::new(&db, "b".to_string())),
        Some(
            [ContractLayoutError::ExplicitSlotOverlap {
                other: IdentId::new(&db, "a".to_string())
            }]
            .as_slice()
        )
    );
}

#[test]
fn a_transient_cell_takes_its_slot_in_transient_storage_too() {
    parse_module!(
        db,
        top_mod,
        r#"
use std::evm::TSlot

contract Overlapping {
    #[slot(5)]
    mut cell: TSlot<u256>,
    #[slot(5)]
    mut flag: TSlot<bool>,
}

contract Skipping {
    #[slot(0)]
    mut cell: TSlot<u256>,
    mut flag: TSlot<bool>,
}
"#,
    );
    // The cell's value lives in transient slot 5, which `flag` also names.
    let overlapping = find_contract(&db, top_mod, "Overlapping").storage_layout(&db);
    assert_eq!(
        overlapping.field_errors(&IdentId::new(&db, "flag".to_string())),
        Some(
            [ContractLayoutError::ExplicitSlotOverlap {
                other: IdentId::new(&db, "cell".to_string())
            }]
            .as_slice()
        )
    );
    // `flag` skips transient slot 0, which the cell's value takes.
    let skipping = find_contract(&db, top_mod, "Skipping").storage_layout(&db);
    let fields = &skipping
        .allocated
        .as_ref()
        .expect("contract layout should be allocated")
        .fields;
    assert_eq!(
        fields[&IdentId::new(&db, "flag".to_string())].slot_offset,
        1
    );
}

#[test]
fn a_transient_cell_takes_a_number_per_word_of_its_value() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::TSlot

struct Pair { left: u256, right: u256 }

contract C {
    mut words: TSlot<[u256; 4]>,
    mut pair: TSlot<Pair>,
    mut flag: TSlot<bool>,
    mut unit: TSlot<()>,
    mut pairs: [TSlot<Pair>; 2],
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db);
    let fields = &layout
        .allocated
        .as_ref()
        .expect("contract layout should be allocated")
        .fields;
    // A cell's value lies from the cell's number on, so it takes a number
    // per word, and at least one.
    for (name, offset, count) in [
        ("words", 0, 4),
        ("pair", 4, 2),
        ("flag", 6, 1),
        ("unit", 7, 1),
        ("pairs", 8, 4),
    ] {
        let field = &fields[&IdentId::new(&db, name.to_string())];
        assert_eq!(field.slot_offset, offset, "{name}");
        assert_eq!(field.slot_count, count, "{name}");
    }
}

#[test]
fn mutex_fields_are_placed_before_transient_cells() {
    parse_ok!(
        db,
        top_mod,
        r#"
use std::evm::{Mutex, TSlot}

contract C {
    mut t0: TSlot<bool>,
    mut t1: TSlot<bool>,
    mut m: Mutex<u256>,
    mut m2: Mutex<u256>,
    mut after: TSlot<bool>,
    mut stored: u256,
}
"#,
    );
    let contract = find_contract(&db, top_mod, "C");
    let layout = contract.storage_layout(&db);
    let fields = &layout
        .allocated
        .as_ref()
        .expect("contract layout should be allocated")
        .fields;
    // A mutex mixes spaces, so it is placed among the storage fields and
    // takes its range in both; the transient cells fill the transient
    // numbers left free, and move no storage field.
    for (name, offset, count) in [
        ("t0", 4, 1),
        ("t1", 5, 1),
        ("m", 0, 2),
        ("m2", 2, 2),
        ("after", 6, 1),
        ("stored", 4, 1),
    ] {
        let field = &fields[&IdentId::new(&db, name.to_string())];
        assert_eq!(field.slot_offset, offset, "{name}");
        assert_eq!(field.slot_count, count, "{name}");
    }
}

#[test]
fn layout_extent_overflow_rejects_without_panicking() {
    parse_module!(
        db,
        top_mod,
        r#"
contract C {
    huge: [[u256; 0x80000000]; 0x10000000],
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
