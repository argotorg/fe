use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use fe_hir::test_db::{HirAnalysisTestDb, find_func, format_diagnostics};
use fe_hir::{
    analysis::{
        initialize_analysis_pass,
        semantic::{
            BorrowSummary, CtfeError, FieldIndex, LayoutEvidenceError, NDataProjection,
            NEffectArgValue, NExpr, NIndex, NPlace, NPlaceBase, NRootKind, NStatementKind,
            NValueDefinition, NormalizedArtifacts, ReadMode, SExpr, SStmtKind, STerminatorKind,
            SemanticAnalysisError, SemanticBodyAdmission, SemanticDiagnosticKind, SemanticInstance,
            SemanticNormalizationFailure, canonicalize_semantic_consts,
            capability::{
                external::ExternalOrigin,
                footprint::AccessExtent,
                guard::ValueOccurrence,
                handle::{AddressOccurrence, HandleAddressSpace},
                index::IndexExpr,
                path::{Projection as CapabilityProjection, RegionPath, StructuralPath},
                source::InputSource,
                value::{ValueInterner, ValueLimits},
            },
            check_semantic_borrows, check_semantic_boundaries,
            collect_semantic_borrow_diagnostic_vouchers, contract_init_assigned_fields,
            get_or_build_semantic_instance, identity_semantic_instance_key, layout_evidence_body,
            normalize_semantic_body,
            normalized::{
                HandleOrigin, NLayoutBackingSource, NormalizedBodyVerifyError, normalize_raw_body,
                verify_normalized_body,
            },
            root_semantic_instance_key, semantic_body_admission, semantic_borrow_summary,
        },
        ty::{
            ProviderAddressSpace,
            corelib::{MemoryAccessKind, resolve_lib_func_path},
            ty_check::{BodyOwner, EffectPassMode, LocalBinding},
            ty_def::{BorrowKind, TyData},
        },
    },
    hir_def::{ItemKind, Partial},
    projection::Projection,
};

fn borrow_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    )
}

fn checked_borrow_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    )
}

fn checked_trusted_borrow_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_trusted_effect_handle_module("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    )
}

fn assert_pending_validation(src: &str, name: &str) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("pending_validation.fe".into(), src);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let diagnostics = format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, module),
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
    let instance = func_instance(&db, module, name);
    for result in [
        check_semantic_borrows(&db, instance),
        check_semantic_boundaries(&db, instance),
        semantic_borrow_summary(&db, instance).map(|_| ()),
    ] {
        let Err(SemanticAnalysisError::Pending(validation)) = result else {
            panic!("expected explicit pending validation for {name}: {result:?}");
        };
        assert!(!validation.callees.is_empty());
    }
}

#[test]
fn pending_generic_validation_is_discharged_by_concrete_implementations() {
    for declaration in ["fn apply(value: mut u256)", "fn apply(value: mut u256) {}"] {
        let source = format!(
            r#"
trait Operation {{ {declaration} }}
struct Safe {{}}
impl Operation for Safe {{ fn apply(value: mut u256) {{ value += 1 }} }}
fn generic<T: Operation>(value: mut u256) {{
    T::apply(value)
    value += 1
}}
fn forward<T: Operation>(value: mut u256) {{ generic<T>(value) }}
fn recursive<T: Operation>(value: mut u256, again: bool) {{
    if again {{ recursive<T>(value, again: false) }} else {{ forward<T>(value) }}
}}
fn concrete(value: mut u256) {{ recursive<Safe>(value, again: true) }}
"#
        );
        for name in ["generic", "forward", "recursive"] {
            assert_pending_validation(&source, name);
        }
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("concrete_validation.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let concrete = func_instance(&db, module, "concrete");
        check_semantic_borrows(&db, concrete).unwrap();
        check_semantic_boundaries(&db, concrete).unwrap();
        semantic_borrow_summary(&db, concrete).unwrap().unwrap();
        // Querying concrete code must not discharge the identity template.
        assert!(matches!(
            check_semantic_borrows(&db, func_instance(&db, module, "generic")),
            Err(SemanticAnalysisError::Pending(_))
        ));
    }
}

#[test]
fn pending_generic_validation_rechecks_moves_clobbers_and_returned_borrows() {
    for (source, expected) in [
        (
            r#"
struct Item { n: u256 }
trait Operation { fn apply(pointer: *Item) }
struct Consume {}
impl Operation for Consume {
    fn apply(pointer: *Item) { let moved = *pointer }
}
fn generic<T: Operation>(pointer: *Item) -> Item { T::apply(pointer)
*pointer }
fn concrete(pointer: *Item) -> Item { generic<Consume>(pointer) }
"#,
            "move conflict",
        ),
        (
            r#"
trait Operation { fn apply(slot: *ref u256) }
struct Clobber {}
impl Operation for Clobber {
    fn apply(slot: *ref u256) { core::ptr::zero_bytes(core::ptr::byte_ptr(slot), 32) }
}
fn generic<T: Operation>(slot: *ref u256) -> ref u256 { T::apply(slot)
*slot }
fn concrete(slot: *ref u256) -> ref u256 { generic<Clobber>(slot) }
"#,
            "invalidated by a raw write",
        ),
        (
            r#"
trait Lender { fn lend(pointer: *u256) -> mut u256 }
struct Alias {}
impl Lender for Alias { fn lend(pointer: *u256) -> mut u256 { mut *pointer } }
fn generic<T: Lender>(pointer: *u256) {
    let first = T::lend(pointer)
    let second = mut *pointer
    first = 1
    second = 2
}
fn concrete(pointer: *u256) { generic<Alias>(pointer) }
"#,
            "borrow conflict",
        ),
    ] {
        let diagnostics = checked_borrow_diags(source);
        assert!(diagnostics.contains(expected), "{source}\n{diagnostics}");
        assert!(
            !diagnostics.contains("internal borrow checking error"),
            "{diagnostics}"
        );
    }
}

#[test]
fn pending_calls_preserve_implementation_independent_ownership_checks() {
    for body in [
        "take_pair(item, item)\nT::apply()",
        "T::apply()\ntake_pair(item, item)",
        "T::apply()\ntake(item)\ntake(item)",
        "T::apply()\nif flag { take(item) }\ntake(item)",
        "if flag { T::apply() } else { take_pair(item, item) }",
    ] {
        let source = format!(
            "struct Item {{ n: u256 }}\n\
             trait Operation {{ fn apply() }}\n\
             fn take(_ item: own Item) {{}}\n\
             fn take_pair(_ first: own Item, _ second: own Item) {{}}\n\
             fn bad<T: Operation>(item: own Item, flag: bool) {{ {body} }}"
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("move conflict"),
            "{source}\n{diagnostics}"
        );
    }
    let diagnostics = checked_borrow_diags(
        "struct Item { n: u256 }\n\
         trait Operation { fn apply(_ first: own Item, _ second: own Item) }\n\
         fn bad<T: Operation>(item: own Item) { T::apply(item, item) }",
    );
    assert!(diagnostics.contains("move conflict"), "{diagnostics}");
}

#[test]
fn unresolved_executable_calls_require_validation_even_without_arguments() {
    let source = "extern { fn opaque() }\nfn executable() { opaque() }";
    let diagnostics = checked_borrow_diags(source);
    assert!(
        diagnostics.contains("pending borrow validation in `fn executable`"),
        "{diagnostics}"
    );
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("opaque_validation.fe".into(), source);
    let (module, _) = db.top_mod(file);
    for name in ["opaque", "executable"] {
        let instance = func_instance(&db, module, name);
        assert!(matches!(
            check_semantic_borrows(&db, instance),
            Err(SemanticAnalysisError::Pending(_))
        ));
        assert!(matches!(
            check_semantic_boundaries(&db, instance),
            Err(SemanticAnalysisError::Pending(_))
        ));
        assert!(matches!(
            semantic_borrow_summary(&db, instance),
            Err(SemanticAnalysisError::Pending(_))
        ));
    }
}

#[test]
fn pending_calls_preserve_independent_boundary_diagnostics() {
    for (source, expected) in [
        (
            r#"
trait Operation { fn apply() }
fn bad<T: Operation>(destination: std::evm::StorPtr<*u256>, value: *u256) {
    destination.write(value)
    T::apply()
}
"#,
            "noesc violation",
        ),
        (
            r#"
trait Operation { fn apply() }
fn bad<T: Operation>(flag: bool, fallback: ref u256) -> ref u256 {
    if flag {
        let local: u256 = 0
        return ref local
    }
    T::apply()
    fallback
}
"#,
            "cannot return a borrow to local",
        ),
    ] {
        let diagnostics = checked_borrow_diags(source);
        assert!(diagnostics.contains(expected), "{source}\n{diagnostics}");
    }
}

#[test]
fn owned_effect_provider_representation_is_transferred_before_callee_requirements() {
    let diagnostics = checked_borrow_diags(include_str!(
        "../../fe/tests/fixtures/fe_test/zero_sized_capability_provider.fe"
    ));
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn operation_operands_cannot_duplicate_ownership() {
    let mut accepted = Vec::new();
    for binding in ["item", "mut item"] {
        for (result, body) in [
            ("()", "take_pair(item, item)"),
            ("Pair", "Pair { left: item, right: item }"),
            ("(Item, Item)", "(item, item)"),
            ("Either", "Either::Both(item, item)"),
            ("[Item; 2]", "[item, item]"),
        ] {
            let source = format!(
                "struct Item {{ n: u256 }}\n\
                 struct Pair {{ left: Item, right: Item }}\n\
                 enum Either {{ Both(Item, Item) }}\n\
                 fn take_pair(_ first: own Item, _ second: own Item) {{}}\n\
                 fn bad({binding}: own Item) -> {result} {{ {body} }}"
            );
            let diagnostics = checked_borrow_diags(&source);
            if !diagnostics.contains("move conflict in `fn bad`") {
                accepted.push(format!("{source}\n{diagnostics}"));
            }
        }
    }
    assert!(accepted.is_empty(), "{}", accepted.join("\n\n"));
}

#[test]
fn operation_operands_preserve_distinct_owners_fields_and_copies() {
    for binding in ["pair", "mut pair"] {
        let source = format!(
            "struct Item {{ n: u256 }}\n\
             struct Pair {{ left: Item, right: Item }}\n\
             fn split({binding}: own Pair) -> (Item, Item) {{ (pair.left, pair.right) }}\n\
             fn distinct(first: own Item, second: own Item) -> [Item; 2] {{ [first, second] }}\n\
             fn pointers(value: *Item) -> (*Item, *Item) {{ (value, value) }}\n\
             fn native(value: ref u256) -> (ref u256, ref u256) {{ (value, value) }}\n\
             fn scalar(value: u256) -> (u256, u256) {{ (value, value) }}"
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
    }
}

#[test]
fn physical_storage_intrinsics_conflict_with_native_storage_borrows() {
    for operation in ["raw.sstore(slot: 0, value: 1)", "let value = raw.sload(0)"] {
        for call in [operation, "access()", "forward()"] {
            let source = format!(
                r#"
use std::evm::RawStorage
fn access() uses (raw: mut RawStorage) {{ {operation} }}
fn forward() uses (raw: mut RawStorage) {{ access() }}
pub contract Cell {{
    mut slot: u256
    init() uses (mut slot, raw: mut RawStorage) {{
        let native = mut slot
        {call}
        native = 2
    }}
}}
fn memory() uses (raw: mut RawStorage) {{
    let mut value: u256 = 0
    let native = mut value
    {call}
    native = 2
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            assert!(
                diagnostics.contains("borrow conflict in `fn Cell::__init__`"),
                "{source}\n{diagnostics}"
            );
            assert!(
                !diagnostics.contains("borrow conflict in `fn memory`"),
                "{source}\n{diagnostics}"
            );
        }
    }
}

#[test]
fn external_call_intrinsic_summaries_include_current_state_effects() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("external_effects.fe".into(), "fn anchor() {}");
    let (module, _) = db.top_mod(file);
    let anchor = find_func(&db, module, "anchor");
    for name in ["delegatecall", "call", "create", "create2", "staticcall"] {
        let function =
            resolve_lib_func_path(&db, anchor.scope(), &format!("std::evm::ops::{name}")).unwrap();
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(function)),
        );
        // Query the trusted contract before diagnostics or a caller body runs.
        let summary = semantic_borrow_summary(&db, instance).unwrap().unwrap();
        assert!(
            summary.availability.reinitialized.is_empty(),
            "external may-writes cannot certify typed initialization: {name}"
        );
        for space in [
            ProviderAddressSpace::Storage,
            ProviderAddressSpace::Transient,
        ] {
            for access in summary.accesses.iter().filter(|access| {
                access.region.clauses().iter().any(|clause| {
                    clause.payload.root.address_space() == HandleAddressSpace::Known(space)
                })
            }) {
                assert_eq!(access.extent, AccessExtent::Unknown);
                assert!(
                    access.authorizers.is_empty(),
                    "whole-space effects carry no native authority"
                );
            }
            for kind in [MemoryAccessKind::Read, MemoryAccessKind::Write] {
                let found = summary.accesses.iter().any(|access| {
                    access.kind == kind
                        && access.extent == AccessExtent::Unknown
                        && access.region.clauses().iter().any(|clause| {
                            clause.payload.root.address_space() == HandleAddressSpace::Known(space)
                        })
                });
                assert_eq!(
                    found,
                    name != "staticcall" || kind == MemoryAccessKind::Read,
                    "{name}: {space:?} {kind:?}\n{summary:#?}"
                );
            }
        }
    }
}

#[test]
fn delegatecall_conflicts_with_native_state_loans_but_not_disjoint_memory() {
    for (method, value, writes) in [
        ("raw_delegatecall", "", true),
        ("raw_call", "value: 0,", true),
        ("raw_staticcall", "", false),
    ] {
        let operation = format!(
            r#"
let mut output = MemBuffer::empty()
let outcome = call.{method}(
    addr: Address {{ inner: 1 }}, gas: 100000, {value}
    args: MemSpan::empty(), ret: mut output,
)
"#
        );
        for slot_ty in ["u256", "TStorPtr<u256>"] {
            for kind in ["ref", "mut"] {
                for call in [
                    operation.as_str(),
                    "access()",
                    "forward()",
                    "recursive(again: true)",
                    "specialized<u256>(0)",
                ] {
                    let source = format!(
                        r#"
use core::ptr::{{MemBuffer, MemSpan}}
use std::evm::{{Address, Call}}
use std::evm::effects::TStorPtr
fn access() uses (call: mut Call) {{ {operation} }}
fn forward() uses (call: mut Call) {{ access() }}
fn recursive(again: bool) uses (call: mut Call) {{
    if again {{ recursive(again: false) }} else {{ forward() }}
}}
fn specialized<T: Copy>(_ witness: T) uses (call: mut Call) {{ forward() }}
pub contract Cell {{
    mut slot: {slot_ty}
    init() uses (mut slot, call: mut Call) {{
        let native = {kind} slot
        {call}
        let observed: u256 = native
    }}
}}
fn memory() uses (call: mut Call) {{
    let mut value: u256 = 0
    let native = {kind} value
    {call}
    let observed: u256 = native
}}
"#
                    );
                    let diagnostics = checked_borrow_diags(&source);
                    assert_eq!(
                        diagnostics.contains("borrow conflict in `fn Cell::__init__`"),
                        writes || kind == "mut",
                        "{source}\n{diagnostics}"
                    );
                    assert!(
                        !diagnostics.contains("borrow conflict in `fn memory`"),
                        "{source}\n{diagnostics}"
                    );
                    assert!(
                        !diagnostics.contains("internal borrow checking error"),
                        "{diagnostics}"
                    );
                }
            }
        }
    }
}

#[test]
fn fresh_native_slots_require_typed_initialization_before_reads() {
    let mut accepted = Vec::new();
    for kind in ["ref", "mut"] {
        for read in ["*slot", "read(slot)", "forward(slot)"] {
            let source = format!(
                r#"
fn read(_ slot: *{kind} u256) -> u256 {{ *slot }}
fn forward(_ slot: *{kind} u256) -> u256 {{ read(slot) }}
fn bad() -> u256 {{
    let slot = core::ptr::alloc<{kind} u256>()
    {read}
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            if !diagnostics.contains("cannot use a native borrow") {
                accepted.push(format!("{source}\n{diagnostics}"));
            }
            assert!(
                !diagnostics.contains("internal borrow checking error"),
                "{diagnostics}"
            );
        }
    }
    assert!(accepted.is_empty(), "{}", accepted.join("\n\n"));
}

#[test]
fn fresh_native_slots_preserve_invalid_alternatives_and_typed_restoration() {
    for kind in ["ref", "mut"] {
        for (initialization, valid) in [
            ("", false),
            ("*slot = native", true),
            ("replace(slot, native)", true),
            ("if condition { *slot = native }", false),
            (
                "if condition { *slot = native } else { *slot = native }",
                true,
            ),
            ("while condition { *slot = native }", false),
            (
                "let selected = if condition { slot } else { other }\n*selected = native",
                false,
            ),
            ("ptr::zero_bytes(ptr::byte_ptr(slot), 32)", false),
            (
                "ptr::copy_raw(ptr::byte_ptr(slot), source: ptr::byte_ptr(owner), len: 32)",
                false,
            ),
            (
                "*slot = native\nptr::zero_bytes(ptr::byte_ptr(slot), 32)",
                false,
            ),
            (
                "ptr::zero_bytes(ptr::byte_ptr(slot), 32)\nreplace(slot, native)",
                true,
            ),
        ] {
            let source = format!(
                r#"
use core::ptr
fn fresh() -> *{kind} u256 {{ ptr::alloc<{kind} u256>() }}
fn forward() -> *{kind} u256 {{ fresh() }}
fn replace(_ slot: *{kind} u256, _ native: {kind} u256) {{ *slot = native }}
fn read(_ slot: *{kind} u256) -> u256 {{ *slot }}
fn inspect(condition: bool) -> u256 {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    let native = {kind} *owner
    let slot = forward()
    let other = fresh()
    {initialization}
    read(slot)
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            if valid {
                assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
            } else {
                assert!(
                    diagnostics.contains("cannot use a native borrow"),
                    "{source}\n{diagnostics}"
                );
            }
        }
    }
}

#[test]
fn fresh_raw_slot_transport_and_initialized_helper_poststates_are_valid() {
    for kind in ["ref", "mut"] {
        let source = format!(
            r#"
use core::ptr
fn fresh() -> *{kind} u256 {{ ptr::alloc<{kind} u256>() }}
fn forward() -> *{kind} u256 {{ fresh() }}
fn raw_transport() -> *{kind} u256 {{ forward() }}
fn initialize(_ value: {kind} u256) -> *{kind} u256 {{
    let slot = forward()
    *slot = value
    slot
}}
fn initialized_forward(_ value: {kind} u256) -> *{kind} u256 {{ initialize(value) }}
fn inspect() -> u256 {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    let slot = initialized_forward({kind} *owner)
    *slot
}}
"#
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
    }
}

#[test]
fn fresh_native_transport_and_structural_leaves_require_initialization() {
    for (body, valid) in [
        (
            "let slot = ptr::alloc<ref u256>()\nlet loaded = *slot",
            false,
        ),
        ("let slot = ptr::alloc<ref u256>()\ndiscard(*slot)", false),
        ("let slot = ptr::alloc<Pair>()\nlet loaded = *slot", false),
        (
            "let slot = ptr::alloc<Pair>()\nlet loaded: u256 = (*slot).native",
            false,
        ),
        (
            "let slot = ptr::alloc<Pair>()\n*slot = Pair { native: ref *owner, other: 0 }\ndiscard((*slot).native)",
            true,
        ),
        (
            "let slot = ptr::alloc<[ref u256; 2]>()\ndiscard((*slot)[1])",
            false,
        ),
        (
            "let slot = ptr::alloc<[ref u256; 2]>()\n*ptr::cast<[ref u256; 2], ref u256>(slot) = ref *owner\ndiscard((*slot)[1])",
            false,
        ),
        (
            "let slot = ptr::alloc<[ref u256; 2]>()\n*slot = [ref *owner, ref *owner]\ndiscard((*slot)[1])",
            true,
        ),
        ("let slot = ptr::alloc<Choice>()\nlet loaded = *slot", false),
        (
            "let slot = ptr::alloc<Choice>()\n*slot = Choice::Empty\nlet loaded = *slot",
            true,
        ),
        (
            "let slot = ptr::alloc<Choice>()\n*slot = Choice::Some(ref *owner)\nlet loaded = *slot",
            true,
        ),
    ] {
        let source = format!(
            r#"
use core::ptr
struct Pair {{ native: ref u256, other: u256 }}
enum Choice {{ Empty, Some(ref u256) }}
fn discard(_ value: ref u256) {{}}
fn inspect() {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    {body}
}}
"#
        );
        let diagnostics = checked_borrow_diags(&source);
        if valid {
            assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("cannot use a native borrow"),
                "{source}\n{diagnostics}"
            );
        }
    }
}

#[test]
fn fresh_slot_discovery_replays_typed_stores_before_reads_in_either_query_order() {
    for initialized in [false, true] {
        for query_first in ["inspect", "read", "fresh"] {
            let store = if initialized {
                "*slot = ref *owner"
            } else {
                ""
            };
            let source = format!(
                r#"
use core::ptr
fn fresh() -> *u8 {{ ptr::alloc_bytes(32) }}
fn read(_ slot: *ref u256) -> u256 {{ *slot }}
fn inspect() -> u256 {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    let bytes = fresh()
    let slot: *ref u256 = ptr::cast(bytes)
    {store}
    read(ptr::cast(bytes))
}}
"#
            );
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone("fresh_query_order.fe".into(), &source);
            let (module, _) = db.top_mod(file);
            db.assert_no_diags(module);
            let _ = semantic_borrow_summary(&db, func_instance(&db, module, query_first));
            let diagnostics = format_diagnostics(
                &db,
                &collect_semantic_borrow_diagnostic_vouchers(&db, module),
            );
            if initialized {
                assert!(
                    diagnostics.is_empty(),
                    "{query_first}: {source}\n{diagnostics}"
                );
            } else {
                assert!(
                    diagnostics.contains("cannot use a native borrow"),
                    "{query_first}: {source}\n{diagnostics}"
                );
            }
        }
    }
}

#[test]
fn allocation_birth_does_not_reuse_previous_loop_initialization() {
    for (store, valid) in [
        ("*slot = ref *owner", true),
        ("if index == 0 { *slot = ref *owner }", false),
    ] {
        let source = format!(
            r#"
use core::ptr
fn inspect() {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    let mut index: u256 = 0
    while index < 2 {{
        let slot = ptr::alloc<ref u256>()
        {store}
        let loaded: u256 = *slot
        index += 1
    }}
}}
"#
        );
        let diagnostics = checked_borrow_diags(&source);
        if valid {
            assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("cannot use a native borrow"),
                "{source}\n{diagnostics}"
            );
        }
    }
}

fn allocation_birth_loop_source(allocation: &str) -> String {
    format!(
        r#"
struct Item {{ n: u256 }}
fn consume(_ value: own Item) {{}}
fn initialized(_ n: u256) -> *Item {{
    let p = core::ptr::alloc<Item>()
    *p = Item {{ n }}
    p
}}
fn forward(_ n: u256) -> *Item {{ initialized(n) }}
fn check(_ count: u256) {{
    let mut i: u256 = 0
    while i < count {{
        {allocation}
        consume(*p)
        i += 1
    }}
}}
"#
    )
}

#[test]
fn allocation_birth_inline_loop_is_available() {
    let source =
        allocation_birth_loop_source("let p = core::ptr::alloc<Item>()\n*p = Item { n: i }");
    let diagnostics = checked_borrow_diags(&source);
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn allocation_birth_factory_loop_is_available() {
    let diagnostics = checked_borrow_diags(&allocation_birth_loop_source("let p = initialized(i)"));
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn allocation_birth_forwarded_factory_loop_is_available() {
    let diagnostics = checked_borrow_diags(&allocation_birth_loop_source("let p = forward(i)"));
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn allocation_birth_preserves_duplicate_and_callee_exit_moves() {
    for allocation in [
        "let p = initialized(i)\nconsume(*p)",
        "let p = moved(i)",
        "let pair = aliased(i)\nconsume(*pair.left)\nlet p = pair.right",
    ] {
        let source = allocation_birth_loop_source(allocation)
            + r#"
struct Pair { left: *Item, right: *Item }
fn moved(_ n: u256) -> *Item {
    let p = initialized(n)
    consume(*p)
    p
}
fn aliased(_ n: u256) -> Pair {
    let p = initialized(n)
    Pair { left: p, right: p }
}
"#;
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("move conflict"),
            "{source}\n{diagnostics}"
        );
        assert!(!diagnostics.contains("internal"), "{diagnostics}");
    }
}

#[test]
fn allocation_birth_distinguishes_multiple_factory_results() {
    let source = allocation_birth_loop_source(
        "let pair = distinct(i)\nconsume(*pair.left)\nlet p = pair.right",
    ) + r#"
struct Pair { left: *Item, right: *Item }
fn distinct(_ n: u256) -> Pair {
    Pair { left: initialized(n), right: initialized(n) }
}
"#;
    let diagnostics = checked_borrow_diags(&source);
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn allocation_birth_retains_older_moved_pointers_before_the_next_move() {
    for (declaration, old, retain) in [
        ("let mut old = initialized(0)", "old", "old = p"),
        (
            "let mut old = Holder { ptr: initialized(0) }",
            "old.ptr",
            "old.ptr = p",
        ),
        ("let mut old = [initialized(0)]", "old[0]", "old[0] = p"),
    ] {
        let source = allocation_birth_loop_source(&format!(
            "let p = forward(i)\nif i > 0 {{ consume(*{old}) }}\n{retain}",
        ))
        .replace(
            "let mut i: u256 = 0",
            &format!("{declaration}\nlet mut i: u256 = 0"),
        ) + "\nstruct Holder { ptr: *Item }\n";
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("move conflict"),
            "{source}\n{diagnostics}"
        );
        assert!(!diagnostics.contains("internal"), "{diagnostics}");
    }
}

#[test]
fn boolean_selected_factory_loops_retain_conservative_move_diagnostics() {
    // Boolean predicates are not represented by the existing edge guards. This
    // precision limit is separate from allocation lifetime transfer.
    let source =
        allocation_birth_loop_source("let p = if i == 0 { initialized(i) } else { forward(i) }");
    let diagnostics = checked_borrow_diags(&source);
    assert!(diagnostics.contains("move conflict"), "{diagnostics}");
    assert!(!diagnostics.contains("internal"), "{diagnostics}");
}

#[test]
fn recursive_fresh_allocation_returns_fail_closed_on_nonconvergence() {
    // Existing summary choices can grow through recursive allocation returns.
    // Keep the bounded diagnostic instead of accepting an opaque fallback.
    let source = allocation_birth_loop_source("let p = recursive(i, depth: 2)")
        + r#"
fn recursive(_ n: u256, depth: u256) -> *Item {
    if depth == 0 { initialized(n) } else { recursive(n, depth: depth - 1) }
}
"#;
    let diagnostics = checked_borrow_diags(&source);
    assert!(
        diagnostics.contains("recursive boundary requirements did not converge"),
        "{diagnostics}"
    );
    assert!(diagnostics.contains("transport violation"), "{diagnostics}");
    assert!(!diagnostics.contains("internal"), "{diagnostics}");
}

#[test]
fn allocation_birth_does_not_initialize_native_bytes_in_a_factory() {
    let source = r#"
fn uninitialized() -> *ref u256 { core::ptr::alloc<ref u256>() }
fn check(_ count: u256) {
    let mut i: u256 = 0
    while i < count {
        let slot = uninitialized()
        let loaded: u256 = *slot
        i += 1
    }
}
"#;
    let diagnostics = checked_borrow_diags(source);
    assert!(
        diagnostics.contains("cannot use a native borrow"),
        "{diagnostics}"
    );
    assert!(!diagnostics.contains("move conflict"), "{diagnostics}");
}

#[test]
fn allocation_birth_cast_views_and_query_order_preserve_ownership() {
    let source = allocation_birth_loop_source("let p = typed(bytes(i))")
        + r#"
fn bytes(_ n: u256) -> *u8 { core::ptr::cast<Item, u8>(initialized(n)) }
fn typed(_ p: *u8) -> *Item { core::ptr::cast<u8, Item>(p) }
"#;
    for first in ["check", "typed", "bytes", "initialized"] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("birth_query_order.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        semantic_borrow_summary(&db, func_instance(&db, module, first)).unwrap();
        let diagnostics = format_diagnostics(
            &db,
            &collect_semantic_borrow_diagnostic_vouchers(&db, module),
        );
        assert!(diagnostics.is_empty(), "{first}: {diagnostics}");
        // Repeated queries exercise the cached closed operation facts.
        let again = format_diagnostics(
            &db,
            &collect_semantic_borrow_diagnostic_vouchers(&db, module),
        );
        assert_eq!(diagnostics, again);
    }
}

#[test]
fn allocation_birth_does_not_revive_an_input_pointer_or_its_loaded_referent() {
    for allocation in [
        "let old = initialized(i)\nconsume(*old)\nlet p = identity(old)",
        "let old = initialized(i)\nconsume(*old)\nlet slot = container(old)\nlet p = *slot",
    ] {
        let source = allocation_birth_loop_source(allocation)
            + r#"
fn identity(_ p: *Item) -> *Item { p }
fn container(_ p: *Item) -> **Item {
    let slot = core::ptr::alloc<*Item>()
    *slot = p
    slot
}
"#;
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("move conflict"),
            "{source}\n{diagnostics}"
        );
        assert!(!diagnostics.contains("internal"), "{diagnostics}");
    }
}

#[test]
fn allocation_birth_nested_loops_and_recursive_pointer_forwarding() {
    for allocation in [
        "let p = recursive_pointer(forward(i), 2)",
        "let mut j: u256 = 0\nwhile j < 2 { let q = forward(j)\nconsume(*q)\nj += 1 }\nlet p = forward(i)",
    ] {
        let source = allocation_birth_loop_source(allocation)
            + r#"
fn recursive_pointer(_ p: *Item, _ depth: u256) -> *Item {
    if depth == 0 { p } else { recursive_pointer(p, depth - 1) }
}
fn zero() { check(0) }
fn multiple() { check(3) }
"#;
        let diagnostics = checked_borrow_diags(&source);
        assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
    }
}

#[test]
fn allocation_birth_enum_selected_factories_keep_their_guards() {
    let source = allocation_birth_loop_source(
        "let p = match pick { Choice::First => initialized(i), Choice::Second => forward(i) }",
    )
    .replace(
        "fn check(_ count: u256)",
        "fn check(_ count: u256, _ pick: Choice)",
    ) + "\nenum Choice { First, Second }\nimpl Copy for Choice {}\n";
    let diagnostics = checked_borrow_diags(&source);
    assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
}

#[test]
fn allocation_birth_fresh_or_old_return_does_not_revive_old_alternative() {
    let source = allocation_birth_loop_source(
        "let old = initialized(i)\nconsume(*old)\nlet p = maybe(Choice::Old, old, i)",
    ) + r#"
enum Choice { Fresh, Old }
fn maybe(_ pick: Choice, _ old: *Item, _ n: u256) -> *Item {
    match pick { Choice::Fresh => initialized(n), Choice::Old => old }
}
"#;
    let diagnostics = checked_borrow_diags(&source);
    assert!(diagnostics.contains("move conflict"), "{diagnostics}");
    assert!(!diagnostics.contains("internal"), "{diagnostics}");
}

#[test]
fn allocation_birth_nonreturning_calls_have_no_return_state() {
    let source = allocation_birth_loop_source("let p = allocate_then_stop(i)")
        + r#"
fn allocate_then_stop(_ n: u256) -> *Item {
    let p = initialized(n)
    core::panic()
}
"#;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("birth_nonreturning.fe".into(), &source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let summary = semantic_borrow_summary(&db, func_instance(&db, module, "allocate_then_stop"))
        .unwrap()
        .unwrap();
    assert!(!summary.may_return);
    assert!(summary.availability.reinitialized.is_empty());
    assert!(summary.availability.unavailable.is_empty());
    let diagnostics = format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, module),
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn allocation_birth_repeated_pointer_arrays_have_one_identity() {
    for (allocation, valid) in [
        ("let values = repeated(i)\nlet p = values[0]", true),
        (
            "let values = repeated(i)\nconsume(*values[0])\nlet p = values[1]",
            false,
        ),
    ] {
        let source = allocation_birth_loop_source(allocation)
            + "\nfn repeated(_ n: u256) -> [*Item; 2] { [initialized(n); 2] }\n";
        let diagnostics = checked_borrow_diags(&source);
        if valid {
            assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("move conflict"),
                "{source}\n{diagnostics}"
            );
        }
    }
}

#[test]
fn allocation_birth_repeated_native_slots_keep_fresh_invalid_seeds() {
    for (store, valid) in [
        ("*slots[0] = ref *owner", true),
        ("if i == 0 { *slots[0] = ref *owner }", false),
    ] {
        let source = format!(
            r#"
fn repeated() -> [*ref u256; 2] {{ [core::ptr::alloc<ref u256>(); 2] }}
fn check(_ count: u256) {{
    let owner = core::ptr::alloc<u256>()
    *owner = 7
    let mut i: u256 = 0
    while i < count {{
        let slots = repeated()
        {store}
        let loaded: u256 = *slots[1]
        i += 1
    }}
}}
"#
        );
        let diagnostics = checked_borrow_diags(&source);
        if valid {
            assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("cannot use a native borrow"),
                "{source}\n{diagnostics}"
            );
        }
    }
}

#[test]
fn normalized_array_repeat_requires_copy_under_instance_assumptions() {
    for (declarations, generics, ty, mode, copy) in [
        ("struct Item { value: u256 }", "", "Item", "own ", false),
        ("struct Item { value: *u256 }", "", "Item", "own ", false),
        ("struct Item { value: ref u256 }", "", "Item", "own ", false),
        (
            "struct Item { value: u256 }\nimpl Copy for Item {}",
            "",
            "Item",
            "own ",
            true,
        ),
        ("", "<T>", "T", "own ", false),
        ("", "<T: Copy>", "T", "own ", true),
        ("", "", "u256", "", true),
        ("", "", "*u256", "", true),
        ("", "", "ref u256", "", true),
        ("", "", "mut u256", "", true),
    ] {
        for (len, fields) in [(1, "first"), (2, "first, second")] {
            let source = format!(
                r#"
{declarations}
fn inspect{generics}(first: {mode}{ty}, second: {mode}{ty}) -> [{ty}; {len}] {{
    [{fields}]
}}
"#
            );
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone("repeat_copy.fe".into(), &source);
            let (module, _) = db.top_mod(file);
            db.assert_no_diags(module);
            let mut artifacts = normalized_func_body(&db, module, "inspect");
            verify_normalized_body(&db, &artifacts.body).expect("valid source array construction");
            let expr = artifacts
                .body
                .blocks
                .iter_mut()
                .flat_map(|block| &mut block.statements)
                .find_map(|statement| match &mut statement.kind {
                    NStatementKind::Define {
                        expr: expr @ NExpr::AggregateMake { .. },
                        ..
                    } => Some(expr),
                    _ => None,
                })
                .expect("array construction");
            let NExpr::AggregateMake { ty, fields } = expr else {
                unreachable!()
            };
            assert!(ty.is_array(&db));
            *expr = NExpr::ArrayRepeat {
                ty: *ty,
                value: fields[0],
            };
            let expected = if copy {
                Ok(())
            } else {
                Err(NormalizedBodyVerifyError::ArrayRepeatRequiresCopy)
            };
            assert_eq!(
                verify_normalized_body(&db, &artifacts.body),
                expected,
                "{source}"
            );
        }
    }
}

#[test]
fn raw_allocation_effect_elision_assumes_the_complete_range_contract() {
    for len in [32, 64] {
        // Only len=32 satisfies this allocation's raw range contract. The
        // len=64 candidate deliberately documents an unchecked caller assertion:
        // borrow acceptance is not a bounds proof, and this case is never run.
        with_borrow_summary(
            &format!(
                r#"
fn raw_range() {{
    let bytes = core::ptr::alloc_bytes(32)
    core::ptr::zero_bytes(bytes, {len})
}}
"#
            ),
            "raw_range",
            |_, summary| assert!(summary.accesses.is_empty(), "{summary:#?}"),
        );
    }
}

#[test]
fn physical_casts_do_not_inherit_zero_sized_pointee_disjointness() {
    for write in [
        "*ptr::cast<(), u256>(empty) = 1",
        "write(empty)",
        "forward(empty)",
    ] {
        let source = format!(
            r#"
use core::ptr
fn write(_ empty: *()) {{ *ptr::cast<(), u256>(empty) = 1 }}
fn forward(_ empty: *()) {{ write(empty) }}
fn bad(empty: *(), word: *u256) {{
    let native = mut *word
    {write}
    native = 2
}}
"#
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("borrow conflict in `fn bad`"),
            "{source}\n{diagnostics}"
        );
    }
}

#[test]
fn operation_operands_preserve_zero_sized_ownership() {
    for binding in ["item", "mut item"] {
        let source = format!(
            "struct Item {{}}\nfn take(_ first: own Item, _ second: own Item) {{}}\nfn bad({binding}: own Item) {{ take(item, item) }}"
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("move conflict in `fn bad`"),
            "{source}\n{diagnostics}"
        );
    }
}

#[test]
fn physical_offsets_with_overlapping_word_accesses_conflict() {
    for borrow in ["mut *first", "lend(first)", "forward(first)"] {
        let second_borrow = borrow.replace("first", "second");
        let diagnostics = checked_borrow_diags(&format!(
            r#"
use core::ptr
fn lend(_ pointer: *u256) -> mut u256 {{ mut *pointer }}
fn forward(_ pointer: *u256) -> mut u256 {{ lend(pointer) }}
fn bad() {{
    let base = ptr::alloc_bytes(96)
    let first = ptr::cast<u8, u256>(ptr::offset_bytes(base, 1))
    let second = ptr::cast<u8, u256>(ptr::offset_bytes(base, 2))
    *first = 1
    *second = 2
    let a = {borrow}
    let b = {second_borrow}
    a = 3
    b = 4
}}
"#,
        ));
        assert!(
            diagnostics.contains("borrow conflict in `fn bad`"),
            "{diagnostics}"
        );
    }
}

#[test]
fn physical_offsets_preserve_disjoint_typed_elements() {
    for borrow in ["mut *first", "lend(first)", "forward(first)"] {
        let second_borrow = borrow.replace("first", "second");
        let diagnostics = checked_borrow_diags(&format!(
            r#"
use core::ptr
fn lend(_ pointer: *u256) -> mut u256 {{ mut *pointer }}
fn forward(_ pointer: *u256) -> mut u256 {{ lend(pointer) }}
fn valid() {{
    let base = ptr::alloc_bytes(96)
    let first = ptr::cast<u8, u256>(ptr::offset_bytes(base, 1))
    let second = ptr::cast<u8, u256>(ptr::offset_bytes(base, 33))
    *first = 1
    *second = 2
    let a = {borrow}
    let b = {second_borrow}
    a = 3
    b = 4
}}
"#,
        ));
        assert!(diagnostics.is_empty(), "{diagnostics}");
    }
}

#[test]
fn physical_copy_ranges_preserve_extent_through_forwarding_and_restoration() {
    for copy in [
        "ptr::copy_raw(destination, source, len)",
        "copy(destination, source, len)",
        "forward(destination, source, len)",
    ] {
        for len in [0, 1, 32] {
            for restore in [false, true] {
                let restoration = if restore { "*slot = mut *value" } else { "" };
                let source = format!(
                    r#"
use core::ptr
fn copy(_ destination: *u8, _ source: *u8, _ len: u256) {{ ptr::copy_raw(destination, source, len) }}
fn forward(_ destination: *u8, _ source: *u8, _ len: u256) {{ copy(destination, source, len) }}
fn inspect() {{
    let base = ptr::alloc_bytes(96)
    let value = ptr::alloc<u256>()
    *value = 7
    let slot = ptr::cast<u8, mut u256>(ptr::offset_bytes(base, 2))
    *slot = mut *value
    let destination = ptr::offset_bytes(base, 1)
    let source = ptr::alloc_bytes(96)
    let len = {len}
    {copy}
    {restoration}
    let native = *slot
    native = 8
}}
"#
                );
                let diagnostics = checked_borrow_diags(&source);
                if len <= 1 || restore {
                    assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
                } else {
                    assert!(
                        diagnostics.contains("invalidated by a raw write")
                            && diagnostics.contains("let native = *slot"),
                        "{source}\n{diagnostics}"
                    );
                }
            }
        }
    }
}

#[test]
fn physical_intrinsic_byte_and_word_stores_have_distinct_extents() {
    for method in ["mstore8", "mstore"] {
        for call in [
            format!("mem.{method}(addr: destination, value: 1)"),
            "store(destination)".into(),
            "forward(destination)".into(),
        ] {
            let source = format!(
                r#"
use core::ptr
use std::evm::RawMem
fn store(_ destination: *u8) uses (mem: mut RawMem) {{ mem.{method}(addr: destination, value: 1) }}
fn forward(_ destination: *u8) uses (mem: mut RawMem) {{ store(destination) }}
fn inspect() uses (mem: mut RawMem) {{
    let base = ptr::alloc_bytes(96)
    let value = ptr::alloc<u256>()
    *value = 7
    let slot = ptr::cast<u8, mut u256>(ptr::offset_bytes(base, 2))
    *slot = mut *value
    let destination = ptr::offset_bytes(base, 1)
    {call}
    let native = *slot
    native = 8
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            if method == "mstore8" {
                assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
            } else {
                assert!(
                    diagnostics.contains("invalidated by a raw write")
                        && diagnostics.contains("let native = *slot"),
                    "{source}\n{diagnostics}"
                );
            }
        }
    }
}

#[test]
fn physical_read_ranges_survive_recursive_summary_composition() {
    for call in [
        "ptr::copy_raw(destination, source, len)",
        "copy(destination, source, len)",
        "repeat(destination, source, len, true)",
    ] {
        for len in [0, 1, 32] {
            let source = format!(
                r#"
use core::ptr
fn copy(_ destination: *u8, _ source: *u8, _ len: u256) {{ ptr::copy_raw(destination, source, len) }}
fn repeat(_ destination: *u8, _ source: *u8, _ len: u256, _ again: bool) {{
    if again {{ repeat(destination, source, len, false) }} else {{ copy(destination, source, len) }}
}}
fn inspect() {{
    let base = ptr::alloc_bytes(96)
    let word = ptr::cast<u8, u256>(ptr::offset_bytes(base, 2))
    *word = 7
    let source = ptr::offset_bytes(base, 1)
    let destination = ptr::alloc_bytes(96)
    let native = mut *word
    let len = {len}
    {call}
    native = 8
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            if len <= 1 {
                assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
            } else {
                assert!(
                    diagnostics.contains("borrow conflict in `fn inspect`"),
                    "{source}\n{diagnostics}"
                );
            }
        }
    }
}

#[test]
fn physical_unknown_copy_length_is_not_an_empty_footprint() {
    let diagnostics = checked_borrow_diags(
        r#"
use core::ptr
fn copy(_ destination: *u8, _ source: *u8, _ len: u256) { ptr::copy_raw(destination, source, len) }
fn inspect(len: u256) {
    let base = ptr::alloc_bytes(96)
    let value = ptr::alloc<u256>()
    *value = 7
    let slot = ptr::cast<u8, mut u256>(ptr::offset_bytes(base, 2))
    *slot = mut *value
    let source = ptr::alloc_bytes(96)
    copy(ptr::offset_bytes(base, 1), source, len)
    let native = *slot
    native = 8
}
"#,
    );
    assert!(
        diagnostics.contains("invalidated by a raw write"),
        "{diagnostics}"
    );
}

#[test]
fn signature_fallback_native_result_cannot_lose_its_referent() {
    assert_pending_validation(
        r#"
trait Lender { fn lend(pointer: *u256) -> mut u256 }
fn bad<T: Lender>(pointer: *u256) {
    let first = T::lend(pointer)
    let second = mut *pointer
    first = 1
    second = 2
}
"#,
        "bad",
    );
}

#[test]
fn signature_fallback_writable_native_contents_can_be_invalid() {
    assert_pending_validation(
        r#"
trait Operation { fn apply(slot: *ref u256) }
fn bad<T: Operation>(slot: *ref u256) -> ref u256 {
    T::apply(slot)
    *slot
}
"#,
        "bad",
    );
}

#[test]
fn implicit_views_require_available_owners() {
    for setup in [
        "fn bad(item: own Item) -> u256 {",
        "fn bad(n: u256) -> u256 { let item = Item { n }",
        "fn bad(n: u256) -> u256 { let mut item = Item { n }",
    ] {
        let source = format!(
            "struct Item {{ n: u256 }}\n\
             fn consume(_ item: own Item) {{}}\n\
             fn read(_ item: Item) -> u256 {{ item.n }}\n\
             {setup}\nconsume(item)\nread(item)\n}}"
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("move conflict in `fn bad`"),
            "{source}\n{diagnostics}"
        );
    }
}

#[test]
fn ownership_moves_conflict_with_live_shared_loans() {
    for kind in ["ref", "mut"] {
        let diagnostics = checked_borrow_diags(&format!(
            "struct Item {{ n: u256 }}\n\
             fn consume(_ item: own Item) {{}}\n\
             fn read(_ item: {kind} Item) -> u256 {{ item.n }}\n\
             fn bad(mut item: own Item) -> u256 {{\n\
                 let borrowed = {kind} item\n\
                 consume(item)\n\
                 read(borrowed)\n\
             }}"
        ));
        assert!(
            diagnostics.contains("borrow conflict in `fn bad`"),
            "{kind}: {diagnostics}"
        );
    }
}

#[test]
fn owned_temporary_pointer_places_consume_the_selected_value() {
    for (declaration, expression) in [
        ("", "*identity(pointer)"),
        (
            "struct Pair { item: Item, other: u256 }",
            "(*identity(pointer)).item",
        ),
        ("", "pointer[0]"),
    ] {
        let pointee = if declaration.is_empty() {
            "Item"
        } else {
            "Pair"
        };
        let pointee = if expression.contains("[0]") {
            "[Item; 2]"
        } else {
            pointee
        };
        for copy in [false, true] {
            let copy_impl = if copy {
                "impl core::Copy for Item {}"
            } else {
                ""
            };
            let diagnostics = checked_borrow_diags(&format!(
                r#"
struct Item {{ n: u256 }}
{copy_impl}
{declaration}
fn identity(pointer: *{pointee}) -> *{pointee} {{ pointer }}
fn consume(_ item: own Item) {{}}
fn check(pointer: *{pointee}) {{
    consume({expression})
    consume({expression})
}}
"#
            ));
            if copy {
                assert!(diagnostics.is_empty(), "{expression}: {diagnostics}");
            } else {
                assert!(
                    diagnostics.contains("move conflict in `fn check`"),
                    "{expression}: {diagnostics}"
                );
            }
        }
    }
}

#[test]
fn pointee_moves_survive_call_boundaries() {
    for access in ["*pointer", "take(pointer)", "forward(pointer)"] {
        let diagnostics = checked_borrow_diags(&format!(
            "struct Item {{ n: u256 }}\n\
             fn consume(_ item: own Item) {{}}\n\
             fn take(pointer: *Item) -> Item {{ *pointer }}\n\
             fn forward(pointer: *Item) -> Item {{ take(pointer) }}\n\
             fn bad(pointer: *Item) {{\n\
                 let first = {access}\n\
                 let second = {access}\n\
                 consume(first)\n\
                 consume(second)\n\
             }}"
        ));
        assert!(
            diagnostics.contains("move conflict in `fn bad`"),
            "{access}: {diagnostics}"
        );
    }
}

#[test]
fn pointee_availability_is_required_by_read_and_view_calls() {
    for access in ["(*pointer).n", "read(pointer)", "forward(pointer)"] {
        let diagnostics = checked_borrow_diags(&format!(
            "struct Item {{ n: u256 }}\n\
             fn consume(_ item: own Item) {{}}\n\
             fn view(_ item: Item) -> u256 {{ item.n }}\n\
             fn read(pointer: *Item) -> u256 {{ view(*pointer) }}\n\
             fn forward(pointer: *Item) -> u256 {{ read(pointer) }}\n\
             fn bad(pointer: *Item) -> u256 {{\n\
                 consume(*pointer)\n\
                 {access}\n\
             }}"
        ));
        assert!(
            diagnostics.contains("move conflict in `fn bad`"),
            "{access}: {diagnostics}"
        );
    }
}

#[test]
fn repeated_views_and_pointer_copies_preserve_ownership() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Item { n: u256 }
fn view(_ item: Item) -> u256 { item.n }
fn read(pointer: *Item) -> u256 { view(*pointer) }
fn forward(pointer: *Item) -> u256 { read(pointer) }
fn valid(pointer: *Item) -> u256 {
    let copied = pointer
    let first = forward(pointer)
    first + read(pointer: copied)
}
fn local(item: own Item) -> u256 { view(item) + view(item) }
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn raw_copy_poststates_cannot_restore_entry_pointer_provenance() {
    for copy in [
        "*target = first\nptr::copy_raw(ptr::byte_ptr(target), source: ptr::byte_ptr(source), len: 32)",
        "copy_slot(target, source, first)",
        "forward(target, source, first)",
        "overwrite_word(target, source, first)",
        "loop_copy_slot(target, source, first)",
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
use core::ptr
fn copy_slot(target: **u256, source: **u256, first: *u256) {{
    *target = first
    ptr::copy_raw(ptr::byte_ptr(target), source: ptr::byte_ptr(source), len: 32)
}}
fn forward(target: **u256, source: **u256, first: *u256) {{
    copy_slot(target, source, first)
}}
fn loop_copy_slot(target: **u256, source: **u256, first: *u256) {{
    *target = first
    let mut index: usize = 0
    while index < 2 {{
        ptr::copy_raw(ptr::byte_ptr(target), source: ptr::byte_ptr(source), len: 32)
        index += 1
    }}
}}
fn overwrite_word(target: **u256, source: **u256, first: *u256) {{
    *target = first
    let destination_word = ptr::cast<*u256, u256>(target)
    let source_word = ptr::cast<*u256, u256>(source)
    *destination_word = *source_word
}}
fn bad() {{
    let first = ptr::alloc<u256>()
    let second = ptr::alloc<u256>()
    let source = ptr::alloc<*u256>()
    let target = ptr::alloc<*u256>()
    *source = second
    *target = first
    {copy}
    let borrowed = mut *second
    let alias = mut *(*target)
    borrowed = 1
    alias = 2
}}
"#
        ));
        assert!(
            diagnostics.contains("borrow conflict in `fn bad`"),
            "{copy}: {diagnostics}"
        );
    }
}

#[test]
fn definite_pointer_store_after_raw_copy_recovers_precise_contents() {
    // A further forwarding summary can conservatively join possibly aliased
    // source/target poststates. It is not an ordered trace of these stores.
    for copy in [
        "ptr::copy_raw(ptr::byte_ptr(target), source: ptr::byte_ptr(source), len: 32)\n*target = first",
        "copy_slot(target, source, first)",
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
use core::ptr
fn copy_slot(target: **u256, source: **u256, first: *u256) {{
    ptr::copy_raw(ptr::byte_ptr(target), source: ptr::byte_ptr(source), len: 32)
    *target = first
}}
fn valid() {{
    let first = ptr::alloc<u256>()
    let second = ptr::alloc<u256>()
    let source = ptr::alloc<*u256>()
    let target = ptr::alloc<*u256>()
    *source = second
    *target = first
    {copy}
    let borrowed = mut *second
    let independent = mut *(*target)
    borrowed = 1
    independent = 2
}}
"#
        ));
        assert!(diagnostics.is_empty(), "{copy}: {diagnostics}");
    }
}

#[test]
fn raw_overwrites_do_not_manufacture_native_borrow_authority() {
    for (reinitialize, access) in [false, true].into_iter().flat_map(|reinitialize| {
        ["*slot", "load(slot)", "forward(slot)"].map(|access| (reinitialize, access))
    }) {
        let restoration = if reinitialize {
            "*slot = mut *owner"
        } else {
            ""
        };
        let diagnostics = checked_borrow_diags(&format!(
            r#"
use core::ptr
fn load(slot: *mut u256) -> mut u256 {{ *slot }}
fn forward(slot: *mut u256) -> mut u256 {{ load(slot) }}
fn inspect() {{
    let owner = ptr::alloc<u256>()
    let source = ptr::alloc<u256>()
    let slot = ptr::alloc<mut u256>()
    *source = 0
    *slot = mut *owner
    ptr::copy_raw(ptr::byte_ptr(slot), source: ptr::byte_ptr(source), len: 32)
    {restoration}
    let borrowed = {access}
    borrowed = 1
}}
"#
        ));
        if reinitialize {
            assert!(diagnostics.is_empty(), "{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("invalidated by a raw write"),
                "{diagnostics}"
            );
        }
    }
}

#[test]
fn overlapping_same_shape_stores_cannot_preserve_native_authority() {
    for overwrite in [
        "*shifted = mut *second",
        "store(slot: shifted)",
        "forward(slot: shifted)",
    ] {
        for restore in [false, true] {
            let restoration = if restore { "*slot = mut *first" } else { "" };
            let source = format!(
                r#"
use core::ptr
fn store(slot: *mut u256) {{
    let source = ptr::alloc<u256>()
    *source = 2
    *slot = mut *source
}}
fn forward(slot: *mut u256) {{ store(slot) }}
fn inspect() {{
    let first = ptr::alloc<u256>()
    let second = ptr::alloc<u256>()
    *first = 1
    *second = 2
    let slot = ptr::alloc<mut u256>()
    *slot = mut *first
    let shifted = ptr::cast<u8, mut u256>(ptr::offset_bytes(ptr::byte_ptr(slot), 1))
    {overwrite}
    {restoration}
    let restored = *slot
    restored = 3
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            if restore {
                assert!(diagnostics.is_empty(), "{overwrite}: {diagnostics}");
            } else {
                assert!(
                    diagnostics.contains("invalidated by a raw write")
                        && diagnostics.contains("let restored = *slot"),
                    "{overwrite}: {diagnostics}"
                );
            }
        }
    }
}

#[test]
fn ambiguous_writes_do_not_reinitialize_moved_owners() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Item { n: u256 }
fn consume(_ item: own Item) {}
fn bad(first: *Item, second: *Item, flag: bool) {
    let old = *first
    let destination = if flag { first } else { second }
    *destination = Item { n: 7 }
    let reused = *first
    consume(old)
    consume(reused)
}
"#,
    );
    assert!(
        diagnostics.contains("move conflict in `fn bad`"),
        "{diagnostics}"
    );
}

#[test]
fn exact_and_dynamic_index_writes_have_distinct_reinitialization_guarantees() {
    for destination in ["0", "index"] {
        let source = format!(
            r#"
struct Item {{ n: u256 }}
fn consume(_ item: own Item) {{}}
fn inspect(array: *[Item; 2], index: usize) {{
    let old = (*array)[0]
    (*array)[{destination}] = Item {{ n: 7 }}
    let reused = (*array)[0]
    consume(old)
    consume(reused)
}}
"#
        );
        let diagnostics = checked_borrow_diags(&source);
        if destination == "0" {
            assert!(diagnostics.is_empty(), "{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("move conflict in `fn inspect`"),
                "{diagnostics}"
            );
        }
    }
}

#[test]
fn a_field_write_cannot_reinitialize_a_moved_whole_aggregate() {
    for replacement in [
        "(*pointer).left = Item { n: 7 }",
        "*pointer = Pair { left: Item { n: 7 }, right: Item { n: 8 } }",
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
struct Item {{ n: u256 }}
struct Pair {{ left: Item, right: Item }}
fn consume(_ pair: own Pair) {{}}
fn inspect(pointer: *Pair) {{
    let old = *pointer
    {replacement}
    let reused = *pointer
    consume(old)
    consume(reused)
}}
"#
        ));
        if replacement.starts_with("(*pointer).left") {
            assert!(
                diagnostics.contains("move conflict in `fn inspect`"),
                "{diagnostics}"
            );
        } else {
            assert!(diagnostics.is_empty(), "{diagnostics}");
        }
    }
}

#[test]
fn call_availability_preserves_move_then_reinitialize_order() {
    for replace in ["replace(pointer)", "forward(pointer)"] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
struct Item {{ n: u256 }}
fn consume(_ item: own Item) {{}}
fn replace(pointer: *Item) -> Item {{
    let old = *pointer
    *pointer = Item {{ n: 7 }}
    old
}}
fn forward(pointer: *Item) -> Item {{ replace(pointer) }}
fn valid(pointer: *Item) {{
    let previous = {replace}
    let fresh = *pointer
    consume(previous)
    consume(fresh)
}}
"#
        ));
        assert!(diagnostics.is_empty(), "{replace}: {diagnostics}");
    }
}

#[test]
fn call_initialization_discharges_incoming_availability_requirements() {
    for action in [
        "initialize(pointer)\nlet fresh = *pointer",
        "let fresh = initialize_then_take(pointer)",
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
struct Item {{ n: u256 }}
fn consume(_ item: own Item) {{}}
fn initialize(pointer: *Item) {{ *pointer = Item {{ n: 7 }} }}
fn initialize_then_take(pointer: *Item) -> Item {{
    *pointer = Item {{ n: 7 }}
    *pointer
}}
fn valid(pointer: *Item) {{
    let previous = *pointer
    {action}
    consume(previous)
    consume(fresh)
}}
"#
        ));
        assert!(diagnostics.is_empty(), "{action}: {diagnostics}");
    }
}

#[test]
fn call_reinitialization_requires_every_normal_return_path() {
    for (body, initialized) in [
        (
            "if flag { *pointer = Item { n: 1 } } else { *pointer = Item { n: 2 } }",
            true,
        ),
        ("if flag { *pointer = Item { n: 1 } }", false),
        (
            "if flag { *pointer = Item { n: 1 }\nreturn\n}\n*pointer = Item { n: 2 }",
            true,
        ),
        ("if flag { return }\n*pointer = Item { n: 2 }", false),
        ("while flag { *pointer = Item { n: 1 } }", false),
        (
            "while flag { *pointer = Item { n: 1 } }\n*pointer = Item { n: 2 }",
            true,
        ),
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
struct Item {{ n: u256 }}
fn consume(_ item: own Item) {{}}
fn initialize(pointer: *Item, flag: bool) {{ {body} }}
fn forward(pointer: *Item, flag: bool) {{ initialize(pointer, flag) }}
fn inspect(pointer: *Item, flag: bool) {{
    let old = *pointer
    forward(pointer, flag)
    let fresh = *pointer
    consume(old)
    consume(fresh)
}}
"#
        ));
        if initialized {
            assert!(diagnostics.is_empty(), "{body}: {diagnostics}");
        } else {
            assert!(
                diagnostics.contains("move conflict in `fn inspect`"),
                "{body}: {diagnostics}"
            );
        }
    }
}

#[test]
fn recursive_availability_keeps_base_case_initialization_and_consumption() {
    for (body, available) in [
        ("*pointer = Item { n: 7 }", true),
        ("consume(*pointer)", false),
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
struct Item {{ n: u256 }}
fn consume(_ item: own Item) {{}}
fn recurse(pointer: *Item, again: bool) {{
    if again {{ recurse(pointer, again: false) }} else {{ {body} }}
}}
fn inspect(pointer: *Item) {{
    recurse(pointer, again: true)
    let remaining = *pointer
    consume(remaining)
}}
"#
        ));
        if available {
            assert!(diagnostics.is_empty(), "{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("move conflict in `fn inspect`"),
                "{diagnostics}"
            );
        }
    }
}

#[test]
fn initialization_before_consumption_leaves_the_caller_unavailable() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Item { n: u256 }
fn consume(_ item: own Item) {}
fn initialize_then_take(pointer: *Item) -> Item {
    *pointer = Item { n: 7 }
    *pointer
}
fn forward(pointer: *Item) -> Item { initialize_then_take(pointer) }
fn bad(pointer: *Item) {
    let first = forward(pointer)
    let second = *pointer
    consume(first)
    consume(second)
}
"#,
    );
    assert!(
        diagnostics.contains("move conflict in `fn bad`"),
        "{diagnostics}"
    );
}

#[test]
fn restored_call_moves_still_conflict_with_shared_loans() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Item { n: u256 }
fn replace(pointer: *Item) -> Item {
    let old = *pointer
    *pointer = Item { n: 7 }
    old
}
fn read(_ item: ref Item) -> u256 { item.n }
fn bad(pointer: *Item) -> u256 {
    let held = ref *pointer
    let old = replace(pointer)
    read(held) + old.n
}
"#,
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn bad`"),
        "{diagnostics}"
    );
}

#[test]
fn call_availability_is_independent_of_the_owned_value_shape() {
    for (definition, replacement) in [
        ("struct Item { value: u256 }", "Item { value: 7 }"),
        (
            "struct Item { value: (u256, u256) }",
            "Item { value: (7, 8) }",
        ),
        ("struct Item { value: [u256; 2] }", "Item { value: [7, 8] }"),
        (
            "struct Item { value: *u256 }",
            "Item { value: core::ptr::alloc<u256>() }",
        ),
        ("enum Item { Value(u256), Empty }", "Item::Value(7)"),
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
{definition}
fn consume(_ item: own Item) {{}}
fn replace(pointer: *Item) -> Item {{
    let old = *pointer
    *pointer = {replacement}
    old
}}
fn forward(pointer: *Item) -> Item {{ replace(pointer) }}
fn valid(pointer: *Item) {{
    let old = forward(pointer)
    let fresh = *pointer
    consume(old)
    consume(fresh)
}}
"#
        ));
        assert!(diagnostics.is_empty(), "{definition}: {diagnostics}");
    }
}

#[test]
fn opaque_pointer_operations_conservatively_consume_noncopy_pointees() {
    assert_pending_validation(
        r#"
struct Item { n: u256 }
trait Operation { fn apply(pointer: *Item) }
fn consume(_ item: own Item) {}
fn bad<T: Operation>(pointer: *Item) {
    T::apply(pointer)
    let reused = *pointer
    consume(reused)
}
"#,
        "bad",
    );
}

#[test]
fn availability_summaries_do_not_depend_on_diagnostic_queries() {
    let source = r#"
struct Item { n: u256 }
fn replace(pointer: *Item) -> Item {
    let old = *pointer
    *pointer = Item { n: 7 }
    old
}
fn initialize_then_take(pointer: *Item) -> Item {
    *pointer = Item { n: 7 }
    *pointer
}
fn forward(pointer: *Item) -> Item { replace(pointer) }
"#;
    for function in ["replace", "forward", "initialize_then_take"] {
        with_borrow_summary(source, function, |_, summary| {
            assert!(
                summary
                    .accesses
                    .iter()
                    .any(|access| access.kind == MemoryAccessKind::Move)
            );
            assert!(!summary.availability.reinitialized.is_empty());
            if function == "initialize_then_take" {
                assert!(!summary.availability.unavailable.is_empty());
                assert!(
                    summary
                        .availability
                        .incoming
                        .iter()
                        .all(|requirement| requirement.kind == MemoryAccessKind::Write)
                );
            } else {
                assert!(summary.availability.unavailable.is_empty());
                assert!(
                    summary
                        .availability
                        .incoming
                        .iter()
                        .any(|requirement| requirement.kind == MemoryAccessKind::Read)
                );
            }
        });
    }
}

#[test]
fn borrowed_aggregate_availability_survives_forwarding() {
    let diagnostics = checked_borrow_diags(
        r#"
use core::ptr
struct Item { value: ref u256 }
fn consume(_ item: own Item) {}
fn replace(pointer: *Item, owner: ref u256) -> Item {
    let old = *pointer
    *pointer = Item { value: owner }
    old
}
fn forward(pointer: *Item, owner: ref u256) -> Item { replace(pointer, owner) }
fn valid() {
    let owner = ptr::alloc<u256>()
    let pointer = ptr::alloc<Item>()
    *pointer = Item { value: ref *owner }
    let old = forward(pointer, owner: ref *owner)
    let fresh = *pointer
    consume(old)
    consume(fresh)
}
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn typed_call_restoration_precedes_native_contents_requirements() {
    let diagnostics = checked_borrow_diags(
        r#"
use core::ptr
fn restore(slot: *mut u256) -> mut u256 {
    let owner = ptr::alloc<u256>()
    *owner = 0
    *slot = mut *owner
    *slot
}
fn forward(slot: *mut u256) -> mut u256 { restore(slot) }
fn valid() {
    let owner = ptr::alloc<u256>()
    let source = ptr::alloc<u256>()
    let slot = ptr::alloc<mut u256>()
    *source = 0
    *slot = mut *owner
    ptr::copy_raw(ptr::byte_ptr(slot), source: ptr::byte_ptr(source), len: 32)
    let borrowed = forward(slot)
    borrowed = 1
}
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn raw_pointer_assignments_initialize_native_slots_without_reading_old_contents() {
    for kind in ["ref", "mut"] {
        for assignment in [
            "*slot = native",
            "*identity(slot) = native",
            "replace(slot, native)",
            "forward(slot, native)",
        ] {
            let source = format!(
                r#"
use core::ptr
fn identity<T>(_ pointer: *T) -> *T {{ pointer }}
fn replace(_ slot: *{kind} u256, _ value: {kind} u256) {{ *slot = value }}
fn forward(_ slot: *{kind} u256, _ value: {kind} u256) {{ replace(slot, value) }}
fn inspect() -> u256 {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    let slot = ptr::alloc<{kind} u256>()
    let native = {kind} *owner
    ptr::zero_bytes(ptr::byte_ptr(slot), 32)
    {assignment}
    *slot
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
        }
    }
}

#[test]
fn native_pointer_copy_reads_require_valid_carriers() {
    for kind in ["ref", "mut"] {
        for read in ["*slot", "read(slot)", "forward(slot)"] {
            let source = format!(
                r#"
use core::ptr
fn read(_ slot: *{kind} u256) -> u256 {{ *slot }}
fn forward(_ slot: *{kind} u256) -> u256 {{ read(slot) }}
fn inspect() -> u256 {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    let slot = ptr::alloc<{kind} u256>()
    *slot = {kind} *owner
    ptr::zero_bytes(ptr::byte_ptr(slot), 32)
    {read}
}}
"#
            );
            let diagnostics = checked_borrow_diags(&source);
            assert!(
                diagnostics.contains("invalidated by a raw write"),
                "{source}\n{diagnostics}"
            );
        }
    }
}

#[test]
fn native_validity_obligations_distinguish_disjoint_and_aliased_callers() {
    for call in [
        "clobber(slot, destination)",
        "forward(slot, destination)",
        "recursive(slot, destination, again: true)",
    ] {
        for aliases in [false, true] {
            let destination = if aliases {
                "ptr::byte_ptr(slot)"
            } else {
                "ptr::alloc_bytes(32)"
            };
            let diagnostics = checked_borrow_diags(&format!(
                r#"
use core::ptr
fn read(_ value: ref u256) -> u256 {{ value }}
fn clobber(slot: *ref u256, destination: *u8) -> ref u256 {{
    ptr::zero_bytes(destination, 32)
    *slot
}}
fn forward(slot: *ref u256, destination: *u8) -> ref u256 {{ clobber(slot, destination) }}
fn recursive(slot: *ref u256, destination: *u8, again: bool) -> ref u256 {{
    if again {{ return recursive(slot, destination, again: false) }}
    clobber(slot, destination)
}}
fn check() -> u256 {{
    let owner = ptr::alloc<u256>()
    *owner = 1
    let slot = ptr::alloc<ref u256>()
    *slot = ref *owner
    let destination = {destination}
    let borrowed = {call}
    read(borrowed)
}}
"#
            ));
            if aliases {
                assert!(
                    diagnostics.contains("borrow conflict in `fn check`"),
                    "{call}: {diagnostics}"
                );
            } else {
                assert!(diagnostics.is_empty(), "{call}: {diagnostics}");
            }
        }
    }
}

#[test]
fn write_requirements_cannot_hide_partial_writes_behind_whole_writes() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Item { n: u256 }
fn consume(_ item: own Item) {}
fn field_then_whole(field: *Item, whole: *Item) {
    (*field).n = 1
    *whole = Item { n: 7 }
}
fn bad(pointer: *Item) {
    let old = *pointer
    field_then_whole(field: pointer, whole: pointer)
    consume(old)
}
"#,
    );
    assert!(
        diagnostics.contains("move conflict in `fn bad`"),
        "{diagnostics}"
    );
}

#[test]
fn reinitializing_one_summarized_move_preserves_unrelated_moves() {
    for field in ["left", "right"] {
        let diagnostics = checked_borrow_diags(&format!(
            r#"
struct Item {{ n: u256 }}
struct Pair {{ left: Item, right: Item }}
fn consume(_ item: own Item) {{}}
fn take_both(pointer: *Pair) {{
    consume((*pointer).left)
    consume((*pointer).right)
}}
fn initialize_left(pointer: *Pair) {{ (*pointer).left = Item {{ n: 7 }} }}
fn inspect(pointer: *Pair) {{
    take_both(pointer)
    initialize_left(pointer)
    let fresh = (*pointer).{field}
    consume(fresh)
}}
"#
        ));
        if field == "left" {
            assert!(diagnostics.is_empty(), "{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("move conflict in `fn inspect`"),
                "{diagnostics}"
            );
        }
    }
}

#[test]
fn packed_encoding_fixture_does_not_report_semantic_borrow_errors() {
    let diagnostics = checked_borrow_diags(include_str!(
        "../../fe/tests/fixtures/fe_test/packed_encoding.fe"
    ));
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn raw_call_preserves_fresh_return_buffer_pointer() {
    let source = r#"
use core::ptr::{MemBuffer, MemSpan}
use std::evm::{Address, Call}
fn returned() -> *u8 uses (call: mut Call) {
    let mut ret = MemBuffer::empty()
    let _ = call.raw_call(addr: Address { inner: 0 }, gas: 0, value: 0, args: MemSpan::empty(), ret: mut ret)
    ret.ptr()
}
"#;
    let diagnostics = checked_borrow_diags(source);
    assert!(diagnostics.is_empty(), "{diagnostics}");
    with_concrete_borrow_summary(source, "returned", |db, summary| {
        let values = ValueInterner::new(db, ValueLimits::default());
        let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
        assert!(!leaves.is_empty());
        for leaf in leaves {
            assert!(
                matches!(leaf.payload.source.origin, ExternalOrigin::Allocation(_)),
                "{:?}",
                leaf.payload
            );
        }
    });
}

#[test]
fn scalar_field_writes_preserve_buffer_pointer_provenance() {
    let source = r#"
use std::evm::{Packed, RawMem}
fn filled() -> Packed uses (mem: mut RawMem) {
    let mut value = Packed::with_capacity(bytes: 64)
    value.u256(1)
    value.u256(2)
    value
}
"#;
    let diagnostics = checked_borrow_diags(source);
    assert!(diagnostics.is_empty(), "{diagnostics}");
    with_concrete_borrow_summary(source, "filled", |db, summary| {
        let values = ValueInterner::new(db, ValueLimits::default());
        let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
        assert!(!leaves.is_empty());
        assert!(
            leaves
                .iter()
                .all(|leaf| matches!(leaf.payload.source.origin, ExternalOrigin::Allocation(_)))
        );
    });
}

#[test]
fn blocked_invalid_body_is_not_admitted_by_semantic_consumers() {
    let source = r#"
fn invalid(result: mut u256) -> mut u256 uses (values: mut [mut u256; 2]) {
    missing = 1
    result
}

fn caller(result: mut u256) -> mut u256 uses (values: mut [mut u256; 2]) {
    invalid(result)
}
"#;
    assert!(borrow_diags(source).is_empty());

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let instance_for = |name: &str| {
        top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|func_name| func_name.data(&db) == name) =>
                {
                    Some(get_or_build_semantic_instance(
                        &db,
                        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                    ))
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing `{name}` function"))
    };
    let instance = instance_for("invalid");

    let SemanticBodyAdmission::Blocked(blocked) = semantic_body_admission(&db, instance) else {
        panic!("invalid body should be blocked before normalization")
    };
    assert_eq!(blocked.instance, instance);
    assert!(!blocked.causes.is_empty());
    assert!(matches!(
        normalize_semantic_body(&db, instance),
        Err(SemanticNormalizationFailure::Blocked(_))
    ));
    assert!(matches!(
        check_semantic_borrows(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        check_semantic_boundaries(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        semantic_borrow_summary(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::Blocked(_))
    ));
    assert!(matches!(
        canonicalize_semantic_consts(&db, instance),
        Err(CtfeError::InvalidBody { .. })
    ));

    let caller = instance_for("caller");
    let invalid_owner = instance.key(&db).owner(&db);
    let caller_owner = caller.key(&db).owner(&db);
    let Err(SemanticAnalysisError::Blocked(blocked)) = semantic_borrow_summary(&db, caller) else {
        panic!("blocked callee summary must keep its status through the caller")
    };
    assert_eq!(blocked.instance.key(&db).owner(&db), invalid_owner);
    assert_ne!(blocked.instance.key(&db).owner(&db), caller_owner);
    let Err(SemanticAnalysisError::Blocked(blocked)) = check_semantic_borrows(&db, caller) else {
        panic!("blocked callee analysis must keep its status through the caller")
    };
    assert_eq!(blocked.instance.key(&db).owner(&db), invalid_owner);
    assert_ne!(blocked.instance.key(&db).owner(&db), caller_owner);
}

#[test]
fn blocked_contract_body_keeps_its_declaration_layout_signature() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Rooted<const ROOT: u256 = _> {}

pub contract InvalidInit {
    values: [Rooted; 2]

    init() uses (values) {
        missing
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "InvalidInit");
    assert!(matches!(
        semantic_body_admission(&db, instance),
        SemanticBodyAdmission::Blocked(_)
    ));
    let BodyOwner::ContractInit { contract } = instance.key(&db).owner(&db) else {
        unreachable!()
    };
    assert!(matches!(
        contract_init_assigned_fields(&db, contract),
        Err(SemanticNormalizationFailure::Blocked(_))
    ));

    let signature = instance.key(&db).layout_bundle_signature(&db);
    assert_eq!(signature.inputs.len(), 1);
    assert!(!signature.inputs[0].interface.schema.components.is_empty());
}

#[test]
fn generated_zero_field_abi_bodies_are_admitted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
msg Empty {
    #[selector = 1]
    Ping,
}

#[error]
struct EmptyError {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let abi_funcs = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .filter(|func| {
            func.name(&db)
                .to_opt()
                .is_some_and(|name| matches!(name.data(&db).as_str(), "payload_size" | "encode"))
        })
        .collect::<Vec<_>>();
    assert_eq!(abi_funcs.len(), 4);
    for func in abi_funcs {
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        assert!(matches!(
            semantic_body_admission(&db, instance),
            SemanticBodyAdmission::Ready(_)
        ));
    }
}

fn assert_borrow_conflict(source: &str) {
    let diagnostics = checked_borrow_diags(source);
    assert!(diagnostics.contains("borrow conflict"), "{diagnostics}");
    assert!(
        !diagnostics.contains("internal borrow checking error"),
        "{diagnostics}"
    );
}

fn assert_mut_borrow_conflict(src: &str) {
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

fn assert_no_borrow_conflict(src: &str) {
    let diags = borrow_diags(src);
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn pointer_store_through_cast_is_valid() {
    assert_no_borrow_conflict(
        r#"
use core::ptr

fn store_word(ptr: *u8, value: u256) {
    *ptr::cast<u8, u256>(ptr) = value
}
"#,
    );
}

fn contract_init_instance<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    contract_name: &str,
) -> SemanticInstance<'db> {
    top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Contract(contract)
                if contract
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == contract_name) =>
            {
                Some(get_or_build_semantic_instance(
                    db,
                    identity_semantic_instance_key(
                        db,
                        BodyOwner::ContractInit {
                            contract: *contract,
                        },
                    ),
                ))
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing contract init `{contract_name}`"))
}

fn mixed_returned_borrow_provenance_src() -> &'static str {
    r#"
struct Ledger {
    b: u256,
}

impl Ledger {
    fn pick_mixed(mut self, cond: bool, value: mut u256) -> mut u256 {
        if cond {
            value
        } else {
            mut self.b
        }
    }
}

fn add(by: u256) -> u256 uses (value: mut u256) {
    value += by
    value
}

pub contract Mixed {
    mut ledger: Ledger

    init() uses (mut ledger) {
        let mut local: u256 = 0
        let target = ledger.pick_mixed(cond: true, value: mut local)
        with (target) {
            add(by: 1)
        }
    }
}
"#
}

fn for_each_fixture_instance(
    src: &str,
    mut f: impl FnMut(&HirAnalysisTestDb, SemanticInstance<'_>),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let mut pending = VecDeque::new();

    for item in top_mod.all_items(&db) {
        match item {
            ItemKind::Func(func) => pending.push_back(get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
            )),
            ItemKind::Contract(contract) => {
                pending.push_back(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(
                        &db,
                        BodyOwner::ContractInit {
                            contract: *contract,
                        },
                    ),
                ));
                for (recv_idx, recv) in contract.recvs(&db).data(&db).iter().enumerate() {
                    for arm_idx in 0..recv.arms.data(&db).len() {
                        pending.push_back(get_or_build_semantic_instance(
                            &db,
                            identity_semantic_instance_key(
                                &db,
                                BodyOwner::ContractRecvArm {
                                    contract: *contract,
                                    recv_idx: recv_idx as u32,
                                    arm_idx: arm_idx as u32,
                                },
                            ),
                        ));
                    }
                }
            }
            ItemKind::Const(_)
            | ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::TopMod(_)
            | ItemKind::Body(_) => {}
        }
    }

    let mut seen = rustc_hash::FxHashSet::default();
    while let Some(instance) = pending.pop_front() {
        if !seen.insert(instance.key(&db)) {
            continue;
        }
        f(&db, instance);
        for callee in instance.callees(&db) {
            pending.push_back(get_or_build_semantic_instance(&db, callee.key));
        }
    }
}

fn owner_name(db: &HirAnalysisTestDb, owner: BodyOwner<'_>) -> String {
    match owner {
        BodyOwner::Func(func) => match func.name(db) {
            Partial::Present(name) => name.data(db).to_string(),
            Partial::Absent => "<fn>".to_string(),
        },
        BodyOwner::Const(const_) => match const_.name(db) {
            Partial::Present(name) => name.data(db).to_string(),
            Partial::Absent => "<const>".to_string(),
        },
        BodyOwner::AnonConstBody { .. } => "<anon const>".to_string(),
        BodyOwner::ContractInit { contract } => match contract.name(db) {
            Partial::Present(name) => format!("{}::__init__", name.data(db)),
            Partial::Absent => "<contract>::__init__".to_string(),
        },
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => match contract.name(db) {
            Partial::Present(name) => format!("{}::recv[{recv_idx}][{arm_idx}]", name.data(db)),
            Partial::Absent => format!("<contract>::recv[{recv_idx}][{arm_idx}]"),
        },
    }
}

fn normalized_func_body<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    func_name: &str,
) -> NormalizedArtifacts<'db> {
    let instance = top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == func_name) =>
            {
                Some(get_or_build_semantic_instance(
                    db,
                    identity_semantic_instance_key(db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{func_name}`"));
    normalize_semantic_body(db, instance).expect("normalized body")
}

#[test]
fn self_referential_param_layout_backing_sources_use_the_param_root() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Pair {
    x: u256,
    y: u256,
}

fn rebuild(mut _ value: own Pair) -> Pair {
    let x = value.x
    value = Pair { x, y: value.y }
    value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "rebuild");
    let source = normalized.body.owner.body(&db);
    let param_local = source
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| matches!(local.source, Some(LocalBinding::Param { idx: 0, .. })))
        .map(|(index, _)| fe_hir::analysis::semantic::SLocalId::new(index))
        .expect("missing value parameter");
    let param_root = normalized
        .body
        .roots
        .iter()
        .enumerate()
        .find_map(|(index, root)| {
            (matches!(root.kind, NRootKind::ParamPlace { param: 0 })
                && normalized
                    .layout_plan
                    .root_source(fe_hir::analysis::semantic::NRootId::new(index))
                    == Some(param_local))
            .then_some(fe_hir::analysis::semantic::NRootId::new(index))
        })
        .expect("mutable owned aggregate parameter must have a root");

    assert!(
        normalized
            .layout_plan
            .use_backings
            .iter()
            .any(|source| matches!(source.source, fe_hir::analysis::semantic::NLayoutBackingSource::Root { root, .. } if root == param_root))
    );
}

fn func_instance<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: fe_hir::hir_def::TopLevelMod<'db>,
    func_name: &str,
) -> SemanticInstance<'db> {
    top_mod
        .all_items(db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == func_name) =>
            {
                Some(get_or_build_semantic_instance(
                    db,
                    identity_semantic_instance_key(db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{func_name}`"))
}

fn with_borrow_summary(
    src: &str,
    func_name: &str,
    f: impl for<'db> FnOnce(&'db HirAnalysisTestDb, BorrowSummary<'db>),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let instance = func_instance(&db, top_mod, func_name);
    let summary = semantic_borrow_summary(&db, instance)
        .expect("borrow summary")
        .expect("borrow-returning function should produce a summary");
    f(&db, summary);
}

fn with_concrete_borrow_summary(
    src: &str,
    name: &str,
    check: impl for<'db> FnOnce(&'db HirAnalysisTestDb, BorrowSummary<'db>),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("concrete_summary.fe".into(), src);
    let (module, _) = db.top_mod(file);
    let key = root_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name)))
        .expect("concrete root specialization");
    let instance = get_or_build_semantic_instance(&db, key);
    check_semantic_borrows(&db, instance).unwrap();
    check(
        &db,
        semantic_borrow_summary(&db, instance).unwrap().unwrap(),
    );
}

#[test]
fn memory_summary_combines_access_store_and_return_provenance() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
fn replace_and_return(slot: **u256, value: *u256) -> *u256 {
    *slot = value
    *slot
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = func_instance(&db, top_mod, "replace_and_return");
    let summary = semantic_borrow_summary(&db, instance)
        .expect("summary")
        .expect("return summary");
    let values = ValueInterner::new(&db, ValueLimits::default());
    let returned = values.leaves(&summary.result, ValueOccurrence::Summary);
    assert!(summary.may_return);
    assert_eq!(returned.len(), 1);
    assert_eq!(
        returned[0].payload.source.origin,
        ExternalOrigin::Input(InputSource::slot(1, StructuralPath::default()))
    );
    assert_eq!(summary.mutable_inputs.len(), 1);
    assert_eq!(
        summary.mutable_inputs[0].destination.source.origin,
        ExternalOrigin::Input(InputSource::slot(0, StructuralPath::default()))
    );
    assert_eq!(summary.mutable_inputs[0].value, summary.result);
    for kind in [MemoryAccessKind::Read, MemoryAccessKind::Write] {
        assert!(summary.accesses.iter().any(|access| access.kind == kind && access.region.clauses().iter().any(|clause| matches!(&clause.payload.root, fe_hir::analysis::semantic::capability::region::RegionRoot::External(source) if source.param() == Some(0)))));
    }
}

#[test]
fn memory_summary_records_raw_mem_separately_from_pointer_read() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
use core::ptr
use std::evm::RawMem

fn direct_read(p: *u256) -> u256 {
    *p
}

fn raw_mem_read(p: *u256) -> u256 uses (mem: RawMem) {
    mem.mload(ptr::byte_ptr(p))
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    for (func_name, has_authority) in [("direct_read", false), ("raw_mem_read", true)] {
        let instance = func_instance(&db, top_mod, func_name);
        let summary = semantic_borrow_summary(&db, instance)
            .expect("summary")
            .expect("access summary");
        let access = summary.accesses.iter().find(|access| access.kind == MemoryAccessKind::Read && access.region.clauses().iter().any(|clause| matches!(&clause.payload.root, fe_hir::analysis::semantic::capability::region::RegionRoot::External(source) if source.param() == Some(0)))).expect("pointee access");
        assert_eq!(
            !access.authorizers.is_empty(),
            has_authority,
            "{summary:#?}"
        );
    }
}

#[test]
fn branch_return_borrow_summary_flows_through_empty_entry_blocks() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Ledger {
    a: u256,
    b: u256,
    c: u256,
}

impl Ledger {
    fn pick(mut self, _ pick_c: bool) -> mut u256 {
        if pick_c {
            mut self.c
        } else {
            mut self.a
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "pick") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("pick instance");
    let summary = semantic_borrow_summary(&db, instance)
        .expect("borrow summary")
        .expect("borrow-returning function should produce a summary");
    let values = ValueInterner::new(&db, ValueLimits::default());
    let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
    assert_eq!(leaves.len(), 2, "unexpected summary: {summary:#?}");
    for field in [0, 2] {
        assert!(leaves.iter().any(|leaf| leaf.payload.source.origin
            == ExternalOrigin::Input(InputSource::slot(0, StructuralPath::default()))
            && leaf.payload.path
                == RegionPath::new([CapabilityProjection::Field(
                    fe_hir::analysis::semantic::FieldIndex(field)
                )])));
    }
    check_semantic_borrows(&db, instance).expect("borrowck should accept branch-returned borrow");
}

#[test]
fn pointer_returned_borrow_uses_matching_pointer_param() {
    with_borrow_summary(
        r#"
fn second(_ a: *u256, b: *u256) -> mut u256 {
    let q = b
    mut *q
}
"#,
        "second",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(1, StructuralPath::default()))
            );
            assert_eq!(leaves[0].payload.path, RegionPath::default());
        },
    );
}

#[test]
fn pointer_local_copy_conflicts_with_original_pointer_borrow() {
    let diags = borrow_diags(
        r#"
fn bad(p: *u256) {
    let q = p
    let a = mut *q
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn pointer_field_aggregate_conflicts_with_original_pointer_borrow() {
    let diags = borrow_diags(
        r#"
struct Holder {
    ptr: *u256,
}

fn bad(p: *u256) {
    let h = Holder { ptr: p }
    let a = mut *h.ptr
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn pointer_field_access_does_not_move_noncopy_holder() {
    let diags = borrow_diags(
        r#"
struct Data {
    a: u256,
    b: u256,
}

struct Holder {
    data: *Data,
}

fn ok() {
    let data = core::ptr::alloc<Data>()
    data.a = 5
    data.b = 8
    let words = core::ptr::cast<Data, u256>(data)
    let second = core::ptr::offset(words, 1)
    *second = *second + 3
    assert(data.a == 5)
    assert(data.b == 11)
    let holder = Holder { data: data }
    holder.data.b = holder.data.a + holder.data.b
    assert(holder.data.b == 16)
    assert(data.b == 16)
}
"#,
    );
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn moving_pointer_bearing_aggregate_re_roots_destination() {
    let source = r#"
struct Holder {
    ptr: *u8,
    len: u256,
}

impl Holder {
    fn write(mut self, _ value: u8) {
        *self.ptr = value
        self.len = 1
    }
}

fn transfer(_ input: own Holder) -> Holder {
    let mut output = input
    output.write(7)
    output
}
"#;
    let diags = borrow_diags(source);
    assert!(diags.is_empty(), "{diags}");

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "transfer");
    assert!(
        normalized
            .body
            .roots
            .iter()
            .any(|root| matches!(root.kind, NRootKind::LocalSlot { .. })
                && root.mutability == fe_hir::analysis::semantic::Mutability::Mutable),
        "moved destination must have its own mutable container root: {normalized:#?}"
    );
}

#[test]
fn moving_pointer_bearing_aggregate_still_moves_its_fields() {
    let diags = borrow_diags(
        r#"
struct Holder {
    ptr: *u8,
    len: u256,
}

fn bad(mut _ input: own Holder) -> Holder {
    let output = input
    input.len = 1
    output
}
"#,
    );
    assert!(diags.contains("move conflict in `fn bad`"), "{diags:?}");
    assert!(
        diags.contains("cannot write through a moved value"),
        "{diags:?}"
    );
}

#[test]
fn pointer_returned_borrow_uses_matching_aggregate_param() {
    with_borrow_summary(
        r#"
struct Holder {
    ptr: *u256,
}

fn pick(_ a: *u256, h: Holder) -> mut u256 {
    mut *h.ptr
}
"#,
        "pick",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(
                    1,
                    StructuralPath::new([CapabilityProjection::Field(FieldIndex(0))])
                ))
            );
            assert_eq!(leaves[0].payload.path, RegionPath::default());
        },
    );
}

#[test]
fn call_summary_deref_transform_uses_caller_pointer_provenance() {
    let diags = borrow_diags(
        r#"
struct Holder {
    ptr: *u256,
}

fn pick(h: Holder) -> mut u256 {
    mut *h.ptr
}

fn bad(p: *u256) {
    let h = Holder { ptr: p }
    let a = pick(h)
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn mem_span_from_raw_parts_preserves_pointer_provenance() {
    let diags = borrow_diags(
        r#"
fn bad(p: *u256, len: u256) {
    let span = core::ptr::MemSpan::from_raw_parts(ptr: core::ptr::byte_ptr(p), len: len)
    let x = mut *p
    *span.ptr() = 1
    x = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn mem_buffer_preserves_fresh_pointer_provenance() {
    assert_no_borrow_conflict(
        r#"
fn read_buffer(buffer: own core::ptr::MemBuffer) -> u8 {
    *buffer.ptr()
}

fn ok(p: *u256) {
    let borrowed = mut *p
    let buffer = core::ptr::MemBuffer::alloc(32)
    let value = read_buffer(buffer)
    assert(value == 0)
    borrowed = 1
}
"#,
    );
}

#[test]
fn encoded_mem_buffer_preserves_fresh_pointer_provenance() {
    assert_no_borrow_conflict(
        r#"
fn read_encoded(args: own (u256, u256)) -> u8 {
    let buffer = std::evm::encode_abi_payload<(u256, u256)>(args)
    *buffer.ptr()
}

fn ok(p: *u256) {
    let borrowed = mut *p
    let value = read_encoded((1, 2))
    assert(value == 0)
    borrowed = 1
}
"#,
    );
}

#[test]
fn encode_alloc_returns_fresh_pointer_provenance() {
    with_borrow_summary(
        r#"
fn encoded(args: own (u256, u256)) -> *u8 {
    core::abi::encode_alloc<std::abi::Sol, (u256, u256)>(args).ptr()
}
"#,
        "encoded",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert!(
                matches!(
                    leaves[0].payload.source.origin,
                    ExternalOrigin::Allocation(_)
                ),
                "{summary:#?}"
            );
        },
    );
}

#[test]
fn writing_fresh_allocation_preserves_pointer_value_provenance() {
    with_borrow_summary(
        r#"
fn fresh() -> *u8 {
    let ptr = core::ptr::alloc_bytes(32)
    *ptr = 1
    ptr
}
"#,
        "fresh",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert!(
                matches!(
                    leaves[0].payload.source.origin,
                    ExternalOrigin::Allocation(_)
                ),
                "{summary:#?}"
            );
        },
    );
}

#[test]
fn mem_array_returned_borrow_uses_matching_array_param() {
    with_borrow_summary(
        r#"
fn second_array(
    _ a: core::ptr::MemArray<u256>,
    b: core::ptr::MemArray<u256>,
) -> mut u256 {
    mut *b.ptr()
}
"#,
        "second_array",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(
                    1,
                    StructuralPath::new([CapabilityProjection::Field(FieldIndex(0))])
                ))
            );
            assert_eq!(leaves[0].payload.path, RegionPath::default());
        },
    );
}

#[test]
fn pointer_array_returned_borrow_preserves_formal_index() {
    with_borrow_summary(
        r#"
fn elem(array: *[u256; 64], i: usize) -> mut u256 {
    mut (*array)[i]
}
"#,
        "elem",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(0, StructuralPath::default()))
            );
            assert_eq!(
                leaves[0].payload.path,
                RegionPath::new([CapabilityProjection::Index(IndexExpr::FormalValue(1))])
            );
        },
    );
}

#[test]
fn pointer_array_returned_borrow_conflicts_with_constant_element() {
    assert_mut_borrow_conflict(
        r#"
fn elem(array: *[u256; 64], i: usize) -> mut u256 {
    mut (*array)[i]
}

fn bad(array: *[u256; 64], i: usize) {
    let a = elem(array, i)
    let b = mut (*array)[0]
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn store_through_pointer_conflicts_with_active_pointee_borrow() {
    assert_mut_borrow_conflict(
        r#"
fn bad(p: *u256) {
    let a = mut *p
    *p = 1
    a = 2
}
"#,
    );
}

#[test]
fn read_through_pointer_conflicts_with_active_mut_borrow() {
    let diags = borrow_diags(
        r#"
fn bad(p: *u256) {
    let borrowed = mut *p
    let value = *p
    assert(value == 0)
    borrowed = 1
}
"#,
    );
    assert!(diags.contains("borrow conflict"), "{diags:?}");
    assert!(diags.contains("cannot immutably borrow"), "{diags:?}");
}

#[test]
fn call_read_through_pointer_conflicts_with_active_mut_borrow() {
    let diags = borrow_diags(
        r#"
fn read(p: *u256) -> u256 {
    *p
}

fn bad(p: *u256) {
    let borrowed = mut *p
    let value = read(p)
    assert(value == 0)
    borrowed = 1
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot immutably borrow"), "{diags:?}");
}

#[test]
fn call_read_through_pointer_allows_active_ref_borrow() {
    assert_no_borrow_conflict(
        r#"
fn read(p: *u256) -> u256 {
    *p
}

fn ok(p: *u256) {
    let borrowed: ref u256 = ref *p
    let value = read(p)
    assert(borrowed == value)
}
"#,
    );
}

#[test]
fn call_write_through_pointer_conflicts_with_active_borrow() {
    assert_mut_borrow_conflict(
        r#"
fn write(p: *u256) {
    *p = 1
}

fn bad(p: *u256) {
    let borrowed = mut *p
    write(p)
    borrowed = 2
}
"#,
    );
}

#[test]
fn passing_mut_borrow_does_not_authorize_other_pointer_access() {
    assert_mut_borrow_conflict(
        r#"
fn write_other(_ allowed: mut u256, other: *u256) {
    *other = 1
}

fn bad(p: *u256) {
    let borrowed = mut *p
    write_other(mut borrowed, p)
    borrowed = 2
}
"#,
    );
}

#[test]
fn call_through_mut_borrow_handle_remains_allowed() {
    assert_no_borrow_conflict(
        r#"
fn write(_ value: mut u256) {
    value = 1
}

fn ok(p: *u256) {
    let borrowed = mut *p
    write(mut borrowed)
    borrowed = 2
}
"#,
    );
}

#[test]
fn call_write_through_unrelated_pointer_allows_active_borrow() {
    assert_no_borrow_conflict(
        r#"
fn write(p: *u256) {
    *p = 1
}

fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let borrowed = mut *q
    write(p)
    borrowed = 2
}
"#,
    );
}

#[test]
fn bodyless_pointer_call_conservatively_conflicts_with_active_borrow() {
    assert_mut_borrow_conflict(
        r#"
extern {
    fn touch(p: *u256)
}

fn bad(p: *u256) {
    let borrowed = mut *p
    touch(p)
    borrowed = 1
}
"#,
    );
}

#[test]
fn bodyless_pointer_call_cannot_assume_argument_bounded_effects() {
    assert_borrow_conflict(
        r#"
extern {
    fn touch(p: *u256)
}

fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let borrowed = mut *q
    touch(p)
    borrowed = 1
}
"#,
    );
}

#[test]
fn signature_fallback_without_arguments_can_touch_existing_storage() {
    let diagnostics = checked_borrow_diags(
        r#"
extern { fn unknown() }
fn bad() {
    let pointer = core::ptr::alloc<u256>()
    let native = mut *pointer
    unknown()
    native = 2
}
"#,
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn bad`"),
        "{diagnostics}"
    );
}

#[test]
fn bodyless_pointer_call_conflicts_with_nested_pointee_borrow() {
    assert_mut_borrow_conflict(
        r#"
struct Holder {
    child: *u256,
}

extern {
    fn touch(holder: *Holder)
}

fn bad() {
    let child = core::ptr::alloc<u256>()
    let holder = core::ptr::alloc<Holder>()
    holder.child = child
    let borrowed = mut *child
    touch(holder)
    borrowed = 1
}
"#,
    );
}

#[test]
fn bodyless_pointer_bearing_value_call_conflicts_with_pointee_borrow() {
    assert_mut_borrow_conflict(
        r#"
struct Holder {
    child: *u256,
}

extern {
    fn touch(holder: own Holder)
}

fn bad() {
    let child = core::ptr::alloc<u256>()
    let holder = Holder { child }
    let borrowed = mut *child
    touch(holder)
    borrowed = 1
}
"#,
    );
}

#[test]
fn recursive_pointer_call_propagates_memory_effects() {
    assert_mut_borrow_conflict(
        r#"
fn write_recursive(n: u256, p: *u256) {
    if n == 0 {
        *p = 1
        return
    }
    write_recursive(n - 1, p)
}

fn bad(p: *u256) {
    let borrowed = mut *p
    write_recursive(1, p)
    borrowed = 2
}
"#,
    );
}

#[test]
fn recursive_pointer_call_without_memory_effects_remains_pure() {
    assert_no_borrow_conflict(
        r#"
fn recurse(n: u256, p: *u256) {
    if n > 0 {
        recurse(n - 1, p)
    }
}

fn ok(p: *u256) {
    let borrowed = mut *p
    recurse(1, p)
    borrowed = 1
}
"#,
    );
}

#[test]
fn recursive_pointer_slot_write_retains_precise_provenance() {
    assert_no_borrow_conflict(
        r#"
fn replace(n: u256, slot: **u256, value: *u256) {
    if n == 0 {
        *slot = value
        return
    }
    replace(n - 1, slot, value)
}

fn ok() {
    let old = core::ptr::alloc<u256>()
    let new = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = old
    let old_borrow = mut *old
    replace(1, slot, new)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    old_borrow = 2
}
"#,
    );
}

#[test]
fn call_pointer_slot_write_updates_caller_provenance() {
    assert_mut_borrow_conflict(
        r#"
fn replace(slot: **u256, value: *u256) {
    *slot = value
}

fn bad() {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = second
    let first_borrow = mut *first
    replace(slot, first)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    first_borrow = 2
}
"#,
    );
}

#[test]
fn call_pointer_effect_slot_write_updates_caller_provenance() {
    assert_mut_borrow_conflict(
        r#"
fn replace(_ slot: mut *u256, value: *u256) {
    slot = value
}

fn bad() {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = second
    let first_borrow = mut *first
    replace(mut *slot, first)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    first_borrow = 2
}
"#,
    );
}

#[test]
fn precise_call_pointer_effect_slot_write_drops_old_provenance() {
    assert_no_borrow_conflict(
        r#"
fn replace(_ slot: mut *u256, value: *u256) {
    slot = value
}

fn ok() {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let third = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = second
    let first_borrow = mut *first
    replace(mut *slot, third)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    first_borrow = 2
}
"#,
    );
}

#[test]
fn precise_call_pointer_slot_write_drops_old_provenance() {
    assert_no_borrow_conflict(
        r#"
fn replace(slot: **u256, value: *u256) {
    *slot = value
}

fn ok() {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let third = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = second
    let first_borrow = mut *first
    replace(slot, third)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    first_borrow = 2
}
"#,
    );
}

#[test]
fn transitive_call_pointer_slot_write_updates_caller_provenance() {
    assert_mut_borrow_conflict(
        r#"
fn replace(slot: **u256, value: *u256) {
    *slot = value
}

fn forward_replace(slot: **u256, value: *u256) {
    replace(slot, value)
}

fn bad() {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = second
    let first_borrow = mut *first
    forward_replace(slot, first)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    first_borrow = 2
}
"#,
    );
}

#[test]
fn conditional_call_pointer_slot_write_keeps_old_and_new_provenance() {
    assert_mut_borrow_conflict(
        r#"
fn maybe_replace(cond: bool, slot: **u256, value: *u256) {
    if cond {
        *slot = value
    }
}

fn bad(cond: bool) {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = second
    let first_borrow = mut *first
    maybe_replace(cond, slot, first)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    first_borrow = 2
}
"#,
    );
    assert_mut_borrow_conflict(
        r#"
fn maybe_replace(cond: bool, slot: **u256, value: *u256) {
    if cond {
        *slot = value
    }
}

fn bad(cond: bool) {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = second
    let second_borrow = mut *second
    maybe_replace(cond, slot, first)
    let slot_borrow = mut *(*slot)
    slot_borrow = 1
    second_borrow = 2
}
"#,
    );
}

#[test]
fn call_pointer_slot_write_conflicts_with_active_slot_borrow() {
    assert_mut_borrow_conflict(
        r#"
fn replace(slot: **u256, value: *u256) {
    *slot = value
}

fn bad(value: *u256) {
    let slot = core::ptr::alloc<*u256>()
    let borrowed = mut *slot
    replace(slot, value)
    borrowed = value
}
"#,
    );
}

#[test]
fn pointer_slot_read_does_not_conflict_with_pointee_borrow() {
    assert_no_borrow_conflict(
        r#"
fn ok() {
    let pointee = core::ptr::alloc<u256>()
    let slot = core::ptr::alloc<*u256>()
    *slot = pointee
    let borrowed = mut *(*slot)
    let copied = *slot
    assert(copied == pointee)
    borrowed = 1
}
"#,
    );
}

#[test]
fn store_through_active_borrow_handle_is_allowed() {
    assert_no_borrow_conflict(
        r#"
fn ok(p: *u256) {
    let a = mut *p
    a = 1
}
"#,
    );
}

#[test]
fn normalized_verifier_rejects_analysis_only_any_index() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
fn write(array: *[u256; 2]) {
    (*array)[0] = 1
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = func_instance(&db, top_mod, "write");
    let mut raw = instance.body(&db).clone();
    let mut replaced = false;
    for block in &mut raw.blocks {
        for statement in &mut block.stmts {
            if let SStmtKind::Store { dst, .. } = &mut statement.kind {
                let mut path = fe_hir::projection::ProjectionPath::new();
                for projection in dst.path.iter() {
                    if matches!(projection, Projection::Index(_)) {
                        path.push(Projection::Index(fe_hir::projection::IndexSource::Any));
                        replaced = true;
                    } else {
                        path.push(projection.clone());
                    }
                }
                dst.path = path;
            }
        }
    }
    assert!(replaced, "fixture must contain an indexed store");
    let error = normalize_raw_body(&db, instance, &raw, instance.assumptions(&db))
        .expect_err("wildcards cannot enter executable normalized paths");
    assert!(
        format!("{error:?}").contains("UnsupportedPlaceProjection"),
        "{error:?}"
    );
}

#[test]
fn scalar_store_conflicts_with_active_local_borrow() {
    assert_mut_borrow_conflict(
        r#"
fn bad() {
    let mut x: u256 = 0
    let a = mut x
    x = 1
    a = 2
}
"#,
    );
}

#[test]
fn raw_pointer_offset_preserves_input_allocation_provenance() {
    with_borrow_summary(
        r#"
fn borrow_at(p: *u256, i: usize) -> mut u256 {
    let q = core::ptr::offset<u256>(p, i as u256)
    mut *q
}
"#,
        "borrow_at",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(leaves[0].payload.source.param(), Some(0));
            assert!(matches!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Memory { .. }
            ));
        },
    );
}

#[test]
fn core_pointer_cast_preserves_pointee_provenance() {
    with_borrow_summary(
        r#"
fn borrow_cast(p: *u256) -> mut u8 {
    let q = core::ptr::cast<u256, u8>(p)
    mut *q
}
"#,
        "borrow_cast",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(leaves[0].payload.source.param(), Some(0));
            assert!(matches!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Memory { .. }
            ));
        },
    );
}

#[test]
fn pointer_field_store_updates_pointer_provenance() {
    let diags = borrow_diags(
        r#"
struct Holder {
    ptr: *u256,
}

fn bad(p: *u256, q: *u256) {
    let mut h = Holder { ptr: q }
    h.ptr = p
    let a = mut *h.ptr
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn raw_pointer_params_may_alias() {
    assert_mut_borrow_conflict(
        r#"
fn bad(a: *u256, b: *u256) {
    let x = mut *a
    let y = mut *b
    y = 1
    x = 2
}
"#,
    );
}

#[test]
fn pointer_aggregate_overwrite_clears_stale_pointer_provenance() {
    let diags = borrow_diags(
        r#"
struct Holder {
    ptr: *u256,
}

fn ok(q: *u256) {
    let p = core::ptr::alloc<u256>()
    let mut h = Holder { ptr: p }
    h = Holder { ptr: q }
    let a = mut *h.ptr
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn scalar_store_through_cast_pointer_invalidates_pointer_provenance() {
    assert_mut_borrow_conflict(
        r#"
struct Holder {
    ptr: *u256,
}

fn bad(q: *u256) {
    let p = core::ptr::alloc<u256>()
    let h = core::ptr::alloc<Holder>()
    (*h).ptr = q
    let word = core::ptr::cast<Holder, u256>(h)
    *word = 0
    let r = (*h).ptr
    let x = mut *r
    let y = mut *p
    y = 1
    x = 2
}
"#,
    );
}

#[test]
fn pointer_returning_aggregate_call_preserves_field_provenance() {
    let diags = borrow_diags(
        r#"
struct Holder {
    ptr: *u256,
}

fn wrap(p: *u256) -> Holder {
    Holder { ptr: p }
}

fn bad(p: *u256) {
    let h = wrap(p)
    let a = mut *h.ptr
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn pointer_pointee_read_from_view_param_is_not_move_from_param() {
    let diags = borrow_diags(
        r#"
fn read(p: *u256) -> u256 {
    *p
}
"#,
    );
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn fresh_pointer_return_does_not_alias_input_pointer() {
    let diags = borrow_diags(
        r#"
fn fresh(_ p: *u256) -> *u256 {
    core::ptr::alloc<u256>()
}

fn ok(p: *u256) {
    let q = fresh(p)
    let a = mut *q
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn unknown_raw_address_conflicts_with_input_pointer() {
    assert_mut_borrow_conflict(
        r#"
extern {
    fn unknown_ptr<T>() -> *T
}

fn bad(p: *u256) {
    let q = unknown_ptr<u256>()
    let a = mut *q
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn branching_pointer_return_summary_conflicts_with_each_input() {
    assert_mut_borrow_conflict(
        r#"
fn pick(cond: bool, p: *u256, q: *u256) -> *u256 {
    if cond {
        p
    } else {
        q
    }
}

fn bad(cond: bool, p: *u256, q: *u256) {
    let r = pick(cond, p, q)
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert_mut_borrow_conflict(
        r#"
fn pick(cond: bool, p: *u256, q: *u256) -> *u256 {
    if cond {
        p
    } else {
        q
    }
}

fn bad(cond: bool, p: *u256, q: *u256) {
    let r = pick(cond, p, q)
    let a = mut *r
    let b = mut *q
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn branching_pointer_aggregate_return_summary_conflicts_with_each_input() {
    assert_mut_borrow_conflict(
        r#"
struct Holder {
    ptr: *u256,
}

fn wrap(cond: bool, p: *u256, q: *u256) -> Holder {
    if cond {
        Holder { ptr: p }
    } else {
        Holder { ptr: q }
    }
}

fn bad(cond: bool, p: *u256, q: *u256) {
    let h = wrap(cond, p, q)
    let a = mut *h.ptr
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert_mut_borrow_conflict(
        r#"
struct Holder {
    ptr: *u256,
}

fn wrap(cond: bool, p: *u256, q: *u256) -> Holder {
    if cond {
        Holder { ptr: p }
    } else {
        Holder { ptr: q }
    }
}

fn bad(cond: bool, p: *u256, q: *u256) {
    let h = wrap(cond, p, q)
    let a = mut *h.ptr
    let b = mut *q
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn branching_mem_array_return_summary_conflicts_with_each_input() {
    assert_borrow_conflict(
        r#"
fn pick(
    cond: bool,
    a: core::ptr::MemArray<u256>,
    b: core::ptr::MemArray<u256>,
) -> core::ptr::MemArray<u256> {
    if cond {
        a
    } else {
        b
    }
}

fn bad(
    cond: bool,
    a: core::ptr::MemArray<u256>,
    b: core::ptr::MemArray<u256>,
) {
    let r = pick(cond, a, b)
    let x = mut *r.ptr()
    let y = mut *a.ptr()
    y = 1
    x = 2
}
"#,
    );
    assert_borrow_conflict(
        r#"
fn pick(
    cond: bool,
    a: core::ptr::MemArray<u256>,
    b: core::ptr::MemArray<u256>,
) -> core::ptr::MemArray<u256> {
    if cond {
        a
    } else {
        b
    }
}

fn bad(
    cond: bool,
    a: core::ptr::MemArray<u256>,
    b: core::ptr::MemArray<u256>,
) {
    let r = pick(cond, a, b)
    let x = mut *r.ptr()
    let y = mut *b.ptr()
    y = 1
    x = 2
}
"#,
    );
}

#[test]
fn pointer_array_literal_propagates_element_provenance() {
    assert_mut_borrow_conflict(
        r#"
fn bad(p: *u256, q: *u256) {
    let arr = [p, q]
    let r = arr[0]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn pointer_array_repeat_propagates_element_provenance() {
    assert_mut_borrow_conflict(
        r#"
fn bad(p: *u256) {
    let arr = [p; 2]
    let r = arr[1]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn dynamic_pointer_array_write_weakly_updates_possible_slots() {
    assert_mut_borrow_conflict(
        r#"
fn bad(p: *u256, q: *u256, i: usize) {
    let mut arr = [q, q]
    arr[i] = p
    let r = arr[0]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn dynamic_pointer_array_read_joins_possible_slots() {
    assert_mut_borrow_conflict(
        r#"
fn bad(p: *u256, q: *u256, i: usize) {
    let arr = [p, q]
    let r = arr[i]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn pointer_return_summary_dynamic_pointer_slot_is_unknown() {
    assert_mut_borrow_conflict(
        r#"
fn pick(arr: [*u256; 2], i: usize) -> *u256 {
    arr[i]
}

fn bad(p: *u256, q: *u256, i: usize) {
    let arr = [p, q]
    let r = pick(arr, i)
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn pointer_slot_reassignment_does_not_clear_pointee_facts() {
    assert_mut_borrow_conflict(
        r#"
fn bad(pp: * *u256, p: *u256, other: * *u256) {
    let mut pp_local = pp
    let q = pp_local
    *pp_local = p
    pp_local = other

    let r = *q
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn unknown_memory_pointer_conflicts_with_memory_provider_root() {
    assert_pending_validation(
        r#"
extern {
    fn unknown_ptr<T>() -> *T
}

struct Store {
    value: u256,
}

fn bad() uses (store: mut Store) {
    let p = unknown_ptr<Store>()
    let a = mut *p
    let b = mut store
    b.value = 1
    a.value = 2
}
"#,
        "bad",
    );
}

#[test]
fn fresh_allocation_pointer_slots_default_to_unknown() {
    assert_mut_borrow_conflict(
        r#"
fn bad(p: *u256) {
    let pp = core::ptr::alloc<*u256>()
    let r = *pp
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn dynamic_index_reassignment_keeps_weak_pointer_facts() {
    assert_mut_borrow_conflict(
        r#"
fn bad(p: *u256, q: *u256, i: usize, j: usize) {
    let mut arr = [q, q]
    let mut k = i
    arr[k] = p
    k = j
    arr[k] = q
    let r = arr[i]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn may_target_pointer_store_keeps_unwritten_left_slot_targets() {
    assert_mut_borrow_conflict(
        r#"
fn bad(pp1: * *u256, pp2: * *u256, p: *u256, q: *u256, choose: bool) {
    *pp1 = p
    let mut pp = pp1
    if choose {
        pp = pp2
    }
    *pp = q
    let r = *pp1
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn may_target_pointer_store_keeps_unwritten_right_slot_targets() {
    assert_mut_borrow_conflict(
        r#"
fn bad(pp1: * *u256, pp2: * *u256, p: *u256, q: *u256, choose: bool) {
    *pp2 = p
    let mut pp = pp1
    if choose {
        pp = pp2
    }
    *pp = q
    let r = *pp2
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn unknown_pointer_store_does_not_shadow_default_targets() {
    assert_mut_borrow_conflict(
        r#"
extern {
    fn unknown_ptr<T>() -> *T
}

fn bad(p: *u256, q: *u256) {
    let pp = core::ptr::alloc<*u256>()
    let unknown = unknown_ptr<*u256>()
    *unknown = q
    let r = *pp
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn multiple_unknown_pointer_stores_accumulate_targets() {
    assert_mut_borrow_conflict(
        r#"
extern {
    fn unknown_ptr<T>() -> *T
}

fn bad(p: *u256, q: *u256) {
    let unknown1 = unknown_ptr<*u256>()
    *unknown1 = p
    let unknown2 = unknown_ptr<*u256>()
    *unknown2 = q
    let unknown3 = unknown_ptr<*u256>()
    let r = *unknown3
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn dynamic_pointer_read_keeps_default_targets_for_uncovered_slots() {
    assert_mut_borrow_conflict(
        r#"
fn bad(pp: *[*u256; 2], p: *u256, i: usize) {
    let old = (*pp)[1]
    let a = mut *old
    (*pp)[0] = p
    let r = (*pp)[i]
    let b = mut *r
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn pointer_summary_read_keeps_default_targets_for_uncovered_slots() {
    assert_mut_borrow_conflict(
        r#"
fn pick(pp: *[*u256; 2], i: usize) -> *u256 {
    (*pp)[i]
}

fn bad(pp: *[*u256; 2], p: *u256, i: usize) {
    let old = (*pp)[1]
    let a = mut *old
    (*pp)[0] = p
    let r = pick(pp, i)
    let b = mut *r
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn unrelated_constant_pointer_slots_do_not_conflict() {
    assert_no_borrow_conflict(
        r#"
fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let a = mut *q
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert_no_borrow_conflict(
        r#"
fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let arr = [q, q]
    let r = arr[0]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert_no_borrow_conflict(
        r#"
fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let arr = [p, q]
    let r = arr[1]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert_no_borrow_conflict(
        r#"
fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let arr = [q; 64]
    let r = arr[63]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert_no_borrow_conflict(
        r#"
fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let arr = [
        q, q, q, q, q, q, q, q,
        q, q, q, q, q, q, q, q,
        q, q, q, q, q, q, q, q,
        q, q, q, q, q, q, q, q,
        q,
    ]
    let r = arr[32]
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
    assert_no_borrow_conflict(
        r#"
fn ok() {
    let p = core::ptr::alloc<u256>()
    let q = core::ptr::alloc<u256>()
    let mut arr = [q; 64]
    arr[0] = p
    let r = arr[0]
    let a = mut *r
    let b = mut *q
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn returned_pointer_aggregate_from_unrelated_input_does_not_conflict() {
    assert_no_borrow_conflict(
        r#"
struct Holder {
    ptr: *u256,
}

fn wrap(q: *u256) -> Holder {
    Holder { ptr: q }
}

fn ok(q: *u256) {
    let p = core::ptr::alloc<u256>()
    let h = wrap(q)
    let a = mut *h.ptr
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn raw_pointer_array_distinct_element_stores_do_not_conflict() {
    assert_no_borrow_conflict(
        r#"
fn ok(scratch: *[u256; 2], left: u256, right: u256) {
    scratch[0] = left
    scratch[1] = right
}
"#,
    );
}

#[test]
fn branching_pointer_summary_records_each_input_target() {
    with_borrow_summary(
        r#"
fn pick(cond: bool, p: *u256, q: *u256) -> *u256 {
    if cond {
        p
    } else {
        q
    }
}
"#,
        "pick",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 2, "{summary:#?}");
            let mut params = leaves
                .iter()
                .map(|leaf| leaf.payload.source.param().unwrap())
                .collect::<Vec<_>>();
            params.sort();
            assert_eq!(params, [1, 2]);
            assert!(
                leaves
                    .iter()
                    .all(|leaf| leaf.path.is_empty() && leaf.payload.path.is_empty())
            );
        },
    );
}

#[test]
fn local_pointer_array_summary_keeps_constant_slot_precision() {
    with_borrow_summary(
        r#"
fn pick(_ p: *u256, q: *u256) -> *u256 {
    let arr = [q, q]
    arr[0]
}
"#,
        "pick",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(1, StructuralPath::default()))
            );
            assert_eq!(leaves[0].payload.path, RegionPath::default());
        },
    );
}

#[test]
fn local_pointer_array_borrow_summary_keeps_constant_slot_precision() {
    with_borrow_summary(
        r#"
fn pick(_ p: *u256, q: *u256) -> mut u256 {
    let arr = [q, q]
    let r = arr[0]
    mut *r
}
"#,
        "pick",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(1, StructuralPath::default()))
            );
            assert_eq!(leaves[0].payload.path, RegionPath::default());
        },
    );
}

#[test]
fn pointer_to_array_of_pointers_summary_preserves_indexed_pointee() {
    with_borrow_summary(
        r#"
fn pick(pp: *[*u256; 64], i: usize) -> *u256 {
    (*pp)[i]
}
"#,
        "pick",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(0, StructuralPath::default()))
            );
            assert_eq!(leaves[0].payload.path, RegionPath::default());
            assert_eq!(
                leaves[0].payload.source.dereferences(),
                &[RegionPath::new([CapabilityProjection::Index(
                    IndexExpr::FormalValue(1)
                )])]
            );
        },
    );
}

#[test]
fn pointer_to_large_pointer_array_caller_keeps_element_precision() {
    assert_no_borrow_conflict(
        r#"
fn pick(pp: *[*u256; 64], i: usize) -> *u256 {
    (*pp)[i]
}

fn ok(q: *u256, i: usize) {
    let p = core::ptr::alloc<u256>()
    let pp = core::ptr::alloc<[*u256; 64]>()
    *pp = [q; 64]
    let r = pick(pp, i)
    let a = mut *r
    let b = mut *p
    b = 1
    a = 2
}
"#,
    );
}

#[test]
fn enum_pointer_payloads_participate_in_pointer_summaries() {
    with_borrow_summary(
        r#"
enum E {
    A(*u256),
    B,
}

fn wrap(p: *u256) -> E {
    E::A(p)
}
"#,
        "wrap",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(0, StructuralPath::default()))
            );
            assert_eq!(leaves[0].payload.path, RegionPath::default());
            assert!(
                matches!(leaves[0].path.as_slice(), [CapabilityProjection::VariantField { variant, field }] if variant.0 == 0 && field.0 == 0)
            );
        },
    );
}

#[test]
fn enum_pointer_payload_extraction_preserves_provenance() {
    assert_mut_borrow_conflict(
        r#"
enum E {
    A(*u256),
    B,
}

fn bad(p: *u256) {
    let e = E::A(p)
    match e {
        E::A(r) => {
            let a = mut *r
            let b = mut *p
            b = 1
            a = 2
        }
        E::B => {}
    }
}
"#,
    );
}

#[test]
fn returning_pointer_bearing_provider_value_is_rejected() {
    let diags = borrow_diags(
        r#"
struct Holder {
    ptr: *u256,
}

fn bad() -> Holder uses (holder: Holder) {
    holder
}
"#,
    );
    assert!(diags.contains("invalid return borrow"), "{diags:?}");
    assert!(
        diags.contains("cannot return a borrow derived from an effect parameter"),
        "{diags:?}"
    );
}

#[test]
fn pointer_bearing_capability_return_summary_tracks_exposed_pointer_value() {
    with_borrow_summary(
        r#"
struct Holder {
    ptr: *u256,
}

impl Holder {
    fn ptr_slot(mut self) -> mut *u256 {
        mut self.ptr
    }
}
"#,
        "ptr_slot",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 1, "{summary:#?}");
            assert_eq!(
                leaves[0].payload.source.origin,
                ExternalOrigin::Input(InputSource::slot(0, StructuralPath::default()))
            );
            assert_eq!(
                leaves[0].payload.path,
                RegionPath::new([CapabilityProjection::Field(FieldIndex(0))])
            );
        },
    );
}

#[test]
fn mem_span_reassignment_does_not_keep_stale_carrier_target() {
    assert_no_borrow_conflict(
        r#"
fn ok(q: *u256, len: u256) {
    let p = core::ptr::alloc<u256>()
    let mut span = core::ptr::MemSpan::from_raw_parts(ptr: core::ptr::byte_ptr(p), len: len)
    span = core::ptr::MemSpan::from_raw_parts(ptr: core::ptr::byte_ptr(q), len: len)
    let x = mut *core::ptr::cast<u8, u256>(span.ptr())
    let y = mut *p
    y = 1
    x = 2
}
"#,
    );
}

#[test]
fn forwarded_memory_borrow_param_keeps_incoming_loan_targets() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Holder {
    tag: u256,
}

impl Holder {
    fn forward(mut self, _ value: mut u256) -> mut u256 {
        value
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "forward") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("forward instance");
    let summary = semantic_borrow_summary(&db, instance)
        .expect("borrow summary")
        .expect("forward should produce a borrow summary");
    let values = ValueInterner::new(&db, ValueLimits::default());
    let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
    assert_eq!(leaves.len(), 1);
    assert_eq!(
        leaves[0].payload.source.origin,
        ExternalOrigin::Input(InputSource::slot(1, StructuralPath::default()))
    );
    check_semantic_borrows(&db, instance).expect("borrowck should accept forwarded borrows");
}

#[test]
fn contract_field_mut_borrow_matrix_fixture_borrowchecks() {
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                let type_detail = match error {
                    fe_hir::analysis::semantic::normalized::NormalizedBodyVerifyError::ForwardType {
                        result,
                        source,
                    } => format!(
                        "result_ty={} source_ty={}",
                        artifacts.body.value(result).expect("result value").ty.pretty_print(db),
                        artifacts.body.value(source).expect("source value").ty.pretty_print(db),
                    ),
                    fe_hir::analysis::semantic::normalized::NormalizedBodyVerifyError::StoreType {
                        value,
                        destination: fe_hir::analysis::semantic::normalized::NPlaceBase::Root(root),
                    } => format!(
                        "source_ty={} destination_ty={}",
                        artifacts.body.value(value).expect("source value").ty.pretty_print(db),
                        artifacts.body.root(root).expect("destination root").ty.pretty_print(db),
                    ),
                    _ => String::new(),
                };
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?} {type_detail}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                )
            });
            assert!(matches!(
                fe_hir::analysis::semantic::normalized::semantic_body_admission(db, instance),
                fe_hir::analysis::semantic::normalized::SemanticBodyAdmission::Ready(_),
            ));
            if let Err(diag) = check_semantic_borrows(db, instance) {
                if matches!(diag, SemanticAnalysisError::Pending(_))
                    && instance
                        .key(db)
                        .subst(db)
                        .generic_args(db)
                        .iter()
                        .any(|ty| {
                            ty.has_param(db) || ty.has_var(db) || ty.contains_assoc_ty_of_param(db)
                        })
                {
                    return;
                }
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                );
            }
        },
    );
}

#[test]
fn diverging_if_branch_does_not_forward_never_into_join_result() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
fn diverge() -> ! {
    core::panic()
}

fn choose(flag: bool) {
    if flag {
        diverge()
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let choose = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "choose") =>
            {
                Some(*func)
            }
            _ => None,
        })
        .expect("missing `choose` function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(choose)),
    );
    let raw = instance.body(&db);
    let mut saw_diverging_call = false;
    for block in &raw.blocks {
        for statement in &block.stmts {
            let SStmtKind::Assign { dst, expr } = &statement.kind else {
                continue;
            };
            if matches!(expr, SExpr::Call { .. }) && raw.locals[dst.index()].ty.is_never(&db) {
                saw_diverging_call = true;
                assert!(matches!(
                    block.terminator.kind,
                    STerminatorKind::Assert { message: None }
                ));
            }
            if let SExpr::Forward(source) = expr {
                assert_eq!(
                    raw.locals[dst.index()].ty,
                    raw.locals[source.value.index()].ty
                );
            }
        }
    }
    assert!(saw_diverging_call);

    let artifacts = normalize_raw_body(&db, instance, raw, instance.assumptions(&db))
        .expect("never-branch body should normalize");
    verify_normalized_body(&db, &artifacts.body)
        .expect("never-branch normalized body should verify");
}

#[test]
fn returned_storage_borrow_effect_args_are_finalized_in_normalized_body() {
    let mut saw_storage_add_effect = false;
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"),
        |db, instance| {
            let normalized = normalize_semantic_body(db, instance).expect("normalized body");
            for stmt in normalized
                .body
                .blocks
                .iter()
                .flat_map(|block| block.statements.iter())
            {
                let NStatementKind::Define {
                    expr:
                        NExpr::Call {
                            callee,
                            effect_args,
                            ..
                        },
                    ..
                } = &stmt.kind
                else {
                    continue;
                };
                let BodyOwner::Func(func) = callee.key.owner(db) else {
                    continue;
                };
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == "add")
                    && effect_args
                        .iter()
                        .any(|arg| arg.provider == Some(ProviderAddressSpace::Storage))
                {
                    saw_storage_add_effect = true;
                }
            }
        },
    );
    assert!(
        saw_storage_add_effect,
        "expected storage provider on normalized add effect arg"
    );
}

#[test]
fn mixed_returned_borrow_provenance_is_rejected_before_runtime_lowering() {
    let diags = borrow_diags(mixed_returned_borrow_provenance_src());

    assert!(
        diags.contains("provider provenance conflict in `fn Mixed::__init__`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("effect argument may come from multiple address spaces"),
        "{diags:?}"
    );
}

#[test]
fn mixed_returned_borrow_provenance_poison_normalization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        mixed_returned_borrow_provenance_src(),
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "Mixed");

    let err = normalize_semantic_body(&db, instance)
        .expect_err("mixed provider provenance must poison normalization");
    let SemanticNormalizationFailure::InternalFailure(err) = err else {
        panic!("provider provenance conflict must be an internal normalization failure")
    };
    assert_eq!(err.kind, SemanticDiagnosticKind::ProviderProvenanceConflict);
    assert_eq!(
        err.primary.message,
        "effect argument may come from multiple address spaces: memory, storage"
    );
}

#[test]
fn mixed_returned_borrow_provenance_poison_noesc() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        mixed_returned_borrow_provenance_src(),
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "Mixed");

    let err = check_semantic_boundaries(&db, instance)
        .expect_err("mixed provider provenance must poison noesc");
    let SemanticAnalysisError::Diagnostic(err) = err else {
        panic!("provider provenance conflict must produce a diagnostic")
    };
    assert_eq!(
        err.message,
        "provider provenance conflict in `fn Mixed::__init__`"
    );
    assert_eq!(
        err.sub_diagnostics[0].message,
        "effect argument may come from multiple address spaces: memory, storage"
    );
}

#[test]
fn mixed_returned_borrow_provenance_collects_one_diagnostic() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        mixed_returned_borrow_provenance_src(),
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = collect_semantic_borrow_diagnostic_vouchers(&db, top_mod);
    assert_eq!(
        diags.len(),
        1,
        "unexpected diagnostics: {:#?}",
        borrow_diags(mixed_returned_borrow_provenance_src())
    );
    let rendered = format_diagnostics(&db, &diags);
    assert!(
        rendered.contains("provider provenance conflict in `fn Mixed::__init__`"),
        "{rendered:?}"
    );
}

#[test]
fn reports_mut_borrow_conflict() {
    let diags = borrow_diags(
        r#"
fn bad() {
    let mut x: u256 = 0
    let p: mut u256 = mut x
    let q: mut u256 = mut x
    q = 1
    p = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(
        diags.contains("cannot mutably borrow") || diags.contains("mutable borrow"),
        "{diags:?}",
    );
}

#[test]
fn mutable_enum_payload_reborrow_suspends_the_parent_loan() {
    let diags = borrow_diags(
        r#"
struct Item {
    value: u256,
}

impl Item {
    fn set(mut self, value: u256) {
        self.value = value
    }
}

enum Choice {
    Pair([Item; 2]),
    Triple([Item; 3]),
}

impl Choice {
    fn set(mut self, index: usize, value: u256) {
        match self {
            Choice::Pair(mut items) => items[index].set(value: value),
            Choice::Triple(mut items) => items[index].set(value: value),
        }
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn mutable_enum_payload_reborrow_still_rejects_independent_aliases() {
    let diags = checked_borrow_diags(
        r#"
struct Item {
    value: u256,
}

enum Choice {
    Item(Item),
}

fn bad(choice: mut Choice) {
    match choice {
        Choice::Item(mut item) => {
            let first: mut u256 = mut item.value
            let second: mut u256 = mut item.value
            second = 1
            first = 2
        }
    }
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn generic_effect_handle_target_has_a_provider_borrow_root() {
    let diags = borrow_diags(
        r#"
use core::EffectHandle

fn write<H: EffectHandle>(_ handle: H, _ value: H::Target) uses (target: mut H::Target) {
    target = value
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn destructured_tuple_param_field_projection_resolves_its_carrier_root() {
    let diags = borrow_diags(
        r#"
struct Byte {
    val: u8,
}

fn read(input: (Byte, u256)) -> u8 {
    let (byte, _) = input
    byte.val
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn ordinary_effect_handle_fields_use_the_handle_backing_place() {
    let source = r#"
use core::{AddressSpace, EffectHandle}

struct TaggedPtr<T> {
    tag: u256,
    addr: u256,
}

impl<T> EffectHandle for TaggedPtr<T> {
    type Target = T
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Memory

    fn raw(self) -> u256 {
        self.addr
    }
}

fn identity(_ ptr: TaggedPtr<u256>) -> TaggedPtr<u256> {
    ptr
}

fn read_call_result() -> u256 {
    let ptr = identity(TaggedPtr { tag: 7, addr: 8 })
    ptr.tag
}

fn read_nested_array() -> u256 {
    let ptrs: [TaggedPtr<u256>; 2] = [
        TaggedPtr { tag: 7, addr: 8 },
        TaggedPtr { tag: 9, addr: 10 },
    ]
    ptrs[1].tag
}

fn mutate(mut _ ptr: own TaggedPtr<u256>) -> u256 {
    ptr.tag = 11
    ptr.tag
}
"#;
    let diags = borrow_diags(source);
    assert!(diags.is_empty(), "{diags}");

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "read_call_result");
    let (value, path) = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::ProjectValue { value, path },
                ..
            } if normalized.body.values[result.index()].ty.pretty_print(&db) == "u256"
                && matches!(
                    path.0.iter().next(),
                    Some(NDataProjection::Field(field)) if field.0 == 0
                ) =>
            {
                Some((value.value, path))
            }
            _ => None,
        })
        .expect("tag field read");
    assert!(
        normalized.body.values[value.index()]
            .ty
            .pretty_print(&db)
            .to_string()
            .starts_with("TaggedPtr<"),
        "ordinary handle field should project the handle value representation: {path:#?}"
    );
}

#[test]
fn ordinary_effect_handle_field_borrows_still_conflict() {
    let diags = borrow_diags(
        r#"
use core::{AddressSpace, EffectHandle}

struct TaggedPtr<T> {
    tag: u256,
    addr: u256,
}

impl<T> EffectHandle for TaggedPtr<T> {
    type Target = T
    type Raw = u256
    const SPACE: AddressSpace = AddressSpace::Memory

    fn raw(self) -> u256 {
        self.addr
    }
}

fn conflict(mut _ ptr: own TaggedPtr<u256>) {
    let first: mut u256 = mut ptr.tag
    let second: mut u256 = mut ptr.tag
    second = 1
    first = 2
}
"#,
    );
    assert!(
        diags.contains("borrow conflict in `fn conflict`"),
        "{diags}"
    );
}

#[test]
fn reports_noesc_storage_escape_through_whole_assignment() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

pub contract NoEscStore {
    mut slot: Esc

    init() uses (mut slot) {
        let mut x: u256 = 0
        let e: Esc = Esc { h: mut x, tag: 0 }
        slot = e
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_noesc_storage_escape_through_field_assignment() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

struct Wrapper {
    e: Esc,
}

pub contract NoEscFieldStore {
    mut slot: Wrapper

    init() uses (mut slot) {
        let mut x: u256 = 0
        let e: Esc = Esc { h: mut x, tag: 0 }
        slot.e = e
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscFieldStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_noesc_storage_escape_through_inline_aggregate_store() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

pub contract NoEscInlineStore {
    mut slot: Esc

    init() uses (mut slot) {
        let mut x: u256 = 0
        slot = Esc { h: mut x, tag: 0 }
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscInlineStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_noesc_storage_escape_for_ref_handle_in_stored_aggregate() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: ref u256,
    tag: u256,
}

pub contract NoEscRefStore {
    mut slot: Esc

    init() uses (mut slot) {
        let x: u256 = 0
        let e: Esc = Esc { h: ref x, tag: 0 }
        slot = e
    }
}
"#,
    );

    assert!(
        diags.contains("noesc violation in `fn NoEscRefStore::__init__`"),
        "{diags:?}"
    );
    assert!(diags.contains("cannot store `Esc` in storage"), "{diags:?}");
}

#[test]
fn reports_storage_borrow_passed_as_regular_function_argument() {
    let diags = borrow_diags(
        r#"
fn bump(_ handle: mut u256) {
    handle += 1
}

pub contract NoEscCallArg {
    mut slot: u256

    init() uses (mut slot) {
        bump(mut slot)
    }
}
"#,
    );

    assert!(
        diags.contains("transport violation in `fn NoEscCallArg::__init__`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("cannot pass `mut u256` from storage as function argument"),
        "{diags:?}"
    );
}

#[test]
fn allows_memory_noesc_values_and_memory_borrow_call_args() {
    let diags = borrow_diags(
        r#"
struct Esc {
    h: mut u256,
    tag: u256,
}

fn bump(_ handle: mut u256) {
    handle += 1
}

fn ok() {
    let mut x: u256 = 0
    let e: Esc = Esc { h: mut x, tag: 0 }
    let mut y: u256 = 1
    let mut dst: Esc = Esc { h: mut y, tag: 1 }
    dst = e
    let mut z: u256 = 2
    bump(mut z)
}
"#,
    );

    assert!(!diags.contains("noesc violation"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn raw_pointers_and_memory_aggregates_cannot_escape_through_storage_handles() {
    for (source, expected) in [
        (
            r#"
use core::ptr
use std::evm::StorPtr

pub contract PointerNoEsc {
    mut saved: StorPtr<*u256>

    init() uses (mut saved) {
        saved = ptr::alloc<u256>()
    }
}
"#,
            "cannot store `*<u256>` in storage",
        ),
        (
            r#"
use core::ptr
use std::evm::TStorPtr

pub contract PointerNoEsc {
    mut saved: TStorPtr<*u256>

    init() uses (mut saved) {
        saved = ptr::alloc<u256>()
    }
}
"#,
            "cannot store `*<u256>` in transient storage",
        ),
        (
            r#"
use core::ptr
use std::evm::StorPtr

pub contract PointerNoEsc {
    mut saved: StorPtr<ptr::MemArray<u256>>

    init() uses (mut saved) {
        saved = ptr::MemArray<u256>::new_uninit(1)
    }
}
"#,
            "cannot store `MemArray<u256>` in storage",
        ),
    ] {
        let diags = borrow_diags(source);
        assert!(diags.contains("noesc violation"), "{diags}");
        assert!(diags.contains(expected), "{diags}");
    }

    let diags = borrow_diags(
        r#"
use core::ptr

fn keep_pointer_in_memory(value: *u256) {
    let destination = ptr::alloc<*u256>()
    *destination = value
}
"#,
    );
    assert!(!diags.contains("noesc violation"), "{diags}");
}

#[test]
fn generic_noesc_store_is_rejected_only_after_storage_specialization() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Box<T> {
    value: T,
}

fn store_generic<T>(value: own T) uses (slot: mut Box<T>) {
    slot = Box<T> { value }
}

pub contract GenericNoEsc {
    mut slot: Box<mut u256>

    init() uses (mut slot) {
        let mut x: u256 = 0
        store_generic<mut u256>(mut x)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let store_generic = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "store_generic") =>
            {
                Some(*func)
            }
            _ => None,
        })
        .expect("store_generic function");
    let identity = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(store_generic)),
    );
    check_semantic_boundaries(&db, identity).expect("generic identity noesc should be accepted");

    let init = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Contract(contract) => Some(get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(
                    &db,
                    BodyOwner::ContractInit {
                        contract: *contract,
                    },
                ),
            )),
            _ => None,
        })
        .expect("contract init instance");
    let specialized = init
        .callees(&db)
        .iter()
        .find_map(|callee| match callee.key.owner(&db) {
            BodyOwner::Func(func) if func == store_generic => {
                Some(get_or_build_semantic_instance(&db, callee.key))
            }
            _ => None,
        })
        .expect("specialized store_generic callee");
    let err = check_semantic_boundaries(&db, specialized)
        .expect_err("specialized noesc store should be rejected");
    let SemanticAnalysisError::Diagnostic(err) = err else {
        panic!("noesc violation must produce a diagnostic")
    };
    assert!(
        err.message
            .contains("noesc violation in `fn store_generic`"),
        "{err:#?}"
    );
    assert!(
        format!("{err:#?}").contains("cannot store `Box<mut u256>` in storage"),
        "{err:#?}"
    );
}

#[test]
fn rejects_return_borrow_to_local() {
    let diags = borrow_diags(
        r#"
struct Pair {
    a: u256,
    b: u256,
}

fn bad() -> mut u256 {
    let mut x = Pair { a: 0, b: 0 }
    mut x.a
}
"#,
    );

    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("cannot return a borrow to local"),
        "{diags:?}"
    );
}

#[test]
fn rejects_return_borrow_derived_from_uses_effect_parameter() {
    let diags = borrow_diags(
        r#"
struct Store {
    value: u256,
}

fn bad() -> mut u256 uses (store: mut Store) {
    mut store.value
}
"#,
    );

    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("cannot return a borrow derived from an effect parameter"),
        "{diags:?}"
    );
}

#[test]
fn array_index_reads_do_not_hit_internal_borrowck_error() {
    let diags = borrow_diags(
        r#"
pub fn cast_u8_usize_cmp(indices: [u8; 8], i: usize, j: usize) -> u8 {
    let path = indices[i]
    if j < path as usize {
        return 1
    }
    if j == path as usize {
        return 2
    }
    if j > path as usize {
        return 3
    }
    0
}
"#,
    );

    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn raw_mem_allocate_does_not_report_move_conflict() {
    let diags = checked_borrow_diags(
        r#"
use core::ptr
use std::evm::RawMem

fn allocate(bytes: u256) -> *u8 uses (mem: mut RawMem) {
    let out = ptr::alloc_bytes(64)
    mem.mstore(addr: out, value: bytes)
    mem.mstore(addr: ptr::offset_bytes(out, 32), value: bytes)
    out
}
"#,
    );

    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn concrete_evm_capability_impl_preserves_receiver_authorization() {
    assert_no_borrow_conflict(
        r#"
use core::ptr
use std::evm::RawMem

struct Data {
    value: u256,
}

fn raw_store() uses (data: *Data, mem: mut RawMem) {
    mem.mstore(addr: ptr::byte_ptr(data), value: 8)
}

fn ok() uses (mem: mut RawMem) {
    let data = ptr::alloc<Data>()
    with (data, mem) {
        raw_store()
    }
}
"#,
    );
}

#[test]
fn transitive_effect_summaries_use_callee_relative_binding_indices() {
    assert_no_borrow_conflict(include_str!(
        "../../fe/tests/fixtures/fe_test/address_call_method.fe"
    ));
}

#[test]
fn nested_effect_receiver_calls_preserve_two_phase_borrows() {
    assert_no_borrow_conflict(
        r#"
use std::evm::{Address, StorageMap}

struct Store {
    balances: StorageMap<Address, u256>,
}

fn add(_ to: Address, amount: u256) uses (store: mut Store) {
    store.balances.set(key: to, value: store.balances.get(key: to) + amount)
}
"#,
    );
}

#[test]
fn two_phase_receiver_reservation_allows_unknown_nested_memory_effects() {
    assert_no_borrow_conflict(
        r#"
struct Sink {
    value: u256,
}

impl Sink {
    fn set(mut self, value: u256) {
        self.value = value
    }
}

extern {
    fn unknown_ptr() -> *u256
}

fn mutate_unknown_memory() -> u256 {
    *unknown_ptr() = 1
    1
}

fn ok(_ sink: mut Sink) {
    sink.set(mutate_unknown_memory())
}
"#,
    );
}

#[test]
fn unknown_memory_effects_do_not_overlap_zero_sized_receiver() {
    assert_no_borrow_conflict(
        r#"
trait PointerSource {
    fn ptr(self) -> *u256
}

struct Token {}

impl Token {
    fn run<K>(mut self, key: K)
        where K: PointerSource
    {
        let ptr = key.ptr()
        let _ value = *ptr
    }
}

fn ok<K>(key: K)
    where K: PointerSource
{
    let mut local = Token {}
    local.run(key)
}
"#,
    );
}

#[test]
fn unknown_memory_effects_overlap_runtime_receiver() {
    assert_pending_validation(
        r#"
trait PointerSource {
    fn ptr(self) -> *u256
}

struct Cell {
    value: u256,
}

impl Cell {
    fn run<K>(mut self, key: K)
        where K: PointerSource
    {
        let ptr = key.ptr()
        let value = *ptr
    }
}

fn bad<K>(key: K)
    where K: PointerSource
{
    let mut local = Cell { value: 0 }
    local.run(key)
}
"#,
        "bad",
    );
}

#[test]
fn receiver_activation_follows_nested_effect_mutation() {
    assert_no_borrow_conflict(
        r#"
use std::evm::{Address, StorageMap}

struct Store {
    balances: StorageMap<Address, u256>,
}

fn replace(_ key: Address, value: u256) -> u256 uses (store: mut Store) {
    store.balances.set(key, value)
    value
}

fn ok(_ key: Address, value: u256) uses (store: mut Store) {
    store.balances.set(key, value: replace(key, value))
}
"#,
    );
}

#[test]
fn transitive_generic_memory_effects_preserve_input_authorization() {
    assert_no_borrow_conflict(
        r#"
trait Writer {
    fn write(mut self)
}

fn call_write<E>(_ writer: mut E)
    where E: Writer
{
    writer.write()
}

fn forward<E>(_ writer: mut E)
    where E: Writer
{
    call_write(mut writer)
}
"#,
    );
}

#[test]
fn generic_view_receiver_authorizes_its_named_immutable_loan() {
    assert_no_borrow_conflict(
        r#"
trait Reader {
    fn read(self)
}

fn read<R>(_ value: ref R)
    where R: Reader
{
    let alias = value
    alias.read()
}
"#,
    );
}

#[test]
fn generic_view_receiver_does_not_authorize_unrelated_loans() {
    assert_borrow_conflict(
        r#"
trait Reader {
    fn read(self)
}

fn bad<R>(_ value: ref R, ptr: *u256)
    where R: Reader
{
    let borrowed = mut *ptr
    value.read()
    borrowed = 1
}
"#,
    );
}

#[test]
fn distinct_generic_memory_effect_authorizers_are_not_merged() {
    assert_pending_validation(
        r#"
trait Writer {
    fn write(mut self)
}

fn choose<E>(_ cond: bool, _ left: mut E, _ right: mut E)
    where E: Writer
{
    if cond {
        left.write()
    } else {
        right.write()
    }
}

fn bad<E>(_ cond: bool, _ left: mut E, _ right: mut E)
    where E: Writer
{
    choose(cond, mut left, mut right)
}
"#,
        "bad",
    );
}

#[test]
fn invalid_typed_bodies_do_not_crash_semantic_borrow_analysis() {
    assert_no_borrow_conflict(include_str!(
        "../../uitest/fixtures/ty_check/event_unsupported_field_type.fe"
    ));
}

#[test]
fn invalid_callee_summaries_do_not_crash_semantic_borrow_analysis() {
    assert_no_borrow_conflict(
        r#"
fn broken_borrow(p: *u256) -> mut u256 {
    undefined = 1
    mut *p
}

fn broken_pointer(p: *u256) -> *u256 {
    undefined = 1
    p
}

fn caller(p: *u256) {
    let borrowed = broken_borrow(p)
    borrowed = 1
    *broken_pointer(p) = 1
}
"#,
    );
}

#[test]
fn code_region_fixture_does_not_report_move_conflict() {
    let diags = borrow_diags(include_str!("../../codegen/tests/fixtures/code_region.fe"));
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn create_contract_fixture_does_not_report_top_level_semantic_borrow_errors() {
    let diags = borrow_diags(include_str!(
        "../../codegen/tests/fixtures/create_contract.fe"
    ));
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn reports_move_conflict_for_reused_owned_binding() {
    let diags = borrow_diags(
        r#"
struct Inner {}

fn bad(x: own Inner) {
    let y = x
    let z = x
}
"#,
    );

    assert!(diags.contains("move conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn reports_move_conflict_for_non_copy_projection_from_view_param() {
    let diags = borrow_diags(
        r#"
struct Wrapper {
    p: Pair,
}

struct Pair {
    x: u32,
    y: u32,
}

fn unwrap(w: Wrapper) -> Pair {
    let p = w.p
    p
}
"#,
    );

    assert!(diags.contains("move conflict in `fn unwrap`"), "{diags:?}");
    assert!(
        diags.contains("cannot move out of a view parameter"),
        "{diags:?}"
    );
}

#[test]
fn non_copy_projection_to_view_receiver_does_not_move_from_view_param() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Row {
    cells: [u256; 4],
}

impl Row {
    fn get_cell(self, col: usize) -> u256 {
        self.cells[col]
    }

    fn has_value(self, val: u256) -> bool {
        let mut c: usize = 0
        while c < 4 {
            if self.cells[c] == val {
                return true
            }
            c += 1
        }
        return false
    }
}

struct Board {
    rows: [Row; 4],
}

fn read_board(board: Board, row: usize, col: usize) -> u256 {
    board.rows[row].get_cell(col: col)
}

fn find_empty(board: Board, row: usize, col: usize) -> bool {
    if board.rows[row].has_value(val: 0) {
        return board.rows[row].get_cell(col: col) == 0
    }
    false
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    );
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );

    let normalized = normalized_func_body(&db, top_mod, "read_board");
    let row_read_mode = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::Load { mode, .. },
            } if normalized.body.values[result.index()].ty.pretty_print(&db) == "Row" => Some(mode),
            _ => None,
        })
        .expect("row projection read");
    assert_eq!(*row_read_mode, ReadMode::Read);
}

#[test]
fn non_copy_field_projection_to_view_receiver_does_not_move_from_mut_receiver() {
    let diags = borrow_diags(
        r#"
struct LockStore {
    active: bool,
}

impl LockStore {
    fn is_active(self) -> bool {
        self.active
    }
}

struct RegistryStore {
    lock_store: LockStore,
}

impl RegistryStore {
    fn check(mut self) -> bool {
        self.lock_store.is_active()
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn nested_copy_projection_from_view_param_remains_allowed() {
    let diags = borrow_diags(
        r#"
struct Wrapper {
    p: Pair,
}

struct Pair {
    x: u32,
    y: u32,
}

fn read_x(w: Wrapper) -> u32 {
    w.p.x
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn non_copy_projection_move_does_not_report_conflict() {
    let diags = borrow_diags(
        r#"
struct E {}
struct Inner {}
struct Container {
    value: Inner,
}

fn sink(_ value: own Inner, _ e: mut E) {}

impl Container {
    fn enc(own self, e: mut E) {
        sink(self.value, mut e)
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn generic_tuple_projection_move_does_not_report_conflict() {
    let diags = borrow_diags(
        r#"
struct E {}

fn sink<T>(_ value: own T, _ e: mut E) {}

trait Enc {
    fn enc(own self, e: mut E)
}

impl<T0> Enc for (T0,) {
    fn enc(own self, e: mut E) {
        sink<T0>(self.0, mut e)
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn enum_variant_test_does_not_consume_owned_value() {
    let diags = borrow_diags(
        r#"
fn decode(word: u256) -> u64 {
    if let Option::Some(value) = word.downcast() {
        return value
    }
    0
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn nested_owned_enum_match_does_not_report_move_conflict() {
    let diags = borrow_diags(
        r#"
enum Inner {
    Unit,
    Value(u8),
}

enum Outer {
    First(Inner),
    Second(u8),
}

fn read(outer: own Outer) -> u8 {
    match outer {
        Outer::First(Inner::Unit) => 0
        Outer::First(Inner::Value(x)) => x
        Outer::Second(y) => y
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn multi_field_owned_enum_match_does_not_report_move_conflict() {
    let diags = borrow_diags(
        r#"
struct Boxed {}

enum Pair {
    Both(Boxed, Boxed),
}

fn take(_ value: own Boxed) {}

fn read(pair: own Pair) {
    match pair {
        Pair::Both(lhs, rhs) => {
            take(lhs)
            take(rhs)
        }
    }
}
"#,
    );

    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn effect_handle_field_deref_fixture_does_not_report_semantic_borrow_errors() {
    let diags = borrow_diags(include_str!(
        "../../codegen/tests/fixtures/effect_handle_field_deref.fe"
    ));
    assert!(!diags.contains("borrow conflict"), "{diags:?}");
    assert!(!diags.contains("move conflict"), "{diags:?}");
    assert!(
        !diags.contains("internal borrow checking error"),
        "{diags:?}"
    );
}

#[test]
fn root_object_direct_values_preserve_provider_roots_in_normalized_borrowck() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
use std::evm::{Address, StorageMap}

struct TokenStore {
    balances: StorageMap<Address, u256>,
}

fn read_balance(addr: Address) -> u256 uses (store: TokenStore) {
    let balance = store.balances.get(key: addr)
    balance
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read_balance") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("read_balance instance");
    if let Err(diag) = check_semantic_borrows(&db, instance) {
        panic!("{diag:?}");
    }
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let source = instance.body(&db);
    let (store_local, store) = source
        .locals
        .iter()
        .enumerate()
        .find_map(|(index, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::EffectParam { .. }) => {
                Some((fe_hir::analysis::semantic::SLocalId::new(index), local))
            }
            _ => None,
        })
        .expect("store effect binding");
    let (provider_root, root) = normalized
        .body
        .roots
        .iter()
        .enumerate()
        .find(|(_, root)| matches!(root.kind, NRootKind::Provider { .. }) && root.ty == store.ty)
        .map(|(index, root)| (fe_hir::analysis::semantic::NRootId::new(index), root))
        .expect("store binding must normalize to a provider root");
    assert_eq!(
        normalized.layout_plan.root_source(provider_root),
        None,
        "provider identity must not be represented by a source local"
    );
    assert_eq!(root.address_space, ProviderAddressSpace::Memory);
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    expr: NExpr::Load { place, .. },
                    ..
                } if place.base == NPlaceBase::Root(provider_root)
                    && matches!(place.path.iter().next(), Some(NDataProjection::Field(field)) if field.0 == 0)
            )
        })
    }));
    assert!(
        normalized
            .layout_plan
            .use_backings
            .iter()
            .any(|backing| matches!(
                backing.source,
                fe_hir::analysis::semantic::NLayoutBackingSource::Root { root, .. }
                    if root == provider_root
            )),
        "provider-rooted loads must retain runtime backing provenance for {store_local:?}"
    );
}

#[test]
fn ref_projection_preserves_place_borrow_lowering() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Pair {
    x: u256,
}

fn read(pair: Pair) -> u256 {
    let r: ref u256 = ref pair.x
    r
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("read instance");
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let borrow = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                expr:
                    NExpr::Borrow {
                        place,
                        kind: BorrowKind::Ref,
                        ..
                    },
                ..
            } => Some(place),
            _ => None,
        })
        .expect("borrow expression");
    let NPlaceBase::CapabilityTarget { carrier } = borrow.base else {
        panic!("expected capability-target view-param place for ref projection: {borrow:#?}")
    };
    assert!(matches!(
        normalized.body.value(carrier).map(|value| value.definition),
        Some(NValueDefinition::EntryParam { param: 0 })
    ));
    assert_eq!(borrow.path.len(), 1);
    assert_eq!(
        borrow.path.iter().next(),
        Some(&NDataProjection::Field(
            fe_hir::analysis::semantic::FieldIndex(0)
        ))
    );
}

#[test]
fn nested_projection_through_borrow_field_uses_explicit_capability_target() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Data {
    x: u256,
}

struct View {
    d: ref Data,
}

fn read(v: own View) -> u256 {
    v.d.x
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "read");
    check_semantic_borrows(&db, normalized.body.owner)
        .unwrap_or_else(|error| panic!("nested projection should borrowcheck: {error:#?}"));
    let (carrier, field) = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| match &statement.kind {
            NStatementKind::Define {
                expr: NExpr::Load { place, .. },
                ..
            } => match place.base {
                NPlaceBase::CapabilityTarget { carrier } => Some((carrier, place)),
                NPlaceBase::Root(_) => None,
            },
            NStatementKind::Define { .. } | NStatementKind::Store { .. } => None,
        })
        .expect("projection through nested borrow field");
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    result,
                    expr: NExpr::ProjectValue { path, .. },
                } if *result == carrier
                    && path.0.iter().eq([&NDataProjection::Field(
                        fe_hir::analysis::semantic::FieldIndex(0),
                    )])
            )
        })
    }));
    assert_eq!(
        field.base,
        NPlaceBase::CapabilityTarget { carrier },
        "nested projection must follow the loaded capability carrier: {field:#?}",
    );
    assert_eq!(
        field.path.iter().collect::<Vec<_>>(),
        vec![&NDataProjection::Field(
            fe_hir::analysis::semantic::FieldIndex(0),
        )],
    );
}

#[test]
fn projected_direct_value_snapshots_keep_lineage_without_reviving_aliases() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Pair {
    x: u256,
}

struct Wrapper {
    pair: Pair,
}

fn read(wrapper: own Wrapper) -> u256 {
    let pair = wrapper.pair
    let copy = pair
    let r: ref Pair = ref copy
    r.x
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "read") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("read instance");
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let source = instance.body(&db);
    let pair_ty = source
        .locals
        .iter()
        .find(|local| {
            matches!(
                local.source,
                Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
            ) && local.ty.is_struct(&db)
        })
        .map(|local| local.ty)
        .expect("pair locals should exist");
    let locals = source
        .locals
        .iter()
        .enumerate()
        .filter_map(|(idx, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
                if local.ty == pair_ty =>
            {
                Some(fe_hir::analysis::semantic::SLocalId::new(idx))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        locals.len(),
        2,
        "expected pair/copy locals, got {locals:#?}"
    );
    let [pair_local, copy_local] = locals.as_slice() else {
        panic!("expected pair/copy locals, got {locals:#?}")
    };
    let root_for = |local| {
        normalized
            .body
            .roots
            .iter()
            .enumerate()
            .find_map(|(index, root)| {
                let root_id = fe_hir::analysis::semantic::NRootId::new(index);
                (normalized.layout_plan.root_source(root_id) == Some(local)
                    && matches!(root.kind, NRootKind::LocalSlot { .. }))
                .then_some(root_id)
            })
            .unwrap_or_else(|| panic!("missing local-slot root for {local:?}"))
    };
    let copy_root = root_for(*copy_local);

    let pair_value = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| match &statement.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::Forward { src },
            } if normalized.layout_plan.value_source(*result) == Some(*pair_local) => {
                Some((*result, src.value))
            }
            _ => None,
        })
        .expect("pair forwarding value");
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    result,
                    expr: NExpr::ProjectValue { path, .. },
                } if *result == pair_value.1
                    && path.0.iter().eq([&NDataProjection::Field(
                        fe_hir::analysis::semantic::FieldIndex(0),
                    )])
            )
        })
    }));
    assert!(normalized.body.roots.iter().enumerate().all(|(index, _)| {
        normalized
            .layout_plan
            .root_source(fe_hir::analysis::semantic::NRootId::new(index))
            != Some(*pair_local)
    }));
    let copy_value = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|statement| match statement.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::Forward { src },
            } if normalized.layout_plan.value_source(result) == Some(*copy_local)
                && src.value == pair_value.0 =>
            {
                Some(result)
            }
            _ => None,
        })
        .expect("copy forwarding value");
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                statement.kind,
                NStatementKind::Store { ref destination, value }
                    if destination.base == NPlaceBase::Root(copy_root)
                        && value.value == copy_value
            )
        })
    }));

    let borrow = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                expr:
                    NExpr::Borrow {
                        place,
                        kind: BorrowKind::Ref,
                        ..
                    },
                ..
            } => Some(place),
            _ => None,
        })
        .expect("borrow expression");
    assert_eq!(borrow.base, NPlaceBase::Root(copy_root));
    assert!(borrow.path.is_empty());
}

#[test]
fn nested_place_reads_normalize_as_one_composite_place() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Table {
    used: [u8; 4],
    keys: [u256; 4],
    values: [u256; 4],
}

impl Table {
    fn get_used(self, _ slot: usize) -> u8 {
        self.used[slot]
    }

    fn get_keys(self, _ slot: usize) -> u256 {
        self.keys[slot]
    }

    fn get_values(self, _ slot: usize) -> u256 {
        self.values[slot]
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    for (name, field, elem_ty) in [
        ("get_used", 0, "u8"),
        ("get_keys", 1, "u256"),
        ("get_values", 2, "u256"),
    ] {
        let normalized = normalized_func_body(&db, top_mod, name);
        let mut saw_nested_read = false;
        for stmt in normalized
            .body
            .blocks
            .iter()
            .flat_map(|block| block.statements.iter())
        {
            let NStatementKind::Define {
                result,
                expr: NExpr::Load { place, .. },
            } = &stmt.kind
            else {
                continue;
            };
            let value = &normalized.body.values[result.index()];
            if value.ty.pretty_print(&db) == elem_ty {
                assert!(
                    matches!(
                        place.path.iter().cloned().collect::<Vec<_>>().as_slice(),
                        [
                            NDataProjection::Field(path_field),
                            NDataProjection::Index(NIndex::Value(_))
                        ] if usize::from(path_field.0) == field
                    ),
                    "unexpected nested place path in {name}: {:?}",
                    place.path
                );
                saw_nested_read = true;
            }
            assert!(
                !(value.ty.array_len(&db).is_some()
                    && place.path.iter().cloned().collect::<Vec<_>>()
                        == vec![NDataProjection::Field(
                            fe_hir::analysis::semantic::FieldIndex(field as u16)
                        )]),
                "unexpected intermediate whole-array read in {name}: {stmt:?}"
            );
        }
        assert!(
            saw_nested_read,
            "missing nested array element read in {name}"
        );
    }
}

#[test]
fn owned_aggregate_value_boundaries_stay_unrooted() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Table {
    used: [u8; 4],
}

impl Table {
    fn get(own self, _ slot: usize) -> u8 {
        let used = self.used
        used[slot]
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "get");
    let source = normalized.body.owner.body(&db);

    let used_local = source
        .locals
        .iter()
        .enumerate()
        .find_map(|(idx, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
                if local.ty.array_len(&db).is_some() =>
            {
                Some(fe_hir::analysis::semantic::SLocalId::new(idx))
            }
            _ => None,
        })
        .expect("owned array local");
    assert!(
        normalized.body.roots.iter().enumerate().all(|(index, _)| {
            let root_id = fe_hir::analysis::semantic::NRootId::new(index);
            normalized.layout_plan.root_source(root_id) != Some(used_local)
        }),
        "immutable owned array projection should not force a local-slot root"
    );
    assert!(normalized.body.blocks.iter().any(|block| {
        block.statements.iter().any(|statement| {
            matches!(
                &statement.kind,
                NStatementKind::Define {
                    expr: NExpr::ProjectValue { path, .. },
                    ..
                } if path.0.iter().eq([&NDataProjection::Field(
                        fe_hir::analysis::semantic::FieldIndex(0),
                    )])
            )
        })
    }));

    let element_path = normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| block.statements.iter())
        .find_map(|stmt| match &stmt.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::ProjectValue { path, .. },
            } if normalized.body.values[result.index()].ty.pretty_print(&db) == "u8" => Some(path),
            _ => None,
        })
        .expect("element read");
    assert!(
        matches!(
            element_path
                .0
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .as_slice(),
            [NDataProjection::Index(NIndex::Value(_))]
        ),
        "unexpected owned-local projection path: {:?}",
        element_path.0
    );
}

#[test]
fn zero_sized_aggregate_fixture_instances_normalize_and_borrowcheck() {
    for_each_fixture_instance(
        include_str!("../../codegen/tests/fixtures/zero_sized_aggregates.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                )
            });
            if let Err(err) = normalize_semantic_body(db, instance) {
                panic!(
                    "normalize failed for {} ({:?}): {err:?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                );
            }
            if let Err(diag) = check_semantic_borrows(db, instance) {
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                );
            }
        },
    );
}

#[test]
fn if_let_fixture_instances_normalize_and_borrowcheck() {
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/if_let_while_let.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}\nraw={raw:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                )
            });
            if let Err(diag) = check_semantic_borrows(db, instance) {
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                );
            }
        },
    );
}

#[test]
fn custom_effect_handle_fixture_instances_normalize_and_borrowcheck() {
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/effect_handle_representation.fe"),
        |db, instance| {
            let raw = instance.body(db);
            let local_types = raw
                .locals
                .iter()
                .map(|local| {
                    format!(
                        "{} role={:?}",
                        local.ty.pretty_print(db),
                        local
                            .role
                            .root_provider(&raw.locals)
                            .map(|provider| provider.provider_ty.pretty_print(db)),
                    )
                })
                .collect::<Vec<_>>();
            let artifacts = normalize_raw_body(db, instance, raw, instance.assumptions(db))
                .unwrap_or_else(|error| {
                    panic!(
                        "phase-one normalization failed for {} ({:?}): {error:#?}\nlocal_types={local_types:#?}\nraw={raw:#?}",
                        owner_name(db, instance.key(db).owner(db)),
                        instance.key(db),
                    )
                });
            verify_normalized_body(db, &artifacts.body).unwrap_or_else(|error| {
                panic!(
                    "phase-one normalized body failed verification for {} ({:?}): {error:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                )
            });
            if let Err(diag) = check_semantic_borrows(db, instance) {
                panic!(
                    "borrowck failed for {} ({:?}): {diag:#?}\nraw={raw:#?}\nnormalized={:#?}",
                    owner_name(db, instance.key(db).owner(db)),
                    instance.key(db),
                    artifacts.body,
                );
            }
        },
    );
}

#[test]
fn decompose_ty_app_handles_deep_ty_app_chains_iteratively() {
    let db = HirAnalysisTestDb::default();
    let arg = fe_hir::analysis::ty::ty_def::TyId::u256(&db);
    let mut ty = fe_hir::analysis::ty::ty_def::TyId::bool(&db);
    for _ in 0..10_000 {
        ty = fe_hir::analysis::ty::ty_def::TyId::new(&db, TyData::TyApp(ty, arg));
    }
    assert_eq!(
        ty.base_ty(&db),
        fe_hir::analysis::ty::ty_def::TyId::bool(&db)
    );
    assert_eq!(ty.generic_args(&db).len(), 10_000);
}

#[test]
fn erc20_has_role_self_ty_app_chain_is_acyclic() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        include_str!("../../codegen/tests/fixtures/erc20.fe"),
    );
    let (top_mod, _) = db.top_mod(file);
    let has_role = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "has_role") =>
            {
                Some(func)
            }
            _ => None,
        })
        .expect("has_role fixture function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(*has_role)),
    );
    let ty = instance.body(&db).locals[0].ty;
    let mut seen = rustc_hash::FxHashSet::default();
    let mut cursor = ty;
    loop {
        assert!(seen.insert(cursor), "cyclic ty app chain at {:?}", cursor);
        match cursor.data(&db) {
            TyData::TyApp(lhs, _) => cursor = *lhs,
            _ => break,
        }
    }
}

#[test]
fn array_of_struct_place_lowers_with_resolved_index_then_field() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Subtree {
    left: u256,
    right: u256,
}

struct Tree {
    last_subtrees: [Subtree; 8],
}

fn write(mut tree: Tree, i: usize, h: u256) -> Tree {
    tree.last_subtrees[i].left = h
    tree
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "write") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("write instance");
    let body = instance.body(&db);
    let dst = body
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            SStmtKind::Store { dst, .. } => Some(dst),
            SStmtKind::Assign { .. } => None,
        })
        .expect("store statement");

    assert_eq!(dst.path.len(), 3);
    let path = dst.path.iter().collect::<Vec<_>>();
    assert!(matches!(path[0], Projection::Field(0)));
    assert!(matches!(path[1], Projection::Index(_)));
    assert!(matches!(path[2], Projection::Field(0)));
}

#[test]
fn nested_borrowed_parameter_access_retains_each_capability_target() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "nested_input_referents.fe".into(),
        r#"
struct Inner { value: mut u256 }
struct Outer { inner: mut Inner }
fn nested(outer: mut Outer) -> mut u256 {
    mut outer.inner.value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let artifacts = normalized_func_body(&db, top_mod, "nested");
    let body = &artifacts.body;
    let carriers: Vec<_> = body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| match &statement.kind {
            NStatementKind::Define {
                expr: NExpr::Load { place, .. } | NExpr::Borrow { place, .. },
                ..
            } => match place.base {
                NPlaceBase::CapabilityTarget { carrier } => Some(carrier),
                NPlaceBase::Root(_) => None,
            },
            _ => None,
        })
        .collect();
    assert_eq!(carriers.len(), 3);
    assert!(carriers.windows(2).all(|pair| pair[0] != pair[1]));
}

#[test]
fn projected_capability_reborrows_load_the_handle_before_borrowing_its_referent() {
    for_each_fixture_instance(
        r#"
struct Inner { value: mut u256 }
struct Outer { inner: mut Inner }
fn shared(outer: ref Outer) -> ref u256 { ref outer.inner.value }
fn mutable_array(values: mut [mut u256; 2], index: usize) -> mut u256 { mut values[index] }
fn shared_array(values: ref [mut u256; 2], index: usize) -> ref u256 { ref values[index] }
fn local(value: mut u256) -> mut u256 {
    let mut holder = Inner { value }
    mut holder.value
}
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("projected reborrow must be admitted");
            let body = &artifacts.body;
            let expected = instance.normalized_result_ty(db);
            let (kind, target) = expected.as_borrow(db).unwrap();
            let (result, place) = body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .find_map(|statement| match &statement.kind {
                    NStatementKind::Define {
                        result,
                        expr:
                            NExpr::Borrow {
                                place,
                                kind: actual,
                                ..
                            },
                    } if *actual == kind && body.values[result.index()].ty == expected => {
                        Some((*result, place))
                    }
                    _ => None,
                })
                .expect("returned reborrow");
            assert_eq!(place.ty, target);
            assert!(place.path.is_empty());
            let NPlaceBase::CapabilityTarget { carrier } = place.base else {
                panic!("reborrow targets the stored handle")
            };
            assert_ne!(result, carrier);
            if owner_name(db, instance.key(db).owner(db)) == "local" {
                let backings: Vec<_> = artifacts.layout_plan.use_backings(carrier).collect();
                assert_eq!(
                    backings.len(),
                    1,
                    "the loaded handle retains its parameter backing"
                );
                assert!(backings[0].target.is_empty());
                let NLayoutBackingSource::Value { value, path } = &backings[0].source else {
                    panic!("the handle came from the incoming parameter")
                };
                assert!(path.is_empty());
                assert!(matches!(
                    body.values[value.index()].definition,
                    NValueDefinition::EntryParam { param: 0 }
                ));
            }
            let value = &body.values[carrier.index()];
            assert_eq!(value.ty.as_borrow(db).unwrap().1, target);
            let NValueDefinition::Statement { block, statement } = value.definition else {
                panic!("projected handle must be loaded")
            };
            assert!(matches!(
                &body.blocks[block.index()].statements[statement as usize].kind,
                NStatementKind::Define {
                    expr: NExpr::Load { .. },
                    ..
                }
            ));
        },
    );
}

#[test]
fn borrowing_scalar_fields_and_direct_parameters_keeps_the_existing_target() {
    for_each_fixture_instance(
        r#"
struct Plain { value: u256 }
fn field(value: mut Plain) -> mut u256 { mut value.value }
fn direct(value: mut u256) -> mut u256 { mut value }
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("ordinary borrow must be admitted");
            let body = &artifacts.body;
            assert!(body.blocks.iter().flat_map(|block| &block.statements).all(
                |statement| !matches!(
                    statement.kind,
                    NStatementKind::Define {
                        expr: NExpr::Load { .. },
                        ..
                    }
                )
            ));
            let place = body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .find_map(|statement| match &statement.kind {
                    NStatementKind::Define {
                        expr: NExpr::Borrow { place, .. },
                        ..
                    } => Some(place),
                    _ => None,
                })
                .unwrap();
            let NPlaceBase::CapabilityTarget { carrier } = place.base else {
                panic!("incoming parameter target")
            };
            assert!(matches!(
                body.values[carrier.index()].definition,
                NValueDefinition::EntryParam { param: 0 }
            ));
        },
    );
}

#[test]
fn stores_through_terminal_capability_fields_target_the_referent() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "terminal_capability_store.fe".into(),
        r#"
struct Wrap { handle: mut u256 }
fn inspect() {
    let mut value: u256 = 0
    let mut values = [Wrap { handle: mut value }]
    values[0].handle = 1
}
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
        .expect("inspect function");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(func)),
    );
    assert!(
        !matches!(
            semantic_body_admission(&db, instance),
            SemanticBodyAdmission::Blocked(_)
        ),
        "store body must be HIR-valid"
    );
    let raw = instance.body(&db);
    let artifacts = normalize_raw_body(&db, instance, raw, instance.assumptions(&db))
        .expect("normalized operations");
    let stores: Vec<_> = artifacts
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| {
            let NStatementKind::Store { destination, value } = &statement.kind else {
                return None;
            };
            Some((
                destination.base,
                destination.path.clone(),
                artifacts.body.values[value.value.index()]
                    .ty
                    .pretty_print(&db),
                destination.ty.pretty_print(&db),
            ))
        })
        .collect();
    assert_eq!(
        verify_normalized_body(&db, &artifacts.body),
        Ok(()),
        "store source/destination types: {stores:#?}"
    );
}

#[test]
fn terminal_capability_stores_preserve_loaded_carriers_and_layout_backings() {
    for_each_fixture_instance(
        r#"
struct Wrap { handle: mut u256 }
struct Outer { inner: mut Wrap }
fn local_struct(value: mut u256) {
    let mut holder = Wrap { handle: value }
    holder.handle = 1
}
fn local_array(value: mut u256) {
    let mut holders = [Wrap { handle: value }]
    holders[0].handle = 1
}
fn borrowed_struct(holder: mut Wrap) { holder.handle = 1 }
fn borrowed_array(holders: mut [Wrap; 2]) { holders[0].handle = 1 }
fn borrowed_handles(handles: mut [mut u256; 2]) { handles[1] = 1 }
fn nested(outer: mut Outer) { outer.inner.handle = 1 }
fn dynamic(holders: mut [Wrap; 2], index: usize) { holders[index].handle = 1 }
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("terminal store admission");
            let body = &artifacts.body;
            let stores: Vec<_> = body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .filter_map(|statement| {
                    let NStatementKind::Store { destination, .. } = &statement.kind else {
                        return None;
                    };
                    statement.source.map(|_| destination)
                })
                .collect();
            assert_eq!(stores.len(), 1);
            let destination = stores[0];
            assert!(destination.path.is_empty());
            let NPlaceBase::CapabilityTarget { carrier } = destination.base else {
                panic!("terminal store must follow its loaded capability: {destination:?}")
            };
            let NValueDefinition::Statement { block, statement } =
                body.values[carrier.index()].definition
            else {
                panic!("terminal store requires an explicit handle load")
            };
            let NStatementKind::Define {
                expr:
                    NExpr::Load {
                        place,
                        mode: ReadMode::Copy,
                    },
                ..
            } = &body.blocks[block.index()].statements[statement as usize].kind
            else {
                panic!("terminal carrier must be loaded from its structural slot")
            };
            assert_eq!(
                place.ty.as_borrow(db),
                Some((BorrowKind::Mut, destination.ty))
            );
            assert!(!place.path.is_empty());
        },
    );
}

#[test]
fn terminal_store_normalization_preserves_direct_targets_and_aggregate_replacement() {
    for_each_fixture_instance(
        r#"
struct Wrap { handle: mut u256 }
struct Scalar { value: u256 }
fn direct(value: mut u256) { value = 1 }
fn scalar_field(value: mut Scalar) { value.value = 1 }
fn whole_element(values: mut [Wrap; 1], replacement: mut u256) {
    values[0] = Wrap { handle: replacement }
}
fn whole_local_element(replacement: mut u256) {
    let mut value: u256 = 0
    let mut values = [Wrap { handle: mut value }]
    values[0] = Wrap { handle: replacement }
}
"#,
        |db, instance| {
            let artifacts =
                normalize_semantic_body(db, instance).expect("ordinary store admission");
            let body = &artifacts.body;
            for statement in body.blocks.iter().flat_map(|block| &block.statements) {
                if let NStatementKind::Store { destination, value } = &statement.kind {
                    assert_eq!(destination.ty, body.values[value.value.index()].ty);
                    if let NPlaceBase::CapabilityTarget { carrier } = destination.base {
                        assert!(
                            matches!(
                                body.values[carrier.index()].definition,
                                NValueDefinition::EntryParam { .. }
                            ),
                            "ordinary stores must keep their direct parameter target"
                        );
                    }
                }
            }
        },
    );
}

#[test]
fn call_result_aggregate_retains_embedded_borrow_loans() {
    let diags = checked_borrow_diags(
        r#"
struct Wrap {
    handle: mut u256,
    tag: u256,
}

fn wrap(handle: mut u256) -> Wrap {
    Wrap { handle, tag: 0 }
}

fn bad() {
    let mut value = 0
    let mut wrapped = wrap(handle: mut value)
    let alias = mut value
    alias = 1
    wrapped.handle = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn aggregate_return_cannot_hide_borrow_of_local() {
    let diags = borrow_diags(
        r#"
struct Wrap {
    handle: mut u256,
}

fn bad() -> Wrap {
    let mut value = 0
    Wrap { handle: mut value }
}
"#,
    );

    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags}"
    );
    assert!(
        diags.contains("cannot return a value that holds a borrow of local `value`"),
        "{diags}"
    );
}

#[test]
fn aggregate_call_arguments_check_embedded_borrow_aliases() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn write_both(mut left: own Borrowed, mut right: own Borrowed) {
    left.value = 1
    right.value = 2
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    let left = Borrowed { value: borrowed }
    let right = Borrowed { value: borrowed }
    write_both(left: left, right: right)
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
    assert!(
        diags.contains("call arguments require conflicting access"),
        "{diags}"
    );
}

#[test]
fn array_call_arguments_require_distinct_mutable_members() {
    let diags = borrow_diags(
        r#"
fn write_both(_ values: own [mut u256; 2]) {
    let first = values[0]
    let second = values[1]
    first = 1
    second = 2
}

fn read_both(_ values: own [ref u256; 2]) {}
fn write_one(_ values: own [mut u256; 1]) {}

fn valid() {
    let mut left = 0
    let mut right = 0
    write_both([mut left, mut right])
}

fn duplicate_mutable() {
    let mut value = 0
    let handle = mut value
    write_both([handle, handle])
}

fn duplicate_shared_is_valid() {
    let value = 0
    read_both([ref value, ref value])
}

fn length_one_is_valid() {
    let mut value = 0
    write_one([mut value])
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn duplicate_mutable`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn duplicate_shared_is_valid`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn length_one_is_valid`"),
        "{diags}"
    );
}

#[test]
fn nested_array_and_product_call_arguments_require_injective_borrows() {
    let diags = borrow_diags(
        r#"
struct BorrowArray {
    values: [mut u256; 2],
}

fn consume_nested(_ values: own [[mut u256; 2]; 2]) {}
fn consume_product(_ value: own BorrowArray) {}

fn duplicate_nested() {
    let mut value = 0
    let mut other_left = 0
    let mut other_right = 0
    let handle = mut value
    consume_nested([[handle, mut other_left], [mut other_right, handle]])
}

fn duplicate_in_product() {
    let mut value = 0
    let handle = mut value
    consume_product(BorrowArray { values: [handle, handle] })
}
"#,
    );

    assert!(
        diags.contains("borrow conflict in `fn duplicate_nested`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_in_product`"),
        "{diags}"
    );
}

#[test]
fn borrowed_aggregate_call_arguments_preserve_nested_borrows() {
    let diags = checked_borrow_diags(
        r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

fn consume_array(_ values: mut [mut u256; 2]) {}
fn consume_pair(_ pair: mut Pair) {}
fn consume_view(_ values: [mut u256; 2]) {}

fn valid_array() {
    let mut left = 0
    let mut right = 0
    let mut values = [mut left, mut right]
    consume_array(values: mut values)
}

fn duplicate_array() {
    let mut value = 0
    let handle = mut value
    let mut values = [handle, handle]
    consume_array(values: mut values)
}

fn valid_pair() {
    let mut left = 0
    let mut right = 0
    let mut pair = Pair { left: mut left, right: mut right }
    consume_pair(pair: mut pair)
}

fn duplicate_pair() {
    let mut value = 0
    let handle = mut value
    let mut pair = Pair { left: handle, right: handle }
    consume_pair(pair: mut pair)
}

fn duplicate_view_control() {
    let mut value = 0
    let handle = mut value
    let values = [handle, handle]
    consume_view(values: values)
}

fn duplicate_local_control() {
    let mut value: u256 = 0
    let handle = mut value
    let values = [handle, handle]
    let first = values[0]
    let second = values[1]
    first = 1
    second = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn valid_array`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_array`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn valid_pair`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_pair`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_view_control`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_local_control`"),
        "{diags}"
    );
}

#[test]
fn borrowed_aggregate_effect_arguments_preserve_nested_borrows() {
    let source = r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

fn consume() uses (pair: mut Pair) {
    let selected = pair.left
    selected = 1
}

fn valid_effect() {
    let mut left = 0
    let mut right = 0
    let mut pair = Pair { left: mut left, right: mut right }
    let target = mut pair
    with (target) {
        consume()
    }
}

fn duplicate_effect() {
    let mut value = 0
    let handle = mut value
    let mut pair = Pair { left: handle, right: handle }
    let target = mut pair
    with (target) {
        consume()
    }
}
"#;
    let diags = borrow_diags(source);

    assert!(
        !diags.contains("borrow conflict in `fn valid_effect`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn duplicate_effect`"),
        "{diags}"
    );
}

#[test]
fn borrowed_aggregate_call_results_preserve_nested_borrows() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

struct Target {
    first: u256,
    second: u256,
}

struct Carrier {
    handle: mut Target,
    plain: u256,
}

fn forward(_ pair: mut Pair) -> mut Pair {
    pair
}

fn forward_carrier(_ carrier: mut Carrier) -> mut Carrier {
    carrier
}

fn distinct_wrapper_and_descendant_targets() {
    let mut target = Target { first: 0, second: 0 }
    let mut carrier = Carrier { handle: mut target, plain: 0 }
    let returned = forward_carrier(carrier: mut carrier)
    let outer_field = mut returned.plain
    let nested = returned.handle
    let nested_field = mut nested.second
    nested_field = 1
    outer_field = 2
}

fn conflict_after_forwarding() {
    let mut left = 0
    let mut right = 0
    let mut pair = Pair { left: mut left, right: mut right }
    let returned = forward(pair: mut pair)
    let selected = returned.left
    let alias = mut left
    alias = 1
    selected = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn distinct_wrapper_and_descendant_targets`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn conflict_after_forwarding`"),
        "{diags}"
    );
}

#[test]
fn array_enum_variants_are_exclusive_only_within_one_element() {
    let diags = borrow_diags(
        r#"
enum Choice {
    A(mut u256),
    B(mut u256),
}

fn consume(_ values: own [Choice; 2]) {}

fn valid() {
    let mut left = 0
    let mut right = 0
    consume([Choice::A(mut left), Choice::B(mut right)])
}

fn aliases_across_elements() {
    let mut value = 0
    let handle = mut value
    consume([Choice::A(handle), Choice::B(handle)])
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn aliases_across_elements`"),
        "{diags}"
    );
}

#[test]
fn reading_one_returned_aggregate_borrow_does_not_retain_siblings() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

fn forward(_ pair: own Pair) -> Pair {
    pair
}

fn valid() {
    let mut left = 0
    let mut right = 0
    let returned = forward(Pair { left: mut left, right: mut right })
    let selected = returned.left
    let other = mut right
    other = 1
    selected = 2
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_array_family_preserves_constant_and_dynamic_aliasing() {
    let diags = borrow_diags(
        r#"
fn forward(_ values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn constant_sibling_is_disjoint() {
    let mut left = 0
    let mut right = 0
    let returned = forward([mut left, mut right])
    let first = returned[0]
    let other = mut right
    other = 1
    first = 2
}

fn dynamic_index_overlaps(index: usize) {
    let mut left = 0
    let mut right = 0
    let returned = forward([mut left, mut right])
    let selected = returned[index]
    let other = mut right
    other = 1
    selected = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn constant_sibling_is_disjoint`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn dynamic_index_overlaps`"),
        "{diags}"
    );
}

#[test]
fn parameter_array_family_checks_dynamic_member_aliasing() {
    let diags = borrow_diags(
        r#"
fn constant_siblings_are_disjoint(values: own [mut u256; 2]) {
    let first = values[0]
    let second = values[1]
    first = 1
    second = 2
}

fn dynamic_overlaps_constant(values: own [mut u256; 2], index: usize) {
    let selected = values[index]
    let first = values[0]
    selected = 1
    first = 2
}

fn dynamic_indices_may_alias(
    values: own [mut u256; 2],
    left_index: usize,
    right_index: usize,
) {
    let left = values[left_index]
    let right = values[right_index]
    left = 1
    right = 2
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn constant_siblings_are_disjoint`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn dynamic_overlaps_constant`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn dynamic_indices_may_alias`"),
        "{diags}"
    );
}

#[test]
fn exact_array_overwrites_partition_symbolic_families() {
    let diags = borrow_diags(
        r#"
struct Wrap {
    handle: mut u256,
}

fn forward(_ values: own [Wrap; 2]) -> [Wrap; 2] {
    values
}

fn replace_first(
    mut _ values: own [Wrap; 2],
    replacement: own Wrap,
) -> [Wrap; 2] {
    values[0] = replacement
    values
}

fn local_replacement_releases_old_member() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
    )
    values[0] = Wrap { handle: mut replacement }
    let released = mut old_left
    released = 1
    values[0].handle = 2
    values[1].handle = 3
}

fn sibling_remains_borrowed() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
    )
    values[0] = Wrap { handle: mut replacement }
    let alias = mut right
    alias = 1
    values[1].handle = 2
}

fn helper_return_preserves_override() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut returned = replace_first(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
        replacement: Wrap { handle: mut replacement },
    )
    let released = mut old_left
    released = 1
    returned[0].handle = 2
    returned[1].handle = 3
}

fn conditional_replacement_keeps_old(condition: bool) {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut right }],
    )
    if condition {
        values[0] = Wrap { handle: mut replacement }
    }
    let alias = mut old_left
    alias = 1
    values[0].handle = 2
}

fn replace_all_local_members(left: mut u256, right: mut u256) -> [Wrap; 2] {
    let mut old_left = 0
    let mut old_right = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut old_right }],
    )
    values[0] = Wrap { handle: left }
    values[1] = Wrap { handle: right }
    values
}

fn retain_one_local_member(left: mut u256) -> [Wrap; 2] {
    let mut old_left = 0
    let mut old_right = 0
    let mut values = forward(
        [Wrap { handle: mut old_left }, Wrap { handle: mut old_right }],
    )
    values[0] = Wrap { handle: left }
    values
}
"#,
    );

    assert!(
        !diags.contains("borrow conflict in `fn local_replacement_releases_old_member`"),
        "{diags}"
    );
    assert!(
        !diags.contains("borrow conflict in `fn helper_return_preserves_override`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn sibling_remains_borrowed`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn conditional_replacement_keeps_old`"),
        "{diags}"
    );
    assert!(
        !diags.contains("invalid return borrow in `fn replace_all_local_members`"),
        "{diags}"
    );
    assert!(
        diags.contains("invalid return borrow in `fn retain_one_local_member`"),
        "{diags}"
    );
}

#[test]
fn array_member_reborrow_suspends_only_that_parent_member() {
    let diags = checked_borrow_diags(
        r#"
fn forward(_ values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn reborrow(value: mut u256) -> mut u256 {
    value
}

fn valid() {
    let mut left = 0
    let mut right = 0
    let mut returned = forward([mut left, mut right])
    let first = reborrow(value: returned[0])
    returned[1] = 1
    first = 2
}

fn bad() {
    let mut left = 0
    let mut right = 0
    let mut returned = forward([mut left, mut right])
    let first = reborrow(value: returned[0])
    let alias = mut right
    alias = 1
    returned[1] = 2
    first = 3
}

fn transitive_bad() {
    let mut left = 0
    let mut right = 0
    let mut returned = forward([mut left, mut right])
    let first = reborrow(value: returned[0])
    let first_again = reborrow(value: first)
    let alias = mut right
    alias = 1
    returned[1] = 2
    first_again = 3
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn transitive_bad`"),
        "{diags}"
    );
}

#[test]
fn mutually_exclusive_enum_borrow_slots_do_not_conflict() {
    let diags = borrow_diags(
        r#"
enum Choice {
    A(mut u256),
    B(mut u256),
}

struct Pair {
    left: Choice,
    right: Choice,
}

fn consume(_ choice: own Choice) {}
fn consume_pair(_ pair: own Pair) {}

fn valid(condition: bool) {
    let mut value = 0
    let choice = if condition {
        Choice::A(mut value)
    } else {
        Choice::B(mut value)
    }
    consume(choice)
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    let pair = Pair {
        left: Choice::A(borrowed),
        right: Choice::B(borrowed),
    }
    consume_pair(pair)
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn reading_a_forwarded_borrow_as_a_value_drops_loan_state() {
    let diags = borrow_diags(
        r#"
struct Holder {
    tag: u256,
}

impl Holder {
    fn forward(mut self, _ value: mut u256) -> mut u256 {
        value
    }
}

fn valid() -> u256 {
    let mut holder = Holder { tag: 0 }
    let mut local = 7
    let value = holder.forward(mut local)
    value += 5
    value
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn mutable_receiver_reservation_activates_after_argument_evaluation() {
    let diags = borrow_diags(
        r#"
struct Cell {
    value: u256,
}

impl Cell {
    fn read(self) -> u256 {
        self.value
    }

    fn write(mut self, value: u256) {
        self.value = value
    }

    fn increment(mut self) -> u256 {
        self.value += 1
        self.value
    }
}

fn valid() {
    let mut cell = Cell { value: 1 }
    cell.write(value: cell.read())
    cell.write(value: cell.increment())
}

fn bad() -> u256 {
    let mut cell = Cell { value: 1 }
    let borrowed = ref cell.value
    cell.write(value: cell.read())
    borrowed
}
"#,
    );

    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn recursive_aggregate_borrow_summary_converges() {
    let diags = checked_borrow_diags(
        r#"
struct Owner {
    value: u256,
}

struct Borrowed {
    value: mut u256,
}

fn borrow_value(owner: mut Owner, recurse: bool) -> Borrowed {
    if recurse {
        borrow_value(owner, recurse: false)
    } else {
        Borrowed { value: mut owner.value }
    }
}

fn bad() {
    let mut owner = Owner { value: 0 }
    let mut borrowed = borrow_value(owner: mut owner, recurse: true)
    let other = mut owner.value
    other = 1
    borrowed.value = 2
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags}");
}

#[test]
fn opaque_aggregate_return_summary_is_conservative() {
    assert_pending_validation(
        r#"
struct Borrowed {
    value: mut u256,
}

trait BorrowValue {
    fn borrow_value(mut self) -> Borrowed
}

fn bad<T: BorrowValue>(value: mut T) {
    let mut first = value.borrow_value()
    let mut second = value.borrow_value()
    first.value = 1
    second.value = 2
}
"#,
        "bad",
    );
}

#[test]
fn opaque_array_result_does_not_assume_pointwise_family_correlation() {
    assert_pending_validation(
        r#"
trait Permute {
    fn permute(self, values: own [mut u256; 2]) -> [mut u256; 2]
}

fn bad<T: Permute>(permuter: T) {
    let mut left = 0
    let mut right = 0
    let returned = permuter.permute(values: [mut left, mut right])
    let selected = returned[0]
    let alias = mut right
    alias = 1
    selected = 2
}
"#,
        "bad",
    );
}

#[test]
fn mutable_input_poststates_match_inline_handle_replacement() {
    let diagnostics = borrow_diags(
        r#"struct Wrap {
    handle: mut u256,
}

fn replace(values: mut [Wrap; 1], replacement: mut u256) {
    values[0] = Wrap { handle: replacement }
}

fn call_replaces_nested_handle() {
    let mut first: u256 = 0
    let mut second: u256 = 0
    let mut values = [Wrap { handle: mut first }]
    replace(values: mut values, replacement: mut second)
    let competing = mut second
    values[0].handle = 1
    competing = 2
}

fn direct_replaces_nested_handle() {
    let mut first: u256 = 0
    let mut second: u256 = 0
    let mut values = [Wrap { handle: mut first }]
    values[0] = Wrap { handle: mut second }
    let competing = mut second
    values[0].handle = 1
    competing = 2
}
"#,
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn call_replaces_nested_handle`"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn direct_replaces_nested_handle`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("internal borrow checking error"),
        "{diagnostics}"
    );
}

#[test]
fn opaque_handle_construction_and_summary_choices_preserve_declared_contracts() {
    let source = r#"
use core::{AddressSpace, EffectHandle}
struct Ptr { addr: u256 }
impl EffectHandle for Ptr {
    type Target = u256
    const SPACE: AddressSpace = AddressSpace::Memory
    type Raw = u256
    fn raw(self) -> u256 { self.addr }
}
fn copied(_ raw: u256) -> [Ptr; 2] {
    let ptr = Ptr { addr: raw }
    [ptr, ptr]
}
fn constructed() -> [Ptr; 2] { [Ptr { addr: 32 }, Ptr { addr: 32 }] }
fn forwarded(_ raw: u256) -> [Ptr; 2] { copied(raw) }
"#;
    assert!(checked_trusted_borrow_diags(source).is_empty());
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_trusted_effect_handle_module("opaque_handles.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    for (name, expected_choices) in [("copied", 1), ("constructed", 2), ("forwarded", 1)] {
        let artifacts = normalized_func_body(&db, top_mod, name);
        let summary = semantic_borrow_summary(&db, artifacts.body.owner)
            .unwrap()
            .unwrap();
        let values = ValueInterner::new(&db, ValueLimits::default());
        let mut choices = Vec::new();
        for leaf in values.leaves(&summary.result, ValueOccurrence::Summary) {
            let ExternalOrigin::OpaqueHandle(source) = leaf.payload.source.origin else {
                panic!("missing opaque constructor source")
            };
            assert!(matches!(source.occurrence, AddressOccurrence::Summary(_)));
            assert_eq!(
                source.contract.address_space,
                HandleAddressSpace::Known(ProviderAddressSpace::Memory)
            );
            assert_eq!(source.contract.target_ty.pretty_print(&db), "u256");
            choices.push(source.occurrence);
        }
        choices.sort();
        choices.dedup();
        assert_eq!(choices.len(), expected_choices, "{name}");
    }
    let mut artifacts = normalized_func_body(&db, top_mod, "constructed");
    let expr = artifacts
        .body
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.statements)
        .find_map(|statement| {
            if let NStatementKind::Define {
                expr: expr @ NExpr::MakeHandle { .. },
                ..
            } = &mut statement.kind
            {
                Some(expr)
            } else {
                None
            }
        })
        .expect("explicit constructor");
    let NExpr::MakeHandle {
        origin: HandleOrigin::Opaque(contract),
        ..
    } = expr
    else {
        panic!("opaque constructor")
    };
    contract.address_space = HandleAddressSpace::Known(ProviderAddressSpace::Storage);
    assert_eq!(
        verify_normalized_body(&db, &artifacts.body),
        Err(NormalizedBodyVerifyError::InvalidHandleOrigin)
    );
}

#[test]
fn call_results_used_as_views_have_explicit_materialization() {
    let source = r#"
struct Cell { value: u256 }
impl Cell { fn inspect(self) -> u256 { self.value } }
fn make() -> Cell { Cell { value: 1 } }
fn validate() { let value = make().inspect() }
"#;
    let diags = checked_borrow_diags(source);
    assert!(diags.is_empty(), "{diags}");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("call_result_view.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let artifacts = normalized_func_body(&db, top_mod, "validate");
    assert!(
        artifacts
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .any(|statement| matches!(
                &statement.kind,
                NStatementKind::Define {
                    expr: NExpr::MakeView {
                        place: NPlace {
                            base: NPlaceBase::Root(_),
                            ..
                        },
                        ..
                    },
                    ..
                }
            ))
    );
}

#[test]
fn shared_member_receivers_create_views_of_the_referent() {
    let diagnostics = checked_borrow_diags(
        r#"
trait Measure { fn size(self) -> usize }
impl Measure for u256 { fn size(self) -> usize { 1 } }
struct Wrapped<T> { base: ref T }
impl<T: Measure> Wrapped<T> {
    fn size(self) -> usize { self.base.size() }
}
fn measure(value: ref u256) -> usize {
    Wrapped { base: value }.size()
}
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn diverging_calls_do_not_construct_contextual_capability_results() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Wrap { handle: mut u256 }
fn abort() -> ! { core::panic() }
fn unreachable_result() -> Wrap { abort() }
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn diverging_calls_still_check_argument_aliasing() {
    let diagnostics = checked_borrow_diags(
        r#"
fn abort(left: mut u256, right: mut u256) -> ! { core::panic() }
fn invalid(value: mut u256) { abort(left: value, right: value) }
"#,
    );
    assert!(diagnostics.contains("borrow conflict"), "{diagnostics}");
}

#[test]
fn nonreturning_wrappers_use_the_structural_summary_fixed_point() {
    let diagnostics = checked_borrow_diags(
        r#"
fn stop() { core::panic() }
fn middle() { stop() }
fn caller() {
    let mut value: u256 = 0
    let live = mut value
    middle()
    value = 1
    live = 2
}
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn generic_effect_handle_bounds_retain_their_declared_target() {
    let diagnostics = checked_borrow_diags(
        r#"
use core::{EffectHandle, EffectRef, Copy}
fn read_generic<H: EffectHandle>(_ handle: H) -> H::Target
    uses (value: H::Target) where H::Target: Copy { value }
fn write_generic<H: EffectHandle>(_ handle: H, value: H::Target)
    uses (target: mut H::Target) { target = value }
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn loop_carried_borrows_preserve_previous_occurrences() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Wrap { handle: mut u256 }
fn both(left: mut u256, right: mut u256) {
    left = 1
    right = 2
}
fn carried(x: mut u256, y: mut u256, again: bool) {
    let mut holder = Wrap { handle: y }
    while again {
        let next = mut x
        both(left: next, right: holder.handle)
        holder = Wrap { handle: next }
    }
}
fn independent(x: mut u256, y: mut u256, again: bool) {
    while again {
        let next = mut x
        both(left: next, right: mut y)
    }
}
"#,
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn carried`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("borrow conflict in `fn independent`"),
        "{diagnostics}"
    );
    assert!(!diagnostics.contains("internal"), "{diagnostics}");
}

#[test]
fn nominal_handles_only_access_targets_at_effect_boundaries() {
    let diags = checked_trusted_borrow_diags(
        r#"
use core::{AddressSpace, EffectHandle, EffectRef, EffectRefMut}
struct Ptr { addr: u256 }
impl EffectHandle for Ptr {
    type Target = u256
    const SPACE: AddressSpace = AddressSpace::Memory
    type Raw = u256
    fn raw(self) -> u256 { self.addr }
}
impl EffectRef<u256> for Ptr {}
impl EffectRefMut<u256> for Ptr {}
fn consume(first: Ptr, second: Ptr) {}
fn update() uses (value: mut u256) { value = 1 }
fn inspect() -> u256 uses (value: u256) { value }
fn valid() {
    let ptr = Ptr { addr: 32 }
    consume(first: ptr, second: ptr)
    with (ptr) { update() }
}
fn conflict() -> u256 {
    let mut value: u256 = 0
    let borrowed = mut value
    let ptr = Ptr { addr: 32 }
    let result = with (ptr) { inspect() }
    borrowed = 2
    result
}
"#,
    );
    assert!(!diags.contains("borrow conflict in `fn valid`"), "{diags}");
    assert!(
        diags.contains("borrow conflict in `fn conflict`"),
        "{diags}"
    );
}

#[test]
fn boundary_storage_uses_populated_capabilities() {
    for (assignment, rejected) in [
        ("slot = Maybe::Empty", false),
        ("slot = Maybe::Full(mut local)", true),
        (
            "let mut value = Maybe::Full(mut local)\nvalue = Maybe::Empty\nslot = value",
            false,
        ),
        ("slot = empty()", false),
    ] {
        let source = format!(
            r#"
enum Maybe {{ Empty, Full(mut u256) }}
fn empty() -> Maybe {{ Maybe::Empty }}
pub contract Store {{
    mut slot: Maybe
    init() uses (mut slot) {{
        let mut local: u256 = 0
        {assignment}
    }}
}}
"#
        );
        let diags = checked_borrow_diags(&source);
        if rejected {
            assert!(
                diags.contains("cannot store `Maybe` in storage"),
                "{assignment}: {diags}"
            );
        } else {
            assert!(diags.is_empty(), "{assignment}: {diags}");
        }
    }
    let diags = checked_borrow_diags(
        r#"
pub contract EmptyArray {
    mut slot: [mut u256; 0]
    init() uses (mut slot) { slot = [] }
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn boundary_transport_checks_nested_mutable_handles() {
    for argument in [
        "owned(Wrap { handle: mut slot })",
        "viewed(Wrap { handle: mut slot })",
        "let mut wrapper = Wrap { handle: mut slot }\nborrowed(mut wrapper)",
        "array([mut slot])",
        "variant(Maybe::Full(mut slot))",
    ] {
        let source = format!(
            r#"
struct Wrap {{ handle: mut u256 }}
enum Maybe {{ Empty, Full(mut u256) }}
fn owned(_ value: own Wrap) {{}}
fn viewed(_ value: Wrap) {{}}
fn borrowed(_ value: mut Wrap) {{}}
fn array(_ value: own [mut u256; 1]) {{}}
fn variant(_ value: own Maybe) {{}}
pub contract Call {{
    mut slot: u256
    init() uses (mut slot) {{ {argument} }}
}}
"#
        );
        let diags = checked_borrow_diags(&source);
        assert!(
            diags.contains("transport violation in `fn Call::__init__`"),
            "{argument}: {diags}"
        );
        assert!(
            diags.contains("from storage as function argument"),
            "{argument}: {diags}"
        );
        assert!(!diags.contains("borrow conflict"), "{argument}: {diags}");
    }
    let diags = checked_borrow_diags(
        r#"
struct Wrap { handle: mut u256 }
enum Maybe { Empty, Full(mut u256) }
fn owned(_ value: own Wrap) {}
fn variant(_ value: own Maybe) {}
fn memory() {
    let mut local: u256 = 0
    owned(Wrap { handle: mut local })
    variant(Maybe::Empty)
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

fn boundary_provider_source(space: &str, target: &str, body: &str) -> String {
    format!(
        r#"
use core::{{AddressSpace, EffectHandle, EffectRef, EffectRefMut}}
struct Ptr {{ addr: u256 }}
impl EffectHandle for Ptr {{
    type Target = {target}
    const SPACE: AddressSpace = AddressSpace::{space}
    type Raw = u256
    fn raw(self) -> u256 {{ self.addr }}
}}
impl EffectRef<{target}> for Ptr {{}}
impl EffectRefMut<{target}> for Ptr {{}}
{body}
"#
    )
}

#[test]
fn boundary_effect_access_preserves_all_address_spaces() {
    for space in ["Memory", "Storage", "TransientStorage", "Calldata", "Code"] {
        let source = boundary_provider_source(
            space,
            "u256",
            r#"
use core::Copy
impl Copy for Ptr {}
fn read() -> u256 uses (value: u256) { value }
fn write() uses (value: mut u256) { value = 1 }
fn transport(_ value: Ptr) -> Ptr { value }
fn allowed() -> Ptr {
    let ptr = Ptr { addr: 32 }
    let result = with (ptr) { read() }
    transport(ptr)
}

fn access() {
    let ptr = Ptr { addr: 32 }
    with (ptr) { write() }
}
"#,
        );
        let diags = checked_trusted_borrow_diags(&source);
        if matches!(space, "Calldata" | "Code") {
            assert!(
                diags.contains(&format!("cannot write to {}", space.to_lowercase())),
                "{space}: {diags}"
            );
            assert!(!diags.contains("transport violation"), "{space}: {diags}");
        } else {
            assert!(diags.is_empty(), "{space}: {diags}");
        }
    }
}

#[test]
fn raw_provider_access_may_alias_a_moved_local_representation() {
    let source = boundary_provider_source(
        "Memory",
        "u256",
        r#"
fn read() -> u256 uses (value: u256) { value }
fn transport(_ value: Ptr) -> Ptr { value }
fn conservative() -> Ptr {
    let ptr = Ptr { addr: 32 }
    let result = with (ptr) { read() }
    transport(ptr)
}
"#,
    );
    let diagnostics = checked_trusted_borrow_diags(&source);
    assert!(
        diagnostics.contains("move conflict in `fn conservative`"),
        "{diagnostics}"
    );
}

#[test]
fn boundary_storage_distinguishes_native_borrows_from_provider_values() {
    for space in ["Memory", "Storage", "TransientStorage", "Calldata", "Code"] {
        let source = boundary_provider_source(
            space,
            "Holder",
            r#"
struct Holder { value: Maybe }
enum Maybe { Empty, Full(ref u256) }
fn empty() {
    let ptr = Ptr { addr: 32 }
    with (ptr) { clear() }
}
fn clear() uses (holder: mut Holder) { holder.value = Maybe::Empty }
fn populated(_ handle: ref u256) {
    let ptr = Ptr { addr: 32 }
    with (ptr) { store(handle) }
}
fn store(_ handle: ref u256) uses (holder: mut Holder) { holder.value = Maybe::Full(handle) }
"#,
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_trusted_effect_handle_module("boundary_store.fe".into(), &source);
        let (top_mod, _) = db.top_mod(file);
        db.assert_no_diags(top_mod);
        for name in ["empty", "populated"] {
            let func = top_mod
                .all_funcs(&db)
                .iter()
                .copied()
                .find(|func| {
                    func.name(&db)
                        .to_opt()
                        .is_some_and(|ident| ident.data(&db) == name)
                })
                .expect("store fixture function");
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(func)),
            );
            let boundary = check_semantic_boundaries(&db, instance);
            match space {
                "Memory" => {
                    assert!(boundary.is_ok(), "{space} {name}: {boundary:?}");
                    let borrows = check_semantic_borrows(&db, instance);
                    if name == "empty" {
                        // Unknown bytes contain no invented live native loan.
                        assert!(borrows.is_ok(), "{space} {name}: {borrows:?}");
                    } else {
                        // Storing a real incoming loan retains the ordinary
                        // alias check against the unknown destination.
                        let conflict =
                            borrows.expect_err("opaque memory aliasing remains conservative");
                        assert!(
                            conflict.to_string().contains("borrow conflict"),
                            "{conflict}"
                        );
                    }
                }
                "Storage" | "TransientStorage" if name == "empty" => {
                    assert!(boundary.is_ok(), "{space} {name}: {boundary:?}")
                }
                "Storage" | "TransientStorage" => {
                    let destination = if space == "Storage" {
                        "storage"
                    } else {
                        "transient storage"
                    };
                    assert!(
                        format!("{boundary:?}")
                            .contains(&format!("cannot store `Maybe` in {destination}")),
                        "{space} {name}: {boundary:?}"
                    );
                }
                _ => assert!(
                    format!("{boundary:?}").contains("cannot write to"),
                    "{space} {name}: {boundary:?}"
                ),
            }
        }
    }
}

#[test]
fn invalid_record_assignment_is_blocked_before_semantic_lowering() {
    let source = boundary_provider_source(
        "Memory",
        "u256",
        r#"
pub contract InvalidHandle {
    mut slot: Ptr
    init() uses (mut slot) { slot = Ptr { addr: 32 } }
}
"#,
    );
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_trusted_effect_handle_module("invalid_record_assignment.fe".into(), &source);
    let (top_mod, _) = db.top_mod(file);
    let instance = contract_init_instance(&db, top_mod, "InvalidHandle");
    assert!(matches!(
        semantic_body_admission(&db, instance),
        SemanticBodyAdmission::Blocked(_)
    ));
    assert!(matches!(
        normalize_semantic_body(&db, instance),
        Err(SemanticNormalizationFailure::Blocked(_))
    ));
    assert!(matches!(
        semantic_borrow_summary(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::Blocked(_))
    ));
    assert!(matches!(
        canonicalize_semantic_consts(&db, instance),
        Err(CtfeError::InvalidBody { .. })
    ));
    let mut passes = initialize_analysis_pass();
    let diags = format_diagnostics(&db, &passes.run_on_module(&db, top_mod));
    assert!(diags.contains("u256") && diags.contains("Ptr"), "{diags}");
}

#[test]
fn boundary_nominal_handle_representation_can_be_stored() {
    for space in ["Memory", "Storage", "TransientStorage", "Calldata", "Code"] {
        let source = boundary_provider_source(
            space,
            "u256",
            r#"
struct Holder { handle: Ptr }
pub contract StoreHandle {
    mut slot: Holder
    init() uses (mut slot) { slot = Holder { handle: Ptr { addr: 32 } } }
}
"#,
        );
        let diags = checked_trusted_borrow_diags(&source);
        assert!(diags.is_empty(), "nominal {space}: {diags}");
    }
}

#[test]
fn borrow_conflicts_and_boundary_policy_are_independent() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "boundaries.fe".into(),
        r#"
fn pair(left: mut u256, right: mut u256) {
    left = 1
    right = 2
}
fn conflict(value: mut u256) { pair(left: value, right: value) }
fn escape() -> mut u256 {
    let mut value: u256 = 0
    mut value
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    for (name, borrow_ok, boundary_ok) in [("conflict", false, true), ("escape", true, false)] {
        let func = top_mod
            .all_funcs(&db)
            .iter()
            .find(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(&db) == name)
            })
            .expect("test function");
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
        );
        let borrows = check_semantic_borrows(&db, instance);
        let boundaries = check_semantic_boundaries(&db, instance);
        assert_eq!(borrows.is_ok(), borrow_ok, "{name}: {borrows:?}");
        assert_eq!(boundaries.is_ok(), boundary_ok, "{name}: {boundaries:?}");
    }
}

#[test]
fn boundary_native_transport_preserves_provider_contracts() {
    for space in ["Memory", "Storage", "TransientStorage", "Calldata", "Code"] {
        let source = boundary_provider_source(
            space,
            "u256",
            r#"
struct Wrap { handle: mut u256 }
fn consume(_ value: own Wrap) {}
fn forward() uses (value: mut u256) { consume(Wrap { handle: mut value }) }
fn caller() {
    let ptr = Ptr { addr: 32 }
    with (ptr) { forward() }
}
"#,
        );
        let diags = checked_trusted_borrow_diags(&source);
        if space == "Memory" {
            assert!(diags.is_empty(), "{space}: {diags}");
        } else {
            assert!(
                diags.contains("transport violation in `fn forward`"),
                "{space}: {diags}"
            );
        }
        let source = boundary_provider_source(
            space,
            "u256",
            r#"
struct Shared { handle: ref u256 }
fn consume(_ value: own Shared) {}
fn forward() uses (value: u256) { consume(Shared { handle: ref value }) }
fn caller() {
    let ptr = Ptr { addr: 32 }
    with (ptr) { forward() }
}
"#,
        );
        let diags = checked_trusted_borrow_diags(&source);
        assert!(diags.is_empty(), "shared {space}: {diags}");
    }
}

#[test]
fn boundary_return_permission_is_distinct_from_handle_transport() {
    for space in ["Memory", "Storage", "TransientStorage", "Calldata", "Code"] {
        let source = boundary_provider_source(
            space,
            "u256",
            r#"
struct Returned { handle: ref u256 }
fn borrow_provider() -> Returned uses (value: u256) { Returned { handle: ref value } }
fn caller() -> Returned {
    let ptr = Ptr { addr: 32 }
    with (ptr) { borrow_provider() }
}
"#,
        );
        let diags = checked_trusted_borrow_diags(&source);
        assert!(
            diags.contains("cannot return a borrow derived from an effect parameter"),
            "{space}: {diags}"
        );
        assert!(!diags.contains("transport violation"), "{space}: {diags}");
    }
}

#[test]
fn boundary_receiver_forwarding_preserves_transport_requirements() {
    let diags = checked_borrow_diags(
        r#"
struct Cell { value: u256 }
fn ordinary(_ value: mut Cell) { value.value = 1 }
impl Cell {
    fn write(mut self) { self.value = 1 }
    fn forward(mut self) { ordinary(self) }
}
pub contract Direct {
    mut slot: Cell
    init() uses (mut slot) { slot.write() }
}
pub contract Forwarded {
    mut slot: Cell
    init() uses (mut slot) { slot.forward() }
}
"#,
    );
    assert!(
        !diags.contains("transport violation in `fn Direct::__init__`"),
        "{diags}"
    );
    assert!(
        diags.contains("transport violation"),
        "storage receiver must not erase an ordinary parameter's memory contract: {diags}"
    );
}

#[test]
fn boundary_requirements_follow_receiver_chains_and_nested_inputs() {
    for space in ["Memory", "Storage", "TransientStorage"] {
        for call in [
            "value.chain()",
            "value.recursive(false)",
            "value.mutual_a(false)",
            "let mut wrapper = Wrapper { cell: mut value }
wrapper.forward()",
            "let mut wrapper = ArrayWrapper { cells: [mut value] }
wrapper.forward(0)",
        ] {
            let body = format!(
                r#"
struct Cell {{ value: u256 }}
fn ordinary(_ value: mut Cell) {{ value.value = 1 }}
impl Cell {{
    fn forward(mut self) {{ ordinary(self) }}
    fn chain(mut self) {{ self.forward() }}
    fn recursive(mut self, _ again: bool) {{
        if again {{ self.recursive(false) }} else {{ ordinary(self) }}
    }}
    fn mutual_a(mut self, _ again: bool) {{
        if again {{ self.mutual_b(false) }} else {{ ordinary(self) }}
    }}
    fn mutual_b(mut self, _ again: bool) {{ self.mutual_a(again) }}
}}
struct Wrapper {{ cell: mut Cell }}
impl Wrapper {{ fn forward(mut self) {{ ordinary(self.cell) }} }}
struct ArrayWrapper {{ cells: [mut Cell; 1] }}
impl ArrayWrapper {{ fn forward(mut self, _ index: usize) {{ ordinary(self.cells[index]) }} }}
fn forward() uses (value: mut Cell) {{ {call} }}
fn caller() {{ let ptr = Ptr {{ addr: 32 }}
with (ptr) {{ forward() }} }}
"#
            );
            let source = boundary_provider_source(space, "Cell", &body);
            let diags = checked_trusted_borrow_diags(&source);
            if space == "Memory" {
                assert!(diags.is_empty(), "{space} {call}: {diags}");
            } else {
                assert!(
                    diags.contains("transport violation") && !diags.contains("internal borrow"),
                    "{space} {call}: {diags}"
                );
            }
        }
    }
}

#[test]
fn boundary_forwarded_storage_requirements_preserve_empty_variants() {
    for (value, erase) in [
        ("Maybe::Empty", ""),
        ("Maybe::Full(ref local)", ""),
        ("Maybe::Empty", "self.value = Maybe::Empty"),
        ("Maybe::Full(ref local)", "self.value = Maybe::Empty"),
    ] {
        let source = format!(
            r#"
enum Maybe {{ Empty, Full(ref u256) }}
struct Holder {{ value: Maybe }}
impl Holder {{
    fn store(mut self, _ value: own Maybe) {{
self.value = value
{erase}
}}
    fn forward(mut self, _ value: own Maybe) {{ self.store(value) }}
}}
pub contract Store {{
    mut slot: Holder
    init() uses (mut slot) {{
        let local: u256 = 0
        slot.forward({value})
    }}
}}
"#
        );
        let diags = checked_trusted_borrow_diags(&source);
        if value == "Maybe::Empty" {
            assert!(diags.is_empty(), "{diags}");
        } else {
            assert!(
                diags.contains("cannot store") && !diags.contains("internal borrow"),
                "{diags}"
            );
        }
    }
}

#[test]
fn pointer_accesses_discharge_native_input_separation_at_calls() {
    let diagnostics = checked_borrow_diags(
        r#"
struct Holder { ptr: *u256 }
impl Holder {
    fn write(mut self) { *self.ptr = 1 }
}
fn write_both(_ target: mut u256, pointer: *u256) {
    *pointer = 1
    target = 2
}
fn bad() {
    let pointer = core::ptr::alloc<u256>()
    write_both(mut *pointer, pointer)
}
fn retained() {
    let pointer = core::ptr::alloc<u256>()
    let borrowed = mut *pointer
    let mut holder = Holder { ptr: pointer }
    holder.write()
    borrowed = 2
}
fn disjoint() {
    let left = core::ptr::alloc<u256>()
    let right = core::ptr::alloc<u256>()
    write_both(mut *left, pointer: right)
}
"#,
    );
    assert!(
        !diagnostics.contains("borrow conflict in `fn write`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("borrow conflict in `fn write_both`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("borrow conflict in `fn disjoint`"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn bad`"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("borrow conflict in `fn retained`"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("internal borrow checking error"),
        "{diagnostics}"
    );
}

#[test]
fn raw_memory_copy_invalidates_overwritten_pointer_provenance() {
    assert_borrow_conflict(
        r#"
fn bad() {
    let first = core::ptr::alloc<u256>()
    let second = core::ptr::alloc<u256>()
    let source = core::ptr::alloc<*u256>()
    let target = core::ptr::alloc<*u256>()
    *source = second
    *target = first
    let borrowed = mut *second
    core::ptr::copy_raw(core::ptr::byte_ptr(target), source: core::ptr::byte_ptr(source), len: 32)
    let alias = mut *(*target)
    borrowed = 1
    alias = 2
}
"#,
    );
}

#[test]
fn invalid_cast_is_blocked_before_semantic_lowering() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "invalid_cast.fe".into(),
        "fn invalid(_ value: mut u256) -> mut u256 { value as mut u256 }",
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "invalid"))),
    );
    assert!(matches!(
        semantic_body_admission(&db, instance),
        SemanticBodyAdmission::Blocked(_)
    ));
    assert!(matches!(
        normalize_semantic_body(&db, instance),
        Err(SemanticNormalizationFailure::Blocked(_))
    ));
    assert!(matches!(
        semantic_borrow_summary(&db, instance),
        Err(SemanticAnalysisError::Blocked(_))
    ));
    assert!(matches!(
        layout_evidence_body(&db, instance),
        Err(LayoutEvidenceError::Blocked(_))
    ));
    assert!(matches!(
        canonicalize_semantic_consts(&db, instance),
        Err(CtfeError::InvalidBody { .. })
    ));
    let diags = format_diagnostics(&db, &db.run_on_top_mod(top_mod));
    assert!(diags.contains("cast is not provably lossless"), "{diags}");
    assert!(!diags.contains("internal"), "{diags}");
}

fn literal_birth_loop_source(allocation: &str) -> String {
    format!(
        r#"
use core::ptr
struct Item {{ n: u256 }}
fn consume(_ value: own Item) {{}}
fn wrapped() -> Text {{ "hello" }}
fn check(_ count: u256) {{
    let mut i: u256 = 0
    while i < count {{
        {allocation}
        let p = ptr::cast<u8, Item>(text.encoded_span().ptr())
        consume(*p)
        i += 1
    }}
}}
"#
    )
}

#[test]
fn literal_birth_direct_and_wrapped_loops_are_available() {
    for allocation in ["let text: Text = \"hello\"", "let text = wrapped()"] {
        let source = literal_birth_loop_source(allocation);
        let diagnostics = checked_borrow_diags(&source);
        assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
    }
}

#[test]
fn literal_birth_preserves_duplicate_and_retained_old_moves() {
    for (setup, read) in [
        ("", "consume(*p)"),
        (
            "let mut old = ptr::cast<u8, Item>(wrapped().encoded_span().ptr())",
            "if i > 0 { consume(*old) }\nold = p",
        ),
        (
            "let mut old = [ptr::cast<u8, Item>(wrapped().encoded_span().ptr())]",
            "if i > 0 { consume(*old[0]) }\nold[0] = p",
        ),
    ] {
        let source = literal_birth_loop_source("let text: Text = \"hello\"")
            .replace(
                "let mut i: u256 = 0",
                &format!("{setup}\nlet mut i: u256 = 0"),
            )
            .replace("consume(*p)", &format!("{read}\nconsume(*p)"));
        let diagnostics = checked_borrow_diags(&source);
        assert!(
            diagnostics.contains("move conflict"),
            "{source}\n{diagnostics}"
        );
        assert!(!diagnostics.contains("internal"), "{diagnostics}");
    }
}

#[test]
fn literal_birth_distinguishes_evaluations_but_preserves_copied_aliases() {
    for (allocation, uses, valid) in [
        (
            "let text: Text = \"hello\"\nlet other: Text = \"hello\"",
            "consume(*ptr::cast<u8, Item>(other.encoded_span().ptr()))",
            true,
        ),
        (
            "let text: Text = \"hello\"",
            "let alias = p\nconsume(*alias)",
            false,
        ),
        (
            "let text: Text = \"hello\"",
            "let aliases = [p; 2]\nconsume(*aliases[1])",
            false,
        ),
    ] {
        let source = literal_birth_loop_source(allocation)
            .replace("consume(*p)", &format!("{uses}\nconsume(*p)"));
        let diagnostics = checked_borrow_diags(&source);
        if valid {
            assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
        } else {
            assert!(
                diagnostics.contains("move conflict"),
                "{source}\n{diagnostics}"
            );
        }
    }
}

#[test]
fn literal_birth_native_views_require_each_iterations_typed_initialization() {
    for native in ["ref", "mut"] {
        for (store, valid) in [
            ("", false),
            ("*slot = BORROW *owner", true),
            ("if i == 0 { *slot = BORROW *owner }", false),
        ] {
            let source = format!(
                r#"
use core::ptr
fn check(_ count: u256) {{
    let owner = ptr::alloc<u256>()
    *owner = 7
    let mut i: u256 = 0
    while i < count {{
        let text: Text = "hello"
        let slot = ptr::cast<u8, {native} u256>(text.encoded_span().ptr())
        {store}
        let loaded: u256 = *slot
        i += 1
    }}
}}
"#
            )
            .replace("BORROW", native);
            let diagnostics = checked_borrow_diags(&source);
            if valid {
                assert!(diagnostics.is_empty(), "{source}\n{diagnostics}");
            } else {
                assert!(
                    diagnostics.contains("cannot use a native borrow"),
                    "{source}\n{diagnostics}"
                );
            }
            assert!(!diagnostics.contains("internal"), "{diagnostics}");
        }
    }
}

#[test]
fn literal_birth_nested_loops_and_late_views_are_query_order_independent() {
    let source = literal_birth_loop_source(
        r#"
let mut j: u256 = 0
while j < count {
    let inner: Text = "inner"
    consume(*typed(inner.encoded_span().ptr()))
    j += 1
}
let text: Text = "outer"
"#,
    )
    .replace(
        "let p = ptr::cast<u8, Item>(text.encoded_span().ptr())",
        "let p = typed(text.encoded_span().ptr())",
    ) + r#"
fn typed(_ p: *u8) -> *Item { ptr::cast<u8, Item>(p) }
fn zero() { check(0) }
fn multiple() { check(3) }
"#;
    for first in ["typed", "check", "wrapped", "multiple"] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("literal_query_order.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        semantic_borrow_summary(&db, func_instance(&db, module, first)).unwrap();
        for _ in 0..2 {
            let diagnostics = format_diagnostics(
                &db,
                &collect_semantic_borrow_diagnostic_vouchers(&db, module),
            );
            assert!(diagnostics.is_empty(), "{first}: {diagnostics}");
        }
    }
}

#[test]
fn dynamic_string_literals_export_distinct_allocations() {
    with_borrow_summary(
        r#"
fn literals() -> (Text, Text) {
    let first: Text = "first"
    let second: Text = "second"
    (first, second)
}

"#,
        "literals",
        |db, summary| {
            let values = ValueInterner::new(db, ValueLimits::default());
            let leaves = values.leaves(&summary.result, ValueOccurrence::Summary);
            assert_eq!(leaves.len(), 2, "{summary:#?}");
            assert!(
                leaves.iter().all(|leaf| matches!(
                    leaf.payload.source.origin,
                    ExternalOrigin::Allocation(_)
                ))
            );
            assert_ne!(leaves[0].payload.source, leaves[1].payload.source);
        },
    );
}

#[test]
fn summarized_partially_initialized_heap_cells_keep_unknown_contents() {
    assert_borrow_conflict(
        r#"
use core::ptr
fn staged(_ cursor: mut u256, _ count: u256, _ initialize: bool) -> u256 {
    let mut children = ptr::MemArray<ptr::MemSpan>::new_uninit(count)
    let mut i: u256 = 0
    while i < count {
        if initialize {
            let data = ptr::MemBuffer::alloc(32)
            *ptr::cast<u8, u256>(data.ptr()) = 7
            children[i as usize] = data.span()
        }
        i += 1
    }
    if count == 0 { return 0 }
    let child = children[0]
    cursor += 1
    *ptr::cast<u8, u256>(child.ptr())
}
fn run(_ count: u256, _ initialize: bool) -> u256 {
    let mut cursor: u256 = 0
    staged(mut cursor, count, initialize)
}
"#,
    );
}

#[test]
fn repeated_trait_provider_calls_borrow_the_same_noncopy_binding() {
    for binding in [
        "Tick = counter()",
        "Tick = Counter { value: 0 }",
        "Tick = { counter() }",
        "Tick = { local }",
        "Tick = local",
        "local",
    ] {
        let source = format!(
            r#"
trait Tick {{ fn next(mut self) -> i32 }}
struct Counter {{ value: i32 }}
impl Tick for Counter {{
    fn next(mut self) -> i32 {{
        self.value += 1
        self.value
    }}
}}
fn counter() -> Counter {{ Counter {{ value: 0 }} }}
fn next() -> i32 uses (tick: mut Tick) {{ tick.next() }}
fn run() -> i32 {{
    let mut local = counter()
    with ({binding}) {{ next() + next() }}
}}
"#
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(diagnostics.is_empty(), "{binding}: {diagnostics}");
    }
}

#[test]
fn trait_provider_calls_preserve_real_moves_and_alias_conflicts() {
    let prefix = r#"
trait Tick { fn next(mut self) -> i32 }
struct Counter { value: i32 }
impl Tick for Counter {
    fn next(mut self) -> i32 {
        self.value += 1
        self.value
    }
}
fn next() -> i32 uses (tick: mut Tick) { tick.next() }
fn consume(_ value: own Counter) {}
fn pass(_ value: own Counter) -> Counter { value }
"#;
    for (body, message) in [
        ("next()\n consume(value)\n next()", "move conflict"),
        (
            "let borrowed = mut value\n next()\n borrowed.value = 9",
            "borrow conflict",
        ),
    ] {
        let source = format!(
            "{prefix}\nfn bad() {{\n let mut value = Counter {{ value: 0 }}\n with (Tick = value) {{ {body} }}\n}}"
        );
        let diagnostics = checked_borrow_diags(&source);
        assert!(diagnostics.contains(message), "{message}: {diagnostics}");
    }
    let source = format!(
        "{prefix}\nfn bad() {{\n let mut value = Counter {{ value: 0 }}\n with (Tick = pass(value)) {{ next() }}\n consume(value)\n}}"
    );
    let diagnostics = checked_borrow_diags(&source);
    assert!(diagnostics.contains("move conflict"), "{diagnostics}");
}

#[test]
fn trait_provider_borrows_cannot_escape() {
    let diagnostics = checked_borrow_diags(
        r#"
trait View { fn view(ref self) -> ref i32 }
struct Counter { value: i32 }
impl View for Counter {
    fn view(ref self) -> ref i32 { ref self.value }
}
fn bad() -> ref i32 uses (value: View) { value.view() }
fn root() {
    with (View = Counter { value: 0 }) { bad() }
}
"#,
    );
    assert!(
        diagnostics.contains("cannot return a borrow derived from an effect parameter"),
        "{diagnostics}"
    );
}

#[test]
fn normalized_effect_places_have_coherent_transport() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
trait Tick { fn next(mut self) -> i32 }
struct Counter { value: i32 }
impl Tick for Counter {
    fn next(mut self) -> i32 {
        self.value += 1
        self.value
    }
}
fn next() -> i32 uses (tick: mut Tick) { tick.next() }
fn run() -> i32 {
    with (Tick = Counter { value: 0 }) { next() + next() }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let instance = func_instance(&db, top_mod, "run");
    let mut normalized = normalize_semantic_body(&db, instance)
        .expect("normalized body")
        .body;
    let arguments = normalized
        .blocks
        .iter_mut()
        .flat_map(|block| &mut block.statements)
        .filter_map(|stmt| match &mut stmt.kind {
            NStatementKind::Define {
                expr: NExpr::Call { effect_args, .. },
                ..
            } => Some(effect_args),
            _ => None,
        })
        .flatten()
        .collect::<Vec<_>>();
    assert_eq!(arguments.len(), 2);
    let NEffectArgValue::Place(first) = arguments[0].arg.clone() else {
        panic!("provider must be a place");
    };
    for arg in arguments {
        let NEffectArgValue::Place(place) = &arg.arg else {
            panic!("provider must be a place");
        };
        assert_eq!(place.base, first.base);
        assert_eq!(place.path, first.path);
        assert_eq!(arg.pass_mode, EffectPassMode::ByPlace);
        assert!(arg.required_mut);
        arg.pass_mode = EffectPassMode::ByValue;
    }
    assert_eq!(
        verify_normalized_body(&db, &normalized),
        Err(NormalizedBodyVerifyError::ExpressionType),
    );
}

#[test]
fn host_import_contracts_preserve_live_native_borrows() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "host_io_borrows.fe".into(),
        r#"
use std::io::{Read, Write, host, read_char, write_char}
fn run() -> i32 {
    let mut value: i32 = 0
    let borrowed = mut value
    with (Read = host(), Write = host()) {
        let start = std::native::cpu_clock_ticks()
        write_char(read_char())
        borrowed = if std::native::cpu_clock_ticks() >= start { 42 } else { 0 }
    }
    value
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let instance = func_instance(&db, module, "run");
    check_semantic_borrows(&db, instance).unwrap();
    check_semantic_boundaries(&db, instance).unwrap();
    semantic_borrow_summary(&db, instance).unwrap().unwrap();
}

#[test]
fn user_scalar_externs_still_require_effect_contracts() {
    for name in ["getchar", "putchar", "abs", "unknown"] {
        let diagnostics = checked_borrow_diags(&format!(
            "extern {{ fn {name}(value: i32) -> i32 }}\nfn run() -> i32 {{ {name}(value: 42) }}"
        ));
        assert!(
            diagnostics.contains(
                "executable calls require concrete implementations or verified effect contracts"
            ),
            "{name}: {diagnostics}"
        );
    }
}

#[test]
fn user_clock_extern_still_requires_effect_contract() {
    let diagnostics =
        checked_borrow_diags("extern { fn clock() -> i64 }\nfn run() -> i64 { clock() }");
    assert!(
        diagnostics.contains(
            "executable calls require concrete implementations or verified effect contracts"
        ),
        "{diagnostics}"
    );
}

#[test]
fn native_byte_buffer_contracts_preserve_unrelated_live_borrows() {
    let diagnostics = checked_borrow_diags(
        r#"
use std::native::ByteBuffer
fn run() -> u8 {
    let mut value: u8 = 0
    let borrowed = mut value
    let mut buffer = ByteBuffer::new()
    if buffer.try_resize(64) {
        buffer.set_byte(index: 0, value: 42)
        buffer.copy_within(dest: 1, source: 0, len: 1)
        borrowed = buffer.byte_at(1)
        buffer.clear()
    }
    buffer.release()
    borrowed += 1
    value
}
"#,
    );
    assert!(diagnostics.is_empty(), "{diagnostics}");
}

#[test]
fn native_byte_buffer_owner_cannot_be_reused_or_released_while_borrowed() {
    for (body, expected) in [
        ("buffer.release()\nbuffer.release()", "move conflict"),
        ("buffer.release()\nlet size = buffer.len()", "move conflict"),
        (
            "let view = ref buffer\nbuffer.release()\nlet size = view.len()",
            "borrow conflict",
        ),
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            "use std::native::ByteBuffer\nfn run() {{ let buffer = ByteBuffer::new()\n{body} }}"
        ));
        assert!(diagnostics.contains(expected), "{body}: {diagnostics}");
    }
}

#[test]
fn user_allocator_externs_still_require_effect_contracts() {
    for (declaration, body) in [
        (
            "fn malloc(size: u64) -> *u8",
            "let pointer = malloc(size: 64)",
        ),
        ("fn free(_ address: *u8)", "free(pointer)"),
        (
            "fn is_null(_ address: *u8) -> bool",
            "let empty = is_null(pointer)",
        ),
    ] {
        let diagnostics = checked_borrow_diags(&format!(
            "extern {{ {declaration} }}\nfn run(pointer: *u8) {{ {body} }}"
        ));
        assert!(
            diagnostics.contains(
                "executable calls require concrete implementations or verified effect contracts"
            ),
            "{declaration}: {diagnostics}"
        );
    }
}
