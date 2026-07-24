use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use fe_hir::test_db::{HirAnalysisTestDb, format_diagnostics};
use fe_hir::{
    analysis::{
        semantic::{
            BorrowInputRef, BorrowTransform, MemoryAccessKind, MemorySummaryItem,
            MemorySummaryTarget, NBorrowRoot, NExpr, NLocalOrigin, NSPlaceRoot, NSProjectionPath,
            NSStmtKind, NormalizedBindingLowering, ReadMode, SStmtKind, SemanticBorrowDiagKind,
            SemanticInstance, SemanticLocalKind, check_semantic_borrows, check_semantic_noesc,
            collect_semantic_borrow_diagnostic_vouchers, get_or_build_semantic_instance,
            identity_semantic_instance_key, normalize_semantic_body, semantic_borrow_summary,
            semantic_memory_summary, verify_normalized_semantic_body,
        },
        ty::{
            ProviderAddressSpace,
            ty_check::{BodyOwner, LocalBinding},
            ty_def::{BorrowKind, TyData},
        },
    },
    hir_def::{ItemKind, Partial},
    projection::{IndexSource, Projection, ProjectionPath},
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
) -> fe_hir::analysis::semantic::NormalizedSemanticBody<'db> {
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
    let param = normalized
        .locals
        .iter()
        .find(|local| matches!(local.source, Some(LocalBinding::Param { idx: 0, .. })))
        .expect("missing value parameter");
    let root = param
        .lowering
        .root()
        .expect("mutable owned aggregate parameter must have a root");

    assert!(!param.layout_backing_sources().is_empty());
    assert!(
        param
            .layout_backing_sources()
            .iter()
            .all(|source| source.source.root.borrow_root() == Some(root))
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
    f: impl for<'db> FnOnce(Vec<BorrowTransform<'db>>),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let instance = func_instance(&db, top_mod, func_name);
    let summary = semantic_borrow_summary(&db, instance)
        .expect("borrow summary")
        .expect("borrow-returning function should produce a summary");
    f(summary);
}

fn with_pointer_summary(
    src: &str,
    func_name: &str,
    f: impl for<'db> FnOnce(Vec<(NSProjectionPath<'db>, Vec<MemorySummaryTarget<'db>>)>),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let instance = func_instance(&db, top_mod, func_name);
    let summary = semantic_memory_summary(&db, instance).expect("memory summary");
    f(summary
        .items
        .into_iter()
        .filter_map(|item| match item {
            MemorySummaryItem::ReturnPointer { output, targets } => Some((output, targets)),
            MemorySummaryItem::Access { .. } | MemorySummaryItem::StorePointer { .. } => None,
        })
        .collect());
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
    let summary = semantic_memory_summary(&db, instance).expect("memory summary");
    let input = |idx| MemorySummaryTarget::Input {
        input: BorrowInputRef::Param(idx),
        proj: ProjectionPath::from_projection(Projection::Deref),
    };
    let slot = input(0);
    let value = input(1);

    assert!(summary.may_return);
    assert_eq!(
        summary.items,
        vec![
            MemorySummaryItem::ReturnPointer {
                output: ProjectionPath::default(),
                targets: vec![value.clone()],
            },
            MemorySummaryItem::Access {
                target: slot.clone(),
                kind: MemoryAccessKind::Read,
                authorizers: vec![],
            },
            MemorySummaryItem::Access {
                target: slot.clone(),
                kind: MemoryAccessKind::Write,
                authorizers: vec![],
            },
            MemorySummaryItem::StorePointer {
                target: slot,
                targets: vec![value],
                weak: false,
            },
        ]
    );
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
    assert_eq!(summary.len(), 2, "unexpected summary: {summary:#?}");
    assert!(summary.iter().any(|transform| {
        matches!(transform.input, BorrowInputRef::Param(0))
            && transform.proj.iter().cloned().collect::<Vec<_>>() == vec![Projection::Field(2)]
    }));
    assert!(summary.iter().any(|transform| {
        matches!(transform.input, BorrowInputRef::Param(0))
            && transform.proj.iter().cloned().collect::<Vec<_>>() == vec![Projection::Field(0)]
    }));
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
        |summary| {
            assert_eq!(
                summary,
                vec![BorrowTransform {
                    input: BorrowInputRef::Param(1),
                    proj: ProjectionPath::from_projection(Projection::Deref),
                }]
            );
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
    let output = normalized
        .locals
        .iter()
        .find(|local| matches!(local.source, Some(LocalBinding::Local { is_mut: true, .. })))
        .expect("missing mutable output local");
    let NormalizedBindingLowering::ValueLocal { place } = &output.lowering else {
        panic!("output must be a direct value: {output:#?}");
    };
    let root = place.root.borrow_root().expect("output root");
    assert!(
        matches!(normalized.root(root), Some(NBorrowRoot::LocalSlot { .. })),
        "moved destination must own a fresh container root: {output:#?}"
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
        |summary| {
            let mut expected = ProjectionPath::from_projection(Projection::Field(0));
            expected.push(Projection::Deref);
            assert_eq!(
                summary,
                vec![BorrowTransform {
                    input: BorrowInputRef::Param(1),
                    proj: expected,
                }]
            );
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
fn mem_array_from_raw_parts_preserves_pointer_provenance() {
    let diags = borrow_diags(
        r#"
fn bad(p: *u256, len: u256) {
    let a = core::ptr::MemArray<u256>::from_raw_parts(ptr: p, len: len)
    let x = mut *p
    a[0] = 1
    x = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
    assert!(diags.contains("cannot mutably borrow"), "{diags:?}");
}

#[test]
fn generic_into_mem_buffer_preserves_fresh_pointer_provenance() {
    assert_no_borrow_conflict(
        r#"
fn read_buffer<B>(buffer: own B) -> u8
    where B: core::convert::Into<core::ptr::MemBuffer>
{
    let buffer = buffer.into()
    *buffer.ptr
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
    *buffer.ptr
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
    with_pointer_summary(
        r#"
fn encoded(args: own (u256, u256)) -> (*u8, u256) {
    core::abi::encode_alloc<std::abi::Sol, (u256, u256)>(args)
}
"#,
        "encoded",
        |summary| {
            assert!(
                matches!(
                    summary[0].1.as_slice(),
                    [MemorySummaryTarget::FreshAllocation { .. }]
                ),
                "{summary:#?}"
            );
        },
    );
}

#[test]
fn writing_fresh_allocation_preserves_pointer_value_provenance() {
    with_pointer_summary(
        r#"
fn fresh() -> *u8 {
    let ptr = core::ptr::alloc_bytes(32)
    *ptr = 1
    ptr
}
"#,
        "fresh",
        |summary| {
            assert!(
                matches!(
                    summary[0].1.as_slice(),
                    [MemorySummaryTarget::FreshAllocation { .. }]
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
    mut *b.ptr
}
"#,
        "second_array",
        |summary| {
            let mut expected = ProjectionPath::from_projection(Projection::Field(0));
            expected.push(Projection::Deref);
            assert_eq!(
                summary,
                vec![BorrowTransform {
                    input: BorrowInputRef::Param(1),
                    proj: expected,
                }]
            );
        },
    );
}

#[test]
fn pointer_array_returned_borrow_uses_wildcard_index() {
    with_borrow_summary(
        r#"
fn elem(array: *[u256; 64], i: usize) -> mut u256 {
    mut (*array)[i]
}
"#,
        "elem",
        |summary| {
            let mut expected = ProjectionPath::from_projection(Projection::Deref);
            expected.push(Projection::Index(IndexSource::Any));
            assert_eq!(
                summary,
                vec![BorrowTransform {
                    input: BorrowInputRef::Param(0),
                    proj: expected,
                }]
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
fn bodyless_pointer_call_does_not_conflict_with_unrelated_borrow() {
    assert_no_borrow_conflict(
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
    let mut normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let mut replaced = false;
    'blocks: for block in &mut normalized.blocks {
        for stmt in &mut block.stmts {
            let NSStmtKind::Store { dst, .. } = &mut stmt.kind else {
                continue;
            };
            let mut path = ProjectionPath::new();
            for projection in dst.path.iter() {
                if !replaced && matches!(projection, Projection::Index(_)) {
                    path.push(Projection::Index(IndexSource::Any));
                    replaced = true;
                } else {
                    path.push(projection.clone());
                }
            }
            dst.path = path;
            if replaced {
                break 'blocks;
            }
        }
    }
    assert!(replaced, "fixture did not contain an indexed store");
    let err = verify_normalized_semantic_body(&db, instance, &normalized)
        .expect_err("verifier should reject wildcard runtime indices");
    assert!(
        format!("{err:#?}").contains("analysis-only wildcard index"),
        "{err:#?}"
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
        |summary| {
            assert_eq!(
                summary,
                vec![BorrowTransform {
                    input: BorrowInputRef::Param(0),
                    proj: ProjectionPath::from_projection(Projection::Deref),
                }]
            );
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
        |summary| {
            assert_eq!(
                summary,
                vec![BorrowTransform {
                    input: BorrowInputRef::Param(0),
                    proj: ProjectionPath::from_projection(Projection::Deref),
                }]
            );
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
    assert_mut_borrow_conflict(
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
    let x = mut *r.ptr
    let y = mut *a.ptr
    y = 1
    x = 2
}
"#,
    );
    assert_mut_borrow_conflict(
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
    let x = mut *r.ptr
    let y = mut *b.ptr
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
    assert_mut_borrow_conflict(
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
    with_pointer_summary(
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
        |summary| {
            assert_eq!(summary.len(), 1, "unexpected summary: {summary:#?}");
            let item = &summary[0];
            assert!(item.0.is_empty(), "unexpected output: {:?}", item.0);
            assert_eq!(
                item.1,
                vec![
                    MemorySummaryTarget::Input {
                        input: BorrowInputRef::Param(1),
                        proj: ProjectionPath::from_projection(Projection::Deref),
                    },
                    MemorySummaryTarget::Input {
                        input: BorrowInputRef::Param(2),
                        proj: ProjectionPath::from_projection(Projection::Deref),
                    },
                ]
            );
        },
    );
}

#[test]
fn local_pointer_array_summary_keeps_constant_slot_precision() {
    with_pointer_summary(
        r#"
fn pick(_ p: *u256, q: *u256) -> *u256 {
    let arr = [q, q]
    arr[0]
}
"#,
        "pick",
        |summary| {
            assert_eq!(summary.len(), 1, "unexpected summary: {summary:#?}");
            assert_eq!(
                summary[0].1,
                vec![MemorySummaryTarget::Input {
                    input: BorrowInputRef::Param(1),
                    proj: ProjectionPath::from_projection(Projection::Deref),
                }]
            );
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
        |summary| {
            assert_eq!(
                summary,
                vec![BorrowTransform {
                    input: BorrowInputRef::Param(1),
                    proj: ProjectionPath::from_projection(Projection::Deref),
                }]
            );
        },
    );
}

#[test]
fn pointer_to_array_of_pointers_summary_preserves_wildcard_pointee() {
    with_pointer_summary(
        r#"
fn pick(pp: *[*u256; 64], i: usize) -> *u256 {
    (*pp)[i]
}
"#,
        "pick",
        |summary| {
            assert_eq!(summary.len(), 1, "unexpected summary: {summary:#?}");
            let mut expected = ProjectionPath::from_projection(Projection::Deref);
            expected.push(Projection::Index(IndexSource::Any));
            expected.push(Projection::Deref);
            assert_eq!(
                summary[0].1,
                vec![MemorySummaryTarget::Input {
                    input: BorrowInputRef::Param(0),
                    proj: expected,
                }]
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
    with_pointer_summary(
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
        |summary| {
            assert_eq!(summary.len(), 1, "unexpected summary: {summary:#?}");
            assert!(matches!(
                summary[0].0.iter().next(),
                Some(Projection::VariantField { variant, field_idx, .. })
                    if variant.0 == 0 && *field_idx == 0
            ));
            assert_eq!(
                summary[0].1,
                vec![MemorySummaryTarget::Input {
                    input: BorrowInputRef::Param(0),
                    proj: ProjectionPath::from_projection(Projection::Deref),
                }]
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
        diags.contains("cannot return a pointer derived from an effect parameter"),
        "{diags:?}"
    );
}

#[test]
fn pointer_bearing_capability_return_summary_tracks_exposed_pointer_value() {
    with_pointer_summary(
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
        |summary| {
            assert_eq!(summary.len(), 1, "unexpected summary: {summary:#?}");
            let mut expected = ProjectionPath::from_projection(Projection::Field(0));
            expected.push(Projection::Deref);
            assert_eq!(
                summary[0].1,
                vec![MemorySummaryTarget::Input {
                    input: BorrowInputRef::Param(0),
                    proj: expected,
                }]
            );
        },
    );
}

#[test]
fn mem_array_ptr_field_update_does_not_keep_stale_carrier_target() {
    assert_no_borrow_conflict(
        r#"
fn ok(q: *u256, len: u256) {
    let p = core::ptr::alloc<u256>()
    let mut a = core::ptr::MemArray<u256>::from_raw_parts(ptr: p, len: len)
    a.ptr = q
    let x = mut *a.ptr
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
    assert_eq!(
        summary,
        vec![BorrowTransform {
            input: BorrowInputRef::Param(1),
            proj: ProjectionPath::default(),
        }]
    );
    check_semantic_borrows(&db, instance).expect("borrowck should accept forwarded borrows");
}

#[test]
fn contract_field_mut_borrow_matrix_fixture_borrowchecks() {
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"),
        |db, instance| {
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
fn returned_storage_borrow_effect_args_are_finalized_in_normalized_body() {
    let mut saw_storage_add_effect = false;
    for_each_fixture_instance(
        include_str!("../../fe/tests/fixtures/fe_test/contract_field_mut_borrow_matrix.fe"),
        |db, instance| {
            let normalized = normalize_semantic_body(db, instance).expect("normalized body");
            for stmt in normalized
                .blocks
                .iter()
                .flat_map(|block| block.stmts.iter())
            {
                let NSStmtKind::Assign {
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
    assert_eq!(err.kind, SemanticBorrowDiagKind::ProviderProvenanceConflict);
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

    let err = check_semantic_noesc(&db, instance)
        .expect_err("mixed provider provenance must poison noesc");
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
    let diags = borrow_diags(
        r#"
struct Item {
    value: u256,
}

enum Choice {
    Item(Item),
}

fn bad(mut choice: Choice) {
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
    assert!(borrow_diags(source).is_empty());

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), source);
    let (top_mod, _) = db.top_mod(file);
    let normalized = normalized_func_body(&db, top_mod, "read_call_result");
    let place = normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
                expr: NExpr::ReadPlace { place, .. },
                ..
            } if matches!(place.path.iter().next(), Some(Projection::Field(0))) => Some(place),
            _ => None,
        })
        .expect("tag field read");
    let root = place
        .root
        .borrow_root()
        .expect("ordinary handle backing root");
    assert!(
        matches!(normalized.root(root), Some(NBorrowRoot::LocalSlot { .. })),
        "ordinary handle field should read its ADT backing local: {place:#?}"
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
        diags.contains("noesc violation in `fn NoEscCallArg::__init__`"),
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
    check_semantic_noesc(&db, identity).expect("generic identity noesc should be accepted");

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
    let err = check_semantic_noesc(&db, specialized)
        .expect_err("specialized noesc store should be rejected");
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
    let src = r#"
use core::ptr
use std::evm::RawMem

fn allocate(bytes: u256) -> *u8 uses (mem: mut RawMem) {
    let out = ptr::alloc_bytes(64)
    mem.mstore(out, bytes)
    mem.mstore(ptr::offset_bytes(out, 32), bytes)
    out
}
"#;
    let diags = borrow_diags(src);

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
    mem.mstore(addr: ptr::as_bytes(data), value: 8)
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
    let diags = borrow_diags(
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
        let _ value = *ptr
    }
}

fn bad<K>(key: K)
    where K: PointerSource
{
    let mut local = Cell { value: 0 }
    local.run(key)
}
"#,
    );

    assert!(diags.contains("borrow conflict"), "{diags:?}");
}

#[test]
fn two_phase_receiver_reservation_rejects_nested_mutation() {
    assert_mut_borrow_conflict(
        r#"
use std::evm::{Address, StorageMap}

struct Store {
    balances: StorageMap<Address, u256>,
}

fn replace(_ key: Address, value: u256) -> u256 uses (store: mut Store) {
    store.balances.set(key, value)
    value
}

fn bad(_ key: Address, value: u256) uses (store: mut Store) {
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
    assert_mut_borrow_conflict(
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
    let diags = borrow_diags(
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
    );

    assert!(diags.contains("borrow conflict"), "{diags:?}");
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
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
                dst,
                expr: NExpr::ReadPlace { mode, .. },
            } if normalized.locals[dst.index()].ty.pretty_print(&db) == "Row" => Some(mode),
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
    let store_local = normalized
        .locals
        .iter()
        .enumerate()
        .find_map(|(idx, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::EffectParam { .. }) => {
                Some((idx, local))
            }
            _ => None,
        })
        .expect("store effect binding");
    let root = match &store_local.1.lowering {
        NormalizedBindingLowering::ValueLocal { place } => place
            .root
            .borrow_root()
            .expect("store binding should preserve a borrow root"),
        ref lowering => panic!("unexpected lowering for store binding: {lowering:?}"),
    };
    let Some(NBorrowRoot::Provider { value_ty, .. }) = normalized.root(root) else {
        panic!(
            "expected provider root for store binding, got {:?}",
            normalized.root(root)
        );
    };
    assert_eq!(*value_ty, store_local.1.layout_ty());
    assert_eq!(
        store_local.1.facts.interface,
        SemanticLocalKind::DirectValue
    );
    assert!(matches!(
        store_local.1.facts.origin,
        NLocalOrigin::RootProvider(_)
    ));
    assert!(store_local.1.snapshot_source_place().is_some());
    let field_local = normalized
        .locals
        .get(3)
        .expect("field projection temp should exist");
    let root = match &field_local.lowering {
        NormalizedBindingLowering::ValueLocal { place } => place
            .root
            .borrow_root()
            .expect("field projection should preserve a local root"),
        ref lowering => panic!("unexpected lowering for provider field temp: {lowering:?}"),
    };
    assert!(
        matches!(
            normalized.root(root),
            Some(NBorrowRoot::LocalSlot { local }) if *local == fe_hir::analysis::semantic::SLocalId::from_u32(3)
        ),
        "expected self-rooted local slot for provider field temp, got {:?}",
        normalized.root(root)
    );
    assert_eq!(field_local.facts.interface, SemanticLocalKind::DirectValue);
    assert!(matches!(field_local.facts.origin, NLocalOrigin::SelfRooted));
    let backing_place = field_local
        .backing_place()
        .expect("field projection temp should keep its own backing place");
    let backing_root = backing_place
        .root
        .borrow_root()
        .expect("field projection backing root");
    assert!(
        matches!(
            normalized.root(backing_root),
            Some(NBorrowRoot::LocalSlot { local }) if *local == fe_hir::analysis::semantic::SLocalId::from_u32(3)
        ),
        "expected self-rooted backing place for provider field temp, got {:?}",
        normalized.root(backing_root)
    );
    assert!(backing_place.path.is_empty());
    let snapshot_source = field_local
        .snapshot_source_place()
        .expect("field projection temp should preserve its source place");
    let snapshot_root = snapshot_source
        .root
        .borrow_root()
        .expect("field projection snapshot source root");
    let Some(NBorrowRoot::Provider { value_ty, .. }) = normalized.root(snapshot_root) else {
        panic!(
            "expected provider-root snapshot source for provider field temp, got {:?}",
            normalized.root(snapshot_root)
        );
    };
    assert_eq!(*value_ty, store_local.1.layout_ty());
    assert_eq!(
        snapshot_source.path.iter().next(),
        Some(&fe_hir::projection::Projection::Field(0))
    );
    assert!(!field_local.facts.root_demand.needs_runtime_root());
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
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
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
    assert!(
        matches!(borrow.root, NSPlaceRoot::CarrierDerefLocal(local) if normalized.local(local).is_some_and(|local| {
            matches!(
                local.source,
                Some(fe_hir::analysis::ty::ty_check::LocalBinding::Param { .. })
            )
        })),
        "expected carrier-rooted view param place for ref projection, got {:?}",
        borrow.root
    );
    assert_eq!(borrow.path.len(), 1);
    assert_eq!(
        borrow.path.iter().next(),
        Some(&fe_hir::projection::Projection::Field(0))
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
    let pair_ty = normalized
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
    let locals = normalized
        .locals
        .iter()
        .enumerate()
        .filter_map(|(idx, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
                if local.ty == pair_ty =>
            {
                Some((
                    fe_hir::analysis::semantic::SLocalId::from_u32(idx as u32),
                    local,
                ))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        locals.len(),
        2,
        "expected pair/copy locals, got {locals:#?}"
    );
    let (pair_local_id, pair_local) = locals[0];
    let (copy_local_id, copy_local) = locals[1];

    for (local_id, local) in [(pair_local_id, pair_local), (copy_local_id, copy_local)] {
        assert_eq!(local.facts.interface, SemanticLocalKind::DirectValue);
        assert!(matches!(local.facts.origin, NLocalOrigin::SelfRooted));
        let backing_place = local
            .backing_place()
            .expect("projected snapshot should keep a backing place");
        let backing_root = backing_place
            .root
            .borrow_root()
            .expect("projected snapshot backing root");
        assert!(
            matches!(
                normalized.root(backing_root),
                Some(NBorrowRoot::LocalSlot { local: root_local }) if *root_local == local_id
            ),
            "expected self-rooted backing place for {local_id:?}, got {:?}",
            normalized.root(backing_root)
        );
        assert!(backing_place.path.is_empty());
    }

    let pair_snapshot = pair_local
        .snapshot_source_place()
        .expect("projected snapshot should preserve source lineage");
    let pair_snapshot_root = pair_snapshot
        .root
        .borrow_root()
        .expect("projected snapshot source root");
    assert!(
        matches!(
            normalized.root(pair_snapshot_root),
            Some(NBorrowRoot::Param { .. })
        ),
        "expected param-root snapshot lineage for projected local, got {:?}",
        normalized.root(pair_snapshot_root)
    );
    assert_eq!(
        pair_snapshot.path.iter().next(),
        Some(&fe_hir::projection::Projection::Field(0))
    );

    let copy_snapshot = copy_local
        .snapshot_source_place()
        .expect("forwarded snapshot should preserve source lineage");
    assert_eq!(copy_snapshot, pair_snapshot);

    let borrow = normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
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
    let borrow_root = borrow.root.borrow_root().expect("borrow root");
    assert!(
        matches!(
            normalized.root(borrow_root),
            Some(NBorrowRoot::LocalSlot { local }) if *local == copy_local_id
        ),
        "expected borrow of copied snapshot to use its own local root, got {:?}",
        normalized.root(borrow_root)
    );
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
            .blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
        {
            let NSStmtKind::Assign {
                dst,
                expr: NExpr::ReadPlace { place, .. },
            } = &stmt.kind
            else {
                continue;
            };
            let local = &normalized.locals[dst.index()];
            if local.ty.pretty_print(&db) == elem_ty {
                assert!(
                    matches!(
                        place.path.iter().cloned().collect::<Vec<_>>().as_slice(),
                        [
                            Projection::Field(path_field),
                            Projection::Index(IndexSource::Dynamic(_))
                        ] if *path_field == field
                    ),
                    "unexpected nested place path in {name}: {:?}",
                    place.path
                );
                saw_nested_read = true;
            }
            assert!(
                !(local.ty.array_len(&db).is_some()
                    && place.path.iter().cloned().collect::<Vec<_>>()
                        == vec![Projection::Field(field)]),
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
fn owned_aggregate_value_boundaries_project_from_the_owned_local() {
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

    let (used_local_id, used_local) = normalized
        .locals
        .iter()
        .enumerate()
        .find_map(|(idx, local)| match local.source {
            Some(fe_hir::analysis::ty::ty_check::LocalBinding::Local { .. })
                if local.ty.array_len(&db).is_some() =>
            {
                Some((
                    fe_hir::analysis::semantic::SLocalId::from_u32(idx as u32),
                    local,
                ))
            }
            _ => None,
        })
        .expect("owned array local");

    assert_eq!(used_local.facts.interface, SemanticLocalKind::DirectValue);
    assert!(matches!(used_local.facts.origin, NLocalOrigin::SelfRooted));
    let backing_place = used_local
        .backing_place()
        .expect("owned aggregate local should keep backing storage");
    let backing_root = backing_place.root.borrow_root().expect("backing root");
    assert!(
        matches!(
            normalized.root(backing_root),
            Some(NBorrowRoot::LocalSlot { local }) if *local == used_local_id
        ),
        "expected owned aggregate backing root to be the local itself, got {:?}",
        normalized.root(backing_root)
    );
    assert!(backing_place.path.is_empty());
    let snapshot_source = used_local
        .snapshot_source_place()
        .expect("owned aggregate should preserve lineage");
    let snapshot_root = snapshot_source.root.borrow_root().expect("snapshot root");
    assert!(
        matches!(
            normalized.root(snapshot_root),
            Some(NBorrowRoot::Param { .. })
        ),
        "expected source lineage to point at the parameter root, got {:?}",
        normalized.root(snapshot_root)
    );
    assert_eq!(
        snapshot_source.path.iter().cloned().collect::<Vec<_>>(),
        vec![Projection::Field(0)]
    );

    let element_read = normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
                dst,
                expr: NExpr::ReadPlace { place, .. },
            } if normalized.locals[dst.index()].ty.pretty_print(&db) == "u8" => Some(place),
            _ => None,
        })
        .expect("element read");
    let read_root = element_read.root.borrow_root().expect("element read root");
    assert!(
        matches!(
            normalized.root(read_root),
            Some(NBorrowRoot::LocalSlot { local }) if *local == used_local_id
        ),
        "expected owned aggregate projection to read from the owned local, got {:?}",
        normalized.root(read_root)
    );
    assert!(
        matches!(
            element_read
                .path
                .iter()
                .cloned()
                .collect::<Vec<_>>()
                .as_slice(),
            [Projection::Index(IndexSource::Dynamic(_))]
        ),
        "unexpected owned-local projection path: {:?}",
        element_read.path
    );
}

#[test]
fn zero_sized_aggregate_fixture_instances_normalize_and_borrowcheck() {
    for_each_fixture_instance(
        include_str!("../../codegen/tests/fixtures/zero_sized_aggregates.fe"),
        |db, instance| {
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
