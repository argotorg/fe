use fe_hir::{
    analysis::{
        initialize_analysis_pass,
        semantic::{
            NExpr, NSStmtKind, collect_semantic_borrow_diagnostic_vouchers,
            get_or_build_semantic_instance, identity_semantic_instance_key,
            normalize_semantic_body, normalize_semantic_body_for_layout_evidence,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::ItemKind,
    test_db::{HirAnalysisTestDb, format_diagnostics},
};

fn definite_init_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("definite_initialization.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    )
}

fn all_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("definite_initialization.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod))
}

#[test]
fn rejects_direct_read_before_initialization() {
    let diags = definite_init_diags(
        r#"
fn read() -> u256 {
    let mut value: u256
    value
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn read`"),
        "{diags}"
    );
    assert!(
        diags.contains("local `value` may be used before it is initialized"),
        "{diags}"
    );
    assert!(
        diags.contains("`value` is declared without an initial value"),
        "{diags}"
    );
}

#[test]
fn rejects_copy_capture_before_initialization() {
    let diags = definite_init_diags(
        r#"
use core::functional::Fn

fn capture() -> u256 {
    let mut value: u256
    let read = || value
    value = 42
    read.call()
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn capture`"),
        "{diags}"
    );
    assert!(
        diags.contains("local `value` may be used before it is initialized"),
        "{diags}"
    );
}

#[test]
fn rejects_initialization_on_only_one_branch() {
    let diags = definite_init_diags(
        r#"
fn maybe(cond: bool) -> u256 {
    let mut value: u256
    if cond {
        value = 1
    }
    value
}
"#,
    );
    assert!(
        diags.contains("local `value` may be used before it is initialized"),
        "{diags}"
    );
}

#[test]
fn rejects_ref_and_mut_borrows_before_initialization() {
    let diags = definite_init_diags(
        r#"
fn borrow_ref() {
    let mut value: u256
    let borrowed = ref value
}

fn borrow_mut() {
    let mut value: u256
    let borrowed = mut value
}
"#,
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2,
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn borrow_ref`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn borrow_mut`"),
        "{diags}"
    );
}

#[test]
fn rejects_uninitialized_dynamic_index_reads_and_writes() {
    let diags = definite_init_diags(
        r#"
fn read_index(values: [u256; 2]) -> u256 {
    let mut index: usize
    values[index]
}

fn write_index() {
    let mut values: [u256; 2] = [0, 0]
    let mut index: usize
    values[index] = 1
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn read_index`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn write_index`"),
        "{diags}"
    );
}

#[test]
fn accepts_initialization_before_read_on_all_paths() {
    let diags = definite_init_diags(
        r#"
fn direct() -> u256 {
    let mut value: u256
    value = 1
    value
}

fn branches(cond: bool) -> u256 {
    let mut value: u256
    if cond {
        value = 1
    } else {
        value = 2
    }
    value
}

fn unused() {
    let mut value: u256
}

fn borrow_after_init() {
    let mut ref_value: u256
    ref_value = 1
    let shared = ref ref_value

    let mut mut_value: u256
    mut_value = 2
    let exclusive = mut mut_value
}

fn index_after_init(values: [u256; 2]) -> u256 {
    let mut index: usize
    index = 1
    values[index]
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_body_locals_are_allocated_and_checked_for_initialization() {
    let diags = definite_init_diags(
        r#"
use core::functional::Fn

fn initialized_scalar() -> u256 {
    let run = || {
        let mut value: u256
        value = 42
        value
    }
    run.call()
}

fn initialized_dynamic_index(index: usize) -> u256 {
    let run = || {
        let mut values: [u256; 2]
        values[index] = 42
        values[index]
    }
    run.call()
}

fn initialized_ref_dynamic_index(index: ref usize) -> u256 {
    let run = || {
        let mut values: [u256; 2]
        values[index] = 42
        values[index]
    }
    run.call()
}

fn initialized_nested_pattern() -> u256 {
    let run = || {
        let (mut left, mut right): (u256, u256)
        left = 20
        right = 22
        left + right
    }
    run.call()
}

fn uninitialized_scalar() -> u256 {
    let run = || {
        let mut value: u256
        value
    }
    run.call()
}

fn mutable_dynamic_index(index: mut usize) -> u256 {
    let run = || {
        let mut values: [u256; 2]
        values[index] = 42
        index = 1
        values[index]
    }
    run.call()
}

fn distinct_dynamic_indices(write_index: usize, read_index: usize) -> u256 {
    let run = || {
        let mut values: [u256; 2]
        values[write_index] = 42
        values[read_index]
    }
    run.call()
}
"#,
    );
    assert!(
        diags.contains("local `value` may be used before it is initialized"),
        "{diags}"
    );
    assert_eq!(
        diags
            .matches("local `values` may be used before it is initialized")
            .count(),
        2,
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        3,
        "{diags}"
    );
}

#[test]
fn deduplicates_closure_specializations_without_merging_distinct_locals() {
    let diags = definite_init_diags(
        r#"
use core::functional::Fn

fn generic_closures<T>(_ marker: T) -> u256 {
    let read_first = || {
        let mut first: u256
        first
    }
    let read_second = || {
        let mut second: u256
        second
    }
    read_first.call() + read_second.call()
}

fn instantiate() -> u256 {
    generic_closures(1 as u256) + generic_closures(true)
}
"#,
    );
    assert!(
        diags.contains("local `first` may be used before it is initialized"),
        "{diags}"
    );
    assert!(
        diags.contains("local `second` may be used before it is initialized"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2,
        "{diags}"
    );
}

#[test]
fn ignores_reads_after_all_proven_nonreturning_call_graph_shapes() {
    let diags = definite_init_diags(
        r#"
fn sccp_abort() -> u256 {
    let done = true
    if done {
        core::panic()
    }
    0
}

fn maybe_abort(flag: bool) -> u256 {
    if flag {
        core::panic()
    }
    0
}

fn leaf_abort() -> u256 {
    core::panic()
}

fn middle_abort() -> u256 {
    leaf_abort()
}

fn outer_abort() -> u256 {
    middle_abort()
}

fn direct_recursive() -> u256 {
    direct_recursive()
}

fn mutual_left() -> u256 {
    mutual_right()
}

fn mutual_right() -> u256 {
    mutual_left()
}

fn recursive_with_base(flag: bool) -> u256 {
    if flag {
        return 0
    }
    recursive_with_base(flag)
}

fn after_sccp() -> u256 {
    let mut value: u256
    sccp_abort()
    value
}

fn after_transitive() -> u256 {
    let mut value: u256
    outer_abort()
    value
}

fn after_direct_recursive() -> u256 {
    let mut value: u256
    direct_recursive()
    value
}

fn after_mutual_recursion() -> u256 {
    let mut value: u256
    mutual_left()
    value
}

fn after_maybe(flag: bool) -> u256 {
    let mut value: u256
    maybe_abort(flag)
    value
}

fn after_recursive_base(flag: bool) -> u256 {
    let mut value: u256
    recursive_with_base(flag)
    value
}
"#,
    );

    for name in [
        "after_sccp",
        "after_transitive",
        "after_direct_recursive",
        "after_mutual_recursion",
    ] {
        assert!(
            !diags.contains(&format!("possibly uninitialized local in `fn {name}`")),
            "{diags}"
        );
    }
    for name in ["after_maybe", "after_recursive_base"] {
        assert!(
            diags.contains(&format!("possibly uninitialized local in `fn {name}`")),
            "{diags}"
        );
    }
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2,
        "{diags}"
    );
}

#[test]
fn finite_self_specialization_closes_the_never_return_graph() {
    let diags = definite_init_diags(
        r#"
fn finite_self<T>(_ value: T) -> u256 {
    finite_self(0 as u256)
}

fn after_finite_self() -> u256 {
    let value: u256
    finite_self(true)
    value
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_calls_definitely_initialize_captured_contract_fields() {
    let diags = all_diags(
        r#"
use core::functional::{Fn, FnOnce}

struct Boxed {
    value: u256,
}

contract ReusableCapture {
    value: u256

    init(input: u256) uses (mut value) {
        let initialize = || {
            value = input
        }
        initialize.call()
    }
}

contract ConsumingCapture {
    value: u256

    init(input: own Boxed) uses (mut value) {
        let initialize = || {
            value = input.value
            let consumed = input
        }
        initialize.call_once()
    }
}

contract ClosureArgument {
    value: u256

    init(input: u256) uses (mut value) {
        let initialize = |target: mut u256| {
            target = input
        }
        initialize.call(mut value)
    }
}

contract NestedCapture {
    value: u256

    init(input: u256) uses (mut value) {
        let outer = || {
            let inner = || {
                value = input
            }
            inner.call()
        }
        outer.call()
    }
}

contract BothClosurePaths {
    value: u256

    init(flag: bool, input: u256) uses (mut value) {
        let initialize = || {
            value = input
        }
        if flag {
            initialize.call()
        } else {
            initialize.call()
        }
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_assignment_credit_requires_every_normal_path_and_an_actual_call() {
    let diags = all_diags(
        r#"
use core::functional::Fn

struct Pair {
    left: u256,
    right: u256,
}

contract ConditionalBody {
    conditional_body: u256

    init(flag: bool, input: u256) uses (mut conditional_body) {
        let initialize = || {
            if flag {
                conditional_body = input
            }
        }
        initialize.call()
    }
}

contract ConditionalCall {
    conditional_call: u256

    init(flag: bool, input: u256) uses (mut conditional_call) {
        let initialize = || {
            conditional_call = input
        }
        if flag {
            initialize.call()
        }
    }
}

contract UncalledClosure {
    uncalled: u256

    init(input: u256) uses (mut uncalled) {
        let initialize = || {
            uncalled = input
        }
    }
}

contract PartialReferentWrite {
    pair: Pair

    init(input: u256) uses (mut pair) {
        let update = || {
            pair.left = input
        }
        update.call()
    }
}

contract DynamicIndexWrite {
    values: [u256; 2]

    init(index: usize, input: u256) uses (mut values) {
        let update = || {
            values[index] = input
        }
        update.call()
    }
}
"#,
    );

    for field in [
        "conditional_body",
        "conditional_call",
        "uncalled",
        "pair",
        "values",
    ] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        5,
        "{diags}",
    );
}

#[test]
fn closure_never_returning_paths_do_not_require_spurious_assignment() {
    let diags = all_diags(
        r#"
use core::functional::Fn

contract AlwaysDiverges {
    value: u256

    init() uses (mut value) {
        let stop = || {
            core::panic()
        }
        stop.call()
    }
}

contract AssignsOnEveryNormalClosureExit {
    value: u256

    init(flag: bool, input: u256) uses (mut value) {
        let initialize = || {
            if flag {
                core::panic()
            }
            value = input
        }
        initialize.call()
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn generic_closure_forwarding_maps_only_definite_calls_back_to_captures() {
    let diags = all_diags(
        r#"
use core::functional::{Fn, FnOnce}

struct Boxed {
    value: u256,
}

fn invoke<F: Fn<(), ()>>(_ function: F) {
    function.call()
}

fn invoke_once<F: FnOnce<(), ()>>(_ function: own F) {
    function.call_once()
}

fn maybe_invoke<F: Fn<(), ()>>(_ function: F, flag: bool) {
    if flag {
        function.call()
    }
}

fn ignore<F: Fn<(), ()>>(_ function: F) {}

contract GenericForward {
    forwarded: u256

    init(input: u256) uses (mut forwarded) {
        invoke(|| {
            forwarded = input
        })
    }
}

contract GenericConsumingForward {
    consumed_value: u256

    init(input: own Boxed) uses (mut consumed_value) {
        invoke_once(|| {
            consumed_value = input.value
            let consumed = input
        })
    }
}

contract ConditionalGenericForward {
    conditional: u256

    init(flag: bool, input: u256) uses (mut conditional) {
        maybe_invoke(
            || {
                conditional = input
            },
            flag,
        )
    }
}

contract IgnoredGenericClosure {
    ignored: u256

    init(input: u256) uses (mut ignored) {
        ignore(|| {
            ignored = input
        })
    }
}
"#,
    );

    assert!(!diags.contains("`forwarded` must be assigned"), "{diags}",);
    assert!(
        !diags.contains("`consumed_value` must be assigned"),
        "{diags}",
    );
    for field in ["conditional", "ignored"] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        2,
        "{diags}",
    );
}

#[test]
fn by_value_materialization_does_not_forward_assignment_provenance() {
    let diags = all_diags(
        r#"
fn initialize(target: mut u256, input: u256) {
    target = input
}

fn copy_then_initialize(target: mut u256, input: u256) {
    let mut copy: u256 = target
    initialize(target: mut copy, input: input)
}

struct Handle {
    target: mut u256,
}

fn copy_projected_then_initialize(handle: Handle, input: u256) {
    let mut copy: u256 = handle.target
    initialize(target: mut copy, input: input)
}

fn copy_shared_then_initialize(target: ref u256, input: u256) {
    let mut copy: u256 = target
    initialize(target: mut copy, input: input)
}

fn copy_value_then_initialize(target: u256, input: u256) {
    let mut copy: u256 = target
    initialize(target: mut copy, input: input)
}

struct Boxed {
    value: u256,
}

fn initialize_box(target: mut Boxed, input: u256) {
    target = Boxed { value: input }
}

fn move_then_initialize(target: own Boxed, input: u256) {
    let mut copy: Boxed = target
    initialize_box(target: mut copy, input: input)
}

contract DirectMaterialization {
    direct_value: u256

    init(input: u256) uses (mut direct_value) {
        copy_then_initialize(target: mut direct_value, input: input)
    }
}

contract ProjectedMaterialization {
    projected_value: u256

    init(input: u256) uses (mut projected_value) {
        copy_projected_then_initialize(
            handle: Handle { target: mut projected_value },
            input: input,
        )
    }
}

contract SharedMaterialization {
    shared_value: u256

    init(input: u256) uses (mut shared_value) {
        copy_shared_then_initialize(target: ref shared_value, input: input)
    }
}

contract ByValueMaterialization {
    by_value: u256

    init(input: u256) uses (mut by_value) {
        copy_value_then_initialize(target: by_value, input: input)
    }
}

contract OwnedValueMaterialization {
    owned_box: Boxed

    init(input: u256) uses (mut owned_box) {
        move_then_initialize(target: owned_box, input: input)
    }
}
"#,
    );

    for field in [
        "direct_value",
        "projected_value",
        "shared_value",
        "by_value",
        "owned_box",
    ] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        5,
        "{diags}",
    );
}

#[test]
fn mutable_carrier_rebinding_invalidates_initializer_provenance() {
    let diags = all_diags(
        r#"
use core::functional::Fn

struct Handle {
    target: mut u256,
}

impl Copy for Handle {}

struct NestedHandle {
    handle: Handle,
}

struct SharedNested {
    shared: mut Handle,
}

struct PairHandle {
    left: mut u256,
    right: mut u256,
}

fn initialize(target: mut u256, input: u256) {
    target = input
}

fn rebind(handle: mut Handle, replacement: mut u256) {
    handle.target = mut replacement
}

fn rebind_nested(handle: mut NestedHandle, replacement: mut u256) {
    handle.handle.target = mut replacement
}

fn replace_nested_handle(handle: mut NestedHandle, replacement: own Handle) {
    handle.handle = replacement
}

fn rebind_then_abort(handle: mut Handle, replacement: mut u256) {
    handle.target = mut replacement
    core::panic()
}

fn rebind_copy(mut handle: Handle, replacement: mut u256) {
    handle.target = mut replacement
}

fn rebind_through_shared(handle: own SharedNested, replacement: mut u256) {
    handle.shared.target = mut replacement
}

fn rebind_owned_then_initialize(
    mut handle: own Handle,
    replacement: mut u256,
    input: u256,
) {
    handle.target = mut replacement
    initialize(target: handle.target, input: input)
}

contract StableCarrier {
    stable: u256

    init(input: u256) uses (mut stable) {
        let holder = Handle { target: mut stable }
        initialize(target: holder.target, input: input)
    }
}

contract StableMutableCarrier {
    stable_mutable: u256

    init(input: u256) uses (mut stable_mutable) {
        let mut holder = Handle { target: mut stable_mutable }
        initialize(target: holder.target, input: input)
    }
}

contract DirectRebind {
    direct_first: u256
    direct_second: u256

    init(input: u256) uses (mut direct_first, mut direct_second) {
        let mut holder = Handle { target: mut direct_first }
        holder.target = mut direct_second
        initialize(target: holder.target, input: input)
    }
}

contract NestedRebind {
    nested_first: u256
    nested_second: u256

    init(input: u256) uses (mut nested_first, mut nested_second) {
        let mut holder = NestedHandle {
            handle: Handle { target: mut nested_first },
        }
        holder.handle.target = mut nested_second
        initialize(target: holder.handle.target, input: input)
    }
}

contract WholeWrapperRebind {
    whole_first: u256
    whole_second: u256

    init(input: u256) uses (mut whole_first, mut whole_second) {
        let mut holder = Handle { target: mut whole_first }
        holder = Handle { target: mut whole_second }
        initialize(target: holder.target, input: input)
    }
}

contract BranchRebind {
    branch_first: u256
    branch_second: u256

    init(flag: bool, input: u256) uses (mut branch_first, mut branch_second) {
        let mut holder = Handle { target: mut branch_first }
        if flag {
            holder.target = mut branch_second
        }
        initialize(target: holder.target, input: input)
    }
}

contract CallMediatedRebind {
    call_first: u256
    call_second: u256

    init(input: u256) uses (mut call_first, mut call_second) {
        let mut holder = Handle { target: mut call_first }
        rebind(handle: mut holder, replacement: mut call_second)
        initialize(target: holder.target, input: input)
    }
}

contract MutableOwnedParamRebind {
    owned_first: u256
    owned_second: u256

    init(input: u256) uses (mut owned_first, mut owned_second) {
        rebind_owned_then_initialize(
            handle: Handle { target: mut owned_first },
            replacement: mut owned_second,
            input: input,
        )
    }
}

contract SameTargetOnBothBranches {
    same_first: u256
    same_second: u256

    init(flag: bool, input: u256) uses (mut same_first, mut same_second) {
        let mut holder = Handle { target: mut same_first }
        if flag {
            holder.target = mut same_second
        } else {
            holder.target = mut same_second
        }
        initialize(target: holder.target, input: input)
    }
}

contract PureRebind {
    pure_first: u256
    pure_second: u256

    init() uses (mut pure_first, mut pure_second) {
        let mut holder = Handle { target: mut pure_first }
        holder.target = mut pure_second
    }
}

contract NestedCallRebind {
    nested_call_first: u256
    nested_call_second: u256

    init(input: u256) uses (mut nested_call_first, mut nested_call_second) {
        let mut holder = NestedHandle {
            handle: Handle { target: mut nested_call_first },
        }
        rebind_nested(handle: mut holder, replacement: mut nested_call_second)
        initialize(target: holder.handle.target, input: input)
    }
}

contract NestedWholeCallRebind {
    nested_whole_first: u256
    nested_whole_second: u256

    init(input: u256) uses (mut nested_whole_first, mut nested_whole_second) {
        let mut holder = NestedHandle {
            handle: Handle { target: mut nested_whole_first },
        }
        replace_nested_handle(
            handle: mut holder,
            replacement: Handle { target: mut nested_whole_second },
        )
        initialize(target: holder.handle.target, input: input)
    }
}

contract CapturedRebind {
    captured_first: u256
    captured_second: u256

    init(input: u256) uses (mut captured_first, mut captured_second) {
        let mut holder = Handle { target: mut captured_first }
        let holder_ref = mut holder
        let rebind_captured = || {
            holder_ref.target = mut captured_second
        }
        rebind_captured.call()
        initialize(target: holder.target, input: input)
    }
}

contract DivergingRebind {
    diverging_first: u256
    diverging_second: u256

    init(flag: bool, input: u256) uses (mut diverging_first, mut diverging_second) {
        let mut holder = Handle { target: mut diverging_first }
        if flag {
            rebind_then_abort(
                handle: mut holder,
                replacement: mut diverging_second,
            )
        }
        initialize(target: holder.target, input: input)
    }
}

contract SiblingCarrier {
    stale_left: u256
    stable_right: u256
    replacement_left: u256

    init(input: u256)
        uses (mut stale_left, mut stable_right, mut replacement_left)
    {
        let mut holder = PairHandle {
            left: mut stale_left,
            right: mut stable_right,
        }
        holder.left = mut replacement_left
        initialize(target: holder.left, input: input)
        initialize(target: holder.right, input: input)
    }
}

contract CopiedCarrierRebind {
    copied_original: u256
    copied_replacement: u256

    init(input: u256) uses (mut copied_original, mut copied_replacement) {
        let holder = Handle { target: mut copied_original }
        rebind_copy(handle: holder, replacement: mut copied_replacement)
        initialize(target: holder.target, input: input)
    }
}

contract NestedSharedCarrierRebind {
    shared_first: u256
    shared_second: u256

    init(input: u256) uses (mut shared_first, mut shared_second) {
        let mut holder = Handle { target: mut shared_first }
        rebind_through_shared(
            handle: SharedNested { shared: mut holder },
            replacement: mut shared_second,
        )
        initialize(target: holder.target, input: input)
    }
}
"#,
    );

    assert!(!diags.contains("`stable` must be assigned"), "{diags}");
    assert!(
        !diags.contains("`stable_mutable` must be assigned"),
        "{diags}"
    );
    for field in [
        "direct_first",
        "nested_first",
        "whole_first",
        "branch_first",
        "branch_second",
        "call_first",
        "owned_first",
        "same_first",
        "pure_first",
        "pure_second",
        "nested_call_first",
        "nested_whole_first",
        "captured_first",
        "diverging_second",
        "stale_left",
        "copied_replacement",
        "shared_first",
    ] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    for field in [
        "direct_second",
        "nested_second",
        "whole_second",
        "call_second",
        "owned_second",
        "same_second",
        "nested_call_second",
        "nested_whole_second",
        "captured_second",
        "diverging_first",
        "stable_right",
        "replacement_left",
        "copied_original",
        "shared_second",
    ] {
        assert!(
            !diags.contains(&format!("`{field}` must be assigned")),
            "unexpected immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        17,
        "{diags}",
    );
}

#[test]
fn projected_effect_carriers_preserve_write_and_rebind_provenance() {
    let diags = all_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct NestedHandle {
    handle: Handle,
}

fn write_effect(input: u256)
    uses (handle: Handle)
{
    handle.target = input
}

fn write_nested_effect(input: u256)
    uses (holder: NestedHandle)
{
    holder.handle.target = input
}

fn rebind_and_write_effect(replacement: mut u256, input: u256)
    uses (handle: mut Handle)
{
    handle.target = mut replacement
    handle.target = input
}

fn rebind_and_write_nested_effect(replacement: mut u256, input: u256)
    uses (holder: mut NestedHandle)
{
    holder.handle.target = mut replacement
    holder.handle.target = input
}

fn rebind_effect_only(replacement: mut u256)
    uses (handle: mut Handle)
{
    handle.target = mut replacement
}

contract ValueEffect {
    value: u256

    init(input: u256) uses (mut value) {
        let holder = Handle { target: mut value }
        with (holder) {
            write_effect(input)
        }
    }
}

contract NestedValueEffect {
    nested_value: u256

    init(input: u256) uses (mut nested_value) {
        let holder = NestedHandle {
            handle: Handle { target: mut nested_value },
        }
        with (holder) {
            write_nested_effect(input)
        }
    }
}

contract MutableEffect {
    mutable_first: u256
    mutable_second: u256

    init(input: u256) uses (mut mutable_first, mut mutable_second) {
        let mut holder = Handle { target: mut mutable_first }
        with (mut holder) {
            rebind_and_write_effect(
                replacement: mut mutable_second,
                input: input,
            )
        }
    }
}

contract NestedMutableEffect {
    nested_first: u256
    nested_second: u256

    init(input: u256) uses (mut nested_first, mut nested_second) {
        let mut holder = NestedHandle {
            handle: Handle { target: mut nested_first },
        }
        with (mut holder) {
            rebind_and_write_nested_effect(
                replacement: mut nested_second,
                input: input,
            )
        }
    }
}

contract PureEffectRebind {
    pure_first: u256
    pure_second: u256

    init() uses (mut pure_first, mut pure_second) {
        let mut holder = Handle { target: mut pure_first }
        with (mut holder) {
            rebind_effect_only(replacement: mut pure_second)
        }
    }
}
"#,
    );

    for field in ["value", "nested_value", "mutable_second", "nested_second"] {
        assert!(
            !diags.contains(&format!("`{field}` must be assigned")),
            "unexpected immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    for field in ["mutable_first", "nested_first", "pure_first", "pure_second"] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        4,
        "{diags}",
    );
}

#[test]
fn returned_closure_preserves_captured_assignment_provenance() {
    let diags = all_diags(
        r#"
use core::functional::Fn

fn identity<F>(function: own F) -> F {
    function
}

contract ReturnedClosure {
    value: u256

    init(input: u256) uses (mut value) {
        let initialize = identity(function: || {
            value = input
        })
        initialize.call()
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn identity_cast_preserves_only_carrier_provenance() {
    let diags = all_diags(
        r#"
struct Handle {
    target: mut u256,
}

fn initialize(target: mut u256, input: u256) {
    target = input
}

contract CastCarrier {
    carrier_value: u256

    init(input: u256) uses (mut carrier_value) {
        let holder = Handle { target: mut carrier_value }
        let casted = holder as Handle
        initialize(target: casted.target, input: input)
    }
}

contract CastScalar {
    scalar_value: u256

    init(input: u256) uses (mut scalar_value) {
        let casted = scalar_value as u256
        let mut local = casted
        initialize(target: mut local, input: input)
    }
}
"#,
    );

    assert!(
        !diags.contains("`carrier_value` must be assigned"),
        "{diags}"
    );
    assert!(diags.contains("`scalar_value` must be assigned"), "{diags}");
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        1,
        "{diags}",
    );
}

#[test]
fn assignment_analysis_preserves_calls_but_ignores_unreachable_writes() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "definite_assignment_operation_view.fe".into(),
        r#"
const fn folded_marker() -> u256 {
    7
}

fn folded_subject() -> u256 {
    folded_marker()
}

fn initialize(target: mut u256, input: u256) {
    target = input
}

contract LiveAndDeadWrites {
    live_value: u256
    dead_value: u256

    init(input: u256) uses (mut live_value, mut dead_value) {
        let marker = folded_marker()
        initialize(target: mut live_value, input: input + marker)
        if false {
            initialize(target: mut dead_value, input: input)
        }
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let subject = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "folded_subject") =>
            {
                Some(*func)
            }
            _ => None,
        })
        .expect("missing folded_subject");
    let instance = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(subject)),
    );
    let runtime = normalize_semantic_body(&db, instance).expect("runtime normalization");
    let analysis = normalize_semantic_body_for_layout_evidence(&db, instance)
        .expect("operation-preserving normalization");
    let call_count = |body: &fe_hir::analysis::semantic::NormalizedSemanticBody<'_>| {
        body.blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .filter(|stmt| {
                matches!(
                    stmt.kind,
                    NSStmtKind::Assign {
                        expr: NExpr::Call { .. },
                        ..
                    }
                )
            })
            .count()
    };
    assert_eq!(call_count(&runtime), 0);
    assert_eq!(call_count(&analysis), 1);

    let diags = format_diagnostics(&db, &initialize_analysis_pass().run_on_module(&db, top_mod));
    assert!(!diags.contains("`live_value` must be assigned"), "{diags}");
    assert!(diags.contains("`dead_value` must be assigned"), "{diags}");
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        1,
        "{diags}",
    );
}

#[test]
fn static_array_and_enum_carrier_projections_preserve_provenance() {
    let diags = all_diags(
        r#"
struct Handle {
    target: mut u256,
}

enum Choice {
    Selected(Handle),
    Other(Handle),
}

fn initialize(target: mut u256, input: u256) {
    target = input
}

contract ConstantArrayProjection {
    array_value: u256

    init(input: u256) uses (mut array_value) {
        let holders = [Handle { target: mut array_value }]
        initialize(target: holders[0].target, input: input)
    }
}

contract KnownEnumVariant {
    enum_value: u256

    init(input: u256) uses (mut enum_value) {
        let choice = Choice::Selected(Handle { target: mut enum_value })
        match choice {
            Choice::Selected(holder) => {
                initialize(target: holder.target, input: input)
            }
            Choice::Other(_) => {}
        }
    }
}

contract DynamicArrayProjection {
    dynamic_first: u256
    dynamic_second: u256

    init(index: usize, input: u256) uses (mut dynamic_first, mut dynamic_second) {
        let holders = [
            Handle { target: mut dynamic_first },
            Handle { target: mut dynamic_second },
        ]
        initialize(target: holders[index].target, input: input)
    }
}

contract DynamicEnumVariant {
    variant_first: u256
    variant_second: u256

    init(flag: bool, input: u256) uses (mut variant_first, mut variant_second) {
        let choice = if flag {
            Choice::Selected(Handle { target: mut variant_first })
        } else {
            Choice::Other(Handle { target: mut variant_second })
        }
        match choice {
            Choice::Selected(holder) => {
                initialize(target: holder.target, input: input)
            }
            Choice::Other(holder) => {
                initialize(target: holder.target, input: input)
            }
        }
    }
}
"#,
    );

    for field in ["array_value", "enum_value"] {
        assert!(
            !diags.contains(&format!("`{field}` must be assigned")),
            "unexpected immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    for field in [
        "dynamic_first",
        "dynamic_second",
        "variant_first",
        "variant_second",
    ] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        4,
        "{diags}",
    );
}

#[test]
fn structured_carrier_updates_are_sparse_and_alias_safe() {
    let diags = all_diags(
        r#"
struct Handle {
    target: mut u256,
}

impl Copy for Handle {}

struct PairHandle {
    left: mut u256,
    right: ref Handle,
}

struct HandleRefs {
    handles: [ref Handle; 2],
}

enum HandleArrayChoice {
    Selected([ref Handle; 2]),
    Other([ref Handle; 2]),
}

fn initialize(target: mut u256, input: u256) {
    target = input
}

fn rebind(handle: mut Handle, replacement: mut u256) {
    handle.target = mut replacement
}

fn forward(value: mut u256) -> mut u256 {
    value
}

fn rebind_same(handle: mut Handle) {
    handle.target = forward(value: mut handle.target)
}

fn rebind_at(
    handles: mut [Handle; 2],
    index: usize,
    replacement: mut u256,
) {
    handles[index].target = mut replacement
}

fn initialize_at(
    handles: [ref Handle; 2],
    index: usize,
    input: u256,
) {
    initialize(target: handles[index].target, input: input)
}

fn initialize_effect_at(index: usize, input: u256)
    uses (provider: mut HandleRefs)
{
    initialize(
        target: provider.handles[index].target,
        input: input,
    )
}

contract DynamicSameSelector {
    same_first: u256
    same_second: u256
    same_replacement: u256

    init(index: usize, input: u256)
        uses (mut same_first, mut same_second, mut same_replacement)
    {
        let mut holders = [
            Handle { target: mut same_first },
            Handle { target: mut same_second },
        ]
        holders[index].target = mut same_replacement
        initialize(target: holders[index].target, input: input)
    }
}

contract DynamicDifferentSelector {
    different_first: u256
    different_second: u256
    different_replacement: u256

    init(write_index: usize, read_index: usize, input: u256)
        uses (
            mut different_first,
            mut different_second,
            mut different_replacement,
        )
    {
        let mut holders = [
            Handle { target: mut different_first },
            Handle { target: mut different_second },
        ]
        holders[write_index].target = mut different_replacement
        initialize(target: holders[read_index].target, input: input)
    }
}

contract ConstantThenDynamic {
    constant_dynamic_first: u256
    constant_dynamic_second: u256
    constant_dynamic_replacement: u256

    init(index: usize, input: u256)
        uses (
            mut constant_dynamic_first,
            mut constant_dynamic_second,
            mut constant_dynamic_replacement,
        )
    {
        let mut holders = [
            Handle { target: mut constant_dynamic_first },
            Handle { target: mut constant_dynamic_second },
        ]
        holders[0].target = mut constant_dynamic_replacement
        initialize(target: holders[index].target, input: input)
    }
}

contract DynamicThenConstant {
    dynamic_constant_first: u256
    dynamic_constant_second: u256
    dynamic_constant_replacement: u256
    dynamic_constant_final: u256

    init(index: usize, input: u256)
        uses (
            mut dynamic_constant_first,
            mut dynamic_constant_second,
            mut dynamic_constant_replacement,
            mut dynamic_constant_final,
        )
    {
        let mut holders = [
            Handle { target: mut dynamic_constant_first },
            Handle { target: mut dynamic_constant_second },
        ]
        holders[index].target = mut dynamic_constant_replacement
        holders[0].target = mut dynamic_constant_final
        initialize(target: holders[index].target, input: input)
    }
}

contract DynamicSibling {
    sibling_first: u256
    sibling_second: u256
    sibling_replacement: u256
    sibling_stable: u256

    init(index: usize, input: u256)
        uses (
            mut sibling_first,
            mut sibling_second,
            mut sibling_replacement,
            mut sibling_stable,
        )
    {
        let stable_holder = Handle { target: mut sibling_stable }
        let mut holders = [
            PairHandle {
                left: mut sibling_first,
                right: ref stable_holder,
            },
            PairHandle {
                left: mut sibling_second,
                right: ref stable_holder,
            },
        ]
        holders[index].left = mut sibling_replacement
        initialize(target: holders[index].left, input: input)
        initialize(target: holders[index].right.target, input: input)
    }
}

contract WholeStructuredReplacement {
    whole_old_first: u256
    whole_old_second: u256
    whole_stale_override: u256
    whole_final_first: u256
    whole_final_second: u256

    init(input: u256)
        uses (
            mut whole_old_first,
            mut whole_old_second,
            mut whole_stale_override,
            mut whole_final_first,
            mut whole_final_second,
        )
    {
        let mut holders = [
            Handle { target: mut whole_old_first },
            Handle { target: mut whole_old_second },
        ]
        holders[0].target = mut whole_stale_override
        holders = [
            Handle { target: mut whole_final_first },
            Handle { target: mut whole_final_second },
        ]
        initialize(target: holders[0].target, input: input)
        initialize(target: holders[1].target, input: input)
    }
}

contract ExplicitAllEqual {
    explicit_equal: u256

    init(index: usize, input: u256) uses (mut explicit_equal) {
        let holder = Handle { target: mut explicit_equal }
        let holders = [ref holder, ref holder]
        initialize(target: holders[index].target, input: input)
    }
}

contract HugeRepeatedArray {
    huge_repeat: u256

    init(input: u256) uses (mut huge_repeat) {
        let holder = Handle { target: mut huge_repeat }
        let holders = [ref holder; 1000000]
        initialize(target: holders[999999].target, input: input)
    }
}

contract SameRepeatedBranches {
    same_branch: u256

    init(flag: bool, index: usize, input: u256) uses (mut same_branch) {
        let holder = Handle { target: mut same_branch }
        let mut holders = [ref holder; 2]
        if flag {
            holders = [ref holder; 2]
        } else {
            holders = [ref holder; 2]
        }
        initialize(target: holders[index].target, input: input)
    }
}

contract DifferentRepeatedBranches {
    different_branch_left: u256
    different_branch_right: u256

    init(flag: bool, index: usize, input: u256)
        uses (mut different_branch_left, mut different_branch_right)
    {
        let left = Handle { target: mut different_branch_left }
        let right = Handle { target: mut different_branch_right }
        let mut holders = [ref left; 2]
        if flag {
            holders = [ref left; 2]
        } else {
            holders = [ref right; 2]
        }
        initialize(target: holders[index].target, input: input)
    }
}

contract DirectNoOpRebind {
    direct_noop: u256

    init(flag: bool, input: u256) uses (mut direct_noop) {
        let mut holder = Handle { target: mut direct_noop }
        if flag {
            holder.target = mut direct_noop
        }
        initialize(target: holder.target, input: input)
    }
}

contract WholeNoOpRebind {
    whole_noop: u256

    init(flag: bool, input: u256) uses (mut whole_noop) {
        let mut holder = Handle { target: mut whole_noop }
        if flag {
            holder = Handle { target: mut whole_noop }
        }
        initialize(target: holder.target, input: input)
    }
}

contract CallNoOpRebind {
    call_noop: u256

    init(flag: bool, input: u256) uses (mut call_noop) {
        let mut holder = Handle { target: mut call_noop }
        if flag {
            rebind(handle: mut holder, replacement: mut call_noop)
        }
        initialize(target: holder.target, input: input)
    }
}

contract CallSummaryNoopRebind {
    call_summary_noop: u256

    init(input: u256) uses (mut call_summary_noop) {
        let mut holder = Handle { target: mut call_summary_noop }
        rebind_same(handle: mut holder)
        initialize(target: holder.target, input: input)
    }
}

contract DynamicHelperRebind {
    helper_rebind_first: u256
    helper_rebind_second: u256
    helper_rebind_replacement: u256

    init(index: usize, input: u256)
        uses (
            mut helper_rebind_first,
            mut helper_rebind_second,
            mut helper_rebind_replacement,
        )
    {
        let mut holders = [
            Handle { target: mut helper_rebind_first },
            Handle { target: mut helper_rebind_second },
        ]
        rebind_at(
            handles: mut holders,
            index: index,
            replacement: mut helper_rebind_replacement,
        )
        initialize(target: holders[0].target, input: input)
    }
}

contract DynamicHelperWriteSame {
    helper_write_same: u256

    init(index: usize, input: u256) uses (mut helper_write_same) {
        let holder = Handle { target: mut helper_write_same }
        let holders = [ref holder; 2]
        initialize_at(handles: holders, index: index, input: input)
    }
}

contract DynamicHelperWriteDifferent {
    helper_write_left: u256
    helper_write_right: u256

    init(index: usize, input: u256)
        uses (mut helper_write_left, mut helper_write_right)
    {
        let left = Handle { target: mut helper_write_left }
        let right = Handle { target: mut helper_write_right }
        let holders = [ref left, ref right]
        initialize_at(handles: holders, index: index, input: input)
    }
}

contract NestedRepeatedBranches {
    nested_repeat: u256

    init(
        flag: bool,
        outer: usize,
        inner: usize,
        input: u256,
    ) uses (mut nested_repeat) {
        let holder = Handle { target: mut nested_repeat }
        let mut holders = [[ref holder; 2], [ref holder; 2]]
        if flag {
            holders = [[ref holder; 2], [ref holder; 2]]
        } else {
            holders = [[ref holder; 2], [ref holder; 2]]
        }
        initialize(target: holders[outer][inner].target, input: input)
    }
}

contract DynamicEffectWriteSame {
    effect_write_same: u256

    init(index: usize, input: u256) uses (mut effect_write_same) {
        let holder = Handle { target: mut effect_write_same }
        let mut provider = HandleRefs {
            handles: [ref holder; 2],
        }
        with (mut provider) {
            initialize_effect_at(index: index, input: input)
        }
    }
}

contract DynamicEffectWriteDifferent {
    effect_write_left: u256
    effect_write_right: u256

    init(index: usize, input: u256)
        uses (mut effect_write_left, mut effect_write_right)
    {
        let left = Handle { target: mut effect_write_left }
        let right = Handle { target: mut effect_write_right }
        let mut provider = HandleRefs {
            handles: [ref left, ref right],
        }
        with (mut provider) {
            initialize_effect_at(index: index, input: input)
        }
    }
}

contract NestedEnumArrayBranches {
    nested_enum_array: u256

    init(flag: bool, index: usize, input: u256)
        uses (mut nested_enum_array)
    {
        let holder = Handle { target: mut nested_enum_array }
        let mut choice = HandleArrayChoice::Selected([ref holder; 2])
        if flag {
            choice = HandleArrayChoice::Selected([ref holder; 2])
        } else {
            choice = HandleArrayChoice::Selected([ref holder; 2])
        }
        match choice {
            HandleArrayChoice::Selected(handles) => {
                initialize(target: handles[index].target, input: input)
            }
            HandleArrayChoice::Other(_) => {}
        }
    }
}
"#,
    );

    for field in [
        "same_replacement",
        "sibling_replacement",
        "sibling_stable",
        "whole_final_first",
        "whole_final_second",
        "explicit_equal",
        "huge_repeat",
        "same_branch",
        "direct_noop",
        "whole_noop",
        "call_noop",
        "call_summary_noop",
        "helper_write_same",
        "nested_repeat",
        "effect_write_same",
        "nested_enum_array",
    ] {
        assert!(
            !diags.contains(&format!("`{field}` must be assigned")),
            "unexpected immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    for field in [
        "same_first",
        "same_second",
        "different_first",
        "different_second",
        "different_replacement",
        "constant_dynamic_first",
        "constant_dynamic_second",
        "constant_dynamic_replacement",
        "dynamic_constant_first",
        "dynamic_constant_second",
        "dynamic_constant_replacement",
        "dynamic_constant_final",
        "sibling_first",
        "sibling_second",
        "whole_old_first",
        "whole_old_second",
        "whole_stale_override",
        "different_branch_left",
        "different_branch_right",
        "helper_rebind_first",
        "helper_rebind_second",
        "helper_rebind_replacement",
        "helper_write_left",
        "helper_write_right",
        "effect_write_left",
        "effect_write_right",
    ] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        26,
        "{diags}",
    );
}

#[test]
fn returned_carrier_sources_require_unanimous_callsite_mapping() {
    let diags = all_diags(
        r#"
struct Handle {
    target: mut u256,
}

impl Copy for Handle {}

fn initialize(target: mut u256, input: u256) {
    target = input
}

fn select_branch(
    handles: [ref Handle; 2],
    flag: bool,
) -> mut u256 {
    if flag {
        handles[0].target
    } else {
        handles[1].target
    }
}

fn select_dynamic(
    handles: [ref Handle; 2],
    index: usize,
) -> mut u256 {
    handles[index].target
}

contract ReturnedBranchesSame {
    branch_same: u256

    init(flag: bool, input: u256) uses (mut branch_same) {
        let holder = Handle { target: mut branch_same }
        let handles = [ref holder, ref holder]
        let target = select_branch(handles: handles, flag: flag)
        initialize(target: target, input: input)
    }
}

contract ReturnedBranchesDifferent {
    branch_left: u256
    branch_right: u256

    init(flag: bool, input: u256)
        uses (mut branch_left, mut branch_right)
    {
        let left = Handle { target: mut branch_left }
        let right = Handle { target: mut branch_right }
        let handles = [ref left, ref right]
        let target = select_branch(handles: handles, flag: flag)
        initialize(target: target, input: input)
    }
}

contract ReturnedDynamicSame {
    dynamic_same: u256

    init(index: usize, input: u256) uses (mut dynamic_same) {
        let holder = Handle { target: mut dynamic_same }
        let handles = [ref holder; 2]
        let target = select_dynamic(handles: handles, index: index)
        initialize(target: target, input: input)
    }
}

contract ReturnedDynamicDifferent {
    dynamic_left: u256
    dynamic_right: u256

    init(index: usize, input: u256)
        uses (mut dynamic_left, mut dynamic_right)
    {
        let left = Handle { target: mut dynamic_left }
        let right = Handle { target: mut dynamic_right }
        let handles = [ref left, ref right]
        let target = select_dynamic(handles: handles, index: index)
        initialize(target: target, input: input)
    }
}
"#,
    );

    for field in ["branch_same", "dynamic_same"] {
        assert!(
            !diags.contains(&format!("`{field}` must be assigned")),
            "unexpected immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    for field in [
        "branch_left",
        "branch_right",
        "dynamic_left",
        "dynamic_right",
    ] {
        assert!(
            diags.contains(&format!("`{field}` must be assigned")),
            "missing immutable-field diagnostic for {field}:\n{diags}",
        );
    }
    assert_eq!(
        diags
            .matches("immutable contract field is not initialized")
            .count(),
        4,
        "{diags}",
    );
}
