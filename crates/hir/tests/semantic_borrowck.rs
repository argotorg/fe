use std::collections::VecDeque;

use cranelift_entity::EntityRef;
use fe_hir::test_db::{HirAnalysisTestDb, format_diagnostics};
use fe_hir::{
    analysis::{
        semantic::{
            BorrowInput, BorrowResult, BorrowTransform, FieldIndex, LayoutBackingProjection,
            NBorrowRoot, NExpr, NLocalOrigin, NSPlace, NSPlaceRoot, NSStmtKind,
            NormalizedBindingLowering, ReadMode, SStmtKind, SemanticBorrowDiagKind,
            SemanticInstance, SemanticInstanceCompleteness, SemanticLocalKind,
            check_semantic_borrows, check_semantic_noesc,
            collect_semantic_borrow_diagnostic_vouchers, get_or_build_semantic_instance,
            identity_semantic_instance_key, normalize_semantic_body,
            normalize_semantic_body_for_layout_evidence, semantic_borrow_summary,
        },
        ty::{
            ProviderAddressSpace,
            ty_check::{BodyOwner, LocalBinding},
            ty_def::{BorrowKind, TyData},
        },
    },
    hir_def::{ItemKind, Partial},
    projection::{IndexSource, Projection},
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

fn analysis_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("semantic_borrowck.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    format_diagnostics(&db, &db.run_on_top_mod(top_mod))
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

#[test]
fn closure_returned_capture_preserves_borrow_provenance() {
    let src = r#"
fn probe() -> u256 {
    let mut x = 0
    let borrowed = mut x
    let pass = |_ unit: own ()| -> mut u256 { borrowed }
    let returned = pass.call(())
    let other = mut x
    other = 1
    returned
}
"#;
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
    assert!(diags.contains("cannot mutably borrow"), "{diags}");
}

#[test]
fn closure_returned_capture_does_not_retain_unrelated_capture_loans() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut x = 0
    let mut y = 0
    let borrowed_x = mut x
    let borrowed_y = mut y
    let pass = |_ unit: own ()| -> mut u256 {
        borrowed_y = 1
        borrowed_x
    }
    let returned = pass.call(())
    let other_y = mut y
    other_y = 2
    returned = 3
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_argument_return_does_not_borrow_unrelated_noncopy_environment() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let captured = Boxed { value: 1 }
    let value = 41
    let project = |argument: ref u256| -> ref u256 {
        captured.value
        argument
    }
    let returned = project.call(ref value)
    let moved = project
    returned + moved.call(ref value) - 40
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn generic_higher_order_call_preserves_closure_parameter_borrow_provenance() {
    let src = r#"
use core::functional::Fn

fn invoke<T, F: Fn<(mut T,), mut T>>(
    _ function: F,
    _ value: mut T,
) -> mut T {
    function.call(value)
}

fn probe() {
    let mut value: u256 = 0
    let project = |target: mut u256| -> mut u256 { target }
    let returned = invoke(project, mut value)
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn generic_higher_order_call_preserves_closure_capture_borrow_provenance() {
    let src = r#"
use core::functional::Fn

fn invoke<F: Fn<((),), mut u256>>(_ function: F) -> mut u256 {
    function.call(())
}

fn probe() {
    let mut value = 0
    let captured = mut value
    let project = |_ unit: own ()| -> mut u256 { captured }
    let returned = invoke(project)
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn generic_higher_order_return_borrows_the_reusable_closure_environment() {
    let src = r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn invoke<F: Fn<(), ref u256>>(_ function: F) -> ref u256 {
    function.call()
}

fn probe() -> u256 {
    let boxed = Boxed { value: 42 }
    let read = || -> ref u256 { ref boxed.value }
    let returned = invoke(read)
    let moved = read
    returned
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(
        diags.contains("cannot move out of a value while it is borrowed"),
        "{diags}"
    );
}

#[test]
fn generic_higher_order_call_rejects_aliasing_between_two_closure_environments() {
    let src = r#"
use core::functional::Fn

fn borrow_both<F: Fn<(), mut u256>, G: Fn<(), mut u256>>(
    _ first: F,
    _ second: G,
) {
    let left = first.call()
    let right = second.call()
    left = 1
    right = 2
}

fn probe() {
    let mut value = 0
    let captured = mut value
    let first = || -> mut u256 { captured }
    let second = || -> mut u256 { captured }
    borrow_both(first, second)
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn generic_fn_once_call_preserves_consumed_closure_capture_borrow_provenance() {
    let src = r#"
use core::functional::FnOnce

struct Boxed {
    value: u256,
}

fn consume(_ value: own Boxed) {}

fn invoke<F: FnOnce<(), mut u256>>(_ function: own F) -> mut u256 {
    function.call_once()
}

fn probe() {
    let mut value = 0
    let captured = mut value
    let consumed = Boxed { value: 42 }
    let project = || -> mut u256 {
        consume(consumed)
        captured
    }
    let returned = invoke(project)
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn generic_higher_order_call_preserves_wrapped_closure_capture_provenance() {
    let src = r#"
use core::functional::Fn

struct Wrapped<F> {
    function: F,
}

fn invoke<F: Fn<((),), mut u256>>(_ wrapped: own Wrapped<F>) -> mut u256 {
    wrapped.function.call(())
}

fn probe() {
    let mut value = 0
    let captured = mut value
    let project = |_ unit: own ()| -> mut u256 { captured }
    let returned = invoke(Wrapped { function: project })
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn generic_higher_order_call_preserves_dynamically_projected_closure_provenance() {
    let src = r#"
use core::functional::Fn

fn invoke<F: Fn<((),), mut u256>>(
    _ functions: own [F; 2],
    _ index: usize,
) -> mut u256 {
    functions[index].call(())
}

fn probe(_ index: usize) {
    let mut value = 0
    let captured = mut value
    let project = |_ unit: own ()| -> mut u256 { captured }
    let returned = invoke([project, project], index)
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn generic_higher_order_call_rejects_aliasing_closure_capture_and_argument() {
    let src = r#"
use core::functional::Fn

fn invoke<F: Fn<(mut u256,), ()>>(_ function: F, _ target: mut u256) {
    function.call(target)
}

fn probe() {
    let mut value = 0
    let captured = mut value
    let write = |target: mut u256| {
        captured = 1
        target = 2
    }
    invoke(write, mut value)
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn recursive_higher_order_summary_preserves_closure_return_provenance() {
    let src = r#"
use core::functional::Fn

fn forward<F: Fn<(mut u256,), mut u256>>(
    _ function: F,
    _ target: mut u256,
    _ depth: u256,
) -> mut u256 {
    if depth == 0 {
        function.call(target)
    } else {
        forward(function, target, depth - 1)
    }
}

fn probe(_ depth: u256) {
    let mut value = 0
    let project = |target: mut u256| -> mut u256 { target }
    let returned = forward(project, mut value, depth)
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn recursive_higher_order_summary_converges_through_parameter_permutation() {
    let src = r#"
use core::functional::Fn

fn alternate<F: Fn<(mut u256,), mut u256>>(
    _ function: F,
    _ left: mut u256,
    _ right: mut u256,
    _ depth: u256,
) -> mut u256 {
    if depth == 0 {
        function.call(left)
    } else {
        alternate(function, right, left, depth - 1)
    }
}

fn probe(_ depth: u256) {
    let mut left = 0
    let mut right = 0
    let project = |target: mut u256| -> mut u256 { target }
    let returned = alternate(project, mut left, mut right, depth)
    let other = mut right
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn recursive_closure_forwarding_preserves_capture_provenance() {
    let src = r#"
use core::functional::Fn

fn forward<F: Fn<((),), mut u256>>(
    _ function: own F,
    _ depth: u256,
) -> F {
    if depth == 0 {
        function
    } else {
        forward(function, depth - 1)
    }
}

fn probe(_ depth: u256) {
    let mut value = 0
    let captured = mut value
    let project = |_ unit: own ()| -> mut u256 { captured }
    let forwarded = forward(project, depth)
    let returned = forwarded.call(())
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn mutually_recursive_direct_borrow_summaries_converge() {
    let src = r#"
fn forward_left(_ target: mut u256, _ recurse: bool) -> mut u256 {
    if recurse {
        forward_right(target)
    } else {
        target
    }
}

fn forward_right(_ target: mut u256) -> mut u256 {
    forward_left(target, recurse: false)
}

fn probe(_ recurse: bool) {
    let mut value = 0
    let returned = forward_left(mut value, recurse)
    let other = mut value
    other = 1
    returned = 2
}
"#;
    let analysis = analysis_diags(src);
    assert!(analysis.is_empty(), "{analysis}");
    let diags = borrow_diags(src);
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn repeated_closure_calls_cannot_duplicate_returned_mut_capture() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut x = 0
    let borrowed = mut x
    let pass = |_ unit: own ()| -> mut u256 { borrowed }
    let first = pass.call(())
    let second = pass.call(())
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn stored_deferred_closure_cannot_duplicate_returned_mut_parameter() {
    let diags = borrow_diags(
        r#"
struct Holder<F> {
    function: F,
}

fn probe() {
    let project = |target: mut| -> mut u256 { target }
    let holder = Holder { function: project }
    let mut value: u256 = 0
    let first = holder.function.call(mut value)
    let second = holder.function.call(mut value)
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn stored_deferred_closure_preserves_owned_parameter_move() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Holder<F> {
    function: F,
}

fn probe(_ boxed: own Boxed) {
    let take = |value: own| value.value
    let holder = Holder { function: take }
    let result = holder.function.call_once(boxed)
    let reused = boxed.value
}
"#,
    );
    assert!(
        diags.contains("cannot use a value after it was moved"),
        "{diags}"
    );
}

#[test]
fn stored_closure_receiver_and_mut_argument_allow_disjoint_fields() {
    let diags = borrow_diags(
        r#"
struct Holder<F> {
    function: F,
    value: u256,
}

fn probe() -> u256 {
    let write = |target: mut u256| {
        target = 42
    }
    let mut holder = Holder { function: write, value: 0 }
    holder.function.call(mut holder.value)
    holder.value
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn stored_mut_enum_pattern_projection_can_be_reborrowed() {
    let diags = borrow_diags(
        r#"
struct Holder {
    value: mut Option<usize>,
}

fn update(value: mut Option<Option<usize>>) {
    if let Option::Some(mut inner) = value {
        let holder = Holder { value: mut inner }
        if let Option::Some(mut payload) = holder.value {
            payload = 42
        }
    }
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn stored_view_enum_pattern_projection_can_be_reborrowed() {
    let diags = borrow_diags(
        r#"
struct Holder {
    value: view Option<usize>,
}

fn nested(value: Option<Option<usize>>) {
    if let Option::Some(inner) = value {
        let holder = Holder { value: ref inner }
        if let Option::Some(payload) = holder.value {
            let result = payload
        }
    }
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn mutable_pattern_alias_reassignment_conflicts_with_live_shared_borrow() {
    let diags = borrow_diags(
        r#"
fn probe(value: mut Option<u256>) -> u256 {
    if let Option::Some(mut payload) = value {
        let borrowed = ref payload
        payload = 42
        borrowed
    } else {
        0
    }
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn stored_mut_handle_pattern_through_ref_container_retains_pointee_loan() {
    let diags = borrow_diags(
        r#"
struct Handles {
    value: mut u256,
}

fn probe() {
    let mut value = 0
    let handles = Handles { value: mut value }
    let Handles { value: projected } = ref handles
    projected = 42
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_can_return_stored_mut_handle_through_ref_container() {
    let diags = borrow_diags(
        r#"
struct Handles {
    value: mut u256,
}

fn probe() {
    let mut value = 0
    let handles = Handles { value: mut value }
    let borrowed = ref handles
    let project = || -> mut u256 {
        let Handles { value: projected } = borrowed
        projected
    }
    let first = project.call()
    first = 1
    let second = project.call()
    second = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn overlapping_closure_returns_from_stored_mut_handle_conflict() {
    let diags = borrow_diags(
        r#"
struct Handles {
    value: mut u256,
}

fn probe() {
    let mut value = 0
    let handles = Handles { value: mut value }
    let borrowed = ref handles
    let project = || -> mut u256 {
        let Handles { value: projected } = borrowed
        projected
    }
    let first = project.call()
    let second = project.call()
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_can_return_dynamically_indexed_stored_mut_handle_through_ref_array() {
    let diags = borrow_diags(
        r#"
fn probe(index: usize) {
    let mut x = 0
    let mut y = 0
    let handles = [mut x, mut y]
    let borrowed = ref handles
    let project = |index: own usize| -> mut u256 { borrowed[index] }

    let first = project.call(index)
    first = 1
    let second = project.call(index)
    second = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn dynamically_indexed_stored_mut_closure_summary_retains_capture_path() {
    let src = r#"
fn probe(index: usize) {
    let mut x = 0
    let mut y = 0
    let handles = [mut x, mut y]
    let borrowed = ref handles
    let project = |index: own usize| -> mut u256 { borrowed[index] }
    let returned = project.call(index)
    returned = 1
}
"#;
    let mut summary = None;
    for_each_fixture_instance(src, |db, instance| {
        if matches!(instance.key(db).owner(db), BodyOwner::Closure { .. }) {
            summary = semantic_borrow_summary(db, instance)
                .expect("closure borrow summary")
                .or(summary.take());
        }
    });
    assert_eq!(
        summary,
        Some(vec![BorrowTransform {
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: Vec::new(),
            },
            input: BorrowInput::Place {
                param: 0,
                projection: vec![
                    LayoutBackingProjection::Field(FieldIndex(0)),
                    LayoutBackingProjection::Index(None),
                ],
            },
        }])
    );
}

#[test]
fn disjoint_stored_mut_fields_returned_by_closures_do_not_conflict() {
    let diags = borrow_diags(
        r#"
struct Handles {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let handles = Handles {
        left: mut left,
        right: mut right,
    }
    let borrowed = ref handles
    let project_left = || -> mut u256 {
        let Handles { left, right: _ } = borrowed
        left
    }
    let project_right = || -> mut u256 {
        let Handles { left: _, right } = borrowed
        right
    }

    let returned_left = project_left.call()
    let returned_right = project_right.call()
    returned_left = 1
    returned_right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn same_stored_mut_field_returned_by_distinct_closures_conflicts() {
    let diags = borrow_diags(
        r#"
struct Handles {
    value: mut u256,
}

fn probe() {
    let mut value = 0
    let handles = Handles { value: mut value }
    let first_ref = ref handles
    let second_ref = ref handles
    let first_project = || -> mut u256 {
        let Handles { value } = first_ref
        value
    }
    let second_project = || -> mut u256 {
        let Handles { value } = second_ref
        value
    }

    let first = first_project.call()
    let second = second_project.call()
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn overlapping_dynamic_stored_mut_closure_returns_conflict() {
    let diags = borrow_diags(
        r#"
fn probe(first_index: usize, second_index: usize) {
    let mut x = 0
    let mut y = 0
    let handles = [mut x, mut y]
    let borrowed = ref handles
    let project = |index: own usize| -> mut u256 { borrowed[index] }

    let first = project.call(first_index)
    let second = project.call(second_index)
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn copied_closures_cannot_duplicate_returned_mut_capture() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let project = || -> mut u256 { captured }
    let copied = project

    let first = project.call()
    let second = copied.call()
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn fn_once_calls_on_copyable_reusable_closures_retain_capture_provenance() {
    let direct = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let project = || -> mut u256 { captured }
    let first = project.call_once()
    let second = project.call()
    first = 1
    second = 2
}
"#,
    );
    assert!(direct.contains("borrow conflict in `fn probe`"), "{direct}");

    let generic = borrow_diags(
        r#"
use core::functional::Fn

fn invoke_once<F: Fn<(), mut u256>>(_ function: own F) -> mut u256 {
    function.call_once()
}

fn probe() {
    let mut value = 0
    let captured = mut value
    let project = || -> mut u256 { captured }
    let first = invoke_once(project)
    let second = project.call()
    first = 1
    second = 2
}
"#,
    );
    assert!(
        generic.contains("borrow conflict in `fn probe`"),
        "{generic}"
    );
}

#[test]
fn fn_once_on_noncopy_reusable_closure_owns_the_returned_borrow_storage() {
    let allowed = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let boxed = Boxed { value: 42 }
    let project = || -> ref u256 { ref boxed.value }
    let returned = project.call_once()
    returned
}
"#,
    );
    assert!(allowed.is_empty(), "{allowed}");

    let reused = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let boxed = Boxed { value: 42 }
    let project = || -> ref u256 { ref boxed.value }
    let returned = project.call_once()
    project.call() + returned
}
"#,
    );
    assert!(
        reused.contains("cannot use a value after it was moved"),
        "{reused}"
    );
}

#[test]
fn distinct_closures_cannot_duplicate_the_same_returned_mut_capture() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let first_project = || -> mut u256 { captured }
    let second_project = || -> mut u256 { captured }

    let first = first_project.call()
    let second = second_project.call()
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn copied_closure_mut_returns_are_reusable_after_last_use() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let project = || -> mut u256 { captured }
    let copied = project

    let first = project.call()
    first = 1
    let second = copied.call()
    second = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_projected_from_ref_borrowed_tuple_retains_mut_capture_source() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let project = || -> mut u256 { captured }
    let wrapped = (project, 0 as u256)
    let (projected, _) = ref wrapped

    let first = projected.call()
    first = 1
    let second = projected.call()
    second = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_projected_from_ref_borrowed_enum_retains_mut_capture_source() {
    let diags = borrow_diags(
        r#"
use core::option::Option::{self, Some}

fn probe() {
    let mut value = 0
    let captured = mut value
    let project = || -> mut u256 { captured }
    let wrapped = Some(project)
    let Option::Some(projected) = ref wrapped

    let first = projected.call()
    first = 1
    let second = projected.call()
    second = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_closure_instances_keep_disjoint_mut_capture_sources() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let make = |target: mut u256| {
        let project = || -> mut u256 { target }
        project
    }
    let project_left = make.call(mut left)
    let project_right = make.call(mut right)

    let returned_left = project_left.call()
    let returned_right = project_right.call()
    returned_left = 1
    returned_right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn dynamic_projection_of_distinct_returned_closure_instances_is_conservative() {
    let diags = borrow_diags(
        r#"
fn probe(first_index: usize, second_index: usize) {
    let mut left = 0
    let mut right = 0
    let make = |target: mut u256| {
        let project = || -> mut u256 { target }
        project
    }
    let project_left = make.call(mut left)
    let project_right = make.call(mut right)
    let functions = [project_left, project_right]

    let first = functions[first_index].call()
    let second = functions[second_index].call()
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_pattern_mut_return_retains_capture_source() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let mut wrapped: Option<Boxed> = Option::Some(Boxed { value: 0 })
    let handle = mut wrapped
    let project = || -> mut Boxed {
        match handle {
            Option::Some(boxed) => boxed,
            Option::None => core::panic_with_value(0 as u256),
        }
    }
    let first = project.call()
    first.value = 1
    let second = project.call()
    second.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn repeated_closure_pattern_mut_returns_conflict() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let mut wrapped: Option<Boxed> = Option::Some(Boxed { value: 0 })
    let handle = mut wrapped
    let project = || -> mut Boxed {
        match handle {
            Option::Some(boxed) => boxed,
            Option::None => core::panic_with_value(0 as u256),
        }
    }
    let first = project.call()
    let second = project.call()
    first.value = 1
    second.value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_pattern_mut_return_keeps_environment_source_borrowed() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let mut wrapped: Option<Boxed> = Option::Some(Boxed { value: 0 })
    let handle = mut wrapped
    let project = || -> mut Boxed {
        match handle {
            Option::Some(boxed) => boxed,
            Option::None => core::panic_with_value(0 as u256),
        }
    }
    let returned = project.call()
    let moved = wrapped
    returned.value = 1
}
"#,
    );
    assert!(
        diags.contains("cannot move out of a value while it is borrowed"),
        "{diags}"
    );
}

#[test]
fn returned_ref_capture_remains_live_in_the_caller() {
    let diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let mut x = 0
    let borrowed = ref x
    let pass = |_ unit: own ()| -> ref u256 { borrowed }
    let returned = pass.call(())
    let other = mut x
    other = 1
    returned
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn repeated_closure_calls_may_duplicate_returned_ref_capture() {
    let diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let x = 1
    let borrowed = ref x
    let pass = |_ unit: own ()| -> ref u256 { borrowed }
    let first = pass.call(())
    let second = pass.call(())
    first + second
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn repeated_function_calls_cannot_duplicate_returned_mut_borrow() {
    let diags = borrow_diags(
        r#"
fn pass(_ value: mut u256) -> mut u256 {
    value
}

fn probe() {
    let mut x = 0
    let borrowed = mut x
    let first = pass(borrowed)
    let second = pass(borrowed)
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn returned_mut_reborrow_releases_to_its_parent_after_last_use() {
    let diags = borrow_diags(
        r#"
fn pass(_ value: mut u256) -> mut u256 {
    value
}

fn probe() {
    let mut x = 0
    let borrowed = mut x
    let first = pass(borrowed)
    first = 1
    let second = pass(borrowed)
    second = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn opaque_trait_call_cannot_duplicate_returned_mut_borrow() {
    let diags = borrow_diags(
        r#"
trait ValueMut {
    fn value_mut(mut self) -> mut u256
}

fn probe<T: ValueMut>(mut value: T) {
    let first = value.value_mut()
    let second = value.value_mut()
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn opaque_mut_borrow_result_does_not_alias_immutable_scalar_args() {
    let diags = borrow_diags(
        r#"
trait ValueMut {
    fn value_mut(mut self, flag: bool) -> mut u256
}

fn probe<T: ValueMut>(mut value: T, mut flag: bool) {
    let returned = value.value_mut(flag)
    let other = mut flag
    other = true
    returned = 1
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn opaque_trait_call_tracks_borrows_nested_in_aggregate_results() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

trait BorrowValue {
    fn borrow_value(mut self) -> Borrowed
}

fn probe<T: BorrowValue>(mut value: T) {
    let first = value.borrow_value()
    let second = value.borrow_value()
    first.value = 1
    second.value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn conservative_array_summary_uses_disjoint_nested_family_axes() {
    let src = r#"
trait Permute {
    fn permute(
        self,
        values: own [[mut u256; 2]; 2],
        other: own [mut u256; 2],
    ) -> [[mut u256; 2]; 2]
}

fn instantiate<T: Permute>(
    permuter: T,
    values: own [[mut u256; 2]; 2],
    other: own [mut u256; 2],
) -> [[mut u256; 2]; 2] {
    permuter.permute(values, other)
}
"#;
    let mut summary = None;
    for_each_fixture_instance(src, |db, instance| {
        if owner_name(db, instance.key(db).owner(db)) == "permute" {
            summary = semantic_borrow_summary(db, instance)
                .expect("opaque trait borrow summary")
                .or(summary.take());
        }
    });
    let summary = summary.expect("missing opaque trait borrow summary");
    let result = summary
        .iter()
        .find_map(|transform| match transform.result.projection.as_slice() {
            [
                LayoutBackingProjection::IndexFamily(outer),
                LayoutBackingProjection::IndexFamily(inner),
            ] => Some((*outer, *inner)),
            _ => None,
        })
        .expect("missing nested result family");
    let input = summary
        .iter()
        .find_map(|transform| match &transform.input {
            BorrowInput::Place {
                param: 1,
                projection,
            } => match projection.as_slice() {
                [
                    LayoutBackingProjection::IndexFamily(outer),
                    LayoutBackingProjection::IndexFamily(inner),
                ] => Some((*outer, *inner)),
                _ => None,
            },
            _ => None,
        })
        .expect("missing nested input family");
    let other = summary
        .iter()
        .find_map(|transform| match &transform.input {
            BorrowInput::Place {
                param: 2,
                projection,
            } => match projection.as_slice() {
                [LayoutBackingProjection::IndexFamily(family)] => Some(*family),
                _ => None,
            },
            _ => None,
        })
        .expect("missing second input family");
    let families = [result.0, result.1, input.0, input.1, other]
        .into_iter()
        .collect::<rustc_hash::FxHashSet<_>>();
    assert_eq!(families.len(), 5, "{summary:#?}");
}

#[test]
fn opaque_array_result_does_not_assume_pointwise_family_correlation() {
    let diags = borrow_diags(
        r#"
trait Permute {
    fn permute(
        self,
        values: own [[mut u256; 2]; 2],
        other: own [mut u256; 2],
    ) -> [[mut u256; 2]; 2]
}

fn nested_input_can_be_permuted<T: Permute>(permuter: T) {
    let mut a = 0
    let mut b = 0
    let mut c = 0
    let mut d = 0
    let mut e = 0
    let mut f = 0
    let returned = permuter.permute(
        [[mut a, mut b], [mut c, mut d]],
        [mut e, mut f],
    )
    let selected = returned[0][0]
    let alias = mut d
    alias = 1
    selected = 2
}

fn separate_input_can_supply_any_result<T: Permute>(permuter: T) {
    let mut a = 0
    let mut b = 0
    let mut c = 0
    let mut d = 0
    let mut e = 0
    let mut f = 0
    let returned = permuter.permute(
        [[mut a, mut b], [mut c, mut d]],
        [mut e, mut f],
    )
    let selected = returned[0][0]
    let alias = mut f
    alias = 1
    selected = 2
}
"#,
    );
    for name in [
        "nested_input_can_be_permuted",
        "separate_input_can_supply_any_result",
    ] {
        assert!(
            diags.contains(&format!("borrow conflict in `fn {name}`")),
            "{diags}"
        );
    }
}

#[test]
fn returned_aggregate_tracks_fresh_mut_borrow_from_view_param() {
    let diags = borrow_diags(
        r#"
struct Owner {
    value: u256,
}

struct Borrowed {
    value: mut u256,
}

fn borrow_value(mut owner: Owner) -> Borrowed {
    Borrowed { value: mut owner.value }
}

fn probe() {
    let mut owner = Owner { value: 0 }
    let borrowed = borrow_value(owner)
    let other = mut owner.value
    other = 1
    borrowed.value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn fresh_borrow_of_plain_field_in_borrow_holding_aggregate_is_tracked() {
    let diags = borrow_diags(
        r#"
struct Mixed {
    owner: u256,
    held: mut u256,
}

fn borrow_owner(mut mixed: Mixed) -> mut u256 {
    mut mixed.owner
}

fn probe() {
    let mut held_owner = 0
    let mixed = Mixed { owner: 0, held: mut held_owner }
    let borrowed = borrow_owner(mixed)
    let other = mut mixed.owner
    other = 1
    borrowed = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn fresh_borrow_of_whole_borrow_holding_aggregate_tracks_its_storage() {
    let diags = borrow_diags(
        r#"
struct Mixed {
    owner: u256,
    held: mut u256,
}

fn borrow_mixed(mut mixed: Mixed) -> mut Mixed {
    mut mixed
}

fn probe() {
    let mut held_owner = 0
    let mixed = Mixed { owner: 0, held: mut held_owner }
    let borrowed = borrow_mixed(mixed)
    let other = mut mixed.owner
    other = 1
    borrowed.owner = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn reading_one_aggregate_borrow_field_does_not_retain_sibling_borrows() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut x = 0
    let mut y = 0
    let pair = Pair { left: mut x, right: mut y }
    let left = pair.left
    let other_y = mut y
    other_y = 1
    left = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn overwriting_aggregate_field_releases_its_old_held_borrows() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

struct Wrapper {
    borrowed: Borrowed,
}

fn probe() {
    let mut x = 0
    let mut y = 0
    let mut wrapper = Wrapper { borrowed: Borrowed { value: mut x } }
    wrapper.borrowed = Borrowed { value: mut y }
    let other_x = mut x
    other_x = 1
    wrapper.borrowed.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn writing_through_borrow_field_keeps_its_loan_active() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn probe() {
    let mut x = 0
    let borrowed = Borrowed { value: mut x }
    borrowed.value = 1
    let other = mut x
    other = 2
    borrowed.value = 3
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn returned_enum_tracks_only_the_active_borrow_variant() {
    let diags = borrow_diags(
        r#"
enum Choice {
    Left(mut u256),
    Right(mut u256),
}

fn borrow_left(mut value: u256) -> Choice {
    Choice::Left(mut value)
}

fn probe() {
    let mut x = 0
    let choice = borrow_left(x)
    let other = mut x
    other = 1
    match choice {
        Choice::Left(value) => value = 2,
        Choice::Right(value) => value = 3,
    }
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
    assert!(!diags.contains("internal borrow checker error"), "{diags}");
}

#[test]
fn nested_inactive_enum_borrow_variant_does_not_require_a_source() {
    let diags = borrow_diags(
        r#"
enum Choice {
    Empty,
    Borrowed(mut u256),
}

struct Wrapper {
    choice: Choice,
}

fn empty() -> Wrapper {
    Wrapper { choice: Choice::Empty }
}

fn probe() {
    let wrapper = empty()
    match wrapper.choice {
        Choice::Empty => (),
        Choice::Borrowed(value) => value = 1,
    }
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn recursive_aggregate_borrow_summary_converges() {
    let diags = borrow_diags(
        r#"
struct Owner {
    value: u256,
}

struct Borrowed {
    value: mut u256,
}

fn borrow_value(mut owner: Owner, recurse: bool) -> Borrowed {
    if recurse {
        borrow_value(owner, recurse: false)
    } else {
        Borrowed { value: mut owner.value }
    }
}

fn probe() {
    let mut owner = Owner { value: 0 }
    let borrowed = borrow_value(owner, recurse: true)
    let other = mut owner.value
    other = 1
    borrowed.value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn mutually_recursive_aggregate_borrow_summaries_converge() {
    let diags = borrow_diags(
        r#"
struct Owner {
    value: u256,
}

struct Borrowed {
    value: mut u256,
}

fn borrow_left(mut owner: Owner, recurse: bool) -> Borrowed {
    if recurse {
        borrow_right(owner)
    } else {
        Borrowed { value: mut owner.value }
    }
}

fn borrow_right(mut owner: Owner) -> Borrowed {
    borrow_left(owner, recurse: false)
}

fn probe() {
    let mut owner = Owner { value: 0 }
    let borrowed = borrow_left(owner, recurse: true)
    let other = mut owner.value
    other = 1
    borrowed.value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn reading_one_array_borrow_element_does_not_retain_siblings() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut x = 0
    let mut y = 0
    let values = [mut x, mut y]
    let first = values[0]
    let other_y = mut y
    other_y = 1
    first = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn large_fixed_array_borrow_summary_uses_one_symbolic_slot() {
    let src = r#"
fn forward(values: own [mut u256; 1000000]) -> [mut u256; 1000000] {
    values
}
"#;
    let mut summary = None;
    for_each_fixture_instance(src, |db, instance| {
        let BodyOwner::Func(func) = instance.key(db).owner(db) else {
            return;
        };
        if func
            .name(db)
            .to_opt()
            .is_none_or(|name| name.data(db) != "forward")
        {
            return;
        }
        summary = semantic_borrow_summary(db, instance)
            .expect("large-array borrow summary")
            .or(summary.take());
    });
    assert_eq!(
        summary,
        Some(vec![BorrowTransform {
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: vec![LayoutBackingProjection::IndexFamily(0)],
            },
            input: BorrowInput::Place {
                param: 0,
                projection: vec![LayoutBackingProjection::IndexFamily(0)],
            },
        }])
    );
}

#[test]
fn zero_length_array_axes_do_not_create_phantom_borrow_families() {
    let src = r#"
fn empty(values: own [mut u256; 0]) -> [mut u256; 0] {
    values
}

fn nested_inner_empty(
    values: own [[mut u256; 0]; 2],
) -> [[mut u256; 0]; 2] {
    values
}

fn nested_outer_empty(
    values: own [[mut u256; 2]; 0],
) -> [[mut u256; 2]; 0] {
    values
}

struct EmptyThenBorrowing {
    empty: [[mut u256; 0]; 2],
    values: [mut u256; 2],
}

fn mixed(values: own EmptyThenBorrowing) -> EmptyThenBorrowing {
    values
}
"#;
    assert!(analysis_diags(src).is_empty());
    let mut seen = rustc_hash::FxHashSet::default();
    let mut mixed_summary = None;
    for_each_fixture_instance(src, |db, instance| {
        let name = owner_name(db, instance.key(db).owner(db));
        if matches!(
            name.as_str(),
            "empty" | "nested_inner_empty" | "nested_outer_empty"
        ) {
            seen.insert(name);
            assert_eq!(
                semantic_borrow_summary(db, instance).expect("zero-array borrow summary"),
                None
            );
        } else if name == "mixed" {
            mixed_summary = semantic_borrow_summary(db, instance)
                .expect("mixed zero-array borrow summary")
                .or(mixed_summary.take());
        }
    });
    assert_eq!(seen.len(), 3, "{seen:?}");
    assert_eq!(
        mixed_summary,
        Some(vec![BorrowTransform {
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: vec![
                    LayoutBackingProjection::Field(FieldIndex(1)),
                    LayoutBackingProjection::IndexFamily(0),
                ],
            },
            input: BorrowInput::Place {
                param: 0,
                projection: vec![
                    LayoutBackingProjection::Field(FieldIndex(1)),
                    LayoutBackingProjection::IndexFamily(0),
                ],
            },
        }])
    );
}

#[test]
fn returned_array_family_keeps_constant_siblings_disjoint() {
    let diags = borrow_diags(
        r#"
fn forward(values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn forward_twice(values: own [mut u256; 2]) -> [mut u256; 2] {
    forward(values)
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let returned = forward_twice([mut left, mut right])
    let first = returned[0]
    let other = mut right
    other = 1
    first = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_array_family_dynamic_index_overlaps_every_sibling() {
    let diags = borrow_diags(
        r#"
fn forward(values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn probe(index: usize) {
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
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn returned_array_literal_materializes_only_its_exact_slots() {
    let src = r#"
fn pair(left: mut u256, right: mut u256) -> [mut u256; 2] {
    [left, right]
}
"#;
    let mut summary = None;
    for_each_fixture_instance(src, |db, instance| {
        let BodyOwner::Func(func) = instance.key(db).owner(db) else {
            return;
        };
        if func
            .name(db)
            .to_opt()
            .is_none_or(|name| name.data(db) != "pair")
        {
            return;
        }
        summary = semantic_borrow_summary(db, instance)
            .expect("array-literal borrow summary")
            .or(summary.take());
    });
    assert_eq!(summary.as_ref().map(Vec::len), Some(2), "{summary:#?}");
    let summary = summary.expect("pair summary");
    assert!(summary.iter().any(|transform| {
        transform.result.projection == [LayoutBackingProjection::Index(Some(0))]
            && transform.input
                == BorrowInput::Place {
                    param: 0,
                    projection: Vec::new(),
                }
    }));
    assert!(summary.iter().any(|transform| {
        transform.result.projection == [LayoutBackingProjection::Index(Some(1))]
            && transform.input
                == BorrowInput::Place {
                    param: 1,
                    projection: Vec::new(),
                }
    }));
}

#[test]
fn closure_returned_array_literal_materializes_only_its_exact_slots() {
    let src = r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let pair = |left: mut u256, right: mut u256| -> [mut u256; 2] {
        [left, right]
    }
    let returned = pair.call(mut left, mut right)
    returned[0] = 1
    returned[1] = 2
}
"#;
    let diags = borrow_diags(src);
    assert!(diags.is_empty(), "{diags}");

    let mut summaries = Vec::new();
    for_each_fixture_instance(src, |db, instance| {
        if !matches!(instance.key(db).owner(db), BodyOwner::Closure { .. }) {
            return;
        }
        if let Some(summary) =
            semantic_borrow_summary(db, instance).expect("closure array-literal borrow summary")
        {
            summaries.push(summary);
        }
    });
    assert!(!summaries.is_empty(), "missing closure borrow summary");
    let expected = vec![
        BorrowTransform {
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: vec![LayoutBackingProjection::Index(Some(0))],
            },
            input: BorrowInput::Place {
                param: 1,
                projection: vec![LayoutBackingProjection::Field(FieldIndex(0))],
            },
        },
        BorrowTransform {
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: vec![LayoutBackingProjection::Index(Some(1))],
            },
            input: BorrowInput::Place {
                param: 1,
                projection: vec![LayoutBackingProjection::Field(FieldIndex(1))],
            },
        },
    ];
    for summary in summaries {
        assert_eq!(summary, expected);
    }
}

#[test]
fn returned_array_sparse_override_retains_the_symbolic_remainder() {
    let src = r#"
fn replace_first(
    mut values: own [mut u256; 2],
    replacement: mut u256,
) -> [mut u256; 2] {
    values[0] = mut replacement
    values
}

fn maybe_replace_first(
    mut values: own [mut u256; 2],
    replacement: mut u256,
    replace: bool,
) -> [mut u256; 2] {
    if replace {
        values[0] = mut replacement
    }
    values
}

fn releases_replaced() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_first(
        [mut old_left, mut right],
        mut replacement,
    )
    let released = mut old_left
    released = 1
    returned[0] = 2
    returned[1] = 3
}

fn retains_sibling() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_first(
        [mut old_left, mut right],
        mut replacement,
    )
    let alias = mut right
    alias = 1
    returned[1] = 2
}

fn conditional_replacement_retains_old(replace: bool) {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = maybe_replace_first(
        [mut old_left, mut right],
        mut replacement,
        replace,
    )
    let alias = mut old_left
    alias = 1
    returned[0] = 2
}
"#;
    let diags = borrow_diags(src);
    assert!(
        !diags.contains("borrow conflict in `fn releases_replaced`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn retains_sibling`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn conditional_replacement_retains_old`"),
        "{diags}"
    );
}

#[test]
fn sparse_array_override_forwarded_through_family_summary_keeps_untouched_siblings() {
    let src = r#"
fn forward(values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn replace_then_forward(
    mut values: own [mut u256; 2],
    replacement: mut u256,
) -> [mut u256; 2] {
    values[0] = mut replacement
    forward(values)
}

fn releases_replaced_through_forward() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_then_forward(
        [mut old_left, mut right],
        mut replacement,
    )
    let released = mut old_left
    released = 1
    returned[0] = 2
    returned[1] = 3
}

fn retains_sibling_through_forward() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_then_forward(
        [mut old_left, mut right],
        mut replacement,
    )
    let alias = mut right
    alias = 1
    returned[1] = 2
}
"#;
    let diags = borrow_diags(src);
    assert!(
        !diags.contains("borrow conflict in `fn releases_replaced_through_forward`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn retains_sibling_through_forward`"),
        "{diags}"
    );
}

#[test]
fn nested_sparse_array_override_partitions_only_the_exact_member() {
    let diags = borrow_diags(
        r#"
fn replace_nested(
    mut values: own [[mut u256; 2]; 2],
    replacement: mut u256,
) -> [[mut u256; 2]; 2] {
    values[0][1] = mut replacement
    values
}

fn releases_replaced_nested_member() {
    let mut a = 0
    let mut old = 0
    let mut b = 0
    let mut c = 0
    let mut replacement = 0
    let returned = replace_nested(
        [[mut a, mut old], [mut b, mut c]],
        mut replacement,
    )
    let released = mut old
    released = 1
    returned[0][1] = 2
}

fn retains_untouched_nested_member() {
    let mut a = 0
    let mut old = 0
    let mut b = 0
    let mut c = 0
    let mut replacement = 0
    let returned = replace_nested(
        [[mut a, mut old], [mut b, mut c]],
        mut replacement,
    )
    let alias = mut c
    alias = 1
    returned[1][1] = 2
}
"#,
    );
    assert!(
        !diags.contains("borrow conflict in `fn releases_replaced_nested_member`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn retains_untouched_nested_member`"),
        "{diags}"
    );
}

#[test]
fn sparse_array_partitions_include_sources_from_every_return_path() {
    let diags = borrow_diags(
        r#"
fn replace_or_return_untouched(
    mut values: own [mut u256; 2],
    replacement: mut u256,
    replace: bool,
) -> [mut u256; 2] {
    if replace {
        values[0] = mut replacement
        return values
    }
    values
}

fn return_untouched_or_replace_member_one(
    mut values: own [mut u256; 2],
    replacement: mut u256,
    replace: bool,
) -> [mut u256; 2] {
    if replace == false {
        return values
    }
    values[1] = mut replacement
    values
}

fn replace_nested_or_return_untouched(
    mut values: own [[mut u256; 2]; 2],
    replacement: mut u256,
    replace: bool,
) -> [[mut u256; 2]; 2] {
    if replace {
        values[1][1] = mut replacement
        return values
    }
    values
}

fn old_member_source_is_retained(replace: bool) {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_or_return_untouched(
        [mut old_left, mut right],
        mut replacement,
        replace,
    )
    let alias = mut old_left
    alias = 1
    returned[0] = 2
}

fn replacement_source_is_retained(replace: bool) {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_or_return_untouched(
        [mut old_left, mut right],
        mut replacement,
        replace,
    )
    let alias = mut replacement
    alias = 1
    returned[0] = 2
}

fn family_complement_retains_untouched_sibling(replace: bool) {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_or_return_untouched(
        [mut old_left, mut right],
        mut replacement,
        replace,
    )
    let alias = mut right
    alias = 1
    returned[1] = 2
}

fn reversed_return_retains_old_member_one(replace: bool) {
    let mut left = 0
    let mut old_right = 0
    let mut replacement = 0
    let returned = return_untouched_or_replace_member_one(
        [mut left, mut old_right],
        mut replacement,
        replace,
    )
    let alias = mut old_right
    alias = 1
    returned[1] = 2
}

fn nested_return_retains_old_exact_member(replace: bool) {
    let mut a = 0
    let mut b = 0
    let mut c = 0
    let mut old = 0
    let mut replacement = 0
    let returned = replace_nested_or_return_untouched(
        [[mut a, mut b], [mut c, mut old]],
        mut replacement,
        replace,
    )
    let alias = mut old
    alias = 1
    returned[1][1] = 2
}
"#,
    );
    for name in [
        "old_member_source_is_retained",
        "replacement_source_is_retained",
        "family_complement_retains_untouched_sibling",
        "reversed_return_retains_old_member_one",
        "nested_return_retains_old_exact_member",
    ] {
        assert!(
            diags.contains(&format!("borrow conflict in `fn {name}`")),
            "{diags}"
        );
    }
}

#[test]
fn unreachable_array_returns_do_not_add_borrow_sources() {
    let diags = borrow_diags(
        r#"
fn replace_on_known_true(
    mut values: own [mut u256; 2],
    replacement: mut u256,
) -> [mut u256; 2] {
    if true {
        values[0] = mut replacement
        return values
    }
    values
}

fn replace_after_known_false_return(
    mut values: own [mut u256; 2],
    replacement: mut u256,
) -> [mut u256; 2] {
    if false {
        return values
    }
    values[1] = mut replacement
    values
}

fn releases_old_zero() {
    let mut old_left = 0
    let mut right = 0
    let mut replacement = 0
    let returned = replace_on_known_true(
        [mut old_left, mut right],
        mut replacement,
    )
    let released = mut old_left
    released = 1
    returned[0] = 2
}

fn releases_old_one() {
    let mut left = 0
    let mut old_right = 0
    let mut replacement = 0
    let returned = replace_after_known_false_return(
        [mut left, mut old_right],
        mut replacement,
    )
    let released = mut old_right
    released = 1
    returned[1] = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn statically_unreachable_blocks_do_not_produce_borrow_diagnostics() {
    let diags = borrow_diags(
        r#"
fn return_live_param(value: mut u256) -> mut u256 {
    if true {
        return value
    }
    let mut local = 0
    mut local
}

fn dead_alias_conflict() {
    let mut value = 0
    if true {
        return
    }
    let first = mut value
    let second = mut value
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn refined_dead_branch_ownership_sources_do_not_cause_false_moves() {
    let src = r#"
struct Boxed {
    value: u256,
}

struct Holder {
    boxed: Boxed,
}

fn inspect(holder: Holder) -> u256 {
    holder.boxed.value
}

fn probe() -> u256 {
    let live = Holder { boxed: Boxed { value: 1 } }
    let dead = Holder { boxed: Boxed { value: 2 } }
    let consumed = dead.boxed
    let done = true
    inspect(if done { live } else { dead })
}
"#;
    let diags = borrow_diags(src);

    let mut multi_source_locals = Vec::new();
    for_each_fixture_instance(src, |db, instance| {
        if owner_name(db, instance.key(db).owner(db)) != "probe" {
            return;
        }
        let body = normalize_semantic_body_for_layout_evidence(db, instance)
            .expect("analysis normalized body");
        multi_source_locals.extend(
            body.locals
                .iter()
                .enumerate()
                .filter(|(_, local)| local.ownership_sources().len() > 1)
                .map(|(idx, local)| (idx, local.ownership_sources().len())),
        );
    });
    assert!(diags.is_empty(), "{diags}");
    assert!(multi_source_locals.is_empty(), "{multi_source_locals:#?}");
}

#[test]
fn dynamic_branch_ownership_sources_remain_conservative() {
    let src = r#"
struct Boxed {
    value: u256,
}

struct Holder {
    boxed: Boxed,
}

fn inspect(holder: Holder) -> u256 {
    holder.boxed.value
}

fn probe(flag: bool) -> u256 {
    let live = Holder { boxed: Boxed { value: 1 } }
    let dead = Holder { boxed: Boxed { value: 2 } }
    let consumed = dead.boxed
    inspect(if flag { live } else { dead })
}
"#;
    let diags = borrow_diags(src);
    assert!(
        diags.contains("cannot use a value after it was moved"),
        "{diags}"
    );

    let mut max_ownership_sources = 0;
    for_each_fixture_instance(src, |db, instance| {
        if owner_name(db, instance.key(db).owner(db)) != "probe" {
            return;
        }
        let body = normalize_semantic_body_for_layout_evidence(db, instance)
            .expect("analysis normalized body");
        max_ownership_sources = body
            .locals
            .iter()
            .map(|local| local.ownership_sources().len())
            .max()
            .unwrap_or_default();
    });
    assert_eq!(max_ownership_sources, 2);
}

#[test]
fn refined_dead_capability_join_tracks_only_the_live_target() {
    let diags = borrow_diags(
        r#"
fn known_join() {
    let mut first = 1
    let mut second = 2
    let known = true
    let selected = if known { mut first } else { mut second }
    let other = mut second
    other = 8
    selected = 9
}

fn dynamic_join(flag: bool) {
    let mut first = 1
    let mut second = 2
    let selected = if flag { mut first } else { mut second }
    let other = mut second
    other = 8
    selected = 9
}
"#,
    );
    assert!(
        !diags.contains("borrow conflict in `fn known_join`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn dynamic_join`"),
        "{diags}"
    );
}

#[test]
fn statically_unreachable_effect_calls_do_not_refine_provider_provenance() {
    let src = r#"
fn increment() uses (target: mut u256) {
    target += 1
}

fn update() uses (target: mut u256) {
    let done = true
    if done {
        return
    }
    increment()
}

pub contract C {
    mut target: u256

    init() uses (mut target) {
        update()
    }
}
"#;
    let diags = borrow_diags(src);
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn statically_dead_index_writes_do_not_destroy_constant_array_precision() {
    let diags = borrow_diags(
        r#"
fn forward(values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn set_one(index: mut usize) {
    index = 1
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let returned = forward([mut left, mut right])
    let mut index: usize = 0
    if false {
        set_one(mut index)
    }
    let selected = returned[index]
    let alias = mut right
    alias = 1
    selected = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn known_branch_constant_index_flows_through_the_join() {
    let diags = borrow_diags(
        r#"
fn forward(values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let returned = forward([mut left, mut right])
    let index: usize = if false { 1 } else { 0 }
    let selected = returned[index]
    let alias = mut right
    alias = 1
    selected = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn mutably_aliased_constant_index_remains_dynamic() {
    let diags = borrow_diags(
        r#"
fn forward(values: own [mut u256; 2]) -> [mut u256; 2] {
    values
}

fn set_one(index: mut usize) {
    index = 1
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let returned = forward([mut left, mut right])
    let mut index: usize = 0
    set_one(mut index)
    let selected = returned[index]
    let alias = mut right
    alias = 1
    selected = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn statically_dead_uses_do_not_destroy_fresh_owned_call_storage() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn probe() {
    let borrow = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let mut returned = Both {
        left: borrow.call(Boxed { value: 0 }),
        right: borrow.call(Boxed { value: 0 }),
    }
    let mut index: usize = 0
    while index < 2 {
        let boxed = Boxed { value: 0 }
        if false {
            let ignored = boxed.value
        }
        let next = borrow.call(boxed)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_dynamic_array_override_retains_all_possible_sources() {
    let diags = borrow_diags(
        r#"
fn replace_dynamic(
    mut values: own [mut u256; 2],
    replacement: mut u256,
    index: usize,
) -> [mut u256; 2] {
    values[index] = mut replacement
    values
}

fn old_left_may_remain(index: usize) {
    let mut old_left = 0
    let mut old_right = 0
    let mut replacement = 0
    let returned = replace_dynamic(
        [mut old_left, mut old_right],
        mut replacement,
        index,
    )
    let alias = mut old_left
    alias = 1
    returned[0] = 2
}

fn old_right_may_remain(index: usize) {
    let mut old_left = 0
    let mut old_right = 0
    let mut replacement = 0
    let returned = replace_dynamic(
        [mut old_left, mut old_right],
        mut replacement,
        index,
    )
    let alias = mut old_right
    alias = 1
    returned[1] = 2
}

fn replacement_is_returned(index: usize) {
    let mut old_left = 0
    let mut old_right = 0
    let mut replacement = 0
    let returned = replace_dynamic(
        [mut old_left, mut old_right],
        mut replacement,
        index,
    )
    let alias = mut replacement
    alias = 1
    returned[index] = 2
}
"#,
    );
    for name in [
        "old_left_may_remain",
        "old_right_may_remain",
        "replacement_is_returned",
    ] {
        assert!(
            diags.contains(&format!("borrow conflict in `fn {name}`")),
            "{diags}"
        );
    }
}

#[test]
fn overwriting_constant_array_element_releases_only_that_elements_borrows() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn probe() {
    let mut x = 0
    let mut y = 0
    let mut z = 0
    let mut values = [
        Borrowed { value: mut x },
        Borrowed { value: mut y },
    ]
    values[0] = Borrowed { value: mut z }
    let other_x = mut x
    other_x = 1
    values[1].value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn dynamic_array_element_overwrite_retains_possibly_untouched_borrows() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn probe(index: usize) {
    let mut x = 0
    let mut y = 0
    let mut z = 0
    let mut values = [
        Borrowed { value: mut x },
        Borrowed { value: mut y },
    ]
    values[index] = Borrowed { value: mut z }
    let other_x = mut x
    other_x = 1
    values[1].value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn returned_dynamic_index_borrow_is_tracked_conservatively() {
    let diags = borrow_diags(
        r#"
fn get(mut values: [u256; 2], index: usize) -> mut u256 {
    mut values[index]
}

fn probe(index: usize) {
    let mut values = [0, 1]
    let returned = get(values, index)
    let other = mut values[0]
    other = 1
    returned = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
    assert!(
        !diags.contains("return borrows with dynamic indices are not supported"),
        "{diags}"
    );
}

#[test]
fn returned_dynamic_index_borrow_releases_after_its_last_use() {
    let diags = borrow_diags(
        r#"
fn get(mut values: [u256; 2], index: usize) -> mut u256 {
    mut values[index]
}

fn probe(index: usize) {
    let mut values = [0, 1]
    let returned = get(values, index)
    returned = 1
    let other = mut values[0]
    other = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_constant_index_borrow_does_not_alias_sibling_elements() {
    let diags = borrow_diags(
        r#"
fn first(mut values: [u256; 2]) -> mut u256 {
    mut values[0]
}

fn probe() {
    let mut values = [0, 1]
    let returned = first(values)
    let other = mut values[1]
    other = 1
    returned = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_aggregate_preserves_each_borrow_fields_kind() {
    let diags = borrow_diags(
        r#"
struct Borrows {
    immutable: ref u256,
    mutable: mut u256,
}

fn borrow_both(left: u256, mut right: u256) -> Borrows {
    Borrows { immutable: ref left, mutable: mut right }
}

fn probe() {
    let left = 0
    let mut right = 0
    let borrowed = borrow_both(left, right)
    let second_ref = ref left
    borrowed.mutable = 1
    let sum = borrowed.immutable + second_ref
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn nested_returned_closure_preserves_captured_borrow_provenance() {
    let diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let mut x = 0
    let borrowed = mut x
    let outer = |_ unit: own ()| {
        |_ inner_unit: own ()| -> mut u256 { borrowed }
    }
    let inner = outer.call(())
    let returned = inner.call(())
    let other = mut x
    other = 1
    returned
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn returned_nested_closure_preserves_default_view_parameter_provenance() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let boxed = Boxed { value: 42 }
    let make = |value| {
        || value.value
    }
    let read = make.call(boxed)
    let moved = boxed
    read.call() + moved.value
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn returned_nested_closure_rebuilds_owned_array_view_storage() {
    let cases = [
        (
            "array literal",
            r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let make = |values: own [view Boxed; 2]| {
        || values[0].value + values[1].value
    }
    let reader = make.call([
        Boxed { value: 20 },
        Boxed { value: 22 },
    ])
    let _result = reader.call()
}
"#,
        ),
        (
            "array repeat",
            r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let boxed = Boxed { value: 21 }
    let make = |values: own [view Boxed; 2]| {
        || values[0].value + values[1].value
    }
    let reader = make.call([boxed; 2])
    let _result = reader.call()
}
"#,
        ),
    ];

    for (name, src) in cases {
        let diags = borrow_diags(src);
        assert!(diags.is_empty(), "{name}: {diags}");
    }
}

#[test]
fn nested_closure_cannot_escape_a_borrow_of_its_call_local() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let make = |_ unit: own ()| {
        let mut local = 0
        let captured = mut local
        let project = |_ inner_unit: own ()| -> mut u256 { captured }
        project
    }
    let project = make.call(())
    let escaped = project.call(())
    escaped = 1
}
"#,
    );
    assert!(
        diags.contains("invalid return borrow in `fn <closure>`")
            && diags.contains("cannot return a borrow to local `local`"),
        "{diags}"
    );
}

#[test]
fn returned_nested_closure_does_not_retain_unrelated_outer_capture() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut x = 0
    let mut y = 0
    let borrowed_x = mut x
    let borrowed_y = mut y
    let outer = |_ unit: own ()| {
        borrowed_y = 1
        let inner = |_ inner_unit: own ()| -> mut u256 { borrowed_x }
        inner
    }
    let inner = outer.call(())
    let other_y = mut y
    other_y = 2
    let returned = inner.call(())
    returned = 3
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
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

#[test]
fn partial_effect_provider_instance_borrowchecks_parametrically() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        include_str!(
            "../../uitest/fixtures/semantic_borrowck/closure_memory_handle_capture_through_generic.fe"
        ),
    );
    let (top_mod, _) = db.top_mod(file);
    let provide = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "provide") =>
            {
                Some(*func)
            }
            _ => None,
        })
        .expect("provide function");
    let provide_identity = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(provide)),
    );
    assert_eq!(
        provide_identity.key(&db).completeness(&db),
        SemanticInstanceCompleteness::Parametric,
    );
    let partial_capture_target = provide_identity
        .callees(&db)
        .iter()
        .find_map(|callee| {
            (owner_name(&db, callee.key.owner(&db)) == "capture_target")
                .then(|| get_or_build_semantic_instance(&db, callee.key))
        })
        .expect("partially specialized capture_target callee");

    let providers = partial_capture_target
        .key(&db)
        .effect_providers(&db)
        .providers(&db);
    assert!(
        providers
            .iter()
            .any(|specialization| specialization.provider.provider_ty.has_param(&db)),
        "expected the provider to retain provide's H parameter: {providers:#?}"
    );
    assert_eq!(
        partial_capture_target.key(&db).completeness(&db),
        SemanticInstanceCompleteness::Partial,
    );
    check_semantic_borrows(&db, partial_capture_target)
        .expect("partial effect-provider instances should borrowcheck parametrically");
    check_semantic_noesc(&db, partial_capture_target)
        .expect("partial effect-provider instances should check noesc parametrically");

    let concrete_entry = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func.name(&db).to_opt().is_some_and(|name| {
                    name.data(&db) == "closure_memory_handle_capture_through_generic"
                }) =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("concrete entry function");
    let concrete_provide = concrete_entry
        .callees(&db)
        .iter()
        .find_map(|callee| {
            (owner_name(&db, callee.key.owner(&db)) == "provide")
                .then(|| get_or_build_semantic_instance(&db, callee.key))
        })
        .expect("concrete provide callee");
    assert_eq!(
        concrete_provide.key(&db).completeness(&db),
        SemanticInstanceCompleteness::Complete,
    );
    let concrete_capture_target = concrete_provide
        .callees(&db)
        .iter()
        .find_map(|callee| {
            (owner_name(&db, callee.key.owner(&db)) == "capture_target")
                .then(|| get_or_build_semantic_instance(&db, callee.key))
        })
        .expect("concrete capture_target callee");
    assert_eq!(
        concrete_capture_target.key(&db).completeness(&db),
        SemanticInstanceCompleteness::Complete,
    );
    check_semantic_borrows(&db, concrete_capture_target)
        .expect("complete memory-provider instances should use final borrow checking");
    check_semantic_noesc(&db, concrete_capture_target)
        .expect("complete memory-provider instances should retain final noesc semantics");
}

#[test]
fn partial_effect_provider_borrow_summary_is_parametric() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
use core::effect_ref::{EffectHandle, EffectRefMut}
use ingot::Copy

fn update_target(value: mut u256) -> mut u256 uses (target: mut u256) {
    target = 1
    value
}

fn provide<H: EffectHandle<Target = u256> + EffectRefMut<u256> + Copy>(
    _ ptr: H,
    _ value: mut u256,
) -> mut u256 {
    with (ptr) {
        update_target(value)
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let provide = top_mod
        .all_items(&db)
        .iter()
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "provide") =>
            {
                Some(*func)
            }
            _ => None,
        })
        .expect("provide function");
    let provide_identity = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(provide)),
    );
    let partial_update_target = provide_identity
        .callees(&db)
        .iter()
        .find_map(|callee| {
            (owner_name(&db, callee.key.owner(&db)) == "update_target")
                .then(|| get_or_build_semantic_instance(&db, callee.key))
        })
        .expect("partially specialized update_target callee");
    assert_eq!(
        partial_update_target.key(&db).completeness(&db),
        SemanticInstanceCompleteness::Partial,
    );

    assert_eq!(
        semantic_borrow_summary(&db, partial_update_target)
            .expect("partial borrow summary should not fail normalization"),
        Some(vec![BorrowTransform {
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: Vec::new(),
            },
            input: BorrowInput::Place {
                param: 0,
                projection: Vec::new(),
            },
        }]),
    );
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
        BodyOwner::Closure { .. } => "<closure>".to_string(),
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
        transform.input
            == BorrowInput::Place {
                param: 0,
                projection: vec![LayoutBackingProjection::Field(FieldIndex(2))],
            }
    }));
    assert!(summary.iter().any(|transform| {
        transform.input
            == BorrowInput::Place {
                param: 0,
                projection: vec![LayoutBackingProjection::Field(FieldIndex(0))],
            }
    }));
    check_semantic_borrows(&db, instance).expect("borrowck should accept branch-returned borrow");
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
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: Vec::new(),
            },
            input: BorrowInput::Place {
                param: 1,
                projection: Vec::new(),
            },
        }]
    );
    check_semantic_borrows(&db, instance).expect("borrowck should accept forwarded borrows");
}

#[test]
fn writing_through_borrow_field_preserves_returned_handle_source() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Borrowed {
    value: mut u256,
}

fn stash(mut borrowed: own Borrowed, value: mut u256) -> Borrowed {
    borrowed.value = value
    borrowed
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
                    .is_some_and(|name| name.data(&db) == "stash") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("stash instance");
    let summary = semantic_borrow_summary(&db, instance)
        .expect("borrow summary")
        .expect("stash should produce a borrow summary");
    assert_eq!(
        summary,
        vec![BorrowTransform {
            result: BorrowResult {
                kind: BorrowKind::Mut,
                projection: vec![LayoutBackingProjection::Field(FieldIndex(0))],
            },
            input: BorrowInput::Place {
                param: 0,
                projection: vec![LayoutBackingProjection::Field(FieldIndex(0))],
            },
        }]
    );
}

#[test]
fn capability_field_rebind_and_write_through_have_distinct_store_places() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Handle {
    target: mut u256,
}

fn probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = mut second
    handle.target = 7
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
                    .is_some_and(|name| name.data(&db) == "probe") =>
            {
                Some(get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                ))
            }
            _ => None,
        })
        .expect("probe instance");
    let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
    let stores = normalized
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .filter_map(|stmt| {
            let NSStmtKind::Store { dst, src } = &stmt.kind else {
                return None;
            };
            Some((
                dst,
                *src,
                normalized.place_ty(&db, dst),
                normalized.local(src.local).map(|local| local.ty),
            ))
        })
        .collect::<Vec<_>>();

    assert!(
        stores.iter().any(|(dst, _, dst_ty, src_ty)| {
            matches!(dst.root, NSPlaceRoot::Root(_))
                && dst.path.len() == 1
                && dst_ty.is_some_and(|ty| ty.as_capability(&db).is_some())
                && src_ty.is_some_and(|ty| ty.as_capability(&db).is_some())
        }),
        "explicit `mut` must store the new carrier in the field slot: {stores:#?}",
    );
    assert!(
        stores.iter().any(|(dst, _, dst_ty, src_ty)| {
            matches!(dst.root, NSPlaceRoot::CarrierDerefLocal(_))
                && dst.path.is_empty()
                && dst_ty.is_some_and(|ty| ty.as_capability(&db).is_none())
                && src_ty.is_some_and(|ty| ty.as_capability(&db).is_none())
        }),
        "ordinary assignment must store through the materialized carrier: {stores:#?}",
    );
    check_semantic_borrows(&db, instance).expect("both store forms must borrow-check");
}

#[test]
fn capability_returning_operator_calls_rebind_at_the_semantic_store_boundary() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
use core::ops::{Add, Neg}

struct Handle {
    target: mut u256,
}

struct Forwarder {
    target: mut u256,
}

impl Add<Forwarder> for Forwarder {
    type Output = mut u256

    fn add(own self, _ other: own Forwarder) -> mut u256 {
        self.target
    }
}

impl Neg for Forwarder {
    type Output = mut u256

    fn neg(own self) -> mut u256 {
        self.target
    }
}

fn binary_probe() {
    let mut old: u256 = 1
    let mut selected: u256 = 2
    let mut other: u256 = 3
    let mut handle = Handle { target: mut old }
    let left = Forwarder { target: mut selected }
    let right = Forwarder { target: mut other }
    handle.target = left + right
}

fn unary_probe() {
    let mut old: u256 = 4
    let mut selected: u256 = 5
    let mut handle = Handle { target: mut old }
    let forwarder = Forwarder { target: mut selected }
    handle.target = -forwarder
}

fn primitive_probe() {
    let mut value: u256 = 6
    let mut handle = Handle { target: mut value }
    handle.target = 20 + 22
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let store_shapes = |name: &str| {
        let instance = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|ident| ident.data(&db) == name) =>
                {
                    Some(get_or_build_semantic_instance(
                        &db,
                        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                    ))
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("{name} instance"));
        let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
        check_semantic_borrows(&db, instance)
            .unwrap_or_else(|diag| panic!("{name} should borrow-check: {diag:#?}"));
        normalized
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .filter_map(|stmt| {
                let NSStmtKind::Store { dst, src } = &stmt.kind else {
                    return None;
                };
                Some((
                    matches!(dst.root, NSPlaceRoot::Root(_)),
                    matches!(dst.root, NSPlaceRoot::CarrierDerefLocal(_)),
                    dst.path.len(),
                    normalized
                        .place_ty(&db, dst)
                        .is_some_and(|ty| ty.as_capability(&db).is_some()),
                    normalized
                        .local(src.local)
                        .is_some_and(|local| local.ty.as_capability(&db).is_some()),
                ))
            })
            .collect::<Vec<_>>()
    };

    for name in ["binary_probe", "unary_probe"] {
        let shapes = store_shapes(name);
        assert!(
            shapes
                .iter()
                .any(|shape| shape.0 && !shape.1 && shape.2 == 1 && shape.3 && shape.4),
            "{name} must store the operator's carrier in the capability slot: {shapes:#?}",
        );
    }

    let primitive_shapes = store_shapes("primitive_probe");
    assert!(
        primitive_shapes.iter().any(|shape| !shape.0
            && shape.1
            && shape.2 == 0
            && !shape.3
            && !shape.4),
        "primitive arithmetic must retain payload write-through: {primitive_shapes:#?}",
    );
}

#[test]
fn deferred_capability_call_revalidates_assignment_slot_mutability_once() {
    let immutable_diags = analysis_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct Forwarder {}

impl Forwarder {
    fn forward(self, value: mut u256) -> mut u256 {
        value
    }
}

fn probe() {
    let assign = |forwarder| {
        let mut first: u256 = 1
        let mut second: u256 = 2
        let handle = Handle { target: mut first }
        handle.target = forwarder.forward(value: mut second)
    }
    assign.call_once(Forwarder {})
}
"#,
    );
    assert_eq!(
        immutable_diags
            .matches("left-hand side of assignment is immutable")
            .count(),
        1,
        "{immutable_diags}",
    );

    let mutable_diags = analysis_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct Forwarder {}

impl Forwarder {
    fn forward(self, value: mut u256) -> mut u256 {
        value
    }
}

fn probe() {
    let assign = |forwarder| {
        let mut first: u256 = 1
        let mut second: u256 = 2
        let mut handle = Handle { target: mut first }
        handle.target = forwarder.forward(value: mut second)
    }
    assign.call_once(Forwarder {})
}
"#,
    );
    assert!(mutable_diags.is_empty(), "{mutable_diags}");
}

#[test]
fn capability_field_assignment_ignores_only_proven_never_result_arms() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "semantic_borrowck.fe".into(),
        r#"
struct Handle {
    target: mut u256,
}

struct Cell {
    value: u256,
}

struct CellHandle {
    target: mut Cell,
}

enum Choice {
    Second,
    Abort,
}

enum KnownChoice {
    Second,
    Payload,
}

struct Diverger {}

struct NeverWrapper {
    value: u256,
}

impl Diverger {
    fn fail(self) -> ! {
        core::panic()
    }

    fn fail_mut(mut self) -> ! {
        core::panic()
    }
}

fn make_diverger() -> Diverger {
    Diverger {}
}

fn abort() -> ! {
    core::panic()
}

fn if_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { abort() }
}

fn match_block_rebind(choice: Choice) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = match choice {
        Choice::Second => { mut second }
        Choice::Abort => {
            abort()
        }
    }
}

fn method_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    let diverger = Diverger {}
    handle.target = if flag { mut second } else { diverger.fail() }
}

fn call_receiver_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { make_diverger().fail() }
}

fn record_receiver_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { (Diverger {}).fail() }
}

fn block_receiver_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { ({ make_diverger() }).fail() }
}

fn if_receiver_rebind(flag: bool, choose_call: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        (if choose_call { make_diverger() } else { Diverger {} }).fail()
    }
}

fn mutable_receiver_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    let mut diverger = Diverger {}
    handle.target = if flag { mut second } else { diverger.fail_mut() }
}

fn aggregate_wrapped_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        NeverWrapper { value: abort() }
    }
}

fn eager_prefix_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        assert!(abort())
        let fallback: u256 = 3
        fallback
    }
}

fn forced_and_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { true && abort() }
}

fn forced_or_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { false || abort() }
}

fn constant_if_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { if true { abort() } else { 3 } }
}

fn infinite_loop_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        while true {}
        3
    }
}

fn assert_false_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        assert!(false)
        3
    }
}

fn known_match_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = match KnownChoice::Second {
        KnownChoice::Second => mut second,
        KnownChoice::Payload => 3,
    }
}

fn block_tail_condition_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if { true } { mut second } else { 3 }
}

fn with_body_condition_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: bool = false
    let mut handle = Handle { target: mut first }
    handle.target = if with (marker) { true } { mut second } else { 3 }
}

fn nested_if_condition_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if (if true { true } else { false }) {
        mut second
    } else {
        3
    }
}

fn irrefutable_if_let_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: u256 = 0
    let mut handle = Handle { target: mut first }
    handle.target = if let ignored = marker { mut second } else { 3 }
}

fn known_if_let_match_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let KnownChoice::Second = KnownChoice::Second {
        mut second
    } else {
        3
    }
}

fn known_if_let_miss_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let KnownChoice::Payload = KnownChoice::Second {
        3
    } else {
        mut second
    }
}

fn irrefutable_while_let_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: u256 = 0
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        while let ignored = marker {}
        3
    }
}

fn irrefutable_let_and_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: u256 = 0
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        if let ignored = marker && abort() { 3 } else { 4 }
    }
}

fn dynamic_if_let_write(choice: KnownChoice) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let KnownChoice::Second = choice { mut second } else { 3 }
}

fn absorbing_and_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag && false { 3 } else { mut second }
}

fn absorbing_or_rebind(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag || true { mut second } else { 3 }
}

fn escape_normal_true_rebind(escape: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if {
        if escape {
            return
        }
        true
    } {
        mut second
    } else {
        3
    }
}

fn literal_if_let_match_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let true = true { mut second } else { 3 }
}

fn literal_if_let_miss_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let false = true { 3 } else { mut second }
}

fn known_literal_match_rebind() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = match true {
        true => mut second,
        false => 3,
    }
}

fn completing_short_circuit_write(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        false && abort()
        3
    }
}

fn completing_loop_write(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        while true {
            break
        }
        3
    }
}

fn mixed_live_write(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { 3 }
}

fn aggregate_write() {
    let mut first = Cell { value: 1 }
    let mut handle = CellHandle { target: mut first }
    handle.target = Cell { value: 4 }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let store_shapes = |name: &str| {
        let instance = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|ident| ident.data(&db) == name) =>
                {
                    Some(get_or_build_semantic_instance(
                        &db,
                        identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
                    ))
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("{name} instance"));
        let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
        check_semantic_borrows(&db, instance)
            .unwrap_or_else(|diag| panic!("{name} should borrow-check: {diag:#?}"));
        normalized
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .filter_map(|stmt| {
                let NSStmtKind::Store { dst, src } = &stmt.kind else {
                    return None;
                };
                Some((
                    matches!(dst.root, NSPlaceRoot::Root(_)),
                    matches!(dst.root, NSPlaceRoot::CarrierDerefLocal(_)),
                    dst.path.len(),
                    normalized
                        .place_ty(&db, dst)
                        .is_some_and(|ty| ty.as_capability(&db).is_some()),
                    normalized
                        .local(src.local)
                        .is_some_and(|local| local.ty.as_capability(&db).is_some()),
                ))
            })
            .collect::<Vec<_>>()
    };

    for name in [
        "if_rebind",
        "match_block_rebind",
        "method_rebind",
        "call_receiver_rebind",
        "record_receiver_rebind",
        "block_receiver_rebind",
        "if_receiver_rebind",
        "mutable_receiver_rebind",
        "aggregate_wrapped_rebind",
        "eager_prefix_rebind",
        "forced_and_rebind",
        "forced_or_rebind",
        "constant_if_rebind",
        "infinite_loop_rebind",
        "assert_false_rebind",
        "known_match_rebind",
        "block_tail_condition_rebind",
        "with_body_condition_rebind",
        "nested_if_condition_rebind",
        "irrefutable_if_let_rebind",
        "known_if_let_match_rebind",
        "known_if_let_miss_rebind",
        "irrefutable_while_let_rebind",
        "irrefutable_let_and_rebind",
        "absorbing_and_rebind",
        "absorbing_or_rebind",
        "escape_normal_true_rebind",
        "literal_if_let_match_rebind",
        "literal_if_let_miss_rebind",
        "known_literal_match_rebind",
    ] {
        let shapes = store_shapes(name);
        assert!(
            shapes
                .iter()
                .any(|shape| { shape.0 && !shape.1 && shape.2 == 1 && shape.3 && shape.4 }),
            "{name} must store a carrier in the capability slot: {shapes:#?}",
        );
    }
    for name in [
        "mixed_live_write",
        "aggregate_write",
        "completing_short_circuit_write",
        "completing_loop_write",
        "dynamic_if_let_write",
    ] {
        let shapes = store_shapes(name);
        assert!(
            shapes
                .iter()
                .any(|shape| { !shape.0 && shape.1 && shape.2 == 0 && !shape.3 && !shape.4 }),
            "{name} must remain a payload write through the carrier: {shapes:#?}",
        );
    }
}

#[test]
fn divergent_capability_arms_rebind_borrow_provenance() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

enum Choice {
    Second,
    Abort,
}

struct Diverger {}

struct NeverWrapper {
    value: u256,
}

impl Diverger {
    fn fail(self) -> ! {
        core::panic()
    }

    fn fail_mut(mut self) -> ! {
        core::panic()
    }
}

fn make_diverger() -> Diverger {
    Diverger {}
}

fn abort() -> ! {
    core::panic()
}

fn if_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { abort() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn match_probe(choice: Choice) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = match choice {
        Choice::Second => { mut second }
        Choice::Abort => {
            abort()
        }
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn method_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    let diverger = Diverger {}
    handle.target = if flag { mut second } else { diverger.fail() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn call_receiver_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { make_diverger().fail() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn record_receiver_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { (Diverger {}).fail() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn block_receiver_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { ({ make_diverger() }).fail() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn if_receiver_probe(flag: bool, choose_call: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        (if choose_call { make_diverger() } else { Diverger {} }).fail()
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn mutable_receiver_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    let mut diverger = Diverger {}
    handle.target = if flag { mut second } else { diverger.fail_mut() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn aggregate_wrapped_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        NeverWrapper { value: abort() }
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn eager_prefix_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        assert!(abort())
        let fallback: u256 = 3
        fallback
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}
"#,
    );
    for name in [
        "if_probe",
        "match_probe",
        "method_probe",
        "call_receiver_probe",
        "record_receiver_probe",
        "block_receiver_probe",
        "if_receiver_probe",
        "mutable_receiver_probe",
        "aggregate_wrapped_probe",
        "eager_prefix_probe",
    ] {
        assert!(
            diags.contains(&format!("borrow conflict in `fn {name}`")),
            "missing new-target conflict for {name}: {diags}",
        );
    }
}

#[test]
fn statically_unreachable_payload_results_rebind_borrow_provenance() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

enum KnownChoice {
    Second,
    Payload,
}

fn abort() -> ! {
    core::panic()
}

fn forced_and_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { true && abort() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn forced_or_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { false || abort() }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn constant_if_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { if true { abort() } else { 3 } }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn infinite_loop_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        while true {}
        3
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn assert_false_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        assert!(false)
        3
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn known_match_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = match KnownChoice::Second {
        KnownChoice::Second => mut second,
        KnownChoice::Payload => 3,
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn block_tail_condition_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if { true } { mut second } else { 3 }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn with_body_condition_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: bool = false
    let mut handle = Handle { target: mut first }
    handle.target = if with (marker) { true } { mut second } else { 3 }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn nested_if_condition_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if (if true { true } else { false }) {
        mut second
    } else {
        3
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn irrefutable_if_let_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: u256 = 0
    let mut handle = Handle { target: mut first }
    handle.target = if let ignored = marker { mut second } else { 3 }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn known_if_let_match_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let KnownChoice::Second = KnownChoice::Second {
        mut second
    } else {
        3
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn known_if_let_miss_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let KnownChoice::Payload = KnownChoice::Second {
        3
    } else {
        mut second
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn irrefutable_while_let_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: u256 = 0
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        while let ignored = marker {}
        3
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn irrefutable_let_and_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let marker: u256 = 0
    let mut handle = Handle { target: mut first }
    handle.target = if flag {
        mut second
    } else {
        if let ignored = marker && abort() { 3 } else { 4 }
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn absorbing_and_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag && false { 3 } else { mut second }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn absorbing_or_probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag || true { mut second } else { 3 }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn escape_normal_true_probe(escape: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if {
        if escape {
            return
        }
        true
    } {
        mut second
    } else {
        3
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn literal_if_let_match_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let true = true { mut second } else { 3 }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn literal_if_let_miss_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let false = true { 3 } else { mut second }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}

fn known_literal_match_probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = match true {
        true => mut second,
        false => 3,
    }
    let rebound = handle.target
    let alias = mut second
    alias = 3
    rebound = 4
}
"#,
    );
    for name in [
        "forced_and_probe",
        "forced_or_probe",
        "constant_if_probe",
        "infinite_loop_probe",
        "assert_false_probe",
        "known_match_probe",
        "block_tail_condition_probe",
        "with_body_condition_probe",
        "nested_if_condition_probe",
        "irrefutable_if_let_probe",
        "known_if_let_match_probe",
        "known_if_let_miss_probe",
        "irrefutable_while_let_probe",
        "irrefutable_let_and_probe",
        "absorbing_and_probe",
        "absorbing_or_probe",
        "escape_normal_true_probe",
        "literal_if_let_match_probe",
        "literal_if_let_miss_probe",
        "known_literal_match_probe",
    ] {
        assert!(
            diags.contains(&format!("borrow conflict in `fn {name}`")),
            "missing new-target conflict for {name}: {diags}",
        );
    }
}

#[test]
fn live_mixed_capability_and_payload_arms_keep_old_borrow_provenance() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

fn probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { 3 }
    let stored = handle.target
    let alias = mut first
    alias = 4
    stored = 5
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn dynamic_refutable_if_let_keeps_old_borrow_provenance() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

enum Choice {
    Second,
    Payload,
}

fn probe(choice: Choice) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if let Choice::Second = choice { mut second } else { 3 }
    let stored = handle.target
    let alias = mut first
    alias = 4
    stored = 5
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn temporary_receiver_divergent_rebind_releases_old_borrow_provenance() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct Diverger {}

impl Diverger {
    fn fail(self) -> ! {
        core::panic()
    }
}

fn make_diverger() -> Diverger {
    Diverger {}
}

fn probe(flag: bool) {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut handle = Handle { target: mut first }
    handle.target = if flag { mut second } else { make_diverger().fail() }
    let rebound = handle.target
    let old = mut first
    old = 3
    rebound = 4
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn nested_capability_field_rebind_tracks_the_new_target() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct NestedHandle {
    target: mut Handle,
}

fn probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut inner = Handle { target: mut first }
    let outer = NestedHandle { target: mut inner }

    outer.target.target = mut second
    let rebound = outer.target.target
    let alias = mut second
    alias = 3
    rebound = 4
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn nested_capability_field_rebind_releases_the_old_target() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct NestedHandle {
    target: mut Handle,
}

fn probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let mut inner = Handle { target: mut first }
    let outer = NestedHandle { target: mut inner }

    outer.target.target = mut second
    let rebound = outer.target.target
    let old = mut first
    old = 3
    rebound = 4
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn nested_capability_field_without_rebind_retains_the_original_target() {
    let diags = borrow_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct NestedHandle {
    target: mut Handle,
}

fn probe() {
    let mut first: u256 = 1
    let mut inner = Handle { target: mut first }
    let outer = NestedHandle { target: mut inner }

    let stored = outer.target.target
    let alias = mut first
    alias = 3
    stored = 4
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
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
fn nested_borrow_slots_cannot_join_different_provider_spaces() {
    let diags = borrow_diags(
        r#"
struct Holder {
    value: mut u256,
}

fn update(flag: bool, local: mut u256) uses (target: mut u256) {
    let holder = if flag {
        Holder { value: mut target }
    } else {
        Holder { value: local }
    }
    holder.value = 42
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    mut target: u256

    recv Msg {
        Go uses (mut target) {
            let mut local: u256 = 0
            update(flag: true, local: mut local)
        }
    }
}
"#,
    );

    assert!(
        diags.contains("provider provenance conflict in `fn update`"),
        "{diags:?}"
    );
    assert!(
        diags.contains("borrow slot may come from multiple address spaces: memory, storage"),
        "{diags:?}"
    );
}

#[test]
fn nested_ref_slots_cannot_join_different_provider_spaces() {
    let diags = borrow_diags(
        r#"
struct Holder {
    value: ref u256,
}

fn read(flag: bool, local: ref u256) -> u256 uses (target: u256) {
    let holder = if flag {
        Holder { value: ref target }
    } else {
        Holder { value: local }
    }
    holder.value
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    target: u256

    recv Msg {
        Go uses (target) {
            let local: u256 = 0
            let _ = read(flag: true, local: ref local)
        }
    }
}
"#,
    );

    assert!(
        diags.contains("provider provenance conflict in `fn read`"),
        "{diags:?}"
    );
}

#[test]
fn nested_implicit_view_slots_cannot_join_different_provider_spaces() {
    let diags = borrow_diags(
        r#"
struct Data {
    value: u256,
}

struct Holder {
    value: view Data,
}

fn read(flag: bool, local: Data) -> u256 uses (target: Data) {
    let holder = if flag {
        Holder { value: target }
    } else {
        Holder { value: local }
    }
    holder.value.value
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    target: Data

    recv Msg {
        Go uses (target) {
            let local = Data { value: 0 }
            let _ = read(flag: true, local)
        }
    }
}
"#,
    );

    assert!(
        diags.contains("provider provenance conflict in `fn read`"),
        "{diags:?}"
    );
}

#[test]
fn closures_can_capture_effect_bindings_and_forward_them_to_effectful_calls() {
    let direct_mutation = r#"
fn probe() uses (target: mut u256) {
    let update = || {
        target += 1
    }
}
"#;
    let direct_read = r#"
fn probe() uses (target: u256) {
    let read = || target
}
"#;
    let immutable_effectful_call = r#"
fn read() -> u256 uses (target: u256) {
    target
}

fn probe() uses (target: u256) {
    let read_target = || read()
}
"#;
    let mutable_effectful_call = r#"
fn increment() uses (target: mut u256) {
    target += 1
}

fn probe() uses (target: mut u256) {
    let update = || increment()
}
"#;
    let projected_effectful_call = r#"
struct Pair {
    value: u256,
}

fn increment() uses (target: mut u256) {
    target += 1
}

fn probe() uses (pair: mut Pair) {
    with (mut pair.value) {
        let update = || increment()
    }
}
"#;

    for src in [
        direct_mutation,
        direct_read,
        immutable_effectful_call,
        mutable_effectful_call,
        projected_effectful_call,
    ] {
        let diags = analysis_diags(src);
        assert!(diags.is_empty(), "{diags:?}");

        let borrow_diags = borrow_diags(src);
        assert!(borrow_diags.is_empty(), "{borrow_diags:?}");
    }
}

#[test]
fn aggregate_stored_mut_param_retains_its_alias_source() {
    let diags = borrow_diags(
        r#"
struct Holder {
    value: mut u256,
}

fn write_both(_ left: mut u256, _ right: mut u256) {}

fn bad(_ target: mut u256) {
    let holder = Holder { value: target }
    write_both(holder.value, target)
}

fn probe() {
    let mut value: u256 = 0
    bad(mut value)
}
"#,
    );

    assert!(
        diags.contains("call arguments require conflicting access to the same place"),
        "{diags:?}"
    );
}

#[test]
fn sequential_nested_borrow_slot_assignments_cannot_change_address_space() {
    let diags = borrow_diags(
        r#"
struct Holder {
    value: mut u256,
}

fn update(local: mut u256) uses (target: mut u256) {
    let mut holder = Holder { value: local }
    holder = Holder { value: mut target }
    holder.value = 42
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    mut target: u256

    recv Msg {
        Go uses (mut target) {
            let mut local: u256 = 0
            update(local: mut local)
        }
    }
}
"#,
    );

    assert!(
        diags.contains("provider provenance conflict in `fn update`"),
        "{diags:?}"
    );
}

#[test]
fn array_borrow_elements_cannot_use_different_address_spaces() {
    let diags = borrow_diags(
        r#"
fn update(local: mut u256) uses (target: mut u256) {
    let handles = [mut target, local]
    handles[0] = 42
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    mut target: u256

    recv Msg {
        Go uses (mut target) {
            let mut local: u256 = 0
            update(local: mut local)
        }
    }
}
"#,
    );

    assert!(
        diags.contains("provider provenance conflict in `fn update`"),
        "{diags:?}"
    );
}

#[test]
fn distinct_struct_borrow_fields_may_use_different_address_spaces() {
    let diags = borrow_diags(
        r#"
struct Handles {
    stored: mut u256,
    local: mut u256,
}

fn update(local: mut u256) uses (target: mut u256) {
    let handles = Handles {
        stored: mut target,
        local,
    }
    handles.stored = 1
    handles.local = 2
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    mut target: u256

    recv Msg {
        Go uses (mut target) {
            let mut local: u256 = 0
            update(local: mut local)
        }
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn distinct_enum_variant_borrow_fields_may_use_different_address_spaces() {
    let diags = borrow_diags(
        r#"
enum Handle {
    Stored(mut u256),
    Local(mut u256),
}

fn update(flag: bool, local: mut u256) uses (target: mut u256) {
    let handle = if flag {
        Handle::Stored(mut target)
    } else {
        Handle::Local(local)
    }
    match handle {
        Handle::Stored(value) => value = 1,
        Handle::Local(value) => value = 2,
    }
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    mut target: u256

    recv Msg {
        Go uses (mut target) {
            let mut local: u256 = 0
            update(flag: true, local: mut local)
        }
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn closure_arguments_cannot_smuggle_storage_borrows_past_noesc() {
    let diags = borrow_diags(
        r#"
enum Handle {
    Stored(mut u256),
    Local(mut u256),
}

fn update(flag: bool, local: mut u256) uses (target: mut u256) {
    let choose = |choose_stored: own bool, stored: mut u256, local: mut u256| {
        if choose_stored {
            Handle::Stored(stored)
        } else {
            Handle::Local(local)
        }
    }
    let handle = choose.call(flag, mut target, local)
    match handle {
        Handle::Stored(value) => value = 1,
        Handle::Local(value) => value = 2,
    }
}

msg Msg {
    #[selector = 1]
    Go
}

pub contract C {
    mut target: u256

    recv Msg {
        Go uses (mut target) {
            let mut local: u256 = 0
            update(flag: true, local: mut local)
        }
    }
}
"#,
    );

    assert!(
        diags.contains("cannot pass `(bool, mut u256, mut u256)` from storage"),
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
    assert!(
        !instance.known_never_returns(&db),
        "normalization failure must conservatively preserve a possible normal return"
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
fn writing_borrowed_place_while_ref_is_live_is_rejected() {
    let diags = borrow_diags(
        r#"
fn bad() -> u256 {
    let mut x = 0
    let borrowed = ref x
    x = 1
    borrowed
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn reading_borrowed_place_while_mut_borrow_is_live_is_rejected() {
    let diags = borrow_diags(
        r#"
fn bad() -> u256 {
    let mut x = 0
    let borrowed = mut x
    let value = x
    borrowed = 1
    value
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn access_to_disjoint_field_while_mut_borrow_is_live_is_allowed() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: u256,
    right: u256,
}

fn valid() -> u256 {
    let mut pair = Pair { left: 0, right: 1 }
    let borrowed = mut pair.left
    let value = pair.right
    borrowed = 2
    value
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn reading_parent_while_field_mut_borrow_is_live_is_rejected() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: u256,
    right: u256,
}

fn consume(_ pair: Pair) -> u256 {
    pair.right
}

fn bad() -> u256 {
    let mut pair = Pair { left: 0, right: 1 }
    let borrowed = mut pair.left
    let value = consume(pair)
    borrowed = 2
    value
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn reading_and_writing_through_active_mut_borrow_is_allowed() {
    let diags = borrow_diags(
        r#"
fn valid() -> u256 {
    let mut x = 0
    let borrowed = mut x
    borrowed = 1
    borrowed
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn writing_borrowed_place_after_last_borrow_use_is_allowed() {
    let diags = borrow_diags(
        r#"
fn valid() -> u256 {
    let mut x = 0
    let borrowed = ref x
    let value = borrowed + 0
    x = 1
    value
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn moving_borrowed_value_into_aggregate_is_rejected() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Holder {
    boxed: Boxed,
}

fn bad() -> u256 {
    let boxed = Boxed { value: 1 }
    let borrowed = ref boxed
    let holder = Holder { boxed }
    borrowed.value + holder.boxed.value
}
"#,
    );

    assert!(
        diags.contains("cannot move out of a value while it is borrowed"),
        "{diags:?}"
    );
}

#[test]
fn store_source_cannot_reuse_moved_value() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Holder {
    boxed: Boxed,
}

fn bad() -> u256 {
    let mut holder = Holder { boxed: Boxed { value: 0 } }
    let boxed = Boxed { value: 1 }
    let moved = boxed
    holder.boxed = boxed
    moved.value
}
"#,
    );

    assert!(
        diags.contains("cannot use a value after it was moved"),
        "{diags:?}"
    );
}

#[test]
fn aggregate_cannot_move_same_value_twice_in_one_expression() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Pair {
    left: Boxed,
    right: Boxed,
}

fn bad(_ boxed: own Boxed) -> Pair {
    Pair { left: boxed, right: boxed }
}
"#,
    );

    assert!(
        diags.contains("cannot use a value after it was moved"),
        "{diags:?}"
    );
}

#[test]
fn call_cannot_move_same_value_into_two_owned_params() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn consume(_ left: own Boxed, _ right: own Boxed) {}

fn bad(_ boxed: own Boxed) {
    consume(boxed, boxed)
}
"#,
    );

    assert!(
        diags.contains("cannot use a value after it was moved"),
        "{diags:?}"
    );
}

#[test]
fn call_cannot_alias_one_mut_handle_across_two_params() {
    let diags = borrow_diags(
        r#"
fn write_both(_ left: mut u256, _ right: mut u256) {
    left = 1
    right = 2
}

fn bad() {
    let mut x = 0
    let borrowed = mut x
    write_both(borrowed, borrowed)
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn call_may_alias_one_ref_handle_across_two_params() {
    let diags = borrow_diags(
        r#"
fn sum(_ left: ref u256, _ right: ref u256) -> u256 {
    left + right
}

fn valid() -> u256 {
    let x = 21
    let borrowed = ref x
    sum(borrowed, borrowed)
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn call_may_mutably_access_disjoint_fields() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: u256,
    right: u256,
}

fn write_both(_ left: mut u256, _ right: mut u256) {
    left = 1
    right = 2
}

fn valid() {
    let mut pair = Pair { left: 0, right: 0 }
    let left = mut pair.left
    let right = mut pair.right
    write_both(left, right)
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn call_cannot_mix_parent_mut_and_child_ref_access() {
    let diags = borrow_diags(
        r#"
fn read_and_write(_ writer: mut u256, reader: ref u256) {
    writer = reader
}

fn bad() {
    let mut value = 0
    let writer = mut value
    let reader = ref writer
    read_and_write(writer, reader)
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn call_cannot_alias_mut_borrows_nested_in_aggregate_args() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn write_both(left: Borrowed, right: Borrowed) {
    left.value = 1
    right.value = 2
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    let left = Borrowed { value: borrowed }
    let right = Borrowed { value: borrowed }
    write_both(left, right)
}
"#,
    );

    assert!(
        diags.contains("call arguments require conflicting access to the same place"),
        "{diags:?}"
    );
}

#[test]
fn call_may_alias_ref_borrows_nested_in_aggregate_args() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: ref u256,
}

fn sum(left: Borrowed, right: Borrowed) -> u256 {
    left.value + right.value
}

fn valid() -> u256 {
    let value = 21
    let borrowed = ref value
    let left = Borrowed { value: borrowed }
    let right = Borrowed { value: borrowed }
    sum(left, right)
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn call_cannot_alias_mut_borrows_within_one_aggregate_arg() {
    let diags = borrow_diags(
        r#"
struct BorrowedPair {
    left: mut u256,
    right: mut u256,
}

fn write_both(pair: BorrowedPair) {
    pair.left = 1
    pair.right = 2
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    let pair = BorrowedPair { left: borrowed, right: borrowed }
    write_both(pair)
}
"#,
    );

    assert!(
        diags.contains("call arguments require conflicting access to the same place"),
        "{diags:?}"
    );
}

#[test]
fn call_cannot_alias_regular_and_effect_mut_access() {
    let diags = borrow_diags(
        r#"
fn write_both(regular: mut u256) uses (effect: mut u256) {
    regular = 1
    effect = 2
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    with (borrowed) {
        write_both(borrowed)
    }
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn call_may_copy_value_before_mut_effect_access() {
    let diags = borrow_diags(
        r#"
fn write(_ snapshot: u256) uses (target: mut u256) {
    target = snapshot
}

fn valid() {
    let mut value: u256 = 1
    with (value) {
        write(value)
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn mutable_receiver_reservation_allows_nested_shared_receiver_call() {
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
}

fn valid() {
    let mut cell = Cell { value: 1 }
    cell.write(value: cell.read())
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn mutable_receiver_activation_rejects_lingering_shared_borrow() {
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
}

fn bad() -> u256 {
    let mut cell = Cell { value: 1 }
    let borrowed = ref cell.value
    cell.write(value: cell.read())
    borrowed
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn mutable_receiver_reservation_allows_nested_mut_receiver_call() {
    let diags = borrow_diags(
        r#"
struct Cell {
    value: u256,
}

impl Cell {
    fn take(mut self) -> u256 {
        self.value += 1
        self.value
    }

    fn write(mut self, value: u256) {
        self.value = value
    }
}

fn bad() {
    let mut cell = Cell { value: 1 }
    cell.write(value: cell.take())
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn mutable_receiver_activation_rejects_borrow_returned_by_nested_call() {
    let diags = borrow_diags(
        r#"
struct Cell {
    value: u256,
}

impl Cell {
    fn borrow(mut self) -> mut u256 {
        mut self.value
    }

    fn write(mut self, value: mut u256) {
        self.value = value
    }
}

fn bad() {
    let mut cell = Cell { value: 1 }
    cell.write(value: cell.borrow())
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn call_cannot_alias_nested_mut_borrows_across_regular_and_effect_args() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn write_both(regular: Borrowed) uses (effect: Borrowed) {
    regular.value = 1
    effect.value = 2
}

fn bad() {
    let mut value = 0
    let borrowed = mut value
    let regular = Borrowed { value: borrowed }
    let effect = Borrowed { value: borrowed }
    with (effect) {
        write_both(regular)
    }
}
"#,
    );

    assert!(
        diags.contains("call arguments require conflicting access to the same place"),
        "{diags:?}"
    );
}

#[test]
fn call_may_alias_nested_ref_borrows_across_regular_and_effect_args() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: ref u256,
}

fn sum(regular: Borrowed) -> u256 uses (effect: Borrowed) {
    regular.value + effect.value
}

fn valid() -> u256 {
    let value = 21
    let borrowed = ref value
    let regular = Borrowed { value: borrowed }
    let effect = Borrowed { value: borrowed }
    with (effect) {
        sum(regular)
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn mutable_effect_call_cannot_write_borrowed_place() {
    let diags = borrow_diags(
        r#"
fn set() uses (value: mut u256) {
    value = 1
}

fn bad() -> u256 {
    let mut x = 0
    let borrowed = ref x
    with (x) {
        set()
    }
    borrowed
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
}

#[test]
fn mutable_effect_call_through_active_mut_borrow_is_allowed() {
    let diags = borrow_diags(
        r#"
fn set() uses (value: mut u256) {
    value = 1
}

fn valid() -> u256 {
    let mut x = 0
    let borrowed = mut x
    with (borrowed) {
        set()
    }
    borrowed
}
"#,
    );

    assert!(diags.is_empty(), "{diags:?}");
}

#[test]
fn immutable_effect_call_cannot_read_mutably_borrowed_place() {
    let diags = borrow_diags(
        r#"
fn get() -> u256 uses (value: u256) {
    value
}

fn bad() -> u256 {
    let mut x = 0
    let borrowed = mut x
    let value = with (x) {
        get()
    }
    borrowed = 1
    value
}
"#,
    );

    assert!(diags.contains("borrow conflict in `fn bad`"), "{diags:?}");
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
    const SPACE: AddressSpace = AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { tag: 1, addr: raw }
    }

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
    const SPACE: AddressSpace = AddressSpace::Memory

    fn from_raw(_ raw: u256) -> Self {
        Self { tag: 1, addr: raw }
    }

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
    check_semantic_borrows(&db, init)
        .expect("passing a local capability to generic noesc analysis should be borrow-safe");
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
    let diags = borrow_diags(
        r#"
use std::evm::RawMem

fn allocate(bytes: u256) -> u256 uses (mem: mut RawMem) {
    let mut ptr = mem.mload(0x40)
    if ptr == 0 {
        ptr = 0x60
    }
    mem.mstore(0x40, ptr + bytes)
    ptr
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
fn closure_owned_param_projection_honors_view_receiver_mode() {
    let src = r#"
struct Row {
    cells: [u256; 4],
}

impl Row {
    fn get_cell(self, col: usize) -> u256 {
        self.cells[col]
    }
}

struct Board {
    rows: [Row; 4],
}

fn read_via_closure(_ marker: u256, _ decoy: Board, board: own Board) -> u256 {
    let read = |value: own Board| -> u256 { value.rows[0].get_cell(col: 0) }
    read.call(board)
}
"#;
    let mut found_closure = false;
    for_each_fixture_instance(src, |db, instance| {
        if !matches!(instance.key(db).owner(db), BodyOwner::Closure { .. }) {
            return;
        }
        found_closure = true;
        let normalized = normalize_semantic_body(db, instance).expect("normalized closure body");
        let (row_read_mode, logical_param_local) = normalized
            .blocks
            .iter()
            .flat_map(|block| block.stmts.iter())
            .find_map(|stmt| match &stmt.kind {
                NSStmtKind::Assign {
                    dst,
                    expr: NExpr::ReadPlace { mode, place },
                } if normalized.locals[dst.index()].ty.pretty_print(db) == "Row" => {
                    let NSPlaceRoot::Root(root) = place.root else {
                        return None;
                    };
                    let Some(NBorrowRoot::LocalSlot { local }) = normalized.root(root) else {
                        return None;
                    };
                    Some((mode, *local))
                }
                _ => None,
            })
            .expect("row projection read from the owned closure parameter");
        assert_eq!(*row_read_mode, ReadMode::Read);
        let snapshot_source = normalized.locals[logical_param_local.index()]
            .facts
            .snapshot_source_place
            .as_ref()
            .expect("logical closure parameter must retain its tuple-field source");
        let NSPlaceRoot::Root(root) = snapshot_source.root else {
            panic!("closure argument source must be a physical parameter root");
        };
        assert!(matches!(
            normalized.root(root),
            Some(NBorrowRoot::Param { param_idx: 1, .. })
        ));
        assert_eq!(
            snapshot_source.path.iter().cloned().collect::<Vec<_>>(),
            vec![Projection::Field(0)]
        );
    });
    assert!(found_closure);
}

#[test]
fn closure_logical_param_modes_govern_moves_from_tuple_pack_fields() {
    for (mode, src, message) in [
        (
            "view",
            r#"
struct Wrapper {
    value: Boxed,
}

struct Boxed {
    value: u256,
}

fn probe() {
    let take = |wrapped: Wrapper| -> Boxed { wrapped.value }
}
"#,
            "cannot move out of a view parameter",
        ),
        (
            "ref",
            r#"
struct Wrapper {
    value: Boxed,
}

struct Boxed {
    value: u256,
}

fn probe() {
    let take = |wrapped: ref Wrapper| -> Boxed { wrapped.value }
}
"#,
            "cannot move out through a borrow handle",
        ),
        (
            "mut",
            r#"
struct Wrapper {
    value: Boxed,
}

struct Boxed {
    value: u256,
}

fn probe() {
    let take = |wrapped: mut Wrapper| -> Boxed { wrapped.value }
}
"#,
            "cannot move out through a borrow handle",
        ),
    ] {
        let diags = borrow_diags(src);
        assert!(
            diags.contains("move conflict in `fn <closure>`") && diags.contains(message),
            "{mode} parameter diagnostics: {diags}"
        );
    }

    let own_diags = borrow_diags(
        r#"
struct Wrapper {
    value: Boxed,
}

struct Boxed {
    value: u256,
}

fn probe() -> Boxed {
    let take = |wrapped: own Wrapper| -> Boxed { wrapped.value }
    take.call_once(Wrapper { value: Boxed { value: 42 } })
}
"#,
    );
    assert!(own_diags.is_empty(), "{own_diags}");
}

#[test]
fn suppressed_closure_params_retain_their_logical_modes() {
    let own_diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let discard = |_: own| {}
    discard.call_once(value)
    value.value
}
"#,
    );
    assert!(
        own_diags.contains("cannot use a value after it was moved"),
        "{own_diags}"
    );

    let aliased_diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 42
    let discard = |_: mut, _: view| {}
    discard.call(mut value, value)
}
"#,
    );
    assert!(
        aliased_diags.contains("borrow conflict in `fn probe`"),
        "{aliased_diags}"
    );

    let shared_diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let discard = |_, _| {}
    discard.call(value, value)
    value.value
}
"#,
    );
    assert!(shared_diags.is_empty(), "{shared_diags}");
}

#[test]
fn closure_default_view_param_projection_rebinding_remains_a_move() {
    let src = r#"
struct Wrapper {
    value: Boxed,
}

struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let take = |wrapped: Wrapper| -> Boxed {
        let value = wrapped.value
        value
    }
    let result = take.call(Wrapper { value: Boxed { value: 42 } })
    result.value
}
"#;
    let diags = borrow_diags(src);
    assert!(
        diags.contains("cannot move out of a view parameter"),
        "{diags}"
    );
}

#[test]
fn closure_view_param_call_reads_non_copy_argument() {
    let src = r#"
struct Boxed {
    value: u256,
}

fn read_twice(_ value: own Boxed) -> u256 {
    let read = |item: Boxed| -> u256 { item.value }
    let first = read.call(value)
    first + value.value
}
"#;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("closure_view_param_call.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let diags = format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    );
    assert!(diags.is_empty(), "{diags}");

    let normalized = normalized_func_body(&db, top_mod, "read_twice");
    let pack_field_mode = normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
                expr: NExpr::AggregateMake { ty, fields },
                ..
            } if ty.is_tuple(&db) => fields.first().map(|field| field.mode),
            _ => None,
        })
        .expect("closure call argument-pack field");
    let call_pack_mode = normalized
        .blocks
        .iter()
        .flat_map(|block| block.stmts.iter())
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign {
                expr: NExpr::Call { callee, args, .. },
                ..
            } if matches!(callee.key.owner(&db), BodyOwner::Closure { .. }) => {
                args.get(1).map(|arg| arg.mode)
            }
            _ => None,
        })
        .expect("closure call argument pack");
    assert_eq!(pack_field_mode, ReadMode::Read);
    assert_eq!(call_pack_mode, ReadMode::Move);
}

#[test]
fn closure_multiple_mut_args_reject_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let write = |left: mut u256, right: mut u256| {
        left = 1
        right = 2
    }
    write.call(mut value, mut value)
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_mut_and_ref_args_reject_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let mut value = 0
    let write_and_read = |write: mut u256, read: ref u256| -> u256 {
        write = 1
        read
    }
    write_and_read.call(mut value, ref value)
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_multiple_ref_args_allow_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let value = 21
    let add = |left: ref u256, right: ref u256| -> u256 { left + right }
    add.call(ref value, ref value)
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_multiple_inferred_view_args_allow_aliasing_noncopy_place() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 21 }
    let add = |left, right| left.value + right.value
    add.call(value, value)
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_inferred_mut_and_view_args_reject_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let mut value = 0
    let write_and_read = |write: mut, read| -> u256 {
        write = 1
        read
    }
    write_and_read.call(mut value, value)
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_inferred_owned_and_view_args_reject_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let consume = |owned: own, borrowed| -> u256 {
        owned.value + borrowed.value
    }
    consume.call_once(value, value)
}
"#,
    );
    assert!(
        diags.contains("move conflict in `fn probe`")
            || diags.contains("borrow conflict in `fn probe`"),
        "{diags}"
    );
}

#[test]
fn closure_owned_then_borrowed_arg_rejects_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let consume = |owned: own Boxed, borrowed: ref Boxed| -> u256 {
        owned.value + borrowed.value
    }
    consume.call_once(value, ref value)
}
"#,
    );
    assert!(
        diags.contains("move conflict in `fn probe`")
            || diags.contains("borrow conflict in `fn probe`"),
        "{diags}"
    );
}

#[test]
fn closure_borrowed_then_owned_arg_rejects_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let consume = |borrowed: ref Boxed, owned: own Boxed| -> u256 {
        borrowed.value + owned.value
    }
    consume.call_once(ref value, value)
}
"#,
    );
    assert!(
        diags.contains("move conflict in `fn probe`")
            || diags.contains("borrow conflict in `fn probe`"),
        "{diags}"
    );
}

#[test]
fn closure_argument_pack_preserves_disjoint_caller_field_paths() {
    let allowed = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Values {
    owned: Boxed,
    borrowed: Boxed,
    unrelated: Boxed,
}

fn probe(_ values: own Values) -> u256 {
    let select = |owned: own Boxed, borrowed: Boxed| -> view Boxed {
        owned.value
        borrowed
    }
    let returned = select.call_once(values.owned, values.borrowed)
    let moved = values.unrelated
    returned.value + moved.value
}
"#,
    );
    assert!(allowed.is_empty(), "{allowed}");

    let conflicting = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Values {
    owned: Boxed,
    borrowed: Boxed,
}

fn probe(_ values: own Values) -> u256 {
    let select = |owned: own Boxed, borrowed: Boxed| -> view Boxed {
        owned.value
        borrowed
    }
    let returned = select.call_once(values.owned, values.borrowed)
    let moved = values.borrowed
    returned.value + moved.value
}
"#,
    );
    assert!(
        conflicting.contains("borrow conflict in `fn probe`"),
        "{conflicting}"
    );
}

#[test]
fn closure_multiple_mut_args_allow_distinct_places() {
    let diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let mut left = 0
    let mut right = 0
    let write = |left: mut u256, right: mut u256| {
        left = 20
        right = 22
    }
    write.call(mut left, mut right)
    left + right
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_returned_first_mut_arg_retains_only_first_provenance() {
    let selected_diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let first = |left: mut u256, right: mut u256| -> mut u256 { left }
    let returned = first.call(mut left, mut right)
    let other = mut left
    other = 1
    returned = 2
}
"#,
    );
    assert!(
        selected_diags.contains("borrow conflict in `fn probe`"),
        "{selected_diags}"
    );

    let unrelated_diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let first = |left: mut u256, right: mut u256| -> mut u256 { left }
    let returned = first.call(mut left, mut right)
    let other = mut right
    other = 1
    returned = 2
}
"#,
    );
    assert!(unrelated_diags.is_empty(), "{unrelated_diags}");
}

#[test]
fn closure_returned_last_mut_arg_retains_only_last_provenance() {
    let selected_diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let last = |left: mut u256, right: mut u256| -> mut u256 { right }
    let returned = last.call(mut left, mut right)
    let other = mut right
    other = 1
    returned = 2
}
"#,
    );
    assert!(
        selected_diags.contains("borrow conflict in `fn probe`"),
        "{selected_diags}"
    );

    let unrelated_diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let last = |left: mut u256, right: mut u256| -> mut u256 { right }
    let returned = last.call(mut left, mut right)
    let other = mut left
    other = 1
    returned = 2
}
"#,
    );
    assert!(unrelated_diags.is_empty(), "{unrelated_diags}");
}

#[test]
fn erased_closure_arguments_do_not_shift_returned_borrow_provenance() {
    let allowed = borrow_diags(
        r#"
struct Marker {}

fn probe() {
    let mut left = 0
    let mut right = 0
    let pick_right = |
        _ first: own Marker,
        left: mut u256,
        _ middle: own Marker,
        right: mut u256,
        _ last: own Marker,
    | -> mut u256 { right }
    let returned = pick_right.call(
        Marker {},
        mut left,
        Marker {},
        mut right,
        Marker {},
    )
    let other = mut left
    other = 1
    returned = 2
}
"#,
    );
    assert!(allowed.is_empty(), "{allowed}");

    let conflicting = borrow_diags(
        r#"
struct Marker {}

fn probe() {
    let mut left = 0
    let mut right = 0
    let pick_right = |
        _ first: own Marker,
        left: mut u256,
        _ middle: own Marker,
        right: mut u256,
        _ last: own Marker,
    | -> mut u256 { right }
    let returned = pick_right.call(
        Marker {},
        mut left,
        Marker {},
        mut right,
        Marker {},
    )
    let other = mut right
    other = 1
    returned = 2
}
"#,
    );
    assert!(
        conflicting.contains("borrow conflict in `fn probe`"),
        "{conflicting}"
    );
}

#[test]
fn inferred_closure_return_retains_selected_mut_arg_provenance() {
    let selected_diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let first = |left: mut, right: mut| left
    let returned = first.call(mut left, mut right)
    let other = mut left
    other = 1
    returned = 2
}
"#,
    );
    assert!(
        selected_diags.contains("borrow conflict in `fn probe`"),
        "{selected_diags}"
    );

    let unrelated_diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let first = |left: mut, right: mut| left
    let returned = first.call(mut left, mut right)
    let other = mut right
    other = 1
    returned = 2
}
"#,
    );
    assert!(unrelated_diags.is_empty(), "{unrelated_diags}");
}

#[test]
fn closure_mut_parameter_reborrow_suspends_and_releases_its_parent() {
    let sequential_diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let parent = mut value
    let reborrow = |target: mut u256| -> mut u256 { mut target }
    let first = reborrow.call(parent)
    first = 1
    let second = reborrow.call(parent)
    second = 2
}
"#,
    );
    assert!(sequential_diags.is_empty(), "{sequential_diags}");

    let overlapping_diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let parent = mut value
    let reborrow = |target: mut u256| -> mut u256 { mut target }
    let first = reborrow.call(parent)
    let second = reborrow.call(parent)
    first = 1
    second = 2
}
"#,
    );
    assert!(
        overlapping_diags.contains("borrow conflict in `fn probe`"),
        "{overlapping_diags}"
    );
}

#[test]
fn inferred_closure_return_retains_default_view_arg_provenance() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let identity = |value| value
    let returned = identity.call(value)
    let moved = value
    returned.value + moved.value
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn looped_closure_calls_treat_single_use_owned_rvalues_as_fresh_allocations() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn probe() {
    let borrow = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let mut returned = Both {
        left: borrow.call(Boxed { value: 0 }),
        right: borrow.call(Boxed { value: 0 }),
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow.call(Boxed { value: 0 })
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_closure_calls_treat_new_owned_bindings_as_fresh_allocations() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn probe() {
    let borrow = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let mut returned = Both {
        left: borrow.call(Boxed { value: 0 }),
        right: borrow.call(Boxed { value: 0 }),
    }
    let mut index: usize = 0
    while index < 2 {
        let boxed = Boxed { value: 0 }
        let next = borrow.call(boxed)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_closure_calls_preserve_freshness_through_field_projection() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Wrapped {
    boxed: mut Boxed,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn probe() {
    let allocate = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let mut returned = Both {
        left: allocate.call(Boxed { value: 0 }),
        right: allocate.call(Boxed { value: 0 }),
    }
    let mut index: usize = 0
    while index < 2 {
        let wrapped = Wrapped {
            boxed: allocate.call(Boxed { value: 0 }),
        }
        let next = wrapped.boxed
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_local_borrows_treat_each_binding_execution_as_fresh_storage() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let mut boxed = Boxed { value: 0 }
        let next = mut boxed.value
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_owned_scalar_closure_arguments_get_fresh_storage_per_call() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let borrow = |mut value: own u256| -> mut u256 { mut value }
    let mut returned = Both {
        left: borrow.call(0),
        right: borrow.call(0),
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow.call(0)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_named_copy_arguments_get_fresh_owned_parameter_storage_per_call() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let borrow = |mut value: own u256| -> mut u256 { mut value }
    let seed: u256 = 0
    let mut returned = Both {
        left: borrow.call(seed),
        right: borrow.call(seed),
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow.call(seed)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_projected_copy_arguments_get_fresh_owned_parameter_storage_per_call() {
    let diags = borrow_diags(
        r#"
struct SeedValue {
    value: u256,
}

struct Seed {
    nested: SeedValue,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let borrow = |mut value: own u256| -> mut u256 { mut value }
    let seed = Seed {
        nested: SeedValue { value: 0 },
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow.call(seed.nested.value)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn fresh_owned_aggregate_fields_do_not_inherit_embedded_borrow_provenance() {
    let diags = borrow_diags(
        r#"
struct Carrier {
    value: u256,
    named: mut u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let borrow_value = |mut carrier: own Carrier| -> mut u256 { mut carrier.value }
    let mut target = 0
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow_value.call(Carrier {
            value: 0,
            named: mut target,
        })
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_owned_aggregate_arguments_preserve_embedded_named_borrow_provenance() {
    let diags = borrow_diags(
        r#"
struct Carrier {
    value: u256,
    named: mut u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let unwrap = |carrier: own Carrier| -> mut u256 { carrier.named }
    let mut target = 0
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let next = unwrap.call(Carrier {
            value: 0,
            named: mut target,
        })
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn looped_dynamic_owned_array_arguments_use_fresh_parameter_storage() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let borrow_at = |mut values: own [u256; 2], index: own usize| -> mut u256 {
        mut values[index]
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow_at.call([0 as u256, 0], index)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_owned_enum_payloads_use_fresh_parameter_storage() {
    let diags = borrow_diags(
        r#"
enum Value {
    Present(u256),
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let borrow = |mut value: own Value| -> mut u256 {
        match value {
            Value::Present(mut inner) => mut inner,
        }
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow.call(Value::Present(0))
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_dynamic_array_element_borrows_get_fresh_storage_per_binding() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let mut values = [0 as u256, 0]
        let next = mut values[index]
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_local_borrows_from_one_execution_still_conflict() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let mut index: usize = 0
    while index < 1 {
        let mut boxed = Boxed { value: 0 }
        let first = mut boxed.value
        let second = mut boxed.value
        first = 1
        second = 2
        index += 1
    }
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn looped_fresh_local_cannot_be_read_while_current_borrow_is_live() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let mut index: usize = 0
    while index < 1 {
        let mut boxed = Boxed { value: 0 }
        let borrowed = mut boxed.value
        let copied = boxed.value
        borrowed = copied
        index += 1
    }
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn looped_reassignment_does_not_create_fresh_local_storage() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut boxed = Boxed { value: 0 }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        boxed = Boxed { value: 0 }
        let next = mut boxed.value
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn looped_aggregate_reassignment_does_not_hide_duplicate_embedded_mut_borrows() {
    let diags = borrow_diags(
        r#"
struct Mixed {
    value: u256,
    borrowed: mut u256,
}

fn probe() {
    let allocate = |mut value: own u256| -> mut u256 { mut value }
    let mut index: usize = 0
    while index < 1 {
        let mut mixed = Mixed {
            value: 0,
            borrowed: allocate.call(0),
        }
        mixed = Mixed {
            value: 1,
            borrowed: allocate.call(0),
        }
        let first = mixed.borrowed
        let second = mixed.borrowed
        first = 1
        second = 2
        index += 1
    }
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn looped_aggregate_reassignment_tracks_the_new_embedded_fresh_borrow() {
    let diags = borrow_diags(
        r#"
struct Mixed {
    value: u256,
    borrowed: mut u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let allocate = |mut value: own u256| -> mut u256 { mut value }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let mut mixed = Mixed {
            value: 0,
            borrowed: allocate.call(0),
        }
        mixed = Mixed {
            value: 1,
            borrowed: allocate.call(0),
        }
        let next = mixed.borrowed
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn inner_loop_reuses_storage_allocated_by_the_outer_iteration() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut outer: usize = 0
    while outer < 1 {
        let mut boxed = Boxed { value: 0 }
        let mut initial_left = 0
        let mut initial_right = 0
        let mut returned = Both {
            left: mut initial_left,
            right: mut initial_right,
        }
        let mut inner: usize = 0
        while inner < 2 {
            let next = mut boxed.value
            returned = if inner == 0 {
                Both { left: next, right: returned.right }
            } else {
                Both { left: returned.left, right: next }
            }
            inner += 1
        }
        returned.left = 1
        returned.right = 2
        outer += 1
    }
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn nested_loop_local_borrows_get_fresh_storage_per_inner_iteration() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut outer: usize = 0
    while outer < 1 {
        let mut inner: usize = 0
        while inner < 2 {
            let mut boxed = Boxed { value: 0 }
            let next = mut boxed.value
            returned = if inner == 0 {
                Both { left: next, right: returned.right }
            } else {
                Both { left: returned.left, right: next }
            }
            inner += 1
        }
        outer += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn continue_edges_age_retained_borrows_before_the_next_local_allocation() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let mut value = 0
        let next = mut value
        if index == 0 {
            returned = Both { left: next, right: returned.right }
            index += 1
            continue
        }
        returned = Both { left: returned.left, right: next }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn for_loop_backedges_age_retained_borrows_before_the_next_local_allocation() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    for index in [0 as usize, 1] {
        let mut value = 0
        let next = mut value
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn while_let_condition_calls_allocate_fresh_owned_parameter_storage() {
    let diags = borrow_diags(
        r#"
use core::option::Option::{self, Some}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let make = |mut value: own u256, index: own usize| -> Option<mut u256> {
        if index < 2 {
            Some(mut value)
        } else {
            Option::None
        }
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while let Some(next) = make.call(0, index) {
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_fresh_local_provenance_survives_nested_closure_storage() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let make = |target: mut u256| {
        let get = || -> mut u256 { target }
        get
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut functions = [
        make.call(mut initial_left),
        make.call(mut initial_right),
    ]
    let mut index: usize = 0
    while index < 2 {
        let mut value = 0
        let next = make.call(mut value)
        if index == 0 {
            functions[0] = next
        } else {
            functions[1] = next
        }
        index += 1
    }
    let left = functions[0].call()
    let right = functions[1].call()
    left = 1
    right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn copied_fresh_nested_closure_storage_retains_alias_identity() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let make = |target: mut u256| {
        let get = || -> mut u256 { target }
        get
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut functions = [
        make.call(mut initial_left),
        make.call(mut initial_right),
    ]
    let mut index: usize = 0
    while index < 1 {
        let mut value = 0
        let next = make.call(mut value)
        functions[0] = next
        functions[1] = next
        index += 1
    }
    let left = functions[0].call()
    let right = functions[1].call()
    left = 1
    right = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn looped_fresh_nested_closure_provenance_survives_aggregate_reconstruction() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let make = |target: mut u256| {
        let get = || -> mut u256 { target }
        get
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut functions = [
        make.call(mut initial_left),
        make.call(mut initial_right),
    ]
    let mut index: usize = 0
    while index < 2 {
        let mut value = 0
        let next = make.call(mut value)
        functions = if index == 0 {
            [next, functions[1]]
        } else {
            [functions[0], next]
        }
        index += 1
    }
    let left = functions[0].call()
    let right = functions[1].call()
    left = 1
    right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_owned_parameter_provenance_survives_nested_closure_storage() {
    let diags = analysis_diags(
        r#"
fn probe() {
    let make = |mut value: own u256| {
        let target = mut value
        let get = || -> mut u256 { target }
        get
    }
    let mut functions = [
        make.call(0),
        make.call(0),
    ]
    let mut index: usize = 0
    while index < 2 {
        let next = make.call(0)
        if index == 0 {
            functions[0] = next
        } else {
            functions[1] = next
        }
        index += 1
    }
    let left = functions[0].call()
    let right = functions[1].call()
    left = 1
    right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_fresh_nested_closure_provenance_survives_nested_field_storage() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let make = |target: mut u256| {
        let get = || -> mut u256 { target }
        get
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut functions = [
        (make.call(mut initial_left), 0 as u256),
        (make.call(mut initial_right), 0 as u256),
    ]
    let mut index: usize = 0
    while index < 2 {
        let mut value = 0
        let next = make.call(mut value)
        if index == 0 {
            functions[0].0 = next
            functions[0].1 = 1
        } else {
            functions[1].0 = next
            functions[1].1 = 2
        }
        index += 1
    }
    let left = functions[0].0.call()
    let right = functions[1].0.call()
    left = functions[0].1
    right = functions[1].1
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_fresh_nested_closure_provenance_survives_enum_storage() {
    let diags = borrow_diags(
        r#"
use core::option::Option::Some

fn probe() {
    let make = |target: mut u256| {
        let get = || -> mut u256 { target }
        get
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut functions = [
        Some(make.call(mut initial_left)),
        Some(make.call(mut initial_right)),
    ]
    let mut index: usize = 0
    while index < 2 {
        let mut value = 0
        let next = make.call(mut value)
        if index == 0 {
            functions[0] = Some(next)
        } else {
            functions[1] = Some(next)
        }
        index += 1
    }
    let Some(left_function) = functions[0]
    let Some(right_function) = functions[1]
    let left = left_function.call()
    let right = right_function.call()
    left = 1
    right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn repeated_fresh_nested_closure_storage_retains_alias_identity() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let make = |target: mut u256| {
        let get = || -> mut u256 { target }
        get
    }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut functions = [
        make.call(mut initial_left),
        make.call(mut initial_right),
    ]
    let mut index: usize = 0
    while index < 1 {
        let mut value = 0
        let function = make.call(mut value)
        functions = [function; 2]
        index += 1
    }
    let left = functions[0].call()
    let right = functions[1].call()
    left = 1
    right = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn mutually_exclusive_borrows_share_one_fresh_local_generation() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let mut boxed = Boxed { value: 0 }
        let next = if index == 0 {
            mut boxed.value
        } else {
            mut boxed.value
        }
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn disjoint_borrows_share_one_fresh_local_generation() {
    let diags = borrow_diags(
        r#"
struct Pair {
    left: u256,
    right: u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let mut pair = Pair { left: 0, right: 0 }
        let next_left = mut pair.left
        let next_right = mut pair.right
        returned = if index == 0 {
            Both { left: next_left, right: returned.right }
        } else {
            Both { left: returned.left, right: next_right }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_closure_calls_preserve_freshness_through_nested_forwarding() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn probe() {
    let allocate = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let forward = |boxed: mut Boxed| -> mut Boxed { mut boxed }
    let mut returned = Both {
        left: allocate.call(Boxed { value: 0 }),
        right: allocate.call(Boxed { value: 0 }),
    }
    let mut index: usize = 0
    while index < 2 {
        let next = forward.call(allocate.call(Boxed { value: 0 }))
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_closure_calls_preserve_freshness_through_control_flow_forwarding() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn probe() {
    let allocate = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let forward = |boxed: mut Boxed| -> mut Boxed { mut boxed }
    let mut returned = Both {
        left: allocate.call(Boxed { value: 0 }),
        right: allocate.call(Boxed { value: 0 }),
    }
    let mut index: usize = 0
    while index < 2 {
        let next = forward.call(if true {
            allocate.call(Boxed { value: 0 })
        } else {
            allocate.call(Boxed { value: 0 })
        })
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_closure_calls_track_multiple_fresh_owned_arguments_independently() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn probe() {
    let borrow_both = |mut left: own Boxed, mut right: own Boxed| -> Both {
        Both { left: mut left, right: mut right }
    }
    let mut returned = borrow_both.call(
        Boxed { value: 0 },
        Boxed { value: 0 },
    )
    let mut index: usize = 0
    while index < 2 {
        let next = borrow_both.call(
            Boxed { value: 0 },
            Boxed { value: 0 },
        )
        returned = if index == 0 {
            Both { left: next.left, right: returned.right }
        } else {
            Both { left: returned.left, right: next.right }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn copied_source_passed_to_multiple_owned_params_gets_distinct_parameter_storage() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let borrow_both = |mut left: own u256, mut right: own u256| -> Both {
        Both { left: mut left, right: mut right }
    }
    let seed: u256 = 0
    let returned = borrow_both.call(seed, seed)
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn fresh_aggregate_arguments_do_not_freshen_embedded_named_borrows() {
    let diags = borrow_diags(
        r#"
struct Carrier {
    handle: mut u256,
}

fn probe() {
    let unwrap = |carrier: own Carrier| -> mut u256 { carrier.handle }
    let mut target = 0
    let first = unwrap.call(Carrier { handle: mut target })
    let second = unwrap.call(Carrier { handle: mut target })
    first = 1
    second = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn mixed_fresh_and_named_closure_results_preserve_named_conflicts() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Mixed {
    fresh: mut Boxed,
    named: mut Boxed,
}

fn probe() {
    let combine = |mut fresh: own Boxed, named: mut Boxed| -> Mixed {
        Mixed { fresh: mut fresh, named }
    }
    let mut target = Boxed { value: 0 }
    let first = combine.call(Boxed { value: 0 }, mut target)
    let second = combine.call(Boxed { value: 0 }, mut target)
    first.named.value = 1
    second.named.value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn fresh_root_renewal_requires_all_same_site_sources_to_be_current() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let allocate = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let reborrow = |boxed: mut Boxed| -> mut Boxed { mut boxed }
    let choose = |pick_current: own bool, current: mut Boxed, old: mut Boxed| {
        if pick_current { current } else { old }
    }
    let mut retained = allocate.call(Boxed { value: 0 })
    let mut index: usize = 0
    while index < 2 {
        let current = allocate.call(Boxed { value: 0 })
        if index == 0 {
            retained = current
        } else {
            let existing = reborrow.call(retained)
            let selected = choose.call(false, current, retained)
            existing.value = 1
            selected.value = 2
        }
        index += 1
    }
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn ordinary_function_calls_share_fresh_argument_allocation_semantics() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Both {
    left: mut Boxed,
    right: mut Boxed,
}

fn allocate(mut boxed: own Boxed) -> mut Boxed {
    mut boxed
}

fn probe() {
    let mut returned = Both {
        left: allocate(Boxed { value: 0 }),
        right: allocate(Boxed { value: 0 }),
    }
    let mut index: usize = 0
    while index < 2 {
        let next = allocate(Boxed { value: 0 })
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left.value = 1
    returned.right.value = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn ordinary_owned_function_params_copy_projected_values_into_fresh_storage() {
    let diags = borrow_diags(
        r#"
struct Seed {
    value: u256,
}

struct Both {
    left: mut u256,
    right: mut u256,
}

fn borrow(mut value: own u256) -> mut u256 {
    mut value
}

fn probe() {
    let seed = Seed { value: 0 }
    let mut initial_left = 0
    let mut initial_right = 0
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let next = borrow(seed.value)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn looped_closure_calls_do_not_treat_named_reborrowed_places_as_fresh_allocations() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut target = 0
    let mut initial_left = 0
    let mut initial_right = 0
    let reborrow = |value: mut u256| -> mut u256 { mut value }
    let mut returned = Both {
        left: mut initial_left,
        right: mut initial_right,
    }
    let mut index: usize = 0
    while index < 2 {
        let next = reborrow.call(mut target)
        returned = if index == 0 {
            Both { left: next, right: returned.right }
        } else {
            Both { left: returned.left, right: next }
        }
        index += 1
    }
    returned.left = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn reusing_one_fresh_call_result_does_not_create_new_allocations() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let allocate = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let reborrow = |boxed: mut Boxed| -> mut Boxed { mut boxed }
    let allocated = allocate.call(Boxed { value: 0 })
    let first = reborrow.call(allocated)
    let second = reborrow.call(allocated)
    first.value = 1
    second.value = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn reusing_one_fresh_call_result_does_not_hide_mutating_call_conflicts() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let allocate = |mut boxed: own Boxed| -> mut Boxed { mut boxed }
    let reborrow = |boxed: mut Boxed| -> mut Boxed { mut boxed }
    let write = |boxed: mut Boxed| {
        boxed.value = 2
    }
    let allocated = allocate.call(Boxed { value: 0 })
    let first = reborrow.call(allocated)
    write.call(allocated)
    first.value = 1
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_view_of_rvalue_cannot_escape_the_callers_frame() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn bad() -> view Boxed {
    let identity = |value| value
    identity.call(Boxed { value: 42 })
}
"#,
    );
    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags}"
    );
}

#[test]
fn closure_environment_view_cannot_escape_the_callers_frame() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn bad() -> view Boxed {
    let boxed = Boxed { value: 42 }
    let project = || -> view Boxed { boxed }
    project.call()
}

fn bad_immediate() -> view Boxed {
    let boxed = Boxed { value: 42 }
    (|| -> view Boxed { boxed }).call()
}
"#,
    );
    assert_eq!(diags.matches("invalid return borrow").count(), 2, "{diags}");
}

#[test]
fn aggregate_holding_closure_view_of_rvalue_cannot_escape_the_callers_frame() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Borrowed {
    value: view Boxed,
}

fn bad() -> Borrowed {
    let identity = |value| value
    Borrowed { value: identity.call(Boxed { value: 42 }) }
}
"#,
    );
    assert!(
        diags.contains("invalid return borrow in `fn bad`"),
        "{diags}"
    );
}

#[test]
fn consuming_closure_may_return_borrow_from_owned_argument() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let borrow = |value: own Boxed| -> ref u256 { ref value.value }
    let returned = borrow.call_once(Boxed { value: 42 })
    returned
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn consuming_closure_may_return_aggregate_borrow_from_owned_argument() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Borrowed {
    value: ref u256,
}

fn probe() -> u256 {
    let borrow = |value: own Boxed| -> Borrowed {
        Borrowed { value: ref value.value }
    }
    let returned = borrow.call_once(Boxed { value: 42 })
    returned.value
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn returned_borrow_does_not_restore_moved_closure_argument() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe(_ value: own Boxed) -> u256 {
    let borrow = |argument: own Boxed| -> ref u256 { ref argument.value }
    let returned = borrow.call_once(value)
    value.value
    returned
}
"#,
    );
    assert!(
        diags.contains("cannot use a value after it was moved"),
        "{diags}"
    );
}

#[test]
fn layout_backing_does_not_let_borrows_of_fresh_fields_escape() {
    for (owner, src) in [
        (
            "function",
            r#"
struct Boxed {
    value: u256,
}

fn probe(_ input: own Boxed) -> ref u256 {
    let local = Boxed { value: input.value }
    ref local.value
}
"#,
        ),
        (
            "closure",
            r#"
struct Boxed {
    value: u256,
}

fn probe(_ input: own Boxed) -> ref u256 {
    let borrow_fresh = |value: own Boxed| -> ref u256 {
        let local = Boxed { value: value.value }
        ref local.value
    }
    borrow_fresh.call_once(input)
}
"#,
        ),
    ] {
        let diags = borrow_diags(src);
        assert!(
            diags.contains("cannot return a borrow to local `local`"),
            "{owner}: {diags}"
        );
    }

    let aggregate_shape_diags = borrow_diags(
        r#"
use core::option::Option::{self, Some}

struct Boxed {
    value: u256,
}

fn tuple_field(_ input: u256) -> ref u256 {
    let local = (input,)
    ref local.0
}

fn array_element(_ input: u256) -> ref u256 {
    let local = [input]
    ref local[0]
}

fn nested_field(_ input: own Boxed) -> ref u256 {
    let local = (Boxed { value: input.value },)
    ref local.0.value
}

fn enum_field(_ input: u256) -> ref u256 {
    let local = Some(input)
    match local {
        Some(value) => ref value
        Option::None => ref input
    }
}
"#,
    );
    assert_eq!(
        aggregate_shape_diags
            .matches("cannot return a borrow to local `local`")
            .count(),
        4,
        "{aggregate_shape_diags}"
    );

    let capability_field_diags = borrow_diags(
        r#"
struct Holder {
    value: ref u256,
}

fn probe(_ input: u256) -> ref u256 {
    let local = Holder { value: ref input }
    local.value
}
"#,
    );
    assert!(
        capability_field_diags.is_empty(),
        "{capability_field_diags}"
    );
}

#[test]
fn inferred_view_closure_payload_operations_do_not_return_local_borrows() {
    for (kind, src) in [
        (
            "contextual identity",
            r#"
use core::functional::Fn

fn apply<T, F: Fn<(view T,), T>>(_ func: F, _ value: own T) -> T {
    func.call(value)
}

fn probe() -> u256 {
    apply(|value| value, 42 as u256)
}
"#,
        ),
        (
            "arithmetic",
            r#"
fn probe() -> u256 {
    (|value| value + 1 as u256).call(41 as u256)
}
"#,
        ),
        (
            "field",
            r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let field = |value| value.value
    field.call(Boxed { value: 42 })
}
"#,
        ),
        (
            "cast",
            r#"
fn probe() -> u256 {
    let cast = |value| value as u256
    cast.call(42 as u8)
}
"#,
        ),
        (
            "unary",
            r#"
fn probe() -> bool {
    let invert = |value| !value
    invert.call(false)
}
"#,
        ),
        (
            "index",
            r#"
fn probe() -> u256 {
    let at = |values| values[0]
    at.call([42 as u256, 0])
}
"#,
        ),
        (
            "higher order",
            r#"
fn probe() -> u256 {
    let call_with = |function, value| function.call(value)
    call_with.call(|value| value + 1 as u256, 41 as u256)
}
"#,
        ),
        (
            "for loop",
            r#"
fn probe() -> u256 {
    let sum_values = |values| -> u256 {
        let mut total: u256 = 0
        for value in values {
            total += value
        }
        total
    }
    sum_values.call([20 as u256, 22 as u256])
}
"#,
        ),
        (
            "conditional",
            r#"
fn probe() -> u256 {
    let branch = |value| -> u256 { if true { value } else { 0 as u256 } }
    branch.call(42 as u256)
}
"#,
        ),
        (
            "match",
            r#"
fn probe() -> u256 {
    let branch = |value| -> u256 {
        match true {
            true => value
            false => 0 as u256
        }
    }
    branch.call(42 as u256)
}
"#,
        ),
        (
            "tuple destructure",
            r#"
fn probe() -> u256 {
    let unpack = |pair| {
        let (left, right) = pair
        left + right
    }
    unpack.call((20 as u256, 22 as u256))
}
"#,
        ),
        (
            "named view field",
            r#"
struct view {
    value: u256,
}

fn probe() -> u256 {
    let field = |value: view view| value.value
    field.call(view { value: 42 })
}
"#,
        ),
        (
            "nested capture",
            r#"
fn probe() -> u256 {
    let outer = |left| {
        let inner = |right| left + right
        inner.call(22 as u256)
    }
    outer.call(20 as u256)
}
"#,
        ),
    ] {
        let diags = borrow_diags(src);
        assert!(diags.is_empty(), "{kind}: {diags}");
    }
}

#[test]
fn contextual_closure_result_cannot_own_noncopy_view_parameter() {
    let diags = analysis_diags(
        r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

fn duplicate<F: Fn<(view Boxed,), Boxed>>(
    _ function: F,
    _ value: own Boxed,
) -> u256 {
    let returned = function.call(value)
    returned.value + value.value
}

fn probe() -> u256 {
    duplicate(|value| value, Boxed { value: 21 })
}
"#,
    );
    assert!(
        diags.contains(
            "expected `semantic_borrowck::Boxed`, but `view semantic_borrowck::Boxed` is given"
        ),
        "{diags}"
    );
}

#[test]
fn closure_default_view_param_cannot_return_owned_whole_value() {
    let diags = analysis_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> Boxed {
    let identity = |value: Boxed| -> Boxed { value }
    identity.call(Boxed { value: 42 })
}
"#,
    );
    assert!(
        diags.contains(
            "expected `semantic_borrowck::Boxed`, but `view semantic_borrowck::Boxed` is given"
        ),
        "{diags}"
    );
}

#[test]
fn specialized_copy_capture_preserves_generic_move_semantics() {
    let diags = borrow_diags(
        r#"
fn passthrough<T>(_ value: own T) -> T {
    let take = || value
    take.call_once()
}

fn probe() -> u256 {
    passthrough(42 as u256)
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn reusable_closure_projected_from_aggregate_is_not_moved_by_call() {
    let diags = borrow_diags(
        r#"
struct Holder<F> {
    function: F,
}

fn probe() -> u256 {
    let value: u256 = 21
    let shared = ref value
    let read = || shared
    let holder = Holder { function: read }
    holder.function.call() + holder.function.call()
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn view_argument_projected_from_owned_aggregate_is_not_moved_by_call() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Holder {
    item: Boxed,
}

fn read(item: Boxed) -> u256 {
    item.value
}

fn probe() -> u256 {
    let holder = Holder {
        item: Boxed { value: 21 },
    }
    read(item: holder.item) + read(item: holder.item)
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn inferred_copy_view_control_flow_join_materializes_values() {
    for src in [
        r#"
fn probe() {
    let branch = |value| if true { value } else { 0 as u256 }
    branch.call(42 as u256)
}
"#,
        r#"
fn probe() {
    let branch = |value| match true {
        true => value
        false => 0 as u256
    }
    branch.call(42 as u256)
}
"#,
    ] {
        let diags = borrow_diags(src);
        assert!(diags.is_empty(), "{diags}");
    }
}

#[test]
fn inferred_noncopy_view_known_control_flow_ignores_unreachable_temporary_views() {
    for body in [
        "if true { value } else { Boxed { value: 0 } }",
        "match true { true => value, false => Boxed { value: 0 } }",
    ] {
        let diags = borrow_diags(&format!(
            r#"
struct Boxed {{
    value: u256,
}}

fn probe() {{
    let branch = |value| {body}
    branch.call(Boxed {{ value: 42 }})
}}
"#,
        ));
        assert!(diags.is_empty(), "{body}: {diags}");
    }
}

#[test]
fn inferred_noncopy_view_dynamic_control_flow_cannot_return_a_temporary_view() {
    for body in [
        "if flag { value } else { Boxed { value: 0 } }",
        "match flag { true => value, false => Boxed { value: 0 } }",
    ] {
        let diags = borrow_diags(&format!(
            r#"
struct Boxed {{
    value: u256,
}}

fn probe(_ flag: bool) {{
    let branch = |value, flag: own| {body}
    branch.call(Boxed {{ value: 42 }}, flag)
}}
"#,
        ));
        assert!(
            diags.contains("invalid return borrow in `fn <closure>`"),
            "{body}: {diags}"
        );
    }
}

#[test]
fn returned_view_from_function_retains_argument_provenance() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn identity(_ value: view Boxed) -> view Boxed {
    value
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let returned = identity(value)
    let moved = value
    returned.value + moved.value
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn contextual_views_of_owned_rvalues_materialize_stable_backing_values() {
    let src = r#"
fn value() -> u256 {
    42
}

fn probe() -> u256 {
    let from_call: view u256 = value()
    let from_binary: view u256 = 20 + 22
    from_call + from_binary
}
"#;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("contextual_view_rvalue.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    let diags = format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    );
    assert!(diags.is_empty(), "{diags}");

    let normalized = normalized_func_body(&db, top_mod, "probe");
    let call_result = normalized
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| match stmt.kind {
            NSStmtKind::Assign {
                dst,
                expr: NExpr::Call { .. },
            } => Some(dst),
            _ => None,
        })
        .expect("owned call result");
    assert_eq!(
        normalized.locals[call_result.index()].ty.pretty_print(&db),
        "u256"
    );
    assert!(
        normalized
            .blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .any(|stmt| matches!(
                &stmt.kind,
                NSStmtKind::Assign {
                    expr: NExpr::ReadPlace {
                        place:
                            NSPlace {
                                root: NSPlaceRoot::Root(root),
                                ..
                            },
                        ..
                    },
                    ..
                } if matches!(
                    normalized.root(*root),
                    Some(NBorrowRoot::LocalSlot { local }) if *local == call_result
                )
            )),
        "the contextual view must read from the separately materialized owned result"
    );

    let invalid_return_diags = borrow_diags(
        r#"
fn value() -> u256 {
    42
}

fn bad() -> view u256 {
    value()
}
"#,
    );
    assert!(
        invalid_return_diags.contains("invalid return borrow in `fn bad`"),
        "{invalid_return_diags}"
    );
}

#[test]
fn implicit_view_aggregate_field_retains_argument_provenance() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Holder {
    value: view Boxed,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let holder = Holder { value }
    let moved = value
    holder.value.value + moved.value
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn implicit_view_tuple_array_and_enum_fields_retain_argument_provenance() {
    for (kind, src) in [
        (
            "tuple",
            r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let holder: (view Boxed,) = (value,)
    let moved = value
    holder.0.value + moved.value
}
"#,
        ),
        (
            "array repeat",
            r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let holder: [view Boxed; 2] = [value; 2]
    let moved = value
    holder[0].value + moved.value
}
"#,
        ),
        (
            "enum",
            r#"
struct Boxed {
    value: u256,
}

enum Holder {
    Value(view Boxed),
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let holder = Holder::Value(value)
    let moved = value
    match holder {
        Holder::Value(borrowed) => borrowed.value + moved.value,
    }
}
"#,
        ),
        (
            "record enum",
            r#"
struct Boxed {
    value: u256,
}

enum Holder {
    Value { value: view Boxed },
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let holder = Holder::Value { value }
    let moved = value
    match holder {
        Holder::Value { value: borrowed } => borrowed.value + moved.value,
    }
}
"#,
        ),
    ] {
        let diags = borrow_diags(src);
        assert!(
            diags.contains("borrow conflict in `fn probe`"),
            "{kind}: {diags}"
        );
    }
}

#[test]
fn owned_tuple_and_array_call_arguments_receive_nested_view_context() {
    let array_allowed = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn first(_ values: own [view Boxed; 2]) -> view Boxed {
    values[0]
}

fn probe() -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    let returned = first([left, right])
    let moved = right
    returned.value + moved.value
}
"#,
    );
    assert!(array_allowed.is_empty(), "{array_allowed}");

    let array_conflict = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn first(_ values: own [view Boxed; 2]) -> view Boxed {
    values[0]
}

fn probe() -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    let returned = first([left, right])
    let moved = left
    returned.value + moved.value
}
"#,
    );
    assert!(
        array_conflict.contains("borrow conflict in `fn probe`"),
        "{array_conflict}"
    );

    let tuple_allowed = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn second(_ values: own (view Boxed, view Boxed)) -> view Boxed {
    values.1
}

fn probe() -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    let returned = second((left, right))
    let moved = left
    returned.value + moved.value
}
"#,
    );
    assert!(tuple_allowed.is_empty(), "{tuple_allowed}");

    let tuple_conflict = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn second(_ values: own (view Boxed, view Boxed)) -> view Boxed {
    values.1
}

fn probe() -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    let returned = second((left, right))
    let moved = right
    returned.value + moved.value
}
"#,
    );
    assert!(
        tuple_conflict.contains("borrow conflict in `fn probe`"),
        "{tuple_conflict}"
    );
}

#[test]
fn closure_call_aggregate_context_recurses_through_repeat_and_control_flow() {
    let repeat_src = r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let first = |values: own [view Boxed; 2]| -> view Boxed { values[0] }
    let returned = first.call([value; 2])
    let moved = value
    returned.value + moved.value
}
"#;
    let repeat_analysis = analysis_diags(repeat_src);
    let repeat_borrows = borrow_diags(repeat_src);
    assert!(
        !repeat_analysis.contains("type mismatch"),
        "{repeat_analysis}"
    );
    assert!(
        repeat_borrows.contains("borrow conflict in `fn probe`"),
        "{repeat_borrows}"
    );

    let allowed = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe(_ choose_left: bool) -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    let first = |values: own (view Boxed, view Boxed)| -> view Boxed { values.0 }
    let returned = first.call(if choose_left {
        (left, right)
    } else {
        (left, right)
    })
    let moved = right
    returned.value + moved.value
}
"#,
    );
    assert!(allowed.is_empty(), "{allowed}");

    let conflicting = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe(_ choose_left: bool) -> u256 {
    let left = Boxed { value: 20 }
    let right = Boxed { value: 22 }
    let first = |values: own (view Boxed, view Boxed)| -> view Boxed { values.0 }
    let returned = first.call(if choose_left {
        (left, right)
    } else {
        (left, right)
    })
    let moved = left
    returned.value + moved.value
}
"#,
    );
    assert!(
        conflicting.contains("borrow conflict in `fn probe`"),
        "{conflicting}"
    );
}

#[test]
fn returned_aggregate_with_view_field_retains_argument_provenance() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct Holder {
    value: view Boxed,
}

fn wrap(_ value: view Boxed) -> Holder {
    Holder { value }
}

fn probe() -> u256 {
    let value = Boxed { value: 42 }
    let holder = wrap(value)
    let moved = value
    holder.value.value + moved.value
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn user_defined_callable_mixed_capability_args_enforce_aliasing() {
    let distinct_diags = borrow_diags(
        r#"
use core::functional::FnOnce

struct AddInto {}

impl FnOnce<(mut u256, view u256), ()> for AddInto {
    fn call_once(self: own Self, mut _ args: own (mut u256, view u256)) {
        args.0 += args.1
    }
}

fn probe() {
    let mut left = 20
    let right = 22
    AddInto {}.call_once(mut left, right)
}
"#,
    );
    assert!(distinct_diags.is_empty(), "{distinct_diags}");

    let aliased_diags = borrow_diags(
        r#"
use core::functional::FnOnce

struct AddInto {}

impl FnOnce<(mut u256, view u256), ()> for AddInto {
    fn call_once(self: own Self, mut _ args: own (mut u256, view u256)) {
        args.0 += args.1
    }
}

fn probe() {
    let mut value = 21
    AddInto {}.call_once(mut value, value)
}
"#,
    );
    assert!(
        aliased_diags.contains("borrow conflict in `fn probe`"),
        "{aliased_diags}"
    );
}

#[test]
fn user_defined_fn_once_return_retains_tuple_field_provenance() {
    let selected_diags = borrow_diags(
        r#"
use core::functional::FnOnce

struct First {}

impl FnOnce<(mut u256, mut u256), mut u256> for First {
    fn call_once(self: own Self, _ args: own (mut u256, mut u256)) -> mut u256 {
        args.0
    }
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let returned = First {}.call_once(mut left, mut right)
    let other = mut left
    other = 1
    returned = 2
}
"#,
    );
    assert!(
        selected_diags.contains("borrow conflict in `fn probe`"),
        "{selected_diags}"
    );

    let unrelated_diags = borrow_diags(
        r#"
use core::functional::FnOnce

struct First {}

impl FnOnce<(mut u256, mut u256), mut u256> for First {
    fn call_once(self: own Self, _ args: own (mut u256, mut u256)) -> mut u256 {
        args.0
    }
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let returned = First {}.call_once(mut left, mut right)
    let other = mut right
    other = 1
    returned = 2
}
"#,
    );
    assert!(unrelated_diags.is_empty(), "{unrelated_diags}");
}

#[test]
fn closure_capture_and_mut_arg_reject_aliasing_the_same_place() {
    let diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let write = |arg: mut u256| {
        captured = 1
        arg = 2
    }
    write.call(mut value)
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_cannot_mutably_borrow_an_owned_capture() {
    let diags = analysis_diags(
        r#"
fn probe() {
    let mut value: u256 = 0
    let borrow = || -> mut u256 { mut value }
    borrow.call()
}
"#,
    );
    assert!(
        diags.contains("cannot mutably borrow a binding captured by a closure"),
        "{diags}"
    );
}

#[test]
fn closure_can_write_through_a_captured_mut_handle() {
    let diags = analysis_diags(
        r#"
fn probe() {
    let mut value: u256 = 0
    let captured = mut value
    let write = || {
        captured = 42
    }
    write.call()
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_copyability_preserves_move_and_borrow_tracking() {
    let copy_diags = borrow_diags(
        r#"
fn probe() -> u256 {
    let offset = 1
    let add = |value: own u256| value + offset
    let copied = add
    add.call(20) + copied.call(20)
}
"#,
    );
    assert!(copy_diags.is_empty(), "{copy_diags}");

    let noncopy_diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() {
    let boxed = Boxed { value: 42 }
    let read = || boxed.value
    let moved = read
    read.call()
}
"#,
    );
    assert!(
        noncopy_diags.contains("cannot use a value after it was moved"),
        "{noncopy_diags}"
    );

    let aliased_diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let get = || captured
    let copied = get
    let first = get.call()
    let second = copied.call()
    first = 1
    second = 2
}
"#,
    );
    assert!(
        aliased_diags.contains("borrow conflict in `fn probe`"),
        "{aliased_diags}"
    );

    let sequential_diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let captured = mut value
    let get = || captured
    let copied = get
    let first = get.call()
    first = 1
    let second = copied.call()
    second = 2
}
"#,
    );
    assert!(sequential_diags.is_empty(), "{sequential_diags}");
}

#[test]
fn closure_can_write_through_a_mut_handle_field_in_an_owned_capture() {
    let diags = analysis_diags(
        r#"
struct MutHolder {
    value: mut u256,
}

fn probe() {
    let mut value: u256 = 0
    let holder = MutHolder { value: mut value }
    let write = || {
        holder.value = 42
    }
    write.call()
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn ref_handle_fields_do_not_inherit_container_mutability() {
    let diags = analysis_diags(
        r#"
struct RefHolder {
    value: ref u256,
}

fn probe() {
    let value: u256 = 0
    let mut holder = RefHolder { value: ref value }
    holder.value = 42
}
"#,
    );
    assert!(
        diags.contains("left-hand side of assignment is immutable"),
        "{diags}"
    );
}

#[test]
fn capability_rebind_diagnostic_does_not_suggest_an_invalid_mutable_binding() {
    let diags = analysis_diags(
        r#"
fn probe() {
    let mut first: u256 = 1
    let mut second: u256 = 2
    let target: mut u256 = mut first
    target = mut second
}
"#,
    );
    assert!(
        diags.contains("the capability handle `target` cannot be rebound"),
        "{diags}"
    );
    assert!(
        diags.contains("handle bindings stay attached to their original borrow provider"),
        "{diags}"
    );
    assert!(!diags.contains("try changing to `mut target`"), "{diags}");
}

#[test]
fn array_element_capabilities_determine_projection_mutability() {
    let mutable_diags = analysis_diags(
        r#"
fn probe() {
    let mut value: u256 = 0
    let handles: [mut u256; 1] = [mut value]
    handles[0] = 42
}
"#,
    );
    assert!(mutable_diags.is_empty(), "{mutable_diags}");

    let immutable_diags = analysis_diags(
        r#"
fn probe() {
    let value: u256 = 0
    let mut handles: [ref u256; 1] = [ref value]
    handles[0] = 42
}
"#,
    );
    assert!(
        immutable_diags.contains("left-hand side of assignment is immutable"),
        "{immutable_diags}"
    );
}

#[test]
fn closure_preexisting_mut_handles_preserve_argument_aliasing() {
    let distinct_diags = borrow_diags(
        r#"
fn probe() {
    let mut left = 0
    let mut right = 0
    let left_handle = mut left
    let right_handle = mut right
    let write = |left: mut u256, right: mut u256| {
        left = 1
        right = 2
    }
    write.call(left_handle, right_handle)
}
"#,
    );
    assert!(distinct_diags.is_empty(), "{distinct_diags}");

    let aliased_diags = borrow_diags(
        r#"
fn probe() {
    let mut value = 0
    let handle = mut value
    let write = |left: mut u256, right: mut u256| {
        left = 1
        right = 2
    }
    write.call(handle, handle)
}
"#,
    );
    assert!(
        aliased_diags.contains("borrow conflict in `fn probe`"),
        "{aliased_diags}"
    );
}

#[test]
fn closure_returned_borrow_nested_in_owned_arg_pack_is_tracked() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn probe() {
    let mut value = 0
    let unwrap = |borrowed: own Borrowed| -> mut u256 { borrowed.value }
    let returned = unwrap.call_once(Borrowed { value: mut value })
    let other = mut value
    other = 1
    returned = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_returned_borrow_nested_in_view_arg_pack_is_tracked() {
    let diags = borrow_diags(
        r#"
struct Borrowed {
    value: mut u256,
}

fn probe() {
    let mut value = 0
    let borrowed = Borrowed { value: mut value }
    let unwrap = |borrowed: Borrowed| -> mut u256 { borrowed.value }
    let returned = unwrap.call(borrowed)
    let other = mut value
    other = 1
    returned = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_control_flow_mut_return_retains_every_possible_source() {
    let diags = borrow_diags(
        r#"
fn probe(_ flag: bool) {
    let mut left = 0
    let mut right = 0
    let choose = |flag: own bool, left: mut u256, right: mut u256| -> mut u256 {
        if flag { left } else { right }
    }
    let returned = choose.call(flag, mut left, mut right)
    let other = mut right
    other = 1
    returned = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_mixed_fresh_and_forwarded_aggregate_returns_retain_borrow_sources() {
    let cases = [
        (
            "empty enum variant",
            r#"
use core::option::Option::{self, Some}

fn probe(_ flag: bool) {
    let maybe = |target: mut u256, flag: own bool| -> Option<mut u256> {
        if flag {
            Some(target)
        } else {
            Option::None
        }
    }
    let mut value = 0
    let held = maybe.call(mut value, flag)
    let conflicting = mut value
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "forwarded local initialized by control flow",
            r#"
use core::option::Option::{self, Some}

fn probe(_ flag: bool) {
    let maybe = |target: mut u256, flag: own bool| -> Option<mut u256> {
        let result = if flag {
            Some(target)
        } else {
            Option::None
        }
        result
    }
    let mut value = 0
    let held = maybe.call(mut value, flag)
    let conflicting = mut value
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "mutable local replaced with a fresh variant",
            r#"
use core::option::Option::{self, Some}

fn probe(_ flag: bool) {
    let maybe = |target: mut u256, flag: own bool| -> Option<mut u256> {
        let mut result = Some(target)
        if flag {
            result = Option::None
        }
        result
    }
    let mut value = 0
    let held = maybe.call(mut value, flag)
    let conflicting = mut value
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "fresh mutable local replaced with a forwarded variant",
            r#"
use core::option::Option::{self, Some}

fn probe(_ flag: bool) {
    let maybe = |target: mut u256, flag: own bool| -> Option<mut u256> {
        let mut result = Option::None
        if flag {
            result = Some(target)
        }
        result
    }
    let mut value = 0
    let held = maybe.call(mut value, flag)
    let conflicting = mut value
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "match arm",
            r#"
use core::option::Option::{self, Some}

fn probe(_ flag: bool) {
    let maybe = |target: mut u256, flag: own bool| -> Option<mut u256> {
        match flag {
            true => Some(target),
            false => Option::None,
        }
    }
    let mut value = 0
    let held = maybe.call(mut value, flag)
    let conflicting = mut value
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "fresh helper callee",
            r#"
use core::option::Option::{self, Some}

fn maybe(_ target: mut u256, _ flag: bool) -> Option<mut u256> {
    if flag {
        Some(target)
    } else {
        Option::None
    }
}

fn probe(_ flag: bool) {
    let indirect = |target: mut u256, flag: own bool| -> Option<mut u256> {
        maybe(target, flag)
    }
    let mut value = 0
    let held = indirect.call(mut value, flag)
    let conflicting = mut value
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "same enum variant with fresh storage",
            r#"
use core::option::Option::Some

fn probe(_ flag: bool) {
    let choose = |mut owned: own u256, target: mut u256, flag: own bool|
        -> Option<mut u256>
    {
        if flag {
            Some(target)
        } else {
            Some(mut owned)
        }
    }
    let mut value = 0
    let held = choose.call(0, mut value, flag)
    let conflicting = mut value
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "struct field with fresh storage",
            r#"
struct Borrowed {
    value: mut u256,
}

fn probe(_ flag: bool) {
    let choose = |mut owned: own u256, target: mut u256, flag: own bool| -> Borrowed {
        if flag {
            Borrowed { value: target }
        } else {
            Borrowed { value: mut owned }
        }
    }
    let mut value = 0
    let held = choose.call(0, mut value, flag)
    let conflicting = mut value
    held.value = 1
    conflicting = 2
}
"#,
        ),
        (
            "dynamic and constant projections for one borrow slot",
            r#"
use core::option::Option::Some

fn probe(_ index: usize, _ flag: bool) {
    let choose = |items: view [mut u256; 2], index: own usize, flag: own bool|
        -> Option<mut u256>
    {
        if flag {
            Some(items[index])
        } else {
            Some(items[0])
        }
    }
    let mut left = 0
    let mut right = 0
    let handles: [mut u256; 2] = [mut left, mut right]
    let held = choose.call(handles, index, flag)
    let conflicting = mut right
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "unreplayable mutable index",
            r#"
use core::option::Option::{self, Some}

fn probe(_ index: usize, _ flag: bool) {
    let choose = |items: view [mut u256; 2], mut index: own usize, flag: own bool|
        -> Option<mut u256>
    {
        index = 1
        if flag {
            Some(items[index])
        } else {
            Option::None
        }
    }
    let mut left = 0
    let mut right = 0
    let handles: [mut u256; 2] = [mut left, mut right]
    let held = choose.call(handles, index, flag)
    let conflicting = mut right
    if let Some(mut returned) = held {
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
        (
            "returned closure carrying a borrow",
            r#"
use core::option::Option::{self, Some}
use core::functional::Fn

fn probe(_ flag: bool) {
    let make = |target: mut u256, flag: own bool| {
        let get = || -> mut u256 { target }
        if flag {
            Some(get)
        } else {
            Option::None
        }
    }
    let mut value = 0
    let held = make.call(mut value, flag)
    let conflicting = mut value
    if let Some(get) = held {
        let returned = get.call()
        returned = 1
    }
    conflicting = 2
}
"#,
        ),
    ];

    for (name, src) in cases {
        let diags = borrow_diags(src);
        assert!(
            diags.contains("borrow conflict in `fn probe`"),
            "{name}: {diags}"
        );
    }
}

#[test]
fn closure_mixed_capture_and_argument_return_retains_every_possible_source() {
    for (name, borrowed_place) in [("capture", "left"), ("argument", "right")] {
        let src = format!(
            r#"
fn probe(_ flag: bool) {{
    let mut left = 0
    let mut right = 0
    let captured = mut left
    let choose = |flag: own bool, argument: mut u256| -> mut u256 {{
        if flag {{ captured }} else {{ argument }}
    }}
    let returned = choose.call(flag, mut right)
    let other = mut {borrowed_place}
    other = 1
    returned = 2
}}
"#,
        );
        let diags = borrow_diags(&src);
        assert!(
            diags.contains("borrow conflict in `fn probe`"),
            "{name}: {diags}"
        );
    }

    let unrelated = borrow_diags(
        r#"
fn probe(_ flag: bool) {
    let mut left = 0
    let mut right = 0
    let mut unrelated = 0
    let captured = mut left
    let choose = |flag: own bool, argument: mut u256| -> mut u256 {
        if flag { captured } else { argument }
    }
    let returned = choose.call(flag, mut right)
    let other = mut unrelated
    other = 1
    returned = 2
}
"#,
    );
    assert!(unrelated.is_empty(), "{unrelated}");
}

#[test]
fn closure_explicit_mut_returns_retain_every_possible_source() {
    let diags = borrow_diags(
        r#"
fn probe(_ flag: bool) {
    let mut left = 0
    let mut right = 0
    let choose = |flag: own bool, left: mut, right: mut| {
        if flag {
            return left
        }
        return right
    }
    let returned = choose.call(flag, mut left, mut right)
    let other = mut right
    other = 1
    returned = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_projected_capture_return_retains_the_underlying_source() {
    let diags = borrow_diags(
        r#"
struct Holder {
    value: mut u256,
}

fn probe() {
    let mut value = 0
    let holder = Holder { value: mut value }
    let get = || holder.value
    let returned = get.call()
    let other = mut value
    other = 1
    returned = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_environment_cannot_move_while_a_returned_borrow_is_live() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let boxed = Boxed { value: 42 }
    let read = || -> ref u256 { ref boxed.value }
    let returned = read.call()
    let moved = read
    returned
}
"#,
    );
    assert!(
        diags.contains("cannot move out of a value while it is borrowed"),
        "{diags}"
    );
}

#[test]
fn projected_closure_environment_borrow_remains_field_precise() {
    let allowed = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let captured = Boxed { value: 20 }
    let project = || -> ref u256 { ref captured.value }
    let holder = (project, Boxed { value: 22 })
    let returned = holder.0.call()
    let moved = holder.1
    returned + moved.value
}
"#,
    );
    assert!(allowed.is_empty(), "{allowed}");

    let conflicting = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

fn probe() -> u256 {
    let captured = Boxed { value: 42 }
    let project = || -> ref u256 { ref captured.value }
    let holder = (project, Boxed { value: 0 })
    let returned = holder.0.call()
    let moved = holder.0
    returned + moved.call()
}
"#,
    );
    assert!(
        conflicting.contains("cannot move out of a value while it is borrowed"),
        "{conflicting}"
    );
}

#[test]
fn closure_aggregate_mut_return_retains_each_used_source() {
    let diags = borrow_diags(
        r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let mut left = 0
    let mut right = 0
    let borrow_both = |left: mut u256, right: mut u256| -> Both {
        Both { left, right }
    }
    let returned = borrow_both.call(mut left, mut right)
    let other = mut right
    other = 1
    returned.right = 2
}
"#,
    );
    assert!(diags.contains("borrow conflict in `fn probe`"), "{diags}");
}

#[test]
fn closure_may_copy_one_mut_parameter_into_sequential_return_slots() {
    let src = r#"
struct Both {
    left: mut u256,
    right: mut u256,
}

fn probe() {
    let duplicate = |value: mut u256| -> Both {
        Both { left: value, right: value }
    }
    let mut target = 0
    let returned = duplicate.call(mut target)
    returned.left = 1
    returned.right = 2
}
"#;
    let diags = borrow_diags(src);
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn closure_control_flow_pattern_move_consumes_captured_value() {
    let diags = borrow_diags(
        r#"
use core::functional::FnOnce

struct Boxed {
    value: u256,
}

fn bad(_ boxed: own Boxed, _ choose_left: bool) -> u256 {
    let take = |_ unit: own ()| -> u256 {
        let selected = if choose_left { boxed } else { boxed }
        selected.value + boxed.value
    }
    take.call_once(())
}
"#,
    );

    assert!(
        diags.contains("move conflict in `fn <closure>`"),
        "{diags:?}",
    );
    assert!(
        diags.contains("cannot use a value after it was moved"),
        "{diags:?}",
    );
}

#[test]
fn closure_control_flow_moves_cover_divergent_and_fresh_predecessors() {
    for body in [
        "if choose_left { left } else { right }",
        "if choose_left { left } else { Boxed { value: 0 } }",
        "match choose_left { true => left, false => right }",
    ] {
        let diags = borrow_diags(&format!(
            r#"
use core::functional::FnOnce

struct Boxed {{
    value: u256,
}}

fn bad(_ left: own Boxed, _ right: own Boxed, _ choose_left: bool) -> u256 {{
    let take = |_ unit: own ()| -> u256 {{
        let selected = {body}
        selected.value + left.value + right.value
    }}
    take.call_once(())
}}
"#,
        ));

        assert!(
            diags.contains("move conflict in `fn <closure>`")
                && diags.contains("cannot use a value after it was moved"),
            "{body}: {diags:?}",
        );
    }
}

#[test]
fn closure_control_flow_copy_patterns_remain_non_consuming() {
    let src = r#"
use core::functional::Fn

struct Boxed {
    value: u256,
}

struct Mixed {
    copy: u256,
    owned: Boxed,
}

fn valid(_ mixed: own Mixed, _ choose_left: bool) -> u256 {
    let inspect = |_ unit: own ()| -> u256 {
        let Mixed { copy, owned: _ } =
            if choose_left { mixed } else { mixed }
        copy + mixed.copy
    }
    inspect.call(()) + inspect.call(())
}
"#;
    let diags = borrow_diags(src);

    assert!(diags.is_empty(), "{diags}");
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
fn mutable_enum_can_be_replaced_from_its_copy_payload() {
    let diags = borrow_diags(
        r#"
enum Choice {
    Left(u256),
    Right(u256),
}

impl Choice {
    fn flip(mut self) {
        match self {
            Choice::Left(value) => self = Choice::Right(value),
            Choice::Right(value) => self = Choice::Left(value),
        }
    }
}
"#,
    );

    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn constant_folding_does_not_erase_non_copy_moves() {
    let diags = borrow_diags(
        r#"
use core::option::Option

struct Boxed {
    value: u256,
}

struct Holder {
    value: Boxed,
}

const fn consume(_ value: own Boxed) -> u256 {
    value.value
}

fn move_direct_twice() {
    let original = Boxed { value: 21 }
    let first = original
    let second = original
    assert(first.value + second.value == 42)
}

fn move_into_aggregate_twice() {
    let original = Boxed { value: 21 }
    let first = Holder { value: original }
    let second = Holder { value: original }
    assert(first.value.value + second.value.value == 42)
}

fn match_local_twice() {
    let wrapped: Option<Boxed> = Option::Some(Boxed { value: 21 })
    let first = match wrapped {
        Option::Some(value) => value,
        Option::None => Boxed { value: 0 },
    }
    let second = match wrapped {
        Option::Some(value) => value,
        Option::None => Boxed { value: 0 },
    }
    assert(first.value + second.value == 42)
}

fn if_let_local_twice() {
    let wrapped: Option<Boxed> = Option::Some(Boxed { value: 21 })
    if let Option::Some(value) = wrapped {
        assert(value.value == 21)
    }
    if let Option::Some(value) = wrapped {
        assert(value.value == 21)
    }
}

fn nested_match_local_twice() {
    let wrapped: Option<Holder> = Option::Some(Holder {
        value: Boxed { value: 21 },
    })
    let first = match wrapped {
        Option::Some(Holder { value }) => value,
        Option::None => Boxed { value: 0 },
    }
    let second = match wrapped {
        Option::Some(Holder { value }) => value,
        Option::None => Boxed { value: 0 },
    }
    assert(first.value + second.value == 42)
}

fn const_call_local_twice() {
    let original = Boxed { value: 21 }
    let first = consume(original)
    let second = consume(original)
    assert(first + second == 42)
}

fn while_let_reuses_local() {
    let wrapped: Option<Boxed> = Option::Some(Boxed { value: 21 })
    while let Option::Some(value) = wrapped {
        assert(value.value == 21)
    }
}

fn move_constant_while_borrowed() {
    let original = Boxed { value: 21 }
    let borrowed: ref Boxed = ref original
    let moved = original
    assert(borrowed.value + moved.value == 42)
}
"#,
    );

    for name in [
        "move_direct_twice",
        "move_into_aggregate_twice",
        "match_local_twice",
        "if_let_local_twice",
        "nested_match_local_twice",
        "const_call_local_twice",
        "while_let_reuses_local",
    ] {
        assert!(
            diags.contains(&format!("move conflict in `fn {name}`")),
            "missing move conflict for {name}: {diags:?}"
        );
    }
    assert!(
        diags.contains("borrow conflict in `fn move_constant_while_borrowed`")
            && diags.contains("cannot move out of a value while it is borrowed"),
        "{diags:?}"
    );
}

#[test]
fn constant_folded_single_moves_and_copy_reads_remain_valid() {
    let diags = borrow_diags(
        r#"
use core::option::Option

struct Boxed {
    value: u256,
}

fn move_direct_once() -> u256 {
    let original = Boxed { value: 21 }
    let moved = original
    moved.value
}

fn match_local_once() -> u256 {
    let wrapped: Option<Boxed> = Option::Some(Boxed { value: 21 })
    let moved = match wrapped {
        Option::Some(value) => value,
        Option::None => Boxed { value: 0 },
    }
    moved.value
}

fn inspect_tag_twice() -> u256 {
    let wrapped: Option<Boxed> = Option::Some(Boxed { value: 21 })
    let first = match wrapped {
        Option::Some(_) => 1,
        Option::None => 0,
    }
    let second = match wrapped {
        Option::Some(_) => 1,
        Option::None => 0,
    }
    first + second
}

fn copy_scrutinee_twice() -> u256 {
    let original: u256 = 21
    let first = match original {
        value => value,
    }
    let second = match original {
        value => value,
    }
    first + second
}

fn borrowed_match_twice() -> u256 {
    let wrapped: Option<Boxed> = Option::Some(Boxed { value: 21 })
    let borrowed: ref Option<Boxed> = ref wrapped
    let first = match borrowed {
        Option::Some(value) => value.value,
        Option::None => 0,
    }
    let second = match borrowed {
        Option::Some(value) => value.value,
        Option::None => 0,
    }
    first + second
}

fn reassign_after_match() -> u256 {
    let mut wrapped: Option<Boxed> = Option::Some(Boxed { value: 20 })
    let first = match wrapped {
        Option::Some(value) => value,
        Option::None => Boxed { value: 0 },
    }
    wrapped = Option::Some(Boxed { value: 22 })
    let second = match wrapped {
        Option::Some(value) => value,
        Option::None => Boxed { value: 0 },
    }
    first.value + second.value
}

fn borrow_ends_before_move() -> u256 {
    let original = Boxed { value: 21 }
    let borrowed: ref Boxed = ref original
    let value = borrowed.value
    let moved = original
    value + moved.value
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
fn copy_pattern_bindings_do_not_move_non_copy_containers() {
    let diags = borrow_diags(
        r#"
struct Boxed {
    value: u256,
}

struct MixedStruct {
    copy: u256,
    owned: Boxed,
}

enum MixedEnum {
    Value(u256, Boxed),
}

fn consume(_ value: own Boxed) {}

fn match_enum_twice(mixed: own MixedEnum) -> u256 {
    let first = match mixed {
        MixedEnum::Value(value, _) => value,
    }
    let second = match mixed {
        MixedEnum::Value(value, _) => value,
    }
    first + second
}

fn if_let_enum_twice(mixed: own MixedEnum) -> u256 {
    let mut total: u256 = 0
    if let MixedEnum::Value(value, _) = mixed {
        total += value
    }
    if let MixedEnum::Value(value, _) = mixed {
        total += value
    }
    total
}

fn wildcarded_struct_field_remains_available(mixed: own MixedStruct) -> u256 {
    let MixedStruct { copy, owned: _ } = mixed
    copy + mixed.copy
}

fn copied_struct_field_remains_available_after_sibling_move(
    mixed: own MixedStruct,
) -> u256 {
    let MixedStruct { copy, owned } = mixed
    consume(owned)
    copy + mixed.copy
}

fn binding_free_projected_place_remains_available(mixed: own MixedStruct) -> u256 {
    let _ = mixed.owned
    let marker = match mixed.owned {
        _ => 1,
    }
    marker + mixed.owned.value
}

fn block_wrapped_noncopy_binding_moves_once(mixed: own MixedStruct) {
    let owned = { mixed.owned }
    consume(owned)
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
