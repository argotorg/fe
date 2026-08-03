use fe_hir::{
    analysis::{
        semantic::{
            NExpr, NSStmtKind, NSTerminatorKind, ReadMode, SCallReturnProjectionStep,
            collect_semantic_borrow_diagnostic_vouchers, get_or_build_semantic_instance,
            identity_semantic_instance_key, normalize_semantic_body, normalized_cfg_successors,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::ItemKind,
    test_db::{HirAnalysisTestDb, find_func, format_diagnostics},
};

fn definite_init_diags(src: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("definite_initialization_audit.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);
    format_diagnostics(
        &db,
        &collect_semantic_borrow_diagnostic_vouchers(&db, top_mod),
    )
}

#[test]
fn tracks_partial_aggregate_initialization_by_projection() {
    let diags = definite_init_diags(
        r#"
struct Pair {
    first: u256,
    second: u256,
}

fn read_written_field() -> u256 {
    let mut pair: Pair
    pair.first = 1
    pair.first
}

fn read_other_field() -> u256 {
    let mut pair: Pair
    pair.first = 1
    pair.second
}

fn initialize_all_fields() -> u256 {
    let mut pair: Pair
    pair.first = 1
    pair.second = 2
    pair.first + pair.second
}

fn whole_or_field(cond: bool) -> u256 {
    let mut pair: Pair
    if cond {
        pair = Pair { first: 1, second: 2 }
    } else {
        pair.first = 1
    }
    pair.first
}

fn disjoint_fields(cond: bool) -> u256 {
    let mut pair: Pair
    if cond {
        pair.first = 1
    } else {
        pair.second = 2
    }
    pair.first
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn read_other_field`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn disjoint_fields`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2
    );
}

#[test]
fn zero_sized_aggregates_still_require_an_explicit_definition() {
    let diags = definite_init_diags(
        r#"
struct Empty {}

fn uninitialized_empty_struct() -> Empty {
    let value: Empty
    value
}

fn initialized_empty_struct() -> Empty {
    let value = Empty {}
    value
}

fn uninitialized_empty_array() -> [u256; 0] {
    let value: [u256; 0]
    value
}

fn initialized_empty_array() -> [u256; 0] {
    let value: [u256; 0] = []
    value
}

fn consume_unit(value: own ()) {}

fn uninitialized_unit() {
    let value: ()
    consume_unit(value)
}

fn initialized_unit() {
    let value: () = ()
    consume_unit(value)
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn uninitialized_empty_struct`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn uninitialized_empty_array`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn uninitialized_unit`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn initialized_empty_struct`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn initialized_empty_array`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn initialized_unit`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        3,
        "{diags}"
    );
}

#[test]
fn handles_early_returns_loops_and_literal_reachability() {
    let diags = definite_init_diags(
        r#"
fn early_return(cond: bool) -> u256 {
    let mut value: u256
    if cond {
        value = 1
    } else {
        return 0
    }
    value
}

fn maybe_loop(cond: bool) -> u256 {
    let mut value: u256
    while cond {
        value = 1
    }
    value
}

fn constant_loop() -> u256 {
    let mut value: u256
    while true {
        value = 1
        break
    }
    value
}

fn constant_branch() -> u256 {
    let mut value: u256
    if true {
        value = 1
    }
    value
}

fn false_loop() -> u256 {
    let mut value: u256
    while false {
        value = 1
    }
    value
}

enum Choice {
    Initialize,
    Skip,
}

fn constant_enum_match() -> u256 {
    let mut value: u256
    match Choice::Initialize {
        Choice::Initialize => value = 1
        Choice::Skip => {}
    }
    value
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn maybe_loop`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn false_loop`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2,
        "{diags}"
    );
}

#[test]
fn does_not_fold_mutable_discriminants_after_indirect_writes() {
    let diags = definite_init_diags(
        r#"
fn mutated_bool() -> u256 {
    let mut cond = true
    let target = mut cond
    target = false

    let mut value: u256
    if cond {
        value = 1
    }
    value
}

enum Choice {
    Initialize,
    Skip,
}

fn mutated_enum() -> u256 {
    let mut choice = Choice::Initialize
    let target = mut choice
    target = Choice::Skip

    let mut value: u256
    match choice {
        Choice::Initialize => value = 1
        Choice::Skip => {}
    }
    value
}

fn set_false()
    uses (cond: mut bool)
{
    cond = false
}

fn mutated_bool_effect() -> u256 {
    let mut cond = true
    with (mut cond) {
        set_false()
    }

    let mut value: u256
    if cond {
        value = 1
    }
    value
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn mutated_bool`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn mutated_enum`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn mutated_bool_effect`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        3,
        "{diags}"
    );
}

#[test]
fn mutable_aliases_only_invalidate_constants_when_they_can_mutate() {
    let diags = definite_init_diags(
        r#"
fn observe(value: mut bool) -> bool {
    value
}

fn alias_dead_before_direct_reassignment() -> u256 {
    let mut cond = false
    let alias = mut cond
    let _ = observe(alias)
    cond = true

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn alias_used_after_direct_reassignment() -> u256 {
    let mut cond = false
    let alias = mut cond
    cond = true

    let mut value: u256
    if cond {
        value = 42
    }
    alias = false
    value
}

fn alias_mutation_in_dead_branch() -> u256 {
    let mut cond = true
    let alias = mut cond
    if false {
        alias = false
    }

    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        !diags
            .contains("possibly uninitialized local in `fn alias_dead_before_direct_reassignment`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn alias_mutation_in_dead_branch`"),
        "{diags}"
    );
    assert!(
        diags.contains("borrow conflict in `fn alias_used_after_direct_reassignment`"),
        "{diags}"
    );
}

#[test]
fn mutable_carriers_only_invalidate_the_roots_they_can_reach() {
    let diags = definite_init_diags(
        r#"
use core::functional::Fn

struct BoolAlias {
    value: mut bool,
}

fn set_false(_ value: mut bool) {
    value = false
}

fn forward_alias(_ value: mut bool) -> mut bool {
    value
}

fn unrelated_live_aliases() -> u256 {
    let mut cond = true
    let mut other = true
    let _cond_alias = mut cond
    let other_alias = mut other
    set_false(other_alias)
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn matching_alias_mutation() -> u256 {
    let mut cond = true
    let cond_alias = mut cond
    set_false(cond_alias)
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn dead_alias_and_unrelated_mutation() -> u256 {
    let mut cond = true
    let mut other = true
    let dead_alias = mut cond
    let _ = dead_alias
    let other_alias = mut other
    set_false(other_alias)
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn aggregate_carried_alias_mutation() -> u256 {
    let mut cond = true
    let holder = BoolAlias { value: mut cond }
    holder.value = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn closure_carried_alias_mutation() -> u256 {
    let mut cond = true
    let captured = mut cond
    let write = || {
        captured = false
    }
    write.call()
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn call_result_alias_mutation() -> u256 {
    let mut cond = true
    let forwarded = forward_alias(mut cond)
    set_false(forwarded)
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn joined_unrelated_aliases(choose: bool) -> u256 {
    let mut cond = true
    let mut left = true
    let mut right = true
    let _cond_alias = mut cond
    let selected: mut bool = if choose { mut left } else { mut right }
    set_false(selected)
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn joined_maybe_matching_alias(choose: bool) -> u256 {
    let mut cond = true
    let mut other = true
    let selected: mut bool = if choose { mut cond } else { mut other }
    set_false(selected)
    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn unrelated_live_aliases`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn dead_alias_and_unrelated_mutation`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn joined_unrelated_aliases`"),
        "{diags}"
    );
    for function in [
        "matching_alias_mutation",
        "aggregate_carried_alias_mutation",
        "closure_carried_alias_mutation",
        "call_result_alias_mutation",
        "joined_maybe_matching_alias",
    ] {
        assert!(
            diags.contains(&format!("possibly uninitialized local in `fn {function}`")),
            "{diags}"
        );
    }
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        5,
        "{diags}"
    );
}

#[test]
fn call_rebound_carrier_provenance_does_not_fold_the_previous_root() {
    let diags = definite_init_diags(
        r#"
struct BoolAlias {
    value: mut bool,
}

fn rebind(alias: mut BoolAlias, replacement: mut bool) {
    alias.value = replacement
}

fn call_rebound_alias() -> u256 {
    let mut previous = false
    let mut replacement = false
    let mut alias = BoolAlias { value: mut previous }
    rebind(mut alias, mut replacement)
    alias.value = true

    let mut value: u256
    if previous {
        value = 42
    }
    value
}

fn stable_alias() -> u256 {
    let mut previous = false
    let mut alias = BoolAlias { value: mut previous }
    alias.value = true

    let mut value: u256
    if previous {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn call_rebound_alias`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn stable_alias`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn referent_only_calls_preserve_carrier_shape_for_later_exact_stores() {
    let diags = definite_init_diags(
        r#"
struct BoolAlias {
    value: mut bool,
}

fn observe_scalar(_ value: mut bool) {}

fn consume_owned_scalar_alias(_ value: own BoolAlias) {}

fn scalar_mut_arg() -> u256 {
    let mut cond = false
    let alias = mut cond
    observe_scalar(alias)
    alias = true

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn owned_scalar_alias_arg() -> u256 {
    let mut cond = false
    let alias = BoolAlias { value: mut cond }
    consume_owned_scalar_alias(alias)

    let retained = mut cond
    retained = true
    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn mutable_effect_value_rebind_invalidates_carrier_provenance() {
    let diags = definite_init_diags(
        r#"
struct BoolAlias {
    value: mut bool,
}

fn rebind_effect(_ replacement: mut bool)
    uses (alias: mut BoolAlias)
{
    alias.value = replacement
}

fn effect_rebound_alias(flag: bool) -> u256 {
    let mut previous = false
    let mut replacement = false
    let mut alias = BoolAlias { value: mut previous }
    with (if flag { mut alias } else { mut alias }) {
        rebind_effect(mut replacement)
    }
    alias.value = true

    let mut value: u256
    if previous {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn effect_rebound_alias`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn readonly_effect_invalidates_nested_mutable_referents_without_rebinding_the_wrapper() {
    let diags = definite_init_diags(
        r#"
struct BoolAlias {
    value: mut bool,
}

fn set_false_effect()
    uses (alias: BoolAlias)
{
    alias.value = false
}

fn readonly_effect_mutates_nested_scalar() -> u256 {
    let mut cond = true
    let alias = BoolAlias { value: mut cond }
    with (alias) {
        set_false_effect()
    }

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn readonly_effect_value_mutates_nested_scalar(flag: bool) -> u256 {
    let mut cond = true
    let alias = BoolAlias { value: mut cond }
    with (if flag { alias } else { alias }) {
        set_false_effect()
    }

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn readonly_wrapper_stays_precise() -> u256 {
    let mut cond = true
    let alias = BoolAlias { value: mut cond }
    with (alias) {
        set_false_effect()
    }
    alias.value = true

    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags
            .contains("possibly uninitialized local in `fn readonly_effect_mutates_nested_scalar`"),
        "{diags}"
    );
    assert!(
        diags.contains(
            "possibly uninitialized local in `fn readonly_effect_value_mutates_nested_scalar`"
        ),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn readonly_wrapper_stays_precise`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2,
        "{diags}"
    );
}

#[test]
fn readonly_effect_nested_aggregate_rebind_invalidates_carrier_provenance() {
    let diags = definite_init_diags(
        r#"
struct BoolAlias {
    value: mut bool,
}

struct NestedAlias {
    value: mut BoolAlias,
}

fn rebind_nested_effect(replacement: mut bool)
    uses (alias: NestedAlias)
{
    alias.value.value = replacement
}

fn readonly_effect_rebinds_nested_carrier() -> u256 {
    let mut previous = false
    let mut replacement = false
    let mut inner = BoolAlias { value: mut previous }
    let alias = NestedAlias { value: mut inner }
    with (alias) {
        rebind_nested_effect(mut replacement)
    }
    alias.value.value = true

    let mut value: u256
    if previous {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags.contains(
            "possibly uninitialized local in `fn readonly_effect_rebinds_nested_carrier`"
        ),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn owned_and_view_outer_arguments_only_rebind_nested_aggregate_referents() {
    let diags = definite_init_diags(
        r#"
struct BoolAlias {
    value: mut bool,
}

struct NestedAlias {
    value: mut BoolAlias,
}

fn set_false_owned(alias: own BoolAlias) {
    alias.value = false
}

fn set_false_view(alias: BoolAlias) {
    alias.value = false
}

fn rebind_owned_nested(alias: own NestedAlias, replacement: mut bool) {
    alias.value.value = replacement
}

fn rebind_view_nested(alias: NestedAlias, replacement: mut bool) {
    alias.value.value = replacement
}

fn owned_scalar_referent_is_invalidated() -> u256 {
    let mut cond = true
    let alias = BoolAlias { value: mut cond }
    set_false_owned(alias)

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn owned_scalar_referent_can_be_reinitialized() -> u256 {
    let mut cond = true
    let alias = BoolAlias { value: mut cond }
    set_false_owned(alias)
    let retained = mut cond
    retained = true

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn view_scalar_wrapper_stays_precise() -> u256 {
    let mut cond = true
    let alias = BoolAlias { value: mut cond }
    set_false_view(alias)
    alias.value = true

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn owned_nested_rebind() -> u256 {
    let mut previous = false
    let mut replacement = false
    let mut inner = BoolAlias { value: mut previous }
    let alias = NestedAlias { value: mut inner }
    rebind_owned_nested(alias, mut replacement)
    inner.value = true

    let mut value: u256
    if previous {
        value = 42
    }
    value
}

fn view_nested_rebind() -> u256 {
    let mut previous = false
    let mut replacement = false
    let mut inner = BoolAlias { value: mut previous }
    let alias = NestedAlias { value: mut inner }
    rebind_view_nested(alias, mut replacement)
    alias.value.value = true

    let mut value: u256
    if previous {
        value = 42
    }
    value
}
"#,
    );
    for function in [
        "owned_scalar_referent_is_invalidated",
        "owned_nested_rebind",
        "view_nested_rebind",
    ] {
        assert!(
            diags.contains(&format!("possibly uninitialized local in `fn {function}`")),
            "{diags}"
        );
    }
    for function in [
        "owned_scalar_referent_can_be_reinitialized",
        "view_scalar_wrapper_stays_precise",
    ] {
        assert!(
            !diags.contains(&format!("possibly uninitialized local in `fn {function}`")),
            "{diags}"
        );
    }
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        3,
        "{diags}"
    );
}

#[test]
fn readonly_enum_argument_keeps_its_tag_while_nested_mutable_referents_change() {
    let diags = definite_init_diags(
        r#"
enum Choice {
    Initialize(mut bool),
    Skip,
}

fn set_false(choice: Choice) {
    match choice {
        Choice::Initialize(value) => value = false
        Choice::Skip => {}
    }
}

fn readonly_enum_tag_stays_known() -> u256 {
    let mut cond = true
    let choice = Choice::Initialize(mut cond)
    set_false(choice)

    let mut value: u256
    match choice {
        Choice::Initialize(_) => value = 42
        Choice::Skip => {}
    }
    value
}

fn readonly_enum_referent_is_invalidated() -> u256 {
    let mut cond = true
    let choice = Choice::Initialize(mut cond)
    set_false(choice)

    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn readonly_enum_tag_stays_known`"),
        "{diags}"
    );
    assert!(
        diags
            .contains("possibly uninitialized local in `fn readonly_enum_referent_is_invalidated`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn aggregate_store_replacement_retargets_nested_carrier_provenance() {
    let diags = definite_init_diags(
        r#"
struct Handle {
    target: mut u256,
}

struct Nested {
    target: mut Handle,
}

struct BoolHandles {
    cond: mut bool,
    other: mut bool,
}

fn exact_nested_replacement() -> u256 {
    let mut old = 1
    let mut next = 2
    let mut inner = Handle { target: mut old }
    let outer = Nested { target: mut inner }
    outer.target = Handle { target: mut next }
    outer.target.target = 37
    let mut value: u256
    if old == 1 && next == 37 {
        value = 42
    }
    value
}

fn projected_sibling_alias_is_unrelated() -> u256 {
    let mut cond = true
    let mut other = true
    let handles = BoolHandles {
        cond: mut cond,
        other: mut other,
    }
    handles.other = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn ambiguous_same_type_aggregate_replacement(choose: bool) -> u256 {
    let mut old_left = 1
    let mut old_right = 2
    let mut next = 3
    let mut left = Handle { target: mut old_left }
    let mut right = Handle { target: mut old_right }
    let selected: mut Handle = if choose { mut left } else { mut right }
    selected = Handle { target: mut next }
    selected.target = 37
    let mut value: u256
    if next == 37 {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn exact_nested_replacement`"),
        "{diags}"
    );
    assert!(
        !diags
            .contains("possibly uninitialized local in `fn projected_sibling_alias_is_unrelated`"),
        "{diags}"
    );
    assert!(
        diags.contains(
            "possibly uninitialized local in `fn ambiguous_same_type_aggregate_replacement`"
        ),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn projected_array_aliases_only_invalidate_selected_elements() {
    let diags = definite_init_diags(
        r#"
fn projected_array_alias_is_unrelated() -> u256 {
    let mut cond = true
    let mut other = true
    let handles = [mut cond, mut other]
    handles[1] = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn dynamic_array_alias_is_conservative(index: usize) -> u256 {
    let mut cond = true
    let mut other = true
    let handles = [mut cond, mut other]
    handles[index] = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn projected_array_alias_is_unrelated`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn dynamic_array_alias_is_conservative`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn call_result_projection_preserves_alias_field_identity() {
    let diags = definite_init_diags(
        r#"
struct ReturnedBoolHandles {
    cond: mut bool,
    other: mut bool,
}

fn take_other(handles: own ReturnedBoolHandles) -> mut bool {
    handles.other
}

fn take_cond(handles: own ReturnedBoolHandles) -> mut bool {
    handles.cond
}

fn returned_sibling_alias_is_unrelated() -> u256 {
    let mut cond = true
    let mut other = true
    let returned = take_other(ReturnedBoolHandles {
        cond: mut cond,
        other: mut other,
    })
    cond = true
    returned = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn returned_matching_alias_is_mutated() -> u256 {
    let mut cond = true
    let mut other = true
    let returned = take_cond(ReturnedBoolHandles {
        cond: mut cond,
        other: mut other,
    })
    cond = true
    returned = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn returned_sibling_alias_is_unrelated`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn returned_matching_alias_is_mutated`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn call_result_projection_retains_instantiated_dynamic_index_liveness() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "call_result_dynamic_index_liveness.fe".into(),
        r#"
struct IndexedHandles {
    values: [mut bool; 2],
    index: usize,
}

fn take_dynamic(handles: IndexedHandles) -> mut bool {
    handles.values[handles.index]
}

fn take_any(handles: [mut bool; 2], index: usize) -> mut bool {
    handles[index + 0]
}

fn dynamic_caller(handles: IndexedHandles) -> mut bool {
    take_dynamic(handles)
}

fn any_caller(handles: [mut bool; 2], index: usize) -> mut bool {
    take_any(handles, index)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);

    let dynamic = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(
            &db,
            BodyOwner::Func(find_func(&db, top_mod, "dynamic_caller")),
        ),
    );
    let dynamic = normalize_semantic_body(&db, dynamic).expect("dynamic caller must normalize");
    let dynamic_call = dynamic
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .find_map(|stmt| {
            let NSStmtKind::Assign {
                expr:
                    expr @ NExpr::Call {
                        return_sources,
                        return_sources_complete,
                        ..
                    },
                ..
            } = &stmt.kind
            else {
                return None;
            };
            let index = return_sources
                .iter()
                .flat_map(|source| source.result_projection.iter().chain(&source.projection))
                .find_map(|step| match step {
                    SCallReturnProjectionStep::DynamicIndex(index) => Some(*index),
                    _ => None,
                })?;
            Some((expr, index, *return_sources_complete))
        })
        .expect("callsite should retain its materialized DynamicIndex");
    assert!(
        dynamic_call.2,
        "a direct ordinary function forward must have a complete source set"
    );
    let mut operands = Vec::new();
    dynamic_call
        .0
        .for_each_value_operand(|operand| operands.push(operand));
    assert!(
        operands.iter().any(|operand| {
            operand.local == dynamic_call.1
                && operand.mode == ReadMode::Copy
                && operand.origin.is_none()
        }),
        "DynamicIndex metadata must be a synthetic copy operand so liveness cannot discard it"
    );

    let any = get_or_build_semantic_instance(
        &db,
        identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, top_mod, "any_caller"))),
    );
    let any = normalize_semantic_body(&db, any).expect("AnyIndex caller must normalize");
    assert!(
        any.blocks
            .iter()
            .flat_map(|block| &block.stmts)
            .any(|stmt| matches!(
                &stmt.kind,
                NSStmtKind::Assign {
                    expr: NExpr::Call { return_sources, .. },
                    ..
                } if return_sources.iter().any(|source| source
                    .result_projection
                    .iter()
                    .chain(&source.projection)
                    .any(|step| matches!(step, SCallReturnProjectionStep::AnyIndex)))
            )),
        "an unprovable index must remain AnyIndex at the callsite"
    );
}

#[test]
fn call_result_any_index_alias_is_conservative() {
    let diags = definite_init_diags(
        r#"
fn take_any(handles: [mut bool; 2], index: usize) -> mut bool {
    handles[index + 0]
}

fn returned_any_index_alias_is_conservative(index: usize) -> u256 {
    let mut cond = true
    let mut other = true
    let returned = take_any([mut cond, mut other], index)
    cond = true
    returned = false

    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags.contains(
            "possibly uninitialized local in `fn returned_any_index_alias_is_conservative`"
        ),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn constant_projection_includes_overlapping_any_index_call_sources() {
    let diags = definite_init_diags(
        r#"
fn choose_handles(
    handles: own [mut bool; 2],
    first: mut bool,
    second: mut bool,
    flag: bool,
) -> [mut bool; 2] {
    if flag {
        [first, second]
    } else {
        handles
    }
}

fn returned_constant_index_may_come_from_whole_array(flag: bool) -> u256 {
    let mut cond = true
    let mut spare = true
    let mut other = true
    let mut last = true
    let returned = choose_handles(
        [mut cond, mut spare],
        mut other,
        mut last,
        flag,
    )
    cond = true
    returned[0] = false

    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags.contains(
            "possibly uninitialized local in `fn returned_constant_index_may_come_from_whole_array`"
        ),
        "{diags}"
    );
}

#[test]
fn call_result_projection_maps_effect_place_and_value_sources() {
    let diags = definite_init_diags(
        r#"
struct BoolAlias {
    value: mut bool,
}

struct AliasPair {
    first: BoolAlias,
    second: BoolAlias,
}

fn first_alias() -> BoolAlias
    uses (aliases: AliasPair)
{
    aliases.first
}

fn returned_effect_place_alias_is_exact() -> u256 {
    let mut cond = false
    let mut other = false
    let aliases = AliasPair {
        first: BoolAlias { value: mut cond },
        second: BoolAlias { value: mut other },
    }
    let returned = with (aliases) {
        first_alias()
    }
    returned.value = true

    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn returned_effect_value_alias_is_exact(flag: bool) -> u256 {
    let mut cond = false
    let mut other = false
    let aliases = AliasPair {
        first: BoolAlias { value: mut cond },
        second: BoolAlias { value: mut other },
    }
    let returned = with (if flag { aliases } else { aliases }) {
        first_alias()
    }
    returned.value = true

    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags.contains("cannot return a borrow derived from an effect parameter"),
        "{diags}"
    );
    assert!(
        !diags
            .contains("possibly uninitialized local in `fn returned_effect_place_alias_is_exact`"),
        "{diags}"
    );
    assert!(
        !diags
            .contains("possibly uninitialized local in `fn returned_effect_value_alias_is_exact`"),
        "{diags}"
    );
}

#[test]
fn call_result_provenance_summary_cycles_fall_back_conservatively() {
    let diags = definite_init_diags(
        r#"
fn direct_alias(value: mut bool, recurse: bool) -> mut bool {
    if recurse {
        direct_alias(value, recurse: false)
    } else {
        value
    }
}

fn mutual_alias_left(value: mut bool, recurse: bool) -> mut bool {
    if recurse {
        mutual_alias_right(value)
    } else {
        value
    }
}

fn mutual_alias_right(value: mut bool) -> mut bool {
    mutual_alias_left(value, recurse: false)
}

fn direct_recursive_result_is_conservative() -> u256 {
    let mut cond = true
    let returned = direct_alias(mut cond, recurse: false)
    cond = true
    returned = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn mutually_recursive_result_is_conservative() -> u256 {
    let mut cond = true
    let returned = mutual_alias_left(mut cond, recurse: false)
    cond = true
    returned = false
    let mut value: u256
    if cond {
        value = 42
    }
    value
}
"#,
    );
    for function in [
        "direct_recursive_result_is_conservative",
        "mutually_recursive_result_is_conservative",
    ] {
        assert!(
            diags.contains(&format!("possibly uninitialized local in `fn {function}`")),
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
fn tracks_constant_array_elements_and_dynamic_reads() {
    let diags = definite_init_diags(
        r#"
fn initialized_elements(index: usize) -> u256 {
    let mut values: [u256; 2]
    values[0] = 20
    values[1] = 22
    values[index]
}

fn partial_dynamic_read(index: usize) -> u256 {
    let mut values: [u256; 2]
    values[0] = 42
    values[index]
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn partial_dynamic_read`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn tracks_stable_dynamic_and_singleton_array_writes_without_overgeneralizing() {
    let diags = definite_init_diags(
        r#"
use core::functional::Fn

fn same_dynamic_index(index: usize) -> u256 {
    let mut values: [u256; 2]
    values[index] = 42
    values[index]
}

fn captured_immutable_dynamic_index(index: usize) -> u256 {
    let run = || {
        let mut values: [u256; 2]
        values[index] = 42
        values[index]
    }
    run.call()
}

fn same_mutable_dynamic_index(mut index: own usize) -> u256 {
    let mut values: [u256; 2]
    values[index] = 42
    values[index]
}

fn same_dynamic_index_across_join(index: usize, cond: bool) -> u256 {
    let mut values: [u256; 2]
    values[index] = 42
    if cond {}
    values[index]
}

fn correlated_branch_indices(cond: bool, left: usize, right: usize) -> u256 {
    let mut write_index: usize
    let mut read_index: usize
    if cond {
        write_index = left
        read_index = left
    } else {
        write_index = right
        read_index = right
    }
    let mut values: [u256; 2]
    values[write_index] = 42
    values[read_index]
}

fn path_specific_write_before_join(cond: bool, left: usize, right: usize) -> u256 {
    let mut index: usize
    let mut values: [u256; 2]
    if cond {
        index = left
        values[index] = 42
    } else {
        index = right
        values[index] = 42
    }
    values[index]
}

fn literal_path_specific_write_before_join(cond: bool) -> u256 {
    let mut index: usize
    let mut values: [u256; 2]
    if cond {
        index = 0
        values[index] = 42
    } else {
        index = 1
        values[index] = 42
    }
    values[index]
}

fn same_dynamic_index_within_each_loop_iteration(mut index: own usize) -> u256 {
    let mut values: [u256; 2]
    while true {
        values[index] = 42
        let result = values[index]
        if index == 1 {
            return result
        }
        index = 1
    }
    0
}

fn correlated_loop_indices(left: usize, right: usize) -> u256 {
    let mut values: [u256; 2]
    let mut write_index = left
    let mut read_index = left
    let mut first = true
    while true {
        values[write_index] = 42
        let value = values[read_index]
        if !first {
            return value
        }
        write_index = right
        read_index = right
        first = false
    }
    0
}

fn unrelated_alias_mutation_preserves_dynamic_index(
    mut index: own usize,
    mut other: own usize,
) -> u256 {
    let _index_alias = mut index
    let other_alias = mut other
    let mut values: [u256; 2]
    values[index] = 42
    set_one(other_alias)
    values[index]
}

fn dead_index_alias_and_unrelated_mutation(
    mut index: own usize,
    mut other: own usize,
) -> u256 {
    let dead_alias = mut index
    let _ = dead_alias
    let other_alias = mut other
    let mut values: [u256; 2]
    values[index] = 42
    set_one(other_alias)
    values[index]
}

fn singleton_dynamic_write(index: usize) -> u256 {
    let mut values: [u256; 1]
    values[index] = 42
    values[0]
}

fn distinct_dynamic_indices(write_index: usize, read_index: usize) -> u256 {
    let mut values: [u256; 2]
    values[write_index] = 42
    values[read_index]
}

fn mutated_dynamic_index(mut index: own usize) -> u256 {
    let mut values: [u256; 2]
    values[index] = 42
    index = 1
    values[index]
}

fn maybe_mutated_dynamic_index(mut index: own usize, cond: bool) -> u256 {
    let mut values: [u256; 2]
    values[index] = 42
    if cond {
        index = 1
    }
    values[index]
}

fn aliased_dynamic_index(mut index: own usize) -> u256 {
    let target = mut index
    let mut values: [u256; 2]
    values[index] = 42
    target = 1
    values[index]
}

fn swapped_branch_indices(cond: bool, left: usize, right: usize) -> u256 {
    let mut write_index: usize
    let mut read_index: usize
    if cond {
        write_index = left
        read_index = right
    } else {
        write_index = right
        read_index = left
    }
    let mut values: [u256; 2]
    values[write_index] = 42
    values[read_index]
}

fn set_one(index: mut usize) {
    index = 1
}

fn call_mutated_dynamic_index(mut index: own usize) -> u256 {
    let alias = mut index
    let mut values: [u256; 2]
    values[index] = 42
    set_one(alias)
    values[index]
}

fn call_mutation_before_dynamic_access(mut index: own usize) -> u256 {
    let alias = mut index
    set_one(alias)
    let mut values: [u256; 2]
    values[index] = 42
    values[index]
}

fn set_index_effect()
    uses (index: mut usize)
{
    index = 1
}

fn effect_mutated_dynamic_index(mut index: own usize) -> u256 {
    let mut values: [u256; 2]
    values[index] = 42
    with (mut index) {
        set_index_effect()
    }
    values[index]
}

fn effect_mutation_before_dynamic_access(mut index: own usize) -> u256 {
    with (mut index) {
        set_index_effect()
    }
    let mut values: [u256; 2]
    values[index] = 42
    values[index]
}

fn partial_constant_then_dynamic(index: usize) -> u256 {
    let mut values: [u256; 2]
    values[0] = 42
    values[index]
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn distinct_dynamic_indices`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn mutated_dynamic_index`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn maybe_mutated_dynamic_index`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn aliased_dynamic_index`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn swapped_branch_indices`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn call_mutated_dynamic_index`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn effect_mutated_dynamic_index`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn partial_constant_then_dynamic`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        8,
        "{diags}"
    );
}

#[test]
fn checks_value_and_place_effect_operands() {
    let diags = definite_init_diags(
        r#"
fn read_effect() -> u256
    uses (value: u256)
{
    value
}

fn mutate_effect()
    uses (value: mut u256)
{
    value += 1
}

fn uninitialized_value_effect() -> u256 {
    let mut value: u256
    with (value) {
        read_effect()
    }
}

fn uninitialized_place_effect() {
    let mut value: u256
    with (mut value) {
        mutate_effect()
    }
}

fn initialized_effects() -> u256 {
    let mut value: u256
    value = 41
    with (mut value) {
        mutate_effect()
    }
    with (value) {
        read_effect()
    }
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn uninitialized_value_effect`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn uninitialized_place_effect`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2,
        "{diags}"
    );
}

#[test]
fn program_point_constants_survive_only_later_mutations() {
    let diags = definite_init_diags(
        r#"
enum Choice {
    Initialize,
    Skip,
}

fn bool_later_mutation() -> u256 {
    let mut cond = true
    let mut value: u256
    if cond {
        value = 42
    }
    cond = false
    value
}

fn enum_later_mutation() -> u256 {
    let mut choice = Choice::Initialize
    let mut value: u256
    match choice {
        Choice::Initialize => value = 42
        Choice::Skip => {}
    }
    choice = Choice::Skip
    value
}

fn bool_direct_reassignment() -> u256 {
    let mut cond = false
    cond = true
    let mut value: u256
    if cond {
        value = 42
    }
    value
}

fn enum_direct_reassignment() -> u256 {
    let mut choice = Choice::Skip
    choice = Choice::Initialize
    let mut value: u256
    match choice {
        Choice::Initialize => value = 42
        Choice::Skip => {}
    }
    value
}

fn reflexive_identity(index: usize) -> u256 {
    let mut value: u256
    if index == index {
        value = 42
    }
    value
}

fn copied_identity(index: usize) -> u256 {
    let copied = index
    let mut value: u256
    if index == copied {
        value = 42
    }
    value
}

fn correlated_phi_identity(cond: bool, left: usize, right: usize) -> u256 {
    let mut lhs: usize
    let mut rhs: usize
    if cond {
        lhs = left
        rhs = left
    } else {
        lhs = right
        rhs = right
    }
    let mut value: u256
    if lhs == rhs {
        value = 42
    }
    value
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn distinct_phi_identities_do_not_fold_as_equal() {
    let diags = definite_init_diags(
        r#"
fn swapped_phi_identity(cond: bool, left: usize, right: usize) -> u256 {
    let mut lhs: usize
    let mut rhs: usize
    if cond {
        lhs = left
        rhs = right
    } else {
        lhs = right
        rhs = left
    }
    let mut value: u256
    if lhs == rhs {
        value = 42
    }
    value
}

fn distinct_dynamic_identity(left: usize, right: usize) -> u256 {
    let mut value: u256
    if left == right {
        value = 42
    }
    value
}
"#,
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn swapped_phi_identity`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn distinct_dynamic_identity`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        2,
        "{diags}"
    );
}

#[test]
fn executable_edges_exclude_constant_false_loop_backedges() {
    let diags = definite_init_diags(
        r#"
enum Choice {
    Loop,
    Exit,
}

fn bool_dead_loop() -> u256 {
    let mut flag = false
    let mut value: u256
    while flag {
        flag = true
        let _ = value
    }
    value = 42
    value
}

fn enum_dead_loop() -> u256 {
    let mut choice = Choice::Exit
    let mut value: u256
    while choice == Choice::Loop {
        choice = Choice::Loop
        let _ = value
    }
    value = 42
    value
}
"#,
    );
    assert!(diags.is_empty(), "{diags}");
}

#[test]
fn loop_phi_edges_substitute_their_selected_initialized_elements() {
    let diags = definite_init_diags(
        r#"
fn initialized_on_entry_and_backedge(initial: usize) -> u256 {
    let mut values: [u256; 2]
    let mut index = initial
    values[index] = 42
    let mut first = true
    while true {
        let result = values[index]
        if !first {
            return result
        }
        index = 1
        values[index] = 42
        first = false
    }
    0
}

fn unchanged_backedge_index(initial: usize) -> u256 {
    let mut values: [u256; 2]
    let index = initial
    values[index] = 42
    let mut first = true
    while true {
        let result = values[index]
        if !first {
            return result
        }
        first = false
    }
    0
}

fn missing_backedge_initialization(initial: usize) -> u256 {
    let mut values: [u256; 2]
    let mut index = initial
    values[index] = 42
    let mut first = true
    while true {
        let result = values[index]
        if !first {
            return result
        }
        index = 1
        first = false
    }
    0
}

fn set_one(_ index: mut usize) {
    index = 1
}

fn mutated_backedge_index_without_initialization(initial: usize) -> u256 {
    let mut values: [u256; 2]
    let mut index = initial
    values[index] = 42
    let mut first = true
    while true {
        let result = values[index]
        if !first {
            return result
        }
        let alias = mut index
        set_one(alias)
        first = false
    }
    0
}

fn set_index_effect()
    uses (index: mut usize)
{
    index = 1
}

fn effect_mutated_backedge_index_without_initialization(initial: usize) -> u256 {
    let mut values: [u256; 2]
    let mut index = initial
    values[index] = 42
    let mut first = true
    while true {
        let result = values[index]
        if !first {
            return result
        }
        with (mut index) {
            set_index_effect()
        }
        first = false
    }
    0
}
"#,
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn initialized_on_entry_and_backedge`"),
        "{diags}"
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn unchanged_backedge_index`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn missing_backedge_initialization`"),
        "{diags}"
    );
    assert!(
        diags.contains(
            "possibly uninitialized local in `fn mutated_backedge_index_without_initialization`"
        ),
        "{diags}"
    );
    assert!(
        diags.contains(
            "possibly uninitialized local in `fn effect_mutated_backedge_index_without_initialization`"
        ),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        3,
        "{diags}"
    );
}

#[test]
fn nested_phi_substitutions_do_not_cross_correlate_index_positions() {
    let diags = definite_init_diags(
        r#"
fn nested_correlated_indices(cond: bool) -> u256 {
    let mut outer: usize
    let mut inner: usize
    let mut values: [[u256; 2]; 2]
    if cond {
        outer = 0
        inner = 1
        values[outer][inner] = 42
    } else {
        outer = 1
        inner = 0
        values[outer][inner] = 42
    }
    values[outer][inner]
}

fn nested_swapped_indices(cond: bool) -> u256 {
    let mut outer: usize
    let mut inner: usize
    let mut values: [[u256; 2]; 2]
    if cond {
        outer = 0
        inner = 1
        values[outer][inner] = 42
    } else {
        outer = 1
        inner = 0
        values[outer][inner] = 42
    }
    values[inner][outer]
}
"#,
    );
    assert!(
        !diags.contains("possibly uninitialized local in `fn nested_correlated_indices`"),
        "{diags}"
    );
    assert!(
        diags.contains("possibly uninitialized local in `fn nested_swapped_indices`"),
        "{diags}"
    );
    assert_eq!(
        diags.matches("possibly uninitialized local in `fn").count(),
        1,
        "{diags}"
    );
}

#[test]
fn executable_cfg_keeps_dynamic_branches_and_refines_known_ones() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "executable_cfg_edges.fe".into(),
        r#"
enum Choice {
    Initialize,
    Skip,
}

fn known_bool() -> u256 {
    let cond = true
    if cond { 42 } else { 0 }
}

fn known_enum() -> u256 {
    let choice = Choice::Initialize
    match choice {
        Choice::Initialize => 42
        Choice::Skip => 0
    }
}

fn dynamic(cond: bool) -> u256 {
    if cond { 42 } else { 0 }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    let mut widths = std::collections::HashMap::new();
    for item in top_mod.all_items(&db) {
        let ItemKind::Func(func) = item else {
            continue;
        };
        let Some(name) = func.name(&db).to_opt() else {
            continue;
        };
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(*func)),
        );
        let body = normalize_semantic_body(&db, instance).expect("normalized body");
        let successors = normalized_cfg_successors(&db, &body);
        let branch_widths = body
            .blocks
            .iter()
            .enumerate()
            .filter_map(|(idx, block)| {
                matches!(
                    &block.terminator.kind,
                    NSTerminatorKind::Branch { .. } | NSTerminatorKind::MatchEnum { .. }
                )
                .then_some(successors[idx].len())
            })
            .collect::<Vec<_>>();
        widths.insert(name.data(&db).to_string(), branch_widths);
    }
    assert_eq!(widths["known_bool"], [1]);
    assert_eq!(widths["known_enum"], [1]);
    assert_eq!(widths["dynamic"], [2]);
}
