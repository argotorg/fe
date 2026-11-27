//! Behavioral discriminators for the evidence-only core. These are new tests,
//! not claimed reproductions of the original p01-p22 packet.
use fe_hir::{
    analysis::{
        analysis_pass::AnalysisPassManager,
        semantic::{
            GenericSubst, SemConstScalar, SemConstValue, SemanticAccessAnalysisPass,
            SemanticInstance, eval_body_owner_const, root_semantic_instance_key,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::ItemKind,
    test_db::{HirAnalysisTestDb, find_func, format_diagnostics},
};
use num_traits::ToPrimitive;

fn values(source: &str, expected: &[(&str, usize)]) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("named_evidence.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for &(name, expected) in expected {
        let owner = BodyOwner::Func(find_func(&db, module, name), None);
        let evaluated =
            eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
        let detail = format!("{evaluated:?}");
        let value = evaluated
            .into_ready()
            .unwrap_or_else(|| panic!("{name} did not evaluate: {detail}"));
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&db)
        else {
            panic!("{name} was not an integer: {:?}", value.value(&db));
        };
        assert_eq!(value.to_usize(), Some(expected), "{name}");
    }
}

// A program the compiler accepts at check time but rejects where the instance
// is built: `name` does not evaluate, because an instance it reaches is not
// admitted, and the semantic access analysis reports `16-0019` on `goal`.
// Compile-time evaluation drops the diagnostic, so it is read from the
// analysis of the module, with a runtime function that demands `name` when the
// constant roots alone report nothing.
fn refused(source: &str, name: &str, goal: &str) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("named_refused.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, name), None);
    let evaluated =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let detail = format!("{evaluated:?}");
    assert!(
        evaluated.into_ready().is_none(),
        "{name} was accepted: {detail}"
    );
    assert!(
        detail.contains("InvalidBody"),
        "{name} failed for another reason: {detail}"
    );
    let access = |source: &str| {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("named_refused_access.fe".into(), source);
        let (module, _) = db.top_mod(file);
        let mut passes = AnalysisPassManager::new();
        passes.add_module_pass("SemanticAccess", Box::new(SemanticAccessAnalysisPass));
        format_diagnostics(&db, &passes.run_on_module(&db, module))
    };
    let mut report = access(source);
    if !report.contains("error[16-0019]") {
        report = access(&format!("{source}\nfn demand() -> u256 {{ {name}() }}\n"));
    }
    assert!(report.contains("error[16-0019]"), "{name}: {report}");
    assert!(report.contains(&format!("`{goal}`")), "{name}: {report}");
}

const PICKS: &str = r#"
struct Item {}
trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
impl Pick for Item as Discount { const fn value() -> u256 { 70 } }
impl Pick for Item as Tiny { const fn value() -> u256 { 3 } }
"#;

#[test]
fn unresolved_calls_inside_choices_keep_the_annotation_diagnostic() {
    for call in ["read()", "forward()"] {
        let source = format!(
            r#"{PICKS}
fn read<T: Pick>() -> u256 {{ T::value() }}
fn forward<T: Pick>() -> u256 {{ read<T>() }}
fn bad() {{ let result = {call} }}
"#
        );
        let ordinary = diagnostics(&source);
        assert!(ordinary.contains(&"8-0031".to_string()), "{ordinary:?}");
        let chosen = diagnostics(&source.replace(
            &format!("let result = {call}"),
            &format!("with (Discount) {{ let result = {call} }}"),
        ));
        assert_eq!(chosen, ordinary, "{call}");
    }
}

#[test]
fn trait_keyed_effects_transport_chosen_evidence() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "chosen_effect_provider.fe".into(),
        r#"
struct Item {}
trait Price { fn amount(self) -> u256 }
impl Price for Item { fn amount(self) -> u256 { 100 } }
impl Price for Item as Wholesale { fn amount(self) -> u256 { 70 } }
fn charge() -> u256 uses (p: Price) { p.amount() }
fn relay() -> u256 uses (p: Price) { charge() }
fn ordinary() -> u256 { with (Price = Item {}) { charge() } }
fn simultaneous() -> u256 { with (Wholesale, Price = Item {}) { charge() } }
fn choice_outer() -> u256 { with (Wholesale) { with (Price = Item {}) { charge() } } }
fn provider_outer() -> u256 { with (Price = Item {}) { with (Wholesale) { charge() } } }
fn forwarded() -> u256 { with (Wholesale, Price = Item {}) { relay() } }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    for name in [
        "simultaneous",
        "choice_outer",
        "provider_outer",
        "forwarded",
    ] {
        let owner = BodyOwner::Func(find_func(&db, module, name), None);
        let root = SemanticInstance::new(&db, root_semantic_instance_key(&db, owner).unwrap());
        let mut call = root.callees(&db)[0].key;
        if name == "forwarded" {
            let relay = SemanticInstance::new(&db, call);
            call = relay.callees(&db)[0].key;
        }
        assert!(
            call.evidence(&db)
                .entries(&db)
                .iter()
                .flatten()
                .any(|proof| proof.has_named_impl(&db)),
            "{name} lost its selected provider dictionary"
        );
    }
}

#[test]
fn failed_chosen_effect_provider_conditions_do_not_fall_back() {
    let source = r#"
struct Item {}
struct Box<T> { value: T }
trait Gate {}
trait Price { fn amount(self) -> u256 }
impl<T> Price for Box<T> { fn amount(self) -> u256 { 100 } }
impl<T: Gate> Price for Box<T> as RequireGate { fn amount(self) -> u256 { 70 } }
fn charge() -> u256 uses (p: Price) { p.amount() }
fn chosen() -> u256 { with (RequireGate, Price = Box { value: Item {} }) { charge() } }
"#;
    let codes = diagnostics(source);
    assert_eq!(
        codes,
        ["8-0075"],
        "the effect provider needs its chosen proof"
    );
    assert!(diagnostics(&source.replace("RequireGate, ", "")).is_empty());
}

#[test]
fn lexical_override_restores_and_concrete_calls_mask_choices() {
    values(
        &format!(
            r#"{PICKS}
const fn helper() -> u256 {{ Item::value() }}
const fn ordinary() -> u256 {{ Item::value() }}
const fn chosen() -> u256 {{ with (Discount) {{ Item::value() }} }}
const fn masked() -> u256 {{ with (Discount) {{ helper() }} }}
const fn nested() -> u256 {{ with (Discount) {{
    let a = Item::value()
    let b = with (Tiny) {{ Item::value() }}
    a + b + Item::value()
}} }}
const fn after() -> u256 {{ let _ = with (Discount) {{ Item::value() }}
 Item::value() }}
"#
        ),
        &[
            ("ordinary", 100),
            ("chosen", 70),
            ("masked", 100),
            ("nested", 143),
            ("after", 100),
        ],
    );
}

#[test]
fn concrete_declared_bounds_transport_distinct_proofs() {
    values(
        &format!(
            r#"{PICKS}
const fn bound<T: Pick>() -> u256 {{ T::value() }}
const fn relay<T: Pick>() -> u256 {{ bound<T>() }}
const fn chosen() -> u256 {{ with (Discount) {{ relay<Item>() }} }}
const fn ordinary() -> u256 {{ relay<Item>() }}
const fn tiny() -> u256 {{ with (Tiny) {{ relay<Item>() }} }}
const fn both() -> u256 {{ chosen() + ordinary() + tiny() }}
"#
        ),
        &[
            ("chosen", 70),
            ("ordinary", 100),
            ("tiny", 3),
            ("both", 173),
        ],
    );
}

#[test]
fn implementation_conditions_inherit_lexical_choices() {
    values(
        &format!(
            r#"{PICKS}
struct Box<T> {{ value: T }}
trait Total {{ const fn total() -> u256 }}
impl<T: Pick> Total for Box<T> {{ const fn total() -> u256 {{ T::value() }} }}
const fn ordinary() -> u256 {{ Box<Item>::total() }}
const fn chosen() -> u256 {{ with (Discount) {{ Box<Item>::total() }} }}
"#
        ),
        &[("ordinary", 100), ("chosen", 70)],
    );
}

#[test]
fn implied_supertrait_follows_subtrait_proof_not_unrelated_override() {
    values(
        &format!(
            r#"{PICKS}
trait Sub: Pick {{
    const fn extra() -> u256 {{ Self::value() + 1 }}
}}
impl Sub for Item {{}}
const fn via<T: Sub>() -> u256 {{ T::value() * 1000 + T::extra() }}
const fn ordinary() -> u256 {{ via<Item>() }}
const fn chosen() -> u256 {{ with (Discount) {{ via<Item>() }} }}
"#
        ),
        &[("ordinary", 100101), ("chosen", 100101)],
    );
}

#[test]
fn named_methods_see_their_own_implementation() {
    values(
        r#"
struct Item {}
trait Pair {
    const fn base() -> u256
    const fn doubled() -> u256
}
impl Pair for Item {
    const fn base() -> u256 { 100 }
    const fn doubled() -> u256 { Self::base() * 2 }
}
impl Pair for Item as Small {
    const fn base() -> u256 { 7 }
    const fn doubled() -> u256 { Self::base() * 2 }
}
const fn ordinary() -> u256 { Item::doubled() }
const fn chosen() -> u256 { with (Small) { Item::doubled() } }
"#,
        &[("ordinary", 200), ("chosen", 14)],
    );
}

#[test]
fn a_named_wrapper_can_explicitly_call_the_ordinary_implementation() {
    values(
        &format!(
            r#"{PICKS}
impl Pick for Item as Markup {{
    const fn value() -> u256 {{ with (<Item as Pick>) {{ Item::value() + 1 }} }}
}}
const fn chosen() -> u256 {{ with (Markup) {{ Item::value() }} }}
"#
        ),
        &[("chosen", 101)],
    );
}

fn diagnostics(source: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("named_negative.fe".into(), source);
    let (module, _) = db.top_mod(file);
    fe_hir::analysis::initialize_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(&db).error_code.to_string())
        .collect()
}

#[test]
fn concrete_bounds_keep_the_base_language_diagnostic() {
    let codes = diagnostics(&format!(
        r#"{PICKS}
const fn concrete_bound() -> u256 where Item: Pick {{ Item::value() }}
"#
    ));
    assert!(codes.contains(&"6-0005".to_string()), "{codes:?}");
}

#[test]
fn named_only_implementation_is_available_inside_its_scope() {
    values(
        r#"
struct Item {}
trait Pick { const fn value() -> u256 }
impl Pick for Item as Only { const fn value() -> u256 { 23 } }
const fn chosen() -> u256 { with (Only) { Item::value() } }
"#,
        &[("chosen", 23)],
    );
}

#[test]
fn failed_selected_conditions_do_not_fall_back_to_ordinary() {
    let codes = diagnostics(
        r#"
struct Item {}
trait Missing {}
trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
impl<T: Missing> Pick for T as NeedsMissing { const fn value() -> u256 { 7 } }
const fn chosen() -> u256 { with (NeedsMissing) { Item::value() } }
"#,
    );
    assert!(
        !codes.is_empty(),
        "a selected implementation with an unproved condition must be diagnosed"
    );
}

#[test]
fn cyclic_selected_conditions_do_not_fall_back_to_ordinary() {
    let codes = diagnostics(
        r#"
struct Item {}
trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
impl<T: Pick> Pick for T as Recursive { const fn value() -> u256 { 7 } }
const fn chosen() -> u256 { with (Recursive) { Item::value() } }
"#,
    );
    assert!(
        !codes.is_empty(),
        "a cyclic selected proof must be diagnosed"
    );
}

#[test]
fn associated_outputs_agree_at_the_declaration() {
    for (ordinary, named, accepted) in [
        ("type Out = u256", "type Out = u256", true),
        ("type Out = u256", "type Out = bool", false),
    ] {
        let source = format!(
            "struct Item {{}}\ntrait Output {{ type Out }}\nimpl Output for Item {{ {ordinary} }}\nimpl Output for Item as Named {{ {named} }}"
        );
        let codes = diagnostics(&source);
        assert_eq!(codes.is_empty(), accepted, "{source}: {codes:?}");
        if !accepted {
            assert!(codes.contains(&"5-0005".to_string()), "{codes:?}");
        }
    }
    for (ordinary, named, accepted) in [("10", "5 + 5", true), ("10", "11", false)] {
        let source = format!(
            "struct Item {{}}\ntrait Size {{ const SIZE: u256 }}\nimpl Size for Item {{ const SIZE: u256 = {ordinary} }}\nimpl Size for Item as Named {{ const SIZE: u256 = {named} }}"
        );
        let codes = diagnostics(&source);
        assert_eq!(codes.is_empty(), accepted, "{source}: {codes:?}");
        if !accepted {
            assert!(codes.contains(&"5-0005".to_string()), "{codes:?}");
        }
    }
}

#[test]
fn supertrait_projection_preserves_the_subproofs_conditions() {
    values(
        &format!(
            r#"{PICKS}
struct Box<T> {{ value: T }}
impl<T: Pick> Pick for Box<T> {{ const fn value() -> u256 {{ T::value() }} }}
trait Sub: Pick {{ const fn extra() -> u256 {{ Self::value() + 1 }} }}
impl<T: Pick> Sub for Box<T> {{}}
const fn via<T: Sub>() -> u256 {{ T::value() * 1000 + T::extra() }}
const fn ordinary() -> u256 {{ via<Box<Item>>() }}
const fn chosen() -> u256 {{ with (Discount) {{ via<Box<Item>>() }} }}
"#
        ),
        &[("ordinary", 100101), ("chosen", 70071)],
    );
}

#[test]
fn associated_type_bounds_follow_the_implementation_proof() {
    values(
        &format!(
            r#"{PICKS}
struct Other {{}}
impl Pick for Other {{ const fn value() -> u256 {{ 5 }} }}
struct Wrap<T> {{ t: T }}
trait Cur {{
    type Cursor: Pick
    const fn cvalue() -> u256 {{ Self::Cursor::value() }}
}}
impl Cur for Other {{ type Cursor = Item }}
impl<T: Pick> Cur for Wrap<T> {{ type Cursor = T }}
const fn via<C: Cur>() -> u256 {{ C::cvalue() }}
const fn relay<T: Pick>() -> u256 {{ via<Wrap<T>>() }}
const fn concrete() -> u256 {{ Other::cvalue() }}
const fn concrete_chosen() -> u256 {{ with (Discount) {{ Other::cvalue() }} }}
const fn generic() -> u256 {{ via<Wrap<Item>>() }}
const fn generic_chosen() -> u256 {{ with (Discount) {{ via<Wrap<Item>>() }} }}
const fn relayed() -> u256 {{ with (Discount) {{ relay<Item>() }} }}
const fn relayed_tiny() -> u256 {{ with (Tiny) {{ relay<Item>() }} }}
"#
        ),
        &[
            ("concrete", 100),
            ("concrete_chosen", 100),
            ("generic", 100),
            ("generic_chosen", 70),
            ("relayed", 70),
            ("relayed_tiny", 3),
        ],
    );
}

#[test]
fn named_conditions_on_bounded_associated_types_are_selected() {
    values(
        &format!(
            r#"{PICKS}
struct Other {{}}
trait Cur {{ type Cursor: Pick }}
impl Cur for Other {{ type Cursor = Item }}
struct Box<T> {{ v: T }}
trait Total {{ const fn total() -> u256 }}
impl<T: Cur> Total for Box<T> {{ const fn total() -> u256 {{ 1 }} }}
impl<T: Cur> Total for Box<T> as Named {{ const fn total() -> u256 {{ 2 }} }}
const fn ordinary() -> u256 {{ Box<Other>::total() }}
const fn chosen() -> u256 {{ with (Named) {{ Box<Other>::total() }} }}
"#
        ),
        &[("ordinary", 1), ("chosen", 2)],
    );
}

#[test]
fn implied_bounds_normalized_by_an_equality_still_project() {
    values(
        &format!(
            r#"{PICKS}
trait Tag {{ const fn tag() -> u256 }}
impl Tag for u8 {{ const fn tag() -> u256 {{ 1 }} }}
struct Wrap<T> {{ t: T }}
trait Cur {{ type Cursor: Tag }}
impl<T: Tag> Cur for Wrap<T> {{ type Cursor = T }}
const fn h<C: Cur<Cursor = U>, U>() -> u256 {{ U::tag() }}
const fn ordinary() -> u256 {{ h<Wrap<u8>, u8>() }}
const fn chosen() -> u256 {{ with (Discount) {{ h<Wrap<u8>, u8>() }} }}
"#
        ),
        &[("ordinary", 1), ("chosen", 1)],
    );
}

// A bound that an equality makes concrete (`u8: Copy` from `B: Tr<A = u8>`,
// `Item: Pick` from `B: Tr<A = Item>`) has no slot among the assumptions of
// the body that declares the equality. It is answered by the component of the
// dictionary the body received for `B`, never by a fresh selection.
const CONCRETE_IMPLIED: &str = r#"
struct Other {}
trait Mark { const fn mark() -> u256 }
impl Mark for Other { const fn mark() -> u256 { 1 } }
impl Mark for Other as Aside { const fn mark() -> u256 { 2 } }
struct Slot {}
trait Tr { type A: Copy
    const fn get() -> u256 }
impl Tr for Slot { type A = u8
    const fn get() -> u256 { 7 } }
"#;

#[test]
fn a_choice_that_does_not_name_a_concrete_implied_bound_leaves_it_to_the_dictionary() {
    values(
        &format!(
            r#"{PICKS}{CONCRETE_IMPLIED}
const fn plain<B: Tr<A = u8>>() -> u256 {{ B::get() }}
const fn chosen<B: Tr<A = u8>>() -> u256 {{ with (Aside) {{ B::get() }} }}
const fn named<B: Tr<A = u8>>() -> u256 {{ with (Discount) {{ B::get() }} }}
const fn nested<B: Tr<A = u8>>() -> u256 {{ with (Discount) {{ with (Aside) {{ B::get() }} }} }}
const fn run_plain() -> u256 {{ plain<Slot>() }}
const fn run_chosen() -> u256 {{ chosen<Slot>() }}
const fn run_named() -> u256 {{ named<Slot>() }}
const fn run_nested() -> u256 {{ nested<Slot>() }}
"#
        ),
        &[
            ("run_plain", 7),
            ("run_chosen", 7),
            ("run_named", 7),
            ("run_nested", 7),
        ],
    );
}

// `caller` is bounded by `B: Tr<A = Item>` where `Tr::A: Pick`: `Item: Pick`
// follows from the equality. `read` uses it as `<B::A as Pick>` and, through
// `B::via()`, as the implementation's own condition. Both read one dictionary.
const WRAPPED_PICK: &str = r#"
struct Wrap<T> { t: T }
trait Tr { type A: Pick
    const fn via() -> u256 }
impl<T: Pick> Tr for Wrap<T> { type A = T
    const fn via() -> u256 { T::value() } }
const fn read<B: Tr>() -> u256 { <B::A as Pick>::value() * 1000 + B::via() }
struct Other {}
trait Mark { const fn mark() -> u256 }
impl Mark for Other { const fn mark() -> u256 { 1 } }
impl Mark for Other as Aside { const fn mark() -> u256 { 2 } }
"#;

#[test]
fn an_implied_bound_answers_from_the_received_dictionary_without_a_local_choice() {
    values(
        &format!(
            r#"{PICKS}{WRAPPED_PICK}
const fn caller<B: Tr<A = Item>>() -> u256 {{ read<B>() }}
const fn top_plain() -> u256 {{ caller<Wrap<Item>>() }}
const fn top() -> u256 {{ with (Discount) {{ caller<Wrap<Item>>() }} }}
"#
        ),
        &[("top_plain", 100100), ("top", 70070)],
    );
}

#[test]
fn an_implied_bound_answers_from_the_received_dictionary_under_a_choice_naming_it() {
    values(
        &format!(
            r#"{PICKS}{WRAPPED_PICK}
const fn caller<B: Tr<A = Item>>() -> u256 {{ with (Discount) {{ read<B>() }} }}
const fn caller_explicit<B: Tr<A = Item>>() -> u256 {{ with (<Item as Pick>) {{ read<B>() }} }}
const fn top_plain() -> u256 {{ caller<Wrap<Item>>() }}
const fn top() -> u256 {{ with (Discount) {{ caller<Wrap<Item>>() }} }}
const fn top_explicit() -> u256 {{ with (Discount) {{ caller_explicit<Wrap<Item>>() }} }}
"#
        ),
        &[
            ("top_plain", 100100),
            ("top", 70070),
            ("top_explicit", 70070),
        ],
    );
}

#[test]
fn an_implied_bound_answers_from_the_received_dictionary_under_an_unrelated_choice() {
    values(
        &format!(
            r#"{PICKS}{WRAPPED_PICK}
const fn caller<B: Tr<A = Item>>() -> u256 {{ with (Aside) {{ read<B>() }} }}
const fn top() -> u256 {{ with (Discount) {{ caller<Wrap<Item>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

// Two traits that each declare `A: Pick`. `target` takes `A` of both as one
// type `U`, so `U: Pick` is implied by two dictionaries. `L` holds the
// ordinary `Item: Pick`; `Wrap<Item>` holds the one chosen where it is
// proved.
const TWO_DICTIONARIES: &str = r#"
struct Wrap<T> { t: T }
struct L {}
trait Lt { type A: Pick }
trait Rt { type A: Pick }
impl Lt for L { type A = Item }
impl<T: Pick> Lt for Wrap<T> { type A = T }
impl<T: Pick> Rt for Wrap<T> { type A = T }
"#;

#[test]
fn dictionaries_that_project_different_implementations_are_refused() {
    refused(
        &format!(
            r#"{PICKS}{TWO_DICTIONARIES}
const fn target<B: Lt<A = U>, C: Rt<A = U>, U>() -> u256 {{ U::value() }}
const fn relay<B: Lt<A = Item>, C: Rt<A = Item>>() -> u256 {{
    with (Discount) {{ target<B, C, Item>() }}
}}
const fn top() -> u256 {{ with (Discount) {{ relay<L, Wrap<Item>>() }} }}
"#
        ),
        "top",
        "Item: Pick",
    );
}

#[test]
fn disagreeing_dictionaries_are_refused_when_the_goal_is_unused() {
    refused(
        &format!(
            r#"{PICKS}{TWO_DICTIONARIES}
const fn target<B: Lt<A = U>, C: Rt<A = U>, U>() -> u256 {{ 1 }}
const fn relay<B: Lt<A = Item>, C: Rt<A = Item>>() -> u256 {{
    with (Discount) {{ target<B, C, Item>() }}
}}
const fn top() -> u256 {{ with (Discount) {{ relay<L, Wrap<Item>>() }} }}
"#
        ),
        "top",
        "Item: Pick",
    );
}

#[test]
fn dictionaries_that_project_one_implementation_are_accepted() {
    values(
        &format!(
            r#"{PICKS}{TWO_DICTIONARIES}
const fn target<B: Lt<A = U>, C: Rt<A = U>, U>() -> u256 {{ U::value() }}
const fn relay<B: Lt<A = Item>, C: Rt<A = Item>>() -> u256 {{
    with (Discount) {{ target<B, C, Item>() }}
}}
const fn top() -> u256 {{ with (Discount) {{ relay<Wrap<Item>, Wrap<Item>>() }} }}
"#
        ),
        &[("top", 70)],
    );
}

// `read` bounds `X: Pick` explicitly and `B: Tr<A = X>` implies it. Called as
// `read<B, Item>` from a body whose `B: Tr<A = Item>`, the explicit bound
// selects the choice in scope, and the dictionary received for `B` holds the
// implementation chosen where `B` was proved. Both must be the same one.
const EXPLICIT_AND_IMPLIED: &str = r#"
const fn read_explicit<B: Tr<A = X>, X: Pick>() -> u256 { X::value() * 1000 + B::via() }
const fn caller<B: Tr<A = Item>>() -> u256 { with (Discount) { read_explicit<B, Item>() } }
"#;

#[test]
fn an_explicit_bound_that_differs_from_the_received_dictionary_is_refused() {
    refused(
        &format!(
            r#"{PICKS}{WRAPPED_PICK}{EXPLICIT_AND_IMPLIED}
const fn top() -> u256 {{ caller<Wrap<Item>>() }}
"#
        ),
        "top",
        "Item: Pick",
    );
}

#[test]
fn an_explicit_bound_that_matches_the_received_dictionary_is_accepted() {
    values(
        &format!(
            r#"{PICKS}{WRAPPED_PICK}{EXPLICIT_AND_IMPLIED}
const fn top() -> u256 {{ with (Discount) {{ caller<Wrap<Item>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

// A closure in the body adds its own bounds after the body's signature. The
// implied bound the body reads directly is still the dictionary's component:
// the ordinary `value` is never reached, only the chosen one. Closures are
// not evaluated at compile time, so the callees of the instances are read.
#[test]
fn a_body_with_a_closure_still_reads_implied_bounds_from_the_dictionary() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "named_closure.fe".into(),
        r#"
struct Item {}
trait Pick { fn value() -> u256 }
impl Pick for Item { fn value() -> u256 { 100 } }
impl Pick for Item as Discount { fn value() -> u256 { 70 } }
struct Wrap<T> { t: T }
trait Tr { type A: Pick
    fn via() -> u256 }
impl<T: Pick> Tr for Wrap<T> { type A = T
    fn via() -> u256 { T::value() } }
trait Price { fn amount(self) -> u256 }
struct Tag {}
impl Price for Tag { fn amount(self) -> u256 { 0 } }
fn read_closure<B: Tr>() -> u256 uses (p: Price) {
    let f = || p.amount()
    <B::A as Pick>::value() * 1000 + B::via() + f()
}
fn caller<B: Tr<A = Item>>() -> u256 uses (p: Price) { read_closure<B>() }
fn top() -> u256 { with (Discount, Price = Tag {}) { caller<Wrap<Item>>() } }
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "top"), None);
    let mut pending = vec![root_semantic_instance_key(&db, owner).expect("root key")];
    let mut seen = Vec::new();
    let mut values = Vec::new();
    while let Some(key) = pending.pop() {
        if seen.contains(&key) {
            continue;
        }
        seen.push(key);
        if let BodyOwner::Func(func, _) = key.owner(&db)
            && func
                .name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "value")
            && let Some(ItemKind::ImplTrait(implementation)) = func.scope().parent_item(&db)
        {
            values.push(implementation.selection_name(&db).is_some());
        }
        for callee in SemanticInstance::new(&db, key).callees(&db) {
            pending.push(callee.key);
        }
    }
    assert!(!values.is_empty(), "no `value` implementation was reached");
    assert!(
        values.iter().all(|&named| named),
        "the ordinary `value` was reached beside the chosen one"
    );
}

#[test]
fn failed_conditions_on_bounded_associated_types_do_not_fall_back() {
    let codes = diagnostics(&format!(
        r#"{PICKS}
trait Missing {{}}
trait Cur {{ type Cursor: Pick }}
impl Cur for Item {{ type Cursor = Item }}
struct Box<T> {{ v: T }}
trait Total {{ const fn total() -> u256 }}
impl<T: Cur> Total for Box<T> {{ const fn total() -> u256 {{ 1 }} }}
impl<T: Cur + Missing> Total for Box<T> as Named {{ const fn total() -> u256 {{ 2 }} }}
const fn chosen() -> u256 {{ with (Named) {{ Box<Item>::total() }} }}
"#
    ));
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
}

#[test]
fn inferred_receiver_keeps_the_lexical_choice() {
    values(
        r#"
struct Item {}
trait Pick { const fn value(self) -> u256 }
impl Pick for Item { const fn value(self) -> u256 { 100 } }
impl Pick for Item as Discount { const fn value(self) -> u256 { 70 } }
const fn identity<T>(_ item: T) -> T { item }
const fn chosen() -> u256 {
    let item = identity(Item {})
    with (Discount) { item.value() }
}
"#,
        &[("chosen", 70)],
    );
}

#[test]
fn applied_named_arguments_preserve_the_callers_parameters() {
    values(
        r#"
struct Item {}
struct Other {}
trait Pick { const fn value() -> u256 }
impl<T> Pick for T { const fn value() -> u256 { 100 } }
impl<T> Pick for T as Small { const fn value() -> u256 { 7 } }
const fn one() -> u256 { with (Small<Item>) { Item::value() * 1000 + Other::value() } }
const fn generic<T>() -> u256 { with (Small<T>) { T::value() * 1000 + Other::value() } }
const fn two() -> u256 { generic<Item>() }
"#,
        &[("one", 7100), ("two", 7100)],
    );
}

#[test]
fn incompatible_supertrait_dictionaries_are_not_silently_merged() {
    let codes = diagnostics(&format!(
        r#"{PICKS}
struct Box<T> {{ value: T }}
impl<T: Pick> Pick for Box<T> {{ const fn value() -> u256 {{ T::value() }} }}
trait Left: Pick {{}}
trait Right: Pick {{}}
impl Left for Box<Item> {{}}
impl<T: Pick> Right for Box<T> {{}}
const fn diamond<T: Left + Right>() -> u256 {{ T::value() }}
const fn chosen() -> u256 {{ with (Discount) {{ diamond<Box<Item>>() }} }}
"#
    ));
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
    let codes = diagnostics(&format!(
        r#"{PICKS}
trait Sub: Pick {{}}
impl Sub for Item {{}}
const fn both<T: Sub + Pick>() -> u256 {{ T::value() }}
const fn chosen() -> u256 {{ with (Discount) {{ both<Item>() }} }}
"#
    ));
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
}

#[test]
fn transitive_supertrait_evidence_flows_through_conditions() {
    values(
        &format!(
            r#"{PICKS}
struct Box<T> {{ value: T }}
impl<T: Pick> Pick for Box<T> {{ const fn value() -> u256 {{ T::value() }} }}
trait Left: Pick {{}}
trait Right: Pick {{}}
impl<T: Pick> Left for Box<T> {{}}
impl<T: Pick> Right for Box<T> {{}}
trait Diamond: Left + Right {{ const N: u256 = 1 }}
impl<T: Left + Right> Diamond for T {{}}
const fn via<T: Diamond>() -> u256 {{ T::value() }}
const fn forward<T: Diamond>() -> u256 {{ via<T>() }}
struct Wrap<T> {{ value: T }}
trait Count {{ const N: u256 }}
impl<T: Diamond> Count for Wrap<T> {{ const N: u256 = 1 }}
trait Total {{ const fn total() -> u256 }}
impl<T: Diamond> Total for Wrap<T> as Through {{
    const fn total() -> u256 {{ T::value() }}
}}
const fn ordinary() -> u256 {{ forward<Box<Item>>() }}
const fn chosen() -> u256 {{ with (Discount) {{ forward<Box<Item>>() }} }}
const fn method() -> u256 {{ with (Discount, Through) {{ <Wrap<Box<Item>> as Total>::total() }} }}
const fn constant() -> u256 {{ with (Discount) {{ Box<Item>::N }} }}
const fn impl_constant() -> u256 {{ with (Discount) {{ <Wrap<Box<Item>> as Count>::N }} }}
const fn ordinary_constant() -> u256 {{ <Wrap<Box<Item>> as Count>::N }}
"#
        ),
        &[
            ("ordinary", 100),
            ("chosen", 70),
            ("method", 70),
            ("constant", 1),
            ("impl_constant", 1),
            ("ordinary_constant", 1),
        ],
    );
}

#[test]
fn transitive_conflicts_are_rejected_for_calls_and_constant_defaults() {
    for extra in ["", " + Pick"] {
        for access in ["via<Box<Item>>()", "Box<Item>::N"] {
            let source = format!(
                r#"{PICKS}
struct Box<T> {{ value: T }}
impl<T: Pick> Pick for Box<T> {{ const fn value() -> u256 {{ T::value() }} }}
trait Left: Pick {{}}
trait Right: Pick {{}}
impl Left for Box<Item> {{}}
impl<T: Pick> Right for Box<T> {{}}
trait Diamond: Left + Right {{ const N: u256 = 1 }}
impl<T: Left + Right{extra}> Diamond for T {{}}
const fn via<T: Diamond>() -> u256 {{ T::N }}
const fn chosen() -> u256 {{ with (Discount) {{ {access} }} }}
"#
            );
            let codes = diagnostics(&source);
            assert!(
                codes.contains(&"6-0003".to_string()),
                "{extra}, {access}: {codes:?}"
            );
            values(
                &source.replace(&format!("with (Discount) {{ {access} }}"), access),
                &[("chosen", 1)],
            );
        }
    }
}

#[test]
fn conflicting_private_implementation_conditions_are_rejected() {
    let source = format!(
        r#"{PICKS}
struct Box<T> {{ value: T }}
impl<T: Pick> Pick for Box<T> {{ const fn value() -> u256 {{ T::value() }} }}
trait Left: Pick {{}}
trait Right: Pick {{}}
impl Left for Box<Item> {{}}
impl<T: Pick> Right for Box<T> {{}}
struct Wrap<T> {{ value: T }}
trait Total {{ const fn total() -> u256 }}
impl<T: Left + Right> Total for Wrap<T> as Through {{
    const fn total() -> u256 {{ T::value() }}
}}
const fn read<T: Total>() -> u256 {{ T::total() }}
const fn chosen() -> u256 {{ with (Discount, Through) {{ read<Wrap<Box<Item>>>() }} }}
"#
    );
    let codes = diagnostics(&source);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
    values(
        &source.replace("Discount, Through", "Through"),
        &[("chosen", 100)],
    );
    values(
        &source.replace(
            "impl Left for Box<Item> {}",
            "impl<T: Pick> Left for Box<T> {}",
        ),
        &[("chosen", 70)],
    );
}

#[test]
fn written_types_refuse_selected_reads_but_concrete_helpers_mask_choices() {
    for value in ["Item::value()", "bound<Item>()"] {
        let codes = diagnostics(&format!(
            r#"{PICKS}
struct Array<const N: u256> {{}}
const fn bound<T: Pick>() -> u256 {{ T::value() }}
fn chosen() {{ with (Discount) {{ let x: Array<{{ {value} }}> = Array {{}} }} }}
"#
        ));
        assert!(codes.contains(&"8-0138".to_string()), "{value}: {codes:?}");
    }
    let source = format!(
        r#"{PICKS}
struct Array<const N: u256> {{}}
const fn helper() -> u256 {{ Item::value() }}
fn allowed() {{
    let before: Array<{{ Item::value() }}> = Array {{}}
    with (Discount) {{
        let inherited = before
        let masked: Array<{{ helper() }}> = Array {{}}
    }}
}}
"#
    );
    assert!(diagnostics(&source).is_empty());
}

#[test]
fn written_types_refuse_selected_reads_with_unmet_conditions() {
    for value in [
        "Item::value()",
        "bound<Item>()",
        "<Box<Item> as Total>::total()",
        "bound_total<Box<Item>>()",
    ] {
        let source = format!(
            r#"
struct Item {{}}
struct Box<T> {{ value: T }}
trait Missing {{}}
trait Pick {{ const fn value() -> u256 }}
impl Pick for Item {{ const fn value() -> u256 {{ 100 }} }}
impl<T: Missing> Pick for T as NeedsMissing {{ const fn value() -> u256 {{ 7 }} }}
trait Total {{ const fn total() -> u256 }}
impl<T: Pick> Total for Box<T> {{ const fn total() -> u256 {{ T::value() }} }}
struct Array<const N: u256> {{}}
const fn bound<T: Pick>() -> u256 {{ T::value() }}
const fn bound_total<T: Total>() -> u256 {{ T::total() }}
fn chosen() {{
    with (NeedsMissing) {{
        let x: Array<{{ {value} }}> = Array {{}}
        let y: Array<100> = x
    }}
}}
"#
        );
        let codes = diagnostics(&source);
        assert!(codes.contains(&"8-0138".to_string()), "{value}: {codes:?}");
        assert!(diagnostics(&source.replace("with (NeedsMissing) ", "")).is_empty());
    }
}

#[test]
fn a_written_alias_keeps_its_type_constant_provenance() {
    let codes = diagnostics(&format!(
        r#"{PICKS}
struct Array<const N: u256> {{}}
type Count = Array<{{ Item::value() }}>
fn chosen() {{ with (Discount) {{ let x: Count = Array {{}} }} }}
"#
    ));
    assert!(codes.contains(&"8-0138".to_string()), "{codes:?}");
}

#[test]
fn names_follow_module_visibility_and_imports() {
    let source = r#"
struct Item {}
trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
mod choices {
    use super::{Item, Pick}
    pub impl Pick for Item as Public { const fn value() -> u256 { 7 } }
    impl Pick for Item as Private { const fn value() -> u256 { 3 } }
}
use choices::Public as Imported
const fn chosen() -> u256 { with (Imported) { Item::value() } }
"#;
    values(source, &[("chosen", 7)]);
    let source = format!(
        "{source}\nconst fn hidden() -> u256 {{ with (choices::Private) {{ Item::value() }} }}"
    );
    assert_eq!(diagnostics(&source), vec!["2-0003"]);
}

#[test]
fn an_invisible_implementation_name_does_not_hide_a_visible_value() {
    let source = r#"
pub struct Store { pub n: u256 }
mod m {
    use super::Store
    trait Pick { fn value() -> u256 }
    impl Pick for Store as Name { fn value() -> u256 { 1 } }
    pub const Name: Store = Store { n: 7 }
}
use m::Name
fn read() -> u256 uses (s: Store) { s.n }
pub fn qualified() -> u256 { with (m::Name) { read() } }
pub fn imported() -> u256 { with (Name) { read() } }
"#;
    assert_eq!(diagnostics(source), Vec::<String>::new());
    // Without the value, the operand names only the implementation, which
    // is not visible here.
    let hidden = source.replace("    pub const Name: Store = Store { n: 7 }\n", "");
    assert!(
        diagnostics(&hidden).contains(&"2-0003".to_string()),
        "{:?}",
        diagnostics(&hidden)
    );
}

#[test]
fn an_ambiguous_implementation_name_does_not_hide_a_visible_value() {
    values(
        r#"
trait Pick {}
mod a {
    use super::Pick
    pub impl Pick for u8 as K {}
}
mod b {
    use super::Pick
    pub impl Pick for u16 as K {}
}
use a::*
use b::*
const K: u256 = 1
const fn read() -> u256 { K }
"#,
        &[("read", 1)],
    );
}

#[test]
fn named_import_reports_implementation_ambiguity() {
    let source = r#"
pub trait Pick {}
mod a {
    use super::Pick
    pub impl Pick for u8 as K {}
}
mod b {
    use super::Pick
    pub impl Pick for u16 as K {}
}
mod choices {
    pub use super::a::*
    pub use super::b::*
}
use choices::K as Selected
"#;
    assert_eq!(diagnostics(source), vec!["2-0004"]);

    let source = source.replace("pub use super::b::*", "pub const K: u256 = 7");
    values(
        &format!("{source}\nconst fn read() -> u256 {{ Selected }}"),
        &[("read", 7)],
    );
}

#[test]
fn associated_output_errors_blame_only_the_nonconforming_declaration() {
    for declarations in [
        "impl Output for Item as Good { type Out = u256 }\nimpl Output for Item as Bad { type Out = bool }",
        "impl Output for Item as Bad { type Out = bool }\nimpl Output for Item as Good { type Out = u256 }",
    ] {
        let source = format!(
            "struct Item {{}}\ntrait Output {{ type Out }}\nimpl Output for Item {{ type Out = u256 }}\n{declarations}"
        );
        assert_eq!(diagnostics(&source), vec!["5-0005"]);
    }
}

#[test]
fn applied_names_validate_their_arguments() {
    let source = r#"
struct Item {}
trait Pick { const fn value() -> u256 }
impl<T> Pick for T as Small { const fn value() -> u256 { 7 } }
const fn invalid() -> u256 { with (Small<3>) { Item::value() } }
"#;
    assert!(!diagnostics(source).is_empty());
}

#[test]
fn a_local_named_header_captures_its_enclosing_type_parameter() {
    values(
        r#"
struct Item {}
struct Other {}
trait Pick { const fn value() -> u256 }
impl<T> Pick for T { const fn value() -> u256 { 100 } }
const fn generic<T>() -> u256 {
    impl Pick for T as Local { const fn value() -> u256 { 7 } }
    with (Local) { T::value() * 1000 + Other::value() }
}
const fn chosen() -> u256 { generic<Item>() }
"#,
        &[("chosen", 7100)],
    );
}

#[test]
fn erased_type_arguments_and_const_call_arguments_keep_the_type_boundary() {
    for statement in [
        "let x: Drop<{ Item::value() }> = 0",
        "let x = count<{ Item::value() }>()",
    ] {
        let codes = diagnostics(&format!(
            r#"{PICKS}
type Drop<const N: u256> = u256
const fn count<const N: u256>() -> u256 {{ N }}
fn chosen() {{ with (Discount) {{ {statement} }} }}
"#
        ));
        assert!(
            codes.contains(&"8-0138".to_string()),
            "{statement}: {codes:?}"
        );
    }
}

#[test]
fn associated_constant_default_waits_for_its_implementation() {
    values(
        r#"
struct Item {}
trait Price {
    const fn price() -> u256
    const N: u256 = Self::price()
}
impl Price for Item { const fn price() -> u256 { 100 } }
const fn ordinary() -> u256 { Item::N }
"#,
        &[("ordinary", 100)],
    );
}

#[test]
fn inherited_associated_constant_compares_the_selected_methods() {
    let source = r#"
struct Item {}
trait Price {
    const fn price() -> u256 { 100 }
    const N: u256 = Self::price()
}
impl Price for Item {}
impl Price for Item as Alternate { const fn price() -> u256 { 100 } }
const fn chosen() -> u256 { with (Alternate) { Item::N } }
"#;
    values(source, &[("chosen", 100)]);
    let differing = source.replace("price() -> u256 { 100 } }", "price() -> u256 { 70 } }");
    assert_eq!(diagnostics(&differing), vec!["5-0005"]);
}

#[test]
fn generic_inherited_associated_constant_compares_selected_methods() {
    let source = r#"
struct Item<T> { value: T }
trait Price {
    const fn price() -> u256 { 100 }
    const N: u256 = Self::price()
}
impl<T> Price for Item<T> {}
impl<T> Price for Item<T> as Alternate { const fn price() -> u256 { 100 } }
"#;
    assert!(diagnostics(source).is_empty(), "{:?}", diagnostics(source));
    let differing = source.replace("price() -> u256 { 100 } }", "price() -> u256 { 70 } }");
    assert_eq!(diagnostics(&differing), vec!["5-0005"]);
}

#[test]
fn generic_associated_constant_comparison_keeps_selection_through_default_calls() {
    let source = r#"
struct Item<T> { value: T }
trait Price {
    const fn base() -> u256
    const fn price() -> u256 { Self::base() + 1 }
    const N: u256 = Self::price()
}
impl<T> Price for Item<T> { const fn base() -> u256 { 99 } }
impl<T> Price for Item<T> as Alternate { const fn base() -> u256 { 100 - 1 } }
"#;
    assert_eq!(diagnostics(source), Vec::<String>::new());
    let differing = source.replace("100 - 1", "100 - 2");
    assert_eq!(diagnostics(&differing), vec!["5-0005"]);
}

#[test]
fn associated_constant_compares_over_a_generic_overlap() {
    let source = r#"
struct Item<const N: usize> {}
trait Size { const VALUE: usize }
impl<const N: usize> Size for Item<N> { const VALUE: usize = N + 1 }
impl<const M: usize> Size for Item<M> as Alternate { const VALUE: usize = M + 1 }
const fn chosen() -> usize { with (Alternate<5>) { Item<5>::VALUE } }
"#;
    values(source, &[("chosen", 6)]);
    let differing = source.replace("M + 1", "M + 2");
    assert_eq!(diagnostics(&differing), vec!["5-0005"]);
}

#[test]
fn named_outputs_require_an_ordinary_type_normalization_representative() {
    values(
        r#"
struct Item {}
trait Size { const N: usize }
impl Size for Item { const N: usize = 3 }
const fn size<T: Size>() -> u256 {
    let data: [u256; T::N] = [1; T::N]
    data[0]
}
const fn ordinary() -> u256 { size<Item>() }
"#,
        &[("ordinary", 1)],
    );
    let codes = diagnostics(
        r#"
struct Item {}
trait Size { const N: usize }
impl Size for Item as Sized { const N: usize = 3 }
const fn size<T: Size>() -> u256 {
    let data: [u256; T::N] = [1; T::N]
    data[0]
}
const fn chosen() -> u256 { with (Sized) { size<Item>() } }
"#,
    );
    assert!(codes.contains(&"5-0005".to_string()), "{codes:?}");
}

#[test]
fn choice_priority_waits_for_arguments_determined_by_other_bounds() {
    values(
        r#"
struct Item {}
struct Other {}
trait Pick<A> { const fn value() -> u256 }
impl Pick<Item> for Item { const fn value() -> u256 { 100 } }
impl Pick<Other> for Item { const fn value() -> u256 { 200 } }
impl Pick<Item> for Item as Discount { const fn value() -> u256 { 70 } }
trait Determine<A> {}
impl Determine<Other> for Item {}
const fn first<T: Pick<A> + Determine<A>, A>(_ item: T) -> u256 { T::value() }
const fn second<T: Determine<A> + Pick<A>, A>(_ item: T) -> u256 { T::value() }
const fn forward() -> u256 { with (Discount) { first(Item {}) } }
const fn reverse() -> u256 { with (Discount) { second(Item {}) } }
struct Box<T> { value: T }
trait Total<A> { const fn total() -> u256 }
impl<A, T: Determine<A> + Pick<A>> Total<A> for Box<T> { const fn total() -> u256 { T::value() } }
const fn nested<T: Total<A>, A>(_ item: T) -> u256 { T::total() }
const fn recursive() -> u256 { with (Discount) { nested(Box { value: Item {} }) } }
"#,
        &[("forward", 200), ("reverse", 200), ("recursive", 200)],
    );
}

#[test]
fn choice_priority_preserves_unique_ordinary_argument_inference() {
    values(
        r#"
struct Item {}
struct Arg {}
trait Pick<A> { const fn value() -> u256 }
impl Pick<Arg> for Item { const fn value() -> u256 { 100 } }
impl Pick<Arg> for Item as Discount { const fn value() -> u256 { 70 } }
const fn infer<T: Pick<A>, A>(_ item: T) -> u256 { T::value() }
const fn chosen() -> u256 { with (Discount) { infer(Item {}) } }
"#,
        &[("chosen", 70)],
    );
}

#[test]
fn applied_named_conditions_keep_arguments_absent_from_the_header() {
    values(
        r#"
struct Item {}
struct Arg {}
trait Value { const fn value() -> u256 }
impl Value for Arg { const fn value() -> u256 { 7 } }
trait Pick {
    const fn pick() -> u256
    const fn base() -> u256
}
impl<T: Value> Pick for Item as Chosen {
    const fn base() -> u256 { T::value() }
    const fn pick() -> u256 { Self::base() }
}
const fn chosen() -> u256 { with (Chosen<Arg>) { Item::pick() } }
"#,
        &[("chosen", 7)],
    );
}

#[test]
fn local_named_conditions_keep_captured_arguments_absent_from_the_header() {
    values(
        r#"
struct Item {}
struct Arg {}
struct Other {}
trait Value { const fn value() -> u256 }
impl Value for Arg { const fn value() -> u256 { 7 } }
impl Value for Other { const fn value() -> u256 { 9 } }
trait Pick { const fn pick() -> u256 }
const fn generic<T: Value>() -> u256 {
    impl<U: Value> Pick for Item as Local where T: Value {
        const fn pick() -> u256 { T::value() * 10 + U::value() }
    }
    with (Local<Arg>) { Item::pick() }
}
const fn chosen() -> u256 { generic<Arg>() }
const fn other() -> u256 { generic<Other>() }
"#,
        &[("chosen", 77), ("other", 97)],
    );
}

#[test]
fn choice_priority_preserves_joint_ordinary_argument_inference() {
    values(
        r#"struct Item {}
struct One {}
struct Two {}
struct Three {}
trait A<X> { const fn value() -> u256 }
trait B<X> {}
impl A<One> for Item { const fn value() -> u256 { 1 } }
impl A<Two> for Item { const fn value() -> u256 { 2 } }
impl B<Two> for Item {}
impl B<Three> for Item {}
impl A<One> for Item as ChosenA { const fn value() -> u256 { 7 } }
impl B<Three> for Item as ChosenB {}
trait Total<X> { const fn total() -> u256 }
impl<X> Total<X> for Item where Item: A<X> + B<X> { const fn total() -> u256 { <Item as A<X>>::value() } }
const fn infer<T: Total<X>, X>(_ item: T) -> u256 { T::total() }
const fn chosen() -> u256 { with (ChosenA, ChosenB) { infer(Item {}) } }
"#,
        &[("chosen", 2)],
    );
}

#[test]
fn deferred_named_only_bound_uses_the_authored_choice_scope() {
    let source = r#"
struct Item {}
trait Pick { fn value() -> u256 }
impl Pick for Item as Only { fn value() -> u256 { 71 } }
struct Deferred<T> { value: T }
trait Read { fn read<T: Pick>(self, _ value: T) -> u256 }
impl Read for Deferred<u256> { fn read<T: Pick>(self, _ value: T) -> u256 { T::value() } }
impl Read for Deferred<u8> { fn read<T: Pick>(self, _ value: T) -> u256 { T::value() } }
fn run() -> u256 {
    let x = 5
    let result: u256 = with (Only) { Deferred { value: x }.read(Item {}) }
    let settled: u256 = x
    result
}
"#;
    assert!(diagnostics(source).is_empty());
    let outside = source.replace(
        "with (Only) { Deferred { value: x }.read(Item {}) }",
        "Deferred { value: x }.read(Item {})",
    );
    assert_eq!(diagnostics(&outside), ["6-0003"]);
}

#[test]
fn ordinary_argument_inference_does_not_discharge_a_failed_choice() {
    let source = r#"
struct Item {}
struct Arg {}
trait Ready {}
trait Pick<A> { fn value() -> u256 }
impl Pick<Arg> for Item { fn value() -> u256 { 100 } }
impl<T: Ready> Pick<Arg> for T as Broken { fn value() -> u256 { 70 } }
fn infer<T: Pick<A>, A>(_ item: T) -> u256 { T::value() }
fn chosen() -> u256 { with (Broken) { infer(Item {}) } }
"#;
    let codes = diagnostics(source);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
    assert!(!codes.contains(&"8-0031".to_string()), "{codes:?}");
}

#[test]
fn a_choice_does_not_resolve_ambiguous_ordinary_arguments() {
    let source = r#"
struct Item {}
struct One {}
struct Two {}
trait Pick<A> { fn value() -> u256 }
impl Pick<One> for Item { fn value() -> u256 { 1 } }
impl Pick<Two> for Item { fn value() -> u256 { 2 } }
impl Pick<One> for Item as Chosen { fn value() -> u256 { 7 } }
fn infer<T: Pick<A>, A>(_ item: T) -> u256 { T::value() }
fn chosen() -> u256 { with (Chosen) { infer(Item {}) } }
"#;
    let codes = diagnostics(source);
    assert!(codes.contains(&"8-0031".to_string()), "{codes:?}");
}

#[test]
fn with_operand_rejects_a_value_and_named_impl_with_the_same_bare_name() {
    let declarations = [
        "impl Pick for Item as Collision { const fn value() -> u256 { 7 } }\nconst Collision: u256 = 99",
        "const Collision: u256 = 99\nimpl Pick for Item as Collision { const fn value() -> u256 { 7 } }",
    ];
    let mut cases = declarations
        .into_iter()
        .map(|declarations| {
            format!(
                "struct Item {{}}\ntrait Pick {{ const fn value() -> u256 }}\n{declarations}\nconst fn run() -> u256 {{ with (Collision) {{ Item::value() }} }}"
            )
        })
        .collect::<Vec<_>>();
    for binding in ["let Collision: u256 = 99", ""] {
        let parameter = if binding.is_empty() {
            "_ Collision: u256"
        } else {
            ""
        };
        cases.push(format!(
            "struct Item {{}}\ntrait Pick {{ const fn value() -> u256 }}\nimpl Pick for Item as Collision {{ const fn value() -> u256 {{ 7 }} }}\nconst fn run({parameter}) -> u256 {{ {binding}\n with (Collision) {{ Item::value() }} }}"
        ));
    }
    cases.push(
        "pub struct Item {}\ntrait Pick { const fn value() -> u256 }\nmod options {\n use ingot::{Item, Pick}\n pub impl Pick for Item as Collision { const fn value() -> u256 { 7 } }\n pub const Collision: u256 = 99\n}\nuse options::Collision\nconst fn run() -> u256 { with (Collision) { Item::value() } }".into(),
    );
    for source in cases {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("ambiguous_with_operand.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let diagnostics = fe_hir::analysis::initialize_analysis_pass()
            .run_on_module(&db, module)
            .into_iter()
            .map(|diagnostic| diagnostic.to_complete(&db))
            .collect::<Vec<_>>();
        let ambiguity = diagnostics
            .iter()
            .find(|diagnostic| {
                diagnostic.message.contains("Collision") && diagnostic.message.contains("ambiguous")
            })
            .unwrap_or_else(|| {
                panic!("expected a value/implementation ambiguity: {diagnostics:?}")
            });
        assert!(
            ambiguity
                .sub_diagnostics
                .iter()
                .filter(|label| label.message.starts_with("candidate "))
                .count()
                >= 2,
            "both meanings must be identified: {ambiguity:?}"
        );
    }
}

#[test]
fn with_operand_rejects_a_value_and_named_impl_with_the_same_qualified_path() {
    let codes = diagnostics(
        r#"
mod m {
    pub struct Store { pub n: u256 }
    pub trait Price { fn amount(self) -> u256 }
    impl Price for Store { fn amount(self) -> u256 { 100 } }
    pub impl Price for Store as Cheap { fn amount(self) -> u256 { 70 } }
    pub const Cheap: Store = Store { n: 5 }
}
use m::{Store, Price}
fn read() -> u256 uses (s: Store) { s.n }
fn outer() -> u256 uses (s: Store) { with (m::Cheap) { read() } }
"#,
    );
    assert!(codes.contains(&"2-0004".to_string()), "{codes:?}");
}

#[test]
fn two_operands_of_one_with_answering_one_goal_are_refused() {
    let prelude = r#"
struct Item {}
struct Box<T> { pub value: T }
trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
impl Pick for Item as Discount { const fn value() -> u256 { 70 } }
impl Pick for Item as Tiny { const fn value() -> u256 { 3 } }
trait Size { const fn size() -> u256 }
impl<T> Size for Box<T> { const fn size() -> u256 { 100 } }
impl<T> Size for Box<T> as AnyBox { const fn size() -> u256 { 70 } }
impl Size for Box<Item> as ItemBox { const fn size() -> u256 { 50 } }
"#;
    for body in [
        "with (Discount, Tiny) { Item::value() }",
        "with (Tiny, Discount) { Item::value() }",
        "with (Discount, Discount) { Item::value() }",
        "with (Discount, <Item as Pick>) { Item::value() }",
        "with (<Item as Pick>, Discount) { Item::value() }",
        "with (AnyBox, ItemBox) { <Box<Item> as Size>::size() }",
        "with (ItemBox, AnyBox) { <Box<Item> as Size>::size() }",
    ] {
        let source = format!("{prelude}const fn run() -> u256 {{ {body} }}\n");
        assert_eq!(diagnostics(&source), vec!["8-0160"], "{body}");
    }
    for (body, message) in [
        (
            "with (Discount, Tiny) { Item::value() }",
            "`Item: Pick` is chosen twice in one `with`",
        ),
        (
            "with (ItemBox, AnyBox) { <Box<Item> as Size>::size() }",
            "`Box<Item>: Size` is chosen twice in one `with`",
        ),
        (
            "with (AnyBox, ItemBox) { <Box<Item> as Size>::size() }",
            "`Box<Item>: Size` is chosen twice in one `with`",
        ),
    ] {
        let source = format!("{prelude}const fn run() -> u256 {{ {body} }}\n");
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("duplicate_with_choice.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let complete = fe_hir::analysis::initialize_analysis_pass()
            .run_on_module(&db, module)
            .into_iter()
            .map(|diagnostic| diagnostic.to_complete(&db))
            .collect::<Vec<_>>();
        let [duplicate] = complete.as_slice() else {
            panic!("expected one diagnostic for {body}: {complete:?}");
        };
        assert_eq!(duplicate.message, message, "{body}");
        assert_eq!(duplicate.sub_diagnostics.len(), 2, "{duplicate:?}");
    }
}

#[test]
fn operands_answering_distinct_goals_stay_accepted() {
    values(
        r#"
struct Item {}
struct Other {}
struct Box<T> { v: T }
trait Price { const fn price() -> u256 }
impl Price for Item { const fn price() -> u256 { 100 } }
impl Price for Item as W { const fn price() -> u256 { 70 } }
impl Price for Item as V { const fn price() -> u256 { 50 } }
trait Gate { const fn g() -> u256 }
impl Gate for Item { const fn g() -> u256 { 1 } }
impl Gate for Item as G { const fn g() -> u256 { 5 } }
trait Conv<T> { const fn c() -> u256 }
impl Conv<u8> for Item as C8 { const fn c() -> u256 { 8 } }
impl Conv<u16> for Item as C16 { const fn c() -> u256 { 16 } }
trait Sub: Price { const fn s() -> u256 }
impl Sub for Item { const fn s() -> u256 { 2 } }
impl Sub for Item as S { const fn s() -> u256 { 3 } }
trait Size { const fn size() -> u256 }
impl<T> Size for Box<T> { const fn size() -> u256 { 100 } }
impl<T> Size for Box<T> as BW { const fn size() -> u256 { 70 } }
impl Size for Box<Item> as BX { const fn size() -> u256 { 50 } }
const fn two_traits() -> u256 { with (W, G) { Item::price() + Item::g() } }
const fn trait_args() -> u256 { with (C8, C16) { <Item as Conv<u8>>::c() + <Item as Conv<u16>>::c() } }
const fn sub_and_super() -> u256 { with (S, W) { Item::s() + Item::price() } }
const fn nested() -> u256 { with (W) { with (V) { Item::price() } } }
const fn disjoint_applications() -> u256 { with (BW<Other>, BX) { <Box<Item> as Size>::size() + <Box<Other> as Size>::size() } }
"#,
        &[
            ("two_traits", 75),
            ("trait_args", 24),
            ("sub_and_super", 73),
            ("nested", 50),
            ("disjoint_applications", 120),
        ],
    );
}

#[test]
fn an_openings_conditions_do_not_depend_on_operand_order() {
    values(
        r#"
struct Item {}
struct Box<T> { v: T }
trait Gate { const fn g() -> u256 }
impl Gate for Item as G { const fn g() -> u256 { 5 } }
trait Total { const fn total() -> u256 }
impl<T: Gate> Total for Box<T> { const fn total() -> u256 { T::g() + 1 } }
trait Named { const fn n() -> u256 }
impl<T> Named for Box<T> { const fn n() -> u256 { 0 } }
impl<T: Gate> Named for Box<T> as BN { const fn n() -> u256 { T::g() + 1 } }
trait Mark { const fn m() -> u256 }
impl Mark for Item { const fn m() -> u256 { 5 } }
impl Mark for Item as M2 { const fn m() -> u256 { 9 } }
trait Tally { const fn t() -> u256 }
impl<T: Mark> Tally for Box<T> { const fn t() -> u256 { T::m() + 1 } }
const fn gate_first() -> u256 { with (G, <Box<Item> as Total>) { <Box<Item> as Total>::total() } }
const fn opening_first() -> u256 { with (<Box<Item> as Total>, G) { <Box<Item> as Total>::total() } }
const fn named_gate_first() -> u256 { with (G, BN) { <Box<Item> as Named>::n() } }
const fn named_named_first() -> u256 { with (BN, G) { <Box<Item> as Named>::n() } }
const fn ordinary_opening_first() -> u256 { with (<Box<Item> as Tally>, M2) { <Box<Item> as Tally>::t() } }
const fn ordinary_mark_first() -> u256 { with (M2, <Box<Item> as Tally>) { <Box<Item> as Tally>::t() } }
"#,
        &[
            ("gate_first", 6),
            ("opening_first", 6),
            ("named_gate_first", 6),
            ("named_named_first", 6),
            ("ordinary_opening_first", 10),
            ("ordinary_mark_first", 10),
        ],
    );
}

#[test]
fn an_ordinary_opening_requires_a_unique_header_before_proving_conditions() {
    let codes = diagnostics(
        r#"
struct Item {}
struct Box<T> { value: T }
trait Gate {}
impl Gate for u8 {}
trait Pick {}
impl<T: Gate> Pick for Box<T> {}
impl Pick for Box<Item> {}
fn open() { with (<Box<Item> as Pick>) {} }
"#,
    );
    assert_eq!(codes, vec!["6-0003"]);
}

#[test]
fn openings_whose_conditions_lead_to_each_other_are_refused() {
    for source in [
        r#"
struct Item {}
struct Box<T> { v: T }
trait Ga { const fn a() -> u256 }
trait Gb { const fn b() -> u256 }
impl<T> Ga for Box<T> where Box<T>: Gb { const fn a() -> u256 { 1 } }
impl<T> Gb for Box<T> where Box<T>: Ga { const fn b() -> u256 { 2 } }
const fn cyc() -> u256 { with (<Box<Item> as Ga>, <Box<Item> as Gb>) { <Box<Item> as Ga>::a() } }
"#,
        r#"
struct Item {}
struct Box<T> { v: T }
trait Ga { const fn a() -> u256 }
trait Gb { const fn b() -> u256 }
impl<T: Gb> Ga for Box<T> { const fn a() -> u256 { 1 } }
impl<T: Ga> Gb for Box<T> { const fn b() -> u256 { 2 } }
const fn both() -> u256 { with (<Box<Box<Item>> as Ga>, <Box<Item> as Gb>) { 0 } }
"#,
    ] {
        let codes = diagnostics(source);
        assert!(!codes.is_empty(), "the cycle must be refused");
        assert!(codes.iter().all(|code| code == "6-0003"), "{codes:?}");
    }
}

#[test]
fn duplicate_implementation_names_are_refused_at_declaration() {
    let codes = diagnostics(
        r#"
struct Item {}
trait Price { fn amount(self) -> u256 }
impl Price for Item { fn amount(self) -> u256 { 100 } }
impl Price for Item as W { fn amount(self) -> u256 { 70 } }
impl Price for Item as W { fn amount(self) -> u256 { 60 } }
"#,
    );
    assert!(codes.contains(&"3-0100".to_string()), "{codes:?}");
    // One name in two modules, and an implementation name equal to a value
    // name, are separate meanings.
    let codes = diagnostics(
        r#"
struct Item {}
trait Price { fn amount(self) -> u256 }
impl Price for Item as W { fn amount(self) -> u256 { 70 } }
mod other {
    use ingot::{Item, Price}
    impl Price for Item as W { fn amount(self) -> u256 { 60 } }
}
const V: u256 = 1
impl Price for Item as V { fn amount(self) -> u256 { 50 } }
"#,
    );
    assert!(codes.is_empty(), "{codes:?}");
}

#[test]
fn local_duplicate_implementation_names_are_refused_at_declaration() {
    let codes = diagnostics(
        r#"
struct Item {}
trait Price { fn price(self) -> u256 }
impl Price for Item { fn price(self) -> u256 { 100 } }
pub fn f() -> u256 {
    impl Price for Item as W { fn price(self) -> u256 { 70 } }
    impl Price for Item as W { fn price(self) -> u256 { 60 } }
    with (W) { Item {}.price() }
}
"#,
    );
    assert!(codes.contains(&"3-0100".to_string()), "{codes:?}");
    // A nested block and the module are other scopes: the nearer name is meant.
    for source in [
        "struct Item {}\ntrait Price { fn price(self) -> u256 }\nimpl Price for Item { fn price(self) -> u256 { 100 } }\npub fn f() -> u256 {\n    impl Price for Item as W { fn price(self) -> u256 { 70 } }\n    let a = with (W) { Item {}.price() }\n    let b = {\n        impl Price for Item as W { fn price(self) -> u256 { 60 } }\n        with (W) { Item {}.price() }\n    }\n    a * 1000 + b\n}",
        "struct Item {}\ntrait Price { fn price(self) -> u256 }\nimpl Price for Item { fn price(self) -> u256 { 100 } }\nimpl Price for Item as W { fn price(self) -> u256 { 70 } }\npub fn f() -> u256 {\n    impl Price for Item as W { fn price(self) -> u256 { 60 } }\n    with (W) { Item {}.price() }\n}",
    ] {
        let codes = diagnostics(source);
        assert!(codes.is_empty(), "{source}: {codes:?}");
    }
}

#[test]
fn with_operands_that_name_nothing_or_two_meanings_are_reported_once() {
    let unknown =
        format!("{PICKS}\nconst fn run() -> u256 {{ with (Nope) {{ Item::value() }} }}\n");
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("unknown_with_operand.fe".into(), &unknown);
    let (module, _) = db.top_mod(file);
    let complete = fe_hir::analysis::initialize_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(&db))
        .collect::<Vec<_>>();
    assert_eq!(complete.len(), 1, "{complete:?}");
    assert_eq!(complete[0].error_code.to_string(), "8-0012");
    assert_eq!(
        complete[0].message,
        "no implementation or value named `Nope` is in scope"
    );
    // The body of a `with` whose operand has two meanings is not checked, so
    // it reports no missing effect that the value would have provided.
    let codes = diagnostics(
        r#"
mod m {
    pub struct Store { pub n: u256 }
    pub trait Price { fn amount(self) -> u256 }
    impl Price for Store { fn amount(self) -> u256 { 100 } }
    pub impl Price for Store as Cheap { fn amount(self) -> u256 { 70 } }
    pub const Cheap: Store = Store { n: 5 }
}
use m::{Store, Price}
fn read() -> u256 uses (s: Store) { s.n }
fn qualified() -> u256 { with (m::Cheap) { read() } }
"#,
    );
    assert_eq!(codes, vec!["2-0004".to_string()]);
}

#[test]
fn an_openings_condition_answered_by_an_operand_whose_conditions_fail_is_refused() {
    let codes = diagnostics(
        r#"
struct Item {}
struct Box<T> { v: T }
trait Bad {}
trait Gate { const fn g() -> u256 }
impl Gate for Item { const fn g() -> u256 { 1 } }
impl<T: Bad> Gate for T as G { const fn g() -> u256 { 5 } }
trait Total { const fn total() -> u256 }
impl<T: Gate> Total for Box<T> { const fn total() -> u256 { T::g() + 1 } }
const fn opening_first() -> u256 { with (<Box<Item> as Total>, G) { 0 } }
const fn gate_first() -> u256 { with (G, <Box<Item> as Total>) { 0 } }
"#,
    );
    // No fallback to the ordinary `Gate`, in either order, although the block
    // uses neither goal.
    assert_eq!(codes, vec!["6-0003".to_string(), "6-0003".to_string()]);
    values(
        r#"
struct Item {}
struct Box<T> { v: T }
trait Gate { const fn g() -> u256 }
impl Gate for Item { const fn g() -> u256 { 1 } }
trait Total { const fn total() -> u256 }
impl<T: Gate> Total for Box<T> { const fn total() -> u256 { T::g() + 1 } }
const fn alone() -> u256 { with (<Box<Item> as Total>) { <Box<Item> as Total>::total() } }
"#,
        &[("alone", 2)],
    );
}

#[test]
fn a_condition_written_through_a_projection_projects_its_associated_type_bound() {
    values(
        r#"
trait Cur { type Cursor: Copy
    const fn start() -> u256 }
struct Item {}
impl Cur for Item { type Cursor = u256
    const fn start() -> u256 { 7 } }
trait Wrap { type Inner }
struct W {}
impl Wrap for W { type Inner = Item }
struct Box<T> { v: T }
trait Total { const fn total() -> u256 }
impl<T> Total for Box<T> where <T as Wrap>::Inner: Cur {
    const fn total() -> u256 { <T as Wrap>::Inner::start() }
}
impl<T> Total for Box<T> as BT where <T as Wrap>::Inner: Cur {
    const fn total() -> u256 { <T as Wrap>::Inner::start() + 100 }
}
const fn ordinary() -> u256 { <Box<W> as Total>::total() }
const fn chosen() -> u256 { with (BT) { <Box<W> as Total>::total() } }
"#,
        &[("ordinary", 7), ("chosen", 107)],
    );
}

#[test]
fn named_implementations_of_traits_decided_by_presence_are_refused() {
    for source in [
        "use core::marker::Copy\nstruct Item { n: u256 }\nimpl Copy for Item as Copying {}",
        "use core::ops::Drop\nstruct Token {}\nimpl Drop for Token as Heavy { fn drop(own self) {} }",
        "use core::marker::{Copy, Snapshot}\nstruct H { v: u256 }\nimpl Copy for H {}\nimpl Snapshot for H as Snap {}",
        "use core::marker::{Pinned, Storage}\nstruct P { v: u256 }\nimpl Pinned for P { type Space = Storage }\nimpl Pinned for P as Pin { type Space = Storage }",
        "use core::abi::{AbiFrame, AbiSize, Encode}\nuse core::error::ErrorVariant\nuse std::abi::Sol\nstruct Oops {}\nimpl AbiSize for Oops {\n    const HEAD_SIZE: u256 = 0\n    const IS_DYNAMIC: bool = false\n}\nimpl Encode<Sol> for Oops { fn encode(self, _ out: AbiFrame) {} }\nimpl ErrorVariant<Sol> for Oops { const SELECTOR: u32 = 1 }\nimpl ErrorVariant<Sol> for Oops as Again { const SELECTOR: u32 = 1 }",
    ] {
        let codes = diagnostics(source);
        assert!(codes.contains(&"5-0005".to_string()), "{source}: {codes:?}");
    }
    for source in [
        "use core::marker::Copy\nstruct Item { n: u256 }\nimpl Copy for Item {}",
        "use core::ops::Drop\nstruct Token {}\nimpl Drop for Token { fn drop(own self) {} }",
        "use core::marker::{Copy, Snapshot}\nstruct H { v: u256 }\nimpl Copy for H {}\nimpl Snapshot for H {}",
    ] {
        let codes = diagnostics(source);
        assert!(codes.is_empty(), "{source}: {codes:?}");
    }
    // Refused once: a named `Pinned` without an ordinary one is not also
    // reported for its outputs.
    assert_eq!(
        diagnostics(
            "use core::marker::{Pinned, Storage}\nstruct P { v: u256 }\nimpl Pinned for P as Pin { type Space = Storage }"
        ),
        vec!["5-0005".to_string()]
    );
}

#[test]
fn one_trait_instance_with_a_length_written_in_two_forms_is_one_method_candidate() {
    // The ordinary and the chosen implementation name one instance: a method
    // through an associated type, through a trait argument, and an operator.
    for source in [
        r#"
trait Holds {
    type Base
    fn size(self) -> u256
}
struct H {}
impl Holds for H {
    type Base = [u8; 3]
    fn size(self) -> u256 { 3 }
}
impl Holds for H as Other {
    type Base = [u8; 3]
    fn size(self) -> u256 { 4 }
}
fn chosen() -> u256 { with (Other) { H {}.size() } }
"#,
        r#"
trait Measure<T> { fn measure(self) -> u256 }
struct Cell {}
impl Measure<[u8; 3]> for Cell { fn measure(self) -> u256 { 1 } }
impl Measure<[u8; 3]> for Cell as Doubled { fn measure(self) -> u256 { 2 } }
fn chosen() -> u256 { with (Doubled) { Cell {}.measure() } }
"#,
        r#"
use core::ops::Eq
struct Row<const N: usize> { v: u256 }
impl Copy for Row<3> {}
impl Eq for Row<3> { fn eq(self, _ o: Row<3>) -> bool { self.v == o.v } }
impl Eq for Row<3> as AnyRow { fn eq(self, _ o: Row<3>) -> bool { true } }
fn chosen(_ a: Row<3>, _ b: Row<3>) -> bool { with (AnyRow) { a == b } }
"#,
    ] {
        let codes = diagnostics(source);
        assert!(codes.is_empty(), "{source}: {codes:?}");
    }
    // `Measure<[u8; 3]>` and `Measure<[u8; 4]>` are two instances: a call that
    // nothing disambiguates stays ambiguous, with and without a choice.
    let codes = diagnostics(
        r#"
trait Measure<T> { fn measure(self) -> u256 }
struct Cell {}
impl Measure<[u8; 3]> for Cell { fn measure(self) -> u256 { 3 } }
impl Measure<[u8; 4]> for Cell { fn measure(self) -> u256 { 4 } }
impl Measure<[u8; 3]> for Cell as Three { fn measure(self) -> u256 { 30 } }
fn outside() -> u256 { Cell {}.measure() }
fn chosen() -> u256 { with (Three) { Cell {}.measure() } }
"#,
    );
    assert_eq!(codes, vec!["8-0026".to_string(), "8-0026".to_string()]);
}

#[test]
fn with_operand_keeps_unambiguous_and_explicit_value_meanings() {
    values(
        r#"
struct Item {}
trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
impl Pick for Item as Named { const fn value() -> u256 { 7 } }
const OnlyValue: u256 = 99
const Named: u256 = 99
struct TypeOnly {}
impl Pick for Item as TypeOnly { const fn value() -> u256 { 3 } }
mod options {
 use ingot::{Item, Pick}
 pub impl Pick for Item as Qualified { const fn value() -> u256 { 11 } } }
const fn value_only() -> u256 { with (OnlyValue) { Item::value() } }
const fn explicit_value() -> u256 { with ({ Named }) { Item::value() } }
const fn type_only_name() -> u256 { with (TypeOnly) { Item::value() } }
const fn qualified_name() -> u256 { with (options::Qualified) { Item::value() } }
"#,
        &[
            ("value_only", 100),
            ("explicit_value", 100),
            ("type_only_name", 3),
            ("qualified_name", 11),
        ],
    );
}

#[test]
fn conditional_named_implementation_in_where_clause_const_terminates() {
    // Lowering these `where` clause constants reaches a fixed point: the
    // retained named implementation's condition leaf names its assumption by
    // owner and position instead of carrying the caller's assumption list.
    // `S1` is refused with 8-0138 because its written type's own constant
    // selects a named implementation (see docs/language/named-implementations.md).
    let codes = diagnostics(
        r#"
struct Wrap<T> {
    value: T,
}

trait Len {
    const LEN: usize
}

trait Pick {
    const fn value() -> usize
}

impl<T: Len> Pick for Wrap<T> {
    const fn value() -> usize { T::LEN }
}

impl<T: Len> Pick for Wrap<T> as ChosenWrap {
    const fn value() -> usize { T::LEN }
}

trait Indexed<const I: usize> {
    const V: usize
}

const fn read_pick<U: Pick>() -> usize {
    U::value()
}

const fn via_wrap<T: Len>() -> usize {
    with (ChosenWrap) {
        read_pick<Wrap<T>>()
    }
}

struct S1<T, const L: usize = 5>
where
    T: Len + Indexed<{ with (ChosenWrap) { read_pick<Wrap<T>>() } }>,
{
    t: T,
}

struct S2<T, const L: usize = 5>
where
    T: Len + Indexed<{ via_wrap<T>() }>,
{
    t: T,
}
"#,
    );
    assert_eq!(codes, vec!["8-0138".to_string()]);
}

#[test]
fn supertrait_projection_and_effect_rows_in_where_clause_consts_terminate() {
    // A supertrait projection (`T: Sub + Len`) combined with a `where` clause
    // constant and a caller-side `with (ShortLen)` instantiation.
    let projection_codes = diagnostics(
        r#"
struct Item {}

trait Len {
    const LEN: usize
    const fn len() -> usize
}

impl Len for Item {
    const LEN: usize = 5
    const fn len() -> usize { 100 }
}

impl Len for Item as ShortLen {
    const LEN: usize = 5
    const fn len() -> usize { 5 }
}

trait Sub: Len {}
impl Sub for Item {}

trait Indexed<const I: usize> {
    const V: usize
}

impl Indexed<5> for Item {
    const V: usize = 1
}

const fn read_len<T: Len>() -> usize {
    T::LEN
}

struct S1<T, const L: usize = 5>
where
    T: Sub + Len + Indexed<{ read_len<T>() }>,
{
    t: T,
}

fn use_s1(_ x: S1<Item>) {}

fn caller() {
    with (ShortLen) {
        let s = S1 { t: Item {} }
        use_s1(s)
    }
}
"#,
    );
    assert!(projection_codes.is_empty(), "{projection_codes:?}");

    // A trait-keyed effect provider paired with a named implementation inside
    // a helper called from a `where` clause constant under a defaulted const
    // parameter.
    let provider_codes = diagnostics(
        r#"
struct Item {}

trait Price {
    const fn amount(self) -> usize
}

impl Price for Item {
    const fn amount(self) -> usize { 100 }
}

impl Price for Item as Wholesale {
    const fn amount(self) -> usize { 5 }
}

trait Len {
    const LEN: usize
}

trait Indexed<const I: usize> {
    const V: usize
}

const fn charge() -> usize uses (p: Price) {
    p.amount()
}

const fn read_len<T: Len>() -> usize {
    with (Wholesale, Price = Item {}) {
        let _ = charge()
        T::LEN
    }
}

struct S1<T, const L: usize = 5>
where
    T: Len + Indexed<{ read_len<T>() }>,
{
    t: T,
}
"#,
    );
    assert!(provider_codes.is_empty(), "{provider_codes:?}");
}

#[test]
fn wrapped_length_where_clause_consts_terminate_with_expected_outcomes() {
    // A type constant cannot open a named implementation. A const helper can
    // open its own implementation, and generic reads wait for their instance.
    let inline_choice_codes = diagnostics(
        r#"
trait Len { const fn len() -> usize }
trait Indexed<const I: usize> {}
struct Wrap<T> { t: T }
impl<T: Len> Len for Wrap<T> as WrapLen { const fn len() -> usize { T::len() + 1 } }
const fn read_len<T: Len>() -> usize { T::len() }
struct S<T>
where
    T: Len + Indexed<{ with (WrapLen) { read_len<Wrap<T>>() } }>,
{ t: T }
"#,
    );
    assert_eq!(inline_choice_codes, vec!["8-0138"]);

    values(
        r#"
struct Item {}
trait Len { const fn len() -> usize }
impl Len for Item { const fn len() -> usize { 5 } }
trait Indexed<const I: usize> {}
struct Wrap<T> { t: T }
impl<T: Len> Len for Wrap<T> as WrapLen { const fn len() -> usize { T::len() + 1 } }
const fn read_len<T: Len>() -> usize { T::len() }
const fn via_wrap<T: Len>() -> usize { with (WrapLen) { read_len<Wrap<T>>() } }
struct S2<T> where T: Len + Indexed<{ via_wrap<T>() }> { t: T }
type Slots = [u256; { via_wrap<Item>() }]
const fn read_slot() -> u256 {
    let slots: Slots = [9; 6]
    slots[5]
}
"#,
        &[("read_slot", 9)],
    );

    let provider_len_codes = diagnostics(
        r#"
struct Item {}
trait Price { const fn amount(self) -> usize }
impl Price for Item { const fn amount(self) -> usize { 1 } }
const Q: Item = Item {}
trait Len { const fn len() -> usize }
trait Indexed<const I: usize> {}
const fn read_len<T: Len>() -> usize uses (p: Price) { p.amount() + T::len() }
struct S<T>
where
    T: Len + Indexed<{ with (Price = Q) { read_len<T>() } }>,
{ t: T }
"#,
    );
    assert!(provider_len_codes.is_empty(), "{provider_len_codes:?}");

    let generic_provider_codes = diagnostics(
        r#"
struct Wrap<T> {}
trait Len { const LEN: usize }
trait Price { const fn amount(self) -> usize }
impl<T: Len> Price for Wrap<T> { const fn amount(self) -> usize { 1 } }
trait Indexed<const I: usize> {}
const fn read_len<T: Len>() -> usize uses (p: Price) { T::LEN }
struct S<T>
where
    T: Len + Indexed<{ with (Price = Wrap<T> {}) { read_len<T>() } }>,
{ t: T }
"#,
    );
    assert!(
        generic_provider_codes.is_empty(),
        "{generic_provider_codes:?}"
    );

    let default_param_codes = diagnostics(
        r#"
trait Len {
    const LEN: usize
}
trait Indexed<const I: usize> {
    const V: usize
}
const fn read_len<T: Len>() -> usize {
    T::LEN
}
struct S1<T, const L: usize = 5>
where
    T: Len + Indexed<{ read_len<T>() }>,
{
    t: T,
}
"#,
    );
    assert!(default_param_codes.is_empty(), "{default_param_codes:?}");
}

const PROJECTED_PICK: &str = r#"
trait Source { type Out }
struct Z {}
impl Source for Z { type Out = u8 }
struct Wrap<T> { value: T }
trait Pick<A> { const fn n() -> usize }
impl Pick<u8> for Wrap<Z> { const fn n() -> usize { 1 } }
impl Pick<u16> for Wrap<Z> { const fn n() -> usize { 1 } }
impl<T: Source> Pick<T::Out> for Wrap<T> as Projected { const fn n() -> usize { 2 } }
impl Pick<u8> for Wrap<Z> as Direct { const fn n() -> usize { 3 } }
impl Pick<u16> for Wrap<Z> as Disjoint { const fn n() -> usize { 4 } }
"#;

#[test]
fn projected_openings_select_at_the_whole_header() {
    let source = format!(
        r#"{PROJECTED_PICK}
const fn explicit() -> usize {{ with (Projected<Z>) {{ <Wrap<Z> as Pick<u8>>::n() }} }}
const fn bare() -> usize {{ with (Projected) {{ <Wrap<Z> as Pick<u8>>::n() }} }}
const fn nearest() -> usize {{ with (Direct) {{ with (Projected) {{ <Wrap<Z> as Pick<u8>>::n() }} }} }}
const fn disjoint() -> usize {{ with (Projected<Z>) {{ <Wrap<Z> as Pick<u16>>::n() }} }}
const fn together() -> usize {{ with (Projected, Disjoint) {{ <Wrap<Z> as Pick<u8>>::n() }} }}
const fn generic<U: Source<Out = u8>>() -> usize {{ with (Projected<U>) {{ <Wrap<U> as Pick<u8>>::n() }} }}
const fn through_caller_bound() -> usize {{ generic<Z>() }}
trait Later<A, B> {{ const fn n() -> usize }}
impl Later<u8, Z> for Wrap<Z> {{ const fn n() -> usize {{ 1 }} }}
impl<T: Source> Later<T::Out, T> for Wrap<Z> as LaterProjected {{ const fn n() -> usize {{ 2 }} }}
const fn anchor_after_projection() -> usize {{ with (LaterProjected) {{ <Wrap<Z> as Later<u8, Z>>::n() }} }}
"#
    );
    values(
        &source,
        &[
            ("explicit", 2),
            ("bare", 2),
            ("nearest", 2),
            ("disjoint", 1),
            ("together", 2),
            ("through_caller_bound", 2),
            ("anchor_after_projection", 2),
        ],
    );
}

#[test]
fn overlapping_projected_openings_in_one_with_are_rejected() {
    for openings in [
        "Projected<Z>, Direct",
        "Direct, Projected<Z>",
        "Projected, Direct",
        "Direct, Projected",
    ] {
        let source = format!("{PROJECTED_PICK}fn reject() {{ with ({openings}) {{}} }}");
        assert_eq!(diagnostics(&source), vec!["8-0160"], "{openings}");
    }
}

#[test]
fn projection_prediction_duplicate_bare_headers() {
    let declarations = r#"
trait Source { type Out }
struct Z {}
struct W {}
impl Source for Z { type Out = u8 }
impl Source for W { type Out = u16 }
struct Wrap<T> { value: T }
trait Pick<A> { const fn n() -> usize }
impl<T: Source<Out = u8>> Pick<T::Out> for Wrap<T> as Projected {
    const fn n() -> usize { 2 }
}
impl<T> Pick<u8> for Wrap<T> as Other {
    const fn n() -> usize { 1 }
}
"#;
    let checked = |openings| {
        diagnostics(&format!(
            "{declarations}fn run() {{ with ({openings}) {{}} }}"
        ))
    };
    assert_eq!(
        [
            checked("Projected, Other"),
            checked("Other, Projected"),
            checked("Projected<W>, Other<W>"),
        ],
        [vec!["8-0160"], vec!["8-0160"], vec![]],
    );
}

#[test]
fn unresolved_projected_overlap_is_rejected_before_obligations() {
    let declarations = r#"
trait Source { type Out }
struct Item {}
impl Source for Item { type Out = u16 }
struct Wrap<T> {}
trait Pick<A> { const fn n() -> usize }
impl<T: Source<Out = u8>> Pick<T::Out> for Wrap<T> as Projected {
    const fn n() -> usize { 2 }
}
impl<T> Pick<u16> for Wrap<T> as Other { const fn n() -> usize { 3 } }
"#;
    for (openings, expected) in [
        (
            "Projected, Other",
            "these openings may choose the same trait goal",
        ),
        (
            "Other, Projected",
            "these openings may choose the same trait goal",
        ),
        (
            "Projected<Item>, Other<Item>",
            "`Wrap<Item>: Pick<u16>` is chosen twice in one `with`",
        ),
        (
            "Other<Item>, Projected<Item>",
            "`Wrap<Item>: Pick<u16>` is chosen twice in one `with`",
        ),
    ] {
        let source = format!("{declarations}fn run() {{ with ({openings}) {{}} }}");
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("projected_overlap.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let errors = fe_hir::analysis::initialize_analysis_pass().run_on_module(&db, module);
        assert_eq!(errors.len(), 1, "{openings}: {:?}", diagnostics(&source));
        assert_eq!(errors[0].to_complete(&db).message, expected, "{openings}");
    }
}

#[test]
fn repeated_projections_and_rigid_arguments_can_prove_disjointness() {
    let declarations = r#"
trait Source { type Out }
struct Wrap<T> {}
trait Pick<A, B, C> {}
impl<T: Source> Pick<T::Out, T::Out, bool> for Wrap<T> as Projected {}
impl<T> Pick<u8, u16, bool> for Wrap<T> as DifferentOutputs {}
impl<T> Pick<u8, u8, u256> for Wrap<T> as DifferentMarker {}
impl<T> Pick<u8, u8, bool> for Wrap<T> as SameOutputs {}
"#;
    for (other, expected) in [
        ("DifferentOutputs", vec![]),
        ("DifferentMarker", vec![]),
        ("SameOutputs", vec!["8-0160"]),
    ] {
        for openings in [format!("Projected, {other}"), format!("{other}, Projected")] {
            let source = format!("{declarations}fn run() {{ with ({openings}) {{}} }}");
            assert_eq!(diagnostics(&source), expected, "{openings}");
        }
    }
}

#[test]
fn projection_prediction_later_arguments_determine_earlier_projections() {
    let declarations = r#"
trait Source { type Out }
trait Reveal<A> { type Val }
struct Token {}
struct Z<X> {}
struct Leaf {}
struct Other {}
impl Source for Leaf { type Out = u8 }
impl Source for Other { type Out = u16 }
impl<X: Source> Source for Z<X> { type Out = X::Out }
impl<X> Reveal<X> for Token { type Val = X }
impl<X> Reveal<X> for Other { type Val = bool }
struct Wrap<K> {}
trait Pick<A, B> { const fn n() -> usize }
impl<A, B> Pick<A, B> for Wrap<Token> { const fn n() -> usize { 1 } }
"#;
    let mut failures = Vec::new();
    for (header, goal) in [
        ("T::Out, <K as Reveal<T>>::Val", "Y::Out, Z<Y>"),
        ("<K as Reveal<T>>::Val, T::Out", "Z<Y>, Y::Out"),
    ] {
        for opening in ["Named", "Named<Token, Z<Y>>"] {
            let source = format!(
                r#"{declarations}
impl<K: Reveal<T>, T: Source> Pick<{header}> for Wrap<K> as Named {{
    const fn n() -> usize {{ 2 }}
}}
const fn run<Y: Source>() -> usize {{
    with ({opening}) {{ <Wrap<Token> as Pick<{goal}>>::n() }}
}}
fn check() -> [u8; 2] {{ [0; run<Leaf>()] }}
"#
            );
            let codes = diagnostics(&source);
            if !codes.is_empty() {
                failures.push((header, opening, codes));
            }
        }
    }
    assert!(failures.is_empty(), "{failures:?}");
}

#[test]
fn projection_prediction_never_is_distinct_in_a_header() {
    let mut failures = Vec::new();
    for (output, expected) in [("!", 1), ("u8", 2), ("u16", 1)] {
        let declarations = PROJECTED_PICK.replace("type Out = u8", &format!("type Out = {output}"));
        let source = format!(
            r#"{declarations}
const fn run() -> usize {{ with (Projected<Z>) {{ <Wrap<Z> as Pick<u8>>::n() }} }}
fn check() -> [u8; {expected}] {{ [0; run()] }}
"#
        );
        let codes = diagnostics(&source);
        if !codes.is_empty() {
            failures.push((output, expected, codes));
        }
    }
    assert!(failures.is_empty(), "{failures:?}");
}

#[test]
fn ordinary_trait_matching_keeps_never_distinct_from_other_types() {
    let argument = r#"
struct Item {}
trait Pick<A> { const fn n() -> usize }
impl Pick<!> for Item { const fn n() -> usize { 2 } }
fn check() -> usize { <Item as Pick<u8>>::n() }
"#;
    let binding = r#"
struct Item {}
trait Pick { type Out }
impl Pick for Item { type Out = ! }
fn accepts<T: Pick<Out = u8>>(_ value: T) {}
fn check() { accepts(Item {}) }
"#;
    for (source, exact) in [
        (argument, argument.replace("as Pick<u8>", "as Pick<!>")),
        (binding, binding.replace("Out = u8", "Out = !")),
    ] {
        let codes = diagnostics(source);
        assert!(codes.iter().any(|code| code == "6-0003"), "{codes:?}");
        assert!(diagnostics(&exact).is_empty(), "{exact}");
    }
}

#[test]
fn ordinary_projected_header_is_matched_after_binding_the_receiver() {
    values(
        r#"
trait Source { type Out }
struct Item {}
impl Source for Item { type Out = u8 }
struct Wrap<T> { value: T }
trait Pick<A> {
    type Result
    const fn n() -> usize
}
impl<T: Source> Pick<T::Out> for Wrap<T> {
    type Result = u16
    const fn n() -> usize { 2 }
}
const fn result() -> usize { <Wrap<Item> as Pick<u8>>::n() }
fn output(value: <Wrap<Item> as Pick<u8>>::Result) -> u16 { value }
"#,
        &[("result", 2)],
    );
}

#[test]
fn a_nonmatching_projected_header_does_not_use_its_own_prerequisite() {
    let declarations = r#"
trait Source { type Out }
struct Item {}
impl Source for Item { type Out = u16 }
struct Wrap<T> { value: T }
trait Pick<A> { const fn n() -> usize }
impl<T: Source<Out = u8>> Pick<T::Out> for Wrap<T> as Projected {
    const fn n() -> usize { 2 }
}
impl Pick<u8> for Wrap<Item> { const fn n() -> usize { 1 } }
impl Pick<u16> for Wrap<Item> { const fn n() -> usize { 1 } }
"#;
    for opening in ["Projected", "Projected<Item>"] {
        let source = format!(
            "{declarations}const fn run() -> usize {{ with ({opening}) {{ <Wrap<Item> as Pick<u8>>::n() }} }}"
        );
        values(&source, &[("run", 1)]);
        let reached = source.replace("as Pick<u8>", "as Pick<u16>");
        let codes = diagnostics(&reached);
        assert!(codes.iter().any(|code| code == "6-0003"), "{codes:?}");

        let matched = source.replace("type Out = u16", "type Out = u8");
        values(&matched, &[("run", 2)]);
    }
}

#[test]
fn generic_associated_outputs_wait_for_later_header_arguments() {
    for arguments in ["T::Out, T", "T, T::Out"] {
        let target = arguments.replace('T', "U");
        for (declaration, receiver, trait_ref) in [
            (
                "trait Pick { type Result }",
                format!("Pair<{arguments}>"),
                "Pick".to_string(),
            ),
            (
                "trait Pick<A, B> { type Result }",
                "Receiver".to_string(),
                format!("Pick<{arguments}>"),
            ),
        ] {
            let target_receiver = receiver.replace('T', "U");
            let target_trait = trait_ref.replace(arguments, &target);
            let source = format!(
                r#"
trait Source {{ type Out }}
struct Pair<A, B> {{}}
struct Receiver {{}}
{declaration}
impl<T: Source> {trait_ref} for {receiver} {{ type Result = u16 }}
fn same<U: Source>(value: <{target_receiver} as {target_trait}>::Result) -> u16 {{ value }}
"#
            );
            let codes = diagnostics(&source);
            assert!(codes.is_empty(), "{source}\n{codes:?}");
        }
    }
}

#[test]
fn projection_evidence_keeps_never_distinct_from_other_trait_arguments() {
    let declarations = r#"
trait Source<A> { type Out }
struct Wrap<T> {}
trait Pick<A> { const fn n() -> usize }
impl<T> Pick<u8> for Wrap<T> { const fn n() -> usize { 1 } }
impl<T> Pick<u16> for Wrap<T> { const fn n() -> usize { 1 } }
impl<T: Source<u16>> Pick<<T as Source<u16>>::Out> for Wrap<T> as Projected {
    const fn n() -> usize { 2 }
}
"#;
    for bounds in [
        "Source<!, Out = u8> + Source<u16, Out = u16>",
        "Source<u16, Out = u16> + Source<!, Out = u8>",
    ] {
        let source = format!(
            r#"{declarations}
const fn probe<T: {bounds}>() -> usize {{
    with (Projected<T>) {{
        <Wrap<T> as Pick<u16>>::n() * 10 + <Wrap<T> as Pick<u8>>::n()
    }}
}}
fn check<T: {bounds}>() -> [u8; 21] {{ [0; probe<T>()] }}
"#
        );
        assert_eq!(diagnostics(&source), Vec::<String>::new(), "{bounds}");
    }
    for bounds in [
        "Source<!, Out = u8> + Source<u16>",
        "Source<u16> + Source<!, Out = u8>",
    ] {
        let unspecified = format!(
            r#"
trait Source<A> {{ type Out }}
fn wrong<T: {bounds}>(x: <T as Source<u16>>::Out) -> u8 {{ x }}
"#
        );
        assert_eq!(diagnostics(&unspecified), vec!["8-0000"]);
    }
}

#[test]
fn ordinary_projection_lookup_and_coherence_use_exact_never_arguments() {
    let source = r#"
trait Source<A> { type Out }
struct Item {}
impl Source<!> for Item { type Out = u8 }
impl Source<u16> for Item { type Out = u16 }

fn for_never(value: <Item as Source<!>>::Out) -> u8 { value }
fn for_u16(value: <Item as Source<u16>>::Out) -> u16 { value }
"#;
    assert_eq!(diagnostics(source), Vec::<String>::new());
}

#[test]
fn ordinary_coherence_can_use_equal_projection_receivers_as_an_overlap_witness() {
    let declarations = r#"
trait Source { type Out }
trait Pick<A, B, C> {}
struct Receiver {}
struct Leaf {}
impl Source for Leaf { type Out = u8 }
"#;
    let left = "impl<T: Source, U> Pick<T::Out, T, U> for Receiver {}";
    let right = "impl<X, Y: Source> Pick<Y::Out, X, Y> for Receiver {}";
    for (first, second) in [(left, right), (right, left)] {
        assert_eq!(
            diagnostics(&format!("{declarations}{first}\n{second}")),
            vec!["5-0001"],
        );
    }
}

#[test]
fn local_projected_openings_keep_the_enclosing_type_equality() {
    let declarations = r#"
trait Source { type Out }
struct Item {}
impl Source for Item { type Out = u8 }
struct Wrap<T> { value: T }
trait Pick<A> { const fn n() -> usize }
"#;
    let source = format!(
        r#"{declarations}
const fn run<T: Source<Out = u8>>() -> usize {{
    impl Pick<<T as Source>::Out> for Wrap<T> as Local where T: Source<Out = u8> {{
        const fn n() -> usize {{ 2 }}
    }}
    with (Local) {{ <Wrap<T> as Pick<u8>>::n() }}
}}
const fn result() -> usize {{ run<Item>() }}
"#
    );
    values(&source, &[("result", 2)]);
    let overlap = source.replace(
        "with (Local) { <Wrap<T> as Pick<u8>>::n() }",
        "impl Pick<u8> for Wrap<T> as Other where T: Source<Out = u8> { const fn n() -> usize { 3 } }\n    with (Local, Other) { 0 }",
    );
    let codes = diagnostics(&overlap);
    assert!(codes.iter().any(|code| code == "8-0160"), "{codes:?}");
}

#[test]
fn named_output_comparison_normalizes_both_header_argument_orders() {
    let declarations = r#"
trait Source { type Out }
trait Reveal<A> { type Val }
struct Token {}
impl<T> Reveal<T> for Token { type Val = T }
struct Wrap<K> {}
trait Pick<A, B> { type Result }
"#;
    for (ordinary, named) in [
        ("T::Out, T", "T::Out, <Token as Reveal<T>>::Val"),
        ("T, T::Out", "<Token as Reveal<T>>::Val, T::Out"),
    ] {
        let source = format!(
            r#"{declarations}
impl<T: Source> Pick<{ordinary}> for Wrap<Token> {{ type Result = u8 }}
impl<T: Source> Pick<{named}> for Wrap<Token> as Named {{ type Result = bool }}
"#
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("projected_outputs.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        let errors = fe_hir::analysis::initialize_analysis_pass().run_on_module(&db, module);
        assert_eq!(errors.len(), 1, "{source}");
        assert_eq!(
            errors[0].to_complete(&db).message,
            "named implementation changes an ordinary associated type"
        );
        assert!(diagnostics(&source.replace("type Result = bool", "type Result = u8")).is_empty());
    }
}

#[test]
fn named_outputs_agree_at_the_overlap_under_both_declarations_bounds() {
    for (direct, projected) in [("u8, T", "T::Out, T"), ("T, u8", "T, T::Out")] {
        for (output, agreed, changed, expected) in [
            (
                "type Result",
                "type Result = u8",
                "type Result = bool",
                "named implementation changes an ordinary associated type",
            ),
            (
                "const RESULT: u8",
                "const RESULT: u8 = 1",
                "const RESULT: u8 = 2",
                "named implementation changes a type-readable associated constant",
            ),
        ] {
            let source = format!(
                r#"
trait Source {{ type Out }}
trait Extra {{}}
struct Wrap<T> {{}}
trait Output<A, B> {{ {output} }}
impl<T> Output<{direct}> for Wrap<T> {{ {agreed} }}
impl<T: Source + Extra> Output<{projected}> for Wrap<T> {{ {changed} }}
impl<T: Source<Out = u8>> Output<{projected}> for Wrap<T> as Named {{ {agreed} }}
struct Item {{}}
impl Source for Item {{ type Out = u8 }}
impl Extra for Item {{}}
"#
            );
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone("overlapping_outputs.fe".into(), &source);
            let (module, _) = db.top_mod(file);
            let messages = fe_hir::analysis::initialize_analysis_pass()
                .run_on_module(&db, module)
                .into_iter()
                .map(|diagnostic| diagnostic.to_complete(&db).message)
                .collect::<Vec<_>>();
            assert_eq!(messages, vec![expected], "{source}");
            assert!(
                diagnostics(&source.replace(changed, agreed)).is_empty(),
                "{source}"
            );
        }
    }
}

#[test]
fn failure_of_the_selected_projected_opening_does_not_fall_back() {
    let source = PROJECTED_PICK.replace(
        "impl<T: Source> Pick<T::Out>",
        "trait Ready {}\nimpl<T: Source + Ready> Pick<T::Out>",
    );
    let source = format!(
        r#"{source}
fn reject() -> usize {{
    with (Direct) {{ with (Projected<Z>) {{ <Wrap<Z> as Pick<u8>>::n() }} }}
}}
"#
    );
    let codes = diagnostics(&source);
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
}

#[test]
fn projected_named_outputs_must_agree_with_the_ordinary_representative() {
    for (declaration, ordinary, disjoint, changed) in [
        (
            "type Out",
            "type Out = u8",
            "type Out = u16",
            "type Out = bool",
        ),
        (
            "const N: usize",
            "const N: usize = 1",
            "const N: usize = 3",
            "const N: usize = 2",
        ),
    ] {
        for (named, expected) in [(ordinary, vec![]), (changed, vec!["5-0005"])] {
            let source = format!(
                r#"
trait Source {{ type Out }}
struct Wrap<T> {{ value: T }}
trait Output<A> {{ {declaration} }}
impl<T> Output<u8> for Wrap<T> {{ {ordinary} }}
impl<T> Output<u16> for Wrap<T> {{ {disjoint} }}
impl<T: Source<Out = u8>> Output<T::Out> for Wrap<T> as Projected {{ {named} }}
"#
            );
            assert_eq!(diagnostics(&source), expected, "{source}");
        }
    }
}

#[test]
fn mut_condition_keeps_choice_at_mut() {
    values(
        r#"
struct Item { a: u256, b: u256 }
struct Box<T> { v: T }
trait Pick { const fn slot(mut? self) -> mut? u256 }
impl Pick for Item { const fn slot(mut? self) -> mut? u256 { mut? self.a } }
impl Pick for Item as Alt { const fn slot(mut? self) -> mut? u256 { mut? self.b } }
trait Wrap { const fn wrap(mut? self) -> mut? u256 }
impl<T: mut? Pick> Wrap for Box<T> {
    const fn wrap(mut? self) -> mut? u256 { mut? self.v.slot() }
}
const fn at_view() -> u256 {
    let bx = Box<Item> { v: Item { a: 1, b: 2 } }
    with (Alt) { bx.wrap() }
}
const fn chosen_b() -> u256 {
    var bx = Box<Item> { v: Item { a: 1, b: 2 } }
    with (Alt) { bx.wrap() = 9 }
    bx.v.b
}
const fn chosen_a() -> u256 {
    var bx = Box<Item> { v: Item { a: 1, b: 2 } }
    with (Alt) { bx.wrap() = 9 }
    bx.v.a
}
const fn ordinary_a() -> u256 {
    var bx = Box<Item> { v: Item { a: 1, b: 2 } }
    bx.wrap() = 9
    bx.v.a
}
const fn ordinary_b() -> u256 {
    var bx = Box<Item> { v: Item { a: 1, b: 2 } }
    bx.wrap() = 9
    bx.v.b
}
"#,
        &[
            ("at_view", 2),
            ("chosen_b", 9),
            ("chosen_a", 1),
            ("ordinary_a", 9),
            ("ordinary_b", 2),
        ],
    );
}

#[test]
fn chosen_header_is_not_a_fact() {
    for (name, declarations, body, ann) in [
        (
            "header",
            "trait Missing {}\ntrait Sub {}\nimpl<T: Missing> Sub for T as Broken {}\nstruct NeedsSub<T: Sub> {}",
            "Broken<X>",
            "NeedsSub<X>",
        ),
        (
            "supertrait",
            "trait Missing {}\ntrait Super {}\ntrait Sub: Super {}\nimpl<T: Missing + Super> Sub for T as Broken {}\nstruct NeedsSuper<T: Super> {}",
            "Broken<X>",
            "NeedsSuper<X>",
        ),
    ] {
        let without = format!("{declarations}\nfn f<X>() {{ let _: {ann} }}\n");
        let with = format!("{declarations}\nfn f<X>() {{ with ({body}) {{ let _: {ann} }} }}\n");
        let ordinary = diagnostics(&without);
        assert!(
            ordinary.contains(&"6-0003".to_string()),
            "{name}: {ordinary:?}"
        );
        assert_eq!(diagnostics(&with), ordinary, "{name}");
    }
    // Where the conditions hold, the annotation is accepted.
    let source = r#"
trait Missing {}
trait Super {}
trait Sub: Super {}
impl<T: Missing + Super> Sub for T as Broken {}
struct NeedsSuper<T: Super> {}
fn f<X: Missing + Super>() { with (Broken<X>) { let _: NeedsSuper<X> } }
"#;
    assert_eq!(diagnostics(source), Vec::<String>::new());
}

#[test]
fn unapplied_generic_choice_finds_method() {
    values(
        r#"
struct Box<T> { v: T }
trait Pick { const fn pick(self) -> u256 }
struct Item {}
impl<T> Pick for Box<T> as Alt {
    const fn pick(self) -> u256 { 7 }
}
const fn run() -> u256 {
    let b = Box<Item> { v: Item {} }
    with (Alt) { b.pick() }
}
const fn run_path() -> u256 {
    let b = Box<Item> { v: Item {} }
    with (Alt) { Pick::pick(b) }
}
"#,
        &[("run", 7), ("run_path", 7)],
    );
    values(
        r#"
struct Box<T> { v: T }
trait Pick { const fn pick(self) -> u256 }
struct Item {}
impl Pick for Box<Item> as Alt {
    const fn pick(self) -> u256 { 7 }
}
const fn run() -> u256 {
    let b = Box<Item> { v: Item {} }
    with (Alt) { b.pick() }
}
"#,
        &[("run", 7)],
    );
}

#[test]
fn open_constant_headers_may_overlap() {
    let program = |first: &str, second: &str, owner: &str, element: &str| {
        format!(
            r#"
trait HasN {{ const N: usize }}
struct Z {{}}
impl HasN for Z {{ const N: usize = 3 }}
struct Wrap<T> {{}}
struct Other<T> {{}}
trait Tr<A> {{ fn n() -> u256 }}
impl<T: HasN> Tr<[u8; T::N]> for Wrap<T> as P {{ fn n() -> u256 {{ 1 }} }}
impl<T> Tr<[{element}; 3]> for {owner}<T> as Q {{ fn n() -> u256 {{ 2 }} }}
fn f() {{ with ({first}, {second}) {{}} }}
"#
        )
    };
    for (first, second) in [("P", "Q"), ("Q", "P")] {
        let codes = diagnostics(&program(first, second, "Wrap", "u8"));
        assert!(
            codes.contains(&"8-0160".to_string()),
            "{first}, {second}: {codes:?}"
        );
    }
    // A different element type or self type keeps the headers disjoint.
    for (owner, element) in [("Wrap", "u16"), ("Other", "u8")] {
        assert_eq!(
            diagnostics(&program("P", "Q", owner, element)),
            Vec::<String>::new(),
            "{owner} with {element}"
        );
    }
}

#[test]
fn possible_overlap_named_outputs_must_agree() {
    let program = |named: &str, value: &str| {
        format!(
            r#"
trait Source {{ type Out }}
trait Extra {{}}
struct Wrap<T> {{}}
trait Output<A, B> {{
    type Result
    fn make() -> Self::Result
}}
impl<T> Output<u8, T> for Wrap<T> {{
    type Result = u256
    fn make() -> u256 {{ 1 }}
}}
impl<T: Source + Extra> Output<T::Out, T> for Wrap<T> {{
    type Result = {named}
    fn make() -> {named} {{ {value} }}
}}
impl<T: Source + Extra> Output<T::Out, T> for Wrap<T> as Named {{
    type Result = {named}
    fn make() -> {named} {{ {value} }}
}}
"#
        )
    };
    // The second ordinary implementation gives `bool`, which the first does not.
    let changed = program("bool", "true").replace(
        "impl<T> Output<u8, T> for Wrap<T> {\n    type Result = u256\n    fn make() -> u256 { 1 }",
        "impl<T> Output<u8, T> for Wrap<T> {\n    type Result = (u256, u256)\n    fn make() -> (u256, u256) { (1, 2) }",
    );
    assert_eq!(diagnostics(&changed), vec!["5-0005"]);
    assert_eq!(
        diagnostics(&program("u256", "2")),
        Vec::<String>::new(),
        "equal outputs agree"
    );
}

#[test]
fn inherent_const_receives_choice() {
    values(
        r#"
trait Foo { const fn value() -> u256 }
impl Foo for u256 { const fn value() -> u256 { 1 } }
impl Foo for u256 as AltFoo { const fn value() -> u256 { 2 } }
struct Wrapper<T> {}
impl<T: Foo> Wrapper<T> {
    const X: u256 = T::value()
    const fn x() -> u256 { T::value() }
}
const fn read() -> u256 { with (AltFoo) { Wrapper<u256>::X } }
const fn read_method() -> u256 { with (AltFoo) { Wrapper<u256>::x() } }
const fn read_ordinary() -> u256 { Wrapper<u256>::X }
const fn read_after() -> u256 {
    let inside = with (AltFoo) { Wrapper<u256>::X }
    inside * 10 + Wrapper<u256>::X
}
"#,
        &[
            ("read", 2),
            ("read_method", 2),
            ("read_ordinary", 1),
            ("read_after", 21),
        ],
    );
}

#[test]
fn written_types_refuse_inherent_constant_reads_of_choices() {
    let program = |use_: &str| {
        format!(
            r#"{PICKS}
struct Array<const N: u256> {{}}
struct Wrapper<T> {{}}
impl<T: Pick> Wrapper<T> {{ const X: u256 = T::value() }}
fn chosen() {{ with (Discount) {{ let x: Array<{{ {use_} }}> = Array {{}} }} }}
"#
        )
    };
    // The twin through a trait constant's goal is refused the same way.
    let trait_const = format!(
        r#"{PICKS}
struct Array<const N: u256> {{}}
fn chosen() {{ with (Discount) {{ let x: Array<{{ Item::value() }}> = Array {{}} }} }}
"#
    );
    let twin = diagnostics(&trait_const);
    assert!(twin.contains(&"8-0138".to_string()), "{twin:?}");
    let codes = diagnostics(&program("Wrapper<Item>::X"));
    assert_eq!(codes, twin, "{codes:?}");
    let ordinary = program("Wrapper<Item>::X").replace("with (Discount) ", "");
    assert!(diagnostics(&ordinary).is_empty());
}
// An implied bound is one component of the dictionary that implies it. A proof
// written for the same goal, a second dictionary, or the implementation's own
// condition must name the same implementation, or the instance is refused.
// Each program below reads `Item: Pick` (or `Item: Price`) through two paths.
const PROJECTION_PRELUDE: &str = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl<T> Pick for T as Named { const fn val() -> u256 { 70 } }
struct Wrap<T> { t: T }
trait Tr { type A: Pick
    const fn via() -> u256 }
impl<T: Pick> Tr for Wrap<T> { type A = T
    const fn via() -> u256 { T::val() } }
const fn target<B: Tr<A = X>, X: Pick>() -> u256 { X::val() * 1000 + B::via() }
const fn relay<B: Tr<A = X>, X: Pick>() -> u256 { with (Named<X>) { target<B, X>() } }
"#;

#[test]
fn a_written_choice_that_differs_from_the_received_dictionary_is_refused() {
    refused(
        &format!(
            r#"{PROJECTION_PRELUDE}
const fn top() -> u256 {{ relay<Wrap<Item>, Item>() }}
"#
        ),
        "top",
        "Item: Pick",
    );
}

#[test]
fn a_written_choice_that_matches_the_received_dictionary_is_accepted() {
    values(
        &format!(
            r#"{PROJECTION_PRELUDE}
const fn top() -> u256 {{ with (Named<Item>) {{ relay<Wrap<Item>, Item>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

const SUPER_PRELUDE: &str = r#"
struct Item {}
trait Price { const fn price() -> u256 }
impl Price for Item { const fn price() -> u256 { 100 } }
impl<T> Price for T as Wholesale { const fn price() -> u256 { 70 } }
trait Sub: Price {}
impl Sub for Item {}
const fn target<T: Sub + Price>() -> u256 { T::price() }
const fn relay<T: Sub>() -> u256 { with (Wholesale<T>) { target<T>() } }
"#;

#[test]
fn a_written_choice_that_differs_from_a_supertrait_component_is_refused() {
    refused(
        &format!(
            r#"{SUPER_PRELUDE}
const fn top() -> u256 {{ relay<Item>() }}
"#
        ),
        "top",
        "Item: Price",
    );
}

#[test]
fn a_written_choice_that_differs_from_a_supertrait_component_is_refused_under_a_caller_choice() {
    refused(
        &format!(
            r#"{SUPER_PRELUDE}
const fn top() -> u256 {{ with (Wholesale<Item>) {{ relay<Item>() }} }}
"#
        ),
        "top",
        "Item: Price",
    );
}

const TRANSITIVE_PRELUDE: &str = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
struct Wrap<T> { t: T }
trait Middle { type V: Pick
    const fn via() -> u256 }
impl<T: Pick> Middle for Wrap<T> { type V = T
    const fn via() -> u256 { T::val() } }
struct Slot<T> { t: T }
trait Outer { type M: Middle }
impl<T: Middle<V = Item>> Outer for Slot<T> { type M = T }
const fn read<B: Outer>() -> u256 {
    <<B::M as Middle>::V as Pick>::val() * 1000 + <B::M as Middle>::via()
}
const fn relay<B: Outer<M = Wrap<Item>>>() -> u256 { read<B>() }
"#;

// `Outer`'s implementation fixes `Middle::V = Item`, so `Item: Pick` follows
// from the `Outer` dictionary, and also from the `Middle` dictionary inside it.
// Both read the choice that proved `Slot<Wrap<Item>>: Outer`.
#[test]
fn transitive_implied_bounds_read_one_dictionary_through_a_relay() {
    values(
        &format!(
            r#"{TRANSITIVE_PRELUDE}
const fn chosen() -> u256 {{ with (Named) {{ relay<Slot<Wrap<Item>>>() }} }}
"#
        ),
        &[("chosen", 70070)],
    );
}

#[test]
fn transitive_implied_bounds_read_the_ordinary_dictionary_through_a_relay() {
    values(
        &format!(
            r#"{TRANSITIVE_PRELUDE}
const fn plain() -> u256 {{ relay<Slot<Wrap<Item>>>() }}
"#
        ),
        &[("plain", 100100)],
    );
}

#[test]
fn transitive_implied_bounds_read_one_dictionary_without_a_relay() {
    values(
        &format!(
            r#"{TRANSITIVE_PRELUDE}
const fn direct() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
"#
        ),
        &[("direct", 70070)],
    );
}

// The implementation's condition `T: Holder<V = Item>` carries the dictionary
// from which `Item: Pick` follows. It is not proved again beside the condition.
const CONDITION_PRELUDE: &str = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
struct H<T> { t: T }
trait Holder { type V: Pick
    const fn via() -> u256 }
impl<T: Pick> Holder for H<T> { type V = T
    const fn via() -> u256 { T::val() } }
struct Slot<T> { t: T }
trait Outer { type M: Pick
    const fn hv() -> u256 }
impl<T: Holder<V = Item>> Outer for Slot<T> { type M = <T as Holder>::V
    const fn hv() -> u256 { T::via() } }
const fn read<B: Outer>() -> u256 { <B::M as Pick>::val() * 1000 + B::hv() }
"#;

#[test]
fn an_implementation_projects_its_concrete_implied_goal_from_its_condition() {
    values(
        &format!(
            r#"{CONDITION_PRELUDE}
const fn chosen() -> u256 {{ with (Named) {{ read<Slot<H<Item>>>() }} }}
"#
        ),
        &[("chosen", 70070)],
    );
}

#[test]
fn an_implementation_projects_its_concrete_implied_goal_from_an_ordinary_condition() {
    values(
        &format!(
            r#"{CONDITION_PRELUDE}
const fn plain() -> u256 {{ read<Slot<H<Item>>>() }}
"#
        ),
        &[("plain", 100100)],
    );
}

// A named implementation that is the root of the dictionary answers its own
// associated bound.
#[test]
fn a_named_root_implementation_answers_its_implied_bound() {
    values(
        r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
struct Slot<T> { t: T }
trait Outer { type M: Pick
    const fn via() -> u256 }
impl<T: Pick> Outer for Slot<T> { type M = T
    const fn via() -> u256 { T::val() } }
impl<T: Pick> Outer for Slot<T> as OuterNamed { type M = T
    const fn via() -> u256 { T::val() } }
const fn read<B: Outer>() -> u256 { <B::M as Pick>::val() * 1000 + B::via() }
const fn relay<B: Outer<M = Item>>() -> u256 { read<B>() }
const fn top() -> u256 { with (Named, OuterNamed) { relay<Slot<Item>>() } }
"#,
        &[("top", 70070)],
    );
}

#[test]
fn a_supertrait_chain_through_an_associated_bound_reads_one_dictionary() {
    values(
        r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
trait Middle: Pick { const fn via() -> u256 }
impl<T: Pick> Middle for T { const fn via() -> u256 { T::val() } }
struct Slot<T> { t: T }
trait Outer { type M: Middle }
impl<T: Middle> Outer for Slot<T> { type M = T }
const fn read<B: Outer>() -> u256 { <B::M as Pick>::val() * 1000 + <B::M as Middle>::via() }
const fn relay<B: Outer<M = Item>>() -> u256 { read<B>() }
const fn top() -> u256 { with (Named) { relay<Slot<Item>>() } }
"#,
        &[("top", 70070)],
    );
}

// `Outer`'s implementation fixes `M = Item` by itself. Its condition `T: Holder`
// does not constrain `Holder::V`, so the named `Holder` dictionary says nothing
// about `Item: Pick`. The component is the ordinary implementation even when
// `Holder::V` happens to be `Item`, and it follows the implementation's own
// header, not the types a caller substitutes.
const COINCIDENT_PRELUDE: &str = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
struct Wrap<T> { t: T }
trait Holder { type V: Pick }
impl<T: Pick> Holder for Wrap<T> { type V = T }
struct Slot<T> { t: T }
trait Outer { type M: Pick }
"#;

#[test]
fn an_unconstrained_condition_does_not_supply_an_independent_component() {
    values(
        &format!(
            r#"{COINCIDENT_PRELUDE}
impl<T: Holder> Outer for Slot<T> {{ type M = Item }}
const fn read<B: Outer>() -> u256 {{ <B::M as Pick>::val() }}
const fn independent() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
"#
        ),
        &[("independent", 100)],
    );
}

#[test]
fn a_condition_that_fixes_the_associated_type_supplies_the_component() {
    values(
        &format!(
            r#"{COINCIDENT_PRELUDE}
impl<T: Holder<V = Item>> Outer for Slot<T> {{ type M = Item }}
const fn read<B: Outer>() -> u256 {{ <B::M as Pick>::val() }}
const fn dependent() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
"#
        ),
        &[("dependent", 70)],
    );
}

// A generic caller proves `Slot<T>: Outer` from `T: Holder<V = Item>`. The
// projection of `Item: Pick` reaches the caller's own assumption, which has no
// slot for the concrete bound yet. An unrelated block choice around the call
// leaves the slot to the instance, which holds the dictionary.
const RELAY_CONSTRUCTED: &str = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
struct H<T> { t: T }
trait Holder { type V: Pick
    const fn via() -> u256 }
impl<T: Pick> Holder for H<T> { type V = T
    const fn via() -> u256 { T::val() } }
struct Slot<T> { t: T }
trait Outer { type M: Pick
    const fn hv() -> u256 }
impl<T: Holder<V = Item>> Outer for Slot<T> { type M = <T as Holder>::V
    const fn hv() -> u256 { T::via() } }
struct Other {}
trait Mark { const fn mark() -> u256 }
impl Mark for Other { const fn mark() -> u256 { 0 } }
impl Mark for Other as Aside { const fn mark() -> u256 { 1 } }
const fn read<B: Outer>() -> u256 { <B::M as Pick>::val() * 1000 + B::hv() }
"#;

#[test]
fn an_unrelated_choice_around_a_constructed_dictionary_leaves_the_projection_pending() {
    values(
        &format!(
            r#"{RELAY_CONSTRUCTED}
const fn relay<T: Holder<V = Item>>() -> u256 {{ with (Aside) {{ read<Slot<T>>() }} }}
const fn chosen() -> u256 {{ with (Named) {{ relay<H<Item>>() }} }}
"#
        ),
        &[("chosen", 70070)],
    );
}

#[test]
fn a_constructed_dictionary_without_a_choice_reads_one_dictionary() {
    values(
        &format!(
            r#"{RELAY_CONSTRUCTED}
const fn relay<T: Holder<V = Item>>() -> u256 {{ read<Slot<T>>() }}
const fn chosen() -> u256 {{ with (Named) {{ relay<H<Item>>() }} }}
"#
        ),
        &[("chosen", 70070)],
    );
}

// `Left::Out` requires `Right<Back = Self>` and `Right::Back` requires
// `Left<Out = Self>`: the two slots of `call` imply each other.
const MUTUAL_PRELUDE: &str = r#"
struct L {}
struct R {}
trait Left { type Out: Right<Back = Self> }
trait Right { type Back: Left<Out = Self> }
impl Left for L { type Out = R }
impl Right for R { type Back = L }
const fn call<T: Left<Out = U>, U: Right<Back = T>>() -> u256 { 1 }
"#;

#[test]
fn mutually_implied_slots_agree_and_are_accepted() {
    values(
        &format!(
            r#"{MUTUAL_PRELUDE}
const fn top() -> u256 {{ call<L, R>() }}
"#
        ),
        &[("top", 1)],
    );
}

// `R`'s `Right` implementation is fixed where `impl Left for L` is declared,
// so a block choice cannot replace it for `call`'s second slot.
#[test]
fn mutually_implied_slots_refuse_a_choice_that_differs_from_the_fixed_component() {
    let codes = diagnostics(&format!(
        r#"{MUTUAL_PRELUDE}
impl Right for R as Alt {{ type Back = L }}
const fn top() -> u256 {{ with (Alt) {{ call<L, R>() }} }}
"#
    ));
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
}

// The condition `T: Grow<Next = Wrap<Item>>` makes the bound `Next: Grow`
// concrete. The search for the conditions that imply a goal keeps concrete
// bounds, and must still end for a trait whose associated type is bounded by
// the trait itself.
#[test]
fn a_self_bounded_associated_type_does_not_loop_the_concrete_bound_walk() {
    values(
        r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
struct Wrap<T> { t: T }
trait Grow { type Next: Grow }
impl Grow for Item { type Next = Item }
impl<T: Grow> Grow for Wrap<T> { type Next = Wrap<Item> }
struct Slot<T> { t: T }
trait Outer { type M: Pick }
impl<T: Grow<Next = Wrap<Item>>> Outer for Slot<T> { type M = Item }
const fn read<B: Outer>() -> u256 { <B::M as Pick>::val() }
const fn top() -> u256 { read<Slot<Wrap<Item>>>() }
"#,
        &[("top", 100)],
    );
}

// The explicit-bound conflict through a trait method call: `Reader::repeated`
// bounds `X: Pick` explicitly beside `B: Tr<A = X>`, as the free function does.
#[test]
fn an_explicit_bound_of_a_trait_method_is_checked_against_the_received_dictionary() {
    let signature = "const fn repeated<B: Tr<A = X>, X: Pick>() -> u256";
    let body = "{ X::value() * 1000 + B::via() }";
    let source = format!(
        r#"{PICKS}{WRAPPED_PICK}
struct Reader {{}}
trait Read {{ {signature} }}
impl Read for Reader {{ {signature} {body} }}
const fn relay<B: Tr<A = Item>>() -> u256 {{ with (Discount) {{ Reader::repeated<B, Item>() }} }}
const fn top() -> u256 {{ relay<Wrap<Item>>() }}
"#
    );
    refused(&source, "top", "Item: Pick");
    values(
        &source.replace(
            "{ relay<Wrap<Item>>() }",
            "{ with (Discount) { relay<Wrap<Item>>() } }",
        ),
        &[("top", 70070)],
    );
}

// `Holder::V: Middle` and `Middle: Pick`. The condition `T: Holder<V = Item>`
// makes `Item: Middle` concrete, and the dictionary of that bound holds the
// `Pick` implementation chosen where `Slot<Wrap<Item>>: Outer` is proved. The
// goal `Item: Pick` is implied past the concrete intermediate bound, so the
// walk over the condition follows the supertrait edge of `Item: Middle`.
const CONCRETE_SUPER_PRELUDE: &str = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
trait Middle: Pick {}
impl<T: Pick> Middle for T {}
trait Holder { type V: Middle }
struct Wrap<T> { t: T }
impl<T: Middle> Holder for Wrap<T> { type V = T }
trait Outer { type M: Middle }
struct Slot<T> { t: T }
impl<T: Holder<V = Item>> Outer for Slot<T> { type M = Item }
const fn read<B: Outer>() -> u256 { <B::M as Pick>::val() }
"#;

#[test]
fn a_supertrait_of_a_concrete_intermediate_bound_is_read_from_the_received_dictionary() {
    values(
        &format!(
            r#"{CONCRETE_SUPER_PRELUDE}
const fn plain() -> u256 {{ read<Slot<Wrap<Item>>>() }}
const fn chosen() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
"#
        ),
        &[("plain", 100), ("chosen", 70)],
    );
}

// Programs without a block choice. A bound that an equality or a supertrait
// implies is read from the dictionary of the bound that implies it, so each
// of these compiles as it does without the projection.
#[test]
fn a_binding_that_names_a_sibling_associated_type_is_accepted() {
    values(
        r#"
struct X {}
struct Y {}
trait Show { type K }
trait Tr {
    type K
    type A: Show<K = Self::K>
}
impl Show for Y { type K = u8 }
impl Tr for X {
    type K = u8
    type A = Y
}
const fn f<T: Tr<K = u8>>() -> u256 { 5 }
const fn g<T: Tr<K = u8>>() -> u256 { f<T>() }
const fn top() -> u256 { f<X>() }
const fn relay() -> u256 { g<X>() }
"#,
        &[("top", 5), ("relay", 5)],
    );
}

#[test]
fn a_binding_that_names_a_sibling_associated_type_is_accepted_under_a_choice() {
    values(
        r#"
struct X {}
struct Y {}
struct Z {}
trait Show { type K }
trait Tr {
    type K
    type A: Show<K = Self::K>
}
impl Show for Y { type K = u8 }
impl Tr for X {
    type K = u8
    type A = Y
}
trait Q {}
impl Q for Z as Aside {}
const fn f<T: Tr<K = u8>>() -> u256 { 5 }
const fn top() -> u256 { with (Aside) { f<X>() } }
"#,
        &[("top", 5)],
    );
}

#[test]
fn a_binding_that_names_a_sibling_associated_type_is_accepted_for_a_generic_implementation() {
    values(
        r#"
struct G<P> {}
struct Y {}
trait Show { type K }
trait Tr {
    type K
    type A: Show<K = Self::K>
}
impl Show for Y { type K = u8 }
impl<P> Tr for G<P> {
    type K = u8
    type A = Y
}
const fn f<T: Tr<K = u8>>() -> u256 { 6 }
const fn top() -> u256 { f<G<u16>>() }
"#,
        &[("top", 6)],
    );
}

#[test]
fn a_binding_that_names_a_different_associated_type_stays_accepted() {
    values(
        r#"
struct X {}
struct Y {}
trait Show { type K }
trait Tr {
    type K
    type J
    type A: Show<K = Self::J>
}
impl Show for Y { type K = u8 }
impl Tr for X {
    type K = u8
    type J = u8
    type A = Y
}
const fn f<T: Tr<K = u8>>() -> u256 { 7 }
const fn top() -> u256 { f<X>() }
"#,
        &[("top", 7)],
    );
}

#[test]
fn a_three_level_chain_of_pinned_bindings_is_accepted() {
    values(
        r#"
struct X {}
struct Y {}
struct Z {}
trait Leaf { type K }
trait Mid { type K
    type L: Leaf<K = Self::K> }
trait Top { type K
    type M: Mid<K = Self::K> }
impl Leaf for Z { type K = u8 }
impl Mid for Y { type K = u8
    type L = Z }
impl Top for X { type K = u8
    type M = Y }
const fn f<T: Top>() -> u256 { 1 }
const fn g<T: Top<K = u8>>() -> u256 { 2 }
const fn top() -> u256 { f<X>() }
const fn top2() -> u256 { g<X>() }
"#,
        &[("top", 1), ("top2", 2)],
    );
}

const INTO_ITER: &str = r#"
trait Iter { type Item
    const fn next(self) -> Self::Item }
trait IntoIt { type Item
    type It: Iter<Item = Self::Item>
    const fn into_it(self) -> Self::It }
struct MyIter { v: u256 }
impl Iter for MyIter { type Item = u256
    const fn next(self) -> u256 { self.v } }
struct Src { v: u256 }
impl IntoIt for Src { type Item = u256
    type It = MyIter
    const fn into_it(self) -> MyIter { MyIter { v: self.v + 1 } } }
"#;

#[test]
fn an_iterator_item_pinned_by_the_caller_is_accepted() {
    values(
        &format!(
            r#"{INTO_ITER}
const fn first<C: IntoIt>(c: C) -> C::Item {{ c.into_it().next() }}
const fn relay<C: IntoIt<Item = u256>>(c: C) -> u256 {{ first(c) * 2 }}
const fn v() -> u256 {{ first(c: Src {{ v: 4 }}) * 100 + relay(c: Src {{ v: 9 }}) }}
"#
        ),
        &[("v", 520)],
    );
}

#[test]
fn a_blanket_implementation_that_pins_an_item_is_accepted() {
    values(
        &format!(
            r#"{INTO_ITER}
trait Summer {{ const fn sum(self) -> u256 }}
impl<C: IntoIt<Item = u256>> Summer for C {{ const fn sum(self) -> u256 {{ self.into_it().next() }} }}
const fn total<S: Summer>(s: S) -> u256 {{ s.sum() }}
const fn v() -> u256 {{ total(s: Src {{ v: 41 }}) }}
"#
        ),
        &[("v", 42)],
    );
}

const NODE: &str = r#"
struct A {}
struct B {}
trait Node { type Next: Node<Next = Self>
    const fn id() -> u256 }
impl Node for A { type Next = B
    const fn id() -> u256 { 1 } }
impl Node for B { type Next = A
    const fn id() -> u256 { 2 } }
struct Z {}
trait Q {}
impl Q for Z as Aside {}
"#;
#[test]
fn an_associated_type_that_names_its_owner_back_is_accepted() {
    values(
        &format!(
            r#"{NODE}
const fn f<T: Node>() -> u256 {{ T::id() * 10 + <T::Next as Node>::id() }}
const fn v() -> u256 {{ f<A>() * 100 + f<B>() }}
"#
        ),
        &[("v", 1221)],
    );
}

#[test]
fn an_associated_type_that_names_its_owner_back_is_accepted_when_unused() {
    values(
        &format!(
            r#"{NODE}
const fn g<T: Node>() -> u256 {{ 3 }}
const fn plain() -> u256 {{ g<A>() }}
"#
        ),
        &[("plain", 3)],
    );
}

#[test]
fn an_associated_type_that_names_its_owner_back_is_accepted_under_a_choice() {
    values(
        &format!(
            r#"{NODE}
const fn g<T: Node>() -> u256 {{ 3 }}
const fn aside() -> u256 {{ with (Aside) {{ g<A>() + g<B>() }} }}
"#
        ),
        &[("aside", 6)],
    );
}

#[test]
fn a_node_pair_written_in_both_directions_stays_accepted() {
    values(
        &format!(
            r#"{NODE}
const fn f<T: Node<Next = U>, U: Node<Next = T>>() -> u256 {{ 4 }}
const fn top() -> u256 {{ f<A, B>() }}
"#
        ),
        &[("top", 4)],
    );
}

#[test]
fn a_node_cycle_without_a_back_reference_stays_accepted() {
    values(
        r#"
struct A {}
struct B {}
struct C {}
trait Node { type Next: Node }
impl Node for A { type Next = B }
impl Node for B { type Next = C }
impl Node for C { type Next = A }
const fn f<T: Node>() -> u256 { 5 }
const fn top() -> u256 { f<A>() }
"#,
        &[("top", 5)],
    );
}

const LEFT_RIGHT: &str = r#"
struct L {}
struct R {}
trait Left { type Out: Right<Back = Self>
    const fn l() -> u256 }
trait Right { type Back: Left<Out = Self>
    const fn r() -> u256 }
impl Left for L { type Out = R
    const fn l() -> u256 { 1 } }
impl Right for R { type Back = L
    const fn r() -> u256 { 20 } }
"#;

#[test]
fn a_left_bound_alone_is_accepted_when_unused() {
    values(
        &format!(
            r#"{LEFT_RIGHT}
const fn only_left<T: Left>() -> u256 {{ 7 }}
const fn n14() -> u256 {{ only_left<L>() }}
"#
        ),
        &[("n14", 7)],
    );
}

#[test]
fn a_left_bound_alone_reads_the_right_component() {
    values(
        &format!(
            r#"{LEFT_RIGHT}
const fn left_out<T: Left>() -> u256 {{ <T::Out as Right>::r() }}
const fn n15() -> u256 {{ left_out<L>() }}
"#
        ),
        &[("n15", 20)],
    );
}

#[test]
fn a_right_bound_alone_reads_the_left_component() {
    values(
        &format!(
            r#"{LEFT_RIGHT}
const fn only_right<U: Right>() -> u256 {{ <U::Back as Left>::l() }}
const fn n16() -> u256 {{ only_right<R>() }}
"#
        ),
        &[("n16", 1)],
    );
}

#[test]
fn a_left_bound_alone_is_accepted_when_only_checked() {
    values(
        &format!(
            r#"{LEFT_RIGHT}
const fn only_left<T: Left>() -> u256 {{ 7 }}
const fn n17() -> u256 {{ only_left<L>() }}
"#
        ),
        &[("n17", 7)],
    );
}

#[test]
fn a_left_bound_alone_is_accepted_through_a_relay() {
    values(
        &format!(
            r#"{LEFT_RIGHT}
const fn only_left<T: Left>() -> u256 {{ 7 }}
const fn relay<T: Left>() -> u256 {{ only_left<T>() }}
const fn n19() -> u256 {{ relay<L>() }}
"#
        ),
        &[("n19", 7)],
    );
}

// A bound a trait states on its associated type is answered by the bound the
// implementation's condition states, when that bound says at least as much.
fn strength(holder: &str, outer: &str) -> String {
    format!(
        r#"
struct I {{}}
trait P {{ type Out
    const fn n() -> u256 }}
impl P for I {{ type Out = u8
    const fn n() -> u256 {{ 31 }} }}
impl P for I as Tiny {{ type Out = u8
    const fn n() -> u256 {{ 7 }} }}
struct W<T> {{}}
struct Box<T> {{}}
trait H {{ type A: {holder}
    const fn via() -> u256 }}
impl<T: {holder}> H for W<T> {{ type A = T
    const fn via() -> u256 {{ T::n() }} }}
trait K {{ type A: {outer}
    const fn via() -> u256 }}
impl<T: H<A = I>> K for Box<T> {{ type A = I
    const fn via() -> u256 {{ T::via() }} }}
const fn get<B: K>() -> u256 {{ <B::A as P>::n() * 100 + B::via() }}
const fn chosen() -> u256 {{ with (Tiny) {{ get<Box<W<I>>>() }} }}
const fn plain() -> u256 {{ get<Box<W<I>>>() }}
"#
    )
}

#[test]
fn a_stronger_condition_bound_answers_a_weaker_associated_bound() {
    // The condition says `Out = u8`; the implied bound only needs `P`.
    values(
        &strength("P<Out = u8>", "P"),
        &[("chosen", 707), ("plain", 3131)],
    );
}

#[test]
fn a_condition_bound_equal_to_the_associated_bound_answers_it() {
    values(
        &strength("P<Out = u8>", "P<Out = u8>"),
        &[("chosen", 707), ("plain", 3131)],
    );
}

// The condition says only `P`, and the associated bound `P<Out = u8>` is on
// the closed type `I`. A proof names no goal, so the dictionary that holds
// `I: P` holds the implementation of `I: P<Out = u8>` too, and the
// implementation's component is read from it, not proved again.
#[test]
fn a_weaker_condition_bound_answers_a_closed_goal_by_identity() {
    values(
        &strength("P", "P<Out = u8>"),
        &[("chosen", 707), ("plain", 3131)],
    );
}

// Two conditions that hold the same implied goal in different dictionaries are
// refused, at the call or where the instance is admitted.
#[test]
fn conditions_that_hold_an_implied_goal_in_different_dictionaries_are_refused() {
    let source = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
trait Holder { type V: Pick
    const fn hv() -> u256 }
struct Wrap<T> {}
impl<T: Pick> Holder for Wrap<T> { type V = T
    const fn hv() -> u256 { T::val() } }
struct Fixed {}
impl Holder for Fixed { type V = Item
    const fn hv() -> u256 { Item::val() } }
trait Outer { type M: Pick
    const fn ov() -> u256 }
struct Pair<T, U> {}
impl<T: Holder<V = Item>, U: Holder<V = Item>> Outer for Pair<T, U> { type M = Item
    const fn ov() -> u256 { T::hv() * 1000 + U::hv() } }
const fn read<B: Outer>() -> u256 { <B::M as Pick>::val() * 1000000 + B::ov() }
const fn chosen() -> u256 { with (Named) { read<Pair<Wrap<Item>, Fixed>>() } }
"#;
    let codes = diagnostics(source);
    if codes.is_empty() {
        refused(source, "chosen", "Item: Pick");
    } else {
        assert_eq!(codes, vec!["6-0003".to_string()], "{codes:?}");
    }
}

// A concrete bound that a declared bound implies, such as `Foo: Show` from
// `B: Tr<A = Foo>` where `Tr::A: Show`, is a component of the dictionary the
// body received for `B`. A goal written on `Foo` reads that component. The
// programs below read `Show` for `Foo` directly (`one<B::A>`, `Foo::show`)
// and through `B::via`, which reads the dictionary of `B`.
const SHOWS: &str = r#"
trait Show { const fn show() -> u256 }
struct Foo {}
impl Show for Foo { const fn show() -> u256 { 100 } }
impl Show for Foo as Loud { const fn show() -> u256 { 70 } }
struct W<T> {}
struct V<T> {}
struct Box<T> {}
trait Tr { type A: Show
    const fn via() -> u256 }
impl<T: Show> Tr for W<T> { type A = T
    const fn via() -> u256 { T::show() } }
trait Tr2 { type A: Show
    const fn via2() -> u256 }
impl<T: Show> Tr2 for V<T> { type A = T
    const fn via2() -> u256 { T::show() } }
const fn one<S: Show>() -> u256 { S::show() }
trait Other { const fn o() -> u256 }
impl Other for Foo { const fn o() -> u256 { 1 } }
impl Other for Foo as Aside { const fn o() -> u256 { 2 } }
"#;

// The `show` implementations a program reaches from `root`, sorted, each
// flagged when it is a named one. Closures and function items are not
// evaluated at compile time, so the callees of the instances are read.
fn reached_shows(source: &str, root: &str) -> Vec<bool> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("named_reached.fe".into(), source);
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, root), None);
    let mut pending = vec![root_semantic_instance_key(&db, owner).expect("root key")];
    let mut seen = Vec::new();
    let mut named = Vec::new();
    while let Some(key) = pending.pop() {
        if seen.contains(&key) {
            continue;
        }
        seen.push(key);
        if let BodyOwner::Func(func, _) = key.owner(&db)
            && func
                .name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "show")
            && let Some(ItemKind::ImplTrait(implementation)) = func.scope().parent_item(&db)
        {
            named.push(implementation.selection_name(&db).is_some());
        }
        for callee in SemanticInstance::new(&db, key).callees(&db) {
            pending.push(callee.key);
        }
    }
    named.sort();
    named.dedup();
    named
}

#[test]
fn concrete_component_call_bound() {
    values(
        &format!(
            r#"{SHOWS}
const fn caller<B: Tr<A = Foo>>() -> u256 {{ one<B::A>() * 1000 + B::via() }}
const fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
const fn plain() -> u256 {{ caller<W<Foo>>() }}
"#
        ),
        &[("top", 70070), ("plain", 100100)],
    );
}

#[test]
fn concrete_component_dispatch() {
    values(
        &format!(
            r#"{SHOWS}
const fn written<B: Tr<A = Foo>>() -> u256 {{ Foo::show() * 1000 + B::via() }}
const fn qualified<B: Tr<A = Foo>>() -> u256 {{ <B::A as Show>::show() * 1000 + B::via() }}
const fn top_written() -> u256 {{ with (Loud) {{ written<W<Foo>>() }} }}
const fn top_qualified() -> u256 {{ with (Loud) {{ qualified<W<Foo>>() }} }}
"#
        ),
        &[("top_written", 70070), ("top_qualified", 70070)],
    );
}

#[test]
fn concrete_component_condition() {
    values(
        &format!(
            r#"{SHOWS}
impl<T: Show> Show for Box<T> {{ const fn show() -> u256 {{ T::show() }} }}
const fn caller<B: Tr<A = Foo>>() -> u256 {{ one<Box<B::A>>() * 1000 + B::via() }}
const fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

// A choice written in the body that names the goal answers it there.
#[test]
fn concrete_component_block_names_goal() {
    values(
        &format!(
            r#"{SHOWS}
const fn caller<B: Tr<A = Foo>>() -> u256 {{
    let inner = with (Loud) {{ one<B::A>() }}
    inner * 1000 + B::via()
}}
const fn top() -> u256 {{ caller<W<Foo>>() }}
"#
        ),
        &[("top", 70100)],
    );
}

#[test]
fn concrete_component_unrelated_block() {
    values(
        &format!(
            r#"{SHOWS}
const fn caller<B: Tr<A = Foo>>() -> u256 {{
    let inner = with (Aside) {{ one<B::A>() }}
    inner * 1000 + B::via()
}}
const fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

// Two dictionaries that hold different implementations of one component make
// the instance an error, used or not. Two that agree give one value.
fn two_roots(body: &str) -> String {
    format!(
        r#"{SHOWS}
const fn f<B: Tr<A = Foo>, C: Tr2<A = Foo>>() -> u256 {{
    Foo::show() * 1000000 + B::via() * 1000 + C::via2()
}}
const fn relay<C: Tr2<A = Foo>>() -> u256 {{ with (Loud) {{ f<W<Foo>, C>() }} }}
{body}
"#
    )
}

#[test]
fn concrete_component_two_roots_disagree() {
    refused(
        &two_roots("const fn disagree() -> u256 { relay<V<Foo>>() }"),
        "disagree",
        "Foo: Show",
    );
}

#[test]
fn concrete_component_two_roots_agree() {
    values(
        &two_roots("const fn agree() -> u256 { with (Loud) { f<W<Foo>, V<Foo>>() } }"),
        &[("agree", 70070070)],
    );
}

// The order of the bounds of `f` does not decide whether two dictionaries
// agree: the swapped form is refused and accepted as the written one is.
#[test]
fn concrete_component_two_roots_swapped() {
    let program = |body: &str| {
        format!(
            r#"{SHOWS}
const fn f<C: Tr2<A = Foo>, B: Tr<A = Foo>>() -> u256 {{
    Foo::show() * 1000000 + B::via() * 1000 + C::via2()
}}
const fn relay<C: Tr2<A = Foo>>() -> u256 {{ with (Loud) {{ f<C, W<Foo>>() }} }}
{body}
"#
        )
    };
    refused(
        &program("const fn disagree() -> u256 { relay<V<Foo>>() }"),
        "disagree",
        "Foo: Show",
    );
    values(
        &program("const fn agree() -> u256 { with (Loud) { f<V<Foo>, W<Foo>>() } }"),
        &[("agree", 70070070)],
    );
}

// A choice in scope that names the goal wins over the component, whether the
// goal is written on the projection or on the type it settles to.
#[test]
fn concrete_component_choice_names_projected_goal() {
    values(
        &format!(
            r#"{SHOWS}
const fn caller<B: Tr<A = Foo>>() -> u256 {{
    with (Loud) {{ <B::A as Show>::show() * 1000 + Foo::show() }}
}}
const fn top() -> u256 {{ caller<W<Foo>>() }}
"#
        ),
        &[("top", 70070)],
    );
}

#[test]
fn concrete_component_closure() {
    let source = format!(
        r#"{SHOWS}
fn caller<B: Tr<A = Foo>>() -> u256 {{
    let c = || one<B::A>()
    c() * 1000 + B::via()
}}
fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
"#
    )
    .replace("const fn", "fn");
    assert_eq!(reached_shows(&source, "top"), vec![true]);
}

#[test]
fn concrete_component_late_function_item() {
    let source = format!(
        r#"{SHOWS}
fn caller<B: Tr<A = Foo>>() -> u256 {{
    let f = one<B::A>
    f() * 1000 + B::via()
}}
fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
"#
    )
    .replace("const fn", "fn");
    assert_eq!(reached_shows(&source, "top"), vec![true]);
}

// A function item passed to a callable bound carries the proofs of its bounds
// made where it is passed. Its `Foo: Show` is the component of `B`.
#[test]
fn concrete_component_function_item_passed() {
    let source = format!(
        r#"{SHOWS}
fn run<F: Fn() -> u256>(_ f: F) -> u256 uses (F::E) {{ f() }}
fn caller<B: Tr<A = Foo>>() -> u256 {{ run(one<B::A>) * 1000 + B::via() }}
fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
"#
    )
    .replace("const fn", "fn");
    assert_eq!(reached_shows(&source, "top"), vec![true]);
}

const PICK_ITEM: &str = r#"
struct Item {}
trait Pick { const fn val() -> u256 }
impl Pick for Item { const fn val() -> u256 { 100 } }
impl Pick for Item as Named { const fn val() -> u256 { 70 } }
struct Wrap<T> {}
"#;

#[test]
fn concrete_component_impl_method() {
    values(
        &format!(
            r#"{PICK_ITEM}
trait Holder {{ type V: Pick }}
impl<T: Pick> Holder for Wrap<T> {{ type V = T }}
struct Slot<T> {{}}
trait Outer {{
    type M: Pick
    const fn via() -> u256
}}
impl<T: Holder<V = Item>> Outer for Slot<T> {{
    type M = <T as Holder>::V
    const fn via() -> u256 {{ <<T as Holder>::V as Pick>::val() }}
}}
const fn read<B: Outer>() -> u256 {{ <B::M as Pick>::val() * 1000 + B::via() }}
const fn top() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
const fn plain() -> u256 {{ read<Slot<Wrap<Item>>>() }}
"#
        ),
        &[("top", 70070), ("plain", 100100)],
    );
    values(
        &format!(
            r#"{PICK_ITEM}
trait Holder {{
    type V: Pick
    const fn hv() -> u256
}}
impl<T: Pick> Holder for Wrap<T> {{
    type V = T
    const fn hv() -> u256 {{ T::val() }}
}}
struct Slot<T> {{}}
trait Outer {{
    const fn a() -> u256
    const fn ov() -> u256
}}
impl<T: Holder<V = Item>> Outer for Slot<T> {{
    const fn a() -> u256 {{ Item::val() }}
    const fn ov() -> u256 {{ T::hv() }}
}}
const fn read<B: Outer>() -> u256 {{ B::a() * 1000 + B::ov() }}
const fn top() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

#[test]
fn concrete_component_impl_const() {
    for condition in ["Holder<V = Item>", "Holder"] {
        let value = if condition == "Holder" {
            "<T::V as Pick>::val()"
        } else {
            "Item::val()"
        };
        values(
            &format!(
                r#"{PICK_ITEM}
trait Holder {{
    type V: Pick
    const fn hv() -> u256
}}
impl<T: Pick> Holder for Wrap<T> {{
    type V = T
    const fn hv() -> u256 {{ T::val() }}
}}
struct Slot<T> {{}}
trait Outer {{
    const K: u256
    const fn ov() -> u256
}}
impl<T: {condition}> Outer for Slot<T> {{
    const K: u256 = {value}
    const fn ov() -> u256 {{ T::hv() }}
}}
const fn read<B: Outer>() -> u256 {{ B::K * 1000 + B::ov() }}
const fn top() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
"#
            ),
            &[("top", 70070)],
        );
    }
}

// A goal states `Read` or `Read<Out = u8>`, the dictionary holds `Read`: the
// implementation is the same, and one body reads one dictionary.
#[test]
fn concrete_component_two_spellings() {
    let program = |body: &str| {
        format!(
            r#"
struct Foo {{}}
struct W<T> {{}}
trait Read {{
    type Out
    const fn r() -> u256
}}
impl Read for Foo {{
    type Out = u8
    const fn r() -> u256 {{ 100 }}
}}
impl Read for Foo as RLoud {{
    type Out = u8
    const fn r() -> u256 {{ 70 }}
}}
trait Tr3 {{
    type A: Read
    const fn via() -> u256
}}
impl<T: Read> Tr3 for W<T> {{
    type A = T
    const fn via() -> u256 {{ T::r() }}
}}
const fn need8<X: Read<Out = u8>>() -> u256 {{ X::r() }}
const fn need<X: Read>() -> u256 {{ X::r() }}
{body}
"#
        )
    };
    values(
        &program(
            r#"
const fn strong<B: Tr3<A = Foo>>() -> u256 { need8<B::A>() * 1000 + B::via() }
const fn weak<B: Tr3<A = Foo>>() -> u256 { need<B::A>() * 1000 + B::via() }
const fn top_strong() -> u256 { with (RLoud) { strong<W<Foo>>() } }
const fn top_weak() -> u256 { with (RLoud) { weak<W<Foo>>() } }
"#,
        ),
        &[("top_strong", 70070), ("top_weak", 70070)],
    );
    values(
        &program(
            r#"
const fn both<B: Tr3<A = Foo>>() -> u256 {
    need<B::A>() * 1000000 + need8<B::A>() * 1000 + B::via()
}
const fn top() -> u256 { with (RLoud) { both<W<Foo>>() } }
"#,
        ),
        &[("top", 70070070)],
    );
}

// Two components of one identity reached through different dictionaries of one
// body must be one dictionary.
fn diamond(body: &str) -> String {
    format!(
        r#"{PICK_ITEM}
struct Fixed {{}}
trait Holder {{
    type V: Pick
    const fn hv() -> u256
}}
impl<T: Pick> Holder for Wrap<T> {{
    type V = T
    const fn hv() -> u256 {{ T::val() }}
}}
impl Holder for Fixed {{
    type V = Item
    const fn hv() -> u256 {{ Item::val() }}
}}
struct Pair<T, U> {{}}
trait Two {{
    type M: Holder<V = Item>
    type N: Holder<V = Item>
}}
impl<T: Holder<V = Item>, U: Holder<V = Item>> Two for Pair<T, U> {{
    type M = T
    type N = U
}}
const fn read<B: Two>() -> u256 {{
    Item::val() * 1000000 + <B::M as Holder>::hv() * 1000 + <B::N as Holder>::hv()
}}
{body}
"#
    )
}

#[test]
fn concrete_component_diamond_mixed() {
    refused(
        &diamond("const fn mixed() -> u256 { with (Named) { read<Pair<Wrap<Item>, Fixed>>() } }"),
        "mixed",
        "Item: Pick",
    );
}

#[test]
fn concrete_component_diamond_agree() {
    values(
        &diamond(
            "const fn agree() -> u256 { with (Named) { read<Pair<Wrap<Item>, Wrap<Item>>>() } }",
        ),
        &[("agree", 70070070)],
    );
}

#[test]
fn concrete_component_relay() {
    values(
        &format!(
            r#"{SHOWS}
const fn caller<B: Tr<A = Foo>>() -> u256 {{ one<B::A>() * 1000 + B::via() }}
const fn outer<B: Tr<A = Foo>>() -> u256 {{ caller<B>() }}
const fn top() -> u256 {{ with (Loud) {{ outer<W<Foo>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

// A choice at the call that reads a generic goal keeps the received
// dictionary of the body's own bound (decision 2): unchanged.
#[test]
fn concrete_component_decision_two() {
    values(
        &format!(
            r#"{SHOWS}
const fn read<B: Tr>() -> u256 {{ <B::A as Show>::show() * 1000 + B::via() }}
const fn caller<B: Tr<A = Foo>>() -> u256 {{ with (Loud) {{ read<B>() }} }}
const fn top() -> u256 {{ caller<W<Foo>>() }}
"#
        ),
        &[("top", 100100)],
    );
}

#[test]
fn concrete_component_plain() {
    values(
        &format!(
            r#"{SHOWS}
const fn caller<B: Tr<A = Foo>>() -> u256 {{
    one<B::A>() * 1000 + Foo::show() + B::via() - 100
}}
const fn top() -> u256 {{ caller<W<Foo>>() }}
"#
        ),
        &[("top", 100100)],
    );
}

// A named implementation with a generic condition, opened in a function whose
// bound pins the condition's associated type, is accepted (as with an unpinned
// bound).
#[test]
fn concrete_component_pinned_caller_named_condition() {
    values(
        &format!(
            r#"{PICK_ITEM}
trait Holder {{ type V: Pick }}
impl<T: Pick> Holder for Wrap<T> {{ type V = T }}
struct Slot<T> {{}}
trait Outer {{ const fn ov() -> u256 }}
impl<T: Holder> Outer for Slot<T> {{ const fn ov() -> u256 {{ 1 }} }}
impl<T: Holder> Outer for Slot<T> as NamedOuter {{
    const fn ov() -> u256 {{ <T::V as Pick>::val() }}
}}
const fn read<B: Outer>() -> u256 {{ B::ov() }}
const fn relay<X: Holder<V = Item>>() -> u256 {{ with (NamedOuter) {{ read<Slot<X>>() }} }}
const fn relay_generic<X: Holder>() -> u256 {{ with (NamedOuter) {{ read<Slot<X>>() }} }}
const fn top() -> u256 {{ relay<Wrap<Item>>() }}
const fn top_generic() -> u256 {{ relay_generic<Wrap<Item>>() }}
"#
        ),
        &[("top", 100), ("top_generic", 100)],
    );
}

// A condition of a selected implementation that states `mut?` is read at the
// mode of the use. The component of the received dictionary answers it at
// `mut` as at `ref`: the write reaches the field the chosen implementation names.
#[test]
fn concrete_component_mut_condition() {
    let program = |field: &str| {
        format!(
            r#"
struct Item {{ a: u256, b: u256 }}
struct Box<T> {{ v: T }}
trait Pick {{ const fn slot(mut? self) -> mut? u256 }}
impl Pick for Item {{ const fn slot(mut? self) -> mut? u256 {{ mut? self.a }} }}
impl Pick for Item as Alt {{ const fn slot(mut? self) -> mut? u256 {{ mut? self.b }} }}
trait Wrap {{ const fn wrap(mut? self) -> mut? u256 }}
impl<T: mut? Pick> Wrap for Box<T> {{
    const fn wrap(mut? self) -> mut? u256 {{ mut? self.v.slot() }}
}}
struct P<T> {{}}
trait Tr {{ type A: Pick }}
impl<T: Pick> Tr for P<T> {{ type A = T }}
const fn caller<B: Tr<A = Item>>() -> u256 {{
    var bx = Box<Item> {{ v: Item {{ a: 1, b: 2 }} }}
    bx.wrap() = 9
    bx.v.{field}
}}
const fn chosen() -> u256 {{ with (Alt) {{ caller<P<Item>>() }} }}
const fn ordinary() -> u256 {{ caller<P<Item>>() }}
"#
        )
    };
    values(&program("b"), &[("chosen", 9), ("ordinary", 2)]);
    values(&program("a"), &[("chosen", 1), ("ordinary", 9)]);
}

// A closure that adds an effect bound after the signature's bounds reads the
// component of its parent's dictionary.
#[test]
fn concrete_component_closure_with_effect() {
    let source = format!(
        r#"{SHOWS}
trait Store {{ fn get(self) -> u256 }}
struct S {{}}
impl Store for S {{ fn get(self) -> u256 {{ 0 }} }}
fn caller<B: Tr<A = Foo>>() -> u256 uses (s: Store) {{
    let c = || s.get() + one<B::A>()
    c() * 1000 + B::via()
}}
fn top() -> u256 {{ with (Loud, Store = S {{}}) {{ caller<W<Foo>>() }} }}
"#
    )
    .replace("const fn", "fn");
    assert_eq!(reached_shows(&source, "top"), vec![true]);
}

// A trait constant's default body reads the component of the dictionary of the
// supertrait bound that implementations fix where they are declared.
#[test]
fn concrete_component_trait_constant_default() {
    values(
        &format!(
            r#"{SHOWS}
trait C: Tr<A = Foo> {{ const K: u256 = Foo::show() }}
impl C for W<Foo> {{}}
const fn read<X: C>() -> u256 {{ X::K }}
const fn top() -> u256 {{ with (Loud) {{ read<W<Foo>>() }} }}
const fn plain() -> u256 {{ read<W<Foo>>() }}
"#
        ),
        &[("top", 100), ("plain", 100)],
    );
}

// An inherent constant reads the component of the dictionary its
// implementation's bound received.
#[test]
fn concrete_component_inherent_constant() {
    values(
        &format!(
            r#"{SHOWS}
struct Wrapper<B> {{}}
impl<B: Tr<A = Foo>> Wrapper<B> {{ const X: u256 = Foo::show() }}
const fn caller<B: Tr<A = Foo>>() -> u256 {{ Wrapper<B>::X }}
const fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
const fn plain() -> u256 {{ caller<W<Foo>>() }}
"#
        ),
        &[("top", 70), ("plain", 100)],
    );
}

// Programs in which `Show` has an associated output that both implementations
// give equally: the output agreement walk sees the resolved proofs, so it names
// no disagreement.
#[test]
fn concrete_component_equal_outputs() {
    let shows = SHOWS
        .replace(
            "impl Show for Foo { const fn",
            "impl Show for Foo { type Out = u8\n    const fn",
        )
        .replace(
            "impl Show for Foo as Loud { const fn",
            "impl Show for Foo as Loud { type Out = u8\n    const fn",
        )
        .replace(
            "trait Show { const fn",
            "trait Show { type Out\n    const fn",
        );
    values(
        &format!(
            r#"{shows}
const fn caller<B: Tr<A = Foo>>() -> u256 {{ one<B::A>() * 1000 + B::via() }}
const fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

// An associated constant that an implementation reads from a component, read
// through a default method of its trait.
#[test]
fn concrete_component_constant_through_a_default() {
    values(
        &format!(
            r#"{PICK_ITEM}
trait Holder {{
    type V: Pick
    const fn hv() -> u256
}}
impl<T: Pick> Holder for Wrap<T> {{
    type V = T
    const fn hv() -> u256 {{ T::val() }}
}}
struct Slot<T> {{}}
trait Outer {{
    const K: u256
    const fn ov() -> u256
    const fn dflt() -> u256 {{ Self::K }}
}}
impl<T: Holder<V = Item>> Outer for Slot<T> {{
    const K: u256 = Item::val()
    const fn ov() -> u256 {{ T::hv() }}
}}
const fn read<B: Outer>() -> u256 {{ B::dflt() * 1000 + B::ov() }}
const fn top() -> u256 {{ with (Named) {{ read<Slot<Wrap<Item>>>() }} }}
"#
        ),
        &[("top", 70070)],
    );
}

// A component one associated-type level below the first concrete bound is read
// from the dictionary as well: `Item: Pick` below `Wrap<Item>: Middle<Inner =
// Item>`, which `Slot`'s condition holds.
#[test]
fn concrete_component_depth() {
    values(
        &format!(
            r#"{PICK_ITEM}
trait Middle {{
    type Inner: Pick
    const fn via() -> u256
}}
impl<T: Pick> Middle for Wrap<T> {{
    type Inner = T
    const fn via() -> u256 {{ T::val() }}
}}
trait Holder {{ type V: Middle<Inner = Item> }}
struct H<T> {{}}
impl<T: Middle<Inner = Item>> Holder for H<T> {{ type V = T }}
struct Slot<T> {{}}
trait Outer {{
    type M: Middle<Inner = Item>
    type P: Pick
}}
impl<T: Holder<V = Wrap<Item>>> Outer for Slot<T> {{
    type M = Wrap<Item>
    type P = Item
}}
const fn read<B: Outer>() -> u256 {{
    <B::P as Pick>::val() * 1000 + <B::M as Middle>::via()
}}
const fn top() -> u256 {{ with (Named) {{ read<Slot<H<Wrap<Item>>>>() }} }}
const fn plain() -> u256 {{ read<Slot<H<Wrap<Item>>>>() }}
"#
        ),
        &[("top", 70070), ("plain", 100100)],
    );
}

// Five associated-type levels below the first concrete bound are past the
// limit of four. A program that holds a named implementation in that
// dictionary is refused and never given a value that mixes it with an ordinary
// proof; the same program without one keeps its ordinary value.
#[test]
fn concrete_component_depth_limit() {
    let program = r#"
trait Show { const fn show() -> u256 }
struct Foo {}
impl Show for Foo { const fn show() -> u256 { 100 } }
impl Show for Foo as Loud { const fn show() -> u256 { 70 } }
struct C0<T> {}
struct C1<T> {}
struct C2<T> {}
struct C3<T> {}
struct C4<T> {}
struct C5<T> {}
trait H0 { type V: H1<V = C2<C3<C4<C5<Foo>>>>> }
trait H1 { type V: H2<V = C3<C4<C5<Foo>>>> }
trait H2 { type V: H3<V = C4<C5<Foo>>> }
trait H3 { type V: H4<V = C5<Foo>> }
trait H4 { type V: H5<V = Foo> }
trait H5 { type V: Show }
impl<T: H1<V = C2<C3<C4<C5<Foo>>>>>> H0 for C0<T> { type V = T }
impl<T: H2<V = C3<C4<C5<Foo>>>>> H1 for C1<T> { type V = T }
impl<T: H3<V = C4<C5<Foo>>>> H2 for C2<T> { type V = T }
impl<T: H4<V = C5<Foo>>> H3 for C3<T> { type V = T }
impl<T: H5<V = Foo>> H4 for C4<T> { type V = T }
impl<T: Show> H5 for C5<T> { type V = T }
const fn read<B: H0<V = C1<C2<C3<C4<C5<Foo>>>>>>>() -> u256 { Foo::show() }
const fn plain() -> u256 { read<C0<C1<C2<C3<C4<C5<Foo>>>>>>>() }
const fn chosen() -> u256 { with (Loud) { read<C0<C1<C2<C3<C4<C5<Foo>>>>>>>() } }
"#;
    values(program, &[("plain", 100)]);
    refused(program, "chosen", "C5<Foo>: H5<V = Foo>");
}

// Two pinned roots, each with a chain five associated-type levels deep. The
// chain of `A` (over `Bar`) holds no named implementation; the chain of `B`
// (over `Foo`) holds `Loud` at its bottom. Both chains are cut at the limit,
// and the refusal does not depend on which root comes first: the root whose
// dictionary holds the named implementation is the one refused for it. A value
// that reads `Foo: Show` from the ordinary proof and `C5<Foo>::via` from the
// chosen dictionary is never given.
#[test]
fn concrete_component_depth_two_frontiers() {
    let program = |bounds: &str, arguments: &str| {
        r#"
trait Show { const fn show() -> u256 }
struct Foo {}
struct Bar {}
impl Show for Foo { const fn show() -> u256 { 100 } }
impl Show for Foo as Loud { const fn show() -> u256 { 70 } }
impl Show for Bar { const fn show() -> u256 { 5 } }
struct C0<T> {}
struct C1<T> {}
struct C2<T> {}
struct C3<T> {}
struct C4<T> {}
struct C5<T> {}
trait H0 { type V: H1<V = C2<C3<C4<C5<Foo>>>>> }
trait H1 { type V: H2<V = C3<C4<C5<Foo>>>> }
trait H2 { type V: H3<V = C4<C5<Foo>>> }
trait H3 { type V: H4<V = C5<Foo>> }
trait H4 { type V: H5<V = Foo> }
trait H5 {
    type V: Show
    const fn via() -> u256
}
impl<T: H1<V = C2<C3<C4<C5<Foo>>>>>> H0 for C0<T> { type V = T }
impl<T: H2<V = C3<C4<C5<Foo>>>>> H1 for C1<T> { type V = T }
impl<T: H3<V = C4<C5<Foo>>>> H2 for C2<T> { type V = T }
impl<T: H4<V = C5<Foo>>> H3 for C3<T> { type V = T }
impl<T: H5<V = Foo>> H4 for C4<T> { type V = T }
impl<T: Show> H5 for C5<T> {
    type V = T
    const fn via() -> u256 { T::show() }
}
struct D0<T> {}
struct D1<T> {}
struct D2<T> {}
struct D3<T> {}
struct D4<T> {}
struct D5<T> {}
trait G0 { type V: G1<V = D2<D3<D4<D5<Bar>>>>> }
trait G1 { type V: G2<V = D3<D4<D5<Bar>>>> }
trait G2 { type V: G3<V = D4<D5<Bar>>> }
trait G3 { type V: G4<V = D5<Bar>> }
trait G4 { type V: G5<V = Bar> }
trait G5 { type V: Show }
impl<T: G1<V = D2<D3<D4<D5<Bar>>>>>> G0 for D0<T> { type V = T }
impl<T: G2<V = D3<D4<D5<Bar>>>>> G1 for D1<T> { type V = T }
impl<T: G3<V = D4<D5<Bar>>>> G2 for D2<T> { type V = T }
impl<T: G4<V = D5<Bar>>> G3 for D3<T> { type V = T }
impl<T: G5<V = Bar>> G4 for D4<T> { type V = T }
impl<T: Show> G5 for D5<T> { type V = T }
const fn read<@BOUNDS@>() -> u256 { Foo::show() * 1000 + <C5<Foo> as H5>::via() }
const fn plain() -> u256 { read<@ARGUMENTS@>() }
const fn chosen() -> u256 { with (Loud) { read<@ARGUMENTS@>() } }
"#
        .replace("@BOUNDS@", bounds)
        .replace("@ARGUMENTS@", arguments)
    };
    let a = "A: G0<V = D1<D2<D3<D4<D5<Bar>>>>>>";
    let b = "B: H0<V = C1<C2<C3<C4<C5<Foo>>>>>>";
    let (d, c) = ("D0<D1<D2<D3<D4<D5<Bar>>>>>>", "C0<C1<C2<C3<C4<C5<Foo>>>>>>");
    for (bounds, arguments) in [
        (format!("{a}, {b}"), format!("{d}, {c}")),
        (format!("{b}, {a}"), format!("{c}, {d}")),
    ] {
        let program = program(&bounds, &arguments);
        values(&program, &[("plain", 100100)]);
        refused(&program, "chosen", "C5<Foo>: H5<V = Foo>");
    }
}

// One identity written two ways, `Foo: Read` and `Foo: Read<Out = u8>`, is one
// component however it is spelled. The call that holds both under an unrelated
// choice reads the one dictionary each condition supplies; it is not refused
// for having two references to it.
#[test]
fn concrete_component_identity_held_two_spellings() {
    let program = |unrelated: bool| {
        let call = if unrelated {
            "with (Aside) { h<Pair<B, C>>() }"
        } else {
            "h<Pair<B, C>>()"
        };
        format!(
            r#"
trait Read {{
    type Out
    const fn r() -> u256
}}
struct Foo {{}}
impl Read for Foo {{
    type Out = u8
    const fn r() -> u256 {{ 100 }}
}}
impl Read for Foo as Loud {{
    type Out = u8
    const fn r() -> u256 {{ 70 }}
}}
struct W<T> {{}}
struct V<T> {{}}
trait Tr {{ type A: Read }}
trait Tr2 {{ type A: Read<Out = u8> }}
impl<T: Read> Tr for W<T> {{ type A = T }}
impl<T: Read<Out = u8>> Tr2 for V<T> {{ type A = T }}
struct Pair<T, U> {{}}
trait Both {{ type X: Read }}
impl<T: Tr<A = Foo>, U: Tr2<A = Foo>> Both for Pair<T, U> {{ type X = Foo }}
trait Other {{ const fn o() -> u256 }}
impl Other for Foo {{ const fn o() -> u256 {{ 1 }} }}
impl Other for Foo as Aside {{ const fn o() -> u256 {{ 2 }} }}
const fn h<P: Both>() -> u256 {{ <P::X as Read>::r() }}
const fn g<B: Tr<A = Foo>, C: Tr2<A = Foo>>() -> u256 {{ {call} }}
const fn top() -> u256 {{ with (Loud) {{ g<W<Foo>, V<Foo>>() }} }}
const fn top_plain() -> u256 {{ g<W<Foo>, V<Foo>>() }}
"#
        )
    };
    values(&program(true), &[("top", 70), ("top_plain", 100)]);
    values(&program(false), &[("top", 70), ("top_plain", 100)]);
}

// Two conditions of one implementation that hold the component `Item: Pick`
// from different dictionaries are refused where the call is made, and the
// refusal names the two conditions rather than reporting that the goal is not
// implemented.
#[test]
fn concrete_component_conditions_that_disagree_are_named() {
    let source = r#"
struct Item {}
trait Pick { fn val() -> u256 }
impl Pick for Item { fn val() -> u256 { 100 } }
impl Pick for Item as Named { fn val() -> u256 { 70 } }
trait Holder {
    type V: Pick
    fn hv() -> u256
}
struct Wrap<T> {}
impl<T: Pick> Holder for Wrap<T> {
    type V = T
    fn hv() -> u256 { T::val() }
}
struct Fixed {}
impl Holder for Fixed {
    type V = Item
    fn hv() -> u256 { Item::val() }
}
trait Outer {
    type M: Pick
    fn ov() -> u256
}
struct Pair<T, U> {}
impl<T: Holder<V = Item>, U: Holder<V = Item>> Outer for Pair<T, U> {
    type M = Item
    fn ov() -> u256 { T::hv() * 1000 + U::hv() }
}
fn read<B: Outer>() -> u256 { <B::M as Pick>::val() * 1000000 + B::ov() }
fn chosen() -> u256 { with (Named) { read<Pair<Wrap<Item>, Fixed>>() } }
"#;
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("conditions_disagree.fe".into(), source);
    let (module, _) = db.top_mod(file);
    let report = format_diagnostics(&db, &db.run_on_top_mod(module));
    assert!(report.contains("error[6-0003]"), "{report}");
    assert!(
        report.contains(
            "receives `Named` through `T: Holder<V = Item>` (the bound on `V`) and `impl Pick for Item` through `U: Holder<V = Item>` (the bound on `V`)"
        ),
        "{report}"
    );
    assert!(!report.contains("doesn't implement"), "{report}");
}

// A closure that escapes a `with` block of an unrelated choice and is called
// outside it keeps the dictionary of the instance that defined it.
#[test]
fn concrete_component_escaping_closure() {
    let source = format!(
        r#"{SHOWS}
fn caller<B: Tr<A = Foo>>() -> u256 {{
    let c = with (Aside) {{ || one<B::A>() }}
    c() * 1000 + B::via()
}}
fn top() -> u256 {{ with (Loud) {{ caller<W<Foo>>() }} }}
"#
    )
    .replace("const fn", "fn");
    assert_eq!(reached_shows(&source, "top"), vec![true]);
}

#[test]
fn with_naming_fewer_arguments_than_the_implementation_takes_reports_once() {
    let source = |argument: &str| {
        format!(
            r#"
struct Pair<T, U> {{ a: T, b: U }}
trait Pick {{ fn pick(self) -> u256 }}
impl<T, U> Pick for Pair<T, U> as Named {{ fn pick(self) -> u256 {{ 7 }} }}
fn run() -> u256 {{
    let p = Pair<u8, u16> {{ a: 1, b: 2 }}
    with (Named{argument}) {{ p.pick() }}
}}
"#
        )
    };
    // Naming every parameter, or none, is accepted.
    assert_eq!(diagnostics(&source("<u8, u16>")), Vec::<String>::new());
    assert_eq!(diagnostics(&source("")), Vec::<String>::new());
    // One of two is a count error alone: no "too many" report, and no
    // follow-on report that the method is missing.
    let codes = diagnostics(&source("<u8>"));
    assert_eq!(codes.len(), 1, "{codes:?}");
    assert!(!codes.contains(&"8-0022".to_string()), "{codes:?}");
    assert!(!codes.contains(&"2-0010".to_string()), "{codes:?}");
    assert_eq!(codes, vec!["8-0163"]);
}

const LEN_PICKS: &str = r#"
trait Len { const fn len() -> usize }
struct Item {}
impl Len for Item { const fn len() -> usize { 3 } }
impl Len for Item as Big { const fn len() -> usize { 4 } }
struct H<const N: usize, const M: usize = { Item::len() }> { v: u256 }
struct Only<const N: usize> { v: u256 }
"#;

#[test]
fn record_literals_refuse_defaults_that_read_a_choice() {
    for literal in ["H<1> { v: 1 }", "H<{ 1 }> { v: 1 }"] {
        let source = format!("{LEN_PICKS}\nfn f() {{ with (Big) {{ let _h = {literal} }} }}\n");
        let codes = diagnostics(&source);
        assert!(
            codes.contains(&"8-0138".to_string()),
            "{literal}: {codes:?}"
        );
        // The annotation of the same type is refused the same way, and
        // neither is refused outside the choice.
        let annotated = format!("{LEN_PICKS}\nfn f() {{ with (Big) {{ let _h: H<1> }} }}\n");
        assert!(diagnostics(&annotated).contains(&"8-0138".to_string()));
        let ordinary = format!("{LEN_PICKS}\nfn f() {{ let _h = {literal} }}\n");
        assert_eq!(diagnostics(&ordinary), Vec::<String>::new(), "{literal}");
    }
    // A default that is written out is not read from the choice.
    let written =
        format!("{LEN_PICKS}\nfn f() {{ with (Big) {{ let _h = H<1, 5> {{ v: 1 }} }} }}\n");
    assert_eq!(diagnostics(&written), Vec::<String>::new());
}

#[test]
fn provider_expressions_refuse_reads_of_the_choices_before_them() {
    let source =
        |operands: &str| format!("{LEN_PICKS}\nfn p() -> u256 {{ with ({operands}) {{ 1 }} }}\n");
    let after = diagnostics(&source("Big, Only<{ Item::len() }> { v: 1 }"));
    assert!(after.contains(&"8-0138".to_string()), "{after:?}");
    // The same type written in the body of the choice is refused.
    let body = format!(
        "{LEN_PICKS}\nfn p() -> u256 {{ with (Big) {{ let _h: Only<{{ Item::len() }}> = Only<{{ Item::len() }}> {{ v: 1 }}\n1 }} }}\n"
    );
    assert!(diagnostics(&body).contains(&"8-0138".to_string()));
    // A provider before the choice does not see it.
    assert_eq!(
        diagnostics(&source("Only<{ Item::len() }> { v: 1 }, Big")),
        Vec::<String>::new()
    );
    assert_eq!(
        diagnostics(&source("Only<{ Item::len() }> { v: 1 }")),
        Vec::<String>::new()
    );
}

#[test]
fn overlap_through_a_projection_that_normalizes_to_an_application() {
    let declarations = r#"
struct Wrap<T> {}
struct Applied<X> {}
struct Z {}
trait Inner { type B }
impl Inner for Z { type B = u8 }
trait Source { type Out }
impl Source for Z { type Out = Applied<<Z as Inner>::B> }
trait Pick<A, B> { const fn n() -> usize }
impl<T: Source> Pick<T::Out, T> for Wrap<T> as Projected { const fn n() -> usize { 1 } }
"#;
    // The projection normalizes to an application whose argument is a
    // projection of its own, and the other header's argument decides it.
    for (other, expected) in [
        ("Applied<u8>", vec!["8-0160"]),
        ("Applied<u16>", vec![]),
        ("Applied<<Z as Inner>::B>", vec!["8-0160"]),
    ] {
        for openings in ["Projected, Other", "Other, Projected"] {
            let source = format!(
                "{declarations}impl<T> Pick<{other}, Z> for Wrap<T> as Other {{ const fn n() -> usize {{ 2 }} }}\nfn run() {{ with ({openings}) {{}} }}"
            );
            assert_eq!(diagnostics(&source), expected, "{openings}: {other}");
        }
    }
}

#[test]
fn selected_headers_with_projections_keep_the_goals_coordinates() {
    values(
        r#"
struct Item {}
struct Other {}
struct Wrap<T> {}
struct Pair<A, B> {}
trait Tr { type Out }
impl Tr for Item { type Out = u8 }
impl Tr for Other { type Out = u8 }
trait One<X> { const fn v() -> u256 }
impl<T: Tr> One<T::Out> for Wrap<T> { const fn v() -> u256 { 1 } }
impl<T: Tr> One<T::Out> for Wrap<T> as Alt { const fn v() -> u256 { 1 } }
trait Two<X, Y> { const fn v() -> u256 }
impl<A: Tr, B: Tr> Two<A::Out, B::Out> for Pair<A, B> { const fn v() -> u256 { 2 } }
impl<A: Tr, B: Tr> Two<A::Out, B::Out> for Pair<A, B> as Alt2 { const fn v() -> u256 { 2 } }
const fn one<T: Tr>() -> u256 { with (Alt) { <Wrap<T> as One<T::Out>>::v() } }
const fn two<A: Tr, B: Tr>() -> u256 { with (Alt2) { <Pair<A, B> as Two<A::Out, B::Out>>::v() } }
const fn run_one() -> u256 { one<Item>() }
const fn run_two() -> u256 { two<Item, Item>() * 10 + two<Item, Other>() }
"#,
        &[("run_one", 1), ("run_two", 22)],
    );
}

#[test]
fn the_compiler_provided_codec_has_no_named_implementation_to_choose() {
    // The deferred proof of `SolLanes: LaneCodec<T>` for a generic element is
    // answered by the compiler. Both the trait and the type are external to
    // any package, so no named implementation of the goal can be declared and
    // no choice can answer it.
    let source = r#"
use std::evm::SolLanes
use core::ops::LaneCodec
impl<T> LaneCodec<T> for SolLanes as Mine {
    const BITS: u256 = 8
    fn get(_ word: u256, offset: u256) -> T { <SolLanes as LaneCodec<T>>::get(word, offset) }
    fn set(_ word: u256, offset: u256, value: own T) -> u256 { word }
}
"#;
    let codes = diagnostics(source);
    assert!(codes.contains(&"5-0000".to_string()), "{codes:?}");
}

#[test]
fn selected_conditions_reach_every_call_form_of_a_method() {
    values(
        r#"
trait Pick { const fn value() -> u256 }
struct Item {}
impl Pick for Item { const fn value() -> u256 { 1 } }
impl Pick for Item as Alt { const fn value() -> u256 { 2 } }
struct Boxed<T> { x: T }
trait Wrap { const fn go(self) -> u256 }
impl<T: Pick> Wrap for Boxed<T> { const fn go(self) -> u256 { T::value() } }
const fn via<U: Wrap>(_ u: U) -> u256 { u.go() }
const fn method() -> u256 {
    let b = Boxed<Item> { x: Item {} }
    with (Alt) { b.go() }
}
const fn path() -> u256 {
    let b = Boxed<Item> { x: Item {} }
    with (Alt) { Wrap::go(b) }
}
const fn qualified() -> u256 {
    let b = Boxed<Item> { x: Item {} }
    with (Alt) { <Boxed<Item> as Wrap>::go(b) }
}
const fn bound() -> u256 {
    let b = Boxed<Item> { x: Item {} }
    with (Alt) { via(b) }
}
"#,
        &[("method", 2), ("path", 2), ("qualified", 2), ("bound", 2)],
    );
}

// The agreement rule: every route by which a body receives or derives the
// proof of one goal supplies one implementation. A goal is identified in the
// coordinates of the body's declaration, so two bounds that the signature
// does not identify are separate goals even when an instantiation makes them
// equal, and a dictionary received from outside keeps its own conditions. The
// tests below pin programs that the single check must keep accepting, with the
// values they have.
const AGREE: &str = r#"
trait Show { const fn show() -> u256 }
struct Foo {}
impl Show for Foo { const fn show() -> u256 { 100 } }
impl Show for Foo as Loud { const fn show() -> u256 { 70 } }
impl Show for Foo as Quiet { const fn show() -> u256 { 30 } }
struct W<T> {}
struct V<T> {}
struct U<T> {}
trait Tr { type A: Show
    const fn via() -> u256 }
impl<T: Show> Tr for W<T> { type A = T
    const fn via() -> u256 { T::show() } }
impl<T: Show> Tr for U<T> { type A = T
    const fn via() -> u256 { T::show() + 1 } }
trait Tr2 { type A: Tr
    const fn via2() -> u256 }
impl<T: Tr> Tr2 for V<T> { type A = T
    const fn via2() -> u256 { T::via() } }
"#;

// Two separate bounds `B: Tr` and `C: Tr` are two goals: the bound on `B` is
// built under the choice and the bound on `C` arrives from outside.
#[test]
fn agreement_unidentified_bounds_may_differ() {
    values(
        &format!(
            r#"{AGREE}
const fn g<B: Tr, C: Tr>() -> u256 {{ B::via() * 1000 + C::via() }}
const fn relay<C: Tr>() -> u256 {{ with (Loud) {{ g<W<Foo>, C>() }} }}
const fn disagree() -> u256 {{ relay<U<Foo>>() }}
"#
        ),
        &[("disagree", 70101)],
    );
}

// Two type parameters instantiated to one type are two goals.
#[test]
fn agreement_two_parameters_one_type_may_differ() {
    values(
        r#"
trait Show { const fn show() -> u256 }
struct Foo {}
impl Show for Foo { const fn show() -> u256 { 100 } }
impl Show for Foo as Loud { const fn show() -> u256 { 70 } }
const fn both<X: Show, Y: Show>() -> u256 { X::show() * 1000 + Y::show() }
const fn relay<X: Show>() -> u256 { with (Loud) { both<X, Foo>() } }
const fn top() -> u256 { relay<Foo>() }
"#,
        &[("top", 100070)],
    );
}

// A dictionary made outside a block keeps the dictionaries of its own
// conditions: `R: Root` holds the ordinary `Item: Pick`, and the bound `T: Pick`
// is answered inside the block. The conditions of the received dictionary are
// no goal of the callee, so the program is accepted with 12 (reading A of the
// agreement rule; the instance-wide reading is a language change that has not
// been chosen).
#[test]
fn agreement_received_condition_keeps_its_dictionary() {
    let program = |signature: &str, call: &str| {
        format!(
            r#"
struct Item {{}}
struct Fixed {{}}
trait Pick {{ const fn value() -> u256 }}
impl Pick for Item {{ const fn value() -> u256 {{ 1 }} }}
impl<T> Pick for T as Alt {{ const fn value() -> u256 {{ 2 }} }}
trait Root {{ const fn root() -> u256 }}
impl Root for Fixed where Item: Pick {{ const fn root() -> u256 {{ Item::value() }} }}
{signature}
const fn top() -> u256 {{ {call} }}
"#
        )
    };
    values(
        &program(
            "const fn target<R: Root, T: Pick>() -> u256 { R::root() * 10 + T::value() }
const fn relay<R: Root, T: Pick>() -> u256 { with (Alt) { target<R, T>() } }",
            "relay<Fixed, Item>()",
        ),
        &[("top", 12)],
    );
    values(
        &program(
            "const fn target<T: Pick, R: Root>() -> u256 { T::value() + R::root() * 10 }
const fn relay<T: Pick, R: Root>() -> u256 { with (Alt) { target<T, R>() } }",
            "relay<Item, Fixed>()",
        ),
        &[("top", 12)],
    );
}

// The bound that `H: Holder<Out = Item>` implies on `Item` and the separate
// bound `T: Pick` are two goals of the callee, so both orders are accepted
// with 12. The bound `Item: Pick` implied by the equality is fixed where
// `impl Holder for Fixed` is declared.
#[test]
fn agreement_implied_bound_and_separate_parameter_may_differ() {
    let program = |signature: &str| {
        format!(
            r#"
struct Item {{}}
struct Fixed {{}}
trait Pick {{ const fn value() -> u256 }}
impl Pick for Item {{ const fn value() -> u256 {{ 1 }} }}
impl Pick for Item as Alt {{ const fn value() -> u256 {{ 2 }} }}
trait Holder {{ type Out: Pick }}
impl Holder for Fixed {{ type Out = Item }}
{signature}
"#
        )
    };
    values(
        &program(
            "const fn target<H: Holder<Out = Item>, T: Pick>() -> u256 { <H::Out as Pick>::value() * 10 + T::value() }
const fn go() -> u256 { with (Alt) { target<Fixed, Item>() } }",
        ),
        &[("go", 12)],
    );
    values(
        &program(
            "const fn target<T: Pick, H: Holder<Out = Item>>() -> u256 { T::value() + <H::Out as Pick>::value() * 10 }
const fn go() -> u256 { with (Alt) { target<Item, Fixed>() } }",
        ),
        &[("go", 12)],
    );
}

// When the signature identifies the two bounds (`H: Holder<Out = T>`), they
// are one goal and the differing dictionaries are refused at the call.
#[test]
fn agreement_identified_bound_refused() {
    let codes = diagnostics(
        r#"
struct Item {}
struct Fixed {}
trait Pick { fn value() -> u256 }
impl Pick for Item { fn value() -> u256 { 1 } }
impl Pick for Item as Alt { fn value() -> u256 { 2 } }
trait Holder { type Out: Pick }
impl Holder for Fixed { type Out = Item }
fn target<H: Holder<Out = T>, T: Pick>() -> u256 {
    <H::Out as Pick>::value() * 10 + T::value()
}
fn go() -> u256 { with (Alt) { target<Fixed, Item>() } }
"#,
    );
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
}

// Ordinary implementations only: a pinned bound whose component has its own
// conditions, reached by two roots and through an implementation's conditions.
#[test]
fn agreement_ordinary_pinned_conditions() {
    values(
        r#"
trait Show { const fn show() -> u256 }
trait Sub: Show {}
struct Foo {}
impl Show for Foo { const fn show() -> u256 { 1 } }
impl Sub for Foo {}
struct W<T> {}
struct V<T> {}
trait Tr { type A: Show
    const fn via() -> u256 }
impl<T: Sub> Tr for W<T> { type A = T
    const fn via() -> u256 { T::show() } }
trait Tr2 { type A: Tr
    const fn via2() -> u256 }
impl<T: Tr> Tr2 for V<T> { type A = T
    const fn via2() -> u256 { T::via() } }
const fn f<B: Tr<A = Foo>, C: Tr2<A = W<Foo>>>() -> u256 { Foo::show() * 1000 + B::via() + C::via2() }
struct Both<X, Y> {}
trait Tb { const fn get() -> u256 }
impl<X: Tr<A = Foo>, Y: Tr2<A = W<Foo>>> Tb for Both<X, Y> { const fn get() -> u256 { Foo::show() * 1000 + 1 } }
const fn f1002() -> u256 { f<W<Foo>, V<W<Foo>>>() }
const fn tb1001() -> u256 { <Both<W<Foo>, V<W<Foo>>> as Tb>::get() }
"#,
        &[("f1002", 1002), ("tb1001", 1001)],
    );
}

// Mutually implied bounds with equalities and no named implementation.
#[test]
fn agreement_mutual_bounds_without_choice() {
    values(
        r#"
trait Left { type Out: Right
    const fn l() -> u256 }
trait Right { type Back: Left
    const fn r() -> u256 }
struct A {}
struct B {}
impl Left for A { type Out = B
    const fn l() -> u256 { 1 } }
impl Right for B { type Back = A
    const fn r() -> u256 { 2 } }
const fn call<T: Left<Out = U>, U: Right<Back = T>>() -> u256 { T::l() * 10 + U::r() }
const fn v12() -> u256 { call<A, B>() }
"#,
        &[("v12", 12)],
    );
}

// A named implementation of `Tr` whose own condition is ordinary: the
// component `Foo: Show` of `B: Tr<A = Foo>` is the ordinary one unless the
// block also names `Loud`.
#[test]
fn agreement_named_component_condition() {
    values(
        &format!(
            r#"{AGREE}
impl<T: Show> Tr for W<T> as LoudTr {{ type A = T
    const fn via() -> u256 {{ T::show() + 5 }} }}
const fn caller<B: Tr<A = Foo>>() -> u256 {{ Foo::show() * 1000 + B::via() }}
const fn v100105() -> u256 {{ with (LoudTr) {{ caller<W<Foo>>() }} }}
const fn v70075() -> u256 {{ with (LoudTr, Loud) {{ caller<W<Foo>>() }} }}
"#
        ),
        &[("v100105", 100105), ("v70075", 70075)],
    );
}

// One pinned bound that gives `Foo: Show` one level down.
#[test]
fn agreement_depth_one_control() {
    values(
        &format!(
            r#"{AGREE}
const fn f<B: Tr<A = Foo>>() -> u256 {{ Foo::show() * 1000 + B::via() }}
const fn v70070() -> u256 {{ with (Loud) {{ f<W<Foo>>() }} }}
"#
        ),
        &[("v70070", 70070)],
    );
}

// A constant whose value is computed from a condition's dictionary, read as a
// value beside a type formed from the same constant in one generic body: the
// value follows the choice and the type is formed ordinarily, so the two
// differ (1002: array length 1 beside value 2). Whether a constant may see a
// choice is a separate rule that has not been decided; these results are
// pinned as they are today and do not follow from the agreement rule.
#[test]
fn agreement_constants_keep_current_values_pending_the_rule() {
    values(
        r#"
trait Foo { const fn value() -> usize }
impl Foo for u256 { const fn value() -> usize { 1 } }
impl Foo for u256 as AltFoo { const fn value() -> usize { 2 } }
struct W<T> {}
trait Tk { const K: usize }
impl<T: Foo> Tk for W<T> { const K: usize = T::value() }
const fn k<T: Tk>() -> usize { T::K }
const fn mix<T: Tk>() -> usize {
    let a: [u8; T::K] = [0; T::K]
    length_of(a) * 1000 + T::K
}
const fn h<const N: usize>() -> usize { N }
const fn length_of<const N: usize>(_ a: [u8; N]) -> usize { N }
const fn split<T: Tk>() -> usize { h<{ T::K }>() * 1000 + T::K }
struct Wrapper<T> {}
impl<T: Foo> Wrapper<T> { const X: usize = T::value() }
const fn inherent<T: Foo>() -> usize {
    let a: [u8; Wrapper<T>::X] = [0; Wrapper<T>::X]
    length_of(a) * 1000 + Wrapper<T>::X
}
const fn direct() -> usize { with (AltFoo) { <W<u256> as Tk>::K } }
const fn generic() -> usize { with (AltFoo) { k<W<u256>>() } }
const fn mixed() -> usize { with (AltFoo) { mix<W<u256>>() } }
const fn const_generic_argument() -> usize { with (AltFoo) { split<W<u256>>() } }
const fn inherent_chosen() -> usize { with (AltFoo) { inherent<u256>() } }
const fn direct_with_literal_type() -> usize {
    with (AltFoo) {
        let a: [u8; 1] = [0; 1]
        length_of(a) * 1000 + <W<u256> as Tk>::K
    }
}
"#,
        &[
            ("direct", 2),
            ("generic", 2),
            ("mixed", 1002),
            ("const_generic_argument", 1002),
            ("inherent_chosen", 1002),
            ("direct_with_literal_type", 1002),
        ],
    );
}

// A concrete bound that a declared bound implies two associated types down:
// the implementation that defines each associated type fixes the next
// (`U<Foo>::A` is `Foo`), so `Foo: Show` is a component of the dictionary of
// `C: Tr2<A = U<Foo>>`, and the body's own `Foo::show()` reads that component.
#[test]
fn agreement_component_through_implementation_type() {
    values(
        &format!(
            r#"{AGREE}
const fn f<C: Tr2<A = U<Foo>>>() -> u256 {{ Foo::show() * 1000 + C::via2() }}
const fn g<C: Tr2<A = U<Foo>>>() -> u256 {{ <<C::A as Tr>::A as Show>::show() * 1000 + C::via2() }}
const fn f_chosen() -> u256 {{ with (Loud) {{ f<V<U<Foo>>>() }} }}
const fn g_chosen() -> u256 {{ with (Loud) {{ g<V<U<Foo>>>() }} }}
const fn f_plain() -> u256 {{ f<V<U<Foo>>>() }}
"#
        ),
        &[
            ("f_chosen", 70071),
            ("g_chosen", 70071),
            ("f_plain", 100101),
        ],
    );
}

// The same shape in the conditions of an implementation.
#[test]
fn agreement_component_through_implementation_type_in_conditions() {
    values(
        &format!(
            r#"{AGREE}
struct Hold<C> {{}}
trait Get {{ const fn get() -> u256 }}
impl<C: Tr2<A = U<Foo>>> Get for Hold<C> {{ const fn get() -> u256 {{ Foo::show() * 1000 + C::via2() }} }}
const fn chosen() -> u256 {{ with (Loud) {{ <Hold<V<U<Foo>>> as Get>::get() }} }}
const fn plain() -> u256 {{ <Hold<V<U<Foo>>> as Get>::get() }}
"#
        ),
        &[("chosen", 70071), ("plain", 100101)],
    );
}

// Two bounds whose dictionaries hold `Foo: Show`, one directly (`B: Tr<A =
// Foo>`) and one two levels down (`C: Tr2<A = U<Foo>>`), are refused when one
// is built under a choice and the other is not, whichever comes first, and
// whether or not the body reads the goal.
#[test]
fn agreement_two_routes_through_implementation_type_refused() {
    let program = |signature: &str, relay: &str, body: &str| {
        format!(
            r#"{AGREE}
{signature}
{relay}
{body}
"#
        )
    };
    let read = "const fn f<B: Tr<A = Foo>, C: Tr2<A = U<Foo>>>() -> u256 { Foo::show() * 1000000 + B::via() * 1000 + C::via2() }";
    let relay = "const fn relay<C: Tr2<A = U<Foo>>>() -> u256 { with (Loud) { f<W<Foo>, C>() } }";
    let body = "const fn disagree() -> u256 { relay<V<U<Foo>>>() }";
    refused(&program(read, relay, body), "disagree", "Foo: Show");
    // The bounds of `f` swapped.
    refused(
        &program(
            "const fn f<C: Tr2<A = U<Foo>>, B: Tr<A = Foo>>() -> u256 { Foo::show() * 1000000 + B::via() * 1000 + C::via2() }",
            "const fn relay<C: Tr2<A = U<Foo>>>() -> u256 { with (Loud) { f<C, W<Foo>>() } }",
            body,
        ),
        "disagree",
        "Foo: Show",
    );
    // The body reads nothing.
    refused(
        &program(
            "const fn f<B: Tr<A = Foo>, C: Tr2<A = U<Foo>>>() -> u256 { 5 }",
            relay,
            body,
        ),
        "disagree",
        "Foo: Show",
    );
    // Both dictionaries agree when both are built under the choice.
    values(
        &program(
            read,
            relay,
            "const fn agree() -> u256 { with (Loud) { relay<V<U<Foo>>>() } }",
        ),
        &[("agree", 70070071)],
    );
}

// Both dictionaries are named and differ.
#[test]
fn agreement_two_named_routes_refused() {
    refused(
        &format!(
            r#"{AGREE}
const fn f<B: Tr<A = Foo>, C: Tr2<A = U<Foo>>>() -> u256 {{ Foo::show() * 1000000 + B::via() * 1000 + C::via2() }}
const fn relay<C: Tr2<A = U<Foo>>>() -> u256 {{ with (Loud) {{ f<W<Foo>, C>() }} }}
const fn disagree() -> u256 {{ with (Quiet) {{ relay<V<U<Foo>>>() }} }}
"#
        ),
        "disagree",
        "Foo: Show",
    );
}

// The goal written through the projection of `C` is the component, so the
// two dictionaries of `Foo: Show` are one goal of the body.
#[test]
fn agreement_written_projection_two_routes_refused() {
    refused(
        &format!(
            r#"{AGREE}
const fn f<B: Tr<A = Foo>, C: Tr2<A = U<Foo>>>() -> u256 {{ <<C::A as Tr>::A as Show>::show() * 1000000 + B::via() * 1000 + C::via2() }}
const fn relay<C: Tr2<A = U<Foo>>>() -> u256 {{ with (Loud) {{ f<W<Foo>, C>() }} }}
const fn disagree() -> u256 {{ relay<V<U<Foo>>>() }}
"#
        ),
        "disagree",
        "Foo: Show",
    );
}

// An opening of the ordinary implementation lets the choice name the goal of
// `B`, while the component of `C` stays ordinary.
#[test]
fn agreement_opening_against_component_refused() {
    refused(
        &format!(
            r#"{AGREE}
const fn f<B: Tr<A = Foo>, C: Tr2<A = W<Foo>>>() -> u256 {{ Foo::show() * 1000000 + B::via() * 1000 + C::via2() }}
const fn relay<C: Tr2<A = W<Foo>>>() -> u256 {{ with (Loud, <W<Foo> as Tr>) {{ f<W<Foo>, C>() }} }}
const fn disagree() -> u256 {{ relay<V<W<Foo>>>() }}
"#
        ),
        "disagree",
        "Foo: Show",
    );
}

const MUTUAL: &str = r#"
trait Left { type Out: Right
    const fn l() -> u256 }
trait Right { type Back: Left
    const fn r() -> u256 }
struct A {}
struct B {}
impl Left for A { type Out = B
    const fn l() -> u256 { 1 } }
impl Left for A as BigA { type Out = B
    const fn l() -> u256 { 3 } }
impl Right for B { type Back = A
    const fn r() -> u256 { 2 } }
"#;

// `T: Left<Out = U>` and the `T: Left` that `U: Right<Back = T>` implies are
// one goal of the body, reached by two routes. A choice that names it for one
// route and not the other is refused where the call is checked, here through a
// generic relay whose own bounds repeat the two routes.
#[test]
fn agreement_slots_of_one_bound_refused() {
    let codes = diagnostics(&format!(
        r#"{MUTUAL}
const fn call<T: Left<Out = U>, U: Right<Back = T>>() -> u256 {{ T::l() * 10 + U::r() }}
const fn relay<T: Left<Out = U>, U: Right<Back = T>>() -> u256 {{ call<T, U>() }}
const fn go() -> u256 {{ with (BigA) {{ relay<A, B>() }} }}
"#
    ));
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
}

// The same two routes at a call that shows both dictionaries: the use is
// refused where it is checked, in either order of the bounds, whether or not
// the body reads the second route.
#[test]
fn agreement_slots_of_one_bound_refused_at_the_use() {
    for (signature, call, body) in [
        (
            "call<T: Left<Out = U>, U: Right<Back = T>>",
            "call<A, B>()",
            "T::l() * 10 + U::r() + A::l() * 100",
        ),
        (
            "call<T: Left<Out = U>, U: Right<Back = T>>",
            "call<A, B>()",
            "T::l() * 10 + <U::Back as Left>::l()",
        ),
        (
            "call<U: Right<Back = T>, T: Left<Out = U>>",
            "call<B, A>()",
            "T::l() * 10 + <U::Back as Left>::l()",
        ),
    ] {
        let codes = diagnostics(&format!(
            r#"{MUTUAL}
const fn {signature}() -> u256 {{ {body} }}
const fn go() -> u256 {{ with (BigA) {{ {call} }} }}
"#
        ));
        assert!(codes.contains(&"6-0003".to_string()), "{call}: {codes:?}");
    }
}

#[test]
fn agreement_slots_of_one_bound_plain() {
    values(
        &format!(
            r#"{MUTUAL}
const fn d04<T: Left<Out = U>, U: Right<Back = T>>() -> u256 {{ T::l() * 10 + U::r() + A::l() * 100 }}
const fn q01<T: Left<Out = U>, U: Right<Back = T>>() -> u256 {{ T::l() * 10 + <U::Back as Left>::l() }}
const fn plain_d04() -> u256 {{ d04<A, B>() }}
const fn plain_q01() -> u256 {{ q01<A, B>() }}
"#
        ),
        &[("plain_d04", 112), ("plain_q01", 11)],
    );
}

// Checking the conditions of implementations that form a chain asks for the
// implementations of the trait, which lowers the headers of those very
// implementations. Deriving the associated-type bound of a closed condition
// while a header is lowered must not read an implementation.
#[test]
fn closed_condition_chain_lowers() {
    let codes = diagnostics(
        r#"
pub trait Show {}
pub trait Hold { type A: Show }
pub struct Foo {}
impl Show for Foo {}
pub struct X {}
impl Hold for X { type A = Foo }
pub struct S {}
impl Hold for S where X: Hold { type A = Foo }
pub struct D {}
impl Hold for D where S: Hold { type A = Foo }
"#,
    );
    assert!(codes.is_empty(), "{codes:?}");
}

// The instance check reports every goal it refuses, in an order that does not
// depend on the order of the bounds: the first by the printed goal, and the
// rest after it.
#[test]
fn agreement_two_conflicts_reported_in_one_order() {
    let report = |signature: &str| {
        let source = format!(
            r#"
trait Show {{ const fn show() -> u256 }}
struct Foo {{}}
struct Bar {{}}
impl Show for Foo {{ const fn show() -> u256 {{ 100 }} }}
impl Show for Foo as Loud {{ const fn show() -> u256 {{ 70 }} }}
impl Show for Bar {{ const fn show() -> u256 {{ 1 }} }}
impl Show for Bar as LoudBar {{ const fn show() -> u256 {{ 7 }} }}
struct W<T> {{}}
struct V<T> {{}}
trait Tr {{ type A: Show }}
impl<T: Show> Tr for W<T> {{ type A = T }}
trait Tr2 {{ type A: Show }}
impl<T: Show> Tr2 for V<T> {{ type A = T }}
{signature}
const fn relay<C: Tr2<A = Foo>, E: Tr2<A = Bar>>() -> u256 {{ with (Loud, LoudBar) {{ f<W<Foo>, C, W<Bar>, E>() }} }}
const fn demand() -> u256 {{ relay<V<Foo>, V<Bar>>() }}
"#
        );
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("named_two_conflicts.fe".into(), &source);
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let mut passes = AnalysisPassManager::new();
        passes.add_module_pass("SemanticAccess", Box::new(SemanticAccessAnalysisPass));
        let report = format_diagnostics(&db, &passes.run_on_module(&db, module));
        report
            .lines()
            .find(|line| line.contains(" receives "))
            .map(|line| line.split('^').next_back().unwrap_or("").trim().to_string())
            .unwrap_or_else(|| panic!("no refusal in {report}"))
    };
    let foo_first = report(
        "const fn f<B: Tr<A = Foo>, C: Tr2<A = Foo>, D: Tr<A = Bar>, E: Tr2<A = Bar>>() -> u256 { 5 }",
    );
    let bar_first = report(
        "const fn f<B, C, D, E>() -> u256 where D: Tr<A = Bar>, E: Tr2<A = Bar>, B: Tr<A = Foo>, C: Tr2<A = Foo> { 5 }",
    );
    assert_eq!(foo_first, bar_first);
    assert!(foo_first.starts_with("`Bar: Show`"), "{foo_first}");
    assert!(foo_first.contains("also `Foo: Show`"), "{foo_first}");
}

// A route that returns to a projection that is still being computed cannot be
// followed (a salsa cycle in the evidence recursion). It refuses only where a
// dictionary on it selects a named implementation, and it never panics.
fn access_report(source: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("named_cycle.fe".into(), source);
    let (module, _) = db.top_mod(file);
    let mut passes = AnalysisPassManager::new();
    passes.add_module_pass("SemanticAccess", Box::new(SemanticAccessAnalysisPass));
    format_diagnostics(&db, &passes.run_on_module(&db, module))
}

fn checked_clean(source: &str) {
    let codes = diagnostics(source);
    assert!(codes.is_empty(), "{codes:?}");
    let report = access_report(source);
    assert!(report.trim().is_empty(), "{report}");
}

const GROWING_ONE: &str = r#"
pub trait Br { type A: Br
    type B: Br
    fn v() -> u256 }
pub struct Leaf {}
impl Br for Leaf { type A = Leaf
    type B = Leaf
    fn v() -> u256 { 1 } }
impl Br for Leaf as LN { type A = Leaf
    type B = Leaf
    fn v() -> u256 { 2 } }
pub struct N1<T> {}
pub trait Hb { type O: Br }
pub struct Bx<T> {}
impl<T: Br> Hb for Bx<T> { type O = N1<T> }
fn g<H: Hb<O = N1<Leaf>>>() -> u256 { <H::O as Br>::v() }
impl<T: Br> Br for N1<T> { type A = N1<N1<T>>
    type B = T
    fn v() -> u256 { T::v() } }
"#;

const GROWING_TWO: &str = r#"
pub trait Br { type A: Br
    type B: Br
    fn v() -> u256 }
pub struct Leaf {}
impl Br for Leaf { type A = Leaf
    type B = Leaf
    fn v() -> u256 { 1 } }
impl Br for Leaf as LN { type A = Leaf
    type B = Leaf
    fn v() -> u256 { 2 } }
pub struct N1<T> {}
pub struct N2<T> {}
impl<T: Br> Br for N1<T> { type A = N1<N1<T>>
    type B = N2<N1<T>>
    fn v() -> u256 { T::v() } }
impl<T: Br> Br for N2<T> { type A = N1<N2<T>>
    type B = N2<N2<T>>
    fn v() -> u256 { T::v() } }
pub trait Hb { type O: Br }
pub struct Bx<T> {}
impl<T: Br> Hb for Bx<T> { type O = N1<T> }
fn g<H: Hb<O = N1<Leaf>>>() -> u256 { <H::O as Br>::v() }
"#;

#[test]
fn evidence_cycle_at_instance_admission_is_a_refusal() {
    let source = format!("{GROWING_ONE}\nfn named() -> u256 {{ with (LN) {{ g<Bx<Leaf>>() }} }}\n");
    let report = access_report(&source);
    assert!(report.contains("error[16-0019]"), "{report}");
}

#[test]
fn evidence_cycle_with_several_growing_types_is_a_refusal() {
    let source = format!(
        "{GROWING_TWO}\nfn named() -> u256 {{ with (LN) {{ g<Bx<Leaf>>() }} }}\nfn plain() -> u256 {{ g<Bx<Leaf>>() }}\n"
    );
    let report = access_report(&source);
    assert!(report.contains("error[16-0019]"), "{report}");
}

// The generic body reads closed goals of the growing implementation under a
// choice (the entrance through type checking). The result is an acceptance or
// a refusal for the cycle alone; it is never a panic.
#[test]
fn evidence_cycle_during_body_checking_does_not_panic() {
    let source = format!(
        "{GROWING_ONE}\nfn g3<H: Hb<O = N1<Leaf>>>() -> u256 {{ with (LN) {{ <N1<N1<Leaf>> as Br>::v() + <N1<Leaf> as Br>::v() }} }}\nfn go() -> u256 {{ g3<Bx<Leaf>>() }}\n"
    );
    let codes = diagnostics(&source);
    assert!(codes.is_empty(), "{codes:?}");
    let report = access_report(&source);
    assert!(
        report.trim().is_empty() || report.contains("error[16-0019]"),
        "{report}"
    );
}

// The entrance through the use: a concrete use under a choice of a callee whose
// bound is pinned through the growing implementation.
#[test]
fn evidence_cycle_at_the_use_does_not_panic() {
    let source = format!(
        "{GROWING_ONE}\nfn g4<T: Br<A = N1<N1<Leaf>>, B = Leaf>, U: Hb<O = T>>() -> u256 {{ T::v() + <T::A as Br>::v() }}\nfn go() -> u256 {{ with (LN) {{ g4<N1<Leaf>, Bx<Leaf>>() }} }}\n"
    );
    let report = access_report(&source);
    assert!(report.contains("error[16-0019]"), "{report}");
}

#[test]
fn evidence_cycle_without_a_choice_is_accepted() {
    let source = format!("{GROWING_ONE}\nfn plain() -> u256 {{ g<Bx<Leaf>>() }}\n");
    checked_clean(&source);
}

// No named implementation is declared and no choice is written: the program
// uses nothing of the feature, and it is accepted.
#[test]
fn evidence_cycle_without_any_named_implementation_is_accepted() {
    let source = format!(
        "{}\nfn plain() -> u256 {{ g<Bx<Leaf>>() }}\n",
        GROWING_TWO.replace("impl Br for Leaf as LN { type A = Leaf\n    type B = Leaf\n    fn v() -> u256 { 2 } }\n", "")
    );
    assert!(!source.contains("as LN"), "{source}");
    checked_clean(&source);
}

// A member at the depth limit whose bounds lead only to members already
// reached is followed completely, so three levels below an equality, ending in
// a member whose own `Next` is itself, are accepted and read the chosen
// implementation.
#[test]
fn agreement_member_at_the_limit_followed() {
    values(
        r#"
trait Chain { type Next: Chain
    const fn v() -> u256 }
struct End0 {}
impl Chain for End0 { type Next = End0
    const fn v() -> u256 { 9 } }
struct E {}
impl Chain for E { type Next = End0
    const fn v() -> u256 { 0 } }
impl Chain for E as E3 { type Next = End0
    const fn v() -> u256 { 5 } }
struct L<T> {}
impl<T: Chain> Chain for L<T> { type Next = T
    const fn v() -> u256 { T::v() + 1 } }
trait Holder { type Out: Chain }
struct Bx<T> {}
impl<T: Chain> Holder for Bx<T> { type Out = L<L<L<T>>> }
const fn g<H: Holder<Out = L<L<L<E>>>>>() -> u256 { <H::Out as Chain>::v() }
const fn go() -> u256 { with (E3) { g<Bx<E>>() } }
"#,
        &[("go", 8)],
    );
}

// Route text: a refusal names each route by the bound it starts from and the
// steps after it, and the implementation each route gives.
fn messages(source: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("named_route_text.fe".into(), source);
    let (module, _) = db.top_mod(file);
    let diagnostics = fe_hir::analysis::initialize_analysis_pass().run_on_module(&db, module);
    let mut passes = AnalysisPassManager::new();
    passes.add_module_pass("SemanticAccess", Box::new(SemanticAccessAnalysisPass));
    format!(
        "{}{}",
        format_diagnostics(&db, &diagnostics),
        format_diagnostics(&db, &passes.run_on_module(&db, module))
    )
}

const ROUTES_A01: &str = r#"
pub trait Show { fn show() -> u256 }
pub struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
impl Show for Foo as Loud { fn show() -> u256 { 70 } }
impl Show for Foo as Quiet { fn show() -> u256 { 30 } }
pub struct W<T> {}
pub struct V<T> {}
pub trait Tr { type A: Show
    fn via() -> u256 }
impl<T: Show> Tr for W<T> { type A = T
    fn via() -> u256 { T::show() } }
pub trait Tr2 { type A: Tr
    fn via2() -> u256 }
impl<T: Tr> Tr2 for V<T> { type A = T
    fn via2() -> u256 { T::via() } }
pub struct U<T> {}
impl<T: Show> Tr for U<T> { type A = T
    fn via() -> u256 { T::show() + 1 } }
pub trait Mk { type Out }
pub struct X {}
impl Mk for X { type Out = U<Foo> }
fn f<B: Tr<A = Foo>, C: Tr2<A = <X as Mk>::Out>>() -> u256 { Foo::show() * 1000000 + B::via() * 1000 + C::via2() }
fn relay<C: Tr2<A = <X as Mk>::Out>>() -> u256 { with (Loud) { f<W<Foo>, C>() } }
fn disagree() -> u256 { relay<V<U<Foo>>>() }
"#;

const ROUTES_A10: &str = r#"
pub trait Show { fn show() -> u256 }
pub struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
impl Show for Foo as Loud { fn show() -> u256 { 70 } }
impl Show for Foo as Quiet { fn show() -> u256 { 30 } }
pub struct W<T> {}
pub struct V<T> {}
pub trait Tr { type A: Show
    fn via() -> u256 }
impl<T: Show> Tr for W<T> { type A = T
    fn via() -> u256 { T::show() } }
pub trait Tr2 { type A: Tr
    fn via2() -> u256 }
impl<T: Tr> Tr2 for V<T> { type A = T
    fn via2() -> u256 { T::via() } }
pub struct U<T> {}
impl<T: Show> Tr for U<T> { type A = T
    fn via() -> u256 { T::show() + 1 } }
pub trait Mk { type Out }
pub struct X {}
impl Mk for X { type Out = U<Foo> }
fn f<C: Tr2<A = <X as Mk>::Out>, B: Tr<A = Foo>>() -> u256 { Foo::show() * 1000000 + B::via() * 1000 + C::via2() }
fn relay<C: Tr2<A = <X as Mk>::Out>>() -> u256 { with (Loud) { f<C, W<Foo>>() } }
fn disagree() -> u256 { relay<V<U<Foo>>>() }
"#;

const ROUTES_T05: &str = r#"
pub trait Chain { type Next: Chain
    fn v() -> u256 }
pub struct End {}
impl Chain for End { type Next = End
    fn v() -> u256 { 0 } }
impl Chain for End as E2 { type Next = End
    fn v() -> u256 { 5 } }
pub struct L<T> {}
impl<T: Chain> Chain for L<T> { type Next = T
    fn v() -> u256 { T::v() + 1 } }
pub trait Holder { type Out: Chain }
pub struct Bx<T> {}
impl<T: Chain> Holder for Bx<T> { type Out = T }
fn g<H: Holder<Out = End>>() -> u256 { <H::Out as Chain>::v() + End::v() * 100 }
fn go() -> u256 { with (E2) { g<Bx<End>>() } }
"#;

#[test]
fn route_text_names_each_implementation() {
    let report = messages(ROUTES_A01);
    assert!(
        report.contains("`Foo: Show` receives `Loud` through `B: Tr<A = Foo>` (the bound on `A`) and `impl Show for Foo` through `C: Tr2<A = X::Out>` (the bound on `A`, then the bound on `A` of `U<Foo>: Tr`)"),
        "{report}"
    );
}

#[test]
fn route_text_written_route_is_chosen_here() {
    let report = messages(&format!(
        r#"{MUTUAL}
const fn call<T: Left<Out = U>, U: Right<Back = T>>() -> u256 {{ T::l() * 10 + U::r() }}
const fn relay<T: Left<Out = U>, U: Right<Back = T>>() -> u256 {{ call<T, U>() }}
const fn go() -> u256 {{ with (BigA) {{ relay<A, B>() }} }}
"#
    ));
    assert!(
        report.contains("`A: Left` receives `BigA` through `T: Left<Out = U>`, as chosen here, and `impl Left for A` through `U: Right<Back = T>` (the bound on `Back`)"),
        "{report}"
    );
}

#[test]
fn route_text_is_independent_of_bound_order() {
    let label = |source: &str| {
        messages(source)
            .lines()
            .find(|line| line.contains("receives"))
            .map(|line| line.split('^').next_back().unwrap_or("").trim().to_string())
            .unwrap_or_else(|| panic!("no refusal in {source}"))
    };
    let (first, second) = (label(ROUTES_A01), label(ROUTES_A10));
    assert_eq!(first, second);
}

#[test]
fn route_text_route_through_the_goal_itself() {
    let report = messages(&format!("{ROUTES_T05}\nfn go2() -> u256 {{ go() }}\n"));
    assert!(
        report.contains("`End: Chain` receives `E2` through `H: Holder<Out = End>` (the bound on `Out`) and `impl Chain for End` through `H: Holder<Out = End>` (the bound on `Out`, then the bound on `Next` of `End: Chain`), which passes through `End: Chain` itself"),
        "{report}"
    );
}
