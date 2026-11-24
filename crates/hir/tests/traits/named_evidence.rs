//! Behavioral discriminators for the evidence-only core. These are new tests,
//! not claimed reproductions of the original p01-p22 packet.
use fe_hir::{
    analysis::{
        semantic::{
            GenericSubst, SemConstScalar, SemConstValue, SemanticInstance, eval_body_owner_const,
            root_semantic_instance_key,
        },
        ty::ty_check::BodyOwner,
    },
    test_db::{HirAnalysisTestDb, find_func},
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
