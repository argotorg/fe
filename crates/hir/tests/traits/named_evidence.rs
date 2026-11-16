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
    assert!(codes.contains(&"6-0003".to_string()), "{codes:?}");
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
    // The first three programs read `T::len()` of a type parameter, which is
    // not an integer in `T::len() + 1`, so constant evaluation refuses them
    // with 3-0052 before the instance key matters. The last two read `T::LEN`
    // and lower through the instance key.
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
    assert!(
        inline_choice_codes.contains(&"3-0052".to_string()),
        "{inline_choice_codes:?}"
    );

    let helper_choice_codes = diagnostics(
        r#"
trait Len { const fn len() -> usize }
trait Indexed<const I: usize> {}
struct Wrap<T> { t: T }
impl<T: Len> Len for Wrap<T> as WrapLen { const fn len() -> usize { T::len() + 1 } }
const fn read_len<T: Len>() -> usize { T::len() }
const fn via_wrap<T: Len>() -> usize { with (WrapLen) { read_len<Wrap<T>>() } }
struct S2<T> where T: Len + Indexed<{ via_wrap<T>() }> { t: T }
"#,
    );
    assert!(
        helper_choice_codes.contains(&"3-0052".to_string()),
        "{helper_choice_codes:?}"
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
    assert!(
        provider_len_codes.contains(&"3-0052".to_string()),
        "{provider_len_codes:?}"
    );

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
