//! Behavioral discriminators for the evidence-only core. These are new tests,
//! not claimed reproductions of Sean's original p01-p22 packet.
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
