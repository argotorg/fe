//! A module's `with` declaration parses and lowers to one item. Its meaning is
//! not available yet, so the checker refuses it instead of ignoring it.
use fe_hir::{hir_def::ItemKind, test_db::HirAnalysisTestDb};

const PICKS: &str = r#"
pub struct Item {}
pub trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
pub impl Pick for Item as Discount { const fn value() -> u256 { 70 } }
pub impl Pick for Item as Tiny { const fn value() -> u256 { 3 } }
"#;

fn with_picks(rest: &str) -> String {
    [PICKS, rest].concat()
}

fn diagnostics(source: &str) -> Vec<String> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("module_with.fe".into(), source);
    let (module, _) = db.top_mod(file);
    fe_hir::analysis::initialize_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(&db).error_code.to_string())
        .collect()
}

#[test]
fn a_declaration_lowers_to_one_item_with_its_operands() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "module_with.fe".into(),
        &with_picks("with (Discount, <Item as Pick>)\n"),
    );
    let (module, _) = db.top_mod(file);
    let declarations: Vec<_> = module
        .children_non_nested(&db)
        .filter_map(|item| match item {
            ItemKind::With(declaration) => Some(declaration),
            _ => None,
        })
        .collect();
    assert_eq!(declarations.len(), 1);
    assert_eq!(declarations[0].operands(&db).len(), 2);
    assert_eq!(
        declarations[0].pretty_print(&db),
        "with (Discount, <Item as Pick>)"
    );
}

#[test]
fn a_declaration_is_refused_until_its_meaning_is_available() {
    assert_eq!(
        diagnostics(&with_picks("with (Discount)\n")),
        vec!["8-0171"]
    );
}

#[test]
fn a_declaration_in_a_child_module_is_refused_once() {
    assert_eq!(
        diagnostics(&with_picks(
            "mod m {\n    use super::*\n    with (Discount)\n    pub fn f() -> u256 { 1 }\n}\n"
        )),
        vec!["8-0171"]
    );
}

#[test]
fn a_program_without_a_declaration_is_unaffected() {
    assert_eq!(diagnostics(&with_picks("")), Vec::<String>::new());
    assert_eq!(
        diagnostics(&with_picks(
            "pub fn f() -> u256 { with (Discount) { <Item as Pick>::value() } }\n"
        )),
        Vec::<String>::new()
    );
}

/// Names an operand can resolve to, shared by the operand tests.
const OPERANDS: &str = r#"
pub trait Show { fn show() -> u256 }
pub struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
pub impl Show for Foo as Loud { fn show() -> u256 { 70 } }
pub impl Show for Foo as Quiet { fn show() -> u256 { 1 } }
pub fn Loud2() -> u256 { 3 }
pub impl Show for Foo as Loud2 { fn show() -> u256 { 71 } }
pub struct Item {}
pub trait Missing {}
pub trait Sub {}
impl<T: Missing> Sub for T as Broken {}
pub struct NeedsSub<T: Sub> {}
mod hidden { use super::*
  impl Show for Item as Secret { fn show() -> u256 { 5 } } }
pub struct G<T> {}
pub impl<T> Show for G<T> as GenLoud { fn show() -> u256 { 9 } }
pub struct G2<T, U> {}
pub impl<T, U> Show for G2<T, U> as Two { fn show() -> u256 { 9 } }
pub struct Restricted<const N: u256> where { N > 0 } {}
pub struct Gx<T> {}
pub impl<T> Show for Gx<T> as Choice { fn show() -> u256 { 9 } }
pub fn val() -> u256 { 3 }
"#;

const PROVIDER_AND_IMPL: &str = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Tr { fn op(self) -> u256 }
struct R { a: u256 }
struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn op(self) -> u256 { 1 } }
    }
}
impl Tr for R { fn op(self) -> u256 { 5 } }
impl Tr for R as P { fn op(self) -> u256 { 6 } }
"#;

fn sorted_codes(source: &str) -> Vec<String> {
    let mut codes = diagnostics(source);
    codes.sort();
    codes
}

/// The operands of a block `with`, and the codes each one is refused with.
/// These pin what the block says about an operand, whatever code decides it.
#[test]
fn block_operand_codes() {
    let rows: &[(&str, &str, &[&str])] = &[
        ("Nope", "fn f() -> u256 { with (Nope) { 1 } }", &["8-0012"]),
        (
            "nope::Loud",
            "fn f() -> u256 { with (nope::Loud) { 1 } }",
            &["2-0002"],
        ),
        (
            "hidden::Secret",
            "fn f() -> u256 { with (hidden::Secret) { 1 } }",
            &["2-0003"],
        ),
        (
            "Loud2",
            "fn f() -> u256 { with (Loud2) { 1 } }",
            &["2-0004"],
        ),
        (
            "Loud<u8>",
            "fn f() -> u256 { with (Loud<u8>) { 1 } }",
            &["3-0016"],
        ),
        (
            "Two<u8>",
            "fn f() -> u256 { with (Two<u8>) { 1 } }",
            &["8-0163"],
        ),
        (
            "Loud, Quiet",
            "fn f() -> u256 { with (Loud, Quiet) { Foo::show() } }",
            &["8-0160"],
        ),
        (
            "<Item as Show>",
            "fn f() -> u256 { with (<Item as Show>) { 1 } }",
            &["6-0003"],
        ),
        (
            "Choice<Restricted<0>>",
            "fn f() -> u256 { with (Choice<Restricted<0>>) { 1 } }",
            &["8-0093"],
        ),
        (
            "Broken",
            "fn f() { with (Broken) { let _: NeedsSub<Item> } }",
            &["6-0003"],
        ),
    ];
    for (name, source, codes) in rows {
        assert_eq!(
            sorted_codes(&[OPERANDS, source].concat()),
            *codes,
            "operand {name}"
        );
    }
    assert_eq!(
        sorted_codes(
            &[
                PROVIDER_AND_IMPL,
                "fn f() -> u256 { with (P) { R { a: 0 }.op() } }"
            ]
            .concat()
        ),
        ["2-0004"],
        "operand P, a provider type and a named implementation"
    );
}

/// The codes of a program, sorted, without the refusal that a module `with`
/// still earns until its frame reaches the bodies.
fn codes_without_refusal(source: &str) -> Vec<String> {
    let mut codes = diagnostics(source);
    codes.retain(|code| code != "8-0171");
    codes.sort();
    codes
}

/// The same operand, written as a module's declaration.
#[test]
fn module_operand_codes() {
    let rows: &[(&str, &str, &[&str])] = &[
        ("Nope", "with (Nope)", &["8-0012"]),
        ("nope::Loud", "with (nope::Loud)", &["2-0002"]),
        ("hidden::Secret", "with (hidden::Secret)", &["2-0003"]),
        ("Loud2", "with (Loud2)", &["2-0004"]),
        ("Loud<u8>", "with (Loud<u8>)", &["3-0016"]),
        ("Two<u8>", "with (Two<u8>)", &["8-0163"]),
        ("Loud, Quiet", "with (Loud, Quiet)", &["8-0160"]),
        ("<Item as Show>", "with (<Item as Show>)", &["6-0003"]),
        (
            "Choice<Restricted<0>>",
            "with (Choice<Restricted<0>>)",
            &["8-0093"],
        ),
    ];
    for (name, declaration, codes) in rows {
        let source = [
            OPERANDS,
            &format!("mod m {{\n    use super::*\n    {declaration}\n    pub fn f() -> u256 {{ 1 }}\n}}\n"),
        ]
        .concat();
        assert_eq!(codes_without_refusal(&source), *codes, "operand {name}");
    }
    let broken = [
        OPERANDS,
        "mod m {\n    use super::*\n    with (Broken)\n    pub fn f() { let _: NeedsSub<Item> }\n}\n",
    ]
    .concat();
    assert_eq!(codes_without_refusal(&broken), ["6-0003"], "operand Broken");
    let provider = [
        PROVIDER_AND_IMPL,
        "mod m {\n    use super::*\n    with (P)\n    pub fn f() -> u256 { 1 }\n}\n",
    ]
    .concat();
    assert_eq!(
        codes_without_refusal(&provider),
        ["2-0004"],
        "operand P, a provider type and a named implementation"
    );
}

/// A value, a provider or any other name that is not an implementation is not
/// a module operand: values and providers are written in a `with` block.
#[test]
fn a_module_operand_that_is_not_an_implementation_is_refused() {
    for operand in ["val", "Show", "Foo", "hidden"] {
        let source = [
            OPERANDS,
            &format!("mod m {{\n    use super::*\n    with ({operand})\n}}\n"),
        ]
        .concat();
        assert_eq!(
            codes_without_refusal(&source),
            ["2-0021"],
            "operand {operand}"
        );
    }
}

#[test]
fn module_operands_follow_visibility() {
    assert_eq!(
        codes_without_refusal(&with_picks(
            r#"
mod hidden {
    use super::*
    impl Pick for Item as Private { const fn value() -> u256 { 1 } }
}
mod m {
    use super::*
    with (hidden::Private)
}
"#,
        )),
        ["2-0003"]
    );
}

#[test]
fn ambiguous_module_operands_are_refused() {
    assert_eq!(
        codes_without_refusal(&with_picks(
            r#"
mod a {
    use super::*
    pub impl Pick for Item as Same { const fn value() -> u256 { 1 } }
}
mod b {
    use super::*
    pub impl Pick for Item as Same { const fn value() -> u256 { 2 } }
}
mod m {
    use super::a::*
    use super::b::*
    with (Same)
}
"#,
        )),
        ["2-0004"]
    );
}

#[test]
fn two_operands_answering_one_goal_are_refused() {
    let codes = codes_without_refusal(&with_picks(
        r#"
mod a {
    use super::*
    with (Tiny, Discount)
    pub const fn f() -> u256 { Item::value() }
}
mod b {
    use super::*
    with (Discount, <Item as Pick>)
    pub const fn f() -> u256 { Item::value() }
}
mod c {
    use super::*
    with (Discount)
    pub mod d {
        use super::super::*
        with (Tiny)
        pub const fn f() -> u256 { Item::value() }
    }
}
"#,
    ));
    // One for each of `a` and `b`; a child's declaration is another frame.
    assert_eq!(codes, ["8-0160", "8-0160"]);
}

#[test]
fn an_openings_conditions_are_proved_once_at_the_declaration() {
    let codes = codes_without_refusal(&with_picks(
        r#"
pub trait Gate {}
pub struct Box<T> {}
impl<T: Gate> Pick for Box<T> { const fn value() -> u256 { 1 } }
mod m {
    use super::*
    with (<Box<Item> as Pick>)
    pub const fn a() -> u256 { 1 }
    pub const fn b() -> u256 { 2 }
}
"#,
    ));
    assert_eq!(codes, ["6-0003"]);
}

#[test]
fn an_openings_conditions_see_every_other_operand_of_the_declaration() {
    let codes = codes_without_refusal(&with_picks(
        r#"
pub trait Gate { const fn g() -> u256 }
pub impl Gate for Item as G { const fn g() -> u256 { 5 } }
pub struct Box<T> {}
pub trait Total { const fn total() -> u256 }
impl<T: Gate> Total for Box<T> { const fn total() -> u256 { T::g() + 1 } }
mod opening_first {
    use super::*
    with (<Box<Item> as Total>, G)
    pub const fn f() -> u256 { 1 }
}
mod gate_first {
    use super::*
    with (G, <Box<Item> as Total>)
    pub const fn f() -> u256 { 1 }
}
"#,
    ));
    assert!(codes.is_empty(), "{codes:?}");
}

#[test]
fn types_inside_module_operands_are_checked_for_their_requirements() {
    let source = |operand: &str| {
        format!(
            r#"
pub struct Restricted<const N: u256> where {{ N > 0 }} {{}}
pub struct Boxed<T> {{}}
pub trait Mark {{ const fn mark() -> u256 }}
impl<T> Mark for Boxed<T> {{ const fn mark() -> u256 {{ 1 }} }}
pub impl<T> Mark for Boxed<T> as Choice {{ const fn mark() -> u256 {{ 2 }} }}
mod m {{
    use super::*
    with ({operand})
}}
"#
        )
    };
    for operand in [
        "Choice<Restricted<0>>",
        "<Boxed<Restricted<0>> as Mark>",
        "<Boxed<(Restricted<0>, u8)> as Mark>",
    ] {
        assert_eq!(
            codes_without_refusal(&source(operand)),
            ["8-0093"],
            "{operand}"
        );
    }
    for operand in [
        "Choice<Restricted<1>>",
        "<Boxed<Restricted<1>> as Mark>",
        "<Boxed<(Restricted<1>, u8)> as Mark>",
    ] {
        assert!(
            codes_without_refusal(&source(operand)).is_empty(),
            "{operand}"
        );
    }
}

/// Resolving the declaration needs a constant whose body is checked under the
/// frame being resolved. The result is an error, never a panic and never an
/// accepted program.
#[test]
fn a_declaration_that_needs_its_own_modules_constant_is_refused() {
    let codes = codes_without_refusal(&with_picks(
        r#"
pub struct Arr<const N: u256> {}
pub trait Size { const fn size() -> u256 }
mod m {
    use super::*
    with (Discount, <Arr<70> as Size>)
    pub const K: u256 = Item::value()
    impl Size for Arr<{ K }> { const fn size() -> u256 { 1 } }
    pub const fn f() -> u256 { Arr<70>::size() }
}
"#,
    ));
    assert!(!codes.is_empty(), "the declaration is accepted");
}
