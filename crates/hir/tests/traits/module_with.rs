//! A module's `with` declaration: the outermost frame of every body of its
//! module.
use fe_hir::{
    analysis::{
        analysis_pass::AnalysisPassManager,
        semantic::{
            GenericSubst, SemConstScalar, SemConstValue, SemanticAccessAnalysisPass,
            eval_body_owner_const,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::ItemKind,
    test_db::{HirAnalysisTestDb, find_func, format_diagnostics},
};
use num_traits::ToPrimitive;

/// The program has no diagnostics, and each named `const fn` evaluates to its
/// value.
fn values(source: &str, expected: &[(&str, usize)]) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("module_with.fe".into(), source);
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
pub struct Item {}
pub trait Pick { const fn value() -> u256 }
impl Pick for Item { const fn value() -> u256 { 100 } }
pub impl Pick for Item as Discount { const fn value() -> u256 { 70 } }
pub impl Pick for Item as Tiny { const fn value() -> u256 { 3 } }
pub const fn read<T: Pick>() -> u256 { T::value() }
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
fn a_declaration_applies_to_the_bodies_of_its_module() {
    values(
        &with_picks(
            "mod m {\n    use super::*\n    with (Discount)\n    pub const fn f() -> u256 { Item::value() }\n}\nconst fn f() -> u256 { m::f() }\nconst fn outside() -> u256 { Item::value() }\n",
        ),
        &[("f", 70), ("outside", 100)],
    );
}

#[test]
fn a_declaration_in_a_child_module_applies_to_that_module_once() {
    assert_eq!(
        diagnostics(&with_picks(
            "mod m {\n    use super::*\n    with (Discount)\n    pub fn f() -> u256 { 1 }\n}\n"
        )),
        Vec::<String>::new()
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
        assert_eq!(sorted_codes(&source), *codes, "operand {name}");
    }
    let broken = [
        OPERANDS,
        "mod m {\n    use super::*\n    with (Broken)\n    pub fn f() { let _: NeedsSub<Item> }\n}\n",
    ]
    .concat();
    assert_eq!(sorted_codes(&broken), ["6-0003"], "operand Broken");
    let provider = [
        PROVIDER_AND_IMPL,
        "mod m {\n    use super::*\n    with (P)\n    pub fn f() -> u256 { 1 }\n}\n",
    ]
    .concat();
    assert_eq!(
        sorted_codes(&provider),
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
        assert_eq!(sorted_codes(&source), ["2-0021"], "operand {operand}");
    }
}

#[test]
fn module_operands_follow_visibility() {
    assert_eq!(
        sorted_codes(&with_picks(
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
        sorted_codes(&with_picks(
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
    let codes = sorted_codes(&with_picks(
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
    let codes = sorted_codes(&with_picks(
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
    let codes = sorted_codes(&with_picks(
        r#"
pub trait Gate { const fn g() -> u256 }
pub impl Gate for Item as G { const fn g() -> u256 { 5 } }
pub struct Box<T> {}
pub trait Total { const fn total() -> u256 }
impl<T: Gate> Total for Box<T> { const fn total() -> u256 { T::g() + 1 } }
mod opening_first {
    use super::*
    with (<Box<Item> as Total>, G)
    pub const fn f() -> u256 { Box<Item>::total() }
}
mod gate_first {
    use super::*
    with (G, <Box<Item> as Total>)
    pub const fn f() -> u256 { Box<Item>::total() }
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
        assert_eq!(sorted_codes(&source(operand)), ["8-0093"], "{operand}");
    }
    for operand in [
        "Choice<Restricted<1>>",
        "<Boxed<Restricted<1>> as Mark>",
        "<Boxed<(Restricted<1>, u8)> as Mark>",
    ] {
        assert!(sorted_codes(&source(operand)).is_empty(), "{operand}");
    }
}

/// Operand `<Arr<70> as Size>` names an implementation of `Size` for `Arr<K>`,
/// where `K` is 70 only under the declaration's own `Discount`. The frame is
/// built without reading `K`; `K` is fixed under `Discount`, and the opening
/// is chosen afterwards, so the declaration is well founded. A constant in a
/// type is refused once the constant, not the use, decides it (`8-0138`);
/// until then the declaration is accepted.
const SELF_READING: &str = r#"
pub struct Arr<const N: u256> {}
pub trait Size { const fn size() -> u256 }
mod m {
    use super::*
    with (Discount, <Arr<70> as Size>)
    pub const K: u256 = Item::value()
    impl Size for Arr<{ K }> { const fn size() -> u256 { 1 } }
    pub const fn f() -> u256 { Arr<70>::size() }
}
const fn f() -> u256 { m::f() }
"#;

#[test]
fn a_declaration_whose_opening_reads_its_modules_constant_is_well_founded() {
    values(&with_picks(SELF_READING), &[("f", 1)]);
}

/// The same program with a type and a reader of `m::K` before the module:
/// the entry order changes nothing.
#[test]
fn the_order_of_the_readers_of_a_framed_constant_changes_nothing() {
    values(
        &with_picks(&format!(
            "{SELF_READING}\npub struct S {{ pub a: Arr<{{ m::K }}> }}\nconst fn early() -> u256 {{ m::K }}\n"
        )),
        &[("early", 70), ("f", 1)],
    );
}

/// The frame is built from names and deferred lowering, so the headers
/// `impl Size for Arr<{ K }>` and `impl Pick for Arr<{ K }>` are not read
/// while `K = Item::value()` is checked under it. `K` asks a `Pick` goal,
/// which the `Size` operand never answers.
#[test]
fn a_module_frame_is_built_without_evaluating_its_constants() {
    values(
        r#"
pub struct Arr<const N: u256> {}
pub trait Pick { const fn value() -> u256 }
pub trait Size { const fn size() -> u256 }
pub struct Item {}
impl Pick for Item { const fn value() -> u256 { 100 } }
pub impl Pick for Item as Seventy { const fn value() -> u256 { 70 } }
impl Size for Arr<100> { const fn size() -> u256 { 1 } }

pub mod outer {
    use super::*
    with (Seventy)
    pub mod m {
        use super::*
        with (<Arr<70> as Size>)
        pub const K: u256 = Item::value()
        impl Size for Arr<{ K }> { const fn size() -> u256 { 2 } }
        impl Pick for Arr<{ K }> { const fn value() -> u256 { 3 } }
        pub const fn f() -> u256 { Arr<70>::size() }
    }
}
const fn f() -> u256 { outer::m::f() }
"#,
        &[("f", 2)],
    );
}

/// The traits `Show` and `Show2` both declare `show`. The operand answers
/// `Show2` for `Arr<100>`, so `Foo::show()` in the constant is answered by
/// `Show` for `Foo` and never asks the operand, which reads the constant.
#[test]
fn a_declared_operand_for_another_type_is_not_chosen_by_a_method_of_the_same_name() {
    values(
        r#"
pub struct Arr<const N: u256> {}
pub struct Foo {}
pub trait Show { const fn show() -> u256 }
pub trait Show2 { const fn show() -> u256 }
impl Show for Foo { const fn show() -> u256 { 100 } }
pub mod m {
    use super::*
    with (<Arr<100> as Show2>)
    pub const K: u256 = Foo::show()
    impl Show2 for Arr<{ K }> { const fn show() -> u256 { 2 } }
    pub const fn f() -> u256 { K }
}
const fn f() -> u256 { m::f() }
"#,
        &[("f", 100)],
    );
}

/// The operand answers `Show` for `Foo`, the type that the constant calls
/// `show` on, so the call chooses it: the outer frame says 70, the operand
/// restores the ordinary implementation.
#[test]
fn a_declared_operand_for_the_receivers_type_is_chosen_by_a_method_of_its_trait() {
    values(
        r#"
pub struct Foo {}
pub trait Show { const fn show() -> u256 }
impl Show for Foo { const fn show() -> u256 { 100 } }
pub impl Show for Foo as Seventy { const fn show() -> u256 { 70 } }
pub mod outer {
    use super::*
    with (Seventy)
    pub mod m {
        use super::*
        with (<Foo as Show>)
        pub const K: u256 = Foo::show()
        pub const fn f() -> u256 { K }
    }
}
const fn f() -> u256 { outer::m::f() }
"#,
        &[("f", 100)],
    );
}

/// An implementation header that reads a constant whose own check asks the
/// operand's trait: with an alternative answer the states alternate. Closed
/// when a framed constant in an implementation header must be a literal
/// (specification amendment 5.2); until then a known limit.
#[test]
#[ignore = "known limit: a header that reads a constant whose check asks the operand's trait iterates; closed by the framed-constant rule for implementation headers"]
fn a_header_constant_that_asks_its_own_operand_ends_without_iteration() {
    let codes = sorted_codes(
        r#"
pub struct Arr<const N: u256> {}
pub trait Pick { const fn value() -> u256 }
pub trait Size { const fn size() -> u256 }
pub struct Item {}
impl Pick for Item { const fn value() -> u256 { 100 } }
pub impl Size for Arr<70> as S70 { const fn size() -> u256 { 7 } }
impl Size for Arr<100> { const fn size() -> u256 { 1 } }

pub mod outer {
    use super::*
    with (S70)
    pub mod m {
        use super::*
        with (<Arr<70> as Size>)
        pub const K: u256 = Arr<70>::size()
        impl Size for Arr<{ K }> { const fn size() -> u256 { 2 } }
        impl Pick for Arr<{ K }> { const fn value() -> u256 { 3 } }
    }
}
"#,
    );
    assert!(!codes.is_empty(), "{codes:?}");
}

/// A constant of the module is read by the operand that the constant's own
/// check asks for. The two states alternate (the opening restores the
/// implementation that made the constant 100), so no meaning exists.
const ALTERNATING: &str = r#"
pub struct Arr<const N: u256> {}
pub trait Pick { const fn value() -> u256 }
impl Pick for Arr<70> { const fn value() -> u256 { 100 } }
impl Pick for Arr<100> { const fn value() -> u256 { 100 } }
pub impl Pick for Arr<70> as Seventy { const fn value() -> u256 { 70 } }
"#;

#[test]
#[ignore = "known limit until module operands take literal constant arguments: the constant's check forces the operand that reads the constant"]
fn an_operand_that_reads_a_constant_of_its_own_check_is_refused() {
    let codes = sorted_codes(&format!(
        r#"{ALTERNATING}
pub mod outer {{
    use super::*
    with (Seventy)
    pub mod m {{
        use super::*
        with (<Arr<{{ K }}> as Pick>)
        pub const K: u256 = Arr<70>::value()
        pub fn f() -> u256 {{ K }}
    }}
}}
"#
    ));
    assert_eq!(codes, ["2-0022"]);
}

#[test]
#[ignore = "known limit until module operands take literal constant arguments: the constant function's check forces the operand that calls it"]
fn an_operand_that_calls_a_function_of_its_own_check_is_refused() {
    let codes = sorted_codes(&format!(
        r#"{ALTERNATING}
pub mod outer {{
    use super::*
    with (Seventy)
    pub mod m {{
        use super::*
        with (<Arr<{{ kf() }}> as Pick>)
        pub const fn kf() -> u256 {{ Arr<70>::value() }}
    }}
}}
"#
    ));
    assert_eq!(codes, ["2-0022"]);
}

// The frame of every body

#[test]
fn the_declaration_is_the_outermost_frame_of_every_body() {
    values(
        &with_picks(
            r#"
mod m {
    use super::*
    with (Discount)
    pub const fn direct() -> u256 { Item::value() }
    pub const fn bound() -> u256 { read<Item>() }
    pub const K: u256 = Item::value()
    pub const fn constant() -> u256 { K }
    pub const fn nearest() -> u256 { with (Tiny) { Item::value() } }
    pub const fn ordinary() -> u256 { with (<Item as Pick>) { Item::value() } }
    pub const fn local() -> u256 {
        const fn inner() -> u256 { Item::value() }
        inner()
    }
    static_assert(Item::value() == 70)
}
const fn direct() -> u256 { m::direct() }
const fn bound() -> u256 { m::bound() }
const fn constant() -> u256 { m::constant() }
const fn nearest() -> u256 { m::nearest() }
const fn ordinary() -> u256 { m::ordinary() }
const fn local() -> u256 { m::local() }
const fn outside() -> u256 { read<Item>() + Item::value() }
"#,
        ),
        &[
            ("direct", 70),
            ("bound", 70),
            ("constant", 70),
            ("nearest", 3),
            ("ordinary", 100),
            ("local", 70),
            ("outside", 200),
        ],
    );
}

#[test]
fn a_const_initializer_is_validated_with_its_modules_choice() {
    values(
        r#"
struct Item {}
trait Pick { const fn value() -> u256 }
impl Pick for Item as OnlyChoice { const fn value() -> u256 { 70 } }
const fn from_bound<I: Pick>() -> u256 { I::value() }
mod chosen {
    use super::{Item, OnlyChoice, from_bound}
    with (OnlyChoice)
    pub const K: u256 = from_bound<Item>()
}
const fn from_module() -> u256 { chosen::K }
"#,
        &[("from_module", 70)],
    );
}

#[test]
fn child_modules_inherit_the_frame_and_their_own_declaration_is_nearer() {
    values(
        &with_picks(
            r#"
mod m {
    use super::*
    with (Discount)
    pub mod c {
        use super::super::*
        pub const fn f() -> u256 { Item::value() }
    }
    pub mod d {
        use super::super::*
        with (Tiny)
        pub const fn f() -> u256 { Item::value() }
        pub mod e {
            use super::super::super::*
            pub const fn f() -> u256 { Item::value() }
        }
    }
    pub mod o {
        use super::super::*
        with (<Item as Pick>)
        pub const fn f() -> u256 { Item::value() }
    }
}
const fn c() -> u256 { m::c::f() }
const fn d() -> u256 { m::d::f() }
const fn e() -> u256 { m::d::e::f() }
const fn o() -> u256 { m::o::f() }
"#,
        ),
        &[("c", 70), ("d", 3), ("e", 3), ("o", 100)],
    );
}

#[test]
fn importing_a_module_brings_its_names_and_not_its_frame() {
    values(
        &with_picks(
            r#"
mod m {
    use super::*
    with (Discount)
    pub const fn f() -> u256 { Item::value() }
    pub const fn g<T: Pick>() -> u256 { T::value() }
}
mod n {
    use super::*
    use super::m::*
    pub const fn mine() -> u256 { Item::value() }
    pub const fn called() -> u256 { f() }
    pub const fn generic() -> u256 { g<Item>() }
}
const fn mine() -> u256 { n::mine() }
const fn called() -> u256 { n::called() }
const fn generic() -> u256 { n::generic() }
"#,
        ),
        &[("mine", 100), ("called", 70), ("generic", 100)],
    );
}

#[test]
fn a_named_implementations_methods_keep_their_own_implementation_nearest() {
    values(
        r#"
pub struct Item {}
pub trait Pair {
    const fn base() -> u256
    const fn doubled() -> u256
}
impl Pair for Item {
    const fn base() -> u256 { 100 }
    const fn doubled() -> u256 { Self::base() * 2 }
}
pub impl Pair for Item as Big {
    const fn base() -> u256 { 1000 }
    const fn doubled() -> u256 { Self::base() * 2 }
}
mod m {
    use super::*
    with (Big)
    impl Pair for Item as Small {
        const fn base() -> u256 { 7 }
        const fn doubled() -> u256 { Self::base() * 2 }
    }
    pub const fn big() -> u256 { Item::doubled() }
    pub const fn small() -> u256 { with (Small) { Item::doubled() } }
}
const fn big() -> u256 { m::big() }
const fn small() -> u256 { m::small() }
"#,
        &[("big", 2000), ("small", 14)],
    );
}

#[test]
fn an_ordinary_implementations_methods_keep_their_own_implementation_nearest() {
    values(
        r#"
pub struct Item {}
pub trait Pair {
    const fn base() -> u256
    const fn doubled() -> u256
    const fn plus_one() -> u256
}
pub impl Pair for Item as Big {
    const fn base() -> u256 { 1000 }
    const fn doubled() -> u256 { Self::base() * 2 }
    const fn plus_one() -> u256 { Item::base() + 1 }
}
mod m {
    use super::*
    with (Big)
    impl Pair for Item {
        const fn base() -> u256 { 100 }
        const fn doubled() -> u256 { Self::base() * 2 }
        const fn plus_one() -> u256 { Item::base() + 1 }
    }
    pub const fn framed() -> u256 { Item::doubled() }
}
const fn outside_doubled() -> u256 { Item::doubled() }
const fn outside_plus_one() -> u256 { Item::plus_one() }
const fn framed() -> u256 { m::framed() }
"#,
        &[
            ("outside_doubled", 200),
            ("outside_plus_one", 101),
            ("framed", 2000),
        ],
    );
}

#[test]
fn trait_defaults_are_checked_in_the_module_that_writes_them() {
    // A default body belongs to its trait's module, whichever module
    // implements the trait or calls the method.
    let framed_trait = r#"
mod framed_trait {
    use super::*
    with (Discount)
    pub trait Seen2 { const fn seen() -> u256 { Item::value() } }
}
"#;
    let users = r#"
pub struct Plain {}
pub struct Mark {}
pub trait Seen { const fn seen() -> u256 { Item::value() } }
mod framed_impl {
    use super::*
    with (Discount)
    impl Seen for Plain {}
    pub const fn f() -> u256 { Plain::seen() }
}
impl framed_trait::Seen2 for Mark {}
use framed_trait::Seen2
mod other_frame {
    use super::*
    use super::framed_trait::Seen2
    with (Tiny)
    pub const fn f() -> u256 { Mark::seen() }
}
const fn plain_from_root() -> u256 { Plain::seen() }
const fn plain_from_framed_impl() -> u256 { framed_impl::f() }
const fn mark_from_root() -> u256 { Mark::seen() }
const fn mark_from_other_frame() -> u256 { other_frame::f() }
"#;
    for source in [
        with_picks(&[framed_trait, users].concat()),
        with_picks(&[users, framed_trait].concat()),
    ] {
        values(
            &source,
            &[
                ("plain_from_root", 100),
                ("plain_from_framed_impl", 100),
                ("mark_from_root", 70),
                ("mark_from_other_frame", 70),
            ],
        );
    }
}

#[test]
fn chosen_conditions_that_fail_do_not_fall_back() {
    let source = |body: &str, declaration: &str| {
        with_picks(&format!(
            r#"
pub trait Gate {{}}
pub struct Box<T> {{}}
impl<T: Pick> Pick for Box<T> {{ const fn value() -> u256 {{ T::value() + 1 }} }}
pub impl<T: Gate> Pick for Box<T> as RequireGate {{ const fn value() -> u256 {{ 0 }} }}
mod m {{
    use super::*
    {declaration}
    pub const fn f() -> u256 {{ {body} }}
}}
"#
        ))
    };
    let declared = diagnostics(&source("Box<Item>::value()", "with (RequireGate)"));
    let twin = diagnostics(&source("with (RequireGate) { Box<Item>::value() }", ""));
    // The declaration gives the diagnostics of its block twin: the chosen
    // implementation's failing condition is an error, and the ordinary
    // implementation is not used instead.
    assert_eq!(declared, twin);
    assert!(
        !declared.is_empty() && declared.iter().all(|code| code == "6-0003"),
        "{declared:?}"
    );
}

#[test]
fn contract_bodies_are_checked_under_the_frame() {
    let source = |declaration: &str, wrap: bool| {
        let body = if wrap {
            "with (RequireGate) { Box<Item>::value() }"
        } else {
            "Box<Item>::value()"
        };
        format!(
            r#"
pub struct Item {{}}
pub trait Pick {{ const fn value() -> u256 }}
impl Pick for Item {{ const fn value() -> u256 {{ 100 }} }}
pub trait Gate {{}}
pub struct Box<T> {{}}
impl<T: Pick> Pick for Box<T> {{ const fn value() -> u256 {{ T::value() + 1 }} }}
pub impl<T: Gate> Pick for Box<T> as RequireGate {{ const fn value() -> u256 {{ 0 }} }}
msg M {{
    #[selector = 0x10000001]
    Get -> u256,
}}
mod c {{
    use super::*
    {declaration}
    pub contract C {{
        init() {{ let _ = {body} }}
        recv M {{ Get -> u256 {{ {body} }} }}
    }}
}}
"#
        )
    };
    let declared = diagnostics(&source("with (RequireGate)", false));
    let twin = diagnostics(&source("", true));
    // `init` and `recv` are bodies of the module: each fails as it does in
    // the block twin. The test database has no standard library, so the
    // entry code reports the same codes in a module without a declaration;
    // the declaration adds the refusals of the chosen implementation.
    assert_eq!(declared, twin);
    let plain = diagnostics(&source("", false));
    let refusals = |codes: &[String]| codes.iter().filter(|code| *code == "6-0003").count();
    assert_eq!(
        refusals(&declared),
        refusals(&plain) + 4,
        "{declared:?} {plain:?}"
    );
}

#[test]
fn a_child_file_module_sees_the_declaration_of_its_parent_file() {
    use common::InputDb;
    use url::Url;
    let mut db = HirAnalysisTestDb::default();
    let mut touch = |path: &str, content: &str| {
        db.workspace().touch(
            &mut db,
            Url::parse(&format!("file:///module-with-files/{path}")).expect("valid test URL"),
            Some(content.to_string()),
        )
    };
    touch(
        "fe.toml",
        "[ingot]\nname = \"module_with_files\"\nversion = \"0.1.0\"\n",
    );
    touch(
        "src/lib.fe",
        r#"
pub struct Item {}
pub trait Price { const fn price() -> u256 }
impl Price for Item { const fn price() -> u256 { 100 } }
pub impl Price for Item as Discount { const fn price() -> u256 { 70 } }
with (Discount)
"#,
    );
    let child = touch(
        "src/a.fe",
        r#"
use ingot::Item
use ingot::Price
pub const fn p() -> u256 { Item::price() }
"#,
    );
    let (module, _) = db.top_mod(child);
    db.assert_no_diags(module);
    let owner = BodyOwner::Func(find_func(&db, module, "p"), None);
    let evaluated =
        eval_body_owner_const(&db, owner, GenericSubst::for_body_owner(&db, owner, vec![]));
    let value = evaluated.into_ready().expect("p evaluates");
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(&db)
    else {
        panic!("p was not an integer");
    };
    assert_eq!(value.to_usize(), Some(70));
}

// `where` conditions read the frame on a function only

const WHERE_FN: &str = "pub const fn g() -> u256 where { COND } { 1 }";
const WHERE_STRUCT: &str = "pub struct S where { COND } {}";
const WHERE_IMPL: &str = "pub struct S {}\n    impl S where { COND } {}";

fn where_diagnostics(frame: &str, item: &str, value: u16) -> Vec<String> {
    let item = item.replace("COND", &format!("Item::value() == {value}"));
    diagnostics(&with_picks(&format!(
        "mod m {{\n    use super::*\n    {frame}\n    {item}\n}}\n"
    )))
}

#[test]
fn a_function_where_condition_is_evaluated_under_the_modules_frame() {
    // Each refusal pairs with an acceptance, so the check is not vacuous.
    assert_eq!(
        where_diagnostics("with (Discount)", WHERE_FN, 100),
        vec!["8-0091"]
    );
    assert!(where_diagnostics("with (Discount)", WHERE_FN, 70).is_empty());
    assert!(where_diagnostics("", WHERE_FN, 100).is_empty());
    assert_eq!(where_diagnostics("", WHERE_FN, 70), vec!["8-0091"]);
}

#[test]
fn a_struct_or_implementation_where_condition_is_evaluated_under_the_ordinary_choice() {
    for item in [WHERE_STRUCT, WHERE_IMPL] {
        assert_eq!(
            where_diagnostics("with (Discount)", item, 70),
            vec!["8-0091"],
            "{item}"
        );
        assert!(
            where_diagnostics("with (Discount)", item, 100).is_empty(),
            "{item}"
        );
    }
}

// Both forms accept a bound made concrete by an associated type.

fn bound_made_concrete(frame: &str, body: &str) -> String {
    format!(
        r#"
mod m {{
    pub struct Marker {{}}
    pub trait Choice {{ fn value() -> u64 }}
    impl Choice for Marker as W {{ fn value() -> u64 {{ 1 }} }}
    {frame}
    pub trait Tr {{
        type A: Copy
        const fn get(self) -> Self::A
    }}
    impl Tr for Marker {{
        type A = u8
        const fn get(self) -> u8 {{ 7 }}
    }}
    pub const fn f<B: Tr<A = u8>>(b: B) -> u8 {body}
    pub const fn run() -> u8 {{ f(b: Marker {{}}) }}
}}
const fn run() -> u8 {{ m::run() }}
"#
    )
}

#[test]
fn a_bound_made_concrete_by_an_associated_type_is_accepted_in_a_block() {
    values(
        &bound_made_concrete("", "{ with (W) { b.get() } }"),
        &[("run", 7)],
    );
}

#[test]
fn a_bound_made_concrete_by_an_associated_type_is_accepted_in_a_module() {
    values(
        &bound_made_concrete("with (W)", "{ b.get() }"),
        &[("run", 7)],
    );
}

// Twins of the concrete-slot and output rules

/// The report of the semantic access analysis, which refuses a proof that two
/// roots or two outputs of a bound disagree where the instance is built.
fn access_report(source: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("module_with_access.fe".into(), source);
    let (module, _) = db.top_mod(file);
    let mut passes = AnalysisPassManager::new();
    passes.add_module_pass("SemanticAccess", Box::new(SemanticAccessAnalysisPass));
    format_diagnostics(&db, &passes.run_on_module(&db, module))
}

const TWO_ROOTS: &str = r#"
pub trait Show { fn show() -> u256 }
pub struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
pub impl Show for Foo as Loud { fn show() -> u256 { 70 } }
pub struct W<T> {}
pub struct V<T> {}
pub trait Tr {
    type A: Show
    fn via() -> u256
}
impl<T: Show> Tr for W<T> {
    type A = T
    fn via() -> u256 { T::show() }
}
pub trait Tr2 {
    type A: Show
    fn via2() -> u256
}
impl<T: Show> Tr2 for V<T> {
    type A = T
    fn via2() -> u256 { T::show() }
}
fn f<B: Tr<A = Foo>, C: Tr2<A = Foo>>() -> u256 { Foo::show() * 1000000 + B::via() * 1000 + C::via2() }
"#;

#[test]
fn components_that_disagree_are_refused_in_a_module_as_in_a_block() {
    let block = [
        TWO_ROOTS,
        "fn relay<C: Tr2<A = Foo>>() -> u256 { with (Loud) { f<W<Foo>, C>() } }\nfn disagree() -> u256 { relay<V<Foo>>() }\nfn demand() -> u256 { disagree() }\n",
    ]
    .concat();
    let module = [
        TWO_ROOTS,
        "mod r {\n    use super::*\n    with (Loud)\n    pub fn relay<C: Tr2<A = Foo>>() -> u256 { f<W<Foo>, C>() }\n}\nfn disagree() -> u256 { r::relay<V<Foo>>() }\nfn demand() -> u256 { disagree() }\n",
    ]
    .concat();
    for source in [block, module] {
        assert!(diagnostics(&source).is_empty());
        assert!(access_report(&source).contains("error[16-0019]"));
    }
}

const OUTPUT_DISAGREEMENT: &str = r#"
trait Marker {}
trait Foo {
    type Out
    fn make() -> Self::Out
}
impl<T: Marker> Foo for T {
    type Out = u256
    fn make() -> u256 { 1 }
}
struct S {}
impl Foo for S {
    type Out = u8
    fn make() -> u8 { 2 }
}
impl Marker for S as FakeMarker {}
fn gen<T: Marker>() -> <T as Foo>::Out { <T as Foo>::make() }
"#;

#[test]
fn outputs_that_disagree_are_refused_in_a_module_as_in_a_block() {
    let block = [
        OUTPUT_DISAGREEMENT,
        "fn boom() -> u8 { with (FakeMarker) { gen<S>() } }\n",
    ]
    .concat();
    let module = [
        OUTPUT_DISAGREEMENT,
        "mod m {\n    use super::*\n    with (FakeMarker)\n    pub fn boom() -> u8 { gen<S>() }\n}\n",
    ]
    .concat();
    for source in [block, module] {
        assert!(diagnostics(&source).is_empty());
        assert!(access_report(&source).contains("error[16-0020]"));
    }
}

#[test]
fn a_chosen_implementation_whose_conditions_fail_is_refused_in_a_module_as_in_a_block() {
    let block = [
        OPERANDS,
        "fn f() { with (Broken) { let _: NeedsSub<Item> } }\n",
    ]
    .concat();
    let module = [
        OPERANDS,
        "mod m {\n    use super::*\n    with (Broken)\n    pub fn f() { let _: NeedsSub<Item> }\n}\n",
    ]
    .concat();
    assert_eq!(sorted_codes(&block), ["6-0003"]);
    assert_eq!(sorted_codes(&module), ["6-0003"]);
}

const COMPANION: &str = r#"
pub trait Codec { type Wire
    fn enc(_ x: u256) -> Self::Wire }
pub struct Wsol {}
impl Codec for Wsol { type Wire = u256
    fn enc(_ x: u256) -> u256 { x } }
"#;

/// A module's `with` never decides a type: a closed projection reads the
/// companion implementation, in the framed module and outside it.
#[test]
fn a_module_with_does_not_decide_a_type() {
    let accepted = [
        COMPANION,
        r#"
pub impl Codec for Wsol as Shifted { type Wire = u256
    fn enc(_ x: u256) -> u256 { x + 1000 } }
pub mod a { use super::*
    with (Shifted)
    pub type WA = <Wsol as Codec>::Wire }
pub mod b { use super::*
    pub type WB = <Wsol as Codec>::Wire }
fn same(_ x: a::WA) -> b::WB { x }
"#,
    ]
    .concat();
    assert!(
        diagnostics(&accepted).is_empty(),
        "{:?}",
        diagnostics(&accepted)
    );
    let shifted_wire = |frame: &str| {
        sorted_codes(
            &[
                COMPANION,
                &format!(
                    r#"
pub impl Codec for Wsol as Shifted {{ type Wire = u8
    fn enc(_ x: u256) -> u8 {{ 1 }} }}
pub mod a {{ use super::*
    {frame}
    pub type WA = <Wsol as Codec>::Wire }}
"#
                ),
            ]
            .concat(),
        )
    };
    assert_eq!(shifted_wire(""), ["5-0005"]);
    assert_eq!(shifted_wire("with (Shifted)"), ["5-0005"]);
}
