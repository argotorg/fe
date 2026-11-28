//! A module's `with` declaration: the outermost frame of every body of its
//! module.
use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
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
use url::Url;

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

/// Each diagnostic as its code and the source text its primary label covers,
/// in the order of the codes.
fn located(source: &str) -> Vec<(String, String)> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("module_with.fe".into(), source);
    let (module, _) = db.top_mod(file);
    let mut found: Vec<_> = fe_hir::analysis::initialize_analysis_pass()
        .run_on_module(&db, module)
        .into_iter()
        .map(|diagnostic| {
            let diagnostic = diagnostic.to_complete(&db);
            let range = diagnostic
                .first_span()
                .expect("a diagnostic with a span")
                .range;
            let text = &source[usize::from(range.start())..usize::from(range.end())];
            (diagnostic.error_code.to_string(), text.to_string())
        })
        .collect();
    found.sort();
    found
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
/// built without reading `K`. A constant whose value reads the module's frame
/// is refused in a type (8-0138), and the header `impl Size for Arr<{ K }>`
/// is a type.
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
fn a_declaration_whose_opening_reads_its_modules_constant_is_refused_at_the_header() {
    assert_eq!(sorted_codes(&with_picks(SELF_READING)), ["8-0138"]);
}

/// The same program with a type and a reader of `m::K` before the module:
/// the entry order changes nothing. The struct field and the header are each
/// refused.
#[test]
fn the_order_of_the_readers_of_a_framed_constant_changes_nothing() {
    assert_eq!(
        sorted_codes(&with_picks(&format!(
            "{SELF_READING}\npub struct S {{ pub a: Arr<{{ m::K }}> }}\nconst fn early() -> u256 {{ m::K }}\n"
        ))),
        ["8-0138", "8-0138"]
    );
}

/// The frame is built from names and deferred lowering, so the headers
/// `impl Size for Arr<{ K }>` and `impl Pick for Arr<{ K }>` are not read
/// while `K = Item::value()` is checked under it. `K` asks a `Pick` goal,
/// which the `Size` operand never answers. Both headers read the constant `K`,
/// whose value reads the frame, so both are refused (8-0138), and nothing
/// iterates or panics.
#[test]
fn a_module_frame_is_built_without_evaluating_its_constants() {
    assert_eq!(
        sorted_codes(
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
"#
        ),
        ["8-0138", "8-0138"]
    );
}

/// The same headers written through a constant function, which reads the
/// frame's `K` (70) and is accepted. The frame is still built without reading
/// the constants, and the operand chooses the header for `Arr<70>`.
#[test]
fn a_header_through_a_constant_function_reads_the_frames_value() {
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
        pub const fn kk() -> u256 { K }
        impl Size for Arr<{ kk() }> { const fn size() -> u256 { 2 } }
        impl Pick for Arr<{ kk() }> { const fn value() -> u256 { 3 } }
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

/// An operand whose implementation needs a value that is checked under the
/// module's own frame has no consistent meaning: the value decides which
/// implementation answers, and the implementation decides the value. The
/// operand is refused where the cycle meets it, whatever way the cycle runs.
///
/// The header `impl Size for Arr<{ K }>` reads a constant whose own check asks
/// the operand's trait.
const HEADER_CYCLE: &str = r#"
pub struct Arr<const N: u256> {}
pub trait Size { const fn size() -> u256 }
pub impl Size for Arr<70> as S70 { const fn size() -> u256 { 70 } }
pub mod outer {
    use super::*
    with (S70)
    pub mod m {
        use super::*
        with (<Arr<70> as Size>)
        pub const K: u256 = Arr<70>::size()
        impl Size for Arr<{ K }> { const fn size() -> u256 { 100 } }
    }
}
"#;

fn at_the_operand(code: &str, operand: &str) -> Vec<(String, String)> {
    vec![(code.to_string(), operand.to_string())]
}

#[test]
fn a_header_constant_that_asks_its_own_operand_is_refused_at_the_operand() {
    // The header also reads a constant whose value reads the frame (8-0138).
    assert_eq!(
        located(HEADER_CYCLE),
        [
            ("2-0022".to_string(), "<Arr<70> as Size>".to_string()),
            ("8-0138".to_string(), "Arr<{ K }>".to_string()),
        ]
    );
}

/// The same program with a type that reads the constant, written before the
/// module: the order in which the constant is first read changes nothing.
#[test]
fn the_order_of_the_first_reader_does_not_change_a_refused_operand() {
    let source = format!(
        "pub struct S {{ pub a: Arr<{{ outer::m::K }}> }}\n{}",
        HEADER_CYCLE
    );
    assert_eq!(
        located(&source),
        [
            ("2-0022".to_string(), "<Arr<70> as Size>".to_string()),
            ("8-0138".to_string(), "Arr<{ K }>".to_string()),
            ("8-0138".to_string(), "Arr<{ outer::m::K }>".to_string()),
        ]
    );
}

/// An implementation header that reads a constant whose own check asks the
/// operand's trait, with another header in the program.
#[test]
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
    // Each header reads the constant (8-0138).
    assert_eq!(codes, ["2-0022", "8-0138", "8-0138"]);
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
fn an_operand_that_reads_a_constant_of_its_own_check_is_refused() {
    let source = format!(
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
    );
    // The operand's own type reads the constant as well (8-0138).
    assert_eq!(
        located(&source),
        [
            ("2-0022".to_string(), "<Arr<{ K }> as Pick>".to_string()),
            ("8-0138".to_string(), "Arr<{ K }>".to_string()),
        ]
    );
}

#[test]
fn an_operand_that_calls_a_function_of_its_own_check_is_refused() {
    let source = format!(
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
    );
    assert_eq!(
        located(&source),
        at_the_operand("2-0022", "<Arr<{ kf() }> as Pick>")
    );
}

/// The constant reaches the operand through an alias that the operand names.
#[test]
fn an_operand_that_reads_the_constant_through_an_alias_is_refused_at_the_operand() {
    let source = r#"
pub struct Arr<const N: u256> {}
pub trait Pick { const fn value() -> u256 }
impl Pick for Arr<70> { const fn value() -> u256 { 100 } }
impl Pick for Arr<100> { const fn value() -> u256 { 100 } }
pub impl Pick for Arr<70> as Seventy { const fn value() -> u256 { 70 } }
pub mod outer {
    use super::*
    with (Seventy)
    pub mod m {
        use super::*
        pub type A = Arr<K>
        with (<A as Pick>)
        pub const K: u256 = Arr<70>::value()
    }
}
"#;
    let found = located(source);
    assert!(
        found.contains(&("2-0022".to_string(), "<A as Pick>".to_string())),
        "{found:?}"
    );
}

/// A named implementation whose parameter has a default that nothing applies is
/// not an implementation of its trait for any type: the operand names it, and
/// a body that asks for the trait finds none. That is so whatever the default
/// reads, in a module and in a block alike, and has nothing to do with the
/// frame: the operand is chosen and is no cycle.
#[test]
fn a_named_implementation_with_an_unapplied_default_is_not_chosen_in_a_module_as_in_a_block() {
    let program = |frame: &str, body: &str| {
        format!(
            r#"
pub struct Item {{}}
pub trait Pick {{ const fn value() -> u256 }}
impl Pick for Item {{ const fn value() -> u256 {{ 100 }} }}
pub impl<const N: u256 = {{ 70 + 1 }}> Pick for Item as D {{ const fn value() -> u256 {{ N }} }}
pub mod m {{
    use super::*
    {frame}
    pub const fn f() -> u256 {{ {body} }}
}}
"#
        )
    };
    let mut module = sorted_codes(&program("with (D)", "Item::value()"));
    let mut block = sorted_codes(&program("", "with (D) { Item::value() }"));
    module.dedup();
    block.dedup();
    assert_eq!(block, ["6-0003"]);
    assert_eq!(module, block);
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

// A header's requirements are proved at the header, under its module's frame

/// The implementation `N` of `Tr` is declared in a framed module. The proof
/// that its associated type `Foo` implements `Show` is made where `N` is
/// declared, under that module's frame, so a reader outside the module gets
/// the framed `show` (70) and `N`'s own method gets the same (70).
#[test]
fn the_components_of_an_implementation_in_a_framed_module_are_proved_under_its_frame() {
    values(
        r#"
pub trait Show { const fn show() -> u256 }
pub struct Foo {}
impl Show for Foo { const fn show() -> u256 { 100 } }
pub impl Show for Foo as Loud { const fn show() -> u256 { 70 } }
pub trait Tr {
    type A: Show
    const fn via() -> u256
}
pub mod n {
    use super::*
    with (Loud)
    pub struct N {}
    impl Tr for N {
        type A = Foo
        const fn via() -> u256 { Foo::show() }
    }
}
const fn read<B: Tr>() -> u256 { <B::A as Show>::show() * 1000 + B::via() }
const fn f() -> u256 { read<n::N>() }
"#,
        &[("f", 70070)],
    );
}

/// A second root of the same component, passed ordinarily, proves `Foo: Show`
/// with the ordinary implementation: the two roots disagree where they meet.
#[test]
fn a_component_proved_under_a_frame_disagrees_with_an_ordinary_one() {
    let source = [
        TWO_ROOTS,
        r#"
pub mod n {
    use super::*
    with (Loud)
    pub struct N {}
    impl Tr for N {
        type A = Foo
        fn via() -> u256 { Foo::show() }
    }
}
fn disagree() -> u256 { f<n::N, V<Foo>>() }
fn demand() -> u256 { disagree() }
"#,
    ]
    .concat();
    assert!(diagnostics(&source).is_empty());
    assert!(access_report(&source).contains("error[16-0019]"));
}

/// The implementation of `Tr` for `n::N` is declared in a framed module, and
/// the dictionary that reaches it is an ordinary one: `Bx: Holder` selects no
/// implementation of a framed module and holds no named implementation. The
/// proof that `n::N`'s associated type implements `Show` is still made under
/// the frame of `n`, so `Foo: Show` is `Loud` through `H` and the ordinary
/// implementation through `C`. The two routes disagree where they meet.
fn holder_program(framed: bool, swapped: bool) -> String {
    let frame = if framed { "with (Loud)" } else { "" };
    let (bounds, arguments) = if swapped {
        ("C: Tr2<A = Foo>, H: Holder<Out = n::N>", "V<Foo>, Bx")
    } else {
        ("H: Holder<Out = n::N>, C: Tr2<A = Foo>", "Bx, V<Foo>")
    };
    [
        TWO_ROOTS,
        &format!(
            r#"
pub trait Holder {{ type Out: Tr }}
pub struct Bx {{}}
impl Holder for Bx {{ type Out = n::N }}
pub mod n {{
    use super::*
    {frame}
    pub struct N {{}}
    impl Tr for N {{
        type A = Foo
        fn via() -> u256 {{ Foo::show() }}
    }}
}}
fn read<B: Tr>() -> u256 {{ <B::A as Show>::show() }}
fn f2<{bounds}>() -> u256 {{ Foo::show() * 1000000 + read<H::Out>() * 1000 + C::via2() }}
fn disagree() -> u256 {{ f2<{arguments}>() }}
fn demand() -> u256 {{ disagree() }}
"#
        ),
    ]
    .concat()
}

#[test]
fn a_component_reached_through_an_ordinary_dictionary_is_proved_under_its_frame() {
    for swapped in [false, true] {
        let source = holder_program(true, swapped);
        assert!(diagnostics(&source).is_empty());
        let report = access_report(&source);
        assert!(report.contains("error[16-0019]"), "{report}");
        assert!(
            report.contains("`Loud` through `H: Holder<Out = N>`"),
            "{report}"
        );
    }
    let control = holder_program(false, false);
    assert!(diagnostics(&control).is_empty());
    let report = access_report(&control);
    assert!(!report.contains("error["), "{report}");
}

/// `L` adds a level of associated types. The chain below `g`'s equality is
/// `n::N` under four `L`s: the walk follows four levels and stops below
/// `n::N: Step`, so the dictionary of `Foo: Show` that `n::N` holds under its
/// frame is not read. It is refused, as the block form is, and not given the
/// ordinary value. With three `L`s the bound is read and the frame's value
/// is given.
fn depth_program(levels: usize, bx_in_module: bool, constant: bool) -> String {
    let fn_kw = if constant { "const fn" } else { "fn" };
    let chain = |inner: &str| {
        let mut ty = inner.to_string();
        for _ in 0..levels {
            ty = format!("L<{ty}>");
        }
        ty
    };
    let (framed_chain, root_chain) = (chain("n::N"), chain("N"));
    let (bx_in_n, bx_in_root) = if bx_in_module {
        (
            format!(
                "    pub struct Bx {{}}\n    impl Holder for Bx {{ type Out = {root_chain} }}\n"
            ),
            String::new(),
        )
    } else {
        (
            String::new(),
            format!("pub struct Bx {{}}\nimpl Holder for Bx {{ type Out = {framed_chain} }}\n"),
        )
    };
    let call = if bx_in_module { "n::Bx" } else { "Bx" };
    format!(
        r#"
pub trait Show {{ {fn_kw} show() -> u256 }}
pub struct Foo {{}}
impl Show for Foo {{ {fn_kw} show() -> u256 {{ 100 }} }}
pub impl Show for Foo as Loud {{ {fn_kw} show() -> u256 {{ 70 }} }}
pub struct Bar {{}}
impl Show for Bar {{ {fn_kw} show() -> u256 {{ 1 }} }}
pub trait Step {{
    type Next: Step
    type A: Show
}}
pub struct L<T> {{}}
impl<T: Step> Step for L<T> {{
    type Next = T
    type A = Bar
}}
pub trait Holder {{ type Out: Step }}
{bx_in_root}pub mod n {{
    use super::*
    with (Loud)
    pub struct N {{}}
    impl Step for N {{
        type Next = N
        type A = Foo
    }}
{bx_in_n}}}
{fn_kw} g<H: Holder<Out = {framed_chain}>>() -> u256 {{ Foo::show() }}
{fn_kw} use_it() -> u256 {{ g<{call}>() }}
{fn_kw} demand() -> u256 {{ use_it() }}
"#
    )
}

#[test]
fn a_component_below_the_followed_levels_of_a_framed_dictionary_is_refused() {
    for bx_in_module in [true, false] {
        let source = depth_program(4, bx_in_module, false);
        assert!(diagnostics(&source).is_empty());
        let report = access_report(&source);
        assert!(report.contains("error[16-0019]"), "{report}");
        assert!(
            report.contains("lies below the four associated-type levels"),
            "{report}"
        );
        assert!(
            report.contains("a module-level `with` can choose"),
            "{report}"
        );
    }
    values(&depth_program(3, true, true), &[("use_it", 70)]);
}

/// A chosen implementation whose conditions fail at the header of the
/// implementation that reads it is an error, not a fallback to the ordinary
/// implementation.
fn failing_choice_program(failing: bool) -> String {
    let condition = if failing { " where Foo: Missing" } else { "" };
    format!(
        r#"
pub trait Show {{ const fn show() -> u256 }}
pub trait Missing {{}}
pub struct Foo {{}}
impl Show for Foo {{ const fn show() -> u256 {{ 100 }} }}
pub impl Show for Foo as Broken{condition} {{ const fn show() -> u256 {{ 70 }} }}
pub trait Tr {{
    type A: Show
    const fn via() -> u256
}}
pub mod n {{
    use super::*
    with (Broken)
    pub struct N {{}}
    impl Tr for N {{
        type A = Foo
        const fn via() -> u256 {{ 1 }}
    }}
}}
const fn read<B: Tr>() -> u256 {{ <B::A as Show>::show() }}
const fn f() -> u256 {{ read<n::N>() }}
"#
    )
}

#[test]
fn a_frame_choice_that_fails_at_a_header_is_not_replaced_by_the_ordinary_one() {
    let report = access_report(&failing_choice_program(true));
    assert!(report.contains("error[16-0019]"), "{report}");
    values(&failing_choice_program(false), &[("f", 70)]);
}

/// The associated types of `n::N1` grow without end, so the walk below a
/// dictionary of `g` returns to a projection that is still being computed.
/// `Leaf: Br` is chosen by the frame of `n` (`LN`, 2) and not by ordinary
/// resolution (1): the route is refused and the program ends.
#[test]
fn a_cycle_below_a_framed_dictionary_is_refused() {
    let source = r#"
pub trait Br {
    type A: Br
    type B: Br
    fn v() -> u256
}
pub struct Leaf {}
impl Br for Leaf {
    type A = Leaf
    type B = Leaf
    fn v() -> u256 { 1 }
}
pub impl Br for Leaf as LN {
    type A = Leaf
    type B = Leaf
    fn v() -> u256 { 2 }
}
pub mod n {
    use super::*
    with (LN)
    pub struct N1<T> {}
    pub struct N2<T> {}
    impl<T: Br> Br for N1<T> {
        type A = N1<N1<T>>
        type B = Leaf
        fn v() -> u256 { T::v() }
    }
    impl<T: Br> Br for N2<T> {
        type A = N1<N2<T>>
        type B = N2<N2<T>>
        fn v() -> u256 { T::v() }
    }
}
pub trait Hb { type O: Br }
pub struct Bx<T> {}
impl<T: Br> Hb for Bx<T> { type O = n::N1<T> }
fn g<H: Hb<O = n::N1<Leaf>>>() -> u256 { <H::O as Br>::v() }
fn plain() -> u256 { g<Bx<Leaf>>() }
fn demand() -> u256 { plain() }
"#;
    assert!(diagnostics(source).is_empty());
    let report = access_report(source);
    assert!(report.contains("error[16-0019]"), "{report}");
}

/// Pinned limit. A chain of five associated-type levels below an equality is
/// past the four levels that are followed. No implementation is named, so in
/// a program with no module-level `with` the route is not read and ordinary
/// resolution answers it. Where a module-level `with` is visible the route is
/// refused, because the walk cannot say whether a frame answers below it. A
/// later rule that follows the traits a frame can answer would accept more
/// programs.
#[test]
fn a_visible_module_with_refuses_an_unfollowed_route() {
    let chain = r#"
pub trait Chain {
    type Next: Chain
    fn v() -> u256
}
pub struct End {}
impl Chain for End {
    type Next = End
    fn v() -> u256 { 0 }
}
pub struct L<T> {}
impl<T: Chain> Chain for L<T> {
    type Next = T
    fn v() -> u256 { T::v() + 1 }
}
pub trait Holder { type Out: Chain }
pub struct Bx<T> {}
impl<T: Chain> Holder for Bx<T> { type Out = L<L<L<L<L<T>>>>> }
fn g<H: Holder<Out = L<L<L<L<L<End>>>>>>>() -> u256 { <H::Out as Chain>::v() }
fn go() -> u256 { g<Bx<End>>() }
fn demand() -> u256 { go() }
"#;
    let visible = [
        chain,
        "mod m {\n    use super::*\n    pub trait Q {}\n    pub struct Z {}\n    pub impl Q for Z as Zq {}\n    with (Zq)\n}\n",
    ]
    .concat();
    assert!(diagnostics(&visible).is_empty());
    let report = access_report(&visible);
    assert!(report.contains("error[16-0019]"), "{report}");
    assert!(diagnostics(chain).is_empty());
    let report = access_report(chain);
    assert!(!report.contains("error["), "{report}");
}

fn touch(db: &mut HirAnalysisTestDb, url: &str, content: &str) -> File {
    db.workspace().touch(
        db,
        Url::parse(url).expect("valid test URL"),
        Some(content.to_string()),
    )
}

/// The access report of the root module of an ingot `app` that depends on an
/// ingot `dep`.
fn two_ingot_report(dep: &str, app: &str) -> String {
    let mut db = HirAnalysisTestDb::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    touch(
        &mut db,
        "file:///module-with-two-ingots/dep/fe.toml",
        "[ingot]\nname = \"dep\"\nversion = \"0.1.0\"\n",
    );
    touch(
        &mut db,
        "file:///module-with-two-ingots/dep/src/lib.fe",
        dep,
    );
    touch(
        &mut db,
        "file:///module-with-two-ingots/app/fe.toml",
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n\n[dependencies]\ndep = { path = \"../dep\" }\n",
    );
    let app = touch(
        &mut db,
        "file:///module-with-two-ingots/app/src/lib.fe",
        app,
    );
    let (module, _) = db.top_mod(app);
    db.assert_no_diags(module);
    let mut passes = AnalysisPassManager::new();
    passes.add_module_pass("SemanticAccess", Box::new(SemanticAccessAnalysisPass));
    format_diagnostics(&db, &passes.run_on_module(&db, module))
}

/// A trait constant is read inside a dependency, and its instance is built at
/// types of the root ingot. The solve context of such an instance is the
/// scope where the constant is written, in the dependency, which sees no
/// module-level `with`. The frame of the root ingot answers `N: Tr` with `X`
/// through `H` and the ordinary implementation answers it through `J`, so the
/// routes disagree, as they do when the program is one file.
const TWO_INGOT_DEP: &str = r#"
pub trait Tr { const fn v() -> u256 }
pub trait Holder { type Out: Tr }
pub trait Holder2 { type Out: Tr }
pub trait K { const C: u256 }
pub struct W<T, H, J> {}
impl<T: Tr, H: Holder<Out = T>, J: Holder2<Out = T>> K for W<T, H, J> { const C: u256 = T::v() }
pub fn k<X: K>() -> u256 { X::C }
"#;

const TWO_INGOT_APP: &str = r#"
pub struct N {}
impl Tr for N { const fn v() -> u256 { 100 } }
pub impl Tr for N as X { const fn v() -> u256 { 70 } }
pub mod n {
    use super::{Holder, N, X}
    FRAME
    pub struct Bx {}
    impl Holder for Bx { type Out = N }
}
pub struct Ky {}
impl Holder2 for Ky { type Out = N }
fn via_dep() -> u256 { k<W<N, n::Bx, Ky>>() }
fn demand() -> u256 { via_dep() }
"#;

#[test]
fn a_frame_of_the_root_ingot_is_seen_by_a_trait_constant_read_in_a_dependency() {
    let app =
        |frame: &str, imports: &str| [imports, &TWO_INGOT_APP.replace("FRAME", frame)].concat();
    let imports = "use dep::{Holder, Holder2, K, Tr, W, k}\n";
    let report = two_ingot_report(TWO_INGOT_DEP, &app("with (X)", imports));
    assert!(report.contains("error[16-0019]"), "{report}");
    assert!(report.contains("receives `X`"), "{report}");
    let report = two_ingot_report(TWO_INGOT_DEP, &app("", imports));
    assert!(!report.contains("error["), "{report}");
    // The same program in one file.
    for (frame, refused) in [("with (X)", true), ("", false)] {
        let source = [TWO_INGOT_DEP, "\n", &TWO_INGOT_APP.replace("FRAME", frame)].concat();
        let report = access_report(&source);
        assert_eq!(report.contains("error[16-0019]"), refused, "{report}");
    }
}

/// Pinned. The chain and `g` are in a dependency, and the frame is in the root
/// ingot. The bound of `g` names its innermost type as a parameter, so no route
/// reaches the `Foo: Show` that `g` reads, and the ordinary value is the
/// program's value, as it is when the program is one file.
#[test]
fn a_chain_over_a_parameter_in_a_dependency_keeps_the_ordinary_value() {
    let dep = r#"
pub trait Show { fn show() -> u256 }
pub struct Foo {}
impl Show for Foo { fn show() -> u256 { 100 } }
pub impl Show for Foo as Loud { fn show() -> u256 { 70 } }
pub struct Bar {}
impl Show for Bar { fn show() -> u256 { 1 } }
pub trait Step {
    type Next: Step
    type A: Show
}
pub struct L<T> {}
impl<T: Step> Step for L<T> {
    type Next = T
    type A = Bar
}
pub trait Holder { type Out: Step }
pub fn g<T: Step, H: Holder<Out = L<L<L<L<T>>>>>>() -> u256 { Foo::show() }
"#;
    let app = r#"
use dep::{Foo, Holder, L, Loud, Show, Step, g}
pub mod n {
    use dep::{Foo, Holder, L, Loud, Show, Step}
    with (Loud)
    pub struct N {}
    impl Step for N {
        type Next = N
        type A = Foo
    }
    pub struct Bx {}
    impl Holder for Bx { type Out = L<L<L<L<N>>>> }
}
fn use_it() -> u256 { g<n::N, n::Bx>() }
fn demand() -> u256 { use_it() }
"#;
    let report = two_ingot_report(dep, app);
    assert!(!report.contains("error["), "{report}");
}

/// The operator `+` on a type that has nothing to do with the operand
/// `<Arr<100> as Add2>` does not choose it, though `Add2` declares a method
/// named `add`: the operand's implementation reads the constant that the
/// operator is checked in.
#[test]
fn a_declared_operand_for_another_type_is_not_chosen_by_an_operator() {
    values(
        r#"
use core::ops::Add
pub struct Arr<const N: u256> {}
pub struct Money { pub v: u256 }
impl Add for Money {
    type Output = Money
    const fn add(own self, _ other: own Money) -> Money { Money { v: self.v + other.v } }
}
pub trait Add2 { const fn add(self) -> u256 }
pub mod m {
    use super::*
    with (<Arr<100> as Add2>)
    pub const K: u256 = {
        let a = Money { v: 1 }
        let b = Money { v: 2 }
        let s = a + b
        s.v + 97
    }
    impl Add2 for Arr<{ K }> { const fn add(self) -> u256 { 2 } }
    pub const fn f() -> u256 { K }
}
const fn f() -> u256 { m::f() }
"#,
        &[("f", 100)],
    );
}

// Constants refused by the constant

/// Module `a` is framed and its constants read the frame (`Discount`, 70, not
/// the ordinary 100). Module `b` is not framed and uses them in types.
fn constants_of_a_framed_module(uses: &str) -> String {
    format!(
        r#"
pub struct Item {{}}
pub trait CPick {{ const fn value() -> u256 }}
impl CPick for Item {{ const fn value() -> u256 {{ 100 }} }}
pub impl CPick for Item as Discount {{ const fn value() -> u256 {{ 70 }} }}
pub trait HasN {{ const N: u256 }}
pub struct Arr<const N: u256> {{}}
impl<const N: u256> Arr<N> {{ pub const fn n(self) -> u256 {{ N }} }}
pub mod a {{
    use super::*
    with (Discount)
    pub const K: u256 = <Item as CPick>::value()
    pub const K5: u256 = 5
    pub struct Bar {{}}
    impl HasN for Bar {{ const N: u256 = <Item as CPick>::value() }}
    impl Bar {{ pub const X: u256 = <Item as CPick>::value() }}
}}
pub mod b {{
    use super::*
    pub const K4: u256 = a::K
    pub const fn kk() -> u256 {{ a::K }}
    {uses}
}}
const fn run() -> u256 {{ b::f() }}
"#
    )
}

fn use_in_a_type(ty: &str) -> String {
    constants_of_a_framed_module(&format!(
        "pub const fn f() -> u256 {{ let v: {ty} = Arr {{}}\n    v.n() }}"
    ))
}

#[test]
fn a_constant_that_reads_a_module_frame_is_refused_in_a_type_of_any_module() {
    for ty in [
        "Arr<a::K>",
        "Arr<{ a::K }>",
        "Arr<{ <a::Bar as HasN>::N }>",
        "Arr<{ a::Bar::X }>",
        "Arr<K4>",
    ] {
        assert_eq!(sorted_codes(&use_in_a_type(ty)), ["8-0138"], "{ty}");
    }
}

/// A constant function reads the frame of its own module, and so does a
/// constant that reads no choice: neither is refused in a type.
#[test]
fn a_constant_function_and_a_constant_that_reads_no_choice_are_accepted_in_a_type() {
    for ty in ["Arr<{ kk() }>", "Arr<a::K5>"] {
        assert_eq!(
            sorted_codes(&use_in_a_type(ty)),
            Vec::<String>::new(),
            "{ty}"
        );
    }
    values(&use_in_a_type("Arr<{ kk() }>"), &[("run", 70)]);
    values(&use_in_a_type("Arr<a::K5>"), &[("run", 5)]);
}

/// A constant that holds a block `with` is not a read of a module frame: the
/// block form of the same constants is unchanged.
#[test]
fn constants_read_through_a_block_are_unchanged() {
    let source = |ty: &str| {
        with_picks(&format!(
            r#"
pub struct Arr<const N: u256> {{}}
impl<const N: u256> Arr<N> {{ pub const fn n(self) -> u256 {{ N }} }}
pub const K2: u256 = with (Discount) {{ <Item as Pick>::value() }}
pub const K3: u256 = K2
const fn kk() -> u256 {{ K2 }}
pub const fn f() -> u256 {{ let a: {ty} = Arr {{}}
    a.n() }}
"#
        ))
    };
    for ty in ["Arr<{ K2 }>", "Arr<K3>", "Arr<{ kk() }>"] {
        values(&source(ty), &[("f", 70)]);
    }
}

#[test]
fn module_level_types_that_read_a_choice_are_refused() {
    assert_eq!(
        diagnostics(&with_picks(
            r#"
pub struct Array<const N: u256> {}
mod m {
    use super::*
    with (Discount)
    pub struct Holder { pub a: Array<{ Item::value() }> }
    pub fn sig(_ a: Array<{ read<Item>() }>) {}
    type Count = Array<{ Item::value() }>
    pub fn body() { let x: Array<{ Item::value() }> = Array {} }
}
"#,
        )),
        vec!["8-0138"; 5]
    );
}

/// A default declared in an unframed module takes the ordinary value where a
/// signature uses it; a body of the framed module that uses it is refused.
#[test]
fn defaults_read_under_the_frame_are_refused_where_they_are_used() {
    assert_eq!(
        diagnostics(&with_picks(
            r#"
pub struct Arr<const L: u256 = { Item::value() }> {}
mod m {
    use super::*
    with (Discount)
    pub fn sig(_ a: Arr) {}
    pub fn body() { let x: Arr = Arr {} }
}
"#,
        )),
        vec!["8-0138"; 2]
    );
}

#[test]
fn a_framed_constant_reaching_a_chosen_implementation_through_another_constant_is_refused() {
    assert_eq!(
        diagnostics(&with_picks(
            r#"
pub struct Arr<const N: u256> {}
mod m {
    use super::*
    with (Discount)
    pub const K: u256 = Item::value()
    pub const J: u256 = K + 1
    pub fn sig(_ a: Arr<{ J }>) {}
}
"#,
        )),
        vec!["8-0138"]
    );
}

/// `kk()` reads `K`, sealed at the module's choice (70), and an explicit
/// ordinary opening reads the ordinary value (100): the types differ.
#[test]
fn const_fn_reads_of_constants_and_explicit_ordinary_openings_in_types_are_accepted() {
    let source = |body: &str| {
        with_picks(&format!(
            r#"
pub struct Array<const N: u256> {{}}
mod m {{
    use super::*
    with (Discount)
    pub const K: u256 = Item::value()
    pub struct Opened {{ pub a: Array<{{ with (<Item as Pick>) {{ Item::value() }} }}> }}
    pub fn inside() {{ with (<Item as Pick>) {{ let x: Array<{{ Item::value() }}> = Array {{}} }} }}
    pub const fn kk() -> u256 {{ K }}
    pub fn sealed(a: Array<{{ kk() }}>) {{ {body} }}
}}
"#
        ))
    };
    assert!(diagnostics(&source("let x: Array<70> = a")).is_empty());
    assert_eq!(
        diagnostics(&source("let x: Array<100> = a")),
        vec!["8-0000"]
    );
}

// Entry code is planned under the contract module's frame

/// The decoder of `Packet` reads a word and builds its `Word<u256>` field
/// itself. The module chooses a `Decode` for `Word<T>` whose bound is unmet,
/// but nothing the receiver runs calls it, so the entry code is accepted.
#[test]
fn a_receiver_choice_for_a_field_decoder_that_the_message_decoder_never_uses_is_accepted() {
    let source = |prerequisite: &str| {
        format!(
            r#"
use core::abi::{{AbiDecoder, AbiFrame, AbiSize, ByteInput, Decode, Encode}}
use core::message::MsgVariant
use std::abi::Sol

pub struct Word<T> {{ v: u256 }}
pub struct Packet {{ w: Word<u256> }}
pub trait Gate {{}}
{prerequisite}
impl<T> AbiSize for Word<T> {{
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}}
impl<T> Decode<Sol> for Word<T> {{
    fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {{
        Word {{ v: d.read_word(input) }}
    }}
}}
impl<T: Gate> Decode<Sol> for Word<T> as StrictWord {{
    fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {{
        Word {{ v: d.read_word(input) + 1 }}
    }}
}}
impl MsgVariant<Sol> for Packet {{
    const SELECTOR: u32 = 0x11223344
    type Return = u256
}}
impl AbiSize for Packet {{
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}}
impl Encode<Sol> for Packet {{
    fn encode(self, _ out: AbiFrame) {{ out.store_word(at: 0, value: self.w.v) }}
}}
impl Decode<Sol> for Packet {{
    fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {{
        Packet {{ w: Word {{ v: d.read_word(input) }} }}
    }}
}}
with (StrictWord)
pub contract Receiver {{
    recv {{
        Packet {{ w }} -> u256 {{ w.v }}
    }}
}}
"#
        )
    };
    assert!(diagnostics(&source("")).is_empty());
    assert!(diagnostics(&source("impl Gate for u256 {}")).is_empty());
}
