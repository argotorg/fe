//! An explicit derive header `impl Tr for T with (P)` takes every member from
//! its provider: methods, constants and associated types, admitted by the
//! same conformance checks as a handwritten implementation body.
use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

const URL: &str = "file:///tmp/complete-member-derive.fe";

fn prepare_output(source: &str) -> Result<DriverDataBase, String> {
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::parse(URL).unwrap(), Some(source.into()));
    let result = prepare_file(&db, file).map_err(|error| error.cause);
    // Nothing is published into the authored view, on success or failure.
    assert!(db.generation_inputs().functions(&db).is_empty());
    assert!(db.generation_inputs().requests(&db).is_empty());
    assert_eq!(file.text(&db).as_str(), source);
    result?.ok_or_else(|| "expected a prepared view".into())
}

fn output_errors(output: &DriverDataBase) -> Option<String> {
    let file = output
        .workspace()
        .get(output, &Url::parse(URL).unwrap())
        .unwrap();
    let diagnostics = output.run_on_top_mod(output.top_mod(file));
    diagnostics
        .has_errors(output)
        .then(|| diagnostics.format_diags(output))
}

fn prepare(source: &str) -> Result<DriverDataBase, String> {
    let output = prepare_output(source)?;
    assert_eq!(output_errors(&output), None);
    Ok(output)
}

fn prepare_err(source: &str) -> String {
    match prepare_output(source) {
        Ok(_) => panic!("expected preparation to refuse"),
        Err(cause) => cause,
    }
}

fn written_errors(source: &str) -> String {
    let output = prepare_output(source).expect("written errors are checked in the prepared view");
    output_errors(&output).expect("the written program should be refused")
}

fn eval(db: &DriverDataBase, name: &str) -> String {
    let file = db.workspace().get(db, &Url::parse(URL).unwrap()).unwrap();
    // Module-level functions only: trait and impl methods share these names.
    let module = db.top_mod(file);
    let function = module
        .all_funcs(db)
        .iter()
        .copied()
        .find(|function| {
            function.scope().parent(db) == Some(module.scope())
                && function
                    .name(db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"));
    let owner = BodyOwner::Func(function, None);
    let outcome = eval_body_owner_const(db, owner, GenericSubst::for_body_owner(db, owner, vec![]));
    let EvalOutcome::Ready(value) = outcome else {
        panic!("`{name}` did not evaluate: {outcome:?}")
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("`{name}` did not return an integer: {:?}", value.value(db))
    };
    value.to_string()
}

#[test]
fn provider_selection_uses_the_complete_defaulted_trait_application() {
    let prelude = r#"
use core::derive::Derive
use core::meta::{Code, Item, Record, type_ref}

trait Match<Rhs = Self> { const fn score(_ rhs: Rhs) -> usize }
struct Recipe {}
struct Packet {}
"#;
    let generic_provider = r#"
impl<A> Derive<trait Match<A>> for Recipe {
    const fn derive(_ record: Record) -> Code<Item> {
        let rhs = type_ref<A>()
        quote { const fn score(_ rhs: ${rhs}) -> usize { 7 } }
    }
}
"#;
    let bare_provider = r#"
impl Derive<trait Match> for Recipe {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score(_ rhs: Packet) -> usize { 9 } }
    }
}
"#;

    for (application, argument) in [
        ("Match", "Packet {}"),
        ("Match<Packet>", "Packet {}"),
        ("Match<u8>", "1"),
    ] {
        let source = format!(
            "{prelude}{generic_provider}\nimpl {application} for Packet with (Recipe)\n\
             const fn run() -> usize {{ <Packet as {application}>::score({argument}) }}"
        );
        let output = prepare(&source).unwrap_or_else(|cause| panic!("{application}: {cause}"));
        assert_eq!(eval(&output, "run"), "7");
    }

    let source = format!(
        "{prelude}{bare_provider}\nimpl Match for Packet with (Recipe)\n\
         const fn run() -> usize {{ <Packet as Match>::score(Packet {{}}) }}"
    );
    assert_eq!(eval(&prepare(&source).unwrap(), "run"), "9");

    for (first, second) in [
        (generic_provider, bare_provider),
        (bare_provider, generic_provider),
    ] {
        for application in ["Match", "Match<Packet>"] {
            let source =
                format!("{prelude}{first}{second}\nimpl {application} for Packet with (Recipe)");
            let cause = prepare_err(&source);
            assert!(
                cause.contains("multiple derive families for the same trait application"),
                "{application}: {cause}"
            );
        }
    }
}

#[test]
fn a_quoted_opening_uses_the_producers_named_implementation() {
    let source = r#"
trait Gate { const fn code(_ value: u8) -> u8 }
impl Gate for u8 { const fn code(_ value: u8) -> u8 { value } }

mod producer {
    use ingot::Gate
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Item, Record}

    trait Eligible {}
    impl Eligible for u8 {}
    impl<T: Eligible> Gate for T as Encoded {
        const fn code(_ value: u8) -> u8 { value ^ 1 }
    }

    pub struct Provider {}
    impl DeriveInherent for Provider {
        const fn derive(_ record: Record) -> Code<Item> {
            quote {
                pub const fn code(self, _ input: u8) -> u8 {
                    with (Encoded<u8>) { <u8 as Gate>::code(input) }
                }
            }
        }
    }
}

impl Gate for u8 as Encoded {
    const fn code(_ value: u8) -> u8 { value ^ 2 }
}
struct Generated {}
impl Generated with (producer::Provider)

const fn generated() -> u8 { Generated {}.code(7) }
const fn written() -> u8 { with (Encoded) { <u8 as Gate>::code(7) } }
"#;
    let db = prepare(source).unwrap();
    assert_eq!(eval(&db, "generated"), "6");
    assert_eq!(eval(&db, "written"), "5");

    let missing = source.replace("impl Eligible for u8 {}", "");
    assert!(prepare_err(&missing).contains("trait bound is not satisfied"));
}

/// A trait with a constant, an associated type, two required methods and a
/// default method; the provider supplies all but the default. `Square` has
/// three fields, so every expected value below follows from SIDES = 3.
const SHAPE: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}

trait Shape {
    const SIDES: usize
    type Unit
    const fn area() -> usize
    const fn perimeter() -> usize
    const fn described() -> usize { Self::SIDES * 10 }
}

struct Square { a: u8, b: u8, c: u8 }

struct StableShape {}

impl Derive<trait Shape> for StableShape {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote {
            const SIDES: usize = ${n}
            type Unit = u256
            const fn area() -> usize { Self::SIDES * Self::SIDES }
            const fn perimeter() -> usize { 4 * Self::SIDES }
        }
    }
}

impl Shape for Square with (StableShape)

const fn sides() -> usize { Square::SIDES }
const fn area() -> usize { Square::area() }
const fn perimeter() -> usize { Square::perimeter() }
const fn described() -> usize { Square::described() }
const fn unit() -> u256 {
    let unit: <Square as Shape>::Unit = 9
    unit
}
const fn rows() -> u256 {
    let rows: [u256; <Square as Shape>::SIDES] = [7; 3]
    rows[0] + rows[1] + rows[2]
}
"#;

#[test]
fn explicit_derive_supplies_every_member_of_its_header() {
    let output = prepare(SHAPE).unwrap();
    assert_eq!(eval(&output, "sides"), "3");
    assert_eq!(eval(&output, "area"), "9");
    assert_eq!(eval(&output, "perimeter"), "12");
    // The trait's default method stays available and reads the generated const.
    assert_eq!(eval(&output, "described"), "30");
    // The generated associated type and const are usable in types.
    assert_eq!(eval(&output, "unit"), "9");
    assert_eq!(eval(&output, "rows"), "21");
}

#[test]
fn missing_generated_members_and_extra_methods_are_ordinary_conformance_errors() {
    for (from, to, expected) in [
        (
            "            const fn perimeter() -> usize { 4 * Self::SIDES }\n",
            "",
            "not all trait methods are implemented",
        ),
        (
            "            const SIDES: usize = ${n}\n",
            "",
            "missing associated const `SIDES`",
        ),
        (
            "            type Unit = u256\n",
            "",
            "missing associated type `Unit`",
        ),
        (
            "            type Unit = u256\n",
            "            type Unit = u256\n            const fn volume() -> usize { 0 }\n",
            "method `volume` is not defined in trait `Shape`",
        ),
    ] {
        assert!(SHAPE.contains(from), "{from:?}");
        let cause = written_errors(&SHAPE.replace(from, to));
        assert!(cause.contains(expected), "expected {expected:?} in {cause}");
    }
}

#[test]
fn extra_generated_constant_is_reported_at_its_quotation() {
    let source = SHAPE.replace(
        "            type Unit = u256\n",
        "            type Unit = u256\n            const EXTRA: usize = 1\n",
    );
    let cause = prepare_err(&source);
    assert!(
        cause.contains("associated const `EXTRA` is not defined in trait `Shape`")
            && cause.contains("const EXTRA: usize = 1"),
        "{cause}"
    );
}

#[test]
fn generic_associated_type_members_are_refused() {
    let source = SHAPE.replace("type Unit = u256", "type Unit<T> = T");
    let cause = prepare_err(&source);
    assert!(
        cause.contains("a generated associated type cannot take generic parameters"),
        "{cause}"
    );
}

/// Members spliced from another producer keep their place and resolve their
/// free names where that producer wrote them: the private `four` in `helpers`
/// (4), not the root `four` (99), which would give 297.
#[test]
fn members_from_another_producer_splice_in_order_with_their_own_hygiene() {
    let source = SHAPE
        .replace(
            "struct StableShape {}\n",
            "struct StableShape {}\n\nconst fn four() -> usize { 99 }\n\nmod helpers {\n    use core::meta::{Code, Item}\n    const fn four() -> usize { 4 }\n    pub const fn perimeter_member() -> Code<Item> {\n        quote { const fn perimeter() -> usize { four() * Self::SIDES } }\n    }\n}\n",
        )
        .replace(
            "        let n = field_count(record)\n",
            "        let n = field_count(record)\n        let perimeter = helpers::perimeter_member()\n",
        )
        .replace(
            "            const fn perimeter() -> usize { 4 * Self::SIDES }\n",
            "            ${perimeter}\n",
        );
    let output = prepare(&source).unwrap();
    assert_eq!(eval(&output, "perimeter"), "12");
    assert_eq!(eval(&output, "area"), "9");
}

/// Named implementations with associated outputs need an ordinary
/// representative and must agree with it, generated or handwritten.
#[test]
fn named_generated_outputs_need_and_agree_with_an_ordinary_implementation() {
    let named = SHAPE.replace(
        "impl Shape for Square with (StableShape)",
        "impl Shape for Square as Generated with (StableShape)",
    );
    let named = named[..named.find("const fn sides()").unwrap()].to_string()
        + "const fn area() -> usize { with (Generated) { Square::area() } }\n";
    let cause = written_errors(&named);
    assert!(
        cause.contains("named associated outputs require an ordinary implementation"),
        "{cause}"
    );

    let ordinary = |sides: &str| {
        format!(
            "{named}impl Shape for Square {{\n    const SIDES: usize = {sides}\n    type Unit = u256\n    const fn area() -> usize {{ 100 }}\n    const fn perimeter() -> usize {{ 100 }}\n}}\nconst fn ordinary_area() -> usize {{ Square::area() }}\n"
        )
    };
    // Equal outputs: both coexist and each opening selects its own methods.
    let output = prepare(&ordinary("3")).unwrap();
    assert_eq!(eval(&output, "area"), "9");
    assert_eq!(eval(&output, "ordinary_area"), "100");
    // A different constant is refused at the named declaration.
    let cause = written_errors(&ordinary("5"));
    assert!(
        cause.contains("named implementation changes a type-readable associated constant"),
        "{cause}"
    );
}

#[test]
fn a_prepared_written_derive_header_supplies_ordinary_outputs() {
    let source = format!(
        "{SHAPE}impl Shape for Square as Named {{\n    const SIDES: usize = 3\n    type Unit = u256\n    const fn area() -> usize {{ 17 }}\n    const fn perimeter() -> usize {{ 19 }}\n}}\nconst fn named_area() -> usize {{ with (Named) {{ Square::area() }} }}\n"
    );
    let output = prepare(&source).unwrap();
    assert_eq!(eval(&output, "area"), "9");
    assert_eq!(eval(&output, "named_area"), "17");

    let different_constant = source.replace("const SIDES: usize = 3", "const SIDES: usize = 4");
    let cause = written_errors(&different_constant);
    assert!(
        cause.contains("named implementation changes a type-readable associated constant"),
        "{cause}"
    );
    let different_type = source.replace(
        "type Unit = u256\n    const fn area() -> usize { 17 }",
        "type Unit = u8\n    const fn area() -> usize { 17 }",
    );
    let cause = written_errors(&different_type);
    assert!(
        cause.contains("named implementation changes an ordinary associated type"),
        "{cause}"
    );
}

/// A header's provider takes constants, types and methods from a helper's
/// quotation through one `${members}` hole that holds every member.
#[test]
fn member_holes_in_quoted_impls_supply_types_and_constants() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}

trait Shape {
    const SIDES: usize
    type Unit
    const fn area() -> usize
}

const fn members(_ n: usize) -> Code<Item> {
    quote {
        const SIDES: usize = ${n}
        type Unit = u256
        const fn area() -> usize { Self::SIDES * Self::SIDES }
    }
}

struct ShapeP {}
impl Derive<trait Shape> for ShapeP {
    const fn derive(_ record: Record) -> Code<Item> {
        let body = members(field_count(record))
        quote { ${body} }
    }
}

struct Square { a: u8, b: u8, c: u8 }
impl Shape for Square with (ShapeP)

const fn area() -> usize { Square::area() }
const fn unit() -> u256 {
    let unit: <Square as Shape>::Unit = 9
    unit
}
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval(&output, "area"), "9");
    assert_eq!(eval(&output, "unit"), "9");
}

/// A header written in the record's own module sees its private fields; the
/// same request from outside that module is refused before the provider runs
/// (requester-side visibility). Nested headers take every member like root
/// ones and are reachable from anywhere the trait is.
#[test]
fn explicit_derive_headers_in_nested_modules_take_their_members() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}

trait Score {
    const BASE: usize
    const fn score() -> usize
}

struct Counter {}

impl Derive<trait Score> for Counter {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote {
            const BASE: usize = 10
            const fn score() -> usize { Self::BASE + ${n} }
        }
    }
}

mod owner {
    use super::{Counter, Score}

    pub struct Secret { hidden: u256, kept: bool }

    impl Score for Secret with (Counter)

    pub const fn inside() -> usize { Secret::score() }
}

const fn inside() -> usize { owner::inside() }
const fn outside() -> usize { owner::Secret::score() }
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval(&output, "inside"), "12");
    assert_eq!(eval(&output, "outside"), "12");

    // Requester side: the same header at the root cannot observe `hidden`.
    let requester = source
        .replace("    impl Score for Secret with (Counter)\n", "")
        .replace(
            "const fn inside() -> usize { owner::inside() }\n",
            "impl Score for owner::Secret with (Counter)\n",
        )
        .replace(
            "    pub const fn inside() -> usize { Secret::score() }\n",
            "",
        );
    let cause = prepare_err(&requester);
    assert!(
        cause.contains("every field of its target to be visible where the header is written"),
        "{cause}"
    );
}

/// Explicit headers over instances of generic records. A field-type splice is
/// the field's type in that instance: `T` is `u8` in `Envelope<u8>` and
/// `u256` in `Envelope<u256>`, `Pair<u8>` in `Nest<u8>`, `[u8; 4]` in
/// `Buf<4>`, and the payload of `Choice<u256>`'s first variant is `u256`.
/// Expected values are the widths written in the `Bits` impls.
const INSTANCES: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field, field_type, variant}

trait Bits { const BITS: u256 }
impl Bits for u8 { const BITS: u256 = 8 }
impl Bits for u256 { const BITS: u256 = 256 }
impl<T: Bits> Bits for Pair<T> { const BITS: u256 = 2 * <T as Bits>::BITS }
impl<const N: usize> Bits for [u8; N] { const BITS: u256 = 8 * (N as u256) }

trait Report { const fn payload_bits() -> u256 }

struct Envelope<T> { value: T }
struct Pair<T> { a: T, b: T }
struct Nest<T> { inner: Pair<T> }
struct Buf<const N: usize> { data: [u8; N] }
enum Choice<T> { Some(T), Empty }

struct FirstField {}
impl Derive<trait Report> for FirstField {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        quote { const fn payload_bits() -> u256 { <${ty} as Bits>::BITS } }
    }
}

struct FirstPayload {}
impl Derive<trait Report> for FirstPayload {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(variant(record, 0), 0))
        quote { const fn payload_bits() -> u256 { <${ty} as Bits>::BITS } }
    }
}

impl Report for Envelope<u8> with (FirstField)
impl Report for Envelope<u256> with (FirstField)
impl Report for Nest<u8> with (FirstField)
impl Report for Buf<4> with (FirstField)
impl Report for Choice<u256> with (FirstPayload)

const fn narrow() -> u256 { <Envelope<u8> as Report>::payload_bits() }
const fn wide() -> u256 { <Envelope<u256> as Report>::payload_bits() }
const fn nested() -> u256 { <Nest<u8> as Report>::payload_bits() }
const fn buffer() -> u256 { <Buf<4> as Report>::payload_bits() }
const fn payload() -> u256 { <Choice<u256> as Report>::payload_bits() }
"#;

#[test]
fn explicit_derive_over_generic_record_instances_keeps_their_arguments() {
    let output = prepare(INSTANCES).unwrap();
    assert_eq!(eval(&output, "narrow"), "8");
    assert_eq!(eval(&output, "wide"), "256");
    assert_eq!(eval(&output, "nested"), "16");
    assert_eq!(eval(&output, "buffer"), "32");
    assert_eq!(eval(&output, "payload"), "256");
}

/// The instance's arguments reach ordinary checking of the generated body:
/// an argument without the `Bits` the body needs is the ordinary unmet bound,
/// reported for that instance only.
#[test]
fn an_instance_argument_without_the_needed_bound_is_an_ordinary_error() {
    let source = INSTANCES.replace(
        "impl Report for Envelope<u256> with (FirstField)\n",
        "impl Report for Envelope<u256> with (FirstField)\nstruct Opaque {}\nimpl Report for Envelope<Opaque> with (FirstField)\n",
    );
    assert_ne!(source, INSTANCES);
    let cause = prepare_err(&source);
    assert!(cause.contains("Bits"), "{cause}");
    assert!(cause.contains("Opaque"), "{cause}");
}

/// A provider applied to arguments is evaluated at that application: the
/// constant `K` of `Scaled<10>` is 10 in its quote.
#[test]
fn a_provider_applied_to_arguments_evaluates_at_that_application() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}

trait Report { const fn payload_bits() -> u256 }

struct Scaled<const K: u256> {}
impl<const K: u256> Derive<trait Report> for Scaled<K> {
    const fn derive(_ record: Record) -> Code<Item> {
        let k = K
        quote { const fn payload_bits() -> u256 { ${k} } }
    }
}

struct Plain { x: u8 }
struct Other { y: u8 }
impl Report for Plain with (Scaled<10>)
impl Report for Other with (Scaled<20>)

const fn ten() -> u256 { Plain::payload_bits() }
const fn twenty() -> u256 { Other::payload_bits() }
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval(&output, "ten"), "10");
    assert_eq!(eval(&output, "twenty"), "20");
}

/// A complete authored header with binders: the provider sees the target over
/// the header's own parameter, and the generated body names that parameter
/// through the header, never by spelling. Each instance then has its own
/// width, as for a handwritten `impl<T: Bits> Report for Envelope<T>`.
#[test]
fn a_generic_header_generates_for_every_instance_under_its_own_bounds() {
    let header = INSTANCES
        .replace(
            "impl Report for Envelope<u8> with (FirstField)\nimpl Report for Envelope<u256> with (FirstField)\n",
            "impl<T: Bits> Report for Envelope<T> with (FirstField)\n",
        )
        .replace(
            "impl Report for Buf<4> with (FirstField)\n",
            "impl<const N: usize> Report for Buf<N> with (FirstField)\n",
        );
    assert_ne!(header, INSTANCES);
    let output = prepare(&header).unwrap();
    assert_eq!(eval(&output, "narrow"), "8");
    assert_eq!(eval(&output, "wide"), "256");
    assert_eq!(eval(&output, "buffer"), "32");
    assert_eq!(eval(&output, "nested"), "16");

    // The same requirement written as a `where` clause after the provider.
    let where_clause = header.replace(
        "impl<T: Bits> Report for Envelope<T> with (FirstField)\n",
        "impl<T> Report for Envelope<T> with (FirstField) where T: Bits\n",
    );
    assert_ne!(where_clause, header);
    let output = prepare(&where_clause).unwrap();
    assert_eq!(eval(&output, "narrow"), "8");
    assert_eq!(eval(&output, "wide"), "256");
}

/// A bound of the target can be proved by an implementation with conditions,
/// here `impl<T: Inner> Outer for T`. A premise the header states satisfies such
/// a condition as it does for the handwritten implementation; a header that
/// states no such premise is still refused.
#[test]
fn a_generic_header_may_rely_on_a_premise_inside_a_conditional_proof_of_a_target_bound() {
    const HEADER: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}

trait Inner {}
trait Outer {}
impl<T: Inner> Outer for T {}

struct Target<T: Outer> {
    v: T,
}

trait Tr {
    const fn m() -> u256
}

struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ r: Record) -> Code<Item> {
        quote { const fn m() -> u256 { 1 } }
    }
}

impl<T: Inner> Tr for Target<T> with (P)

struct Marked {}
impl Inner for Marked {}

const fn run() -> u256 {
    <Target<Marked> as Tr>::m()
}
"#;
    let output = prepare(HEADER).unwrap();
    assert_eq!(eval(&output, "run"), "1");

    // The handwritten implementation is accepted for the same reason. A header
    // on another type keeps a generated view, which is checked in full.
    let handwritten = HEADER.replace(
        "impl<T: Inner> Tr for Target<T> with (P)\n",
        "impl<T: Inner> Tr for Target<T> { const fn m() -> u256 { 1 } }\nstruct Other {}\nimpl Tr for Other with (P)\n",
    );
    assert_ne!(handwritten, HEADER);
    let output = prepare(&handwritten).unwrap();
    assert_eq!(eval(&output, "run"), "1");

    // Without the premise nothing proves the condition, and the header is refused.
    let unstated = HEADER.replace(
        "impl<T: Inner> Tr for Target<T> with (P)\n",
        "impl<T> Tr for Target<T> with (P)\n",
    );
    assert_ne!(unstated, HEADER);
    let cause = prepare_err(&unstated);
    assert!(cause.contains("Outer"), "{cause}");
    assert!(cause.contains("bound is not proven"), "{cause}");
}

/// The header's bounds are the generated implementation's requirements: an
/// instance without `Bits` is the ordinary unmet bound where it is used, and a
/// header that omits the bound its generated body needs is an ordinary error
/// in that body.
#[test]
fn generic_header_bounds_are_ordinary_requirements() {
    let header = INSTANCES.replace(
        "impl Report for Envelope<u8> with (FirstField)\nimpl Report for Envelope<u256> with (FirstField)\n",
        "impl<T: Bits> Report for Envelope<T> with (FirstField)\nstruct Opaque {}\nconst fn opaque() -> u256 { <Envelope<Opaque> as Report>::payload_bits() }\n",
    );
    assert_ne!(header, INSTANCES);
    let cause = written_errors(&header);
    assert!(cause.contains("Bits"), "{cause}");
    assert!(cause.contains("Opaque"), "{cause}");

    let unbounded = INSTANCES.replace(
        "impl Report for Envelope<u8> with (FirstField)\nimpl Report for Envelope<u256> with (FirstField)\n",
        "impl<T> Report for Envelope<T> with (FirstField)\n",
    );
    let cause = prepare_err(&unbounded);
    assert!(cause.contains("Bits"), "{cause}");
}

/// Reflection over a header parameter has no ground answer: asking whether
/// `T` is `u256` fails at the request instead of answering false.
#[test]
fn reflecting_on_a_header_parameter_fails_at_the_request() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field, field_type, type_is}

trait Report { const fn payload_bits() -> u256 }
struct Envelope<T> { value: T }

struct Asks {}
impl Derive<trait Report> for Asks {
    const fn derive(_ record: Record) -> Code<Item> {
        let wide = type_is<u256>(field_type(field(record, 0)))
        quote { const fn payload_bits() -> u256 { if ${wide} { 256 } else { 0 } } }
    }
}

impl<T> Report for Envelope<T> with (Asks)
"#;
    let cause = prepare_err(source);
    assert!(cause.contains("concrete ground types"), "{cause}");
}

/// A trait with an associated shape. The provider supplies the shape and the
/// method whose result it describes; the shape keeps the mode it is written
/// with, so `mut? u256` agrees with the method's `mut? u256`.
const SHAPED: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field}

trait Driver {
    shape Step
    fn at(mut? self) -> Self::Step
}

struct One { x: u256 }

struct Stepper {}
impl Derive<trait Driver> for Stepper {
    const fn derive(_ record: Record) -> Code<Item> {
        let f = field(record, 0)
        quote {
            shape Step = mut? u256
            fn at(mut? self) -> mut? u256 { mut? self.${f} }
        }
    }
}

impl Driver for One with (Stepper)
"#;

#[test]
fn quoted_shape_members_keep_their_modes() {
    prepare(SHAPED).unwrap();
    // A shape whose mode disagrees with the method is the error a
    // handwritten implementation gets, not a lost mode.
    let disagreeing = SHAPED.replace("shape Step = mut? u256", "shape Step = own u256");
    assert_ne!(disagreeing, SHAPED);
    let generated = prepare_err(&disagreeing);
    let handwritten = r#"trait Driver {
    shape Step
    fn at(mut? self) -> Self::Step
}

struct One { x: u256 }

impl Driver for One {
    shape Step = own u256
    fn at(mut? self) -> mut? u256 { mut? self.x }
}
"#;
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::parse(URL).unwrap(), Some(handwritten.into()));
    let expected = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    let headers = |text: &str| {
        let mut lines = text
            .lines()
            .filter(|line| line.starts_with("error"))
            .map(str::to_string)
            .collect::<Vec<_>>();
        lines.sort();
        lines
    };
    assert!(!headers(&expected).is_empty(), "{expected}");
    for header in headers(&expected) {
        assert!(
            generated.contains(&header),
            "missing {header:?} in {generated}"
        );
    }
}

/// Declaration order in one file does not matter: the header may come before
/// its record and its provider. Lowering names both by authored reference, so
/// nothing waits for them to be lowered first.
#[test]
fn a_header_may_precede_its_record_and_provider() {
    let header = "impl Shape for Square with (StableShape)\n";
    assert!(SHAPE.contains(header));
    let reordered = SHAPE.replace(header, "").replacen(
        "trait Shape {",
        &format!("{header}\ntrait Shape {{"),
        1,
    );
    assert_ne!(reordered, SHAPE);
    let output = prepare(&reordered).unwrap();
    assert_eq!(eval(&output, "sides"), "3");
    assert_eq!(eval(&output, "area"), "9");
    assert_eq!(eval(&output, "rows"), "21");
}

// ---- Headers for traits of core and std ----------------------------------------
//
// A header is a written implementation whose members a provider supplies, so
// it asks for what a handwritten `impl Drop for T { ... }` has. The trait need
// not belong to the package.

/// The diagnostics of `source` after preparation, errors and warnings, without
/// requiring that there be none.
fn prepared_diagnostics(source: &str) -> Result<String, String> {
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::parse(URL).unwrap(), Some(source.into()));
    let output = match prepare_file(&db, file).map_err(|error| error.cause)? {
        Some(output) => output,
        None => return Err("no request to prepare".to_string()),
    };
    let output_file = output
        .workspace()
        .get(&output, &Url::parse(URL).unwrap())
        .unwrap();
    let diagnostics = output.run_on_top_mod(output.top_mod(output_file));
    Ok(diagnostics.format_diags(&output))
}

/// The diagnostics of `source` as written, with no generation.
fn written_diagnostics(source: &str) -> String {
    let mut db = DriverDataBase::default();
    let file = db
        .workspace()
        .touch(&mut db, Url::parse(URL).unwrap(), Some(source.into()));
    db.run_on_top_mod(db.top_mod(file)).format_diags(&db)
}

const MARKER_HEADERS: &str = r#"use core::derive::Derive
use core::marker::{Copy, Snapshot}
use core::meta::{Code, Item, Record}

struct Marks {}
impl Derive<trait Copy> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote {} }
}
impl Derive<trait Snapshot> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote {} }
}

struct P { v: u256 }
impl Copy for P with (Marks)
impl Snapshot for P with (Marks)

const fn twice(_ p: own P) -> u256 {
    let a = p
    let b = p
    a.v + b.v
}
const fn copied() -> u256 { twice(P { v: 3 }) }
fn needs_copy<T: Copy>(_ t: T) -> u256 { 1 }
fn copy_bound() -> u256 { needs_copy(P { v: 1 }) }
fn needs_snapshot<T: Snapshot>(_ t: T) -> u256 { 1 }
fn snapshot_bound() -> u256 { needs_snapshot(P { v: 1 }) }
"#;

/// A marker trait has no members, so its provider returns an empty quotation.
/// `Copy` and `Snapshot` are the traits of core that change what the compiler
/// does by being implemented.
#[test]
fn a_header_implements_a_marker_trait_of_core_with_no_members() {
    let output = prepare(MARKER_HEADERS).unwrap();
    assert_eq!(eval(&output, "copied"), "6");
    // Control: without the headers neither bound holds.
    let written = MARKER_HEADERS
        .replace("impl Copy for P with (Marks)\n", "")
        .replace("impl Snapshot for P with (Marks)\n", "");
    let diagnostics = written_diagnostics(&written);
    assert!(
        diagnostics.contains("`P` doesn't implement `Copy`"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("`P` doesn't implement `Snapshot`"),
        "{diagnostics}"
    );
}

const DROP_HEADER: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use core::ops::Drop

struct Releases {}
impl Derive<trait Drop> for Releases {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn drop(own self) {} }
    }
}

struct Res { v: u256 }
impl Drop for Res with (Releases)

fn run() -> u256 {
    let r = Res { v: 3 }
    r.v
}
"#;

#[test]
fn a_header_implements_drop_for_a_type_of_the_package() {
    prepare(DROP_HEADER).unwrap();
}

#[test]
fn a_header_implements_pinned_and_state_index() {
    let source = r#"use core::derive::Derive
use core::marker::{Pinned, Storage}
use core::meta::{Code, Item, Record}
use core::ops::{Location, StateIndex}

struct Pin {}
impl Derive<trait Pinned> for Pin {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { type Space = Storage }
    }
}

struct Index {}
impl Derive<trait StateIndex> for Index {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            type Key = u256
            type Entry = u256
            fn locate(self, _ root: u256, _ key: u256) -> Location {
                Location { slot: root + key, offset: 0 }
            }
        }
    }
}

struct Cells { len: u256 }
impl Pinned for Cells with (Pin)
unsafe impl StateIndex for Cells with (Index)

impl Cells {
    fn bump(mut self, _ key: u256) {
        let cell = mut self[key]
        cell += 1
    }
}
"#;
    prepare(source).unwrap();
}

#[test]
fn a_header_implements_default_and_clone() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}

struct Fill {}
impl Derive<trait Default> for Fill {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn default() -> Self { P { v: 7 } } }
    }
}
impl Derive<trait Clone> for Fill {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn clone(self) -> Self { P { v: self.v } } }
    }
}

struct P { v: u256 }
impl Default for P with (Fill)
impl Clone for P with (Fill)

fn run() -> u256 {
    let p = P::default()
    p.clone().v
}
"#;
    prepare(source).unwrap();
}

#[test]
fn a_header_for_std_sol_packed_supplies_its_members() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use std::evm::SolPacked

struct Pack {}
impl Derive<trait SolPacked> for Pack {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const BITS: u256 = 8
            fn to_packed(own self) -> u256 { self.a }
            fn from_packed(_ bits: u256) -> X { X { a: bits } }
        }
    }
}

struct X { a: u256 }
impl SolPacked for X with (Pack)
"#;
    prepare(source).unwrap();
}

/// A header is a written implementation: a second one for the same trait and
/// type conflicts with it, as two handwritten ones do.
#[test]
fn a_header_and_a_written_implementation_conflict() {
    let source = DROP_HEADER.replace(
        "fn run()",
        "impl Drop for Res { fn drop(own self) {} }\nfn run()",
    );
    let cause = prepared_diagnostics(&source).unwrap_or_else(|cause| cause);
    assert!(
        cause.contains("conflicting trait implementations"),
        "{cause}"
    );
}

/// The orphan rule is unchanged: a header cannot implement a trait of another
/// ingot for a type of another ingot either.
#[test]
fn a_header_cannot_implement_a_core_trait_for_a_foreign_type() {
    let source = DROP_HEADER.replace("impl Drop for Res with", "impl Drop for Option<u8> with");
    let cause = match prepared_diagnostics(&source) {
        Ok(diagnostics) => diagnostics,
        Err(cause) => cause,
    };
    assert!(
        cause.contains(
            "a header cannot implement a trait of another package for a type of another package"
        ),
        "{cause}"
    );
}

// ---- Headers for traits with further parameters ---------------------------------
//
// The provider is keyed by the bare trait (`Derive<trait Eq>`) and is not shown the
// header's other arguments, which are defaulted (`Eq` is `Eq<Self>`) or
// written (`Decode<Sol>`). What it returns is checked against the header's
// actual arguments by the same conformance rules as a handwritten body.

const PARAMETERIZED_HEADERS: &str = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use core::ops::{Add, Eq, Ord}

struct Same {}
impl Derive<trait Eq> for Same {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn eq(self, _ other: P) -> bool { self.v == other.v } }
    }
}
impl Derive<trait Ord> for Same {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            fn lt(self, _ other: P) -> bool { self.v < other.v }
            fn le(self, _ other: P) -> bool { self.v <= other.v }
            fn gt(self, _ other: P) -> bool { self.v > other.v }
            fn ge(self, _ other: P) -> bool { self.v >= other.v }
        }
    }
}
impl Derive<trait Add> for Same {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn add(own self, _ other: own P) -> P { P { v: self.v + other.v } } }
    }
}

struct P { v: u256 }
impl Eq for P with (Same)
impl Ord for P with (Same)
impl Add for P with (Same)

fn compared(a: P, b: P) -> bool { a == b || a < b }
fn summed(a: P, b: P) -> u256 { (a + b).v }
"#;

/// `Eq`, `Ord` and the operator traits have a defaulted parameter, so the
/// header names none; the operators then run the provider's members.
#[test]
fn a_header_implements_a_trait_whose_parameter_is_defaulted() {
    prepare(PARAMETERIZED_HEADERS).unwrap();
    // Control: without the headers the operators are not defined.
    let written = PARAMETERIZED_HEADERS
        .replace("impl Eq for P with (Same)\n", "")
        .replace("impl Ord for P with (Same)\n", "")
        .replace("impl Add for P with (Same)\n", "");
    let diagnostics = written_diagnostics(&written);
    assert!(diagnostics.contains("`P`"), "{diagnostics}");
}

/// A parameter written in the header is part of the goal; the provider's
/// members are checked against it.
#[test]
fn a_header_implements_a_trait_whose_parameter_is_written() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use core::abi::{AbiSize, AbiFrame, Decode, AbiDecoder, ByteInput, Encode}
use core::error::ErrorVariant
use std::abi::Sol

struct Impl {}
impl Derive<trait Decode<Sol>> for Impl {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {
                let _ = input
                P { v: 0 }
            }
        }
    }
}
impl Derive<trait ErrorVariant<Sol>> for Impl {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const SELECTOR: u32 = 0x12345678 }
    }
}

struct P { v: u256 }
impl AbiSize for P {
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}
impl Decode<Sol> for P with (Impl)

struct E { code: u256 }
impl AbiSize for E {
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}
impl Encode<Sol> for E {
    fn encode(self, _ out: AbiFrame) { out.store_word(at: 0, value: self.code) }
}
impl ErrorVariant<Sol> for E with (Impl)
"#;
    prepare(source).unwrap();
    // The bounds of the header's trait are checked as for a written one:
    // `ErrorVariant<Sol>` needs `Encode<Sol>`.
    let diagnostics = prepared_diagnostics(&source.replace(
        "impl Encode<Sol> for E {\n    fn encode(self, _ out: AbiFrame) { out.store_word(at: 0, value: self.code) }\n}\n",
        "",
    ))
    .unwrap_or_else(|cause| cause);
    assert!(
        diagnostics.contains("doesn't implement `Encode<Sol>`"),
        "{diagnostics}"
    );
}

/// The provider's output is checked against the header's arguments: a method
/// with the wrong parameter type is an ordinary conformance error.
#[test]
fn a_provider_output_that_does_not_fit_the_headers_arguments_is_a_conformance_error() {
    let source = PARAMETERIZED_HEADERS
        .replace(
            "impl Derive<trait Eq> for Same",
            "impl Derive<trait Eq<u256>> for Same",
        )
        .replace(
            "impl Eq for P with (Same)",
            "impl Eq<u256> for P with (Same)",
        );
    let cause = prepared_diagnostics(&source).unwrap_or_else(|cause| cause);
    assert!(
        cause.contains("mismatch")
            || cause.contains("incompatible")
            || cause.contains("does not match"),
        "{cause}"
    );
}

/// A header is a written implementation: a second one conflicts with it.
#[test]
fn a_header_with_a_parameter_and_a_written_implementation_conflict() {
    let source = PARAMETERIZED_HEADERS.replace(
        "fn compared",
        "impl Eq for P { fn eq(self, _ other: P) -> bool { true } }\nfn compared",
    );
    let cause = prepared_diagnostics(&source).unwrap_or_else(|cause| cause);
    assert!(
        cause.contains("conflicting trait implementations"),
        "{cause}"
    );
}

/// The orphan rule is unchanged: both trait and type belong to core.
#[test]
fn a_header_with_a_parameter_cannot_implement_a_core_trait_for_a_foreign_type() {
    let source = PARAMETERIZED_HEADERS.replace(
        "impl Eq for P with (Same)",
        "impl Eq for Option<u8> with (Same)",
    );
    let cause = prepared_diagnostics(&source).unwrap_or_else(|cause| cause);
    assert!(
        cause.contains(
            "a header cannot implement a trait of another package for a type of another package"
        ),
        "{cause}"
    );
}

/// A header for a callable trait is the request form of the callable decision.
/// The provider is keyed by the bare trait and is not shown the parameters or
/// the `Out` of `Fn(A) -> R`; what it returns is checked against them like a
/// handwritten body, so it writes the signature the header asks for.
const CALLABLE_HEADERS: &str = r#"use core::derive::Derive
use core::functional::{Fn, FnMut}
use core::meta::{Code, Item, Record}

struct Calls {}
impl Derive<trait Fn(u256) -> u256> for Calls {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn call(self, _ x: u256) -> u256 { x * 3 } }
    }
}
struct CallsMut {}
impl Derive<trait FnMut(u256) -> u256> for CallsMut {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn call_mut(mut self, _ x: u256) -> u256 { x * 2 } }
    }
}
struct Triple {}
impl Fn(u256) -> u256 for Triple with (Calls)
struct Double {}
impl FnMut(u256) -> u256 for Double with (CallsMut)

const fn tripled() -> u256 {
    let t = Triple {}
    t(5)
}
const fn doubled() -> u256 {
    var d = Double {}
    d(5)
}
"#;

#[test]
fn a_header_requests_a_callable_trait() {
    let output = prepare(CALLABLE_HEADERS).unwrap_or_else(|cause| panic!("{cause}"));
    assert_eq!(eval(&output, "tripled"), "15");
    assert_eq!(eval(&output, "doubled"), "10");
    // The `Out` may be left open in the header and supplied by the provider.
    let open = CALLABLE_HEADERS
        .replace(
            "quote { const fn call(self",
            "quote { type Out = u256\n const fn call(self",
        )
        .replace(
            "impl Derive<trait Fn(u256) -> u256> for Calls",
            "impl Derive<trait Fn(u256) -> _> for Calls",
        )
        .replace(
            "impl Fn(u256) -> u256 for Triple",
            "impl Fn(u256) -> _ for Triple",
        );
    let output = prepare(&open).unwrap_or_else(|cause| panic!("{cause}"));
    assert_eq!(eval(&output, "tripled"), "15");
}

/// What a provider returns for a callable is checked against the header's
/// parameters and `Out` like any written implementation.
#[test]
fn a_callable_header_is_checked_against_its_parameters_and_out() {
    let wrong_parameter =
        CALLABLE_HEADERS.replace("_ x: u256) -> u256 { x * 3 }", "_ x: u8) -> u256 { 3 }");
    let cause = prepared_diagnostics(&wrong_parameter).unwrap_or_else(|cause| cause);
    assert!(
        cause.contains("method `call` has incompatible argument type"),
        "{cause}"
    );
    let wrong_out =
        CALLABLE_HEADERS.replace("_ x: u256) -> u256 { x * 3 }", "_ x: u256) -> u8 { 3 }");
    let cause = prepared_diagnostics(&wrong_out).unwrap_or_else(|cause| cause);
    assert!(
        cause.contains("method `call` has incompatible return type"),
        "{cause}"
    );
    // Control: the provider that matches is accepted.
    prepare(CALLABLE_HEADERS).unwrap_or_else(|cause| panic!("{cause}"));
}

/// A bare type parameter is no record: the refusal says so.
#[test]
fn a_header_for_a_bare_type_parameter_names_its_cause() {
    let diagnostics = prepared_diagnostics(
        r#"use core::derive::Derive
use core::marker::Copy
use core::meta::{Code, Item, Record}

struct Marks {}
impl Derive<trait Copy> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote {} }
}
impl<T> Copy for T with (Marks)
"#,
    )
    .unwrap_or_else(|cause| cause);
    assert!(diagnostics.contains("bare type parameter"), "{diagnostics}");
    assert!(!diagnostics.contains("another package"), "{diagnostics}");
}

// ---- A header's members are checked as a written implementation's are -----------
//
// Each case below has the same error with the implementation written by hand;
// the header and its provider add no way around the checks.

const SOUNDNESS_HEAD: &str = r#"use core::derive::Derive
use core::marker::{Copy, Pinned, Snapshot, Storage}
use core::meta::{Code, Item, Record}
use core::ops::{Default, Drop, Location, StateIndex}

struct Marks {}
impl Derive<trait Copy> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote {} }
}
impl Derive<trait Snapshot> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote {} }
}
impl Derive<trait Drop> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote { fn drop(own self) {} } }
}
"#;

fn soundness(body: &str) -> String {
    prepared_diagnostics(&format!("{SOUNDNESS_HEAD}{body}")).unwrap_or_else(|cause| cause)
}

#[test]
fn a_copy_header_on_a_type_with_a_drop_field_is_an_error() {
    let diagnostics = soundness(
        "struct Res { v: u256 }
impl Drop for Res { fn drop(own self) {} }
struct S { r: Res }
impl Copy for S with (Marks)
",
    );
    assert!(
        diagnostics.contains("`S` cannot be `Copy`"),
        "{diagnostics}"
    );
}

#[test]
fn a_copy_header_beside_a_drop_implementation_is_an_error() {
    let diagnostics = soundness(
        "struct S { v: u256 }
impl Drop for S with (Marks)
impl Copy for S with (Marks)
",
    );
    assert!(
        diagnostics.contains("`S` cannot implement both `Copy` and `Drop`"),
        "{diagnostics}"
    );
}

#[test]
fn a_snapshot_header_without_copy_is_an_error() {
    let diagnostics = soundness(
        "struct S { v: u256 }
impl Snapshot for S with (Marks)
",
    );
    assert!(
        diagnostics.contains("trait bound is not satisfied"),
        "{diagnostics}"
    );
}

#[test]
fn a_state_index_header_without_unsafe_is_an_error() {
    let diagnostics = soundness(
        "struct PinStorage {}
impl Derive<trait Pinned> for PinStorage {
    const fn derive(_ record: Record) -> Code<Item> { quote { type Space = Storage } }
}
struct Spread {}
impl Derive<trait StateIndex> for Spread {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            type Key = u256
            type Entry = u256
            fn locate(self, _ root: u256, _ key: u256) -> Location {
                Location { slot: root + key, offset: 0 }
            }
        }
    }
}
struct Cells { len: u256 }
impl Pinned for Cells with (PinStorage)
impl StateIndex for Cells with (Spread)
",
    );
    assert!(
        diagnostics.contains("implementing `StateIndex` requires `unsafe impl`"),
        "{diagnostics}"
    );
}

#[test]
fn a_pinned_header_with_a_space_that_is_not_one_is_an_error() {
    let diagnostics = soundness(
        "struct BadSpace {}
impl Derive<trait Pinned> for BadSpace {
    const fn derive(_ record: Record) -> Code<Item> { quote { type Space = u256 } }
}
struct S { v: u256 }
impl Pinned for S with (BadSpace)
",
    );
    assert!(
        diagnostics.contains("a type is pinned to a state space"),
        "{diagnostics}"
    );
}

#[test]
fn a_member_the_trait_does_not_have_is_an_error() {
    let diagnostics = soundness(
        "struct Fill {}
impl Derive<trait Default> for Fill {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            fn default() -> Self { S { v: 7 } }
            fn sneaky(self) -> u256 { 99 }
        }
    }
}
struct S { v: u256 }
impl Default for S with (Fill)
",
    );
    assert!(
        diagnostics.contains("method not defined in trait"),
        "{diagnostics}"
    );
}

#[test]
fn a_header_beside_a_written_marker_implementation_conflicts() {
    let diagnostics = soundness(
        "struct S { v: u256 }
impl Copy for S {}
impl Copy for S with (Marks)
",
    );
    assert!(
        diagnostics.contains("conflicting trait implementations"),
        "{diagnostics}"
    );
}

// ---- A header requests what a generated implementation may not provide ------------
//
// A provider cannot generate an implementation of a trait that a written
// implementation of a deciding trait is bounded on, directly or through a chain
// of bounds (see `package_boundary`). The request form is a header, which
// counts as written code. Each case below is such a trait, and each header
// works: the provider supplies the members, which conform as a written body's
// do.

fn header_case(imports: &str, written: &str, goal: &str, members: &str) -> String {
    format!(
        "use core::derive::Derive
use core::meta::{{Code, Item, Record}}
{imports}
pub struct P {{ pub v: u256 }}
{written}
struct D {{}}
impl Derive<trait {goal}> for D {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{
{members}
        }}
    }}
}}
impl {goal} for P with (D)
"
    )
}

const WRITTEN_COLLECTION: &str = "impl Collection for P {
    type Cursor = usize
    type Item = u256
    fn start(self) -> usize { 0 }
    fn has(self, _ c: usize) -> bool { c < 3 }
    fn next(self, _ c: usize) -> usize { c + 1 }
    fn at(mut? self, _ c: usize) -> mut? u256 { mut? self.v }
}";

const WRITTEN_ABI_SIZE: &str = "impl AbiSize for P {
    const HEAD_SIZE: u256 = 32
    const IS_DYNAMIC: bool = false
}";

#[test]
fn a_header_requests_collection() {
    let source = header_case(
        "use core::iter::Collection",
        "",
        "Collection",
        "type Cursor = usize
type Item = u256
fn start(self) -> usize { 0 }
fn has(self, _ c: usize) -> bool { c < 3 }
fn next(self, _ c: usize) -> usize { c + 1 }
fn at(mut? self, _ c: usize) -> mut? u256 { mut? self.v }",
    );
    prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
}

#[test]
fn a_header_requests_bidirectional() {
    let source = header_case(
        "use core::iter::{Bidirectional, Collection}",
        WRITTEN_COLLECTION,
        "Bidirectional",
        "fn last(self) -> usize { 2 }
fn prev(self, _ c: usize) -> usize { c.wrapping_sub(1) }",
    );
    prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
}

#[test]
fn a_header_requests_abi_size() {
    let source = header_case(
        "use core::abi::AbiSize",
        "",
        "AbiSize",
        "const HEAD_SIZE: u256 = 32
const IS_DYNAMIC: bool = false",
    );
    prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
}

#[test]
fn a_header_requests_decode_and_abi_span() {
    let decode = header_case(
        "use core::abi::{AbiDecoder, AbiSize, ByteInput, Decode}\nuse std::abi::Sol",
        WRITTEN_ABI_SIZE,
        "Decode<Sol>",
        "fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {
    let _ = input
    P { v: 0 }
}",
    );
    prepare(&decode).unwrap_or_else(|cause| panic!("{cause}"));
    let span = header_case(
        "use core::abi::{AbiSize, AbiSpan, ByteInput}\nuse std::abi::Sol",
        WRITTEN_ABI_SIZE,
        "AbiSpan<Sol>",
        "fn payload_end<I: ByteInput>(_: I, base: u256, pos: u256) -> u256 { pos + 32 }",
    );
    prepare(&span).unwrap_or_else(|cause| panic!("{cause}"));
}

#[test]
fn a_header_requests_storage_key() {
    let source = header_case(
        "use std::evm::StorageKey",
        "",
        "StorageKey",
        "fn encoded_len(self) -> u256 { 32 }
unsafe fn write_key(ptr: *u8, self) { let _ = ptr }",
    );
    prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
}

#[test]
fn a_header_requests_sol_word_repr() {
    let source = header_case(
        "use std::abi::sol::SolWordRepr",
        "",
        "SolWordRepr",
        "const BITS: u256 = 8
fn canonical_word(self) -> u256 { self.v }",
    );
    prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
}

#[test]
fn a_header_requests_lane_codec() {
    let source = header_case(
        "use core::ops::LaneCodec",
        "",
        "LaneCodec<u8>",
        "const BITS: u256 = 8
fn get(_ word: u256, offset: u256) -> u8 { 0 }
fn set(_ word: u256, offset: u256, value: own u8) -> u256 { word }",
    );
    prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
}

#[test]
fn a_header_requests_msg_variant() {
    let source = header_case(
        "use core::abi::{AbiDecoder, AbiFrame, AbiSize, ByteInput, Decode, Encode}\nuse core::message::MsgVariant\nuse std::abi::Sol",
        &format!(
            "{WRITTEN_ABI_SIZE}
impl Encode<Sol> for P {{ fn encode(self, _ out: AbiFrame) {{ let _ = out }} }}
impl Decode<Sol> for P {{
    fn decode_payload<D: AbiDecoder<Sol>, I: ByteInput>(_ d: mut D, _ input: I) -> Self {{
        let _ = input
        P {{ v: 0 }}
    }}
}}"
        ),
        "MsgVariant<Sol>",
        "const SELECTOR: u32 = 0x12345678
type Return = u256",
    );
    prepare(&source).unwrap_or_else(|cause| panic!("{cause}"));
}

// ---- Plain refusals of a header ---------------------------------------------------

/// A struct that `#[abi]`, `#[error]` or `#[event]` expands, and the struct of
/// a `msg` variant, are declarations written in source: the expansion adds
/// implementations and leaves the fields as written. A header for them reads
/// the record like a header for a plain struct.
const EXPANDED_TYPE_HEADERS: &str = r#"use core::derive::Derive
use core::marker::Copy
use core::meta::{Code, Item, Record}
use core::ops::Eq

struct Marks {}
impl Derive<trait Copy> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote {} }
}
struct Same {}
impl Derive<trait Eq> for Same {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn eq(self, _ other: P) -> bool { self.v == other.v } }
    }
}

#[abi]
struct P { v: u256 }
impl Copy for P with (Marks)
impl Eq for P with (Same)

const fn twice(_ p: own P) -> u256 {
    let a = p
    let b = p
    a.v + b.v
}
const fn copied() -> u256 { twice(P { v: 3 }) }
fn needs_copy<T: Copy>(_ t: T) -> u256 { 1 }
fn copy_bound() -> u256 { needs_copy(P { v: 1 }) }
"#;

#[test]
fn a_header_on_a_type_that_an_attribute_expands_reads_its_fields() {
    for attribute in ["#[abi]", "#[error]", "#[event]"] {
        let source = EXPANDED_TYPE_HEADERS.replace("#[abi]", attribute);
        let output = prepare(&source).unwrap_or_else(|cause| panic!("{attribute}: {cause}"));
        assert_eq!(eval(&output, "copied"), "6", "{attribute}");
        // Control: without the header the type is not `Copy`.
        let written = source.replace("impl Copy for P with (Marks)\n", "");
        let diagnostics = written_diagnostics(&written);
        assert!(
            diagnostics.contains("`P` doesn't implement `Copy`"),
            "{attribute}: {diagnostics}"
        );
    }
}

/// An inherent header on the struct that `#[abi]`, `#[error]` or `#[event]`
/// expands requests the provider for that struct as it does for a plain
/// struct, and the provider reads the fields as written.
#[test]
fn an_inherent_header_on_a_type_that_an_attribute_expands_reads_its_fields() {
    for attribute in ["#[abi]", "#[error]", "#[event]"] {
        let source = format!(
            "use core::derive::{{Derive, DeriveInherent}}
use core::meta::{{Code, Item, Record, field_count}}

struct Count {{}}
impl DeriveInherent for Count {{
    const fn derive(_ record: Record) -> Code<Item> {{
        let n = field_count(record)
        quote {{ const fn fields() -> usize {{ ${{n}} }} }}
    }}
}}

{attribute}
struct P {{ a: u256, b: u256 }}
impl P with (Count)

const fn fields() -> usize {{ P::fields() }}
"
        );
        let output = prepare(&source).unwrap_or_else(|cause| panic!("{attribute}: {cause}"));
        assert_eq!(eval(&output, "fields"), "2", "{attribute}");
    }
}

/// A header's provider supplies the members of an implementation for the
/// expanded struct, and the implementation applies.
#[test]
fn a_generated_implementation_for_a_type_that_an_attribute_expands_applies() {
    for attribute in ["#[abi]", "#[error]", "#[event]"] {
        let source = format!(
            "use core::derive::Derive
use core::meta::{{Code, Item, Record}}
use core::ops::Eq

struct Same {{}}
impl Derive<trait Eq> for Same {{
    const fn derive(_ record: Record) -> Code<Item> {{
        quote {{ fn eq(self, _ other: Self) -> bool {{ self.a == other.a }} }}
    }}
}}

{attribute}
struct P {{ a: u256, b: u256 }}
impl Eq for P with (Same)

fn needs_eq<T: Eq>(_ t: T) -> u256 {{ 1 }}
fn eq_bound() -> u256 {{ needs_eq(P {{ a: 1, b: 2 }}) }}
"
        );
        // The bound holds only through the header.
        prepare(&source).unwrap_or_else(|cause| panic!("{attribute}: {cause}"));
        let written = source.replace("impl Eq for P with (Same)\n", "");
        let diagnostics = written_diagnostics(&written);
        assert!(
            diagnostics.contains("`P` doesn't implement `Eq<P>`"),
            "{attribute}: {diagnostics}"
        );
    }
}

/// The struct of a `msg` variant is named by its path, and its fields are the
/// variant's parameters.
#[test]
fn a_header_on_a_msg_variant_reads_the_parameters() {
    let source = r#"use core::derive::Derive
use core::marker::Copy
use core::meta::{Code, Item, Record}
use core::ops::Eq

struct Marks {}
impl Derive<trait Copy> for Marks {
    const fn derive(_ record: Record) -> Code<Item> { quote {} }
}
struct Same {}
impl Derive<trait Eq> for Same {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn eq(self, _ other: M::Ping) -> bool { self.v == other.v } }
    }
}

msg M {
    #[selector = 0x11111111]
    Ping { v: u256 } -> u256,
}
impl Copy for M::Ping with (Marks)
impl Eq for M::Ping with (Same)

const fn twice(_ p: own M::Ping) -> u256 {
    let a = p
    let b = p
    a.v + b.v
}
const fn copied() -> u256 { twice(M::Ping { v: 3 }) }
fn needs_copy<T: Copy>(_ t: T) -> u256 { 1 }
fn copy_bound() -> u256 { needs_copy(M::Ping { v: 1 }) }
"#;
    let output = prepare(source).unwrap_or_else(|cause| panic!("{cause}"));
    assert_eq!(eval(&output, "copied"), "6");
    // Control: without the header the type is not `Copy`.
    let written = source.replace("impl Copy for M::Ping with (Marks)\n", "");
    let diagnostics = written_diagnostics(&written);
    assert!(
        diagnostics.contains("`Ping` doesn't implement `Copy`"),
        "{diagnostics}"
    );
}

/// A header can request `MsgVariant` for an `#[abi]` struct, which has the
/// codecs it needs from the attribute. (For a `msg` variant the header is for
/// another ABI; the fixture `msg_variant_by_abi` runs that.)
#[test]
fn a_header_requests_msg_variant_for_an_abi_struct() {
    let source = r#"use core::derive::Derive
use core::message::MsgVariant
use core::meta::{Code, Item, Record}
use std::abi::Sol

struct Provide {}
impl Derive<trait MsgVariant<Sol>> for Provide {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const SELECTOR: u32 = 0x12345678
            type Return = u256
        }
    }
}

#[abi]
pub struct Ping { pub v: u256 }
impl MsgVariant<Sol> for Ping with (Provide)

const fn selector() -> u256 { <Ping as MsgVariant<Sol>>::SELECTOR as u256 }
"#;
    let output = prepare(source).unwrap_or_else(|cause| panic!("{cause}"));
    assert_eq!(eval(&output, "selector"), "305419896");
}

#[test]
fn a_provider_is_keyed_by_the_applied_family() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use core::ops::Eq

struct ByKey {}
impl Derive<trait Eq<u8>> for ByKey {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn eq(self, _ other: u8) -> bool { true } }
    }
}
struct P { v: u256 }
impl Eq<u8> for P with (ByKey)
"#;
    prepare(source).expect("the fixed RHS family applies to Eq<u8>");
    let different_family = source.replace(
        "impl Derive<trait Eq<u8>> for ByKey",
        "impl Derive<trait Eq> for ByKey",
    );
    let cause = prepared_diagnostics(&different_family).unwrap_or_else(|cause| cause);
    assert!(cause.contains("Derive<trait Eq<u8>>"), "{cause}");
}

#[test]
fn written_provider_keeps_the_applied_trait_family() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, type_ref}

trait Wire { const CODE: u256 }
impl Wire for u8 { const CODE: u256 = 32 }
impl Wire for u16 { const CODE: u256 = 64 }

trait Decode<A> {
    const fn decode(_ value: A) -> A
    const fn wire_code() -> u256
}

struct Fields {}
impl<A: Wire> Derive<trait Decode<A>> for Fields {
    const fn derive(_ record: Record) -> Code<Item> {
        let wire = type_ref<A>()
        let code = A::CODE
        quote {
            const fn decode(_ value: ${wire}) -> ${wire} { value }
            const fn wire_code() -> u256 { ${code} }
        }
    }
}

struct Packet {}
impl Decode<u8> for Packet with (Fields)
impl Decode<u16> for Packet with (Fields)
const fn narrow() -> u256 {
    let v: u8 = <Packet as Decode<u8>>::decode(10)
    <Packet as Decode<u8>>::wire_code() + v as u256
}
const fn wide() -> u256 {
    let v: u16 = <Packet as Decode<u16>>::decode(20)
    <Packet as Decode<u16>>::wire_code() + v as u256
}
"#;
    let output = prepare(source).unwrap_or_else(|cause| panic!("{cause}"));
    assert_eq!(eval(&output, "narrow"), "42");
    assert_eq!(eval(&output, "wide"), "84");

    // A symbolic type can pass through the generator. Its value is read by
    // the generated method after ordinary implementation specialization.
    let generic_header = source
        .replace(
            "impl Decode<u8> for Packet with (Fields)\nimpl Decode<u16> for Packet with (Fields)",
            "impl<A: Wire> Decode<A> for Packet with (Fields)",
        )
        .replace("        let code = A::CODE\n", "")
        .replace("{ ${code} }", "{ <${wire} as Wire>::CODE }");
    let output = prepare(&generic_header).unwrap_or_else(|cause| panic!("{cause}"));
    assert_eq!(eval(&output, "narrow"), "42");
    assert_eq!(eval(&output, "wide"), "84");
}
