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

impl Derive<Shape> for StableShape {
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

/// A provider opening written inside a generated member has no authored
/// identity to prepare against. Methods and constants under an explicit
/// header are both generated owners, so either is refused with the
/// generated-owner diagnostic instead of being keyed by quote syntax.
#[test]
fn provider_openings_inside_generated_members_are_refused() {
    let with_counter = SHAPE.replace(
        "struct StableShape {}\n",
        "struct StableShape {}\n\ntrait Score { const fn score() -> usize }\n\nstruct Counter {}\n\nimpl Derive<Score> for Counter {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote { const fn score() -> usize { 1 } }\n    }\n}\n",
    );
    for (from, to) in [
        (
            "            const SIDES: usize = ${n}\n",
            "            const SIDES: usize = with (Counter) { Square::score() }\n",
        ),
        (
            "            const fn area() -> usize { Self::SIDES * Self::SIDES }\n",
            "            const fn area() -> usize { with (Counter) { Square::score() } }\n",
        ),
    ] {
        assert!(with_counter.contains(from), "{from:?}");
        let cause = prepare_err(&with_counter.replace(from, to));
        assert!(
            cause.contains("generated openings need a portable generated-owner identity"),
            "{cause}"
        );
    }
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
impl Derive<Shape> for ShapeP {
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

impl Derive<Score> for Counter {
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
impl Derive<Report> for FirstField {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        quote { const fn payload_bits() -> u256 { <${ty} as Bits>::BITS } }
    }
}

struct FirstPayload {}
impl Derive<Report> for FirstPayload {
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
impl<const K: u256> Derive<Report> for Scaled<K> {
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
impl Derive<Tr> for P {
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
impl Derive<Report> for Asks {
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
impl Derive<Driver> for Stepper {
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
