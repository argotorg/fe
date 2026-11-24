use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::{ItemKind, Partial, Stmt},
};
use url::Url;

#[path = "support/generation.rs"]
mod support;

const SOURCE: &str = "use core::derive::{Derive, DeriveInherent}\nuse core::meta::{Code, Item, Record, field_count}\nmod author {\n use core::derive::{Derive, DeriveInherent}\n use core::meta::{Code, Item, Record, field_count}\n const fn boost(_ n: usize) -> u256 { n as u256 + 40 }\n pub struct Make {}\n impl DeriveInherent for Make {\n  const fn derive(_ record: Record) -> Code<Item> { let n = field_count(record)\n   quote { pub const fn generated_count() -> u256 { boost(${n}) } } }\n }\n}\nfn boost(_ n: usize) -> u256 { 999 }\nstruct Pair { first: u256, second: bool }\nimpl Pair with (author::Make)\n";

fn input(source: &str) -> (DriverDataBase, common::file::File) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/explicit-record-provider.fe").unwrap(),
        Some(source.into()),
    );
    (db, file)
}

fn assert_generated_integer(
    db: &DriverDataBase,
    file: common::file::File,
    function_name: &str,
    expected: &str,
) {
    let function = db
        .top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|function| {
            function
                .name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == function_name)
        })
        .expect("the admitted generated function must exist");
    let owner = BodyOwner::Func(function, None);
    let result = eval_body_owner_const(db, owner, GenericSubst::for_body_owner(db, owner, vec![]));
    let EvalOutcome::Ready(value) = result else {
        panic!("generated function did not evaluate: {result:?}")
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("generated function did not return an integer")
    };
    assert_eq!(value.to_string(), expected);
}

#[test]
fn output_view_keeps_the_compilation_settings_of_the_input() {
    let (mut db, file) = input(SOURCE);
    fe_driver::project_target::set_target(&mut db, common::compilation::Target::Native);
    let output = prepare_file(&db, file).unwrap().unwrap();
    assert_eq!(
        output.compilation_settings().target(&output),
        common::compilation::Target::Native
    );
    assert_eq!(
        output.compilation_settings().profile(&output),
        db.compilation_settings().profile(&db)
    );
    assert_eq!(
        output.compilation_settings().platform(&output),
        db.compilation_settings().platform(&db)
    );
}

#[test]
fn explicit_provider_is_evaluated_then_one_output_view_is_admitted() {
    let (db, file) = input(SOURCE);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_eq!(
        output_file.text(&output),
        file.text(&db),
        "generation must not rewrite source"
    );
    let module = output.top_mod(output_file);
    let function = module
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "generated_count")
        })
        .unwrap();
    let owner = BodyOwner::Func(*function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(
        matches!(value.value(&output), SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. } if value.to_string() == "42")
    );
    assert!(
        prepare_file(&output, output_file)
            .err()
            .expect("output view must refuse a second generation pass")
            .cause
            .contains("authored input view")
    );
    assert!(!db.top_mod(file).all_items(&db).iter().any(|item| matches!(item, ItemKind::Func(f) if f.name(&db).to_opt().is_some_and(|n| n.data(&db) == "generated_count"))));
}

#[test]
fn invalid_output_private_provider_and_fault_keep_their_causes() {
    for (source, cause) in [
        (SOURCE.replace("boost(${n})", "true"), "type mismatch"),
        (
            SOURCE.replace("pub struct Make", "struct Make"),
            "provider type is not visible",
        ),
        (
            SOURCE.replace(
                "let n = field_count(record)",
                "let zero: u256 = 0\n let failed = 1 / zero\n let n = field_count(record)",
            ),
            "division by zero",
        ),
    ] {
        let (db, file) = input(&source);
        let error = prepare_file(&db, file).err().expect("intended refusal");
        assert!(error.cause.contains(cause), "{}", error.cause);
        assert!(db.generation_inputs().functions(&db).is_empty());
    }
}

#[test]
fn metadata_provider_keeps_ordinary_move_and_mut_alias_checks() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

struct NonCopy {
    v: u256,
}

const fn consume(_ x: own NonCopy) -> u256 {
    x.v
}

const fn alias_bump(_ a: mut u256, _ b: mut u256) -> u256 {
    a = a + b
    a
}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let x = NonCopy { v: 20 }
        let first = consume(x)
        let second = consume(x)
        var base: u256 = 0
        let bumped = alias_bump(mut base, mut base)
        let words: usize = (first + second + bumped).downcast_unchecked()
        let total: usize = words + field_count(r)
        quote {
            pub fn generated() -> usize {
                ${total}
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (Inspect)

#[test]
fn test_borrow_bypass_executes() {
    assert(Pair::generated() == 42)
}
"#;
    let (db, file) = input(source);
    let error = prepare_file(&db, file)
        .err()
        .expect("invalid move must refuse before provider execution");
    assert!(
        error.cause.contains("move conflict") && error.cause.contains("16-0002"),
        "{error}"
    );
    assert!(error.range.start() > 0.into());

    let alias_only = source.replace(
        "let second = consume(x)",
        "let second = consume(NonCopy { v: 20 })",
    );
    let (db, file) = input(&alias_only);
    let error = prepare_file(&db, file)
        .err()
        .expect("invalid mut alias must refuse before provider execution");
    assert!(
        error.cause.contains("access conflict") && error.cause.contains("16-0001"),
        "{error}"
    );
    assert!(error.range.start() > 0.into());

    let (db, file) = input(
        r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

struct NonCopy {
    v: u256,
}

const fn consume(_ x: own NonCopy) -> u256 {
    x.v
}

const fn alias_bump(_ a: mut u256, _ b: mut u256) -> u256 {
    a = a + b
    a
}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let x = NonCopy { v: 20 }
        let first = consume(x)
        let second = consume(NonCopy { v: 20 })
        var base: u256 = 0
        var other: u256 = 0
        let bumped = alias_bump(mut base, mut other)
        let words: usize = (first + second + bumped).downcast_unchecked()
        let total: usize = words + field_count(r)
        quote {
            pub fn generated() -> usize {
                ${total}
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (Inspect)

#[test]
fn test_borrow_bypass_executes() {
    assert(Pair::generated() == 42)
}
"#,
    );
    assert!(prepare_file(&db, file).unwrap().is_some());
}

#[test]
fn helper_type_error_retains_original_cause_and_span() {
    let (db, file) = input(
        r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

const fn broken_helper() -> usize {
    let bad: usize = true
    bad
}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let n = field_count(r) + broken_helper()
        quote {
            pub fn generated() -> usize {
                ${n}
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (Inspect)
"#,
    );
    let error = prepare_file(&db, file)
        .err()
        .expect("invalid helper must refuse");
    assert!(
        error.cause.contains("type mismatch")
            && error.cause.contains("usize")
            && error.cause.contains("bool"),
        "{error}"
    );
    assert!(
        !error.cause.contains("internal borrow checking error") && !error.cause.contains("Ctfe("),
        "{error}"
    );
    assert!(error.range.start() > 0.into());
}

#[test]
fn nested_hole_refuses_with_original_constructor_cause() {
    let (db, file) = input(
        r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let n = field_count(r)
        quote {
            pub fn generated() -> usize {
                ${ ${n} }
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (Inspect)
"#,
    );
    let error = prepare_file(&db, file)
        .err()
        .expect("nested holes remain unsupported");
    assert!(
        error.cause.contains("nested quotation holes") && !error.cause.contains("Ctfe("),
        "{error}"
    );
    assert!(error.range.start() > 0.into());
}

#[test]
fn runtime_metadata_refuses_and_compile_time_quotes_stay_valid() {
    for source in [
        "use core::meta::{Code, Item}\nfn runtime_quote() -> Code<Item> { quote { fn generated() -> usize { 1 } } }\n",
        "use core::meta::Record\nfn runtime_tuple(_ value: (Record, usize)) -> (Record, usize) { value }\n",
        "use core::meta::Record\nstruct Wrapped { value: Record }\nfn runtime_wrapper(_ value: Wrapped) -> Wrapped { value }\n",
    ] {
        let (db, file) = input(source);
        let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
        assert!(
            diagnostics.contains("8-0137") && diagnostics.contains("compile-time-only"),
            "{diagnostics}"
        );
        assert!(!diagnostics.contains("16-0004"), "{diagnostics}");
    }
    for source in [
        r#"use core::meta::{Code, Item}

const fn helper_with_local_quote() -> usize {
    let _q: Code<Item> = quote {
        fn generated() -> usize {
            1
        }
    }
    1
}

#[test]
fn test_helper() {
    assert(helper_with_local_quote() == 1)
}
"#,
        "use core::meta::{Code, Item, Record}\nconst fn compile_time_tuple(_ value: (Record, usize)) -> usize { let _q: Code<Item> = quote { fn generated() -> usize { 1 } }\n value.1 }\n",
        "use core::meta::{Code, Item}\nconst Q: Code<Item> = quote { fn generated() -> usize { 1 } }\n",
    ] {
        let (db, file) = input(source);
        let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
        assert!(diagnostics.is_empty(), "{diagnostics}");
    }
}

#[test]
fn checked_metadata_tuple_and_compile_time_wrapper_declaration_stay_valid() {
    let (db, file) = input(
        "use core::meta::{Code, Item}\nconst PAIR: (Code<Item>, usize) = (quote { fn generated() -> usize { 1 } }, 42)\n",
    );
    let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    assert!(diagnostics.is_empty(), "{diagnostics}");
    let (db, file) =
        input("use core::meta::Record\npub struct UnusedWrapper {\n    pub value: Record,\n}\n");
    let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    assert!(
        diagnostics.is_empty(),
        "unused declaration may be consumed only at compile time: {diagnostics}"
    );
}

#[test]
fn function_local_static_helper_retains_producer_origin() {
    let (db, file) = input(
        r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

mod author {
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Item, Record, field_count}

    pub struct Inspect {}
    impl DeriveInherent for Inspect {
        const fn derive(_ r: Record) -> Code<Item> {
            const fn local_boost(_ n: usize) -> usize {
                n + 40
            }
            let n = field_count(r)
            quote {
                pub fn generated() -> usize {
                    local_boost(${n})
                }
            }
        }
    }
}

fn local_boost(_ n: usize) -> usize {
    999
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (author::Inspect)

#[test]
fn test_func_local_hygiene() {
    assert(Pair::generated() == 42)
}
"#,
    );
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert!(
        output
            .run_on_top_mod(output.top_mod(output_file))
            .is_empty()
    );
}

#[test]
fn unspliced_producer_local_is_not_a_runtime_capture() {
    let (db, file) = input(
        r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

mod author {
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Item, Record, field_count}

    pub struct Inspect {}
    impl DeriveInherent for Inspect {
        const fn derive(_ r: Record) -> Code<Item> {
            const fn local_boost(_ n: usize) -> usize {
                n + 40
            }
            let n = field_count(r)
            quote {
                pub fn generated() -> usize {
                    n
                }
            }
        }
    }
}

fn local_boost(_ n: usize) -> usize {
    999
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (author::Inspect)

#[test]
fn test_func_local_hygiene() {
    assert(Pair::generated() == 42)
}
"#,
    );
    let errors = prepare_file(&db, file)
        .err()
        .expect("unspliced local must refuse");
    let rendered = format!("{errors:?}");
    assert!(
        rendered.contains("undefined variable") && rendered.contains("n"),
        "{rendered}"
    );
}

#[test]
fn provider_with_declared_trait_bound_and_named_choice_splices_into_generated_item() {
    let source = r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

trait Base {
    const B: usize
    const fn base(self) -> usize
}

impl Base for u32 {
    const B: usize = 30
    const fn base(self) -> usize { 40 }
}

trait Rate {
    const fn rate(self) -> usize
}

impl Rate for u32
where
    u32: Base
{
    const fn rate(self) -> usize {
        <u32 as Base>::B + self.base()
    }
}

impl Rate for u32 as FlatRate {
    const fn rate(self) -> usize { 10 }
}

const fn rate_with<T: Rate>(_ x: T) -> usize {
    x.rate()
}

pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let fields = field_count(record)
        let zero: u32 = 0
        let proved = rate_with(zero)
        let flat = with (FlatRate) { zero.rate() }
        let total = fields + proved + flat
        quote {
            pub const fn generated_quote_total() -> usize {
                ${total}
            }
        }
    }
}

struct Pair {
    first: u256,
    second: bool,
}
impl Pair with (Make)
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(output.run_on_top_mod(module).is_empty());
    let function = module
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "generated_quote_total")
        })
        .unwrap();
    let owner = BodyOwner::Func(*function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(
        matches!(
            value.value(&output),
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } if value.to_string() == "82"
        ),
        "expected 2 + (30 + 40) + 10 = 82, got {value:?}"
    );
}

#[test]
fn generic_struct_within_with_fold_fields_and_field_type_splice() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Expr, Field, Item, Record, field, field_count, field_type}
use core::ops::Ord
use std::derive::{FieldStep, fold_fields}

struct WithinStep {}
impl FieldStep for WithinStep {
    const fn unit() -> Code<Expr> {
        quote { true }
    }

    const fn step(_ acc: Code<Expr>, _ f: Field) -> Code<Expr> {
        quote(min, max) {
            ${acc} && self.${f} >= min && self.${f} <= max
        }
    }
}

pub struct Within {}
impl DeriveInherent for Within {
    const fn derive(_ record: Record) -> Code<Item> {
        let _ = field_count(record)
        let ty = field_type(field(record, 0))
        let body = fold_fields<WithinStep>(record)
        quote(min, max) {
            pub fn within(self, _ min: ${ty}, _ max: ${ty}) -> bool {
                ${body}
            }
        }
    }
}

pub struct ConstWithin {}
impl DeriveInherent for ConstWithin {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        let body = fold_fields<WithinStep>(record)
        quote(min, max) {
            pub const fn is_within(self, _ min: ${ty}, _ max: ${ty}) -> bool {
                ${body}
            }
        }
    }
}

struct Box2<T: Ord> {
    width: T,
    height: T,
}
impl<T: Ord> Box2<T> with (Within)

struct U256Box {
    width: u256,
    height: u256,
}
impl U256Box with (ConstWithin)

fn check_generic_within() -> bool {
    let ok = Box2 { width: 10 as u256, height: 20 as u256 }.within(5, 25)
    let bad = Box2 { width: 10 as u256, height: 30 as u256 }.within(5, 25)
    ok && !bad
}

const fn check_within() -> bool {
    let ok = U256Box { width: 10, height: 20 }.is_within(5, 25)
    let bad = U256Box { width: 10, height: 30 }.is_within(5, 25)
    ok && !bad
}
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    let diags = output.run_on_top_mod(module);
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));
    let function = module
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "check_within")
        })
        .unwrap();
    let owner = BodyOwner::Func(*function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(matches!(
        value.value(&output),
        SemConstValue::Scalar {
            value: SemConstScalar::Bool(true),
            ..
        }
    ));
}

#[test]
fn enum_eq_across_unit_tuple_record_and_generic_enums_with_hygienic_binders() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Arm, Code, Expr, Item, Record, binders, empty, field, field_count, variant, variant_count}
use core::ops::Eq

pub struct EnumEq {}
impl DeriveInherent for EnumEq {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = variant_count(record)

        var arms = empty<Arm>()
        var i: usize = 0
        while i < n {
            let v = variant(record, i)
            let lhs = binders(v, 0)
            let rhs = binders(v, 1)
            let m = field_count(v)

            var eq: Code<Expr> = quote { true }
            var j: usize = 0
            while j < m {
                let f = field(v, j)
                let part = quote { ${lhs.at(f)} == ${rhs.at(f)} }
                eq = quote { ${eq} && ${part} }
                j += 1
            }

            arms = quote(other) {
                ${arms}
                ${lhs} => match other {
                    ${rhs} => ${eq}
                    _ => false
                }
            }
            i += 1
        }

        quote(other) {
            pub fn eq(self, _ other: Self) -> bool {
                match self {
                    ${arms}
                }
            }
        }
    }
}

pub struct ConstEnumEq {}
impl DeriveInherent for ConstEnumEq {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = variant_count(record)

        var arms = empty<Arm>()
        var i: usize = 0
        while i < n {
            let v = variant(record, i)
            let lhs = binders(v, 0)
            let rhs = binders(v, 1)
            let m = field_count(v)

            var eq: Code<Expr> = quote { true }
            var j: usize = 0
            while j < m {
                let f = field(v, j)
                let part = quote { ${lhs.at(f)} == ${rhs.at(f)} }
                eq = quote { ${eq} && ${part} }
                j += 1
            }

            arms = quote(other) {
                ${arms}
                ${lhs} => match other {
                    ${rhs} => ${eq}
                    _ => false
                }
            }
            i += 1
        }

        quote(other) {
            pub const fn is_eq(self, _ other: Self) -> bool {
                match self {
                    ${arms}
                }
            }
        }
    }
}

enum Direction {
    North,
    South,
}
impl Direction with (EnumEq)
impl Direction with (ConstEnumEq)

enum Shape {
    Unit,
    Scale(u256),
    Box { w: u256, h: u256 },
    Pair { other: u256, x: u256 },
}
impl Shape with (EnumEq)
impl Shape with (ConstEnumEq)

enum Envelope<T: Eq> {
    Empty,
    Value(T),
    Pair { other: T, x: T },
}
impl<T: Eq> Envelope<T> with (EnumEq)

enum Opt<T> {
    Nothing,
    Just(T),
}
impl<T> Opt<T> with (EnumEq)
where
    T: Eq

fn check_generic_enums() -> bool {
    let p1 = Envelope::Pair { other: 10 as u256, x: 20 as u256 }
    let p2 = Envelope::Pair { other: 10 as u256, x: 20 as u256 }
    let p3 = Envelope::Pair { other: 99 as u256, x: 20 as u256 }
    let e1 = p1.eq(p2)
    let e2 = !p1.eq(p3)
    let j1: Opt<u256> = Opt::Just(7)
    let j2: Opt<u256> = Opt::Just(7)
    let n1: Opt<u256> = Opt::Nothing
    let o1 = j1.eq(j2)
    let o2 = !j1.eq(n1)
    e1 && e2 && o1 && o2
}

const fn check_enums() -> bool {
    let d1 = Direction::North.is_eq(Direction::North)
    let d2 = !Direction::North.is_eq(Direction::South)
    let s1 = Shape::Scale(7).is_eq(Shape::Scale(7))
    let s2 = !Shape::Scale(7).is_eq(Shape::Scale(8))
    let b1 = Shape::Box { w: 3, h: 4 }
    let b2 = Shape::Box { w: 3, h: 4 }
    let b3 = Shape::Box { w: 3, h: 4 }
    let s3 = b1.is_eq(b2)
    let s4 = !b3.is_eq(Shape::Unit)
    let sp1 = Shape::Pair { other: 10, x: 20 }
    let sp2 = Shape::Pair { other: 10, x: 20 }
    let sp3 = Shape::Pair { other: 10, x: 20 }
    let sp4 = Shape::Pair { other: 99, x: 20 }
    let s5 = sp1.is_eq(sp2)
    let s6 = !sp3.is_eq(sp4)
    d1 && d2 && s1 && s2 && s3 && s4 && s5 && s6
}
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    let diags = output.run_on_top_mod(module);
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));
    let function = module
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "check_enums")
        })
        .unwrap();
    let owner = BodyOwner::Func(*function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(matches!(
        value.value(&output),
        SemConstValue::Scalar {
            value: SemConstScalar::Bool(true),
            ..
        }
    ));
}

#[test]
fn field_type_queries_and_abi_derive_helpers_evaluate_in_provider() {
    let source = r#"use core::abi::AbiSize
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, FieldQuery, Item, Record, field, field_count, field_type, query_field, type_is}
use std::derive::{AbiHeadSize, AbiIsDynamic, fold_fields}

struct AbiFacts<T> {}
impl<T: AbiSize> FieldQuery<(u256, bool)> for AbiFacts<T> {
    const fn evaluate() -> (u256, bool) {
        (<T as AbiSize>::HEAD_SIZE, <T as AbiSize>::IS_DYNAMIC)
    }
}

pub struct InspectAbi {}
impl DeriveInherent for InspectAbi {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        var head_bytes: u256 = 0
        var u256_fields: u256 = 0
        var i: usize = 0
        while i < n {
            let f = field(record, i)
            let ty = field_type(f)
            let (field_head, _field_dyn) = query_field<AbiFacts, (u256, bool)>(ty)
            head_bytes += field_head
            if type_is<u256>(ty) {
                u256_fields += 1
            }
            i += 1
        }
        let head_expr = fold_fields<AbiHeadSize>(record)
        let dynamic_expr = fold_fields<AbiIsDynamic>(record)
        quote {
            pub const fn record_abi_summary() -> (u256, u256, u256, bool) {
                (${head_bytes}, ${u256_fields}, ${head_expr}, ${dynamic_expr})
            }
        }
    }
}

struct Packet {
    id: u256,
    ok: bool,
    amount: u256,
}
impl Packet with (InspectAbi)

const fn check_summary() -> u256 {
    let (queried_bytes, u256_count, folded_bytes, dynamic) = Packet::record_abi_summary()
    if !dynamic && queried_bytes == folded_bytes {
        queried_bytes + u256_count
    } else {
        0
    }
}
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(output.run_on_top_mod(module).is_empty());
    let function = module
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "check_summary")
        })
        .unwrap();
    let owner = BodyOwner::Func(*function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(
        matches!(
            value.value(&output),
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } if value.to_string() == "98"
        ),
        "expected 3 * 32 + 2 = 98, got {value:?}"
    );
}

#[test]
fn a_hidden_member_refusal_names_its_own_header_when_headers_share_a_provider() {
    // Both headers run the same provider, so the text of the hidden member
    // lies in a quotation that both requests hold. The refusal names the
    // header of the record whose member is hidden, not the first one.
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, empty, field, field_count}

pub struct Keys {}
impl DeriveInherent for Keys {
    const fn derive(_ record: Record) -> Code<Item> {
        var items = empty<Item>()
        var i: usize = 0
        while i < field_count(record) {
            let f = field(record, i)
            items = quote {
                ${items}
                pub const ${f}: u256 = 0
            }
            i += 1
        }
        items
    }
}

pub struct Scal { pub id: u256 }
impl Scal with (Keys)

pub struct Acct { pub owner: u256, pub amount: u256 }
impl Acct {
    pub fn amount(self) -> u256 { self.amount }
}
impl Acct with (Keys)
"#;
    let (db, file) = input(source);
    let err = prepare_file(&db, file)
        .err()
        .expect("a generated key hidden by a written method must fail");
    let header = "impl Acct with (Keys)";
    let start = source.find(header).unwrap();
    let end = start + header.len();
    assert_eq!(
        (usize::from(err.range.start()), usize::from(err.range.end())),
        (start, end),
        "{}",
        err.cause
    );
    assert!(
        err.cause.contains(&format!(
            "the generated `amount` requested at file:///tmp/explicit-record-provider.fe:{start}..{end} would be hidden: `Acct` already has an inherent method `amount` of `Acct`"
        )),
        "{}",
        err.cause
    );
}

#[test]
fn quote_binder_hygiene_rejects_sibling_arm_after_match_nested_group_and_foreign_field() {
    let sibling_arm = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ record: Record) -> Code<Item> {
        let v = variant(record, 0)
        let lhs = binders(v, 0)
        let f = field(v, 0)
        quote {
            pub fn check(self) -> bool {
                match self {
                    ${lhs} => true,
                    _ => ${lhs.at(f)} == 0
                }
            }
        }
    }
}

enum Choice {
    One(u256),
    Two,
}
impl Choice with (Bad)
"#;
    let after_match = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ record: Record) -> Code<Item> {
        let v = variant(record, 0)
        let lhs = binders(v, 0)
        let f = field(v, 0)
        quote {
            pub fn check(self) -> bool {
                let matched = match self {
                    ${lhs} => true,
                    _ => false
                }
                matched && ${lhs.at(f)} == 0
            }
        }
    }
}

enum Choice {
    One(u256),
    Two,
}
impl Choice with (Bad)
"#;
    let same_group_nested = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ record: Record) -> Code<Item> {
        let v = variant(record, 0)
        let outer = binders(v, 0)
        let inner = binders(v, 0)
        let f = field(v, 0)
        quote(other) {
            pub fn check(self, _ other: Self) -> bool {
                match self {
                    ${outer} => match other {
                        ${inner} => ${outer.at(f)} == 0,
                        _ => false
                    },
                    _ => false
                }
            }
        }
    }
}

enum Choice {
    One(u256),
    Two,
}
impl Choice with (Bad)
"#;
    let foreign_field = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ record: Record) -> Code<Item> {
        let v0 = variant(record, 0)
        let v1 = variant(record, 1)
        let lhs = binders(v0, 0)
        let foreign = field(v1, 0)
        let bad_expr = lhs.at(foreign)
        quote {
            pub fn check(self) -> bool {
                match self {
                    ${lhs} => ${bad_expr} == 0,
                    _ => false
                }
            }
        }
    }
}

enum Choice {
    One(u256),
    Two(u256),
}
impl Choice with (Bad)
"#;

    for (source, expected) in [
        (sibling_arm, "binder group is not bound in this scope"),
        (after_match, "binder group is not bound in this scope"),
        (
            same_group_nested,
            "binder group already bound in an enclosing pattern",
        ),
        (
            foreign_field,
            "`.at` field belongs to another owner or variant",
        ),
    ] {
        let (db, file) = input(source);
        let error = prepare_file(&db, file)
            .err()
            .expect("expected binder hygiene refusal");
        assert!(
            error.cause.contains(expected),
            "expected `{expected}`, got `{}`",
            error.cause
        );
    }
}

#[test]
fn sub_quote_hygiene_and_cross_module_field_type_and_generic_bounds_resolution() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Expr, Field, Item, Record, field, field_type}

mod helper_mod {
    use core::meta::{Code, Expr}

    const fn secret_offset() -> u256 {
        40
    }

    pub const fn make_sub_expr() -> Code<Expr> {
        quote(extra) {
            secret_offset() + extra
        }
    }
}

mod provider_mod {
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Expr, Field, Item, Record, field, field_type}
    use super::helper_mod::make_sub_expr

    pub struct GenerateImpl {}
    impl DeriveInherent for GenerateImpl {
        const fn derive(_ record: Record) -> Code<Item> {
            let f0: Field = field(record, 0)
            let ty0 = field_type(f0)
            let sub: Code<Expr> = make_sub_expr()
            quote(extra) {
                pub const fn first_val(self) -> ${ty0} {
                    self.${f0}
                }

                pub const fn compute(self, _ extra: u256) -> u256 {
                    ${sub}
                }
            }
        }
    }
}

fn secret_offset() -> u256 {
    999
}

trait LocalTrait {
    const fn tag(self) -> u256
}

struct LocalField {
    v: u256,
}

impl LocalTrait for u256 {
    const fn tag(self) -> u256 {
        self
    }
}

struct TargetRecord<T: LocalTrait> {
    first: LocalField,
    second: T,
}
impl<T: LocalTrait> TargetRecord<T> with (provider_mod::GenerateImpl)

const fn verify_hygiene() -> u256 {
    let rec = TargetRecord {
        first: LocalField { v: 2 },
        second: 10 as u256,
    }
    rec.first_val().v + rec.compute(0)
}
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    let diags = output.run_on_top_mod(module);
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));
    let function = module
        .all_funcs(&output)
        .iter()
        .find(|f| {
            f.name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "verify_hygiene")
        })
        .unwrap();
    let owner = BodyOwner::Func(*function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(
        matches!(
            value.value(&output),
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } if value.to_string() == "42"
        ),
        "expected 2 + 40 = 42, got {value:?}"
    );

    let unbound_open = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

pub struct BadOpen {}
impl DeriveInherent for BadOpen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote(missing_binder) {
            pub fn check(self) -> u256 {
                missing_binder
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (BadOpen)
"#;
    let (db, file) = input(unbound_open);
    let error = prepare_file(&db, file)
        .err()
        .expect("unbound open name must be refused");
    assert!(
        error
            .cause
            .contains("quoted open name `missing_binder` has no enclosing binder"),
        "{}",
        error.cause
    );
}

#[test]
fn binder_hygiene_avoids_user_ident_collision_and_scopes_let_if_let_and_logical_or() {
    let no_collision = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct MakeCheck {}
impl DeriveInherent for MakeCheck {
    const fn derive(_ record: Record) -> Code<Item> {
        let v = variant(record, 0)
        let lhs = binders(v, 0)
        let f = field(v, 0)
        quote {
            pub const fn extract(self) -> u256 {
                let __fe_binder_0_0: u256 = 100
                match self {
                    ${lhs} => ${lhs.at(f)} + __fe_binder_0_0,
                    _ => 0
                }
            }
        }
    }
}

enum CollisionEnum {
    Val { __fe_binder_0_0: u256 },
    Empty,
}
impl CollisionEnum with (MakeCheck)

const fn check_collision() -> u256 {
    let v = CollisionEnum::Val { __fe_binder_0_0: 42 }
    v.extract()
}
"#;
    let (db, file) = input(no_collision);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    let diags = output.run_on_top_mod(module);
    assert!(diags.is_empty(), "{}", diags.format_diags(&output));

    let let_init_leak = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ record: Record) -> Code<Item> {
        let v = variant(record, 0)
        let lhs = binders(v, 0)
        let f = field(v, 0)
        quote {
            pub fn check(self) -> bool {
                let ${lhs} = ${lhs.at(f)}
                true
            }
        }
    }
}

enum Single {
    One(u256),
}
impl Single with (Bad)
"#;
    let if_let_else_leak = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ record: Record) -> Code<Item> {
        let v = variant(record, 0)
        let lhs = binders(v, 0)
        let f = field(v, 0)
        quote {
            pub fn check(self) -> bool {
                if let ${lhs} = self {
                    ${lhs.at(f)} == 0
                } else {
                    ${lhs.at(f)} == 0
                }
            }
        }
    }
}

enum Choice {
    One(u256),
    Two,
}
impl Choice with (Bad)
"#;
    let while_let_after_leak = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, binders, field, variant}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ record: Record) -> Code<Item> {
        let v = variant(record, 0)
        let lhs = binders(v, 0)
        let f = field(v, 0)
        quote {
            pub fn check(self) -> bool {
                while let ${lhs} = self {
                    break
                }
                ${lhs.at(f)} == 0
            }
        }
    }
}

enum Choice {
    One(u256),
    Two,
}
impl Choice with (Bad)
"#;
    for source in [let_init_leak, if_let_else_leak, while_let_after_leak] {
        let (db, file) = input(source);
        let error = prepare_file(&db, file)
            .err()
            .expect("expected binder scope refusal");
        assert!(
            error
                .cause
                .contains("binder group is not bound in this scope"),
            "got `{}`",
            error.cause
        );
    }
}

#[test]
fn query_field_evaluate_and_transitive_callee_enforce_access_checks() {
    let bad_query_field = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, FieldQuery, Item, Record, field, field_type, query_field}

struct NonCopy {
    v: u256,
}

const fn consume(_ x: own NonCopy) -> u256 {
    x.v
}

struct BadQuery<T> {}
impl<T> FieldQuery<u256> for BadQuery<T> {
    const fn evaluate() -> u256 {
        let x = NonCopy { v: 1 }
        let a = consume(x)
        let b = consume(x)
        a + b
    }
}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let ty = field_type(field(r, 0))
        let v = query_field<BadQuery, u256>(ty)
        quote {
            pub const fn out() -> u256 {
                ${v}
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (Inspect)
"#;
    let (db, file) = input(bad_query_field);
    let error = prepare_file(&db, file)
        .err()
        .expect("FieldQuery::evaluate with move conflict must be refused");
    assert!(
        error
            .cause
            .contains("invalid body in const evaluation context")
            || error.cause.contains("move conflict"),
        "{}",
        error.cause
    );

    let bad_transitive_helper = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

struct NonCopy {
    v: usize,
}

const fn consume(_ x: own NonCopy) -> usize {
    x.v
}

const fn transitive_bad() -> usize {
    let x = NonCopy { v: 1 }
    consume(x) + consume(x)
}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let n = field_count(r) + transitive_bad()
        quote {
            pub const fn out() -> usize {
                ${n}
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (Inspect)
"#;
    let (db, file) = input(bad_transitive_helper);
    let error = prepare_file(&db, file)
        .err()
        .expect("transitive callee with move conflict must be refused");
    assert!(
        error.cause.contains("move conflict") && error.cause.contains("16-0002"),
        "{}",
        error.cause
    );
}

#[test]
fn a_constant_read_by_a_field_query_is_checked_like_a_called_function() {
    // `evaluate` reads a constant whose initializer calls a function with a
    // move conflict. The constant is not a callee, but evaluating it runs the
    // function, so the query is refused as it is when `evaluate` calls the
    // function directly.
    let bad_constant = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, FieldQuery, Item, Record, field, field_type, query_field}

struct NonCopy {
    v: u256,
}

const fn consume(_ x: own NonCopy) -> u256 {
    x.v
}

const fn bad() -> u256 {
    let x = NonCopy { v: 1 }
    let a = consume(x)
    let b = consume(x)
    a + b
}

const K: u256 = bad()

struct Q<T> {}
impl<T> FieldQuery<u256> for Q<T> {
    const fn evaluate() -> u256 {
        K
    }
}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let ty = field_type(field(r, 0))
        let v = query_field<Q, u256>(ty)
        quote {
            pub const fn out() -> u256 {
                ${v}
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (Inspect)
"#;
    let (db, file) = input(bad_constant);
    let error = prepare_file(&db, file)
        .err()
        .expect("a constant whose initializer calls an invalid function must be refused");
    assert!(
        error
            .cause
            .contains("invalid body in const evaluation context"),
        "{}",
        error.cause
    );
}

#[test]
fn mismatched_splice_kinds_are_refused_and_a_quoted_generate_attribute_is_ignored() {
    let field_in_type_hole = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ r: Record) -> Code<Item> {
        let f = field(r, 0)
        quote {
            pub fn generated(_ x: ${f}) -> u256 {
                0
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (Bad)
"#;
    let expr_in_where_hole = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Expr, Item, Record}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ r: Record) -> Code<Item> {
        let e: Code<Expr> = quote { true }
        quote {
            impl ${r} where ${e} {
                pub fn check(self) -> bool {
                    true
                }
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (Bad)
"#;
    let expr_in_arm_hole = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Expr, Item, Record}

pub struct Bad {}
impl DeriveInherent for Bad {
    const fn derive(_ r: Record) -> Code<Item> {
        let e: Code<Expr> = quote { true }
        quote {
            pub fn check(self) -> bool {
                match self {
                    ${e}
                }
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (Bad)
"#;
    let recursive_generate = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

pub struct LoopGen {}
impl DeriveInherent for LoopGen {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            #[generate(LoopGen)]
            pub fn generated() -> u256 {
                1
            }
        }
    }
}

struct Pair {
    a: u256,
}
impl Pair with (LoopGen)
"#;
    for (source, expected) in [
        (
            field_in_type_hole,
            "type quotation hole requires a record, field type, or checked type reference",
        ),
        (
            expr_in_where_hole,
            "where-clause quotation hole requires Code<Where> splice",
        ),
        (
            expr_in_arm_hole,
            "match-arm quotation hole requires Code<Arm> splice",
        ),
    ] {
        let (db, file) = input(source);
        let error = prepare_file(&db, file)
            .err()
            .expect("expected splice/attribute refusal");
        assert!(
            error.cause.contains(expected),
            "expected `{expected}`, got `{}`",
            error.cause
        );
    }
    let (db, file) = input(recursive_generate);
    assert!(
        prepare_file(&db, file).is_ok(),
        "a #[generate] attribute in generated code is an ordinary unknown attribute"
    );
}

#[test]
fn provider_field_and_field_type_projection_and_boundary_refusals() {
    let valid_source = r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Field, FieldType, Item, Record, field, field_count, field_type}

pub struct InspectFields {}
impl DeriveInherent for InspectFields {
    const fn derive(_ record: Record) -> Code<Item> {
        let count = field_count(record)
        let first: Field = field(record, 0)
        let second: Field = field(record, 1)
        let _first_ty: FieldType = field_type(first)
        let _second_ty: FieldType = field_type(second)
        quote {
            pub const fn generated_field_summary() -> usize {
                ${count}
            }
        }
    }
}

struct Pair {
    first: u256,
    second: bool,
}
impl Pair with (InspectFields)
"#;
    let (db, file) = input(valid_source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(output.run_on_top_mod(module).is_empty());

    let oob_source = valid_source.replace(
        "let second: Field = field(record, 1)",
        "let second: Field = field(record, 2)",
    );
    let (db, file) = input(&oob_source);
    let oob_err = prepare_file(&db, file)
        .err()
        .expect("out-of-bounds field index must refuse");
    assert!(
        oob_err
            .cause
            .contains("constant index outside record or aggregate"),
        "{}",
        oob_err.cause
    );

    // A header written in a nested module is a member of that module: it
    // resolves its provider there and gives the record declared there its
    // members.
    let submod_source = r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ r: Record) -> Code<Item> {
        let n = field_count(r)
        quote {
            pub const fn generated() -> usize { ${n} }
        }
    }
}

mod nested {
    pub struct Inner {
        pub x: u256,
        pub y: u256,
    }
    impl Inner with (super::Make)
}

const fn probe() -> usize { nested::Inner::generated() }
"#;
    let (db, file) = input(submod_source);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{error}"))
        .expect("a prepared view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_integer(&output, output_file, "probe", "2");

    // A header declared in a function body is refused, never ignored.
    let in_body = submod_source.replace(
        "const fn probe() -> usize { nested::Inner::generated() }",
        "fn local() {\n    struct Local { x: u256 }\n    impl Local with (Make)\n}",
    );
    let (db, file) = input(&in_body);
    assert!(
        prepare_file(&db, file).is_err(),
        "a body-local header must refuse, never be ignored"
    );
}

#[test]
fn transitive_helper_access_check_and_unsafe_block_hygiene_and_quoted_attrs() {
    // Transitive #1821 ownership/move check on provider helper callees before CTFE.
    let helper_move_source = r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

struct NonCopy {
    v: u256,
}

const fn consume(_ x: own NonCopy) -> u256 {
    x.v
}

const fn bad_move_helper() -> usize {
    let x = NonCopy { v: 20 }
    let first = consume(x)
    let second = consume(x)
    let words: usize = (first + second).downcast_unchecked()
    words
}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        let total = field_count(r) + bad_move_helper()
        quote {
            pub fn generated() -> usize {
                ${total}
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (Inspect)
"#;
    let (db, file) = input(helper_move_source);
    let helper_err = prepare_file(&db, file)
        .err()
        .expect("helper move conflict must fail before CTFE");
    assert!(
        helper_err
            .cause
            .contains("authored provider ownership/capability validation failed")
            && helper_err.cause.contains("move conflict")
            && helper_err.cause.contains("16-0002"),
        "{helper_err}"
    );

    // Quote directly inside `unsafe { ... }` block preserves producer-block lexical hygiene without panicking.
    let unsafe_quote_source = r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field_count}

mod author {
    use core::derive::{Derive, DeriveInherent}
    use core::meta::{Code, Item, Record, field_count}

    pub struct Inspect {}
    impl DeriveInherent for Inspect {
        const fn derive(_ r: Record) -> Code<Item> {
            let n = field_count(r)
            unsafe {
                const fn unsafe_local_boost(_ x: usize) -> usize {
                    x + 40
                }
                quote {
                    pub const fn generated_from_unsafe() -> usize {
                        unsafe_local_boost(${n})
                    }
                }
            }
        }
    }
}

const fn unsafe_local_boost(_ x: usize) -> usize {
    999
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}
impl Pair with (author::Inspect)
"#;
    let (db, file) = input(unsafe_quote_source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(output.run_on_top_mod(module).is_empty());

    // A #[generate(..)] attribute, written or quoted, is an unknown attribute and is ignored.
    let module_generate_source = r#"
#[generate(Inspect)]
pub struct Pair {
    pub a: u256,
}
"#;
    let (db, file) = input(module_generate_source);
    let module = db.top_mod(file);
    let diags = db.run_on_top_mod(module).format_diags(&db);
    assert!(!diags.contains("#[generate]"), "{diags}");

    let quoted_generate_source = r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            #[generate(Inspect)]
            pub fn generated() -> usize {
                1
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
}
impl Pair with (Inspect)
"#;
    let (db, file) = input(quoted_generate_source);
    assert!(prepare_file(&db, file).is_ok());

    let trait_header_quoted_generate_source = r#"
use core::derive::Derive
use core::meta::{Code, Item, Record}

pub trait Tag {
    fn tag(self) -> usize
}

pub struct MakeTag {}
impl Derive<trait Tag> for MakeTag {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            #[generate(MakeTag)]
            fn tag(self) -> usize {
                1
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
}
impl Tag for Pair with (MakeTag)
"#;
    let (db, file) = input(trait_header_quoted_generate_source);
    assert!(prepare_file(&db, file).is_ok());

    // Quoted function carrying invalid function attribute (`#[event]`) is diagnosed by `validate_func_attrs`.
    let quoted_event_attr_source = r#"
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

pub struct Inspect {}
impl DeriveInherent for Inspect {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            #[event]
            pub fn generated() -> usize {
                1
            }
        }
    }
}

pub struct Pair {
    pub a: u256,
}
impl Pair with (Inspect)
"#;
    let (db, file) = input(quoted_event_attr_source);
    let quoted_attr_err = prepare_file(&db, file)
        .err()
        .expect("quoted #[event] on function must fail output admission");
    assert!(
        quoted_attr_err.cause.contains("`#[event]`") && quoted_attr_err.cause.contains("structs"),
        "{}",
        quoted_attr_err.cause
    );
}

#[test]
fn generation_provider_path_trivia_preserves_identity() {
    for path in ["author :: Make", "author:: Make", "author ::Make"] {
        let source = SOURCE.replace("author::Make", path);
        let (db, file) = input(&source);
        let output = prepare_file(&db, file).unwrap().unwrap();
        let output_file = output
            .workspace()
            .get(&output, &file.url(&db).unwrap())
            .unwrap();
        let module = output.top_mod(output_file);
        let function = module
            .all_funcs(&output)
            .iter()
            .find(|function| {
                function
                    .name(&output)
                    .to_opt()
                    .is_some_and(|name| name.data(&output) == "generated_count")
            })
            .copied()
            .unwrap();
        let owner = BodyOwner::Func(function, None);
        let result = eval_body_owner_const(
            &output,
            owner,
            GenericSubst::for_body_owner(&output, owner, vec![]),
        );
        let EvalOutcome::Ready(value) = result else {
            panic!("{path}: {result:?}")
        };
        assert!(
            matches!(value.value(&output), SemConstValue::Scalar {
            value: SemConstScalar::Int { value }, ..
        } if value.to_string() == "42"),
            "{path}"
        );
    }
}

#[test]
fn quoted_impl_same_name_generic_bounds_are_preserved() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record, field, field_type}
trait Extra { const fn value() -> u256 }
struct HasExtra {}
struct NoExtra {}
impl Extra for HasExtra { const fn value() -> u256 { 7 } }
pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        quote {
            pub const fn mark() -> u256 { <${ty} as Extra>::value() }
        }
    }
}
struct Box<T> { value: T }
impl<T: Extra> Box<T> with (Make)
const fn observe() -> u256 { Box<HasExtra>::mark() }
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    let function = module
        .all_funcs(&output)
        .iter()
        .find(|function| {
            function
                .name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "observe")
        })
        .copied()
        .unwrap();
    let owner = BodyOwner::Func(function, None);
    let result = eval_body_owner_const(
        &output,
        owner,
        GenericSubst::for_body_owner(&output, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(matches!(value.value(&output), SemConstValue::Scalar {
        value: SemConstScalar::Int { value }, ..
    } if value.to_string() == "7"));
    let invalid = source.replace("Box<HasExtra>", "Box<NoExtra>");
    let (db, file) = input(&invalid);
    let error = support::program_errors(&db, file);
    assert!(error.contains("Extra") || error.contains("Mark"), "{error}");
}

#[test]
fn quoted_impl_incompatible_same_name_generic_kind_is_refused() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}
pub struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn f() -> usize { 1 } }
    }
}
struct Box<T> { value: T }
impl<const T: usize> Box<T> with (Make)
"#;
    let (db, file) = input(source);
    let error = prepare_file(&db, file)
        .err()
        .expect("type/const binder conflict must refuse");
    assert!(
        error.cause.contains("the inherent header has errors")
            && error.cause.contains("expected a normal type"),
        "{error}"
    );
}

/// Until frozen requests carried type arguments, both splices below were
/// refused ("... from instantiated generic records are not supported yet"),
/// because the generated type would have lost `Box<u256>`'s argument. The
/// record's actual arguments now cross into the output view, so `${ty}` is
/// `u256` and `${record}` is `Box<u256>`. The `u8` instance discriminates: a
/// lost argument would let `255 + 1` evaluate there. (A literal's range is
/// not checked against a narrower type, so the arithmetic is the
/// discriminator.)
#[test]
fn instantiated_generic_field_type_and_record_splices_keep_their_actual_arguments() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field, field_count, field_type}
trait Value { const fn value() -> u256 }
struct Box<T> { value: T }
struct Provider {}
impl Derive<trait Value> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        quote { const fn value() -> u256 { let v: ${ty} = 255
            let w = v + 1
            w as u256 } }
    }
}
impl Value for Box<u256> with (Provider)
const fn observe() -> u256 { <Box<u256> as Value>::value() }
"#;
    let eval_observe = |source: &str| -> Result<String, String> {
        let (db, file) = input(source);
        let output = prepare_file(&db, file)
            .unwrap_or_else(|error| panic!("{error}"))
            .expect("a prepared view");
        let output_file = output
            .workspace()
            .get(&output, &file.url(&db).unwrap())
            .unwrap();
        let function = output
            .top_mod(output_file)
            .all_funcs(&output)
            .iter()
            .copied()
            .find(|function| {
                function
                    .name(&output)
                    .to_opt()
                    .is_some_and(|name| name.data(&output) == "observe")
            })
            .expect("observe");
        let owner = BodyOwner::Func(function, None);
        let result = eval_body_owner_const(
            &output,
            owner,
            GenericSubst::for_body_owner(&output, owner, vec![]),
        );
        let EvalOutcome::Ready(value) = result else {
            return Err(format!("{result:?}"));
        };
        let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = value.value(&output)
        else {
            panic!("{:?}", value.value(&output))
        };
        Ok(value.to_string())
    };
    assert_eq!(eval_observe(source), Ok("256".to_string()));

    let narrow = source.replace("Box<u256>", "Box<u8>");
    let outcome = eval_observe(&narrow);
    assert!(
        outcome.is_err(),
        "`255 + 1` overflows the actual argument u8, got {outcome:?}"
    );

    let record_splice = source.replace(
        "let v: ${ty} = 255\n            let w = v + 1",
        "let b: ${record} = Box { value: 255 }\n            let w = b.value + 1",
    );
    assert_ne!(record_splice, source);
    assert_eq!(eval_observe(&record_splice), Ok("256".to_string()));
    let narrow = record_splice.replace("Box<u256>", "Box<u8>");
    let outcome = eval_observe(&narrow);
    assert!(
        outcome.is_err(),
        "`Box<u8>` must overflow at 255 + 1, got {outcome:?}"
    );

    let scalar = source
        .replace(
            "let ty = field_type(field(record, 0))",
            "let n = field_count(record)",
        )
        .replace(
            "let v: ${ty} = 255\n            let w = v + 1",
            "let v: u256 = ${n} as u256\n            let w = v",
        );
    assert_ne!(scalar, source);
    assert_eq!(eval_observe(&scalar), Ok("1".to_string()));
}

/// An implementation written on the splice of a record at actual arguments
/// implements that instance, as the handwritten `impl Box<u256>` does. The
/// declaration's generic parameter `T` is not copied onto it: the target
/// `Box<u256>` never uses it, so a copied `T` stays unconstrained and the call
/// `Box<u256>::marker()` is refused with "type annotation is needed".
#[test]
fn a_block_local_impl_on_an_instantiated_reflected_record_declares_no_copied_binders() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}

trait Tr {
    const fn m() -> u256
}

pub struct Gen {}
impl Derive<trait Tr> for Gen {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            const fn m() -> u256 {
                impl ${r} {
                    pub const fn marker() -> u256 { 2 }
                }
                Box<u256>::marker()
            }
        }
    }
}

struct Box<T> { value: T }
impl Tr for Box<u256> with (Gen)

const fn observe() -> u256 { <Box<u256> as Tr>::m() }
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{error}"))
        .expect("a prepared view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_integer(&output, output_file, "observe", "2");

    // The handwritten implementation of the same instance evaluates the same.
    let handwritten = r#"struct Box<T> { value: T }

const fn m() -> u256 {
    impl Box<u256> {
        pub const fn marker() -> u256 { 2 }
    }
    Box<u256>::marker()
}

const fn observe() -> u256 { m() }
"#;
    let (db, file) = input(handwritten);
    assert_generated_integer(&db, file, "observe", "2");
}

#[test]
fn reflected_field_query_family_rejects_named_implementations() {
    let (db, file) = input(
        r#"
use core::meta::FieldQuery
struct Query<T> {}
impl<T> FieldQuery<u256> for Query<T> as Alternate {
    const fn evaluate() -> u256 { 7 }
}
"#,
    );
    let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    assert!(
        diagnostics.contains("error[5-0005]: `FieldQuery` cannot have a named implementation"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("`query_field` evaluates it outside every `with`, so no choice made with `with` reaches it"),
        "{diagnostics}"
    );
}

#[test]
fn output_warnings_remain_diagnostics_without_refusing_generation() {
    // This warning is emitted by the same HIR pass used for output admission.
    let source = format!(
        "{SOURCE}\nstruct Cell {{ word: u256 }}\nimpl Cell {{ fn get_mut(mut self) -> mut u256 {{ mut self.word }} }}\nfn var_of_mut(_ cell: mut Cell) {{ var w = cell.get_mut()\n w += 1 }}\n"
    );
    let (db, file) = input(&source);
    let output = prepare_file(&db, file)
        .expect("warnings must not reject generation")
        .unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let diagnostics = output.run_on_top_mod(output.top_mod(output_file));
    assert!(!diagnostics.has_errors(&output));
    let rendered = diagnostics.format_diags(&output);
    assert!(rendered.contains("warning[8-0121]"), "{rendered}");
    assert!(
        rendered.contains("`var` copies a `mut` access"),
        "{rendered}"
    );
    assert_generated_integer(&output, output_file, "generated_count", "42");

    let invalid = source.replace("boost(${n})", "true");
    let (db, file) = input(&invalid);
    let error = prepare_file(&db, file)
        .err()
        .expect("actual errors still refuse output");
    assert!(error.cause.contains("type mismatch"), "{}", error.cause);
    assert!(db.generation_inputs().functions(&db).is_empty());
}

#[test]
fn quotation_provider_preserves_master_destructured_function_parameters() {
    let source = SOURCE.replace(
        "const fn boost(_ n: usize) -> u256 { n as u256 + 40 }",
        "const fn sum(_ (left, right): (usize, usize)) -> usize { left + right }\n const fn boost(_ n: usize) -> u256 { sum((n, 40)) as u256 }",
    );
    let (db, file) = input(&source);
    let prepared = prepare_file(&db, file).unwrap().unwrap();
    let prepared_file = prepared
        .workspace()
        .get(&prepared, &file.url(&db).unwrap())
        .unwrap();
    let module = prepared.top_mod(prepared_file);
    assert!(prepared.run_on_top_mod(module).is_empty());
    let function = module
        .all_funcs(&prepared)
        .iter()
        .find(|function| {
            function
                .name(&prepared)
                .to_opt()
                .is_some_and(|name| name.data(&prepared) == "generated_count")
        })
        .unwrap();
    let owner = BodyOwner::Func(*function, None);
    let result = eval_body_owner_const(
        &prepared,
        owner,
        GenericSubst::for_body_owner(&prepared, owner, vec![]),
    );
    let EvalOutcome::Ready(value) = result else {
        panic!("{result:?}")
    };
    assert!(matches!(
        value.value(&prepared),
        SemConstValue::Scalar { value: SemConstScalar::Int { value }, .. }
        if value.to_string() == "42"
    ));
}

#[test]
fn canonical_field_queries_allow_only_ordinary_implementations() {
    let prefix = "struct Family<T> {}\n";
    for (header, expected_error) in [
        ("impl core::meta::FieldQuery<usize> for Family<u256>", false),
        (
            "impl core::meta::FieldQuery<usize> for Family<u256> as Named",
            true,
        ),
    ] {
        let (db, file) = input(&format!(
            "{prefix}{header} {{ const fn evaluate() -> usize {{ 42 }} }}"
        ));
        let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
        if expected_error {
            assert!(
                diagnostics.contains("`FieldQuery` cannot have a named implementation"),
                "{diagnostics}"
            );
        } else {
            assert!(diagnostics.is_empty(), "{diagnostics}");
        }
    }
    let (db, file) = input(
        "mod core { pub mod meta { pub trait FieldQuery<R> { const fn evaluate() -> R } } }\nstruct Family {}\nimpl ingot::core::meta::FieldQuery<usize> for Family as Named { const fn evaluate() -> usize { 42 } }\n",
    );
    let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    assert!(
        diagnostics.is_empty(),
        "a same-spelling user trait remains eligible: {diagnostics}"
    );
}

#[test]
fn provider_body_warnings_do_not_refuse_generation_or_hide_errors() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
struct Cell { word: u256 }
impl Cell { const fn get_mut(mut self) -> mut u256 { mut self.word } }
struct Target {}
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        var cell = Cell { word: 0 }
        var w = cell.get_mut()
        w += 1
        quote { pub const fn value() -> u256 { 7 } }
    }
}
impl Target with (Make)
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file)
        .expect("warning-only provider is valid")
        .unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let diagnostics = output.run_on_top_mod(output.top_mod(output_file));
    assert!(!diagnostics.has_errors(&output));
    assert!(
        diagnostics
            .format_diags(&output)
            .contains("warning[8-0121]")
    );
    assert_generated_integer(&output, output_file, "value", "7");
    let invalid = source.replace("word: 0", "word: true");
    let (db, file) = input(&invalid);
    let error = prepare_file(&db, file)
        .err()
        .expect("real body error refuses provider");
    assert!(error.cause.contains("type mismatch"), "{}", error.cause);
    assert!(db.generation_inputs().functions(&db).is_empty());
}

#[test]
fn generated_function_does_not_resolve_producer_generic_self_or_params() {
    let generic_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
const fn emit<const N: usize>() -> Code<Item> {
    quote {
        pub fn generated() -> [u8; N] {
            [0; N]
        }
    }
}
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ r: Record) -> Code<Item> {
        emit<4>()
    }
}
pub struct Pair {
    pub a: u256,
}
impl Pair with (Make)
"#;
    let (db, file) = input(generic_source);
    let error = prepare_file(&db, file)
        .err()
        .expect("generated function must not resolve producer generic parameter");
    assert!(
        error.cause.contains("undefined"),
        "expected undefined name error, got: {}",
        error.cause
    );

    let self_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
pub struct Helper {}
impl Helper {
    pub const fn helper_only() -> u256 { 1 }
    const fn emit() -> Code<Item> {
        quote {
            pub fn generated() -> u256 {
                Self::helper_only()
            }
        }
    }
}
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ r: Record) -> Code<Item> {
        Helper::emit()
    }
}
pub struct Pair {
    pub a: u256,
}
impl Pair with (Make)
"#;
    let (db, file) = input(self_source);
    let error = prepare_file(&db, file)
        .err()
        .expect("generated function must not resolve producer Self");
    assert!(
        error.cause.contains("helper_only"),
        "expected Self resolution error on target Pair, got: {}",
        error.cause
    );

    let param_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
const fn emit(_ x: usize) -> Code<Item> {
    quote {
        pub fn generated() -> usize {
            x
        }
    }
}
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ r: Record) -> Code<Item> {
        emit(4)
    }
}
pub struct Pair {
    pub a: u256,
}
impl Pair with (Make)
"#;
    let (db, file) = input(param_source);
    let error = prepare_file(&db, file)
        .err()
        .expect("generated function must not resolve producer parameter");
    assert!(
        error.cause.contains("undefined variable `x`")
            && !error.cause.contains("resolves to a function parameter"),
        "expected undefined variable `x`, got: {}",
        error.cause
    );
}

#[test]
fn meta_carrier_check_normalizes_associated_types_and_checks_all_adt_specializations() {
    for source in [
        r#"use core::meta::Record
pub trait Carry {
    type Out
}
impl Carry for u8 {
    type Out = Record
}
pub struct Hidden<T: Carry> {
    pub inner: T::Out,
}
pub fn runtime_smuggle(_ x: Hidden<u8>) {}
"#,
        r#"use core::meta::Record
pub struct Wrap<T> {
    pub inner: T,
}
pub fn runtime_smuggle_pair(_ x: (Wrap<u8>, Wrap<Record>)) {}
"#,
        r#"use core::meta::Record
pub trait Carry {
    type Out
}
impl Carry for u16 {
    type Out = u16
}
impl Carry for u8 {
    type Out = Record
}
pub struct Hidden<T: Carry> {
    pub inner: T::Out,
}
pub fn runtime_smuggle_assoc_pair(_ x: (Hidden<u16>, Hidden<u8>)) {}
"#,
    ] {
        let (db, file) = input(source);
        let diagnostics = db.run_on_top_mod(db.top_mod(file));
        assert!(
            diagnostics.has_errors(&db),
            "expected compile-time phase error for:\n{source}"
        );
        let rendered = diagnostics.format_diags(&db);
        assert!(
            rendered.contains("`Record` is compile-time-only in a runtime parameter"),
            "unexpected diagnostics:\n{rendered}"
        );
    }
}

#[test]
fn unconsumed_quoted_function_validates_zero_arg_nongeneric_no_where_no_uses() {
    for source in [
        r#"use core::meta::{Code, Item}
pub const BAD: Code<Item> = quote {
    fn generated<T>(_ x: T) -> T {
        x
    }
}
"#,
        r#"use core::meta::{Code, Item}
pub const BAD: Code<Item> = quote {
    fn generated(_ x: usize) -> usize {
        x
    }
}
"#,
    ] {
        let (db, file) = input(source);
        let diagnostics = db.run_on_top_mod(db.top_mod(file));
        assert!(
            !diagnostics.has_errors(&db),
            "unexpected quote signature validation error for:\n{source}\n{}",
            diagnostics.format_diags(&db)
        );
    }
}

#[test]
fn helper_body_error_reports_const_helper_diagnostics_not_ownership_failure() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}
const fn helper() -> usize {
    missing_var
}
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ r: Record) -> Code<Item> {
        let n = helper()
        quote {
            pub fn generated() -> usize {
                ${n}
            }
        }
    }
}
pub struct Pair {
    pub a: u256,
}
impl Pair with (Make)
"#;
    let (db, file) = input(source);
    let error = prepare_file(&db, file)
        .err()
        .expect("helper body error must refuse generation");
    assert!(
        error
            .cause
            .contains("whose body does not check without errors")
            && error.cause.contains("undefined variable `missing_var`")
            && !error
                .cause
                .contains("authored provider ownership/capability validation failed"),
        "unexpected error cause: {}",
        error.cause
    );
}

#[test]
fn field_query_body_warnings_remain_nonfatal_but_errors_refuse() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item, FieldQuery, field, field_type, query_field}
struct Cell { word: u256 }
impl Cell { const fn get_mut(mut self) -> mut u256 { mut self.word } }
struct Query<T> {}
impl<T> FieldQuery<u256> for Query<T> {
    const fn evaluate() -> u256 {
        var cell = Cell { word: 0 }
        var w = cell.get_mut()
        w += 1
        7
    }
}
struct Target { word: u256 }
struct Make {}
impl DeriveInherent for Make {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = query_field<Query, u256>(field_type(field(record, 0)))
        quote { pub const fn value() -> u256 { ${n} } }
    }
}
impl Target with (Make)
"#;
    for source in [
        source.to_owned(),
        source.replace("var w = cell.get_mut()", "let w = mut cell.get_mut()"),
    ] {
        let (db, file) = input(&source);
        let output = prepare_file(&db, file)
            .expect("lawful query body must evaluate")
            .unwrap();
        let output_file = output
            .workspace()
            .get(&output, &file.url(&db).unwrap())
            .unwrap();
        let diagnostics = output.run_on_top_mod(output.top_mod(output_file));
        assert!(!diagnostics.has_errors(&output));
        if source.contains("var w") {
            assert!(
                diagnostics
                    .format_diags(&output)
                    .contains("warning[8-0121]")
            );
        }
        assert_generated_integer(&output, output_file, "value", "7");
    }
    let invalid = source.replace("word: 0", "word: true");
    let (db, file) = input(&invalid);
    assert!(
        prepare_file(&db, file).is_err(),
        "query body errors must refuse evaluation"
    );
    assert!(db.generation_inputs().functions(&db).is_empty());
}

#[test]
fn cross_file_spliced_fragment_type_errors_keep_source_spans() {
    let std_fold_mismatch = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}
use std::derive::{AbiHeadSize, fold_fields}

struct BadProvider {}
impl DeriveInherent for BadProvider {
    const fn derive(_ r: Record) -> Code<Item> {
        let expr = fold_fields<AbiHeadSize>(r)
        quote {
            pub fn bad() -> bool {
                ${expr}
            }
        }
    }
}

pub struct Sample {
    pub x: u256,
}
impl Sample with (BadProvider)
"#;
    let (db, file) = input(std_fold_mismatch);
    let error = prepare_file(&db, file)
        .err()
        .expect("type error in cross-file stdlib spliced expression must refuse output");
    assert!(error.cause.contains("type mismatch"), "{}", error.cause);

    let std_is_dynamic_mismatch = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}
use std::derive::{AbiIsDynamic, fold_fields}

struct BadProvider {}
impl DeriveInherent for BadProvider {
    const fn derive(_ r: Record) -> Code<Item> {
        let expr = fold_fields<AbiIsDynamic>(r)
        quote {
            pub fn bad() -> u256 {
                ${expr}
            }
        }
    }
}

pub struct Sample {
    pub x: u256,
}
impl Sample with (BadProvider)
"#;
    let (db, file) = input(std_is_dynamic_mismatch);
    let error = prepare_file(&db, file)
        .err()
        .expect("type error in cross-file stdlib boolean splice must refuse output");
    assert!(error.cause.contains("type mismatch"), "{}", error.cause);
}

#[test]
fn inherent_provider_headers_evaluate_derive_inherent() {
    let source = r#"use core::derive::{Derive, DeriveInherent}
use core::ops::Ord
use core::meta::{Code, Item, Record, field, field_count, field_type}

struct InspectFields {}
impl DeriveInherent for InspectFields {
    const fn derive(_ record: Record) -> Code<Item> {
        let count = field_count(record)
        quote {
            pub const FIELD_COUNT: usize = ${count}
            pub const fn summary() -> usize {
                Self::FIELD_COUNT * 10
            }
        }
    }
}

struct Within {}
impl DeriveInherent for Within {
    const fn derive(_ record: Record) -> Code<Item> {
        let f0 = field(record, 0)
        let f1 = field(record, 1)
        let t0 = field_type(f0)
        quote {
            pub fn within(self, _ lo: ${t0}, _ hi: ${t0}) -> bool {
                self.${f0} >= lo && self.${f1} <= hi
            }
            pub const fn first(self) -> ${t0} {
                self.${f0}
            }
        }
    }
}

struct Scaled<const N: u256> {}
impl<const N: u256> DeriveInherent for Scaled<N> {
    const fn derive(_ record: Record) -> Code<Item> {
        let f0 = field(record, 0)
        quote {
            pub const SCALE: u256 = ${N}
            pub const fn scaled(self) -> u256 {
                self.${f0} * Self::SCALE
            }
        }
    }
}

struct Packet { id: u256, ok: bool, amount: u256 }
impl Packet with (InspectAbiAlias)
type InspectAbiAlias = InspectFields

struct Box2<T> { pub lo: T, pub hi: T }
impl<T: Ord> Box2<T> with (Within)

struct Counter { pub value: u256 }
impl Counter with (Scaled<10>)

struct LegacyGen {}
impl DeriveInherent for LegacyGen {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        quote { pub const fn legacy_summary() -> usize { ${n} } }
    }
}

struct Legacy { a: u256, b: u256 }
impl Legacy with (LegacyGen)

fn check_runtime_within() -> bool {
    let b: Box2<u256> = Box2 { lo: 10, hi: 20 }
    b.within(5, 25) && !b.within(15, 25)
}

const fn check() -> u256 {
    let b: Box2<u256> = Box2 { lo: 1, hi: 20 }
    let c = Counter { value: 4 }.scaled()
    (Packet::summary() + Legacy::FIELD_COUNT_FALLBACK() + Legacy::legacy_summary()) as u256 + b.first() + c
}

impl Legacy {
    const fn FIELD_COUNT_FALLBACK() -> usize { 0 }
}
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file)
        .expect("inherent headers must prepare cleanly")
        .expect("output view must be produced");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let diagnostics = output.run_on_top_mod(output.top_mod(output_file));
    assert!(
        !diagnostics.has_errors(&output),
        "{}",
        diagnostics.format_diags(&output)
    );
    assert_generated_integer(&output, output_file, "check", "73");
}

/// The names an inherent header writes, its target, its provider and its
/// bounds, are reported as for any implementation. A refusal of the header
/// does not hide them, and neither does the report that the header has no
/// members yet.
#[test]
fn inherent_header_reports_the_name_errors_of_its_target_and_provider() {
    let unknown_names = "pub struct Packet { id: u256 }\n\nimpl<T> Missing<T> with (NoSuchProvider) where T: NoSuchTrait\n";
    let (db, file) = input(unknown_names);
    let err = prepare_file(&db, file)
        .err()
        .expect("a header that names nothing must be refused");
    assert!(
        err.cause.contains("`Missing` is not found"),
        "{}",
        err.cause
    );
    assert!(
        err.cause
            .contains("cannot resolve the derive provider type"),
        "{}",
        err.cause
    );

    let unknown_target = "use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

pub struct P {}
impl DeriveInherent for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn answer() -> u256 { 42 } }
    }
}

impl<T> Missing<T> with (P)
";
    let (db, file) = input(unknown_target);
    let err = prepare_file(&db, file)
        .err()
        .expect("a header for an unknown type must be refused");
    assert!(
        err.cause.contains("`Missing` is not found"),
        "{}",
        err.cause
    );

    // Without preparation, the analysis of the module reports the header's
    // own errors beside the members it does not have yet.
    let (db, file) = input(unknown_target);
    let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    assert!(
        diagnostics.contains("`Missing` is not found"),
        "{diagnostics}"
    );
}

#[test]
fn inherent_header_refuses_hidden_members_invalid_member_items_and_missing_derive_inherent() {
    let written_method_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

struct Within {}
impl DeriveInherent for Within {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn within(self) -> bool {
                true
            }
        }
    }
}

struct Box2<T> { pub width: T, pub height: T }
impl<T> Box2<T> with (Within)
impl<T> Box2<T> {
    pub fn within(self) -> bool {
        false
    }
}
"#;
    let (db, file) = input(written_method_source);
    let err = prepare_file(&db, file)
        .err()
        .expect("generated method hidden by written inherent method must fail");
    let header_offset = written_method_source
        .find("impl<T> Box2<T> with (Within)")
        .unwrap();
    assert_eq!(usize::from(err.range.start()), header_offset);
    let written_name = written_method_source.rfind("fn within").unwrap() + "fn ".len();
    let written_end = written_name + "within".len();
    assert!(
        err.cause.contains(&format!(
            "would be hidden: `Box2<T>` already has an inherent method `within` of `Box2<T>`, `within` declared at file:///tmp/explicit-record-provider.fe:{written_name}..{written_end}"
        )),
        "{}",
        err.cause
    );

    let written_const_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

struct TagConst {}
impl DeriveInherent for TagConst {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub const TAG: u256 = 2
        }
    }
}

struct Packet { id: u256 }
impl Packet with (TagConst)
impl Packet {
    pub const TAG: u256 = 1
}
"#;
    let (db, file) = input(written_const_source);
    let err = prepare_file(&db, file)
        .err()
        .expect("generated constant hidden by written inherent constant must fail");
    assert!(
        err.cause.contains(
            "would be hidden: `Packet` already has an inherent constant `TAG` of `Packet`"
        ),
        "{}",
        err.cause
    );

    let trait_method_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

trait Seed {
    const fn seed() -> u256
}

struct MakeSeed {}
impl DeriveInherent for MakeSeed {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn seed() -> u256 { 2 }
        }
    }
}

struct Token {}
impl Seed for Token {
    const fn seed() -> u256 { 1 }
}
impl Token with (MakeSeed)
"#;
    let (db, file) = input(trait_method_source);
    let err = prepare_file(&db, file)
        .err()
        .expect("generated method hidden by trait method must fail");
    assert!(
        err.cause
            .contains("would be hidden: `Token` already has the method `seed` of the trait `Seed`"),
        "{}",
        err.cause
    );

    let trait_const_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}

trait Tr {
    const N: usize
}

struct MakeConst {}
impl DeriveInherent for MakeConst {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub const N: usize = 5
        }
    }
}

struct X {}
impl Tr for X {
    const N: usize = 2
}
impl X with (MakeConst)
"#;
    let (db, file) = input(trait_const_source);
    let err = prepare_file(&db, file)
        .err()
        .expect("generated constant hidden by trait constant must fail");
    assert!(
        err.cause
            .contains("would be hidden: `X` already has the constant `N` of the trait `Tr`"),
        "{}",
        err.cause
    );

    let bad_impl_source = "use core::derive::{Derive, DeriveInherent}\nuse core::meta::{Code, Item, Record}\nstruct Bad {}\nimpl DeriveInherent for Bad {\n    const fn derive(_ record: Record) -> Code<Item> {\n        quote {\n            impl Packet { fn bad() -> u256 { 1 } }\n        }\n    }\n}\nstruct Packet { id: u256 }\nimpl Packet with (Bad)\n";
    let (db, file) = input(bad_impl_source);
    let err = prepare_file(&db, file)
        .err()
        .expect("impl member in inherent header must fail");
    let header_offset = bad_impl_source.find("impl Packet with (Bad)").unwrap();
    assert_eq!(usize::from(err.range.start()), header_offset);
    assert!(
        err.cause.contains(
            "A member of an inherent header must be a function or a constant; the quotation at explicit-record-provider.fe:6 writes an `impl`"
        ),
        "{}",
        err.cause
    );

    for (quoted_item, expected_what) in [
        ("type Alias = u256", "a `type`"),
        ("struct Nested {}", "a `struct`"),
    ] {
        use parser::ast::{self, prelude::*};
        use salsa::Setter;

        let bad_member_source = format!(
            "use core::derive::{{Derive, DeriveInherent}}\nuse core::meta::{{Code, Item, Record}}\nstruct Bad {{}}\nimpl DeriveInherent for Bad {{\n    const fn derive(_ record: Record) -> Code<Item> {{\n        quote {{\n            {quoted_item}\n        }}\n    }}\n}}\nstruct Packet {{ id: u256 }}\nimpl Packet with (Bad)\n"
        );
        let (mut db, file) = input(&bad_member_source);
        let url = file.url(&db).unwrap();
        let (green, _) =
            parser::parse_source_file(&bad_member_source, parser::RecoveryMode::Recover);
        let root = ast::Root::cast(parser::SyntaxNode::new_root(green)).unwrap();
        let quote_range = root
            .syntax()
            .descendants()
            .find_map(ast::QuoteExpr::cast)
            .unwrap()
            .syntax()
            .text_range();
        let header_range = root
            .syntax()
            .descendants()
            .filter_map(ast::Impl::cast)
            .find(|imp| imp.derive_provider().is_some())
            .unwrap()
            .syntax()
            .text_range();
        db.generation_inputs().set_requests(&mut db).to(vec![
            common::generation::FrozenGenerationRequest {
                target_file: url.clone(),
                request_impl: Some(header_range),
                code: common::generation::FrozenCode::Template {
                    kind: common::generation::FrozenCodeKind::Item,
                    quote_file: url,
                    quote_range,
                    producer_module: None,
                    producer_block: None,
                    holes: Vec::new(),
                },
            },
        ]);
        let module = db.top_mod(file);
        let errors = hir::lower::generation_lowering_errors(&db, module);
        let first = errors
            .first()
            .expect("non-function non-constant member in inherent header must fail");
        let header_offset = bad_member_source.find("impl Packet with (Bad)").unwrap();
        assert_eq!(usize::from(first.primary_range.start()), header_offset);
        assert!(
            first.message.contains(&format!(
                "A member of an inherent header must be a function or a constant; the quotation at explicit-record-provider.fe:6 writes {expected_what}"
            )),
            "{}",
            first.message
        );
    }

    let missing_derive_source = r#"use core::meta::{Code, Item, Record}

struct Within {}
struct Box2<T> { pub width: T, pub height: T }
impl<T> Box2<T> with (Within)
"#;
    let (db, file) = input(missing_derive_source);
    let err = prepare_file(&db, file)
        .err()
        .expect("provider without DeriveInherent must fail");
    assert!(
        err.cause.contains(
            "`Within` has no `impl DeriveInherent`: the inherent header `impl Box2<T> with (Within)` needs a provider that implements `DeriveInherent`"
        ),
        "{}",
        err.cause
    );
}

#[test]
fn a_drop_implementation_nested_in_a_derive_header_member_and_an_inherent_header_member_is_refused()
{
    let trait_header_with_drop = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use core::ops::Drop

pub struct Counted { counter: *u256 }
pub trait Tr { fn v() -> u256 }
pub struct S {}
pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            fn v() -> u256 {
                impl Drop for Counted {
                    fn drop(own self) { unsafe { *self.counter += 1 } }
                }
                0
            }
        }
    }
}
impl Tr for S with (P)
"#;
    let (db, file) = input(trait_header_with_drop);
    let err = prepare_file(&db, file)
        .err()
        .expect("nested Drop in trait header member must be refused");
    assert!(
        err.cause
            .contains("the generated implementation of `Drop` for `Counted`"),
        "{}",
        err.cause
    );

    let inherent_header_with_drop = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Item, Record}
use core::ops::Drop

pub struct Counted { counter: *u256 }
pub struct S {}
pub struct P {}
impl DeriveInherent for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            pub fn v() -> u256 {
                impl Drop for Counted {
                    fn drop(own self) { unsafe { *self.counter += 1 } }
                }
                0
            }
        }
    }
}
impl S with (P)
"#;
    let (db, file) = input(inherent_header_with_drop);
    let err = prepare_file(&db, file)
        .err()
        .expect("nested Drop in inherent header member must be refused");
    assert!(
        err.cause
            .contains("the generated implementation of `Drop` for `Counted`"),
        "{}",
        err.cause
    );

    let inherent_header_control = inherent_header_with_drop.replace(
        "impl Drop for Counted {\n                    fn drop(own self) { unsafe { *self.counter += 1 } }\n                }",
        "",
    );
    let (db, file) = input(&inherent_header_control);
    let output = prepare_file(&db, file)
        .expect("control without nested Drop must prepare")
        .expect("output view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert!(
        !output
            .run_on_top_mod(output.top_mod(output_file))
            .has_errors(&output)
    );
}

#[test]
fn note_fixtures_packet_inspect_abi_and_generic_box2_within_work_as_inherent_headers() {
    let packet_source = r#"use core::abi::AbiSize
use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, FieldQuery, Item, Record, field, field_count, field_type, query_field, type_is}
use std::derive::{AbiHeadSize, AbiIsDynamic, fold_fields}

struct AbiFacts<T> {}
impl<T: AbiSize> FieldQuery<(u256, bool)> for AbiFacts<T> {
    const fn evaluate() -> (u256, bool) {
        (<T as AbiSize>::HEAD_SIZE, <T as AbiSize>::IS_DYNAMIC)
    }
}

struct InspectAbi {}
impl DeriveInherent for InspectAbi {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record)
        var head_bytes: u256 = 0
        var u256_fields: u256 = 0
        var i: usize = 0
        while i < n {
            let f = field(record, i)
            let ty = field_type(f)
            let (field_head, _field_dyn) = query_field<AbiFacts, (u256, bool)>(ty)
            head_bytes += field_head
            if type_is<u256>(ty) {
                u256_fields += 1
            }
            i += 1
        }
        let head_expr = fold_fields<AbiHeadSize>(record)
        let dynamic_expr = fold_fields<AbiIsDynamic>(record)
        quote {
            const fn record_abi_summary() -> (u256, u256, u256, bool) {
                (${head_bytes}, ${u256_fields}, ${head_expr}, ${dynamic_expr})
            }
        }
    }
}

struct Packet {
    id: u256,
    ok: bool,
    amount: u256,
}

impl Packet with (InspectAbi)

const fn check_summary() -> u256 {
    let (queried_bytes, u256_count, folded_bytes, dynamic) = Packet::record_abi_summary()
    if !dynamic && queried_bytes == folded_bytes {
        queried_bytes + u256_count
    } else {
        0
    }
}
"#;
    let (db, file) = input(packet_source);
    let output = prepare_file(&db, file)
        .expect("Packet with InspectAbi must prepare")
        .expect("output view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(!output.run_on_top_mod(module).has_errors(&output));
    assert_generated_integer(&output, output_file, "check_summary", "98");

    let within_source = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Code, Expr, Field, Item, Record, field, field_count, field_type}
use core::ops::Ord
use std::derive::{FieldStep, fold_fields}

struct WithinStep {}
impl FieldStep for WithinStep {
    const fn unit() -> Code<Expr> {
        quote { true }
    }

    const fn step(_ acc: Code<Expr>, _ f: Field) -> Code<Expr> {
        quote(min, max) {
            ${acc} && self.${f} >= min && self.${f} <= max
        }
    }
}

pub struct Within {}
impl DeriveInherent for Within {
    const fn derive(_ record: Record) -> Code<Item> {
        let _ = field_count(record)
        let ty = field_type(field(record, 0))
        let body = fold_fields<WithinStep>(record)
        quote(min, max) {
            pub fn within(self, _ min: ${ty}, _ max: ${ty}) -> bool {
                ${body}
            }
        }
    }
}

pub struct ConstWithin {}
impl DeriveInherent for ConstWithin {
    const fn derive(_ record: Record) -> Code<Item> {
        let ty = field_type(field(record, 0))
        let body = fold_fields<WithinStep>(record)
        quote(min, max) {
            pub const fn is_within(self, _ min: ${ty}, _ max: ${ty}) -> bool {
                ${body}
            }
        }
    }
}

struct Box2<T: Ord> {
    width: T,
    height: T,
}

impl<T: Ord> Box2<T> with (Within)

struct UnboundedBox2<T> {
    width: T,
    height: T,
}

impl<T> UnboundedBox2<T> with (Within)
    where T: Ord

struct U256Box {
    width: u256,
    height: u256,
}

impl U256Box with (ConstWithin)

fn check_generic_within() -> bool {
    let b = Box2 { width: 10 as u256, height: 20 as u256 }
    let ok = b.within(5, 25)
    let bad = Box2 { width: 10 as u256, height: 30 as u256 }.within(5, 25)
    let u_ok = UnboundedBox2 { width: 10 as u256, height: 20 as u256 }.within(5, 25)
    ok && !bad && u_ok
}

const fn check_within() -> u256 {
    let ok = U256Box { width: 10, height: 20 }.is_within(5, 25)
    let bad = U256Box { width: 10, height: 30 }.is_within(5, 25)
    if ok && !bad { 1 } else { 0 }
}
"#;
    let (db, file) = input(within_source);
    let output = prepare_file(&db, file)
        .expect("Box2<T: Ord> with Within must prepare")
        .expect("output view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(!output.run_on_top_mod(module).has_errors(&output));
    assert_generated_integer(&output, output_file, "check_within", "1");

    // Bounds must come from the author's header: omitting `T: Ord` on the header is refused.
    let missing_header_bound = within_source.replace(
        "impl<T: Ord> Box2<T> with (Within)",
        "impl<T> Box2<T> with (Within)",
    );
    let (db, file) = input(&missing_header_bound);
    let err = prepare_file(&db, file)
        .err()
        .expect("omitting T: Ord from impl<T> Box2<T> with (Within) must fail target admission");
    assert!(err.cause.contains("Ord"), "{}", err.cause);

    let missing_body_bound = within_source.replace("    where T: Ord\n", "");
    let (db, file) = input(&missing_body_bound);
    let err = prepare_file(&db, file)
        .err()
        .expect("generated `within` body must fail admission when header omits `where T: Ord`");
    assert!(
        err.cause.contains("Add a `T: core::ops::Ord` bound"),
        "{}",
        err.cause
    );
}

#[test]
fn struct_eq_enum_eq_and_abi_size_in_inherent_header_form() {
    let source = r#"use core::abi::{AbiSize, dynamic_payload_size}
use core::derive::{Derive, DeriveInherent}
use core::meta::{
    Arm, Code, Expr, Field, Item, Record, binders, empty, field, field_count, field_type, variant,
    variant_count,
}
use core::ops::Eq
use std::derive::{AbiHeadSize, AbiIsDynamic, FieldStep, fold_fields}

struct EqStep {}
impl FieldStep for EqStep {
    const fn unit() -> Code<Expr> {
        quote { true }
    }

    const fn step(_ acc: Code<Expr>, _ f: Field) -> Code<Expr> {
        quote(other) { ${acc} && self.${f} == other.${f} }
    }
}

pub struct StructEq {}
impl DeriveInherent for StructEq {
    const fn derive(_ record: Record) -> Code<Item> {
        let body = fold_fields<EqStep>(record)
        quote(other) {
            pub fn eq(self, _ other: Self) -> bool {
                ${body}
            }
        }
    }
}

pub struct ConstStructEq {}
impl DeriveInherent for ConstStructEq {
    const fn derive(_ record: Record) -> Code<Item> {
        let body = fold_fields<EqStep>(record)
        quote(other) {
            pub const fn eq(self, _ other: Self) -> bool {
                ${body}
            }
        }
    }
}

pub struct EnumEq {}
impl DeriveInherent for EnumEq {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = variant_count(record)
        var arms = empty<Arm>()
        var i: usize = 0
        while i < n {
            let v = variant(record, i)
            let lhs = binders(v, 0)
            let rhs = binders(v, 1)
            let m = field_count(v)
            var eq: Code<Expr> = quote { true }
            var j: usize = 0
            while j < m {
                let f = field(v, j)
                eq = quote { ${eq} && ${lhs.at(f)} == ${rhs.at(f)} }
                j += 1
            }
            arms = quote(other) {
                ${arms}
                ${lhs} => match other {
                    ${rhs} => ${eq}
                    _ => false
                }
            }
            i += 1
        }
        quote(other) {
            pub fn eq(self, _ other: Self) -> bool {
                match self {
                    ${arms}
                }
            }
        }
    }
}

pub struct ConstEnumEq {}
impl DeriveInherent for ConstEnumEq {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = variant_count(record)
        var arms = empty<Arm>()
        var i: usize = 0
        while i < n {
            let v = variant(record, i)
            let lhs = binders(v, 0)
            let rhs = binders(v, 1)
            let m = field_count(v)
            var eq: Code<Expr> = quote { true }
            var j: usize = 0
            while j < m {
                let f = field(v, j)
                eq = quote { ${eq} && ${lhs.at(f)} == ${rhs.at(f)} }
                j += 1
            }
            arms = quote(other) {
                ${arms}
                ${lhs} => match other {
                    ${rhs} => ${eq}
                    _ => false
                }
            }
            i += 1
        }
        quote(other) {
            pub const fn eq(self, _ other: Self) -> bool {
                match self {
                    ${arms}
                }
            }
        }
    }
}

struct AbiPayloadSize {}
impl FieldStep for AbiPayloadSize {
    const fn unit() -> Code<Expr> {
        quote { 0 }
    }

    const fn step(_ acc: Code<Expr>, _ f: Field) -> Code<Expr> {
        let ty = field_type(f)
        quote { ${acc} + dynamic_payload_size<${ty}>(self.${f}) }
    }
}

pub struct StructAbiSize {}
impl DeriveInherent for StructAbiSize {
    const fn derive(_ record: Record) -> Code<Item> {
        let head = fold_fields<AbiHeadSize>(record)
        let is_dyn = fold_fields<AbiIsDynamic>(record)
        let payload = fold_fields<AbiPayloadSize>(record)
        quote {
            pub const HEAD_SIZE: u256 = ${head}
            pub const IS_DYNAMIC: bool = ${is_dyn}

            pub fn payload_size(self) -> u256 {
                Self::HEAD_SIZE + ${payload}
            }
        }
    }
}

pub struct Pair<T> {
    pub a: T,
    pub b: bool,
}
impl<T: Eq> Pair<T> with (StructEq)

pub enum Choice<T> {
    None,
    One(T),
    Both { left: T, right: bool },
}
impl<T: Eq> Choice<T> with (EnumEq)

pub struct ConstPair {
    pub a: u256,
    pub b: bool,
}
impl ConstPair with (ConstStructEq)

pub enum ConstChoice {
    None,
    One(u256),
    Both { left: u256, right: bool },
}
impl ConstChoice with (ConstEnumEq)

pub struct TwoWords {
    pub a: u256,
    pub b: bool,
}
impl TwoWords with (StructAbiSize)

fn check_generic_inherent_size() -> bool {
    let p1 = Pair { a: 7 as u256, b: true }
    let p2 = Pair { a: 7 as u256, b: true }
    let c1 = Choice::Both { left: 10 as u256, right: false }
    let c2 = Choice::Both { left: 10 as u256, right: false }
    let w = TwoWords { a: 1, b: true }
    p1.eq(p2) && c1.eq(c2) && w.payload_size() == 64
}

const fn check_inherent_size() -> u256 {
    let p1 = ConstPair { a: 7, b: true }
    let p2 = ConstPair { a: 7, b: true }
    let p3 = ConstPair { a: 8, b: true }
    let c1 = ConstChoice::Both { left: 10, right: false }
    let c2 = ConstChoice::Both { left: 10, right: false }
    let c3 = ConstChoice::One(10)
    if p1.eq(p2) && !p1.eq(p3) && c1.eq(c2) && !c1.eq(c3) && !TwoWords::IS_DYNAMIC {
        TwoWords::HEAD_SIZE
    } else {
        0
    }
}
"#;
    let (db, file) = input(source);
    let output = prepare_file(&db, file)
        .expect("StructEq, EnumEq, and StructAbiSize must prepare as inherent headers")
        .expect("output view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(!output.run_on_top_mod(module).has_errors(&output));
    assert_generated_integer(&output, output_file, "check_inherent_size", "64");

    // Inherent StructEq does not implement core::ops::Eq, so `a == b` fails typechecking.
    let operator_eq = format!(
        "{source}\nfn uses_operator() -> bool {{\n    let a = Pair {{ a: 1 as u256, b: true }}\n    let b = Pair {{ a: 1 as u256, b: true }}\n    a == b\n}}\n"
    );
    let (db, file) = input(&operator_eq);
    let error = support::program_errors(&db, file);
    assert!(error.contains("Eq"), "{error}");
}

#[test]
fn an_enum_payload_and_a_struct_field_read_an_associated_type_alike() {
    const PROBES: &str = r#"use core::derive::Derive
use core::meta::{Record, Code, Item, variant, field, field_type, type_is}

trait Has { type Out }
struct A {}
impl Has for A { type Out = u256 }

enum E<T: Has> { V(T::Out), W }
struct S<T: Has> { f: T::Out }

trait Probe { const fn probe() -> usize }

struct EnumP {}
impl Derive<trait Probe> for EnumP {
    const fn derive(_ r: Record) -> Code<Item> {
        let ty = field_type(field(variant(r, 0), 0))
        let n: usize = if type_is<u256>(ty) { 1 } else { 0 }
        quote { const fn probe() -> usize { ${n} } }
    }
}
struct StructP {}
impl Derive<trait Probe> for StructP {
    const fn derive(_ r: Record) -> Code<Item> {
        let ty = field_type(field(r, 0))
        let n: usize = if type_is<u256>(ty) { 1 } else { 0 }
        quote { const fn probe() -> usize { ${n} } }
    }
}

impl Probe for E<A> with (EnumP)
impl Probe for S<A> with (StructP)

const fn enum_probe() -> usize { <E<A> as Probe>::probe() }
const fn struct_probe() -> usize { <S<A> as Probe>::probe() }
"#;
    let (db, file) = input(PROBES);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{error}"))
        .expect("a prepared view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_integer(&output, output_file, "enum_probe", "1");
    assert_generated_integer(&output, output_file, "struct_probe", "1");
}

#[test]
fn a_header_whose_cfg_does_not_hold_is_not_a_request() {
    const DISABLED_HEADER: &str = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}

struct P {}
impl DeriveInherent for P {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { pub const fn marker() -> u256 { 7 } }
    }
}

struct S { a: u256 }

#[cfg(target_os = "linux")]
impl S with (P)
"#;
    let (db, file) = input(DISABLED_HEADER);
    let prepared = prepare_file(&db, file);
    assert!(
        prepared.is_ok(),
        "{:?}",
        prepared.err().map(|error| error.cause)
    );
}

/// The `use` items of a quotation's block are declarations of the fragment,
/// as the items a block declares are: the names they bind are not looked up in
/// the producer's scope.
#[test]
fn a_block_local_import_in_a_quotation_declares_its_name() {
    const SOURCE: &str = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

mod types {
    pub struct Value { pub n: u256 }
}

trait Tr {
    const fn m() -> u256
}

pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            const fn m() -> u256 {
                if true {
                    use types::Value as LocalValue
                    LocalValue { n: 7 }.n
                } else {
                    0
                }
            }
        }
    }
}

struct S {
    a: u256,
}
impl Tr for S with (P)
"#;
    let (db, file) = input(SOURCE);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{}", error.cause))
        .expect("output view must be produced");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_method_integer(&output, output_file, "m", "7");
}

/// A name that the block does not import keeps the producer's scope, also
/// where the destination declares an item of the same name, and an import in
/// the block does not make other names local.
#[test]
fn a_quotation_name_that_no_block_import_binds_keeps_the_producer_scope() {
    const SOURCE: &str = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

mod types {
    pub struct Other { pub n: u256 }
}

trait Tr {
    const fn m() -> u256
}

mod author {
    use core::derive::Derive
    use core::meta::{Record, Code, Item}

    pub struct Value { pub n: u256 }

    pub struct P {}
    impl Derive<trait super::Tr> for P {
        const fn derive(_ r: Record) -> Code<Item> {
            quote {
                const fn m() -> u256 {
                    if true {
                        use super::types::Other as LocalOther
                        let other = LocalOther { n: 0 }
                        Value { n: 7 }.n + other.n
                    } else {
                        0
                    }
                }
            }
        }
    }
}

struct Value { flag: bool }

struct S {
    a: u256,
}
impl Tr for S with (author::P)
"#;
    let (db, file) = input(SOURCE);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{}", error.cause))
        .expect("output view must be produced");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_method_integer(&output, output_file, "m", "7");
}

/// An import in a quotation's block shadows an item of the same name in the
/// producer's scope, as an import in a written block does.
#[test]
fn a_block_local_import_in_a_quotation_shadows_a_producer_item_of_the_same_name() {
    const SOURCE: &str = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

mod types {
    pub struct Value { pub n: u256 }
    impl Value { pub const fn get(self) -> u256 { 1 } }
}

struct LocalValue { pub n: u256 }
impl LocalValue { pub const fn get(self) -> u256 { 2 } }

trait Tr {
    const fn m() -> u256
}

pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            const fn m() -> u256 {
                if true {
                    use types::Value as LocalValue
                    LocalValue { n: 0 }.get()
                } else {
                    0
                }
            }
        }
    }
}

struct S {
    a: u256,
}
impl Tr for S with (P)
"#;
    let (db, file) = input(SOURCE);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{}", error.cause))
        .expect("output view must be produced");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_method_integer(&output, output_file, "m", "1");
}

/// Each leaf of a nested import binds its alias, its last segment, or for
/// `self` the module before it.
#[test]
fn a_nested_block_local_import_in_a_quotation_declares_each_name_it_binds() {
    const SOURCE: &str = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

mod types {
    pub struct Value { pub n: u256 }
    pub struct Other { pub n: u256 }
    pub mod inner {
        pub struct Deep { pub n: u256 }
    }
}

trait Tr {
    const fn m() -> u256
}

pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            const fn m() -> u256 {
                if true {
                    use types::{Value, Other as O, inner::{self, Deep}}
                    use types::inner::{self as I}
                    let a = Value { n: 1 }
                    let b = O { n: 2 }
                    let c = inner::Deep { n: 3 }
                    let d = Deep { n: 4 }
                    let e = I::Deep { n: 5 }
                    a.n + b.n + c.n + d.n + e.n
                } else {
                    0
                }
            }
        }
    }
}

struct S {
    a: u256,
}
impl Tr for S with (P)
"#;
    let (db, file) = input(SOURCE);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{}", error.cause))
        .expect("output view must be produced");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_method_integer(&output, output_file, "m", "15");
}

/// A glob import in a quotation's block is refused: its names cannot be
/// bound where the quotation is written.
#[test]
fn a_glob_import_in_a_quotation_is_refused() {
    let shadow = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

mod types {
    pub struct Value { pub n: u256 }
    pub struct Other { pub n: u256 }
    pub mod inner {
        pub struct Deep { pub n: u256 }
    }
    impl inner::Deep { pub const fn get(self) -> u256 { 1 } }
}
struct Deep { pub n: u256 }
impl Deep { pub const fn get(self) -> u256 { 2 } }

trait Tr {
    const fn m() -> u256
}

pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            const fn m() -> u256 {
                if true {
                    use types::inner::*
                    Deep { n: 0 }.get()
                } else {
                    0
                }
            }
        }
    }
}

struct S {
    a: u256,
}
impl Tr for S with (P)
"#;
    let nested = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

mod types {
    pub struct Value { pub n: u256 }
    pub struct Other { pub n: u256 }
    pub mod inner {
        pub struct Deep { pub n: u256 }
    }
}

trait Tr {
    const fn m() -> u256
}

pub struct P {}
impl Derive<trait Tr> for P {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            const fn m() -> u256 {
                if true {
                    use types::{Value, inner::*}
                    Value { n: 7 }.n
                } else {
                    0
                }
            }
        }
    }
}

struct S {
    a: u256,
}
impl Tr for S with (P)
"#;

    for source in [shadow, nested] {
        let (db, file) = input(source);
        let error = prepare_file(&db, file)
            .err()
            .expect("expected a glob import in a quotation to be refused");
        assert!(
            error
                .cause
                .contains("a glob import is not supported inside a quotation"),
            "{}",
            error.cause
        );
    }
}

fn assert_generated_method_integer(
    db: &DriverDataBase,
    file: common::file::File,
    function_name: &str,
    expected: &str,
) {
    let diagnostics = db.run_on_top_mod(db.top_mod(file));
    assert!(
        !diagnostics.has_errors(db),
        "{}",
        diagnostics.format_diags(db)
    );
    let function = db
        .top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|function| {
            function.body(db).is_some()
                && function
                    .name(db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(db) == function_name)
        })
        .expect("the generated method must exist");
    let owner = BodyOwner::Func(function, None);
    let result = eval_body_owner_const(db, owner, GenericSubst::for_body_owner(db, owner, vec![]));
    let EvalOutcome::Ready(value) = result else {
        panic!("generated method did not evaluate: {result:?}")
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("generated method did not return an integer")
    };
    assert_eq!(value.to_string(), expected);
}

/// An attribute on the first statement of an expression quotation stays on
/// that statement when the fragment is spliced into a body.
#[test]
fn an_attribute_on_the_first_statement_of_an_expression_quotation_survives_splicing() {
    const SOURCE: &str = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Expr, Item}

const fn looped() -> Code<Expr> {
    quote {
        #[unroll]
        for x in [1, 2] { let _y: u256 = x }
    }
}

pub struct P {}
impl DeriveInherent for P {
    const fn derive(_ r: Record) -> Code<Item> {
        let body = looped()
        quote {
            pub fn run() -> u256 {
                ${body}
                7
            }
        }
    }
}

struct S {
    a: u256,
}
impl S with (P)
"#;
    let (db, file) = input(SOURCE);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{}", error.cause))
        .expect("output view must be produced");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let diagnostics = output.run_on_top_mod(output.top_mod(output_file));
    assert!(
        !diagnostics.has_errors(&output),
        "{}",
        diagnostics.format_diags(&output)
    );
    let run = output
        .top_mod(output_file)
        .all_funcs(&output)
        .iter()
        .copied()
        .find(|function| {
            function.body(&output).is_some()
                && function
                    .name(&output)
                    .to_opt()
                    .is_some_and(|ident| ident.data(&output) == "run")
        })
        .expect("the generated function must exist");
    let body = run.body(&output).unwrap();
    let hint = body.stmts(&output).iter().find_map(|(_, stmt)| match stmt {
        Partial::Present(Stmt::For(.., hint)) => Some(*hint),
        Partial::Present(_) | Partial::Absent => None,
    });
    assert_eq!(hint, Some(Some(true)));
}

#[test]
fn a_provider_that_recurses_polymorphically_is_refused_without_hanging() {
    const RECURSIVE_PROVIDER: &str = r#"use core::derive::{Derive, DeriveInherent}
use core::meta::{Record, Code, Item}

const fn deep<T>(_ n: usize) -> bool {
    if n == 0 { true } else { deep<(T,)>(n - 1) }
}

struct P {}
impl DeriveInherent for P {
    const fn derive(_ r: Record) -> Code<Item> {
        let ok = deep<u256>(2)
        quote { pub const fn marker() -> u256 { 7 } }
    }
}

struct S { a: u256 }
impl S with (P)
"#;
    let (db, file) = input(RECURSIVE_PROVIDER);
    assert!(prepare_file(&db, file).is_err());
}

#[test]
fn binders_of_a_tuple_struct_bind_by_position() {
    let source = "use core::derive::DeriveInherent
use core::meta::{Code, Item, Record, binders, field}
struct Tup {}
impl DeriveInherent for Tup {
    const fn derive(_ record: Record) -> Code<Item> {
        let b = binders(record, 0)
        let f = field(record, 1)
        quote { pub const fn second(self) -> u256 { match self { ${b} => ${b.at(f)} } } }
    }
}
struct T2(u256, u256)
impl T2 with (Tup)
const fn run() -> u256 { T2(3, 4).second() }
";
    let (db, file) = input(source);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{error}"))
        .expect("a prepared view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_integer(&output, output_file, "run", "4");
}

#[test]
fn an_import_opens_a_quoted_expression_block_that_a_provider_splices() {
    let source = "use core::derive::DeriveInherent
use core::meta::{Code, Item, Expr, Record}
mod m {
    pub const fn a() -> u256 { 1 }
    pub const fn b() -> u256 { 2 }
}
const fn body() -> Code<Expr> {
    quote {
        use m::{a, b}
        a() + b()
    }
}
struct P {}
impl DeriveInherent for P {
    const fn derive(_ record: Record) -> Code<Item> {
        let e = body()
        quote { pub const fn f() -> u256 { ${e} } }
    }
}
struct S {}
impl S with (P)
const fn run() -> u256 { S::f() }
";
    let (db, file) = input(source);
    let output = prepare_file(&db, file)
        .unwrap_or_else(|error| panic!("{error}"))
        .expect("a prepared view");
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert_generated_integer(&output, output_file, "run", "3");
}

#[test]
fn empty_takes_only_the_kinds_that_seed_a_fold() {
    for kind in ["Item", "Arm", "Where"] {
        let source = format!(
            "use core::meta::{{Arm, Code, Item, Where, empty}}\nconst fn seed() -> Code<{kind}> {{ empty<{kind}>() }}\n"
        );
        let (db, file) = input(&source);
        let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
        assert!(diagnostics.is_empty(), "{kind}: {diagnostics}");
    }
    for kind in ["Expr", "Pat", "u256", "bool"] {
        let source = format!(
            "use core::meta::{{Code, Expr, Pat, empty}}\nconst fn seed() -> Code<{kind}> {{ empty<{kind}>() }}\n"
        );
        let (db, file) = input(&source);
        let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
        assert!(diagnostics.contains("Seed"), "{kind}: {diagnostics}");
    }
}
