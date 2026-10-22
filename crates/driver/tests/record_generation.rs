use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::{
    analysis::{
        semantic::{
            EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::ItemKind,
};
use url::Url;

const SOURCE: &str = "use core::meta::{Code, Item, Record, field_count}\nmod author {\n use core::meta::{Code, Item, Record, field_count}\n const fn boost(_ n: usize) -> u256 { n as u256 + 40 }\n pub const fn make(_ record: Record) -> Code<Item> { let n = field_count(record)\n quote { const fn generated_count() -> u256 { boost(${n}) } } }\n}\nfn boost(_ n: usize) -> u256 { 999 }\n#[generate(author::make)]\nstruct Pair { first: u256, second: bool }\n";

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
    use salsa::Setter;

    let (mut db, file) = input(SOURCE);
    db.compilation_settings()
        .set_target(&mut db)
        .to(common::compilation::Target::Native);
    let output = prepare_file(&db, file).unwrap().unwrap();
    assert_eq!(
        output.compilation_settings().target(&output),
        common::compilation::Target::Native
    );
    assert_eq!(
        output.compilation_settings().profile(&output),
        db.compilation_settings().profile(&db)
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
            SOURCE.replace("pub const fn make", "const fn make"),
            "provider is not visible",
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
    let source = r#"use core::meta::{Code, Item, Record, field_count}

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

pub const fn inspect(_ r: Record) -> Code<Item> {
    let x = NonCopy { v: 20 }
    let first = consume(x)
    let second = consume(x)
    var base: u256 = 0
    let bumped = alias_bump(mut base, mut base)
    let words: usize = (first + second + bumped).downcast_unchecked()
    let total: usize = words + field_count(r)
    quote {
        fn generated() -> usize {
            ${total}
        }
    }
}

#[generate(inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}

#[test]
fn test_borrow_bypass_executes() {
    assert(generated() == 42)
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
        r#"use core::meta::{Code, Item, Record, field_count}

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

pub const fn inspect(_ r: Record) -> Code<Item> {
    let x = NonCopy { v: 20 }
    let first = consume(x)
    let second = consume(NonCopy { v: 20 })
    var base: u256 = 0
    var other: u256 = 0
    let bumped = alias_bump(mut base, mut other)
    let words: usize = (first + second + bumped).downcast_unchecked()
    let total: usize = words + field_count(r)
    quote {
        fn generated() -> usize {
            ${total}
        }
    }
}

#[generate(inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}

#[test]
fn test_borrow_bypass_executes() {
    assert(generated() == 42)
}
"#,
    );
    assert!(prepare_file(&db, file).unwrap().is_some());
}

#[test]
fn helper_type_error_retains_original_cause_and_span() {
    let (db, file) = input(
        r#"use core::meta::{Code, Item, Record, field_count}

const fn broken_helper() -> usize {
    let bad: usize = true
    bad
}

pub const fn inspect(_ r: Record) -> Code<Item> {
    let n = field_count(r) + broken_helper()
    quote {
        fn generated() -> usize {
            ${n}
        }
    }
}

#[generate(inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}
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
        r#"use core::meta::{Code, Item, Record, field_count}

pub const fn inspect(_ r: Record) -> Code<Item> {
    let n = field_count(r)
    quote {
        fn generated() -> usize {
            ${ ${n} }
        }
    }
}

#[generate(inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}
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
        r#"use core::meta::{Code, Item, Record, field_count}

mod author {
    use core::meta::{Code, Item, Record, field_count}

    pub const fn inspect(_ r: Record) -> Code<Item> {
        const fn local_boost(_ n: usize) -> usize {
            n + 40
        }
        let n = field_count(r)
        quote {
            fn generated() -> usize {
                local_boost(${n})
            }
        }
    }
}

fn local_boost(_ n: usize) -> usize {
    999
}

#[generate(author::inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}

#[test]
fn test_func_local_hygiene() {
    assert(generated() == 42)
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
        r#"use core::meta::{Code, Item, Record, field_count}

mod author {
    use core::meta::{Code, Item, Record, field_count}

    pub const fn inspect(_ r: Record) -> Code<Item> {
        const fn local_boost(_ n: usize) -> usize {
            n + 40
        }
        let n = field_count(r)
        quote {
            fn generated() -> usize {
                n
            }
        }
    }
}

fn local_boost(_ n: usize) -> usize {
    999
}

#[generate(author::inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}

#[test]
fn test_func_local_hygiene() {
    assert(generated() == 42)
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

pub const fn make(_ record: Record) -> Code<Item> {
    let fields = field_count(record)
    let zero: u32 = 0
    let proved = rate_with(zero)
    let flat = with (FlatRate) { zero.rate() }
    let total = fields + proved + flat
    quote {
        const fn generated_quote_total() -> usize {
            ${total}
        }
    }
}

#[generate(make)]
struct Pair {
    first: u256,
    second: bool,
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
    let source = r#"use core::meta::{Code, Expr, Field, Item, Record, field, field_count, field_type}
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

pub const fn within(_ record: Record) -> Code<Item> {
    let _ = field_count(record)
    let ty = field_type(field(record, 0))
    let body = fold_fields<WithinStep>(record)
    quote(min, max) {
        impl ${record} {
            pub fn within(self, _ min: ${ty}, _ max: ${ty}) -> bool {
                ${body}
            }
        }
    }
}

pub const fn const_within(_ record: Record) -> Code<Item> {
    let ty = field_type(field(record, 0))
    let body = fold_fields<WithinStep>(record)
    quote(min, max) {
        impl ${record} {
            pub const fn is_within(self, _ min: ${ty}, _ max: ${ty}) -> bool {
                ${body}
            }
        }
    }
}

#[generate(within)]
struct Box2<T: Ord> {
    width: T,
    height: T,
}

#[generate(const_within)]
struct U256Box {
    width: u256,
    height: u256,
}

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
    let source = r#"use core::meta::{Arm, Code, Expr, Item, Record, Where, binders, empty, field, field_count, field_type, variant, variant_count}
use core::ops::Eq

pub const fn enum_eq(_ record: Record) -> Code<Item> {
    let n = variant_count(record)

    var arms = empty<Arm>()
    var bounds = empty<Where>()
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
            bounds = quote { where ${bounds}, ${field_type(f)}: Eq }
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
        impl Eq for ${record} where ${bounds} {
            fn eq(self, _ other: Self) -> bool {
                match self {
                    ${arms}
                }
            }
        }
    }
}

pub const fn const_enum_eq(_ record: Record) -> Code<Item> {
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
        impl ${record} {
            pub const fn is_eq(self, _ other: Self) -> bool {
                match self {
                    ${arms}
                }
            }
        }
    }
}

#[generate(enum_eq)]
#[generate(const_enum_eq)]
enum Direction {
    North,
    South,
}

#[generate(enum_eq)]
#[generate(const_enum_eq)]
enum Shape {
    Unit,
    Scale(u256),
    Box { w: u256, h: u256 },
    Pair { other: u256, x: u256 },
}

#[generate(enum_eq)]
enum Envelope<T: Eq> {
    Empty,
    Value(T),
    Pair { other: T, x: T },
}

#[generate(enum_eq)]
enum Opt<T> {
    Nothing,
    Just(T),
}

fn check_generic_enums() -> bool {
    let p1 = Envelope::Pair { other: 10 as u256, x: 20 as u256 }
    let p2 = Envelope::Pair { other: 10 as u256, x: 20 as u256 }
    let p3 = Envelope::Pair { other: 99 as u256, x: 20 as u256 }
    let e1 = p1 == p2
    let e2 = p1 != p3
    let j1: Opt<u256> = Opt::Just(7)
    let j2: Opt<u256> = Opt::Just(7)
    let n1: Opt<u256> = Opt::Nothing
    let o1 = j1 == j2
    let o2 = j1 != n1
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
use core::meta::{Code, FieldQuery, Item, Record, field, field_count, field_type, query_field, type_is}
use std::derive::{AbiHeadSize, AbiIsDynamic, fold_fields}

struct AbiFacts<T> {}
impl<T: AbiSize> FieldQuery<(u256, bool)> for AbiFacts<T> {
    const fn evaluate() -> (u256, bool) {
        (<T as AbiSize>::HEAD_SIZE, <T as AbiSize>::IS_DYNAMIC)
    }
}

pub const fn inspect_abi(_ record: Record) -> Code<Item> {
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

#[generate(inspect_abi)]
struct Packet {
    id: u256,
    ok: bool,
    amount: u256,
}

const fn check_summary() -> u256 {
    let (queried_bytes, u256_count, folded_bytes, dynamic) = record_abi_summary()
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
fn quote_binder_hygiene_rejects_sibling_arm_after_match_nested_group_and_foreign_field() {
    let sibling_arm = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn bad(_ record: Record) -> Code<Item> {
    let v = variant(record, 0)
    let lhs = binders(v, 0)
    let f = field(v, 0)
    quote {
        impl ${record} {
            pub fn check(self) -> bool {
                match self {
                    ${lhs} => true,
                    _ => ${lhs.at(f)} == 0
                }
            }
        }
    }
}

#[generate(bad)]
enum Choice {
    One(u256),
    Two,
}
"#;
    let after_match = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn bad(_ record: Record) -> Code<Item> {
    let v = variant(record, 0)
    let lhs = binders(v, 0)
    let f = field(v, 0)
    quote {
        impl ${record} {
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

#[generate(bad)]
enum Choice {
    One(u256),
    Two,
}
"#;
    let same_group_nested = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn bad(_ record: Record) -> Code<Item> {
    let v = variant(record, 0)
    let outer = binders(v, 0)
    let inner = binders(v, 0)
    let f = field(v, 0)
    quote(other) {
        impl ${record} {
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

#[generate(bad)]
enum Choice {
    One(u256),
    Two,
}
"#;
    let foreign_field = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn bad(_ record: Record) -> Code<Item> {
    let v0 = variant(record, 0)
    let v1 = variant(record, 1)
    let lhs = binders(v0, 0)
    let foreign = field(v1, 0)
    let bad_expr = lhs.at(foreign)
    quote {
        impl ${record} {
            pub fn check(self) -> bool {
                match self {
                    ${lhs} => ${bad_expr} == 0,
                    _ => false
                }
            }
        }
    }
}

#[generate(bad)]
enum Choice {
    One(u256),
    Two(u256),
}
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
    let source = r#"use core::meta::{Code, Expr, Field, Item, Record, field, field_type}

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
    use core::meta::{Code, Expr, Field, Item, Record, field, field_type}
    use super::helper_mod::make_sub_expr

    pub const fn generate_impl(_ record: Record) -> Code<Item> {
        let f0: Field = field(record, 0)
        let ty0 = field_type(f0)
        let sub: Code<Expr> = make_sub_expr()
        quote(extra) {
            impl ${record} {
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

#[generate(provider_mod::generate_impl)]
struct TargetRecord<T: LocalTrait> {
    first: LocalField,
    second: T,
}

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

    let unbound_open = r#"use core::meta::{Code, Item, Record}

pub const fn bad_open(_ record: Record) -> Code<Item> {
    quote(missing_binder) {
        impl ${record} {
            pub fn check(self) -> u256 {
                missing_binder
            }
        }
    }
}

#[generate(bad_open)]
struct Pair {
    a: u256,
}
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
    let no_collision = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn make_check(_ record: Record) -> Code<Item> {
    let v = variant(record, 0)
    let lhs = binders(v, 0)
    let f = field(v, 0)
    quote {
        impl ${record} {
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

#[generate(make_check)]
enum CollisionEnum {
    Val { __fe_binder_0_0: u256 },
    Empty,
}

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

    let let_init_leak = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn bad(_ record: Record) -> Code<Item> {
    let v = variant(record, 0)
    let lhs = binders(v, 0)
    let f = field(v, 0)
    quote {
        impl ${record} {
            pub fn check(self) -> bool {
                let ${lhs} = ${lhs.at(f)}
                true
            }
        }
    }
}

#[generate(bad)]
enum Single {
    One(u256),
}
"#;
    let if_let_else_leak = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn bad(_ record: Record) -> Code<Item> {
    let v = variant(record, 0)
    let lhs = binders(v, 0)
    let f = field(v, 0)
    quote {
        impl ${record} {
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

#[generate(bad)]
enum Choice {
    One(u256),
    Two,
}
"#;
    let while_let_after_leak = r#"use core::meta::{Code, Item, Record, binders, field, variant}

pub const fn bad(_ record: Record) -> Code<Item> {
    let v = variant(record, 0)
    let lhs = binders(v, 0)
    let f = field(v, 0)
    quote {
        impl ${record} {
            pub fn check(self) -> bool {
                while let ${lhs} = self {
                    break
                }
                ${lhs.at(f)} == 0
            }
        }
    }
}

#[generate(bad)]
enum Choice {
    One(u256),
    Two,
}
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
    let bad_query_field = r#"use core::meta::{Code, FieldQuery, Item, Record, field, field_type, query_field}

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

pub const fn inspect(_ r: Record) -> Code<Item> {
    let ty = field_type(field(r, 0))
    let v = query_field<BadQuery, u256>(ty)
    quote {
        const fn out() -> u256 {
            ${v}
        }
    }
}

#[generate(inspect)]
struct Pair {
    a: u256,
}
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

    let bad_transitive_helper = r#"use core::meta::{Code, Item, Record, field_count}

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

pub const fn inspect(_ r: Record) -> Code<Item> {
    let n = field_count(r) + transitive_bad()
    quote {
        const fn out() -> usize {
            ${n}
        }
    }
}

#[generate(inspect)]
struct Pair {
    a: u256,
}
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
fn mismatched_splice_kinds_and_recursive_generate_attribute_are_refused() {
    let field_in_type_hole = r#"use core::meta::{Code, Item, Record, field}

pub const fn bad(_ r: Record) -> Code<Item> {
    let f = field(r, 0)
    quote {
        fn generated(_ x: ${f}) -> u256 {
            0
        }
    }
}

#[generate(bad)]
struct Pair {
    a: u256,
}
"#;
    let expr_in_where_hole = r#"use core::meta::{Code, Expr, Item, Record}

pub const fn bad(_ r: Record) -> Code<Item> {
    let e: Code<Expr> = quote { true }
    quote {
        impl ${r} where ${e} {
            pub fn check(self) -> bool {
                true
            }
        }
    }
}

#[generate(bad)]
struct Pair {
    a: u256,
}
"#;
    let expr_in_arm_hole = r#"use core::meta::{Code, Expr, Item, Record}

pub const fn bad(_ r: Record) -> Code<Item> {
    let e: Code<Expr> = quote { true }
    quote {
        impl ${r} {
            pub fn check(self) -> bool {
                match self {
                    ${e}
                }
            }
        }
    }
}

#[generate(bad)]
struct Pair {
    a: u256,
}
"#;
    let recursive_generate = r#"use core::meta::{Code, Item, Record}

pub const fn loop_gen(_ r: Record) -> Code<Item> {
    quote {
        #[generate(loop_gen)]
        fn generated() -> u256 {
            1
        }
    }
}

#[generate(loop_gen)]
struct Pair {
    a: u256,
}
"#;
    for (source, expected) in [
        (
            field_in_type_hole,
            "type quotation hole requires Type<T> or FieldType<T, F> splice",
        ),
        (
            expr_in_where_hole,
            "where-clause quotation hole requires Code<Where> splice",
        ),
        (
            expr_in_arm_hole,
            "match-arm quotation hole requires Code<Arm> splice",
        ),
        (recursive_generate, "cannot carry #[generate] attributes"),
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
}

#[test]
fn provider_field_and_field_type_projection_and_boundary_refusals() {
    let valid_source = r#"
use core::meta::{Code, Field, FieldType, Item, Record, field, field_count, field_type}

pub const fn inspect_fields(_ record: Record) -> Code<Item> {
    let count = field_count(record)
    let first: Field = field(record, 0)
    let second: Field = field(record, 1)
    let _first_ty: FieldType = field_type(first)
    let _second_ty: FieldType = field_type(second)
    quote {
        const fn generated_field_summary() -> usize {
            ${count}
        }
    }
}

#[generate(inspect_fields)]
struct Pair {
    first: u256,
    second: bool,
}
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

    let submod_source = r#"
use core::meta::{Code, Item, Record}

pub const fn make(_ r: Record) -> Code<Item> {
    quote {
        fn generated() -> usize { 1 }
    }
}

mod nested {
    #[generate(super::make)]
    pub struct Inner {
        pub x: u256,
    }
}
"#;
    let (db, file) = input(submod_source);
    let submod_err = prepare_file(&db, file)
        .err()
        .expect("submodule #[generate] must refuse");
    assert!(
        submod_err
            .cause
            .contains("generation request requires an authored concrete root record"),
        "{}",
        submod_err.cause
    );
}

#[test]
fn transitive_helper_access_check_and_unsafe_block_hygiene_and_quoted_attrs() {
    // Finding 1: Transitive #1821 ownership/move check on provider helper callees before CTFE.
    let helper_move_source = r#"
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

pub const fn inspect(_ r: Record) -> Code<Item> {
    let total = field_count(r) + bad_move_helper()
    quote {
        fn generated() -> usize {
            ${total}
        }
    }
}

#[generate(inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}
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

    // Finding 3: Quote directly inside `unsafe { ... }` block preserves producer-block lexical hygiene without panicking.
    let unsafe_quote_source = r#"
use core::meta::{Code, Item, Record, field_count}

mod author {
    use core::meta::{Code, Item, Record, field_count}

    pub const fn inspect(_ r: Record) -> Code<Item> {
        let n = field_count(r)
        unsafe {
            const fn unsafe_local_boost(_ x: usize) -> usize {
                x + 40
            }
            quote {
                const fn generated_from_unsafe() -> usize {
                    unsafe_local_boost(${n})
                }
            }
        }
    }
}

const fn unsafe_local_boost(_ x: usize) -> usize {
    999
}

#[generate(author::inspect)]
pub struct Pair {
    pub a: u256,
    pub b: u256,
}
"#;
    let (db, file) = input(unsafe_quote_source);
    let output = prepare_file(&db, file).unwrap().unwrap();
    let output_file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    let module = output.top_mod(output_file);
    assert!(output.run_on_top_mod(module).is_empty());

    // Finding 4a: Quoted function carrying `#[generate(...)]` is rejected.
    let quoted_generate_source = r#"
use core::meta::{Code, Item, Record}

pub const fn inspect(_ r: Record) -> Code<Item> {
    quote {
        #[generate(inspect)]
        fn generated() -> usize {
            1
        }
    }
}

#[generate(inspect)]
pub struct Pair {
    pub a: u256,
}
"#;
    let (db, file) = input(quoted_generate_source);
    let quoted_gen_err = prepare_file(&db, file)
        .err()
        .expect("quoted #[generate] on function must refuse");
    assert!(
        quoted_gen_err
            .cause
            .contains("quoted functions cannot carry #[generate] attributes"),
        "{}",
        quoted_gen_err.cause
    );

    // Finding 4b: Quoted function carrying invalid function attribute (`#[event]`) is diagnosed by `validate_func_attrs`.
    let quoted_event_attr_source = r#"
use core::meta::{Code, Item, Record}

pub const fn inspect(_ r: Record) -> Code<Item> {
    quote {
        #[event]
        fn generated() -> usize {
            1
        }
    }
}

#[generate(inspect)]
pub struct Pair {
    pub a: u256,
}
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
    for path in ["author :: make", "author:: make", "author ::make"] {
        let source = SOURCE.replace("author::make", path);
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
    let source = r#"use core::meta::{Code, Item, Record}
trait Extra { const fn value() -> u256 }
trait Mark { const fn mark() -> u256 }
struct HasExtra {}
struct NoExtra {}
impl Extra for HasExtra { const fn value() -> u256 { 7 } }
pub const fn make(_ record: Record) -> Code<Item> {
    quote {
        impl<T: Extra> Mark for ${record} {
            const fn mark() -> u256 { <T as Extra>::value() }
        }
    }
}
#[generate(make)]
struct Box<T> { value: T }
const fn observe() -> u256 { <Box<HasExtra> as Mark>::mark() }
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
    let error = prepare_file(&db, file)
        .err()
        .expect("missing Extra must prevent Mark application");
    assert!(
        error.cause.contains("Extra") || error.cause.contains("Mark"),
        "{error}"
    );
}

#[test]
fn quoted_impl_incompatible_same_name_generic_kind_is_refused() {
    let source = r#"use core::meta::{Code, Item, Record}
pub const fn make(_ record: Record) -> Code<Item> {
    quote { impl<const T: usize> ${record} {} }
}
#[generate(make)]
struct Box<T> { value: T }
"#;
    let (db, file) = input(source);
    let error = prepare_file(&db, file)
        .err()
        .expect("type/const binder conflict must refuse");
    assert!(
        error
            .cause
            .contains("incompatible type or const declaration"),
        "{error}"
    );
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
        diagnostics.contains("this trait cannot have a named implementation"),
        "{diagnostics}"
    );
    assert!(diagnostics.contains("FieldQuery"), "{diagnostics}");
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
fn provider_body_warnings_do_not_refuse_generation_or_hide_errors() {
    let source = r#"use core::meta::{Record, Code, Item}
struct Cell { word: u256 }
impl Cell { const fn get_mut(mut self) -> mut u256 { mut self.word } }
#[generate(make)]
struct Target {}
const fn make(_ record: Record) -> Code<Item> {
    var cell = Cell { word: 0 }
    var w = cell.get_mut()
    w += 1
    quote { const fn value() -> u256 { 7 } }
}
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
