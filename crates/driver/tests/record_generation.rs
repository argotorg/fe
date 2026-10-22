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

    let valid_twin = source
        .replace(
            "let second = consume(x)",
            "let second = consume(NonCopy { v: 20 })",
        )
        .replace(
            "var base: u256 = 0\n    let bumped = alias_bump(mut base, mut base)",
            "var base: u256 = 0\n    var other: u256 = 0\n    let bumped = alias_bump(mut base, mut other)",
        );
    let (db, file) = input(&valid_twin);
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
        "use core::meta::{Code, Item}\nconst fn helper_with_local_quote() -> usize {\n    let _q: Code<Item> = quote {\n        fn generated() -> usize {\n            1\n        }\n    }\n    1\n}\n#[test]\nfn test_helper() {\n    assert(helper_with_local_quote() == 1)\n}\n",
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
    assert_eq!(output.generation_inputs().functions(&output).len(), 1);

    let invalid = source.replace("boost(${n})", "true");
    let (db, file) = input(&invalid);
    let error = prepare_file(&db, file)
        .err()
        .expect("actual errors still refuse output");
    assert!(error.cause.contains("type mismatch"), "{}", error.cause);
    assert!(db.generation_inputs().functions(&db).is_empty());
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
    assert_eq!(output.generation_inputs().functions(&output).len(), 1);
    let invalid = source.replace("word: 0", "word: true");
    let (db, file) = input(&invalid);
    let error = prepare_file(&db, file)
        .err()
        .expect("real body error refuses provider");
    assert!(error.cause.contains("type mismatch"), "{}", error.cause);
    assert!(db.generation_inputs().functions(&db).is_empty());
}
