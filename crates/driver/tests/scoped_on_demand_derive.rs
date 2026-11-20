use common::InputDb;
use fe_driver::{DriverDataBase, generation::prepare_file};
use hir::analysis::{
    semantic::{EvalOutcome, GenericSubst, SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::ty_check::BodyOwner,
};
use url::Url;

fn input(source: &str) -> (DriverDataBase, common::file::File) {
    let mut db = DriverDataBase::default();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap(),
        Some(source.into()),
    );
    (db, file)
}

fn prepare(source: &str) -> Result<DriverDataBase, String> {
    let (db, file) = input(source);
    let result = prepare_file(&db, file)
        .map_err(|e| e.cause)?
        .ok_or_else(|| "expected generation output database".to_string())?;
    assert!(db.generation_inputs().functions(&db).is_empty());
    assert_eq!(file.text(&db).as_str(), source);
    Ok(result)
}

fn prepare_err(source: &str) -> String {
    match prepare(source) {
        Ok(_) => panic!("expected prepare_file to fail, but it succeeded"),
        Err(err) => err,
    }
}

fn prepared_diagnostics(source: &str) -> String {
    let output = prepare(source).expect("the error is in written code");
    let url = Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap();
    let file = output.workspace().get(&output, &url).unwrap();
    output
        .run_on_top_mod(output.top_mod(file))
        .format_diags(&output)
}

fn eval_const_fn_u256(db: &DriverDataBase, fn_name: &str) -> String {
    try_eval_const_fn_u256(db, fn_name).unwrap_or_else(|outcome| {
        panic!("expected Ready for `{fn_name}`, got {outcome}");
    })
}

/// The integer a const function evaluates to, or its unfinished outcome.
fn try_eval_const_fn_u256(db: &DriverDataBase, fn_name: &str) -> Result<String, String> {
    let url = Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap();
    let file = db.workspace().get(db, &url).unwrap();
    let module = db.top_mod(file);
    let function = module
        .all_funcs(db)
        .iter()
        .copied()
        .find(|f| {
            f.name(db)
                .to_opt()
                .is_some_and(|name| name.data(db) == fn_name)
        })
        .unwrap_or_else(|| panic!("missing function `{fn_name}`"));
    let owner = BodyOwner::Func(function, None);
    let outcome = eval_body_owner_const(
        db,
        owner,
        GenericSubst::for_body_owner(db, owner, Vec::new()),
    );
    let EvalOutcome::Ready(value) = outcome else {
        return Err(format!("{outcome:?}"));
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("expected Int scalar for `{fn_name}`");
    };
    Ok(value.to_string())
}

#[test]
fn derive_on_demand_in_with_block_for_struct_and_enum() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count, variant_count}

trait Score {
    const fn score() -> u256
}

trait ItemScore {
    const fn item_score(self) -> u256
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}

pub enum Choice {
    One,
    Two,
    Three,
}

struct FieldCountScore {}
impl Derive<Score> for FieldCountScore {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record) as u256
        quote {
            const fn score() -> u256 { 20 + ${n} }
        }
    }
}
impl Derive<ItemScore> for FieldCountScore {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record) as u256
        quote {
            const fn item_score(self) -> u256 { 30 + ${n} }
        }
    }
}

struct VariantCountScore {}
impl Derive<Score> for VariantCountScore {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = variant_count(record) as u256
        quote {
            const fn score() -> u256 { 40 + ${n} }
        }
    }
}

const fn check_type_and_method_calls() -> u256 {
    let p = Pair { a: 1, b: 2 }
    let a = with (FieldCountScore) {
        Pair::score() + p.item_score()
    }
    let b = with (VariantCountScore {}) {
        Choice::score()
    }
    a + b
}

const fn Helper<T: Score + ItemScore>(_ x: T) -> u256 {
    T::score() + x.item_score()
}

const fn check_generic_bound_delivery() -> u256 {
    let p = Pair { a: 1, b: 2 }
    with (FieldCountScore) {
        Helper(p)
    }
}
"#;
    let output = prepare(source).expect("scoped on-demand derive should succeed");
    // Pair has 2 fields -> score() = 22, item_score() = 32; Choice has 3 variants -> score() = 43.
    // Total = 22 + 32 + 43 = 97.
    assert_eq!(
        eval_const_fn_u256(&output, "check_type_and_method_calls"),
        "97"
    );
    // Helper(p) -> 22 + 32 = 54.
    assert_eq!(
        eval_const_fn_u256(&output, "check_generic_bound_delivery"),
        "54"
    );

    // Each opening retains its selected behavior, and prepared output cannot
    // be submitted as a new authored generation input.
    let url = Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap();
    let output_file = output.workspace().get(&output, &url).unwrap();
    let second_pass_err = prepare_file(&output, output_file)
        .err()
        .expect("second generation pass on output DB must be refused");
    assert!(
        second_pass_err.cause.contains("authored input view"),
        "{}",
        second_pass_err.cause
    );
}

#[test]
fn derive_on_demand_outside_block_and_unparameterized_callee_do_not_leak() {
    let outside_block = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct Pair {
    pub a: u256,
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> u256 { 42 }
        }
    }
}

const fn inside() -> u256 {
    with (Provider) {
        Pair::score()
    }
}

const fn outside() -> u256 {
    Pair::score()
}
"#;

    let callee_body = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct Pair {
    pub a: u256,
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> u256 { 42 }
        }
    }
}

const fn unparameterized_callee() -> u256 {
    Pair::score()
}

const fn caller() -> u256 {
    with (Provider) {
        let a = Pair::score()
        a + unparameterized_callee()
    }
}
"#;
    for source in [outside_block, callee_body] {
        let output = prepare(source).expect("the error is in written code");
        let url = Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap();
        let file = output.workspace().get(&output, &url).unwrap();
        let diagnostics = output
            .run_on_top_mod(output.top_mod(file))
            .format_diags(&output);
        assert!(
            diagnostics.contains("no method named `score` found for struct `Pair`"),
            "{diagnostics}"
        );
    }
}

#[test]
fn closer_wins_over_ordinary_impl_and_nested_with_frames() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

trait Score {
    const fn score(self) -> u256
}

pub struct Pair {
    pub a: u256,
    pub b: u256,
}

impl Score for Pair {
    const fn score(self) -> u256 { 10 }
}

impl Score for Pair as NamedScore {
    const fn score(self) -> u256 { 90 }
}

struct ProviderA {}
impl Derive<Score> for ProviderA {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record) as u256
        quote {
            const fn score(self) -> u256 { 20 + ${n} }
        }
    }
}

struct ProviderB {}
impl Derive<Score> for ProviderB {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record) as u256
        quote {
            const fn score(self) -> u256 { 100 + ${n} }
        }
    }
}

const fn ordinary_then_scoped_then_restored() -> u256 {
    let p = Pair { a: 1, b: 2 }
    let before = p.score()
    let during = with (ProviderA) {
        p.score()
    }
    let after = p.score()
    before * 10000 + during * 100 + after
}

const fn nested_named_and_providers() -> u256 {
    let p = Pair { a: 1, b: 2 }
    let a = with (NamedScore) {
        let outer_named = p.score()
        let inner_prov = with (ProviderA) {
            p.score()
        }
        let restored_named = p.score()
        outer_named + inner_prov + restored_named
    }
    let b = with (ProviderA) {
        let outer_prov = p.score()
        let inner_named = with (NamedScore) {
            p.score()
        }
        let inner_prov_b = with (ProviderB) {
            p.score()
        }
        let restored_prov = p.score()
        outer_prov + inner_named + inner_prov_b + restored_prov
    }
    a * 1000 + b
}
"#;
    let output = prepare(source).expect("closer-wins scoped derive should succeed");
    // before = 10; during = 22; after = 10 -> 102210
    assert_eq!(
        eval_const_fn_u256(&output, "ordinary_then_scoped_then_restored"),
        "102210"
    );
    // a = 90 + 22 + 90 = 202
    // b = 22 + 90 + 102 + 22 = 236
    // a * 1000 + b = 202236
    assert_eq!(
        eval_const_fn_u256(&output, "nested_named_and_providers"),
        "202236"
    );
}

#[test]
fn same_header_ambiguity_and_foreign_trait_refuse_before_provider_ctfe() {
    let provider_and_named = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct Pair {}

impl Score for Pair as NamedScore {
    const fn score() -> u256 { 90 }
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote {
            const fn score() -> u256 { ${fault} }
        }
    }
}

const fn conflict() -> u256 {
    with (Provider, NamedScore) {
        Pair::score()
    }
}
"#;
    let rendered = prepare_err(provider_and_named);
    assert!(
        rendered
            .contains("one `with` header gives a derive provider and a choice for the same trait")
            && !rendered.contains("division by zero"),
        "{rendered}"
    );

    let two_providers = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct Pair {}

struct ProviderA {}
impl Derive<Score> for ProviderA {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn score() -> u256 { ${fault} } }
    }
}

struct ProviderB {}
impl Derive<Score> for ProviderB {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> u256 { 2 } }
    }
}

const fn conflict() -> u256 {
    with (ProviderA, ProviderB) {
        Pair::score()
    }
}
"#;
    for source in [
        two_providers.to_owned(),
        two_providers.replace("ProviderA, ProviderB", "ProviderB, ProviderA"),
    ] {
        let rendered2 = prepare_err(&source);
        assert!(
            rendered2.contains("one `with` header gives two derive providers for the same trait")
                && !rendered2.contains("division by zero"),
            "{rendered2}"
        );
    }

    let protected_scoped = r#"
use core::default::Default
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub struct Pair {}

struct Provider {}
impl Derive<Default> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let _ = 1 / zero
        quote {
            fn default() -> Self { Pair {} }
        }
    }
}

fn forbidden() -> Pair {
    with (Provider) {
        Pair::default()
    }
}
"#;
    let rendered3 = prepare_err(protected_scoped);
    assert!(
        rendered3.contains("a scoped derive supports traits of the same package; for a trait of another package, write a header")
            && !rendered3.contains("division by zero"),
        "{rendered3}"
    );
}

#[test]
fn preflight_admission_refuses_false_provider_and_ill_formed_nominals_before_ctfe() {
    let false_provider_cond = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct Pair {}

struct BadProvider {}
impl Derive<Score> for BadProvider where false {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote {
            const fn score() -> u256 { ${fault} }
        }
    }
}

const fn run() -> u256 {
    with (BadProvider) {
        Pair::score()
    }
}
"#;
    let err = prepare_err(false_provider_cond);
    assert!(
        err.contains("provider application not admitted")
            && err.contains("const where predicate failed")
            && !err.contains("division by zero"),
        "{err}"
    );

    let ill_formed_provider_nominal = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct Pair {}

struct BadProvider where false {}
impl Derive<Score> for BadProvider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote {
            const fn score() -> u256 { ${fault} }
        }
    }
}

const fn run() -> u256 {
    with (BadProvider) {
        Pair::score()
    }
}
"#;
    let err2 = prepare_err(ill_formed_provider_nominal);
    assert!(
        err2.contains("const where predicate failed")
            && err2.contains("BadProvider")
            && !err2.contains("division by zero"),
        "{err2}"
    );

    let ill_formed_target_nominal = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct BadTarget where false {}

struct FaultingProvider {}
impl Derive<Score> for FaultingProvider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote {
            const fn score() -> u256 { ${fault} }
        }
    }
}

const fn run() -> u256 {
    with (FaultingProvider) {
        BadTarget::score()
    }
}
"#;
    let err3 = prepare_err(ill_formed_target_nominal);
    assert!(
        err3.contains("const where predicate failed")
            && err3.contains("BadTarget")
            && !err3.contains("division by zero"),
        "{err3}"
    );

    let generic_target = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

struct FaultingProvider {}
impl Derive<Score> for FaultingProvider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote {
            const fn score() -> u256 { ${fault} }
        }
    }
}

const fn generic_caller<T>(_ x: T) -> u256 {
    with (FaultingProvider) {
        T::score()
    }
}
"#;
    let err4 = prepare_err(generic_target);
    assert!(
        err4.contains("scoped derive requires a concrete authored record")
            && !err4.contains("division by zero"),
        "{err4}"
    );
}

#[test]
fn written_types_refuse_provider_choices_while_ordinary_layout_stays_valid() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> usize
}

pub struct Pair {}

impl Score for Pair {
    const fn score() -> usize { 2 }
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> usize { 4 }
        }
    }
}

const fn array_len_under_provider() -> usize {
    with (Provider) {
        let xs: [u256; Pair::score()] = [0; 2]
        Pair::score()
    }
}
"#;
    let err = prepared_diagnostics(source);
    assert!(
        err.contains("8-0138") && err.contains("this written type reads a chosen implementation"),
        "{err}"
    );
    let ordinary = source.replace("with (Provider) {", "{");
    let (db, file) = input(&ordinary);
    assert!(prepare_file(&db, file).unwrap().is_none());
    let diagnostics = db.run_on_top_mod(db.top_mod(file));
    assert!(
        !diagnostics.has_errors(&db),
        "ordinary layout must remain valid"
    );
    assert_eq!(eval_const_fn_u256(&db, "array_len_under_provider"), "2");
}

#[test]
fn non_derivable_targets_under_with_provider_refuse_instead_of_falling_back() {
    let primitive_method = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score(self) -> u256
}

impl Score for u256 {
    const fn score(self) -> u256 { 10 }
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote {
            const fn score(self) -> u256 { ${fault} }
        }
    }
}

const fn run() -> u256 {
    let x: u256 = 5
    with (Provider) {
        x.score()
    }
}
"#;
    let err1 = prepare_err(primitive_method);
    assert!(
        err1.contains("scoped derive requires a concrete authored record")
            && !err1.contains("division by zero"),
        "{err1}"
    );

    let primitive_bound_call = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score(self) -> u256
}

impl Score for u256 {
    const fn score(self) -> u256 { 10 }
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score(self) -> u256 { 42 }
        }
    }
}

const fn score_of<T: Score>(_ x: T) -> u256 {
    x.score()
}

const fn run() -> u256 {
    let x: u256 = 5
    with (Provider) {
        score_of(x)
    }
}
"#;
    let err2 = prepare_err(primitive_bound_call);
    assert!(
        err2.contains("scoped derive requires a concrete authored record"),
        "{err2}"
    );

    let generic_bound_fallback = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score(self) -> u256
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score(self) -> u256 { 42 }
        }
    }
}

const fn generic_caller<T: Score>(_ x: T) -> u256 {
    with (Provider) {
        x.score()
    }
}
"#;
    let err4 = prepare_err(generic_bound_fallback);
    assert!(
        err4.contains("scoped derive requires a concrete authored record"),
        "{err4}"
    );
}

#[test]
fn unmaterialized_scoped_derive_demand_fails_closed() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score(self) -> u256
}

pub struct Pair {
    pub a: u256,
}

impl Score for Pair {
    const fn score(self) -> u256 { 10 }
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score(self) -> u256 { 42 }
        }
    }
}

const fn run() -> u256 {
    let p = Pair { a: 1 }
    with (Provider) {
        p.score()
    }
}
"#;
    let mut db = DriverDataBase::default();
    let url = Url::parse("file:///unmaterialized_scoped_derive.fe").unwrap();
    let file = db.workspace().touch(&mut db, url, Some(source.to_owned()));
    let top_mod = db.top_mod(file);
    let func = top_mod
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| func.name(&db).to_opt().map(|id| id.data(&db).as_str()) == Some("run"))
        .expect("expected `run` function");
    let (diags, typed) = hir::analysis::ty::ty_check::check_func_body(&db, func);
    assert!(
        typed
            .generation_demands()
            .iter()
            .any(|request| request.demand.is_ok()),
        "raw checking must preserve the actual preparation request"
    );
    assert!(
        diags.iter().any(|diag| matches!(
            diag,
            hir::analysis::ty::diagnostics::FuncBodyDiag::Body(
                hir::analysis::ty::diagnostics::BodyDiag::CompileTimePhase { message, .. }
            ) if message.contains("prepared generated implementation")
        )),
        "expected unfulfilled scoped derive diagnostic when checking without prepare_file, got: {diags:#?}"
    );

    let pass2_generated_demand = r#"
use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> u256
}

pub struct Seed {}

pub struct LateTarget {}

impl Score for LateTarget {
    const fn score() -> u256 { 10 }
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> u256 { 42 }
        }
    }
}

struct GenFn {}
impl Derive<Inherent> for GenFn {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn late_caller() -> u256 {
                with (Provider) {
                    LateTarget::score()
                }
            }
        }
    }
}

pub struct Trigger {
    pub seed: Seed,
}
impl Trigger with (GenFn)
"#;
    let err = prepare_err(pass2_generated_demand);
    assert!(
        err.contains("generated output admission failed")
            && err.contains("generated openings need a portable generated-owner identity"),
        "{err}"
    );
}

#[test]
fn scoped_derive_demands_in_contract_and_assoc_const_bodies_are_materialized() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

trait Score {
    const fn score() -> u256
}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record) as u256
        quote {
            const fn score() -> u256 { 100 + ${n} }
        }
    }
}

pub struct InImplConst {
    pub a: u256,
}

pub struct InImplTraitConst {
    pub a: u256,
    pub b: u256,
}

pub struct InContractRecv {
    pub a: u256,
    pub b: u256,
    pub c: u256,
}

pub struct Holder {}

impl Holder {
    const C: u256 = with (Provider) {
        InImplConst::score()
    }
}

trait HasConst {
    const TC: u256
}

impl HasConst for Holder {
    const TC: u256 = with (Provider) {
        InImplTraitConst::score()
    }
}

msg AppMsg {
    #[selector = 0x01]
    Ping -> u256,
}

pub contract App {
    recv AppMsg {
        Ping -> u256 {
            with (Provider) {
                InContractRecv::score()
            }
        }
    }
}

const fn total() -> u256 {
    Holder::C + <Holder as HasConst>::TC
}
"#;
    let output = prepare(source).expect("contract and assoc-const scoped derive should succeed");
    let url = Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap();
    let file = output.workspace().get(&output, &url).unwrap();
    let top_mod = output.top_mod(file);
    use hir::span::LazySpan;
    let contract_target = top_mod
        .all_structs(&output)
        .iter()
        .copied()
        .find(|record| {
            record
                .name(&output)
                .to_opt()
                .is_some_and(|name| name.data(&output) == "InContractRecv")
        })
        .expect("the authored contract target exists");
    let target_range = contract_target.span().resolve(&output).unwrap().range;
    assert!(
        output
            .generation_inputs()
            .requests(&output)
            .iter()
            .any(
                |request| request.scoped_impl.as_ref().is_some_and(|scoped| scoped
                    .demand
                    .target
                    .target_file
                    == url
                    && scoped.demand.target.target_range == target_range)
            ),
        "contract receive preparation must retain its exact target declaration"
    );

    assert_eq!(eval_const_fn_u256(&output, "total"), "203");
}

#[test]
fn scoped_derive_selection_name_avoids_module_delimiter_collision() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item, field_count}

pub trait Score {
    const fn score() -> u256
}

pub struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = field_count(record) as u256
        quote {
            const fn score() -> u256 { 10 + ${n} }
        }
    }
}

pub struct a_Pair {
    pub x: u256,
}

mod a {
    pub struct Pair {
        pub x: u256,
        pub y: u256,
    }
}

const fn check_both() -> u256 {
    with (Provider) {
        a_Pair::score() * 100 + a::Pair::score()
    }
}
"#;
    let output = prepare(source).expect("distinct module paths should not collide");
    assert_eq!(eval_const_fn_u256(&output, "check_both"), "1112");
}

/// Scoped requests over instances of a generic record were refused ("scoped
/// derive cannot yet preserve concrete generic target arguments") until frozen
/// demands carried the target's arguments. Each instance is now its own
/// implementation, and a field-type splice is that instance's field type.
#[test]
fn scoped_derive_preserves_private_producer_helper_and_generic_target_arguments() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Value { const fn value() -> u256 }
struct Pair {}
mod author {
    use super::Value
    use core::derive::Derive
    use core::meta::{Code, Item, Record}
    const fn helper() -> u256 { 7 }
    pub struct Provider {}
    impl Derive<Value> for Provider {
        const fn derive(_ record: Record) -> Code<Item> {
            quote { const fn value() -> u256 { helper() } }
        }
    }
}
const fn helper() -> u256 { 99 }
const fn observe() -> u256 {
    with (author::Provider) { Pair::value() }
}
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "observe"), "7");
    let both = source
        .replace("struct Pair {}", "struct Box<T> { value: T }")
        .replace(
            "Pair::value()",
            "<Box<u8> as Value>::value() * 100 + <Box<u256> as Value>::value()",
        );
    let output = prepare(&both).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "observe"), "707");

    // The instance's field type reaches the generated body: 255 + 1 fits
    // `u256` but overflows `u8`. (A literal's range is not checked against a
    // narrower type, so the arithmetic is the discriminator.)
    let typed = both
        .replace(
            "    use core::meta::{Code, Item, Record}\n    const fn helper",
            "    use core::meta::{Code, Item, Record, field, field_type}\n    const fn helper",
        )
        .replace(
            "            quote { const fn value() -> u256 { helper() } }",
            "            let ty = field_type(field(record, 0))\n            quote { const fn value() -> u256 { let v: ${ty} = 255\n                let w = v + 1\n                w as u256 + helper() } }",
        );
    assert_ne!(typed, both);
    let wide = typed.replace("<Box<u8> as Value>::value() * 100 + ", "");
    let output = prepare(&wide).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "observe"), "263");
    let narrow = typed.replace(" + <Box<u256> as Value>::value()", "");
    let output = prepare(&narrow).unwrap();
    let outcome = try_eval_const_fn_u256(&output, "observe");
    assert!(
        outcome.is_err(),
        "`Box<u8>` must overflow at 255 + 1, got {outcome:?}"
    );
}

/// A scoped provider may be a generic type applied to arguments. Which
/// application a `with` block names is decided where the block is written:
/// the request is identified by the binding, so two applications of one
/// generic provider in one function stay two implementations, also when they
/// are named through aliases.
#[test]
fn scoped_derive_admits_generic_provider_applications() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Value { const fn value() -> u256 }
struct Pair {}
struct Provider<T> {}
impl Derive<Value> for Provider<u8> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 1 } }
    }
}
impl Derive<Value> for Provider<u256> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 2 } }
    }
}
type P8 = Provider<u8>
type P256 = Provider<u256>
const fn observe() -> u256 {
    with (P8) { Pair::value() } * 100 + with (P256) { Pair::value() }
}
const fn spelled() -> u256 {
    with (Provider<u256>) { Pair::value() } * 100 + with (Provider<u8>) { Pair::value() }
}
struct Scaled<const K: u256> {}
impl<const K: u256> Derive<Value> for Scaled<K> {
    const fn derive(_ record: Record) -> Code<Item> {
        let k = K
        quote { const fn value() -> u256 { ${k} } }
    }
}
const fn scaled() -> u256 {
    with (Scaled<10>) { Pair::value() } * 100 + with (Scaled<20>) { Pair::value() }
}
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "observe"), "102");
    assert_eq!(eval_const_fn_u256(&output, "spelled"), "201");
    assert_eq!(eval_const_fn_u256(&output, "scaled"), "1020");
}

#[test]
fn quoted_members_do_not_capture_provider_impl_parameters() {
    let prelude = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Value { const fn value() -> u256 }
struct Pair {}
"#;
    let constants = r#"struct Provider<const K: u256> {}
impl<const K: u256> Derive<Value> for Provider<K> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { K } }
    }
}
"#;
    let types = r#"struct Provider<T> {}
impl<T> Derive<Value> for Provider<T> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn value() -> u256 {
                let _: Option<T> = Option::None
                7
            }
        }
    }
}
"#;
    for (provider, arg, parameter) in [(constants, "7", "K"), (types, "u8", "T")] {
        for request in [
            format!("impl Value for Pair with (Provider<{arg}>)"),
            format!("const fn result() -> u256 {{ with (Provider<{arg}>) {{ Pair::value() }} }}"),
        ] {
            let source = format!("{prelude}{provider}{request}");
            let (db, file) = input(&source);
            let diagnostics = match prepare_file(&db, file) {
                Err(error) => error.cause,
                Ok(Some(output)) => output
                    .run_on_top_mod(output.top_mod(file))
                    .format_diags(&output),
                Ok(None) => panic!("expected generation in {request}"),
            };
            assert!(
                diagnostics.contains(&format!("`{parameter}`"))
                    && (diagnostics.contains("undefined") || diagnostics.contains("is not found")),
                "{request}: {diagnostics}"
            );
        }
    }
}

#[test]
fn scoped_provider_supplies_prerequisite_of_a_named_implementation() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field_count}

trait Arity { const fn arity() -> usize }
trait Report { const fn report() -> usize }
struct Pair { a: u256, b: u256 }
struct Box<T> { value: T }

struct CountFields {}
impl Derive<Arity> for CountFields {
    const fn derive(_ record: Record) -> Code<Item> {
        let fields = field_count(record)
        quote { const fn arity() -> usize { ${fields} } }
    }
}
impl<T: Arity> Report for Box<T> as Fancy {
    const fn report() -> usize { T::arity() + 1 }
}

const fn nested() -> usize {
    with (CountFields) { with (Fancy) { <Box<Pair> as Report>::report() } }
}
const fn listed() -> usize {
    with (CountFields, Fancy) { <Box<Pair> as Report>::report() }
}
"#;
    let output = prepare(source).unwrap();
    for function in ["nested", "listed"] {
        assert_eq!(eval_const_fn_u256(&output, function), "3");
    }
}

#[test]
fn scoped_derive_refuses_associated_effect_rows_before_running_the_provider() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}

trait WithRow {
    uses Row
    const fn value() -> u256
}
struct Pair {}
struct Provider {}
impl Derive<WithRow> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault: u256 = 1 / zero
        quote { const fn value() -> u256 { ${fault} } }
    }
}
const fn result() -> u256 { with (Provider) { Pair::value() } }
"#;
    let cause = prepare_err(source);
    assert!(cause.contains("without associated outputs"), "{cause}");
    assert!(!cause.contains("division by zero"), "{cause}");
}

/// A type-generic provider applied to a concrete argument, and a constant
/// generic provider applied through an alias.
#[test]
fn scoped_derive_admits_applications_through_type_and_constant_arguments() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Report { const fn weight() -> u256 }
struct Pair {}
struct Weighted<T> {}
impl<T> Derive<Report> for Weighted<T> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn weight() -> u256 { 7 } }
    }
}
struct Scaled<const K: u256> {}
impl<const K: u256> Derive<Report> for Scaled<K> {
    const fn derive(_ record: Record) -> Code<Item> {
        let k = K
        quote { const fn weight() -> u256 { ${k} } }
    }
}
type Ten = Scaled<10>
const fn concrete() -> u256 { with (Weighted<u8>) { Pair::weight() } }
const fn aliased() -> u256 { with (Ten) { Pair::weight() } }
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "concrete"), "7");
    assert_eq!(eval_const_fn_u256(&output, "aliased"), "10");
}

/// The provider application and the target's arguments are independent
/// parts of one request: each instance of a generic record gets the output of
/// the application named for it, and the provider sees that instance.
#[test]
fn scoped_derive_combines_generic_provider_and_generic_target() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record, field, field_type}
trait Value { const fn value() -> u256 }
struct Box<T> { value: T }
struct Scaled<const K: u256> {}
impl<const K: u256> Derive<Value> for Scaled<K> {
    const fn derive(_ record: Record) -> Code<Item> {
        let k = K
        let ty = field_type(field(record, 0))
        quote { const fn value() -> u256 { let v: ${ty} = 255
            v as u256 + ${k} } }
    }
}
const fn observe() -> u256 {
    with (Scaled<10>) { <Box<u8> as Value>::value() } * 1000
        + with (Scaled<20>) { <Box<u256> as Value>::value() }
}
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "observe"), "265275");
}

/// A generic provider keeps the private helpers of the module that wrote it.
#[test]
fn scoped_generic_provider_keeps_its_producer_helpers() {
    let source = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Value { const fn value() -> u256 }
struct Pair {}
mod author {
    use super::Value
    use core::derive::Derive
    use core::meta::{Code, Item, Record}
    const fn helper() -> u256 { 7 }
    pub struct Scaled<const K: u256> {}
    impl<const K: u256> Derive<Value> for Scaled<K> {
        const fn derive(_ record: Record) -> Code<Item> {
            let k = K * helper()
            quote { const fn value() -> u256 { ${k} + helper() } }
        }
    }
}
const fn helper() -> u256 { 99 }
const fn observe() -> u256 {
    with (author::Scaled<3>) { Pair::value() }
}
"#;
    let output = prepare(source).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "observe"), "28");
}

/// An application that has no provider implementation, or whose bound fails,
/// is refused where it is written, as an explicit header is.
#[test]
fn scoped_derive_refuses_unadmitted_generic_provider_applications() {
    let header = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Value { const fn value() -> u256 }
struct Pair {}
struct Provider<T> {}
impl Derive<Value> for Provider<u8> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 1 } }
    }
}
"#;
    // `Provider<u16>` has no `Derive` implementation, so the block is not a
    // provider opening at all and ordinary checking refuses it.
    let missing =
        format!("{header}const fn run() -> u256 {{ with (Provider<u16>) {{ Pair::value() }} }}\n");
    let (db, file) = input(&missing);
    assert!(
        db.run_on_top_mod(db.top_mod(file)).has_errors(&db),
        "an unadmitted application must be refused"
    );

    let bounded = r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Value { const fn value() -> u256 }
trait Wide {}
impl Wide for u256 {}
struct Pair {}
struct Provider<T> {}
impl<T: Wide> Derive<Value> for Provider<T> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 1 } }
    }
}
const fn good() -> u256 { with (Provider<u256>) { Pair::value() } }
"#;
    let output = prepare(bounded).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "good"), "1");
    let bad = prepare_err(&format!(
        "{bounded}const fn bad() -> u256 {{ with (Provider<u8>) {{ Pair::value() }} }}\n"
    ));
    assert!(
        bad.contains("no ordinary Derive<requested goal> provider implementation was admitted"),
        "{bad}"
    );
}

/// A provider application that names a type parameter of the enclosing
/// function is not ground; it is refused with the provider's own message and
/// is not mistaken for an application of the parameter's instances.
#[test]
fn scoped_derive_refuses_generic_provider_over_an_enclosing_parameter() {
    let cause = prepare_err(
        r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
trait Value { const fn value() -> u256 }
struct Pair {}
struct Provider<T> {}
impl<T> Derive<Value> for Provider<T> {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 1 } }
    }
}
fn run<T>() -> u256 { with (Provider<T>) { Pair::value() } }
"#,
    );
    assert!(cause.contains("concrete provider type"), "{cause}");
}

#[test]
fn ordinary_named_choice_does_not_require_generation() {
    let (db, file) = input(
        r#"
trait Score { const fn score() -> u256 }
struct Pair {}
impl Score for Pair as Named { const fn score() -> u256 { 7 } }
const fn run() -> u256 { with (Named) { Pair::score() } }
"#,
    );
    assert!(prepare_file(&db, file).unwrap().is_none());
}

const RULE: &str = "what generation produces in its own package is not visible to it";

#[test]
fn provider_cannot_open_another_providers_implementation() {
    let cause = prepare_err(
        r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
trait Seed { const fn seed() -> u256 }
struct Pair {}
struct Token {}
struct A {}
struct B {}
impl Derive<Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = helper()
        quote { const fn score() -> u256 { ${n} } }
    }
}
const fn helper() -> u256 { with (B) { Token::seed() } }
impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> u256 { 42 } }
    }
}
const fn run() -> u256 { with (A) { Pair::score() } }
"#,
    );
    assert!(cause.contains(RULE), "{cause}");
}

#[test]
fn provider_cannot_read_a_constant_that_opens_an_implementation() {
    let cause = prepare_err(
        r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
trait Seed { const fn seed() -> u256 }
struct Pair {}
struct Token {}
struct A {}
struct B {}
impl Derive<Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = SEEDED
        quote { const fn score() -> u256 { ${n} } }
    }
}
const SEEDED: u256 = with (B) { Token::seed() }
impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> u256 { 42 } }
    }
}
const fn run() -> u256 { with (A) { Pair::score() } }
"#,
    );
    assert!(cause.contains(RULE), "{cause}");
}

#[test]
fn providers_share_a_value_through_an_ordinary_helper() {
    let output = prepare(
        r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
trait Seed { const fn seed() -> u256 }
struct Pair {}
struct Token {}
struct A {}
struct B {}
const fn shared() -> u256 { 42 }
impl Derive<Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = shared()
        quote { const fn score() -> u256 { ${n} } }
    }
}
impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = shared()
        quote { const fn seed() -> u256 { ${n} } }
    }
}
const fn run() -> u256 { with (A) { Pair::score() } + with (B) { Token::seed() } }
"#,
    )
    .expect("providers that share an authored helper prepare in one round");
    assert_eq!(eval_const_fn_u256(&output, "run"), "84");
}

#[test]
fn self_opening_provider_is_refused_before_it_executes() {
    let cause = prepare_err(
        r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
struct Pair {}
struct A {}
impl Derive<Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (A) { Pair::score() }
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn score() -> u256 { ${n} + ${fault} } }
    }
}
const fn run() -> u256 { with (A) { Pair::score() } }
"#,
    );
    assert!(cause.contains(RULE), "{cause}");
    assert!(!cause.contains("division by zero"), "{cause}");
}

#[test]
fn dependent_provider_is_refused_before_it_executes() {
    let cause = prepare_err(
        r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
trait Value { const fn value() -> u256 }
struct Pair {}
struct Out {}
struct A {}
struct B {}
impl Derive<Score> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> u256 { missing_generated_value } }
    }
}
impl Derive<Value> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (B) { Pair::score() }
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn value() -> u256 { ${n} + ${fault} } }
    }
}
const fn run() -> u256 { with (A) { Out::value() } }
"#,
    );
    assert!(cause.contains(RULE), "{cause}");
    assert!(!cause.contains("division by zero"), "{cause}");
}

#[test]
fn provider_literals_refuse_field_evaluation_without_losing_empty_literal_support() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
struct Pair {}
impl Score for Pair { const fn score() -> u256 { 100 } }
struct Provider { n: u256 }
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> u256 { 42 } }
    }
}
const fn field_fault() -> u256 { let zero: u256 = 0
 1 / zero }
const fn run() -> u256 { with (Provider { n: field_fault() }) { Pair::score() } }
"#;
    let cause = prepare_err(source);
    assert!(
        cause.contains("provider literals must have no fields"),
        "{cause}"
    );
    assert!(!cause.contains("division by zero"), "{cause}");
    let positive = source
        .replace("struct Provider { n: u256 }", "struct Provider {}")
        .replace("Provider { n: field_fault() }", "Provider {}");
    let output = prepare(&positive).expect("empty literals remain valid provider operands");
    assert_eq!(eval_const_fn_u256(&output, "run"), "42");
}

#[test]
fn scoped_generation_in_static_assert_uses_its_checked_body_owner() {
    let output = prepare(
        r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
struct Pair {}
impl Score for Pair { const fn score() -> u256 { 100 } }
struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> u256 { 42 } }
    }
}
static_assert(with (Provider) { Pair::score() == 42 })
"#,
    )
    .expect("static assertions must rebind the same checked opening as preparation");
    assert!(
        output
            .generation_inputs()
            .requests(&output)
            .iter()
            .any(|request| request.scoped_impl.is_some())
    );
}

#[test]
fn provider_condition_cannot_wait_for_a_generated_implementation() {
    let source = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> usize }
trait Seed { const fn seed() -> usize }
pub struct Pair {}
pub struct Token {}
struct B {}
impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> usize { 42 } }
    }
}
struct A {}
impl Derive<Score> for A where { with (B) { Token::seed() == 42 } } {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: usize = 0
        let fault = 1 / zero
        quote { const fn score() -> usize { ${fault} } }
    }
}
const fn run() -> usize { with (A) { Pair::score() } }
"#;
    let error = prepare_err(source);
    assert!(
        error.contains("decides whether the provider applies") && error.contains(RULE),
        "{error}"
    );
    assert!(!error.contains("division by zero"), "{error}");
    let false_condition = source.replace("Token::seed() == 42", "Token::seed() == 41");
    let error = prepare_err(&false_condition);
    assert!(
        error.contains("decides whether the provider applies") && error.contains(RULE),
        "{error}"
    );
    assert!(!error.contains("division by zero"), "{error}");
}

#[test]
fn provider_condition_prepares_prerequisites_of_the_helpers_it_calls() {
    // The generated prerequisite is opened inside helpers that the provider's
    // condition calls. A provider runs against authored text only, so its
    // condition cannot wait for the prerequisite: the program is refused with
    // the rule whatever the order of the declarations, with the use declared
    // before or after its helpers, and whether the condition holds or not.
    let template = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> usize }
trait Seed { const fn seed() -> usize }
pub struct Pair {}
pub struct Token {}
struct B {}
impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> usize { 42 } }
    }
}
struct A {}
impl Derive<Score> for A where { condition() } {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> usize { 42 } }
    }
}
const fn run() -> usize { with (A) { Pair::score() } }
const fn condition() -> bool { inner() }
const fn inner() -> bool { with (B) { Token::seed() == 42 } }
"#;
    let run = "const fn run() -> usize { with (A) { Pair::score() } }\n";
    let helpers = "const fn condition() -> bool { inner() }\nconst fn inner() -> bool { with (B) { Token::seed() == 42 } }\n";
    for source in [
        template.to_owned(),
        template
            .replace(run, "")
            .replace(helpers, &format!("{helpers}{run}")),
        // The condition is the target type's rather than the provider's.
        template
            .replace(
                "pub struct Pair {}",
                "pub struct Pair where { condition() } {}",
            )
            .replace("for A where { condition() }", "for A"),
    ] {
        let error = prepare_err(&source);
        assert!(
            error.contains("decides whether the provider applies") && error.contains(RULE),
            "{error}"
        );
        let false_condition = source.replace("Token::seed() == 42", "Token::seed() == 41");
        let error = prepare_err(&false_condition);
        assert!(
            error.contains("decides whether the provider applies") && error.contains(RULE),
            "{error}"
        );
    }
}

#[test]
fn scoped_provider_type_respects_lexical_value_shadowing() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
pub struct Pair {}
impl Score for Pair { const fn score() -> u256 { 100 } }
struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ r: Record) -> Code<Item> {
        quote { const fn score() -> u256 { 42 } }
    }
}
const fn local(_ Provider: ()) -> u256 { let Provider = ()
    with (Provider) { Pair::score() }
}
const fn run() -> u256 { local(()) }
const fn parameter(_ Provider: ()) -> u256 { with (Provider) { Pair::score() } }
const fn parameter_run() -> u256 { parameter(()) }
"#;
    let (db, file) = input(source);
    assert!(prepare_file(&db, file).unwrap().is_none());
    assert_eq!(eval_const_fn_u256(&db, "run"), "100");
    assert_eq!(eval_const_fn_u256(&db, "parameter_run"), "100");
    // A bare name matching an existing struct is a constructor pattern, not
    // a new binding. The parameter supplies a lawful value binding so the
    // nested let can exercise local rebinding of the same provider spelling.
    let source = source
        .replace(
            "const fn local(_ Provider: ())",
            "const fn local(_ unused: ())",
        )
        .replace("let Provider = ()", "let other = ()");
    let output = prepare(&source).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "run"), "42");
}

#[test]
fn scoped_provider_alias_requires_its_own_visibility() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
pub struct Pair {}
mod author {
    use super::Score
    use core::derive::Derive
    use core::meta::{Record, Code, Item}
    pub struct Provider {}
    type Alias = Provider
    impl Derive<Score> for Provider {
        const fn derive(_ r: Record) -> Code<Item> {
            quote { const fn score() -> u256 { 42 } }
        }
    }
}
const fn run() -> u256 { with (author::Alias) { Pair::score() } }
"#;
    let error = prepare_err(source);
    assert!(error.contains("provider path is not visible"), "{error}");
    let output = prepare(&source.replace("type Alias", "pub type Alias")).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "run"), "42");
}

#[test]
fn scoped_external_target_is_refused_before_provider_execution() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ r: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn score() -> u256 { ${fault} } }
    }
}
const fn run() -> u256 { with (Provider) { core::iter::Identity<u256>::score() } }
"#;
    let error = prepare_err(source);
    assert!(
        error.contains("target declared in the requesting ingot"),
        "{error}"
    );
    assert!(!error.contains("division by zero"), "{error}");
}

#[test]
fn ordinary_choice_in_an_ingot_does_not_trigger_standalone_generation_restriction() {
    let mut db = DriverDataBase::default();
    db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/ordinary-choice-ingot/fe.toml").unwrap(),
        Some("[ingot]\nname = \"ordinary-choice\"\nversion = \"0.1.0\"\n".into()),
    );
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///tmp/ordinary-choice-ingot/src/lib.fe").unwrap(),
        Some(
            r#"
trait Score { const fn score() -> u256 }
struct Pair {}
impl Score for Pair as Named { const fn score() -> u256 { 7 } }
const fn run() -> u256 { with (Named) { Pair::score() } }
"#
            .into(),
        ),
    );
    assert_eq!(
        file.containing_ingot(&db).unwrap().kind(&db),
        common::ingot::IngotKind::Local
    );
    assert!(prepare_file(&db, file).unwrap().is_none());
    assert!(!db.run_on_top_mod(db.top_mod(file)).has_errors(&db));
}

#[test]
fn deferred_closure_effect_rows_preserve_pending_generation_outcomes() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { fn score(self) -> u256 }
pub struct Pair {}
impl Score for Pair { fn score(self) -> u256 { 100 } }
struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn score(self) -> u256 { 42 } }
    }
}
fn charge() -> u256 uses (p: Score) { p.score() }
fn apply<F: Fn() -> u256>(_ f: F) -> u256 uses F::E { f() }
fn run() -> u256 {
    let f = || charge()
    with (Provider, Score = Pair {}) { apply(f) }
}
"#;
    let (db, file) = input(source);
    let module = db.top_mod(file);
    let demands = hir::analysis::ty::trait_resolution::generation::preparation_demands(&db, module);
    assert!(
        !demands.is_empty(),
        "the substituted effect row must request generation"
    );
    assert!(demands.iter().all(|d| d.demand.is_ok()));
    let diagnostics = db.run_on_top_mod(module).format_diags(&db);
    assert!(diagnostics.contains("error[8-0137]"), "{diagnostics}");
    assert!(!diagnostics.contains("error[8-0031]"), "{diagnostics}");
    let output = prepare(source).unwrap();
    let file = output
        .workspace()
        .get(&output, &file.url(&db).unwrap())
        .unwrap();
    assert!(
        !output
            .run_on_top_mod(output.top_mod(file))
            .has_errors(&output)
    );
}

#[test]
fn scoped_derive_refuses_sibling_item_holes_without_dropping_output() {
    let source = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Value { const fn value() -> u256 }
struct Key {}
struct Provider {}
impl Derive<Value> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let extra = quote { const fn ignored() -> bool { missing } }
        quote {
            const fn value() -> u256 { 7 }
            ${extra}
        }
    }
}
const fn run() -> u256 { with (Provider) { Key::value() } }
const RESULT: u256 = run()
"#;
    for rejected in [
        source.to_owned(),
        source.replace("${extra}", "const fn ignored() -> bool { missing }"),
    ] {
        let (db, file) = input(&rejected);
        let error = prepare_file(&db, file)
            .err()
            .expect("every sibling output node must be admitted or refused");
        assert!(
            error
                .cause
                .contains("bounded derive requires one function template"),
            "{error}"
        );
        assert!(db.generation_inputs().functions(&db).is_empty());
        assert!(db.generation_inputs().requests(&db).is_empty());
        assert_eq!(file.text(&db).as_str(), rejected);
    }
    let source = source
        .replace(
            "        let extra = quote { const fn ignored() -> bool { missing } }\n",
            "        let answer: u256 = 7\n",
        )
        .replace("            ${extra}\n", "")
        .replace(
            "const fn value() -> u256 { 7 }",
            "const fn value() -> u256 { ${answer} }",
        );
    let output = prepare(&source).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "run"), "7");
}

#[test]
fn ambiguous_implementation_operand_refuses_before_provider_execution() {
    for header in ["Collision, Provider", "Provider, Collision"] {
        let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
trait Pick { const fn pick() -> u256 }
struct Pair {}
struct Provider {}
impl Pick for Pair as Collision { const fn pick() -> u256 { 7 } }
const Collision: u256 = 99
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn score() -> u256 { ${fault} } }
    }
}
const fn run() -> u256 { with (HEADER) { Pair::score() } }
"#
        .replace("HEADER", header);
        let cause = prepare_err(&source);
        assert!(cause.contains("ambiguous"), "{cause}");
        assert!(!cause.contains("division by zero"), "{cause}");
    }
    for header in ["m::Collision, Provider", "Provider, m::Collision"] {
        let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> u256 }
struct Pair {}
struct Provider {}
mod m {
    use ingot::Pair
    pub trait Pick { const fn pick() -> u256 }
    pub impl Pick for Pair as Collision { const fn pick() -> u256 { 7 } }
    pub const Collision: u256 = 99
}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote { const fn score() -> u256 { ${fault} } }
    }
}
const fn run() -> u256 { with (HEADER) { Pair::score() } }
"#
        .replace("HEADER", header);
        let cause = prepare_err(&source);
        assert!(cause.contains("ambiguous"), "{cause}");
        assert!(!cause.contains("division by zero"), "{cause}");
    }
}

#[test]
fn trait_associated_const_under_scoped_provider_refuses_before_provider_execution() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait HasLimit {
    const LIMIT: usize
}

trait Tag {
    const fn tag(self) -> usize
}

pub struct Pair {
    pub a: usize,
    pub b: usize,
}

struct Provider {}

impl Derive<HasLimit> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: usize = 0
        let boom: usize = 1 / zero
        quote {
            const LIMIT: usize = ${boom}
        }
    }
}

impl Derive<Tag> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: usize = 0
        let boom: usize = 1 / zero
        quote {
            const fn tag(self) -> usize { ${boom} }
        }
    }
}

pub fn bad_const() -> usize {
    with (Provider) {
        Pair::LIMIT
    }
}

pub fn run() -> usize {
    with (Provider) {
        Pair { a: 20, b: 22 }.tag()
    }
}
"#;
    for expr in ["Pair::LIMIT", "<Pair as HasLimit>::LIMIT"] {
        let source = source.replace("Pair::LIMIT", expr);
        let cause = prepare_err(&source);
        assert!(
            cause.contains(
                "scoped derive requires an unparameterized goal without associated outputs"
            ),
            "{expr}: {cause}"
        );
        assert!(!cause.contains("division by zero"), "{expr}: {cause}");
    }
}

#[test]
fn record_literal_provider_through_invisible_alias_refuses_before_provider_execution() {
    for literal in ["author::Hidden {}", "author::HiddenWithField { x: 1 }"] {
        let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Tag {
    const fn tag(self) -> usize
}

pub struct Pair {
    pub a: usize,
    pub b: usize,
}

mod author {
    use core::derive::Derive
    use core::meta::{Record, Code, Item}
    use super::Tag

    pub struct Provider {}
    pub struct ProviderWithField {
        pub x: usize,
    }
    type Hidden = Provider
    type HiddenWithField = ProviderWithField

    impl Derive<Tag> for Provider {
        const fn derive(_ record: Record) -> Code<Item> {
            let zero: usize = 0
            let boom: usize = 1 / zero
            quote {
                const fn tag(self) -> usize { ${boom} }
            }
        }
    }

    impl Derive<Tag> for ProviderWithField {
        const fn derive(_ record: Record) -> Code<Item> {
            let zero: usize = 0
            let boom: usize = 1 / zero
            quote {
                const fn tag(self) -> usize { ${boom} }
            }
        }
    }
}

pub fn bad() -> usize {
    with (LITERAL) {
        Pair { a: 20, b: 22 }.tag()
    }
}

pub fn valid_opening() -> usize {
    with (author::Provider) {
        Pair { a: 20, b: 22 }.tag()
    }
}
"#
        .replace("LITERAL", literal);
        let cause = prepare_err(&source);
        assert!(
            cause.contains("scoped derive provider path is not visible"),
            "{literal}: {cause}"
        );
        assert!(!cause.contains("division by zero"), "{literal}: {cause}");
    }
}

#[test]
fn qualified_path_wf_under_scoped_provider_sees_active_choices() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Tag {
    const fn tag() -> u256
}

trait HasAssoc {
    type Out
}

pub struct Pair {}
pub struct Wrap<T: Tag> {
    pub inner: T,
}
struct MyTag {}

impl<T> HasAssoc for T {
    type Out = u256
}

impl Derive<Tag> for MyTag {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn tag() -> u256 { 42 }
        }
    }
}

const fn with_call() -> u256 {
    with (MyTag) {
        let x: <Wrap<Pair> as HasAssoc>::Out = Pair::tag()
        x
    }
}

const fn wf_only() -> u256 {
    with (MyTag) {
        let x: <Wrap<Pair> as HasAssoc>::Out = 7
        x
    }
}
"#;
    let output = prepare(source).expect("qualified path wf under scoped provider should succeed");
    assert_eq!(eval_const_fn_u256(&output, "with_call"), "42");
    assert_eq!(eval_const_fn_u256(&output, "wf_only"), "7");
}

#[test]
fn provider_admission_discovers_scoped_derive_in_callee_and_type_where_clauses() {
    let callee_where_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Seed {
    const fn seed() -> u256
}

trait Score {
    const fn score() -> u256
}

pub struct Token {}
pub struct Pair {}
struct A {}
struct B {}

pub const fn run() -> u256 {
    with (A) { Pair::score() }
}

const fn check_seed<const N: u256>() -> bool
    where { read_seed() == N }
{
    true
}

const fn read_seed() -> u256 {
    with (B) { Token::seed() }
}

impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn seed() -> u256 { 42 }
        }
    }
}

impl Derive<Score> for A
    where { check_seed<42>() }
{
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> u256 { 7 }
        }
    }
}
"#;
    // Admission finds the scoped derive that the callee's where clause opens,
    // and refuses the provider with the rule instead of waiting for it.
    let error = prepare_err(callee_where_source);
    assert!(
        error.contains("decides whether the provider applies") && error.contains(RULE),
        "{error}"
    );

    let type_where_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Seed {
    const fn seed() -> u256
}

trait Score {
    const fn score() -> u256
}

pub struct Token {}
pub struct Guarded<const N: u256>
    where { read_seed() == N }
{}
pub struct Pair {
    pub g: Guarded<42>,
}
struct A {}
struct B {}

pub const fn run() -> u256 {
    with (A) { Pair::score() }
}

const fn read_seed() -> u256 {
    with (B) { Token::seed() }
}

impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn seed() -> u256 { 42 }
        }
    }
}

impl Derive<Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let _g: Guarded<42> = Guarded {}
        quote {
            const fn score() -> u256 { 9 }
        }
    }
}
"#;
    // The same for a type whose where clause opens it, constructed in the
    // derive function.
    let error = prepare_err(type_where_source);
    assert!(
        error.contains("decides whether the provider applies") && error.contains(RULE),
        "{error}"
    );
}

#[test]
fn anon_const_bodies_in_expr_and_written_type_positions_prepare_and_rebind() {
    let expr_anon_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Score {
    const fn score() -> usize
}

pub trait VisibleTrait {
    const fn val(self) -> usize
}

pub struct Pair {}

impl VisibleTrait for Pair {
    const fn val(self) -> usize { 4 }
}

mod sub {
    use super::Score
    pub trait HiddenTrait {
        const fn val(self) -> usize
    }
    impl<T: Score> HiddenTrait for T {
        const fn val(self) -> usize { 9 }
    }
}

struct MyScore {}

impl Derive<Score> for MyScore {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> usize { 4 }
        }
    }
}

pub const fn run() -> u256 {
    let xs = [true; { with (MyScore) { (Pair {}).val() } }]
    if xs[0] { 4 } else { 0 }
}
"#;
    let output = prepare(expr_anon_source)
        .expect("expression-position anon const body should rebind provider opening");
    assert_eq!(eval_const_fn_u256(&output, "run"), "4");

    let type_anon_rebind_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Score {
    const fn score() -> usize
}

pub trait VisibleTrait {
    const fn val(self) -> usize
}

pub struct Pair {}

impl VisibleTrait for Pair {
    const fn val(self) -> usize { 4 }
}

mod sub {
    use super::Score
    pub trait HiddenTrait {
        const fn val(self) -> usize
    }
    impl<T: Score> HiddenTrait for T {
        const fn val(self) -> usize { 9 }
    }
}

struct MyScore {}

impl Derive<Score> for MyScore {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> usize { 4 }
        }
    }
}

pub struct Buffer {
    pub bytes: [u8; { with (MyScore) { (Pair {}).val() } }],
}

pub const fn run() -> u256 {
    let xs: [u8; { with (MyScore) { (Pair {}).val() } }] = [7; 4]
    xs[0] as u256
}
"#;
    let output = prepare(type_anon_rebind_source)
        .expect("written-type anon const bodies should prepare and rebind provider opening");
    assert_eq!(eval_const_fn_u256(&output, "run"), "7");

    let type_anon_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score {
    const fn score() -> usize
}

pub struct Pair {}

struct MyScore {}

impl Derive<Score> for MyScore {
    const fn derive(_ record: Record) -> Code<Item> {
        quote {
            const fn score() -> usize { 4 }
        }
    }
}

pub struct Buffer {
    pub bytes: [u8; { with (MyScore) { Pair::score() } }],
}

pub const fn run() -> u256 {
    let xs: [u8; { with (MyScore) { Pair::score() } }] = [7; 4]
    xs[0] as u256
}
"#;
    let cause = prepared_diagnostics(type_anon_source);
    assert!(
        cause.contains("8-0138")
            && cause.contains("this written type reads a chosen implementation"),
        "{cause}"
    );

    let dropped_demand_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Score {
    const fn score() -> usize
}

pub trait VisibleTrait {
    const fn val(self) -> usize
}

pub struct Pair {}

impl VisibleTrait for Pair {
    const fn val(self) -> usize { 4 }
}

mod sub {
    use super::Score
    pub trait HiddenTrait {
        const fn val(self) -> usize
    }
    impl<T: Score> HiddenTrait for T {
        const fn val(self) -> usize { 9 }
    }
}

struct MyScore {}

impl Derive<Score> for MyScore {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: usize = 0
        let boom: usize = 1 / zero
        quote {
            const fn score() -> usize { ${boom} }
        }
    }
}

pub struct Buffer {
    pub bytes: [u8; { with (MyScore) { (Pair {}).val() } }],
}
"#;
    let cause = prepare_err(dropped_demand_source);
    assert!(cause.contains("division by zero"), "{cause}");
}

#[test]
fn derive_headers_respect_scoped_dependencies_and_preflight_refusals() {
    let dep_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Score {
    const fn score() -> u256
}

trait Seed {
    const fn seed() -> u256
}

pub struct Pair {}
pub struct Token {}
pub struct A {}
struct B {}

impl Derive<Score> for A {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = helper()
        quote { const fn score() -> u256 { ${n} } }
    }
}

const fn helper() -> u256 {
    with (B) { Token::seed() }
}

impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> u256 { 42 } }
    }
}

impl Score for Pair with (A)

pub const fn run() -> u256 {
    Pair::score()
}
"#;
    // A provider runs against authored text only: the header's provider
    // cannot wait for the output of a scoped derive and is refused with the
    // rule.
    let error = prepare_err(dep_source);
    assert!(
        error.contains("only generation can supply") && error.contains(RULE),
        "{error}"
    );

    let attr_dep_source = r#"
use core::derive::{Derive, Inherent}
use core::meta::{Record, Code, Item}

trait Seed {
    const fn seed() -> u256
}

pub struct Token {}
struct B {}

impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> u256 { 42 } }
    }
}

struct MakeHelper {}
impl Derive<Inherent> for MakeHelper {
    const fn derive(_ record: Record) -> Code<Item> {
        let n = with (B) { Token::seed() }
        quote {
            pub const fn generated_score() -> u256 { ${n} }
        }
    }
}

pub struct Host {}
impl Host with (MakeHelper)

pub const fn run() -> u256 {
    Host::generated_score()
}
"#;
    // The same for the provider of an inherent header.
    let error = prepare_err(attr_dep_source);
    assert!(
        error.contains("only generation can supply") && error.contains(RULE),
        "{error}"
    );

    let order_source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Score {
    const fn score() -> u256
}

pub struct Pair {}
pub struct FaultingProvider {}

impl Derive<Score> for FaultingProvider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let boom: u256 = 1 / zero
        quote {
            const fn score() -> u256 { ${boom} }
        }
    }
}

mod author {
    use super::Score
    use core::derive::Derive
    use core::meta::{Record, Code, Item}

    pub struct PublicProvider {}
    type HiddenAlias = PublicProvider

    impl Derive<Score> for PublicProvider {
        const fn derive(_ record: Record) -> Code<Item> {
            quote {
                const fn score() -> u256 { 1 }
            }
        }
    }
}

impl Score for Pair with (FaultingProvider)

pub fn bad_opening() -> u256 {
    with (author::HiddenAlias) { Pair::score() }
}
"#;
    // The program has two refusals: the provider of the header faults, and
    // the block opens a provider path that is not visible. The provider of a
    // header runs in the same view as every other provider, before the first
    // view is built, so its fault is the refusal that is reported; the order
    // of the channels decides only which refusal comes first.
    let cause = prepare_err(order_source);
    assert!(cause.contains("division by zero"), "{cause}");
    // Without the fault, the block's opening is refused.
    let cause = prepare_err(&order_source.replace("1 / zero", "1 + zero"));
    assert!(
        cause.contains("scoped derive provider path is not visible"),
        "{cause}"
    );
    assert!(!cause.contains("division by zero"), "{cause}");
}

#[test]
fn derive_header_admission_refuses_before_scoped_providers_run() {
    // `f` opens a provider that faults at compile time. A request refused at
    // admission is refused before that provider runs.
    let faulting_opening = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

pub trait Tag {
    const fn tag() -> u256
}
pub struct Pair {}

struct Faulty {}
impl Derive<Tag> for Faulty {
    const fn derive(_ r: Record) -> Code<Item> {
        let zero: u256 = 0
        let boom: u256 = 1 / zero
        quote {
            const fn tag() -> u256 { ${boom} }
        }
    }
}

pub fn f() -> u256 {
    with (Faulty) { Pair::tag() }
}
"#;
    let refused_explicit_target = format!(
        "{faulting_opening}
pub trait Score {{
    const fn score() -> u256
}}
pub struct Refused
    where {{ false }}
{{
    pub a: u256,
}}
struct Fine {{}}
impl Derive<Score> for Fine {{
    const fn derive(_ r: Record) -> Code<Item> {{
        quote {{ const fn score() -> u256 {{ 1 }} }}
    }}
}}
impl Score for Refused with (Fine)
"
    );
    let cause = prepare_err(&refused_explicit_target);
    assert!(cause.contains("const where predicate failed"), "{cause}");
    assert!(!cause.contains("division by zero"), "{cause}");

    let refused_inherent_target = format!(
        "{faulting_opening}
use core::derive::Inherent
struct Make {{}}
impl Derive<Inherent> for Make {{
    const fn derive(_ r: Record) -> Code<Item> {{
        quote {{ pub fn made() -> u256 {{ 1 }} }}
    }}
}}
pub struct Refused
    where {{ false }}
{{
    pub a: u256,
}}
impl Refused with (Make)
"
    );
    let cause = prepare_err(&refused_inherent_target);
    assert!(cause.contains("const where predicate failed"), "{cause}");
    assert!(!cause.contains("division by zero"), "{cause}");

    // An inherent header whose provider does not implement `Derive<Inherent>`
    // is refused at the request itself.
    let inherent_without_provider = format!(
        "{faulting_opening}
struct NoInherent {{}}
impl Pair with (NoInherent)
"
    );
    let (db, file) = input(&inherent_without_provider);
    let error = prepare_file(&db, file)
        .err()
        .expect("an inherent header without a provider implementation must be refused");
    assert!(!error.cause.contains("division by zero"), "{}", error.cause);
    let request = inherent_without_provider
        .find("impl Pair with (NoInherent)")
        .unwrap();
    assert_eq!(
        u32::from(error.range.start()) as usize,
        request,
        "{}",
        error.cause
    );

    // A provider does not wait for generated output: an explicit request
    // whose condition reads a generated implementation is refused at admission.
    let condition_reads_generated = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}
trait Score { const fn score() -> usize }
trait Seed { const fn seed() -> usize }
pub struct Pair {}
pub struct Token {}
struct B {}
impl Derive<Seed> for B {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn seed() -> usize { 42 } }
    }
}
struct A {}
impl Derive<Score> for A where { with (B) { Token::seed() == 42 } } {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn score() -> usize { 42 } }
    }
}
impl Score for Pair with (A)
pub const fn run() -> usize { Pair::score() }
"#;
    let error = prepare_err(condition_reads_generated);
    assert!(
        error.contains("provider application not admitted")
            && error.contains("scoped derive requires a prepared generated implementation"),
        "{error}"
    );
}

#[test]
fn pattern_trait_constant_under_a_provider_is_a_preparation_demand() {
    // A trait constant matched in a pattern under an opened provider waits for
    // the generated implementation as one read in an expression does, so both
    // reach the same refusal of the request.
    let template = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Limit {
    const LIMIT: u256
    const fn limit() -> u256
}
pub struct Pair {}
struct Faulty {}
impl Derive<Limit> for Faulty {
    const fn derive(_ r: Record) -> Code<Item> {
        let zero: u256 = 0
        let boom: u256 = 1 / zero
        quote {
            const fn limit() -> u256 { ${boom} }
        }
    }
}

pub fn matched(x: u256) -> u256 {
    with (Faulty) {
        USE
    }
}
"#;
    for read in [
        "match x {\n            Pair::LIMIT => 1\n            _ => 0\n        }",
        "if x == Pair::LIMIT { 1 } else { 0 }",
    ] {
        let cause = prepare_err(&template.replace("USE", read));
        assert!(
            cause.contains(
                "scoped derive requires an unparameterized goal without associated outputs"
            ),
            "{read}: {cause}"
        );
        assert!(!cause.contains("division by zero"), "{read}: {cause}");
    }
}

/// A header that a package writes is a fact when the code that a scoped
/// request generates is checked. The generated `Value` implementation needs
/// `Size`, which the header supplies, as a written implementation would.
#[test]
fn a_scoped_implementation_may_rely_on_a_supertrait_that_a_written_header_supplies() {
    let common = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Size {
    const fn size() -> u256
}
trait Value: Size {
    const fn value() -> u256
}

struct PSize {}
impl Derive<Size> for PSize {
    const fn derive(_ r: Record) -> Code<Item> {
        quote { const fn size() -> u256 { 5 } }
    }
}

struct QValue {}
impl Derive<Value> for QValue {
    const fn derive(_ r: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 7 } }
    }
}

struct S {
    a: u256,
}
"#;
    let tail = r#"
const fn use_it() -> u256 {
    with (QValue) { S::value() }
}
"#;
    let written =
        format!("{common}impl Size for S {{\n    const fn size() -> u256 {{ 5 }}\n}}\n{tail}");
    let output = prepare(&written).expect("the written implementation supplies the supertrait");
    assert_eq!(eval_const_fn_u256(&output, "use_it"), "7");

    let header = format!("{common}impl Size for S with (PSize)\n{tail}");
    let output = prepare(&header).expect("the written header supplies the supertrait");
    assert_eq!(eval_const_fn_u256(&output, "use_it"), "7");
}

/// A generated implementation whose supertrait nothing satisfies is refused
/// with the diagnostic of the failed bound, as an error and not a panic: the
/// diagnostic carries no source span of its own, so the error keeps the
/// location of the `with` binding.
#[test]
fn a_scoped_implementation_with_an_unsatisfied_supertrait_is_refused_without_a_panic() {
    let source = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Size {
    const fn size() -> u256
}
trait Value: Size {
    const fn value() -> u256
}

struct QValue {}
impl Derive<Value> for QValue {
    const fn derive(_ r: Record) -> Code<Item> {
        quote { const fn value() -> u256 { 7 } }
    }
}

struct S {
    a: u256,
}

const fn use_it() -> u256 {
    with (QValue) { S::value() }
}
"#;
    let error = prepare_err(source);
    assert!(
        error.contains("generated implementation admission failed"),
        "{error}"
    );
    assert!(error.contains("trait bound is not satisfied"), "{error}");
}

#[test]
fn with_scan_of_a_generic_field_extent_reports_the_ordinary_type_mismatch() {
    // `wrap<N>()` in a field type calls a const function whose body builds a
    // value, and checking that body reads which types grow.
    let source = r#"
trait Compute: Copy {
    type Item
    const fn compute(self) -> Self::Item
}

struct Size { value: usize }
impl Copy for Size {}
impl Compute for Size {
    type Item = usize
    const fn compute(self) -> usize { self.value }
}

const fn plain<const N: usize>() -> usize { N + 1 }
const fn plus<const N: usize>() -> usize uses (size: Compute<Item = usize>) {
    N + size.compute()
}
const fn wrap<const N: usize>() -> usize {
    with (Compute<Item = usize> = Size { value: 1 }) { plus<N>() }
}

struct Plain<const N: usize> { data: [u8; plain<N>()] }
struct Wrapped<const N: usize> { data: [u8; wrap<N>()] }

fn concrete() -> Plain<2> {
    let wrapped = Wrapped<2> { data: [1; 3] }
    Plain<2> { data: wrapped.data }
}

fn generic<const N: usize>(_ wrapped: Wrapped<N>) -> Plain<N> {
    Plain<N> { data: wrapped.data }
}
"#;
    let (db, file) = input(source);
    assert!(prepare_file(&db, file).unwrap().is_none());
    let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    assert_eq!(diagnostics.matches("error[").count(), 1, "{diagnostics}");
    assert!(
        diagnostics.contains("error[8-0000]: type mismatch"),
        "{diagnostics}"
    );
    assert!(
        diagnostics.contains("but `[u8; wrap<const N: usize>()]` is given"),
        "{diagnostics}"
    );
}

#[test]
fn with_scan_accepts_a_struct_value_in_a_generic_field_extent() {
    let source = r#"
trait Compute: Copy {
    const fn compute(self) -> usize
}

struct Size { value: usize }
impl Copy for Size {}
impl Compute for Size {
    const fn compute(self) -> usize { self.value }
}

const fn extent<const N: usize>() -> usize {
    let size = Size { value: N }
    size.value + 1
}

struct Wrapped<const N: usize> { data: [u8; extent<N>()] }

fn uses_with() -> usize {
    with (Compute = Size { value: 1 }) { 0 }
}
"#;
    let (db, file) = input(source);
    assert!(prepare_file(&db, file).unwrap().is_none());
    assert!(!db.run_on_top_mod(db.top_mod(file)).has_errors(&db));
}

#[test]
fn derive_header_on_a_record_with_a_generic_field_extent_prepares() {
    let source = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

trait Score { fn score(self) -> u256 }

struct Size { value: usize }

const fn extent<const N: usize>() -> usize {
    let size = Size { value: N }
    size.value + 1
}

pub struct Wrapped<const N: usize> { data: [u8; extent<N>()] }

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn score(self) -> u256 { 42 } }
    }
}

impl Score for Wrapped<2> with (Provider)
"#;
    let output = prepare(source).unwrap();
    let url = Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap();
    let file = output.workspace().get(&output, &url).unwrap();
    assert!(
        !output
            .run_on_top_mod(output.top_mod(file))
            .has_errors(&output)
    );
}

#[test]
fn an_operator_under_a_provider_for_a_trait_of_another_package_names_the_rule() {
    let (db, file) = input(
        r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
use core::ops::Eq
struct EqNothing {}
impl Derive<Eq> for EqNothing {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { fn eq(self, _ other: Self) -> bool { true } }
    }
}
struct Line { a: u256 }
fn scoped_core(a: Line, b: Line) -> bool { with (EqNothing) { a == b } }
"#,
    );
    let diagnostics = db.run_on_top_mod(db.top_mod(file)).format_diags(&db);
    assert!(diagnostics.contains("error[8-0016]"), "{diagnostics}");
    assert!(
        diagnostics.contains("a scoped derive supports traits of the same package; for a trait of another package, write a header `impl Trait for Type with (Provider)`"),
        "{diagnostics}"
    );
}

#[test]
fn an_error_in_written_code_is_not_reported_as_a_generation_failure() {
    let source = r#"use core::derive::Derive
use core::meta::{Record, Code, Item}
pub trait Arity { const fn arity() -> u256 }
pub struct Gen {}
impl Derive<Arity> for Gen {
    const fn derive(_ record: Record) -> Code<Item> {
        quote { const fn arity() -> u256 { 2 } }
    }
}
pub struct P {}
impl Arity for P with (Gen)
fn bad() -> u256 { true }
"#;
    let output = prepare(source).expect("an error in written code does not fail preparation");
    let url = Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap();
    let file = output.workspace().get(&output, &url).unwrap();
    let diagnostics = output
        .run_on_top_mod(output.top_mod(file))
        .format_diags(&output);
    assert!(
        diagnostics.contains("error[8-0000]: type mismatch"),
        "{diagnostics}"
    );
    assert!(
        !diagnostics.contains("generated output admission failed"),
        "{diagnostics}"
    );
}
