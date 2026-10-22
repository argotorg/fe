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

fn eval_const_fn_u256(db: &DriverDataBase, fn_name: &str) -> String {
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
        panic!("expected Ready for `{fn_name}`, got {outcome:?}");
    };
    let SemConstValue::Scalar {
        value: SemConstScalar::Int { value },
        ..
    } = value.value(db)
    else {
        panic!("expected Int scalar for `{fn_name}`");
    };
    value.to_string()
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

    // Verify single-pass authored-only input rule (RD117) and deduplication:
    // Two FieldCountScore openings each bind Score and ItemScore; the enum opening binds Score. Repeated expressions within one opening share its plan.
    let module = output.top_mod(
        output
            .workspace()
            .get(
                &output,
                &Url::parse("file:///tmp/scoped-on-demand-derive.fe").unwrap(),
            )
            .unwrap(),
    );
    let generated = module
        .all_impl_traits(&output)
        .iter()
        .filter(|implementation| implementation.selection_name(&output).is_some())
        .count();
    assert_eq!(
        generated, 5,
        "each distinct scoped demand has one named implementation"
    );
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
    let err = prepare_err(outside_block);
    assert!(err.contains("generated output admission failed"), "{err}");

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
    let callee_err = prepare_err(callee_body);
    assert!(
        callee_err.contains("generated output admission failed"),
        "{callee_err}"
    );
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
fn same_header_ambiguity_and_canonical_trait_refuse_before_provider_ctfe() {
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

    let canonical_scoped = r#"
use core::derive::Derive
use core::meta::{Record, Code, Item}

#[canonical]
trait CanonicalScore {
    const fn score() -> u256
}

pub struct Pair {}

struct Provider {}
impl Derive<CanonicalScore> for Provider {
    const fn derive(_ record: Record) -> Code<Item> {
        let zero: u256 = 0
        let fault = 1 / zero
        quote {
            const fn score() -> u256 { ${fault} }
        }
    }
}

const fn forbidden() -> u256 {
    with (Provider) {
        Pair::score()
    }
}
"#;
    let rendered3 = prepare_err(canonical_scoped);
    assert!(
        rendered3.contains(
            "scoped derive of canonical or library goals requires a separate authority API"
        ) && !rendered3.contains("division by zero"),
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
        err2.contains("scoped derive provider admission failed")
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
        err3.contains("scoped derive target admission failed")
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
        err4.contains("generated output admission failed") && !err4.contains("division by zero"),
        "{err4}"
    );
}

#[test]
fn written_const_inside_with_provider_is_unsupplied_not_ordinary_fallback() {
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
    let err = prepare_err(source);
    assert!(err.contains("generated output admission failed"), "{err}");
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
    let (diags, _) = hir::analysis::ty::ty_check::check_func_body(&db, func);
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
use core::derive::Derive
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

const fn gen_fn(_ record: Record) -> Code<Item> {
    quote {
        const fn late_caller() -> u256 {
            with (Provider) {
                LateTarget::score()
            }
        }
    }
}

#[generate(gen_fn)]
pub struct Trigger {
    pub seed: Seed,
}
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
    let selection_names = top_mod
        .all_impl_traits(&output)
        .iter()
        .filter_map(|impl_trait| impl_trait.selection_name(&output))
        .map(|ident| ident.data(&output).clone())
        .collect::<Vec<_>>();
    assert!(
        selection_names
            .iter()
            .any(|name| name.contains("InContractRecv")),
        "{selection_names:?}"
    );
    // Holder::C = 101, <Holder as HasConst>::TC = 102 -> 203
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

#[test]
fn scoped_derive_preserves_private_producer_helper_and_refuses_generic_targets() {
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
    for target in ["Box<u8>", "Box<u256>"] {
        let generic = source
            .replace("struct Pair {}", "struct Box<T> { value: T }")
            .replace("Pair::value()", &format!("<{target} as Value>::value()"));
        let error = prepare_err(&generic);
        assert!(
            error.contains(
                "scoped on-demand derive for generic record targets is not supported yet"
            ),
            "{target}: {error}"
        );
    }
}

#[test]
fn scoped_derive_refuses_generic_provider_aliases_before_materialization() {
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
    with (P8) { Pair::value() } + with (P256) { Pair::value() }
}
"#;
    let error = prepare_err(source);
    assert!(
        error.contains(
            "scoped on-demand derive for generic provider applications is not supported yet"
        ),
        "{error}"
    );
    // A nongeneric alias still denotes a valid ordinary provider.
    let positive = source
        .replace("struct Provider<T> {}", "struct Provider {}")
        .replace("Provider<u8>", "Provider");
    let positive = positive[..positive
        .find("impl Derive<Value> for Provider<u256>")
        .unwrap()]
        .to_string()
        + "type P = Provider\nconst fn observe() -> u256 { with (P) { Pair::value() } }\n";
    let output = prepare(&positive).unwrap();
    assert_eq!(eval_const_fn_u256(&output, "observe"), "1");
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

#[test]
fn provider_dependency_is_prepared_before_its_body_executes() {
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
    )
    .expect("provider dependencies should prepare before evaluation");
    assert_eq!(eval_const_fn_u256(&output, "run"), "42");
}

#[test]
fn cyclic_provider_dependency_refuses_before_execution() {
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
    assert!(cause.contains("cannot make progress"), "{cause}");
    assert!(!cause.contains("division by zero"), "{cause}");
}

#[test]
fn invalid_generated_body_refuses_before_dependent_provider_execution() {
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
    assert!(cause.contains("missing_generated_value"), "{cause}");
    assert!(!cause.contains("division by zero"), "{cause}");
}
