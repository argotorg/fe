//! FCO-M5: obligation-level discharge of `where`-clause const predicates.
//!
//! This is the end-to-end "platform fact" demo: a backend fact expressed as an
//! associated const (`B::WORD_BITS`) appears in a `where` predicate, becomes a
//! first-class obligation at the call site, and is discharged by CTFE under the
//! call's type substitution — never inside the trait solver. The satisfying
//! backend compiles and records evidence; the non-satisfying backend fails with
//! a named diagnostic; a generic caller that forwards its own type leaves the
//! predicate as its own assumption (no false discharge, no false error).

use common::diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics};
use fe_hir::analysis::ty::ty_check::{CheckPremise, DischargeRoute, check_func_body};
use fe_hir::hir_def::{Expr, Func, Partial, TopLevelMod, WhereClauseOwner};
use fe_hir::test_db::{HirAnalysisTestDb, initialize_analysis_pass};

/// A `Platform` trait carrying a backend fact (`WORD_BITS`), a satisfying
/// backend (`Evm`, 256) and a non-satisfying one (`Tiny`, 16), and a generic
/// function gated on the fact.
const PLATFORM: &str = r#"
trait Platform {
    const WORD_BITS: u256
}

struct Evm {}
struct Tiny {}

impl Platform for Evm {
    const WORD_BITS: u256 = 256
}

impl Platform for Tiny {
    const WORD_BITS: u256 = 16
}

fn word_op<B: Platform>() where B::WORD_BITS == 256 {
}
"#;

fn diagnostics_for<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<CompleteDiagnostic> {
    let mut manager = initialize_analysis_pass();
    let mut diags: Vec<_> = manager
        .run_on_module(db, top_mod)
        .into_iter()
        .map(|diag| diag.to_complete(db))
        .collect();
    diags.sort_by(cmp_complete_diagnostics);
    diags
}

fn func_named<'db>(db: &'db HirAnalysisTestDb, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| func.name(db).to_opt().is_some_and(|n| n.data(db) == name))
        .unwrap_or_else(|| panic!("missing `{name}` function"))
}

fn only_call_expr<'db>(db: &'db HirAnalysisTestDb, func: Func<'db>) -> fe_hir::hir_def::ExprId {
    let body = func.body(db).expect("function has a body");
    body.exprs(db)
        .iter()
        .find_map(|(expr, data)| matches!(data, Partial::Present(Expr::Call(..))).then_some(expr))
        .expect("body contains a call expression")
}

#[test]
fn platform_fact_discharges_for_satisfying_backend() {
    let mut db = HirAnalysisTestDb::default();
    let src = format!("{PLATFORM}\nfn caller() {{\n    word_op<Evm>()\n}}\n");
    let file = db.new_stand_alone("platform_fact_pass.fe".into(), &src);
    let (top_mod, _) = db.top_mod(file);

    // The satisfying program compiles clean.
    db.assert_no_diags(top_mod);

    // And it recorded const-predicate discharge evidence, keyed to the call,
    // discharged via the CTFE route with an (empty) premises slot.
    let caller = func_named(&db, top_mod, "caller");
    let call_expr = only_call_expr(&db, caller);
    let typed = &check_func_body(&db, caller).1;

    let records: Vec<_> = typed
        .discharged_const_predicates_for_call(call_expr)
        .collect();
    assert_eq!(
        records.len(),
        1,
        "expected exactly one const-predicate discharge for the call"
    );
    let record = records[0];
    assert_eq!(record.route, DischargeRoute::Ctfe);
    assert_eq!(record.generic_args.len(), 1, "B := Evm");
    assert!(
        record.premises.is_empty(),
        "the M5 CTFE route is premise-free; the premises slot must be empty"
    );
    assert_eq!(record.call_expr(), call_expr);
}

#[test]
fn platform_fact_fails_for_non_satisfying_backend() {
    let mut db = HirAnalysisTestDb::default();
    let src = format!("{PLATFORM}\nfn caller() {{\n    word_op<Tiny>()\n}}\n");
    let file = db.new_stand_alone("platform_fact_fail.fe".into(), &src);
    let (top_mod, _) = db.top_mod(file);

    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|d| d.error_code.to_string() == "8-0085"
            && d.message.contains("const predicate is not satisfied")),
        "expected the unsatisfied-predicate diagnostic, got: {diags:#?}"
    );

    // The failing call discharges *no* evidence — the program does not proceed
    // as if the fact held.
    let caller = func_named(&db, top_mod, "caller");
    let typed = &check_func_body(&db, caller).1;
    assert!(
        typed.discharged_const_predicates().is_empty(),
        "a refuted predicate must not record a discharge"
    );
}

#[test]
fn symbolic_forward_discharges_by_matching_assumption() {
    // A generic caller forwards its own type parameter `B`. The predicate
    // `B::WORD_BITS == 256` stays symbolic at this call, but `mid` carries the
    // identical predicate as its own `where`-clause assumption, so it discharges
    // by the Assumption route (term identity) — not CTFE — and does not error.
    let mut db = HirAnalysisTestDb::default();
    let src = format!(
        "{PLATFORM}\nfn mid<B: Platform>() where B::WORD_BITS == 256 {{\n    word_op<B>()\n}}\n"
    );
    let file = db.new_stand_alone("platform_fact_symbolic.fe".into(), &src);
    let (top_mod, _) = db.top_mod(file);

    db.assert_no_diags(top_mod);

    let mid = func_named(&db, top_mod, "mid");
    let typed = &check_func_body(&db, mid).1;
    let records = typed.discharged_const_predicates();
    assert_eq!(
        records.len(),
        1,
        "the forwarded predicate is discharged once"
    );
    assert_eq!(
        records[0].route,
        DischargeRoute::Assumption,
        "a symbolic forwarded predicate discharges by assumption, not CTFE"
    );
    // A3: the assumption route records the matched in-scope assumption as its
    // premise — the logical dependency of the discharge. Matching is by exact
    // identity, so the required and assumption terms are the same term, and the
    // premise's origin points at `mid`'s own `where`-clause predicate body (the
    // anchor a receipt resolves to "discharged by assumption at <span>").
    assert_eq!(records[0].premises.len(), 1, "one matched assumption");
    let CheckPremise::Assumption {
        required_term,
        assumption_term,
        assumption,
    } = records[0].premises[0];
    assert_eq!(
        required_term, assumption_term,
        "M5 matches assumptions by exact term identity"
    );
    let mid_where_pred = WhereClauseOwner::from(mid)
        .where_clause(&db)
        .const_predicates(&db)[0];
    assert_eq!(
        assumption, mid_where_pred,
        "the premise origin is the caller's own `where`-clause predicate body"
    );
}

#[test]
fn assumption_route_requires_an_assumption() {
    // A generic caller with *no* matching assumption cannot discharge the
    // symbolic predicate (CTFE cannot decide it either) — hard failure.
    let mut db = HirAnalysisTestDb::default();
    let src = format!("{PLATFORM}\nfn naked<B: Platform>() {{\n    word_op<B>()\n}}\n");
    let file = db.new_stand_alone("assumption_route_none.fe".into(), &src);
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|d| d.error_code.to_string() == "8-0085"),
        "{diags:#?}"
    );
}

/// Compiles `src` and returns the number of const-predicate discharges recorded
/// in function `func`.
fn discharges_in(name: &str, src: &str, func: &str) -> usize {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(format!("{name}.fe").into(), src);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    let func = func_named(&db, top_mod, func);
    check_func_body(&db, func)
        .1
        .discharged_const_predicates()
        .len()
}

/// Checks `src` and asserts at least one diagnostic carries `code`.
fn assert_has_code(name: &str, src: &str, code: &str) -> Vec<CompleteDiagnostic> {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(format!("{name}.fe").into(), src);
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert!(
        diags.iter().any(|d| d.error_code.to_string() == code),
        "expected diagnostic {code}, got: {diags:#?}"
    );
    diags
}

// Pair 1: the real demo shape carries `B::Word` as a parameter/return type
// alongside the const-predicate gate, proving associated-type projection in the
// signature coexists with const-predicate discharge.
#[test]
fn assoc_type_word_param_discharges() {
    let src = r#"
trait Platform {
    type Word
    const WORD_BITS: u256
}
struct Evm {}
impl Platform for Evm {
    type Word = u256
    const WORD_BITS: u256 = 256
}
fn word_op<B: Platform>(x: B::Word) -> B::Word where B::WORD_BITS == 256 {
    x
}
fn ok(x: Evm::Word) -> Evm::Word {
    word_op<Evm>(x)
}
"#;
    assert_eq!(discharges_in("assoc_type_word_param", src, "ok"), 1);
}

// Pair 3: a relational predicate discharges correctly at the boundary — `>=` is
// evaluated, not approximated.
const FIELD: &str = r#"
trait Field {
    const TWO_ADICITY: u256
}
fn needs_fft_domain<F: Field>() where F::TWO_ADICITY >= 16 {
}
"#;

#[test]
fn relational_predicate_discharges_at_boundary() {
    // 16 >= 16 holds.
    let pos = format!(
        "{FIELD}\nstruct GoodField {{}}\nimpl Field for GoodField {{\n    const TWO_ADICITY: u256 = 16\n}}\nfn ok() {{\n    needs_fft_domain<GoodField>()\n}}\n"
    );
    assert_eq!(discharges_in("relational_pos", &pos, "ok"), 1);

    // 15 >= 16 is refuted.
    let neg = format!(
        "{FIELD}\nstruct SmallField {{}}\nimpl Field for SmallField {{\n    const TWO_ADICITY: u256 = 15\n}}\nfn bad() {{\n    needs_fft_domain<SmallField>()\n}}\n"
    );
    assert_has_code("relational_neg", &neg, "8-0085");
}

// Pair 5: the discharge path evaluates a const *expression* over an associated
// const, not just a stored literal.
const WORD_BYTES: &str = r#"
trait Platform {
    const WORD_BYTES: u256
}
fn word_sized<B: Platform>() where B::WORD_BYTES * 8 == 256 {
}
"#;

#[test]
fn arithmetic_predicate_is_evaluated() {
    // 32 * 8 == 256 holds.
    let pos = format!(
        "{WORD_BYTES}\nstruct Evm {{}}\nimpl Platform for Evm {{\n    const WORD_BYTES: u256 = 32\n}}\nfn ok() {{\n    word_sized<Evm>()\n}}\n"
    );
    assert_eq!(discharges_in("arith_pos", &pos, "ok"), 1);

    // 31 * 8 != 256 is refuted.
    let neg = format!(
        "{WORD_BYTES}\nstruct Weird {{}}\nimpl Platform for Weird {{\n    const WORD_BYTES: u256 = 31\n}}\nfn bad() {{\n    word_sized<Weird>()\n}}\n"
    );
    assert_has_code("arith_neg", &neg, "8-0085");
}

// Pair 6: multiple const predicates on one callee are discharged individually
// (two evidence records) and diagnosed individually (the satisfied one still
// discharges even when its sibling is refuted).
const TWO_PREDS: &str = r#"
trait Platform {
    const WORD_BITS: u256
    const HAS_STORAGE: bool
}
fn evm_like_only<B: Platform>() where B::WORD_BITS == 256, B::HAS_STORAGE == true {
}
"#;

#[test]
fn multiple_predicates_discharge_and_diagnose_individually() {
    // Both hold -> two discharges.
    let pos = format!(
        "{TWO_PREDS}\nstruct Evm {{}}\nimpl Platform for Evm {{\n    const WORD_BITS: u256 = 256\n    const HAS_STORAGE: bool = true\n}}\nfn ok() {{\n    evm_like_only<Evm>()\n}}\n"
    );
    assert_eq!(discharges_in("two_preds_pos", &pos, "ok"), 2);

    // WORD_BITS fails, HAS_STORAGE holds -> one 8-0085, and the satisfied
    // predicate is still discharged (each obligation is independent).
    let mut db = HirAnalysisTestDb::default();
    let neg = format!(
        "{TWO_PREDS}\nstruct TinyStorage {{}}\nimpl Platform for TinyStorage {{\n    const WORD_BITS: u256 = 16\n    const HAS_STORAGE: bool = true\n}}\nfn bad() {{\n    evm_like_only<TinyStorage>()\n}}\n"
    );
    let file = db.new_stand_alone("two_preds_neg.fe".into(), &neg);
    let (top_mod, _) = db.top_mod(file);
    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(
        diags
            .iter()
            .filter(|d| d.error_code.to_string() == "8-0085")
            .count(),
        1,
        "exactly the word-bits predicate is refuted: {diags:#?}"
    );
    let bad = func_named(&db, top_mod, "bad");
    assert_eq!(
        check_func_body(&db, bad)
            .1
            .discharged_const_predicates()
            .len(),
        1,
        "the satisfied sibling predicate is still discharged"
    );
}

// Pair 7: discharge is keyed to the concrete substitution at the use site, not
// global by callee. The Evm call discharges; the Tiny call is refuted and
// records no discharge — the Evm success is never reused for Tiny.
#[test]
fn per_call_substitution_no_stale_evidence() {
    let mut db = HirAnalysisTestDb::default();
    let src = r#"
trait Platform {
    const WORD_BITS: u256
}
struct Evm {}
struct Tiny {}
impl Platform for Evm {
    const WORD_BITS: u256 = 256
}
impl Platform for Tiny {
    const WORD_BITS: u256 = 16
}
fn word_op<B: Platform>() where B::WORD_BITS == 256 {
}
fn ok() {
    word_op<Evm>()
}
fn bad() {
    word_op<Tiny>()
}
"#;
    let file = db.new_stand_alone("per_call.fe".into(), src);
    let (top_mod, _) = db.top_mod(file);

    let diags = diagnostics_for(&db, top_mod);
    assert_eq!(
        diags
            .iter()
            .filter(|d| d.error_code.to_string() == "8-0085")
            .count(),
        1,
        "only the Tiny call is refuted: {diags:#?}"
    );

    let ok = func_named(&db, top_mod, "ok");
    let ok_call = only_call_expr(&db, ok);
    let ok_typed = &check_func_body(&db, ok).1;
    let ok_recs: Vec<_> = ok_typed
        .discharged_const_predicates_for_call(ok_call)
        .collect();
    assert_eq!(ok_recs.len(), 1, "Evm call discharges");
    assert_eq!(ok_recs[0].route, DischargeRoute::Ctfe);

    let bad = func_named(&db, top_mod, "bad");
    assert!(
        check_func_body(&db, bad)
            .1
            .discharged_const_predicates()
            .is_empty(),
        "the refuted Tiny call records no discharge — Evm's success is not reused"
    );
}

// Pair 9: associated-const identity includes the trait/instantiation, not just
// the spelling `SIZE`. `X` impls both `A` (SIZE = 32) and `B` (SIZE = 64); a
// predicate bound through `T: A` must read A::SIZE.
const TWO_TRAITS: &str = r#"
trait A {
    const SIZE: u256
}
trait B {
    const SIZE: u256
}
struct X {}
impl A for X {
    const SIZE: u256 = 32
}
impl B for X {
    const SIZE: u256 = 64
}
"#;

#[test]
fn assoc_const_identity_includes_the_trait() {
    // T: A reads A::SIZE = 32; T: B reads B::SIZE = 64. Both discharge.
    let pos = format!(
        "{TWO_TRAITS}\nfn needs_a<T: A>() where T::SIZE == 32 {{\n}}\nfn needs_b<T: B>() where T::SIZE == 64 {{\n}}\nfn ok() {{\n    needs_a<X>()\n    needs_b<X>()\n}}\n"
    );
    assert_eq!(discharges_in("two_traits_pos", &pos, "ok"), 2);

    // Bound through `T: A` but asserting 64 must fail — A::SIZE is 32, not the
    // sibling B::SIZE = 64.
    let neg = format!(
        "{TWO_TRAITS}\nfn needs_a_wrong<T: A>() where T::SIZE == 64 {{\n}}\nfn bad() {{\n    needs_a_wrong<X>()\n}}\n"
    );
    assert_has_code("two_traits_neg", &neg, "8-0085");
}

// Pair 10: a predicate over an associated const whose trait source is not in
// scope is a *formation/resolution* failure (the const is unavailable), not a
// silently-accepted predicate, an 8-0085 discharge failure, or an ICE.
#[test]
fn formation_requires_available_assoc_const() {
    let src = r#"
fn bad<T>() where T::SIZE >= 50 {
}
"#;
    let diags = assert_has_code("formation_unavailable", src, "2-0002");
    assert!(
        !diags.iter().any(|d| d.error_code.to_string() == "8-0085"),
        "an unavailable assoc const is a formation error, not a discharge failure"
    );
}

// Pair 11: a deeper chained projection (`B::Memory::ADDRESS_SPACE`) is rejected
// with a named diagnostic and never ICEs or silently mislowers. (Simple
// single-segment projections are exercised by the passing demos above.)
#[test]
fn chained_projection_fails_safely() {
    let src = r#"
trait MemoryKind {
    const ADDRESS_SPACE: u256
}
trait Platform {
    type Memory
}
fn needs_linear<B: Platform>() where B::Memory::ADDRESS_SPACE == 1 {
}
"#;
    assert_has_code("chained_projection", src, "2-0002");
}

// Pair 12: a predicate whose evaluation faults (division by zero) is a hard
// error (3-0025), never an 8-0085 "not satisfied" and never a silently removed
// candidate (anti-SFINAE).
const DIVISOR: &str = r#"
trait Divisor {
    const DENOM: u256
}
fn divides_cleanly<T: Divisor>() where 100 / T::DENOM == 10 {
}
"#;

#[test]
fn ctfe_fault_is_hard_error_not_sfinae() {
    // 100 / 10 == 10 holds.
    let pos = format!(
        "{DIVISOR}\nstruct Ten {{}}\nimpl Divisor for Ten {{\n    const DENOM: u256 = 10\n}}\nfn ok() {{\n    divides_cleanly<Ten>()\n}}\n"
    );
    assert_eq!(discharges_in("divisor_pos", &pos, "ok"), 1);

    // 100 / 0 faults — hard error, not a discharge failure.
    let neg = format!(
        "{DIVISOR}\nstruct Zero {{}}\nimpl Divisor for Zero {{\n    const DENOM: u256 = 0\n}}\nfn bad() {{\n    divides_cleanly<Zero>()\n}}\n"
    );
    let diags = assert_has_code("divisor_neg", &neg, "3-0025");
    assert!(
        !diags.iter().any(|d| d.error_code.to_string() == "8-0085"),
        "a CTFE fault is a hard error, not an unsatisfied-predicate result"
    );
}

// Gate 7: a named `const fn` call is a valid predicate, evaluated by CTFE at
// concrete sites (`where fits(LEN, CAP)`). Term identity keys on the callee, so
// a same-bodied "foreign twin" function does not match in the assumption route.
const FITS: &str = r#"
const fn fits(_ len: u256, _ cap: u256) -> bool {
    len <= cap
}
const fn twin(_ len: u256, _ cap: u256) -> bool {
    len <= cap
}
fn store<const LEN: u256, const CAP: u256>() where fits(LEN, CAP) {
}
"#;

#[test]
fn named_const_fn_bound_discharges_by_ctfe() {
    // fits(3, 8) holds and records a CTFE discharge.
    assert_eq!(
        discharges_in(
            "fits_ok",
            &format!("{FITS}\nfn ok() {{\n    store<3, 8>()\n}}\n"),
            "ok",
        ),
        1,
    );
    // fits(8, 3) is refuted.
    assert_has_code(
        "fits_bad",
        &format!("{FITS}\nfn bad() {{\n    store<8, 3>()\n}}\n"),
        "8-0085",
    );
}

#[test]
fn named_const_fn_bound_foreign_twin_does_not_match() {
    // The caller proves `twin(LEN, CAP)`; the callee requires `fits(LEN, CAP)`.
    // Same body, different callee — the `App` terms differ, so the symbolic
    // forward is not discharged by assumption.
    let src = format!(
        "{FITS}\nfn fwd<const LEN: u256, const CAP: u256>() where twin(LEN, CAP) {{\n    store<LEN, CAP>()\n}}\n"
    );
    assert_has_code("fits_twin", &src, "8-0085");
}

// Pair 8 (negative): a generic caller forwarding its own type with a
// *mismatched* assumption (`B::WORD_BITS == 128` calling a callee that requires
// `== 256`) is rejected by exact term identity — no implication, no fuzzy
// matching, no direction flipping, no boolean splitting.
#[test]
fn assumption_route_mismatch_is_rejected() {
    let src = r#"
trait Platform {
    const WORD_BITS: u256
}
fn word_op<B: Platform>() where B::WORD_BITS == 256 {
}
fn bad_forward<B: Platform>() where B::WORD_BITS == 128 {
    word_op<B>()
}
"#;
    assert_has_code("assumption_mismatch", src, "8-0085");
}

// A12: the assumption matcher is *exact* — it does not flip relation direction,
// reason by implication, or split conjunctions. A trait carrying a relational
// and a boolean fact, plus a callee that demands a specific shape.
const RELATIONAL: &str = r#"
trait HasSize {
    const SIZE: u256
    const DYN: bool
}
fn needs_ge16<T: HasSize>() where T::SIZE >= 16 {
}
fn needs_gt0<T: HasSize>() where T::SIZE > 0 {
}
fn needs_both<T: HasSize>() where T::SIZE == 16 && T::DYN == false {
}
"#;

#[test]
fn assumption_route_does_not_flip_direction() {
    // Caller writes the logically-equivalent flip `16 <= T::SIZE`; it must NOT
    // discharge the callee's `T::SIZE >= 16` (distinct relations stay distinct).
    let src = format!(
        "{RELATIONAL}\nfn fwd<T: HasSize>() where 16 <= T::SIZE {{\n    needs_ge16<T>()\n}}\n"
    );
    assert_has_code("assumption_flip", &src, "8-0085");
}

#[test]
fn assumption_route_does_not_imply() {
    // `T::SIZE >= 1` does not discharge `T::SIZE > 0`, even though it implies it.
    let src = format!(
        "{RELATIONAL}\nfn fwd<T: HasSize>() where T::SIZE >= 1 {{\n    needs_gt0<T>()\n}}\n"
    );
    assert_has_code("assumption_imply", &src, "8-0085");
}

#[test]
fn assumption_route_does_not_split_conjunctions() {
    // The caller proves only one conjunct; the callee's `A && B` is one term and
    // is not satisfied by a single-conjunct assumption.
    let src = format!(
        "{RELATIONAL}\nfn fwd<T: HasSize>() where T::SIZE == 16 {{\n    needs_both<T>()\n}}\n"
    );
    assert_has_code("assumption_split", &src, "8-0085");
}

// ---------------------------------------------------------------------------
// Gate 1 + 3: const predicates are first-class at every well-formedness
// position, not only at call sites. A concrete ADT whose own `where`-clause
// const predicate is refuted is ill-formed wherever it appears, via the shared
// `check_ty_wf`; the generic declaration itself is still accepted.
// ---------------------------------------------------------------------------

/// `Bounded<MIN, MAX>` is well-formed only when `MIN <= MAX`.
const BOUNDED: &str = r#"
struct Bounded<const MIN: u256, const MAX: u256> where MIN <= MAX {
    value: u256
}
"#;

fn assert_compiles(name: &str, src: &str) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(format!("{name}.fe").into(), src);
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn wf_const_predicate_declaration_is_accepted() {
    // The generic declaration is not refuted — only concrete instantiations are.
    assert_compiles("bounded_decl", BOUNDED);
}

#[test]
fn wf_const_predicate_construction() {
    // Satisfying instantiation constructs; refuted instantiation is rejected.
    assert_compiles(
        "bounded_construct_ok",
        &format!("{BOUNDED}\nfn m() {{\n    Bounded<1, 4> {{ value: 0 }}\n}}\n"),
    );
    assert_has_code(
        "bounded_construct_bad",
        &format!("{BOUNDED}\nfn m() {{\n    Bounded<4, 1> {{ value: 0 }}\n}}\n"),
        "8-0085",
    );
}

#[test]
fn wf_const_predicate_local_binding() {
    // A refuted ADT in a local binding is rejected without any call site.
    assert_has_code(
        "bounded_local_bad",
        &format!("{BOUNDED}\nfn m() {{\n    let _x = Bounded<4, 1> {{ value: 0 }}\n}}\n"),
        "8-0085",
    );
}

#[test]
fn wf_const_predicate_signature_param_never_called() {
    // A refuted ADT in a parameter type is rejected at the declaration even
    // though the function is never called.
    assert_has_code(
        "bounded_param_bad",
        &format!("{BOUNDED}\nfn impossible(x: Bounded<4, 1>) {{\n}}\n"),
        "8-0085",
    );
}

#[test]
fn wf_const_predicate_signature_return_never_called() {
    // A refuted ADT in the return type is rejected at the declaration.
    let diags = assert_has_code(
        "bounded_return_bad",
        &format!(
            "{BOUNDED}\nextern {{\n    fn diverge() -> !\n}}\nfn impossible() -> Bounded<4, 1> {{\n    diverge()\n}}\n"
        ),
        "8-0085",
    );
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("const predicate is not satisfied")),
        "{diags:#?}"
    );
}

#[test]
fn wf_const_predicate_struct_field_no_body() {
    // A refuted ADT in a struct field is rejected with no function body in
    // sight — this is the position a body-only check would miss.
    assert_has_code(
        "bounded_field_bad",
        &format!("{BOUNDED}\nstruct Holder {{\n    x: Bounded<4, 1>\n}}\n"),
        "8-0085",
    );
}

// Gate D: a selected impl's own `where`-clause const predicates gate it
// (gate-not-select). Coherence already rejects impls differing only by a const
// predicate; here the *selected* impl's residual is discharged after selection.
const IMPL_GATED: &str = r#"
trait Big {}
struct Wrap<const N: u256> {}
impl<const N: u256> Big for Wrap<N> where N >= 50 {}
fn needs<T: Big>(_ t: T) {}
"#;

#[test]
fn impl_const_predicate_gates_via_trait_bound() {
    // Satisfying: Wrap<64> selects the impl and 64 >= 50 holds.
    assert_compiles(
        "impl_gate_ok",
        &format!("{IMPL_GATED}\nfn ok(w: Wrap<64>) {{\n    needs(w)\n}}\n"),
    );
    // Violating: Wrap<10> selects the same impl, but 10 >= 50 is refuted — the
    // selected impl's residual const predicate is gated, not silently accepted.
    assert_has_code(
        "impl_gate_bad",
        &format!("{IMPL_GATED}\nfn bad(w: Wrap<10>) {{\n    needs(w)\n}}\n"),
        "8-0085",
    );
}

#[test]
fn impls_differing_only_by_const_predicate_overlap() {
    // Gate-not-select for coherence: const predicates do not discriminate
    // candidates, so two impls differing only by one are an overlap error.
    assert_has_code(
        "impl_overlap",
        r#"
trait M {}
struct Wrap<const N: u256> {}
impl<const N: u256> M for Wrap<N> where N >= 50 {}
impl<const N: u256> M for Wrap<N> where N < 50 {}
"#,
        "5-0001",
    );
}

// Gate-2 tail: a concrete method call commits to an impl just like a
// trait-bound call, so the selected impl's residual const predicate is gated
// through the same obligation → gate-not-select path. `Wrap<10>.big()` selects
// `impl Big for Wrap<N> where N >= 50`; the gate refutes `10 >= 50`.
#[test]
fn impl_const_predicate_gates_method_call() {
    let src = r#"
trait Big {
    fn big(self) -> u256
}
struct Wrap<const N: u256> {}
impl<const N: u256> Big for Wrap<N> where N >= 50 {
    fn big(self) -> u256 {
        0
    }
}
fn bad(w: Wrap<10>) -> u256 {
    w.big()
}
"#;
    assert_has_code("impl_gate_method", src, "8-0085");
}

// The satisfying receiver compiles: the selected impl's const predicate holds.
#[test]
fn impl_const_predicate_method_call_satisfied_compiles() {
    assert_compiles(
        "impl_gate_method_ok",
        r#"
trait Big {
    fn big(self) -> u256
}
struct Wrap<const N: u256> {}
impl<const N: u256> Big for Wrap<N> where N >= 50 {
    fn big(self) -> u256 {
        0
    }
}
fn good(w: Wrap<50>) -> u256 {
    w.big()
}
"#,
    );
}

// A symbolic receiver in a generic context must NOT be CTFE-gated at the method
// call: `T`'s predicate is the enclosing item's own assumption (here forwarded
// by `where N >= 50`), discharged by identity, not refuted.
#[test]
fn impl_const_predicate_method_call_symbolic_not_refuted() {
    assert_compiles(
        "impl_gate_method_symbolic",
        r#"
trait Big {
    fn big(self) -> u256
}
struct Wrap<const N: u256> {}
impl<const N: u256> Big for Wrap<N> where N >= 50 {
    fn big(self) -> u256 {
        0
    }
}
fn forward<const N: u256>(w: Wrap<N>) -> u256 where N >= 50 {
    w.big()
}
"#,
    );
}

#[test]
fn wf_const_predicate_symbolic_generic_not_refuted() {
    // A symbolic application `Bounded<A, B>` under a matching assumption is the
    // enclosing item's own assumption; it must not be CTFE-evaluated or falsely
    // rejected.
    assert_compiles(
        "bounded_generic_ok",
        &format!(
            "{BOUNDED}\nfn g<const A: u256, const B: u256>(x: Bounded<A, B>) where A <= B {{\n}}\n"
        ),
    );
}

// =====================================================================
// M0: method const-predicate conformance.
//
// An impl method's `where`-clause const predicates must match the trait
// method's *exactly* (by normalized term identity) — not by logical
// implication. This mirrors the obligation discharge path: a caller's
// assumption discharges a callee predicate only by identity, so if the impl
// could publish one predicate to trait-routed callers while its body relied
// on another, the discharge guarantee would break. Reported as `6-0016`
// (`ImplDiag::MethodConstPredicateMismatch`).
// =====================================================================

#[test]
fn method_const_predicate_exact_match_conforms() {
    assert_compiles(
        "m0_exact",
        r#"
trait HasOp {
    fn op<const N: u256>() where N >= 50
}
struct X {}
impl HasOp for X {
    fn op<const N: u256>() where N >= 50 {
    }
}
"#,
    );
}

#[test]
fn method_const_predicate_stronger_is_rejected() {
    // Stronger is unsound for trait-routed callers (they only guarantee the
    // trait's predicate) — and, regardless, fails the exact-identity rule.
    assert_has_code(
        "m0_stronger",
        r#"
trait HasOp {
    fn op<const N: u256>() where N >= 50
}
struct X {}
impl HasOp for X {
    fn op<const N: u256>() where N >= 100 {
    }
}
"#,
        "6-0016",
    );
}

#[test]
fn method_const_predicate_weaker_is_rejected() {
    // Even a logically *weaker* predicate is rejected: matching is by identity,
    // not implication (no `>=` entailment reasoning).
    assert_has_code(
        "m0_weaker",
        r#"
trait HasOp {
    fn op<const N: u256>() where N >= 50
}
struct X {}
impl HasOp for X {
    fn op<const N: u256>() where N >= 10 {
    }
}
"#,
        "6-0016",
    );
}

#[test]
fn method_const_predicate_dropped_is_rejected() {
    // Dropping the predicate entirely is a mismatch.
    assert_has_code(
        "m0_dropped",
        r#"
trait HasOp {
    fn op<const N: u256>() where N >= 50
}
struct X {}
impl HasOp for X {
    fn op<const N: u256>() {
    }
}
"#,
        "6-0016",
    );
}

#[test]
fn method_const_predicate_added_is_rejected() {
    // Adding a predicate the trait does not declare is a mismatch.
    assert_has_code(
        "m0_added",
        r#"
trait HasOp {
    fn op<const N: u256>()
}
struct X {}
impl HasOp for X {
    fn op<const N: u256>() where N >= 50 {
    }
}
"#,
        "6-0016",
    );
}

#[test]
fn method_const_predicate_relation_flip_is_rejected() {
    // `N >= 50` and `50 <= N` are logically equivalent but distinct terms:
    // exact identity, no direction flipping (FV11/A1). The flipped form is a
    // mismatch.
    assert_has_code(
        "m0_flip",
        r#"
trait HasOp {
    fn op<const N: u256>() where N >= 50
}
struct X {}
impl HasOp for X {
    fn op<const N: u256>() where 50 <= N {
    }
}
"#,
        "6-0016",
    );
}

#[test]
fn method_const_predicate_order_independent() {
    // The predicates form a set: declaration order does not matter.
    assert_compiles(
        "m0_order",
        r#"
trait HasOp {
    fn op<const N: u256>() where N >= 50, N <= 100
}
struct X {}
impl HasOp for X {
    fn op<const N: u256>() where N <= 100, N >= 50 {
    }
}
"#,
    );
}

#[test]
fn method_const_predicate_assoc_const_self_conforms() {
    // `Self::SIZE` in the trait method is over the trait's `Self`; the impl's
    // is over the concrete type. Rebasing the trait predicate through the
    // trait→impl substitution makes the two assoc-const terms identical.
    assert_compiles(
        "m0_assoc_ok",
        r#"
trait HasSize {
    const SIZE: u256
    fn check() where Self::SIZE == 8
}
struct X {}
impl HasSize for X {
    const SIZE: u256 = 8
    fn check() where Self::SIZE == 8 {
    }
}
"#,
    );
}

#[test]
fn method_const_predicate_assoc_const_self_mismatch_is_rejected() {
    assert_has_code(
        "m0_assoc_bad",
        r#"
trait HasSize {
    const SIZE: u256
    fn check() where Self::SIZE == 8
}
struct X {}
impl HasSize for X {
    const SIZE: u256 = 16
    fn check() where Self::SIZE == 16 {
    }
}
"#,
        "6-0016",
    );
}
