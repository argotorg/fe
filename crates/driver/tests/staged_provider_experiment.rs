//! Executable staging experiment, not a public provider or Builder API.
//!
//! Only owned source bytes cross database boundaries. Ordinary analysis gates
//! both CTFE and consumption. The receipt is test-side provenance, not a new
//! HIR origin or a claim of hygiene. The byte protocol is deliberately local.

use std::ops::Range;

use common::{
    InputDb,
    diagnostics::Severity,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::DriverDataBase;
use hir::{
    analysis::{
        semantic::{SemConstScalar, SemConstValue, eval_body_owner_const_with_args},
        ty::{ty_check::BodyOwner, ty_def::TyId},
    },
    hir_def::{Func, ItemKind, WhereClauseOwner},
    span::LazySpan,
};
use salsa::Setter;
use url::Url;

fn database() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    db
}

fn input(db: &mut DriverDataBase, name: &str, source: &str) -> File {
    // Virtual inputs, no files are written here.
    let url = Url::parse(&format!("file:///staged-provider-experiment/{name}.fe")).unwrap();
    db.workspace().touch(db, url, Some(source.to_owned()))
}

fn named<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Func<'db> {
    db.top_mod(file)
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| func.name(db).to_opt().is_some_and(|id| id.data(db) == name))
        .unwrap_or_else(|| panic!("missing function {name}"))
}

fn errors(db: &DriverDataBase, file: File) -> Option<String> {
    let diagnostics = db.run_on_top_mod(db.top_mod(file));
    if diagnostics.has_errors(db) {
        return Some(diagnostics.format_diags(db));
    }
    let diagnostics = db.mir_diagnostics_for_top_mod(db.top_mod(file));
    diagnostics
        .iter()
        .any(|diag| diag.severity == Severity::Error)
        .then(|| db.format_complete_diagnostics(&diagnostics))
}

#[derive(Debug, PartialEq, Eq)]
enum Failure {
    Provider(String),
    Protocol,
    Execution(String),
    Output(String),
    ItemCount,
    Consumer(String),
    OutputLimit,
}

#[derive(Debug, PartialEq, Eq)]
struct Receipt {
    provider_url: Url,
    provider_source: String,
    provider_span: Range<usize>,
    // This experiment accepts only a monomorphic, nullary provider.
    invocation: usize,
    output_source: String,
}

#[derive(Default)]
struct Request {
    bytes: usize,
    invocations: usize,
    ctfe_requests: usize,
}

impl Request {
    fn emit(
        &mut self,
        db: &DriverDataBase,
        provider: Func<'_>,
        byte_limit: usize,
    ) -> Result<Receipt, Failure> {
        let file = provider.span().resolve(db).unwrap().file;
        if let Some(message) = errors(db, file) {
            return Err(Failure::Provider(message));
        }
        if !provider.is_const(db)
            || !db
                .top_mod(file)
                .children_non_nested(db)
                .any(|item| item == ItemKind::Func(provider))
            || provider.params(db).next().is_some()
            || provider.has_effects(db)
            || WhereClauseOwner::Func(provider)
                .clause(db)
                .predicates(db)
                .next()
                .is_some()
            || !provider.as_callable(db).unwrap().params(db).is_empty()
        {
            return Err(Failure::Protocol);
        }
        let ty = provider.return_ty(db);
        let Some(len) = ty.array_len(db) else {
            return Err(Failure::Protocol);
        };
        if ty.generic_args(db).first() != Some(&TyId::u8(db)) {
            return Err(Failure::Protocol);
        }
        // Charge the logical output for every requested invocation, including
        // cached CTFE results. Check before execution and byte materialization.
        // This is an output limit, not a bound on type checking or CTFE memory.
        let total = self.bytes.checked_add(len).ok_or(Failure::OutputLimit)?;
        if total > byte_limit {
            return Err(Failure::OutputLimit);
        }
        self.bytes = total;
        let invocation = self.invocations;
        self.invocations += 1;
        self.ctfe_requests += 1;
        let value = eval_body_owner_const_with_args(db, BodyOwner::Func(provider), vec![], vec![])
            .map_err(|error| Failure::Execution(format!("{error:?}")))?;
        let SemConstValue::Array { elems, .. } = value.value(db) else {
            return Err(Failure::Protocol);
        };
        assert_eq!(elems.len(), len);
        let bytes = elems
            .iter()
            .map(|elem| match elem.value(db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } => value
                    .to_string()
                    .parse::<u8>()
                    .map_err(|_| Failure::Protocol),
                _ => Err(Failure::Protocol),
            })
            .collect::<Result<Vec<_>, _>>()?;
        let output_source = String::from_utf8(bytes).map_err(|_| Failure::Protocol)?;
        Ok(Receipt {
            provider_url: file.url(db).unwrap(),
            provider_source: file.text(db).clone(),
            provider_span: provider.span().resolve(db).unwrap().range.into(),
            invocation,
            output_source,
        })
    }
}

fn consume(receipt: &Receipt, consumer: &str) -> Result<(String, Range<usize>), Failure> {
    let mut db = database();
    let file = input(&mut db, "output", &receipt.output_source);
    if let Some(message) = errors(&db, file) {
        return Err(Failure::Output(message));
    }
    let items = db
        .top_mod(file)
        .children_non_nested(&db)
        .filter(|item| !matches!(item, ItemKind::Use(use_) if use_.is_synthetic_use(&db)))
        .collect::<Vec<_>>();
    let [ItemKind::Func(generated)] = items.as_slice() else {
        return Err(Failure::ItemCount);
    };
    let output_span = generated.span().resolve(&db).unwrap().range.into();
    // Re-resolve everything after this input revision; do not retain item IDs
    // from the old graph or the provider database as cross-stage identities.
    file.set_text(&mut db)
        .to(format!("{}\n{consumer}", receipt.output_source));
    if let Some(message) = errors(&db, file) {
        return Err(Failure::Consumer(message));
    }
    let caller = named(&db, file, "consume");
    let value = eval_body_owner_const_with_args(&db, BodyOwner::Func(caller), vec![], vec![])
        .map_err(|error| Failure::Execution(format!("{error:?}")))?;
    Ok((value.pretty_print(&db), output_span))
}

fn provider_bytes(bytes: &[u8]) -> String {
    let values = bytes
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!("const fn emit() -> [u8; {}] {{ [{values}] }}", bytes.len())
}

fn provider(source: &str) -> String {
    provider_bytes(source.as_bytes())
}

const OUTPUT: &str = "const fn generated(value: u256) -> u256 { value + 1 }";
const CALLER: &str = "const fn consume() -> u256 { generated(value: 41) }";

#[test]
fn checked_provider_output_and_real_consumer() {
    let mut db = database();
    let source = provider(OUTPUT);
    let file = input(&mut db, "provider", &source);
    let receipt = Request::default()
        .emit(&db, named(&db, file, "emit"), OUTPUT.len())
        .unwrap();
    assert_eq!(receipt.output_source, OUTPUT);
    assert_eq!(
        &receipt.provider_source[receipt.provider_span.clone()],
        source
    );
    assert_eq!(receipt.provider_url, file.url(&db).unwrap());
    let (value, span) = consume(&receipt, CALLER).unwrap();
    assert_eq!(value, "42");
    assert_eq!(&receipt.output_source[span], OUTPUT);
    assert!(
        matches!(consume(&receipt, "const fn consume() -> u256 { generated(wrong: 41) }"),
        Err(Failure::Consumer(message)) if message.contains("argument label mismatch"))
    );
}

#[test]
fn provider_errors_are_rejected_before_ctfe_even_in_untaken_code() {
    let good = provider(OUTPUT);
    for (source, expected) in [
        (
            good.replacen("{", "{ if false { let bad: bool = 7 }", 1),
            "type mismatch",
        ),
        (
            "fn runtime() -> u8 { 1 }\nconst fn emit() -> [u8; 1] { [runtime()] }".to_owned(),
            "non-const call",
        ),
        (good.replace("-> [u8;", "-> [bool;"), "type mismatch"),
        (
            good.replacen(" {", " uses (value: u256) {", 1),
            "effects are not allowed",
        ),
    ] {
        let mut db = database();
        let file = input(&mut db, "provider", &source);
        let mut request = Request::default();
        let result = request.emit(&db, named(&db, file, "emit"), 1024);
        assert!(
            matches!(result, Err(Failure::Provider(ref message)) if message.contains(expected)),
            "{result:?}"
        );
        assert_eq!(request.ctfe_requests, 0);
    }
}

#[test]
fn provider_protocol_is_checked_by_semantic_type_and_constness() {
    for source in [
        "const fn emit() -> bool { true }".to_owned(),
        "const fn emit() -> [u256; 1] { [1] }".to_owned(),
        provider(OUTPUT).replacen("const fn", "fn", 1),
        provider(OUTPUT).replacen("emit()", "emit<T>()", 1),
        provider(OUTPUT).replacen("emit()", "emit(value: u256)", 1),
    ] {
        let mut db = database();
        let file = input(&mut db, "provider", &source);
        let mut request = Request::default();
        assert_eq!(
            request.emit(&db, named(&db, file, "emit"), 1024),
            Err(Failure::Protocol)
        );
        assert_eq!(request.ctfe_requests, 0);
    }
}

#[test]
fn malformed_wrong_and_extra_outputs_do_not_reach_consumption() {
    for (source, extra) in [
        ("const fn generated(", false),
        ("const fn generated() -> bool { 7 }", false),
        ("const fn generated() -> u256 { 7 }\nfn extra() {}", true),
    ] {
        let mut db = database();
        let file = input(&mut db, "provider", &provider(source));
        let receipt = Request::default()
            .emit(&db, named(&db, file, "emit"), 1024)
            .unwrap();
        let result = consume(&receipt, CALLER);
        if extra {
            assert_eq!(result, Err(Failure::ItemCount));
        } else {
            assert!(matches!(result, Err(Failure::Output(_))), "{result:?}");
        }
    }
    let mut db = database();
    let file = input(&mut db, "provider", &provider_bytes(&[255]));
    assert_eq!(
        Request::default().emit(&db, named(&db, file, "emit"), 1),
        Err(Failure::Protocol)
    );
}

#[test]
fn stage_visibility_does_not_leak_in_either_direction() {
    let mut db = database();
    let file = input(
        &mut db,
        "provider",
        "const fn emit() -> [u8; 1] { [generated()] }",
    );
    assert!(matches!(
        Request::default().emit(&db, named(&db, file, "emit"), 1024),
        Err(Failure::Provider(_))
    ));

    let output = "const fn generated() -> u256 { hidden() }";
    let source = format!("const fn hidden() -> u256 {{ 42 }}\n{}", provider(output));
    file.set_text(&mut db).to(source);
    let receipt = Request::default()
        .emit(&db, named(&db, file, "emit"), 1024)
        .unwrap();
    assert!(matches!(consume(&receipt, CALLER), Err(Failure::Output(_))));
}

#[test]
fn warm_queries_and_provider_edits_match_fresh_databases() {
    let mut db = database();
    let file = input(&mut db, "provider", &provider(OUTPUT));
    for increment in [1, 2, 1] {
        let output = OUTPUT.replace("+ 1", &format!("+ {increment}"));
        let source = provider(&output);
        file.set_text(&mut db).to(source.clone());
        let func = named(&db, file, "emit");
        // Warm a semantic query before ordinary analysis in this ordering.
        assert!(func.return_ty(&db).is_array(&db));
        let receipt = Request::default().emit(&db, func, 1024).unwrap();
        let mut fresh = database();
        let fresh_file = input(&mut fresh, "provider", &source);
        let fresh_receipt = Request::default()
            .emit(&fresh, named(&fresh, fresh_file, "emit"), 1024)
            .unwrap();
        assert_eq!(receipt, fresh_receipt);
        assert_eq!(consume(&receipt, CALLER), consume(&fresh_receipt, CALLER));
        assert_eq!(
            consume(&receipt, CALLER).unwrap().0,
            (41 + increment).to_string()
        );
    }
}

#[test]
fn aggregate_output_charge_is_independent_of_ctfe_cache_warmth() {
    let mut db = database();
    let file = input(&mut db, "provider", &provider(OUTPUT));
    let func = named(&db, file, "emit");
    for _ in 0..2 {
        let mut request = Request::default();
        let first = request.emit(&db, func, OUTPUT.len() * 2).unwrap();
        let second = request.emit(&db, func, OUTPUT.len() * 2).unwrap();
        assert_eq!(first.output_source, second.output_source);
        assert_ne!(first.invocation, second.invocation);
        assert_eq!(
            request.emit(&db, func, OUTPUT.len() * 2),
            Err(Failure::OutputLimit)
        );
        assert_eq!(request.ctfe_requests, 2);
        assert_eq!(request.bytes, OUTPUT.len() * 2);
    }
}

#[test]
fn growing_outputs_are_bounded_before_provider_execution() {
    let prefix = OUTPUT
        .as_bytes()
        .iter()
        .map(u8::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    for size in [64, 128, 256] {
        let source = format!(
            "const fn emit() -> [u8; {size}] {{
                let mut output: [u8; {size}] = [32; {size}]
                let prefix: [u8; {}] = [{prefix}]
                let mut i: usize = 0
                while i < {} {{ output[i] = prefix[i]\n i += 1 }}
                output
            }}",
            OUTPUT.len(),
            OUTPUT.len()
        );
        let mut db = database();
        let file = input(&mut db, "provider", &source);
        let mut request = Request::default();
        let result = request.emit(&db, named(&db, file, "emit"), 128);
        if size <= 128 {
            let receipt = result.unwrap();
            assert_eq!(receipt.output_source.len(), size);
            assert_eq!(consume(&receipt, CALLER).unwrap().0, "42");
        } else {
            assert_eq!(result, Err(Failure::OutputLimit));
            assert_eq!(request.ctfe_requests, 0);
        }
    }
}

#[test]
fn execution_failure_is_distinct_from_a_rejected_provider_type() {
    let mut db = database();
    let file = input(
        &mut db,
        "provider",
        "const fn emit() -> [u8; 1] { let zero: u8 = 0\n [1 / zero] }",
    );
    let mut request = Request::default();
    let result = request.emit(&db, named(&db, file, "emit"), 1);
    assert!(
        matches!(result, Err(Failure::Execution(ref error)) if error.contains("DivisionByZero")),
        "{result:?}"
    );
    assert_eq!(request.ctfe_requests, 1);
}

#[test]
fn unused_provider_requirements_need_independent_evidence() {
    for has_evidence in [false, true] {
        let implementation = if has_evidence {
            "impl Required for u256 {}"
        } else {
            ""
        };
        let helper = provider(OUTPUT)
            .replacen("emit()", "provide<T>()", 1)
            .replacen(" {", " where T: Required {", 1);
        let source = format!(
            "trait Required {{}}\n{implementation}\n{helper}\nconst fn emit() -> [u8; {}] {{ provide<u256>() }}",
            OUTPUT.len()
        );
        let mut db = database();
        let file = input(&mut db, "provider", &source);
        // The helper's body does not use T or Required. The ordinary call in
        // emit must still discharge its prerequisite before root execution.
        let mut request = Request::default();
        let result = request.emit(&db, named(&db, file, "emit"), 1024);
        if has_evidence {
            assert_eq!(consume(&result.unwrap(), CALLER).unwrap().0, "42");
        } else {
            assert!(
                matches!(result, Err(Failure::Provider(ref message)) if message.contains("Required")),
                "{result:?}"
            );
            assert_eq!(request.ctfe_requests, 0);
        }
    }
}

#[test]
fn productive_cascade_can_use_two_explicit_checked_stages() {
    let mut first = database();
    let source = provider(&provider(OUTPUT));
    let first_file = input(&mut first, "first", &source);
    let middle = Request::default()
        .emit(&first, named(&first, first_file, "emit"), 8192)
        .unwrap();
    let mut second = database();
    let second_file = input(&mut second, "second", &middle.output_source);
    let last = Request::default()
        .emit(&second, named(&second, second_file, "emit"), 1024)
        .unwrap();
    assert_eq!(middle.output_source, last.provider_source);
    assert_eq!(consume(&last, CALLER).unwrap().0, "42");
    // The host requests each stage here. This does not claim automatic
    // fixed-point discovery or generated item visibility in the first stage.
}

#[test]
fn borrow_checking_and_ctfe_support_are_separate_gates() {
    for conflicting in [false, true] {
        let write = if conflicting { "x = 1" } else { "" };
        let source = format!(
            "const fn emit() -> [u8; 1] {{
            let mut x: u8 = 0
            let a = mut x
            {write}
            a = 32
            [x]
        }}"
        );
        let mut db = database();
        let file = input(&mut db, "provider", &source);
        assert!(!db.run_on_top_mod(db.top_mod(file)).has_errors(&db));
        let mut request = Request::default();
        let result = request.emit(&db, named(&db, file, "emit"), 1);
        if conflicting {
            assert!(
                matches!(result, Err(Failure::Provider(ref message)) if message.contains("borrow conflict")),
                "{result:?}"
            );
            assert_eq!(request.ctfe_requests, 0);
        } else {
            // The valid local alias passes both analysis gates but this CTFE
            // path currently cannot execute it. Record that limitation without
            // treating well-typed code as automatically const-evaluable.
            assert!(
                matches!(result, Err(Failure::Execution(ref error)) if error.contains("InvalidProviderUse")),
                "{result:?}"
            );
            assert_eq!(request.ctfe_requests, 1);
        }
    }
}
