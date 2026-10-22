use fe_hir::{
    analysis::ty::{
        ty_check::GroundBoundAdmissionCause,
        ty_check::{GroundTypeAdmissionKind, admit_ground_type_application},
    },
    test_db::{HirAnalysisTestDb, find_func},
};

fn inspect(
    source: &str,
    check: impl FnOnce(
        Option<(
            &GroundTypeAdmissionKind<'_>,
            Vec<common::diagnostics::CompleteDiagnostic>,
        )>,
    ),
) {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone("ground-admission.fe".into(), source);
    let module = db.top_mod(file).0;
    let function = find_func(&db, module, "inspect");
    let ty = function.arg_ty(&db, 0).unwrap().instantiate_identity();
    match admit_ground_type_application(&db, ty, function.into(), function.span().into()) {
        Ok(()) => check(None),
        Err(error) => {
            let diagnostics = error
                .vouchers()
                .iter()
                .map(|v| v.to_complete(&db))
                .collect();
            check(Some((&error.kind, diagnostics)));
        }
    }
}

#[test]
fn lawful_concrete_bounds_and_parametric_conditions_are_admitted() {
    for source in [
        "struct Target {}\nfn inspect(_ value: Target) {}",
        "trait Marker {}\nimpl Marker for u8 where true {}\nstruct Target<T: Marker> { value: T }\nfn inspect(_ value: Target<u8>) {}",
        "trait Check { const OK: bool }\nimpl Check for u8 { const OK: bool = true }\nstruct Target<T: Check> where <T as Check>::OK { value: T }\nfn inspect(_ value: Target<u8>) {}",
        "struct Target<const N: usize> where N > 0 { value: u8 }\nfn inspect(_ value: Target<1>) {}",
        "struct Inner where true {}\nenum Target<T> { Left(T), Right }\nfn inspect(_ value: Target<Inner>) {}",
    ] {
        inspect(source, |error| assert!(error.is_none(), "{error:?}"));
    }
}

#[test]
fn root_and_repeated_nested_ground_failures_are_reported_once_at_declaration() {
    for source in [
        "struct Target where false {}\nfn inspect(_ value: Target) {}",
        "struct Poison where false {}\nstruct Target<A, B> { a: A, b: B }\nfn inspect(_ value: Target<Poison, Poison>) {}",
    ] {
        inspect(source, |error| {
            let (kind, diagnostics) = error.unwrap();
            assert!(matches!(kind, GroundTypeAdmissionKind::Declaration));
            assert_eq!(diagnostics.len(), 1);
            let span = diagnostics[0].primary_span().unwrap();
            assert_eq!(
                &source[span.range.start().into()..span.range.end().into()],
                "false"
            );
        });
    }
}

#[test]
fn false_bound_is_checked_and_retains_original_condition_diagnostic() {
    let source = "trait Marker {}\nimpl Marker for u8 where false {}\nstruct Target<T: Marker> { value: T }\nfn inspect(_ value: Target<u8>) {}";
    inspect(source, |error| {
        let (kind, diagnostics) = error.unwrap();
        assert!(matches!(
            kind,
            GroundTypeAdmissionKind::Bound {
                cause: GroundBoundAdmissionCause::ConditionsNotAdmitted,
                ..
            }
        ));
        assert_eq!(diagnostics.len(), 1);
        let span = diagnostics[0].primary_span().unwrap();
        assert_eq!(
            &source[span.range.start().into()..span.range.end().into()],
            "false"
        );
    });
}

#[test]
fn initial_admission_does_not_assume_the_origins_missing_bound() {
    let source = "trait Marker {}\nstruct Target<T: Marker> { value: T }\nfn inspect(_ value: Target<u8>) where u8: Marker {}";
    inspect(source, |error| {
        let (kind, diagnostics) = error.unwrap();
        assert!(matches!(
            kind,
            GroundTypeAdmissionKind::Bound {
                cause: GroundBoundAdmissionCause::NotProven,
                ..
            }
        ));
        assert_eq!(diagnostics.len(), 1);
    });
}

#[test]
fn parametric_failure_stays_at_the_written_application_once() {
    let source = "trait Check { const OK: bool }\nimpl Check for u8 { const OK: bool = false }\nstruct Target<T: Check> where <T as Check>::OK { value: T }\nfn inspect(_ value: Target<u8>) {}";
    inspect(source, |error| {
        let (kind, diagnostics) = error.unwrap();
        assert!(matches!(kind, GroundTypeAdmissionKind::Requirement));
        assert_eq!(diagnostics.len(), 1);
        let span = diagnostics[0].primary_span().unwrap();
        assert_eq!(
            &source[span.range.start().into()..span.range.end().into()],
            "Target<u8>"
        );
    });
}

#[test]
fn unsupported_conditional_nominal_and_generic_impl_conditions_remain_refused() {
    let nominal = "trait Marker {}\nstruct Inner where true {}\nimpl Marker for Inner {}\nstruct Target<T: Marker> { value: T }\nfn inspect(_ value: Target<Inner>) {}";
    inspect(nominal, |error| {
        assert!(matches!(
            error.unwrap().0,
            GroundTypeAdmissionKind::Bound {
                cause: GroundBoundAdmissionCause::UnsupportedConditions,
                ..
            }
        ))
    });
    inspect(&nominal.replace(" where true", ""), |error| {
        assert!(error.is_none(), "{error:?}")
    });
    let generic = "trait Marker {}\nstruct Box<T> { value: T }\nimpl<T> Marker for Box<T> where true {}\nstruct Target<T: Marker> { value: T }\nfn inspect(_ value: Target<Box<u8>>) {}";
    inspect(generic, |error| {
        let (kind, diagnostics) = error.unwrap();
        assert!(matches!(
            kind,
            GroundTypeAdmissionKind::Bound {
                cause: GroundBoundAdmissionCause::ConditionsNotAdmitted,
                ..
            }
        ));
        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message == "const where predicates are not supported here yet"),
            "{diagnostics:?}"
        );
        assert!(
            diagnostics
                .iter()
                .flat_map(|d| &d.sub_diagnostics)
                .any(|d| d.message.contains("trait impls with generic parameters"))
        );
    });
    inspect(&generic.replace(" where true", ""), |error| {
        assert!(error.is_none(), "{error:?}")
    });
}

#[test]
fn condition_fault_cause_is_preserved_and_not_replaced_by_a_boolean() {
    let source = "const fn fault() -> bool { let zero: u256 = 0\nlet unused = 1 / zero\ntrue }\ntrait Marker {}\nimpl Marker for u8 where fault() {}\nstruct Target<T: Marker> { value: T }\nfn inspect(_ value: Target<u8>) {}";
    inspect(source, |error| {
        let (kind, diagnostics) = error.unwrap();
        assert!(matches!(
            kind,
            GroundTypeAdmissionKind::Bound {
                cause: GroundBoundAdmissionCause::ConditionsNotAdmitted,
                ..
            }
        ));
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("division by zero")),
            "{diagnostics:?}"
        );
    });
}

#[test]
fn existing_declaration_and_cycle_checks_remain_in_force() {
    for source in [
        "struct Target { same: u8, same: u8 }\nfn inspect(_ value: Target) {}",
        "struct Target { recursive: Target }\nfn inspect(_ value: Target) {}",
    ] {
        inspect(source, |error| {
            let (kind, diagnostics) = error.unwrap();
            assert!(matches!(kind, GroundTypeAdmissionKind::Declaration));
            assert!(!diagnostics.is_empty());
        });
    }
}

#[test]
fn named_canonical_and_pending_derive_never_supply_ordinary_evidence() {
    use fe_hir::{
        analysis::ty::{evidence::prove, trait_resolution::TraitSolveCx},
        test_db::initialize_test_analysis_pass,
    };

    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "canonical-named-and-unfulfilled-gates.fe".into(),
        r#"use core::derive::Derive
use core::meta::{Code, Item, Record}
use core::permits::PermitAuthority

#[canonical]
trait Score {
    const fn score() -> usize
}

pub struct Key {}
pub struct Other {}

struct Provider {}
impl Derive<Score> for Provider {
    const fn derive(_ r: Record) -> Code<Item> {
        quote {
            const fn score() -> usize { 1 }
        }
    }
}

#[permit_root]
const ROOT_KEY: PermitAuthority<Score<Key>> = PermitAuthority {}

#[permit(ROOT_KEY)]
impl Score for Key as NamedCanonical {
    const fn score() -> usize { 1 }
}

fn use_named_canonical() -> usize {
    with (NamedCanonical) {
        Key::score()
    }
}

#[permit_root]
const ROOT_OTHER: PermitAuthority<Score<Other>> = PermitAuthority {}

#[permit(ROOT_OTHER)]
impl Score for Other with (Provider)
"#,
    );
    let (module, _) = db.top_mod(file);
    let mut pass_manager = initialize_test_analysis_pass();
    let diags = pass_manager.run_on_module(&db, module);
    assert!(
        !diags.is_empty(),
        "named impl of #[canonical] trait and opening it via `with (NamedCanonical)` must report diagnostics"
    );
    let rendered = fe_hir::test_db::format_diagnostics(&db, &diags);
    assert!(
        rendered.contains("error[6-0037]: native derive request was not prepared"),
        "unfulfilled derive needs its own diagnostic code: {rendered}"
    );

    let score_trait = module
        .all_traits(&db)
        .iter()
        .copied()
        .find(|t| t.name(&db).to_opt().unwrap().data(&db) == "Score")
        .unwrap();

    let mut checked = 0;
    for &implementation in module.all_impl_traits(&db) {
        let Ok(goal) = implementation.candidate_trait_inst_result(&db) else {
            continue;
        };
        if goal.def(&db) == score_trait {
            checked += 1;
            assert!(
                prove(&db, TraitSolveCx::new(&db, module.scope()), goal).is_none(),
                "unauthorized named and unprepared declarations supply no ordinary proof"
            );
        }
    }
    assert_eq!(checked, 2, "both authored request headers must be checked");
}

#[test]
fn protocol_port_does_not_enable_public_constraint_kinds() {
    use fe_parser::{RecoveryMode, parse_source_file};

    for source in [
        "trait Constraint {}\ntrait CustomDerive<G: * -> Constraint> {}\n",
        "mod core { pub mod obligations { pub trait Constraint {} } }\nuse ingot::core::obligations::Constraint\ntrait CustomDerive<G: * -> Constraint> {}\n",
    ] {
        let (_, errors) = parse_source_file(source, RecoveryMode::NoRecover);
        assert!(
            !errors.is_empty(),
            "public Constraint kinds remain deferred: {source}"
        );
    }
}
