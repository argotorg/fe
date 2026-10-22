use fe_hir::analysis::ty::{
    corelib::{resolve_core_trait, resolve_lib_func_path, resolve_lib_type_path},
    ty_check::check_func_body,
};
use fe_hir::test_db::HirAnalysisTestDb;
use url::Url;

#[test]
fn const_fn_plus1_body_stays_usize_in_isolation() {
    let mut db = HirAnalysisTestDb::default();
    let url = Url::parse("file:///const_fn_plus1_body_stays_usize_in_isolation.fe").unwrap();
    let src = r#"
const fn plus1(_ x: usize) -> usize {
    x + 1
}
"#;

    let file = db.new_stand_alone(url.path().trim_start_matches('/').into(), src);
    let (top_mod, _) = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        fe_hir::test_db::format_diagnostics(&db, &diags)
    );

    let func = top_mod.all_funcs(&db)[0];
    assert!(
        resolve_core_trait(&db, func.scope(), &["ops", "Add"]).is_some(),
        "failed to resolve core::ops::Add"
    );
    let typed_body = &check_func_body(&db, func).1;
    let body = func.body(&db).expect("plus1 should have a body");

    assert_eq!(typed_body.result_ty().pretty_print(&db), "usize");
    assert_eq!(
        typed_body.expr_ty(&db, body.expr(&db)).pretty_print(&db),
        "usize"
    );
}

#[test]
fn standalone_root_exported_core_and_std_helpers_resolve() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "standalone_root_exported_core_and_std_helpers_resolve.fe".into(),
        "fn f() {}",
    );
    let (top_mod, _) = db.top_mod(file);
    let diags = db.run_on_top_mod(top_mod);
    assert!(
        diags.is_empty(),
        "unexpected diagnostics: {}",
        fe_hir::test_db::format_diagnostics(&db, &diags)
    );

    let func = top_mod.all_funcs(&db)[0];
    assert!(resolve_core_trait(&db, func.scope(), &["EffectRef"]).is_some());
    assert!(resolve_lib_type_path(&db, func.scope(), "core::range::Range").is_some());
    assert!(resolve_lib_func_path(&db, func.scope(), "core::panic").is_some());
    assert!(resolve_lib_type_path(&db, func.scope(), "std::abi::Sol").is_some());
}

#[test]
fn long_sol_signature_literals_remain_compile_time_only() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "long_sol_signature_literals_remain_compile_time_only.fe".into(),
        r#"use std::abi::sol

const SELECTOR: u32 = sol("transferFrom(address,address,uint256)")

pub fn selector() -> u32 {
    SELECTOR
}"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
}

#[test]
fn sol_codec_fallback_proof_waits_for_its_element_type() {
    use fe_hir::analysis::ty::{
        evidence::{Evidence, prove},
        trait_def::{ImplementorOrigin, TraitInstId},
        trait_resolution::PredicateListId,
        ty_def::TyId,
    };
    use fe_hir::hir_def::CallableDef;

    let mut db = release_profile_db();
    let file = db.workspace().touch(
        &mut db,
        Url::parse("file:///sol_codec_fallback_proof.fe").unwrap(),
        Some("pub fn root<T>() {}\n".to_string()),
    );
    let top_mod = db.top_mod(file);
    let func = top_mod.all_funcs(&db)[0];
    let scope = func.scope();
    let codec = resolve_lib_type_path(&db, scope, "std::evm::SolLanes").unwrap();
    let trait_ = resolve_core_trait(&db, scope, &["ops", "LaneCodec"]).unwrap();
    let goal = |element| TraitInstId::new_simple(&db, trait_, vec![codec, element]);
    let generic = goal(CallableDef::Func(func).params(&db)[0]);
    let solve_cx = TraitSolveCx::new(&db, scope);

    // The solver's provisional whole-slot fallback cannot become a lasting
    // implementation proof: substituting a packed element changes its answer.
    assert!(matches!(
        is_goal_satisfiable(&db, solve_cx, generic),
        GoalSatisfiability::Satisfied(_)
    ));
    assert!(prove(&db, solve_cx, generic).is_none());

    let supplied = solve_cx.with_assumptions(PredicateListId::new(&db, vec![generic]));
    assert!(matches!(
        prove(&db, supplied, generic).unwrap().data(&db),
        Evidence::Assumption(_)
    ));
    for (element, structural) in [(TyId::u8(&db), false), (TyId::unit(&db), true)] {
        let proof = prove(&db, solve_cx, goal(element)).unwrap();
        let Evidence::Impl { selected, .. } = proof.data(&db) else {
            panic!("concrete codec must have an implementation proof");
        };
        assert_eq!(
            matches!(selected.origin(&db), ImplementorOrigin::Structural),
            structural
        );
    }
}
