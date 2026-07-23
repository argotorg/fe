use common::{
    InputDb,
    indexmap::IndexMap,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use ena::unify::InPlace;
use fe_hir::{
    analysis::ty::{
        trait_def::TraitInstId,
        trait_resolution::{
            CanonicalGoalQuery, GoalSatisfiability, PredicateListId, TraitSolveCx,
            is_goal_query_satisfiable,
        },
        ty_def::{Kind, TyVarSort},
        unify::{InferenceKey, UnificationTableBase},
    },
    hir_def::Partial,
    test_db::HirAnalysisTestDb,
};
use url::Url;

fn touch(db: &mut HirAnalysisTestDb, url: &str, content: &str) -> common::file::File {
    db.workspace().touch(
        db,
        Url::parse(url).expect("valid test URL"),
        Some(content.to_string()),
    )
}

#[test]
fn later_constraint_uses_ingot_of_type_bound_by_earlier_constraint() {
    let mut db = HirAnalysisTestDb::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();

    touch(
        &mut db,
        "file:///trait-solver-cross-ingot/traits/fe.toml",
        r#"
[ingot]
name = "traits"
version = "0.1.0"
"#,
    );
    touch(
        &mut db,
        "file:///trait-solver-cross-ingot/traits/src/lib.fe",
        "pub trait Accept {}\n",
    );

    touch(
        &mut db,
        "file:///trait-solver-cross-ingot/external/fe.toml",
        r#"
[ingot]
name = "external"
version = "0.1.0"

[dependencies]
traits = { path = "../traits" }
"#,
    );
    touch(
        &mut db,
        "file:///trait-solver-cross-ingot/external/src/lib.fe",
        r#"
use traits::Accept

pub struct External {}
impl Accept for External {}
"#,
    );

    touch(
        &mut db,
        "file:///trait-solver-cross-ingot/bridge/fe.toml",
        r#"
[ingot]
name = "bridge"
version = "0.1.0"

[dependencies]
traits = { path = "../traits" }
external = { path = "../external" }
"#,
    );
    let bridge_file = touch(
        &mut db,
        "file:///trait-solver-cross-ingot/bridge/src/lib.fe",
        r#"
use traits::Accept
use external::External

pub trait Goal<T> {}
trait Choose<T> {}

pub struct Subject {}

impl Choose<External> for Subject {}
impl<SelfT, T> Goal<T> for SelfT
where
    T: Accept,
    SelfT: Choose<T>
{}
"#,
    );

    touch(
        &mut db,
        "file:///trait-solver-cross-ingot/app/fe.toml",
        r#"
[ingot]
name = "app"
version = "0.1.0"

[dependencies]
bridge = { path = "../bridge" }
"#,
    );
    let app_file = touch(
        &mut db,
        "file:///trait-solver-cross-ingot/app/src/lib.fe",
        r#"
use bridge::Subject

fn test_types(_ subject: Subject) {}
"#,
    );

    let (bridge, _) = db.top_mod(bridge_file);
    let (app, _) = db.top_mod(app_file);
    db.assert_no_diags(bridge);
    db.assert_no_diags(app);
    let impls = bridge
        .all_impl_traits(&db)
        .iter()
        .filter_map(|impl_| impl_.trait_inst(&db))
        .map(|inst| inst.pretty_print(&db, true))
        .collect::<Vec<_>>();
    assert_eq!(
        impls.len(),
        2,
        "expected both local impls to lower, got {impls:?}"
    );

    let goal_trait = bridge
        .all_traits(&db)
        .iter()
        .copied()
        .find(|trait_| {
            trait_
                .name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "Goal")
        })
        .expect("missing `Goal` trait");
    let choose_trait = bridge
        .all_traits(&db)
        .iter()
        .copied()
        .find(|trait_| {
            trait_
                .name(&db)
                .to_opt()
                .is_some_and(|name| name.data(&db) == "Choose")
        })
        .expect("missing `Choose` trait");
    let test_types = app
        .all_funcs(&db)
        .iter()
        .copied()
        .find(|func| {
            matches!(
                func.name(&db),
                Partial::Present(name) if name.data(&db) == "test_types"
            )
        })
        .expect("missing `test_types` function");
    let choose_impl = bridge
        .all_impl_traits(&db)
        .iter()
        .find_map(|impl_| {
            impl_
                .trait_inst(&db)
                .filter(|inst| inst.def(&db) == choose_trait)
        })
        .expect("missing `Choose<External> for Subject` impl");
    let subject = choose_impl.self_ty(&db);
    let external = choose_impl.args(&db)[1];

    let assumptions = PredicateListId::empty_list(&db);
    let solve_cx = TraitSolveCx::new(&db, test_types.scope()).with_assumptions(assumptions);
    let direct_choose =
        TraitInstId::new(&db, choose_trait, vec![subject, external], IndexMap::new());
    let direct_choose_result =
        fe_hir::analysis::ty::trait_resolution::is_goal_satisfiable(&db, solve_cx, direct_choose);
    assert!(
        matches!(direct_choose_result, GoalSatisfiability::Satisfied(_)),
        "the direct `Choose<External>` goal must be satisfiable: {direct_choose_result:?}; \
         impls: {impls:?}; goal: {}",
        direct_choose.pretty_print(&db, true),
    );
    let mut table = UnificationTableBase::<InPlace<InferenceKey<'_>>>::new(&db);
    let selected = table.new_var(TyVarSort::General, &Kind::Star);
    let goal = TraitInstId::new(&db, goal_trait, vec![subject, selected], IndexMap::new());
    let query = CanonicalGoalQuery::new(&db, goal, assumptions);

    let result = is_goal_query_satisfiable(&db, solve_cx, &query);
    let unresolved = match &result {
        GoalSatisfiability::UnSat(Some(subgoal)) => Some(
            query
                .extract_subgoal(&mut table, *subgoal)
                .pretty_print(&db, true),
        ),
        _ => None,
    };
    assert!(
        matches!(result, GoalSatisfiability::Satisfied(_)),
        "the second constraint must search the ingot of the type bound by the first constraint: \
         {result:?}; unresolved subgoal: {unresolved:?}",
    );
}
