use fe_hir::{
    analysis::ty::{
        trait_def::{
            CheckedApplicationError, ImplSelectionClass, TraitInstId, impl_selection_class,
            named_impl_candidates, prove_selected_hir_application, resolve_trait_method_instance,
        },
        trait_resolution::{GoalSatisfiability, Selection, TraitSolveCx, is_goal_satisfiable},
    },
    hir_def::IdentId,
    test_db::{HirAnalysisTestDb, find_func},
};

#[test]
fn named_declarations_do_not_enter_ordinary_discovery_or_coherence() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "named_declarations.fe".into(),
        r#"
trait Pick { fn value(self) -> u256 }
struct Subject {}
impl Pick for Subject { fn value(self) -> u256 { 1 } }
impl Pick for Subject as First { fn value(self) -> u256 { 2 } }
impl Pick for Subject as Second { fn value(self) -> u256 { 3 } }
fn request(_ subject: Subject) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let items = module.all_impl_traits(&db);
    let request = find_func(&db, module, "request");
    let subject = request.params(&db).next().unwrap().ty(&db);
    let subject = subject.as_view(&db).unwrap_or(subject);
    let goal = TraitInstId::new_simple(&db, module.all_traits(&db)[0], vec![subject]);
    let cx = TraitSolveCx::new(&db, request.scope());
    assert_eq!(
        impl_selection_class(&db, items[0]),
        ImplSelectionClass::Ordinary
    );
    let method_name = IdentId::new(&db, "value");
    let Selection::Unique(ordinary) = resolve_trait_method_instance(&db, cx, goal, method_name)
    else {
        panic!("named alternatives must not alter ordinary selection")
    };
    assert_eq!(ordinary.body(), items[0].methods(&db).next());
    for (idx, name) in [(1, "First"), (2, "Second")] {
        let name = IdentId::new(&db, name);
        assert_eq!(items[idx].selection_name(&db), Some(name));
        assert_eq!(
            impl_selection_class(&db, items[idx]),
            ImplSelectionClass::Named(name)
        );
        assert_eq!(
            named_impl_candidates(&db, request.scope(), name),
            vec![items[idx]]
        );
        assert!(
            items[idx]
                .pretty_print(&db)
                .contains(&format!(" as {}", name.data(&db)))
        );
        let checked = prove_selected_hir_application(&db, cx, items[idx], goal).unwrap();
        let Selection::Unique(method) = checked.method(method_name) else {
            panic!("checked named application must retain its method")
        };
        assert_eq!(method.body(), items[idx].methods(&db).next());
    }
}

#[test]
fn named_only_declaration_needs_explicit_proof_and_cannot_borrow_another_root() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "named_prerequisites.fe".into(),
        r#"
trait Leaf {}
trait Pick {}
trait Alone {}
struct Subject {}
impl Pick for Subject {}
impl<T: Leaf> Pick for T as NeedsLeaf {}
impl Alone for Subject as Only {}
fn request(_ subject: Subject) {}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let request = find_func(&db, module, "request");
    let subject = request.params(&db).next().unwrap().ty(&db);
    let subject = subject.as_view(&db).unwrap_or(subject);
    let traits = module.all_traits(&db);
    let cx = TraitSolveCx::new(&db, request.scope());
    let pick = TraitInstId::new_simple(&db, traits[1], vec![subject]);
    assert!(matches!(
        is_goal_satisfiable(&db, cx, pick),
        GoalSatisfiability::Satisfied(_)
    ));
    let named = named_impl_candidates(&db, request.scope(), IdentId::new(&db, "NeedsLeaf"))[0];
    assert_eq!(
        prove_selected_hir_application(&db, cx, named, pick).unwrap_err(),
        CheckedApplicationError::NotProven
    );
    let alone = TraitInstId::new_simple(&db, traits[2], vec![subject]);
    assert!(matches!(
        is_goal_satisfiable(&db, cx, alone),
        GoalSatisfiability::UnSat(_)
    ));
    let only = named_impl_candidates(&db, request.scope(), IdentId::new(&db, "Only"))[0];
    assert!(prove_selected_hir_application(&db, cx, only, alone).is_ok());
    assert!(named_impl_candidates(&db, request.scope(), IdentId::new(&db, "impl#1")).is_empty());
}
