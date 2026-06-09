use camino::Utf8PathBuf;
use fe_hir::hir_def::{Func, ItemKind, TopLevelMod};
use fe_hir::semantic::param_env;
use fe_hir::test_db::HirAnalysisTestDb;

fn find_func<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    name: &str,
    parent: impl Fn(Option<ItemKind<'db>>) -> bool,
) -> Func<'db> {
    top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db).to_opt().is_some_and(|n| n.data(db) == name)
                && parent(func.scope().parent_item(db))
        })
        .unwrap_or_else(|| panic!("missing `{name}` function"))
}

fn param_env_preds<'db>(db: &'db HirAnalysisTestDb, item: ItemKind<'db>) -> Vec<String> {
    param_env(db, item)
        .list(db)
        .iter()
        .map(|pred| pred.pretty_print(db, true))
        .collect()
}

#[test]
fn trait_method_param_env_assumes_self_and_supertraits() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_method_param_env_assumes_self_and_supertraits.fe"),
        r#"
trait Base {}

trait Sub: Base {
    fn m(self) -> bool
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let method = find_func(&db, top_mod, "m", |parent| {
        matches!(parent, Some(ItemKind::Trait(_)))
    });
    let preds = param_env_preds(&db, method.into());

    assert!(
        preds.iter().any(|p| p == "Self: Sub"),
        "trait method param env is missing the implicit self predicate: {preds:?}"
    );
    assert!(
        preds.iter().any(|p| p == "Self: Base"),
        "trait method param env is missing the implied super-trait bound: {preds:?}"
    );
}

#[test]
fn trait_param_env_assumes_self_and_supertraits() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("trait_param_env_assumes_self_and_supertraits.fe"),
        r#"
trait Base {}

trait Sub: Base {}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let sub = top_mod
        .all_traits(&db)
        .iter()
        .copied()
        .find(|trait_| {
            trait_
                .name(&db)
                .to_opt()
                .is_some_and(|n| n.data(&db) == "Sub")
        })
        .expect("missing `Sub` trait");
    let preds = param_env_preds(&db, sub.into());

    assert!(
        preds.iter().any(|p| p == "Self: Sub"),
        "trait param env is missing the implicit self predicate: {preds:?}"
    );
    assert!(
        preds.iter().any(|p| p == "Self: Base"),
        "trait param env is missing the implied super-trait bound: {preds:?}"
    );
}

#[test]
fn impl_trait_method_param_env_includes_impl_constraints() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("impl_trait_method_param_env_includes_impl_constraints.fe"),
        r#"
trait A {}

trait T {
    fn m(self) -> bool
}

struct S<X> {}

impl<X: A> T for S<X> {
    fn m(self) -> bool {
        true
    }
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let method = find_func(&db, top_mod, "m", |parent| {
        matches!(parent, Some(ItemKind::ImplTrait(_)))
    });
    let preds = param_env_preds(&db, method.into());

    assert!(
        preds.iter().any(|p| p == "X: A"),
        "impl method param env is missing the impl's bound: {preds:?}"
    );
    assert!(
        !preds.iter().any(|p| p.starts_with("Self:")),
        "impl method param env should not assume a bare `Self` predicate: {preds:?}"
    );
}

/// Elaborating `Self: RecursiveSuper` adds assumptions whose self type is a
/// projection rooted at `Self` (`Self::Item: RecursiveSuper`). Substituting
/// `Self` inside those predicates used to recurse forever in `AssocTySubst`;
/// this pins the occurs check that keeps elaboration terminating.
#[test]
fn param_env_terminates_on_recursive_projection_supertrait() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("param_env_terminates_on_recursive_projection_supertrait.fe"),
        r#"
trait A<T> {}

trait RecursiveSuper: A<Self::Item::Assoc> {
    type Item: RecursiveSuper
    type Assoc
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let trait_ = top_mod
        .all_traits(&db)
        .iter()
        .copied()
        .find(|trait_| {
            trait_
                .name(&db)
                .to_opt()
                .is_some_and(|n| n.data(&db) == "RecursiveSuper")
        })
        .expect("missing `RecursiveSuper` trait");
    let preds = param_env_preds(&db, trait_.into());

    assert!(
        preds.iter().any(|p| p == "Self: RecursiveSuper"),
        "recursive trait param env is missing the implicit self predicate: {preds:?}"
    );
}

#[test]
fn module_func_param_env_matches_declared_bounds() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        Utf8PathBuf::from("module_func_param_env_matches_declared_bounds.fe"),
        r#"
trait A {}

fn f<X: A>() -> bool {
    true
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let func = find_func(&db, top_mod, "f", |parent| {
        matches!(parent, Some(ItemKind::Mod(_)) | None)
            || !matches!(
                parent,
                Some(ItemKind::Trait(_) | ItemKind::Impl(_) | ItemKind::ImplTrait(_))
            )
    });
    let preds = param_env_preds(&db, func.into());

    assert_eq!(
        preds,
        vec!["X: A".to_string()],
        "module-level function param env should be exactly its declared bounds"
    );
}
