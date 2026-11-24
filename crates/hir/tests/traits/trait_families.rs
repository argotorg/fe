use common::indexmap::IndexMap;
use fe_hir::{
    analysis::ty::{
        trait_def::TraitInstId,
        ty_def::{Kind, TraitFamilyId, TyData, TyId},
    },
    hir_def::{ItemKind, ModeMarker},
    test_db::{HirAnalysisTestDb, initialize_test_analysis_pass},
};

#[test]
fn substituted_trait_family_forms_the_same_goal_as_direct_application() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "trait_families.fe".into(),
        r#"
trait Hash<Rhs = Self> {}
trait Display {}
trait Constraint {}
fn existing_bound<T: Constraint>() {}
struct Requirement<G: (Constraint)> {}
type ForField<P: * -> Constraint, T> = Requirement<P<T>>
type HashByte = ForField<trait Hash, u8>
type DisplayByte = ForField<trait Display, u8>
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let def = module
        .children_non_nested(&db)
        .find_map(|item| match item {
            ItemKind::Trait(def) if def.name(&db).to_opt().unwrap().data(&db) == "Hash" => {
                Some(def)
            }
            _ => None,
        })
        .expect("test trait Hash");
    let hash = TyId::new(
        &db,
        TyData::TraitFamily(TraitFamilyId::new(
            &db,
            def,
            Vec::new(),
            IndexMap::new(),
            ModeMarker::None,
        )),
    );

    let aliases = module.all_type_aliases(&db);
    let hash_byte = aliases[1].ty(&db).generic_args(&db)[0];
    let display_byte = aliases[2].ty(&db).generic_args(&db)[0];
    let TyData::ConstraintTerm(goal) = hash_byte.data(&db) else {
        panic!(
            "expected specialized goal, got {}",
            hash_byte.pretty_print(&db)
        );
    };
    let subject = goal.self_ty(&db);
    let direct = TyId::new(
        &db,
        TyData::ConstraintTerm(TraitInstId::new_simple(&db, def, vec![subject, subject])),
    );
    assert_eq!(hash_byte, TyId::app(&db, hash, subject));
    assert_eq!(hash_byte, direct);
    assert_eq!(*display_byte.kind(&db), Kind::Constraint);
    assert_ne!(hash_byte, display_byte);
}

#[test]
fn ordinary_type_arguments_and_runtime_positions_keep_their_kinds() {
    for source in [
        "trait Hash {}\nstruct Key<T> {}\nfn bad(_ key: Key<Hash>) {}",
        "trait Hash {}\nfn ask<P: * -> Constraint>() {}\nfn bad() { ask<Hash>() }",
        "fn bad<G: (Constraint)>(_ goal: G) {}",
        "struct Option<T> {}\nfn ask<P: * -> Constraint>() {}\nfn bad() { ask<Option>() }",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("wrong_family_kind.fe".into(), source);
        let (module, _) = db.top_mod(file);
        let mut passes = initialize_test_analysis_pass();
        let diagnostics = passes.run_on_module(&db, module);
        assert!(!diagnostics.is_empty(), "{source}");
    }
}

#[test]
fn associated_bindings_are_order_independent() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "family_binding_order.fe".into(),
        r#"
trait HasParts {
    type Left
    type Right
}
struct Family<P: * -> Constraint> {}
struct Requirement<G: (Constraint)> {}
type ForField<P: * -> Constraint, T> = Requirement<P<T>>
type First = Family<trait HasParts<Left = u8, Right = u16>>
type Second = Family<trait HasParts<Right = u16, Left = u8>>
type FirstGoal = ForField<trait HasParts<Left = u8, Right = u16>, bool>
type SecondGoal = ForField<trait HasParts<Right = u16, Left = u8>, bool>
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
    let aliases = module.all_type_aliases(&db);
    assert_eq!(aliases[1].ty(&db), aliases[2].ty(&db));
    assert_eq!(aliases[3].ty(&db), aliases[4].ty(&db));
}

#[test]
fn duplicate_associated_bindings_are_diagnosed_in_aliases_and_calls() {
    for use_site in [
        "type Bad = Family<trait HasItem<Item = u8, Item = u16>>",
        "fn bad() { accept<trait HasItem<Item = u8, Item = u16>>() }",
    ] {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "duplicate_family_bindings.fe".into(),
            &format!(
                "trait HasItem {{ type Item }}\nstruct Family<P: * -> Constraint> {{}}\nfn accept<P: * -> Constraint>() {{}}\n{use_site}"
            ),
        );
        let (module, _) = db.top_mod(file);
        let diagnostics = db.run_on_top_mod(module);
        assert!(
            diagnostics.iter().any(|diagnostic| {
                diagnostic.to_complete(&db).message == "`Item` is bound more than once"
            }),
            "{use_site}"
        );
    }
}

#[test]
fn family_capture_uses_the_enclosing_impls_associated_binding() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "family_captures_impl_binding.fe".into(),
        r#"
trait Rel<T> {}
struct Family<P: * -> Constraint> {}
trait Parts {
    type Left
    type Right
}
struct Pair {}
impl Parts for Pair {
    type Left = Family<trait Rel<Self::Right>>
    type Right = u256
}
fn verify(value: <Pair as Parts>::Left) -> Family<trait Rel<u256>> {
    value
}
"#,
    );
    let (module, _) = db.top_mod(file);
    db.assert_no_diags(module);
}

#[test]
fn impls_over_families_with_projections_overlap_in_coherence() {
    let header = "trait Assoc {\n    type Item\n}\ntrait Rel<T> {}\ntrait Other<T> {}\ntrait Marker {}\nstruct Family<P: * -> Constraint> {}\nstruct Holder {}\nimpl Assoc for Holder {\n    type Item = u8\n}\n";
    let diagnostics = |body: &str| {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("family_overlap.fe".into(), &format!("{header}{body}"));
        let (module, _) = db.top_mod(file);
        let mut passes = initialize_test_analysis_pass();
        passes.run_on_module(&db, module).len()
    };

    // Two impls whose family arguments are the same projection overlap.
    assert_ne!(
        diagnostics(
            "impl<T: Assoc> Marker for Family<trait Rel<T::Item>> {}\n\
             impl<U: Assoc> Marker for Family<trait Rel<U::Item>> {}\n"
        ),
        0
    );
    // Different families do not overlap.
    assert_eq!(
        diagnostics(
            "impl<T: Assoc> Marker for Family<trait Rel<T::Item>> {}\n\
             impl<U: Assoc> Marker for Family<trait Other<U::Item>> {}\n"
        ),
        0
    );
}
