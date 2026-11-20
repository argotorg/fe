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
