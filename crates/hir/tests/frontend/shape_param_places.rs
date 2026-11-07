use fe_hir::{
    analysis::{
        semantic::{SemanticInstance, identity_semantic_instance_key, normalize_semantic_body},
        ty::ty_check::BodyOwner,
    },
    test_db::HirAnalysisTestDb,
};

/// A place through an access part of a shape parameter reaches what the
/// access reaches, as a pattern binding does, so the parameter needs no slot.
#[test]
fn access_part_places_need_no_slot() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "access_part_places_need_no_slot.fe".into(),
        r#"
trait Driver {
    shape Step
    fn at(mut? self) -> Self::Step
}

struct Pair { a: u256, b: u256 }

impl Driver for Pair {
    shape Step = (own u256, mut? u256)
    fn at(mut? self) -> (own u256, mut? u256) {
        (self.a, mut? self.b)
    }
}

struct Triple { a: u256, b: u256, c: u256 }

impl Driver for Triple {
    shape Step = (own u256, (mut? u256, mut? u256))
    fn at(mut? self) -> (own u256, (mut? u256, mut? u256)) {
        (self.a, (mut? self.b, mut? self.c))
    }
}

fn read(_ x: ref u256) -> u256 {
    x
}

fn named_ref(_ s: Pair::Step<ref>) -> u256 {
    let x = ref s.1
    x + s.0
}

fn passed_ref(_ s: Pair::Step<mut>) -> u256 {
    read(ref s.1)
}

fn nested_ref(_ s: Triple::Step<ref>) -> u256 {
    let (ref x, ref y) = s.1
    x + y
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);
    for &func in top_mod.all_funcs(&db) {
        let name = func
            .name(&db)
            .to_opt()
            .expect("named")
            .data(&db)
            .to_string();
        if !["named_ref", "passed_ref", "nested_ref"].contains(&name.as_str()) {
            continue;
        }
        let instance = SemanticInstance::new(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func, None)),
        );
        let normalized = normalize_semantic_body(&db, instance).expect("normalized body");
        assert!(
            normalized.body.roots.is_empty(),
            "`{name}` has roots: {:?}",
            normalized.body.roots
        );
    }
}
