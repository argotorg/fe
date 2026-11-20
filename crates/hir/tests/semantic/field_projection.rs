//! A field access through `core::ops::Project` is an ordinary call of
//! `ProjectField::project` in the normalized body, the form that the Lean
//! export reads: it adds no construct of its own.

use fe_hir::test_db::{HirAnalysisTestDb, find_func};
use fe_hir::{
    analysis::{
        semantic::{
            NExpr, NStatementKind, SemanticInstance, identity_semantic_instance_key,
            normalize_semantic_body,
        },
        ty::ty_check::BodyOwner,
    },
    hir_def::TopLevelMod,
};

/// The kind of each defined expression of `name`'s normalized body, with the
/// callee's name for a call.
fn defined_kinds<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    name: &str,
) -> Vec<String> {
    let func = find_func(db, top_mod, name);
    let instance = SemanticInstance::new(
        db,
        identity_semantic_instance_key(db, BodyOwner::Func(func, None)),
    );
    let normalized = normalize_semantic_body(db, instance).expect("normalized body");
    normalized
        .body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| {
            let NStatementKind::Define { expr, .. } = &statement.kind else {
                return None;
            };
            Some(match expr {
                NExpr::Call { callee, .. } => match callee.key.owner(db) {
                    BodyOwner::Func(func, _) => {
                        format!("call {}", func.name(db).to_opt().unwrap().data(db))
                    }
                    _ => "call".to_string(),
                },
                NExpr::ProjectValue { .. } => "project value".to_string(),
                other => format!("{:?}", std::mem::discriminant(other)),
            })
        })
        .collect()
}

#[test]
fn field_syntax_through_project_is_the_projection_call() {
    let mut db = HirAnalysisTestDb::default();
    let file = db.new_stand_alone(
        "field_projection.fe".into(),
        r#"
use core::ops::{Field, Project, ProjectField}

pub struct Rec { pub a: u256, pub b: u256 }

pub struct View<T> { base: u256 }
impl<T> Copy for View<T> {}
impl<T> Project for View<T> { type Target = T }
impl<T, F, B> ProjectField<Field<T, F, B>> for View<T> {
    type Output = u256
    fn project(self) -> ref u256 { self.base }
}

fn by_field(_ v: View<Rec>) -> u256 {
    v.b
}

fn by_call(_ v: View<Rec>) -> u256 {
    <View<Rec> as ProjectField<Field<Rec, u256, (u256,)>>>::project(v)
}
"#,
    );
    let (top_mod, _) = db.top_mod(file);
    db.assert_no_diags(top_mod);

    let by_field = defined_kinds(&db, top_mod, "by_field");
    let by_call = defined_kinds(&db, top_mod, "by_call");
    assert_eq!(
        by_field
            .iter()
            .filter(|kind| *kind == "call project")
            .count(),
        1,
        "{by_field:?}"
    );
    assert!(
        !by_field.iter().any(|kind| kind == "project value"),
        "{by_field:?}"
    );
    assert_eq!(by_field, by_call);
}
