use common::{InputDb, origin::OriginExportKey};
use driver::DriverDataBase;
use fe_mir::{
    RBlockId, RuntimeInstanceKey,
    instance::{RuntimeInstanceSource, get_or_build_runtime_instance},
    origin::{
        RuntimeInstanceOwnerKey, RuntimeStmtIndex, RuntimeStmtOrigin, RuntimeStmtSite,
        RuntimeTerminatorOrigin, RuntimeTerminatorSite,
    },
};
use hir::{
    analysis::{
        semantic::{get_or_build_semantic_instance, root_semantic_instance_key},
        ty::ty_check::BodyOwner,
    },
    hir_def::{Func, TopLevelMod},
};
use url::Url;

fn origin_key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
    OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
}

fn find_func<'db>(db: &'db DriverDataBase, top_mod: TopLevelMod<'db>, name: &str) -> Func<'db> {
    top_mod
        .all_funcs(db)
        .iter()
        .copied()
        .find(|func| {
            func.name(db)
                .to_opt()
                .is_some_and(|ident| ident.data(db) == name)
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

fn runtime_instance_for_func<'db>(
    db: &'db DriverDataBase,
    func: Func<'db>,
) -> fe_mir::RuntimeInstance<'db> {
    let semantic_key = root_semantic_instance_key(db, BodyOwner::Func(func))
        .expect("fixture function should have a root semantic instance key");
    let semantic = get_or_build_semantic_instance(db, semantic_key);
    let runtime_key =
        RuntimeInstanceKey::new(db, RuntimeInstanceSource::Semantic(semantic), Vec::new());
    get_or_build_runtime_instance(db, runtime_key)
}

#[test]
fn runtime_origin_export_keys_include_kind_owner_and_local_identity() {
    let mut db = DriverDataBase::default();
    let file_url = Url::parse("file:///origin_runtime_export_keys.fe").unwrap();
    let file = db.workspace().touch(
        &mut db,
        file_url,
        Some(
            r#"
fn test_origin_keys() -> u256 {
    1
}
"#
            .to_string(),
        ),
    );
    let top_mod = db.top_mod(file);
    let instance = runtime_instance_for_func(&db, find_func(&db, top_mod, "test_origin_keys"));
    let block = RBlockId::from_u32(3);
    let stmt_site = RuntimeStmtSite::new(block, RuntimeStmtIndex::from_u32(5));
    let terminator_site = RuntimeTerminatorSite::new(block);
    let owner_key = RuntimeInstanceOwnerKey::new("runtime:test");

    let stmt_key = RuntimeStmtOrigin::new(instance, stmt_site).export_key(&owner_key);
    let terminator_key =
        RuntimeTerminatorOrigin::new(instance, terminator_site).export_key(&owner_key);

    assert_ne!(stmt_key, terminator_key);
    assert_eq!(
        stmt_key,
        origin_key("runtime.stmt", "runtime:test", "block:3:stmt:5")
    );
    assert_eq!(
        terminator_key,
        origin_key("runtime.terminator", "runtime:test", "block:3:terminator")
    );
}

#[test]
fn runtime_instance_owner_key_rejects_invalid_text() {
    assert!(RuntimeInstanceOwnerKey::try_new("").is_err());
    assert!(RuntimeInstanceOwnerKey::try_new("runtime\u{1f}test").is_err());
}
