use common::InputDb;
use driver::{DriverDataBase, generation::prepare_file};
use fmt::{Config, format_str};
use test_utils::{fixture_shards, fixtures::Fixture};
use url::Url;

fixture_shards!(
    fe_test_fixtures,
    concat!(env!("CARGO_MANIFEST_DIR"), "/tests/fixtures/fe_test"),
    "*.fe",
    DriverDataBase::default,
    check_semantic_roundtrip,
);

fn check_semantic_roundtrip(db: &mut DriverDataBase, fixture: &Fixture) {
    let formatted = format_str(fixture.content(), &Config::default())
        .unwrap_or_else(|err| panic!("format failed for {}: {err:?}", fixture.path()));

    let file_url = Url::from_file_path(fixture.path())
        .unwrap_or_else(|_| panic!("fixture path should be absolute: {}", fixture.path()));
    let file = db.workspace().touch(db, file_url.clone(), Some(formatted));
    let prepared = prepare_file(db, file).unwrap_or_else(|err| {
        panic!(
            "formatted output failed generation for {}: {err:?}",
            fixture.path()
        )
    });
    let db = prepared.as_ref().unwrap_or(&*db);
    let file = db.workspace().get(db, &file_url).unwrap();
    let top_mod = db.top_mod(file);

    let diagnostics = db.run_on_top_mod(top_mod);
    assert!(
        diagnostics.is_empty(),
        "formatted output failed parse/HIR analysis for {}:\n{}",
        fixture.path(),
        diagnostics.format_diags(db),
    );
}
