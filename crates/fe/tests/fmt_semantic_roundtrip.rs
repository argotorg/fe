use common::InputDb;
use dir_test::{Fixture, dir_test};
use driver::DriverDataBase;
use fmt::{Config, format_str};
use mir::lower_module;
use url::Url;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/fe_test",
    glob: "*.fe",
)]
fn test_fmt_fe_test_fixtures_semantic_roundtrip(fixture: Fixture<&str>) {
    let formatted = format_str(fixture.content(), &Config::default())
        .unwrap_or_else(|err| panic!("format failed for {}: {err:?}", fixture.path()));

    let mut db = DriverDataBase::default();
    let file_url = Url::from_file_path(fixture.path())
        .unwrap_or_else(|_| panic!("fixture path should be absolute: {}", fixture.path()));
    let file = db.workspace().touch(&mut db, file_url, Some(formatted));
    let top_mod = db.top_mod(file);

    let diagnostics = db.run_on_top_mod(top_mod);
    assert!(
        diagnostics.is_empty(),
        "formatted output failed parse/HIR analysis for {}:\n{}",
        fixture.path(),
        diagnostics.format_diags(&db),
    );

    if let Err(err) = lower_module(&db, top_mod) {
        panic!(
            "formatted output failed MIR lowering/analysis for {}:\n{}",
            fixture.path(),
            err,
        );
    }
}
