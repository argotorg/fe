use dir_test::{Fixture, dir_test};
use fe_fmt::{Config, format_str};
use test_utils::snap_test;

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures",
    glob: "*.fe"
)]
fn format_snap(fixture: Fixture<&str>) {
    let config = Config::default();
    let output = match format_str(fixture.content(), &config) {
        Ok(formatted) => formatted,
        Err(err) => format!("FORMAT ERROR: {err:?}"),
    };

    snap_test!(output, fixture.path());
}

