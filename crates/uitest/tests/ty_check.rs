use common::InputDb;
use driver::DriverDataBase;
use test_utils::{fixture_shards, fixtures::Fixture, snap_test};
use url::Url;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

fixture_shards!(
    ty_check,
    concat!(env!("CARGO_MANIFEST_DIR"), "/fixtures/ty_check"),
    "**/*.fe",
    DriverDataBase::default,
    run_ty_check,
);

fn run_ty_check(db: &mut DriverDataBase, fixture: &Fixture) {
    let diags = diagnostics(db, fixture.path(), fixture.content());
    snap_test!(diags, fixture.path());
}

/// The diagnostics of a program checked as a standalone file.
fn diagnostics(db: &mut DriverDataBase, path: &str, content: &str) -> String {
    let file = db.workspace().touch(
        db,
        Url::from_file_path(path).expect("path should be absolute"),
        Some(content.to_string()),
    );
    let top_mod = db.top_mod(file);
    db.run_on_top_mod(top_mod).format_diags(db)
}

/// The error headlines of a program checked as a standalone file, in order.
#[cfg(not(target_arch = "wasm32"))]
fn error_headlines(db: &mut DriverDataBase, path: &str, content: &str) -> Vec<String> {
    diagnostics(db, path, content)
        .lines()
        .filter(|line| line.starts_with("error[") || line.starts_with("warning["))
        .map(str::to_string)
        .collect()
}

// `fe fmt` keeps the meaning of the const `where` fixtures: formatting twice
// gives the same text as formatting once, and the formatted program reports
// the same errors. Fixtures that do not parse are left alone by `fe fmt`.
#[cfg(not(target_arch = "wasm32"))]
fixture_shards!(
    formatting_keeps_meaning,
    concat!(env!("CARGO_MANIFEST_DIR"), "/fixtures/ty_check/const_where"),
    "**/*.fe",
    DriverDataBase::default,
    check_formatting_keeps_meaning,
);

#[cfg(not(target_arch = "wasm32"))]
fn check_formatting_keeps_meaning(db: &mut DriverDataBase, fixture: &Fixture) {
    let config = fmt::Config::default();
    let content = fixture.content();
    let Ok(formatted) = fmt::format_str(content, &config) else {
        return;
    };
    let reformatted = fmt::format_str(&formatted, &config).expect("formatted source should parse");
    assert_eq!(formatted, reformatted, "formatting twice changed the text");
    assert_eq!(
        error_headlines(db, fixture.path(), content),
        error_headlines(db, fixture.path(), &formatted),
        "formatting changed the errors:\n{formatted}"
    );
}

#[cfg(target_family = "wasm")]
mod wasm {
    use super::*;
    use dir_test::{Fixture, dir_test};
    use test_utils::url_utils::UrlExt;
    use url::Url;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/fixtures/ty_check",
        glob: "*.fe",
        postfix: "wasm"
    )]
    #[dir_test_attr(
        #[wasm_bindgen_test]
    )]
    fn run_ty_check(fixture: Fixture<&str>) {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            <Url as UrlExt>::from_file_path_lossy(fixture.path()),
            Some(fixture.content().to_string()),
        );

        let top_mod = db.top_mod(file);
        db.run_on_top_mod(top_mod);
    }
}
