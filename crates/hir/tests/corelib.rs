use std::{fs, path::Path};

use common::InputDb;
use common::stdlib::{HasBuiltinCore, HasBuiltinStd};
use driver::{DriverDataBase, db::DiagnosticsCollection, init_ingot};
use tempfile::TempDir;
use url::Url;

#[cfg(target_arch = "wasm32")]
use test_utils::url_utils::UrlExt;

#[test]
fn analyze_corelib() {
    let db = DriverDataBase::default();
    let core = db.builtin_core();

    let core_diags = db.run_on_ingot(core);
    assert_builtin_clean(&db, core_diags, "core");
}

#[test]
fn analyze_stdlib() {
    let db = DriverDataBase::default();
    let std_ingot = db.builtin_std();

    let std_diags = db.run_on_ingot(std_ingot);
    assert_builtin_clean(&db, std_diags, "std");
}

fn assert_builtin_clean(db: &DriverDataBase, diags: DiagnosticsCollection<'_>, name: &str) {
    if diags.is_empty() {
        return;
    }

    diags.emit(db);
    panic!(
        "expected no diagnostics for builtin {name}, but got:\n{}",
        diags.format_diags(db)
    );
}

#[test]
fn analyze_custom_core_missing_effect_ref_does_not_panic_and_reports_requirement_diag() {
    let temp = TempDir::new().unwrap();
    write_file(
        &temp.path().join("fe.toml"),
        "[ingot]\nname = \"core\"\nversion = \"0.1.0\"\n",
    );
    write_file(
        &temp.path().join("src/lib.fe"),
        r#"
pub struct Ctx {}

fn exercise() uses (ctx: Ctx) {}
"#,
    );
    write_file(
        &temp.path().join("src/effect_ref.fe"),
        r#"
pub trait EffectRefMut<T>: EffectRef<T> {}
pub trait EffectHandle {
    type Target
}
"#,
    );

    let diags = analyze_ingot_diags(temp.path());
    assert!(
        diags.contains("missing required core trait `core::effect_ref::EffectRef`"),
        "expected missing EffectRef diagnostic, got:\n{diags}",
    );
}

#[test]
fn analyze_custom_core_parse_broken_effect_ref_reports_missing_traits_without_panicking() {
    let temp = TempDir::new().unwrap();
    write_file(
        &temp.path().join("fe.toml"),
        "[ingot]\nname = \"core\"\nversion = \"0.1.0\"\n",
    );
    write_file(
        &temp.path().join("src/lib.fe"),
        r#"
pub struct Ctx {}

fn exercise() uses (ctx: Ctx) {}
"#,
    );
    write_file(
        &temp.path().join("src/effect_ref.fe"),
        r#"
pub trait EffectRef<Targrevert();et> {}
pub trait EffectRefMut<T>: EffectRef<T> {}
"#,
    );

    let diags = analyze_ingot_diags(temp.path());
    assert!(
        diags.contains("unexpected syntax while parsing generic parameter list"),
        "expected parse diagnostic for malformed EffectRef, got:\n{diags}",
    );
    assert!(
        diags.contains("missing required core trait `core::effect_ref::EffectHandle`"),
        "expected missing EffectHandle diagnostic, got:\n{diags}",
    );
}

#[test]
fn analyze_custom_core_reports_invalid_div_signature_without_panicking() {
    let temp = TempDir::new().unwrap();
    write_file(
        &temp.path().join("fe.toml"),
        "[ingot]\nname = \"core\"\nversion = \"0.1.0\"\n",
    );
    write_file(
        &temp.path().join("src/lib.fe"),
        r#"
fn exercise(x: u8, y: u8) -> u8 {
    x / y
}
"#,
    );
    write_file(
        &temp.path().join("src/ops.fe"),
        r#"
pub trait Div<T = Self> {
    type Output = Self

    fn div(own self) -> Self::Output
}

impl Div for u8 {
    type Output = u8

    fn div(own self, _ other: own u8) -> Self::Output {
        self
    }
}
"#,
    );

    let diags = analyze_ingot_diags(temp.path());
    assert!(
        diags.contains(
            "invalid required core trait method signature `div` on `core::ops::Div`: expected 2 arguments, but 1 given"
        ),
        "expected invalid Div::div signature diagnostic, got:\n{diags}",
    );
}

#[test]
fn analyze_custom_core_reports_invalid_bitor_signature_without_panicking() {
    let temp = TempDir::new().unwrap();
    write_file(
        &temp.path().join("fe.toml"),
        "[ingot]\nname = \"core\"\nversion = \"0.1.0\"\n",
    );
    write_file(
        &temp.path().join("src/lib.fe"),
        r#"
fn exercise(x: bool, y: bool) -> bool {
    x | y
}
"#,
    );
    write_file(
        &temp.path().join("src/ops.fe"),
        r#"
pub trait BitOr<T = Self> {
    type Output = Self

    fn bitor(own self) -> Self::Output
}

impl BitOr for bool {
    type Output = bool

    fn bitor(own self, _ other: own bool) -> Self::Output {
        self
    }
}
"#,
    );

    let diags = analyze_ingot_diags(temp.path());
    assert!(
        diags.contains(
            "invalid required core trait method signature `bitor` on `core::ops::BitOr`: expected 2 arguments, but 1 given"
        ),
        "expected invalid BitOr::bitor signature diagnostic, got:\n{diags}",
    );
}

fn analyze_ingot_diags(root: &Path) -> String {
    let mut db = DriverDataBase::default();
    let root_url = Url::from_directory_path(root).expect("root url");
    let _ = init_ingot(&mut db, &root_url);
    let ingot = db
        .workspace()
        .containing_ingot(&db, root_url)
        .expect("ingot should load");
    db.run_on_ingot(ingot).format_diags(&db)
}

fn write_file(path: &Path, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, content).unwrap();
}
