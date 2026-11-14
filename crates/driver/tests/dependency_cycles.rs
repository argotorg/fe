use std::fs;
use std::path::Path;

use fe_driver::{DriverDataBase, init_ingot, init_workspace};
use url::Url;

fn write_ingot(root: &Path, name: &str, dependencies: &str) {
    let dir = root.join(name);
    fs::create_dir_all(dir.join("src")).unwrap();
    fs::write(
        dir.join("fe.toml"),
        format!("[ingot]\nname = \"{name}\"\nversion = \"0.1.0\"\n{dependencies}"),
    )
    .unwrap();
    fs::write(dir.join("src/lib.fe"), "pub fn main() {}\n").unwrap();
}

/// A database can hold many ingots, as in the language server, which loads every
/// member of a workspace into one. A cycle between two of them is reported for
/// the ingots that reach it only, and must not stop another ingot from loading.
#[test]
fn a_dependency_cycle_does_not_stop_an_unrelated_ingot_from_loading() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().canonicalize().unwrap();
    write_ingot(&root, "a", "\n[dependencies]\nb = { path = \"../b\" }\n");
    write_ingot(&root, "b", "\n[dependencies]\na = { path = \"../a\" }\n");
    write_ingot(&root, "c", "");
    let url = |name: &str| Url::from_directory_path(root.join(name)).unwrap();

    let mut db = DriverDataBase::default();
    assert!(
        init_ingot(&mut db, &url("a")),
        "the cycle was not reported for an ingot in it"
    );
    assert!(
        !init_ingot(&mut db, &url("c")),
        "the cycle was reported for an ingot that does not reach it"
    );
}

/// An ingot that depends on a cycle without being part of it is told about the
/// cycle too, because its dependencies cannot be built.
#[test]
fn an_ingot_that_depends_on_a_cycle_is_told_about_it() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().canonicalize().unwrap();
    write_ingot(&root, "a", "\n[dependencies]\nb = { path = \"../b\" }\n");
    write_ingot(&root, "b", "\n[dependencies]\na = { path = \"../a\" }\n");
    write_ingot(&root, "d", "\n[dependencies]\na = { path = \"../a\" }\n");

    let mut db = DriverDataBase::default();
    assert!(
        init_ingot(&mut db, &Url::from_directory_path(root.join("d")).unwrap()),
        "the cycle was not reported for an ingot that depends on it"
    );
}

/// Members of a workspace can depend on each other. A cycle between them is
/// reported when the workspace is loaded.
#[test]
fn a_workspace_with_a_cycle_between_members_loads_and_reports_the_cycle() {
    let temp = tempfile::tempdir().unwrap();
    let root = temp.path().canonicalize().unwrap();
    fs::write(
        root.join("fe.toml"),
        "[workspace]\nmembers = [\"a\", \"b\", \"c\"]\n",
    )
    .unwrap();
    write_ingot(&root, "a", "\n[dependencies]\nb = { path = \"../b\" }\n");
    write_ingot(&root, "b", "\n[dependencies]\na = { path = \"../a\" }\n");
    write_ingot(&root, "c", "");

    let mut db = DriverDataBase::default();
    assert!(
        init_workspace(&mut db, &Url::from_directory_path(&root).unwrap()),
        "the cycle between members was not reported"
    );
}
