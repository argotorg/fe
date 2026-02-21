use std::fs;
use std::process::Command;

use camino::Utf8PathBuf;
use common::InputDb;
use common::cache::remote_git_cache_dir;
use common::file::IngotFileKind;
use fe_driver::{DriverDataBase, init_ingot, init_workspace};
use resolver::git::{GitDescription, GitResolver};
use tempfile::TempDir;
use url::Url;

fn write_file(path: &Utf8PathBuf, content: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent.as_std_path()).unwrap();
    }
    fs::write(path.as_std_path(), content).unwrap();
}

fn git_commit(repo: &Utf8PathBuf, message: &str) -> String {
    Command::new("git")
        .arg("-C")
        .arg(repo.as_std_path())
        .arg("add")
        .arg(".")
        .status()
        .expect("git add")
        .success()
        .then_some(())
        .expect("git add success");
    Command::new("git")
        .arg("-C")
        .arg(repo.as_std_path())
        .args(["commit", "-m", message])
        .status()
        .expect("git commit")
        .success()
        .then_some(())
        .expect("git commit success");

    let output = Command::new("git")
        .arg("-C")
        .arg(repo.as_std_path())
        .args(["rev-parse", "HEAD"])
        .output()
        .expect("git rev-parse");
    assert!(output.status.success());
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

static REMOTE_CACHE_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn with_remote_cache_dir<T>(cache_root: &Utf8PathBuf, f: impl FnOnce() -> T) -> T {
    let _guard = REMOTE_CACHE_LOCK.lock().expect("remote cache lock");
    let previous = std::env::var("FE_REMOTE_CACHE_DIR").ok();
    unsafe {
        std::env::set_var("FE_REMOTE_CACHE_DIR", cache_root.as_str());
    }
    let result = f();
    unsafe {
        match previous {
            Some(value) => std::env::set_var("FE_REMOTE_CACHE_DIR", value),
            None => std::env::remove_var("FE_REMOTE_CACHE_DIR"),
        }
    }
    result
}

#[cfg(unix)]
#[test]
fn resolves_workspace_member_by_name_local_through_symlinked_workspace_root() {
    use std::os::unix::fs::symlink;

    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let workspace_link = root.join("workspace_link");
    let ingot_a = workspace_root.join("ingots/a");
    let ingot_b = workspace_root.join("ingots/b");

    fs::create_dir_all(workspace_root.as_std_path()).unwrap();
    symlink(workspace_root.as_std_path(), workspace_link.as_std_path()).unwrap();

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "local-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a", version = "0.1.0" },
  { path = "ingots/b", name = "b", version = "0.1.0" },
]
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"

[dependencies]
b = true
"#,
    );
    write_file(&ingot_a.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &ingot_b.join("fe.toml"),
        r#"
[ingot]
name = "b"
version = "0.1.0"
"#,
    );
    write_file(&ingot_b.join("src/lib.fe"), "pub fn main() {}\n");

    let workspace_url =
        Url::from_directory_path(workspace_link.as_std_path()).expect("workspace url");
    let ingot_a_url = Url::from_directory_path(workspace_link.join("ingots/a").as_std_path())
        .expect("ingot a url");
    let ingot_b_url = Url::from_directory_path(workspace_link.join("ingots/b").as_std_path())
        .expect("ingot b url");

    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(!had_diagnostics, "unexpected diagnostics");

    let deps = db.dependency_graph().dependency_urls(&db, &ingot_a_url);
    assert!(deps.contains(&ingot_b_url));
}

#[test]
fn resolves_workspace_member_by_name_local() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let ingot_a = workspace_root.join("ingots/a");
    let ingot_b = workspace_root.join("ingots/b");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "local-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a", version = "0.1.0" },
  { path = "ingots/b", name = "b", version = "0.1.0" },
]
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"

[dependencies]
b = true
"#,
    );
    write_file(&ingot_a.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &ingot_b.join("fe.toml"),
        r#"
[ingot]
name = "b"
version = "0.1.0"
"#,
    );
    write_file(&ingot_b.join("src/lib.fe"), "pub fn main() {}\n");

    let workspace_url =
        Url::from_directory_path(workspace_root.as_std_path()).expect("workspace url");
    let ingot_a_url = Url::from_directory_path(ingot_a.as_std_path()).expect("ingot a url");
    let ingot_b_url = Url::from_directory_path(ingot_b.as_std_path()).expect("ingot b url");

    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(!had_diagnostics, "unexpected diagnostics");

    let deps = db.dependency_graph().dependency_urls(&db, &ingot_a_url);
    assert!(deps.contains(&ingot_b_url));
}

#[test]
fn resolves_workspace_member_by_version_string_local() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let ingot_a = workspace_root.join("ingots/a");
    let ingot_b = workspace_root.join("ingots/b");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "local-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a", version = "0.1.0" },
  { path = "ingots/b", name = "b", version = "0.1.0" },
]
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"

[dependencies]
b = "0.1.0"
"#,
    );
    write_file(&ingot_a.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &ingot_b.join("fe.toml"),
        r#"
[ingot]
name = "b"
version = "0.1.0"
"#,
    );
    write_file(&ingot_b.join("src/lib.fe"), "pub fn main() {}\n");

    let workspace_url =
        Url::from_directory_path(workspace_root.as_std_path()).expect("workspace url");
    let ingot_a_url = Url::from_directory_path(ingot_a.as_std_path()).expect("ingot a url");
    let ingot_b_url = Url::from_directory_path(ingot_b.as_std_path()).expect("ingot b url");

    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(!had_diagnostics, "unexpected diagnostics");

    let deps = db.dependency_graph().dependency_urls(&db, &ingot_a_url);
    assert!(deps.contains(&ingot_b_url));
}

#[test]
fn resolves_workspace_dependency_by_alias_local() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let ingot_a = workspace_root.join("ingots/a");
    let ingot_b = workspace_root.join("ingots/b");
    let util = workspace_root.join("vendor/util");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "local-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a", version = "0.1.0" },
  { path = "ingots/b", name = "b", version = "0.1.0" },
]

[dependencies]
util = { path = "vendor/util", name = "util", version = "0.1.0" }
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"

[dependencies]
b = true
util = true
"#,
    );
    write_file(&ingot_a.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &ingot_b.join("fe.toml"),
        r#"
[ingot]
name = "b"
version = "0.1.0"
"#,
    );
    write_file(&ingot_b.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &util.join("fe.toml"),
        r#"
[ingot]
name = "util"
version = "0.1.0"
"#,
    );
    write_file(&util.join("src/lib.fe"), "pub fn main() {}\n");

    let workspace_url =
        Url::from_directory_path(workspace_root.as_std_path()).expect("workspace url");
    let ingot_a_url = Url::from_directory_path(ingot_a.as_std_path()).expect("ingot a url");
    let ingot_b_url = Url::from_directory_path(ingot_b.as_std_path()).expect("ingot b url");
    let util_url = Url::from_directory_path(util.as_std_path()).expect("util url");

    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(!had_diagnostics, "unexpected diagnostics");

    let deps = db.dependency_graph().dependency_urls(&db, &ingot_a_url);
    assert!(deps.contains(&ingot_b_url));
    assert!(deps.contains(&util_url));
}

#[test]
fn rejects_workspace_with_duplicate_member_names() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let member_a = workspace_root.join("ingots/lib_v1");
    let member_b = workspace_root.join("ingots/lib_v2");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "duplicate-members"
version = "0.1.0"
members = [
  { path = "ingots/lib_v1", name = "lib" },
  { path = "ingots/lib_v2", name = "lib" },
]
"#,
    );

    write_file(
        &member_a.join("fe.toml"),
        r#"
[ingot]
name = "lib"
version = "0.1.0"
"#,
    );
    write_file(
        &member_a.join("src/lib.fe"),
        "pub fn add(a: u256, b: u256) -> u256 { a + b }\n",
    );

    write_file(
        &member_b.join("fe.toml"),
        r#"
[ingot]
name = "lib"
version = "0.2.0"
"#,
    );
    write_file(
        &member_b.join("src/lib.fe"),
        "pub fn add(a: u256, b: u256) -> u256 { a + b }\n",
    );

    let workspace_url =
        Url::from_directory_path(workspace_root.as_std_path()).expect("workspace url");
    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(had_diagnostics, "expected duplicate member diagnostics");
}

#[test]
fn rejects_workspace_dependency_alias_conflicting_with_member_name() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let ingot_a = workspace_root.join("ingots/a");
    let ingot_b = workspace_root.join("ingots/b");
    let dep_b = workspace_root.join("vendor/dep_b");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "local-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a", version = "0.1.0" },
  { path = "ingots/b", name = "b", version = "0.1.0" },
]

[dependencies]
b = { path = "vendor/dep_b", name = "dep_b", version = "0.1.0" }
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"
"#,
    );
    write_file(&ingot_a.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &ingot_b.join("fe.toml"),
        r#"
[ingot]
name = "b"
version = "0.1.0"
"#,
    );
    write_file(&ingot_b.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &dep_b.join("fe.toml"),
        r#"
[ingot]
name = "dep_b"
version = "0.1.0"
"#,
    );
    write_file(&dep_b.join("src/lib.fe"), "pub fn main() {}\n");

    let workspace_url =
        Url::from_directory_path(workspace_root.as_std_path()).expect("workspace url");
    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(had_diagnostics, "expected diagnostics");
}

#[test]
fn rejects_workspace_member_name_mismatch() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let ingot_a = workspace_root.join("ingots/a");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "mismatch-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a" },
]
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "not_a"
version = "0.1.0"
"#,
    );

    let workspace_url =
        Url::from_directory_path(workspace_root.as_std_path()).expect("workspace url");
    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(had_diagnostics, "expected metadata mismatch diagnostics");
}

#[test]
fn rejects_workspace_member_version_mismatch() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let ingot_a = workspace_root.join("ingots/a");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
[workspace]
name = "mismatch-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", version = "9.9.9" },
]
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"
"#,
    );

    let workspace_url =
        Url::from_directory_path(workspace_root.as_std_path()).expect("workspace url");
    let mut db = DriverDataBase::default();
    let had_diagnostics = init_workspace(&mut db, &workspace_url);
    assert!(had_diagnostics, "expected metadata mismatch diagnostics");
}

#[test]
fn resolves_remote_workspace_member_by_name() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_repo = root.join("remote_ws");
    let member_path = workspace_repo.join("ingots/core");

    fs::create_dir_all(workspace_repo.as_std_path()).unwrap();
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .arg("init")
        .status()
        .expect("git init")
        .success()
        .then_some(())
        .expect("git init success");
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .args(["config", "user.email", "fe@example.com"])
        .status()
        .expect("git config email")
        .success()
        .then_some(())
        .expect("git config email success");
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .args(["config", "user.name", "fe"])
        .status()
        .expect("git config name")
        .success()
        .then_some(())
        .expect("git config name success");

    write_file(
        &workspace_repo.join("fe.toml"),
        r#"
name = "remote-workspace"
version = "0.1.0"
members = [
  { path = "ingots/core", name = "core", version = "0.1.0" },
]
"#,
    );
    write_file(
        &member_path.join("fe.toml"),
        r#"
[ingot]
name = "core"
version = "0.1.0"
"#,
    );
    write_file(&member_path.join("src/lib.fe"), "pub fn main() {}\n");

    let rev = git_commit(&workspace_repo, "initial");
    let source_url = Url::from_directory_path(workspace_repo.as_std_path())
        .expect("repo url")
        .to_string();

    let cache_root = root.join("git-cache");
    fs::create_dir_all(cache_root.as_std_path()).unwrap();

    let local_root = root.join("consumer");
    let local_ingot = local_root.join("ingot");
    write_file(
        &local_ingot.join("fe.toml"),
        &format!(
            r#"
[ingot]
name = "consumer"
version = "0.1.0"

[dependencies]
core = {{ source = "{}", rev = "{}", name = "core", version = "0.1.0" }}
"#,
            source_url, rev
        ),
    );
    write_file(&local_ingot.join("src/lib.fe"), "pub fn main() {}\n");

    with_remote_cache_dir(&cache_root, || {
        let ingot_url = Url::from_directory_path(local_ingot.as_std_path()).expect("ingot url");
        let mut db = DriverDataBase::default();
        let had_diagnostics = init_ingot(&mut db, &ingot_url);
        assert!(!had_diagnostics, "unexpected diagnostics");

        let cache_root = remote_git_cache_dir().expect("cache dir");
        let git = GitResolver::new(cache_root);
        let description =
            GitDescription::new(Url::parse(&source_url).expect("source url"), rev.clone());
        let checkout_path = git.checkout_path(&description);
        let member_url = Url::from_directory_path(checkout_path.join("ingots/core").as_std_path())
            .expect("member url");

        let deps = db.dependency_graph().dependency_urls(&db, &ingot_url);
        assert!(deps.contains(&member_url));
        let ingot = db
            .workspace()
            .containing_ingot(&db, member_url)
            .expect("expected member ingot");
        let has_source_files = ingot
            .files(&db)
            .iter()
            .any(|(_, file)| matches!(file.kind(&db), Some(IngotFileKind::Source)));
        assert!(
            has_source_files,
            "expected member ingot to load source files"
        );
    });
}

#[test]
fn resolves_remote_workspace_member_by_name_in_subdir() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_repo = root.join("remote_ws");
    let workspace_root = workspace_repo.join("workspace");
    let member_path = workspace_root.join("ingots/core");

    fs::create_dir_all(workspace_root.as_std_path()).unwrap();
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .arg("init")
        .status()
        .expect("git init")
        .success()
        .then_some(())
        .expect("git init success");
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .args(["config", "user.email", "fe@example.com"])
        .status()
        .expect("git config email")
        .success()
        .then_some(())
        .expect("git config email success");
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .args(["config", "user.name", "fe"])
        .status()
        .expect("git config name")
        .success()
        .then_some(())
        .expect("git config name success");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
name = "remote-workspace"
version = "0.1.0"
members = [
  { path = "ingots/core", name = "core", version = "0.1.0" },
]
"#,
    );
    write_file(
        &member_path.join("fe.toml"),
        r#"
[ingot]
name = "core"
version = "0.1.0"
"#,
    );
    write_file(&member_path.join("src/lib.fe"), "pub fn main() {}\n");

    let rev = git_commit(&workspace_repo, "initial");
    let source_url = Url::from_directory_path(workspace_repo.as_std_path())
        .expect("repo url")
        .to_string();

    let cache_root = root.join("git-cache");
    fs::create_dir_all(cache_root.as_std_path()).unwrap();

    let local_root = root.join("consumer");
    let local_ingot = local_root.join("ingot");
    write_file(
        &local_ingot.join("fe.toml"),
        &format!(
            r#"
[ingot]
name = "consumer"
version = "0.1.0"

[dependencies]
core = {{ source = "{}", rev = "{}", path = "workspace", name = "core", version = "0.1.0" }}
"#,
            source_url, rev
        ),
    );
    write_file(&local_ingot.join("src/lib.fe"), "pub fn main() {}\n");

    with_remote_cache_dir(&cache_root, || {
        let ingot_url = Url::from_directory_path(local_ingot.as_std_path()).expect("ingot url");
        let mut db = DriverDataBase::default();
        let had_diagnostics = init_ingot(&mut db, &ingot_url);
        assert!(!had_diagnostics, "unexpected diagnostics");

        let cache_root = remote_git_cache_dir().expect("cache dir");
        let git = GitResolver::new(cache_root);
        let description =
            GitDescription::new(Url::parse(&source_url).expect("source url"), rev.clone());
        let checkout_path = git.checkout_path(&description);
        let member_url =
            Url::from_directory_path(checkout_path.join("workspace/ingots/core").as_std_path())
                .expect("member url");

        let deps = db.dependency_graph().dependency_urls(&db, &ingot_url);
        assert!(deps.contains(&member_url));
        let ingot = db
            .workspace()
            .containing_ingot(&db, member_url)
            .expect("expected member ingot");
        let has_source_files = ingot
            .files(&db)
            .iter()
            .any(|(_, file)| matches!(file.kind(&db), Some(IngotFileKind::Source)));
        assert!(
            has_source_files,
            "expected member ingot to load source files"
        );
    });
}

#[test]
fn resolves_remote_workspace_member_dependency_urls() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_repo = root.join("remote_ws");
    let member_path = workspace_repo.join("ingots/core");

    fs::create_dir_all(workspace_repo.as_std_path()).unwrap();
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .arg("init")
        .status()
        .expect("git init")
        .success()
        .then_some(())
        .expect("git init success");
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .args(["config", "user.email", "fe@example.com"])
        .status()
        .expect("git config email")
        .success()
        .then_some(())
        .expect("git config email success");
    Command::new("git")
        .arg("-C")
        .arg(workspace_repo.as_std_path())
        .args(["config", "user.name", "fe"])
        .status()
        .expect("git config name")
        .success()
        .then_some(())
        .expect("git config name success");

    write_file(
        &workspace_repo.join("fe.toml"),
        r#"
name = "remote-workspace"
version = "0.1.0"
members = [
  { path = "ingots/core", name = "core", version = "0.1.0" },
]
"#,
    );
    write_file(
        &member_path.join("fe.toml"),
        r#"
[ingot]
name = "core"
version = "0.1.0"
"#,
    );
    write_file(&member_path.join("src/lib.fe"), "pub fn main() {}\n");

    let rev = git_commit(&workspace_repo, "initial");
    let source_url = Url::from_directory_path(workspace_repo.as_std_path())
        .expect("repo url")
        .to_string();

    let cache_root = root.join("git-cache");
    fs::create_dir_all(cache_root.as_std_path()).unwrap();

    let local_root = root.join("consumer");
    let local_ingot = local_root.join("ingot");
    write_file(
        &local_ingot.join("fe.toml"),
        &format!(
            r#"
[ingot]
name = "consumer"
version = "0.1.0"

[dependencies]
remote_core = {{ source = "{}", rev = "{}", name = "core", version = "0.1.0" }}
"#,
            source_url, rev
        ),
    );
    write_file(&local_ingot.join("src/lib.fe"), "pub fn main() {}\n");

    with_remote_cache_dir(&cache_root, || {
        let ingot_url = Url::from_directory_path(local_ingot.as_std_path()).expect("ingot url");
        let mut db = DriverDataBase::default();
        let had_diagnostics = init_ingot(&mut db, &ingot_url);
        assert!(!had_diagnostics, "unexpected diagnostics");

        let cache_root = remote_git_cache_dir().expect("cache dir");
        let git = GitResolver::new(cache_root);
        let description =
            GitDescription::new(Url::parse(&source_url).expect("source url"), rev.clone());
        let checkout_path = git.checkout_path(&description);
        let member_url = Url::from_directory_path(checkout_path.join("ingots/core").as_std_path())
            .expect("member url");

        let ingot = db
            .workspace()
            .containing_ingot(&db, ingot_url.clone())
            .expect("expected consumer ingot");
        let deps = ingot.dependencies(&db);
        assert!(
            deps.iter()
                .any(|(name, url)| name == "remote_core" && url == &member_url)
        );
    });
}

#[test]
fn reports_workspace_dependency_without_selection() {
    let temp = TempDir::new().unwrap();
    let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
    let workspace_root = root.join("workspace");
    let ingot_a = workspace_root.join("ingots/a");
    let ingot_b = workspace_root.join("ingots/b");

    write_file(
        &workspace_root.join("fe.toml"),
        r#"
name = "local-workspace"
version = "0.1.0"
members = [
  { path = "ingots/a", name = "a", version = "0.1.0" },
  { path = "ingots/b", name = "b", version = "0.1.0" },
]
"#,
    );

    write_file(
        &ingot_a.join("fe.toml"),
        r#"
[ingot]
name = "a"
version = "0.1.0"

[dependencies]
workspace = { path = "../.." }
"#,
    );
    write_file(&ingot_a.join("src/lib.fe"), "pub fn main() {}\n");

    write_file(
        &ingot_b.join("fe.toml"),
        r#"
[ingot]
name = "b"
version = "0.1.0"
"#,
    );
    write_file(&ingot_b.join("src/lib.fe"), "pub fn main() {}\n");

    let ingot_a_url = Url::from_directory_path(ingot_a.as_std_path()).expect("ingot a url");
    let mut db = DriverDataBase::default();
    let had_diagnostics = init_ingot(&mut db, &ingot_a_url);
    assert!(had_diagnostics, "expected diagnostics");
}
