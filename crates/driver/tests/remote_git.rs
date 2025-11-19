use std::{fs, process::Command, sync::Mutex};

use camino::Utf8PathBuf;
use common::InputDb;
use fe_driver::{DriverDataBase, init_ingot};
use tempfile::TempDir;
use url::Url;

static ENV_LOCK: Mutex<()> = Mutex::new(());

fn write_file(path: &Utf8PathBuf, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent.as_std_path()).unwrap();
    }
    fs::write(path.as_std_path(), contents).unwrap();
}

fn init_git_repo(path: &Utf8PathBuf) -> String {
    Command::new("git")
        .arg("init")
        .current_dir(path.as_std_path())
        .status()
        .expect("git init");
    Command::new("git")
        .args(["config", "user.email", "remote@example.com"])
        .current_dir(path.as_std_path())
        .status()
        .expect("git config email");
    Command::new("git")
        .args(["config", "user.name", "remote"])
        .current_dir(path.as_std_path())
        .status()
        .expect("git config name");
    Command::new("git")
        .args(["add", "."])
        .current_dir(path.as_std_path())
        .status()
        .expect("git add .");
    Command::new("git")
        .args(["commit", "-m", "init"])
        .current_dir(path.as_std_path())
        .status()
        .expect("git commit");
    let rev = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(path.as_std_path())
        .output()
        .expect("git rev-parse");
    String::from_utf8_lossy(&rev.stdout).trim().to_owned()
}

#[test]
fn remote_dependency_populates_workspace() {
    let _guard = ENV_LOCK.lock().unwrap();

    let cache_dir = TempDir::new().unwrap();
    let cache_path =
        Utf8PathBuf::from_path_buf(cache_dir.path().to_path_buf()).expect("utf8 cache path");
    let checkout_root = cache_path.join("git");
    fs::create_dir_all(checkout_root.as_std_path()).unwrap();
    unsafe { std::env::set_var("FE_REMOTE_CACHE_DIR", checkout_root.as_str()) };

    let remote_dir = TempDir::new().unwrap();
    let remote_path =
        Utf8PathBuf::from_path_buf(remote_dir.path().to_path_buf()).expect("utf8 remote path");

    write_file(
        &remote_path.join("fe.toml"),
        r#"[ingot]
name = "remote_pkg"
version = "0.1.0"
"#,
    );
    write_file(&remote_path.join("src/lib.fe"), r#"pub contract Remote{}"#);
    let rev = init_git_repo(&remote_path);
    let remote_source =
        Url::from_directory_path(remote_path.as_std_path()).expect("remote url from path");

    let workspace_dir = TempDir::new().unwrap();
    let workspace_path =
        Utf8PathBuf::from_path_buf(workspace_dir.path().to_path_buf()).expect("utf8 workspace");

    write_file(
        &workspace_path.join("fe.toml"),
        &format!(
            r#"[ingot]
name = "root"
version = "0.1.0"

[dependencies]
remote_pkg = {{ source = "{remote_source}", rev = "{rev}", path = "." }}
"#
        ),
    );
    write_file(&workspace_path.join("src/lib.fe"), r#"pub contract Root{}"#);

    let workspace_url =
        Url::from_directory_path(workspace_path.as_std_path()).expect("workspace url");

    let mut db = DriverDataBase::default();
    let diags = init_ingot(&mut db, &workspace_url);
    assert!(diags.is_empty(), "expected no diagnostics, found {diags:?}");

    let all_files: Vec<_> = db
        .workspace()
        .all_files(&db)
        .iter()
        .map(|(url, _)| url.clone())
        .collect();

    let remote_config_url = all_files
        .iter()
        .find(|url| {
            url.as_str().contains(checkout_root.as_str()) && url.path().ends_with("fe.toml")
        })
        .cloned()
        .expect("remote config registered");

    let remote_ingot = db
        .workspace()
        .containing_ingot(&db, remote_config_url.clone())
        .expect("remote ingot present");
    remote_ingot
        .root_file(&db)
        .expect("remote ingot has root file");

    unsafe { std::env::remove_var("FE_REMOTE_CACHE_DIR") };
}
