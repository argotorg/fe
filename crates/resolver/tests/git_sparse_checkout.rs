#![cfg(not(target_arch = "wasm32"))]

use std::{fs, process::Command};

use camino::Utf8PathBuf;
use fe_resolver::git::{GitDescription, GitResolver, SparseCheckoutMode};
use tempfile::tempdir;
use url::Url;

fn run_git(repo_dir: &std::path::Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo_dir)
        .args(args)
        .output()
        .expect("failed to run git command");
    assert!(
        output.status.success(),
        "git {:?} failed: {}",
        args,
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).to_string()
}

fn init_test_repo() -> (tempfile::TempDir, String, Url) {
    let repo_dir = tempdir().expect("failed to create temp repo dir");
    run_git(repo_dir.path(), &["init"]);
    run_git(
        repo_dir.path(),
        &["config", "user.email", "test@example.com"],
    );
    run_git(repo_dir.path(), &["config", "user.name", "Test User"]);

    let keep_dir = repo_dir.path().join("keep");
    fs::create_dir_all(&keep_dir).expect("failed to create keep dir");
    fs::write(keep_dir.join("fe.toml"), "name = \"keep\"\n").expect("failed to write keep fe.toml");

    let other_dir = repo_dir.path().join("other");
    fs::create_dir_all(&other_dir).expect("failed to create other dir");
    fs::write(other_dir.join("other.txt"), "nope\n").expect("failed to write other.txt");

    run_git(repo_dir.path(), &["add", "."]);
    run_git(repo_dir.path(), &["commit", "-m", "init"]);
    let rev = run_git(repo_dir.path(), &["rev-parse", "HEAD"])
        .trim()
        .to_string();

    let source =
        Url::from_directory_path(repo_dir.path()).expect("failed to build file URL for repo");

    (repo_dir, rev, source)
}

#[test]
fn sparse_checkout_only_materializes_requested_path() {
    if Command::new("git").arg("--version").output().is_err() {
        return;
    }

    let (_repo_dir, rev, source) = init_test_repo();
    let checkout_root_dir = tempdir().expect("failed to create temp checkout root");
    let checkout_root = Utf8PathBuf::from_path_buf(checkout_root_dir.path().to_path_buf())
        .expect("checkout root is not valid utf-8");
    let description = GitDescription::new(source, rev).with_path("keep");
    let resolver = GitResolver::new(checkout_root);
    let resource = resolver
        .ensure_sparse_checkout(
            &description,
            &[String::from("keep")],
            SparseCheckoutMode::Cone,
        )
        .expect("sparse checkout failed");

    let checkout_path = resource.checkout_path;
    assert!(checkout_path.join("keep/fe.toml").exists());
    assert!(!checkout_path.join("other/other.txt").exists());
}

#[test]
fn sparse_checkout_preserves_existing_patterns() {
    if Command::new("git").arg("--version").output().is_err() {
        return;
    }

    let (_repo_dir, rev, source) = init_test_repo();
    let checkout_root_dir = tempdir().expect("failed to create temp checkout root");
    let checkout_root = Utf8PathBuf::from_path_buf(checkout_root_dir.path().to_path_buf())
        .expect("checkout root is not valid utf-8");
    let resolver = GitResolver::new(checkout_root);

    let keep_description = GitDescription::new(source.clone(), rev.clone()).with_path("keep");
    let keep_resource = resolver
        .ensure_sparse_checkout(
            &keep_description,
            &[String::from("keep")],
            SparseCheckoutMode::Cone,
        )
        .expect("sparse checkout failed");

    assert!(keep_resource.checkout_path.join("keep/fe.toml").exists());
    assert!(!keep_resource.checkout_path.join("other/other.txt").exists());
    assert!(resolver.has_valid_cached_checkout(&keep_description));

    let other_description = GitDescription::new(source, rev).with_path("other");
    assert!(!resolver.has_valid_cached_checkout(&other_description));

    let other_resource = resolver
        .ensure_sparse_checkout(
            &other_description,
            &[String::from("other")],
            SparseCheckoutMode::Cone,
        )
        .expect("sparse checkout failed");

    assert!(other_resource.checkout_path.join("keep/fe.toml").exists());
    assert!(
        other_resource
            .checkout_path
            .join("other/other.txt")
            .exists()
    );
    assert!(resolver.has_valid_cached_checkout(&other_description));
}
