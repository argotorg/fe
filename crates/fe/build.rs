use std::{
    env,
    path::{Path, PathBuf},
};

fn main() {
    println!("cargo:rerun-if-changed=tests/fixtures");
    println!("cargo:rerun-if-changed=tests/doc_fixtures");
    println!("cargo:rerun-if-env-changed=FE_GIT_COMMIT");
    println!("cargo:rerun-if-env-changed=FE_GIT_HASH");

    let repo = git_repo();
    if let Some(repo) = &repo {
        emit_git_rerun_paths(repo);
    }

    let commit_override = env::var("FE_GIT_COMMIT")
        .ok()
        .map(|value| value.trim().to_string())
        .filter(|value| !value.is_empty());
    if let Some(commit) = commit_override.or_else(|| repo_head_commit(repo.as_ref())) {
        println!("cargo:rustc-env=FE_GIT_COMMIT={commit}");
    }

    if let Ok(override_hash) = env::var("FE_GIT_HASH")
        && !override_hash.trim().is_empty()
    {
        println!("cargo:rustc-env=FE_GIT_HASH={}", override_hash.trim());
        return;
    }

    let Some(repo) = repo else {
        return;
    };

    let Ok(head_id) = repo.head_id() else {
        return;
    };

    let hash = head_id.shorten_or_id();
    println!("cargo:rustc-env=FE_GIT_HASH={hash}");
}

fn repo_head_commit(repo: Option<&gix::Repository>) -> Option<String> {
    Some(repo?.head_id().ok()?.to_string())
}

fn git_repo() -> Option<gix::Repository> {
    let manifest_dir = env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
    let workspace_root = manifest_dir.parent()?.parent()?;

    // Open the expected Fe workspace root directly so source exports nested in
    // another checkout do not inherit that checkout's HEAD.
    gix::open(workspace_root).ok()
}

fn emit_git_rerun_paths(repo: &gix::Repository) {
    emit_rerun_path(repo.git_dir().join("HEAD"));
    emit_rerun_path(repo.common_dir().join("packed-refs"));

    if let Ok(Some(head_ref)) = repo.head_ref() {
        emit_rerun_path(repo.common_dir().join(head_ref.name().to_path()));
    }
}

fn emit_rerun_path(path: impl AsRef<Path>) {
    println!("cargo:rerun-if-changed={}", path.as_ref().display());
}
