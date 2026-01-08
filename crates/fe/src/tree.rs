use std::fs;

use camino::Utf8PathBuf;
use common::config::{Manifest, WorkspaceManifest};
use driver::{DependencyTree, DriverDataBase, init_ingot, workspace_members};
use url::Url;

pub fn print_tree(path: &Utf8PathBuf) {
    let mut db = DriverDataBase::default();
    if let Some(name) = name_candidate(path) {
        if let Err(err) = print_workspace_member_tree_by_name(&mut db, &name) {
            eprintln!("❌ {err}");
        }
        return;
    }

    let target_url = match ingot_url(path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("{message}");
            return;
        }
    };

    if let Ok(Some(Manifest::Workspace(workspace_manifest))) = manifest_at_path(path) {
        let _ = init_ingot(&mut db, &target_url);
        if let Err(err) = print_workspace_trees(&db, &workspace_manifest, &target_url) {
            eprintln!("❌ {err}");
        }
        return;
    }

    let _ = init_ingot(&mut db, &target_url);

    let tree = DependencyTree::build(&db, &target_url);
    print!("{}", tree.display());
}

fn ingot_url(path: &Utf8PathBuf) -> Result<Url, String> {
    let canonical_path = path
        .canonicalize_utf8()
        .map_err(|_| format!("Error: invalid or non-existent directory path: {path}"))?;

    if !canonical_path.is_dir() {
        return Err(format!("Error: {path} is not a directory"));
    }

    Url::from_directory_path(canonical_path.as_str())
        .map_err(|_| format!("Error: invalid directory path: {path}"))
}

fn manifest_at_path(path: &Utf8PathBuf) -> Result<Option<Manifest>, String> {
    let manifest_path = path.join("fe.toml");
    if !manifest_path.is_file() {
        return Ok(None);
    }
    let content = fs::read_to_string(manifest_path.as_std_path())
        .map_err(|err| format!("Failed to read {}: {err}", manifest_path))?;
    let manifest = Manifest::parse(&content)
        .map_err(|err| format!("Failed to parse {}: {err}", manifest_path))?;
    Ok(Some(manifest))
}

fn name_candidate(path: &Utf8PathBuf) -> Option<String> {
    if path.exists() {
        return None;
    }
    let value = path.as_str();
    if value.is_empty() {
        return None;
    }
    if value.chars().all(|c| c.is_alphanumeric() || c == '_') {
        Some(value.to_string())
    } else {
        None
    }
}

fn find_workspace_root(path: &Utf8PathBuf) -> Option<Utf8PathBuf> {
    for ancestor in path.ancestors() {
        if let Ok(Some(Manifest::Workspace(_))) = manifest_at_path(&ancestor.to_path_buf()) {
            return Some(ancestor.to_path_buf());
        }
    }
    None
}

fn print_workspace_member_tree_by_name(db: &mut DriverDataBase, name: &str) -> Result<(), String> {
    let cwd = std::env::current_dir()
        .map_err(|err| format!("Failed to read current directory: {err}"))?;
    let cwd = Utf8PathBuf::from_path_buf(cwd)
        .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
    let workspace_root = find_workspace_root(&cwd)
        .ok_or_else(|| "No workspace manifest found in current directory hierarchy".to_string())?;
    let workspace_url = ingot_url(&workspace_root)?;
    let manifest = manifest_at_path(&workspace_root)?
        .and_then(|manifest| match manifest {
            Manifest::Workspace(workspace_manifest) => Some(workspace_manifest),
            Manifest::Ingot(_) => None,
        })
        .ok_or_else(|| "Workspace manifest not found".to_string())?;

    let _ = init_ingot(db, &workspace_url);
    let members = workspace_members(&manifest.workspace, &workspace_url)?;
    let mut matches = members
        .into_iter()
        .filter(|member| member.name.as_deref() == Some(name))
        .collect::<Vec<_>>();

    if matches.is_empty() {
        return Err(format!("No workspace member named \"{name}\""));
    }
    if matches.len() > 1 {
        return Err(format!(
            "Multiple workspace members named \"{name}\"; specify a path instead"
        ));
    }

    let member = matches.remove(0);
    let tree = DependencyTree::build(db, &member.url);
    print!("{}", tree.display());
    Ok(())
}

fn print_workspace_trees(
    db: &DriverDataBase,
    workspace_manifest: &WorkspaceManifest,
    workspace_url: &Url,
) -> Result<(), String> {
    let members = workspace_members(&workspace_manifest.workspace, workspace_url)?;
    if members.is_empty() {
        println!("No workspace members found");
        return Ok(());
    }

    for (idx, member) in members.iter().enumerate() {
        if idx > 0 {
            println!();
        }
        let label = member
            .name
            .as_deref()
            .map(|name| name.to_string())
            .unwrap_or_else(|| member.url.to_string());
        println!("== {label} ==");
        let tree = DependencyTree::build(db, &member.url);
        print!("{}", tree.display());
    }
    Ok(())
}
