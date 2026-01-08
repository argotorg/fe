use std::fs;

use camino::Utf8PathBuf;
use common::{
    config::{Manifest, WorkspaceManifest},
    dependencies::WorkspaceMemberRecord,
    ingot::Version,
};
use smol_str::SmolStr;
use url::Url;

use crate::expand_workspace_members;
use common::InputDb;
use resolver::git::{GitDescription, GitResolver};

pub struct WorkspaceMemberSelectionResult {
    pub member: WorkspaceMemberRecord,
}

#[derive(Debug)]
pub enum WorkspaceLookupError {
    NotWorkspace,
    Message(String),
}

impl std::fmt::Display for WorkspaceLookupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WorkspaceLookupError::NotWorkspace => write!(f, "target is not a workspace"),
            WorkspaceLookupError::Message(message) => write!(f, "{message}"),
        }
    }
}

pub fn resolve_local_workspace_member(
    db: &mut dyn InputDb,
    workspace_root: &Url,
    name: &SmolStr,
    version: Option<&Version>,
    path_hint: Option<&Utf8PathBuf>,
) -> Result<WorkspaceMemberSelectionResult, WorkspaceLookupError> {
    ensure_local_workspace_registry(db, workspace_root)?;
    select_workspace_member(db, workspace_root, name, version, path_hint, None)
}

pub fn resolve_remote_workspace_member(
    db: &mut dyn InputDb,
    git: &GitResolver,
    description: &GitDescription,
    name: &SmolStr,
    version: Option<&Version>,
    path_hint: Option<&Utf8PathBuf>,
) -> Result<WorkspaceMemberSelectionResult, WorkspaceLookupError> {
    let checkout_path = ensure_remote_workspace_registry(db, git, description)?;
    let workspace_root = Url::from_directory_path(checkout_path.as_std_path()).map_err(|_| {
        WorkspaceLookupError::Message(
            "failed to convert workspace checkout path to URL".to_string(),
        )
    })?;
    select_workspace_member(
        db,
        &workspace_root,
        name,
        version,
        path_hint,
        Some(&checkout_path),
    )
}

fn ensure_local_workspace_registry(
    db: &mut dyn InputDb,
    workspace_root: &Url,
) -> Result<(), WorkspaceLookupError> {
    if !db
        .dependency_graph()
        .workspace_member_records(db, workspace_root)
        .is_empty()
    {
        return Ok(());
    }

    let manifest = load_workspace_manifest_from_url(workspace_root)?;
    let members = expand_workspace_members(
        &manifest.workspace,
        workspace_root,
        common::config::WorkspaceMemberSelection::All,
    )
    .map_err(WorkspaceLookupError::Message)?;

    for member in members {
        db.dependency_graph()
            .register_workspace_member_root(db, workspace_root, &member.url);
        if let Some(name) = member.name.clone() {
            let existing =
                db.dependency_graph()
                    .workspace_members_by_name(db, workspace_root, &name);
            if existing.iter().any(|other| other.version == member.version) {
                return Err(WorkspaceLookupError::Message(format!(
                    "workspace member {name} has duplicate version in {workspace_root}"
                )));
            }
            let record = WorkspaceMemberRecord {
                name,
                version: member.version.clone(),
                path: member.path.clone(),
                url: member.url.clone(),
            };
            db.dependency_graph()
                .register_workspace_member(db, workspace_root, record);
        }
        if let (Some(name), Some(version)) = (member.name.clone(), member.version.clone()) {
            db.dependency_graph()
                .register_expected_member_metadata(db, &member.url, name, version);
        }
    }

    Ok(())
}

fn ensure_remote_workspace_registry(
    db: &mut dyn InputDb,
    git: &GitResolver,
    description: &GitDescription,
) -> Result<Utf8PathBuf, WorkspaceLookupError> {
    let checkout = git
        .ensure_checkout_resource(description)
        .map_err(|err| WorkspaceLookupError::Message(err.to_string()))?;
    let manifest = load_workspace_manifest_from_path(&checkout.checkout_path)?;
    let workspace_root =
        Url::from_directory_path(checkout.checkout_path.as_std_path()).map_err(|_| {
            WorkspaceLookupError::Message(
                "failed to convert workspace checkout path to URL".to_string(),
            )
        })?;

    if !db
        .dependency_graph()
        .workspace_member_records(db, &workspace_root)
        .is_empty()
    {
        return Ok(checkout.checkout_path);
    }

    let specs = manifest
        .workspace
        .members_for_selection(common::config::WorkspaceMemberSelection::All);
    for spec in specs {
        let pattern = spec.path.as_str();
        if spec.name.is_none() {
            continue;
        }
        if pattern.contains(['*', '?', '[']) {
            return Err(WorkspaceLookupError::Message(format!(
                "Member path \"{pattern}\" with name/version cannot contain glob patterns"
            )));
        }
        let path = Utf8PathBuf::from(pattern);
        let member_url =
            Url::from_directory_path(checkout.checkout_path.join(path.as_str()).as_std_path())
                .map_err(|_| {
                    WorkspaceLookupError::Message(
                        "failed to convert member path to URL".to_string(),
                    )
                })?;

        let record = WorkspaceMemberRecord {
            name: spec.name.clone().expect("name checked"),
            version: spec.version.clone(),
            path: path.clone(),
            url: member_url.clone(),
        };
        db.dependency_graph()
            .register_workspace_member(db, &workspace_root, record);

        if let (Some(name), Some(version)) = (spec.name.clone(), spec.version.clone()) {
            db.dependency_graph()
                .register_expected_member_metadata(db, &member_url, name, version);
        }
    }

    Ok(checkout.checkout_path)
}

#[allow(clippy::too_many_arguments)]
fn select_workspace_member(
    db: &mut dyn InputDb,
    workspace_root: &Url,
    name: &SmolStr,
    version: Option<&Version>,
    path_hint: Option<&Utf8PathBuf>,
    checkout_path: Option<&Utf8PathBuf>,
) -> Result<WorkspaceMemberSelectionResult, WorkspaceLookupError> {
    let mut candidates = db
        .dependency_graph()
        .workspace_members_by_name(db, workspace_root, name);
    if candidates.is_empty() {
        return Err(WorkspaceLookupError::Message(format!(
            "No workspace member named \"{name}\" found in {workspace_root}"
        )));
    }

    if let Some(version) = version {
        let exact: Vec<_> = candidates
            .iter()
            .filter(|&member| member.version.as_ref() == Some(version))
            .cloned()
            .collect();
        if exact.len() == 1 {
            return finalize_member_selection(&exact[0], path_hint);
        }
        if exact.len() > 1 {
            return Err(WorkspaceLookupError::Message(format!(
                "Multiple workspace members named \"{name}\" with version {version}"
            )));
        }

        let mut matched = None;
        for member in candidates.iter_mut() {
            if member.version.is_none()
                && member_version_matches(member, version, checkout_path).unwrap_or(false)
            {
                if matched.is_some() {
                    return Err(WorkspaceLookupError::Message(format!(
                        "Multiple workspace members named \"{name}\" match version {version}"
                    )));
                }
                matched = Some(member.clone());
            }
        }
        if let Some(member) = matched {
            return finalize_member_selection(&member, path_hint);
        }
        return Err(WorkspaceLookupError::Message(format!(
            "No workspace member named \"{name}\" with version {version}"
        )));
    }

    if candidates.len() == 1 {
        return finalize_member_selection(&candidates[0], path_hint);
    }

    Err(WorkspaceLookupError::Message(format!(
        "Multiple workspace members named \"{name}\"; specify a version"
    )))
}

fn finalize_member_selection(
    member: &WorkspaceMemberRecord,
    path_hint: Option<&Utf8PathBuf>,
) -> Result<WorkspaceMemberSelectionResult, WorkspaceLookupError> {
    if let Some(path_hint) = path_hint
        && path_hint.as_str() != member.path.as_str()
    {
        return Err(WorkspaceLookupError::Message(format!(
            "Workspace member \"{}\" resolved to {} but dependency path hint requested {}",
            member.name, member.path, path_hint
        )));
    }
    Ok(WorkspaceMemberSelectionResult {
        member: member.clone(),
    })
}

fn member_version_matches(
    member: &WorkspaceMemberRecord,
    expected: &Version,
    checkout_path: Option<&Utf8PathBuf>,
) -> Result<bool, WorkspaceLookupError> {
    if let Some(version) = &member.version {
        return Ok(version == expected);
    }

    let member_path = if let Some(checkout_root) = checkout_path {
        checkout_root.join(member.path.as_str())
    } else {
        Utf8PathBuf::from_path_buf(member.url.to_file_path().map_err(|_| {
            WorkspaceLookupError::Message("member URL is not a file path".to_string())
        })?)
        .map_err(|_| WorkspaceLookupError::Message("member path is not UTF-8".to_string()))?
    };
    let manifest_path = member_path.join("fe.toml");
    let content = fs::read_to_string(manifest_path.as_std_path()).map_err(|err| {
        WorkspaceLookupError::Message(format!("Failed to read {}: {err}", manifest_path))
    })?;
    let manifest = Manifest::parse(&content).map_err(|err| {
        WorkspaceLookupError::Message(format!("Failed to parse {manifest_path}: {err}"))
    })?;
    let Manifest::Ingot(ingot) = manifest else {
        return Err(WorkspaceLookupError::Message(format!(
            "Expected ingot manifest at {manifest_path}"
        )));
    };
    Ok(ingot.metadata.version.as_ref() == Some(expected))
}

fn load_workspace_manifest_from_url(
    workspace_root: &Url,
) -> Result<WorkspaceManifest, WorkspaceLookupError> {
    let path = workspace_root.to_file_path().map_err(|_| {
        WorkspaceLookupError::Message("workspace URL is not a file URL".to_string())
    })?;
    let path = Utf8PathBuf::from_path_buf(path)
        .map_err(|_| WorkspaceLookupError::Message("workspace path is not UTF-8".to_string()))?;
    load_workspace_manifest_from_path(&path)
}

fn load_workspace_manifest_from_path(
    path: &Utf8PathBuf,
) -> Result<WorkspaceManifest, WorkspaceLookupError> {
    let manifest_path = path.join("fe.toml");
    let content = fs::read_to_string(manifest_path.as_std_path()).map_err(|err| {
        WorkspaceLookupError::Message(format!("Failed to read {}: {err}", manifest_path))
    })?;
    let manifest = Manifest::parse(&content).map_err(|err| {
        WorkspaceLookupError::Message(format!("Failed to parse {manifest_path}: {err}"))
    })?;
    match manifest {
        Manifest::Workspace(workspace) => Ok(*workspace),
        Manifest::Ingot(_) => Err(WorkspaceLookupError::NotWorkspace),
    }
}
