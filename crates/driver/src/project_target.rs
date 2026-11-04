//! The target `fe.toml` sets for a project.

use std::fmt;

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    InputDb,
    compilation::{Os, Target},
    config::{Config, WorkspaceConfig, WorkspaceMemberSelection},
};
use resolver::workspace::{discover_context, expand_workspace_members};
use salsa::Setter;
use url::Url;

use crate::{
    DriverDataBase,
    cli_target::{CliTarget, resolve_cli_target},
    workspace_members,
};

/// Two parts of a project that set different targets. A database compiles for
/// one target at a time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TargetConflict {
    first: (String, Target),
    other: (String, Target),
}

impl fmt::Display for TargetConflict {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "`{}` targets `{}` and `{}` targets `{}`",
            self.first.0,
            self.first.1.name(),
            self.other.0,
            self.other.1.name(),
        )
    }
}

/// The one target of the parts seen so far.
#[derive(Default)]
struct CommonTarget(Option<(String, Target)>);

impl CommonTarget {
    fn add(&mut self, name: &str, target: Target) -> Result<(), TargetConflict> {
        match &self.0 {
            Some(first) if first.1 != target => Err(TargetConflict {
                first: first.clone(),
                other: (name.to_string(), target),
            }),
            Some(_) => Ok(()),
            None => {
                self.0 = Some((name.to_string(), target));
                Ok(())
            }
        }
    }

    fn target(self) -> Option<Target> {
        self.0.map(|(_, target)| target)
    }
}

/// The target `fe.toml` sets for a command on `path`, if any. A path the
/// command can't resolve reports that itself.
pub fn project_target(
    path: &Utf8PathBuf,
    ingot: Option<&str>,
    force_standalone: bool,
) -> Result<Option<Target>, TargetConflict> {
    let mut db = DriverDataBase::default();
    match resolve_cli_target(&mut db, path, force_standalone) {
        Ok(CliTarget::Directory(dir)) => directory_target(&dir, ingot),
        Ok(CliTarget::StandaloneFile(_)) | Err(_) => Ok(None),
    }
}

/// The target the ingots and workspaces at `urls` set, if any.
pub fn ingots_target(urls: &[Url]) -> Result<Option<Target>, TargetConflict> {
    let mut common = CommonTarget::default();
    for url in urls {
        let Some(dir) = url
            .to_file_path()
            .ok()
            .and_then(|dir| Utf8PathBuf::from_path_buf(dir).ok())
        else {
            continue;
        };
        if let Some(target) = directory_target(&dir, None)? {
            common.add(dir.as_str(), target)?;
        }
    }
    Ok(common.target())
}

/// Sets the target of `db` and the operating system that goes with it, and
/// returns whether that changed the target.
pub fn set_target(db: &mut DriverDataBase, target: Target) -> bool {
    if db.compilation_settings().target(db) == target {
        return false;
    }
    db.compilation_settings().set_target(db).to(target);
    let os = if target == Target::Native {
        Os::host()
    } else {
        None
    };
    db.compilation_settings().set_os(db).to(os);
    true
}

/// A dependency of the ingot at `url` that sets a target other than the one
/// `db` compiles for. Only the ingots a command compiles have to match it, and
/// the ingot itself may be overridden with `--backend`.
pub fn dependency_target_mismatch(db: &DriverDataBase, url: &Url) -> Option<String> {
    let target = db.compilation_settings().target(db);
    let graph = db.dependency_graph();
    graph
        .dependency_urls(db, url)
        .into_iter()
        .find_map(|dependency| {
            let declared = declared_target(db, &dependency)?;
            (declared != target).then(|| {
                format!(
                    "dependency {dependency} targets `{}`, but this command compiles for `{}`",
                    declared.name(),
                    target.name(),
                )
            })
        })
}

/// The config of the project at `url` as the database loaded it.
fn loaded_config(db: &DriverDataBase, url: &Url) -> Option<Config> {
    let file = db.workspace().get(db, &url.join("fe.toml").ok()?)?;
    Config::parse(file.text(db)).ok()
}

fn is_member(workspace: &WorkspaceConfig, workspace_url: &Url, url: &Url) -> bool {
    expand_workspace_members(
        &workspace.workspace,
        workspace_url,
        WorkspaceMemberSelection::All,
    )
    .is_ok_and(|members| members.iter().any(|member| member.url == *url))
}

/// The target the ingot at `url` or its workspace sets.
fn declared_target(db: &DriverDataBase, url: &Url) -> Option<Target> {
    let Config::Ingot(config) = loaded_config(db, url)? else {
        return None;
    };
    config.target.or_else(|| {
        let root = db.dependency_graph().workspace_root_for_member(db, url)?;
        let Config::Workspace(workspace) = loaded_config(db, &root)? else {
            return None;
        };
        is_member(&workspace, &root, url)
            .then_some(workspace.workspace.target)
            .flatten()
    })
}

fn read_config(dir: &Utf8Path) -> Option<Config> {
    let content = std::fs::read_to_string(dir.join("fe.toml")).ok()?;
    Config::parse(&content).ok()
}

fn directory_url(dir: &Utf8Path) -> Option<Url> {
    let dir = dir.canonicalize_utf8().ok()?;
    Url::from_directory_path(dir.as_str()).ok()
}

/// The target the ingot, or the selected members of the workspace, at `dir`
/// set. An ingot without one works on every target.
fn directory_target(dir: &Utf8Path, ingot: Option<&str>) -> Result<Option<Target>, TargetConflict> {
    match read_config(dir) {
        Some(Config::Ingot(config)) => Ok(config.target.or_else(|| inherited_target(dir))),
        Some(Config::Workspace(workspace)) => workspace_target(dir, &workspace, ingot),
        None => Ok(None),
    }
}

/// The target of the workspace that has the ingot at `dir` as a member.
fn inherited_target(dir: &Utf8Path) -> Option<Target> {
    let url = directory_url(dir)?;
    let root = discover_context(&url, false).ok()?.workspace_root?;
    let path = Utf8PathBuf::from_path_buf(root.to_file_path().ok()?).ok()?;
    let Config::Workspace(workspace) = read_config(&path)? else {
        return None;
    };
    is_member(&workspace, &root, &url)
        .then_some(workspace.workspace.target)
        .flatten()
}

fn workspace_target(
    dir: &Utf8Path,
    workspace: &WorkspaceConfig,
    ingot: Option<&str>,
) -> Result<Option<Target>, TargetConflict> {
    let default = workspace.workspace.target;
    let Some(members) =
        directory_url(dir).and_then(|url| workspace_members(&workspace.workspace, &url).ok())
    else {
        return Ok(default);
    };
    let mut common = CommonTarget::default();
    for member in &members {
        let Some(Config::Ingot(config)) = read_config(&dir.join(member.path.as_str())) else {
            continue;
        };
        let name = config.metadata.name.as_deref();
        if ingot.is_some_and(|ingot| name != Some(ingot) && member.name.as_deref() != Some(ingot)) {
            continue;
        }
        if let Some(target) = config.target.or(default) {
            common.add(name.unwrap_or(member.path.as_str()), target)?;
        }
    }
    Ok(common.target().or(default))
}
