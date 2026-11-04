use camino::{Utf8Path, Utf8PathBuf};
use common::{
    compilation::Target,
    config::{Config, WorkspaceConfig},
    file::IngotFileKind,
};
use driver::{
    DriverDataBase,
    cli_target::{CliTarget, resolve_cli_target},
};
use resolver::workspace::discover_context;
use url::Url;

pub const INGOT_REQUIRES_WORKSPACE_ROOT: &str =
    "`--ingot` requires an input path that resolves to a workspace root";

pub(crate) fn ingot_has_source_files(db: &DriverDataBase, ingot: hir::Ingot<'_>) -> bool {
    ingot
        .files(db)
        .iter()
        .any(|(_, file)| matches!(file.kind(db), Some(IngotFileKind::Source)))
}

/// The target `fe.toml` sets for a command on `path`, if any. A path the
/// command can't resolve reports that itself.
pub fn project_target(
    path: &Utf8PathBuf,
    ingot: Option<&str>,
    force_standalone: bool,
) -> Result<Option<Target>, String> {
    let mut db = DriverDataBase::default();
    match resolve_cli_target(&mut db, path, force_standalone) {
        Ok(CliTarget::Directory(dir)) => directory_target(&dir, ingot),
        Ok(CliTarget::StandaloneFile(_)) | Err(_) => Ok(None),
    }
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
fn directory_target(dir: &Utf8Path, ingot: Option<&str>) -> Result<Option<Target>, String> {
    match read_config(dir) {
        Some(Config::Ingot(config)) => Ok(config.target.or_else(|| inherited_target(dir))),
        Some(Config::Workspace(workspace)) => workspace_target(dir, &workspace, ingot),
        None => Ok(None),
    }
}

/// The target of the workspace that has the ingot at `dir` as a member.
fn inherited_target(dir: &Utf8Path) -> Option<Target> {
    let url = directory_url(dir)?;
    let context = discover_context(&url, false).ok()?;
    if !context.ingot_roots.contains(&url) {
        return None;
    }
    let root = context.workspace_root?.to_file_path().ok()?;
    let Config::Workspace(workspace) = read_config(&Utf8PathBuf::from_path_buf(root).ok()?)? else {
        return None;
    };
    workspace.workspace.target
}

/// A database compiles for one target at a time, so the selected members of a
/// workspace must not set different ones.
fn workspace_target(
    dir: &Utf8Path,
    workspace: &WorkspaceConfig,
    ingot: Option<&str>,
) -> Result<Option<Target>, String> {
    let default = workspace.workspace.target;
    let Some(members) = directory_url(dir)
        .and_then(|url| driver::workspace_members(&workspace.workspace, &url).ok())
    else {
        return Ok(default);
    };
    let mut selected: Option<(Target, String)> = None;
    for member in &members {
        let Some(Config::Ingot(config)) = read_config(&member.path) else {
            continue;
        };
        let name = config.metadata.name.as_deref();
        if ingot.is_some_and(|ingot| name != Some(ingot) && member.name.as_deref() != Some(ingot)) {
            continue;
        }
        let Some(target) = config.target.or(default) else {
            continue;
        };
        let name = name.unwrap_or(member.path.as_str()).to_string();
        match &selected {
            Some((first, first_name)) if *first != target => {
                return Err(format!(
                    "workspace members target different machines: `{first_name}` targets `{}` \
                     and `{name}` targets `{}`; select one with `--ingot`, or pass `--backend`",
                    first.name(),
                    target.name(),
                ));
            }
            Some(_) => {}
            None => selected = Some((target, name)),
        }
    }
    Ok(selected.map(|(target, _)| target).or(default))
}

#[derive(Debug, Clone, Copy)]
pub struct WorkspaceMemberRef<'a> {
    path: &'a Utf8Path,
    name: Option<&'a str>,
}

impl<'a> WorkspaceMemberRef<'a> {
    pub fn new(path: &'a Utf8Path, name: Option<&'a str>) -> Self {
        Self { path, name }
    }
}

pub fn select_workspace_member_paths<'a>(
    workspace_root: &Utf8Path,
    workspace_display: &Utf8Path,
    members: impl IntoIterator<Item = WorkspaceMemberRef<'a>>,
    ingot: Option<&str>,
) -> Result<Vec<Utf8PathBuf>, String> {
    let mut member_paths = members
        .into_iter()
        .filter(|member| {
            ingot.is_none_or(|ingot| workspace_member_matches_ingot(workspace_root, *member, ingot))
        })
        .map(|member| workspace_root.join(member.path))
        .collect::<Vec<_>>();
    member_paths.sort();

    if let Some(ingot) = ingot
        && member_paths.is_empty()
    {
        return Err(format!(
            "No workspace member named \"{ingot}\" found in `{workspace_display}`"
        ));
    }

    Ok(member_paths)
}

fn workspace_member_matches_ingot(
    workspace_root: &Utf8Path,
    member: WorkspaceMemberRef<'_>,
    ingot: &str,
) -> bool {
    if member.name == Some(ingot) {
        return true;
    }

    let config_path = workspace_root.join(member.path).join("fe.toml");
    let Ok(content) = std::fs::read_to_string(config_path.as_std_path()) else {
        return false;
    };
    let Ok(Config::Ingot(config)) = Config::parse(&content) else {
        return false;
    };
    config.metadata.name.as_deref() == Some(ingot)
}

#[cfg(test)]
mod tests {
    use super::{WorkspaceMemberRef, select_workspace_member_paths};
    use camino::{Utf8Path, Utf8PathBuf};
    use std::fs;
    use tempfile::tempdir;

    #[test]
    fn select_workspace_member_paths_does_not_match_by_directory_name() {
        let temp = tempdir().expect("tempdir");
        let workspace_root =
            Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).expect("workspace root utf8");
        let non_target = workspace_root.join("libs/a");
        fs::create_dir_all(non_target.as_std_path()).expect("create non-target dir");
        fs::write(
            non_target.join("fe.toml").as_std_path(),
            "[ingot]\nname = \"not_a\"\nversion = \"0.1.0\"\n",
        )
        .expect("write non-target fe.toml");

        let paths = select_workspace_member_paths(
            &workspace_root,
            &workspace_root,
            [
                WorkspaceMemberRef::new(Utf8Path::new("ingots/target"), Some("a")),
                WorkspaceMemberRef::new(Utf8Path::new("libs/a"), None),
            ],
            Some("a"),
        )
        .expect("select paths");

        assert_eq!(paths, vec![workspace_root.join("ingots/target")]);
    }

    #[test]
    fn select_workspace_member_paths_matches_ingot_metadata_name() {
        let temp = tempdir().expect("tempdir");
        let workspace_root =
            Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).expect("workspace root utf8");
        let member = workspace_root.join("ingots/core");
        fs::create_dir_all(member.as_std_path()).expect("create member dir");
        fs::write(
            member.join("fe.toml").as_std_path(),
            "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n",
        )
        .expect("write member fe.toml");

        let paths = select_workspace_member_paths(
            &workspace_root,
            &workspace_root,
            [WorkspaceMemberRef::new(Utf8Path::new("ingots/core"), None)],
            Some("app"),
        )
        .expect("select paths");

        assert_eq!(paths, vec![workspace_root.join("ingots/core")]);
    }
}
