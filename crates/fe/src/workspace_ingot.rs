use camino::{Utf8Path, Utf8PathBuf};
use common::config::Config;

pub const INGOT_REQUIRES_WORKSPACE_ROOT: &str =
    "`--ingot` requires an input path that resolves to a workspace root";

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
    if member.name == Some(ingot) || member.path.file_name().is_some_and(|name| name == ingot) {
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
