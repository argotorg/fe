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
