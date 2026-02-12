use std::fs;

use camino::Utf8PathBuf;
use common::{InputDb, config::Config};
use resolver::{ResolutionHandler, Resolver};
use resolver::{
    files::ancestor_fe_toml_dirs,
    ingot::{FeTomlProbe, infer_config_kind},
};
use smol_str::SmolStr;
use url::Url;

use crate::DriverDataBase;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CliTarget {
    StandaloneFile(Utf8PathBuf),
    Directory(Utf8PathBuf),
}

struct ResolvedMember {
    path: Utf8PathBuf,
    url: Url,
}

struct ConfigProbe;

impl ResolutionHandler<resolver::files::FilesResolver> for ConfigProbe {
    type Item = FeTomlProbe;

    fn handle_resolution(
        &mut self,
        _description: &Url,
        resource: resolver::files::FilesResource,
    ) -> Self::Item {
        for file in &resource.files {
            if file.path.as_str().ends_with("fe.toml") {
                return FeTomlProbe::Present {
                    kind_hint: infer_config_kind(&file.content),
                };
            }
        }
        FeTomlProbe::Missing
    }
}

pub fn resolve_cli_target(
    db: &mut DriverDataBase,
    path: &Utf8PathBuf,
) -> Result<CliTarget, String> {
    let arg = path.as_str();
    let is_name = is_name_candidate(arg);
    let path_exists = path.exists();

    if path.is_file() {
        if path.extension() == Some("fe") {
            // If the file lives under an ingot, operate from that directory so imports resolve
            // in context. For workspace roots, prefer treating the file as standalone unless
            // the user explicitly targets the workspace.
            if let Ok(canonical) = path.canonicalize_utf8()
                && let Some(root) = ancestor_fe_toml_dirs(canonical.as_std_path())
                    .first()
                    .and_then(|root| Utf8PathBuf::from_path_buf(root.to_path_buf()).ok())
            {
                let config_path = root.join("fe.toml");
                if let Ok(content) = fs::read_to_string(&config_path)
                    && matches!(Config::parse(&content), Ok(Config::Ingot(_)))
                {
                    return Ok(CliTarget::Directory(root));
                }
            }

            return Ok(CliTarget::StandaloneFile(path.clone()));
        }
        return Err("Path must be either a .fe file or a directory containing fe.toml".into());
    }

    let name_match = if is_name {
        resolve_member_by_name(db, arg)?
    } else {
        None
    };

    let path_member = if is_name && path_exists {
        resolve_member_by_path(db, path)?
    } else {
        None
    };

    if path_exists && name_match.is_some() {
        match (&name_match, &path_member) {
            (Some(name_member), Some(path_member)) => {
                if name_member.url == path_member.url {
                    return Ok(CliTarget::Directory(path_member.path.clone()));
                }
                return Err(format!(
                    "Argument \"{arg}\" matches a workspace member name but does not match the provided path"
                ));
            }
            (Some(_), None) => {
                return Err(format!(
                    "Argument \"{arg}\" matches a workspace member name but does not match the provided path"
                ));
            }
            _ => {}
        }
    }

    if let Some(name_member) = name_match {
        return Ok(CliTarget::Directory(name_member.path));
    }

    if path_exists {
        if path.is_dir() && path.join("fe.toml").is_file() {
            return Ok(CliTarget::Directory(path.clone()));
        }
        return Err("Path must be either a .fe file or a directory containing fe.toml".into());
    }

    Err("Path must be either a .fe file or a directory containing fe.toml".into())
}

fn is_name_candidate(value: &str) -> bool {
    !value.is_empty() && value.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn resolve_member_by_name(
    db: &mut DriverDataBase,
    name: &str,
) -> Result<Option<ResolvedMember>, String> {
    let cwd = std::env::current_dir()
        .map_err(|err| format!("Failed to read current directory: {err}"))?;
    let cwd = Utf8PathBuf::from_path_buf(cwd)
        .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
    let workspace_root = find_workspace_root(db, &cwd)?;
    let Some(workspace_root) = workspace_root else {
        return Ok(None);
    };
    let workspace_url = dir_url(&workspace_root)?;
    let mut matches =
        db.dependency_graph()
            .workspace_members_by_name(db, &workspace_url, &SmolStr::new(name));
    if matches.is_empty() {
        return Ok(None);
    }
    if matches.len() > 1 {
        return Err(format!(
            "Multiple workspace members named \"{name}\"; specify a path instead"
        ));
    }
    let member = matches.pop().map(|member| ResolvedMember {
        path: workspace_root.join(member.path.as_str()),
        url: member.url,
    });
    Ok(member)
}

fn resolve_member_by_path(
    db: &mut DriverDataBase,
    path: &Utf8PathBuf,
) -> Result<Option<ResolvedMember>, String> {
    if !path.is_dir() {
        return Ok(None);
    }
    let workspace_root = find_workspace_root(db, path)?;
    let Some(workspace_root) = workspace_root else {
        return Ok(None);
    };
    let workspace_url = dir_url(&workspace_root)?;
    let members = db
        .dependency_graph()
        .workspace_member_records(db, &workspace_url);
    let canonical = path
        .canonicalize_utf8()
        .map_err(|_| format!("Invalid or non-existent directory path: {path}"))?;
    let target_url = Url::from_directory_path(canonical.as_str())
        .map_err(|_| format!("Invalid directory path: {path}"))?;

    Ok(members
        .into_iter()
        .find(|member| member.url == target_url)
        .map(|member| ResolvedMember {
            path: workspace_root.join(member.path.as_str()),
            url: member.url,
        }))
}

fn find_workspace_root(
    db: &mut DriverDataBase,
    start: &Utf8PathBuf,
) -> Result<Option<Utf8PathBuf>, String> {
    let dirs = ancestor_fe_toml_dirs(start.as_std_path());
    for dir in dirs {
        let dir = Utf8PathBuf::from_path_buf(dir)
            .map_err(|_| "Encountered non UTF-8 workspace path".to_string())?;
        let url = dir_url(&dir)?;
        let mut resolver = resolver::ingot::minimal_files_resolver();
        let summary = resolver
            .resolve(&mut ConfigProbe, &url)
            .map_err(|err| err.to_string())?;
        if summary.kind_hint() == Some(resolver::ingot::ConfigKind::Workspace) {
            if db
                .dependency_graph()
                .workspace_member_records(db, &url)
                .is_empty()
            {
                let _ = crate::init_ingot(db, &url);
            }
            return Ok(Some(dir));
        }
    }
    Ok(None)
}

fn dir_url(path: &Utf8PathBuf) -> Result<Url, String> {
    let canonical_path = match path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            let cwd = std::env::current_dir()
                .map_err(|err| format!("Failed to read current directory: {err}"))?;
            let cwd = Utf8PathBuf::from_path_buf(cwd)
                .map_err(|_| "Current directory is not valid UTF-8".to_string())?;
            cwd.join(path)
        }
    };
    Url::from_directory_path(canonical_path.as_str())
        .map_err(|_| format!("Invalid or non-existent directory path: {path}"))
}
