use std::fs;

use camino::{Utf8Path, Utf8PathBuf};
use common::config::{Config, WorkspaceMemberSelection};
use resolver::workspace::{discover_context, expand_workspace_members};
use toml::Value;
use url::Url;

const DEFAULT_VERSION: &str = "0.1.0";

pub fn run(
    path: &Utf8PathBuf,
    workspace: bool,
    name: Option<&str>,
    version: Option<&str>,
) -> Result<(), String> {
    let target = absolute_target(path)?;

    if target.exists() && target.is_file() {
        return Err(format!(
            "Target path {target} exists and is a file; expected directory"
        ));
    }

    if workspace {
        create_workspace_layout(&target, name, version)?;
        return Ok(());
    }

    let mut start_dir = target.parent().unwrap_or(target.as_path());
    while !start_dir.exists() {
        let Some(parent) = start_dir.parent() else {
            break;
        };
        start_dir = parent;
    }
    let workspace_root = find_workspace_root(start_dir)?;

    create_ingot_layout(&target, name, version)?;

    if let Some(root) = workspace_root {
        match workspace_member_suggestion(&root, &target) {
            Ok(Some(message)) => println!("{message}"),
            Ok(None) => {}
            Err(err) => eprintln!("⚠️  failed to check workspace members: {err}"),
        }
    }

    Ok(())
}

fn create_workspace_layout(
    base: &Utf8PathBuf,
    explicit_name: Option<&str>,
    explicit_version: Option<&str>,
) -> Result<(), String> {
    fs::create_dir_all(base)
        .map_err(|err| format!("Failed to create workspace directory {base}: {err}"))?;

    let workspace_name = explicit_name
        .map(ToString::to_string)
        .unwrap_or_else(|| infer_workspace_name(base));
    let version = explicit_version.unwrap_or(DEFAULT_VERSION);

    let workspace_config = base.join("fe.toml");
    write_if_absent(
        &workspace_config,
        format!(
            r#"# Workspace config
[workspace]
name = "{workspace_name}"
version = "{version}"

# Members can be a flat array or a table with main/dev groups.
members = []
# members = ["ingots/*"]
# members = {{ main = ["packages/*"], dev = ["examples/*"] }}

# Paths to exclude from member discovery.
exclude = ["target"]
"#
        ),
    )?;

    println!("Created workspace at {base}");
    Ok(())
}

fn create_ingot_layout(
    base: &Utf8PathBuf,
    explicit_name: Option<&str>,
    explicit_version: Option<&str>,
) -> Result<(), String> {
    fs::create_dir_all(base)
        .map_err(|err| format!("Failed to create ingot directory {base}: {err}"))?;

    let src_dir = base.join("src");
    fs::create_dir_all(&src_dir)
        .map_err(|err| format!("Failed to create src directory {src_dir}: {err}"))?;

    let name = explicit_name
        .map(ToString::to_string)
        .unwrap_or_else(|| infer_ingot_name(base));
    let version = explicit_version.unwrap_or(DEFAULT_VERSION);

    let config_path = base.join("fe.toml");
    write_if_absent(
        &config_path,
        format!(
            r#"# Ingot config
[ingot]
name = "{name}"
version = "{version}"

# Optional dependencies
# [dependencies]
# utils = {{ path = "../utils" }}
"#
        ),
    )?;

    let src_lib = src_dir.join("lib.fe");
    write_if_absent(&src_lib, "pub fn main() {\n    // add your code here\n}\n")?;

    println!("Created ingot at {base}");
    Ok(())
}

fn write_if_absent(path: &Utf8PathBuf, content: impl AsRef<str>) -> Result<(), String> {
    if path.exists() {
        return Err(format!("Refusing to overwrite existing {}", path));
    }
    fs::write(path, content.as_ref()).map_err(|err| format!("Failed to write {}: {err}", path))?;
    Ok(())
}

fn infer_ingot_name(path: &Utf8PathBuf) -> String {
    let fallback = "ingot".to_string();
    let Some(stem) = path.file_name().map(|s| s.to_string()) else {
        return fallback;
    };
    sanitize_name(&stem, fallback)
}

fn infer_workspace_name(path: &Utf8PathBuf) -> String {
    let fallback = "workspace".to_string();
    let Some(stem) = path.file_name().map(|s| s.to_string()) else {
        return fallback;
    };
    sanitize_name(&stem, fallback)
}

fn sanitize_name(candidate: &str, fallback: String) -> String {
    let mut sanitized = String::new();
    for c in candidate.chars() {
        if c.is_ascii_alphanumeric() || c == '_' {
            sanitized.push(c);
        } else {
            sanitized.push('_');
        }
    }
    if sanitized.is_empty() {
        fallback
    } else {
        sanitized
    }
}

fn absolute_target(path: &Utf8PathBuf) -> Result<Utf8PathBuf, String> {
    let cwd = std::env::current_dir()
        .map_err(|err| format!("Failed to read current directory: {err}"))?;
    let cwd = Utf8PathBuf::from_path_buf(cwd)
        .map_err(|_| "Current directory is not valid UTF-8".to_string())?;

    if path.is_absolute() {
        Ok(path.clone())
    } else {
        Ok(cwd.join(path))
    }
}

fn find_workspace_root(start: &Utf8Path) -> Result<Option<Utf8PathBuf>, String> {
    let start_url = Url::from_directory_path(start.as_std_path())
        .map_err(|_| "Invalid directory path".to_string())?;
    let discovery = discover_context(&start_url).map_err(|err| err.to_string())?;
    let Some(workspace_url) = discovery.workspace_root else {
        return Ok(None);
    };
    let path = workspace_url
        .to_file_path()
        .map_err(|_| "Workspace URL is not a file URL".to_string())?;
    let path = Utf8PathBuf::from_path_buf(path)
        .map_err(|_| "Workspace path is not valid UTF-8".to_string())?;
    Ok(Some(path))
}

fn workspace_member_suggestion(
    workspace_root: &Utf8PathBuf,
    member_dir: &Utf8PathBuf,
) -> Result<Option<String>, String> {
    let config_path = workspace_root.join("fe.toml");
    let config_str = fs::read_to_string(config_path.as_std_path())
        .map_err(|err| format!("Failed to read {}: {err}", config_path))?;
    let config_file = Config::parse(&config_str)
        .map_err(|err| format!("Failed to parse {}: {err}", config_path))?;
    let workspace = match config_file {
        Config::Workspace(workspace_config) => workspace_config.workspace,
        Config::Ingot(_) => return Ok(None),
    };

    let relative_member = member_dir
        .strip_prefix(workspace_root)
        .map_err(|_| "Ingot path is not inside workspace".to_string())?
        .to_path_buf();
    if relative_member.as_str().is_empty() {
        return Ok(None);
    }

    let base_url = Url::from_directory_path(workspace_root.as_std_path())
        .map_err(|_| format!("Invalid workspace path: {workspace_root}"))?;
    if let Ok(expanded) =
        expand_workspace_members(&workspace, &base_url, WorkspaceMemberSelection::All)
        && expanded.iter().any(|member| member.path == relative_member)
    {
        return Ok(None);
    }

    let value: Value = config_str
        .parse()
        .map_err(|err| format!("Failed to parse {}: {err}", config_path))?;
    let root_table = value
        .as_table()
        .ok_or_else(|| format!("{} is not a workspace config", config_path))?;
    let has_workspace_table = root_table
        .get("workspace")
        .and_then(|value| value.as_table())
        .is_some();
    let workspace_table = if has_workspace_table {
        root_table
            .get("workspace")
            .and_then(|value| value.as_table())
            .ok_or_else(|| "workspace is not a table".to_string())?
    } else {
        root_table
    };

    let members_path = match workspace_table.get("members") {
        Some(Value::Array(_)) | None => {
            if has_workspace_table {
                "[workspace].members"
            } else {
                "members"
            }
        }
        Some(Value::Table(_)) => {
            if has_workspace_table {
                "[workspace].members.main"
            } else {
                "members.main"
            }
        }
        Some(_) => {
            return Err(format!(
                "members is not an array or table in {}",
                config_path
            ));
        }
    };

    Ok(Some(format!(
        "Workspace detected at {workspace_root}. To include this ingot, add \"{relative_member}\" to {members_path} in {config_path}."
    )))
}
