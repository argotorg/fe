use smol_str::SmolStr;
use toml::Value;

use crate::ingot::Version;

use super::{ConfigDiagnostic, dependency, is_valid_name, parse_string_array_field};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct WorkspaceSettings {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
    pub members: Vec<WorkspaceMemberSpec>,
    pub dev_members: Vec<WorkspaceMemberSpec>,
    pub default_members: Option<Vec<SmolStr>>,
    pub exclude: Vec<SmolStr>,
    pub metadata: Option<toml::value::Table>,
    pub profiles: Option<toml::value::Table>,
    pub scripts: Vec<WorkspaceScript>,
    pub resolution: Option<WorkspaceResolution>,
    pub dependencies: Vec<dependency::DependencyEntry>,
}

impl Eq for WorkspaceSettings {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WorkspaceScript {
    pub name: SmolStr,
    pub command: SmolStr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WorkspaceMemberSpec {
    pub path: SmolStr,
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct WorkspaceResolution {
    pub registry: Option<SmolStr>,
    pub source: Option<SmolStr>,
    pub lockfile: Option<bool>,
    pub extra: toml::value::Table,
}

impl Eq for WorkspaceResolution {}

#[derive(Debug, Clone, Copy)]
pub enum WorkspaceMemberSelection {
    All,
    DefaultOnly,
    PrimaryOnly,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WorkspaceConfig {
    pub workspace: WorkspaceSettings,
    pub diagnostics: Vec<ConfigDiagnostic>,
}

impl WorkspaceSettings {
    pub fn members_for_selection(
        &self,
        selection: WorkspaceMemberSelection,
    ) -> Vec<WorkspaceMemberSpec> {
        let mut members = match selection {
            WorkspaceMemberSelection::PrimaryOnly => self.members.clone(),
            WorkspaceMemberSelection::DefaultOnly => {
                if let Some(defaults) = &self.default_members {
                    let mut combined = self.members.clone();
                    combined.extend(self.dev_members.clone());
                    combined
                        .into_iter()
                        .filter(|member| defaults.contains(&member.path))
                        .collect()
                } else {
                    self.members.clone()
                }
            }
            WorkspaceMemberSelection::All => {
                let mut combined = self.members.clone();
                combined.extend(self.dev_members.clone());
                combined
            }
        };
        members.sort_by(|a, b| {
            a.path
                .cmp(&b.path)
                .then(a.name.cmp(&b.name))
                .then(a.version.cmp(&b.version))
        });
        members.dedup();
        members
    }
}

pub(crate) fn parse_workspace(
    parsed: &Value,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> WorkspaceSettings {
    let Some(table) = parsed.as_table() else {
        return WorkspaceSettings::default();
    };

    let mut workspace = WorkspaceSettings::default();

    parse_identity(table, &mut workspace, diagnostics);
    parse_members(table, &mut workspace, diagnostics);
    parse_default_members(table, &mut workspace, diagnostics);
    parse_exclude(table, &mut workspace, diagnostics);
    parse_metadata(table, &mut workspace, diagnostics);
    parse_profiles(table, &mut workspace, diagnostics);
    parse_scripts(table, &mut workspace, diagnostics);
    workspace.resolution = parse_resolution(table, diagnostics);

    workspace
}

fn parse_identity(
    table: &toml::value::Table,
    workspace: &mut WorkspaceSettings,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) {
    if let Some(name) = table.get("name") {
        match name.as_str() {
            Some(name) => workspace.name = Some(SmolStr::new(name)),
            None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: "name".into(),
                found: name.type_str().to_lowercase().into(),
                expected: Some("string".into()),
            }),
        }
    }

    if let Some(version) = table.get("version") {
        match version.as_str() {
            Some(version) => match version.parse() {
                Ok(parsed) => workspace.version = Some(parsed),
                Err(_) => {
                    diagnostics.push(ConfigDiagnostic::InvalidWorkspaceVersion(version.into()))
                }
            },
            None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: "version".into(),
                found: version.type_str().to_lowercase().into(),
                expected: Some("string".into()),
            }),
        }
    }
}

pub(crate) fn parse_workspace_config(parsed: &Value) -> Result<WorkspaceConfig, String> {
    let mut diagnostics = Vec::new();
    let has_workspace_section = parsed
        .get("workspace")
        .and_then(|value| value.as_table())
        .is_some();
    if !has_workspace_section && super::looks_like_workspace(parsed) {
        diagnostics.push(ConfigDiagnostic::MissingWorkspaceSection);
    }
    let workspace_value = if let (Some(root), Some(workspace)) = (
        parsed.as_table(),
        parsed.get("workspace").and_then(|value| value.as_table()),
    ) {
        let mut merged = root.clone();
        for (key, value) in workspace {
            merged.insert(key.clone(), value.clone());
        }
        Value::Table(merged)
    } else {
        parsed.clone()
    };
    let mut workspace = parse_workspace(&workspace_value, &mut diagnostics);

    workspace.dependencies = dependency::parse_root_dependencies(parsed, &mut diagnostics);

    Ok(WorkspaceConfig {
        workspace,
        diagnostics,
    })
}

fn parse_members(
    table: &toml::value::Table,
    workspace: &mut WorkspaceSettings,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) {
    let Some(value) = table.get("members") else {
        diagnostics.push(ConfigDiagnostic::MissingWorkspaceMembers);
        return;
    };

    match value {
        Value::Array(_entries) => {
            workspace.members = parse_member_array_field("members", value, diagnostics);
        }
        Value::Table(member_table) => {
            if let Some(main) = member_table.get("main") {
                workspace.members = parse_member_array_field("members.main", main, diagnostics);
            } else {
                diagnostics.push(ConfigDiagnostic::MissingWorkspaceMembers);
            }
            if let Some(dev) = member_table.get("dev") {
                workspace.dev_members = parse_member_array_field("members.dev", dev, diagnostics);
            }
        }
        other => {
            diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: "members".into(),
                found: other.type_str().to_lowercase().into(),
                expected: Some("array".into()),
            });
        }
    }
}

fn parse_default_members(
    table: &toml::value::Table,
    workspace: &mut WorkspaceSettings,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) {
    let Some(value) = table.get("default-members") else {
        return;
    };

    let defaults = parse_string_array_field("", "default-members", value, diagnostics);
    workspace.default_members = Some(defaults);
}

fn parse_exclude(
    table: &toml::value::Table,
    workspace: &mut WorkspaceSettings,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) {
    let Some(value) = table.get("exclude") else {
        return;
    };
    workspace.exclude = parse_string_array_field("", "exclude", value, diagnostics);
}

fn parse_member_array_field(
    parent: &str,
    value: &Value,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> Vec<WorkspaceMemberSpec> {
    let Value::Array(entries) = value else {
        diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
            field: parent.into(),
            found: value.type_str().to_lowercase().into(),
            expected: Some("array".into()),
        });
        return Vec::new();
    };

    let mut parsed = Vec::new();
    for entry in entries {
        match entry {
            Value::String(value) => parsed.push(WorkspaceMemberSpec {
                path: SmolStr::new(value),
                name: None,
                version: None,
            }),
            Value::Table(member_table) => {
                let Some(path) = member_table.get("path").and_then(|value| value.as_str()) else {
                    diagnostics.push(ConfigDiagnostic::MissingWorkspaceMemberPath);
                    continue;
                };
                let mut member = WorkspaceMemberSpec {
                    path: SmolStr::new(path),
                    name: None,
                    version: None,
                };
                if let Some(name) = member_table.get("name").and_then(|value| value.as_str()) {
                    if is_valid_name(name) {
                        member.name = Some(SmolStr::new(name));
                    } else {
                        diagnostics.push(ConfigDiagnostic::InvalidWorkspaceMemberName(name.into()));
                    }
                }
                if let Some(version) = member_table.get("version").and_then(|value| value.as_str())
                {
                    match version.parse() {
                        Ok(parsed) => member.version = Some(parsed),
                        Err(_) => diagnostics.push(
                            ConfigDiagnostic::InvalidWorkspaceMemberVersion(version.into()),
                        ),
                    }
                }
                parsed.push(member);
            }
            other => diagnostics.push(ConfigDiagnostic::InvalidWorkspaceMember(
                other.to_string().into(),
            )),
        }
    }
    parsed
}

fn parse_metadata(
    table: &toml::value::Table,
    workspace: &mut WorkspaceSettings,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) {
    if let Some(value) = table.get("metadata") {
        match value.as_table() {
            Some(table) => workspace.metadata = Some(table.clone()),
            None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: "metadata".into(),
                found: value.type_str().to_lowercase().into(),
                expected: Some("table".into()),
            }),
        }
    }
}

fn parse_profiles(
    table: &toml::value::Table,
    workspace: &mut WorkspaceSettings,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) {
    if let Some(value) = table.get("profiles") {
        match value.as_table() {
            Some(table) => workspace.profiles = Some(table.clone()),
            None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: "profiles".into(),
                found: value.type_str().to_lowercase().into(),
                expected: Some("table".into()),
            }),
        }
    }
}

fn parse_scripts(
    table: &toml::value::Table,
    workspace: &mut WorkspaceSettings,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) {
    if let Some(value) = table.get("scripts") {
        match value {
            Value::Table(entries) => {
                for (name, value) in entries {
                    match value.as_str() {
                        Some(command) => workspace.scripts.push(WorkspaceScript {
                            name: SmolStr::new(name),
                            command: SmolStr::new(command),
                        }),
                        None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                            field: format!("scripts.{name}").into(),
                            found: value.type_str().to_lowercase().into(),
                            expected: Some("string".into()),
                        }),
                    }
                }
            }
            other => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: "scripts".into(),
                found: other.type_str().to_lowercase().into(),
                expected: Some("table".into()),
            }),
        }
    }
}

fn parse_resolution(
    table: &toml::value::Table,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> Option<WorkspaceResolution> {
    let value = table.get("resolution")?;

    let Value::Table(entries) = value else {
        diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
            field: "resolution".into(),
            found: value.type_str().to_lowercase().into(),
            expected: Some("table".into()),
        });
        return None;
    };

    let mut resolution = WorkspaceResolution::default();
    for (key, value) in entries {
        match key.as_str() {
            "registry" => match value.as_str() {
                Some(registry) => resolution.registry = Some(registry.into()),
                None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                    field: "resolution.registry".into(),
                    found: value.type_str().to_lowercase().into(),
                    expected: Some("string".into()),
                }),
            },
            "source" => match value.as_str() {
                Some(source) => resolution.source = Some(source.into()),
                None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                    field: "resolution.source".into(),
                    found: value.type_str().to_lowercase().into(),
                    expected: Some("string".into()),
                }),
            },
            "lockfile" => match value.as_bool() {
                Some(lockfile) => resolution.lockfile = Some(lockfile),
                None => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                    field: "resolution.lockfile".into(),
                    found: value.type_str().to_lowercase().into(),
                    expected: Some("boolean".into()),
                }),
            },
            _ => {
                resolution.extra.insert(key.clone(), value.clone());
            }
        }
    }

    Some(resolution)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_workspace_section_with_extras() {
        let toml = r#"
name = "workspace-root"
version = "0.1.0"
members = { main = ["ingot-a", "ingot-b/**"], dev = ["examples/**"] }
default-members = ["ingot-a"]
exclude = ["target", "ignored/**"]

[metadata]
docs = true

[profiles.release]
opt-level = 3

[scripts]
fmt = "fe fmt"
ci = "fe check"

[resolution]
registry = "local"
source = "https://example.com"
lockfile = false
other = "keep"

[dependencies]
util = { path = "ingots/util" }
"#;
        let config_file = crate::config::Config::parse(toml).expect("config parses");
        let crate::config::Config::Workspace(workspace_config) = config_file else {
            panic!("expected workspace config");
        };
        let workspace = workspace_config.workspace;
        assert_eq!(workspace.name.as_deref(), Some("workspace-root"));
        assert_eq!(
            workspace
                .version
                .as_ref()
                .map(|version| version.to_string()),
            Some("0.1.0".to_string())
        );
        assert_eq!(
            workspace.members,
            vec![
                WorkspaceMemberSpec {
                    path: "ingot-a".into(),
                    name: None,
                    version: None,
                },
                WorkspaceMemberSpec {
                    path: "ingot-b/**".into(),
                    name: None,
                    version: None,
                }
            ]
        );
        assert_eq!(
            workspace.dev_members,
            vec![WorkspaceMemberSpec {
                path: "examples/**".into(),
                name: None,
                version: None,
            }]
        );
        assert_eq!(
            workspace.default_members.unwrap(),
            vec![SmolStr::new("ingot-a")]
        );
        assert_eq!(workspace.exclude, vec!["target", "ignored/**"]);
        assert!(workspace.metadata.is_some());
        assert!(workspace.profiles.is_some());
        assert_eq!(workspace.scripts.len(), 2);
        let resolution = workspace.resolution.expect("resolution parsed");
        assert_eq!(resolution.registry.as_deref(), Some("local"));
        assert_eq!(resolution.source.as_deref(), Some("https://example.com"));
        assert_eq!(resolution.lockfile, Some(false));
        assert_eq!(workspace.dependencies.len(), 1);
    }
}
