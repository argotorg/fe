use std::fmt::Display;

use smol_str::SmolStr;
use toml::Value;
use url::Url;

use crate::{
    dependencies::{Dependency, DependencyLocation, LocalFiles},
    urlext::UrlExt,
};

mod dependency;
mod ingot;
mod workspace;

pub use dependency::{DependencyEntry, DependencyEntryLocation, parse_dependencies_table};
pub use ingot::{IngotConfig, IngotMetadata};
pub use workspace::{
    WorkspaceConfig, WorkspaceMemberSelection, WorkspaceResolution, WorkspaceSettings,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Config {
    Ingot(IngotConfig),
    Workspace(Box<WorkspaceConfig>),
}

impl Config {
    pub fn parse(content: &str) -> Result<Self, String> {
        let parsed: Value = content
            .parse()
            .map_err(|e: toml::de::Error| e.to_string())?;

        let has_ingot_table = parsed.get("ingot").is_some();
        let has_workspace_table = parsed.get("workspace").is_some();

        if has_ingot_table && has_workspace_table {
            return Err("config cannot contain both [ingot] and [workspace] sections".to_string());
        }

        if has_ingot_table {
            return Ok(Config::Ingot(ingot::IngotConfig::parse_from_value(&parsed)));
        }

        if has_workspace_table || looks_like_workspace(&parsed) {
            return workspace::parse_workspace_config(&parsed)
                .map(|config| Config::Workspace(Box::new(config)));
        }

        Ok(Config::Ingot(ingot::IngotConfig::parse_from_value(&parsed)))
    }
}

pub fn looks_like_workspace(parsed: &Value) -> bool {
    let Some(table) = parsed.as_table() else {
        return false;
    };

    table.contains_key("members")
        || table.contains_key("default-members")
        || table.contains_key("exclude")
        || table.contains_key("resolution")
        || table.contains_key("profiles")
        || table.contains_key("scripts")
}

pub fn is_workspace_content(content: &str) -> bool {
    let parsed: Value = match content.parse() {
        Ok(parsed) => parsed,
        Err(_) => return false,
    };
    parsed.get("workspace").is_some() || looks_like_workspace(&parsed)
}

impl IngotConfig {
    pub fn dependencies(&self, base_url: &Url) -> Vec<Dependency> {
        self.dependency_entries
            .iter()
            .map(|dependency| match &dependency.location {
                DependencyEntryLocation::RelativePath(path) => {
                    let url = base_url.join_directory(path).unwrap();
                    Dependency {
                        alias: dependency.alias.clone(),
                        arguments: dependency.arguments.clone(),
                        location: DependencyLocation::Local(LocalFiles {
                            path: path.clone(),
                            url,
                        }),
                    }
                }
                DependencyEntryLocation::Remote(remote) => Dependency {
                    alias: dependency.alias.clone(),
                    arguments: dependency.arguments.clone(),
                    location: DependencyLocation::Remote(remote.clone()),
                },
                DependencyEntryLocation::WorkspaceCurrent => Dependency {
                    alias: dependency.alias.clone(),
                    arguments: dependency.arguments.clone(),
                    location: DependencyLocation::WorkspaceCurrent,
                },
            })
            .collect()
    }

    pub fn formatted_diagnostics(&self) -> Option<String> {
        if self.diagnostics.is_empty() {
            None
        } else {
            Some(
                self.diagnostics
                    .iter()
                    .map(|diag| format!("  {diag}"))
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConfigDiagnostic {
    MissingIngotMetadata,
    MissingName,
    MissingVersion,
    InvalidName(SmolStr),
    InvalidVersion(SmolStr),
    MissingWorkspaceMembers,
    InvalidWorkspaceMember(SmolStr),
    InvalidWorkspaceDevMember(SmolStr),
    InvalidWorkspaceDefaultMember(SmolStr),
    InvalidWorkspaceExclude(SmolStr),
    InvalidWorkspaceMemberName(SmolStr),
    InvalidWorkspaceMemberVersion(SmolStr),
    InvalidWorkspaceVersion(SmolStr),
    MissingWorkspaceSection,
    MissingWorkspaceMemberPath,
    ConflictingWorkspaceMembersSpec,
    InvalidDependencyAlias(SmolStr),
    InvalidDependencyName(SmolStr),
    InvalidDependencyVersion(SmolStr),
    MissingDependencyPath {
        alias: SmolStr,
        description: String,
    },
    MissingDependencySource {
        alias: SmolStr,
    },
    MissingDependencyRev {
        alias: SmolStr,
    },
    InvalidDependencySource {
        alias: SmolStr,
        value: SmolStr,
    },
    UnexpectedTomlData {
        field: SmolStr,
        found: SmolStr,
        expected: Option<SmolStr>,
    },
}

impl Display for ConfigDiagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingIngotMetadata => write!(f, "Missing ingot metadata"),
            Self::MissingName => write!(f, "Missing ingot name"),
            Self::MissingVersion => write!(f, "Missing ingot version"),
            Self::InvalidName(name) => write!(f, "Invalid ingot name \"{name}\""),
            Self::InvalidVersion(version) => write!(f, "Invalid ingot version \"{version}\""),
            Self::MissingWorkspaceMembers => write!(f, "Missing workspace members"),
            Self::InvalidWorkspaceMember(value) => {
                write!(f, "Invalid workspace member entry \"{value}\"")
            }
            Self::InvalidWorkspaceDevMember(value) => {
                write!(f, "Invalid workspace dev member entry \"{value}\"")
            }
            Self::InvalidWorkspaceDefaultMember(value) => {
                write!(f, "Invalid workspace default member entry \"{value}\"")
            }
            Self::InvalidWorkspaceExclude(value) => {
                write!(f, "Invalid workspace exclude entry \"{value}\"")
            }
            Self::InvalidWorkspaceMemberName(value) => {
                write!(f, "Invalid workspace member name \"{value}\"")
            }
            Self::InvalidWorkspaceMemberVersion(value) => {
                write!(f, "Invalid workspace member version \"{value}\"")
            }
            Self::InvalidWorkspaceVersion(value) => {
                write!(f, "Invalid workspace version \"{value}\"")
            }
            Self::MissingWorkspaceSection => {
                write!(f, "Workspace config is missing a [workspace] section")
            }
            Self::MissingWorkspaceMemberPath => write!(f, "Workspace member is missing a path"),
            Self::ConflictingWorkspaceMembersSpec => {
                write!(f, "Cannot mix flat and categorized workspace members")
            }
            Self::InvalidDependencyAlias(alias) => {
                write!(f, "Invalid dependency alias \"{alias}\"")
            }
            Self::InvalidDependencyName(name) => {
                write!(f, "Invalid dependency name \"{name}\"")
            }
            Self::InvalidDependencyVersion(version) => {
                write!(f, "Invalid dependency version \"{version}\"")
            }
            Self::MissingDependencyPath { alias, description } => write!(
                f,
                "The dependency \"{alias}\" is missing a path argument \"{description}\""
            ),
            Self::MissingDependencySource { alias } => {
                write!(f, "The dependency \"{alias}\" is missing a source field")
            }
            Self::MissingDependencyRev { alias } => {
                write!(f, "The dependency \"{alias}\" is missing a rev field")
            }
            Self::InvalidDependencySource { alias, value } => write!(
                f,
                "The dependency \"{alias}\" has an invalid source \"{value}\""
            ),
            Self::UnexpectedTomlData {
                field,
                found,
                expected,
            } => {
                if let Some(expected) = expected {
                    write!(
                        f,
                        "Expected a {expected} in field {field}, but found a {found}"
                    )
                } else {
                    write!(f, "Unexpected field {field}")
                }
            }
        }
    }
}

pub(crate) fn is_valid_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

pub(crate) fn is_valid_name(s: &str) -> bool {
    s.chars().all(is_valid_name_char)
}

pub(crate) fn parse_string_array_field(
    parent: &str,
    key: &str,
    value: &Value,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> Vec<SmolStr> {
    match value {
        Value::Array(entries) => {
            let mut parsed = Vec::new();
            for entry in entries {
                if let Some(value) = entry.as_str() {
                    parsed.push(SmolStr::new(value));
                } else {
                    diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                        field: format_field_path(parent, key).into(),
                        found: entry.type_str().to_lowercase().into(),
                        expected: Some("string".into()),
                    });
                }
            }
            parsed
        }
        other => {
            diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: format_field_path(parent, key).into(),
                found: other.type_str().to_lowercase().into(),
                expected: Some("array".into()),
            });
            vec![]
        }
    }
}

fn format_field_path(parent: &str, key: &str) -> String {
    if parent.is_empty() {
        key.to_string()
    } else {
        format!("{parent}.{key}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_git_dependency_entry() {
        let toml = r#"
[ingot]
name = "root"
version = "1.0.0"

[dependencies]
remote = { source = "https://example.com/fe.git", rev = "abcd1234", path = "contracts" }
"#;
        let config_file = Config::parse(toml).expect("config parses");
        let Config::Ingot(config) = config_file else {
            panic!("expected ingot config");
        };
        assert!(
            config.diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            config.diagnostics
        );
        let base = Url::parse("file:///workspace/root/").unwrap();
        let dependencies = config.dependencies(&base);
        assert_eq!(dependencies.len(), 1);
        match &dependencies[0].location {
            DependencyLocation::Remote(remote) => {
                assert_eq!(remote.source.as_str(), "https://example.com/fe.git");
                assert_eq!(remote.rev, "abcd1234");
                assert_eq!(
                    remote.path.as_ref().map(|path| path.as_str()),
                    Some("contracts")
                );
            }
            other => panic!("expected git dependency, found {other:?}"),
        }
    }

    #[test]
    fn reports_diagnostics_for_incomplete_git_dependency() {
        let toml = r#"
[ingot]
name = "root"
version = "1.0.0"

[dependencies]
missing_rev = { source = "https://example.com/repo.git" }
invalid_source = { source = "not a url", rev = "1234" }
"#;
        let config_file = Config::parse(toml).expect("config parses");
        let Config::Ingot(config) = config_file else {
            panic!("expected ingot config");
        };
        assert!(
            config
                .diagnostics
                .iter()
                .any(|diag| matches!(diag, ConfigDiagnostic::MissingDependencyRev { .. }))
        );
        assert!(
            config
                .diagnostics
                .iter()
                .any(|diag| matches!(diag, ConfigDiagnostic::InvalidDependencySource { .. }))
        );
    }

    #[test]
    fn parses_name_only_dependency() {
        let toml = r#"
[ingot]
name = "root"
version = "1.0.0"

[dependencies]
util = { name = "utils" }
"#;
        let config_file = Config::parse(toml).expect("config parses");
        let Config::Ingot(config) = config_file else {
            panic!("expected ingot config");
        };
        assert_eq!(
            config
                .dependencies(&Url::parse("file:///workspace/root/").unwrap())
                .len(),
            1
        );
        let dependency = &config.dependencies(&Url::parse("file:///workspace/root/").unwrap())[0];
        assert_eq!(dependency.arguments.name.as_deref(), Some("utils"));
        assert!(matches!(
            dependency.location,
            DependencyLocation::WorkspaceCurrent
        ));
    }

    #[test]
    fn parses_alias_only_dependency() {
        let toml = r#"
[ingot]
name = "root"
version = "1.0.0"

[dependencies]
util = true
"#;
        let config_file = Config::parse(toml).expect("config parses");
        let Config::Ingot(config) = config_file else {
            panic!("expected ingot config");
        };
        let dependency = &config.dependencies(&Url::parse("file:///workspace/root/").unwrap())[0];
        assert_eq!(dependency.arguments.name.as_deref(), Some("util"));
        assert!(matches!(
            dependency.location,
            DependencyLocation::WorkspaceCurrent
        ));
    }

    #[test]
    fn parses_alias_version_dependency() {
        let toml = r#"
[ingot]
name = "root"
version = "1.0.0"

[dependencies]
util = "0.1.0"
"#;
        let config_file = Config::parse(toml).expect("config parses");
        let Config::Ingot(config) = config_file else {
            panic!("expected ingot config");
        };
        let dependency = &config.dependencies(&Url::parse("file:///workspace/root/").unwrap())[0];
        assert_eq!(dependency.arguments.name.as_deref(), Some("util"));
        assert_eq!(
            dependency
                .arguments
                .version
                .as_ref()
                .map(|version| version.to_string()),
            Some("0.1.0".to_string())
        );
        assert!(matches!(
            dependency.location,
            DependencyLocation::WorkspaceCurrent
        ));
    }
}
