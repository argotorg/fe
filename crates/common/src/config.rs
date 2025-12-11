use std::fmt::Display;

use camino::Utf8PathBuf;
use smol_str::SmolStr;
use toml::Value;
use url::Url;

use crate::{
    dependencies::{Dependency, DependencyArguments, DependencyLocation, LocalFiles, RemoteFiles},
    ingot::Version,
    urlext::UrlExt,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Config {
    pub metadata: IngotMetadata,
    pub dependency_entries: Vec<DependencyEntry>,
    pub diagnostics: Vec<ConfigDiagnostic>,
}

impl Config {
    pub fn parse(content: &str) -> Result<Self, String> {
        let mut diagnostics = Vec::new();
        let mut metadata = IngotMetadata::default();
        let mut dependencies = Vec::new();

        let parsed: Value = content
            .parse()
            .map_err(|e: toml::de::Error| e.to_string())?;

        if let Some(table) = parsed.get("ingot").and_then(|value| value.as_table()) {
            if let Some(name) = table.get("name") {
                match name.as_str() {
                    Some(name) if is_valid_name(name) => metadata.name = Some(SmolStr::new(name)),
                    Some(name) => {
                        diagnostics.push(ConfigDiagnostic::InvalidName(SmolStr::new(name)))
                    }
                    None => diagnostics.push(ConfigDiagnostic::InvalidName(SmolStr::new(
                        name.to_string(),
                    ))),
                }
            } else {
                diagnostics.push(ConfigDiagnostic::MissingName);
            }

            if let Some(version) = table.get("version") {
                match version.as_str().and_then(|value| value.parse().ok()) {
                    Some(version) => metadata.version = Some(version),
                    None => diagnostics.push(ConfigDiagnostic::InvalidVersion(SmolStr::from(
                        version.to_string(),
                    ))),
                }
            } else {
                diagnostics.push(ConfigDiagnostic::MissingVersion);
            }
        } else {
            diagnostics.push(ConfigDiagnostic::MissingIngotMetadata);
        }

        if let Some(table) = parsed
            .get("dependencies")
            .and_then(|value| value.as_table())
        {
            for (alias, value) in table {
                // Validate dependency alias
                if !is_valid_name(alias) {
                    diagnostics.push(ConfigDiagnostic::InvalidDependencyAlias(alias.into()));
                }

                let alias = SmolStr::new(alias);
                match value {
                    Value::String(path) => {
                        dependencies.push(DependencyEntry::new(
                            alias.clone(),
                            DependencyEntryLocation::RelativePath(Utf8PathBuf::from(path)),
                            DependencyArguments::default(),
                        ));
                    }
                    Value::Table(table) => {
                        let mut arguments = DependencyArguments::default();
                        if let Some(name) = table.get("name").and_then(|value| value.as_str()) {
                            if is_valid_name(name) {
                                arguments.name = Some(SmolStr::new(name));
                            } else {
                                diagnostics
                                    .push(ConfigDiagnostic::InvalidDependencyName(name.into()));
                            }
                        }
                        if let Some(version) = table.get("version").and_then(|value| value.as_str())
                        {
                            if let Ok(version) = version.parse() {
                                arguments.version = Some(version);
                            } else {
                                diagnostics.push(ConfigDiagnostic::InvalidVersion(version.into()));
                            }
                        }

                        if table.contains_key("source") {
                            match parse_git_dependency(&alias, table) {
                                Ok(remote) => dependencies.push(DependencyEntry::new(
                                    alias.clone(),
                                    DependencyEntryLocation::Remote(remote),
                                    arguments,
                                )),
                                Err(diag) => diagnostics.push(diag),
                            }
                        } else if let Some(path) =
                            table.get("path").and_then(|value| value.as_str())
                        {
                            dependencies.push(DependencyEntry::new(
                                alias.clone(),
                                DependencyEntryLocation::RelativePath(Utf8PathBuf::from(path)),
                                arguments,
                            ));
                        } else {
                            diagnostics.push(ConfigDiagnostic::MissingDependencyPath {
                                alias: alias.clone(),
                                description: value.to_string(),
                            });
                        }
                    }
                    value => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                        field: "dependencies".into(),
                        found: value.type_str().to_lowercase().into(),
                        expected: Some("table".into()),
                    }),
                }
            }
        }

        Ok(Self {
            metadata,
            dependency_entries: dependencies,
            diagnostics,
        })
    }

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

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct IngotMetadata {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependencyEntry {
    pub alias: SmolStr,
    pub location: DependencyEntryLocation,
    pub arguments: DependencyArguments,
}

impl DependencyEntry {
    pub fn new(
        alias: SmolStr,
        location: DependencyEntryLocation,
        arguments: DependencyArguments,
    ) -> Self {
        Self {
            alias,
            location,
            arguments,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DependencyEntryLocation {
    RelativePath(Utf8PathBuf),
    Remote(RemoteFiles),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConfigDiagnostic {
    MissingIngotMetadata,
    MissingName,
    MissingVersion,
    InvalidName(SmolStr),
    InvalidVersion(SmolStr),
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

fn is_valid_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_valid_name(s: &str) -> bool {
    s.chars().all(is_valid_name_char)
}

fn parse_git_dependency(
    alias: &SmolStr,
    table: &toml::map::Map<String, Value>,
) -> Result<RemoteFiles, ConfigDiagnostic> {
    let source_value = table
        .get("source")
        .and_then(|value| value.as_str())
        .ok_or_else(|| ConfigDiagnostic::MissingDependencySource {
            alias: alias.clone(),
        })?;

    let source =
        Url::parse(source_value).map_err(|_| ConfigDiagnostic::InvalidDependencySource {
            alias: alias.clone(),
            value: source_value.into(),
        })?;

    let rev_value = table
        .get("rev")
        .and_then(|value| value.as_str())
        .ok_or_else(|| ConfigDiagnostic::MissingDependencyRev {
            alias: alias.clone(),
        })?;

    let path = table
        .get("path")
        .and_then(|value| value.as_str())
        .map(Utf8PathBuf::from);

    Ok(RemoteFiles {
        source,
        rev: SmolStr::new(rev_value),
        path,
    })
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
        let config = Config::parse(toml).expect("config parses");
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
        let config = Config::parse(toml).expect("config parses");
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
}
