use std::fmt::Display;

use camino::Utf8PathBuf;
use smol_str::SmolStr;
use toml::Value;
use url::Url;

use crate::{
    dependencies::{Dependency, DependencyArguments},
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

                match value {
                    Value::String(path) => {
                        dependencies.push(DependencyEntry::new(
                            alias.into(),
                            Utf8PathBuf::from(path),
                            DependencyArguments::default(),
                        ));
                    }
                    Value::Table(table) => {
                        let path = table.get("path").and_then(|value| value.as_str());
                        if let Some(path) = path {
                            let mut arguments = DependencyArguments::default();
                            if let Some(name) = table.get("name").and_then(|value| value.as_str()) {
                                if is_valid_name(name) {
                                    arguments.name = Some(SmolStr::new(name));
                                } else {
                                    diagnostics
                                        .push(ConfigDiagnostic::InvalidDependencyName(name.into()));
                                }
                            }
                            if let Some(version) =
                                table.get("version").and_then(|value| value.as_str())
                            {
                                if let Ok(version) = version.parse() {
                                    arguments.version = Some(version);
                                } else {
                                    diagnostics
                                        .push(ConfigDiagnostic::InvalidVersion(version.into()));
                                }
                            }
                            dependencies.push(DependencyEntry::new(
                                alias.into(),
                                Utf8PathBuf::from(path),
                                arguments,
                            ));
                        } else {
                            diagnostics.push(ConfigDiagnostic::MissingDependencyPath {
                                alias: alias.into(),
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
            .map(|dependency| {
                let url = base_url.join_directory(&dependency.path).unwrap();
                Dependency {
                    url,
                    alias: dependency.alias.clone(),
                    arguments: dependency.arguments.clone(),
                }
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
    pub path: Utf8PathBuf,
    pub arguments: DependencyArguments,
}

impl DependencyEntry {
    pub fn new(alias: SmolStr, path: Utf8PathBuf, arguments: DependencyArguments) -> Self {
        Self {
            alias,
            path,
            arguments,
        }
    }
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
