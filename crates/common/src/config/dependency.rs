use camino::Utf8PathBuf;
use smol_str::SmolStr;
use toml::Value;
use url::Url;

use crate::dependencies::RemoteFiles;

use super::{ConfigDiagnostic, is_valid_name};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DependencyEntry {
    pub alias: SmolStr,
    pub location: DependencyEntryLocation,
    pub arguments: crate::dependencies::DependencyArguments,
}

impl DependencyEntry {
    pub fn new(
        alias: SmolStr,
        location: DependencyEntryLocation,
        arguments: crate::dependencies::DependencyArguments,
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
    WorkspaceCurrent,
}

pub fn parse_root_dependencies(
    parsed: &Value,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> Vec<DependencyEntry> {
    parsed
        .get("dependencies")
        .and_then(|value| value.as_table())
        .map(|table| parse_dependencies_table(table, "dependencies", diagnostics))
        .unwrap_or_default()
}

pub fn parse_dependencies_table(
    table: &toml::map::Map<String, Value>,
    field_prefix: &str,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> Vec<DependencyEntry> {
    let mut dependencies = Vec::new();
    for (alias, value) in table {
        if !is_valid_name(alias) {
            diagnostics.push(ConfigDiagnostic::InvalidDependencyAlias(alias.into()));
        }

        let alias = SmolStr::new(alias);
        match value {
            Value::String(path) => {
                if let Ok(version) = path.parse() {
                    let arguments = crate::dependencies::DependencyArguments {
                        name: Some(alias.clone()),
                        version: Some(version),
                    };
                    dependencies.push(DependencyEntry::new(
                        alias.clone(),
                        DependencyEntryLocation::WorkspaceCurrent,
                        arguments,
                    ));
                } else {
                    dependencies.push(DependencyEntry::new(
                        alias.clone(),
                        DependencyEntryLocation::RelativePath(Utf8PathBuf::from(path)),
                        crate::dependencies::DependencyArguments::default(),
                    ));
                }
            }
            Value::Boolean(true) => {
                let arguments = crate::dependencies::DependencyArguments {
                    name: Some(alias.clone()),
                    ..Default::default()
                };
                dependencies.push(DependencyEntry::new(
                    alias.clone(),
                    DependencyEntryLocation::WorkspaceCurrent,
                    arguments,
                ));
            }
            Value::Table(table) => {
                let mut arguments = crate::dependencies::DependencyArguments::default();
                let mut has_name_field = false;
                let mut has_name = false;
                if let Some(name) = table.get("name") {
                    has_name_field = true;
                    if let Some(name) = name.as_str() {
                        if is_valid_name(name) {
                            arguments.name = Some(SmolStr::new(name));
                            has_name = true;
                        } else {
                            diagnostics.push(ConfigDiagnostic::InvalidDependencyName(name.into()));
                        }
                    } else {
                        diagnostics.push(ConfigDiagnostic::InvalidDependencyName(
                            name.to_string().into(),
                        ));
                    }
                }
                if let Some(version) = table.get("version").and_then(|value| value.as_str()) {
                    if let Ok(version) = version.parse() {
                        arguments.version = Some(version);
                    } else {
                        diagnostics
                            .push(ConfigDiagnostic::InvalidDependencyVersion(version.into()));
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
                } else if let Some(path) = table.get("path").and_then(|value| value.as_str()) {
                    dependencies.push(DependencyEntry::new(
                        alias.clone(),
                        DependencyEntryLocation::RelativePath(Utf8PathBuf::from(path)),
                        arguments,
                    ));
                } else if has_name || (!has_name_field && arguments.version.is_some()) {
                    if arguments.name.is_none() {
                        arguments.name = Some(alias.clone());
                    }
                    dependencies.push(DependencyEntry::new(
                        alias.clone(),
                        DependencyEntryLocation::WorkspaceCurrent,
                        arguments,
                    ));
                } else if !has_name_field {
                    arguments.name = Some(alias.clone());
                    dependencies.push(DependencyEntry::new(
                        alias.clone(),
                        DependencyEntryLocation::WorkspaceCurrent,
                        arguments,
                    ));
                } else {
                    diagnostics.push(ConfigDiagnostic::MissingDependencyPath {
                        alias: alias.clone(),
                        description: format!("{field_prefix}.{alias}"),
                    });
                }
            }
            value => diagnostics.push(ConfigDiagnostic::UnexpectedTomlData {
                field: field_prefix.into(),
                found: value.type_str().to_lowercase().into(),
                expected: Some("table".into()),
            }),
        }
    }
    dependencies
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
