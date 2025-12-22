use smol_str::SmolStr;
use toml::Value;

use crate::ingot::Version;

use super::{ConfigDiagnostic, dependency, is_valid_name};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct IngotMetadata {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IngotConfig {
    pub metadata: IngotMetadata,
    pub dependency_entries: Vec<dependency::DependencyEntry>,
    pub diagnostics: Vec<ConfigDiagnostic>,
}

impl IngotConfig {
    pub fn parse_from_value(parsed: &Value) -> Self {
        let mut diagnostics = Vec::new();
        let metadata = parse_ingot(parsed, &mut diagnostics);
        let dependency_entries = dependency::parse_root_dependencies(parsed, &mut diagnostics);
        Self {
            metadata,
            dependency_entries,
            diagnostics,
        }
    }
}

pub(crate) fn parse_ingot(
    parsed: &Value,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> IngotMetadata {
    let mut metadata = IngotMetadata::default();

    let table = match parsed.get("ingot").and_then(|value| value.as_table()) {
        Some(table) => Some(table),
        None => parsed.as_table(),
    };

    if let Some(table) = table {
        if let Some(name) = table.get("name") {
            match name.as_str() {
                Some(name) if is_valid_name(name) => metadata.name = Some(SmolStr::new(name)),
                Some(name) => diagnostics.push(ConfigDiagnostic::InvalidName(SmolStr::new(name))),
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

    metadata
}
