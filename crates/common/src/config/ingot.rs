use smol_str::SmolStr;
use toml::Value;

use crate::ingot::Version;

use super::{ConfigDiagnostic, is_valid_name};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct IngotMetadata {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

pub(crate) fn parse_ingot(
    parsed: &Value,
    has_workspace_table: bool,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> IngotMetadata {
    let mut metadata = IngotMetadata::default();

    if let Some(table) = parsed.get("ingot").and_then(|value| value.as_table()) {
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
    } else if !has_workspace_table {
        diagnostics.push(ConfigDiagnostic::MissingIngotMetadata);
    }

    metadata
}
