use std::collections::BTreeMap;

use smol_str::SmolStr;
use toml::Value;

use crate::ingot::Version;

use super::{
    ArithmeticMode, ConfigDiagnostic, DependencyArithmeticMode, ProfileSettings, dependency,
    is_valid_name, parse_arithmetic_field, parse_dependency_arithmetic_field, parse_profiles_table,
};

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct IngotMetadata {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IngotConfig {
    pub metadata: IngotMetadata,
    pub arithmetic: Option<ArithmeticMode>,
    pub dependency_arithmetic: Option<DependencyArithmeticMode>,
    pub profiles: BTreeMap<SmolStr, ProfileSettings>,
    pub dependency_entries: Vec<dependency::DependencyEntry>,
    pub diagnostics: Vec<ConfigDiagnostic>,
}

impl IngotConfig {
    pub fn parse_from_value(parsed: &Value) -> Self {
        let mut diagnostics = Vec::new();
        let (metadata, arithmetic, dependency_arithmetic) = parse_ingot(parsed, &mut diagnostics);
        let profiles = parsed
            .as_table()
            .map(|table| parse_profiles_table(table, &mut diagnostics))
            .unwrap_or_default();
        let dependency_entries = dependency::parse_root_dependencies(parsed, &mut diagnostics);
        Self {
            metadata,
            arithmetic,
            dependency_arithmetic,
            profiles,
            dependency_entries,
            diagnostics,
        }
    }

    pub fn arithmetic_for_profile(&self, profile: &str) -> Option<ArithmeticMode> {
        self.profiles
            .get(profile)
            .and_then(|settings| settings.arithmetic)
    }

    pub fn dependency_arithmetic_for_profile(
        &self,
        profile: &str,
    ) -> Option<DependencyArithmeticMode> {
        self.profiles
            .get(profile)
            .and_then(|settings| settings.dependency_arithmetic)
    }
}

pub(crate) fn parse_ingot(
    parsed: &Value,
    diagnostics: &mut Vec<ConfigDiagnostic>,
) -> (
    IngotMetadata,
    Option<ArithmeticMode>,
    Option<DependencyArithmeticMode>,
) {
    let mut metadata = IngotMetadata::default();
    let mut arithmetic = None;
    let mut dependency_arithmetic = None;

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

        arithmetic = parse_arithmetic_field("ingot", table, diagnostics);
        dependency_arithmetic = parse_dependency_arithmetic_field("ingot", table, diagnostics);
    } else {
        diagnostics.push(ConfigDiagnostic::MissingIngotMetadata);
    }

    (metadata, arithmetic, dependency_arithmetic)
}
