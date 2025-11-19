pub mod local;
pub mod remote;

pub use local::LocalIngotHandler;
pub use remote::RemoteIngotHandler;

use common::{
    InputDb,
    config::ConfigDiagnostic,
    dependencies::{DependencyArguments, DependencyLocation, GitDependency},
};
use resolver::files::FilesResource;
use smol_str::SmolStr;
use url::Url;

use crate::IngotInitDiagnostics;

#[derive(Clone, Debug)]
pub(super) enum ProcessedDependency {
    Local {
        alias: SmolStr,
        arguments: DependencyArguments,
        url: Url,
    },
    Git {
        alias: SmolStr,
        arguments: DependencyArguments,
        git: GitDependency,
    },
}

#[derive(Clone, Copy, Debug)]
pub(super) enum DiagnosticKind {
    Local,
    Remote,
}

impl DiagnosticKind {
    fn push_config_parse_error(
        &self,
        diagnostics: &mut Vec<IngotInitDiagnostics>,
        ingot_url: Url,
        error: String,
    ) {
        diagnostics.push(match self {
            Self::Local => IngotInitDiagnostics::ConfigParseError { ingot_url, error },
            Self::Remote => IngotInitDiagnostics::RemoteConfigParseError { ingot_url, error },
        });
    }

    fn push_config_diagnostics(
        &self,
        diagnostics: &mut Vec<IngotInitDiagnostics>,
        ingot_url: Url,
        config_diags: &[ConfigDiagnostic],
    ) {
        diagnostics.push(match self {
            Self::Local => IngotInitDiagnostics::ConfigDiagnostics {
                ingot_url,
                diagnostics: config_diags.to_vec(),
            },
            Self::Remote => IngotInitDiagnostics::RemoteConfigDiagnostics {
                ingot_url,
                diagnostics: config_diags.to_vec(),
            },
        });
    }
}

#[derive(Clone, Copy, Debug)]
pub(super) enum ProcessingError {
    MissingConfigFile,
    MissingWorkspaceIngot,
    MissingParsedConfig,
    ConfigParseError,
}

pub(super) fn process_ingot_resource(
    db: &mut dyn InputDb,
    ingot_url: &Url,
    resource: FilesResource,
    diagnostics: &mut Vec<IngotInitDiagnostics>,
    diagnostic_kind: DiagnosticKind,
) -> Result<Vec<ProcessedDependency>, ProcessingError> {
    let mut config_found = false;

    for file in resource.files {
        let file_url =
            Url::from_file_path(file.path.as_std_path()).expect("resolved file has valid URL");
        if file.path.as_str().ends_with("fe.toml") {
            config_found = true;
            db.workspace()
                .touch(db, file_url, Some(file.content.clone()));
        } else {
            db.workspace().touch(db, file_url, Some(file.content));
        }
    }

    if !config_found {
        return Err(ProcessingError::MissingConfigFile);
    }

    let Some(ingot) = db.workspace().containing_ingot(db, ingot_url.clone()) else {
        tracing::error!("Unable to locate ingot for config: {}", ingot_url);
        return Err(ProcessingError::MissingWorkspaceIngot);
    };

    if let Some(parse_error) = ingot.config_parse_error(db) {
        diagnostic_kind.push_config_parse_error(diagnostics, ingot_url.clone(), parse_error);
        return Err(ProcessingError::ConfigParseError);
    }

    let Some(parsed_config) = ingot.config(db) else {
        return Err(ProcessingError::MissingParsedConfig);
    };

    if !parsed_config.diagnostics.is_empty() {
        diagnostic_kind.push_config_diagnostics(
            diagnostics,
            ingot_url.clone(),
            &parsed_config.diagnostics,
        );
    }

    let mut dependencies = Vec::new();
    for dependency in parsed_config.dependencies(ingot_url) {
        match dependency.location {
            DependencyLocation::Local(local) => dependencies.push(ProcessedDependency::Local {
                alias: dependency.alias,
                arguments: dependency.arguments,
                url: local.url,
            }),
            DependencyLocation::Git(git) => dependencies.push(ProcessedDependency::Git {
                alias: dependency.alias,
                arguments: dependency.arguments,
                git,
            }),
        }
    }

    Ok(dependencies)
}
