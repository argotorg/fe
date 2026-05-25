use std::fmt;
use std::path::Path;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FeToolingConfig {
    pub lsp: LspToolingConfig,
}

impl Default for FeToolingConfig {
    fn default() -> Self {
        Self {
            lsp: LspToolingConfig::default(),
        }
    }
}

impl FeToolingConfig {
    pub fn load_from_workspace(root: impl AsRef<Path>) -> Result<Self, ConfigLoadError> {
        Self::load_from_sources(
            Some(root.as_ref()),
            Option::<&serde_json::Value>::None,
            FeToolingConfigPatch::default(),
        )
    }

    pub fn load_from_sources(
        workspace_root: Option<&Path>,
        lsp_settings: Option<&serde_json::Value>,
        cli_overrides: FeToolingConfigPatch,
    ) -> Result<Self, ConfigLoadError> {
        let mut config = Self::default();
        if let Some(root) = workspace_root {
            config.apply_patch(project_patch(root)?);
        }
        if let Some(settings) = lsp_settings {
            config.apply_patch(FeToolingConfigPatch::from_lsp_settings(settings)?);
        }
        config.apply_patch(cli_overrides);
        Ok(config)
    }

    pub fn apply_patch(&mut self, patch: FeToolingConfigPatch) {
        if let Some(lsp) = patch.lsp {
            self.lsp.apply_patch(lsp);
        }
    }

    pub fn stable_hash(&self) -> String {
        let json = serde_json::to_vec(self).expect("tooling config should serialize");
        format!("blake3:{}", blake3::hash(&json).to_hex())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LspToolingConfig {
    pub enabled: bool,
    pub trace_mode: TraceMode,
    pub max_analysis_ms: u64,
    pub max_hints_per_file: usize,
    pub inlay_hints: InlayHintsConfig,
    pub hover: HoverConfig,
    pub code_lens: CodeLensConfig,
    pub trace: TraceConfig,
    pub gas: GasConfig,
    pub live: LiveConfig,
}

impl Default for LspToolingConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            trace_mode: TraceMode::Manual,
            max_analysis_ms: 500,
            max_hints_per_file: 80,
            inlay_hints: InlayHintsConfig::default(),
            hover: HoverConfig::default(),
            code_lens: CodeLensConfig::default(),
            trace: TraceConfig::default(),
            gas: GasConfig::default(),
            live: LiveConfig::default(),
        }
    }
}

impl LspToolingConfig {
    fn apply_patch(&mut self, patch: LspToolingConfigPatch) {
        assign(&mut self.enabled, patch.enabled);
        assign(&mut self.trace_mode, patch.trace_mode);
        assign(&mut self.max_analysis_ms, patch.max_analysis_ms);
        assign(&mut self.max_hints_per_file, patch.max_hints_per_file);
        if let Some(inlay_hints) = patch.inlay_hints {
            self.inlay_hints.apply_patch(inlay_hints);
        }
        if let Some(hover) = patch.hover {
            self.hover.apply_patch(hover);
        }
        if let Some(code_lens) = patch.code_lens {
            self.code_lens.apply_patch(code_lens);
        }
        if let Some(trace) = patch.trace {
            self.trace.apply_patch(trace);
        }
        if let Some(gas) = patch.gas {
            self.gas.apply_patch(gas);
        }
        if let Some(live) = patch.live {
            self.live.apply_patch(live);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InlayHintsConfig {
    pub types: bool,
    pub gas: HintMode,
    pub loop_cost: LoopHintMode,
    pub storage: StorageHintMode,
    pub ir_phase: IrPhaseHint,
}

impl Default for InlayHintsConfig {
    fn default() -> Self {
        Self {
            types: true,
            gas: HintMode::Off,
            loop_cost: LoopHintMode::Off,
            storage: StorageHintMode::HoverOnly,
            ir_phase: IrPhaseHint::None,
        }
    }
}

impl InlayHintsConfig {
    fn apply_patch(&mut self, patch: InlayHintsConfigPatch) {
        assign(&mut self.types, patch.types);
        assign(&mut self.gas, patch.gas);
        assign(&mut self.loop_cost, patch.loop_cost);
        assign(&mut self.storage, patch.storage);
        assign(&mut self.ir_phase, patch.ir_phase);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct HoverConfig {
    pub origin_chain: bool,
    pub storage_history: bool,
    pub gas_breakdown: bool,
    pub max_origin_depth: usize,
    pub show_confidence: bool,
}

impl Default for HoverConfig {
    fn default() -> Self {
        Self {
            origin_chain: true,
            storage_history: false,
            gas_breakdown: false,
            max_origin_depth: 8,
            show_confidence: true,
        }
    }
}

impl HoverConfig {
    fn apply_patch(&mut self, patch: HoverConfigPatch) {
        assign(&mut self.origin_chain, patch.origin_chain);
        assign(&mut self.storage_history, patch.storage_history);
        assign(&mut self.gas_breakdown, patch.gas_breakdown);
        assign(&mut self.max_origin_depth, patch.max_origin_depth);
        assign(&mut self.show_confidence, patch.show_confidence);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CodeLensConfig {
    pub view_ir: bool,
    pub trace_loop: bool,
    pub gas_breakdown: bool,
    pub open_graph: bool,
}

impl Default for CodeLensConfig {
    fn default() -> Self {
        Self {
            view_ir: true,
            trace_loop: false,
            gas_breakdown: false,
            open_graph: false,
        }
    }
}

impl CodeLensConfig {
    fn apply_patch(&mut self, patch: CodeLensConfigPatch) {
        assign(&mut self.view_ir, patch.view_ir);
        assign(&mut self.trace_loop, patch.trace_loop);
        assign(&mut self.gas_breakdown, patch.gas_breakdown);
        assign(&mut self.open_graph, patch.open_graph);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TraceConfig {
    pub emit_jsonl: bool,
    pub out_dir: String,
    pub validate: bool,
    pub level: TraceLevel,
    pub max_trace_facts: usize,
    pub max_shape_nodes: usize,
    pub max_query_ms: u64,
    pub debounce_ms: u64,
}

impl Default for TraceConfig {
    fn default() -> Self {
        Self {
            emit_jsonl: false,
            out_dir: "target/fe-traces".to_string(),
            validate: true,
            level: TraceLevel::Summary,
            max_trace_facts: 100_000,
            max_shape_nodes: 50_000,
            max_query_ms: 1_000,
            debounce_ms: 75,
        }
    }
}

impl TraceConfig {
    fn apply_patch(&mut self, patch: TraceConfigPatch) {
        assign(&mut self.emit_jsonl, patch.emit_jsonl);
        assign(&mut self.out_dir, patch.out_dir);
        assign(&mut self.validate, patch.validate);
        assign(&mut self.level, patch.level);
        assign(&mut self.max_trace_facts, patch.max_trace_facts);
        assign(&mut self.max_shape_nodes, patch.max_shape_nodes);
        assign(&mut self.max_query_ms, patch.max_query_ms);
        assign(&mut self.debounce_ms, patch.debounce_ms);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GasConfig {
    pub enabled: bool,
    pub mode: GasMode,
    pub schedule: String,
    pub show_uncertain: bool,
}

impl Default for GasConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            mode: GasMode::Static,
            schedule: "cancun".to_string(),
            show_uncertain: false,
        }
    }
}

impl GasConfig {
    fn apply_patch(&mut self, patch: GasConfigPatch) {
        assign(&mut self.enabled, patch.enabled);
        assign(&mut self.mode, patch.mode);
        assign(&mut self.schedule, patch.schedule);
        assign(&mut self.show_uncertain, patch.show_uncertain);
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LiveConfig {
    pub write_server_info: bool,
    pub http: bool,
    pub browser_graphs: bool,
    pub bind: String,
    pub require_token: bool,
}

impl Default for LiveConfig {
    fn default() -> Self {
        Self {
            write_server_info: true,
            http: true,
            browser_graphs: false,
            bind: "127.0.0.1".to_string(),
            require_token: true,
        }
    }
}

impl LiveConfig {
    fn apply_patch(&mut self, patch: LiveConfigPatch) {
        assign(&mut self.write_server_info, patch.write_server_info);
        assign(&mut self.http, patch.http);
        assign(&mut self.browser_graphs, patch.browser_graphs);
        assign(&mut self.bind, patch.bind);
        assign(&mut self.require_token, patch.require_token);
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceMode {
    Off,
    Manual,
    OnSave,
    Background,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum HintMode {
    Off,
    Summary,
    Hotspots,
    All,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LoopHintMode {
    Off,
    Hotspots,
    All,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StorageHintMode {
    Off,
    #[serde(rename = "hover-only", alias = "hover_only")]
    HoverOnly,
    Hotspots,
    All,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum IrPhaseHint {
    None,
    Hir,
    Mir,
    Sonatina,
    Bytecode,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceLevel {
    Summary,
    Detailed,
    Exhaustive,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GasMode {
    Static,
    TestTrace,
    Both,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FeToolingConfigPatch {
    pub lsp: Option<LspToolingConfigPatch>,
}

impl FeToolingConfigPatch {
    pub fn from_lsp_settings(value: &serde_json::Value) -> Result<Self, ConfigLoadError> {
        let mut patch = FeToolingConfigPatch::default();
        let Some(object) = value.as_object() else {
            return Ok(patch);
        };

        for (key, value) in object {
            match key.as_str() {
                "fe.lsp.enabled" => {
                    patch.lsp_mut().enabled = Some(json_bool(key, value)?);
                }
                "fe.lsp.trace.mode" => {
                    patch.lsp_mut().trace_mode = Some(json_string_enum(key, value)?);
                }
                "fe.lsp.trace.maxAnalysisMs" => {
                    patch.lsp_mut().max_analysis_ms = Some(json_u64(key, value)?);
                }
                "fe.lsp.trace.maxFacts" => {
                    patch.lsp_mut().trace_mut().max_trace_facts = Some(json_usize(key, value)?);
                }
                "fe.lsp.trace.maxShapeNodes" => {
                    patch.lsp_mut().trace_mut().max_shape_nodes = Some(json_usize(key, value)?);
                }
                "fe.lsp.trace.maxQueryMs" => {
                    patch.lsp_mut().trace_mut().max_query_ms = Some(json_u64(key, value)?);
                }
                "fe.lsp.trace.debounceMs" => {
                    patch.lsp_mut().trace_mut().debounce_ms = Some(json_u64(key, value)?);
                }
                "fe.lsp.inlayHints.types" => {
                    patch.lsp_mut().inlay_hints_mut().types = Some(json_bool(key, value)?);
                }
                "fe.lsp.inlayHints.gas" => {
                    patch.lsp_mut().inlay_hints_mut().gas = Some(json_string_enum(key, value)?);
                }
                "fe.lsp.inlayHints.loopCost" => {
                    patch.lsp_mut().inlay_hints_mut().loop_cost =
                        Some(json_string_enum(key, value)?);
                }
                "fe.lsp.hover.originChain" => {
                    patch.lsp_mut().hover_mut().origin_chain = Some(json_bool(key, value)?);
                }
                "fe.lsp.hover.storageHistory" => {
                    patch.lsp_mut().hover_mut().storage_history = Some(json_bool(key, value)?);
                }
                "fe.lsp.live.browserGraphs" => {
                    patch.lsp_mut().live_mut().browser_graphs = Some(json_bool(key, value)?);
                }
                _ => {}
            };
        }
        Ok(patch)
    }

    fn lsp_mut(&mut self) -> &mut LspToolingConfigPatch {
        self.lsp.get_or_insert_with(LspToolingConfigPatch::default)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LspToolingConfigPatch {
    pub enabled: Option<bool>,
    pub trace_mode: Option<TraceMode>,
    pub max_analysis_ms: Option<u64>,
    pub max_hints_per_file: Option<usize>,
    pub inlay_hints: Option<InlayHintsConfigPatch>,
    pub hover: Option<HoverConfigPatch>,
    pub code_lens: Option<CodeLensConfigPatch>,
    pub trace: Option<TraceConfigPatch>,
    pub gas: Option<GasConfigPatch>,
    pub live: Option<LiveConfigPatch>,
}

impl LspToolingConfigPatch {
    fn inlay_hints_mut(&mut self) -> &mut InlayHintsConfigPatch {
        self.inlay_hints
            .get_or_insert_with(InlayHintsConfigPatch::default)
    }

    fn hover_mut(&mut self) -> &mut HoverConfigPatch {
        self.hover.get_or_insert_with(HoverConfigPatch::default)
    }

    fn trace_mut(&mut self) -> &mut TraceConfigPatch {
        self.trace.get_or_insert_with(TraceConfigPatch::default)
    }

    fn live_mut(&mut self) -> &mut LiveConfigPatch {
        self.live.get_or_insert_with(LiveConfigPatch::default)
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct InlayHintsConfigPatch {
    pub types: Option<bool>,
    pub gas: Option<HintMode>,
    pub loop_cost: Option<LoopHintMode>,
    pub storage: Option<StorageHintMode>,
    pub ir_phase: Option<IrPhaseHint>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct HoverConfigPatch {
    pub origin_chain: Option<bool>,
    pub storage_history: Option<bool>,
    pub gas_breakdown: Option<bool>,
    pub max_origin_depth: Option<usize>,
    pub show_confidence: Option<bool>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CodeLensConfigPatch {
    pub view_ir: Option<bool>,
    pub trace_loop: Option<bool>,
    pub gas_breakdown: Option<bool>,
    pub open_graph: Option<bool>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct TraceConfigPatch {
    pub emit_jsonl: Option<bool>,
    pub out_dir: Option<String>,
    pub validate: Option<bool>,
    pub level: Option<TraceLevel>,
    pub max_trace_facts: Option<usize>,
    pub max_shape_nodes: Option<usize>,
    pub max_query_ms: Option<u64>,
    pub debounce_ms: Option<u64>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GasConfigPatch {
    pub enabled: Option<bool>,
    pub mode: Option<GasMode>,
    pub schedule: Option<String>,
    pub show_uncertain: Option<bool>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LiveConfigPatch {
    pub write_server_info: Option<bool>,
    pub http: Option<bool>,
    pub browser_graphs: Option<bool>,
    pub bind: Option<String>,
    pub require_token: Option<bool>,
}

#[derive(Debug)]
pub enum ConfigLoadError {
    Io {
        path: String,
        source: std::io::Error,
    },
    Toml {
        path: String,
        source: toml::de::Error,
    },
    JsonSetting {
        key: String,
        message: String,
    },
}

impl fmt::Display for ConfigLoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io { path, source } => {
                write!(f, "failed to read tooling config {path}: {source}")
            }
            Self::Toml { path, source } => {
                write!(f, "failed to parse tooling config {path}: {source}")
            }
            Self::JsonSetting { key, message } => write!(f, "invalid LSP setting {key}: {message}"),
        }
    }
}

impl std::error::Error for ConfigLoadError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Io { source, .. } => Some(source),
            Self::Toml { source, .. } => Some(source),
            Self::JsonSetting { .. } => None,
        }
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ProjectConfigPatch {
    tooling: Option<ToolingPatch>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ToolingPatch {
    lsp: Option<LspToolingConfigPatch>,
}

fn project_patch(root: &Path) -> Result<FeToolingConfigPatch, ConfigLoadError> {
    let path = root.join("fe.toml");
    if !path.exists() {
        return Ok(FeToolingConfigPatch::default());
    }
    let text = std::fs::read_to_string(&path).map_err(|source| ConfigLoadError::Io {
        path: path.display().to_string(),
        source,
    })?;
    let value = text
        .parse::<toml::Value>()
        .map_err(|source| ConfigLoadError::Toml {
            path: path.display().to_string(),
            source,
        })?;
    let Some(tooling) = value.get("tooling").cloned() else {
        return Ok(FeToolingConfigPatch::default());
    };
    let project = toml::Value::Table([("tooling".to_string(), tooling)].into_iter().collect())
        .try_into::<ProjectConfigPatch>()
        .map_err(|source| ConfigLoadError::Toml {
            path: path.display().to_string(),
            source,
        })?;
    Ok(FeToolingConfigPatch {
        lsp: project.tooling.and_then(|tooling| tooling.lsp),
    })
}

fn assign<T>(target: &mut T, value: Option<T>) {
    if let Some(value) = value {
        *target = value;
    }
}

fn json_bool(key: &str, value: &serde_json::Value) -> Result<bool, ConfigLoadError> {
    value.as_bool().ok_or_else(|| ConfigLoadError::JsonSetting {
        key: key.to_string(),
        message: "expected boolean".to_string(),
    })
}

fn json_u64(key: &str, value: &serde_json::Value) -> Result<u64, ConfigLoadError> {
    value.as_u64().ok_or_else(|| ConfigLoadError::JsonSetting {
        key: key.to_string(),
        message: "expected non-negative integer".to_string(),
    })
}

fn json_usize(key: &str, value: &serde_json::Value) -> Result<usize, ConfigLoadError> {
    json_u64(key, value)?
        .try_into()
        .map_err(|_| ConfigLoadError::JsonSetting {
            key: key.to_string(),
            message: "integer is too large for this platform".to_string(),
        })
}

fn json_string_enum<T>(key: &str, value: &serde_json::Value) -> Result<T, ConfigLoadError>
where
    T: for<'de> Deserialize<'de>,
{
    let Some(value) = value.as_str() else {
        return Err(ConfigLoadError::JsonSetting {
            key: key.to_string(),
            message: "expected string".to_string(),
        });
    };
    serde_json::from_value(serde_json::Value::String(value.to_string())).map_err(|err| {
        ConfigLoadError::JsonSetting {
            key: key.to_string(),
            message: err.to_string(),
        }
    })
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn defaults_keep_introspection_low_noise() {
        let config = FeToolingConfig::default();
        assert!(config.lsp.inlay_hints.types);
        assert_eq!(config.lsp.inlay_hints.gas, HintMode::Off);
        assert_eq!(config.lsp.inlay_hints.loop_cost, LoopHintMode::Off);
        assert!(!config.lsp.hover.storage_history);
        assert!(!config.lsp.hover.gas_breakdown);
        assert!(!config.lsp.code_lens.trace_loop);
        assert!(!config.lsp.code_lens.gas_breakdown);
        assert_eq!(config.lsp.trace.max_trace_facts, 100_000);
        assert_eq!(config.lsp.trace.max_shape_nodes, 50_000);
        assert_eq!(config.lsp.trace.max_query_ms, 1_000);
        assert_eq!(config.lsp.trace.debounce_ms, 75);
        assert!(config.lsp.live.write_server_info);
    }

    #[test]
    fn fe_toml_overrides_defaults_without_parsing_full_project_config() {
        let dir = tempfile_dir();
        fs::write(
            dir.join("fe.toml"),
            r#"
name = "demo"

[tooling.lsp]
max_analysis_ms = 25

[tooling.lsp.inlay_hints]
types = false
gas = "summary"
storage = "hover-only"

[tooling.lsp.trace]
emit_jsonl = true
out_dir = "target/custom-traces"
"#,
        )
        .unwrap();

        let config = FeToolingConfig::load_from_workspace(&dir).unwrap();
        assert_eq!(config.lsp.max_analysis_ms, 25);
        assert!(!config.lsp.inlay_hints.types);
        assert_eq!(config.lsp.inlay_hints.gas, HintMode::Summary);
        assert_eq!(config.lsp.inlay_hints.storage, StorageHintMode::HoverOnly);
        assert!(config.lsp.trace.emit_jsonl);
        assert_eq!(config.lsp.trace.out_dir, "target/custom-traces");
    }

    #[test]
    fn lsp_settings_override_project_config() {
        let dir = tempfile_dir();
        fs::write(
            dir.join("fe.toml"),
            r#"
[tooling.lsp]
max_analysis_ms = 25

[tooling.lsp.inlay_hints]
types = false
"#,
        )
        .unwrap();
        let settings = serde_json::json!({
            "fe.lsp.trace.maxAnalysisMs": 100,
            "fe.lsp.trace.maxFacts": 10,
            "fe.lsp.trace.maxShapeNodes": 11,
            "fe.lsp.trace.maxQueryMs": 12,
            "fe.lsp.trace.debounceMs": 13,
            "fe.lsp.inlayHints.types": true,
            "fe.lsp.inlayHints.gas": "hotspots",
            "fe.lsp.live.browserGraphs": true,
        });

        let config = FeToolingConfig::load_from_sources(
            Some(&dir),
            Some(&settings),
            FeToolingConfigPatch::default(),
        )
        .unwrap();

        assert_eq!(config.lsp.max_analysis_ms, 100);
        assert_eq!(config.lsp.trace.max_trace_facts, 10);
        assert_eq!(config.lsp.trace.max_shape_nodes, 11);
        assert_eq!(config.lsp.trace.max_query_ms, 12);
        assert_eq!(config.lsp.trace.debounce_ms, 13);
        assert!(config.lsp.inlay_hints.types);
        assert_eq!(config.lsp.inlay_hints.gas, HintMode::Hotspots);
        assert!(config.lsp.live.browser_graphs);
    }

    #[test]
    fn cli_patch_wins_last() {
        let settings = serde_json::json!({
            "fe.lsp.inlayHints.gas": "summary",
        });
        let cli = FeToolingConfigPatch {
            lsp: Some(LspToolingConfigPatch {
                inlay_hints: Some(InlayHintsConfigPatch {
                    gas: Some(HintMode::All),
                    ..Default::default()
                }),
                ..Default::default()
            }),
        };

        let config = FeToolingConfig::load_from_sources(None, Some(&settings), cli).unwrap();
        assert_eq!(config.lsp.inlay_hints.gas, HintMode::All);
    }

    #[test]
    fn stable_hash_changes_with_config() {
        let mut config = FeToolingConfig::default();
        let original = config.stable_hash();
        assert_content_digest(&original);
        config.lsp.max_hints_per_file += 1;
        let changed = config.stable_hash();
        assert_content_digest(&changed);
        assert_ne!(original, changed);
    }

    fn assert_content_digest(value: &str) {
        let digest = value.strip_prefix("blake3:").unwrap_or(value);
        assert_eq!(digest.len(), 64);
        assert!(digest.chars().all(|ch| ch.is_ascii_hexdigit()));
        assert!(!value.starts_with("fnv64:"));
    }

    fn tempfile_dir() -> std::path::PathBuf {
        let mut path = std::env::temp_dir();
        path.push(format!(
            "fe-introspection-config-test-{}-{}",
            std::process::id(),
            blake3::hash(
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_nanos()
                    .to_string()
                    .as_bytes()
            )
            .to_hex()
        ));
        fs::create_dir_all(&path).unwrap();
        path
    }
}
