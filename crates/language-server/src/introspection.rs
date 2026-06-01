use common::InputDb;
use driver::DriverDataBase;
use hir::lower::map_file_to_mod;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::BufReader;
use std::str::FromStr;
use std::time::{Duration, Instant};
use trace_facts::{TraceBundle, TraceFact, TraceMetadata, TraceSnapshot};
use trace_query::{
    TraceIntrospectionService, TraceQueryHttpResponse, TraceQueryRequest,
    TraceWorkbenchProjectionRequest, run_trace_query, trace_workbench_manifest,
    trace_workbench_report_projection, trace_workbench_summary_chunk,
};
use url::Url;

use crate::backend::{Backend, TraceViewerRevisionRecord};

const DEFAULT_TRACE_TARGET: &str = "evm";
const DEFAULT_TRACE_VIEW: &str = "source-postopt-bytecode";

#[derive(Clone, Debug)]
pub(crate) struct TraceBackendQueryRequest {
    pub uri: String,
    pub config_hash: Option<String>,
    pub target: Option<String>,
    pub opt_level: Option<String>,
    pub view: Option<String>,
    pub query: TraceQueryRequest,
}

#[derive(Clone, Debug)]
pub(crate) struct TraceWorkbenchSessionRequest {
    pub session_id: String,
}

#[derive(Clone, Debug)]
pub(crate) struct TraceWorkbenchChunkRequest {
    pub session_id: String,
    pub digest: String,
}

#[derive(Clone, Debug)]
pub(crate) struct TraceWorkbenchChunksRequest {
    pub session_id: String,
    pub digests: Vec<String>,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct TraceServiceOptions {
    pub opt_level: codegen::OptLevel,
}

impl Default for TraceServiceOptions {
    fn default() -> Self {
        Self {
            opt_level: codegen::OptLevel::O1,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct TraceWorkbenchBootstrapResponse {
    pub session: crate::backend::TraceViewerSession,
    pub revision: TraceWorkbenchRevision,
    pub capabilities: TraceWorkbenchCapabilities,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct TraceWorkbenchRevision {
    pub id: u64,
    pub document_version: Option<i32>,
    pub status: String,
    pub config_hash: String,
    pub model_digest: Option<String>,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct TraceWorkbenchCapabilities {
    pub events: &'static str,
    pub model_deltas: bool,
    pub chunks: bool,
    pub selection_sync: bool,
    pub revision_history: bool,
}

#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct TraceWorkbenchRevisionHistoryResponse {
    pub session_id: String,
    pub revisions: Vec<TraceViewerRevisionRecord>,
}

pub(crate) async fn handle_trace_workbench_bootstrap(
    backend: &mut Backend,
    request: TraceWorkbenchSessionRequest,
) -> Result<TraceWorkbenchBootstrapResponse, async_lsp::ResponseError> {
    let mut session = backend
        .trace_viewer_session(&request.session_id)
        .ok_or_else(|| {
            internal_error(format!(
                "unknown trace workbench session {}",
                request.session_id
            ))
        })?;
    let uri = Url::parse(&session.uri)
        .map_err(|err| internal_error(format!("invalid session URI: {err}")))?;
    let document_version = backend.document_version(&uri);
    let compiler_config_hash = backend.tooling_config().stable_hash();
    let target_config_hash =
        trace_workbench_target_config_hash(&session.target, &session.opt_level, &session.view);
    let config_hash =
        trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);
    let latest_revision = backend
        .trace_viewer_revisions(&request.session_id)
        .and_then(|revisions| revisions.last().cloned());
    let (revision_status, latest_model_digest) =
        trace_workbench_revision_status(latest_revision.as_ref(), document_version, &config_hash);
    session.document_version = document_version;
    session.config_hash = config_hash.clone();
    Ok(TraceWorkbenchBootstrapResponse {
        revision: TraceWorkbenchRevision {
            id: document_version.unwrap_or_default().max(0) as u64,
            document_version,
            status: revision_status,
            config_hash,
            model_digest: latest_model_digest,
        },
        session,
        capabilities: TraceWorkbenchCapabilities {
            events: "sse",
            model_deltas: false,
            chunks: true,
            selection_sync: true,
            revision_history: true,
        },
    })
}

fn trace_workbench_revision_status(
    latest_revision: Option<&TraceViewerRevisionRecord>,
    document_version: Option<i32>,
    config_hash: &str,
) -> (String, Option<String>) {
    let Some(latest_revision) = latest_revision else {
        return ("pending".to_string(), None);
    };
    if latest_revision.document_version == document_version
        && latest_revision.config_hash == config_hash
        && latest_revision.status == "ready"
    {
        return (
            "ready".to_string(),
            Some(latest_revision.model_digest.clone()),
        );
    }
    (
        "stale_but_usable".to_string(),
        Some(latest_revision.model_digest.clone()),
    )
}

pub(crate) async fn handle_trace_workbench_model(
    backend: &mut Backend,
    request: TraceWorkbenchSessionRequest,
) -> Result<serde_json::Value, async_lsp::ResponseError> {
    let started = Instant::now();
    let session = backend
        .trace_viewer_session(&request.session_id)
        .ok_or_else(|| {
            internal_error(format!(
                "unknown trace workbench session {}",
                request.session_id
            ))
        })?;
    let uri = Url::parse(&session.uri)
        .map_err(|err| internal_error(format!("invalid session URI: {err}")))?;
    ensure_workspace_file(backend, &uri).map_err(internal_error)?;
    let compiler_config_hash = backend.tooling_config().stable_hash();
    let target_config_hash =
        trace_workbench_target_config_hash(&session.target, &session.opt_level, &session.view);
    let service_config_hash =
        trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);
    let opt_level = trace_workbench_parse_opt_level(&session.opt_level).map_err(internal_error)?;
    let document_version = backend.document_version(&uri);
    if let Some(cached_model) = backend.cached_trace_workbench_model(
        &request.session_id,
        document_version,
        &service_config_hash,
    ) {
        return Ok(cached_model);
    }
    let source_text = backend
        .db
        .workspace()
        .get(&backend.db, &uri)
        .map(|file| file.text(&backend.db).to_string());
    let source_hash = source_text.as_deref().map(trace_workbench_digest_text);

    let service = if let Some(version) = document_version
        && let Some(service) = backend.cached_trace_service(&uri, version, &service_config_hash)
    {
        service
    } else {
        let config = backend.tooling_config().clone();
        let worker_uri = uri.clone();
        let worker = backend.spawn_on_workers(move |db| {
            service_for_file_with_options(
                db,
                &worker_uri,
                config,
                TraceServiceOptions { opt_level },
            )?
            .ok_or_else(|| format!("no Fe source file is loaded for URI {worker_uri}"))
        });
        let result = tokio::time::timeout(Duration::from_secs(30), worker)
            .await
            .map_err(|_| internal_error("live trace workbench model exceeded 30s budget"))?
            .map_err(internal_error)?;
        let service = result.map_err(internal_error)?;
        if let Some(version) = document_version {
            backend.cache_trace_service(
                uri.clone(),
                version,
                service_config_hash.clone(),
                service.clone(),
            );
        }
        service
    };
    let related_source_texts =
        trace_workbench_related_source_texts(backend, service.snapshot(), &uri);
    let trace_snapshot_digest = service.snapshot().trace_hash().to_string();
    let projection = trace_workbench_report_projection(
        &service,
        service.snapshot(),
        TraceWorkbenchProjectionRequest {
            input_path: session.uri.clone(),
            target: session.target.clone(),
            opt_level: session.opt_level.clone(),
            view: session.view.clone(),
            include_legacy_closure_debug: false,
            source_text,
            related_source_texts,
            document_version,
            query_duration_ms: elapsed_ms(started),
            compiler_commit: option_env!("FE_GIT_COMMIT")
                .unwrap_or("unknown")
                .to_string(),
            data_source: "lsp-live".to_string(),
        },
    );
    let manifest = trace_workbench_manifest(&projection);
    let model_digest = manifest.root_digest.clone();
    let manifest_value = serde_json::to_value(&manifest)
        .map_err(|err| internal_error(format!("failed to serialize trace manifest: {err}")))?;
    let chunks = trace_workbench_chunk_payloads(&projection, &manifest);
    backend.record_trace_viewer_revision(
        &request.session_id,
        TraceViewerRevisionRecord {
            revision: manifest.revision,
            previous_revision: None,
            document_version,
            status: "ready".to_string(),
            config_hash: service_config_hash.clone(),
            compiler_config_hash,
            target_config_hash,
            target: session.target,
            opt_level: session.opt_level,
            view: session.view,
            source_hash,
            trace_snapshot_digest,
            model_digest: model_digest.clone(),
            summary_digest: manifest.summary_digest,
            source_digest: manifest.source_digest,
            indexes_digest: manifest.indexes_digest,
            rail_components_digest: manifest.rail_components_digest,
            pane_digests: manifest.panes,
            report_digests: manifest.reports,
        },
    );
    backend.cache_trace_workbench_model(
        &request.session_id,
        document_version,
        service_config_hash,
        projection.clone(),
        manifest_value,
        chunks,
    );
    Ok(projection)
}

fn trace_workbench_target_config_hash(target: &str, opt_level: &str, view: &str) -> String {
    trace_workbench_digest_text(&format!(
        "target={target}\nopt_level={opt_level}\nview={view}\n"
    ))
}

fn trace_workbench_opt_level_label(opt_level: codegen::OptLevel) -> String {
    format!("O{opt_level}")
}

fn trace_workbench_service_config_hash(
    compiler_config_hash: &str,
    target_config_hash: &str,
) -> String {
    trace_workbench_digest_text(&format!(
        "compiler_config={compiler_config_hash}\ntarget_config={target_config_hash}\n"
    ))
}

fn trace_workbench_parse_opt_level(input: &str) -> Result<codegen::OptLevel, String> {
    let normalized = input
        .trim()
        .strip_prefix('O')
        .or_else(|| input.trim().strip_prefix('o'))
        .unwrap_or_else(|| input.trim());
    codegen::OptLevel::from_str(normalized)
}

fn trace_workbench_digest_text(text: &str) -> String {
    format!("blake3:{}", blake3::hash(text.as_bytes()).to_hex())
}

fn trace_workbench_source_file_display_name(uri: &Url) -> String {
    uri.path_segments()
        .and_then(|mut segments| segments.next_back())
        .filter(|segment| !segment.is_empty())
        .map(ToString::to_string)
        .unwrap_or_else(|| uri.to_string())
}

fn trace_workbench_related_source_texts(
    backend: &Backend,
    snapshot: &TraceSnapshot,
    entry_uri: &Url,
) -> BTreeMap<String, String> {
    let mut sources = BTreeMap::new();
    for fact in snapshot.facts() {
        let TraceFact::SourceFile(source_file) = fact else {
            continue;
        };
        let Ok(source_uri) = Url::parse(&source_file.uri) else {
            continue;
        };
        if &source_uri == entry_uri {
            continue;
        }
        let Some(text) = trace_workbench_workspace_source_text(backend, &source_uri)
            .or_else(|| trace_workbench_file_source_text(&source_uri))
        else {
            continue;
        };
        sources.insert(source_file.file_key.canonical_storage_key(), text.clone());
        sources.insert(source_file.uri.clone(), text);
    }
    sources
}

fn trace_workbench_workspace_source_text(backend: &Backend, uri: &Url) -> Option<String> {
    let internal_uri = backend.map_client_uri_to_internal(uri.clone());
    backend
        .db
        .workspace()
        .get(&backend.db, &internal_uri)
        .map(|file| file.text(&backend.db).to_string())
}

fn trace_workbench_file_source_text(uri: &Url) -> Option<String> {
    const MAX_RELATED_SOURCE_TEXT_BYTES: u64 = 256 * 1024;
    if uri.scheme() != "file" {
        return None;
    }
    let path = uri.to_file_path().ok()?;
    if fs::metadata(&path).ok()?.len() > MAX_RELATED_SOURCE_TEXT_BYTES {
        return None;
    }
    fs::read_to_string(path).ok()
}

pub(crate) async fn handle_trace_workbench_manifest(
    backend: &mut Backend,
    request: TraceWorkbenchSessionRequest,
) -> Result<serde_json::Value, async_lsp::ResponseError> {
    let (document_version, config_hash) =
        trace_workbench_session_cache_key(backend, &request.session_id)?;
    if let Some(manifest) =
        backend.cached_trace_workbench_manifest(&request.session_id, document_version, &config_hash)
    {
        return Ok(manifest);
    }
    let model = handle_trace_workbench_model(backend, request).await?;
    serde_json::to_value(trace_workbench_manifest(&model))
        .map_err(|err| internal_error(format!("failed to serialize trace manifest: {err}")))
}

pub(crate) async fn handle_trace_workbench_chunk(
    backend: &mut Backend,
    request: TraceWorkbenchChunkRequest,
) -> Result<Option<serde_json::Value>, async_lsp::ResponseError> {
    let (document_version, config_hash) =
        trace_workbench_session_cache_key(backend, &request.session_id)?;
    Ok(backend.cached_trace_workbench_chunk(
        &request.session_id,
        document_version,
        &config_hash,
        &request.digest,
    ))
}

pub(crate) async fn handle_trace_workbench_chunks(
    backend: &mut Backend,
    request: TraceWorkbenchChunksRequest,
) -> Result<serde_json::Value, async_lsp::ResponseError> {
    let (document_version, config_hash) =
        trace_workbench_session_cache_key(backend, &request.session_id)?;
    Ok(backend
        .cached_trace_workbench_chunks_response(
            &request.session_id,
            document_version,
            &config_hash,
            request.digests.clone(),
        )
        .unwrap_or_else(|| {
            serde_json::json!({
                "chunks": [],
                "missing": request.digests,
            })
        }))
}

pub(crate) async fn handle_trace_workbench_revisions(
    backend: &mut Backend,
    request: TraceWorkbenchSessionRequest,
) -> Result<TraceWorkbenchRevisionHistoryResponse, async_lsp::ResponseError> {
    let revisions = backend
        .trace_viewer_revisions(&request.session_id)
        .ok_or_else(|| {
            internal_error(format!(
                "unknown trace workbench session {}",
                request.session_id
            ))
        })?;
    Ok(TraceWorkbenchRevisionHistoryResponse {
        session_id: request.session_id,
        revisions,
    })
}

fn trace_workbench_session_cache_key(
    backend: &Backend,
    session_id: &str,
) -> Result<(Option<i32>, String), async_lsp::ResponseError> {
    let session = backend
        .trace_viewer_session(session_id)
        .ok_or_else(|| internal_error(format!("unknown trace workbench session {session_id}")))?;
    let uri = Url::parse(&session.uri)
        .map_err(|err| internal_error(format!("invalid session URI: {err}")))?;
    let compiler_config_hash = backend.tooling_config().stable_hash();
    let target_config_hash =
        trace_workbench_target_config_hash(&session.target, &session.opt_level, &session.view);
    Ok((
        backend.document_version(&uri),
        trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash),
    ))
}

fn trace_workbench_chunk_payloads(
    model: &serde_json::Value,
    manifest: &trace_query::TraceViewManifest,
) -> BTreeMap<String, serde_json::Value> {
    let mut chunks = BTreeMap::new();
    let digests = std::iter::once(manifest.root_digest.clone())
        .chain(std::iter::once(manifest.summary_digest.clone()))
        .chain(std::iter::once(manifest.metadata_digest.clone()))
        .chain(std::iter::once(manifest.source_digest.clone()))
        .chain(std::iter::once(manifest.indexes_digest.clone()))
        .chain(std::iter::once(manifest.rail_components_digest.clone()))
        .chain(manifest.panes.values().cloned())
        .chain(manifest.reports.values().cloned());
    for digest in digests {
        if let Some(payload) = trace_workbench_chunk_payload(model, &digest) {
            chunks.insert(digest, payload);
        }
    }
    chunks
}

pub(crate) fn trace_workbench_chunk_payload(
    model: &serde_json::Value,
    digest: &str,
) -> Option<serde_json::Value> {
    let manifest = trace_workbench_manifest(model);
    if manifest.root_digest == digest {
        return Some(serde_json::json!({
            "kind": "model",
            "digest": digest,
            "value": model,
        }));
    }
    if manifest.summary_digest == digest {
        return Some(serde_json::json!({
            "kind": "summary",
            "digest": digest,
            "value": trace_workbench_summary_chunk(model),
        }));
    }
    if manifest.metadata_digest == digest {
        return Some(serde_json::json!({
            "kind": "metadata",
            "digest": digest,
            "value": model.get("metadata").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    if manifest.source_digest == digest {
        return Some(serde_json::json!({
            "kind": "source",
            "digest": digest,
            "value": model.get("source").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    if manifest.indexes_digest == digest {
        return Some(serde_json::json!({
            "kind": "indexes",
            "digest": digest,
            "value": model.get("indexes").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    if manifest.rail_components_digest == digest {
        return Some(serde_json::json!({
            "kind": "rail_components",
            "digest": digest,
            "value": model.get("rail_components").cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    for (pane_id, pane_digest) in &manifest.panes {
        if pane_digest != digest {
            continue;
        }
        let pane = model
            .get("panels")
            .and_then(serde_json::Value::as_array)
            .into_iter()
            .flatten()
            .find(|pane| {
                pane.get("id").and_then(serde_json::Value::as_str) == Some(pane_id.as_str())
            })
            .cloned()
            .unwrap_or(serde_json::Value::Null);
        return Some(serde_json::json!({
            "kind": "pane",
            "id": pane_id,
            "digest": digest,
            "value": pane,
        }));
    }
    for (report_id, report_digest) in &manifest.reports {
        if report_digest != digest {
            continue;
        }
        let key = match report_id.as_str() {
            "attribution" => "attribution_audit",
            "static_analysis" => "static_analysis",
            "closure_audit" => "audit",
            "duplicate_shapes" => "duplicate_shapes",
            _ => report_id.as_str(),
        };
        return Some(serde_json::json!({
            "kind": "report",
            "id": report_id,
            "digest": digest,
            "value": model.get(key).cloned().unwrap_or(serde_json::Value::Null),
        }));
    }
    None
}

pub(crate) async fn handle_trace_query(
    backend: &mut Backend,
    request: TraceBackendQueryRequest,
) -> Result<TraceQueryHttpResponse, async_lsp::ResponseError> {
    let started = Instant::now();
    let compiler_config_hash = backend.tooling_config().stable_hash();
    let opt_level = request
        .opt_level
        .as_deref()
        .map(trace_workbench_parse_opt_level)
        .transpose()
        .map_err(internal_error)?
        .unwrap_or_else(|| TraceServiceOptions::default().opt_level);
    let opt_level_text = trace_workbench_opt_level_label(opt_level);
    let target_config_hash = trace_workbench_target_config_hash(
        request.target.as_deref().unwrap_or(DEFAULT_TRACE_TARGET),
        &opt_level_text,
        request.view.as_deref().unwrap_or(DEFAULT_TRACE_VIEW),
    );
    let current_config_hash =
        trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);
    if request
        .config_hash
        .as_deref()
        .is_some_and(|requested| requested != current_config_hash)
    {
        return Ok(TraceQueryHttpResponse::Error {
            reason: format!(
                "live trace config hash mismatch: client has {}, server has {}",
                request.config_hash.unwrap_or_default(),
                current_config_hash
            ),
            cache_hit: false,
            query_duration_ms: elapsed_ms(started),
        });
    }

    let client_uri = parse_trace_uri(&request.uri).map_err(internal_error)?;
    ensure_workspace_file(backend, &client_uri).map_err(internal_error)?;
    let internal_uri = backend.map_client_uri_to_internal(client_uri);
    let query = request.query;
    let config = backend.tooling_config().clone();
    let trace_config = config.lsp.trace.clone();
    let document_version = backend.document_version(&internal_uri);

    if trace_config.debounce_ms > 0 {
        tokio::time::sleep(Duration::from_millis(trace_config.debounce_ms)).await;
    }

    if let Some(version) = document_version
        && let Some(service) =
            backend.cached_trace_service(&internal_uri, version, &current_config_hash)
    {
        return Ok(match run_trace_query(&service, query) {
            Ok(report) => TraceQueryHttpResponse::Ok {
                report,
                cache_hit: true,
                query_duration_ms: elapsed_ms(started),
            },
            Err(err) => TraceQueryHttpResponse::Error {
                reason: err.to_string(),
                cache_hit: true,
                query_duration_ms: elapsed_ms(started),
            },
        });
    }

    let internal_uri_for_worker = internal_uri.clone();
    let worker = backend.spawn_on_workers(move |db| {
        let service = service_for_file_with_options(
            db,
            &internal_uri_for_worker,
            config,
            TraceServiceOptions { opt_level },
        )?
        .ok_or_else(|| format!("no Fe source file is loaded for URI {internal_uri_for_worker}"))?;
        Ok::<_, String>(service)
    });
    let result = tokio::time::timeout(
        Duration::from_millis(trace_config.max_query_ms.max(1)),
        worker,
    )
    .await
    .map_err(|_| {
        internal_error(format!(
            "live trace query exceeded {}ms budget",
            trace_config.max_query_ms.max(1)
        ))
    })?
    .map_err(internal_error)?;

    let service = match result {
        Ok(service) => service,
        Err(reason) => {
            return Ok(TraceQueryHttpResponse::Error {
                reason,
                cache_hit: false,
                query_duration_ms: elapsed_ms(started),
            });
        }
    };
    if let Some(version) = document_version {
        backend.cache_trace_service(
            internal_uri.clone(),
            version,
            current_config_hash.clone(),
            service.clone(),
        );
    }

    Ok(match run_trace_query(&service, query) {
        Ok(report) => TraceQueryHttpResponse::Ok {
            report,
            cache_hit: false,
            query_duration_ms: elapsed_ms(started),
        },
        Err(err) => TraceQueryHttpResponse::Error {
            reason: err.to_string(),
            cache_hit: false,
            query_duration_ms: elapsed_ms(started),
        },
    })
}

fn parse_trace_uri(input: &str) -> Result<Url, String> {
    if let Ok(url) = Url::parse(input) {
        return Ok(url);
    }
    Url::from_file_path(input).map_err(|()| format!("trace query URI is not valid: {input}"))
}

fn ensure_workspace_file(backend: &Backend, uri: &Url) -> Result<(), String> {
    if uri.scheme() != "file" {
        return Err(format!(
            "trace queries only support file:// URIs, got {uri}"
        ));
    }
    let path = uri
        .to_file_path()
        .map_err(|()| format!("trace query URI is not a local file path: {uri}"))?;
    if path.extension().is_none_or(|ext| ext != "fe") {
        return Err(format!("trace query URI must point to a .fe file: {uri}"));
    }
    if let Some(root) = backend.lsp_workspace_root.as_ref()
        && !path.starts_with(root)
    {
        return Err(format!(
            "trace query URI is outside the LSP workspace root: {uri}"
        ));
    }
    Ok(())
}

fn internal_error(message: impl ToString) -> async_lsp::ResponseError {
    async_lsp::ResponseError::new(async_lsp::ErrorCode::INTERNAL_ERROR, message.to_string())
}

pub(crate) fn service_for_file(
    db: &DriverDataBase,
    uri: &Url,
    config: introspection_config::FeToolingConfig,
) -> Result<Option<TraceIntrospectionService>, String> {
    service_for_file_with_options(db, uri, config, TraceServiceOptions::default())
}

pub(crate) fn service_for_file_with_options(
    db: &DriverDataBase,
    uri: &Url,
    config: introspection_config::FeToolingConfig,
    options: TraceServiceOptions,
) -> Result<Option<TraceIntrospectionService>, String> {
    if let Some(attached_trace) = config.lsp.trace.attached_trace.as_deref()
        && !attached_trace.trim().is_empty()
    {
        return attached_trace_service(attached_trace, &config).map(Some);
    }

    let Some(file) = db.workspace().get(db, uri) else {
        return Ok(None);
    };
    let source_text = file.text(db).to_string();
    let top_mod = map_file_to_mod(db, file);
    let facts = codegen::trace::emit_observable_module_trace_facts(
        db,
        top_mod,
        uri.as_str(),
        uri.to_string(),
        trace_workbench_source_file_display_name(uri),
        &source_text,
        options.opt_level,
        None,
    )
    .map_err(|err| format!("observable trace fact emission for live trace: {err}"))?;
    enforce_trace_limits(&facts, &config)?;

    let metadata = TraceMetadata::compiler_emitted(
        option_env!("FE_GIT_COMMIT").unwrap_or("unknown"),
        "evm/sonatina",
        vec!["fe-language-server".to_string(), "trace".to_string()],
        uri.to_string(),
        vec![
            "source=lsp-live".to_string(),
            format!("optimize=O{}", options.opt_level),
        ],
    );
    let snapshot = TraceSnapshot::new(TraceBundle::new(metadata, facts))
        .map_err(|err| format!("live trace validation failed: {err}"))?;
    Ok(Some(TraceIntrospectionService::with_config(
        snapshot, config,
    )))
}

fn attached_trace_service(
    path: &str,
    config: &introspection_config::FeToolingConfig,
) -> Result<TraceIntrospectionService, String> {
    let file =
        File::open(path).map_err(|err| format!("failed to open attached trace {path}: {err}"))?;
    let snapshot = TraceSnapshot::read_jsonl(BufReader::new(file))
        .map_err(|err| format!("failed to read attached trace {path}: {err}"))?;
    enforce_trace_limits(snapshot.facts(), config)?;
    Ok(TraceIntrospectionService::with_config(
        snapshot,
        config.clone(),
    ))
}

fn enforce_trace_limits(
    facts: &[trace_facts::TraceFact],
    config: &introspection_config::FeToolingConfig,
) -> Result<(), String> {
    let trace = &config.lsp.trace;
    if facts.len() > trace.max_trace_facts {
        return Err(format!(
            "trace fact limit exceeded: {} facts > max_trace_facts={}",
            facts.len(),
            trace.max_trace_facts
        ));
    }
    let shape_nodes = facts
        .iter()
        .filter(|fact| matches!(fact, trace_facts::TraceFact::ShapeNodeHash(_)))
        .count();
    if shape_nodes > trace.max_shape_nodes {
        return Err(format!(
            "shape node limit exceeded: {shape_nodes} nodes > max_shape_nodes={}",
            trace.max_shape_nodes
        ));
    }
    Ok(())
}

fn elapsed_ms(started: Instant) -> u64 {
    started.elapsed().as_millis().try_into().unwrap_or(u64::MAX)
}

#[cfg(test)]
mod tests {
    use async_lsp::MainLoop;
    use async_lsp::router::Router;
    use common::{InputDb, origin::OriginExportKey};
    use hir::lower::map_file_to_mod;
    use std::collections::BTreeMap;
    use trace_facts::{
        InstructionFact, JsonlTraceSink, OriginEdgeLabel, OriginNodeFact, OriginNodeKind,
        TraceBundle, TraceFact, TraceMetadata, TraceSnapshot,
    };
    use trace_query::{
        TraceIntrospectionService, TraceQueryHttpResponse, TraceQueryReport, TraceQueryRequest,
        TraceWorkbenchProjectionRequest, trace_workbench_report_projection,
    };
    use url::Url;

    use super::{
        TraceBackendQueryRequest, TraceServiceOptions, TraceWorkbenchChunkRequest,
        TraceWorkbenchChunksRequest, TraceWorkbenchSessionRequest, attached_trace_service,
        handle_trace_query, handle_trace_workbench_bootstrap, handle_trace_workbench_chunk,
        handle_trace_workbench_chunks, handle_trace_workbench_model, service_for_file_with_options,
        trace_workbench_parse_opt_level, trace_workbench_service_config_hash,
        trace_workbench_source_file_display_name, trace_workbench_target_config_hash,
    };
    use crate::backend::{Backend, TraceViewerRevisionRecord};

    fn test_backend() -> Backend {
        test_backend_with_config(introspection_config::FeToolingConfig::default())
    }

    fn test_backend_with_config(config: introspection_config::FeToolingConfig) -> Backend {
        let (_main_loop, client_socket) = MainLoop::new_server(|_client| Router::<()>::new(()));
        Backend::new(client_socket, None, None, None, config)
    }

    fn key(kind: &str, owner: &str, local: &str) -> OriginExportKey {
        OriginExportKey::try_from_raw_parts(kind, owner, local).unwrap()
    }

    fn node(key: OriginExportKey) -> TraceFact {
        TraceFact::OriginNode(OriginNodeFact::new(
            key.clone(),
            OriginNodeKind::new(key.kind()),
        ))
    }

    #[test]
    fn live_trace_service_emits_observable_postopt_bytecode_for_requested_opt_level() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/live_trace.fe").unwrap();
        backend.db.workspace().update(
            &mut backend.db,
            uri.clone(),
            r#"
pub fn main() -> u64 {
    (10 - 3) * 2
}
"#
            .to_string(),
        );

        let service = service_for_file_with_options(
            &backend.db,
            &uri,
            backend.tooling_config().clone(),
            TraceServiceOptions {
                opt_level: codegen::OptLevel::O2,
            },
        )
        .unwrap()
        .expect("live trace service");
        let snapshot = service.snapshot();

        assert!(
            snapshot
                .metadata()
                .flags
                .iter()
                .any(|flag| flag == "optimize=O2")
        );
        assert!(snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::OriginNode(node)
                if node.key.kind() == codegen::trace::SONATINA_POSTOPT_INST_KIND
        )));
        assert!(snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::SourceFile(source) if source.uri == uri.as_str()
        )));
        assert!(
            snapshot
                .facts()
                .iter()
                .any(|fact| matches!(fact, TraceFact::SourceSpan(_)))
        );
        assert!(snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::SourceSpan(span)
                if span.origin.kind() == "code.object"
                    && span.file.kind() == "source.file"
                    && span.start_line == 1
                    && span.end_line >= 4
        )));
        assert!(snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::OriginNode(node) if node.key.kind() == "bytecode.pc"
        )));
        assert!(
            snapshot
                .facts()
                .iter()
                .any(|fact| matches!(fact, TraceFact::ShapeNodeHash(_)))
        );
        assert!(snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to.kind() == codegen::trace::EVM_VCODE_INST_KIND
                    && edge.label == OriginEdgeLabel::EmittedFrom
        )));
        assert!(snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == codegen::trace::EVM_VCODE_INST_KIND
                    && edge.to.kind() == codegen::trace::SONATINA_EVM_PREPARED_INST_KIND
                    && edge.label == OriginEdgeLabel::LoweredFrom
        )));
        assert!(!snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to.kind() == codegen::trace::SONATINA_EVM_PREPARED_INST_KIND
        )));
        assert!(!snapshot.facts().iter().any(|fact| matches!(
            fact,
            TraceFact::OriginEdge(edge)
                if edge.from.kind() == "bytecode.pc"
                    && edge.to.kind() == codegen::trace::SONATINA_POSTOPT_INST_KIND
        )));

        drop(backend);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_model_uses_live_observable_projection_for_requested_opt_level() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/live_trace_model.fe").unwrap();
        backend.db.workspace().update(
            &mut backend.db,
            uri.clone(),
            r#"
pub fn main() -> u64 {
    (10 - 3) * 2
}
"#
            .to_string(),
        );
        backend.set_document_version(uri.clone(), 42);
        let session =
            backend.create_trace_viewer_session(uri, "evm", "O2", "source-postopt-bytecode", None);

        let model = handle_trace_workbench_model(
            &mut backend,
            TraceWorkbenchSessionRequest {
                session_id: session.id.clone(),
            },
        )
        .await
        .unwrap();

        assert_eq!(model["revision"]["id"], 42);
        assert_eq!(model["parity_summary"]["opt_level"], "O2");
        assert_eq!(
            model["parity_summary"]["trace_profile"]["has_sonatina_postopt"],
            true
        );
        assert_eq!(
            model["parity_summary"]["trace_profile"]["has_evm_prepared"],
            true
        );
        assert_eq!(
            model["parity_summary"]["trace_profile"]["has_evm_vcode"],
            true
        );
        assert_eq!(
            model["parity_summary"]["trace_profile"]["has_bytecode"],
            true
        );
        assert!(
            model["parity_summary"]["origin_counts"]["sonatina.postopt.inst"]
                .as_u64()
                .is_some_and(|count| count > 0)
        );
        assert!(
            model["parity_summary"]["origin_counts"]["sonatina.evm.prepared.inst"]
                .as_u64()
                .is_some_and(|count| count > 0)
        );
        assert!(
            model["parity_summary"]["origin_counts"]["evm.vcode.inst"]
                .as_u64()
                .is_some_and(|count| count > 0)
        );
        assert!(
            model["parity_summary"]["origin_counts"]["bytecode.pc"]
                .as_u64()
                .is_some_and(|count| count > 0)
        );
        assert!(model["panels"].as_array().unwrap().iter().any(|panel| {
            panel["id"] == "evm-vcode"
                && panel["rows"]
                    .as_array()
                    .is_some_and(|rows| !rows.is_empty())
        }));
        assert!(model["notes"].as_array().unwrap().iter().any(|note| note
            == "Static and live workbench entry points use the shared trace-query projection path."));

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_live_projection_parity_summary_matches_static_builder_projection() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/live_trace_parity.fe").unwrap();
        let source = r#"
pub fn main() -> u64 {
    (10 - 3) * 2
}
"#
        .to_string();
        backend
            .db
            .workspace()
            .update(&mut backend.db, uri.clone(), source.clone());
        backend.set_document_version(uri.clone(), 17);
        let session = backend.create_trace_viewer_session(
            uri.clone(),
            "evm",
            "O2",
            "source-postopt-bytecode",
            None,
        );

        let live_model = handle_trace_workbench_model(
            &mut backend,
            TraceWorkbenchSessionRequest {
                session_id: session.id.clone(),
            },
        )
        .await
        .unwrap();
        let direct_live_service = service_for_file_with_options(
            &backend.db,
            &uri,
            backend.tooling_config().clone(),
            TraceServiceOptions {
                opt_level: codegen::OptLevel::O2,
            },
        )
        .unwrap()
        .expect("direct trace service");
        let direct_live_projection = trace_workbench_report_projection(
            &direct_live_service,
            direct_live_service.snapshot(),
            TraceWorkbenchProjectionRequest {
                input_path: uri.to_string(),
                target: "evm".to_string(),
                opt_level: "O2".to_string(),
                view: "source-postopt-bytecode".to_string(),
                include_legacy_closure_debug: false,
                source_text: Some(source.clone()),
                related_source_texts: BTreeMap::new(),
                document_version: Some(17),
                query_duration_ms: 0,
                compiler_commit: option_env!("FE_GIT_COMMIT")
                    .unwrap_or("unknown")
                    .to_string(),
                data_source: "lsp-live".to_string(),
            },
        );
        let file = backend
            .db
            .workspace()
            .get(&backend.db, &uri)
            .expect("file should be loaded");
        let top_mod = map_file_to_mod(&backend.db, file);
        let static_facts = codegen::trace::emit_observable_module_trace_facts(
            &backend.db,
            top_mod,
            uri.as_str(),
            uri.to_string(),
            trace_workbench_source_file_display_name(&uri),
            &source,
            codegen::OptLevel::O2,
            None,
        )
        .expect("static-style observable trace facts");
        let static_metadata = TraceMetadata::compiler_emitted(
            option_env!("FE_GIT_COMMIT").unwrap_or("unknown"),
            "evm/sonatina",
            vec![
                "fe-language-server".to_string(),
                "trace-static-test".to_string(),
            ],
            uri.to_string(),
            vec!["source=static-test".to_string(), "optimize=O2".to_string()],
        );
        let static_snapshot = TraceSnapshot::new(TraceBundle::new(static_metadata, static_facts))
            .expect("static-style trace snapshot");
        let static_service = TraceIntrospectionService::new(static_snapshot);
        let static_projection = trace_workbench_report_projection(
            &static_service,
            static_service.snapshot(),
            TraceWorkbenchProjectionRequest {
                input_path: uri.to_string(),
                target: "evm".to_string(),
                opt_level: "O2".to_string(),
                view: "source-postopt-bytecode".to_string(),
                include_legacy_closure_debug: false,
                source_text: Some(source),
                related_source_texts: BTreeMap::new(),
                document_version: Some(17),
                query_duration_ms: 0,
                compiler_commit: option_env!("FE_GIT_COMMIT")
                    .unwrap_or("unknown")
                    .to_string(),
                data_source: "static-test".to_string(),
            },
        );

        assert_eq!(
            live_model["parity_summary"],
            direct_live_projection["parity_summary"]
        );
        assert_eq!(
            live_model["parity_summary"], static_projection["parity_summary"],
            "LSP live and static-style observable trace projections must stay semantically equivalent for the same source/config"
        );

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_session_query_uses_session_opt_level_and_config_hash() {
        let mut config = introspection_config::FeToolingConfig::default();
        config.lsp.trace.max_query_ms = 30_000;
        let mut backend = test_backend_with_config(config);
        let uri = Url::parse("file:///workspace/src/live_trace_query.fe").unwrap();
        backend.db.workspace().update(
            &mut backend.db,
            uri.clone(),
            r#"
pub fn main() -> u64 {
    (10 - 3) * 2
}
"#
            .to_string(),
        );
        backend.set_document_version(uri.clone(), 7);
        let compiler_config_hash = backend.tooling_config().stable_hash();
        let target_config_hash =
            trace_workbench_target_config_hash("evm", "O2", "source-postopt-bytecode");
        let service_config_hash =
            trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);

        let response = handle_trace_query(
            &mut backend,
            TraceBackendQueryRequest {
                uri: uri.to_string(),
                config_hash: Some(service_config_hash),
                target: Some("evm".to_string()),
                opt_level: Some("O2".to_string()),
                view: Some("source-postopt-bytecode".to_string()),
                query: TraceQueryRequest::attribution_audit(),
            },
        )
        .await
        .unwrap();

        let TraceQueryHttpResponse::Ok {
            report: TraceQueryReport::AttributionAudit(report),
            ..
        } = response
        else {
            panic!("expected successful attribution audit response");
        };
        assert!(
            report
                .metadata
                .flags
                .iter()
                .any(|flag| flag == "optimize=O2")
        );

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_query_cache_key_uses_effective_opt_level_without_full_view_tuple() {
        let mut config = introspection_config::FeToolingConfig::default();
        config.lsp.trace.max_query_ms = 30_000;
        let mut backend = test_backend_with_config(config);
        let uri = Url::parse("file:///workspace/src/live_trace_query_cache.fe").unwrap();
        backend.db.workspace().update(
            &mut backend.db,
            uri.clone(),
            r#"
pub fn main() -> u64 {
    (10 - 3) * 2
}
"#
            .to_string(),
        );
        backend.set_document_version(uri.clone(), 11);
        let compiler_config_hash = backend.tooling_config().stable_hash();
        let o1_config_hash = trace_workbench_service_config_hash(
            &compiler_config_hash,
            &trace_workbench_target_config_hash("evm", "O1", "source-postopt-bytecode"),
        );
        let o2_config_hash = trace_workbench_service_config_hash(
            &compiler_config_hash,
            &trace_workbench_target_config_hash("evm", "O2", "source-postopt-bytecode"),
        );

        let o1_response = handle_trace_query(
            &mut backend,
            TraceBackendQueryRequest {
                uri: uri.to_string(),
                config_hash: Some(o1_config_hash),
                target: None,
                opt_level: None,
                view: None,
                query: TraceQueryRequest::attribution_audit(),
            },
        )
        .await
        .unwrap();
        let TraceQueryHttpResponse::Ok {
            report: TraceQueryReport::AttributionAudit(o1_report),
            ..
        } = o1_response
        else {
            panic!("expected successful O1 attribution audit response");
        };
        assert!(
            o1_report
                .metadata
                .flags
                .iter()
                .any(|flag| flag == "optimize=O1")
        );

        let o2_response = handle_trace_query(
            &mut backend,
            TraceBackendQueryRequest {
                uri: uri.to_string(),
                config_hash: Some(o2_config_hash),
                target: None,
                opt_level: Some("O2".to_string()),
                view: None,
                query: TraceQueryRequest::attribution_audit(),
            },
        )
        .await
        .unwrap();
        let TraceQueryHttpResponse::Ok {
            report: TraceQueryReport::AttributionAudit(o2_report),
            cache_hit,
            ..
        } = o2_response
        else {
            panic!("expected successful O2 attribution audit response");
        };
        assert!(
            !cache_hit,
            "O2 query must not reuse the cached O1 trace service"
        );
        assert!(
            o2_report
                .metadata
                .flags
                .iter()
                .any(|flag| flag == "optimize=O2")
        );
        assert!(
            !o2_report
                .metadata
                .flags
                .iter()
                .any(|flag| flag == "optimize=O1")
        );

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[test]
    fn attached_trace_service_loads_validated_snapshot_without_running_compiler() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("combined.trace.jsonl");
        let function = key("bytecode.function", "demo", "runtime");
        let instruction = key("bytecode.pc", "demo", "pc:0");
        let bundle = TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
                "demo.fe",
                vec!["runtime=attached".to_string()],
            ),
            vec![
                node(function.clone()),
                node(instruction.clone()),
                TraceFact::Instruction(InstructionFact::new(instruction, function, 0, "STOP")),
            ],
        );
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();
        std::fs::write(&path, sink.into_inner()).unwrap();

        let mut config = introspection_config::FeToolingConfig::default();
        config.lsp.trace.attached_trace = Some(path.display().to_string());
        let service =
            attached_trace_service(config.lsp.trace.attached_trace.as_deref().unwrap(), &config)
                .unwrap();

        assert_eq!(
            service.snapshot().metadata().flags,
            vec!["runtime=attached"]
        );
        assert_eq!(service.snapshot().validation().summary.instruction_count, 1);
    }

    #[test]
    fn trace_workbench_opt_level_accepts_session_spelling() {
        assert_eq!(
            trace_workbench_parse_opt_level("O0").unwrap(),
            codegen::OptLevel::O0
        );
        assert_eq!(
            trace_workbench_parse_opt_level("O1").unwrap(),
            codegen::OptLevel::O1
        );
        assert_eq!(
            trace_workbench_parse_opt_level("O2").unwrap(),
            codegen::OptLevel::O2
        );
        assert_eq!(
            trace_workbench_parse_opt_level("Os").unwrap(),
            codegen::OptLevel::Os
        );
        assert_eq!(
            trace_workbench_parse_opt_level("2").unwrap(),
            codegen::OptLevel::O2
        );
        assert!(trace_workbench_parse_opt_level("O3").is_err());
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_bootstrap_config_hash_includes_session_opt_level() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/lib.fe").unwrap();
        backend.set_document_version(uri.clone(), 1);
        let o1 = backend.create_trace_viewer_session(
            uri.clone(),
            "evm",
            "O1",
            "source-postopt-bytecode",
            None,
        );
        let o2 =
            backend.create_trace_viewer_session(uri, "evm", "O2", "source-postopt-bytecode", None);

        let o1_response = handle_trace_workbench_bootstrap(
            &mut backend,
            TraceWorkbenchSessionRequest { session_id: o1.id },
        )
        .await
        .unwrap();
        let o2_response = handle_trace_workbench_bootstrap(
            &mut backend,
            TraceWorkbenchSessionRequest { session_id: o2.id },
        )
        .await
        .unwrap();

        assert_ne!(
            o1_response.revision.config_hash,
            o2_response.revision.config_hash
        );
        assert_eq!(o1_response.session.opt_level, "O1");
        assert_eq!(o2_response.session.opt_level, "O2");

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_model_uses_shared_projection_for_attached_trace() {
        let dir = tempfile::tempdir().unwrap();
        let trace_path = dir.path().join("combined.trace.jsonl");
        let source_path = dir.path().join("lib.fe");
        std::fs::write(&source_path, "contract Demo {}\n").unwrap();
        let uri = Url::from_file_path(&source_path).unwrap();
        let function = key("bytecode.function", uri.as_str(), "runtime");
        let instruction = key("bytecode.pc", uri.as_str(), "pc:0");
        let bundle = TraceBundle::new(
            TraceMetadata::compiler_emitted(
                "abc123",
                "evm/sonatina",
                vec!["fe".to_string(), "dev".to_string(), "trace".to_string()],
                uri.as_str(),
                vec!["runtime=attached".to_string()],
            ),
            vec![
                node(function.clone()),
                node(instruction.clone()),
                TraceFact::Instruction(InstructionFact::new(instruction, function, 0, "STOP")),
            ],
        );
        let mut sink = JsonlTraceSink::new(Vec::new());
        sink.write_bundle(&bundle).unwrap();
        std::fs::write(&trace_path, sink.into_inner()).unwrap();
        let mut config = introspection_config::FeToolingConfig::default();
        config.lsp.trace.attached_trace = Some(trace_path.display().to_string());
        let mut backend = test_backend_with_config(config);
        backend.set_document_version(uri.clone(), 11);
        let session =
            backend.create_trace_viewer_session(uri, "evm", "O2", "source-postopt-bytecode", None);

        let model = handle_trace_workbench_model(
            &mut backend,
            TraceWorkbenchSessionRequest {
                session_id: session.id.clone(),
            },
        )
        .await
        .unwrap();

        assert_eq!(model["metadata"]["data_source"], "lsp-live");
        assert_eq!(model["revision"]["id"], 11);
        assert!(model["notes"].as_array().unwrap().iter().any(|note| note
            == "Static and live workbench entry points use the shared trace-query projection path."));
        let revisions = backend.trace_viewer_revisions(&session.id).unwrap();
        assert_eq!(revisions.len(), 1);
        assert_eq!(revisions[0].status, "ready");
        assert!(revisions[0].model_digest.starts_with("blake3:"));
        let compiler_config_hash = backend.tooling_config().stable_hash();
        let target_config_hash =
            trace_workbench_target_config_hash("evm", "O2", "source-postopt-bytecode");
        let config_hash =
            trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);
        assert!(
            backend
                .cached_trace_workbench_model(&session.id, Some(11), &config_hash)
                .is_some()
        );
        let cached_model = handle_trace_workbench_model(
            &mut backend,
            TraceWorkbenchSessionRequest {
                session_id: session.id.clone(),
            },
        )
        .await
        .unwrap();
        assert_eq!(cached_model, model);

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_chunk_handlers_serve_cached_revision_payloads() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/cached_chunks.fe").unwrap();
        backend.set_document_version(uri.clone(), 11);
        let session =
            backend.create_trace_viewer_session(uri, "evm", "O2", "source-postopt-bytecode", None);
        let compiler_config_hash = backend.tooling_config().stable_hash();
        let target_config_hash =
            trace_workbench_target_config_hash("evm", "O2", "source-postopt-bytecode");
        let config_hash =
            trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);
        backend.cache_trace_workbench_model(
            &session.id,
            Some(11),
            config_hash,
            serde_json::json!({ "revision": { "id": 11 } }),
            serde_json::json!({ "revision": 11, "summaryDigest": "blake3:summary" }),
            BTreeMap::from([(
                "blake3:summary".to_string(),
                serde_json::json!({
                    "kind": "summary",
                    "digest": "blake3:summary",
                    "value": { "revision": { "id": 11 } }
                }),
            )]),
        );

        let chunk = handle_trace_workbench_chunk(
            &mut backend,
            TraceWorkbenchChunkRequest {
                session_id: session.id.clone(),
                digest: "blake3:summary".to_string(),
            },
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(chunk["kind"], "summary");
        assert_eq!(chunk["value"]["revision"]["id"], 11);

        let response = handle_trace_workbench_chunks(
            &mut backend,
            TraceWorkbenchChunksRequest {
                session_id: session.id.clone(),
                digests: vec!["blake3:summary".to_string(), "blake3:missing".to_string()],
            },
        )
        .await
        .unwrap();
        assert_eq!(response["chunks"].as_array().unwrap().len(), 1);
        assert_eq!(response["missing"], serde_json::json!(["blake3:missing"]));

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_chunk_handlers_do_not_build_uncached_models() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/uncached_chunks.fe").unwrap();
        backend.set_document_version(uri.clone(), 13);
        let session =
            backend.create_trace_viewer_session(uri, "evm", "O2", "source-postopt-bytecode", None);

        let chunk = handle_trace_workbench_chunk(
            &mut backend,
            TraceWorkbenchChunkRequest {
                session_id: session.id.clone(),
                digest: "blake3:summary".to_string(),
            },
        )
        .await
        .unwrap();
        assert!(
            chunk.is_none(),
            "uncached chunk lookup should not build a full trace model"
        );

        let response = handle_trace_workbench_chunks(
            &mut backend,
            TraceWorkbenchChunksRequest {
                session_id: session.id.clone(),
                digests: vec!["blake3:summary".to_string(), "blake3:pane".to_string()],
            },
        )
        .await
        .unwrap();
        assert!(response["chunks"].as_array().unwrap().is_empty());
        assert_eq!(
            response["missing"],
            serde_json::json!(["blake3:summary", "blake3:pane"])
        );

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_bootstrap_reports_current_document_revision() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/lib.fe").unwrap();
        backend.set_document_version(uri.clone(), 1);
        let session = backend.create_trace_viewer_session(
            uri.clone(),
            "evm",
            "O2",
            "source-postopt-bytecode",
            None,
        );

        backend.set_document_version(uri, 4);

        let response = handle_trace_workbench_bootstrap(
            &mut backend,
            TraceWorkbenchSessionRequest {
                session_id: session.id,
            },
        )
        .await
        .unwrap();

        assert_eq!(response.revision.id, 4);
        assert_eq!(response.revision.document_version, Some(4));
        assert_eq!(response.revision.status, "pending");
        assert_eq!(response.revision.model_digest, None);
        assert_eq!(response.session.document_version, Some(4));

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_bootstrap_reports_latest_model_digest() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/lib.fe").unwrap();
        backend.set_document_version(uri.clone(), 9);
        let session =
            backend.create_trace_viewer_session(uri, "evm", "O2", "source-postopt-bytecode", None);
        let compiler_config_hash = backend.tooling_config().stable_hash();
        let target_config_hash =
            trace_workbench_target_config_hash("evm", "O2", "source-postopt-bytecode");
        let config_hash =
            trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);
        backend.record_trace_viewer_revision(
            &session.id,
            TraceViewerRevisionRecord {
                revision: 9,
                previous_revision: None,
                document_version: Some(9),
                status: "ready".to_string(),
                config_hash: config_hash.clone(),
                compiler_config_hash,
                target_config_hash,
                target: "evm".to_string(),
                opt_level: "O2".to_string(),
                view: "source-postopt-bytecode".to_string(),
                source_hash: Some("blake3:source-text".to_string()),
                trace_snapshot_digest: "blake3:trace".to_string(),
                model_digest: "blake3:model".to_string(),
                summary_digest: "blake3:summary".to_string(),
                source_digest: "blake3:source".to_string(),
                indexes_digest: "blake3:indexes".to_string(),
                rail_components_digest: "blake3:rails".to_string(),
                pane_digests: BTreeMap::new(),
                report_digests: BTreeMap::new(),
            },
        );

        let response = handle_trace_workbench_bootstrap(
            &mut backend,
            TraceWorkbenchSessionRequest {
                session_id: session.id,
            },
        )
        .await
        .unwrap();

        assert_eq!(response.revision.status, "ready");
        assert_eq!(
            response.revision.model_digest.as_deref(),
            Some("blake3:model")
        );

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 1)]
    async fn trace_workbench_bootstrap_reports_stale_ready_revision() {
        let mut backend = test_backend();
        let uri = Url::parse("file:///workspace/src/lib.fe").unwrap();
        backend.set_document_version(uri.clone(), 9);
        let session = backend.create_trace_viewer_session(
            uri.clone(),
            "evm",
            "O2",
            "source-postopt-bytecode",
            None,
        );
        let compiler_config_hash = backend.tooling_config().stable_hash();
        let target_config_hash =
            trace_workbench_target_config_hash("evm", "O2", "source-postopt-bytecode");
        let config_hash =
            trace_workbench_service_config_hash(&compiler_config_hash, &target_config_hash);
        backend.record_trace_viewer_revision(
            &session.id,
            TraceViewerRevisionRecord {
                revision: 9,
                previous_revision: None,
                document_version: Some(9),
                status: "ready".to_string(),
                config_hash,
                compiler_config_hash,
                target_config_hash,
                target: "evm".to_string(),
                opt_level: "O2".to_string(),
                view: "source-postopt-bytecode".to_string(),
                source_hash: Some("blake3:source-text".to_string()),
                trace_snapshot_digest: "blake3:trace".to_string(),
                model_digest: "blake3:model".to_string(),
                summary_digest: "blake3:summary".to_string(),
                source_digest: "blake3:source".to_string(),
                indexes_digest: "blake3:indexes".to_string(),
                rail_components_digest: "blake3:rails".to_string(),
                pane_digests: BTreeMap::new(),
                report_digests: BTreeMap::new(),
            },
        );
        backend.set_document_version(uri, 10);

        let response = handle_trace_workbench_bootstrap(
            &mut backend,
            TraceWorkbenchSessionRequest {
                session_id: session.id,
            },
        )
        .await
        .unwrap();

        assert_eq!(response.revision.id, 10);
        assert_eq!(response.revision.document_version, Some(10));
        assert_eq!(response.revision.status, "stale_but_usable");
        assert_eq!(
            response.revision.model_digest.as_deref(),
            Some("blake3:model")
        );

        tokio::task::spawn_blocking(move || drop(backend))
            .await
            .expect("backend drop task panicked");
    }
}
