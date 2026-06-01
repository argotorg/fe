use common::InputDb;
use driver::DriverDataBase;
use hir::lower::map_file_to_mod;
use serde::Serialize;
use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::BufReader;
use std::time::{Duration, Instant};
use trace_facts::{CompilerPhase, TraceBundle, TraceFact, TraceMetadata, TraceSnapshot};
use trace_query::{
    TraceIntrospectionService, TraceQueryHttpResponse, TraceQueryRequest,
    TraceWorkbenchProjectionRequest, run_trace_query, trace_workbench_manifest,
    trace_workbench_report_projection,
};
use url::Url;

use crate::backend::{Backend, TraceViewerRevisionRecord};

#[derive(Clone, Debug)]
pub(crate) struct TraceBackendQueryRequest {
    pub uri: String,
    pub config_hash: Option<String>,
    pub query: TraceQueryRequest,
}

#[derive(Clone, Debug)]
pub(crate) struct TraceWorkbenchSessionRequest {
    pub session_id: String,
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
    let config_hash = backend.tooling_config().stable_hash();
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
    let current_config_hash = backend.tooling_config().stable_hash();
    let document_version = backend.document_version(&uri);
    let source_text = backend
        .db
        .workspace()
        .get(&backend.db, &uri)
        .map(|file| file.text(&backend.db).to_string());
    let source_hash = source_text.as_deref().map(trace_workbench_digest_text);

    let service = if let Some(version) = document_version
        && let Some(service) = backend.cached_trace_service(&uri, version, &current_config_hash)
    {
        service
    } else {
        let config = backend.tooling_config().clone();
        let worker_uri = uri.clone();
        let worker = backend.spawn_on_workers(move |db| {
            service_for_file(db, &worker_uri, config)?
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
                current_config_hash.clone(),
                service.clone(),
            );
        }
        service
    };
    let related_source_texts =
        trace_workbench_related_source_texts(backend, service.snapshot(), &uri);
    let trace_snapshot_digest = service.snapshot().trace_hash().to_string();
    let target_config_hash =
        trace_workbench_target_config_hash(&session.target, &session.opt_level, &session.view);
    let projection = trace_workbench_report_projection(
        &service,
        service.snapshot(),
        TraceWorkbenchProjectionRequest {
            input_path: session.uri.clone(),
            target: session.target.clone(),
            opt_level: session.opt_level.clone(),
            view: session.view.clone(),
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
    backend.record_trace_viewer_revision(
        &request.session_id,
        TraceViewerRevisionRecord {
            revision: manifest.revision,
            previous_revision: None,
            document_version,
            status: "ready".to_string(),
            config_hash: current_config_hash.clone(),
            compiler_config_hash: current_config_hash,
            target_config_hash,
            target: session.target,
            opt_level: session.opt_level,
            view: session.view,
            source_hash,
            trace_snapshot_digest,
            model_digest: manifest.root_digest,
            summary_digest: manifest.summary_digest,
            source_digest: manifest.source_digest,
            indexes_digest: manifest.indexes_digest,
            rail_components_digest: manifest.rail_components_digest,
            pane_digests: manifest.panes,
            report_digests: manifest.reports,
        },
    );
    Ok(projection)
}

fn trace_workbench_target_config_hash(target: &str, opt_level: &str, view: &str) -> String {
    trace_workbench_digest_text(&format!(
        "target={target}\nopt_level={opt_level}\nview={view}\n"
    ))
}

fn trace_workbench_digest_text(text: &str) -> String {
    format!("blake3:{}", blake3::hash(text.as_bytes()).to_hex())
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
    let model = handle_trace_workbench_model(backend, request).await?;
    serde_json::to_value(trace_workbench_manifest(&model))
        .map_err(|err| internal_error(format!("failed to serialize trace manifest: {err}")))
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

pub(crate) async fn handle_trace_query(
    backend: &mut Backend,
    request: TraceBackendQueryRequest,
) -> Result<TraceQueryHttpResponse, async_lsp::ResponseError> {
    let started = Instant::now();
    let current_config_hash = backend.tooling_config().stable_hash();
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
        let service = service_for_file(db, &internal_uri_for_worker, config)?.ok_or_else(|| {
            format!("no Fe source file is loaded for URI {internal_uri_for_worker}")
        })?;
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
    if let Some(attached_trace) = config.lsp.trace.attached_trace.as_deref()
        && !attached_trace.trim().is_empty()
    {
        return attached_trace_service(attached_trace, &config).map(Some);
    }

    let Some(file) = db.workspace().get(db, uri) else {
        return Ok(None);
    };
    let top_mod = map_file_to_mod(db, file);
    let package = mir::build_runtime_package(db, top_mod)
        .map_err(|err| format!("runtime package lowering for trace: {err}"))?;
    let mut facts = mir::trace::emit_mir_facts(db, package);
    let module_key = top_mod.name(db).data(db).to_string();
    let sonatina_module =
        codegen::compile_runtime_package_sonatina(db, &package, codegen::EVM_LAYOUT)
            .map_err(|err| format!("Sonatina IR lowering for trace: {err}"))?;
    let sonatina_owner = codegen::trace::sonatina_module_owner_key(uri.as_str(), &module_key);
    facts.extend(codegen::trace::emit_sonatina_trace_view_facts(
        &sonatina_owner,
        &sonatina_module,
        CompilerPhase::SonatinaPreOpt,
    ));
    let bytecode = codegen::emit_module_sonatina_bytecode(db, top_mod, codegen::OptLevel::O1, None)
        .map_err(|err| format!("bytecode emission for trace: {err}"))?;
    for (contract_name, artifact) in bytecode {
        let owner_key =
            codegen::trace::bytecode_runtime_owner_key(uri.as_str(), &module_key, &contract_name);
        facts.extend(codegen::trace::emit_bytecode_instruction_facts(
            &owner_key,
            "function:runtime",
            &artifact.runtime,
        ));
    }
    enforce_trace_limits(&facts, &config)?;

    let metadata = TraceMetadata::compiler_emitted(
        option_env!("FE_GIT_COMMIT").unwrap_or("unknown"),
        "evm/sonatina",
        vec!["fe-language-server".to_string(), "trace".to_string()],
        uri.to_string(),
        vec!["source=lsp-live".to_string()],
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
    use common::origin::OriginExportKey;
    use std::collections::BTreeMap;
    use trace_facts::{
        InstructionFact, JsonlTraceSink, OriginNodeFact, OriginNodeKind, TraceBundle, TraceFact,
        TraceMetadata,
    };
    use url::Url;

    use super::{
        TraceWorkbenchSessionRequest, attached_trace_service, handle_trace_workbench_bootstrap,
        handle_trace_workbench_model,
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
            == "The live endpoint uses the shared trace-query workbench projection path."));
        let revisions = backend.trace_viewer_revisions(&session.id).unwrap();
        assert_eq!(revisions.len(), 1);
        assert_eq!(revisions[0].status, "ready");
        assert!(revisions[0].model_digest.starts_with("blake3:"));

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
        let config_hash = backend.tooling_config().stable_hash();
        backend.record_trace_viewer_revision(
            &session.id,
            TraceViewerRevisionRecord {
                revision: 9,
                previous_revision: None,
                document_version: Some(9),
                status: "ready".to_string(),
                config_hash: config_hash.clone(),
                compiler_config_hash: config_hash,
                target_config_hash: "blake3:target".to_string(),
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
        backend.record_trace_viewer_revision(
            &session.id,
            TraceViewerRevisionRecord {
                revision: 9,
                previous_revision: None,
                document_version: Some(9),
                status: "ready".to_string(),
                config_hash: backend.tooling_config().stable_hash(),
                compiler_config_hash: backend.tooling_config().stable_hash(),
                target_config_hash: "blake3:target".to_string(),
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
