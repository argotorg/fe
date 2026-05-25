use common::InputDb;
use driver::DriverDataBase;
use hir::lower::map_file_to_mod;
use std::time::{Duration, Instant};
use trace_facts::{CompilerPhase, TraceBundle, TraceMetadata, TraceSnapshot};
use trace_query::{
    TraceIntrospectionService, TraceQueryHttpResponse, TraceQueryRequest, run_trace_query,
};
use url::Url;

use crate::backend::Backend;

#[derive(Clone, Debug)]
pub(crate) struct TraceBackendQueryRequest {
    pub uri: String,
    pub config_hash: Option<String>,
    pub query: TraceQueryRequest,
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
    if !path.extension().is_some_and(|ext| ext == "fe") {
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
