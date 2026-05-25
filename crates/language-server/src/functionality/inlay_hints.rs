use crate::{backend::Backend, util::to_lsp_range_from_span};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    analysis::ty::ty_check::check_func_body,
    hir_def::{Body, ItemKind, StmtId, TopLevelMod},
    lower::map_file_to_mod,
    visitor::prelude::*,
};
use introspection_config::{FeToolingConfig, HintMode, LoopHintMode};
use std::time::Duration;
use trace_query::{
    GasBreakdownRequest, IntrospectionService, LoopCostRequest, TraceIntrospectionService,
};

pub async fn handle_inlay_hints(
    backend: &mut Backend,
    params: async_lsp::lsp_types::InlayHintParams,
) -> Result<Option<Vec<InlayHint>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(params.text_document.uri.clone());

    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found: {url}"),
            )
        })?;

    let top_mod = map_file_to_mod(&backend.db, file);
    let mut hints = Vec::new();
    let config = &backend.tooling_config().lsp.inlay_hints;

    // Collect hints from all function bodies in the module
    if config.types {
        collect_hints_from_mod(&backend.db, top_mod, &mut hints);
    }
    let trace_anchor =
        first_trace_anchor_position(&backend.db, top_mod).unwrap_or(params.range.start);
    collect_trace_hints(backend, &url, trace_anchor, &mut hints).await;
    hints.truncate(backend.tooling_config().lsp.max_hints_per_file);

    Ok(Some(hints))
}

async fn collect_trace_hints(
    backend: &mut Backend,
    url: &url::Url,
    position: async_lsp::lsp_types::Position,
    hints: &mut Vec<InlayHint>,
) {
    let config = backend.tooling_config().clone();
    let inlay = &config.lsp.inlay_hints;
    let wants_loop = inlay.loop_cost != LoopHintMode::Off;
    let wants_gas = config.lsp.gas.enabled && inlay.gas != HintMode::Off;
    if !wants_loop && !wants_gas {
        return;
    }

    let gas_schedule = config.lsp.gas.schedule.clone();
    let Some(service) = trace_service_for_inlays(backend, url, config.clone()).await else {
        return;
    };

    if wants_loop
        && let Ok(report) = service.loop_cost(LoopCostRequest::default())
        && report.available
    {
        hints.push(InlayHint {
            position,
            label: InlayHintLabel::String(format!(
                " trace: {} inst/iter",
                report.summary.total_instructions
            )),
            kind: Some(InlayHintKind::PARAMETER),
            text_edits: None,
            tooltip: Some(async_lsp::lsp_types::InlayHintTooltip::String(format!(
                "Trace-backed loop cost from {} ({})",
                report.metadata.data_source,
                format!("{:?}", report.confidence)
            ))),
            padding_left: Some(true),
            padding_right: None,
            data: None,
        });
    }

    if wants_gas
        && let Ok(report) = service.gas_breakdown(GasBreakdownRequest {
            schedule: gas_schedule,
        })
        && report.available
        && let Some(total_gas) = report.total_gas
    {
        hints.push(InlayHint {
            position,
            label: InlayHintLabel::String(format!(" ~{total_gas} gas static")),
            kind: Some(InlayHintKind::PARAMETER),
            text_edits: None,
            tooltip: Some(async_lsp::lsp_types::InlayHintTooltip::String(format!(
                "Static opcode gas estimate under {} schedule ({:?} confidence)",
                report.schedule, report.confidence
            ))),
            padding_left: Some(true),
            padding_right: None,
            data: None,
        });
    }
}

async fn trace_service_for_inlays(
    backend: &mut Backend,
    url: &url::Url,
    config: FeToolingConfig,
) -> Option<TraceIntrospectionService> {
    let config_hash = config.stable_hash();
    let document_version = backend.document_version(url);
    if let Some(version) = document_version
        && let Some(service) = backend.cached_trace_service(url, version, &config_hash)
    {
        return Some(service);
    }

    let trace_config = config.lsp.trace.clone();
    if trace_config.debounce_ms > 0 {
        tokio::time::sleep(Duration::from_millis(trace_config.debounce_ms)).await;
    }

    let url_for_worker = url.clone();
    let service_config = config.clone();
    let worker = backend.spawn_on_workers(move |db| {
        crate::introspection::service_for_file(db, &url_for_worker, service_config)
    });
    let service = match tokio::time::timeout(
        Duration::from_millis(trace_config.max_query_ms.max(1)),
        worker,
    )
    .await
    {
        Ok(Ok(Ok(Some(service)))) => service,
        Ok(Ok(Ok(None))) => return None,
        Ok(Ok(Err(err))) => {
            tracing::debug!("trace inlay service unavailable: {err}");
            return None;
        }
        Ok(Err(err)) => {
            tracing::debug!("trace inlay worker failed: {err}");
            return None;
        }
        Err(_) => {
            tracing::debug!(
                "trace inlay service exceeded {}ms budget",
                trace_config.max_query_ms.max(1)
            );
            return None;
        }
    };

    if let Some(version) = document_version {
        backend.cache_trace_service(url.clone(), version, config_hash, service.clone());
    }
    Some(service)
}

fn first_trace_anchor_position(
    db: &DriverDataBase,
    top_mod: TopLevelMod,
) -> Option<async_lsp::lsp_types::Position> {
    for item in top_mod.scope_graph(db).items_dfs(db) {
        if !matches!(item, ItemKind::Contract(_) | ItemKind::Func(_)) {
            continue;
        }
        if let Some(span) = item.span().resolve(db)
            && let Ok(range) = to_lsp_range_from_span(span, db)
        {
            return Some(range.start);
        }
    }
    None
}

fn collect_hints_from_mod(db: &DriverDataBase, top_mod: TopLevelMod, hints: &mut Vec<InlayHint>) {
    // Iterate through all items in the module
    let items = top_mod.scope_graph(db).items_dfs(db);

    for item in items {
        match item {
            ItemKind::Func(func) => {
                // Get the typed body for this function
                let (_, typed_body) = check_func_body(db, func);
                if let Some(body) = typed_body.body() {
                    collect_hints_from_body(db, body, typed_body, hints);
                }
            }
            _ => continue,
        }
    }
}

fn collect_hints_from_body(
    db: &DriverDataBase,
    body: Body,
    typed_body: &hir::analysis::ty::ty_check::TypedBody,
    hints: &mut Vec<InlayHint>,
) {
    // Visit all statements in the body
    let mut visitor_ctxt = VisitorCtxt::with_body(db, body);
    let mut hint_collector = InlayHintCollector {
        db,
        typed_body,
        hints,
    };
    hint_collector.visit_body(&mut visitor_ctxt, body);
}

struct InlayHintCollector<'a, 'db> {
    db: &'db DriverDataBase,
    typed_body: &'a hir::analysis::ty::ty_check::TypedBody<'db>,
    hints: &'a mut Vec<InlayHint>,
}

impl<'a, 'db> Visitor<'db> for InlayHintCollector<'a, 'db> {
    fn visit_stmt(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyStmtSpan<'db>>,
        stmt: StmtId,
        stmt_data: &hir::hir_def::Stmt<'db>,
    ) {
        // Check if this is a let statement without type annotation
        if let hir::hir_def::Stmt::Let(pat, ty, expr) = stmt_data {
            // Only show hint if there's no explicit type annotation and there's an initializer
            if ty.is_none() && expr.is_some() {
                let expr_id = expr.unwrap();
                let inferred_ty = self.typed_body.expr_ty(self.db, expr_id);
                let ty_str = inferred_ty.pretty_print(self.db);

                // Get the span of the pattern
                let body = ctxt.body();
                if let Some(span) = pat.span(body).resolve(self.db)
                    && let Ok(range) = to_lsp_range_from_span(span, self.db)
                {
                    // Position hint after the pattern
                    let hint = InlayHint {
                        position: range.end,
                        label: InlayHintLabel::String(format!(": {}", ty_str)),
                        kind: Some(InlayHintKind::TYPE),
                        text_edits: None,
                        tooltip: None,
                        padding_left: None,
                        padding_right: None,
                        data: None,
                    };
                    self.hints.push(hint);
                }
            }
        }

        // Continue visiting nested statements
        walk_stmt(self, ctxt, stmt);
    }
}
