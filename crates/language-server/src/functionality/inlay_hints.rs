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
use introspection_config::{HintMode, LoopHintMode};
use trace_query::{GasBreakdownRequest, IntrospectionService, LoopCostRequest};

pub async fn handle_inlay_hints(
    backend: &Backend,
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
    collect_trace_hints(backend, &url, params.range.start, &mut hints).await;
    hints.truncate(backend.tooling_config().lsp.max_hints_per_file);

    Ok(Some(hints))
}

async fn collect_trace_hints(
    backend: &Backend,
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

    let url = url.clone();
    let gas_schedule = config.lsp.gas.schedule.clone();
    let service_config = config.clone();
    let service = match backend
        .spawn_on_workers(move |db| {
            crate::introspection::service_for_file(db, &url, service_config)
        })
        .await
    {
        Ok(Ok(Some(service))) => service,
        Ok(Ok(None)) => return,
        Ok(Err(err)) => {
            tracing::debug!("trace inlay service unavailable: {err}");
            return;
        }
        Err(err) => {
            tracing::debug!("trace inlay worker failed: {err}");
            return;
        }
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
                "Static gas estimate under {} schedule",
                report.schedule
            ))),
            padding_left: Some(true),
            padding_right: None,
            data: None,
        });
    }
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
