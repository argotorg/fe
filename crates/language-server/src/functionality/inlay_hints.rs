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

    // Collect hints from all function bodies in the module
    collect_hints_from_mod(&backend.db, top_mod, &mut hints);

    Ok(Some(hints))
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
