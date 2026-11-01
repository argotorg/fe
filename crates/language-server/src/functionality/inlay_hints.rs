use crate::{backend::Backend, util::to_lsp_range_from_span};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    analysis::ty::ty_check::check_func_body,
    hir_def::{Body, Expr, FuncParamName, ItemKind, Partial, StmtId, TopLevelMod},
    lower::map_file_to_mod,
    visitor::prelude::*,
};

pub async fn handle_inlay_hints(
    backend: &Backend,
    params: async_lsp::lsp_types::InlayHintParams,
) -> Result<Option<Vec<InlayHint>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(params.text_document.uri);
    let analysis_db = backend.analysis_db_for_url(&url);

    let file = analysis_db
        .workspace()
        .get(&analysis_db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found: {url}"),
            )
        })?;

    let top_mod = map_file_to_mod(&analysis_db, file);
    let mut hints = Vec::new();

    // Collect hints from all function bodies in the module
    collect_hints_from_mod(&analysis_db, top_mod, &mut hints);

    Ok(Some(hints))
}

fn collect_hints_from_mod(db: &DriverDataBase, top_mod: TopLevelMod, hints: &mut Vec<InlayHint>) {
    // Iterate through all items in the module
    let items = top_mod.all_items(db).iter().copied();

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
        if let hir::hir_def::Stmt::Let(pat, ty, expr, _) = stmt_data {
            let body = ctxt.body();
            // Only show hint if there's no explicit type annotation and there's an initializer.
            // The `let` that destructures a parameter has its type in the signature.
            if ty.is_none()
                && let Some(expr_id) = *expr
                && !matches!(
                    expr_id.data(self.db, body),
                    Partial::Present(Expr::Path(Partial::Present(path)))
                        if FuncParamName::is_destructured_read(self.db, *path)
                )
            {
                let inferred_ty = self.typed_body.expr_ty(self.db, expr_id);
                let ty_str = inferred_ty.pretty_print(self.db);

                // Get the span of the pattern
                if let Some(span) = pat.span(body).resolve(self.db)
                    && let Ok(range) = to_lsp_range_from_span(span, self.db)
                {
                    // Position hint after the pattern
                    let hint = InlayHint {
                        position: range.end,
                        label: InlayHintLabel::String(format!(": {ty_str}")),
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

#[cfg(test)]
mod tests {
    use async_lsp::lsp_types::InlayHintLabel;
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    use super::collect_hints_from_mod;

    #[test]
    fn param_pattern_has_no_hint() {
        let mut db = DriverDataBase::default();
        let code = r#"fn sum(_ (a, b): (u256, u256)) -> u256 {
    let c = a + b
    let f = |(x, y): (u256, u256)| x + y
    f((c, 1))
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///param_pattern_hints.fe").unwrap(),
            Some(code.to_string()),
        );
        let mut hints = Vec::new();
        collect_hints_from_mod(&db, map_file_to_mod(&db, file), &mut hints);
        let labels: Vec<_> = hints
            .into_iter()
            .map(|hint| match hint.label {
                InlayHintLabel::String(label) => label,
                InlayHintLabel::LabelParts(_) => panic!("expected a string label"),
            })
            .collect();
        assert_eq!(labels, [": u256", ": |(u256, u256)| -> u256"]);
    }
}
