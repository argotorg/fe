use trace_facts::{OriginNodeFact, OriginNodeKind, SourceFileFact, SourceSpanFact, TraceFact};

use crate::origin::{
    HIR_EXPR_EXPORT_KIND, HIR_STMT_EXPORT_KIND, HirExprOrigin, HirOriginBodyOwnerKey, HirStmtOrigin,
};
use crate::{
    SpannedHirDb,
    hir_def::Body,
    span::lazy_spans::{LazyExprSpan, LazySpan, LazyStmtSpan},
};

/// Emit HIR-owned trace facts for exported HIR origins.
///
/// HIR owns HIR expression/statement identity. It does not emit MIR, backend,
/// storage, or instruction facts.
pub fn emit_hir_facts<'db>(
    stable_body_key: &HirOriginBodyOwnerKey,
    exprs: impl IntoIterator<Item = HirExprOrigin<'db>>,
    stmts: impl IntoIterator<Item = HirStmtOrigin<'db>>,
) -> Vec<TraceFact> {
    exprs
        .into_iter()
        .map(|origin| origin_node(origin.export_key(stable_body_key), HIR_EXPR_EXPORT_KIND))
        .chain(
            stmts.into_iter().map(|origin| {
                origin_node(origin.export_key(stable_body_key), HIR_STMT_EXPORT_KIND)
            }),
        )
        .collect()
}

/// Emit HIR identity and source-span facts for one HIR body.
///
/// This is intentionally HIR-owned even when a downstream phase calls it to
/// attach its own origins to HIR keys.
pub fn emit_hir_body_facts_with_source_spans<'db>(
    db: &'db dyn SpannedHirDb,
    stable_body_key: &HirOriginBodyOwnerKey,
    body: Body<'db>,
) -> Vec<TraceFact> {
    let exprs = body.exprs(db).keys().collect::<Vec<_>>();
    let stmts = body.stmts(db).keys().collect::<Vec<_>>();
    let mut facts = emit_hir_facts(
        stable_body_key,
        exprs
            .iter()
            .copied()
            .map(|expr| HirExprOrigin::new(body, expr)),
        stmts
            .iter()
            .copied()
            .map(|stmt| HirStmtOrigin::new(body, stmt)),
    );
    let mut emitted_source_files = std::collections::BTreeSet::new();
    let mut line_starts_by_file = std::collections::BTreeMap::new();
    for expr in exprs {
        let origin = HirExprOrigin::new(body, expr).export_key(stable_body_key);
        push_hir_source_span_fact(
            db,
            &mut facts,
            &mut emitted_source_files,
            &mut line_starts_by_file,
            origin,
            LazyExprSpan::new(body, expr),
        );
    }
    for stmt in stmts {
        let origin = HirStmtOrigin::new(body, stmt).export_key(stable_body_key);
        push_hir_source_span_fact(
            db,
            &mut facts,
            &mut emitted_source_files,
            &mut line_starts_by_file,
            origin,
            LazyStmtSpan::new(body, stmt),
        );
    }
    facts
}

fn origin_node(key: common::origin::OriginExportKey, kind: &str) -> TraceFact {
    TraceFact::OriginNode(OriginNodeFact::new(key, OriginNodeKind::new(kind)))
}

fn push_hir_source_span_fact(
    db: &dyn SpannedHirDb,
    facts: &mut Vec<TraceFact>,
    emitted_source_files: &mut std::collections::BTreeSet<common::origin::OriginExportKey>,
    line_starts_by_file: &mut std::collections::BTreeMap<common::file::File, Vec<u32>>,
    origin: common::origin::OriginExportKey,
    span: impl LazySpan,
) {
    let Some(span) = span.resolve(db) else {
        return;
    };
    if matches!(span.kind, common::diagnostics::SpanKind::NotFound) {
        return;
    }
    let file_key = source_file_key(db, span.file);
    if emitted_source_files.insert(file_key.clone()) {
        let text = span.file.text(db);
        facts.push(origin_node(file_key.clone(), file_key.kind()));
        facts.push(TraceFact::SourceFile(SourceFileFact::new(
            file_key.clone(),
            source_file_uri(db, span.file),
            source_file_display_name(db, span.file),
            trace_content_hash(text.as_bytes()),
            None,
        )));
    }
    let line_starts = line_starts_by_file
        .entry(span.file)
        .or_insert_with(|| line_starts(span.file.text(db)));
    let Some(span_fact) = source_span_fact(db, origin, file_key, &span, line_starts) else {
        return;
    };
    facts.push(TraceFact::SourceSpan(span_fact));
}

fn source_span_fact(
    db: &dyn SpannedHirDb,
    origin: common::origin::OriginExportKey,
    file_key: common::origin::OriginExportKey,
    span: &common::diagnostics::Span,
    line_starts: &[u32],
) -> Option<SourceSpanFact> {
    let text = span.file.text(db);
    let start_byte: u32 = span.range.start().into();
    let end_byte: u32 = span.range.end().into();
    if start_byte >= end_byte || end_byte as usize > text.len() {
        return None;
    }
    let (start_line, start_column) = line_column_for_byte(text, line_starts, start_byte);
    let (end_line, end_column) = line_column_for_byte(text, line_starts, end_byte);
    Some(SourceSpanFact::new(
        origin,
        file_key,
        start_byte,
        end_byte,
        start_line,
        start_column,
        end_line,
        end_column,
    ))
}

fn source_file_key(
    db: &dyn SpannedHirDb,
    file: common::file::File,
) -> common::origin::OriginExportKey {
    let owner = source_file_uri(db, file);
    common::origin::OriginExportKey::try_from_raw_parts("source.file", owner, "file:0")
        .expect("source file origin key must be valid")
}

fn source_file_uri(db: &dyn SpannedHirDb, file: common::file::File) -> String {
    file.url(db)
        .map(|url| url.to_string())
        .or_else(|| file.path(db).as_ref().map(ToString::to_string))
        .unwrap_or_else(|| format!("{file:?}"))
}

fn source_file_display_name(db: &dyn SpannedHirDb, file: common::file::File) -> String {
    file.path(db)
        .as_ref()
        .and_then(|path| path.file_name())
        .map(ToString::to_string)
        .or_else(|| file.url(db).map(|url| url.to_string()))
        .unwrap_or_else(|| "source".to_string())
}

/// 0-based byte offsets of every line start, for binary-searched line/column
/// lookups instead of rescanning the file per span boundary.
fn line_starts(text: &str) -> Vec<u32> {
    std::iter::once(0)
        .chain(
            text.char_indices()
                .filter(|(_, ch)| *ch == '\n')
                .map(|(idx, _)| idx as u32 + 1),
        )
        .collect()
}

fn line_column_for_byte(text: &str, line_starts: &[u32], byte: u32) -> (u32, u32) {
    let line_index = line_starts
        .partition_point(|start| *start <= byte)
        .saturating_sub(1);
    let line_start = line_starts.get(line_index).copied().unwrap_or(0);
    let column = text
        .get(line_start as usize..byte as usize)
        .map_or(0, |prefix| prefix.chars().count()) as u32
        + 1;
    (line_index as u32 + 1, column)
}

fn trace_content_hash(bytes: &[u8]) -> String {
    format!("blake3:{}", blake3::hash(bytes).to_hex())
}
