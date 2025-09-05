use hir::{span::{DynLazySpan, LazySpan}, SpannedHirDb};
use parser::SyntaxNode;
use std::ops::Range;

pub fn collect_positions(root: &SyntaxNode) -> Vec<parser::TextSize> {
    use parser::{ast, ast::prelude::AstNode, SyntaxKind};
    fn walk(node: &SyntaxNode, out: &mut Vec<parser::TextSize>) {
        match node.kind() {
            SyntaxKind::Ident => out.push(node.text_range().start()),
            SyntaxKind::Path => {
                if let Some(path) = ast::Path::cast(node.clone()) {
                    for seg in path.segments() {
                        if let Some(id) = seg.ident() { out.push(id.text_range().start()); }
                    }
                }
            }
            SyntaxKind::PathType => {
                if let Some(pt) = ast::PathType::cast(node.clone()) {
                    if let Some(path) = pt.path() {
                        for seg in path.segments() {
                            if let Some(id) = seg.ident() { out.push(id.text_range().start()); }
                        }
                    }
                }
            }
            SyntaxKind::FieldExpr => {
                if let Some(fe) = ast::FieldExpr::cast(node.clone()) {
                    if let Some(tok) = fe.field_name() { out.push(tok.text_range().start()); }
                }
            }
            SyntaxKind::UsePath => {
                if let Some(up) = ast::UsePath::cast(node.clone()) {
                    for seg in up.into_iter() {
                        if let Some(tok) = seg.ident() { out.push(tok.text_range().start()); }
                    }
                }
            }
            _ => {}
        }
        for ch in node.children() { walk(&ch, out); }
    }
    let mut v = Vec::new(); walk(root, &mut v); v.sort(); v.dedup(); v
}

pub fn line_col_from_cursor(cursor: parser::TextSize, s: &str) -> (usize, usize) {
    let mut line=0usize; let mut col=0usize;
    for (i, ch) in s.chars().enumerate() {
        if i == Into::<usize>::into(cursor) { return (line, col); }
        if ch == '\n' { line+=1; col=0; } else { col+=1; }
    }
    (line, col)
}

pub fn format_snapshot(content: &str, lines: &[String]) -> String {
    let header = content.lines().enumerate().map(|(i,l)| format!("{i:?}: {l}")).collect::<Vec<_>>().join("\n");
    let body = lines.join("\n");
    format!("{header}\n---\n{body}")
}

/// Format a snapshot with inline ASCII caret arrows under the indicated
/// (line, col) positions. Each annotation renders one extra line below the
/// corresponding source line, showing a caret under the column and the label.
pub fn format_snapshot_with_arrows(content: &str, anns: &[(usize, usize, String)]) -> String {
    use std::collections::BTreeMap;
    let mut per_line: BTreeMap<usize, Vec<(usize, String)>> = BTreeMap::new();
    for (line, col, label) in anns.iter().cloned() {
        per_line.entry(line).or_default().push((col, label));
    }
    // Sort columns per line to render multiple carets left-to-right
    for v in per_line.values_mut() { v.sort_by_key(|(c, _)| *c); }

    let mut out = String::new();
    for (i, src_line) in content.lines().enumerate() {
        out.push_str(&format!("{i:?}: {src_line}\n"));
        if let Some(cols) = per_line.get(&i) {
            // Build a caret line; if multiple carets, place them and separate labels with " | "
            let mut caret = String::new();
            // Indent to align after the "{i:?}: " prefix; keep a fixed 4-chars spacing for simplicity
            caret.push_str("    ");
            let mut cursor = 0usize;
            for (j, (col, label)) in cols.iter().enumerate() {
                // Pad spaces from current cursor to col
                if *col >= cursor { caret.push_str(&" ".repeat(*col - cursor)); }
                caret.push('^');
                cursor = *col + 1;
                // Append label aligned a few spaces after the caret for the first; subsequent labels go after a separator
                if j == 0 {
                    caret.push_str("  ");
                    caret.push_str(label);
                } else {
                    caret.push_str(" | ");
                    caret.push_str(label);
                }
            }
            out.push_str(&caret);
            out.push('\n');
        }
    }
    out
}

/// Render a codespan-reporting snippet showing a primary caret at `cursor`
/// and secondary carets at each `ref_spans` (byte ranges) with labels.
pub fn codespan_render_refs(
    file_name: &str,
    content: &str,
    cursor: usize,
    ref_spans: &[(Range<usize>, String)],
) -> String {
    use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
    use codespan_reporting::term::{emit, Config};
    use termcolor::Buffer;

    let mut out = Buffer::no_color();
    let cfg = Config::default();
    let mut files = codespan_reporting::files::SimpleFiles::new();
    let file_id = files.add(file_name.to_string(), content.to_string());
    let mut labels: Vec<Label<usize>> = Vec::new();
    labels.push(Label::primary(file_id, cursor..(cursor + 1)).with_message("cursor"));
    for (r, msg) in ref_spans.iter() {
        labels.push(Label::secondary(file_id, r.clone()).with_message(msg.clone()));
    }
    let diag = Diagnostic::new(Severity::Help)
        .with_message("references at cursor")
        .with_labels(labels);
    let _ = emit(&mut out, &cfg, &files, &diag);
    String::from_utf8_lossy(out.as_slice()).into_owned()
}

/// Render a codespan-reporting snippet with primary carets for cursor, defs, and refs.
/// All markers are primary so the visual uses only carets (^) for consistency.
pub fn codespan_render_cursor_defs_refs(
    file_name: &str,
    content: &str,
    cursor: usize,
    defs: &[(Range<usize>, String)],
    refs: &[(Range<usize>, String)],
) -> String {
    use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
    use codespan_reporting::term::{emit, Config};
    use termcolor::Buffer;

    let mut out = Buffer::no_color();
    let cfg = Config::default();
    let mut files = codespan_reporting::files::SimpleFiles::new();
    let file_id = files.add(file_name.to_string(), content.to_string());
    let mut labels: Vec<Label<usize>> = Vec::new();
    labels.push(Label::primary(file_id, cursor..(cursor + 1)).with_message("cursor"));
    for (r, msg) in defs.iter() {
        labels.push(Label::primary(file_id, r.clone()).with_message(format!("def: {}", msg)));
    }
    for (r, msg) in refs.iter() {
        labels.push(Label::primary(file_id, r.clone()).with_message(format!("ref: {}", msg)));
    }
    let diag = Diagnostic::new(Severity::Help)
        .with_message("goto + references at cursor")
        .with_labels(labels);
    let _ = emit(&mut out, &cfg, &files, &diag);
    String::from_utf8_lossy(out.as_slice()).into_owned()
}

/// Render a codespan-reporting snippet with primary carets for defs and refs only (no cursor).
pub fn codespan_render_defs_refs(
    file_name: &str,
    content: &str,
    defs: &[(Range<usize>, String)],
    refs: &[(Range<usize>, String)],
) -> String {
    use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
    use codespan_reporting::term::{emit, Config};
    use termcolor::Buffer;

    let mut out = Buffer::no_color();
    let cfg = Config::default();
    let mut files = codespan_reporting::files::SimpleFiles::new();
    let file_id = files.add(file_name.to_string(), content.to_string());
    let mut labels: Vec<Label<usize>> = Vec::new();
    for (r, msg) in defs.iter() {
        labels.push(Label::primary(file_id, r.clone()).with_message(format!("def: {}", msg)));
    }
    for (r, msg) in refs.iter() {
        labels.push(Label::primary(file_id, r.clone()).with_message(format!("ref: {}", msg)));
    }
    let diag = Diagnostic::new(Severity::Help)
        .with_message("definitions + references")
        .with_labels(labels);
    let _ = emit(&mut out, &cfg, &files, &diag);
    String::from_utf8_lossy(out.as_slice()).into_owned()
}

pub fn pretty_enclosing(db: &dyn SpannedHirDb, top_mod: hir::hir_def::TopLevelMod, off: parser::TextSize) -> Option<String> {
    let items = top_mod.scope_graph(db).items_dfs(db);
    let mut best: Option<(hir::hir_def::ItemKind, u32)> = None;
    for it in items {
        let lazy = DynLazySpan::from(it.span());
        let Some(sp) = lazy.resolve(db) else { continue };
        if sp.range.contains(off) {
            let w: u32 = (sp.range.end() - sp.range.start()).into();
            match best {
                None => best=Some((it,w)),
                Some((_,bw)) if w< bw => best=Some((it,w)),
                _=>{}
            }
        }
    }
    best.and_then(|(it,_)| hir::hir_def::scope_graph::ScopeId::from_item(it).pretty_path(db))
}
