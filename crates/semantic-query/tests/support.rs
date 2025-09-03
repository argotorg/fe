use common::InputDb;
use parser::SyntaxNode;

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

pub fn to_lsp_location_from_span(db: &dyn InputDb, span: common::diagnostics::Span) -> Option<async_lsp::lsp_types::Location> {
    let url = span.file.url(db)?;
    let text = span.file.text(db);
    let starts: Vec<usize> = text.lines().scan(0, |st, ln| { let o=*st; *st+=ln.len()+1; Some(o) }).collect();
    let idx = |off: parser::TextSize| starts.binary_search(&Into::<usize>::into(off)).unwrap_or_else(|n| n.saturating_sub(1));
    let sl = idx(span.range.start()); let el = idx(span.range.end());
    let sc: usize = Into::<usize>::into(span.range.start()) - starts[sl];
    let ec: usize = Into::<usize>::into(span.range.end()) - starts[el];
    Some(async_lsp::lsp_types::Location{ uri:url, range: async_lsp::lsp_types::Range{
        start: async_lsp::lsp_types::Position::new(sl as u32, sc as u32),
        end: async_lsp::lsp_types::Position::new(el as u32, ec as u32)
    }})
}

