use hir::{span::{DynLazySpan, LazySpan}, SpannedHirDb};
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
    let header = content.lines().enumerate().map(|(i,l)| format!(/*"{i:?}": "{l}"*/ "{i}: {l}")).collect::<Vec<_>>().join("\n");
    let body = lines.join("\n");
    format!("{header}\n---\n{body}")
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