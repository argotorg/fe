use std::ops::Range;

pub fn line_col_from_cursor(cursor: parser::TextSize, s: &str) -> (usize, usize) {
    let mut line = 0usize;
    let mut col = 0usize;
    for (i, ch) in s.chars().enumerate() {
        if i == Into::<usize>::into(cursor) {
            return (line, col);
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    (line, col)
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
