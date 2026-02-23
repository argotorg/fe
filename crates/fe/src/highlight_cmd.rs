//! `fe highlight` — syntax-highlight Fe source to HTML.

use std::io::{self, BufRead, Read, Write};

use camino::Utf8PathBuf;
use fe_web::highlight;

pub fn run_highlight(
    file: Option<&Utf8PathBuf>,
    component: bool,
    batch: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if batch {
        return run_batch(component);
    }

    let code = match file {
        Some(path) => std::fs::read_to_string(path.as_std_path())?,
        None => {
            let mut buf = String::new();
            io::stdin().lock().read_to_string(&mut buf)?;
            buf
        }
    };

    let html = if component {
        format!(
            "<fe-code-block highlighted>{}</fe-code-block>",
            highlight::highlight_fe(&code)
        )
    } else {
        highlight::highlight_fe_block(&code)
    };

    print!("{html}");
    Ok(())
}

/// Batch mode: read newline-delimited JSON from stdin, output JSON per line.
///
/// Input:  `{"id":"1","code":"struct Foo {}"}`
/// Output: `{"id":"1","html":"<pre>..."}`
fn run_batch(component: bool) -> Result<(), Box<dyn std::error::Error>> {
    let stdin = io::stdin().lock();
    let stdout = io::stdout();
    let mut out = io::BufWriter::new(stdout.lock());

    for line in stdin.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }

        let entry: serde_json::Value = match serde_json::from_str(&line) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("warning: skipping malformed JSON line: {e}");
                continue;
            }
        };
        let id = entry["id"].as_str().unwrap_or("");
        let code = entry["code"].as_str().unwrap_or("");

        let html = if component {
            format!(
                "<fe-code-block highlighted>{}</fe-code-block>",
                highlight::highlight_fe(code)
            )
        } else {
            highlight::highlight_fe_block(code)
        };

        let result = serde_json::json!({ "id": id, "html": html });
        serde_json::to_writer(&mut out, &result)?;
        writeln!(out)?;
    }

    out.flush()?;
    Ok(())
}
