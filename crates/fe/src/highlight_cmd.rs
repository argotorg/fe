//! `fe highlight` — syntax-highlight Fe source to HTML.

use std::io::{self, BufRead, Read, Write};

use camino::Utf8PathBuf;
use fe_web::highlight;

pub fn run_highlight(
    file: Option<&Utf8PathBuf>,
    standalone: bool,
    batch: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    if batch {
        return run_batch();
    }

    let code = match file {
        Some(path) => std::fs::read_to_string(path.as_std_path())?,
        None => {
            let mut buf = String::new();
            io::stdin().lock().read_to_string(&mut buf)?;
            buf
        }
    };

    let inner = highlight::highlight_fe_block(&code);

    if standalone {
        print!(
            "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\">\
             <style>{css}</style>\
             <script>{js}</script>\
             </head><body>{inner}</body></html>",
            css = fe_web::assets::FE_HIGHLIGHT_CSS,
            js = fe_web::assets::FE_CODE_BLOCK_JS,
            inner = inner,
        );
    } else {
        print!("{inner}");
    }
    Ok(())
}

/// Batch mode: read newline-delimited JSON from stdin, output JSON per line.
///
/// Input:  `{"id":"1","code":"struct Foo {}"}`
/// Output: `{"id":"1","html":"<pre>..."}`
fn run_batch() -> Result<(), Box<dyn std::error::Error>> {
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
        let html = highlight::highlight_fe_block(code);

        let result = serde_json::json!({ "id": id, "html": html });
        serde_json::to_writer(&mut out, &result)?;
        writeln!(out)?;
    }

    out.flush()?;
    Ok(())
}
