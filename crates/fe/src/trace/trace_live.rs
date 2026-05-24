use camino::Utf8PathBuf;

use crate::DevTraceLiveCommand;

pub(super) fn run_trace_live_command(command: &DevTraceLiveCommand) -> Result<String, String> {
    let info = live_lsp_info()?;
    let http = info
        .http
        .as_ref()
        .ok_or_else(|| "discovered .fe-lsp.json has no HTTP endpoint".to_string())?;
    let query_url = format!("{}/query", http.trace_api_url.trim_end_matches('/'));
    let request = match command {
        DevTraceLiveCommand::LoopCost => serde_json::json!({ "kind": "loop_cost" }),
        DevTraceLiveCommand::ExplainLocal { local } => {
            serde_json::json!({ "kind": "explain_local", "local": local })
        }
        DevTraceLiveCommand::GasBreakdown => serde_json::json!({ "kind": "gas_breakdown" }),
    };
    let response = crate::http_post_json(&query_url, &request)?;
    Ok(format!(
        "Fe dev trace live\n\nEndpoint: {query_url}\nRequest: {}\nResponse: {response}\n",
        request
    ))
}

fn live_lsp_info() -> Result<crate::doc::LspServerInfo, String> {
    let root = driver::files::find_project_root()
        .or_else(|| {
            std::env::current_dir()
                .ok()
                .and_then(|path| Utf8PathBuf::from_path_buf(path).ok())
        })
        .ok_or_else(|| {
            "could not determine workspace root for .fe-lsp.json discovery".to_string()
        })?;
    crate::doc::LspServerInfo::read_from_workspace(root.as_std_path())
        .ok_or_else(|| format!("no .fe-lsp.json found at {root}; start fe lsp first"))
}
