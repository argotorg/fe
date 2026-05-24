use camino::Utf8PathBuf;
use trace_query::{TraceQueryHttpRequest, TraceQueryRequest};
use url::Url;

use crate::DevTraceLiveCommand;

pub(super) fn run_trace_live_command(command: &DevTraceLiveCommand) -> Result<String, String> {
    let discovery = live_lsp_discovery()?;
    let http = discovery
        .info
        .http
        .as_ref()
        .ok_or_else(|| "discovered .fe-lsp.json has no HTTP endpoint".to_string())?;
    let query_url = format!("{}/query", http.trace_api_url.trim_end_matches('/'));
    let uri = live_query_uri(&discovery.root, command_uri(command))?;
    let auth_token = live_auth_token(&discovery)?;
    let request = TraceQueryHttpRequest {
        auth_token,
        uri,
        config_hash: discovery.info.config_hash.clone(),
        query: match command {
            DevTraceLiveCommand::LoopCost { .. } => TraceQueryRequest::loop_cost(),
            DevTraceLiveCommand::ExplainLocal { local, .. } => {
                TraceQueryRequest::explain_local(local)
            }
            DevTraceLiveCommand::GasBreakdown { .. } => TraceQueryRequest::gas_breakdown("cancun"),
        },
    };
    let request_json = serde_json::to_value(&request)
        .map_err(|err| format!("failed to serialize live trace request: {err}"))?;
    let response = crate::http_post_json(&query_url, &request_json)?;
    let response = serde_json::from_str::<trace_query::TraceQueryHttpResponse>(&response)
        .ok()
        .and_then(|typed| serde_json::to_string_pretty(&typed).ok())
        .unwrap_or(response);
    Ok(format!(
        "Fe dev trace live\n\nEndpoint: {query_url}\nRequest: {}\nResponse: {response}\n",
        serde_json::to_string_pretty(&request_json).unwrap_or_else(|_| request_json.to_string())
    ))
}

struct LiveLspDiscovery {
    root: Utf8PathBuf,
    info: crate::doc::LspServerInfo,
}

fn live_lsp_discovery() -> Result<LiveLspDiscovery, String> {
    let root = driver::files::find_project_root()
        .or_else(|| {
            std::env::current_dir()
                .ok()
                .and_then(|path| Utf8PathBuf::from_path_buf(path).ok())
        })
        .ok_or_else(|| {
            "could not determine workspace root for .fe-lsp.json discovery".to_string()
        })?;
    let info = crate::doc::LspServerInfo::read_from_workspace(root.as_std_path())
        .ok_or_else(|| format!("no .fe-lsp.json found at {root}; start fe lsp first"))?;
    Ok(LiveLspDiscovery { root, info })
}

fn command_uri(command: &DevTraceLiveCommand) -> Option<&String> {
    match command {
        DevTraceLiveCommand::LoopCost { uri }
        | DevTraceLiveCommand::GasBreakdown { uri }
        | DevTraceLiveCommand::ExplainLocal { uri, .. } => uri.as_ref(),
    }
}

fn live_auth_token(discovery: &LiveLspDiscovery) -> Result<String, String> {
    let auth = discovery
        .info
        .auth
        .as_ref()
        .ok_or_else(|| "discovered .fe-lsp.json has no auth metadata".to_string())?;
    if auth.mode != "localhost-token" {
        return Err(format!("unsupported LSP auth mode: {}", auth.mode));
    }
    let token_path = discovery.root.join(&auth.token_file);
    std::fs::read_to_string(token_path.as_std_path())
        .map(|token| token.trim().to_string())
        .map_err(|err| format!("failed to read LSP auth token at {token_path}: {err}"))
}

fn live_query_uri(root: &Utf8PathBuf, requested: Option<&String>) -> Result<String, String> {
    if let Some(requested) = requested {
        return path_or_uri_to_uri(root, requested);
    }
    let Some(path) = first_fe_file(root) else {
        return Err("live trace query needs --uri because no .fe file was found".to_string());
    };
    path_or_uri_to_uri(root, path.as_str())
}

fn path_or_uri_to_uri(root: &Utf8PathBuf, value: &str) -> Result<String, String> {
    if Url::parse(value).is_ok() {
        return Ok(value.to_string());
    }
    let mut path = Utf8PathBuf::from(value);
    if path.is_relative() {
        path = root.join(path);
    }
    Url::from_file_path(path.as_std_path())
        .map(|url| url.to_string())
        .map_err(|()| format!("trace query path cannot be converted to file URI: {path}"))
}

fn first_fe_file(root: &Utf8PathBuf) -> Option<Utf8PathBuf> {
    let mut stack = vec![root.clone()];
    while let Some(dir) = stack.pop() {
        let entries = std::fs::read_dir(dir.as_std_path()).ok()?;
        for entry in entries.flatten() {
            let path = Utf8PathBuf::from_path_buf(entry.path()).ok()?;
            if path
                .file_name()
                .is_some_and(|name| matches!(name, "target" | ".git" | ".jj" | "node_modules"))
            {
                continue;
            }
            if path.is_file() && path.extension() == Some("fe") {
                return Some(path);
            }
            if path.is_dir() {
                stack.push(path);
            }
        }
    }
    None
}
