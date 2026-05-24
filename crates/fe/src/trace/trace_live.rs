use camino::Utf8PathBuf;
use trace_query::{TraceQueryHttpRequest, TraceQueryRequest};
use url::Url;

use crate::{DevTraceLiveCommand, TraceReportFormat};

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
    if command_format(command) == TraceReportFormat::Json {
        let typed = serde_json::from_str::<trace_query::TraceQueryHttpResponse>(&response)
            .map_err(|err| format!("live trace endpoint returned non-query JSON: {err}"))?;
        return serde_json::to_string_pretty(&typed)
            .map(|mut json| {
                json.push('\n');
                json
            })
            .map_err(|err| format!("failed to render live trace JSON: {err}"));
    }
    let response = serde_json::from_str::<trace_query::TraceQueryHttpResponse>(&response)
        .ok()
        .and_then(|typed| serde_json::to_string_pretty(&typed).ok())
        .unwrap_or(response);
    let display_request = redacted_request_json(request_json.clone());
    Ok(format!(
        "Fe dev trace live\n\nEndpoint: {query_url}\nRequest: {}\nResponse: {response}\n",
        serde_json::to_string_pretty(&display_request)
            .unwrap_or_else(|_| display_request.to_string())
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
    match crate::doc::ExistingInstanceCheck::inspect(root.as_std_path()) {
        crate::doc::ExistingInstanceCheck::None => {
            return Err(format!(
                "no .fe-lsp.json found at {root}; start fe lsp first"
            ));
        }
        crate::doc::ExistingInstanceCheck::Malformed => {
            return Err(format!("malformed .fe-lsp.json at {root}; restart fe lsp"));
        }
        crate::doc::ExistingInstanceCheck::StaleFound {
            stale_pid,
            recorded_workspace_root,
        } => {
            return Err(format!(
                "stale .fe-lsp.json at {root}: pid {stale_pid} is not alive, recorded workspace_root={recorded_workspace_root:?}"
            ));
        }
        crate::doc::ExistingInstanceCheck::RootMismatch {
            other_pid,
            other_workspace_root,
            our_workspace_root,
        } => {
            return Err(format!(
                "refusing live trace query for mismatched .fe-lsp.json: pid {other_pid}, recorded={other_workspace_root:?}, detected={our_workspace_root}"
            ));
        }
        crate::doc::ExistingInstanceCheck::SiblingLive { .. } => {}
    }
    let info = crate::doc::LspServerInfo::read_from_workspace(root.as_std_path())
        .ok_or_else(|| format!("no .fe-lsp.json found at {root}; start fe lsp first"))?;
    Ok(LiveLspDiscovery { root, info })
}

fn command_uri(command: &DevTraceLiveCommand) -> Option<&String> {
    match command {
        DevTraceLiveCommand::LoopCost { uri, .. }
        | DevTraceLiveCommand::GasBreakdown { uri, .. }
        | DevTraceLiveCommand::ExplainLocal { uri, .. } => uri.as_ref(),
    }
}

fn command_format(command: &DevTraceLiveCommand) -> TraceReportFormat {
    match command {
        DevTraceLiveCommand::LoopCost { format, .. }
        | DevTraceLiveCommand::GasBreakdown { format, .. }
        | DevTraceLiveCommand::ExplainLocal { format, .. } => *format,
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
        .and_then(|token| {
            if token.is_empty() {
                Err(format!("LSP auth token at {token_path} is empty"))
            } else {
                Ok(token)
            }
        })
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
    let mut queue = std::collections::VecDeque::from([root.clone()]);
    while let Some(dir) = queue.pop_front() {
        let mut entries = std::fs::read_dir(dir.as_std_path())
            .ok()?
            .filter_map(|entry| entry.ok())
            .filter_map(|entry| Utf8PathBuf::from_path_buf(entry.path()).ok())
            .collect::<Vec<_>>();
        entries.sort();
        for path in entries {
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
                queue.push_back(path);
            }
        }
    }
    None
}

fn redacted_request_json(mut request: serde_json::Value) -> serde_json::Value {
    if let Some(object) = request.as_object_mut()
        && object.contains_key("auth_token")
    {
        object.insert(
            "auth_token".to_string(),
            serde_json::Value::String("<redacted>".to_string()),
        );
    }
    request
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_fe_file_is_deterministic_and_skips_generated_dirs() {
        let temp = tempfile::tempdir().unwrap();
        let root = Utf8PathBuf::from_path_buf(temp.path().to_path_buf()).unwrap();
        std::fs::create_dir(root.join("b").as_std_path()).unwrap();
        std::fs::create_dir(root.join("a").as_std_path()).unwrap();
        std::fs::create_dir(root.join("target").as_std_path()).unwrap();
        std::fs::write(root.join("b").join("z.fe").as_std_path(), "").unwrap();
        std::fs::write(root.join("a").join("x.fe").as_std_path(), "").unwrap();
        std::fs::write(root.join("target").join("ignored.fe").as_std_path(), "").unwrap();

        assert_eq!(first_fe_file(&root), Some(root.join("a").join("x.fe")));
    }

    #[test]
    fn live_request_display_redacts_auth_token() {
        let request = serde_json::json!({
            "auth_token": "secret",
            "uri": "file:///tmp/fib.fe",
            "kind": "loop_cost"
        });

        let redacted = redacted_request_json(request);
        assert_eq!(redacted["auth_token"], "<redacted>");
        assert!(!redacted.to_string().contains("secret"));
    }
}
