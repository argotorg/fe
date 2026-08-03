use crate::{backend::Backend, util::to_offset_from_position};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    Documentation, ParameterInformation, ParameterLabel, SignatureHelp, SignatureHelpParams,
    SignatureInformation,
};
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    core::semantic::{
        CallSiteKind, CallSiteView, LogicalCallableParam, LogicalCallableParamMode,
        LogicalCallableSignature, LogicalCallableTarget,
    },
    hir_def::{Expr, ItemKind, Partial, TopLevelMod},
    lower::map_file_to_mod,
    span::{LazySpan, expr::LazyCallArgListSpan},
};

pub async fn handle_signature_help(
    backend: &Backend,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>, ResponseError> {
    let url = backend.map_client_uri_to_internal(
        params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );

    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found: {url}"),
            )
        })?;

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.text_document_position_params.position, file_text);
    let top_mod = map_file_to_mod(&backend.db, file);

    Ok(find_signature_at_cursor(
        &backend.db,
        top_mod,
        cursor,
        file_text,
    ))
}

/// Find the innermost argument list at the cursor and render the exact
/// callable selected by type checking.
fn find_signature_at_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: parser::TextSize,
    file_text: &str,
) -> Option<SignatureHelp> {
    let mut best: Option<(
        parser::TextSize,
        CallSiteView<'db>,
        LazyCallArgListSpan<'db>,
        parser::TextRange,
        usize,
    )> = None;

    for item in top_mod.scope_graph(db).items_dfs(db) {
        let ItemKind::Body(body) = item else {
            continue;
        };
        for site in body.call_sites(db) {
            let (args, argument_count) = match site.expr_id.data(db, body) {
                Partial::Present(Expr::Call(_, arguments)) => (
                    site.expr_id.span(body).into_call_expr().args(),
                    arguments.len(),
                ),
                Partial::Present(Expr::MethodCall(_, _, _, arguments)) => (
                    site.expr_id.span(body).into_method_call_expr().args(),
                    arguments.len(),
                ),
                _ => continue,
            };
            let Some(span) = args.resolve(db) else {
                continue;
            };
            if !span.range.contains(cursor) {
                continue;
            }
            let size = span.range.len();
            if best
                .as_ref()
                .is_none_or(|(best_size, ..)| size < *best_size)
            {
                best = Some((size, site, args, span.range, argument_count));
            }
        }
    }

    let (_, site, args_span, args_range, argument_count) = best?;
    let signature = LogicalCallableSignature::for_call_site(db, &site)?;
    let active_parameter = compute_active_parameter(
        db,
        &args_span,
        argument_count,
        cursor,
        args_range,
        file_text,
        signature.params.len(),
    );
    Some(build_signature_help(
        db,
        &site,
        signature,
        active_parameter,
        file_text,
    ))
}

/// Select the resolved outer argument containing the cursor. Commas inside an
/// argument expression, including a closure parameter list, cannot affect the
/// result. The text scan is retained only for incomplete syntax whose argument
/// spans cannot all be resolved.
fn compute_active_parameter(
    db: &DriverDataBase,
    args_span: &LazyCallArgListSpan<'_>,
    argument_count: usize,
    cursor: parser::TextSize,
    args_range: parser::TextRange,
    file_text: &str,
    parameter_count: usize,
) -> Option<u32> {
    if parameter_count == 0 {
        return None;
    }
    let max_parameter = parameter_count.saturating_sub(1);
    let argument_ranges = (0..argument_count)
        .map(|idx| {
            args_span
                .clone()
                .arg(idx)
                .resolve(db)
                .map(|span| span.range)
        })
        .collect::<Option<Vec<_>>>();
    if let Some(argument_ranges) = argument_ranges {
        if argument_ranges.is_empty() {
            return Some(0);
        }
        for (idx, range) in argument_ranges.iter().enumerate() {
            if cursor < range.start() {
                return Some(idx.min(max_parameter) as u32);
            }
            if cursor <= range.end() {
                return Some(idx.min(max_parameter) as u32);
            }
            let next_start = argument_ranges
                .get(idx + 1)
                .map_or(args_range.end(), |range| range.start());
            if cursor < next_start || idx + 1 == argument_ranges.len() {
                let gap_start = usize::from(range.end());
                let gap_end = usize::from(cursor.min(args_range.end()));
                let after_separator = file_text
                    .get(gap_start..gap_end)
                    .is_some_and(|gap| gap.contains(','));
                return Some((idx + usize::from(after_separator)).min(max_parameter) as u32);
            }
        }
    }

    let start = usize::from(args_range.start());
    let end = usize::from(args_range.end());
    let cursor = usize::from(cursor).min(end);
    let args_text = file_text.get(start..cursor)?;
    let open = args_text.find('(')?;

    let mut commas = 0_u32;
    let mut delimiters = Vec::new();
    let mut chars = args_text[open + 1..].chars().peekable();
    let mut quote = None;
    let mut escaped = false;
    let mut line_comment = false;
    let mut block_comment_depth = 0_u32;

    while let Some(ch) = chars.next() {
        if line_comment {
            if ch == '\n' {
                line_comment = false;
            }
            continue;
        }
        if block_comment_depth > 0 {
            if ch == '/' && chars.peek() == Some(&'*') {
                chars.next();
                block_comment_depth += 1;
            } else if ch == '*' && chars.peek() == Some(&'/') {
                chars.next();
                block_comment_depth -= 1;
            }
            continue;
        }
        if let Some(expected_quote) = quote {
            if escaped {
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
            } else if ch == expected_quote {
                quote = None;
            }
            continue;
        }

        match ch {
            '"' | '\'' => quote = Some(ch),
            '/' if chars.peek() == Some(&'/') => {
                chars.next();
                line_comment = true;
            }
            '/' if chars.peek() == Some(&'*') => {
                chars.next();
                block_comment_depth = 1;
            }
            '(' | '{' | '[' => delimiters.push(ch),
            ')' => {
                if delimiters.last() == Some(&'(') {
                    delimiters.pop();
                }
            }
            '}' => {
                if delimiters.last() == Some(&'{') {
                    delimiters.pop();
                }
            }
            ']' => {
                if delimiters.last() == Some(&'[') {
                    delimiters.pop();
                }
            }
            ',' if delimiters.is_empty() => commas += 1,
            _ => {}
        }
    }

    Some(commas.min(max_parameter as u32))
}

fn build_signature_help<'db>(
    db: &'db DriverDataBase,
    site: &CallSiteView<'db>,
    signature: LogicalCallableSignature<'db>,
    active_parameter: Option<u32>,
    file_text: &str,
) -> SignatureHelp {
    let name = call_site_name(db, site, file_text);
    let params = signature
        .params
        .iter()
        .map(|param| render_param(db, param))
        .collect::<Vec<_>>();
    let parameters = params
        .iter()
        .map(|label| ParameterInformation {
            label: ParameterLabel::Simple(label.clone()),
            documentation: Some(Documentation::String(label.clone())),
        })
        .collect::<Vec<_>>();
    let ret = signature.ret_ty.pretty_print(db);
    let ret = if ret == "()" {
        String::new()
    } else {
        format!(" -> {ret}")
    };
    let capability = signature
        .capability
        .map(|capability| format!(" [{}]", capability.as_str()))
        .unwrap_or_default();
    let prefix = match signature.target {
        LogicalCallableTarget::Definition(_) if signature.capability.is_none() => "fn ",
        LogicalCallableTarget::Definition(_) | LogicalCallableTarget::Closure(_) => "",
    };
    let label = format!("{prefix}{name}({}){ret}{capability}", params.join(", "));
    let information = SignatureInformation {
        label,
        documentation: None,
        parameters: Some(parameters),
        active_parameter,
    };
    SignatureHelp {
        signatures: vec![information],
        active_signature: Some(0),
        active_parameter,
    }
}

fn render_param(db: &DriverDataBase, param: &LogicalCallableParam<'_>) -> String {
    let name = param
        .name
        .map(|name| name.data(db).to_string())
        .unwrap_or_else(|| "_".to_string());
    let mode = match param.mode {
        LogicalCallableParamMode::View => "",
        LogicalCallableParamMode::Own => "own ",
        LogicalCallableParamMode::Ref => "ref ",
        LogicalCallableParamMode::Mut => "mut ",
    };
    format!("{name}: {mode}{}", param.ty.pretty_print(db))
}

fn call_site_name(db: &DriverDataBase, site: &CallSiteView<'_>, file_text: &str) -> String {
    match site.kind {
        CallSiteKind::MethodCall { method_name } => method_name
            .to_opt()
            .map(|name| name.data(db).to_string())
            .unwrap_or_else(|| "<method>".to_string()),
        CallSiteKind::FnCall => {
            let Partial::Present(Expr::Call(callee, _)) = site.expr_id.data(db, site.body) else {
                return "<callable>".to_string();
            };
            match callee.data(db, site.body) {
                Partial::Present(Expr::Path(Partial::Present(path))) => path
                    .ident(db)
                    .to_opt()
                    .map(|name| name.data(db).to_string())
                    .unwrap_or_else(|| source_name(db, site.body, *callee, file_text)),
                Partial::Present(Expr::Closure { .. }) => "<closure>".to_string(),
                _ => source_name(db, site.body, *callee, file_text),
            }
        }
    }
}

fn source_name(
    db: &DriverDataBase,
    body: hir::hir_def::Body<'_>,
    expr: hir::hir_def::ExprId,
    file_text: &str,
) -> String {
    expr.span(body)
        .resolve(db)
        .and_then(|span| {
            file_text.get(usize::from(span.range.start())..usize::from(span.range.end()))
        })
        .map(str::trim)
        .filter(|source| !source.is_empty() && !source.contains('\n') && source.len() <= 80)
        .unwrap_or("<callable>")
        .to_string()
}

#[cfg(test)]
mod tests {
    use async_lsp::lsp_types::SignatureHelp;
    use common::InputDb;
    use dir_test::{Fixture, dir_test};
    use test_utils::{normalize::normalize_newlines, snap_test};
    use url::Url;

    use super::*;

    fn extract_markers(source: &str) -> (String, Vec<(String, usize)>) {
        let mut cleaned = String::new();
        let mut markers = Vec::new();
        let mut remaining = source;
        while let Some(start) = remaining.find("<|") {
            cleaned.push_str(&remaining[..start]);
            let marker = &remaining[start + 2..];
            let end = marker.find("|>").expect("unterminated signature marker");
            let label = &marker[..end];
            markers.push((label.to_string(), cleaned.len()));
            remaining = &marker[end + 2..];
        }
        cleaned.push_str(remaining);
        (cleaned, markers)
    }

    fn render(help: SignatureHelp) -> String {
        let signature = &help.signatures[0];
        format!("{}\nactive: {:?}\n", signature.label, help.active_parameter)
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "signature_help.fe"
    )]
    fn callable_signature_help_snapshot(fixture: Fixture<&str>) {
        let source = normalize_newlines(fixture.content()).into_owned();
        let (source, markers) = extract_markers(&source);
        let uri = Url::from_file_path(fixture.path()).unwrap();
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(&mut db, uri, Some(source.clone()));
        let top_mod = map_file_to_mod(&db, file);
        let mut snapshot = String::new();
        for (label, offset) in markers {
            let cursor = parser::TextSize::from(offset as u32);
            let help = find_signature_at_cursor(&db, top_mod, cursor, &source)
                .unwrap_or_else(|| panic!("missing signature help for `{label}`"));
            snapshot.push_str(&format!("## {label}\n{}\n", render(help)));
        }
        snap_test!(snapshot, fixture.path());
    }
}
