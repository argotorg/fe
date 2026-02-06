use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    SymbolKind,
};
use common::InputDb;
use hir::{
    core::semantic::reference::Target,
    hir_def::{CallableDef, Func, HirIngot, ItemKind},
    lower::map_file_to_mod,
};
use rustc_hash::FxHashMap;

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_offset_from_position},
};

/// Build a `CallHierarchyItem` from a function.
fn func_to_hierarchy_item(
    db: &driver::DriverDataBase,
    func: Func,
) -> Option<CallHierarchyItem> {
    let location = to_lsp_location_from_scope(db, func.scope()).ok()?;
    let name = func.name(db).to_opt()?.data(db).to_string();

    let kind = if func.is_method(db) {
        SymbolKind::METHOD
    } else {
        SymbolKind::FUNCTION
    };

    // Use the full item span for range
    let item_span: hir::span::DynLazySpan = func.span().into();
    let item_location = to_lsp_location_from_lazy_span(db, item_span).ok()?;

    Some(CallHierarchyItem {
        name,
        kind,
        tags: None,
        detail: None,
        uri: location.uri,
        range: item_location.range,
        selection_range: location.range,
        data: None,
    })
}

/// Handle textDocument/prepareCallHierarchy.
pub async fn handle_prepare(
    backend: &Backend,
    params: CallHierarchyPrepareParams,
) -> Result<Option<Vec<CallHierarchyItem>>, ResponseError> {
    let path_str = params
        .text_document_position_params
        .text_document
        .uri
        .path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let func = match target {
        Target::Scope(scope) => match scope.item() {
            ItemKind::Func(f) => f,
            _ => return Ok(None),
        },
        _ => return Ok(None),
    };

    let item = func_to_hierarchy_item(&backend.db, func);
    Ok(item.map(|i| vec![i]))
}

/// Handle callHierarchy/incomingCalls.
pub async fn handle_incoming_calls(
    backend: &Backend,
    params: CallHierarchyIncomingCallsParams,
) -> Result<Option<Vec<CallHierarchyIncomingCall>>, ResponseError> {
    let path_str = params.item.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(
        params.item.selection_range.start,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let target_func = match target {
        Target::Scope(scope) => match scope.item() {
            ItemKind::Func(f) => f,
            _ => return Ok(None),
        },
        _ => return Ok(None),
    };

    let ingot = top_mod.ingot(&backend.db);
    let all_funcs = ingot.all_funcs(&backend.db);

    // Map from calling function -> Vec<Range> of call sites
    let mut callers: FxHashMap<Func, Vec<async_lsp::lsp_types::Range>> = FxHashMap::default();

    for &func in all_funcs {
        let Some(body) = func.body(&backend.db) else {
            continue;
        };
        for call_site in body.call_sites(&backend.db) {
            if let Some(CallableDef::Func(callee)) = call_site.target(&backend.db) {
                if callee == target_func {
                    if let Ok(loc) =
                        to_lsp_location_from_lazy_span(&backend.db, call_site.callee_span())
                    {
                        callers.entry(func).or_default().push(loc.range);
                    }
                }
            }
        }
    }

    let items: Vec<_> = callers
        .into_iter()
        .filter_map(|(caller, ranges)| {
            let item = func_to_hierarchy_item(&backend.db, caller)?;
            Some(CallHierarchyIncomingCall {
                from: item,
                from_ranges: ranges,
            })
        })
        .collect();

    if items.is_empty() {
        Ok(None)
    } else {
        Ok(Some(items))
    }
}

/// Handle callHierarchy/outgoingCalls.
pub async fn handle_outgoing_calls(
    backend: &Backend,
    params: CallHierarchyOutgoingCallsParams,
) -> Result<Option<Vec<CallHierarchyOutgoingCall>>, ResponseError> {
    let path_str = params.item.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(
        params.item.selection_range.start,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(&backend.db, file);
    let resolution = top_mod.target_at(&backend.db, cursor);
    let Some(target) = resolution.first() else {
        return Ok(None);
    };

    let source_func = match target {
        Target::Scope(scope) => match scope.item() {
            ItemKind::Func(f) => f,
            _ => return Ok(None),
        },
        _ => return Ok(None),
    };

    let Some(body) = source_func.body(&backend.db) else {
        return Ok(None);
    };

    // Group call sites by target function
    let mut targets: FxHashMap<Func, Vec<async_lsp::lsp_types::Range>> = FxHashMap::default();

    for call_site in body.call_sites(&backend.db) {
        if let Some(CallableDef::Func(callee)) = call_site.target(&backend.db) {
            if let Ok(loc) =
                to_lsp_location_from_lazy_span(&backend.db, call_site.callee_span())
            {
                targets.entry(callee).or_default().push(loc.range);
            }
        }
    }

    let items: Vec<_> = targets
        .into_iter()
        .filter_map(|(callee, ranges)| {
            let item = func_to_hierarchy_item(&backend.db, callee)?;
            Some(CallHierarchyOutgoingCall {
                to: item,
                from_ranges: ranges,
            })
        })
        .collect();

    if items.is_empty() {
        Ok(None)
    } else {
        Ok(Some(items))
    }
}
