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
fn func_to_hierarchy_item(db: &driver::DriverDataBase, func: Func) -> Option<CallHierarchyItem> {
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
    let url = backend.map_client_uri_to_internal(
        params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );
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

    let item = func_to_hierarchy_item(&backend.db, func).map(|mut item| {
        item.uri = backend.map_internal_uri_to_client(item.uri);
        item
    });
    Ok(item.map(|i| vec![i]))
}

/// Handle callHierarchy/incomingCalls.
pub async fn handle_incoming_calls(
    backend: &Backend,
    params: CallHierarchyIncomingCallsParams,
) -> Result<Option<Vec<CallHierarchyIncomingCall>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(params.item.uri.clone());
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.item.selection_range.start, file_text.as_str());

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

    let callers = find_incoming_calls(&backend.db, target_func, top_mod);

    let items: Vec<_> = callers
        .into_iter()
        .filter_map(|(caller, ranges)| {
            let item = func_to_hierarchy_item(&backend.db, caller)?;
            let mut item = item;
            item.uri = backend.map_internal_uri_to_client(item.uri);
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

/// Find all functions that call the given target function, with call site spans.
fn find_incoming_calls<'db>(
    db: &'db driver::DriverDataBase,
    target_func: Func<'db>,
    top_mod: hir::hir_def::TopLevelMod<'db>,
) -> Vec<(Func<'db>, Vec<async_lsp::lsp_types::Range>)> {
    let ingot = top_mod.ingot(db);
    let all_funcs = ingot.all_funcs(db);
    let mut callers: FxHashMap<Func, Vec<async_lsp::lsp_types::Range>> = FxHashMap::default();

    for &func in all_funcs {
        let Some(body) = func.body(db) else {
            continue;
        };
        for call_site in body.call_sites(db) {
            if let Some(CallableDef::Func(callee)) = call_site.target(db)
                && callee == target_func
                && let Ok(loc) = to_lsp_location_from_lazy_span(db, call_site.callee_span())
            {
                callers.entry(func).or_default().push(loc.range);
            }
        }
    }

    callers.into_iter().collect()
}

/// Find all functions called by the given source function, with call site spans.
fn find_outgoing_calls<'db>(
    db: &'db driver::DriverDataBase,
    source_func: Func<'db>,
) -> Vec<(Func<'db>, Vec<async_lsp::lsp_types::Range>)> {
    let Some(body) = source_func.body(db) else {
        return vec![];
    };

    let mut targets: FxHashMap<Func, Vec<async_lsp::lsp_types::Range>> = FxHashMap::default();

    for call_site in body.call_sites(db) {
        if let Some(CallableDef::Func(callee)) = call_site.target(db)
            && let Ok(loc) = to_lsp_location_from_lazy_span(db, call_site.callee_span())
        {
            targets.entry(callee).or_default().push(loc.range);
        }
    }

    targets.into_iter().collect()
}

/// Handle callHierarchy/outgoingCalls.
pub async fn handle_outgoing_calls(
    backend: &Backend,
    params: CallHierarchyOutgoingCallsParams,
) -> Result<Option<Vec<CallHierarchyOutgoingCall>>, ResponseError> {
    let url = backend.map_client_uri_to_internal(params.item.uri.clone());
    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.item.selection_range.start, file_text.as_str());

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

    let targets = find_outgoing_calls(&backend.db, source_func);

    let items: Vec<_> = targets
        .into_iter()
        .filter_map(|(callee, ranges)| {
            let item = func_to_hierarchy_item(&backend.db, callee)?;
            let mut item = item;
            item.uri = backend.map_internal_uri_to_client(item.uri);
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

#[cfg(test)]
mod tests {
    use super::*;
    use driver::DriverDataBase;
    use hir::lower::map_file_to_mod;
    use url::Url;

    fn find_func_at<'db>(
        db: &'db DriverDataBase,
        top_mod: hir::hir_def::TopLevelMod<'db>,
        offset: u32,
    ) -> Option<Func<'db>> {
        let cursor = parser::TextSize::from(offset);
        let resolution = top_mod.target_at(db, cursor);
        match resolution.first()? {
            Target::Scope(scope) => match scope.item() {
                ItemKind::Func(f) => Some(f),
                _ => None,
            },
            _ => None,
        }
    }

    #[test]
    fn test_call_hierarchy_prepare() {
        let mut db = DriverDataBase::default();
        let code = "fn foo() -> i32 {\n    1\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // "foo" starts at offset 3 (after "fn ")
        let func = find_func_at(&db, top_mod, 3);
        assert!(func.is_some(), "should find function at cursor");

        let item = func_to_hierarchy_item(&db, func.unwrap());
        assert!(item.is_some());
        let item = item.unwrap();
        assert_eq!(item.name, "foo");
        assert_eq!(item.kind, SymbolKind::FUNCTION);
    }

    #[test]
    fn test_incoming_calls() {
        let mut db = DriverDataBase::default();
        let code = r#"fn target() -> i32 {
    42
}

fn caller_a() -> i32 {
    target()
}

fn caller_b() -> i32 {
    target()
}

fn no_call() -> i32 {
    1
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        // "target" starts at offset 3 (after "fn ")
        let target_func = find_func_at(&db, top_mod, 3).expect("should find target function");
        let callers = find_incoming_calls(&db, target_func, top_mod);

        let mut caller_names: Vec<String> = callers
            .iter()
            .filter_map(|(f, _)| Some(f.name(&db).to_opt()?.data(&db).to_string()))
            .collect();
        caller_names.sort();

        assert_eq!(caller_names.len(), 2, "should have 2 callers");
        assert_eq!(caller_names, vec!["caller_a", "caller_b"]);
    }

    #[test]
    fn test_outgoing_calls() {
        let mut db = DriverDataBase::default();
        let code = r#"fn helper_a() -> i32 {
    1
}

fn helper_b() -> i32 {
    2
}

fn main_func() -> i32 {
    helper_a()
    helper_b()
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);

        let offset = code.find("main_func").unwrap() as u32;
        let main_func = find_func_at(&db, top_mod, offset).expect("should find main_func");
        let targets = find_outgoing_calls(&db, main_func);

        let mut target_names: Vec<String> = targets
            .iter()
            .filter_map(|(f, _)| Some(f.name(&db).to_opt()?.data(&db).to_string()))
            .collect();
        target_names.sort();

        assert_eq!(target_names.len(), 2, "should have 2 outgoing calls");
        assert_eq!(target_names, vec!["helper_a", "helper_b"]);
    }

    #[test]
    fn test_no_incoming_calls() {
        let mut db = DriverDataBase::default();
        let code = "fn lonely() -> i32 {\n    1\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let func = find_func_at(&db, top_mod, 3).expect("should find function");
        let callers = find_incoming_calls(&db, func, top_mod);
        assert!(callers.is_empty(), "lonely function should have no callers");
    }

    #[test]
    fn test_no_outgoing_calls() {
        let mut db = DriverDataBase::default();
        let code = "fn leaf() -> i32 {\n    42\n}\n";
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let func = find_func_at(&db, top_mod, 3).expect("should find function");
        let targets = find_outgoing_calls(&db, func);
        assert!(
            targets.is_empty(),
            "leaf function should have no outgoing calls"
        );
    }
}
