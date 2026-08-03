use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    SymbolKind,
};
use common::InputDb;
use hir::{
    analysis::ty::{ty_check::BodyOwner, ty_def::ClosureTy},
    core::semantic::{LogicalCallableSignature, LogicalCallableTarget, reference::Target},
    hir_def::{Body, CallableDef, Func, HirIngot, ItemKind},
    lower::map_file_to_mod,
    span::LazySpan,
};
use rustc_hash::FxHashMap;

use crate::{
    backend::Backend,
    util::{to_lsp_location_from_lazy_span, to_lsp_location_from_scope, to_offset_from_position},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum HierarchyTarget<'db> {
    Func(Func<'db>),
    Closure(ClosureTy<'db>),
    ContractBody(Body<'db>),
}

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

fn closure_to_hierarchy_item(
    db: &driver::DriverDataBase,
    closure: ClosureTy<'_>,
) -> Option<CallHierarchyItem> {
    let def = closure.def(db);
    let location = to_lsp_location_from_lazy_span(db, def.expr.span(def.body).into()).ok()?;
    let signature = LogicalCallableSignature::for_closure(db, closure);
    let detail = Some(format!(
        "{} {}",
        signature.capability?.as_str(),
        closure.pretty_print(db),
    ));
    Some(CallHierarchyItem {
        name: "<closure>".to_string(),
        kind: SymbolKind::FUNCTION,
        tags: None,
        detail,
        uri: location.uri,
        range: location.range,
        selection_range: location.range,
        data: None,
    })
}

fn contract_body_to_hierarchy_item(
    db: &driver::DriverDataBase,
    body: Body<'_>,
) -> Option<CallHierarchyItem> {
    let (contract, name, kind, detail, item_span, selection_span) =
        match BodyOwner::from_body(db, body)? {
            BodyOwner::ContractInit { contract } => (
                contract,
                "init".to_string(),
                SymbolKind::CONSTRUCTOR,
                "contract initializer".to_string(),
                contract.span().init_block().into(),
                contract.span().init_block().into(),
            ),
            BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            } => (
                contract,
                format!("recv[{recv_idx}:{arm_idx}]"),
                SymbolKind::METHOD,
                "contract receive arm".to_string(),
                contract
                    .span()
                    .recv(recv_idx as usize)
                    .arms()
                    .arm(arm_idx as usize)
                    .into(),
                contract
                    .span()
                    .recv(recv_idx as usize)
                    .arms()
                    .arm(arm_idx as usize)
                    .pat()
                    .into(),
            ),
            BodyOwner::Func(_)
            | BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::Closure { .. } => return None,
        };
    let contract_name = contract.name(db).to_opt()?.data(db);
    let item_location = to_lsp_location_from_lazy_span(db, item_span).ok()?;
    let selection_location = to_lsp_location_from_lazy_span(db, selection_span).ok()?;
    Some(CallHierarchyItem {
        name: format!("{contract_name}::{name}"),
        kind,
        tags: None,
        detail: Some(detail),
        uri: item_location.uri,
        range: item_location.range,
        selection_range: selection_location.range,
        data: None,
    })
}

fn target_to_hierarchy_item(
    db: &driver::DriverDataBase,
    target: HierarchyTarget<'_>,
) -> Option<CallHierarchyItem> {
    match target {
        HierarchyTarget::Func(func) => func_to_hierarchy_item(db, func),
        HierarchyTarget::Closure(closure) => closure_to_hierarchy_item(db, closure),
        HierarchyTarget::ContractBody(body) => contract_body_to_hierarchy_item(db, body),
    }
}

fn closure_at_cursor<'db>(
    db: &'db driver::DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    cursor: parser::TextSize,
) -> Option<ClosureTy<'db>> {
    let mut best = None;
    for item in top_mod.scope_graph(db).items_dfs(db) {
        let ItemKind::Body(body) = item else {
            continue;
        };
        let Some(typed_body) = body.typed_body(db) else {
            continue;
        };
        for (expr, info) in typed_body.closure_infos() {
            let Some(span) = expr.span(body).resolve(db) else {
                continue;
            };
            if span.range.contains(cursor)
                && best
                    .as_ref()
                    .is_none_or(|(size, _)| span.range.len() < *size)
            {
                best = Some((span.range.len(), info.ty));
            }
        }
    }
    best.map(|(_, closure)| closure)
}

fn contract_body_at_cursor<'db>(
    db: &'db driver::DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    cursor: parser::TextSize,
) -> Option<Body<'db>> {
    let mut best = None;
    for item in top_mod.scope_graph(db).items_dfs(db) {
        let ItemKind::Body(body) = item else {
            continue;
        };
        let item_span: hir::span::DynLazySpan<'db> = match BodyOwner::from_body(db, body) {
            Some(BodyOwner::ContractInit { contract }) => contract.span().init_block().into(),
            Some(BodyOwner::ContractRecvArm {
                contract,
                recv_idx,
                arm_idx,
            }) => contract
                .span()
                .recv(recv_idx as usize)
                .arms()
                .arm(arm_idx as usize)
                .into(),
            _ => continue,
        };
        let Some(span) = item_span.resolve(db) else {
            continue;
        };
        if span.range.contains(cursor)
            && best
                .as_ref()
                .is_none_or(|(size, _)| span.range.len() < *size)
        {
            best = Some((span.range.len(), body));
        }
    }
    best.map(|(_, body)| body)
}

fn hierarchy_target_at_cursor<'db>(
    db: &'db driver::DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'db>,
    cursor: parser::TextSize,
) -> Option<HierarchyTarget<'db>> {
    if let Some(target) = top_mod.target_at(db, cursor).first() {
        match target {
            Target::Scope(scope) => {
                if let ItemKind::Func(func) = scope.item() {
                    return Some(HierarchyTarget::Func(func));
                }
            }
            Target::Local { ty, .. } => {
                let payload = ty.as_capability(db).map_or(*ty, |(_, payload)| payload);
                if let Some(closure) = payload.as_closure(db) {
                    return Some(HierarchyTarget::Closure(closure));
                }
            }
        }
    }
    closure_at_cursor(db, top_mod, cursor)
        .map(HierarchyTarget::Closure)
        .or_else(|| contract_body_at_cursor(db, top_mod, cursor).map(HierarchyTarget::ContractBody))
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
    let Some(target) = hierarchy_target_at_cursor(&backend.db, top_mod, cursor) else {
        return Ok(None);
    };

    let item = target_to_hierarchy_item(&backend.db, target).map(|mut item| {
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
    let Some(target) = hierarchy_target_at_cursor(&backend.db, top_mod, cursor) else {
        return Ok(None);
    };

    let callers = find_incoming_calls(&backend.db, target, top_mod);

    let items: Vec<_> = callers
        .into_iter()
        .filter_map(|(caller, ranges)| {
            let item = target_to_hierarchy_item(&backend.db, caller)?;
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

fn find_incoming_calls<'db>(
    db: &'db driver::DriverDataBase,
    target: HierarchyTarget<'db>,
    top_mod: hir::hir_def::TopLevelMod<'db>,
) -> Vec<(HierarchyTarget<'db>, Vec<async_lsp::lsp_types::Range>)> {
    let ingot = top_mod.ingot(db);
    let mut callers: FxHashMap<HierarchyTarget, Vec<async_lsp::lsp_types::Range>> =
        FxHashMap::default();

    for &item in ingot.all_items(db) {
        let ItemKind::Body(body) = item else {
            continue;
        };
        for call_site in body.call_sites(db) {
            if logical_hierarchy_target(db, &call_site) == Some(target)
                && let Some(caller) = call_owner(db, body, call_site.expr_id)
                && let Ok(loc) = to_lsp_location_from_lazy_span(db, call_site.callee_span())
            {
                callers.entry(caller).or_default().push(loc.range);
            }
        }
    }

    callers.into_iter().collect()
}

/// Find all functions called by the given source function, with call site spans.
fn find_outgoing_calls<'db>(
    db: &'db driver::DriverDataBase,
    source: HierarchyTarget<'db>,
) -> Vec<(HierarchyTarget<'db>, Vec<async_lsp::lsp_types::Range>)> {
    let body = match source {
        HierarchyTarget::Func(func) => func.body(db),
        HierarchyTarget::Closure(closure) => Some(closure.def(db).body),
        HierarchyTarget::ContractBody(body) => Some(body),
    };
    let Some(body) = body else {
        return Vec::new();
    };

    let mut targets: FxHashMap<HierarchyTarget, Vec<async_lsp::lsp_types::Range>> =
        FxHashMap::default();

    for call_site in body.call_sites(db) {
        if call_owner(db, body, call_site.expr_id) == Some(source)
            && let Some(callee) = logical_hierarchy_target(db, &call_site)
            && let Ok(loc) = to_lsp_location_from_lazy_span(db, call_site.callee_span())
        {
            targets.entry(callee).or_default().push(loc.range);
        }
    }

    targets.into_iter().collect()
}

fn logical_hierarchy_target<'db>(
    db: &'db driver::DriverDataBase,
    call_site: &hir::core::semantic::CallSiteView<'db>,
) -> Option<HierarchyTarget<'db>> {
    match call_site.logical_target(db)? {
        LogicalCallableTarget::Definition(CallableDef::Func(func)) => {
            Some(HierarchyTarget::Func(func))
        }
        LogicalCallableTarget::Definition(CallableDef::VariantCtor(_)) => None,
        LogicalCallableTarget::Closure(closure) => Some(HierarchyTarget::Closure(closure)),
    }
}

fn call_owner<'db>(
    db: &'db driver::DriverDataBase,
    body: Body<'db>,
    call_expr: hir::hir_def::ExprId,
) -> Option<HierarchyTarget<'db>> {
    let call_span = call_expr.span(body).resolve(db)?;
    let typed_body = body.typed_body(db)?;
    let mut closure_owner = None;
    for (_expr, info) in typed_body.closure_infos() {
        let Some(span) = info.body.span(body).resolve(db) else {
            continue;
        };
        if span.range.contains_range(call_span.range)
            && closure_owner
                .as_ref()
                .is_none_or(|(size, _)| span.range.len() < *size)
        {
            closure_owner = Some((span.range.len(), info.ty));
        }
    }
    closure_owner
        .map(|(_, closure)| HierarchyTarget::Closure(closure))
        .or_else(|| match BodyOwner::from_body(db, body)? {
            BodyOwner::Func(func) => Some(HierarchyTarget::Func(func)),
            BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => {
                Some(HierarchyTarget::ContractBody(body))
            }
            BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } | BodyOwner::Closure { .. } => {
                None
            }
        })
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
    let Some(source) = hierarchy_target_at_cursor(&backend.db, top_mod, cursor) else {
        return Ok(None);
    };

    let targets = find_outgoing_calls(&backend.db, source);

    let items: Vec<_> = targets
        .into_iter()
        .filter_map(|(callee, ranges)| {
            let item = target_to_hierarchy_item(&backend.db, callee)?;
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
        let callers = find_incoming_calls(&db, HierarchyTarget::Func(target_func), top_mod);

        let mut caller_names: Vec<String> = callers
            .iter()
            .filter_map(|(target, _)| match target {
                HierarchyTarget::Func(func) => Some(func.name(&db).to_opt()?.data(&db).to_string()),
                HierarchyTarget::Closure(_) => Some("<closure>".to_string()),
                HierarchyTarget::ContractBody(_) => Some("<contract body>".to_string()),
            })
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
        let targets = find_outgoing_calls(&db, HierarchyTarget::Func(main_func));

        let mut target_names: Vec<String> = targets
            .iter()
            .filter_map(|(target, _)| match target {
                HierarchyTarget::Func(func) => Some(func.name(&db).to_opt()?.data(&db).to_string()),
                HierarchyTarget::Closure(_) => Some("<closure>".to_string()),
                HierarchyTarget::ContractBody(_) => Some("<contract body>".to_string()),
            })
            .collect();
        target_names.sort();

        assert_eq!(target_names.len(), 2, "should have 2 outgoing calls");
        assert_eq!(target_names, vec!["helper_a", "helper_b"]);
    }

    #[test]
    fn closure_calls_stay_at_source_level() {
        let mut db = DriverDataBase::default();
        let code = r#"fn helper(value: own u256) -> u256 {
    value + 1
}

fn main_func() -> u256 {
    let add = |value: own u256| -> u256 { helper(value) }
    add(41)
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let main_offset = code.find("main_func").unwrap() as u32;
        let main_func = find_func_at(&db, top_mod, main_offset).expect("main function");
        let body = main_func.body(&db).expect("main body");
        let closure = body
            .typed_body(&db)
            .expect("typed main body")
            .closure_infos()
            .next()
            .expect("closure")
            .1
            .ty;

        let main_targets = find_outgoing_calls(&db, HierarchyTarget::Func(main_func));
        assert_eq!(main_targets.len(), 1);
        assert_eq!(main_targets[0].0, HierarchyTarget::Closure(closure));

        let closure_targets = find_outgoing_calls(&db, HierarchyTarget::Closure(closure));
        assert_eq!(closure_targets.len(), 1);
        assert!(matches!(closure_targets[0].0, HierarchyTarget::Func(_)));

        let closure_callers = find_incoming_calls(&db, HierarchyTarget::Closure(closure), top_mod);
        assert_eq!(closure_callers.len(), 1);
        assert_eq!(closure_callers[0].0, HierarchyTarget::Func(main_func));
    }

    #[test]
    fn contract_body_closure_calls_stay_at_source_level() {
        let mut db = DriverDataBase::default();
        let code = r#"fn helper(value: own u256) -> u256 {
    value + 1
}

msg CounterMsg {
    #[selector = 0x01]
    Ping -> u256,
}

contract Counter {
    init() {
        let add = |value: own u256| -> u256 { helper(value) }
        add(41)
    }

    recv CounterMsg {
        Ping -> u256 {
            let add = |value: own u256| -> u256 { helper(value) }
            add(41)
        }
    }
}
"#;
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///test.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let init_body = top_mod
            .scope_graph(&db)
            .items_dfs(&db)
            .find_map(|item| {
                let ItemKind::Body(body) = item else {
                    return None;
                };
                matches!(
                    BodyOwner::from_body(&db, body),
                    Some(BodyOwner::ContractInit { .. })
                )
                .then_some(body)
            })
            .expect("contract init body");
        let closure = init_body
            .typed_body(&db)
            .expect("typed init body")
            .closure_infos()
            .next()
            .expect("closure")
            .1
            .ty;

        let init_target = HierarchyTarget::ContractBody(init_body);
        let init_item =
            contract_body_to_hierarchy_item(&db, init_body).expect("contract body item");
        assert_eq!(init_item.name, "Counter::init");
        assert_eq!(
            hierarchy_target_at_cursor(
                &db,
                top_mod,
                parser::TextSize::from(code.find("init()").unwrap() as u32),
            ),
            Some(init_target)
        );

        let init_targets = find_outgoing_calls(&db, init_target);
        assert_eq!(init_targets.len(), 1);
        assert_eq!(init_targets[0].0, HierarchyTarget::Closure(closure));

        let closure_callers = find_incoming_calls(&db, HierarchyTarget::Closure(closure), top_mod);
        assert_eq!(closure_callers.len(), 1);
        assert_eq!(closure_callers[0].0, init_target);

        let closure_targets = find_outgoing_calls(&db, HierarchyTarget::Closure(closure));
        assert_eq!(closure_targets.len(), 1);
        assert!(matches!(closure_targets[0].0, HierarchyTarget::Func(_)));

        let recv_body = top_mod
            .scope_graph(&db)
            .items_dfs(&db)
            .find_map(|item| {
                let ItemKind::Body(body) = item else {
                    return None;
                };
                matches!(
                    BodyOwner::from_body(&db, body),
                    Some(BodyOwner::ContractRecvArm { .. })
                )
                .then_some(body)
            })
            .expect("contract recv body");
        let recv_closure = recv_body
            .typed_body(&db)
            .expect("typed recv body")
            .closure_infos()
            .next()
            .expect("recv closure")
            .1
            .ty;
        let recv_target = HierarchyTarget::ContractBody(recv_body);
        let recv_item =
            contract_body_to_hierarchy_item(&db, recv_body).expect("contract recv item");
        assert_eq!(recv_item.name, "Counter::recv[0:0]");
        assert_eq!(
            hierarchy_target_at_cursor(
                &db,
                top_mod,
                parser::TextSize::from(code.find("Ping -> u256 {").unwrap() as u32),
            ),
            Some(recv_target)
        );
        let recv_targets = find_outgoing_calls(&db, recv_target);
        assert_eq!(recv_targets.len(), 1);
        assert_eq!(recv_targets[0].0, HierarchyTarget::Closure(recv_closure));
        let recv_callers =
            find_incoming_calls(&db, HierarchyTarget::Closure(recv_closure), top_mod);
        assert_eq!(recv_callers.len(), 1);
        assert_eq!(recv_callers[0].0, recv_target);
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
        let callers = find_incoming_calls(&db, HierarchyTarget::Func(func), top_mod);
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
        let targets = find_outgoing_calls(&db, HierarchyTarget::Func(func));
        assert!(
            targets.is_empty(),
            "leaf function should have no outgoing calls"
        );
    }
}
