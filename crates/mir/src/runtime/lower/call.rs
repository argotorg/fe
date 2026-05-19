use rustc_hash::FxHashSet;

use crate::runtime::{
    ConstRegionId, RExpr, RStmt, RTerminator, RuntimeBody, RuntimeCallEdge, RuntimeCodeRegion,
};

pub fn collect_runtime_calls<'db>(body: &RuntimeBody<'db>) -> Vec<RuntimeCallEdge<'db>> {
    let mut seen = FxHashSet::default();
    let mut calls = Vec::new();
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let RStmt::Assign {
                expr: RExpr::Call { callee, .. },
                ..
            } = stmt
                && seen.insert(*callee)
            {
                calls.push(RuntimeCallEdge { callee: *callee });
            }
        }
        if let RTerminator::TerminalCall { callee, .. } = &block.terminator
            && seen.insert(*callee)
        {
            calls.push(RuntimeCallEdge { callee: *callee });
        }
    }
    calls
}

pub fn collect_referenced_const_regions<'db>(body: &RuntimeBody<'db>) -> Vec<ConstRegionId<'db>> {
    let mut seen = FxHashSet::default();
    let mut regions = Vec::new();
    for block in &body.blocks {
        for stmt in &block.stmts {
            let RStmt::Assign { expr, .. } = stmt else {
                continue;
            };
            let region = match expr {
                RExpr::ConstRef { region, .. } => Some(*region),
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::ConstRegionAddr { region }) => {
                    Some(*region)
                }
                _ => None,
            };
            if let Some(region) = region
                && seen.insert(region)
            {
                regions.push(region);
            }
        }
    }
    regions
}

pub fn collect_referenced_code_regions<'db>(
    body: &RuntimeBody<'db>,
) -> Vec<RuntimeCodeRegion<'db>> {
    let mut seen = FxHashSet::default();
    let mut regions = Vec::new();
    for block in &body.blocks {
        for stmt in &block.stmts {
            let RStmt::Assign { expr, .. } = stmt else {
                continue;
            };
            let region = match expr {
                RExpr::Builtin(
                    crate::runtime::RuntimeBuiltin::CodeRegionOffset { region }
                    | crate::runtime::RuntimeBuiltin::CodeRegionLen { region },
                ) => Some(*region),
                RExpr::Builtin(crate::runtime::RuntimeBuiltin::CurrentCodeRegionLen) => None,
                RExpr::Use(_)
                | RExpr::ConstScalar(_)
                | RExpr::Placeholder { .. }
                | RExpr::Builtin(_)
                | RExpr::Unary { .. }
                | RExpr::Binary { .. }
                | RExpr::Cast { .. }
                | RExpr::ConstRef { .. }
                | RExpr::AllocObject { .. }
                | RExpr::MaterializeToObject { .. }
                | RExpr::MaterializePlaceToObject { .. }
                | RExpr::ProviderFromRaw { .. }
                | RExpr::WordToRawAddr { .. }
                | RExpr::ProviderToRaw { .. }
                | RExpr::RetagRef { .. }
                | RExpr::AddrOf { .. }
                | RExpr::Load { .. }
                | RExpr::AggregateExtract { .. }
                | RExpr::Call { .. }
                | RExpr::EnumMake { .. }
                | RExpr::EnumTagOfValue { .. }
                | RExpr::EnumIsVariant { .. }
                | RExpr::EnumExtract { .. }
                | RExpr::EnumGetTag { .. }
                | RExpr::EnumAssertVariantRef { .. } => None,
            };
            if let Some(region) = region
                && seen.insert(region)
            {
                regions.push(region);
            }
        }
    }
    regions
}
