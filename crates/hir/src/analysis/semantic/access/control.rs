//! Which paths of a body can execute, given callees that never return.
use cranelift_entity::EntityRef;
use salsa::SalsaValue;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SStmtId, SemanticInstance,
        normalized::{
            NExpr, NStatementKind, NTerminatorKind, NormalizedBody, normalize_semantic_body,
        },
    },
};

/// The control flow a body can execute. Block indices and successor
/// positions are those of the raw semantic body, which every normalization of
/// an instance preserves; a diverging call is named by its raw statement.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct ExecutableControlFlow(pub Box<[ExecutableBlock]>);

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum ExecutableBlock {
    Unreachable,
    /// Execution ends at this call, whose callee does not return.
    Diverges(SStmtId),
    /// The terminator executes; each successor edge is feasible or not.
    Continues(Box<[bool]>),
}

fn executable_flow<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
) -> (ExecutableControlFlow, bool) {
    let mut blocks = vec![ExecutableBlock::Unreachable; body.blocks.len()];
    let mut reached = vec![false; body.blocks.len()];
    let mut stack = vec![body.entry];
    reached[body.entry.index()] = true;
    let mut may_return = false;
    while let Some(block) = stack.pop() {
        let data = &body.blocks[block.index()];
        let diverges = data
            .statements
            .iter()
            .find_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { callee, .. },
                    ..
                } if !semantic_may_return(db, SemanticInstance::new(db, callee.key)) => {
                    statement.source
                }
                _ => None,
            });
        if let Some(call) = diverges {
            blocks[block.index()] = ExecutableBlock::Diverges(call);
            continue;
        }
        may_return |= matches!(
            data.terminator.kind,
            NTerminatorKind::Return(_) | NTerminatorKind::Yield { .. }
        );
        let successors = data.terminator.kind.successors();
        blocks[block.index()] = ExecutableBlock::Continues(vec![true; successors.len()].into());
        for successor in successors {
            if !reached[successor.block.index()] {
                reached[successor.block.index()] = true;
                stack.push(successor.block);
            }
        }
    }
    (ExecutableControlFlow(blocks.into()), may_return)
}

#[salsa::tracked]
fn executable_flow_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<ExecutableControlFlow> {
    let artifacts = normalize_semantic_body(db, instance).ok()?;
    Some(executable_flow(db, &artifacts.body).0)
}

/// The control flow runtime lowering emits, so representation choices and
/// return inference see the same returning paths as callers.
pub fn semantic_executable_control_flow<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<&'db ExecutableControlFlow> {
    executable_flow_query(db, instance).as_ref()
}

/// Whether a call to `instance` can return normally. A body that cannot be
/// normalized is assumed to return.
#[salsa::tracked(returns(copy), cycle_initial=may_return_cycle_initial)]
pub fn semantic_may_return<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> bool {
    if instance.is_intrinsically_never_returning(db) {
        return false;
    }
    normalize_semantic_body(db, instance)
        .map_or(true, |artifacts| executable_flow(db, &artifacts.body).1)
}

fn may_return_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: SemanticInstance<'db>,
) -> bool {
    false
}
