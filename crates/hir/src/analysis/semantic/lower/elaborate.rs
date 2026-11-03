//! `end` elaboration: every access closes right after its last use on each
//! path.
//!
//! A borrow or a projection call opens an access, which stays open while its
//! carrier, or any carrier derived from it, may still be used. Liveness is
//! computed on the raw semantic body, before any folding, so acceptance never
//! depends on optimization. An access that dies inside a block ends after its
//! last use there; one that dies on a control-flow edge ends at the start of
//! the successor, on an edge block of its own when the successor has other
//! predecessors. An access whose carrier is never used ends right after it
//! opens. Of the accesses ending at one point, borrows close first, then
//! sessions finish in reverse creation order, which finishes a session before
//! any session it depends on.
use std::cmp::Reverse;

use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        SBlock, SBlockId, SExpr, SLocalId, SStmt, SStmtId, SStmtKind, STerminator, STerminatorKind,
        SemOrigin, SemanticBody, capability::semantics::holds_carrier,
    },
    ty::ty_check::BodyOwner,
};

/// An access, by the statement opening it.
type Access = SStmtId;

pub(super) fn elaborate_ends<'db>(db: &'db dyn HirAnalysisDb, body: &mut SemanticBody<'db>) {
    let families = access_families(db, body);
    if families.is_empty() {
        return;
    }
    let sessions: FxHashSet<Access> = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .filter(
            |stmt| matches!(&stmt.kind, SStmtKind::Assign { expr, .. } if opens_session(db, expr)),
        )
        .map(|stmt| stmt.id)
        .collect();
    let sorted = |accesses: &mut dyn Iterator<Item = &Access>| -> Vec<Access> {
        let mut accesses: Vec<_> = accesses.copied().collect();
        accesses.sort_unstable_by_key(|access| (sessions.contains(access), Reverse(*access)));
        accesses
    };
    // Locals each access depends on.
    let mut accesses_of: FxHashMap<SLocalId, Vec<Access>> = FxHashMap::default();
    for (access, family) in &families {
        for local in family {
            accesses_of.entry(*local).or_default().push(*access);
        }
    }
    let live_in = live_in(body);
    let live = |locals: &FxHashSet<SLocalId>| -> FxHashSet<Access> {
        locals
            .iter()
            .flat_map(|local| accesses_of.get(local).into_iter().flatten().copied())
            .collect()
    };
    let mut next_id = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .map(|stmt| stmt.id.as_u32() + 1)
        .max()
        .unwrap_or(0);
    let mut end = |access: Access| {
        let id = SStmtId::from_u32(next_id);
        next_id += 1;
        SStmt {
            id,
            origin: SemOrigin::Synthetic,
            kind: SStmtKind::End { access },
        }
    };
    let mut edge_ends: Vec<(usize, usize, Vec<Access>)> = Vec::new();
    for block in 0..body.blocks.len() {
        let data = &body.blocks[block];
        let mut after: Vec<FxHashSet<SLocalId>> = Vec::with_capacity(data.stmts.len());
        let mut locals: FxHashSet<SLocalId> = data
            .terminator
            .kind
            .successors()
            .iter()
            .flat_map(|successor| live_in[successor.index()].iter().copied())
            .chain(data.terminator.kind.used_locals())
            .collect();
        for stmt in data.stmts.iter().rev() {
            after.push(locals.clone());
            transfer(stmt, &mut locals);
        }
        after.reverse();
        let mut open = live(&live_in[block]);
        let mut stmts = Vec::with_capacity(data.stmts.len());
        for (index, stmt) in data.stmts.iter().enumerate() {
            stmts.push(stmt.clone());
            if families.contains_key(&stmt.id) {
                open.insert(stmt.id);
            }
            let dead = sorted(&mut open.difference(&live(&after[index])));
            for access in dead {
                open.remove(&access);
                stmts.push(end(access));
            }
        }
        // Accesses live at the terminator end on each edge that drops them.
        for (position, successor) in data.terminator.kind.successors().iter().enumerate() {
            let dropped = sorted(&mut open.difference(&live(&live_in[successor.index()])));
            if !dropped.is_empty() {
                edge_ends.push((block, position, dropped));
            }
        }
        body.blocks[block].stmts = stmts;
    }
    let mut predecessors = vec![0usize; body.blocks.len()];
    for block in &body.blocks {
        for successor in block.terminator.kind.successors() {
            predecessors[successor.index()] += 1;
        }
    }
    for (block, position, accesses) in edge_ends {
        let successor = body.blocks[block].terminator.kind.successors()[position];
        let ends: Vec<_> = accesses.into_iter().map(&mut end).collect();
        if predecessors[successor.index()] == 1 {
            body.blocks[successor.index()].stmts.splice(0..0, ends);
            continue;
        }
        let edge = SBlockId::from_u32(body.blocks.len() as u32);
        body.blocks.push(SBlock {
            stmts: ends,
            terminator: STerminator {
                origin: SemOrigin::Synthetic,
                kind: STerminatorKind::Goto(successor),
            },
        });
        *body.blocks[block].terminator.kind.successors_mut()[position] = edge;
    }
}

/// Each access-opening statement and the locals its access depends on: its
/// carrier and every carrier derived from it.
fn access_families<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &SemanticBody<'db>,
) -> FxHashMap<Access, FxHashSet<SLocalId>> {
    let mut derived: FxHashMap<SLocalId, FxHashSet<SLocalId>> = FxHashMap::default();
    let mut families = FxHashMap::default();
    for stmt in body.blocks.iter().flat_map(|block| &block.stmts) {
        let (dst, used) = match &stmt.kind {
            SStmtKind::Assign { dst, expr } => {
                if matches!(expr, SExpr::Borrow { .. }) || opens_session(db, expr) {
                    families.insert(stmt.id, *dst);
                }
                (*dst, expr.used_locals())
            }
            SStmtKind::Store { dst, src } => (dst.local, vec![src.value]),
            SStmtKind::End { .. } | SStmtKind::Drop { .. } => continue,
        };
        if holds_carrier(db, body.locals[dst.index()].ty) {
            for local in used {
                derived.entry(local).or_default().insert(dst);
            }
        }
    }
    // Lowering ends some accesses itself, such as receiver guards.
    let explicit: FxHashSet<Access> = body
        .blocks
        .iter()
        .flat_map(|block| &block.stmts)
        .filter_map(|stmt| match stmt.kind {
            SStmtKind::End { access } => Some(access),
            _ => None,
        })
        .collect();
    families
        .into_iter()
        .filter(|(access, _)| !explicit.contains(access))
        .map(|(access, carrier)| {
            let mut family = FxHashSet::from_iter([carrier]);
            let mut pending = vec![carrier];
            while let Some(local) = pending.pop() {
                for next in derived.get(&local).into_iter().flatten() {
                    if family.insert(*next) {
                        pending.push(*next);
                    }
                }
            }
            (access, family)
        })
        .collect()
}

/// Whether `expr` is a projection call, which opens a session.
fn opens_session<'db>(db: &'db dyn HirAnalysisDb, expr: &SExpr<'db>) -> bool {
    matches!(
        expr,
        SExpr::Call { callee, .. }
            if matches!(callee.key.owner(db), BodyOwner::Func(func, _) if func.is_projection(db))
    )
}

/// Backward liveness transfer of one statement.
fn transfer(stmt: &SStmt<'_>, live: &mut FxHashSet<SLocalId>) {
    match &stmt.kind {
        SStmtKind::Assign { dst, expr } => {
            live.remove(dst);
            live.extend(expr.used_locals());
        }
        SStmtKind::Store { dst, src } => {
            if dst.path.is_empty() {
                live.remove(&dst.local);
            }
            live.extend(dst.used_locals());
            live.insert(src.value);
        }
        // Destruction reads none of the accesses a value carries: an access
        // has no drop glue, and a `#[view]` type implements no `Drop`. A
        // store follows each drop through an access, and keeps it live.
        SStmtKind::Drop { place } => live.extend(
            place
                .used_locals()
                .into_iter()
                .filter(|local| *local != place.local),
        ),
        SStmtKind::End { .. } => {}
    }
}

/// The locals live on entry to each block.
fn live_in(body: &SemanticBody<'_>) -> Vec<FxHashSet<SLocalId>> {
    let mut live_in = vec![FxHashSet::default(); body.blocks.len()];
    loop {
        let mut changed = false;
        for (block, data) in body.blocks.iter().enumerate().rev() {
            let mut live: FxHashSet<SLocalId> = data
                .terminator
                .kind
                .successors()
                .iter()
                .flat_map(|successor| live_in[successor.index()].iter().copied())
                .chain(data.terminator.kind.used_locals())
                .collect();
            for stmt in data.stmts.iter().rev() {
                transfer(stmt, &mut live);
            }
            if live != live_in[block] {
                live_in[block] = live;
                changed = true;
            }
        }
        if !changed {
            return live_in;
        }
    }
}
