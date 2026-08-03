use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap, entity_impl};
use smallvec::SmallVec;

use crate::analysis::semantic::{SBlockId, SLocalId};

use super::ir::{
    NBorrowRoot, NExpr, NSPlace, NSPlaceRoot, NSStmtKind, NSTerminatorKind, NValueOwnershipSource,
    NormalizedSemanticBody,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NAssignmentId(u32);
entity_impl!(NAssignmentId);

pub type NLocalUseList = SmallVec<SLocalId, 4>;
pub type NAssignmentList = SmallVec<NAssignmentId, 2>;
pub type NLocalDependencyList = SmallVec<SLocalId, 2>;

#[derive(Clone, Debug)]
pub struct NAssignmentFacts {
    pub block: SBlockId,
    pub stmt_idx: usize,
    pub dst: SLocalId,
    uses: NLocalUseList,
}

impl NAssignmentFacts {
    pub fn uses(&self) -> &[SLocalId] {
        &self.uses
    }
}

#[derive(Clone, Debug, Default)]
pub struct NStmtFacts {
    assignment: Option<NAssignmentId>,
    uses: NLocalUseList,
}

impl NStmtFacts {
    pub fn assignment(&self) -> Option<NAssignmentId> {
        self.assignment
    }

    pub fn uses(&self) -> &[SLocalId] {
        &self.uses
    }
}

#[derive(Clone, Debug)]
pub struct NormalizedBodyFacts {
    assignments: PrimaryMap<NAssignmentId, NAssignmentFacts>,
    stmt_facts: Vec<Vec<NStmtFacts>>,
    defs_by_local: SecondaryMap<SLocalId, NAssignmentList>,
    assignments_using_local: SecondaryMap<SLocalId, NAssignmentList>,
    local_value_source_uses: SecondaryMap<SLocalId, NLocalDependencyList>,
    local_layout_source_uses: SecondaryMap<SLocalId, NLocalDependencyList>,
    local_dependency_uses: SecondaryMap<SLocalId, NLocalDependencyList>,
    dynamic_dependents_by_local: SecondaryMap<SLocalId, NLocalDependencyList>,
    terminator_uses: SecondaryMap<SBlockId, NLocalUseList>,
}

impl NormalizedBodyFacts {
    pub fn new<'db>(body: &NormalizedSemanticBody<'db>) -> Self {
        let mut assignments = PrimaryMap::new();
        let mut stmt_facts = Vec::with_capacity(body.blocks.len());
        let mut defs_by_local = local_assignment_map(body.locals.len());
        let mut assignments_using_local = local_assignment_map(body.locals.len());

        for (block_idx, block) in body.blocks.iter().enumerate() {
            let block_id = SBlockId::new(block_idx);
            let mut block_stmt_facts = Vec::with_capacity(block.stmts.len());
            for (stmt_idx, stmt) in block.stmts.iter().enumerate() {
                let uses = stmt_used_locals(body, &stmt.kind);
                let assignment = match &stmt.kind {
                    NSStmtKind::Assign { dst, .. } => {
                        let assignment = assignments.push(NAssignmentFacts {
                            block: block_id,
                            stmt_idx,
                            dst: *dst,
                            uses: uses.clone(),
                        });
                        push_unique(&mut defs_by_local[*dst], assignment);
                        for used in &uses {
                            push_unique(&mut assignments_using_local[*used], assignment);
                        }
                        Some(assignment)
                    }
                    NSStmtKind::Store { .. } => None,
                };
                block_stmt_facts.push(NStmtFacts { assignment, uses });
            }
            stmt_facts.push(block_stmt_facts);
        }

        let mut local_value_source_uses = local_dependency_map(body.locals.len());
        let mut local_layout_source_uses = local_dependency_map(body.locals.len());
        for (idx, local_data) in body.locals.iter().enumerate() {
            let local = SLocalId::new(idx);
            if let Some(place) = local_data.backing_place() {
                extend_unique(
                    &mut local_value_source_uses[local],
                    place_used_locals(body, place),
                );
            }
            if let Some(place) = local_data.snapshot_source_place() {
                extend_unique(
                    &mut local_value_source_uses[local],
                    place_used_locals(body, place),
                );
            }
            for source in local_data.ownership_sources() {
                if let NValueOwnershipSource::Place(place) = source {
                    extend_unique(
                        &mut local_value_source_uses[local],
                        place_used_locals(body, place),
                    );
                }
            }
            for source in local_data.layout_backing_sources() {
                extend_unique(
                    &mut local_layout_source_uses[local],
                    place_used_locals(body, &source.source),
                );
            }
        }

        let mut local_dependency_uses = local_value_source_uses.clone();
        for (local, layout_source_uses) in local_layout_source_uses.iter() {
            extend_unique(
                &mut local_dependency_uses[local],
                layout_source_uses.iter().copied(),
            );
        }

        let mut dynamic_dependents_by_local = local_dependency_map(body.locals.len());
        for (local, source_uses) in local_dependency_uses.iter() {
            for source in source_uses {
                push_unique(&mut dynamic_dependents_by_local[*source], local);
            }
        }

        let mut terminator_uses = SecondaryMap::new();
        terminator_uses.resize(body.blocks.len());
        for (block_idx, block) in body.blocks.iter().enumerate() {
            terminator_uses[SBlockId::new(block_idx)] =
                terminator_used_locals(&block.terminator.kind);
        }

        Self {
            assignments,
            stmt_facts,
            defs_by_local,
            assignments_using_local,
            local_value_source_uses,
            local_layout_source_uses,
            local_dependency_uses,
            dynamic_dependents_by_local,
            terminator_uses,
        }
    }

    pub fn assignments(&self) -> &PrimaryMap<NAssignmentId, NAssignmentFacts> {
        &self.assignments
    }

    pub fn assignment(&self, assignment: NAssignmentId) -> Option<&NAssignmentFacts> {
        self.assignments.get(assignment)
    }

    pub fn assignment_count(&self) -> usize {
        self.assignments.len()
    }

    pub fn assignment_ids(&self) -> Vec<NAssignmentId> {
        self.assignments.keys().collect()
    }

    pub fn stmt(&self, block: SBlockId, stmt_idx: usize) -> Option<&NStmtFacts> {
        self.stmt_facts
            .get(block.index())
            .and_then(|block| block.get(stmt_idx))
    }

    pub fn stmt_assignment(&self, block: SBlockId, stmt_idx: usize) -> Option<NAssignmentId> {
        self.stmt(block, stmt_idx)?.assignment()
    }

    pub fn stmt_uses(&self, block: SBlockId, stmt_idx: usize) -> &[SLocalId] {
        self.stmt(block, stmt_idx)
            .map(NStmtFacts::uses)
            .unwrap_or(&[])
    }

    pub fn defs_by_local(&self, local: SLocalId) -> &[NAssignmentId] {
        self.defs_by_local
            .get(local)
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }

    pub fn assignments_using_local(&self, local: SLocalId) -> &[NAssignmentId] {
        self.assignments_using_local
            .get(local)
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }

    pub fn local_value_source_uses(&self, local: SLocalId) -> &[SLocalId] {
        self.local_value_source_uses
            .get(local)
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }

    pub fn local_layout_source_uses(&self, local: SLocalId) -> &[SLocalId] {
        self.local_layout_source_uses
            .get(local)
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }

    pub fn local_dependency_uses(&self, local: SLocalId) -> &[SLocalId] {
        self.local_dependency_uses
            .get(local)
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }

    pub fn dynamic_dependents(&self, local: SLocalId) -> &[SLocalId] {
        self.dynamic_dependents_by_local
            .get(local)
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }

    pub fn terminator_uses(&self, block: SBlockId) -> &[SLocalId] {
        self.terminator_uses
            .get(block)
            .map(SmallVec::as_slice)
            .unwrap_or(&[])
    }
}

fn local_assignment_map(local_count: usize) -> SecondaryMap<SLocalId, NAssignmentList> {
    let mut map = SecondaryMap::new();
    map.resize(local_count);
    map
}

fn local_dependency_map(local_count: usize) -> SecondaryMap<SLocalId, NLocalDependencyList> {
    let mut map = SecondaryMap::new();
    map.resize(local_count);
    map
}

fn stmt_used_locals<'db>(
    body: &NormalizedSemanticBody<'db>,
    stmt: &NSStmtKind<'db>,
) -> NLocalUseList {
    match stmt {
        NSStmtKind::Assign { expr, .. } => expr_used_locals(body, expr),
        NSStmtKind::Store { dst, src } => {
            let mut uses = place_used_locals(body, dst);
            push_unique(&mut uses, src.local);
            uses
        }
    }
}

fn expr_used_locals<'db>(body: &NormalizedSemanticBody<'db>, expr: &NExpr<'db>) -> NLocalUseList {
    let mut uses = NLocalUseList::new();
    expr.for_each_value_operand(|value| push_unique(&mut uses, value.local));
    expr.for_each_place_operand(|place| extend_unique(&mut uses, place_used_locals(body, place)));
    uses
}

fn place_used_locals<'db>(
    body: &NormalizedSemanticBody<'db>,
    place: &NSPlace<'db>,
) -> NLocalUseList {
    let mut uses = NLocalUseList::new();
    match place.root {
        NSPlaceRoot::Root(root) => match body.root(root) {
            Some(NBorrowRoot::Param { local, .. }) | Some(NBorrowRoot::LocalSlot { local }) => {
                push_unique(&mut uses, *local);
            }
            Some(NBorrowRoot::Provider { .. }) | None => {}
        },
        NSPlaceRoot::CarrierDerefLocal(local) => {
            push_unique(&mut uses, local);
        }
    }
    extend_unique(&mut uses, place.dynamic_index_locals());
    uses
}

fn terminator_used_locals(term: &NSTerminatorKind<'_>) -> NLocalUseList {
    let mut uses = NLocalUseList::new();
    match term {
        NSTerminatorKind::Goto(_)
        | NSTerminatorKind::Assert { .. }
        | NSTerminatorKind::Return(None) => {}
        NSTerminatorKind::Branch { cond, .. }
        | NSTerminatorKind::MatchEnum { value: cond, .. }
        | NSTerminatorKind::Return(Some(cond)) => push_unique(&mut uses, cond.local),
    }
    uses
}

fn extend_unique<T: Copy + PartialEq, const N: usize>(
    items: &mut SmallVec<T, N>,
    new_items: impl IntoIterator<Item = T>,
) {
    for item in new_items {
        push_unique(items, item);
    }
}

fn push_unique<T: Copy + PartialEq, const N: usize>(items: &mut SmallVec<T, N>, item: T) {
    if !items.contains(&item) {
        items.push(item);
    }
}
