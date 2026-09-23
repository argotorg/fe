//! Static loop regions and iteration-qualified capability occurrences.
use std::{
    collections::{BTreeMap, BTreeSet},
    iter::once,
};

use cranelift_entity::EntityRef;

use crate::analysis::semantic::{
    capability::{guard::ValueOccurrence, index::IndexExpr},
    normalized::{NBlockId, NValueDefinition, NValueId, NormalizedBody},
};

pub(super) struct LoopRegions {
    reverse_postorder: Vec<usize>,
    blocks: Vec<Option<NBlockId>>,
    values: BTreeMap<NBlockId, BTreeSet<NValueId>>,
    feedback: BTreeSet<(NBlockId, NBlockId)>,
}

impl LoopRegions {
    pub fn new(body: &NormalizedBody<'_>) -> Self {
        let edges: Vec<Vec<_>> = body
            .blocks
            .iter()
            .map(|block| {
                block
                    .terminator
                    .kind
                    .successors()
                    .iter()
                    .map(|edge| edge.block.index())
                    .collect()
            })
            .collect();
        let mut reverse = vec![Vec::new(); edges.len()];
        for (from, edges) in edges.iter().enumerate() {
            for to in edges {
                reverse[*to].push(from);
            }
        }
        let mut seen = vec![false; edges.len()];
        let mut active = vec![false; edges.len()];
        let mut feedback = BTreeSet::new();
        let mut order = Vec::new();
        for first in once(body.entry.index()).chain(0..edges.len()) {
            let mut pending = vec![(first, false)];
            while let Some((block, finished)) = pending.pop() {
                if finished {
                    active[block] = false;
                    order.push(block);
                    continue;
                }
                if std::mem::replace(&mut seen[block], true) {
                    continue;
                }
                active[block] = true;
                pending.push((block, true));
                for next in &edges[block] {
                    // Only ancestor edges close a DFS cycle. A branch join can
                    // have a smaller block number without starting an iteration.
                    if active[*next] {
                        feedback.insert((NBlockId::new(block), NBlockId::new(*next)));
                    }
                    pending.push((*next, false));
                }
            }
        }
        let mut assigned = vec![false; edges.len()];
        let mut blocks = vec![None; edges.len()];
        order.reverse();
        for first in order.iter().copied() {
            if assigned[first] {
                continue;
            }
            let mut component = Vec::new();
            let mut pending = vec![first];
            while let Some(block) = pending.pop() {
                if std::mem::replace(&mut assigned[block], true) {
                    continue;
                }
                component.push(block);
                pending.extend(&reverse[block]);
            }
            if component.len() > 1 || edges[first].contains(&first) {
                let region = NBlockId::new(*component.iter().min().expect("nonempty component"));
                for block in component {
                    blocks[block] = Some(region);
                }
            }
        }
        let mut values: BTreeMap<_, BTreeSet<_>> = blocks
            .iter()
            .flatten()
            .copied()
            .map(|region| (region, BTreeSet::new()))
            .collect();
        for (index, value) in body.values.iter().enumerate() {
            let block = match value.definition {
                NValueDefinition::EntryParam { .. } => continue,
                NValueDefinition::BlockParam { block, .. }
                | NValueDefinition::Statement { block, .. } => block,
            };
            if let Some(region) = blocks[block.index()] {
                values
                    .entry(region)
                    .or_default()
                    .insert(NValueId::new(index));
            }
        }
        Self {
            reverse_postorder: order,
            blocks,
            values,
            feedback,
        }
    }

    pub fn reverse_postorder(&self) -> &[usize] {
        &self.reverse_postorder
    }

    pub fn for_value(&self, body: &NormalizedBody<'_>, value: NValueId) -> Option<NBlockId> {
        let block = match body.values[value.index()].definition {
            NValueDefinition::EntryParam { .. } => return None,
            NValueDefinition::BlockParam { block, .. }
            | NValueDefinition::Statement { block, .. } => block,
        };
        self.blocks[block.index()]
    }

    pub fn arguments<'db>(
        &self,
        body: &NormalizedBody<'_>,
        value: NValueId,
    ) -> Vec<IndexExpr<'db>> {
        self.for_value(body, value)
            .into_iter()
            .flat_map(|region| {
                once(IndexExpr::Iteration(region))
                    .chain(self.values[&region].iter().copied().map(IndexExpr::Runtime))
            })
            .collect()
    }

    pub fn feedback(&self, from: NBlockId, to: NBlockId) -> Option<NBlockId> {
        let region = self.blocks[from.index()]?;
        self.feedback.contains(&(from, to)).then_some(region)
    }

    pub fn repeated(&self, region: NBlockId) -> &BTreeSet<NValueId> {
        &self.values[&region]
    }

    pub fn repeats_occurrence(&self, region: NBlockId, occurrence: ValueOccurrence) -> bool {
        match occurrence {
            ValueOccurrence::Value(value) | ValueOccurrence::CallChoice { result: value, .. } => {
                self.values[&region].contains(&value)
            }
            ValueOccurrence::Root(_) => true,
            ValueOccurrence::Argument(_)
            | ValueOccurrence::Summary
            | ValueOccurrence::SummaryChoice(_) => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{
            semantic::{
                SemOrigin, VariantIndex,
                borrowck::solver::{BorrowSummaryMode, Borrowck},
                get_or_build_semantic_instance, identity_semantic_instance_key,
                normalized::{
                    NBlock, NOperand, NSuccessor, NTerminator, NTerminatorKind, NValue, ReadMode,
                    normalize_semantic_body, verify_normalized_body,
                },
            },
            ty::ty_check::BodyOwner,
        },
        test_db::{HirAnalysisTestDb, find_func},
    };
    use itertools::Itertools;

    #[test]
    fn empty_cycles_have_repeated_values_and_solve_without_returning() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("empty_cycles.fe".into(), "fn anchor() {}");
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "anchor"))),
        );
        for (targets, entry, returns) in [
            (vec![Some(0)], 0, false),
            (vec![Some(1), Some(0)], 0, false),
            (vec![Some(1), Some(2), Some(1)], 2, false),
            (vec![None, Some(1)], 0, true),
        ] {
            let mut body = normalize_semantic_body(&db, instance).unwrap().body;
            let origin = SemOrigin::Body(body.template_owner);
            body.values.clear();
            body.roots.clear();
            body.entry = NBlockId::new(entry);
            body.blocks = targets
                .into_iter()
                .map(|target| NBlock {
                    params: Box::new([]),
                    statements: Vec::new(),
                    terminator: NTerminator {
                        origin,
                        kind: target.map_or(NTerminatorKind::Return(None), |target| {
                            NTerminatorKind::Goto(NSuccessor {
                                block: NBlockId::new(target),
                                args: Box::new([]),
                            })
                        }),
                    },
                })
                .collect();
            verify_normalized_body(&db, &body).unwrap();
            let loops = LoopRegions::new(&body);
            assert!(!loops.feedback.is_empty());
            for (from, to) in &loops.feedback {
                let region = loops.feedback(*from, *to).unwrap();
                assert!(loops.repeated(region).is_empty());
            }
            let mut checker =
                Borrowck::new_with_body(&db, instance, body, BorrowSummaryMode::Final).unwrap();
            checker.solve().unwrap();
            let summary = checker.build_summary().unwrap();
            assert_eq!(summary.may_return, returns);
            assert!(summary.availability.unavailable.is_empty());
        }
    }
    #[test]
    fn parameter_only_cycle_tracks_block_parameters_and_solves() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("parameter_cycle.fe".into(), "fn anchor(_ flag: bool) {}");
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "anchor"))),
        );
        let mut body = normalize_semantic_body(&db, instance).unwrap().body;
        let origin = SemOrigin::Body(body.template_owner);
        let incoming = body
            .values
            .iter()
            .find(|value| matches!(value.definition, NValueDefinition::EntryParam { param: 0 }))
            .unwrap()
            .clone();
        let parameter = NValue {
            definition: NValueDefinition::BlockParam {
                block: NBlockId::new(1),
                index: 0,
            },
            ..incoming.clone()
        };
        body.values = vec![incoming, parameter];
        body.roots.clear();
        body.entry = NBlockId::new(0);
        body.blocks = [0, 1]
            .into_iter()
            .map(|index| NBlock {
                params: if index == 0 {
                    Box::new([])
                } else {
                    Box::new([NValueId::new(1)])
                },
                statements: Vec::new(),
                terminator: NTerminator {
                    origin,
                    kind: NTerminatorKind::Goto(NSuccessor {
                        block: NBlockId::new(1),
                        args: Box::new([NOperand {
                            value: NValueId::new(index),
                            origin: None,
                            mode: ReadMode::Copy,
                        }]),
                    }),
                },
            })
            .collect();
        verify_normalized_body(&db, &body).unwrap();
        let loops = LoopRegions::new(&body);
        let region = loops.feedback(NBlockId::new(1), NBlockId::new(1)).unwrap();
        assert_eq!(loops.repeated(region), &BTreeSet::from([NValueId::new(1)]));
        assert_eq!(loops.for_value(&body, NValueId::new(0)), None);
        let mut checker =
            Borrowck::new_with_body(&db, instance, body, BorrowSummaryMode::Final).unwrap();
        checker.solve().unwrap();
        assert!(!checker.build_summary().unwrap().may_return);
    }

    fn graph_body<'db>(
        template: &NormalizedBody<'db>,
        edges: &[Vec<usize>],
        entry: usize,
    ) -> NormalizedBody<'db> {
        let mut body = template.clone();
        let value = body
            .values
            .iter()
            .find(|value| matches!(value.definition, NValueDefinition::EntryParam { param: 0 }))
            .unwrap()
            .clone();
        let enum_ty = value.ty;
        body.values = vec![value];
        body.roots.clear();
        body.entry = NBlockId::new(entry);
        let origin = SemOrigin::Body(body.template_owner);
        body.blocks = edges
            .iter()
            .map(|targets| NBlock {
                params: Box::new([]),
                statements: Vec::new(),
                terminator: NTerminator {
                    origin,
                    kind: if targets.is_empty() {
                        NTerminatorKind::Return(None)
                    } else {
                        NTerminatorKind::MatchEnum {
                            value: NOperand {
                                value: NValueId::new(0),
                                origin: None,
                                mode: ReadMode::Copy,
                            },
                            enum_ty,
                            cases: targets
                                .iter()
                                .enumerate()
                                .map(|(variant, target)| {
                                    (
                                        VariantIndex(variant.try_into().unwrap()),
                                        NSuccessor {
                                            block: NBlockId::new(*target),
                                            args: Box::new([]),
                                        },
                                    )
                                })
                                .collect(),
                            default: None,
                        }
                    },
                },
            })
            .collect();
        body
    }

    fn assert_graph_contract(body: &NormalizedBody<'_>, edges: &[Vec<usize>]) {
        let loops = LoopRegions::new(body);
        // Independent positive-length reachability, rather than another SCC/DFS implementation.
        let reach: Vec<_> = edges
            .iter()
            .map(|targets| {
                let mut pending = targets.clone();
                let mut seen = BTreeSet::new();
                while let Some(next) = pending.pop() {
                    if seen.insert(next) {
                        pending.extend(&edges[next]);
                    }
                }
                seen
            })
            .collect();
        for (index, targets) in reach.iter().enumerate() {
            let expected = targets.contains(&index).then(|| {
                NBlockId::new(
                    *targets
                        .iter()
                        .find(|target| reach[**target].contains(&index))
                        .unwrap(),
                )
            });
            assert_eq!(
                loops.blocks[index], expected,
                "{edges:?}, entry {:?}",
                body.entry
            );
            if let Some(region) = expected {
                assert!(loops.repeated(region).is_empty());
            }
        }
        // Every forward edge is processed in one sweep, including when block
        // allocation order puts a branch join before its predecessors.
        let mut rank = vec![0; edges.len()];
        assert_eq!(loops.reverse_postorder.len(), edges.len());
        for (position, block) in loops.reverse_postorder.iter().copied().enumerate() {
            rank[block] = position;
        }
        for (from, targets) in edges.iter().enumerate() {
            for to in targets {
                assert!(
                    rank[from] < rank[*to]
                        || loops
                            .feedback
                            .contains(&(NBlockId::new(from), NBlockId::new(*to))),
                    "backward edge outside feedback: {from}->{to}, {edges:?}"
                );
            }
        }
        // Kahn's algorithm is independent of the ancestor-edge construction.
        let mut incoming = vec![0; edges.len()];
        for (from, targets) in edges.iter().enumerate() {
            for to in targets {
                if loops
                    .feedback(NBlockId::new(from), NBlockId::new(*to))
                    .is_none()
                {
                    incoming[*to] += 1;
                }
            }
        }
        let mut ready: Vec<_> = incoming
            .iter()
            .enumerate()
            .filter_map(|(index, count)| (*count == 0).then_some(index))
            .collect();
        let mut visited = 0;
        while let Some(from) = ready.pop() {
            visited += 1;
            for to in &edges[from] {
                if loops
                    .feedback(NBlockId::new(from), NBlockId::new(*to))
                    .is_none()
                {
                    incoming[*to] -= 1;
                    if incoming[*to] == 0 {
                        ready.push(*to);
                    }
                }
            }
        }
        assert_eq!(
            visited,
            edges.len(),
            "feedback failed to cut a cycle: {edges:?}"
        );
    }

    #[test]
    fn feedback_matches_graph_oracles_for_all_four_block_graphs_and_entries() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("graphs.fe".into(),
            "enum Choice { A, B, C, D }\nimpl Copy for Choice {}\nfn anchor(_ choice: own Choice) {}");
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "anchor"))),
        );
        let template = normalize_semantic_body(&db, instance).unwrap().body;
        let mut checked = 0;
        for count in 1..=4 {
            for mask in 0u32..(1 << (count * count)) {
                let edges: Vec<Vec<_>> = (0..count)
                    .map(|from| {
                        (0..count)
                            .filter(|to| mask & (1 << (from * count + to)) != 0)
                            .collect()
                    })
                    .collect();
                for entry in 0..count {
                    let body = graph_body(&template, &edges, entry);
                    // These are actual verified normalized graphs, including
                    // irreducible/multiple-entry and unreachable components.
                    verify_normalized_body(&db, &body).unwrap();
                    assert_graph_contract(&body, &edges);
                    checked += 1;
                }
            }
        }
        assert_eq!(checked, 263_714);
        // The join is allocated before both branches, as in normalized matches.
        // Enumerate every relabeling so numeric ordering cannot become semantic.
        for permutation in (0..5).permutations(5) {
            let original = [vec![2, 3], vec![4], vec![1], vec![1], vec![0]];
            let mut edges = vec![Vec::new(); original.len()];
            for (from, targets) in original.iter().enumerate() {
                edges[permutation[from]] = targets.iter().map(|to| permutation[*to]).collect();
            }
            let body = graph_body(&template, &edges, permutation[0]);
            verify_normalized_body(&db, &body).unwrap();
            assert_graph_contract(&body, &edges);
            let loops = LoopRegions::new(&body);
            for from in [2, 3] {
                assert!(
                    loops
                        .feedback(
                            NBlockId::new(permutation[from]),
                            NBlockId::new(permutation[1])
                        )
                        .is_none()
                );
            }
            assert_eq!(loops.feedback.len(), 1);
            assert!(
                loops
                    .feedback(NBlockId::new(permutation[4]), NBlockId::new(permutation[0]))
                    .is_some()
            );
        }
    }
}
