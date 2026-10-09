//! Static loop regions and iteration-qualified capability occurrences.
use std::{
    collections::{BTreeMap, BTreeSet},
    iter::{once, successors},
};

use cranelift_entity::EntityRef;

use crate::analysis::semantic::{
    capability::{
        guard::{ChoiceKey, Guard, ValueOccurrence},
        index::{BinderScope, IndexExpr},
        path::StructuralPath,
    },
    normalized::{
        NBlockId, NValueDefinition, NValueId, NormalizedBody, NormalizedBodyVerifyError,
        normalized_cfg,
    },
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct LoopEdge {
    pub from: NBlockId,
    pub successor: usize,
    pub to: NBlockId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct ValidatedLoop {
    pub header: NBlockId,
    pub blocks: BTreeSet<NBlockId>,
    pub entries: Vec<LoopEdge>,
    pub backedges: Vec<LoopEdge>,
    pub exits: Vec<LoopEdge>,
}

/// Natural loops are identified by dominance, independently of the SCC used for
/// feedback forgetting. Walking back from each backedge stops at its dominating
/// header, so only the header has entries from outside the loop.
pub(super) fn validated_loops(
    body: &NormalizedBody<'_>,
) -> Result<Vec<ValidatedLoop>, NormalizedBodyVerifyError> {
    let cfg = normalized_cfg(body)?;
    let mut headers: BTreeMap<NBlockId, Vec<LoopEdge>> = BTreeMap::new();
    for (index, block) in body.blocks.iter().enumerate() {
        if !cfg.reachable[index] {
            continue;
        }
        let from = NBlockId::new(index);
        for (successor, edge) in block.terminator.kind.successors().into_iter().enumerate() {
            if cfg.dominators[index].contains(&edge.block) {
                headers.entry(edge.block).or_default().push(LoopEdge {
                    from,
                    successor,
                    to: edge.block,
                });
            }
        }
    }
    let mut result = Vec::new();
    for (header, backedges) in headers {
        let mut blocks = BTreeSet::from([header]);
        let mut pending: Vec<_> = backedges.iter().map(|edge| edge.from).collect();
        while let Some(block) = pending.pop() {
            if blocks.insert(block) {
                pending.extend(
                    cfg.predecessors[block.index()]
                        .iter()
                        .filter(|predecessor| cfg.reachable[predecessor.index()])
                        .copied(),
                );
            }
        }
        let entries = body
            .blocks
            .iter()
            .enumerate()
            .filter(|(index, _)| cfg.reachable[*index] && !blocks.contains(&NBlockId::new(*index)))
            .flat_map(|(index, block)| {
                block
                    .terminator
                    .kind
                    .successors()
                    .into_iter()
                    .enumerate()
                    .filter_map(move |(successor, edge)| {
                        (edge.block == header).then_some(LoopEdge {
                            from: NBlockId::new(index),
                            successor,
                            to: header,
                        })
                    })
            })
            .collect();
        let exits = blocks
            .iter()
            .flat_map(|from| {
                body.blocks[from.index()]
                    .terminator
                    .kind
                    .successors()
                    .into_iter()
                    .enumerate()
                    .filter_map(|(successor, edge)| {
                        (!blocks.contains(&edge.block)).then_some(LoopEdge {
                            from: *from,
                            successor,
                            to: edge.block,
                        })
                    })
                    .collect::<Vec<_>>()
            })
            .collect();
        result.push(ValidatedLoop {
            header,
            blocks,
            entries,
            backedges,
            exits,
        });
    }
    Ok(result)
}

/// Nested iteration regions. Every feedback edge starts the next iteration of
/// one region, which renews only the values defined inside it. A reducible
/// cycle nests natural loops, so an inner latch leaves values of the enclosing
/// body, and occurrences created there, in their current iteration. A cyclic
/// component without a dominating header for each feedback edge, or one not
/// reachable from the entry, repeats as one region.
pub(super) struct LoopRegions {
    reverse_postorder: Vec<usize>,
    successors: Vec<Vec<usize>>,
    /// The innermost region each block repeats in.
    blocks: Vec<Option<NBlockId>>,
    regions: BTreeMap<NBlockId, LoopRegion>,
    /// Each feedback edge and the region whose next iteration it starts.
    feedback: BTreeMap<(NBlockId, NBlockId), NBlockId>,
    /// All predecessors of feedback destinations, including initial function entry.
    entries: BTreeMap<NBlockId, Vec<Option<NBlockId>>>,
}

#[derive(Default)]
struct LoopRegion {
    parent: Option<NBlockId>,
    /// Values defined in this region, including its nested regions.
    values: BTreeSet<NValueId>,
}

impl LoopRegions {
    pub fn new(body: &NormalizedBody<'_>) -> Self {
        let successors: Vec<Vec<_>> = body
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
        let mut reverse = vec![Vec::new(); successors.len()];
        for (from, edges) in successors.iter().enumerate() {
            for to in edges {
                reverse[*to].push(from);
            }
        }
        let mut seen = vec![false; successors.len()];
        let mut active = vec![false; successors.len()];
        let mut edges = BTreeSet::new();
        let mut order = Vec::new();
        for first in once(body.entry.index()).chain(0..successors.len()) {
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
                for next in &successors[block] {
                    // Only ancestor edges close a DFS cycle. A branch join can
                    // have a smaller block number without starting an iteration.
                    if active[*next] {
                        edges.insert((block, *next));
                    }
                    pending.push((*next, false));
                }
            }
        }
        order.reverse();
        // Dominance decides natural loops; acyclic bodies never need it.
        let cfg =
            (!edges.is_empty()).then(|| normalized_cfg(body).expect("verified normalized body"));
        let dominates = |header: usize, block: usize| {
            cfg.as_ref().is_some_and(|cfg| {
                cfg.reachable[block] && cfg.dominators[block].contains(&NBlockId::new(header))
            })
        };
        let mut assigned = vec![false; successors.len()];
        let mut blocks = vec![None; successors.len()];
        let mut regions = BTreeMap::new();
        let mut feedback = BTreeMap::new();
        for first in order.iter().copied() {
            if assigned[first] {
                continue;
            }
            let mut component = BTreeSet::new();
            let mut pending = vec![first];
            while let Some(block) = pending.pop() {
                if std::mem::replace(&mut assigned[block], true) {
                    continue;
                }
                component.insert(block);
                pending.extend(&reverse[block]);
            }
            if component.len() == 1 && !successors[first].contains(&first) {
                continue;
            }
            let latches: Vec<_> = edges
                .iter()
                .copied()
                .filter(|(from, _)| component.contains(from))
                .collect();
            if !latches
                .iter()
                .all(|(from, header)| dominates(*header, *from))
            {
                let region = NBlockId::new(*component.first().expect("nonempty component"));
                for block in &component {
                    blocks[*block] = Some(region);
                }
                regions.insert(region, LoopRegion::default());
                feedback.extend(
                    latches
                        .into_iter()
                        .map(|(from, to)| ((NBlockId::new(from), NBlockId::new(to)), region)),
                );
                continue;
            }
            // Natural loops of one reducible component nest or are disjoint.
            let mut natural: BTreeMap<usize, BTreeSet<usize>> = BTreeMap::new();
            for (latch, header) in &latches {
                let body = natural
                    .entry(*header)
                    .or_insert_with(|| BTreeSet::from([*header]));
                let mut pending = vec![*latch];
                while let Some(block) = pending.pop() {
                    if body.insert(block) {
                        pending.extend(&reverse[block]);
                    }
                }
            }
            let innermost = |block: usize, except: usize| {
                natural
                    .iter()
                    .filter(|(header, body)| **header != except && body.contains(&block))
                    .min_by_key(|(_, body)| body.len())
                    .map(|(header, _)| NBlockId::new(*header))
            };
            for block in &component {
                blocks[*block] = innermost(*block, usize::MAX);
            }
            for header in natural.keys() {
                regions.insert(
                    NBlockId::new(*header),
                    LoopRegion {
                        parent: innermost(*header, *header),
                        values: BTreeSet::new(),
                    },
                );
            }
            feedback.extend(
                latches.into_iter().map(|(from, to)| {
                    ((NBlockId::new(from), NBlockId::new(to)), NBlockId::new(to))
                }),
            );
        }
        let entries = feedback
            .keys()
            .map(|(_, to)| *to)
            .collect::<BTreeSet<_>>()
            .into_iter()
            .map(|to| {
                let predecessors = reverse[to.index()]
                    .iter()
                    .map(|from| Some(NBlockId::new(*from)))
                    .chain((to == body.entry).then_some(None))
                    .collect::<BTreeSet<_>>()
                    .into_iter()
                    .collect();
                (to, predecessors)
            })
            .collect();
        let mut loops = Self {
            reverse_postorder: order,
            successors,
            blocks,
            regions,
            feedback,
            entries,
        };
        for (index, value) in body.values.iter().enumerate() {
            let block = match value.definition {
                NValueDefinition::EntryParam { .. } => continue,
                NValueDefinition::BlockParam { block, .. }
                | NValueDefinition::Statement { block, .. } => block,
            };
            let Some(region) = loops.blocks[block.index()] else {
                continue;
            };
            for region in loops.enclosing(region).collect::<Vec<_>>() {
                loops
                    .regions
                    .get_mut(&region)
                    .expect("region")
                    .values
                    .insert(NValueId::new(index));
            }
        }
        loops
    }

    pub fn has_cycle(&self) -> bool {
        self.blocks.iter().any(Option::is_some)
    }

    pub fn reverse_postorder(&self) -> &[usize] {
        &self.reverse_postorder
    }

    /// The innermost region that renews this value.
    pub fn for_value(&self, body: &NormalizedBody<'_>, value: NValueId) -> Option<NBlockId> {
        self.blocks[definition_block(body, value)?.index()]
    }

    /// `region` and the regions enclosing it, innermost first.
    fn enclosing(&self, region: NBlockId) -> impl Iterator<Item = NBlockId> + '_ {
        successors(Some(region), |region| self.regions[region].parent)
    }

    /// The outermost region around `region`: every value whose instance can
    /// differ between two executions of a block in `region`.
    pub fn outermost(&self, region: NBlockId) -> NBlockId {
        self.enclosing(region).last().expect("region")
    }

    /// The occurrence parameters of a value created in a loop: its innermost
    /// iteration and the repeated values that can be current where it is created.
    /// Regions that have not run yet in this iteration supply no current value,
    /// and their own feedback must not mistake this occurrence for a previous one.
    pub fn arguments<'db>(
        &self,
        body: &NormalizedBody<'_>,
        value: NValueId,
    ) -> Vec<IndexExpr<'db>> {
        let Some(region) = self.for_value(body, value) else {
            return Vec::new();
        };
        let block = definition_block(body, value)
            .expect("repeated value")
            .index();
        let outermost = self.outermost(region);
        let mut later = BTreeSet::<NValueId>::new();
        let mut reached = BTreeSet::new();
        let mut pending = vec![block];
        while let Some(from) = pending.pop() {
            for to in &self.successors[from] {
                if self
                    .feedback
                    .contains_key(&(NBlockId::new(from), NBlockId::new(*to)))
                    || !self.blocks[*to].is_some_and(|inner| self.outermost(inner) == outermost)
                    || !reached.insert(*to)
                {
                    continue;
                }
                let header = NBlockId::new(*to);
                if self.regions.contains_key(&header)
                    && !self.enclosing(region).any(|enclosing| enclosing == header)
                {
                    later.extend(self.regions[&header].values.iter().copied());
                }
                pending.push(*to);
            }
        }
        once(IndexExpr::Iteration(region))
            .chain(
                self.regions[&outermost]
                    .values
                    .iter()
                    .filter(|value| !later.contains(*value))
                    .copied()
                    .map(IndexExpr::Runtime),
            )
            .collect()
    }

    pub fn feedback(&self, from: NBlockId, to: NBlockId) -> Option<NBlockId> {
        self.feedback.get(&(from, to)).copied()
    }

    /// Partition joined loop states by their most recent incoming edge. Feedback
    /// first forgets the prior visit, then both provenance and availability use
    /// this guard, preserving their correlation without accumulating history.
    pub fn entry_guard<'db>(&self, from: Option<NBlockId>, to: NBlockId) -> Option<Guard<'db>> {
        let predecessors = self.entries.get(&to)?;
        let selected = predecessors.binary_search(&from).expect("loop predecessor");
        let bits = usize::BITS - (predecessors.len() - 1).leading_zeros();
        Guard::always(&BinderScope::default()).with_choice_bits(
            ChoiceKey::new(ValueOccurrence::LoopEntry(to), StructuralPath::default()),
            bits as u16,
            |bit| selected & (1 << bit) != 0,
        )
    }

    /// Whether the next iteration of `region` renews this index: its own or a
    /// nested iteration, or a value defined inside it.
    pub fn repeats_index(&self, region: NBlockId, index: IndexExpr<'_>) -> bool {
        match index {
            IndexExpr::Iteration(inner) => self.enclosing(inner).any(|outer| outer == region),
            IndexExpr::Runtime(value) => self.regions[&region].values.contains(&value),
            _ => false,
        }
    }

    /// Whether crossing feedback of `region` drops facts about this index. An
    /// inner latch keeps the enclosing body's values, but facts about them are
    /// still dropped for the whole nest: carrying them through inner iterations
    /// grows guards with the arithmetic relating inner and outer indices.
    pub fn drops_fact(&self, region: NBlockId, index: IndexExpr<'_>) -> bool {
        self.repeats_index(self.outermost(region), index)
    }

    pub fn repeats_occurrence(&self, region: NBlockId, occurrence: ValueOccurrence) -> bool {
        match occurrence {
            ValueOccurrence::Value(value) | ValueOccurrence::CallChoice { result: value, .. } => {
                self.regions[&region].values.contains(&value)
            }
            ValueOccurrence::LoopEntry(block) => self.blocks[block.index()]
                .is_some_and(|inner| self.enclosing(inner).any(|outer| outer == region)),
            ValueOccurrence::Root(_) => true,
            ValueOccurrence::Argument(_)
            | ValueOccurrence::Summary
            | ValueOccurrence::SummaryChoice(_) => false,
        }
    }
}

fn definition_block(body: &NormalizedBody<'_>, value: NValueId) -> Option<NBlockId> {
    match body.values[value.index()].definition {
        NValueDefinition::EntryParam { .. } => None,
        NValueDefinition::BlockParam { block, .. } | NValueDefinition::Statement { block, .. } => {
            Some(block)
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
            for (from, to) in loops.feedback.keys() {
                let region = loops.feedback(*from, *to).unwrap();
                assert!(loops.regions[&region].values.is_empty());
            }
            let mut checker =
                Borrowck::new_with_body(&db, instance, body, BorrowSummaryMode::Final).unwrap();
            checker.solve().unwrap();
            let (summary, _) = checker.build_summary().unwrap();
            assert_eq!(summary.may_return, returns);
            assert!(summary.availability.unavailable.is_empty());
        }
    }
    #[test]
    fn inner_feedback_renews_only_its_own_loop() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "nested.fe".into(),
            "fn nest(n: usize) {\n    let mut x: usize = 0\n    while x < n {\n        \
             let mut y: usize = 0\n        while y < n {\n            y += 1\n        }\n        \
             x += 1\n    }\n}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "nest"))),
        );
        let body = normalize_semantic_body(&db, instance).unwrap().body;
        let loops = LoopRegions::new(&body);
        let [inner, outer] = loops
            .feedback
            .values()
            .copied()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .sorted_by_key(|region| loops.regions[region].parent.is_none())
            .collect::<Vec<_>>()[..]
        else {
            panic!("two nested regions");
        };
        assert_eq!(loops.regions[&inner].parent, Some(outer));
        assert_eq!(loops.outermost(inner), outer);
        let inner_values = &loops.regions[&inner].values;
        let outer_values = &loops.regions[&outer].values;
        assert!(inner_values.is_subset(outer_values) && inner_values != outer_values);
        // A value of the outer header is current throughout the inner loop.
        let header = outer_values
            .iter()
            .copied()
            .find(|value| definition_block(&body, *value) == Some(outer))
            .unwrap();
        assert_eq!(loops.for_value(&body, header), Some(outer));
        assert!(!loops.repeats_index(inner, IndexExpr::Runtime(header)));
        assert!(loops.repeats_index(outer, IndexExpr::Runtime(header)));
        assert!(loops.drops_fact(inner, IndexExpr::Runtime(header)));
        assert!(loops.repeats_index(outer, IndexExpr::Iteration(inner)));
        assert!(!loops.repeats_index(inner, IndexExpr::Iteration(outer)));
        assert!(loops.repeats_occurrence(inner, ValueOccurrence::LoopEntry(inner)));
        assert!(loops.repeats_occurrence(outer, ValueOccurrence::LoopEntry(inner)));
        assert!(!loops.repeats_occurrence(inner, ValueOccurrence::LoopEntry(outer)));
        // An occurrence made before the inner loop names no value it defines.
        let arguments = loops.arguments(&body, header);
        assert_eq!(arguments[0], IndexExpr::Iteration(outer));
        assert!(arguments.contains(&IndexExpr::Runtime(header)));
        assert!(
            inner_values
                .iter()
                .all(|value| !arguments.contains(&IndexExpr::Runtime(*value)))
        );
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
        assert_eq!(
            loops.regions[&region].values,
            BTreeSet::from([NValueId::new(1)])
        );
        assert_eq!(loops.for_value(&body, NValueId::new(0)), None);
        let mut checker =
            Borrowck::new_with_body(&db, instance, body, BorrowSummaryMode::Final).unwrap();
        checker.solve().unwrap();
        assert!(!checker.build_summary().unwrap().0.may_return);
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
        // Each cyclic component is exactly the outermost region of its blocks.
        let mut outermost = BTreeMap::new();
        for (index, targets) in reach.iter().enumerate() {
            let expected = targets.contains(&index).then(|| {
                NBlockId::new(
                    *targets
                        .iter()
                        .find(|target| reach[**target].contains(&index))
                        .unwrap(),
                )
            });
            let region = loops.blocks[index];
            assert_eq!(
                region.is_some(),
                expected.is_some(),
                "{edges:?}, entry {:?}",
                body.entry
            );
            if let (Some(component), Some(region)) = (expected, region) {
                let outer = loops.outermost(region);
                assert_eq!(
                    *outermost.entry(component).or_insert(outer),
                    outer,
                    "{edges:?}, entry {:?}",
                    body.entry
                );
                assert!(loops.regions[&region].values.is_empty());
            }
        }
        let components: BTreeSet<_> = outermost.values().collect();
        assert_eq!(components.len(), outermost.len(), "{edges:?}");
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
                            .contains_key(&(NBlockId::new(from), NBlockId::new(*to))),
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

    #[test]
    fn validated_natural_loops_require_a_dominating_single_entry_header() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "natural_loops.fe".into(),
            "enum Choice { A, B, C, D }\nimpl Copy for Choice {}\nfn anchor(_ choice: own Choice) {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "anchor"))),
        );
        let template = normalize_semantic_body(&db, instance).unwrap().body;
        let edge = |from, successor, to| LoopEdge {
            from: NBlockId::new(from),
            successor,
            to: NBlockId::new(to),
        };
        let blocks = |ids: &[usize]| ids.iter().copied().map(NBlockId::new).collect();
        let cases = [
            (
                vec![vec![1], vec![2, 3], vec![1], vec![]],
                vec![ValidatedLoop {
                    header: NBlockId::new(1),
                    blocks: blocks(&[1, 2]),
                    entries: vec![edge(0, 0, 1)],
                    backedges: vec![edge(2, 0, 1)],
                    exits: vec![edge(1, 1, 3)],
                }],
            ),
            // Two entries into the cycle leave no dominating header.
            (vec![vec![1, 2], vec![2], vec![1]], vec![]),
            (
                vec![vec![1], vec![2, 3, 4], vec![1], vec![1], vec![]],
                vec![ValidatedLoop {
                    header: NBlockId::new(1),
                    blocks: blocks(&[1, 2, 3]),
                    entries: vec![edge(0, 0, 1)],
                    backedges: vec![edge(2, 0, 1), edge(3, 0, 1)],
                    exits: vec![edge(1, 2, 4)],
                }],
            ),
        ];
        for (edges, expected) in cases {
            let body = graph_body(&template, &edges, 0);
            verify_normalized_body(&db, &body).unwrap();
            assert_eq!(validated_loops(&body).unwrap(), expected);
        }
    }
}
