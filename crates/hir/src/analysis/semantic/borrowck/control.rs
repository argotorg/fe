//! Static loop regions and iteration-qualified capability occurrences.
use std::collections::{BTreeMap, BTreeSet};

use cranelift_entity::EntityRef;

use crate::analysis::semantic::{
    capability::{guard::ValueOccurrence, index::IndexExpr},
    normalized::{NBlockId, NValueDefinition, NValueId, NormalizedBody},
};

pub(super) struct LoopRegions {
    blocks: Vec<Option<NBlockId>>,
    values: BTreeMap<NBlockId, BTreeSet<NValueId>>,
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
        let mut order = Vec::new();
        for first in 0..edges.len() {
            let mut pending = vec![(first, false)];
            while let Some((block, finished)) = pending.pop() {
                if finished {
                    order.push(block);
                    continue;
                }
                if std::mem::replace(&mut seen[block], true) {
                    continue;
                }
                pending.push((block, true));
                pending.extend(edges[block].iter().map(|next| (*next, false)));
            }
        }
        let mut assigned = vec![false; edges.len()];
        let mut blocks = vec![None; edges.len()];
        for first in order.into_iter().rev() {
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
        let mut values: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
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
        Self { blocks, values }
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
                std::iter::once(IndexExpr::Iteration(region))
                    .chain(self.values[&region].iter().copied().map(IndexExpr::Runtime))
            })
            .collect()
    }

    pub fn feedback(&self, from: NBlockId, to: NBlockId) -> Option<NBlockId> {
        let region = self.blocks[from.index()]?;
        (self.blocks[to.index()] == Some(region) && to.index() <= from.index()).then_some(region)
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
