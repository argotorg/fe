use cranelift_entity::{EntityRef, SecondaryMap};
use smallvec::SmallVec;

use crate::analysis::semantic::normalized::{
    NBlockId, NDataProjection, NIndex, NPlace, NPlaceBase, NStatementKind, NTerminatorKind,
    NValueId, NormalizedBody,
};

pub type NValueUseList = SmallVec<NValueId, 4>;

#[derive(Clone, Debug, Default)]
struct NStatementFacts {
    uses: NValueUseList,
}

#[derive(Clone, Debug)]
pub struct NormalizedBodyFacts {
    statement_facts: Vec<Vec<NStatementFacts>>,
    terminator_uses: SecondaryMap<NBlockId, NValueUseList>,
}

impl NormalizedBodyFacts {
    pub fn new(body: &NormalizedBody<'_>) -> Self {
        let statement_facts = body
            .blocks
            .iter()
            .map(|block| {
                block
                    .statements
                    .iter()
                    .map(|statement| NStatementFacts {
                        uses: statement_used_values(&statement.kind),
                    })
                    .collect()
            })
            .collect();
        let mut terminator_uses = SecondaryMap::new();
        terminator_uses.resize(body.blocks.len());
        for (block, data) in body.blocks.iter().enumerate() {
            terminator_uses[NBlockId::new(block)] = terminator_used_values(&data.terminator.kind);
        }
        Self {
            statement_facts,
            terminator_uses,
        }
    }

    pub fn statement_uses(&self, block: NBlockId, statement: usize) -> &[NValueId] {
        self.statement_facts
            .get(block.index())
            .and_then(|facts| facts.get(statement))
            .map(|facts| facts.uses.as_slice())
            .unwrap_or_default()
    }

    pub fn terminator_uses(&self, block: NBlockId) -> &[NValueId] {
        self.terminator_uses
            .get(block)
            .map(SmallVec::as_slice)
            .unwrap_or_default()
    }
}

fn statement_used_values(statement: &NStatementKind<'_>) -> NValueUseList {
    let mut uses = NValueUseList::new();
    match statement {
        NStatementKind::Define { expr, .. } => {
            expr.for_each_value_operand(|operand| push_unique(&mut uses, operand.value));
            expr.for_each_place_operand(|place| extend_unique(&mut uses, place_used_values(place)));
        }
        NStatementKind::Store { destination, value } => {
            extend_unique(&mut uses, place_used_values(destination));
            push_unique(&mut uses, value.value);
        }
    }
    uses
}

fn place_used_values(place: &NPlace<'_>) -> NValueUseList {
    let mut uses = NValueUseList::new();
    if let NPlaceBase::CapabilityTarget { carrier } = place.base {
        push_unique(&mut uses, carrier);
    }
    for projection in place.path.iter() {
        if let NDataProjection::Index(NIndex::Value(value)) = projection {
            push_unique(&mut uses, *value);
        }
    }
    uses
}

fn terminator_used_values(terminator: &NTerminatorKind<'_>) -> NValueUseList {
    let mut uses = NValueUseList::new();
    match terminator {
        NTerminatorKind::Goto(target) => {
            extend_unique(&mut uses, target.args.iter().map(|operand| operand.value));
        }
        NTerminatorKind::Branch {
            cond,
            then_target,
            else_target,
        } => {
            push_unique(&mut uses, cond.value);
            extend_unique(
                &mut uses,
                then_target
                    .args
                    .iter()
                    .chain(else_target.args.iter())
                    .map(|operand| operand.value),
            );
        }
        NTerminatorKind::MatchEnum {
            value,
            cases,
            default,
            ..
        } => {
            push_unique(&mut uses, value.value);
            extend_unique(
                &mut uses,
                cases
                    .iter()
                    .flat_map(|(_, target)| target.args.iter())
                    .chain(default.iter().flat_map(|target| target.args.iter()))
                    .map(|operand| operand.value),
            );
        }
        NTerminatorKind::Return(Some(value)) => push_unique(&mut uses, value.value),
        NTerminatorKind::Assert { .. } | NTerminatorKind::Return(None) => {}
    }
    uses
}

fn extend_unique<T: Copy + PartialEq, const N: usize>(
    values: &mut SmallVec<T, N>,
    additional: impl IntoIterator<Item = T>,
) {
    for value in additional {
        push_unique(values, value);
    }
}

fn push_unique<T: Copy + PartialEq, const N: usize>(values: &mut SmallVec<T, N>, value: T) {
    if !values.contains(&value) {
        values.push(value);
    }
}
