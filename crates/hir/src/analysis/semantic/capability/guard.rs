//! Canonical guards, including unions, over semantic indices and scoped enum choices.
//!
//! Index conditions use reduced bit decisions over Fe's 256-bit `usize`, so equality,
//! disequality, and bounds share one Boolean algebra. Enum decisions have index conditions
//! as leaves. Neither graph enumerates array elements or depends on construction order.
use std::{
    cmp::{Ordering, Reverse},
    collections::{BTreeMap, BTreeSet},
};

use super::{
    decision::{Decision, Variable},
    index::{BinderScope, IndexExpr, IndexSubst},
    path::{Projection, StructuralPath},
};
use crate::analysis::semantic::{
    VariantIndex,
    normalized::{NRootId, NValueId},
};

const INDEX_BITS: u16 = 256;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueOccurrence {
    Value(NValueId),
    Root(NRootId),
    Argument(u32),
    Summary,
    SummaryChoice(u32),
    CallChoice { result: NValueId, choice: u32 },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChoiceKey<'db> {
    occurrence: ValueOccurrence,
    path: StructuralPath<IndexExpr<'db>>,
}

impl<'db> ChoiceKey<'db> {
    pub fn new(occurrence: ValueOccurrence, path: StructuralPath<IndexExpr<'db>>) -> Self {
        Self { occurrence, path }
    }
    fn alias_condition(&self, other: &Self) -> Option<IndexCondition<'db>> {
        if self.occurrence != other.occurrence
            || self.path.as_slice().len() != other.path.as_slice().len()
        {
            return None;
        }
        let mut condition = IndexCondition::always();
        for (left, right) in self.path.as_slice().iter().zip(other.path.as_slice()) {
            match (left, right) {
                (Projection::Index(left), Projection::Index(right)) => {
                    condition = condition.and(&IndexCondition::equal(*left, *right));
                }
                (left, right) if left == right => {}
                _ => return None,
            }
        }
        (!condition.is_never()).then_some(condition)
    }

    fn map_indices(&self, map: impl FnMut(&IndexExpr<'db>) -> IndexExpr<'db>) -> Self {
        Self {
            occurrence: self.occurrence,
            path: self.path.map_indices(map),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct IndexBit<'db> {
    // Interleaving words avoids an exponential equality graph.
    bit: Reverse<u16>,
    index: IndexExpr<'db>,
}

impl<'db> IndexBit<'db> {
    fn new(index: IndexExpr<'db>, bit: u16) -> Self {
        Self {
            index,
            bit: Reverse(bit),
        }
    }
    fn substitute(&self, subst: &IndexSubst<'db>) -> Variable<Self> {
        match subst.apply(self.index) {
            IndexExpr::Const(value) => Variable::Constant(constant_bit(value, self.bit.0)),
            index => Variable::Symbol(Self::new(index, self.bit.0)),
        }
    }
}

fn constant_bit(value: usize, bit: u16) -> bool {
    value
        .checked_shr(u32::from(bit))
        .is_some_and(|value| value & 1 != 0)
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct IndexCondition<'db>(Decision<IndexBit<'db>, bool>);

impl<'db> IndexCondition<'db> {
    fn always() -> Self {
        Self(Decision::leaf(true))
    }
    fn never() -> Self {
        Self(Decision::leaf(false))
    }
    fn is_never(&self) -> bool {
        self.0.is_leaf(&false)
    }

    fn equal(lhs: IndexExpr<'db>, rhs: IndexExpr<'db>) -> Self {
        let (lhs, rhs) = if lhs <= rhs { (lhs, rhs) } else { (rhs, lhs) };
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => Self::always(),
            (IndexExpr::Const(_), IndexExpr::Const(_)) => Self::never(),
            (IndexExpr::Const(value), index) => Self(Decision::chain(
                (0..INDEX_BITS).map(|bit| (IndexBit::new(index, bit), constant_bit(value, bit))),
                true,
                false,
            )),
            (lhs, rhs) => Self(Decision::equal_bits(
                (0..INDEX_BITS).map(|bit| (IndexBit::new(lhs, bit), IndexBit::new(rhs, bit))),
                true,
                false,
            )),
        }
    }

    fn bounded(index: IndexExpr<'db>, len: IndexExpr<'db>) -> Self {
        if index == len {
            return Self::never();
        }
        if let (IndexExpr::Const(value), IndexExpr::Const(len)) = (index, len) {
            return if value < len {
                Self::always()
            } else {
                Self::never()
            };
        }
        if let IndexExpr::Const(len) = len {
            return Self(Decision::upper_bound_bits(
                (0..INDEX_BITS).map(|bit| (IndexBit::new(index, bit), constant_bit(len, bit))),
            ));
        }
        Self(Decision::less_bits((0..INDEX_BITS).map(|bit| {
            let word_bit = |index| match index {
                IndexExpr::Const(value) => Variable::Constant(constant_bit(value, bit)),
                index => Variable::Symbol(IndexBit::new(index, bit)),
            };
            (word_bit(index), word_bit(len))
        })))
    }

    fn and(&self, other: &Self) -> Self {
        if self == other || other.0.is_leaf(&true) {
            return self.clone();
        }
        if self.0.is_leaf(&true) {
            return other.clone();
        }
        if self.is_never() || other.is_never() {
            return Self::never();
        }
        Self(self.0.apply(&other.0, |left, right| *left && *right))
    }

    fn or(&self, other: &Self) -> Self {
        if self == other || other.is_never() {
            return self.clone();
        }
        if self.is_never() {
            return other.clone();
        }
        if self.0.is_leaf(&true) || other.0.is_leaf(&true) {
            return Self::always();
        }
        Self(self.0.apply(&other.0, |left, right| *left || *right))
    }

    fn not(&self) -> Self {
        Self(
            self.0
                .map(|bit| Variable::Symbol(bit.clone()), |value| !value),
        )
    }
    fn implies(&self, other: &Self) -> bool {
        self.and(&other.not()).is_never()
    }
    fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
        Self(self.0.map(|bit| bit.substitute(subst), |value| *value))
    }
    fn indices(&self) -> BTreeSet<IndexExpr<'db>> {
        self.0
            .variables()
            .into_iter()
            .map(|bit| bit.index)
            .collect()
    }

    fn restrict(&self, care: &Self) -> Option<Self> {
        self.0
            .restrict(&care.0, &false, |value, care| care.then_some(*value))
            .map(Self)
    }

    // Find a representative only when the complete condition proves equality. Partial
    // known bits and coincident numeric IDs never establish index correlation.
    fn representatives(
        &self,
        indices: &BTreeSet<IndexExpr<'db>>,
    ) -> BTreeMap<IndexExpr<'db>, IndexExpr<'db>> {
        let mut representatives = BTreeMap::new();
        let witness = self.0.witness(|value| *value).unwrap_or_default();
        for index in indices {
            if matches!(index, IndexExpr::Const(_)) {
                continue;
            }
            let mut candidate = Some(0usize);
            for (bit, value) in &witness {
                if bit.index == *index && *value {
                    candidate = candidate.and_then(|candidate| {
                        1usize
                            .checked_shl(u32::from(bit.bit.0))
                            .map(|bit| candidate | bit)
                    });
                }
            }
            if let Some(candidate) = candidate
                && self.implies(&Self::equal(*index, IndexExpr::Const(candidate)))
            {
                representatives.insert(*index, IndexExpr::Const(candidate));
                continue;
            }
            if let Some(previous) = indices
                .range(..*index)
                .find(|previous| self.implies(&Self::equal(**previous, *index)))
            {
                representatives.insert(
                    *index,
                    representatives.get(previous).copied().unwrap_or(*previous),
                );
            }
        }
        representatives
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ChoiceBit<'db> {
    bit: Reverse<u16>,
    choice: ChoiceKey<'db>,
}

impl Ord for ChoiceBit<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Interleaving unrelated enum words makes unions of tag tests exponential.
        // Keep each independent occurrence/structural slot together. Within one
        // slot, indexed selections may alias, so interleave their bits to keep
        // the exact tag-equality relations in Guard::canonical compact as well.
        self.choice
            .occurrence
            .cmp(&other.choice.occurrence)
            .then_with(|| {
                self.choice
                    .path
                    .as_slice()
                    .iter()
                    .map(|step| step.map_index(|_| ()))
                    .cmp(
                        other
                            .choice
                            .path
                            .as_slice()
                            .iter()
                            .map(|step| step.map_index(|_| ())),
                    )
            })
            .then_with(|| self.bit.cmp(&other.bit))
            .then_with(|| self.choice.path.cmp(&other.choice.path))
    }
}

impl PartialOrd for ChoiceBit<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

type Condition<'db> = Decision<ChoiceBit<'db>, IndexCondition<'db>>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Guard<'db> {
    scope: BinderScope,
    condition: Condition<'db>,
}

impl<'db> Guard<'db> {
    pub fn always(scope: &BinderScope) -> Self {
        Self {
            scope: scope.clone(),
            condition: Decision::leaf(IndexCondition::always()),
        }
    }
    pub fn scope(&self) -> &BinderScope {
        &self.scope
    }

    pub fn and(&self, other: &Self) -> Option<Self> {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        Self::canonical(
            &self.scope,
            self.condition.apply(&other.condition, IndexCondition::and),
        )
    }

    pub fn or(&self, other: &Self) -> Self {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        Self::canonical(
            &self.scope,
            self.condition.apply(&other.condition, IndexCondition::or),
        )
        .expect("a union of satisfiable guards is satisfiable")
    }

    pub fn with_equality(&self, lhs: IndexExpr<'db>, rhs: IndexExpr<'db>) -> Option<Self> {
        self.scope.validate(lhs).expect("free equality binder");
        self.scope.validate(rhs).expect("free equality binder");
        self.with_index_condition(&IndexCondition::equal(lhs, rhs))
    }

    pub fn with_disequality(&self, lhs: IndexExpr<'db>, rhs: IndexExpr<'db>) -> Option<Self> {
        self.scope.validate(lhs).expect("free disequality binder");
        self.scope.validate(rhs).expect("free disequality binder");
        self.with_index_condition(&IndexCondition::equal(lhs, rhs).not())
    }

    pub fn with_bound(
        &self,
        index: IndexExpr<'db>,
        len: impl Into<IndexExpr<'db>>,
    ) -> Option<Self> {
        self.scope.validate(index).expect("free bound binder");
        let len = len.into();
        self.scope.validate(len).expect("free bound length binder");
        self.with_index_condition(&IndexCondition::bounded(index, len))
    }

    pub fn with_variant(&self, choice: ChoiceKey<'db>, variant: VariantIndex) -> Option<Self> {
        for index in choice.path.indices() {
            self.scope.validate(index).expect("free enum choice binder");
        }
        let condition = Decision::chain(
            (0..u16::BITS as u16).map(|bit| {
                (
                    ChoiceBit {
                        choice: choice.clone(),
                        bit: Reverse(bit),
                    },
                    variant.0 & (1 << bit) != 0,
                )
            }),
            IndexCondition::always(),
            IndexCondition::never(),
        );
        Self::canonical(
            &self.scope,
            self.condition.apply(&condition, IndexCondition::and),
        )
    }

    pub fn substitute(&self, subst: &IndexSubst<'db>) -> Option<Self> {
        assert_eq!(
            &self.scope,
            subst.source(),
            "substitution source scope must match"
        );
        Self::canonical(
            subst.destination(),
            self.condition.map(
                |bit| {
                    Variable::Symbol(ChoiceBit {
                        choice: bit.choice.map_indices(|index| subst.apply(*index)),
                        bit: bit.bit,
                    })
                },
                |condition| condition.substitute(subst),
            ),
        )
    }

    pub fn in_scope(&self, scope: &BinderScope) -> Self {
        self.substitute(&IndexSubst::new(&self.scope, scope, []).expect("guard scope extension"))
            .expect("scope extension preserves satisfiability")
    }

    pub fn occurrences(&self) -> BTreeSet<ValueOccurrence> {
        self.condition
            .variables()
            .iter()
            .map(|bit| bit.choice.occurrence)
            .collect()
    }

    pub fn map_occurrences(
        &self,
        mut map: impl FnMut(ValueOccurrence) -> ValueOccurrence,
    ) -> Option<Self> {
        let occurrences: BTreeSet<_> = self
            .condition
            .variables()
            .into_iter()
            .map(|bit| bit.choice.occurrence)
            .collect();
        let mappings: BTreeMap<_, _> = occurrences
            .into_iter()
            .map(|occurrence| (occurrence, map(occurrence)))
            .collect();
        Self::canonical(
            &self.scope,
            self.condition.map(
                |bit| {
                    Variable::Symbol(ChoiceBit {
                        choice: ChoiceKey::new(
                            mappings[&bit.choice.occurrence],
                            bit.choice.path.clone(),
                        ),
                        bit: bit.bit,
                    })
                },
                Clone::clone,
            ),
        )
    }

    /// A new execution of a loop may choose another enum alternative.
    pub fn forget_occurrences(&self, mut repeated: impl FnMut(ValueOccurrence) -> bool) -> Self {
        Self::canonical(
            &self.scope,
            self.condition
                .exists(|bit| repeated(bit.choice.occurrence), IndexCondition::or),
        )
        .expect("existential quantification preserves feasibility")
    }

    /// Forget old scalar selectors without identifying them with a new execution.
    pub fn forget_indices(&self, mut repeated: impl FnMut(IndexExpr<'db>) -> bool) -> Self {
        let indices: BTreeSet<_> = self
            .indices()
            .into_iter()
            .filter(|index| repeated(*index))
            .collect();
        let condition = self
            .condition
            .exists(
                |bit| {
                    bit.choice
                        .path
                        .indices()
                        .any(|index| indices.contains(&index))
                },
                IndexCondition::or,
            )
            .map(
                |bit| Variable::Symbol(bit.clone()),
                |condition| {
                    IndexCondition(condition.0.exists(
                        |bit| indices.contains(&bit.index),
                        |left, right| *left || *right,
                    ))
                },
            );
        Self::canonical(&self.scope, condition)
            .expect("existential quantification preserves feasibility")
    }

    pub fn difference(&self, other: &Self) -> Option<Self> {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        Self::canonical(
            &self.scope,
            self.condition
                .apply(&other.condition, |left, right| left.and(&right.not())),
        )
    }

    /// Eliminate clause-local witnesses that are observable only through scalar
    /// constraints. A witness indexing an enum choice remains observable: its
    /// quantification would require also quantifying that indexed choice.
    pub fn project_witnesses(&self, hidden: impl Fn(IndexExpr<'db>) -> bool) -> Self {
        let indexed: BTreeSet<_> = self
            .condition
            .variables()
            .into_iter()
            .flat_map(|bit| bit.choice.path.indices().collect::<Vec<_>>())
            .collect();
        Self::canonical(
            &self.scope,
            self.condition.map(
                |bit| Variable::Symbol(bit.clone()),
                |condition| {
                    IndexCondition(condition.0.exists(
                        |bit| hidden(bit.index) && !indexed.contains(&bit.index),
                        |left, right| *left || *right,
                    ))
                },
            ),
        )
        .expect("existential projection preserves feasibility")
    }

    pub fn implies(&self, other: &Self) -> bool {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        self.condition
            .apply(&other.condition, |left, right| left.and(&right.not()))
            .is_leaf(&IndexCondition::never())
    }

    pub fn proves_equal(&self, lhs: IndexExpr<'db>, rhs: IndexExpr<'db>) -> bool {
        self.scope.validate(lhs).expect("free equality binder");
        self.scope.validate(rhs).expect("free equality binder");
        let equality = IndexCondition::equal(lhs, rhs);
        self.condition
            .leaves()
            .all(|condition| condition.implies(&equality))
    }

    pub fn indices(&self) -> BTreeSet<IndexExpr<'db>> {
        let mut indices: BTreeSet<_> = self
            .condition
            .leaves()
            .flat_map(IndexCondition::indices)
            .collect();
        for bit in self.condition.variables() {
            indices.extend(bit.choice.path.indices());
        }
        indices
    }

    pub fn node_count(&self) -> usize {
        self.condition.node_count()
            + self
                .condition
                .leaves()
                .map(|leaf| leaf.0.node_count())
                .sum::<usize>()
    }

    fn with_index_condition(&self, condition: &IndexCondition<'db>) -> Option<Self> {
        Self::canonical(
            &self.scope,
            self.condition.map(
                |bit| Variable::Symbol(bit.clone()),
                |old| old.and(condition),
            ),
        )
    }

    // Indexed choice keys must also respect equalities proved by their index condition.
    // Identifying choice bits rebuilds the ordered graph and rejects conflicting variants;
    // no map collection is allowed to overwrite a contradictory requirement.
    fn canonical(scope: &BinderScope, condition: Condition<'db>) -> Option<Self> {
        if condition.is_leaf(&IndexCondition::never()) {
            return None;
        }
        if condition.variables().iter().all(|bit| {
            bit.choice
                .path
                .indices()
                .all(|index| matches!(index, IndexExpr::Const(_)))
        }) {
            return Some(Self {
                scope: scope.clone(),
                condition,
            });
        }
        let choice_indices: BTreeSet<_> = condition
            .variables()
            .iter()
            .flat_map(|bit| bit.choice.path.indices())
            .collect();
        let mut canonical = Decision::leaf(IndexCondition::never());
        // Partition by distinct terminal conditions, not graph paths: shared suffixes
        // can represent exponentially many paths through a compact decision graph.
        for indices in condition.leaves().filter(|indices| !indices.is_never()) {
            let terms = choice_indices
                .iter()
                .copied()
                .chain(indices.indices())
                .collect();
            let representatives = indices.representatives(&terms);
            let alternative = condition.map(
                |bit| {
                    Variable::Symbol(ChoiceBit {
                        choice: bit.choice.map_indices(|index| {
                            representatives.get(index).copied().unwrap_or(*index)
                        }),
                        bit: bit.bit,
                    })
                },
                |leaf| {
                    if leaf == indices {
                        leaf.clone()
                    } else {
                        IndexCondition::never()
                    }
                },
            );
            canonical = canonical.apply(&alternative, IndexCondition::or);
        }
        // Choice occurrences at equal indices must have equal tags. Complete the
        // graph outside these feasible valuations so equality partitions can reunite
        // without retaining a spurious dependence on an extra indexed choice.
        let choices: BTreeSet<_> = canonical
            .variables()
            .into_iter()
            .map(|bit| bit.choice)
            .collect();
        let mut care = Decision::leaf(IndexCondition::always());
        for (position, left) in choices.iter().enumerate() {
            for right in choices.iter().skip(position + 1) {
                if let Some(alias) = left.alias_condition(right) {
                    let equality = Decision::equal_bits(
                        (0..u16::BITS as u16).map(|bit| {
                            (
                                ChoiceBit {
                                    bit: Reverse(bit),
                                    choice: left.clone(),
                                },
                                ChoiceBit {
                                    bit: Reverse(bit),
                                    choice: right.clone(),
                                },
                            )
                        }),
                        IndexCondition::always(),
                        alias.not(),
                    );
                    care = care.apply(&equality, IndexCondition::and);
                }
            }
        }
        let canonical =
            canonical.restrict(&care, &IndexCondition::never(), IndexCondition::restrict)?;
        (!canonical.is_leaf(&IndexCondition::never())).then(|| Self {
            scope: scope.clone(),
            condition: canonical,
        })
    }
}
