//! Canonical guards, including unions, over semantic indices and scoped enum choices.
//!
//! Index conditions use reduced bit decisions over Fe's 256-bit `usize`, so equality,
//! disequality, and bounds share one Boolean algebra. Enum decisions have index conditions
//! as leaves. Neither graph enumerates array elements or depends on construction order.
use itertools::{EitherOrBoth, Itertools};
use rustc_hash::{FxHashMap, FxHasher};
#[cfg(test)]
use std::cell::Cell;
use std::{
    cell::RefCell,
    cmp::{Ordering, Reverse},
    collections::{BTreeMap, BTreeSet},
    hash::{Hash, Hasher},
    iter,
    sync::{Arc, Weak},
    thread::LocalKey,
};

#[cfg(feature = "borrowck-profile")]
use super::decision::SubgraphCounter;

use super::{
    decision::{Decision, Variable, WeakDecision},
    index::{BinderScope, IndexExpr, IndexSubst},
    path::{Projection, StructuralPath},
};
use crate::analysis::semantic::{
    VariantIndex,
    normalized::{NRootId, NValueId},
};

const INDEX_BITS: u16 = 256;

#[cfg(test)]
thread_local! {
    static RESTRICTIONS: Cell<usize> = const { Cell::new(0) };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ValueOccurrence {
    Value(NValueId),
    Root(NRootId),
    Argument(u32),
    Summary,
    SummaryChoice(u32),
    CallChoice { result: NValueId, choice: u32 },
}

impl Ord for ValueOccurrence {
    fn cmp(&self, other: &Self) -> Ordering {
        // Keep callee-local observations next to the caller values at that call.
        // Separating all Value and CallChoice occurrences makes unions of related
        // conditions exponential, including after they become SummaryChoices.
        // Within a call, put its branches before the observed aggregate fields.
        // Otherwise mutually exclusive accesses to independent fields encode
        // every subset of those fields before the branch can select one.
        let key = |occurrence: &Self| match *occurrence {
            Self::Value(value) => (0, value.as_u32(), true, 0),
            Self::CallChoice { result, choice } => (0, result.as_u32(), false, choice),
            Self::Root(root) => (1, root.as_u32(), false, 0),
            Self::Argument(argument) => (2, argument, false, 0),
            Self::Summary => (3, 0, false, 0),
            Self::SummaryChoice(choice) => (4, choice, false, 0),
        };
        key(self).cmp(&key(other))
    }
}

impl PartialOrd for ValueOccurrence {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
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

/// A bit of the index in one slot of a condition's sorted index table.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SlotBit {
    // Interleaving words avoids an exponential equality graph.
    bit: Reverse<u16>,
    slot: u16,
}

impl SlotBit {
    fn remap(self, slots: &[u16]) -> Self {
        if slots.is_empty() {
            self
        } else {
            Self {
                slot: slots[usize::from(self.slot)],
                ..self
            }
        }
    }

    fn new(slot: usize, bit: u16) -> Self {
        Self {
            bit: Reverse(bit),
            slot: u16::try_from(slot).expect("index condition table fits a slot"),
        }
    }
}

fn constant_bit(value: usize, bit: u16) -> bool {
    value
        .checked_shr(u32::from(bit))
        .is_some_and(|value| value & 1 != 0)
}

type BitDecision = Decision<SlotBit, bool>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum SlotTarget {
    Slot(u16),
    Const(usize),
}

/// Strictly ordered slot translations into a merged index table. An empty map
/// denotes identity. Operands keep their original graphs and cache identities.
#[derive(Clone, Default, PartialEq, Eq, Hash)]
struct BitAlignment {
    left: Box<[u16]>,
    right: Box<[u16]>,
}

/// Bit-decision operations name table slots rather than indices, so equal
/// operations over different indices share one result. Guard algebra repeats
/// the same few operations across fixpoint iterations and summaries.
#[derive(Clone, PartialEq, Eq, Hash)]
enum BitOperation<G = BitDecision> {
    And(G, G, BitAlignment),
    Or(G, G, BitAlignment),
    Not(G),
    Restrict(G, G, BitAlignment),
    Substitute(G, Box<[SlotTarget]>),
    Exists(G, Box<[u16]>),
}

impl<G> BitOperation<G> {
    fn map_graphs<H>(self, mut map: impl FnMut(G) -> H) -> BitOperation<H> {
        match self {
            Self::And(left, right, slots) => BitOperation::And(map(left), map(right), slots),
            Self::Or(left, right, slots) => BitOperation::Or(map(left), map(right), slots),
            Self::Not(graph) => BitOperation::Not(map(graph)),
            Self::Restrict(source, care, slots) => {
                BitOperation::Restrict(map(source), map(care), slots)
            }
            Self::Substitute(graph, targets) => BitOperation::Substitute(map(graph), targets),
            Self::Exists(graph, slots) => BitOperation::Exists(map(graph), slots),
        }
    }
}

type WeakBitDecision = WeakDecision<SlotBit, bool>;

impl BitOperation<WeakBitDecision> {
    fn metadata_units(&self) -> usize {
        1 + match self {
            Self::And(_, _, slots) | Self::Or(_, _, slots) | Self::Restrict(_, _, slots) => {
                slots.left.len() + slots.right.len()
            }
            Self::Substitute(_, targets) => targets.len(),
            Self::Exists(_, slots) => slots.len(),
            _ => 0,
        }
    }

    fn is_live(&self) -> bool {
        match self {
            Self::And(left, right, _)
            | Self::Or(left, right, _)
            | Self::Restrict(left, right, _) => left.is_live() && right.is_live(),
            Self::Not(graph) | Self::Substitute(graph, _) | Self::Exists(graph, _) => {
                graph.is_live()
            }
        }
    }
}

/// Memoization owns only metadata. A dead result is a cache miss; a recorded
/// infeasible result remains valid while its operand identities are live.
struct BitCache<K> {
    results: FxHashMap<K, (Option<WeakBitDecision>, usize)>,
    recorded: usize,
    kept: usize,
    storage: usize,
}

impl<K> Default for BitCache<K> {
    fn default() -> Self {
        Self {
            results: FxHashMap::default(),
            recorded: 0,
            kept: 0,
            storage: 0,
        }
    }
}

impl<K: Eq + Hash> BitCache<K> {
    const LIMIT: usize = 1 << 16;

    fn get(&self, key: &K) -> Option<Option<BitDecision>> {
        match &self.results.get(key)?.0 {
            Some(result) => result.upgrade().map(Some),
            None => Some(None),
        }
    }

    fn insert(
        &mut self,
        key: K,
        result: Option<&BitDecision>,
        storage: usize,
        live: impl Fn(&K) -> bool,
    ) {
        self.recorded += 1;
        if self.recorded >= self.kept.max(4096) {
            self.sweep(live);
        }
        // Weak descriptors retain no graph buffers, but slot mappings still own
        // storage. Bound it without repeatedly sweeping or discarding live hits.
        let previous = self.results.get(&key).map_or(0, |(_, storage)| *storage);
        if storage > Self::LIMIT || self.storage - previous + storage > Self::LIMIT {
            return;
        }
        self.results
            .insert(key, (result.map(Decision::downgrade), storage));
        self.storage = self.storage - previous + storage;
    }

    fn sweep(&mut self, live: impl Fn(&K) -> bool) {
        self.results.retain(|key, (result, _)| {
            live(key) && result.as_ref().is_none_or(WeakDecision::is_live)
        });
        self.kept = self.results.len();
        self.storage = self.results.values().map(|(_, storage)| storage).sum();
        self.recorded = 0;
    }
}

thread_local! {
    /// One backing graph per distinct bit decision, for the same reason as
    /// `SHARED_CHOICES`: an operation that reduces to a constant would otherwise
    /// keep its own copy of a graph the constants already denote.
    static SHARED_BITS: RefCell<SharedGraphs<SlotBit, bool>> = RefCell::default();
    /// One backing graph per distinct choice decision. Separate operations that
    /// complete an equal graph would otherwise each keep their own copy, and every
    /// value, region and summary holding one would keep it alive. Slots name a
    /// condition's own tables, so these carry no database lifetime and equal graphs
    /// really are interchangeable.
    static SHARED_CHOICES: RefCell<SharedGraphs<SlotChoice, u32>> = RefCell::default();
    static CONSTANT_BITS: [BitDecision; 2] = [Decision::leaf(false), Decision::leaf(true)];
    static BIT_OPERATIONS: RefCell<BitCache<BitOperation<WeakBitDecision>>> = RefCell::default();
}

impl BitOperation {
    fn run(self) -> Option<BitDecision> {
        // Canonical live operands make weak pointer identity a structural cache key.
        // Retaining weak handles prevents their addresses being reused by new graphs.
        let operation = self.map_graphs(|graph| shared(&SHARED_BITS, graph));
        let key = operation.clone().map_graphs(|graph| graph.downgrade());
        if let Some(result) = BIT_OPERATIONS.with_borrow(|results| results.get(&key)) {
            return result;
        }
        let result = match &operation {
            Self::And(lhs, rhs, slots) => {
                Some(lhs.ordered_view(|bit| bit.remap(&slots.left)).apply(
                    rhs.ordered_view(|bit| bit.remap(&slots.right)),
                    |left, right| match (left, right) {
                        (Some(false), _) | (_, Some(false)) => Some(false),
                        (Some(left), Some(right)) => Some(*left && *right),
                        _ => None,
                    },
                ))
            }
            Self::Or(lhs, rhs, slots) => {
                Some(lhs.ordered_view(|bit| bit.remap(&slots.left)).apply(
                    rhs.ordered_view(|bit| bit.remap(&slots.right)),
                    |left, right| match (left, right) {
                        (Some(true), _) | (_, Some(true)) => Some(true),
                        (Some(left), Some(right)) => Some(*left || *right),
                        _ => None,
                    },
                ))
            }
            Self::Not(decision) => Some(decision.map(|bit| Variable::Symbol(*bit), |value| !value)),
            Self::Restrict(decision, care, slots) => decision
                .ordered_view(|bit| bit.remap(&slots.left))
                .restrict(
                    care.ordered_view(|bit| bit.remap(&slots.right)),
                    &false,
                    |value, care| care.then_some(*value),
                ),
            Self::Substitute(decision, targets) => Some(decision.map(
                |bit| match targets[usize::from(bit.slot)] {
                    SlotTarget::Const(value) => Variable::Constant(constant_bit(value, bit.bit.0)),
                    SlotTarget::Slot(slot) => Variable::Symbol(SlotBit { slot, ..*bit }),
                },
                |value| *value,
            )),
            Self::Exists(decision, slots) => Some(decision.exists(
                |bit| slots.contains(&bit.slot),
                |left, right| *left || *right,
            )),
        }
        .map(|decision| shared(&SHARED_BITS, decision));
        BIT_OPERATIONS.with_borrow_mut(|results| {
            let storage = key.metadata_units();
            results.insert(key, result.as_ref(), storage, BitOperation::is_live);
        });
        result
    }
}

/// Decisions that depend only on constants, never on which indices fill their
/// slots. Sharing one graph per shape also turns later equality checks on it
/// into pointer comparisons.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum BitShape {
    /// Slot 0 equals a constant.
    EqualConst(usize),
    /// Slots 0 and 1 are equal.
    Equal,
    /// Slot 0 is below a constant.
    BelowConst(usize),
}

thread_local! {
    static BIT_SHAPES: RefCell<BitCache<BitShape>> = RefCell::default();
}

impl BitShape {
    fn decision(self) -> BitDecision {
        if let Some(decision) = BIT_SHAPES.with_borrow(|shapes| shapes.get(&self).flatten()) {
            return decision;
        }
        let decision = match self {
            Self::EqualConst(value) => Decision::chain(
                (0..INDEX_BITS).map(|bit| (SlotBit::new(0, bit), constant_bit(value, bit))),
                true,
                false,
            ),
            Self::Equal => Decision::equal_bits(
                (0..INDEX_BITS).map(|bit| (SlotBit::new(0, bit), SlotBit::new(1, bit))),
                true,
                false,
            ),
            Self::BelowConst(len) => Decision::upper_bound_bits(
                (0..INDEX_BITS).map(|bit| (SlotBit::new(0, bit), constant_bit(len, bit))),
            ),
        };
        let decision = shared(&SHARED_BITS, decision);
        BIT_SHAPES.with_borrow_mut(|shapes| shapes.insert(self, Some(&decision), 1, |_| true));
        decision
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct IndexCondition<'db> {
    // Sorted and distinct: exactly the indices the decision reads.
    indices: Arc<[IndexExpr<'db>]>,
    decision: BitDecision,
}

impl Ord for IndexCondition<'_> {
    // Order as the decision over the indices themselves.
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            return Ordering::Equal;
        }
        self.decision.cmp_by(
            &other.decision,
            |left, right| {
                left.bit.cmp(&right.bit).then_with(|| {
                    self.indices[usize::from(left.slot)]
                        .cmp(&other.indices[usize::from(right.slot)])
                })
            },
            bool::cmp,
        )
    }
}

impl PartialOrd for IndexCondition<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'db> IndexCondition<'db> {
    fn constant(value: bool) -> Self {
        Self {
            indices: Arc::new([]),
            // Every guard leaf is one of these two, and a fresh leaf would be
            // another backing graph denoting the same constant.
            decision: CONSTANT_BITS.with(|bits| bits[usize::from(value)].clone()),
        }
    }
    fn always() -> Self {
        Self::constant(true)
    }
    fn never() -> Self {
        Self::constant(false)
    }
    fn is_always(&self) -> bool {
        self.decision.is_leaf(&true)
    }
    fn is_never(&self) -> bool {
        self.decision.is_leaf(&false)
    }

    /// Drop table slots the decision no longer reads.
    fn compact(indices: Arc<[IndexExpr<'db>]>, decision: BitDecision) -> Self {
        let mut used = vec![false; indices.len()];
        for bit in decision.variables() {
            used[usize::from(bit.slot)] = true;
        }
        if used.iter().all(|used| *used) {
            return Self {
                indices,
                decision: shared(&SHARED_BITS, decision),
            };
        }
        let mut next = 0;
        // An unread slot's target is never consulted.
        let targets = used
            .iter()
            .map(|used| {
                if *used {
                    next += 1;
                    SlotTarget::Slot(next - 1)
                } else {
                    SlotTarget::Const(0)
                }
            })
            .collect();
        Self {
            indices: indices
                .iter()
                .zip(&used)
                .filter_map(|(index, used)| used.then_some(*index))
                .collect(),
            decision: shared(
                &SHARED_BITS,
                BitOperation::Substitute(decision, targets).run().unwrap(),
            ),
        }
    }

    /// Express both decisions over one merged table without rebuilding operands.
    fn aligned(&self, other: &Self) -> (Arc<[IndexExpr<'db>]>, BitAlignment) {
        if self.indices == other.indices {
            return (self.indices.clone(), BitAlignment::default());
        }
        let mut indices = Vec::with_capacity(self.indices.len() + other.indices.len());
        let mut left = Vec::with_capacity(self.indices.len());
        let mut right = Vec::with_capacity(other.indices.len());
        for pair in self
            .indices
            .iter()
            .merge_join_by(other.indices.iter(), Ord::cmp)
        {
            let slot = u16::try_from(indices.len()).expect("index condition table fits a slot");
            let index = match pair {
                EitherOrBoth::Left(index) => {
                    left.push(slot);
                    index
                }
                EitherOrBoth::Right(index) => {
                    right.push(slot);
                    index
                }
                EitherOrBoth::Both(index, _) => {
                    left.push(slot);
                    right.push(slot);
                    index
                }
            };
            indices.push(*index);
        }
        for slots in [&mut left, &mut right] {
            if slots
                .iter()
                .enumerate()
                .all(|(index, slot)| index == usize::from(*slot))
            {
                slots.clear();
            }
        }
        (
            indices.into(),
            BitAlignment {
                left: left.into(),
                right: right.into(),
            },
        )
    }

    fn equal(lhs: IndexExpr<'db>, rhs: IndexExpr<'db>) -> Self {
        let (lhs, rhs) = if lhs <= rhs { (lhs, rhs) } else { (rhs, lhs) };
        match (lhs, rhs) {
            (lhs, rhs) if lhs == rhs => Self::always(),
            (IndexExpr::Const(_), IndexExpr::Const(_)) => Self::never(),
            (IndexExpr::Const(value), index) => Self {
                indices: Arc::new([index]),
                decision: BitShape::EqualConst(value).decision(),
            },
            (lhs, rhs) => Self {
                indices: Arc::new([lhs, rhs]),
                decision: BitShape::Equal.decision(),
            },
        }
    }

    fn bounded(index: IndexExpr<'db>, len: IndexExpr<'db>) -> Self {
        if index == len {
            return Self::never();
        }
        if let (IndexExpr::Const(value), IndexExpr::Const(len)) = (index, len) {
            return Self::constant(value < len);
        }
        if let IndexExpr::Const(len) = len {
            return Self::compact(Arc::new([index]), BitShape::BelowConst(len).decision());
        }
        let indices: Vec<_> = [index, len]
            .into_iter()
            .filter(|index| !matches!(index, IndexExpr::Const(_)))
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        let decision = Decision::less_bits((0..INDEX_BITS).map(|bit| {
            let word_bit = |index| match index {
                IndexExpr::Const(value) => Variable::Constant(constant_bit(value, bit)),
                index => {
                    Variable::Symbol(SlotBit::new(indices.binary_search(&index).unwrap(), bit))
                }
            };
            (word_bit(index), word_bit(len))
        }));
        Self::compact(indices.into(), decision)
    }

    fn and(&self, other: &Self) -> Self {
        if self == other || other.is_always() {
            return self.clone();
        }
        if self.is_always() {
            return other.clone();
        }
        if self.is_never() || other.is_never() {
            return Self::never();
        }
        let (indices, slots) = self.aligned(other);
        Self::compact(
            indices,
            BitOperation::And(self.decision.clone(), other.decision.clone(), slots)
                .run()
                .unwrap(),
        )
    }

    fn or(&self, other: &Self) -> Self {
        if self == other || other.is_never() {
            return self.clone();
        }
        if self.is_never() {
            return other.clone();
        }
        if self.is_always() || other.is_always() {
            return Self::always();
        }
        let (indices, slots) = self.aligned(other);
        Self::compact(
            indices,
            BitOperation::Or(self.decision.clone(), other.decision.clone(), slots)
                .run()
                .unwrap(),
        )
    }

    fn not(&self) -> Self {
        Self {
            indices: self.indices.clone(),
            decision: shared(
                &SHARED_BITS,
                BitOperation::Not(self.decision.clone()).run().unwrap(),
            ),
        }
    }
    fn implies(&self, other: &Self) -> bool {
        self.and(&other.not()).is_never()
    }
    fn substitute(&self, subst: &IndexSubst<'db>) -> Self {
        let targets: Vec<_> = self
            .indices
            .iter()
            .map(|index| subst.apply(*index))
            .collect();
        if targets.iter().eq(self.indices.iter()) {
            return self.clone();
        }
        let indices: Vec<_> = targets
            .iter()
            .filter(|index| !matches!(index, IndexExpr::Const(_)))
            .copied()
            .collect::<BTreeSet<_>>()
            .into_iter()
            .collect();
        // An order-preserving rename leaves every slot in place.
        if indices.len() == targets.len() && targets.is_sorted() {
            return Self {
                indices: indices.into(),
                decision: self.decision.clone(),
            };
        }
        let targets = targets
            .into_iter()
            .map(|index| match index {
                IndexExpr::Const(value) => SlotTarget::Const(value),
                index => {
                    SlotTarget::Slot(u16::try_from(indices.binary_search(&index).unwrap()).unwrap())
                }
            })
            .collect();
        Self::compact(
            indices.into(),
            BitOperation::Substitute(self.decision.clone(), targets)
                .run()
                .unwrap(),
        )
    }
    fn indices(&self) -> impl Iterator<Item = IndexExpr<'db>> + '_ {
        self.indices.iter().copied()
    }

    /// Existentially quantify the selected indices.
    fn project(&self, mut hidden: impl FnMut(IndexExpr<'db>) -> bool) -> Self {
        let slots: Box<[u16]> = self
            .indices
            .iter()
            .enumerate()
            .filter(|(_, index)| hidden(**index))
            .map(|(slot, _)| u16::try_from(slot).unwrap())
            .collect();
        if slots.is_empty() {
            return self.clone();
        }
        Self::compact(
            self.indices.clone(),
            BitOperation::Exists(self.decision.clone(), slots)
                .run()
                .unwrap(),
        )
    }

    fn restrict(&self, care: &Self) -> Option<Self> {
        #[cfg(test)]
        RESTRICTIONS.set(RESTRICTIONS.get() + 1);
        let (indices, slots) = self.aligned(care);
        BitOperation::Restrict(self.decision.clone(), care.decision.clone(), slots)
            .run()
            .map(|decision| Self::compact(indices, decision))
    }

    fn node_count(&self) -> usize {
        self.decision.node_count()
    }

    // Find a representative only when the complete condition proves equality. Partial
    // known bits and coincident numeric IDs never establish index correlation.
    fn representatives(
        &self,
        indices: &BTreeSet<IndexExpr<'db>>,
    ) -> BTreeMap<IndexExpr<'db>, IndexExpr<'db>> {
        let mut representatives = BTreeMap::new();
        let witness = self.decision.witness(|value| *value).unwrap_or_default();
        for index in indices {
            if matches!(index, IndexExpr::Const(_)) {
                continue;
            }
            let mut candidate = Some(0usize);
            for (bit, value) in &witness {
                if self.indices[usize::from(bit.slot)] == *index && *value {
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

/// Leaves sit behind a handle: inlining an index condition in every node made a
/// branch pay for a payload only the leaves carry.
type Leaf<'db> = Arc<IndexCondition<'db>>;

thread_local! {
    static CONSTANT_LEAVES: [Leaf<'static>; 2] =
        [Arc::new(IndexCondition::never()), Arc::new(IndexCondition::always())];
}

/// The two leaves every guard bottoms out in, shared rather than rebuilt.
fn constant_leaf<'db>(value: bool) -> Leaf<'db> {
    CONSTANT_LEAVES.with(|leaves| leaves[usize::from(value)].clone())
}

/// A choice bit in one slot of a condition's sorted choice table.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct SlotChoice {
    /// Independent occurrences and structural slots stay apart: interleaving them
    /// makes unions of unrelated tag tests exponential.
    group: u16,
    /// Within one group, bits interleave, so indexed selections that may alias keep
    /// exact tag equalities compact. This field order reproduces `ChoiceBit`'s.
    bit: Reverse<u16>,
    slot: u16,
}

/// Dense table translation, shared by eager substitutions and borrowed operations.
struct ChoiceRemap(Vec<(u16, u16)>);

impl ChoiceRemap {
    fn new(source: &[Arc<ChoiceKey<'_>>], target: &[Arc<ChoiceKey<'_>>]) -> Self {
        let groups = choice_groups(target);
        Self(
            source
                .iter()
                .map(|key| {
                    let slot = target
                        .binary_search_by(|candidate| choice_order(candidate, key))
                        .expect("every mapped choice is in the table");
                    (
                        groups[slot],
                        u16::try_from(slot).expect("choice table fits a slot"),
                    )
                })
                .collect(),
        )
    }

    fn variable(&self, bit: &SlotChoice) -> SlotChoice {
        let (group, slot) = self.0[usize::from(bit.slot)];
        SlotChoice {
            group,
            slot,
            bit: bit.bit,
        }
    }
}

/// Choice decisions name table slots rather than keys, so a decision carries no
/// database lifetime and equal graphs can be shared process-wide.
type ChoiceDecision = Decision<SlotChoice, u32>;

/// A guard's decision over enum choices, with index conditions at its leaves.
///
/// The operations are named in terms of choice keys and leaves rather than the
/// graph underneath, because that is the whole vocabulary guards need, and it is
/// what lets the graph itself name only slots.
#[derive(Clone, Debug)]
struct Condition<'db> {
    /// Sorted and distinct: exactly the choices the decision reads.
    choices: Arc<[Arc<ChoiceKey<'db>>]>,
    /// Sorted and distinct: exactly the leaves the decision reaches.
    leaves: Arc<[Leaf<'db>]>,
    decision: ChoiceDecision,
    /// Conditions nest inside interned guards and key several maps, so hash the
    /// tables once rather than on every enclosing hash.
    hash: u64,
}

/// Canonical live graphs. Buckets resolve structural hash collisions by comparing
/// complete graphs. Weak handles own only small descriptors, never node buffers.
struct SharedGraphs<V, T> {
    graphs: FxHashMap<u64, Vec<WeakDecision<V, T>>>,
    added: usize,
    kept: usize,
}

impl<V, T> Default for SharedGraphs<V, T> {
    fn default() -> Self {
        Self {
            graphs: FxHashMap::default(),
            added: 0,
            kept: 0,
        }
    }
}

impl<V: Clone + Ord + Hash, T: Clone + Eq + Hash> SharedGraphs<V, T> {
    fn intern(&mut self, decision: Decision<V, T>) -> Decision<V, T> {
        let bucket = self.graphs.entry(decision.structural_hash()).or_default();
        bucket.retain(WeakDecision::is_live);
        for candidate in bucket.iter() {
            if let Some(existing) = candidate.upgrade()
                && existing == decision
            {
                return existing;
            }
        }
        bucket.push(decision.downgrade());
        self.added += 1;
        // Only metadata survives unused graphs. Amortize sweeping it over entries,
        // not node counts, and keep live entries instead of clearing hot graphs.
        if self.added >= self.kept.max(4096) {
            self.sweep();
        }
        decision
    }

    fn sweep(&mut self) {
        self.kept = 0;
        self.graphs.retain(|_, bucket| {
            bucket.retain(WeakDecision::is_live);
            self.kept += bucket.len();
            !bucket.is_empty()
        });
        self.added = 0;
    }
}

fn shared<V: Clone + Ord + Hash, T: Clone + Eq + Hash>(
    table: &'static LocalKey<RefCell<SharedGraphs<V, T>>>,
    decision: Decision<V, T>,
) -> Decision<V, T> {
    table.with_borrow_mut(|shared| shared.intern(decision))
}

/// Operation, shape, and sharing caches hold only weak descriptors. The constant
/// roots are the only strong graph owners counted as caches here. Upgrading a
/// sharing entry temporarily adds the one diagnostic owner subtracted below.
#[cfg(feature = "borrowck-profile")]
pub(super) fn profile_subgraphs() {
    let mut cached = FxHashMap::default();
    CONSTANT_BITS.with(|graphs| {
        for graph in graphs {
            *cached.entry(graph.allocation_owners().0).or_default() += 1;
        }
    });
    SHARED_BITS.with_borrow(|shared| profile_graph_table("bit", shared, &cached));
    SHARED_CHOICES
        .with_borrow(|shared| profile_graph_table("choice", shared, &FxHashMap::default()));
}

#[cfg(feature = "borrowck-profile")]
#[allow(clippy::print_stderr)] // Opt-in subgraph census reports are intentionally written to stderr.
fn profile_graph_table<V: Clone + Ord + Hash, T: Clone + Eq + Hash>(
    name: &str,
    shared: &SharedGraphs<V, T>,
    cache_owners: &FxHashMap<usize, usize>,
) {
    let mut retained = SubgraphCounter::new();
    let mut external = (!cache_owners.is_empty()).then(SubgraphCounter::new);
    let (mut retained_nodes, mut external_nodes, mut table_only_nodes, mut largest) = (0, 0, 0, 0);
    for graph in shared
        .graphs
        .values()
        .flatten()
        .filter_map(WeakDecision::upgrade)
    {
        let (id, owners) = graph.allocation_owners();
        let nodes = graph.node_count();
        largest = largest.max(nodes);
        if owners == 1 {
            table_only_nodes += nodes;
        } else {
            retained_nodes += nodes;
            retained.record(&graph);
            if owners > 1 + cache_owners.get(&id).copied().unwrap_or(0) {
                external_nodes += nodes;
                if let Some(counter) = &mut external {
                    counter.record(&graph);
                }
            }
        }
    }
    eprintln!(
        "SUBGRAPH_PROFILE kind={name} graphs={} node_size={} retained_nodes={retained_nodes} unique_nodes={} external_nodes={external_nodes} external_unique_nodes={} table_only_nodes={table_only_nodes} largest_nodes={largest}",
        shared.graphs.values().map(Vec::len).sum::<usize>(),
        Decision::<V, T>::node_size(),
        retained.unique_nodes(),
        external
            .as_ref()
            .map_or(retained.unique_nodes(), SubgraphCounter::unique_nodes)
    );
}

/// Choices order by occurrence, then by the shape of their path with indices
/// erased, then by the path itself. Bits sort between the shape and the path,
/// which is what `SlotChoice`'s field order expresses.
fn choice_order(left: &ChoiceKey<'_>, right: &ChoiceKey<'_>) -> Ordering {
    left.occurrence
        .cmp(&right.occurrence)
        .then_with(|| choice_shape(left).cmp(choice_shape(right)))
        .then_with(|| left.path.cmp(&right.path))
}

fn choice_shape<'a>(key: &'a ChoiceKey<'_>) -> impl Iterator<Item = Projection<()>> + 'a {
    key.path
        .as_slice()
        .iter()
        .map(|step| step.map_index(|_| ()))
}

/// Group boundaries fall where the occurrence or the path shape changes.
fn choice_groups(choices: &[Arc<ChoiceKey<'_>>]) -> Arc<[u16]> {
    let mut groups = Vec::with_capacity(choices.len());
    let mut group = 0u16;
    for (position, choice) in choices.iter().enumerate() {
        if position > 0 {
            let previous = &choices[position - 1];
            if previous.occurrence != choice.occurrence
                || !choice_shape(previous).eq(choice_shape(choice))
            {
                group += 1;
            }
        }
        groups.push(group);
    }
    groups.into()
}

impl PartialEq for Condition<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
            && self.decision == other.decision
            && self.choices == other.choices
            && self.leaves == other.leaves
    }
}

impl Eq for Condition<'_> {}

impl Hash for Condition<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl Ord for Condition<'_> {
    /// Order as the decision over the choices and leaves themselves, since slots
    /// name each condition's own tables.
    fn cmp(&self, other: &Self) -> Ordering {
        if self == other {
            return Ordering::Equal;
        }
        self.decision.cmp_by(
            &other.decision,
            |left, right| {
                left.bit.cmp(&right.bit).then_with(|| {
                    choice_order(
                        &self.choices[usize::from(left.slot)],
                        &other.choices[usize::from(right.slot)],
                    )
                })
            },
            |left, right| self.leaves[*left as usize].cmp(&other.leaves[*right as usize]),
        )
    }
}

impl PartialOrd for Condition<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'db> Condition<'db> {
    /// Build a condition over the tables a decision actually reads, dropping slots
    /// and leaves it does not, so equal conditions have equal tables.
    fn compact(
        choices: Vec<Arc<ChoiceKey<'db>>>,
        leaves: Vec<Leaf<'db>>,
        decision: ChoiceDecision,
    ) -> Self {
        let mut read_choice = vec![false; choices.len()];
        for bit in decision.variables() {
            read_choice[usize::from(bit.slot)] = true;
        }
        let mut read_leaf = vec![false; leaves.len()];
        for leaf in decision.leaves() {
            read_leaf[*leaf as usize] = true;
        }
        let dense = |read: &[bool]| {
            let mut next = 0u32;
            read.iter()
                .map(|read| {
                    read.then(|| {
                        next += 1;
                        next - 1
                    })
                })
                .collect::<Vec<_>>()
        };
        let choice_slots = dense(&read_choice);
        let choices: Vec<_> = choices
            .into_iter()
            .zip(&read_choice)
            .filter_map(|(choice, read)| read.then_some(choice))
            .collect();
        // Leaves arrive in whatever order an operation produced them, so sort them:
        // equal conditions must have equal tables or they compare unequal, which
        // would cost the very sharing the tables exist for.
        let mut kept: Vec<_> = leaves
            .iter()
            .enumerate()
            .zip(&read_leaf)
            .filter_map(|((slot, leaf), read)| read.then_some((leaf.clone(), slot)))
            .collect();
        kept.sort_by(|(left, _), (right, _)| left.cmp(right));
        let mut leaf_slots = vec![None; leaves.len()];
        for (position, (_, slot)) in kept.iter().enumerate() {
            leaf_slots[*slot] = Some(u32::try_from(position).expect("leaf table fits"));
        }
        let leaves: Vec<_> = kept.into_iter().map(|(leaf, _)| leaf).collect();
        let groups = choice_groups(&choices);
        let sorted = leaf_slots
            .iter()
            .enumerate()
            .all(|(slot, mapped)| *mapped == Some(u32::try_from(slot).unwrap()));
        let decision = if read_choice.iter().all(|read| *read) && sorted {
            decision
        } else {
            decision.relabel_ordered(
                |bit| SlotChoice {
                    group: groups[choice_slots[usize::from(bit.slot)].unwrap() as usize],
                    bit: bit.bit,
                    slot: u16::try_from(choice_slots[usize::from(bit.slot)].unwrap()).unwrap(),
                },
                |leaf| leaf_slots[*leaf as usize].unwrap(),
            )
        };
        Self::new(choices.into(), leaves.into(), decision)
    }

    fn new(
        choices: Arc<[Arc<ChoiceKey<'db>>]>,
        leaves: Arc<[Leaf<'db>]>,
        decision: ChoiceDecision,
    ) -> Self {
        let decision = shared(&SHARED_CHOICES, decision);
        let mut hasher = FxHasher::default();
        choices.hash(&mut hasher);
        leaves.hash(&mut hasher);
        decision.hash(&mut hasher);
        Self {
            choices,
            leaves,
            decision,
            hash: hasher.finish(),
        }
    }

    /// Union choice tables and translate their slots without copying either graph.
    /// Sorted unions preserve strict variable order in each operand.
    fn aligned_choices(
        &self,
        other: &Self,
    ) -> (Vec<Arc<ChoiceKey<'db>>>, ChoiceRemap, ChoiceRemap) {
        let mut choices: Vec<_> = self
            .choices
            .iter()
            .chain(other.choices.iter())
            .cloned()
            .collect();
        choices.sort_by(|left, right| choice_order(left, right));
        choices.dedup_by(|left, right| choice_order(left, right).is_eq());
        let left = ChoiceRemap::new(&self.choices, &choices);
        let right = ChoiceRemap::new(&other.choices, &choices);
        (choices, left, right)
    }

    /// Map each table slot once, rather than searching semantic keys at every node.
    /// The flag records unchanged numbering, even when the table contents changed.
    fn remap_decision(
        &self,
        renamed: &[Arc<ChoiceKey<'db>>],
        mapped: &[Leaf<'db>],
        choices: &[Arc<ChoiceKey<'db>>],
        leaves: &[Leaf<'db>],
    ) -> (ChoiceDecision, bool) {
        let choice_slots = ChoiceRemap::new(renamed, choices);
        let leaf_slots: Vec<_> = mapped
            .iter()
            .map(|leaf| {
                u32::try_from(
                    leaves
                        .binary_search(leaf)
                        .expect("every mapped leaf is in the table"),
                )
                .expect("leaf table fits")
            })
            .collect();
        let original_groups = choice_groups(&self.choices);
        let unchanged = choice_slots
            .0
            .iter()
            .enumerate()
            .all(|(slot, &(group, mapped))| {
                usize::from(mapped) == slot && group == original_groups[slot]
            })
            && leaf_slots
                .iter()
                .enumerate()
                .all(|(slot, &mapped)| mapped as usize == slot);
        if unchanged {
            return (self.decision.clone(), true);
        }
        let decision = self.decision.map(
            |bit| Variable::Symbol(choice_slots.variable(bit)),
            |leaf| leaf_slots[*leaf as usize],
        );
        (decision, false)
    }

    /// Join two aligned decisions, resolving constant results as traversal reaches them.
    /// A reduced graph holds each leaf once and the traversal memoizes node pairs,
    /// so each reachable pair is joined once. Tabulating every pair of the merged
    /// table instead also joined pairs no valuation reaches, including pairs from
    /// the same side, which made sparse joins quadratic in their leaves.
    fn joined(
        &self,
        other: &Self,
        join: impl Fn(
            Option<&IndexCondition<'db>>,
            Option<&IndexCondition<'db>>,
        ) -> Option<IndexCondition<'db>>,
    ) -> Self {
        let (choices, left, right) = self.aligned_choices(other);
        let results = RefCell::new(Vec::new());
        let decision = self.decision.ordered_view(|bit| left.variable(bit)).apply(
            other.decision.ordered_view(|bit| right.variable(bit)),
            |left, right| {
                let joined = join(
                    left.map(|slot| self.leaves[*slot as usize].as_ref()),
                    right.map(|slot| other.leaves[*slot as usize].as_ref()),
                )?;
                Some(intern_leaf(&mut results.borrow_mut(), joined))
            },
        );
        Self::compact(choices, results.into_inner(), decision)
    }

    fn constant(value: bool) -> Self {
        Self::new(
            Arc::default(),
            Arc::from(vec![constant_leaf(value)]),
            Decision::leaf(0),
        )
    }

    /// One choice's tag bits, accepting exactly the valuation `value` describes.
    fn choice_bits(choice: &Arc<ChoiceKey<'db>>, bits: u16, value: impl Fn(u16) -> bool) -> Self {
        let decision = Decision::chain(
            (0..bits).map(|bit| {
                (
                    SlotChoice {
                        group: 0,
                        bit: Reverse(bit),
                        slot: 0,
                    },
                    value(bit),
                )
            }),
            1,
            0,
        );
        Self::compact(
            vec![choice.clone()],
            vec![constant_leaf(false), constant_leaf(true)],
            decision,
        )
    }

    /// Two choices agree on every tag bit, or `mismatch` holds.
    fn tag_equality(
        left: &Arc<ChoiceKey<'db>>,
        right: &Arc<ChoiceKey<'db>>,
        mismatch: Leaf<'db>,
    ) -> Self {
        let mut choices = vec![left.clone(), right.clone()];
        choices.sort_by(|left, right| choice_order(left, right));
        choices.dedup_by(|left, right| choice_order(left, right).is_eq());
        let groups = choice_groups(&choices);
        let slot = |key: &Arc<ChoiceKey<'db>>| {
            let slot = choices
                .binary_search_by(|candidate| choice_order(candidate, key))
                .expect("both choices are in the table");
            SlotChoice {
                group: groups[slot],
                bit: Reverse(0),
                slot: u16::try_from(slot).expect("choice table fits a slot"),
            }
        };
        let (left_slot, right_slot) = (slot(left), slot(right));
        let decision = Decision::equal_bits(
            (0..u16::BITS as u16).map(|bit| {
                (
                    SlotChoice {
                        bit: Reverse(bit),
                        ..left_slot
                    },
                    SlotChoice {
                        bit: Reverse(bit),
                        ..right_slot
                    },
                )
            }),
            1,
            0,
        );
        Self::compact(choices, vec![mismatch, constant_leaf(true)], decision)
    }

    fn leaf_value(&self) -> Option<&Leaf<'db>> {
        self.decision
            .leaf_value()
            .map(|slot| &self.leaves[*slot as usize])
    }

    fn is_always(&self) -> bool {
        self.leaf_value().is_some_and(|leaf| leaf.is_always())
    }

    fn is_never(&self) -> bool {
        self.leaf_value().is_some_and(|leaf| leaf.is_never())
    }

    fn and(&self, other: &Self) -> Self {
        self.joined(other, |left, right| {
            if left.is_some_and(IndexCondition::is_never)
                || right.is_some_and(IndexCondition::is_never)
            {
                Some(IndexCondition::never())
            } else {
                left.zip(right).map(|(left, right)| left.and(right))
            }
        })
    }

    fn or(&self, other: &Self) -> Self {
        self.joined(other, |left, right| {
            if left.is_some_and(IndexCondition::is_always)
                || right.is_some_and(IndexCondition::is_always)
            {
                Some(IndexCondition::always())
            } else {
                left.zip(right).map(|(left, right)| left.or(right))
            }
        })
    }

    /// Everything this decision accepts that `other` rejects.
    fn without(&self, other: &Self) -> Self {
        self.joined(other, |left, right| {
            if left.is_some_and(IndexCondition::is_never)
                || right.is_some_and(IndexCondition::is_always)
            {
                Some(IndexCondition::never())
            } else {
                left.zip(right).map(|(left, right)| left.and(&right.not()))
            }
        })
    }

    /// Rebuild the choice keys this decision reads.
    fn map_keys(&self, rename: impl FnMut(&ChoiceKey<'db>) -> ChoiceKey<'db>) -> Self {
        self.map_keys_and_leaves(rename, Clone::clone)
    }

    fn map_leaves(&self, leaf: impl FnMut(&Leaf<'db>) -> Leaf<'db>) -> Self {
        self.map_keys_and_leaves(Clone::clone, leaf)
    }

    /// Rebuild keys and leaves together, as canonicalization does per alternative.
    /// Either can reorder its table, so both are rebuilt and the decision is
    /// expressed over the new order.
    fn map_keys_and_leaves(
        &self,
        mut rename: impl FnMut(&ChoiceKey<'db>) -> ChoiceKey<'db>,
        mut leaf: impl FnMut(&Leaf<'db>) -> Leaf<'db>,
    ) -> Self {
        let renamed: Vec<Arc<ChoiceKey<'db>>> = self
            .choices
            .iter()
            .map(|choice| Arc::new(rename(choice)))
            .collect();
        let mut choices = renamed.clone();
        choices.sort_by(|left, right| choice_order(left, right));
        choices.dedup_by(|left, right| choice_order(left, right).is_eq());
        let mapped: Vec<Leaf<'db>> = self.leaves.iter().map(&mut leaf).collect();
        let mut leaves = mapped.clone();
        leaves.sort();
        leaves.dedup();
        let (decision, unchanged) = self.remap_decision(&renamed, &mapped, &choices, &leaves);
        if unchanged {
            // Both target tables are the images of the old tables. Identity slot
            // numbering means neither lost an entry, so they are already compact.
            Self::new(choices.into(), leaves.into(), decision)
        } else {
            Self::compact(choices, leaves, decision)
        }
    }

    /// Existentially quantify every key the predicate selects.
    fn forget_keys(&self, mut selected: impl FnMut(&ChoiceKey<'db>) -> bool) -> Self {
        // Quantification joins its own results, so unlike a pairwise apply its leaves
        // must come from a table closed under the join. Grow one as it goes rather
        // than tabulating a product the results would escape.
        let leaves = RefCell::new(self.leaves.to_vec());
        let joins = RefCell::new(FxHashMap::<(u32, u32), u32>::default());
        let decision = self.decision.exists(
            |bit| selected(&self.choices[usize::from(bit.slot)]),
            |left, right| {
                if let Some(slot) = joins.borrow().get(&(*left, *right)) {
                    return *slot;
                }
                let joined = {
                    let leaves = leaves.borrow();
                    leaves[*left as usize].or(&leaves[*right as usize])
                };
                let slot = intern_leaf(&mut leaves.borrow_mut(), joined);
                joins.borrow_mut().insert((*left, *right), slot);
                slot
            },
        );
        Self::compact(self.choices.to_vec(), leaves.into_inner(), decision)
    }

    /// Complete this decision outside the valuations `care` admits.
    /// Leaf pairs are restricted as the traversal reaches them, as in `joined`.
    fn restricted(&self, care: &Self) -> Option<Self> {
        let (choices, source_slots, care_slots) = self.aligned_choices(care);
        // The sentinel belongs to the care operand's original leaf table. Its slot
        // need not mean `never` in the source or result. If absent, no leaf matches.
        let never = constant_leaf(false);
        let empty = care
            .leaves
            .iter()
            .position(|leaf| *leaf == never)
            .map_or(u32::MAX, |slot| {
                u32::try_from(slot).expect("leaf table fits")
            });
        let results = RefCell::new(Vec::new());
        let decision = self
            .decision
            .ordered_view(|bit| source_slots.variable(bit))
            .restrict(
                care.decision.ordered_view(|bit| care_slots.variable(bit)),
                &empty,
                |value, feasible| {
                    self.leaves[*value as usize]
                        .restrict(&care.leaves[*feasible as usize])
                        .map(|restricted| intern_leaf(&mut results.borrow_mut(), restricted))
                },
            )?;
        Some(Self::compact(choices, results.into_inner(), decision))
    }

    fn keys(&self) -> impl Iterator<Item = &ChoiceKey<'db>> {
        self.choices.iter().map(|choice| &**choice)
    }

    /// The keys as shared handles, for rebuilding decisions over them.
    fn key_handles(&self) -> impl Iterator<Item = &Arc<ChoiceKey<'db>>> {
        self.choices.iter()
    }

    fn leaves(&self) -> impl Iterator<Item = &Leaf<'db>> {
        self.leaves.iter()
    }

    fn node_count(&self) -> usize {
        self.decision.node_count()
            + self
                .leaves
                .iter()
                .map(|leaf| leaf.node_count())
                .sum::<usize>()
    }
}

/// Place a leaf in a result table, reusing the slot of an equal one.
fn intern_leaf<'db>(results: &mut Vec<Leaf<'db>>, leaf: IndexCondition<'db>) -> u32 {
    let leaf = Arc::new(leaf);
    let slot = results
        .iter()
        .position(|result| *result == leaf)
        .unwrap_or_else(|| {
            results.push(leaf);
            results.len() - 1
        });
    u32::try_from(slot).expect("leaf table fits")
}

/// Results of one guard operation by its operands; `None` records an infeasible one.
type Memo<'db, K> = FxHashMap<(Guard<'db>, K), Option<Guard<'db>>>;

/// Fixpoint iteration rebuilds values from the same guards, so their unions and
/// intersections repeat. Guards are immutable; a cached result is the result.
///
/// Separate operations that complete an equal guard would each allocate its tables,
/// and interned values, regions and summaries would retain every copy, so the cache
/// also keeps one representative per structure and hands that out instead.
#[derive(Default)]
pub struct GuardCache<'db> {
    conjunctions: Memo<'db, Guard<'db>>,
    disjunctions: Memo<'db, Guard<'db>>,
    substitutions: Memo<'db, IndexSubst<'db>>,
    /// Each representative keeps its own node count, so a sweep never rewalks graphs.
    representatives: FxHashMap<Guard<'db>, usize>,
    /// Storage recorded since the last sweep and kept by it: one for each operation,
    /// and a representative's nodes.
    recorded: usize,
    kept: usize,
}

impl<'db> GuardCache<'db> {
    /// The least storage recorded between sweeps.
    const SWEEP: usize = 4096;

    pub fn and(&mut self, lhs: &Guard<'db>, rhs: &Guard<'db>) -> Option<Guard<'db>> {
        self.cached(|cache| &mut cache.conjunctions, lhs, rhs, Guard::and)
    }

    pub fn or(&mut self, lhs: &Guard<'db>, rhs: &Guard<'db>) -> Guard<'db> {
        self.cached(
            |cache| &mut cache.disjunctions,
            lhs,
            rhs,
            |lhs, rhs| Some(lhs.or(rhs)),
        )
        .expect("a union of satisfiable guards is satisfiable")
    }

    pub fn substitute(
        &mut self,
        guard: &Guard<'db>,
        subst: &IndexSubst<'db>,
    ) -> Option<Guard<'db>> {
        self.cached(
            |cache| &mut cache.substitutions,
            guard,
            subst,
            Guard::substitute,
        )
    }

    /// The representative of this guard's structure, which the guard becomes if
    /// there is none yet.
    pub fn share(&mut self, guard: Guard<'db>) -> Guard<'db> {
        if let Some((shared, _)) = self.representatives.get_key_value(&guard) {
            return shared.clone();
        }
        let nodes = guard.node_count();
        self.representatives.insert(guard.clone(), nodes);
        self.record(nodes);
        guard
    }

    /// Recomputing an operation rebuilds a complete decision graph even when an equal
    /// one is already live, so hand back the shared representative rather than the
    /// fresh copy.
    fn cached<K: Clone + Eq + Hash>(
        &mut self,
        results: fn(&mut Self) -> &mut Memo<'db, K>,
        lhs: &Guard<'db>,
        rhs: &K,
        operation: impl FnOnce(&Guard<'db>, &K) -> Option<Guard<'db>>,
    ) -> Option<Guard<'db>> {
        let key = (lhs.clone(), rhs.clone());
        if let Some(result) = results(self).get(&key) {
            return result.clone();
        }
        let result = operation(lhs, rhs).map(|guard| self.share(guard));
        results(self).insert(key, result.clone());
        self.record(1);
        result
    }

    /// Sweep once the cache has recorded as much storage as the last sweep kept. That
    /// bounds what the cache holds for nobody else by what it holds for the analysis,
    /// and amortizes each pass over the records it waited for.
    fn record(&mut self, storage: usize) {
        self.recorded += storage;
        if self.recorded >= self.kept.max(Self::SWEEP) {
            self.sweep();
        }
    }

    /// Keep exactly the entries and representatives whose guards something outside
    /// the cache still holds. Entries and representatives hold one another, so each
    /// guard's references from anywhere in the cache are counted first: comparing
    /// against a single owner would let the cache keep its own guards alive forever.
    /// A hot result stays with its operands, and an infeasible one costs only them.
    /// Clearing instead would only make the same graphs be rebuilt again.
    ///
    /// Liveness is decided for every condition before anything is removed, because
    /// dropping a dead entry drops its references too. Reading a strong count during
    /// the passes would let a dead entry's removal make an operand it shared with a
    /// live entry look unheld, taking the live entry and its representative with it.
    fn sweep(&mut self) {
        let mut held = FxHashMap::<*const Condition<'db>, (usize, usize)>::default();
        let pairs = self.conjunctions.iter().chain(&self.disjunctions);
        for guard in self
            .representatives
            .keys()
            .chain(pairs.flat_map(|((lhs, rhs), result)| [lhs, rhs].into_iter().chain(result)))
            .chain(
                self.substitutions
                    .iter()
                    .flat_map(|((guard, _), result)| iter::once(guard).chain(result)),
            )
        {
            held.entry(Arc::as_ptr(&guard.condition))
                .or_insert((0, Arc::strong_count(&guard.condition)))
                .0 += 1;
        }
        held.retain(|_, (cache, owners)| *owners > *cache);
        let live = |guard: &Guard<'db>| held.contains_key(&Arc::as_ptr(&guard.condition));
        for results in [&mut self.conjunctions, &mut self.disjunctions] {
            results.retain(|(lhs, rhs), result| {
                live(lhs) && live(rhs) && result.as_ref().is_none_or(live)
            });
        }
        self.substitutions
            .retain(|(guard, _), result| live(guard) && result.as_ref().is_none_or(live));
        self.representatives.retain(|guard, _| live(guard));
        self.kept = self.conjunctions.len()
            + self.disjunctions.len()
            + self.substitutions.len()
            + self.representatives.values().sum::<usize>();
        self.recorded = 0;
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Guard<'db> {
    scope: BinderScope,
    /// A guard's own allocation, so whether anything still holds the guard is a
    /// question about this handle. The decision graph inside is shared by every
    /// condition of the same shape, so its owner count cannot answer that question.
    condition: Arc<Condition<'db>>,
}

/// An allocation identity that does not keep a guard's graphs or tables alive.
/// The weak reference keeps its address reserved after the condition is dropped.
pub(super) struct WeakGuard<'db> {
    scope: BinderScope,
    condition: Weak<Condition<'db>>,
}

impl PartialEq for WeakGuard<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.scope == other.scope && Weak::ptr_eq(&self.condition, &other.condition)
    }
}

impl Eq for WeakGuard<'_> {}

impl Hash for WeakGuard<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.scope.hash(state);
        self.condition.as_ptr().hash(state);
    }
}

impl WeakGuard<'_> {
    pub(super) fn is_live(&self) -> bool {
        self.condition.strong_count() > 0
    }
}

impl<'db> Guard<'db> {
    pub fn always(scope: &BinderScope) -> Self {
        Self {
            scope: scope.clone(),
            condition: Arc::new(Condition::constant(true)),
        }
    }
    pub fn scope(&self) -> &BinderScope {
        &self.scope
    }

    pub(super) fn downgrade(&self) -> WeakGuard<'db> {
        WeakGuard {
            scope: self.scope.clone(),
            condition: Arc::downgrade(&self.condition),
        }
    }

    pub fn and(&self, other: &Self) -> Option<Self> {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        if self == other || other.condition.is_always() {
            return Some(self.clone());
        }
        if self.condition.is_always() {
            return Some(other.clone());
        }
        Self::canonical(&self.scope, self.condition.and(&other.condition))
    }

    pub fn or(&self, other: &Self) -> Self {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        if self == other || self.condition.is_always() {
            return self.clone();
        }
        if other.condition.is_always() {
            return other.clone();
        }
        Self::canonical(&self.scope, self.condition.or(&other.condition))
            .expect("a union of satisfiable guards is satisfiable")
    }

    pub fn with_equality(&self, lhs: IndexExpr<'db>, rhs: IndexExpr<'db>) -> Option<Self> {
        self.scope.validate(lhs).expect("free equality binder");
        self.scope.validate(rhs).expect("free equality binder");
        self.with_index_condition(&IndexCondition::equal(lhs, rhs))
    }

    pub fn with_equalities(
        &self,
        pairs: impl IntoIterator<Item = (IndexExpr<'db>, IndexExpr<'db>)>,
    ) -> Option<Self> {
        pairs
            .into_iter()
            .try_fold(self.clone(), |guard, (lhs, rhs)| {
                guard.with_equality(lhs, rhs)
            })
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
        self.with_choice_bits(choice, u16::BITS as u16, |bit| variant.0 & (1 << bit) != 0)
    }

    /// Restrict an actual boolean value occurrence to one of its two outcomes.
    pub fn with_boolean(&self, choice: ChoiceKey<'db>, value: bool) -> Option<Self> {
        self.with_choice_bits(choice, 1, |_| value)
    }

    fn with_choice_bits(
        &self,
        choice: ChoiceKey<'db>,
        bits: u16,
        value: impl Fn(u16) -> bool,
    ) -> Option<Self> {
        for index in choice.path.indices() {
            self.scope.validate(index).expect("free choice binder");
        }
        let selected = Condition::choice_bits(&Arc::new(choice), bits, value);
        Self::canonical(&self.scope, self.condition.and(&selected))
    }

    pub fn substitute(&self, subst: &IndexSubst<'db>) -> Option<Self> {
        assert_eq!(
            &self.scope,
            subst.source(),
            "substitution source scope must match"
        );
        if subst.is_identity() {
            return Some(self.clone());
        }
        // A scope extension preserves the shared graph and tables, but needs its
        // own handle so another scope cannot keep this guard's cache entries live.
        if subst.preserves_indices() {
            return Some(Self {
                scope: subst.destination().clone(),
                condition: Arc::new((*self.condition).clone()),
            });
        }
        Self::canonical(
            subst.destination(),
            self.condition.map_keys_and_leaves(
                |choice| choice.map_indices(|index| subst.apply(*index)),
                |condition| Arc::new(condition.substitute(subst)),
            ),
        )
    }

    pub fn in_scope(&self, scope: &BinderScope) -> Self {
        if &self.scope == scope {
            return self.clone();
        }
        self.substitute(&IndexSubst::new(&self.scope, scope, []).expect("guard scope extension"))
            .expect("scope extension preserves satisfiability")
    }

    pub fn occurrences(&self) -> BTreeSet<ValueOccurrence> {
        self.condition
            .keys()
            .map(|choice| choice.occurrence)
            .collect()
    }

    pub fn map_occurrences(
        &self,
        mut map: impl FnMut(ValueOccurrence) -> ValueOccurrence,
    ) -> Option<Self> {
        let occurrences: BTreeSet<_> = self
            .condition
            .keys()
            .map(|choice| choice.occurrence)
            .collect();
        let mappings: BTreeMap<_, _> = occurrences
            .into_iter()
            .map(|occurrence| (occurrence, map(occurrence)))
            .collect();
        if mappings.iter().all(|(from, to)| from == to) {
            return Some(self.clone());
        }
        Self::canonical(
            &self.scope,
            self.condition.map_keys(|choice| {
                ChoiceKey::new(mappings[&choice.occurrence], choice.path.clone())
            }),
        )
    }

    /// A new execution of a loop may choose another enum alternative.
    pub fn forget_occurrences(&self, mut repeated: impl FnMut(ValueOccurrence) -> bool) -> Self {
        if !self.occurrences().into_iter().any(&mut repeated) {
            return self.clone();
        }
        Self::canonical(
            &self.scope,
            self.condition
                .forget_keys(|choice| repeated(choice.occurrence)),
        )
        .expect("existential quantification preserves feasibility")
    }

    /// Keep this condition only where it holds for every hidden choice admitted
    /// by `domain`. The result is restricted to executions the projected domain
    /// admits; existential projection of permission would grant extra authority.
    pub fn forget_occurrences_universally(
        &self,
        domain: &Self,
        hidden: impl Fn(ValueOccurrence) -> bool,
    ) -> Option<Self> {
        let projected = domain.forget_occurrences(&hidden);
        match domain.difference(self) {
            Some(refuted) => projected.difference(&refuted.forget_occurrences(hidden)),
            None => Some(projected),
        }
    }

    /// Forget old scalar selectors without identifying them with a new execution.
    pub fn forget_indices(&self, mut repeated: impl FnMut(IndexExpr<'db>) -> bool) -> Self {
        let indices: BTreeSet<_> = self
            .indices()
            .into_iter()
            .filter(|index| repeated(*index))
            .collect();
        if indices.is_empty() {
            return self.clone();
        }
        // Both projections are existential. Project scalar leaves first so
        // merging indexed choices does not combine facts being discarded.
        let condition = self
            .condition
            .map_leaves(|condition| Arc::new(condition.project(|index| indices.contains(&index))))
            .forget_keys(|choice| choice.path.indices().any(|index| indices.contains(&index)));
        Self::canonical(&self.scope, condition)
            .expect("existential quantification preserves feasibility")
    }

    pub fn difference(&self, other: &Self) -> Option<Self> {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        Self::canonical(&self.scope, self.condition.without(&other.condition))
    }

    /// Eliminate clause-local witnesses that are observable only through scalar
    /// constraints. A witness indexing an enum choice remains observable: its
    /// quantification would require also quantifying that indexed choice.
    /// Projection can make such a choice redundant, exposing its witness, so it
    /// repeats until no hidden witness is projectable.
    pub fn project_witnesses(&self, hidden: impl Fn(IndexExpr<'db>) -> bool) -> Self {
        let mut guard = self.clone();
        loop {
            let indexed: BTreeSet<_> = guard
                .condition
                .keys()
                .flat_map(|choice| choice.path.indices())
                .collect();
            let projected: BTreeSet<_> = guard
                .indices()
                .into_iter()
                .filter(|index| hidden(*index) && !indexed.contains(index))
                .collect();
            if projected.is_empty() {
                return guard;
            }
            guard = Self::canonical(
                &guard.scope,
                guard.condition.map_leaves(|condition| {
                    Arc::new(condition.project(|index| projected.contains(&index)))
                }),
            )
            .expect("existential projection preserves feasibility");
        }
    }

    pub fn implies(&self, other: &Self) -> bool {
        assert_eq!(self.scope, other.scope, "guard scopes must match");
        self.condition.without(&other.condition).is_never()
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
            .flat_map(|leaf| leaf.indices())
            .collect();
        for choice in self.condition.keys() {
            indices.extend(choice.path.indices());
        }
        indices
    }

    pub fn node_count(&self) -> usize {
        self.condition.node_count()
    }

    fn with_index_condition(&self, condition: &IndexCondition<'db>) -> Option<Self> {
        Self::canonical(
            &self.scope,
            self.condition
                .map_leaves(|old| Arc::new(old.and(condition))),
        )
    }

    // Indexed choice keys must also respect equalities proved by their index condition.
    // Identifying choice bits rebuilds the ordered graph and rejects conflicting variants;
    // no map collection is allowed to overwrite a contradictory requirement.
    fn canonical(scope: &BinderScope, condition: Condition<'db>) -> Option<Self> {
        if condition.is_never() {
            return None;
        }
        if condition.keys().all(|choice| {
            choice
                .path
                .indices()
                .all(|index| matches!(index, IndexExpr::Const(_)))
        }) {
            return Some(Self {
                scope: scope.clone(),
                condition: Arc::new(condition),
            });
        }
        let choice_indices: BTreeSet<_> = condition
            .keys()
            .flat_map(|choice| choice.path.indices())
            .collect();
        let mut canonical = Condition::constant(false);
        // Partition by distinct terminal conditions, not graph paths: shared suffixes
        // can represent exponentially many paths through a compact decision graph.
        for indices in condition.leaves().filter(|indices| !indices.is_never()) {
            let terms = choice_indices
                .iter()
                .copied()
                .chain(indices.indices())
                .collect();
            let representatives = indices.representatives(&terms);
            let alternative = condition.map_keys_and_leaves(
                |choice| {
                    choice
                        .map_indices(|index| representatives.get(index).copied().unwrap_or(*index))
                },
                |leaf| {
                    if leaf == indices {
                        leaf.clone()
                    } else {
                        constant_leaf(false)
                    }
                },
            );
            canonical = canonical.or(&alternative);
        }
        // Choice occurrences at equal indices must have equal tags. Complete the
        // graph outside these feasible valuations so equality partitions can reunite
        // without retaining a spurious dependence on an extra indexed choice.
        let choices: BTreeSet<_> = canonical.key_handles().collect();
        let mut care = Condition::constant(true);
        for (position, left) in choices.iter().copied().enumerate() {
            for right in choices.iter().copied().skip(position + 1) {
                if let Some(alias) = left.alias_condition(right) {
                    care = care.and(&Condition::tag_equality(left, right, Arc::new(alias.not())));
                }
            }
        }
        let canonical = canonical.restricted(&care)?;
        (!canonical.is_never()).then(|| Self {
            scope: scope.clone(),
            condition: Arc::new(canonical),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::super::{
        decision::{apply_visits, intern_attempts},
        index::IndexNamespace,
    };
    use super::*;

    fn selected<'db>(scope: &BinderScope, occurrence: u16) -> Guard<'db> {
        Guard::always(scope)
            .with_variant(
                ChoiceKey::new(
                    ValueOccurrence::Value(NValueId::from_u32(occurrence.into())),
                    StructuralPath::default(),
                ),
                VariantIndex(occurrence),
            )
            .unwrap()
    }

    /// Sweep the shared choice graphs and count the ones something still holds.
    fn live_choice_graphs() -> usize {
        SHARED_CHOICES.with_borrow_mut(|shared| {
            shared.sweep();
            shared.kept
        })
    }

    /// Each side selects one of 32 index conditions by one enum choice, so only
    /// matching alternatives meet: 33 reachable leaf pairs, against 65 * 65 in the
    /// merged table.
    fn alternatives<'db>(scope: &BinderScope, index: u32) -> Guard<'db> {
        let choice = ChoiceKey::new(ValueOccurrence::Argument(0), StructuralPath::default());
        (0..32)
            .map(|variant| {
                Guard::always(scope)
                    .with_variant(choice.clone(), VariantIndex(variant))
                    .unwrap()
                    .with_equality(
                        IndexExpr::Runtime(NValueId::from_u32(index)),
                        IndexExpr::Const(variant.into()),
                    )
                    .unwrap()
            })
            .reduce(|left, right| left.or(&right))
            .unwrap()
    }

    #[test]
    fn universal_choice_projection_preserves_only_guaranteed_permission() {
        let scope = BinderScope::default();
        let outer = ChoiceKey::new(ValueOccurrence::Argument(0), StructuralPath::default());
        let permission = ChoiceKey::new(ValueOccurrence::Argument(1), StructuralPath::default());
        let hidden = ChoiceKey::new(ValueOccurrence::SummaryChoice(0), StructuralPath::default());
        let admitted = Guard::always(&scope).with_boolean(outer, true).unwrap();
        let authorized = Guard::always(&scope)
            .with_boolean(permission, true)
            .unwrap();
        for branch in [None, Some(false), Some(true)] {
            let domain = branch.map_or_else(
                || admitted.clone(),
                |value| admitted.with_boolean(hidden.clone(), value).unwrap(),
            );
            for required in [None, Some(false), Some(true)] {
                let condition = required.map_or_else(
                    || authorized.clone(),
                    |value| authorized.with_boolean(hidden.clone(), value).unwrap(),
                );
                let projected = condition.forget_occurrences_universally(&domain, |choice| {
                    matches!(choice, ValueOccurrence::SummaryChoice(_))
                });
                let expected = if required.is_none() || branch == required {
                    admitted.and(&authorized)
                } else {
                    None
                };
                assert_eq!(
                    projected, expected,
                    "domain {branch:?}, permission {required:?}"
                );
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq)]
    struct Collision(u8);

    impl Hash for Collision {
        fn hash<H: Hasher>(&self, state: &mut H) {
            0u8.hash(state);
        }
    }

    #[test]
    fn graph_interning_resolves_collisions_without_retaining_dead_buffers() {
        let mut shared = SharedGraphs::<u8, Collision>::default();
        let first = shared.intern(Decision::leaf(Collision(1)));
        let second = shared.intern(Decision::leaf(Collision(2)));
        assert_eq!(first.structural_hash(), second.structural_hash());
        assert_ne!(first, second);
        assert_eq!(
            first.downgrade(),
            shared.intern(Decision::leaf(Collision(1))).downgrade()
        );
        let expired = first.downgrade();
        drop(first);
        assert!(
            expired.upgrade().is_none(),
            "the interner retained an unused graph"
        );
        shared.sweep();
        assert_eq!(shared.kept, 1);
        assert_eq!(
            second.downgrade(),
            shared.intern(Decision::leaf(Collision(2))).downgrade()
        );
        let rebuilt = shared.intern(Decision::leaf(Collision(1)));
        assert_ne!(expired, rebuilt.downgrade());
        drop((second, rebuilt));
        shared.sweep();
        assert!(shared.graphs.is_empty());
    }

    #[test]
    fn bit_memoization_reuses_live_graphs_and_releases_unused_operands_and_results() {
        let source = BitShape::EqualConst(0x5a31).decision();
        let result = BitOperation::Not(source.clone()).run().unwrap();
        let (source_weak, result_weak) = (source.downgrade(), result.downgrade());
        let key = BitOperation::Not(source_weak.clone());
        BIT_OPERATIONS.with_borrow_mut(|cache| cache.sweep(BitOperation::is_live));
        BIT_SHAPES.with_borrow_mut(|cache| cache.sweep(|_| true));
        let before = intern_attempts();
        assert_eq!(
            BitShape::EqualConst(0x5a31).decision().downgrade(),
            source_weak
        );
        assert_eq!(
            BitOperation::Not(source.clone()).run().unwrap().downgrade(),
            result_weak
        );
        assert_eq!(
            intern_attempts(),
            before,
            "a live cache entry was recomputed"
        );
        // Drop the result while the key is still live: its expired weak result
        // means a cache miss, not a cached infeasible restriction.
        drop(result);
        assert!(result_weak.upgrade().is_none());
        assert!(
            BIT_OPERATIONS
                .with_borrow(|cache| cache.get(&key))
                .is_none()
        );
        let rebuilt = BitOperation::Not(source.clone()).run().unwrap();
        assert_ne!(rebuilt.downgrade(), result_weak);
        let rebuilt_weak = rebuilt.downgrade();
        drop((source, rebuilt));
        assert!(
            source_weak.upgrade().is_none(),
            "operation or shape caches pinned an operand"
        );
        assert!(
            rebuilt_weak.upgrade().is_none(),
            "operation caches pinned a result"
        );
        BIT_OPERATIONS.with_borrow_mut(|cache| {
            cache.sweep(BitOperation::is_live);
            assert!(!cache.results.contains_key(&key));
        });
    }

    #[test]
    fn cached_infeasible_restrictions_follow_operand_lifetimes() {
        let source = BitShape::EqualConst(0x5a32).decision();
        let care = shared(&SHARED_BITS, Decision::leaf(false));
        let key = BitOperation::Restrict(
            source.downgrade(),
            care.downgrade(),
            BitAlignment::default(),
        );
        assert!(
            BitOperation::Restrict(source.clone(), care.clone(), BitAlignment::default())
                .run()
                .is_none()
        );
        BIT_OPERATIONS.with_borrow_mut(|cache| {
            cache.sweep(BitOperation::is_live);
            assert_eq!(cache.get(&key), Some(None));
        });
        drop((source, care));
        BIT_OPERATIONS.with_borrow_mut(|cache| {
            cache.sweep(BitOperation::is_live);
            assert!(!cache.results.contains_key(&key));
        });
    }

    #[test]
    fn table_renaming_reuses_unchanged_decision_numbering() {
        let scope = BinderScope::default();
        let left = selected(&scope, 0);
        let right = selected(&scope, 1);
        let source = left.condition.or(&right.condition);
        let rename = |choice: &ChoiceKey<'static>| {
            let ValueOccurrence::Value(value) = choice.occurrence else {
                unreachable!()
            };
            ChoiceKey::new(
                ValueOccurrence::Argument(value.as_u32()),
                choice.path.clone(),
            )
        };
        let expected =
            Condition::choice_bits(&Arc::new(rename(&left.condition.choices[0])), 16, |_| false)
                .or(&Condition::choice_bits(
                    &Arc::new(rename(&right.condition.choices[0])),
                    16,
                    |bit| bit == 0,
                ));
        let before = intern_attempts();
        let renamed = source.map_keys(rename);
        assert_eq!(renamed, expected);
        assert_ne!(renamed.choices, source.choices);
        assert_eq!(
            intern_attempts(),
            before,
            "unchanged slot numbering rebuilt the decision"
        );
    }

    #[test]
    fn alignment_reuses_graphs_when_only_unused_table_suffixes_are_added() {
        let scope = BinderScope::default();
        let left = selected(&scope, 0);
        let right = left.condition.or(&selected(&scope, 1).condition);
        let before = intern_attempts();
        let (aligned_left, unchanged) = left.condition.remap_decision(
            &left.condition.choices,
            &left.condition.leaves,
            &right.choices,
            &right.leaves,
        );
        assert!(unchanged);
        assert_eq!(aligned_left, left.condition.decision);
        assert_eq!(
            intern_attempts(),
            before,
            "adding unused table slots rebuilt the decision"
        );
    }

    #[test]
    fn table_remapping_preserves_reordered_merged_and_regrouped_choices() {
        let choice = |occurrence, index| {
            Arc::new(ChoiceKey::new(
                ValueOccurrence::Argument(occurrence),
                StructuralPath::new(vec![Projection::Index(IndexExpr::Const(index))]),
            ))
        };
        let original = [choice(0, 0), choice(1, 1)];
        let source = Condition::choice_bits(&original[0], 2, |_| false)
            .and(&Condition::choice_bits(&original[1], 2, |bit| bit == 0));
        for renamed in [
            [choice(1, 0), choice(0, 1)],
            [choice(0, 0), choice(0, 1)],
            [choice(0, 0), choice(0, 0)],
        ] {
            let expected = Condition::choice_bits(&renamed[0], 2, |_| false)
                .and(&Condition::choice_bits(&renamed[1], 2, |bit| bit == 0));
            let mapped = source.map_keys(|key| {
                let position = original.iter().position(|choice| **choice == *key).unwrap();
                (*renamed[position]).clone()
            });
            assert_eq!(mapped, expected);
        }
        let complemented = source.map_leaves(|leaf| Arc::new(leaf.not()));
        assert_eq!(complemented, Condition::constant(true).without(&source));
        let constant = source.map_leaves(|_| constant_leaf(true));
        assert!(constant.is_always());
        assert!(constant.choices.is_empty());
    }

    #[test]
    fn joining_shifted_choice_tables_builds_only_the_result() {
        let scope = BinderScope::default();
        let left = selected(&scope, 1);
        let right = selected(&scope, 0);
        let expected = Condition::compact(
            vec![
                right.condition.choices[0].clone(),
                left.condition.choices[0].clone(),
            ],
            vec![constant_leaf(false), constant_leaf(true)],
            Decision::chain(
                (0..2).flat_map(|slot| {
                    (0..16).map(move |bit| {
                        (
                            SlotChoice {
                                group: slot,
                                slot,
                                bit: Reverse(bit),
                            },
                            slot == 1 && bit == 0,
                        )
                    })
                }),
                1,
                0,
            ),
        );
        let before = intern_attempts();
        let joined = left.condition.and(&right.condition);
        let attempts = intern_attempts() - before;
        assert_eq!(joined, expected);
        assert!(
            attempts <= expected.decision.node_count() + 2,
            "{attempts} interning attempts rebuilt operands for {} result nodes",
            expected.decision.node_count()
        );
    }

    #[test]
    fn absorbing_choice_leaves_skip_excluded_subgraphs() {
        let keys: Vec<_> = (0..2)
            .map(|occurrence| {
                Arc::new(ChoiceKey::new(
                    ValueOccurrence::Argument(occurrence),
                    StructuralPath::default(),
                ))
            })
            .collect();
        let mask = Condition::choice_bits(&keys[0], 1, |_| true);
        for bits in [16, 256] {
            let tail = Condition::choice_bits(&keys[1], bits, |_| true);
            let excluded = Condition::constant(true).without(&mask).and(&tail);
            let included = mask.and(&tail);
            let covering = mask.or(&tail);
            for (left, right, operation, expected) in [
                (
                    &mask,
                    &excluded,
                    Condition::and as fn(&_, &_) -> _,
                    Condition::constant(false),
                ),
                (&excluded, &mask, Condition::and, Condition::constant(false)),
                (&mask, &included, Condition::or, mask.clone()),
                (&included, &mask, Condition::or, mask.clone()),
                (
                    &mask,
                    &covering,
                    Condition::without,
                    Condition::constant(false),
                ),
                (
                    &included,
                    &mask,
                    Condition::without,
                    Condition::constant(false),
                ),
                (
                    &Condition::constant(false),
                    &tail,
                    Condition::and,
                    Condition::constant(false),
                ),
                (
                    &tail,
                    &Condition::constant(true),
                    Condition::or,
                    Condition::constant(true),
                ),
            ] {
                let before = intern_attempts();
                let visits = apply_visits();
                let actual = operation(left, right);
                let attempts = intern_attempts() - before;
                assert_eq!(actual, expected);
                assert!(
                    apply_visits() - visits <= 3,
                    "{} apply visits for an excluded {bits}-bit subgraph",
                    apply_visits() - visits
                );
                assert!(
                    attempts <= 4,
                    "{attempts} interning attempts visited an excluded {bits}-bit subgraph"
                );
            }
        }
    }

    #[test]
    fn borrowed_choice_operations_keep_operand_leaf_tables_separate() {
        let key = |occurrence| {
            Arc::new(ChoiceKey::new(
                ValueOccurrence::Argument(occurrence),
                StructuralPath::default(),
            ))
        };
        let index = IndexExpr::Runtime(NValueId::from_u32(0));
        let source = Condition::compact(
            vec![key(1)],
            vec![
                Arc::new(IndexCondition::equal(index, IndexExpr::Const(0))),
                constant_leaf(true),
            ],
            Decision::chain(
                [(
                    SlotChoice {
                        group: 0,
                        slot: 0,
                        bit: Reverse(0),
                    },
                    true,
                )],
                1,
                0,
            ),
        );
        let care = Condition::choice_bits(&key(0), 1, |_| true);
        let joined = source.and(&care);
        let restricted = source.restricted(&care).unwrap();
        let evaluate = |condition: &Condition<'static>, values: [bool; 2]| {
            let result = condition.decision.map(
                |bit| {
                    let ValueOccurrence::Argument(argument) =
                        condition.choices[usize::from(bit.slot)].occurrence
                    else {
                        unreachable!()
                    };
                    Variable::<SlotChoice>::Constant(values[argument as usize])
                },
                Clone::clone,
            );
            condition.leaves[*result.leaf_value().unwrap() as usize].clone()
        };
        for values in [[false, false], [false, true], [true, false], [true, true]] {
            let expected = evaluate(&source, values).and(&evaluate(&care, values));
            assert_eq!(*evaluate(&joined, values), expected);
            assert_eq!(
                evaluate(&restricted, values).and(&evaluate(&care, values)),
                expected
            );
        }
        let permute = |condition: &Condition<'static>| {
            Condition::new(
                condition.choices.clone(),
                condition.leaves.iter().rev().cloned().collect(),
                condition.decision.relabel_ordered(Clone::clone, |slot| {
                    u32::try_from(condition.leaves.len() - 1).unwrap() - slot
                }),
            )
        };
        for source in [
            source.clone(),
            source.map_leaves(|leaf| Arc::new(leaf.not())),
        ] {
            for left in [&source, &permute(&source)] {
                for right in [
                    &care,
                    &permute(&care),
                    &Condition::constant(true),
                    &Condition::constant(false),
                ] {
                    for (operation, join) in [
                        (
                            Condition::and as fn(&_, &_) -> _,
                            IndexCondition::and as fn(&_, &_) -> _,
                        ),
                        (Condition::or, IndexCondition::or),
                        (Condition::without, |left, right| left.and(&right.not())),
                    ] {
                        let result = operation(left, right);
                        for values in [[false, false], [false, true], [true, false], [true, true]] {
                            assert_eq!(
                                *evaluate(&result, values),
                                join(&evaluate(left, values), &evaluate(right, values))
                            );
                        }
                    }
                }
            }
        }
        // No `never` slot in this care table: its slot zero is feasible.
        assert_eq!(source.restricted(&Condition::constant(true)), Some(source));
    }

    #[test]
    fn joins_and_restrictions_evaluate_only_reachable_leaf_pairs() {
        let scope = BinderScope::default();
        let (left, right) = (alternatives(&scope, 0), alternatives(&scope, 1));
        let joins = Cell::new(0);
        let joined = left.condition.joined(&right.condition, |left, right| {
            left.zip(right).map(|(left, right)| {
                joins.set(joins.get() + 1);
                left.and(right)
            })
        });
        assert_eq!(joined, left.condition.and(&right.condition));
        assert_eq!(joins.get(), 33, "a join evaluated unreachable leaf pairs");
        // Restriction never evaluates a leaf the care decision excludes.
        let before = RESTRICTIONS.get();
        left.condition.restricted(&right.condition).unwrap();
        assert_eq!(
            RESTRICTIONS.get() - before,
            32,
            "a restriction evaluated unreachable leaf pairs"
        );
    }

    #[test]
    fn bit_operation_cache_bounds_owned_metadata_without_discarding_live_hits() {
        let mut cache = BitCache::default();
        let source = Decision::leaf(true);
        let result = Decision::leaf(false);
        let hot = BitOperation::Not(source.downgrade());
        cache.insert(
            hot.clone(),
            Some(&result),
            hot.metadata_units(),
            BitOperation::is_live,
        );
        for slot in 0..256 {
            let key = BitOperation::Exists(source.downgrade(), vec![slot; 512].into_boxed_slice());
            cache.insert(
                key.clone(),
                Some(&source),
                key.metadata_units(),
                BitOperation::is_live,
            );
            assert_eq!(cache.get(&hot), Some(Some(result.clone())));
            assert!(cache.storage <= BitCache::<BitOperation<WeakBitDecision>>::LIMIT);
        }
        assert!(
            cache.results.len() < 256,
            "slot mappings exceeded the metadata budget"
        );
        assert_eq!(
            cache.storage,
            cache
                .results
                .values()
                .map(|(_, storage)| storage)
                .sum::<usize>()
        );
        let (source_weak, result_weak) = (source.downgrade(), result.downgrade());
        drop((source, result));
        assert!(source_weak.upgrade().is_none());
        assert!(result_weak.upgrade().is_none());
        cache.sweep(BitOperation::is_live);
        assert!(cache.results.is_empty());
        assert_eq!(cache.storage, 0);
    }

    #[test]
    fn bit_operation_cache_skips_entries_exceeding_its_metadata_budget() {
        let mut cache = BitCache::default();
        let source = Decision::leaf(true);
        let result = Decision::leaf(false);
        let small = BitOperation::Not(source.downgrade());
        cache.insert(
            small.clone(),
            Some(&result),
            small.metadata_units(),
            BitOperation::is_live,
        );
        let storage = cache.storage;
        cache.insert(
            small.clone(),
            Some(&result),
            small.metadata_units(),
            BitOperation::is_live,
        );
        assert_eq!(
            cache.storage, storage,
            "replacing a cache entry charged it twice"
        );
        let large = BitOperation::Substitute(
            source.downgrade(),
            vec![SlotTarget::Slot(0); BitCache::<BitOperation<WeakBitDecision>>::LIMIT]
                .into_boxed_slice(),
        );
        cache.insert(
            large.clone(),
            Some(&source),
            large.metadata_units(),
            BitOperation::is_live,
        );
        assert_eq!(cache.storage, storage);
        assert_eq!(cache.results.len(), 1);
        assert_eq!(cache.get(&small), Some(Some(result.clone())));
        assert!(cache.get(&large).is_none());
        let infeasible = BitOperation::Restrict(
            source.downgrade(),
            result.downgrade(),
            BitAlignment::default(),
        );
        cache.insert(
            infeasible.clone(),
            None,
            infeasible.metadata_units(),
            BitOperation::is_live,
        );
        assert_eq!(cache.storage, storage + 1);
        assert_eq!(cache.get(&infeasible), Some(None));
    }

    #[test]
    fn scope_extensions_do_not_keep_each_others_cache_entries_alive() {
        let before = live_choice_graphs();
        let scope = BinderScope::default();
        let guard = selected(&scope, 0);
        let mut cache = GuardCache::default();
        let identity = IndexSubst::new(&scope, &scope, []).unwrap();
        let unchanged = cache.substitute(&guard, &identity).unwrap();
        assert!(Arc::ptr_eq(&unchanged.condition, &guard.condition));

        let (mut destination, _) = scope.bind(IndexNamespace::Value);
        let extension = IndexSubst::new(&scope, &destination, []).unwrap();
        let hot = cache.substitute(&guard, &extension).unwrap();
        assert_eq!(hot.scope(), &destination);
        assert_eq!(hot.condition, guard.condition);
        assert!(Arc::ptr_eq(
            &hot.condition.choices,
            &guard.condition.choices
        ));
        assert!(Arc::ptr_eq(&hot.condition.leaves, &guard.condition.leaves));
        for _ in 0..64 {
            (destination, _) = destination.bind(IndexNamespace::Value);
            let subst = IndexSubst::new(&scope, &destination, []).unwrap();
            cache.substitute(&guard, &subst).unwrap();
        }
        cache.sweep();
        assert_eq!(
            (cache.substitutions.len(), cache.representatives.len()),
            (2, 2)
        );
        let again = cache.substitute(&guard, &extension).unwrap();
        assert!(Arc::ptr_eq(&again.condition, &hot.condition));
        assert_eq!(
            live_choice_graphs(),
            before + 1,
            "scope extensions copied the graph"
        );

        drop((hot, again));
        cache.sweep();
        assert_eq!(
            (cache.substitutions.len(), cache.representatives.len()),
            (1, 1)
        );
        drop((guard, unchanged));
        cache.sweep();
        assert_eq!(
            (cache.substitutions.len(), cache.representatives.len()),
            (0, 0)
        );
        assert_eq!(live_choice_graphs(), before);
    }

    #[test]
    fn a_sweep_keeps_a_live_operation_sharing_a_dead_one_s_operand() {
        let scope = BinderScope::default();
        let mut cache = GuardCache::default();
        let (left, right) = (selected(&scope, 0), selected(&scope, 1));
        // A conjunction nothing outside the cache holds, over an operand that the
        // live disjunction below shares. Removing it drops that operand's reference.
        let dead = selected(&scope, 2);
        cache.and(&left, &dead).unwrap();
        drop(dead);
        let hot = cache.or(&left, &right);
        cache.sweep();
        assert_eq!(
            (
                cache.conjunctions.len(),
                cache.disjunctions.len(),
                cache.representatives.len()
            ),
            (0, 1, 1),
            "a sweep dropped a live operation sharing a dead one's operand"
        );
        let again = cache.or(&left, &right);
        assert!(
            Arc::ptr_eq(&again.condition, &hot.condition),
            "a swept cache rebuilt a result the analysis still holds"
        );
    }

    #[test]
    fn cache_sweep_preserves_live_operations_sharing_dead_entry_operands() {
        let scope = BinderScope::default();
        let (left, right, dead) = (
            selected(&scope, 0),
            selected(&scope, 1),
            selected(&scope, 2),
        );
        let mut cache = GuardCache::default();
        let cold = cache.and(&left, &dead).unwrap();
        drop((dead, cold));
        let hot = cache.or(&left, &right);
        cache.sweep();
        assert!(cache.conjunctions.is_empty());
        assert_eq!(cache.disjunctions.len(), 1);
        assert_eq!(cache.representatives.len(), 1);
        let again = cache.or(&left, &right);
        assert!(Arc::ptr_eq(&again.condition, &hot.condition));
        drop((left, right, hot, again));
        cache.sweep();
        assert!(cache.disjunctions.is_empty());
        assert!(cache.representatives.is_empty());
    }

    #[test]
    fn cache_sweeps_keep_exactly_the_guards_held_outside_the_cache() {
        let scope = BinderScope::default();
        let before = live_choice_graphs();
        let mut cache = GuardCache::default();
        let (left, right) = (selected(&scope, 0), selected(&scope, 1));
        let hot = cache.and(&left, &right).unwrap();
        // Operations whose operands and results only the cache holds afterwards.
        for occurrence in 2..34 {
            cache.or(
                &selected(&scope, occurrence),
                &selected(&scope, occurrence + 32),
            );
        }
        cache.sweep();
        assert_eq!(
            (
                cache.conjunctions.len(),
                cache.disjunctions.len(),
                cache.representatives.len()
            ),
            (1, 0, 1),
            "a sweep kept dead entries or dropped the live one"
        );
        let again = cache.and(&left, &right).unwrap();
        assert!(
            Arc::ptr_eq(&again.condition, &hot.condition),
            "a sweep dropped a result the analysis still holds"
        );
        // Once the analysis lets go, neither the cache nor the shared graph table
        // keeps the other's guards alive.
        drop((left, right, hot, again));
        cache.sweep();
        assert_eq!(
            (
                cache.conjunctions.len(),
                cache.disjunctions.len(),
                cache.representatives.len()
            ),
            (0, 0, 0)
        );
        assert_eq!(
            live_choice_graphs(),
            before,
            "released guards pinned their graphs"
        );
    }
}
