//! Reduced ordered decision graphs with canonical, allocation-independent node numbering.
#[cfg(test)]
use std::cell::{Cell, RefCell};

use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use std::{
    cmp::Ordering,
    hash::{Hash, Hasher},
    sync::Arc,
};

#[cfg(test)]
thread_local! {
    pub(crate) static INTERN_ATTEMPTS: Cell<usize> = const { Cell::new(0) };
}

/// What identifies a completed graph for accounting: its canonical hash, node
/// count, node size and node type. The type belongs here because graphs of
/// different node types share sizes and can share a hash, and counting two of
/// them as copies of one structure reports duplication that does not exist.
#[cfg(test)]
type GraphFingerprint = (u64, usize, usize, &'static str);

#[cfg(test)]
thread_local! {
    // Live backing allocations per completed graph, keyed by its canonical hash,
    // node count and node size. Separate operations that complete an equal graph
    // each allocate their own slice, so a structure can hold several copies.
    static LIVE_GRAPHS: RefCell<FxHashMap<GraphFingerprint, usize>> = RefCell::default();
}

/// Diagnostic: the structures with more than one live backing allocation, as
/// (node count, node size, copies).
#[cfg(test)]
pub(super) fn duplicated_graphs() -> Vec<(usize, &'static str, usize)> {
    LIVE_GRAPHS.with_borrow(|live| {
        live.iter()
            .filter(|(_, copies)| **copies > 1)
            .map(|((_, nodes, _, name), copies)| (*nodes, *name, *copies))
            .collect()
    })
}

/// Live completed-graph storage: backing allocations, distinct structures, and the
/// bytes held beyond one copy of each structure.
#[cfg(test)]
pub(super) fn live_graph_storage() -> (usize, usize, usize) {
    LIVE_GRAPHS.with_borrow(|live| {
        (
            live.values().sum(),
            live.len(),
            live.iter()
                .map(|((_, nodes, bytes, _), copies)| nodes * bytes * (copies - 1))
                .sum(),
        )
    })
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Node<V, T> {
    Leaf(T),
    Branch {
        variable: V,
        low: Child,
        high: Child,
    },
}

/// A child position indexes its own graph's node list. Reduced graphs stay far
/// below `u32::MAX` nodes, and a narrower child shrinks every branch.
type Child = u32;

fn child(index: usize) -> Child {
    Child::try_from(index).expect("a decision graph fits u32 nodes")
}

#[derive(Clone, Debug)]
pub(super) struct Decision<V, T> {
    // Postorder, low edge first. The last node is the root; no unreachable nodes remain.
    nodes: Arc<[Node<V, T>]>,
    // Guards nest decisions and sit inside interned values, so hash each graph
    // once rather than on every enclosing hash.
    hash: u64,
}

impl<V: Hash, T: Hash> Decision<V, T> {
    fn new(nodes: Arc<[Node<V, T>]>) -> Self {
        let mut hasher = FxHasher::default();
        nodes.hash(&mut hasher);
        let hash = hasher.finish();
        #[cfg(test)]
        LIVE_GRAPHS.with_borrow_mut(|live| {
            *live
                .entry((
                    hash,
                    nodes.len(),
                    size_of::<Node<V, T>>(),
                    std::any::type_name::<Node<V, T>>(),
                ))
                .or_default() += 1;
        });
        Self { hash, nodes }
    }
}

// Test accounting only: the last owner of a backing slice releases its storage.
#[cfg(test)]
impl<V, T> Drop for Decision<V, T> {
    fn drop(&mut self) {
        if Arc::strong_count(&self.nodes) > 1 {
            return;
        }
        let key = (
            self.hash,
            self.nodes.len(),
            size_of::<Node<V, T>>(),
            std::any::type_name::<Node<V, T>>(),
        );
        // A thread may drop graphs after its own thread locals are destroyed.
        let _ = LIVE_GRAPHS.try_with(|live| {
            let mut live = live.borrow_mut();
            let copies = live.get_mut(&key).expect("counted graph");
            *copies -= 1;
            if *copies == 0 {
                live.remove(&key);
            }
        });
    }
}

impl<V: PartialEq, T: PartialEq> PartialEq for Decision<V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
            && (Arc::ptr_eq(&self.nodes, &other.nodes) || self.nodes == other.nodes)
    }
}

impl<V: Eq, T: Eq> Eq for Decision<V, T> {}

impl<V: Hash, T: Hash> Hash for Decision<V, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl<V: Ord, T: Ord> Ord for Decision<V, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        if Arc::ptr_eq(&self.nodes, &other.nodes) {
            Ordering::Equal
        } else {
            self.nodes.cmp(&other.nodes)
        }
    }
}

impl<V, T> Decision<V, T> {
    /// The derived node order, with variables and leaves compared by the caller,
    /// which a slot-indexed decision needs since its slots name its own tables.
    pub(super) fn cmp_by<W, U>(
        &self,
        other: &Decision<W, U>,
        mut variable: impl FnMut(&V, &W) -> Ordering,
        mut leaf: impl FnMut(&T, &U) -> Ordering,
    ) -> Ordering {
        for (left, right) in self.nodes.iter().zip(other.nodes.iter()) {
            let ordering = match (left, right) {
                (Node::Leaf(left), Node::Leaf(right)) => leaf(left, right),
                (Node::Leaf(_), Node::Branch { .. }) => Ordering::Less,
                (Node::Branch { .. }, Node::Leaf(_)) => Ordering::Greater,
                (
                    Node::Branch {
                        variable: left,
                        low: left_low,
                        high: left_high,
                    },
                    Node::Branch {
                        variable: right,
                        low: right_low,
                        high: right_high,
                    },
                ) => variable(left, right)
                    .then(left_low.cmp(right_low))
                    .then(left_high.cmp(right_high)),
            };
            if ordering.is_ne() {
                return ordering;
            }
        }
        self.nodes.len().cmp(&other.nodes.len())
    }
}

impl<V: Ord, T: Ord> PartialOrd for Decision<V, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Debug)]
pub(super) enum Variable<V> {
    Constant(bool),
    Symbol(V),
}

struct Builder<V, T> {
    nodes: Vec<Node<V, T>>,
    // Node positions by hash, chained through `next` for equal hashes. Keying this
    // by the node itself stored every node twice, once here and once in `nodes`.
    positions: FxHashMap<u64, Child>,
    next: Vec<Child>,
    selections: FxHashMap<(V, usize, usize), usize>,
}

/// No node, so an empty chain link and an unvisited position.
const NONE: Child = Child::MAX;

impl<V: Clone + Ord + Hash, T: Clone + Eq + Hash> Builder<V, T> {
    fn new() -> Self {
        Self::with_capacity(0)
    }

    fn with_capacity(nodes: usize) -> Self {
        Self {
            nodes: Vec::with_capacity(nodes),
            positions: FxHashMap::with_capacity_and_hasher(nodes, Default::default()),
            next: Vec::with_capacity(nodes),
            selections: FxHashMap::default(),
        }
    }

    fn intern(&mut self, node: Node<V, T>) -> usize {
        #[cfg(test)]
        INTERN_ATTEMPTS.set(INTERN_ATTEMPTS.get() + 1);
        let mut hasher = FxHasher::default();
        node.hash(&mut hasher);
        let hash = hasher.finish();
        let mut position = self.positions.get(&hash).copied().unwrap_or(NONE);
        while position != NONE {
            if self.nodes[position as usize] == node {
                return position as usize;
            }
            position = self.next[position as usize];
        }
        let id = self.nodes.len();
        // The chain link is whichever node held this hash before.
        self.next
            .push(self.positions.insert(hash, child(id)).unwrap_or(NONE));
        self.nodes.push(node);
        id
    }

    fn branch(&mut self, variable: V, low: usize, high: usize) -> usize {
        if low == high {
            low
        } else {
            self.intern(Node::Branch {
                variable,
                low: child(low),
                high: child(high),
            })
        }
    }

    fn variable(&self, node: usize) -> Option<&V> {
        match &self.nodes[node] {
            Node::Leaf(_) => None,
            Node::Branch { variable, .. } => Some(variable),
        }
    }

    fn cofactors(&self, node: usize, split: &V) -> (usize, usize) {
        match &self.nodes[node] {
            Node::Branch {
                variable,
                low,
                high,
            } if variable == split => (*low as usize, *high as usize),
            _ => (node, node),
        }
    }

    /// An `idempotent` join is also commutative, as in quantification, so
    /// equal operands and swapped pairs share work.
    fn apply(
        &mut self,
        lhs: usize,
        rhs: usize,
        join: &impl Fn(&T, &T) -> T,
        idempotent: bool,
        memo: &mut FxHashMap<(usize, usize), usize>,
    ) -> usize {
        if idempotent && lhs == rhs {
            return lhs;
        }
        let key = if idempotent {
            (lhs.min(rhs), lhs.max(rhs))
        } else {
            (lhs, rhs)
        };
        if let Some(result) = memo.get(&key) {
            return *result;
        }
        // Keep recursive frames small: only the split variable lives across calls.
        let result =
            if let (Node::Leaf(left), Node::Leaf(right)) = (&self.nodes[lhs], &self.nodes[rhs]) {
                let leaf = join(left, right);
                self.intern(Node::Leaf(leaf))
            } else {
                let variable = match (self.variable(lhs), self.variable(rhs)) {
                    (Some(left), Some(right)) => left.min(right),
                    (Some(variable), None) | (None, Some(variable)) => variable,
                    (None, None) => unreachable!("two leaves are joined directly"),
                }
                .clone();
                let (left_low, left_high) = self.cofactors(lhs, &variable);
                let (right_low, right_high) = self.cofactors(rhs, &variable);
                let low = self.apply(left_low, right_low, join, idempotent, memo);
                let high = self.apply(left_high, right_high, join, idempotent, memo);
                self.branch(variable, low, high)
            };
        memo.insert(key, result);
        result
    }

    /// Apply over two finished graphs, reading them where they already live. Copying
    /// both operands into the builder first doubled its nodes and its interning work
    /// for every conjunction and union, and the copies were never the result.
    fn apply_decisions(
        &mut self,
        left: &Decision<V, T>,
        lhs: usize,
        right: &Decision<V, T>,
        rhs: usize,
        join: &impl Fn(&T, &T) -> T,
        memo: &mut FxHashMap<(Child, Child), usize>,
    ) -> usize {
        let key = (child(lhs), child(rhs));
        if let Some(result) = memo.get(&key) {
            return *result;
        }
        // Keep recursive frames small: only the split variable lives across calls.
        let result = match (&left.nodes[lhs], &right.nodes[rhs]) {
            (Node::Leaf(left), Node::Leaf(right)) => self.intern(Node::Leaf(join(left, right))),
            _ => {
                let variable = match (left.variable(lhs), right.variable(rhs)) {
                    (Some(left), Some(right)) => left.min(right),
                    (Some(variable), None) | (None, Some(variable)) => variable,
                    (None, None) => unreachable!("two leaves are joined directly"),
                }
                .clone();
                let (left_low, left_high) = left.cofactors(lhs, &variable);
                let (right_low, right_high) = right.cofactors(rhs, &variable);
                let low = self.apply_decisions(left, left_low, right, right_low, join, memo);
                let high = self.apply_decisions(left, left_high, right, right_high, join, memo);
                self.branch(variable, low, high)
            }
        };
        memo.insert(key, result);
        result
    }

    // Substitution may reorder variables or identify two decisions. Rebuild by Shannon
    // expansion; directly relabeling an ordered graph would violate its invariant.
    fn select(&mut self, variable: V, low: usize, high: usize) -> usize {
        if low == high {
            return low;
        }
        let key = (variable.clone(), low, high);
        if let Some(result) = self.selections.get(&key) {
            return *result;
        }
        let first = self
            .variable(low)
            .into_iter()
            .chain(self.variable(high))
            .chain([&variable])
            .min()
            .unwrap()
            .clone();
        let (low_false, low_true) = self.cofactors(low, &first);
        let (high_false, high_true) = self.cofactors(high, &first);
        let (low, high) = if first == variable {
            (low_false, high_true)
        } else {
            (
                self.select(variable.clone(), low_false, high_false),
                self.select(variable, low_true, high_true),
            )
        };
        let result = self.branch(first, low, high);
        self.selections.insert(key, result);
        result
    }

    fn finish(self, root: usize) -> Decision<V, T> {
        let mut nodes = Vec::with_capacity(self.nodes.len());
        // Canonical positions are dense, so index them rather than hashing them.
        let mut numbering = vec![NONE; self.nodes.len()];
        self.visit(root, &mut nodes, &mut numbering);
        Decision::new(nodes.into())
    }

    fn visit(&self, id: usize, nodes: &mut Vec<Node<V, T>>, numbering: &mut [Child]) -> usize {
        if numbering[id] != NONE {
            return numbering[id] as usize;
        }
        let node = match &self.nodes[id] {
            Node::Leaf(value) => Node::Leaf(value.clone()),
            Node::Branch {
                variable,
                low,
                high,
            } => Node::Branch {
                variable: variable.clone(),
                low: child(self.visit(*low as usize, nodes, numbering)),
                high: child(self.visit(*high as usize, nodes, numbering)),
            },
        };
        let canonical = nodes.len();
        nodes.push(node);
        numbering[id] = child(canonical);
        canonical
    }
}

impl<V: Clone + Ord + Hash, T: Clone + Eq + Hash> Decision<V, T> {
    /// Pairs arrive from the least significant bit to the most significant bit.
    /// The variable order must interleave both words by bit significance.
    pub(super) fn equal_bits(
        pairs: impl IntoIterator<Item = (V, V)>,
        accepted: T,
        rejected: T,
    ) -> Self {
        let mut builder = Builder::new();
        let reject = builder.intern(Node::Leaf(rejected));
        let mut tail = builder.intern(Node::Leaf(accepted));
        for (left, right) in pairs {
            let (left, right) = if left <= right {
                (left, right)
            } else {
                (right, left)
            };
            if left == right {
                continue;
            }
            let low = builder.branch(right.clone(), tail, reject);
            let high = builder.branch(right, reject, tail);
            tail = builder.branch(left, low, high);
        }
        builder.finish(tail)
    }

    /// Canonical completion outside a set of feasible valuations. An infeasible
    /// branch takes its sibling's value and therefore contributes no decision.
    pub(super) fn restrict(
        &self,
        care: &Self,
        empty: &T,
        leaf: impl Fn(&T, &T) -> Option<T>,
    ) -> Option<Self> {
        let mut builder = Builder::new();
        let mut context = Restriction {
            source: self,
            care,
            empty,
            leaf: &leaf,
            builder: &mut builder,
            memo: FxHashMap::default(),
        };
        let root = context.visit(self.root(), care.root())?;
        Some(builder.finish(root))
    }
    pub(super) fn leaf(value: T) -> Self {
        Self::new(vec![Node::Leaf(value)].into())
    }

    pub(super) fn chain(
        steps: impl IntoIterator<Item = (V, bool)>,
        accepted: T,
        rejected: T,
    ) -> Self {
        let mut builder = Builder::new();
        let mut tail = builder.intern(Node::Leaf(accepted));
        let reject = builder.intern(Node::Leaf(rejected));
        let mut steps: Vec<_> = steps.into_iter().collect();
        steps.sort_unstable_by(|lhs, rhs| rhs.0.cmp(&lhs.0));
        for (variable, expected) in steps {
            tail = if expected {
                builder.select(variable, reject, tail)
            } else {
                builder.select(variable, tail, reject)
            };
        }
        builder.finish(tail)
    }

    pub(super) fn is_leaf(&self, expected: &T) -> bool {
        self.leaf_value() == Some(expected)
    }

    /// The whole graph's value, when it decides nothing.
    pub(super) fn leaf_value(&self) -> Option<&T> {
        match &self.nodes[self.root()] {
            Node::Leaf(value) => Some(value),
            Node::Branch { .. } => None,
        }
    }

    pub(super) fn leaves(&self) -> impl Iterator<Item = &T> {
        self.nodes.iter().filter_map(|node| match node {
            Node::Leaf(value) => Some(value),
            _ => None,
        })
    }

    /// Borrow decision variables without cloning or sorting their payloads.
    /// A variable may occur at more than one node; consumers needing unique
    /// variables collect only those keys they actually use.
    pub(super) fn variables(&self) -> impl Iterator<Item = &V> {
        self.nodes.iter().filter_map(|node| match node {
            Node::Branch { variable, .. } => Some(variable),
            _ => None,
        })
    }

    pub(super) fn map<W: Clone + Ord + Hash, U: Clone + Eq + Hash>(
        &self,
        mut variable: impl FnMut(&V) -> Variable<W>,
        mut leaf: impl FnMut(&T) -> U,
    ) -> Decision<W, U> {
        let mut builder = Builder::with_capacity(self.nodes.len());
        let mut mapped = Vec::with_capacity(self.nodes.len());
        for node in self.nodes.iter() {
            let id = match node {
                Node::Leaf(value) => builder.intern(Node::Leaf(leaf(value))),
                Node::Branch {
                    variable: key,
                    low,
                    high,
                } => {
                    let (low, high) = (mapped[*low as usize], mapped[*high as usize]);
                    match variable(key) {
                        Variable::Constant(value) => {
                            if value {
                                high
                            } else {
                                low
                            }
                        }
                        // A rename ordered before both mapped children keeps this
                        // branch ordered. Other substitutions can identify or reorder
                        // decisions and need select.
                        Variable::Symbol(key)
                            if [low, high].into_iter().all(|child| {
                                builder.variable(child).is_none_or(|child| key < *child)
                            }) =>
                        {
                            builder.branch(key, low, high)
                        }
                        Variable::Symbol(key) => builder.select(key, low, high),
                    }
                }
            };
            mapped.push(id);
        }
        builder.finish(mapped[self.root()])
    }

    /// Existentially quantify selected decisions using an associative,
    /// commutative, idempotent terminal join.
    pub(super) fn exists(
        &self,
        mut selected: impl FnMut(&V) -> bool,
        join: impl Fn(&T, &T) -> T,
    ) -> Self {
        // Ask about each distinct variable once, in decision order.
        let mut variables: Vec<_> = self
            .variables()
            .collect::<FxHashSet<_>>()
            .into_iter()
            .collect();
        variables.sort_unstable();
        let quantified: FxHashMap<_, _> = variables
            .into_iter()
            .map(|variable| (variable, selected(variable)))
            .collect();
        if !quantified.values().any(|selected| *selected) {
            return self.clone();
        }
        let mut builder = Builder::with_capacity(self.nodes.len());
        let mut mapped = Vec::with_capacity(self.nodes.len());
        let mut memo = FxHashMap::default();
        for node in self.nodes.iter() {
            let result = match node {
                Node::Leaf(value) => builder.intern(Node::Leaf(value.clone())),
                Node::Branch {
                    variable,
                    low,
                    high,
                } if quantified[variable] => builder.apply(
                    mapped[*low as usize],
                    mapped[*high as usize],
                    &join,
                    true,
                    &mut memo,
                ),
                Node::Branch {
                    variable,
                    low,
                    high,
                } => builder.branch(
                    variable.clone(),
                    mapped[*low as usize],
                    mapped[*high as usize],
                ),
            };
            mapped.push(result);
        }
        builder.finish(mapped[self.root()])
    }

    pub(super) fn apply(&self, other: &Self, leaf: impl Fn(&T, &T) -> T) -> Self {
        let mut builder = Builder::with_capacity(self.nodes.len().max(other.nodes.len()));
        let root = builder.apply_decisions(
            self,
            self.root(),
            other,
            other.root(),
            &leaf,
            &mut FxHashMap::default(),
        );
        builder.finish(root)
    }

    fn variable(&self, node: usize) -> Option<&V> {
        match &self.nodes[node] {
            Node::Leaf(_) => None,
            Node::Branch { variable, .. } => Some(variable),
        }
    }

    fn cofactors(&self, node: usize, split: &V) -> (usize, usize) {
        match &self.nodes[node] {
            Node::Branch {
                variable,
                low,
                high,
            } if variable == split => (*low as usize, *high as usize),
            _ => (node, node),
        }
    }

    fn root(&self) -> usize {
        self.nodes.len() - 1
    }

    pub(super) fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// No other decision holds this backing graph, so keeping it shares nothing.
    pub(super) fn is_sole_owner(&self) -> bool {
        Arc::strong_count(&self.nodes) == 1
    }

    pub(super) fn witness(&self, mut accepted: impl FnMut(&T) -> bool) -> Option<Vec<(V, bool)>> {
        let mut possible = Vec::with_capacity(self.nodes.len());
        for node in self.nodes.iter() {
            possible.push(match node {
                Node::Leaf(value) => accepted(value),
                Node::Branch { low, high, .. } => {
                    possible[*low as usize] || possible[*high as usize]
                }
            });
        }
        if !possible[self.root()] {
            return None;
        }
        let mut path = Vec::new();
        let mut node = self.root();
        while let Node::Branch {
            variable,
            low,
            high,
        } = &self.nodes[node]
        {
            let value = !possible[*low as usize];
            path.push((variable.clone(), value));
            node = if value { *high as usize } else { *low as usize };
        }
        Some(path)
    }
}

impl<V: Clone + Ord + Hash> Decision<V, bool> {
    pub(super) fn less_bits(bits: impl IntoIterator<Item = (Variable<V>, Variable<V>)>) -> Self {
        let mut builder = Builder::new();
        let reject = builder.intern(Node::Leaf(false));
        let accept = builder.intern(Node::Leaf(true));
        let mut tail = reject;
        for (left, right) in bits {
            let (low, high) = match right {
                Variable::Constant(false) => (tail, reject),
                Variable::Constant(true) => (accept, tail),
                Variable::Symbol(right) => (
                    builder.select(right.clone(), tail, accept),
                    builder.select(right, reject, tail),
                ),
            };
            tail = match left {
                Variable::Constant(false) => low,
                Variable::Constant(true) => high,
                Variable::Symbol(left) => builder.select(left, low, high),
            };
        }
        builder.finish(tail)
    }

    pub(super) fn upper_bound_bits(bits: impl IntoIterator<Item = (V, bool)>) -> Self {
        let mut builder = Builder::new();
        let reject = builder.intern(Node::Leaf(false));
        let accept = builder.intern(Node::Leaf(true));
        let mut tail = reject;
        for (variable, bound) in bits {
            tail = if bound {
                builder.branch(variable, accept, tail)
            } else {
                builder.branch(variable, tail, reject)
            };
        }
        builder.finish(tail)
    }
}

struct Restriction<'a, V, T, F> {
    source: &'a Decision<V, T>,
    care: &'a Decision<V, T>,
    empty: &'a T,
    leaf: &'a F,
    builder: &'a mut Builder<V, T>,
    memo: FxHashMap<(usize, usize), Option<usize>>,
}

impl<V: Clone + Ord + Hash, T: Clone + Eq + Hash, F: Fn(&T, &T) -> Option<T>>
    Restriction<'_, V, T, F>
{
    fn visit(&mut self, source: usize, care: usize) -> Option<usize> {
        if let Some(result) = self.memo.get(&(source, care)) {
            return *result;
        }
        if matches!(&self.care.nodes[care], Node::Leaf(value) if value == self.empty) {
            return None;
        }
        let result = match (&self.source.nodes[source], &self.care.nodes[care]) {
            (Node::Leaf(value), Node::Leaf(care)) => {
                (self.leaf)(value, care).map(|value| self.builder.intern(Node::Leaf(value)))
            }
            (value, care_value) => {
                let variable = match (value, care_value) {
                    (
                        Node::Branch { variable: left, .. },
                        Node::Branch {
                            variable: right, ..
                        },
                    ) => left.min(right),
                    (Node::Branch { variable, .. }, _) | (_, Node::Branch { variable, .. }) => {
                        variable
                    }
                    _ => unreachable!(),
                }
                .clone();
                let (source_low, source_high) = self.source.cofactors(source, &variable);
                let (care_low, care_high) = self.care.cofactors(care, &variable);
                match (
                    self.visit(source_low, care_low),
                    self.visit(source_high, care_high),
                ) {
                    (Some(low), Some(high)) => Some(self.builder.branch(variable, low, high)),
                    (left, right) => left.or(right),
                }
            }
        };
        self.memo.insert((source, care), result);
        result
    }
}

#[cfg(test)]
mod tests {
    use std::array;

    use super::*;

    #[test]
    fn ordered_renaming_preserves_correlated_boolean_decisions() {
        let choice = |variable| Decision::chain([(variable, true)], true, false);
        let source = choice(0).apply(
            &choice(1).apply(&choice(2), |left, right| *left || *right),
            |left, right| *left && *right,
        );
        let expected = choice(10).apply(
            &choice(11).apply(&choice(12), |left, right| *left || *right),
            |left, right| *left && *right,
        );
        assert_eq!(
            source.map(|key| Variable::Symbol(*key + 10), |value| *value),
            expected
        );
        assert_eq!(
            source.map(
                |key| {
                    if *key == 1 {
                        Variable::Constant(false)
                    } else {
                        Variable::Symbol(*key + 10)
                    }
                },
                |value| *value
            ),
            choice(10).apply(&choice(12), |left, right| *left && *right)
        );
    }

    #[test]
    fn existential_projection_preserves_correlated_alternatives() {
        let choice = |variable| Decision::chain([(variable, true)], true, false);
        let (x, y, z) = (choice(0), choice(1), choice(2));
        let not_x = x.map(
            |variable| super::Variable::Symbol(*variable),
            |value| !value,
        );
        let left = x.apply(&y, |left, right| *left && *right);
        let right = not_x.apply(&z, |left, right| *left && *right);
        let relation = left.apply(&right, |left, right| *left || *right);
        let union = y.apply(&z, |left, right| *left || *right);

        assert_eq!(
            relation.exists(|variable| *variable == 0, |left, right| *left || *right),
            union
        );
        assert_eq!(
            relation.exists(|variable| *variable != 1, |left, right| *left || *right),
            Decision::leaf(true)
        );
    }

    #[test]
    fn quantification_shares_work_across_selected_variables() {
        let decision = Decision::chain((0..256).map(|variable| (variable, true)), true, false);
        for select_all in [true, false] {
            let before = INTERN_ATTEMPTS.get();
            let quantified = decision.exists(
                |variable| select_all || variable % 2 == 0,
                |left, right| *left || *right,
            );
            let attempts = INTERN_ATTEMPTS.get() - before;
            let expected = Decision::chain(
                (0..256)
                    .filter(|variable| !select_all && variable % 2 != 0)
                    .map(|variable| (variable, true)),
                true,
                false,
            );
            assert_eq!(quantified, expected);
            assert!(
                attempts <= decision.node_count() * 8,
                "{attempts} interning attempts for {} source nodes",
                decision.node_count()
            );
        }
    }

    #[test]
    fn quantification_matches_exhaustive_terminal_unions() {
        // Every Boolean function of three variables, plus distinct singleton
        // sets at all eight terminals to exercise non-Boolean joins.
        let tables = (0u16..256)
            .map(|bits| array::from_fn::<_, 8, _>(|assignment| ((bits >> assignment) & 1) as u8))
            .chain([array::from_fn(|assignment| 1u8 << assignment)]);
        for table in tables {
            let mut builder = Builder::new();
            let mut level: Vec<_> = table
                .iter()
                .map(|value| builder.intern(Node::Leaf(*value)))
                .collect();
            for variable in (0u8..3).rev() {
                level = level
                    .as_chunks::<2>()
                    .0
                    .iter()
                    .map(|children| builder.branch(variable, children[0], children[1]))
                    .collect();
            }
            let decision = builder.finish(level[0]);
            for selected in 0u8..8 {
                let quantified = decision.exists(
                    |variable| selected & (1 << (2 - variable)) != 0,
                    |left, right| left | right,
                );
                for assignment in 0u8..8 {
                    let expected = table
                        .iter()
                        .enumerate()
                        .filter(|(other, _)| *other as u8 & !selected == assignment & !selected)
                        .fold(0, |union, (_, value)| union | value);
                    let actual = quantified.map(
                        |variable| {
                            Variable::<u8>::Constant(assignment & (1 << (2 - variable)) != 0)
                        },
                        Clone::clone,
                    );
                    assert!(
                        actual.is_leaf(&expected),
                        "table={table:?}, selected={selected}, assignment={assignment}"
                    );
                }
            }
        }
    }

    #[test]
    fn quantification_visits_shared_variables_once_in_order() {
        let first = Decision::chain([(0u8, true)], true, false);
        let second = Decision::chain([(1u8, true)], true, false);
        let parity = first.apply(&second, |left, right| left ^ right);
        assert_eq!(parity.variables().count(), 3);
        let mut observed = Vec::new();
        let quantified = parity.exists(
            |variable| {
                observed.push(*variable);
                *variable == 1
            },
            |left, right| *left || *right,
        );
        assert_eq!(observed, [0, 1]);
        assert!(quantified.is_leaf(&true));
    }
}
