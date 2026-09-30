//! Reduced ordered decision graphs with canonical, allocation-independent node numbering.
#[cfg(test)]
use std::cell::Cell;
#[cfg(any(test, feature = "borrowck-profile"))]
use std::sync::{
    Mutex, OnceLock,
    atomic::{AtomicUsize, Ordering as AtomicOrdering},
};

#[cfg(feature = "borrowck-profile")]
use super::profile::{self, Operation};

use rustc_hash::{FxHashMap, FxHashSet, FxHasher};
use std::{
    cmp::Ordering,
    fmt::{self, Debug},
    hash::{Hash, Hasher},
    ops::Deref,
    sync::{Arc, Weak},
};

#[cfg(test)]
thread_local! {
    pub(crate) static INTERN_ATTEMPTS: Cell<usize> = const { Cell::new(0) };
    static APPLY_VISITS: Cell<usize> = const { Cell::new(0) };
}

#[cfg(test)]
pub(super) fn intern_attempts() -> usize {
    INTERN_ATTEMPTS.get()
}

#[cfg(test)]
pub(super) fn apply_visits() -> usize {
    APPLY_VISITS.get()
}

/// What identifies a completed graph for accounting: its canonical hash, node
/// count, node size and node type. The type belongs here because graphs of
/// different node types share sizes and can share a hash, and counting two of
/// them as copies of one structure reports duplication that does not exist.
#[cfg(any(test, feature = "borrowck-profile"))]
pub(super) type GraphFingerprint = (u64, usize, usize, &'static str);

#[cfg(any(test, feature = "borrowck-profile"))]
#[derive(Debug, Default)]
pub(super) struct LiveGraphs {
    pub(super) bytes: [usize; 2],
    pub(super) copies: FxHashMap<GraphFingerprint, usize>,
}

#[cfg(any(test, feature = "borrowck-profile"))]
thread_local! {
    static LIVE_GRAPHS: Arc<Mutex<LiveGraphs>> = Arc::default();
}

#[cfg(any(test, feature = "borrowck-profile"))]
pub(super) fn live_graphs() -> Arc<Mutex<LiveGraphs>> {
    LIVE_GRAPHS.with(Arc::clone)
}

#[cfg(any(test, feature = "borrowck-profile"))]
#[derive(Debug)]
struct GraphAllocation {
    key: GraphFingerprint,
    owner: Arc<Mutex<LiveGraphs>>,
}

#[cfg(any(test, feature = "borrowck-profile"))]
fn graph_allocations() -> &'static Mutex<FxHashMap<usize, GraphAllocation>> {
    static ALLOCATIONS: OnceLock<Mutex<FxHashMap<usize, GraphAllocation>>> = OnceLock::new();
    ALLOCATIONS.get_or_init(Mutex::default)
}

#[cfg(any(test, feature = "borrowck-profile"))]
impl GraphAllocation {
    fn record(key: GraphFingerprint) -> Option<usize> {
        #[cfg(feature = "borrowck-profile")]
        if !cfg!(test) && !profile::enabled() {
            return None;
        }
        let owner = live_graphs();
        {
            let mut live = owner.lock().expect("graph accounting");
            let (_, nodes, bytes, name) = key;
            live.bytes[usize::from(name.contains("SlotChoice"))] += nodes * bytes;
            if cfg!(test) || nodes >= 1024 {
                *live.copies.entry(key).or_default() += 1;
            }
        }
        // Graphs remain immutable map keys. Their diagnostic ownership record
        // lives in this table and is released by the backing allocation.
        static NEXT: AtomicUsize = AtomicUsize::new(0);
        let id = NEXT
            .try_update(AtomicOrdering::Relaxed, AtomicOrdering::Relaxed, |id| {
                id.checked_add(1)
            })
            .expect("graph allocation identifiers exhausted");
        graph_allocations()
            .lock()
            .expect("allocation accounting")
            .insert(id, Self { key, owner });
        Some(id)
    }

    fn release(&self) {
        let released = {
            let mut live = self.owner.lock().expect("graph accounting");
            let (_, nodes, bytes, name) = self.key;
            let category = usize::from(name.contains("SlotChoice"));
            let bytes = live.bytes[category].checked_sub(nodes * bytes);
            let tracked = cfg!(test) || nodes >= 1024;
            let copies = if tracked {
                live.copies
                    .get(&self.key)
                    .and_then(|copies| copies.checked_sub(1))
            } else {
                Some(0)
            };
            if let (Some(bytes), Some(copies)) = (bytes, copies) {
                live.bytes[category] = bytes;
                if tracked && copies == 0 {
                    live.copies.remove(&self.key);
                } else if tracked {
                    live.copies.insert(self.key, copies);
                }
                Some(())
            } else {
                None
            }
        };
        // An invariant failure must not poison counters used by later destructors.
        released.expect("counted graph storage");
    }
}

/// Diagnostic: the structures with more than one live backing allocation, as
/// (node count, node size, copies).
#[cfg(test)]
pub(super) fn duplicated_graphs() -> Vec<(usize, &'static str, usize)> {
    LIVE_GRAPHS.with(|owner| {
        let stats = owner.lock().expect("graph accounting");
        let live = &stats.copies;
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
    LIVE_GRAPHS.with(|owner| {
        let stats = owner.lock().expect("graph accounting");
        let live = &stats.copies;
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

impl<V, T> Node<V, T> {
    fn leaf_value(&self) -> Option<&T> {
        match self {
            Self::Leaf(value) => Some(value),
            Self::Branch { .. } => None,
        }
    }
}

/// A child position indexes its own graph's node list. Reduced graphs stay far
/// below `u32::MAX` nodes, and a narrower child shrinks every branch.
type Child = u32;

fn child(index: usize) -> Child {
    Child::try_from(index).expect("a decision graph fits u32 nodes")
}

/// Keep weak interning metadata separate from the large backing buffer. An
/// `Arc<[Node]>` would retain that entire allocation until its last weak owner
/// disappeared; this small owner drops its boxed buffer with its last strong one.
struct GraphNodes<V, T> {
    nodes: Box<[Node<V, T>]>,
    #[cfg(any(test, feature = "borrowck-profile"))]
    accounting: Option<usize>,
}

impl<V: Debug, T: Debug> Debug for GraphNodes<V, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GraphNodes").field(&self.nodes).finish()
    }
}

// The allocation, not a handle's sampled strong count, owns its accounting.
// Its final owner may be released concurrently or on a cleanup thread.
#[cfg(any(test, feature = "borrowck-profile"))]
impl<V, T> Drop for GraphNodes<V, T> {
    fn drop(&mut self) {
        if let Some(id) = self.accounting {
            let accounting = graph_allocations()
                .lock()
                .expect("allocation accounting")
                .remove(&id);
            accounting.expect("counted allocation").release();
        }
    }
}

impl<V, T> Deref for GraphNodes<V, T> {
    type Target = [Node<V, T>];
    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

#[derive(Clone, Debug)]
pub(super) struct Decision<V, T> {
    // Postorder, low edge first. The last node is the root; no unreachable nodes remain.
    nodes: Arc<GraphNodes<V, T>>,
    // Guards nest decisions and sit inside interned values, so hash each graph
    // once rather than on every enclosing hash.
    hash: u64,
}

/// Exact diagnostic union of subgraphs across distinct completed arrays. Child
/// IDs refer to this counter's table, so equality includes the entire subtree.
#[cfg(any(test, feature = "borrowck-profile"))]
pub(super) struct SubgraphCounter<V, T> {
    nodes: FxHashMap<Node<V, T>, Child>,
}

#[cfg(any(test, feature = "borrowck-profile"))]
impl<V: Clone + Eq + Hash, T: Clone + Eq + Hash> SubgraphCounter<V, T> {
    pub(super) fn new() -> Self {
        Self {
            nodes: FxHashMap::default(),
        }
    }

    pub(super) fn record(&mut self, graph: &Decision<V, T>) {
        let mut mapped = Vec::with_capacity(graph.nodes.len());
        for node in graph.nodes.iter() {
            let node = match node {
                Node::Leaf(value) => Node::Leaf(value.clone()),
                Node::Branch {
                    variable,
                    low,
                    high,
                } => Node::Branch {
                    variable: variable.clone(),
                    low: mapped[*low as usize],
                    high: mapped[*high as usize],
                },
            };
            let next = child(self.nodes.len());
            mapped.push(*self.nodes.entry(node).or_insert(next));
        }
    }

    pub(super) fn unique_nodes(&self) -> usize {
        self.nodes.len()
    }
}

impl<V: Hash, T: Hash> Decision<V, T> {
    fn new(nodes: Box<[Node<V, T>]>) -> Self {
        let mut hasher = FxHasher::default();
        nodes.hash(&mut hasher);
        let hash = hasher.finish();
        #[cfg(any(test, feature = "borrowck-profile"))]
        let accounting = GraphAllocation::record((
            hash,
            nodes.len(),
            size_of::<Node<V, T>>(),
            std::any::type_name::<Node<V, T>>(),
        ));
        #[cfg(feature = "borrowck-profile")]
        profile::created((
            hash,
            nodes.len(),
            size_of::<Node<V, T>>(),
            std::any::type_name::<Node<V, T>>(),
        ));
        Self {
            hash,
            nodes: Arc::new(GraphNodes {
                nodes,
                #[cfg(any(test, feature = "borrowck-profile"))]
                accounting,
            }),
        }
    }
}

/// A stable, non-owning cache identity. Holding this handle prevents allocation
/// address reuse, even after the node buffer has been reclaimed. Equality and
/// hashing never change when its strong owners disappear.
#[derive(Clone, Debug)]
pub(super) struct WeakDecision<V, T> {
    nodes: Weak<GraphNodes<V, T>>,
    hash: u64,
}

impl<V, T> WeakDecision<V, T> {
    pub(super) fn upgrade(&self) -> Option<Decision<V, T>> {
        Some(Decision {
            nodes: self.nodes.upgrade()?,
            hash: self.hash,
        })
    }

    pub(super) fn is_live(&self) -> bool {
        self.nodes.strong_count() > 0
    }
}

impl<V, T> PartialEq for WeakDecision<V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.nodes.ptr_eq(&other.nodes)
    }
}
impl<V, T> Eq for WeakDecision<V, T> {}
impl<V, T> Hash for WeakDecision<V, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.nodes.as_ptr().hash(state);
    }
}

impl<V: PartialEq, T: PartialEq> PartialEq for Decision<V, T> {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash
            && (Arc::ptr_eq(&self.nodes, &other.nodes) || self.nodes.nodes == other.nodes.nodes)
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
            self.nodes.nodes.cmp(&other.nodes.nodes)
        }
    }
}

impl<V, T> Decision<V, T> {
    pub(super) fn downgrade(&self) -> WeakDecision<V, T> {
        WeakDecision {
            nodes: Arc::downgrade(&self.nodes),
            hash: self.hash,
        }
    }

    pub(super) fn structural_hash(&self) -> u64 {
        self.hash
    }

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

struct ApplyBranch<V> {
    variable: V,
    low: (Child, Child),
    high: (Child, Child),
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
        left: &OrderedView<'_, V, T, impl Fn(&V) -> V>,
        lhs: usize,
        right: &OrderedView<'_, V, T, impl Fn(&V) -> V>,
        rhs: usize,
        join: &impl Fn(Option<&T>, Option<&T>) -> Option<T>,
        memo: &mut FxHashMap<(Child, Child), usize>,
    ) -> usize {
        let root = (child(lhs), child(rhs));
        // Expand low edges first, then join memoized children without native recursion.
        let mut pending = vec![(root, None)];
        while let Some((key, branch)) = pending.pop() {
            #[cfg(test)]
            if branch.is_none() {
                APPLY_VISITS.set(APPLY_VISITS.get() + 1);
            }
            if memo.contains_key(&key) {
                continue;
            }
            let result = if let Some(ApplyBranch {
                variable,
                low,
                high,
            }) = branch
            {
                self.branch(variable, memo[&low], memo[&high])
            } else {
                let (lhs, rhs) = (key.0 as usize, key.1 as usize);
                if let Some(value) = join(
                    left.decision.nodes[lhs].leaf_value(),
                    right.decision.nodes[rhs].leaf_value(),
                ) {
                    self.intern(Node::Leaf(value))
                } else {
                    let variable = match (left.variable(lhs), right.variable(rhs)) {
                        (Some(left), Some(right)) => left.min(right),
                        (Some(variable), None) | (None, Some(variable)) => variable,
                        (None, None) => unreachable!("two leaves are joined directly"),
                    };
                    let (left_low, left_high) = left.cofactors(lhs, &variable);
                    let (right_low, right_high) = right.cofactors(rhs, &variable);
                    let low = (child(left_low), child(right_low));
                    let high = (child(left_high), child(right_high));
                    pending.push((
                        key,
                        Some(ApplyBranch {
                            variable,
                            low,
                            high,
                        }),
                    ));
                    pending.push((high, None));
                    pending.push((low, None));
                    continue;
                }
            };
            memo.insert(key, result);
        }
        #[cfg(feature = "borrowck-profile")]
        if profile::enabled() && lhs == left.decision.root() && rhs == right.decision.root() {
            profile::scratch(
                self.scratch_bytes()
                    + memo.capacity() * size_of::<((Child, Child), usize)>()
                    + pending.capacity() * size_of::<((Child, Child), Option<ApplyBranch<V>>)>(),
            );
        }
        memo[&root]
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

    /// Container payload capacities; hash-table control bytes and allocator
    /// overhead are excluded. This is allocation accounting, not resident memory.
    #[cfg(feature = "borrowck-profile")]
    fn scratch_bytes(&self) -> usize {
        self.nodes.capacity() * size_of::<Node<V, T>>()
            + self.next.capacity() * size_of::<Child>()
            + self.positions.capacity() * size_of::<(u64, Child)>()
            + self.selections.capacity() * size_of::<((V, usize, usize), usize)>()
    }

    fn finish(self, root: usize) -> Decision<V, T> {
        let mut nodes = Vec::with_capacity(self.nodes.len());
        // Canonical positions are dense, so index them rather than hashing them.
        let mut numbering = vec![NONE; self.nodes.len()];
        // Low-edge-first postorder preserves canonical numbering for shared subgraphs.
        let mut pending = vec![(root, false)];
        while let Some((id, expanded)) = pending.pop() {
            if numbering[id] != NONE {
                continue;
            }
            let node = match &self.nodes[id] {
                Node::Leaf(value) => Node::Leaf(value.clone()),
                Node::Branch { low, high, .. } if !expanded => {
                    pending.push((id, true));
                    pending.push((*high as usize, false));
                    pending.push((*low as usize, false));
                    continue;
                }
                Node::Branch {
                    variable,
                    low,
                    high,
                } => Node::Branch {
                    variable: variable.clone(),
                    low: numbering[*low as usize],
                    high: numbering[*high as usize],
                },
            };
            numbering[id] = child(nodes.len());
            nodes.push(node);
        }
        #[cfg(feature = "borrowck-profile")]
        if profile::enabled() {
            profile::scratch(
                self.scratch_bytes()
                    + nodes.capacity() * size_of::<Node<V, T>>()
                    + numbering.capacity() * size_of::<Child>()
                    + pending.capacity() * size_of::<(usize, bool)>(),
            );
        }
        Decision::new(nodes.into())
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
        self.ordered_view(Clone::clone)
            .restrict(care.ordered_view(Clone::clone), empty, leaf)
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
        self.nodes[self.root()].leaf_value()
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
        if let [Node::Leaf(value)] = &**self.nodes {
            return Decision::leaf(leaf(value));
        }
        #[cfg(feature = "borrowck-profile")]
        let operation = Operation::new(0, self.nodes.len());
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
        let result = builder.finish(mapped[self.root()]);
        #[cfg(feature = "borrowck-profile")]
        drop(operation);
        result
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
        #[cfg(feature = "borrowck-profile")]
        let operation = Operation::new(2, self.nodes.len());
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
        let result = builder.finish(mapped[self.root()]);
        #[cfg(feature = "borrowck-profile")]
        drop(operation);
        result
    }

    pub(super) fn apply(
        &self,
        other: &Self,
        leaf: impl Fn(Option<&T>, Option<&T>) -> Option<T>,
    ) -> Self {
        self.ordered_view(Clone::clone)
            .apply(other.ordered_view(Clone::clone), leaf)
    }

    /// Borrow a graph with a strictly order-preserving, injective variable map.
    /// Each operand keeps its own leaf values and node positions; only variables
    /// are translated, when a traversal reaches them. General substitutions that
    /// merge or reorder variables must use `map` instead.
    pub(super) fn ordered_view<F: Fn(&V) -> V>(&self, variable: F) -> OrderedView<'_, V, T, F> {
        OrderedView {
            decision: self,
            variable,
        }
    }

    /// Relabel without changing topology or canonical node numbering. The variable
    /// map must preserve strict order and the leaf map must be injective on the
    /// graph's leaves, so no branches or leaves become equal after relabeling.
    pub(super) fn relabel_ordered<W: Clone + Ord + Hash, U: Clone + Eq + Hash>(
        &self,
        mut variable: impl FnMut(&V) -> W,
        mut leaf: impl FnMut(&T) -> U,
    ) -> Decision<W, U> {
        #[cfg(feature = "borrowck-profile")]
        let operation = Operation::new(0, self.nodes.len());
        let result = Decision::new(
            self.nodes
                .iter()
                .map(|node| match node {
                    Node::Leaf(value) => Node::Leaf(leaf(value)),
                    Node::Branch {
                        variable: key,
                        low,
                        high,
                    } => Node::Branch {
                        variable: variable(key),
                        low: *low,
                        high: *high,
                    },
                })
                .collect(),
        );
        #[cfg(feature = "borrowck-profile")]
        drop(operation);
        result
    }

    fn root(&self) -> usize {
        self.nodes.len() - 1
    }

    pub(super) fn node_count(&self) -> usize {
        self.nodes.len()
    }

    #[cfg(feature = "borrowck-profile")]
    pub(super) fn allocation_owners(&self) -> (usize, usize) {
        (
            Arc::as_ptr(&self.nodes) as usize,
            Arc::strong_count(&self.nodes),
        )
    }

    #[cfg(feature = "borrowck-profile")]
    pub(super) fn node_size() -> usize {
        size_of::<Node<V, T>>()
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

/// An ordered translation of a borrowed decision, with its original leaf space.
pub(super) struct OrderedView<'a, V, T, F> {
    decision: &'a Decision<V, T>,
    variable: F,
}

impl<V: Clone + Ord + Hash, T: Clone + Eq + Hash, F: Fn(&V) -> V> OrderedView<'_, V, T, F> {
    fn variable(&self, node: usize) -> Option<V> {
        match &self.decision.nodes[node] {
            Node::Leaf(_) => None,
            Node::Branch { variable, .. } => Some((self.variable)(variable)),
        }
    }

    fn cofactors(&self, node: usize, split: &V) -> (usize, usize) {
        match &self.decision.nodes[node] {
            Node::Branch {
                variable,
                low,
                high,
            } if (self.variable)(variable) == *split => (*low as usize, *high as usize),
            _ => (node, node),
        }
    }

    /// Resolve constant results before splitting. `None` denotes a nonterminal
    /// operand; a returned value must hold for every valuation of that operand.
    /// Two terminal operands must always produce a value.
    pub(super) fn apply(
        self,
        other: OrderedView<'_, V, T, impl Fn(&V) -> V>,
        leaf: impl Fn(Option<&T>, Option<&T>) -> Option<T>,
    ) -> Decision<V, T> {
        let (left, right) = (self.decision, other.decision);
        if let Some(value) = leaf(left.leaf_value(), right.leaf_value()) {
            return Decision::leaf(value);
        }
        #[cfg(feature = "borrowck-profile")]
        let operation = Operation::new(1, left.nodes.len() + right.nodes.len());
        let mut builder = Builder::with_capacity(left.nodes.len().max(right.nodes.len()));
        let root = builder.apply_decisions(
            &self,
            left.root(),
            &other,
            right.root(),
            &leaf,
            &mut FxHashMap::with_capacity_and_hasher(
                left.nodes.len() + right.nodes.len(),
                Default::default(),
            ),
        );
        let result = builder.finish(root);
        #[cfg(feature = "borrowck-profile")]
        drop(operation);
        result
    }

    pub(super) fn restrict(
        self,
        care: OrderedView<'_, V, T, impl Fn(&V) -> V>,
        empty: &T,
        leaf: impl Fn(&T, &T) -> Option<T>,
    ) -> Option<Decision<V, T>> {
        #[cfg(feature = "borrowck-profile")]
        let operation = Operation::new(3, self.decision.nodes.len() + care.decision.nodes.len());
        let (source_root, care_root) = (self.decision.root(), care.decision.root());
        let mut context = Restriction {
            source: self,
            care,
            empty: empty.clone(),
            leaf,
            builder: Builder::new(),
            memo: FxHashMap::default(),
        };
        let root = context.visit(source_root, care_root)?;
        let result = context.builder.finish(root);
        #[cfg(feature = "borrowck-profile")]
        drop(operation);
        Some(result)
    }
}

struct Restriction<'a, V, T, L, R, F> {
    source: OrderedView<'a, V, T, L>,
    care: OrderedView<'a, V, T, R>,
    empty: T,
    leaf: F,
    builder: Builder<V, T>,
    memo: FxHashMap<(usize, usize), Option<usize>>,
}

impl<V, T, L, R, F> Restriction<'_, V, T, L, R, F>
where
    V: Clone + Ord + Hash,
    T: Clone + Eq + Hash,
    L: Fn(&V) -> V,
    R: Fn(&V) -> V,
    F: Fn(&T, &T) -> Option<T>,
{
    fn visit(&mut self, source: usize, care: usize) -> Option<usize> {
        if let Some(result) = self.memo.get(&(source, care)) {
            return *result;
        }
        if matches!(&self.care.decision.nodes[care], Node::Leaf(value) if *value == self.empty) {
            return None;
        }
        let result = match (
            &self.source.decision.nodes[source],
            &self.care.decision.nodes[care],
        ) {
            (Node::Leaf(value), Node::Leaf(care)) => {
                (self.leaf)(value, care).map(|value| self.builder.intern(Node::Leaf(value)))
            }
            _ => {
                let variable = match (self.source.variable(source), self.care.variable(care)) {
                    (Some(left), Some(right)) => left.min(right),
                    (Some(variable), None) | (None, Some(variable)) => variable,
                    (None, None) => unreachable!("two leaves are restricted directly"),
                };
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
    use std::{array, cell::RefCell, panic::catch_unwind, sync::Barrier, thread};

    use super::*;

    thread_local! {
        static EXIT_GRAPH: RefCell<Option<Decision<u32, bool>>> = const { RefCell::new(None) };
    }

    #[test]
    fn deep_join_and_completion_use_bounded_native_stack() {
        thread::Builder::new()
            .stack_size(128 * 1024)
            .spawn(|| {
                for expected in [false, true] {
                    let left = Decision::chain(
                        (0..8192_u32).map(|variable| (variable, expected)),
                        true,
                        false,
                    );
                    let right = Decision::chain([(8192, expected)], true, false);
                    let joined = left.apply(&right, |left, right| {
                        left.zip(right).map(|(left, right)| *left && *right)
                    });
                    assert_eq!(
                        joined,
                        Decision::chain(
                            (0..=8192_u32).map(|variable| (variable, expected)),
                            true,
                            false,
                        )
                    );
                }
                let equal = Decision::equal_bits(
                    (0..4096_u32)
                        .rev()
                        .map(|variable| (variable * 2, variable * 2 + 1)),
                    true,
                    false,
                );
                assert_eq!(
                    equal.apply(&Decision::leaf(true), |left, right| left
                        .zip(right)
                        .map(|(left, right)| *left && *right)),
                    equal
                );
            })
            .unwrap()
            .join()
            .unwrap();
    }

    fn allocation_is_live(id: usize) -> bool {
        graph_allocations().lock().unwrap().contains_key(&id)
    }

    #[test]
    fn graph_accounting_follows_allocations_across_threads_and_concurrent_drops() {
        let before = live_graph_storage();
        let graph = Decision::chain((0..4096).map(|variable| (variable, true)), true, false);
        let id = graph.nodes.accounting.unwrap();
        let weak = graph.downgrade();
        let counted = live_graph_storage();
        let clone = graph.clone();
        let upgraded = weak.upgrade().unwrap();
        assert_eq!(
            live_graph_storage(),
            counted,
            "handles must not count allocations"
        );
        assert!(allocation_is_live(id));
        drop((clone, upgraded));
        thread::spawn(move || drop(graph)).join().unwrap();
        assert!(!weak.is_live());
        assert!(!allocation_is_live(id));
        assert_eq!(live_graph_storage(), before);

        let graph = Decision::chain((0..4096).map(|variable| (variable, true)), true, false);
        let id = graph.nodes.accounting.unwrap();
        let weak = graph.downgrade();
        let barrier = Barrier::new(3);
        thread::scope(|scope| {
            for graph in [graph.clone(), graph] {
                let barrier = &barrier;
                scope.spawn(move || {
                    barrier.wait();
                    drop(graph);
                });
            }
            barrier.wait();
        });
        assert!(!weak.is_live());
        assert!(!allocation_is_live(id));
        assert_eq!(live_graph_storage(), before);
    }

    #[test]
    fn graph_release_outlives_creator_without_retaining_counters() {
        let (graph, owner) = thread::spawn(|| {
            let graph: Decision<u32, bool> = Decision::leaf(true);
            (graph, live_graphs())
        })
        .join()
        .unwrap();
        let id = graph.nodes.accounting.unwrap();
        let weak = graph.downgrade();
        let weak_owner = Arc::downgrade(&owner);
        assert!(allocation_is_live(id));
        assert_eq!(owner.lock().unwrap().copies.values().sum::<usize>(), 1);
        drop(graph);
        assert!(!weak.is_live());
        assert!(!allocation_is_live(id));
        {
            let stats = owner.lock().unwrap();
            assert_eq!(stats.bytes, [0, 0]);
            assert!(stats.copies.is_empty());
        }
        assert_eq!(Arc::strong_count(&owner), 1);
        drop(owner);
        assert!(weak_owner.upgrade().is_none());
    }

    #[test]
    fn graph_final_release_during_receiver_thread_local_teardown() {
        let before = live_graph_storage();
        let graph = Decision::chain((0..4096).map(|variable| (variable, true)), true, false);
        let id = graph.nodes.accounting.unwrap();
        let weak = graph.downgrade();
        thread::spawn(move || EXIT_GRAPH.with_borrow_mut(|slot| *slot = Some(graph)))
            .join()
            .unwrap();
        assert!(!weak.is_live());
        assert!(!allocation_is_live(id));
        assert_eq!(live_graph_storage(), before);
    }

    #[test]
    fn equal_graphs_from_separate_creators_ignore_diagnostic_identity() {
        let make = || Decision::chain((0..32_u32).map(|variable| (variable, true)), true, false);
        let left = thread::spawn(make).join().unwrap();
        let right = thread::spawn(make).join().unwrap();
        assert_ne!(left.nodes.accounting, right.nodes.accounting);
        assert_eq!(left, right);
        assert_eq!(left.cmp(&right), Ordering::Equal);
        let hashes = [&left, &right].map(|graph| {
            let mut hasher = FxHasher::default();
            graph.hash(&mut hasher);
            hasher.finish()
        });
        assert_eq!(hashes[0], hashes[1]);
        let debug = format!("{left:?}");
        assert_eq!(debug, format!("{right:?}"));
        assert!(!debug.contains("accounting"));
    }

    #[test]
    fn accounting_invariant_failure_does_not_poison_creator_counters() {
        let owner = Arc::new(Mutex::new(LiveGraphs::default()));
        let accounting = GraphAllocation {
            key: (0, 1, 1, "invalid counter"),
            owner: owner.clone(),
        };
        assert!(catch_unwind(|| accounting.release()).is_err());
        let stats = owner
            .lock()
            .expect("invariant failure must leave the mutex usable");
        assert_eq!(stats.bytes, [0, 0]);
        assert!(stats.copies.is_empty());
    }

    #[test]
    fn weak_graph_handles_keep_identity_without_owning_node_buffers() {
        let before = live_graph_storage();
        let graph = Decision::chain((0..4096).map(|variable| (variable, true)), true, false);
        let weak = graph.downgrade();
        let again = weak.upgrade().unwrap();
        assert_eq!(graph, again);
        assert_eq!(weak, again.downgrade());
        let mut hasher = FxHasher::default();
        weak.hash(&mut hasher);
        let hash = hasher.finish();
        drop((graph, again));
        assert!(!weak.is_live());
        assert!(weak.upgrade().is_none());
        assert_eq!(live_graph_storage(), before);
        let mut hasher = FxHasher::default();
        weak.hash(&mut hasher);
        assert_eq!(hasher.finish(), hash);
        let rebuilt = Decision::chain((0..4096).map(|variable| (variable, true)), true, false);
        assert_ne!(
            weak,
            rebuilt.downgrade(),
            "expired cache identities cannot alias a new allocation"
        );
    }

    #[test]
    fn subgraph_accounting_counts_shared_suffixes_exactly() {
        let left = Decision::chain([(0, true), (2, true)], true, false);
        let right = Decision::chain([(1, true), (2, true)], true, false);
        assert_eq!(left.node_count() + right.node_count(), 8);
        let mut counter = SubgraphCounter::new();
        counter.record(&left);
        counter.record(&right);
        assert_eq!(counter.unique_nodes(), 5);
        counter.record(&left);
        assert_eq!(counter.unique_nodes(), 5);
        counter.record(&Decision::chain([(0, false), (2, true)], true, false));
        assert_eq!(counter.unique_nodes(), 6);
    }

    #[test]
    fn absorbing_terminals_skip_variable_translation_in_excluded_subgraphs() {
        for absorbing in [false, true] {
            let mask = Decision::chain([(0, true)], !absorbing, absorbing);
            let join = |left: Option<&bool>, right: Option<&bool>| {
                if left == Some(&absorbing) || right == Some(&absorbing) {
                    Some(absorbing)
                } else {
                    left.zip(right).map(|_| !absorbing)
                }
            };
            for size in [16, 256] {
                let excluded = Decision::chain(
                    [(0, false)]
                        .into_iter()
                        .chain((1..=size).map(|variable| (variable, true))),
                    !absorbing,
                    absorbing,
                );
                let constant = Decision::leaf(absorbing);
                for (left, right) in [
                    (&mask, &excluded),
                    (&excluded, &mask),
                    (&constant, &excluded),
                    (&excluded, &constant),
                ] {
                    let visits = Cell::new(0);
                    let translate = |variable: &i32| {
                        visits.set(visits.get() + 1);
                        variable + 1
                    };
                    let before = intern_attempts();
                    let joined = left
                        .ordered_view(translate)
                        .apply(right.ordered_view(translate), join);
                    assert!(joined.is_leaf(&absorbing));
                    assert!(intern_attempts() - before <= 4);
                    assert!(
                        visits.get() <= 4,
                        "{} translations visited an excluded {size}-node subgraph",
                        visits.get()
                    );
                }
            }
        }
    }

    #[test]
    fn partial_terminal_apply_matches_exhaustive_boolean_truth_tables() {
        let from_table = |table: u8| {
            let mut builder = Builder::new();
            let leaves: [_; 4] =
                array::from_fn(|bit| builder.intern(Node::Leaf(table & (1 << bit) != 0)));
            let low = builder.branch(1u8, leaves[0], leaves[1]);
            let high = builder.branch(1, leaves[2], leaves[3]);
            let root = builder.branch(0, low, high);
            builder.finish(root)
        };
        let evaluate = |decision: &Decision<u8, bool>, assignment: u8| {
            let mut node = decision.root();
            loop {
                match decision.nodes[node] {
                    Node::Leaf(value) => break value,
                    Node::Branch {
                        variable,
                        low,
                        high,
                    } => {
                        node = if assignment & (1 << variable) == 0 {
                            low
                        } else {
                            high
                        } as usize;
                    }
                }
            }
        };
        for left_table in 0..16u8 {
            for right_table in 0..16u8 {
                let (left, right) = (from_table(left_table), from_table(right_table));
                for (left_slots, right_slots) in
                    [([0, 2], [1, 2]), ([1, 3], [0, 3]), ([0, 1], [0, 1])]
                {
                    // Every binary Boolean operator, including noncommutative ones.
                    for operation in 0..16u8 {
                        let output = |left: bool, right: bool| {
                            operation & (1 << (usize::from(left) * 2 + usize::from(right))) != 0
                        };
                        let joined = left
                            .ordered_view(|key| left_slots[usize::from(*key)])
                            .apply(
                                right.ordered_view(|key| right_slots[usize::from(*key)]),
                                |left, right| {
                                    let mut possibilities = [
                                        (false, false),
                                        (false, true),
                                        (true, false),
                                        (true, true),
                                    ]
                                    .into_iter()
                                    .filter(|(a, b)| {
                                        left.is_none_or(|left| left == a)
                                            && right.is_none_or(|right| right == b)
                                    })
                                    .map(|(a, b)| output(a, b));
                                    let first = possibilities.next().unwrap();
                                    possibilities.all(|value| value == first).then_some(first)
                                },
                            );
                        for assignment in 0..16u8 {
                            let lookup = |table, slots: [u8; 2]| {
                                let row = ((assignment >> slots[0]) & 1) * 2
                                    + ((assignment >> slots[1]) & 1);
                                table & (1 << row) != 0
                            };
                            assert_eq!(
                                evaluate(&joined, assignment),
                                output(
                                    lookup(left_table, left_slots),
                                    lookup(right_table, right_slots)
                                )
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn borrowed_operations_match_materialized_ordered_maps() {
        let from_table = |table: [u8; 4]| {
            let mut builder = Builder::new();
            let leaves: Vec<_> = table
                .into_iter()
                .map(|value| builder.intern(Node::Leaf(value)))
                .collect();
            let low = builder.branch(1u8, leaves[0], leaves[1]);
            let high = builder.branch(1, leaves[2], leaves[3]);
            let root = builder.branch(0, low, high);
            builder.finish(root)
        };
        let decisions: Vec<_> = (0u8..16)
            .map(|bits| array::from_fn(|assignment| (bits >> assignment) & 1))
            .chain([[2, 3, 5, 7]])
            .map(from_table)
            .collect();
        for left in &decisions {
            for right in &decisions {
                for (left_slots, right_slots) in
                    [([0, 2], [1, 2]), ([1, 3], [0, 3]), ([0, 1], [0, 1])]
                {
                    let left_map = |key: &u8| left_slots[usize::from(*key)];
                    let right_map = |key: &u8| right_slots[usize::from(*key)];
                    let eager_left = left.map(|key| Variable::Symbol(left_map(key)), Clone::clone);
                    let eager_right =
                        right.map(|key| Variable::Symbol(right_map(key)), Clone::clone);
                    // Relabeling may permute leaves but must keep them distinct.
                    assert_eq!(
                        left.relabel_ordered(left_map, |leaf| 255 - leaf),
                        eager_left.map(|key| Variable::Symbol(*key), |leaf| 255 - leaf)
                    );
                    let join = |left: Option<&u8>, right: Option<&u8>| {
                        left.zip(right).map(|(left, right)| left * 7 + right)
                    };
                    assert_eq!(
                        left.ordered_view(left_map)
                            .apply(right.ordered_view(right_map), join),
                        eager_left.apply(&eager_right, join)
                    );
                    let restrict = |left: &u8, right: &u8| (*right != 0).then_some(left ^ right);
                    assert_eq!(
                        left.ordered_view(left_map).restrict(
                            right.ordered_view(right_map),
                            &0,
                            restrict
                        ),
                        eager_left.restrict(&eager_right, &0, restrict)
                    );
                }
            }
        }
    }

    #[test]
    fn ordered_renaming_preserves_correlated_boolean_decisions() {
        let choice = |variable| Decision::chain([(variable, true)], true, false);
        let source = choice(0).apply(
            &choice(1).apply(&choice(2), |left, right| {
                left.zip(right).map(|(left, right)| *left || *right)
            }),
            |left, right| left.zip(right).map(|(left, right)| *left && *right),
        );
        let expected = choice(10).apply(
            &choice(11).apply(&choice(12), |left, right| {
                left.zip(right).map(|(left, right)| *left || *right)
            }),
            |left, right| left.zip(right).map(|(left, right)| *left && *right),
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
            choice(10).apply(&choice(12), |left, right| left
                .zip(right)
                .map(|(left, right)| *left && *right))
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
        let left = x.apply(&y, |left, right| {
            left.zip(right).map(|(left, right)| *left && *right)
        });
        let right = not_x.apply(&z, |left, right| {
            left.zip(right).map(|(left, right)| *left && *right)
        });
        let relation = left.apply(&right, |left, right| {
            left.zip(right).map(|(left, right)| *left || *right)
        });
        let union = y.apply(&z, |left, right| {
            left.zip(right).map(|(left, right)| *left || *right)
        });

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
        let parity = first.apply(&second, |left, right| {
            left.zip(right).map(|(left, right)| left ^ right)
        });
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
