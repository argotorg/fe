//! Reduced ordered decision graphs with canonical, allocation-independent node numbering.
use rustc_hash::FxHashMap;
use std::{collections::BTreeSet, hash::Hash, sync::Arc};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Node<V, T> {
    Leaf(T),
    Branch {
        variable: V,
        low: usize,
        high: usize,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct Decision<V, T> {
    // Postorder, low edge first. The last node is the root; no unreachable nodes remain.
    nodes: Arc<[Node<V, T>]>,
}

#[derive(Clone, Debug)]
pub(super) enum Variable<V> {
    Constant(bool),
    Symbol(V),
}

struct Builder<V, T> {
    nodes: Vec<Node<V, T>>,
    interned: FxHashMap<Node<V, T>, usize>,
    selections: FxHashMap<(V, usize, usize), usize>,
}

impl<V: Clone + Ord + Hash, T: Clone + Eq + Hash> Builder<V, T> {
    fn new() -> Self {
        Self {
            nodes: Vec::new(),
            interned: FxHashMap::default(),
            selections: FxHashMap::default(),
        }
    }

    fn intern(&mut self, node: Node<V, T>) -> usize {
        if let Some(id) = self.interned.get(&node) {
            return *id;
        }
        let id = self.nodes.len();
        self.nodes.push(node.clone());
        self.interned.insert(node, id);
        id
    }

    fn branch(&mut self, variable: V, low: usize, high: usize) -> usize {
        if low == high {
            low
        } else {
            self.intern(Node::Branch {
                variable,
                low,
                high,
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
            } if variable == split => (*low, *high),
            _ => (node, node),
        }
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
        let mut nodes = Vec::new();
        let mut numbering = FxHashMap::default();
        self.visit(root, &mut nodes, &mut numbering);
        Decision {
            nodes: nodes.into(),
        }
    }

    fn visit(
        &self,
        id: usize,
        nodes: &mut Vec<Node<V, T>>,
        numbering: &mut FxHashMap<usize, usize>,
    ) -> usize {
        if let Some(id) = numbering.get(&id) {
            return *id;
        }
        let node = match &self.nodes[id] {
            Node::Leaf(value) => Node::Leaf(value.clone()),
            Node::Branch {
                variable,
                low,
                high,
            } => Node::Branch {
                variable: variable.clone(),
                low: self.visit(*low, nodes, numbering),
                high: self.visit(*high, nodes, numbering),
            },
        };
        let canonical = nodes.len();
        nodes.push(node);
        numbering.insert(id, canonical);
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
        Self {
            nodes: vec![Node::Leaf(value)].into(),
        }
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
        matches!(&self.nodes[self.root()], Node::Leaf(actual) if actual == expected)
    }

    pub(super) fn leaves(&self) -> impl Iterator<Item = &T> {
        self.nodes.iter().filter_map(|node| match node {
            Node::Leaf(value) => Some(value),
            _ => None,
        })
    }

    pub(super) fn variables(&self) -> BTreeSet<V> {
        self.nodes
            .iter()
            .filter_map(|node| match node {
                Node::Branch { variable, .. } => Some(variable.clone()),
                _ => None,
            })
            .collect()
    }

    pub(super) fn map<W: Clone + Ord + Hash, U: Clone + Eq + Hash>(
        &self,
        mut variable: impl FnMut(&V) -> Variable<W>,
        mut leaf: impl FnMut(&T) -> U,
    ) -> Decision<W, U> {
        let mut builder = Builder::new();
        let mut mapped = Vec::with_capacity(self.nodes.len());
        for node in self.nodes.iter() {
            let id = match node {
                Node::Leaf(value) => builder.intern(Node::Leaf(leaf(value))),
                Node::Branch {
                    variable: key,
                    low,
                    high,
                } => match variable(key) {
                    Variable::Constant(value) => mapped[if value { *high } else { *low }],
                    Variable::Symbol(key) => builder.select(key, mapped[*low], mapped[*high]),
                },
            };
            mapped.push(id);
        }
        builder.finish(mapped[self.root()])
    }

    pub(super) fn apply(&self, other: &Self, leaf: impl Fn(&T, &T) -> T) -> Self {
        let mut builder = Builder::new();
        let root = self.apply_nodes(
            self.root(),
            other,
            other.root(),
            &leaf,
            &mut builder,
            &mut FxHashMap::default(),
        );
        builder.finish(root)
    }

    fn apply_nodes(
        &self,
        lhs: usize,
        other: &Self,
        rhs: usize,
        leaf: &impl Fn(&T, &T) -> T,
        builder: &mut Builder<V, T>,
        memo: &mut FxHashMap<(usize, usize), usize>,
    ) -> usize {
        if let Some(result) = memo.get(&(lhs, rhs)) {
            return *result;
        }
        let result = match (&self.nodes[lhs], &other.nodes[rhs]) {
            (Node::Leaf(left), Node::Leaf(right)) => builder.intern(Node::Leaf(leaf(left, right))),
            (left, right) => {
                let variable = match (left, right) {
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
                };
                let (left_low, left_high) = self.cofactors(lhs, variable);
                let (right_low, right_high) = other.cofactors(rhs, variable);
                let low = self.apply_nodes(left_low, other, right_low, leaf, builder, memo);
                let high = self.apply_nodes(left_high, other, right_high, leaf, builder, memo);
                builder.branch(variable.clone(), low, high)
            }
        };
        memo.insert((lhs, rhs), result);
        result
    }

    fn cofactors(&self, node: usize, split: &V) -> (usize, usize) {
        match &self.nodes[node] {
            Node::Branch {
                variable,
                low,
                high,
            } if variable == split => (*low, *high),
            _ => (node, node),
        }
    }

    fn root(&self) -> usize {
        self.nodes.len() - 1
    }

    pub(super) fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub(super) fn witness(&self, mut accepted: impl FnMut(&T) -> bool) -> Option<Vec<(V, bool)>> {
        let mut possible = Vec::with_capacity(self.nodes.len());
        for node in self.nodes.iter() {
            possible.push(match node {
                Node::Leaf(value) => accepted(value),
                Node::Branch { low, high, .. } => possible[*low] || possible[*high],
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
            let value = !possible[*low];
            path.push((variable.clone(), value));
            node = if value { *high } else { *low };
        }
        Some(path)
    }
}

impl<V: Clone + Ord + Hash> Decision<V, bool> {
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
