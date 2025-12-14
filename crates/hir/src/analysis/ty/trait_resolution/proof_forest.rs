//! The algorithm for the trait resolution here is based on [`Tabled Typeclass Resolution`](https://arxiv.org/abs/2001.04301).
//! Also, [`XSB: Extending Prolog with Tabled Logic Programming`](https://arxiv.org/pdf/1012.5123) is a nice entry point for more detailed discussions about tabled logic solver.

use std::collections::BinaryHeap;

use crate::core::hir_def::HirIngot;
use common::{indexmap::IndexSet, ingot::Ingot};
use cranelift_entity::{PrimaryMap, entity_impl};
use rustc_hash::{FxHashMap, FxHashSet};

use super::{GoalSatisfiability, PredicateListId};
use crate::analysis::{
    HirAnalysisDb,
    ty::{
        binder::Binder,
        canonical::{Canonical, Canonicalized},
        fold::TyFoldable,
        trait_def::{ImplementorId, TraitInstId, impls_for_trait},
        ty_def::{TyData, TyId},
        unify::PersistentUnificationTable,
        visitor::{TyVisitable, TyVisitor},
    },
};
const MAXIMUM_SOLUTION_NUM: usize = 2;
/// The maximum depth of any type that the solver will consider.
///
/// This constant defines the upper limit on the depth of types that the solver
/// will handle. It is used as a termination condition to prevent the solver
/// from entering infinite loops when encountering coinductive cycles. If a
/// solution for subgoal or goal exceeds this limit, the solver stops search and
/// giveup.
const MAXIMUM_TYPE_DEPTH: usize = 256;

/// The query goal.
/// Since `TraitInstId` contains `Self` type as its first argument,
/// the query for `Implements<Ty, Trait<i32>>` is represented as
/// `Trait<Ty, i32>`.
type Goal<'db> = Canonical<TraitInstId<'db>>;
type Solution<'db> = crate::analysis::ty::canonical::Solution<TraitInstId<'db>>;

/// A structure representing a proof forest used for solving trait goals.
///
/// The `ProofForest` contains generator and consumer nodes which work together
/// to find solutions to trait goals. It maintains stacks for generator and
/// consumer nodes to keep track of the solving process, and a mapping from
/// goals to generator nodes to avoid redundant computations.
pub(super) struct ProofForest<'db> {
    ingot: Ingot<'db>,

    /// The root generator node.
    root: GeneratorNode,

    /// An arena of generator nodes.
    g_nodes: PrimaryMap<GeneratorNode, GeneratorNodeData<'db>>,
    /// An arena of consumer nodes.
    c_nodes: PrimaryMap<ConsumerNode, ConsumerNodeData<'db>>,
    /// A stack of generator nodes to be processed.
    g_stack: Vec<GeneratorNode>,
    /// A binary heap used for managing consumer nodes and their solutions.
    ///
    /// This heap stores tuples of [`OrderedConsumerNode`] and [`Solution`],
    /// allowing the solver to efficiently retrieve and prioritize
    /// consumer nodes that are closer to the original goal.
    c_heap: BinaryHeap<(OrderedConsumerNode, Solution<'db>)>,

    /// A mapping from goals to generator nodes.
    goal_to_node: FxHashMap<Goal<'db>, GeneratorNode>,

    /// The list of assumptions.
    assumptions: PredicateListId<'db>,

    /// The maximum number of solutions.
    maximum_solution_num: usize,
    /// The database for HIR analysis.
    db: &'db dyn HirAnalysisDb,
}

/// A structure representing an ordered consumer node in the proof forest.
///
/// The `OrderedConsumerNode` contains a consumer node and its root generator
/// node. It is used to prioritize consumer nodes based on their proximity to
/// the original goal. This allows the solver to efficiently retrieve and
/// process consumer nodes that are closer to the original goal, improving the
/// overall solving process.
#[derive(Debug, PartialEq, Eq)]
struct OrderedConsumerNode {
    node: ConsumerNode,
    root: GeneratorNode,
}
impl PartialOrd for OrderedConsumerNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for OrderedConsumerNode {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.root.cmp(&self.root)
    }
}

impl<'db> ProofForest<'db> {
    /// Creates a new `ProofForest` with the given initial goal and assumptions.
    ///
    /// This function initializes the proof forest with a root generator node
    /// for the given goal and sets up the necessary data structures for
    /// solving trait goals.
    ///
    /// # Parameters
    /// - `db`: A reference to the HIR analysis database.
    /// - `goal`: The initial trait goal to be solved.
    /// - `assumptions`: The list of assumptions to be used during the solving
    ///   process.
    ///
    /// # Returns
    /// A new instance of `ProofForest` initialized with the given goal and
    /// assumptions.
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        ingot: Ingot<'db>,
        goal: Goal<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let assumptions = assumptions.extend_all_bounds(db);

        let mut forest = Self {
            ingot,
            root: GeneratorNode(0), // Set temporary root.
            g_nodes: PrimaryMap::new(),
            c_nodes: PrimaryMap::new(),
            g_stack: Vec::new(),
            c_heap: BinaryHeap::new(),
            goal_to_node: FxHashMap::default(),
            assumptions,
            maximum_solution_num: MAXIMUM_SOLUTION_NUM,
            db,
        };

        let root = forest.new_generator_node(goal);
        forest.root = root;
        forest
    }

    /// Solves the trait goal using a proof forest approach.
    ///
    /// This function iteratively processes generator and consumer nodes until
    /// either the maximum number of solutions is found or no more nodes can
    /// be processed. The solving process involves:
    /// - Popping solutions from the consumer stack and applying them.
    /// - Stepping through generator nodes to find new solutions or sub-goals.
    /// - Registering solutions and propagating them to dependent consumer
    ///   nodes.
    ///
    /// The function returns `GoalSatisfiability` indicating the status of the
    /// goal:
    /// - `Satisfied` if exactly one solution is found.
    /// - `UnSat` if no solutions are found and an unresolved subgoal is
    ///   identified.
    /// - `NeedsConfirmation` if multiple solutions are found.
    pub(super) fn solve(mut self) -> GoalSatisfiability<'db> {
        loop {
            if self.g_nodes[self.root].solutions.len() >= self.maximum_solution_num {
                break;
            }

            if let Some((c_node, solution)) = self.c_heap.pop() {
                if !c_node.node.apply_solution(&mut self, solution) {
                    return GoalSatisfiability::NeedsConfirmation(IndexSet::default());
                }
                continue;
            }

            if let Some(&g_node) = self.g_stack.last() {
                if !g_node.step(&mut self) {
                    self.g_stack.pop();
                }
                continue;
            }

            break;
        }

        let solutions = std::mem::take(&mut self.g_nodes[self.root].solutions);
        match solutions.len() {
            1 => GoalSatisfiability::Satisfied(solutions.into_iter().next().unwrap()),
            0 => {
                let unresolved_subgoal = self.root.unresolved_subgoal(&mut self);
                GoalSatisfiability::UnSat(unresolved_subgoal)
            }
            _ => GoalSatisfiability::NeedsConfirmation(solutions),
        }
    }

    fn new_generator_node(&mut self, goal: Goal<'db>) -> GeneratorNode {
        let ingot = self.ingot;
        let g_node_data = GeneratorNodeData::new(self.db, ingot, goal, self.assumptions);
        let g_node = self.g_nodes.push(g_node_data);
        self.goal_to_node.insert(goal, g_node);
        self.g_stack.push(g_node);
        g_node
    }

    /// Creates a new consumer node and registers it with the proof forest.
    ///
    /// This function takes a root generator node, a list of remaining goals,
    /// and a persistent unification table. It creates a consumer node that
    /// represents a sub-goal that needs to be solved and remaining
    /// subgoals. If the goal is not already associated with a generator
    /// node, a new generator node is created for it.
    ///
    /// The consumer node is then registered as a dependent of the corresponding
    /// generator node, ensuring that solutions found for the generator node are
    /// propagated to the consumer node.
    ///
    /// # Parameters
    /// - `root`: The root generator node of the consumer node.
    /// - `remaining_goals`: A list of trait instances that represent the
    ///   remaining goals to be solved.
    /// - `table`: A persistent unification table used for managing unification
    ///   operations.
    ///
    /// # Returns
    /// A new `ConsumerNode` that is registered with the proof forest.
    fn new_consumer_node(
        &mut self,
        root: GeneratorNode,
        mut remaining_goals: Vec<TraitInstId<'db>>,
        table: PersistentUnificationTable<'db>,
    ) -> ConsumerNode {
        let query = remaining_goals.pop().unwrap();
        let canonicalized_query = Canonicalized::new(self.db, query);
        let goal = canonicalized_query.value;

        let c_node_data = ConsumerNodeData {
            applied_solutions: FxHashSet::default(),
            remaining_goals,
            root,
            query: (query, canonicalized_query),
            table,
            children: Vec::new(),
        };

        let c_node = self.c_nodes.push(c_node_data);
        if !self.goal_to_node.contains_key(&goal) {
            self.new_generator_node(goal);
        }

        self.goal_to_node[&goal].add_dependent(self, c_node);
        c_node
    }
}

/// A structure representing the data associated with a generator node in the
/// proof forest.
///
/// The `GeneratorNodeData` contains information about the goal, the unification
/// table, the candidate implementors, the solutions found, and the dependents
/// of the generator node. It also keeps track of the assumptions, the next
/// candidate to be processed, and the child consumer nodes.
struct GeneratorNodeData<'db> {
    table: PersistentUnificationTable<'db>,
    /// The canonical goal associated with the generator node.
    goal: Goal<'db>,
    /// The trait instance extracted from the goal.
    extracted_goal: TraitInstId<'db>,
    /// A set of solutions found for the goal.
    solutions: IndexSet<Solution<'db>>,
    ///  A list of consumer nodes that depend on this generator node.
    dependents: Vec<ConsumerNode>,
    ///  A list of candidate implementors for the trait.
    cands: &'db [Binder<ImplementorId<'db>>],
    /// The list of assumptions for the goal.
    assumptions: PredicateListId<'db>,
    /// The index of the next candidate to be tried.
    next_cand: usize,
    /// A list of child consumer nodes created for sub-goals.
    children: Vec<ConsumerNode>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct GeneratorNode(u32);
entity_impl!(GeneratorNode);

impl<'db> GeneratorNodeData<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        ingot: Ingot<'db>,
        goal: Goal<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let mut table = PersistentUnificationTable::new(db);
        let extracted_goal = goal.extract_identity(&mut table);
        let cands = impls_for_trait(db, ingot, goal);

        Self {
            table,
            goal,
            extracted_goal,
            solutions: IndexSet::default(),
            dependents: Vec::new(),
            cands,
            assumptions,
            next_cand: 0,
            children: Vec::new(),
        }
    }
}

impl GeneratorNode {
    /// Registers the given solution with the proof forest and propagates it to
    /// dependent consumer nodes.
    ///
    /// This function canonicalizes the solution and inserts it into the set of
    /// solutions for the generator node. If the solution is new, it
    /// propagates the solution to all dependent consumer nodes.
    ///
    /// # Parameters
    /// - `pf`: A mutable reference to the `ProofForest`.
    /// - `table`: A mutable reference to the `PersistentUnificationTable` used
    ///   for managing unification operations.
    fn register_solution_with<'db>(
        self,
        pf: &mut ProofForest<'db>,
        table: &mut PersistentUnificationTable<'db>,
    ) {
        let g_node = &mut pf.g_nodes[self];
        let solution = g_node
            .goal
            .canonicalize_solution(table.db, table, g_node.extracted_goal);
        if g_node.solutions.insert(solution) {
            for &c_node in g_node.dependents.iter() {
                let ordered_c_node = OrderedConsumerNode {
                    node: c_node,
                    root: pf.c_nodes[c_node].root,
                };
                pf.c_heap.push((ordered_c_node, solution));
            }
        }
    }

    /// Advances the solving process for the generator node.
    ///
    /// This function attempts to find a new solution or sub-goal for the
    /// generator node. It iterates through the candidate implementors and
    /// assumptions, unifying them with the goal. If a solution is found, it
    /// is registered. If a sub-goal is found, a new consumer node is
    /// created to handle it.
    ///
    /// # Parameters
    /// - `pf`: A mutable reference to the `ProofForest`.
    ///
    /// # Returns
    /// `true` if a new solution or sub-goal was found and processed; `false`
    /// otherwise.
    fn step(self, pf: &mut ProofForest) -> bool {
        let g_node = &mut pf.g_nodes[self];
        let db = pf.db;
        // Normalize against the ingot where the query originates so associated type
        // projections can resolve using the impls visible from the call-site.
        let scope = pf.ingot.root_mod(db).scope();
        let normalized_goal = g_node
            .extracted_goal
            .normalize(db, scope, g_node.assumptions);

        while let Some(&cand) = g_node.cands.get(g_node.next_cand) {
            g_node.next_cand += 1;

            let mut table = g_node.table.clone();
            let gen_cand = table.instantiate_with_fresh_vars(cand);

            // TODO: require candidates to be pre-normalized
            // Normalize trait instance arguments before unification
            let normalized_gen_cand = {
                gen_cand
                    .trait_inst(db)
                    .normalize(db, scope, g_node.assumptions)
            };

            if table.unify(normalized_gen_cand, normalized_goal).is_err() {
                continue;
            }

            let constraints = gen_cand.constraints(db);

            if constraints.list(db).is_empty() {
                self.register_solution_with(pf, &mut table);
            } else {
                let sub_goals = {
                    constraints
                        .list(db)
                        .iter()
                        .map(|c| c.fold_with(db, &mut table))
                        .collect()
                };
                let child = pf.new_consumer_node(self, sub_goals, table);
                pf.g_nodes[self].children.push(child);
            }

            return true;
        }

        let mut next_cand = g_node.next_cand - g_node.cands.len();
        while let Some(&assumption) = g_node.assumptions.list(db).get(next_cand) {
            g_node.next_cand += 1;
            next_cand += 1;
            let mut table = g_node.table.clone();
            if table.unify(assumption, normalized_goal).is_ok() {
                self.register_solution_with(pf, &mut table);
                return true;
            }
        }

        false
    }

    fn add_dependent(self, pf: &mut ProofForest, dependent: ConsumerNode) {
        let g_node = &mut pf.g_nodes[self];
        g_node.dependents.push(dependent);
        for &solution in g_node.solutions.iter() {
            let ordered_c_node = OrderedConsumerNode {
                node: dependent,
                root: pf.c_nodes[dependent].root,
            };
            pf.c_heap.push((ordered_c_node, solution))
        }
    }

    fn unresolved_subgoal<'db>(self, pf: &mut ProofForest<'db>) -> Option<Solution<'db>> {
        let g_node = &pf.g_nodes[self];
        // If the child nodes branch out more than one, we give up identifying the
        // unresolved subgoal to avoid generating a large number of uncertain unresolved
        // subgoals.
        if g_node.children.len() != 1 {
            return None;
        }

        let child = g_node.children[0];
        child.unresolved_subgoal(pf)
    }
}

struct ConsumerNodeData<'db> {
    /// Holds solutions that are already applied.
    applied_solutions: FxHashSet<Solution<'db>>,
    remaining_goals: Vec<TraitInstId<'db>>,
    /// The root generator node of the consumer node.
    root: GeneratorNode,

    /// The current pending query that is resolved by another [`GeneratorNode`].
    query: (TraitInstId<'db>, Canonicalized<'db, TraitInstId<'db>>),
    table: PersistentUnificationTable<'db>,
    children: Vec<ConsumerNode>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ConsumerNode(u32);
entity_impl!(ConsumerNode);

impl ConsumerNode {
    /// Applies a given solution to the consumer node.
    ///
    /// This function checks if the solution has already been applied. If not,
    /// it attempts to unify the solution with the pending query of the
    /// consumer node. If the unification is successful and there are no
    /// remaining goals, the solution is registered with the root generator
    /// node. If there are remaining goals, a new consumer node is created
    /// to handle them.
    ///
    /// # Parameters
    /// - `pf`: A mutable reference to the `ProofForest`.
    /// - `solution`: The solution to be applied.
    fn apply_solution<'db>(self, pf: &mut ProofForest<'db>, solution: Solution<'db>) -> bool {
        let c_node = &mut pf.c_nodes[self];

        // If the solutions is already applied, do nothing.
        if !c_node.applied_solutions.insert(solution) {
            return true;
        }

        let mut table = c_node.table.clone();
        let db = pf.db;

        // Extract solution to the current env.
        let (pending_inst, canonicalized_pending_inst) = &c_node.query;
        let solution = canonicalized_pending_inst.extract_solution(&mut table, solution);

        // Normalize both instances before unification
        let normalized_pending = {
            let scope = pending_inst.ingot(db).root_mod(db).scope();
            let assumptions = pf.g_nodes[c_node.root].assumptions;
            pending_inst
                .fold_with(db, &mut table)
                .normalize(db, scope, assumptions)
        };

        let normalized_solution = {
            let scope = solution.ingot(db).root_mod(db).scope();
            let assumptions = pf.g_nodes[c_node.root].assumptions;
            solution
                .fold_with(db, &mut table)
                .normalize(db, scope, assumptions)
        };

        // Try to unifies pending inst and solution.
        if table
            .unify(normalized_pending, normalized_solution)
            .is_err()
        {
            return true;
        }

        let tree_root = c_node.root;

        if c_node.remaining_goals.is_empty() {
            // If no remaining goals in the consumer node, it's the solution for the root
            // goal.
            tree_root.register_solution_with(pf, &mut table);
        } else {
            // Create a child consumer node for the subgoals.
            let remaining_goals = c_node.remaining_goals.clone();
            let child = pf.new_consumer_node(tree_root, remaining_goals, table);
            pf.c_nodes[self].children.push(child);
        }

        maximum_ty_depth(db, solution) <= MAXIMUM_TYPE_DEPTH
    }

    fn unresolved_subgoal<'db>(self, pf: &mut ProofForest<'db>) -> Option<Solution<'db>> {
        let c_node = &mut pf.c_nodes[self];
        if c_node.children.len() != 1 {
            let unsat = c_node.query.0;
            let unsat =
                pf.g_nodes[c_node.root]
                    .goal
                    .canonicalize_solution(pf.db, &mut c_node.table, unsat);
            return Some(unsat);
        }

        c_node.children[0].unresolved_subgoal(pf)
    }
}

/// Computes the depth of a given type.
///
/// The depth of a type is defined as the maximum depth of its subcomponents
/// plus one. For example, a simple type like `i32` has a depth of 1, while a
/// compound type like `Option<Result<i32, String>>` would have a depth
/// reflecting the nesting of its components.
///
/// # Parameters
/// - `db`: A reference to the HIR analysis database.
/// - `ty`: The type for which the depth is to be computed.
///
/// # Returns
/// The depth of the type as a `usize`.
///
/// # Note
/// This function is a stop gap solution to ensure termination when the solver
/// encounters coinductive cycles. It serves as a temporary solution until the
/// solver can properly handle coinductive cycles.
#[salsa::tracked]
pub(crate) fn ty_depth_impl<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> usize {
    match ty.data(db) {
        TyData::ConstTy(cty) => ty_depth_impl(db, cty.ty(db)),
        TyData::Invalid(_)
        | TyData::Never
        | TyData::TyBase(_)
        | TyData::TyParam(_)
        | TyData::AssocTy { .. }
        | TyData::TyVar(_) => 1,
        TyData::QualifiedTy(trait_inst) => ty_depth_impl(db, trait_inst.self_ty(db)) + 1,
        TyData::TyApp(lhs, rhs) => {
            let lhs_depth = ty_depth_impl(db, *lhs);
            let rhs_depth = ty_depth_impl(db, *rhs);
            std::cmp::max(lhs_depth, rhs_depth) + 1
        }
    }
}

/// Computes the maximum depth of any type within a visitable structure.
///
/// This function traverses the given visitable structure and computes the
/// maximum depth of any type it encounters. The depth of a type is defined
/// as the maximum depth of its subcomponents plus one. For example, a simple
/// type like `i32` has a depth of 1, while a compound type like
/// `Option<Result<i32, String>>` would have a depth reflecting the nesting
/// of its components.
///
/// # Parameters
/// - `db`: A reference to the HIR analysis database.
/// - `v`: The visitable structure for which the maximum type depth is to be
///   computed.
///
/// # Returns
/// The maximum depth of any type within the visitable structure as a `usize`.
///
/// # Note
/// This function is a stop gap solution to ensure termination when the solver
/// encounters coinductive cycles. It serves as a temporary solution until the
/// solver can properly handle coinductive cycles.
fn maximum_ty_depth<'db, V>(db: &'db dyn HirAnalysisDb, v: V) -> usize
where
    V: TyVisitable<'db>,
{
    struct DepthVisitor<'db> {
        db: &'db dyn HirAnalysisDb,
        max_depth: usize,
    }

    impl<'db> TyVisitor<'db> for DepthVisitor<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId) {
            let depth = ty_depth_impl(self.db, ty);
            if depth > self.max_depth {
                self.max_depth = depth;
            }
        }
    }

    let mut visitor = DepthVisitor { db, max_depth: 0 };
    v.visit_with(&mut visitor);
    visitor.max_depth
}
