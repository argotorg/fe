//! Decision tree generation for efficient pattern matching compilation
//! Based on "Compiling pattern matching to good decision trees"

use super::pattern_analysis::{FeConstructorOracle, to_matchcov_pattern};
use super::pattern_ir::{
    BindingRef, ConstructorKind, PatternStore, ValidatedPatId, ValidatedPatKind,
};
use super::ty_def::TyId;
use crate::analysis::HirAnalysisDb;
use crate::core::hir_def::EnumVariant;
use crate::projection::{Projection as GenericProjection, ProjectionPath as GenericProjectionPath};
use indexmap::IndexMap;
use matchcov::{ConstructorSpace, Pattern, PatternMatrix, PatternRow, Usefulness};

use core::convert::Infallible;

/// Type alias for HIR-specific projections.
///
/// Uses `Infallible` for the index type since pattern matching doesn't support
/// array indexing. This makes `Index(Dynamic(...))` impossible to construct.
pub type Projection<'db> = GenericProjection<TyId<'db>, EnumVariant<'db>, Infallible>;

/// Type alias for HIR-specific projection paths.
///
/// Uses `Infallible` for the index type since pattern matching doesn't support
/// array indexing. This makes `Index(Dynamic(...))` impossible to construct.
pub type ProjectionPath<'db> = GenericProjectionPath<TyId<'db>, EnumVariant<'db>, Infallible>;

/// A decision tree for pattern matching compilation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DecisionTree<'db> {
    /// No arm can be reached because the matched value space is empty.
    Unreachable,
    /// Leaf node - execute this match arm
    Leaf(LeafNode<'db>),
    /// Switch node - test a value and branch
    Switch(SwitchNode<'db>),
}

/// A leaf node in the decision tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LeafNode<'db> {
    pub arm_index: usize,
    pub bindings: IndexMap<BindingRef<'db>, ProjectionPath<'db>>,
}

impl<'db> LeafNode<'db> {
    fn new(arm: ArmData<'db>) -> Self {
        Self {
            arm_index: arm.arm_index,
            bindings: arm.bindings,
        }
    }
}

/// A switch node in the decision tree
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SwitchNode<'db> {
    pub occurrence: ProjectionPath<'db>,
    pub arms: Vec<(Case<'db>, DecisionTree<'db>)>,
}

/// A case in a switch node
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Case<'db> {
    Constructor(ConstructorKind<'db>),
    Default,
}

/// Extension trait for HIR-specific projection path operations.
trait ProjectionPathExt<'db> {
    fn phi_specialize(&self, db: &dyn HirAnalysisDb, ctor: ConstructorKind<'db>) -> Vec<Self>
    where
        Self: Sized;
}

impl<'db> ProjectionPathExt<'db> for ProjectionPath<'db> {
    fn phi_specialize(&self, db: &dyn HirAnalysisDb, ctor: ConstructorKind<'db>) -> Vec<Self> {
        let arity = ctor.arity(db);
        (0..arity)
            .map(|i| {
                let mut path = self.clone();
                let step = match ctor {
                    ConstructorKind::Variant(variant, enum_ty) => Projection::VariantField {
                        variant,
                        enum_ty,
                        field_idx: i,
                    },
                    ConstructorKind::Type(_) | ConstructorKind::Literal(_, _) => {
                        Projection::Field(i)
                    }
                };
                path.push(step);
                path
            })
            .collect()
    }
}

fn collect_pattern_bindings<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    pat: ValidatedPatId,
    path: &ProjectionPath<'db>,
    bindings: &mut IndexMap<BindingRef<'db>, ProjectionPath<'db>>,
) {
    match store.node(pat).kind() {
        ValidatedPatKind::Wildcard {
            binding: Some(binding),
        } => {
            if let Some(previous) = bindings.get(binding) {
                debug_assert_eq!(
                    previous, path,
                    "pattern binding has inconsistent projections"
                );
            } else {
                bindings.insert(*binding, path.clone());
            }
        }
        ValidatedPatKind::Wildcard { binding: None } => {}
        ValidatedPatKind::Constructor { ctor, fields } => {
            let field_paths = path.phi_specialize(db, *ctor);
            debug_assert_eq!(field_paths.len(), fields.len());
            for (&field, field_path) in fields.iter().zip(field_paths) {
                collect_pattern_bindings(db, store, field, &field_path, bindings);
            }
        }
        ValidatedPatKind::Or(alternatives) => {
            // Binding or-patterns are rejected while constructing PatternStore.
            for &alternative in alternatives {
                collect_pattern_bindings(db, store, alternative, path, bindings);
            }
        }
    }
}

/// Column selection policy for decision tree optimization
#[derive(Debug, Clone, Default)]
pub struct ColumnSelectionPolicy(Vec<ColumnScoringFunction>);

impl ColumnSelectionPolicy {
    /// The score of column i is the sum of the negation of the arities of
    /// constructors in sigma(i).
    pub fn arity(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::Arity)
    }

    /// The score is the negation of the cardinal of sigma(i), C(Sigma(i)).
    /// If sigma(i) is NOT complete, the resulting score is C(Sigma(i)) - 1.
    pub fn small_branching(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::SmallBranching)
    }

    /// The score is the number of needed rows of column i in the necessity
    /// matrix.
    pub fn needed_column(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::NeededColumn)
    }

    /// The score is the larger row index j such that column i is needed for all
    /// rows j′; 1 ≤ j′ ≤ j.
    pub fn needed_prefix(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::NeededPrefix)
    }

    fn add_heuristic(&mut self, heuristic: ColumnScoringFunction) -> &mut Self {
        self.0.push(heuristic);
        self
    }

    fn select_column<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        matrix: &DecisionMatrix<'db>,
    ) -> usize {
        let mut candidates: Vec<_> = (0..matrix.ncols()).collect();

        for scoring_fn in &self.0 {
            let mut max_score = i32::MIN;
            for col in std::mem::take(&mut candidates) {
                let score = scoring_fn.score(db, matrix, col);
                match score.cmp(&max_score) {
                    std::cmp::Ordering::Less => {}
                    std::cmp::Ordering::Equal => {
                        candidates.push(col);
                    }
                    std::cmp::Ordering::Greater => {
                        candidates = vec![col];
                        max_score = score;
                    }
                }
            }

            if candidates.len() == 1 {
                return candidates.pop().unwrap();
            }
        }

        // If there are more than one candidates remained, filter the columns with the
        // shortest occurrences among the candidates, then select the rightmost one.
        // This heuristics corresponds to the R pseudo heuristic in the paper.
        let mut shortest_occurrences = usize::MAX;
        for col in std::mem::take(&mut candidates) {
            let occurrences = matrix.occurrences[col].len();
            match occurrences.cmp(&shortest_occurrences) {
                std::cmp::Ordering::Less => {
                    candidates = vec![col];
                    shortest_occurrences = occurrences;
                }
                std::cmp::Ordering::Equal => {
                    candidates.push(col);
                }
                std::cmp::Ordering::Greater => {}
            }
        }

        candidates.pop().unwrap()
    }
}

/// Column scoring functions for decision tree optimization
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ColumnScoringFunction {
    /// The score of column i is the sum of the negation of the arities of
    /// constructors in sigma(i).
    Arity,
    /// The score is the negation of the cardinal of sigma(i), C(Sigma(i)).
    /// If sigma(i) is NOT complete, the resulting score is C(Sigma(i)) - 1.
    SmallBranching,
    /// The score is the number of needed rows of column i in the necessity
    /// matrix.
    NeededColumn,
    /// The score is the larger row index j such that column i is needed for all
    /// rows j′; 1 ≤ j′ ≤ j.
    NeededPrefix,
}

impl ColumnScoringFunction {
    fn score<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        matrix: &DecisionMatrix<'db>,
        col: usize,
    ) -> i32 {
        match self {
            ColumnScoringFunction::Arity => matrix
                .column_signature(db, col)
                .observed
                .iter()
                .map(|c| -(c.arity(db) as i32))
                .sum(),

            ColumnScoringFunction::SmallBranching => {
                let signature = matrix.column_signature(db, col);
                let score = -(signature.observed.len() as i32);
                if matches!(
                    signature.space,
                    ConstructorSpace::Empty | ConstructorSpace::Complete { .. }
                ) {
                    score
                } else {
                    score - 1
                }
            }

            ColumnScoringFunction::NeededColumn => {
                matrix.necessity_matrix(db).compute_needed_column_score(col)
            }

            ColumnScoringFunction::NeededPrefix => {
                matrix.necessity_matrix(db).compute_needed_prefix_score(col)
            }
        }
    }
}

/// Build a decision tree from validated pattern roots with a specific column selection policy.
pub fn build_decision_tree_with_policy<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
    policy: ColumnSelectionPolicy,
) -> DecisionTree<'db> {
    let matrix = DecisionMatrix::new(db, store, roots);
    DecisionTreeBuilder::new(policy).build(db, matrix)
}

/// Build a decision tree from validated pattern roots with optimized column selection.
pub fn build_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
) -> DecisionTree<'db> {
    let policy = {
        let mut policy = ColumnSelectionPolicy::default();
        // PBA heuristics described in the paper.
        policy.needed_prefix().small_branching().arity();
        policy
    };
    build_decision_tree_with_policy(db, store, roots, policy)
}

pub(crate) fn build_pattern_branch_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    root: ValidatedPatId,
) -> DecisionTree<'db> {
    let matrix = DecisionMatrix::for_pattern_branch(db, store, root);

    let mut policy = ColumnSelectionPolicy::default();
    policy.needed_prefix().small_branching().arity();
    DecisionTreeBuilder::new(policy).build(db, matrix)
}

/// Decision tree builder with configurable policy
struct DecisionTreeBuilder {
    policy: ColumnSelectionPolicy,
}

impl DecisionTreeBuilder {
    fn new(policy: ColumnSelectionPolicy) -> Self {
        DecisionTreeBuilder { policy }
    }

    fn build<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        mut matrix: DecisionMatrix<'db>,
    ) -> DecisionTree<'db> {
        if matrix.nrows() == 0 {
            return DecisionTree::Unreachable;
        }

        if matrix.is_first_arm_satisfied() {
            return DecisionTree::Leaf(LeafNode::new(matrix.first_arm().unwrap().clone()));
        }

        let col = self.policy.select_column(db, &matrix);
        matrix.swap(col);

        let mut switch_arms = vec![];
        let occurrence = matrix.occurrences[0].clone();
        let signature = matrix.column_signature(db, 0);
        let needs_default = match signature.space {
            ConstructorSpace::Empty => return DecisionTree::Unreachable,
            ConstructorSpace::Complete { .. } => false,
            ConstructorSpace::Incomplete { .. } => true,
        };
        for ctor in signature.observed {
            let destructured_mat = matrix.phi_specialize(db, ctor);
            let subtree = self.build(db, destructured_mat);
            switch_arms.push((Case::Constructor(ctor), subtree));
        }

        if needs_default {
            let destructured_mat = matrix.d_specialize();
            let subtree = self.build(db, destructured_mat);
            switch_arms.push((Case::Default, subtree));
        }

        DecisionTree::Switch(SwitchNode {
            occurrence,
            arms: switch_arms,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ArmData<'db> {
    arm_index: usize,
    bindings: IndexMap<BindingRef<'db>, ProjectionPath<'db>>,
}

/// matchcov's typed matrix paired with the runtime projection of each column.
#[derive(Clone, Debug)]
struct DecisionMatrix<'db> {
    patterns: PatternMatrix<TyId<'db>, ConstructorKind<'db>, ArmData<'db>>,
    occurrences: Vec<ProjectionPath<'db>>,
}

impl<'db> DecisionMatrix<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        store: &PatternStore<'db>,
        roots: &[ValidatedPatId],
    ) -> Self {
        let column_types = roots
            .first()
            .map(|root| store.node(*root).match_ty().raw())
            .into_iter();
        let rows = roots.iter().copied().enumerate().map(|(arm_index, root)| {
            let mut bindings = IndexMap::new();
            collect_pattern_bindings(db, store, root, &ProjectionPath::default(), &mut bindings);
            PatternRow::new(
                [simplify_pattern(to_matchcov_pattern(store, root))],
                ArmData {
                    arm_index,
                    bindings,
                },
            )
        });
        let patterns = PatternMatrix::new(column_types, rows)
            .expect("validated pattern roots should form a well-typed matrix");
        let occurrences = vec![ProjectionPath::default(); patterns.width()];

        Self {
            patterns,
            occurrences,
        }
    }

    fn for_pattern_branch(
        db: &'db dyn HirAnalysisDb,
        store: &PatternStore<'db>,
        root: ValidatedPatId,
    ) -> Self {
        let mut bindings = IndexMap::new();
        collect_pattern_bindings(db, store, root, &ProjectionPath::default(), &mut bindings);
        let rows = [
            PatternRow::new(
                [simplify_pattern(to_matchcov_pattern(store, root))],
                ArmData {
                    arm_index: 0,
                    bindings,
                },
            ),
            PatternRow::new(
                [Pattern::Wildcard],
                ArmData {
                    arm_index: 1,
                    bindings: IndexMap::new(),
                },
            ),
        ];
        let patterns = PatternMatrix::new([store.node(root).match_ty().raw()], rows)
            .expect("validated branch pattern should form a well-typed matrix");

        Self {
            occurrences: vec![ProjectionPath::default(); patterns.width()],
            patterns,
        }
    }

    fn nrows(&self) -> usize {
        self.patterns.len()
    }

    fn ncols(&self) -> usize {
        self.patterns.width()
    }

    fn pat(&self, row: usize, col: usize) -> &Pattern<ConstructorKind<'db>> {
        &self.patterns.rows()[row].patterns()[col]
    }

    fn first_arm(&self) -> Option<&ArmData<'db>> {
        self.patterns.rows().first().map(PatternRow::payload)
    }

    fn column_signature(
        &self,
        db: &'db dyn HirAnalysisDb,
        col: usize,
    ) -> matchcov::ColumnSignature<ConstructorKind<'db>> {
        self.patterns
            .column_signature(&mut FeConstructorOracle::new(db), col)
            .expect("decision matrix column should be valid")
    }

    fn is_first_arm_satisfied(&self) -> bool {
        self.patterns
            .rows()
            .first()
            .into_iter()
            .flat_map(|row| row.patterns())
            .all(|pat| matches!(pat, Pattern::Wildcard))
    }

    fn swap(&mut self, col: usize) {
        if col == 0 {
            return;
        }

        self.patterns
            .swap_columns(0, col)
            .expect("selected decision matrix column should be valid");
        self.occurrences.swap(0, col);
    }

    fn phi_specialize(&self, db: &'db dyn HirAnalysisDb, ctor: ConstructorKind<'db>) -> Self {
        let patterns = self
            .patterns
            .specialize(&mut FeConstructorOracle::new(db), 0, &ctor)
            .expect("validated pattern constructor should specialize consistently");
        let mut new_occurrences = self.occurrences[0].phi_specialize(db, ctor);
        new_occurrences.extend_from_slice(&self.occurrences.as_slice()[1..]);

        Self {
            patterns,
            occurrences: new_occurrences,
        }
    }

    fn d_specialize(&self) -> Self {
        let patterns = self
            .patterns
            .specialize_default(0)
            .expect("decision matrix should have a first column");
        Self {
            patterns,
            occurrences: self.occurrences[1..].to_vec(),
        }
    }

    fn necessity_matrix(&self, db: &'db dyn HirAnalysisDb) -> NecessityMatrix {
        NecessityMatrix::from_matrix(db, self)
    }
}

/// Necessity matrix for pattern analysis optimization
struct NecessityMatrix {
    data: Vec<bool>,
    ncol: usize,
    nrow: usize,
}

impl NecessityMatrix {
    fn new(ncol: usize, nrow: usize) -> Self {
        let data = vec![false; ncol * nrow];
        Self { data, ncol, nrow }
    }

    fn from_matrix<'db>(db: &'db dyn HirAnalysisDb, matrix: &DecisionMatrix<'db>) -> Self {
        let ncol = matrix.ncols();
        let nrow = matrix.nrows();
        let mut necessity_mat = Self::new(ncol, nrow);
        necessity_mat.compute(db, matrix);
        necessity_mat
    }

    fn compute<'db>(&mut self, db: &'db dyn HirAnalysisDb, matrix: &DecisionMatrix<'db>) {
        for row in 0..self.nrow {
            for col in 0..self.ncol {
                let pat = matrix.pat(row, col);
                let pos = self.pos(row, col);

                if !matches!(pat, Pattern::Wildcard) {
                    self.data[pos] = true;
                } else {
                    let reduced = matrix
                        .patterns
                        .without_column(col)
                        .expect("necessity column should be valid");
                    self.data[pos] =
                        match reduced.row_usefulness(&mut FeConstructorOracle::new(db), row) {
                            Ok(Usefulness::Useful(_)) => false,
                            Ok(Usefulness::Useless | Usefulness::Inconclusive(_)) | Err(_) => true,
                        };
                }
            }
        }
    }

    fn compute_needed_column_score(&self, col: usize) -> i32 {
        let mut num = 0;
        for i in 0..self.nrow {
            if self.data[self.pos(i, col)] {
                num += 1;
            }
        }
        num
    }

    fn compute_needed_prefix_score(&self, col: usize) -> i32 {
        let mut current_row = 0;
        for i in 0..self.nrow {
            if self.data[self.pos(i, col)] {
                current_row += 1;
            } else {
                return current_row;
            }
        }
        current_row
    }

    fn pos(&self, row: usize, col: usize) -> usize {
        self.ncol * row + col
    }
}

/// Removes alternatives made redundant by a preceding wildcard.
fn simplify_pattern<'db>(pat: Pattern<ConstructorKind<'db>>) -> Pattern<ConstructorKind<'db>> {
    match pat {
        Pattern::Wildcard => Pattern::Wildcard,
        Pattern::Constructor { head, arguments } => {
            Pattern::constructor(head, arguments.into_iter().map(simplify_pattern))
        }
        Pattern::Or(alternatives) => {
            let mut simplified = Vec::new();
            for alternative in alternatives {
                let alternative = simplify_pattern(alternative);
                if matches!(alternative, Pattern::Wildcard) {
                    simplified.push(alternative);
                    break;
                } else {
                    simplified.push(alternative);
                }
            }

            if simplified.len() == 1 {
                simplified.pop().unwrap()
            } else {
                Pattern::Or(simplified)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_necessity_matrix_api() {
        // Test necessity matrix creation and basic operations
        let matrix = NecessityMatrix::new(3, 2);

        // Test position calculation
        assert_eq!(matrix.pos(0, 0), 0);
        assert_eq!(matrix.pos(0, 1), 1);
        assert_eq!(matrix.pos(0, 2), 2);
        assert_eq!(matrix.pos(1, 0), 3);
        assert_eq!(matrix.pos(1, 1), 4);
        assert_eq!(matrix.pos(1, 2), 5);
    }
}
