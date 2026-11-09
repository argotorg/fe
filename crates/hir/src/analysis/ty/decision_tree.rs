//! Decision tree generation for efficient pattern matching compilation
//! Based on "Compiling pattern matching to good decision trees" (Maranget, 2008)

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

use core::{cmp::Reverse, convert::Infallible};

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

/// Builds the decision tree of a `match` whose arms have the patterns `roots`.
pub fn build_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
) -> DecisionTree<'db> {
    let column_ty = roots.first().map(|root| store.node(*root).match_ty());
    let rows = roots
        .iter()
        .enumerate()
        .map(|(arm_index, &root)| pattern_row(store, root, arm_index));
    build(db, store, DecisionMatrix::new(column_ty, rows))
}

/// Builds the decision tree of a refutable test of `root`: it reaches arm 0
/// when the pattern matches, and arm 1 when it doesn't.
pub(crate) fn build_pattern_branch_decision_tree<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    root: ValidatedPatId,
) -> DecisionTree<'db> {
    let fallback = PatternRow::new(
        [Pattern::Wildcard],
        ArmData {
            arm_index: 1,
            root: None,
            choices: Vec::new(),
            sources: vec![None],
        },
    );
    let rows = [pattern_row(store, root, 0), fallback];
    build(
        db,
        store,
        DecisionMatrix::new(Some(store.node(root).match_ty()), rows),
    )
}

fn pattern_row<'db>(
    store: &PatternStore<'db>,
    root: ValidatedPatId,
    arm_index: usize,
) -> PatternRow<ConstructorKind<'db>, ArmData> {
    PatternRow::new(
        [simplify_pattern(to_matchcov_pattern(store, root))],
        ArmData {
            arm_index,
            root: Some(root),
            choices: Vec::new(),
            sources: vec![Some(root)],
        },
    )
}

fn build<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    mut matrix: DecisionMatrix<'db>,
) -> DecisionTree<'db> {
    let Some(first) = matrix.patterns.rows().first() else {
        return DecisionTree::Unreachable;
    };
    if first
        .patterns()
        .iter()
        .all(|pat| matches!(pat, Pattern::Wildcard))
    {
        let arm = first.payload();
        return DecisionTree::Leaf(LeafNode {
            arm_index: arm.arm_index,
            bindings: arm.bindings(db, store),
        });
    }

    matrix.swap(select_column(db, &matrix));
    matrix.expand_alternatives(store);
    let signature = matrix.column_signature(db, 0);
    let needs_default = match signature.space {
        ConstructorSpace::Empty => return DecisionTree::Unreachable,
        ConstructorSpace::Complete { .. } => false,
        ConstructorSpace::Incomplete { .. } => true,
    };
    let mut arms: Vec<_> = signature
        .observed
        .into_iter()
        .map(|ctor| {
            let subtree = build(db, store, matrix.phi_specialize(db, store, ctor));
            (Case::Constructor(ctor), subtree)
        })
        .collect();
    if needs_default {
        arms.push((Case::Default, build(db, store, matrix.d_specialize())));
    }

    DecisionTree::Switch(SwitchNode {
        occurrence: matrix.occurrences[0].clone(),
        arms,
    })
}

/// Selects the column to switch on with the paper's `pba` heuristics: the
/// longest prefix of rows that need the column, then the fewest branches,
/// then the smallest constructor arities. Ties go to the column with the
/// shortest occurrence, then to the rightmost one (the paper's `R`).
fn select_column<'db>(db: &'db dyn HirAnalysisDb, matrix: &DecisionMatrix<'db>) -> usize {
    (0..matrix.patterns.width())
        .max_by_key(|&col| {
            let signature = matrix.column_signature(db, col);
            let incomplete = !matches!(
                signature.space,
                ConstructorSpace::Empty | ConstructorSpace::Complete { .. }
            );
            let branches = signature.observed.len() + incomplete as usize;
            let arities: usize = signature.observed.iter().map(|c| c.arity(db)).sum();
            (
                needed_prefix(db, matrix, col),
                Reverse(branches),
                Reverse(arities),
                Reverse(matrix.occurrences[col].len()),
                col,
            )
        })
        .unwrap()
}

/// The number of leading rows that need column `col`: a row needs a column it
/// doesn't match with a wildcard, or without which it would be useless.
fn needed_prefix<'db>(
    db: &'db dyn HirAnalysisDb,
    matrix: &DecisionMatrix<'db>,
    col: usize,
) -> usize {
    let without_col = matrix
        .patterns
        .without_column(col)
        .expect("necessity column should be valid");
    matrix
        .patterns
        .rows()
        .iter()
        .enumerate()
        .take_while(|(row, patterns)| {
            !matches!(patterns.patterns()[col], Pattern::Wildcard)
                || !matches!(
                    without_col.row_usefulness(&mut FeConstructorOracle::new(db), *row),
                    Ok(Usefulness::Useful(_))
                )
        })
        .count()
}

/// The occurrences of `ctor`'s fields in a value at `path`.
fn field_occurrences<'db>(
    db: &'db dyn HirAnalysisDb,
    path: &ProjectionPath<'db>,
    ctor: ConstructorKind<'db>,
) -> Vec<ProjectionPath<'db>> {
    (0..ctor.arity(db))
        .map(|field_idx| {
            let mut path = path.clone();
            path.push(match ctor {
                ConstructorKind::Variant(variant, enum_ty) => Projection::VariantField {
                    variant,
                    enum_ty,
                    field_idx,
                },
                ConstructorKind::Type(_) | ConstructorKind::Literal(..) => {
                    Projection::Field(field_idx)
                }
            });
            path
        })
        .collect()
}

/// The occurrence of each binding in `pat`, matched at `path`, in every
/// alternative of its `|` patterns.
pub(crate) fn binding_occurrences<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    pat: ValidatedPatId,
    path: &ProjectionPath<'db>,
    out: &mut Vec<(BindingRef<'db>, ProjectionPath<'db>)>,
) {
    match store.node(pat).kind() {
        ValidatedPatKind::Wildcard { binding } => {
            out.extend(binding.map(|binding| (binding, path.clone())));
        }
        ValidatedPatKind::Constructor { ctor, fields } => {
            for (&field, field_path) in fields.iter().zip(field_occurrences(db, path, *ctor)) {
                binding_occurrences(db, store, field, &field_path, out);
            }
        }
        ValidatedPatKind::Or(alternatives) => {
            for &alternative in alternatives {
                binding_occurrences(db, store, alternative, path, out);
            }
        }
    }
}

/// A row's arm, and what tells what its pattern binds where the row
/// matches.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ArmData {
    arm_index: usize,
    /// The arm's pattern; none for a test's fallback row.
    root: Option<ValidatedPatId>,
    /// The alternative the row takes of each `|` pattern that binds names,
    /// once a switch tests it.
    choices: Vec<(ValidatedPatId, ValidatedPatId)>,
    /// The checked pattern of each column, where the row has one.
    sources: Vec<Option<ValidatedPatId>>,
}

impl ArmData {
    /// What the row's pattern binds where it matches, in the pattern's
    /// order. A `|` pattern binds as the alternative the row takes, or, if
    /// no switch tested it, as its first, which matches anything.
    fn bindings<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        store: &PatternStore<'db>,
    ) -> IndexMap<BindingRef<'db>, ProjectionPath<'db>> {
        let mut bindings = IndexMap::new();
        if let Some(root) = self.root {
            self.collect_bindings(db, store, root, &ProjectionPath::default(), &mut bindings);
        }
        bindings
    }

    fn collect_bindings<'db>(
        &self,
        db: &'db dyn HirAnalysisDb,
        store: &PatternStore<'db>,
        pat: ValidatedPatId,
        path: &ProjectionPath<'db>,
        bindings: &mut IndexMap<BindingRef<'db>, ProjectionPath<'db>>,
    ) {
        match store.node(pat).kind() {
            ValidatedPatKind::Wildcard { binding } => {
                bindings.extend(binding.map(|binding| (binding, path.clone())));
            }
            ValidatedPatKind::Constructor { ctor, fields } => {
                for (&field, field_path) in fields.iter().zip(field_occurrences(db, path, *ctor)) {
                    self.collect_bindings(db, store, field, &field_path, bindings);
                }
            }
            ValidatedPatKind::Or(alternatives) => {
                let alternative = self
                    .choices
                    .iter()
                    .find_map(|&(or, alternative)| (or == pat).then_some(alternative))
                    .unwrap_or(alternatives[0]);
                self.collect_bindings(db, store, alternative, path, bindings);
            }
        }
    }
}

/// The alternatives of the `|` pattern `or`, those of a nested `|` in its
/// place, up to the first that matches anything.
fn alternatives<'db>(store: &PatternStore<'db>, or: ValidatedPatId) -> Vec<ValidatedPatId> {
    fn flatten<'db>(store: &PatternStore<'db>, pat: ValidatedPatId, out: &mut Vec<ValidatedPatId>) {
        match store.node(pat).kind() {
            ValidatedPatKind::Or(alternatives) => {
                for &alternative in alternatives {
                    flatten(store, alternative, out);
                }
            }
            _ => out.push(pat),
        }
    }
    let mut out = Vec::new();
    flatten(store, or, &mut out);
    if let Some(wildcard) = out.iter().position(|&alternative| {
        matches!(
            simplify_pattern(to_matchcov_pattern(store, alternative)),
            Pattern::Wildcard
        )
    }) {
        out.truncate(wildcard + 1);
    }
    out
}

/// matchcov's typed matrix paired with the runtime projection of each column.
#[derive(Clone, Debug)]
struct DecisionMatrix<'db> {
    patterns: PatternMatrix<TyId<'db>, ConstructorKind<'db>, ArmData>,
    occurrences: Vec<ProjectionPath<'db>>,
}

impl<'db> DecisionMatrix<'db> {
    fn new(
        column_tys: impl IntoIterator<Item = TyId<'db>>,
        rows: impl IntoIterator<Item = PatternRow<ConstructorKind<'db>, ArmData>>,
    ) -> Self {
        let patterns = PatternMatrix::new(column_tys, rows)
            .expect("validated pattern roots should form a well-typed matrix");
        Self {
            occurrences: vec![ProjectionPath::default(); patterns.width()],
            patterns,
        }
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

    fn swap(&mut self, col: usize) {
        if col == 0 {
            return;
        }

        let (mut tys, mut rows) = self.patterns.clone().into_parts();
        tys.swap(0, col);
        for row in &mut rows {
            row.patterns_mut().swap(0, col);
            row.payload_mut().sources.swap(0, col);
        }
        self.patterns = Self::matrix(tys, rows);
        self.occurrences.swap(0, col);
    }

    /// Gives each row whose first column is a `|` pattern that binds names
    /// a row for each alternative, in order, which takes that alternative,
    /// as the switch on the column would. matchcov expands the others,
    /// which bind nothing.
    fn expand_alternatives(&mut self, store: &PatternStore<'db>) {
        let binding_or = |row: &PatternRow<ConstructorKind<'db>, ArmData>| {
            row.payload().sources[0].filter(|&source| {
                matches!(store.node(source).kind(), ValidatedPatKind::Or(_))
                    && store.binds(source)
                    && !matches!(row.patterns()[0], Pattern::Wildcard)
            })
        };
        if !self
            .patterns
            .rows()
            .iter()
            .any(|row| binding_or(row).is_some())
        {
            return;
        }
        let (tys, rows) = self.patterns.clone().into_parts();
        let rows = rows
            .into_iter()
            .flat_map(|row| {
                let Some(or) = binding_or(&row) else {
                    return vec![row];
                };
                let (patterns, data) = row.into_parts();
                alternatives(store, or)
                    .into_iter()
                    .map(|alternative| {
                        let mut patterns = patterns.clone();
                        patterns[0] = simplify_pattern(to_matchcov_pattern(store, alternative));
                        let mut data = data.clone();
                        data.sources[0] = Some(alternative);
                        data.choices.push((or, alternative));
                        PatternRow::new(patterns, data)
                    })
                    .collect()
            })
            .collect::<Vec<_>>();
        self.patterns = Self::matrix(tys, rows);
    }

    fn phi_specialize(
        &self,
        db: &'db dyn HirAnalysisDb,
        store: &PatternStore<'db>,
        ctor: ConstructorKind<'db>,
    ) -> Self {
        // A row that matches `ctor` with its own constructor pattern has
        // that pattern's fields as its new columns.
        let (tys, mut rows) = self.patterns.clone().into_parts();
        for row in &mut rows {
            let sources = &mut row.payload_mut().sources;
            let fields = match sources[0].map(|source| store.node(source).kind()) {
                Some(ValidatedPatKind::Constructor { ctor: head, fields }) if *head == ctor => {
                    fields.iter().map(|&field| Some(field)).collect()
                }
                _ => vec![None; ctor.arity(db)],
            };
            sources.splice(0..1, fields);
        }
        let patterns = Self::matrix(tys, rows)
            .specialize(&mut FeConstructorOracle::new(db), 0, &ctor)
            .expect("validated pattern constructor should specialize consistently");
        let mut occurrences = field_occurrences(db, &self.occurrences[0], ctor);
        occurrences.extend_from_slice(&self.occurrences[1..]);
        Self {
            patterns,
            occurrences,
        }
    }

    fn d_specialize(&self) -> Self {
        let (tys, mut rows) = self.patterns.clone().into_parts();
        for row in &mut rows {
            row.payload_mut().sources.remove(0);
        }
        let patterns = Self::matrix(tys, rows)
            .specialize_default(0)
            .expect("decision matrix should have a first column");
        Self {
            patterns,
            occurrences: self.occurrences[1..].to_vec(),
        }
    }

    fn matrix(
        tys: Vec<TyId<'db>>,
        rows: Vec<PatternRow<ConstructorKind<'db>, ArmData>>,
    ) -> PatternMatrix<TyId<'db>, ConstructorKind<'db>, ArmData> {
        PatternMatrix::new(tys, rows).expect("decision matrix rows should fit its columns")
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
                let is_wildcard = matches!(alternative, Pattern::Wildcard);
                simplified.push(alternative);
                if is_wildcard {
                    break;
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
