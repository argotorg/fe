//! This module contains the decision tree definition and its construction
//! function.
//! The algorithm for efficient decision tree construction is mainly based on [Compiling pattern matching to good decision trees](https://dl.acm.org/doi/10.1145/1411304.1411311).
use std::io;

use fe_analyzer::{
    pattern_analysis::{
        ConstructorKind, PatternMatrix, PatternRowVec, SigmaSet, SimplifiedPattern,
        SimplifiedPatternKind,
    },
    AnalyzerDb,
};
use indexmap::IndexMap;
use smol_str::SmolStr;

use super::tree_vis::TreeRenderer;

pub fn build_decision_tree(
    db: &dyn AnalyzerDb,
    pattern_matrix: &PatternMatrix,
    policy: ColumnSelectionPolicy,
) -> DecisionTree {
    let builder = DecisionTreeBuilder::new(policy);
    let simplified_arms = SimplifiedArmMatrix::new(pattern_matrix);

    builder.build(db, simplified_arms)
}

#[derive(Debug)]
pub enum DecisionTree {
    Leaf(LeafNode),
    Switch(SwitchNode),
}

impl DecisionTree {
    #[allow(unused)]
    pub fn dump_dot<W>(&self, db: &dyn AnalyzerDb, w: &mut W) -> io::Result<()>
    where
        W: io::Write,
    {
        let renderer = TreeRenderer::new(db, self);
        dot2::render(&renderer, w).map_err(|err| match err {
            dot2::Error::Io(err) => err,
            _ => panic!("invalid graphviz id"),
        })
    }
}

#[derive(Debug)]
pub struct LeafNode {
    pub arm_idx: usize,
    pub binds: IndexMap<(SmolStr, usize), Occurrence>,
}

impl LeafNode {
    fn new(arm: SimplifiedArm, occurrences: &[Occurrence]) -> Self {
        let arm_idx = arm.body;
        let binds = arm.finalize_binds(occurrences);
        Self { arm_idx, binds }
    }
}

#[derive(Debug)]
pub struct SwitchNode {
    pub occurrence: Occurrence,
    pub arms: Vec<(Case, DecisionTree)>,
}

#[derive(Debug, Clone, Copy)]
pub enum Case {
    Ctor(ConstructorKind),
    Default,
}

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
    #[allow(unused)]
    pub fn needed_column(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::NeededColumn)
    }

    /// The score is the larger row index j such that column i is needed for all
    /// rows j′; 1 ≤ j′ ≤ j.
    pub fn needed_prefix(&mut self) -> &mut Self {
        self.add_heuristic(ColumnScoringFunction::NeededPrefix)
    }

    fn select_column(&self, db: &dyn AnalyzerDb, mat: &SimplifiedArmMatrix) -> usize {
        let mut candidates: Vec<_> = (0..mat.ncols()).collect();

        for scoring_fn in &self.0 {
            let mut max_score = i32::MIN;
            for col in std::mem::take(&mut candidates) {
                let score = scoring_fn.score(db, mat, col);
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
            let occurrences = mat.occurrences[col].len();
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

    fn add_heuristic(&mut self, heuristic: ColumnScoringFunction) -> &mut Self {
        debug_assert!(!self.0.contains(&heuristic));
        self.0.push(heuristic);
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Occurrence(Vec<usize>);

impl Occurrence {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn iter(&self) -> impl Iterator<Item = &usize> {
        self.0.iter()
    }

    pub fn parent(&self) -> Option<Occurrence> {
        let mut inner = self.0.clone();
        inner.pop().map(|_| Occurrence(inner))
    }

    pub fn last_index(&self) -> Option<usize> {
        self.0.last().cloned()
    }

    fn phi_specialize(&self, db: &dyn AnalyzerDb, ctor: ConstructorKind) -> Vec<Self> {
        let arity = ctor.arity(db);
        (0..arity)
            .map(|i| {
                let mut inner = self.0.clone();
                inner.push(i);
                Self(inner)
            })
            .collect()
    }

    fn len(&self) -> usize {
        self.0.len()
    }
}

struct DecisionTreeBuilder {
    policy: ColumnSelectionPolicy,
}

impl DecisionTreeBuilder {
    fn new(policy: ColumnSelectionPolicy) -> Self {
        DecisionTreeBuilder { policy }
    }

    fn build(&self, db: &dyn AnalyzerDb, mut mat: SimplifiedArmMatrix) -> DecisionTree {
        debug_assert!(mat.nrows() > 0, "unexhausted pattern matrix");

        if mat.is_first_arm_satisfied() {
            mat.arms.truncate(1);
            return DecisionTree::Leaf(LeafNode::new(mat.arms.pop().unwrap(), &mat.occurrences));
        }

        let col = self.policy.select_column(db, &mat);
        mat.swap(col);

        let mut switch_arms = vec![];
        let occurrence = &mat.occurrences[0];
        let sigma_set = mat.sigma_set(0);
        for &ctor in sigma_set.iter() {
            let destructured_mat = mat.phi_specialize(db, ctor, occurrence);
            let subtree = self.build(db, destructured_mat);
            switch_arms.push((Case::Ctor(ctor), subtree));
        }

        if !sigma_set.is_complete(db) {
            let destructured_mat = mat.d_specialize(db, occurrence);
            let subtree = self.build(db, destructured_mat);
            switch_arms.push((Case::Default, subtree));
        }

        DecisionTree::Switch(SwitchNode {
            occurrence: occurrence.clone(),
            arms: switch_arms,
        })
    }
}

#[derive(Clone, Debug)]
struct SimplifiedArmMatrix {
    arms: Vec<SimplifiedArm>,
    occurrences: Vec<Occurrence>,
}

impl SimplifiedArmMatrix {
    fn new(mat: &PatternMatrix) -> Self {
        let cols = mat.ncols();
        let arms: Vec<_> = mat
            .rows()
            .iter()
            .enumerate()
            .map(|(body, pat)| SimplifiedArm::new(pat, body))
            .collect();
        let occurrences = vec![Occurrence::new(); cols];

        SimplifiedArmMatrix { arms, occurrences }
    }

    fn nrows(&self) -> usize {
        self.arms.len()
    }

    fn ncols(&self) -> usize {
        self.arms[0].pat_vec.len()
    }

    fn pat(&self, row: usize, col: usize) -> &SimplifiedPattern {
        self.arms[row].pat(col)
    }

    fn necessity_matrix(&self, db: &dyn AnalyzerDb) -> NecessityMatrix {
        NecessityMatrix::from_mat(db, self)
    }

    fn reduced_pat_mat(&self, col: usize) -> PatternMatrix {
        let mut rows = Vec::with_capacity(self.nrows());
        for arm in self.arms.iter() {
            let reduced_pat_vec = arm
                .pat_vec
                .pats()
                .iter()
                .enumerate()
                .filter(|(i, _)| (*i != col))
                .map(|(_, pat)| pat.clone())
                .collect();
            rows.push(PatternRowVec::new(reduced_pat_vec));
        }

        PatternMatrix::new(rows)
    }

    /// Returns the constructor set in the column i.
    fn sigma_set(&self, col: usize) -> SigmaSet {
        SigmaSet::from_rows(self.arms.iter().map(|arm| &arm.pat_vec), col)
    }

    fn is_first_arm_satisfied(&self) -> bool {
        self.arms[0]
            .pat_vec
            .pats()
            .iter()
            .all(SimplifiedPattern::is_wildcard)
    }

    fn phi_specialize(
        &self,
        db: &dyn AnalyzerDb,
        ctor: ConstructorKind,
        occurrence: &Occurrence,
    ) -> Self {
        let mut new_arms = Vec::new();
        for arm in &self.arms {
            new_arms.extend_from_slice(&arm.phi_specialize(db, ctor, occurrence));
        }

        let mut new_occurrences = self.occurrences[0].phi_specialize(db, ctor);
        new_occurrences.extend_from_slice(&self.occurrences.as_slice()[1..]);

        Self {
            arms: new_arms,
            occurrences: new_occurrences,
        }
    }

    fn d_specialize(&self, db: &dyn AnalyzerDb, occurrence: &Occurrence) -> Self {
        let mut new_arms = Vec::new();
        for arm in &self.arms {
            new_arms.extend_from_slice(&arm.d_specialize(db, occurrence));
        }

        Self {
            arms: new_arms,
            occurrences: self.occurrences.as_slice()[1..].to_vec(),
        }
    }

    fn swap(&mut self, i: usize) {
        for arm in &mut self.arms {
            arm.swap(0, i)
        }
        self.occurrences.swap(0, i);
    }
}

#[derive(Clone, Debug)]
struct SimplifiedArm {
    pat_vec: PatternRowVec,
    body: usize,
    binds: IndexMap<(SmolStr, usize), Occurrence>,
}

impl SimplifiedArm {
    fn new(pat: &PatternRowVec, body: usize) -> Self {
        let pat = PatternRowVec::new(pat.inner.iter().map(generalize_pattern).collect());
        Self {
            pat_vec: pat,
            body,
            binds: IndexMap::new(),
        }
    }

    fn len(&self) -> usize {
        self.pat_vec.len()
    }

    fn pat(&self, col: usize) -> &SimplifiedPattern {
        &self.pat_vec.inner[col]
    }

    fn phi_specialize(
        &self,
        db: &dyn AnalyzerDb,
        ctor: ConstructorKind,
        occurrence: &Occurrence,
    ) -> Vec<Self> {
        let body = self.body;
        let binds = self.new_binds(occurrence);

        self.pat_vec
            .phi_specialize(db, ctor)
            .into_iter()
            .map(|pat| SimplifiedArm {
                pat_vec: pat,
                body,
                binds: binds.clone(),
            })
            .collect()
    }

    fn d_specialize(&self, db: &dyn AnalyzerDb, occurrence: &Occurrence) -> Vec<Self> {
        let body = self.body;
        let binds = self.new_binds(occurrence);

        self.pat_vec
            .d_specialize(db)
            .into_iter()
            .map(|pat| SimplifiedArm {
                pat_vec: pat,
                body,
                binds: binds.clone(),
            })
            .collect()
    }

    fn new_binds(&self, occurrence: &Occurrence) -> IndexMap<(SmolStr, usize), Occurrence> {
        let mut binds = self.binds.clone();
        if let Some(SimplifiedPatternKind::WildCard(Some(bind))) =
            self.pat_vec.head().map(|pat| &pat.kind)
        {
            binds.entry(bind.clone()).or_insert(occurrence.clone());
        }
        binds
    }

    fn finalize_binds(self, occurrences: &[Occurrence]) -> IndexMap<(SmolStr, usize), Occurrence> {
        debug_assert!(self.len() == occurrences.len());

        let mut binds = self.binds;
        for (pat, occurrence) in self.pat_vec.pats().iter().zip(occurrences.iter()) {
            debug_assert!(pat.is_wildcard());

            if let SimplifiedPatternKind::WildCard(Some(bind)) = &pat.kind {
                binds.entry(bind.clone()).or_insert(occurrence.clone());
            }
        }

        binds
    }

    fn swap(&mut self, i: usize, j: usize) {
        self.pat_vec.swap(i, j);
    }
}

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

    fn from_mat(db: &dyn AnalyzerDb, mat: &SimplifiedArmMatrix) -> Self {
        let nrow = mat.nrows();
        let ncol = mat.ncols();
        let mut necessity_mat = Self::new(ncol, nrow);

        necessity_mat.compute(db, mat);
        necessity_mat
    }

    fn compute(&mut self, db: &dyn AnalyzerDb, mat: &SimplifiedArmMatrix) {
        for row in 0..self.nrow {
            for col in 0..self.ncol {
                let pat = mat.pat(row, col);
                let pos = self.pos(row, col);

                if !pat.is_wildcard() {
                    self.data[pos] = true;
                } else {
                    let reduced_pat_mat = mat.reduced_pat_mat(col);
                    self.data[pos] = !reduced_pat_mat.is_row_useful(db, row);
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

    NeededPrefix,
}

impl ColumnScoringFunction {
    fn score(&self, db: &dyn AnalyzerDb, mat: &SimplifiedArmMatrix, col: usize) -> i32 {
        match self {
            ColumnScoringFunction::Arity => mat
                .sigma_set(col)
                .iter()
                .map(|c| -(c.arity(db) as i32))
                .sum(),

            ColumnScoringFunction::SmallBranching => {
                let sigma_set = mat.sigma_set(col);
                let score = -(mat.sigma_set(col).len() as i32);
                if sigma_set.is_complete(db) {
                    score
                } else {
                    score - 1
                }
            }

            ColumnScoringFunction::NeededColumn => {
                mat.necessity_matrix(db).compute_needed_column_score(col)
            }

            ColumnScoringFunction::NeededPrefix => {
                mat.necessity_matrix(db).compute_needed_prefix_score(col)
            }
        }
    }
}

fn generalize_pattern(pat: &SimplifiedPattern) -> SimplifiedPattern {
    match &pat.kind {
        SimplifiedPatternKind::WildCard(_) => pat.clone(),

        SimplifiedPatternKind::Constructor { kind, fields } => {
            let fields = fields.iter().map(generalize_pattern).collect();
            let kind = SimplifiedPatternKind::Constructor {
                kind: *kind,
                fields,
            };
            SimplifiedPattern::new(kind, pat.ty)
        }

        SimplifiedPatternKind::Or(pats) => {
            let mut gen_pats = vec![];
            for pat in pats {
                let gen_pad = generalize_pattern(pat);
                if gen_pad.is_wildcard() {
                    gen_pats.push(gen_pad);
                    break;
                } else {
                    gen_pats.push(gen_pad);
                }
            }

            if gen_pats.len() == 1 {
                gen_pats.pop().unwrap()
            } else {
                SimplifiedPattern::new(SimplifiedPatternKind::Or(gen_pats), pat.ty)
            }
        }
    }
}
