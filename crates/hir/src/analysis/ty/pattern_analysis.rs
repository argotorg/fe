//! Pattern matching analysis for exhaustiveness and reachability checking
//! Based on "Warnings for pattern matching" by Luc Maranget

use std::convert::Infallible;

use common::indexmap::IndexSet;
use matchcov::{
    Analysis, Arm, ConstructorFields, ConstructorOracle, ConstructorSpace, Exhaustiveness,
    InconclusiveReason, InputError, Pattern, Reachability, Usefulness, Witness,
};

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::AdtRef;
use crate::analysis::ty::pattern_ir::{
    BindingRef, ConstructorKind, PatternStore, ValidatedPatId, ValidatedPatKind, ctor_variant_num,
};
use crate::analysis::ty::ty_def::TyId;
use crate::core::hir_def::LitKind;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternMatrix<'db> {
    pub rows: Vec<PatternRowVec<'db>>,
}

impl<'db> PatternMatrix<'db> {
    pub(crate) fn new(rows: Vec<PatternRowVec<'db>>) -> Self {
        Self { rows }
    }

    pub fn from_roots(store: &PatternStore<'db>, roots: &[ValidatedPatId]) -> Self {
        let rows = roots
            .iter()
            .copied()
            .map(|root| PatternRowVec::new(vec![MatrixPat::from_root(store, root)]))
            .collect();
        Self { rows }
    }

    pub fn push_wildcard_row(&mut self, ty: TyId<'db>) {
        self.rows
            .push(PatternRowVec::new(vec![MatrixPat::wildcard(None, ty)]));
    }

    pub(crate) fn row_usefulness(
        &self,
        db: &'db dyn HirAnalysisDb,
        row: usize,
    ) -> Result<Usefulness<ConstructorKind<'db>, Infallible>, matchcov::InputError> {
        debug_assert!(self.nrows() > row);
        let column_types: Vec<_> = self.rows[row].inner.iter().map(|pat| pat.ty).collect();
        let previous: Vec<_> = self.rows[..row]
            .iter()
            .map(PatternRowVec::to_matchcov)
            .collect();
        matchcov::usefulness(
            &mut FeConstructorOracle { db },
            &column_types,
            &previous,
            &self.rows[row].to_matchcov(),
        )
    }

    pub fn is_row_useful(&self, db: &'db dyn HirAnalysisDb, row: usize) -> bool {
        !matches!(self.row_usefulness(db, row), Ok(Usefulness::Useless))
    }

    pub fn phi_specialize(&self, db: &'db dyn HirAnalysisDb, ctor: ConstructorKind<'db>) -> Self {
        let rows = self
            .rows
            .iter()
            .flat_map(|row| row.phi_specialize(db, ctor))
            .collect();
        Self::new(rows)
    }

    pub fn d_specialize(&self) -> Self {
        let rows = self
            .rows
            .iter()
            .flat_map(PatternRowVec::d_specialize)
            .collect();
        Self::new(rows)
    }

    pub fn sigma_set(&self) -> SigmaSet<'db> {
        SigmaSet::from_rows(self.rows.iter(), 0)
    }

    pub fn first_column_ty(&self) -> TyId<'db> {
        self.rows[0].first_column_ty()
    }

    pub fn nrows(&self) -> usize {
        self.rows.len()
    }

    pub fn ncols(&self) -> usize {
        if self.nrows() == 0 {
            0
        } else {
            let ncols = self.rows[0].len();
            debug_assert!(self.rows.iter().all(|row| row.len() == ncols));
            ncols
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PatternRowVec<'db> {
    pub(crate) inner: Vec<MatrixPat<'db>>,
}

impl<'db> PatternRowVec<'db> {
    pub(crate) fn new(inner: Vec<MatrixPat<'db>>) -> Self {
        Self { inner }
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    pub(crate) fn head(&self) -> Option<&MatrixPat<'db>> {
        self.inner.first()
    }

    pub fn phi_specialize(
        &self,
        db: &'db dyn HirAnalysisDb,
        ctor: ConstructorKind<'db>,
    ) -> Vec<Self> {
        debug_assert!(!self.inner.is_empty());

        let first_pat = &self.inner[0];
        let ctor_fields = ctor.field_types(db);

        match &first_pat.kind {
            MatrixPatKind::WildCard(bind) => {
                let mut inner = Vec::with_capacity(self.inner.len() + ctor_fields.len() - 1);
                for field_ty in ctor_fields {
                    inner.push(MatrixPat::wildcard(*bind, field_ty));
                }
                inner.extend_from_slice(&self.inner[1..]);
                vec![Self::new(inner)]
            }
            MatrixPatKind::Constructor { kind, fields } => {
                if *kind == ctor {
                    let mut inner = Vec::with_capacity(self.inner.len() + ctor_fields.len() - 1);
                    for (idx, field_ty) in ctor_fields.iter().copied().enumerate() {
                        if let Some(field) = fields.get(idx) {
                            inner.push(field.clone());
                        } else {
                            inner.push(MatrixPat::wildcard(None, field_ty));
                        }
                    }
                    inner.extend_from_slice(&self.inner[1..]);
                    vec![Self::new(inner)]
                } else {
                    Vec::new()
                }
            }
            MatrixPatKind::Or(pats) => {
                let mut result = Vec::new();
                for pat in pats {
                    let mut tmp_inner = Vec::with_capacity(self.inner.len());
                    tmp_inner.push(pat.clone());
                    tmp_inner.extend_from_slice(&self.inner[1..]);
                    result.extend(PatternRowVec::new(tmp_inner).phi_specialize(db, ctor));
                }
                result
            }
        }
    }

    pub fn d_specialize(&self) -> Vec<Self> {
        debug_assert!(!self.inner.is_empty());

        match &self.inner[0].kind {
            MatrixPatKind::WildCard(_) => vec![Self::new(self.inner[1..].to_vec())],
            MatrixPatKind::Constructor { .. } => Vec::new(),
            MatrixPatKind::Or(pats) => {
                let mut result = Vec::new();
                for pat in pats {
                    let mut tmp_inner = Vec::with_capacity(self.inner.len());
                    tmp_inner.push(pat.clone());
                    tmp_inner.extend_from_slice(&self.inner[1..]);
                    result.extend(PatternRowVec::new(tmp_inner).d_specialize());
                }
                result
            }
        }
    }

    fn to_matchcov(&self) -> Vec<Pattern<ConstructorKind<'db>>> {
        self.inner.iter().map(MatrixPat::to_matchcov).collect()
    }

    fn first_column_ty(&self) -> TyId<'db> {
        debug_assert!(!self.inner.is_empty());
        self.inner[0].ty
    }

    fn collect_column_ctors(&self, column: usize) -> Vec<ConstructorKind<'db>> {
        self.inner[column].kind.collect_ctors()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct MatrixPat<'db> {
    pub(crate) kind: MatrixPatKind<'db>,
    pub(crate) ty: TyId<'db>,
}

impl<'db> MatrixPat<'db> {
    pub(crate) fn new(kind: MatrixPatKind<'db>, ty: TyId<'db>) -> Self {
        Self { kind, ty }
    }

    pub(crate) fn wildcard(bind: Option<BindingRef<'db>>, ty: TyId<'db>) -> Self {
        Self::new(MatrixPatKind::WildCard(bind), ty)
    }

    pub(crate) fn is_wildcard(&self) -> bool {
        matches!(self.kind, MatrixPatKind::WildCard(_))
    }

    pub(crate) fn from_root(store: &PatternStore<'db>, root: ValidatedPatId) -> Self {
        let node = store.node(root);
        let kind = match node.kind() {
            ValidatedPatKind::Wildcard { binding } => MatrixPatKind::WildCard(*binding),
            ValidatedPatKind::Constructor { ctor, fields } => MatrixPatKind::Constructor {
                kind: *ctor,
                fields: fields
                    .iter()
                    .copied()
                    .map(|field| Self::from_root(store, field))
                    .collect(),
            },
            ValidatedPatKind::Or(pats) => MatrixPatKind::Or(
                pats.iter()
                    .copied()
                    .map(|pat| Self::from_root(store, pat))
                    .collect(),
            ),
        };
        Self::new(kind, node.match_ty().raw())
    }

    fn to_matchcov(&self) -> Pattern<ConstructorKind<'db>> {
        match &self.kind {
            MatrixPatKind::WildCard(_) => Pattern::Wildcard,
            MatrixPatKind::Constructor { kind, fields } => {
                Pattern::constructor(*kind, fields.iter().map(Self::to_matchcov))
            }
            MatrixPatKind::Or(pats) => Pattern::or(pats.iter().map(Self::to_matchcov)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum MatrixPatKind<'db> {
    WildCard(Option<BindingRef<'db>>),
    Constructor {
        kind: ConstructorKind<'db>,
        fields: Vec<MatrixPat<'db>>,
    },
    Or(Vec<MatrixPat<'db>>),
}

impl<'db> MatrixPatKind<'db> {
    fn collect_ctors(&self) -> Vec<ConstructorKind<'db>> {
        match self {
            Self::WildCard(_) => Vec::new(),
            Self::Constructor { kind, .. } => vec![*kind],
            Self::Or(pats) => {
                let mut ctors = Vec::new();
                for pat in pats {
                    ctors.extend_from_slice(&pat.kind.collect_ctors());
                }
                ctors
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SigmaSet<'db>(pub IndexSet<ConstructorKind<'db>>);

impl<'db> SigmaSet<'db> {
    pub fn from_rows<'a>(rows: impl Iterator<Item = &'a PatternRowVec<'db>>, column: usize) -> Self
    where
        'db: 'a,
    {
        let mut ctor_set = IndexSet::new();
        for row in rows {
            for ctor in row.collect_column_ctors(column) {
                ctor_set.insert(ctor);
            }
        }
        Self(ctor_set)
    }

    pub fn complete_sigma(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        let mut ctors = IndexSet::new();

        if ty.is_bool(db) {
            ctors.insert(ConstructorKind::Literal(LitKind::Bool(true), ty));
            ctors.insert(ConstructorKind::Literal(LitKind::Bool(false), ty));
        } else if ty.is_tuple(db) {
            ctors.insert(ConstructorKind::Type(ty));
        } else if let Some(adt_def) = ty.adt_def(db) {
            match adt_def.adt_ref(db) {
                AdtRef::Enum(enum_def) => {
                    for (idx, _) in enum_def.variants(db).enumerate() {
                        ctors.insert(ConstructorKind::Variant(
                            crate::core::hir_def::EnumVariant::new(enum_def, idx),
                            ty,
                        ));
                    }
                }
                AdtRef::Struct(_) => {
                    ctors.insert(ConstructorKind::Type(ty));
                }
            }
        }

        Self(ctors)
    }

    pub fn is_complete(&self, db: &'db dyn HirAnalysisDb) -> bool {
        match self.0.first() {
            Some(ctor) => {
                let expected = ctor_variant_num(db, ctor);
                debug_assert!(
                    self.0.len() <= expected,
                    "sigma set {self:?} has {} ctors, expected at most {expected}",
                    self.0.len(),
                );
                self.0.len() == expected
            }
            None => false,
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn difference<'a>(
        &'a self,
        other: &'a Self,
    ) -> impl Iterator<Item = &'a ConstructorKind<'db>> + 'a {
        self.0.difference(&other.0)
    }
}

impl<'db> IntoIterator for SigmaSet<'db> {
    type Item = ConstructorKind<'db>;
    type IntoIter = <IndexSet<ConstructorKind<'db>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

struct FeConstructorOracle<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> ConstructorOracle<TyId<'db>, ConstructorKind<'db>> for FeConstructorOracle<'db> {
    type Error = Infallible;

    fn constructor_space(
        &mut self,
        ty: &TyId<'db>,
        seen: &[ConstructorKind<'db>],
    ) -> Result<ConstructorSpace<ConstructorKind<'db>>, Self::Error> {
        let constructors = if ty.is_bool(self.db) {
            Some(vec![
                ConstructorKind::Literal(LitKind::Bool(false), *ty),
                ConstructorKind::Literal(LitKind::Bool(true), *ty),
            ])
        } else if ty.is_tuple(self.db) {
            Some(vec![ConstructorKind::Type(*ty)])
        } else if let Some(adt_def) = ty.adt_def(self.db) {
            match adt_def.adt_ref(self.db) {
                AdtRef::Enum(enum_def) => Some(
                    enum_def
                        .variants(self.db)
                        .enumerate()
                        .map(|(idx, _)| {
                            ConstructorKind::Variant(
                                crate::core::hir_def::EnumVariant::new(enum_def, idx),
                                *ty,
                            )
                        })
                        .collect(),
                ),
                AdtRef::Struct(_) => Some(vec![ConstructorKind::Type(*ty)]),
            }
        } else {
            seen.iter().find_map(|constructor| match constructor {
                ConstructorKind::Type(_) => Some(vec![*constructor]),
                ConstructorKind::Variant(..) | ConstructorKind::Literal(..) => None,
            })
        };

        // Fe's decision-tree lowering cannot yet represent an exhaustive match
        // with zero arms. Keep empty constructor families conservative so the
        // existing non-exhaustive diagnostic prevents such a match from reaching
        // lowering.
        Ok(match constructors {
            Some(constructors) if !constructors.is_empty() => {
                ConstructorSpace::from_finite(seen, constructors)
            }
            Some(_) | None => ConstructorSpace::open(),
        })
    }

    fn constructor_fields(
        &mut self,
        _ty: &TyId<'db>,
        constructor: &ConstructorKind<'db>,
    ) -> Result<ConstructorFields<TyId<'db>>, Self::Error> {
        Ok(ConstructorFields::Fields(constructor.field_types(self.db)))
    }
}

pub(crate) struct MatchAnalysis {
    pub(crate) unreachable_arms: Vec<usize>,
    pub(crate) exhaustiveness: MatchExhaustiveness,
}

pub(crate) enum MatchExhaustiveness {
    Exhaustive,
    NonExhaustive(Vec<String>),
    Inconclusive(String),
}

pub(crate) fn analyze_match<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
    ty: TyId<'db>,
) -> MatchAnalysis {
    let analysis = match analyze_patterns(db, store, roots, ty) {
        Ok(analysis) => analysis,
        Err(error) => {
            return MatchAnalysis {
                unreachable_arms: Vec::new(),
                exhaustiveness: MatchExhaustiveness::Inconclusive(format!(
                    "invalid pattern matrix: {error}"
                )),
            };
        }
    };

    let unreachable_arms = analysis
        .arms
        .iter()
        .enumerate()
        .filter_map(|(index, reachability)| {
            matches!(reachability, Reachability::Unreachable(_)).then_some(index)
        })
        .collect();
    let exhaustiveness = match analysis.exhaustiveness {
        Exhaustiveness::Exhaustive => MatchExhaustiveness::Exhaustive,
        Exhaustiveness::NonExhaustive(witness) => MatchExhaustiveness::NonExhaustive(
            witness
                .iter()
                .map(|pattern| display_missing_pattern(db, pattern))
                .collect(),
        ),
        Exhaustiveness::Inconclusive(reason) => {
            MatchExhaustiveness::Inconclusive(display_inconclusive_reason(reason))
        }
    };

    MatchAnalysis {
        unreachable_arms,
        exhaustiveness,
    }
}

pub fn check_exhaustiveness<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
    ty: TyId<'db>,
) -> Result<(), Vec<String>> {
    match analyze_patterns(db, store, roots, ty) {
        Ok(Analysis {
            exhaustiveness: Exhaustiveness::Exhaustive,
            ..
        }) => Ok(()),
        Ok(Analysis {
            exhaustiveness: Exhaustiveness::NonExhaustive(witness),
            ..
        }) => Err(witness
            .iter()
            .map(|pattern| display_missing_pattern(db, pattern))
            .collect()),
        Ok(Analysis {
            exhaustiveness: Exhaustiveness::Inconclusive(_),
            ..
        })
        | Err(_) => Err(Vec::new()),
    }
}

pub fn check_reachability<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
) -> Vec<bool> {
    let Some(root) = roots.first() else {
        return Vec::new();
    };
    let ty = store.node(*root).match_ty().raw();
    match analyze_patterns(db, store, roots, ty) {
        Ok(analysis) => analysis
            .arms
            .into_iter()
            .map(|reachability| !matches!(reachability, Reachability::Unreachable(_)))
            .collect(),
        Err(_) => vec![true; roots.len()],
    }
}

pub fn is_exhaustive<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
    ty: TyId<'db>,
) -> bool {
    analyze_patterns(db, store, roots, ty)
        .is_ok_and(|analysis| matches!(analysis.exhaustiveness, Exhaustiveness::Exhaustive))
}

fn analyze_patterns<'db>(
    db: &'db dyn HirAnalysisDb,
    store: &PatternStore<'db>,
    roots: &[ValidatedPatId],
    ty: TyId<'db>,
) -> Result<Analysis<ConstructorKind<'db>, Infallible>, InputError> {
    let arms = roots
        .iter()
        .map(|root| Arm::new([MatrixPat::from_root(store, *root).to_matchcov()]));
    matchcov::analyze(
        &mut FeConstructorOracle { db },
        &[ty],
        &arms.collect::<Vec<_>>(),
    )
}

fn display_inconclusive_reason(reason: InconclusiveReason<Infallible>) -> String {
    match reason {
        InconclusiveReason::Oracle(error) => match error {},
        InconclusiveReason::StepLimitExceeded { max_steps } => {
            format!("pattern analysis exceeded its {max_steps}-step limit")
        }
        InconclusiveReason::DepthLimitExceeded { max_depth } => {
            format!("pattern analysis exceeded its recursion-depth limit of {max_depth}")
        }
        InconclusiveReason::ArityMismatch { expected, actual } => format!(
            "pattern constructor metadata expected {expected} fields but the pattern has {actual}"
        ),
        InconclusiveReason::InconsistentOracle => {
            "pattern constructor metadata is inconsistent".to_string()
        }
    }
}

fn display_missing_pattern<'db>(
    db: &'db dyn HirAnalysisDb,
    pat: &Witness<ConstructorKind<'db>>,
) -> String {
    match pat {
        Witness::Wildcard | Witness::OtherThan { .. } => "_".to_string(),
        Witness::Constructor {
            head: kind,
            arguments: fields,
        } => match kind {
            ConstructorKind::Variant(variant, _) => {
                let variant_name = variant
                    .name(db)
                    .map(|name| name.to_string())
                    .unwrap_or_else(|| "UnknownVariant".to_string());
                let enum_name = match variant.enum_.name(db) {
                    crate::core::hir_def::Partial::Present(name) => name.data(db).to_string(),
                    crate::core::hir_def::Partial::Absent => "UnknownEnum".to_string(),
                };
                let full_name = format!("{enum_name}::{variant_name}");

                match variant.kind(db) {
                    crate::core::hir_def::VariantKind::Unit => full_name,
                    crate::core::hir_def::VariantKind::Tuple(_) => {
                        if fields.is_empty() {
                            format!("{full_name}(..)")
                        } else {
                            let field_patterns: Vec<String> = fields
                                .iter()
                                .map(|f| display_missing_pattern(db, f))
                                .collect();
                            format!("{}({})", full_name, field_patterns.join(", "))
                        }
                    }
                    crate::core::hir_def::VariantKind::Record(_) => format!("{full_name} {{ .. }}"),
                }
            }
            ConstructorKind::Type(ty) => {
                if ty.is_tuple(db) {
                    if fields.is_empty() {
                        "()".to_string()
                    } else {
                        let parts: Vec<String> = fields
                            .iter()
                            .map(|f| display_missing_pattern(db, f))
                            .collect();
                        format!("({})", parts.join(", "))
                    }
                } else {
                    format!("{} {{ .. }}", ty.pretty_print(db))
                }
            }
            ConstructorKind::Literal(lit, _) => match lit {
                LitKind::Bool(b) => b.to_string(),
                LitKind::Int(i) => i.data(db).to_string(),
                LitKind::String(s) => format!("\"{}\"", s.data(db)),
            },
        },
    }
}
