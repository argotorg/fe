//! Pattern matching analysis for exhaustiveness and reachability checking
//! Based on "Warnings for pattern matching" by Luc Maranget

use std::convert::Infallible;

use matchcov::{
    Analysis, Arm, ConstructorFields, ConstructorOracle, ConstructorSpace, Exhaustiveness,
    InconclusiveReason, InputError, Pattern, Reachability, Witness,
};

use crate::analysis::HirAnalysisDb;
use crate::analysis::ty::AdtRef;
use crate::analysis::ty::pattern_ir::{
    ConstructorKind, PatternStore, ValidatedPatId, ValidatedPatKind,
};
use crate::analysis::ty::pattern_types::pattern_match_expected_ty;
use crate::analysis::ty::ty_def::TyId;
use crate::core::hir_def::LitKind;

pub(crate) struct FeConstructorOracle<'db> {
    db: &'db dyn HirAnalysisDb,
}

impl<'db> FeConstructorOracle<'db> {
    pub(crate) fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self { db }
    }
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

        Ok(match constructors {
            Some(constructors) => ConstructorSpace::from_finite(seen, constructors),
            None => ConstructorSpace::open(),
        })
    }

    fn constructor_fields(
        &mut self,
        _ty: &TyId<'db>,
        constructor: &ConstructorKind<'db>,
    ) -> Result<ConstructorFields<TyId<'db>>, Self::Error> {
        Ok(ConstructorFields::Fields(
            constructor
                .field_types(self.db)
                .into_iter()
                .map(|ty| pattern_match_expected_ty(self.db, ty))
                .collect(),
        ))
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
        .map(|root| Arm::new([to_matchcov_pattern(store, *root)]));
    matchcov::analyze(
        &mut FeConstructorOracle::new(db),
        &[ty],
        &arms.collect::<Vec<_>>(),
    )
}

pub(crate) fn to_matchcov_pattern<'db>(
    store: &PatternStore<'db>,
    pat: ValidatedPatId,
) -> Pattern<ConstructorKind<'db>> {
    match store.node(pat).kind() {
        ValidatedPatKind::Wildcard { .. } => Pattern::Wildcard,
        ValidatedPatKind::Constructor { ctor, fields } => Pattern::constructor(
            *ctor,
            fields
                .iter()
                .copied()
                .map(|field| to_matchcov_pattern(store, field)),
        ),
        ValidatedPatKind::Or(pats) => Pattern::or(
            pats.iter()
                .copied()
                .map(|pat| to_matchcov_pattern(store, pat)),
        ),
    }
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
