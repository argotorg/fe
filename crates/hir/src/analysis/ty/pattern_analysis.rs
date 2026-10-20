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
use crate::core::hir_def::{FieldParent, LitKind, VariantKind};
use itertools::Itertools;

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

/// Renders `pat`, a witness of the values no arm matches. A record shows the
/// fields that make it a witness; one with none shows as `{ .. }`.
fn display_missing_pattern<'db>(
    db: &'db dyn HirAnalysisDb,
    pat: &Witness<ConstructorKind<'db>>,
) -> String {
    let Witness::Constructor { head, arguments } = pat else {
        return "_".to_string();
    };
    let args = || arguments.iter().map(|arg| display_missing_pattern(db, arg));
    let tuple = |name: &str| {
        if arguments.is_empty() && !name.is_empty() {
            format!("{name}(..)")
        } else {
            format!("{name}({})", args().join(", "))
        }
    };
    let record = |name: &str| {
        let fields: Vec<_> = (head.field_names(db).into_iter().flatten())
            .zip(args())
            .filter(|(_, arg)| arg != "_")
            .filter_map(|(field, arg)| Some(format!("{}: {arg}", field?.data(db))))
            .collect();
        match (fields.is_empty(), fields.len() == arguments.len()) {
            (true, _) => format!("{name} {{ .. }}"),
            (false, true) => format!("{name} {{ {} }}", fields.join(", ")),
            (false, false) => format!("{name} {{ {}, .. }}", fields.join(", ")),
        }
    };
    match head {
        ConstructorKind::Variant(variant, _) => {
            let name = FieldParent::Variant(*variant)
                .name(db)
                .unwrap_or("UnknownEnum::UnknownVariant".into());
            match variant.kind(db) {
                VariantKind::Unit => name.into_owned(),
                VariantKind::Tuple(_) => tuple(&name),
                VariantKind::Record(_) => record(&name),
            }
        }
        ConstructorKind::Type(ty) if ty.is_tuple(db) => tuple(""),
        ConstructorKind::Type(ty) => {
            let name = match ty.adt_def(db).and_then(|adt| adt.adt_ref(db).name(db)) {
                Some(name) => name.data(db),
                None => ty.pretty_print(db),
            };
            if ty.as_tuple_struct(db).is_some() {
                tuple(name)
            } else {
                record(name)
            }
        }
        ConstructorKind::Literal(lit, _) => lit.pretty_print(db),
    }
}
