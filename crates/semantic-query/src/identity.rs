use hir::{hir_def::TopLevelMod, source_index::OccurrencePayload};

use hir_analysis::{diagnostics::SpannedHirAnalysisDb, lookup::SymbolKey};

/// Returns all possible symbol targets for an occurrence, including ambiguous cases.
/// Now directly returns SymbolIdentity from hir-analysis without translation.
pub(crate) fn occurrence_symbol_targets<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    occ: &OccurrencePayload<'db>,
) -> Vec<SymbolKey<'db>> {
    // Use hir-analysis as the single source of truth for occurrence interpretation
    hir_analysis::lookup::identity_for_occurrence(db, top_mod, occ)
}

/// Returns the first symbol target for an occurrence (backward compatibility).
pub(crate) fn occurrence_symbol_target<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    occ: &OccurrencePayload<'db>,
) -> Option<SymbolKey<'db>> {
    occurrence_symbol_targets(db, top_mod, occ)
        .into_iter()
        .next()
}
