use hir_analysis::diagnostics::SpannedHirAnalysisDb;

use hir::span::DynLazySpan;
use hir::source_index::OccurrencePayload;

pub fn goto_candidates_for_occurrence<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    occ: &OccurrencePayload<'db>,
    top_mod: hir::hir_def::TopLevelMod<'db>,
) -> Vec<DynLazySpan<'db>> {
    // Use the canonical occurrence interpreter to get the symbol target
    let target = crate::identity::occurrence_symbol_target(db, top_mod, occ);
    
    match target {
        Some(target) => {
            // Convert to SymbolKey and get definition span
            if let Some(symbol_key) = crate::occ_target_to_symbol_key(target) {
                if let Some((_tm, def_span)) = crate::def_span_for_symbol(db, symbol_key) {
                    vec![def_span]
                } else {
                    Vec::new()
                }
            } else {
                Vec::new()
            }
        }
        None => Vec::new(),
    }
}

