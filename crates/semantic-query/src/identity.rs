use hir::hir_def::{scope_graph::ScopeId, TopLevelMod};
use hir::source_index::OccurrencePayload;

use hir_analysis::diagnostics::SpannedHirAnalysisDb;
use hir_analysis::ty::{func_def::FuncDef, ty_check::BindingKey};

/// Analysis-side identity for a single occurrence. Mirrors `SymbolKey` mapping
/// without pulling semantic-query’s public type into analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OccTarget<'db> {
    Scope(ScopeId<'db>),
    EnumVariant(hir::hir_def::EnumVariant<'db>),
    FuncParam(hir::hir_def::ItemKind<'db>, u16),
    Method(FuncDef<'db>),
    Local(hir::hir_def::item::Func<'db>, BindingKey<'db>),
}

/// Returns all possible symbol targets for an occurrence, including ambiguous cases.
pub(crate) fn occurrence_symbol_targets<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    occ: &OccurrencePayload<'db>,
) -> Vec<OccTarget<'db>> {
    // Use hir-analysis as the single source of truth for occurrence interpretation
    let identities = hir_analysis::lookup::identity_for_occurrence(db, top_mod, occ);
    identities.into_iter().map(|identity| match identity {
        hir_analysis::lookup::SymbolIdentity::Scope(sc) => OccTarget::Scope(sc),
        hir_analysis::lookup::SymbolIdentity::EnumVariant(v) => OccTarget::EnumVariant(v),
        hir_analysis::lookup::SymbolIdentity::FuncParam(item, idx) => OccTarget::FuncParam(item, idx),
        hir_analysis::lookup::SymbolIdentity::Method(fd) => OccTarget::Method(fd),
        hir_analysis::lookup::SymbolIdentity::Local(func, bkey) => OccTarget::Local(func, bkey),
    }).collect()
}

/// Returns the first symbol target for an occurrence (backward compatibility).
pub(crate) fn occurrence_symbol_target<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    occ: &OccurrencePayload<'db>,
) -> Option<OccTarget<'db>> {
    occurrence_symbol_targets(db, top_mod, occ).into_iter().next()
}

