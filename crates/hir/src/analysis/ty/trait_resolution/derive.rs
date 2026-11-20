use crate::{
    analysis::{HirAnalysisDb, name_resolution::Tier},
    hir_def::{Trait, scope_graph::ScopeId},
};

use super::{
    super::{
        canonical::Canonical,
        trait_def::{TraitInstId, impls_for_trait_in_ingots},
        ty_def::TyId,
        unify::UnificationTable,
    },
    PredicateListId, TraitSolveCx,
    table_solver::match_header_args,
};

/// A defaulted goal can fit distinct ordinary provider keys. Pick from the
/// declared headers before proving prerequisites. The chosen call retains
/// its own ordinary proof.
pub(crate) fn select_family<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    derive: Trait<'db>,
    provider: TyId<'db>,
    goal: TraitInstId<'db>,
) -> Result<TyId<'db>, String> {
    let families = crate::core::meta::derive_families(db, goal, scope, assumptions);
    if let [family] = families.as_slice() {
        return Ok(*family);
    }

    let inventory = TraitInstId::new_simple(db, derive, vec![provider, TyId::unit(db)]);
    let (primary, secondary) =
        TraitSolveCx::new(db, scope, Tier::All).search_ingots_for_trait_inst(db, inventory);
    let headers = impls_for_trait_in_ingots(
        db,
        primary,
        secondary,
        Canonical::new(db, inventory),
        Tier::All,
        scope.ingot(db),
    );
    let mut matching = Vec::new();
    for &family in &families {
        let requested = TraitInstId::new_simple(db, derive, vec![provider, family]);
        if headers.iter().any(|&header| {
            let mut table = UnificationTable::new(db);
            let candidate = table.instantiate_with_fresh_vars(header);
            match_header_args(
                db,
                &mut table,
                candidate.trait_inst(db),
                requested,
                scope,
                [assumptions; 2],
            )
            .is_ok()
        }) {
            matching.push(family);
        }
    }
    choose_family(db, provider, &families, matching)
}

pub(super) fn choose_family<'db>(
    db: &'db dyn HirAnalysisDb,
    provider: TyId<'db>,
    families: &[TyId<'db>],
    matching: Vec<TyId<'db>>,
) -> Result<TyId<'db>, String> {
    if matching.len() == 1 {
        return Ok(matching[0]);
    }
    let provider = provider.pretty_print(db);
    if matching.is_empty() {
        let family = families[0].pretty_print(db);
        return Err(format!(
            "`{provider}` has no `impl Derive<{family}>` that applies"
        ));
    }
    let families = matching
        .into_iter()
        .map(|family| format!("`Derive<{}>`", family.pretty_print(db)))
        .collect::<Vec<_>>()
        .join(", ");
    Err(format!(
        "`{provider}` has multiple derive families for the same trait application: {families}"
    ))
}
