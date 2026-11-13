//! Address-space refinement of call inputs: the space of the place each
//! effect argument and each `mut` or view argument names, which instantiates
//! the callee per space.
use cranelift_entity::EntityRef;
use salsa::SalsaValue;

use super::check::{Analysis, callee_input_access};
use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        CallSiteProviderRefinement, RefinedInput, SemOrigin, SemanticInstance,
        diagnostics::{
            SemanticDiagnostic, SemanticDiagnosticId, SemanticDiagnosticKind,
            SemanticDiagnosticSpan, SemanticNormalizationFailure,
        },
        instance::refined_callee,
        normalized::{
            NEffectArgValue, NExpr, NStatementKind, NormalizedBody,
            normalize_semantic_body_provisional,
        },
        provisional_provider_idx_for_requirement,
    },
    ty::{
        provider::AddressSpace,
        ty_check::{BodyOwner, EffectParamSite},
    },
};

/// The address spaces a provisional body passes to its callees' inputs.
#[derive(Clone, Debug, PartialEq, Eq, SalsaValue)]
pub(crate) enum CallSiteRefinements<'db> {
    /// With the body's result space per access component, if it's a
    /// projection.
    Refined {
        refinements: Vec<CallSiteProviderRefinement>,
        spaces: Vec<Option<AddressSpace>>,
    },
    /// Upstream causes block the provisional body.
    Blocked,
    Rejected(SemanticDiagnosticId<'db>),
}

// A grant's space comes from its projection's refinement, so recursion through
// projections, which the check rejects, is a cycle.
#[salsa::tracked(cycle_initial = refinements_cycle_initial)]
pub(crate) fn provisional_call_site_provider_refinements<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> CallSiteRefinements<'db> {
    let body = match normalize_semantic_body_provisional(db, instance) {
        Ok(artifacts) => artifacts.body,
        Err(SemanticNormalizationFailure::Blocked(_)) => return CallSiteRefinements::Blocked,
        Err(
            SemanticNormalizationFailure::Rejected(diag)
            | SemanticNormalizationFailure::InternalFailure(diag),
        ) => return CallSiteRefinements::Rejected(SemanticDiagnosticId::new(db, diag)),
    };
    let mut analysis = Analysis::new(db, instance, &body, true);
    analysis.solve();
    let sessions: Vec<_> = body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| match &statement.kind {
            NStatementKind::Define {
                result,
                expr: NExpr::Call { call_site, .. },
            } if analysis.is_session(*result) => Some((*result, *call_site)),
            _ => None,
        })
        .collect();
    // A grant lies where its session's callee puts it, the callee as its own
    // call site's refinement instantiates it: refine until every session's
    // callee settles. Each round settles the sessions whose grants come from
    // settled ones, so the sessions bound the rounds.
    let mut round = 0;
    loop {
        let refinements = match input_refinements(db, instance, &body, &mut analysis) {
            Ok(refinements) => refinements,
            Err(diag) => return CallSiteRefinements::Rejected(diag),
        };
        let mut settled = true;
        for &(session, call_site) in &sessions {
            let site: Vec<_> = refinements
                .iter()
                .filter(|refinement| refinement.call_site == call_site)
                .cloned()
                .collect();
            let site = (!site.is_empty()).then_some(site.as_slice());
            if let Some(callee) = refined_callee(db, instance, call_site, site) {
                settled &= !analysis.set_session(session, callee);
            }
        }
        if settled || round == sessions.len() {
            return CallSiteRefinements::Refined {
                refinements,
                // The check reports what's wrong with a yield.
                spaces: analysis.check_yields().unwrap_or_default(),
            };
        }
        // Where a path from a grant leads depends on the grant's space.
        analysis.solve();
        round += 1;
    }
}

fn refinements_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: SemanticInstance<'db>,
) -> CallSiteRefinements<'db> {
    CallSiteRefinements::Refined {
        refinements: Vec::new(),
        spaces: Vec::new(),
    }
}

/// A projection's result space, per access component, as its refinement finds
/// it. A provisional analysis reads this rather than the checked body, which
/// may depend on the refinements being computed.
pub(super) fn provisional_result_spaces<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> &'db [Option<AddressSpace>] {
    match provisional_call_site_provider_refinements(db, instance) {
        CallSiteRefinements::Refined { spaces, .. } => spaces,
        CallSiteRefinements::Blocked | CallSiteRefinements::Rejected(_) => &[],
    }
}

/// The space of each call's inputs, as `analysis` places them.
fn input_refinements<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
    analysis: &mut Analysis<'_, 'db>,
) -> Result<Vec<CallSiteProviderRefinement>, SemanticDiagnosticId<'db>> {
    let mut refinements = Vec::new();
    for block in &body.blocks {
        for statement in &block.statements {
            let NStatementKind::Define {
                expr:
                    NExpr::Call {
                        call_site,
                        callee,
                        args,
                        effect_args,
                    },
                ..
            } = &statement.kind
            else {
                continue;
            };
            let BodyOwner::Func(func, mode) = callee.key.owner(db) else {
                continue;
            };
            let mut inputs = Vec::new();
            for (position, arg) in args.iter().enumerate() {
                if callee_input_access(db, callee.key, position).is_some() {
                    let regions = analysis.passed_regions(arg.value);
                    inputs.push((
                        RefinedInput::Param(position as u32),
                        regions,
                        None,
                        arg.origin.map_or(statement.origin, SemOrigin::Expr),
                    ));
                }
            }
            for arg in effect_args {
                let (regions, origin) = match &arg.arg {
                    NEffectArgValue::Place(place) => {
                        (analysis.resolve(place).regions, statement.origin)
                    }
                    NEffectArgValue::Value(value) => (
                        analysis.values[value.value.index()]
                            .iter()
                            .flat_map(|(token, _)| analysis.tokens[*token as usize].regions.clone())
                            .collect(),
                        value.origin.map_or(statement.origin, SemOrigin::Expr),
                    ),
                };
                let input = RefinedInput::Effect {
                    binding_idx: arg.binding_idx,
                    provider_idx: provisional_provider_idx_for_requirement(
                        db,
                        EffectParamSite::Func(func, mode),
                        arg.binding_idx,
                    ),
                };
                inputs.push((input, regions, arg.provider, origin));
            }
            for (input, regions, declared, origin) in inputs {
                let mut spaces = Vec::new();
                let mut symbolic = regions.is_empty();
                for region in &regions {
                    match analysis.grant_space(region) {
                        Some(space) if !spaces.contains(&space) => spaces.push(space),
                        Some(_) => {}
                        None => symbolic = true,
                    }
                }
                let address_space = match spaces.as_slice() {
                    [] => declared,
                    [space] if !symbolic => Some(*space),
                    [_] => None,
                    _ => {
                        spaces.sort();
                        let diag = SemanticDiagnostic::new(
                            instance,
                            SemanticDiagnosticKind::ProviderProvenanceConflict,
                            format!(
                                "this argument may name places in different address spaces: {}",
                                spaces
                                    .iter()
                                    .map(|space| space.pretty())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            ),
                            SemanticDiagnosticSpan::OriginWithTemplateFallback {
                                owner: instance.key(db).owner(db),
                                template_owner: body.template_owner,
                                origin,
                            },
                        );
                        return Err(SemanticDiagnosticId::new(db, diag));
                    }
                };
                // Data parameters name memory unless refined.
                if let Some(address_space) = address_space
                    && !(matches!(input, RefinedInput::Param(_))
                        && address_space == AddressSpace::Memory)
                {
                    refinements.push(CallSiteProviderRefinement {
                        call_site: *call_site,
                        input,
                        address_space,
                    });
                }
            }
        }
    }
    Ok(refinements)
}
