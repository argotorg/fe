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
        normalized::{NEffectArgValue, NExpr, NStatementKind, normalize_semantic_body_provisional},
        provisional_provider_idx_for_requirement,
    },
    ty::{
        provider::AddressSpace,
        ty_check::{BodyOwner, EffectParamSite, EffectPassMode},
    },
};

/// The address spaces a provisional body passes to its callees' inputs.
#[derive(Clone, Debug, PartialEq, Eq, SalsaValue)]
pub(crate) enum CallSiteRefinements<'db> {
    Refined(Vec<CallSiteProviderRefinement>),
    /// Upstream causes block the provisional body.
    Blocked,
    Rejected(SemanticDiagnosticId<'db>),
}

#[salsa::tracked]
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
    let mut analysis = Analysis::new(db, instance, &body, false);
    analysis.solve();
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
                if matches!(arg.pass_mode, EffectPassMode::Unknown) {
                    continue;
                }
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
                        return CallSiteRefinements::Rejected(SemanticDiagnosticId::new(db, diag));
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
    CallSiteRefinements::Refined(refinements)
}
