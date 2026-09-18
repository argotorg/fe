use crate::analysis::semantic::capability::{region::RegionSet, state::BorrowState};

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        CallSiteProviderRefinement, SemOrigin, SemanticInstance,
        normalized::{
            NEffectArg, NEffectArgValue, NExpr, NOperand, NStatement, NStatementKind,
            normalize_semantic_body_provisional,
        },
        provisional_provider_idx_for_requirement,
    },
    ty::{
        ProviderAddressSpace,
        ty_check::{BodyOwner, EffectParamSite, EffectPassMode},
    },
};

use super::{
    diagnostics::operand_origin,
    ir::{SemanticBorrowDiagnostic, SemanticNormalizationFailure},
    solver::Borrowck,
};

pub(crate) fn provisional_call_site_provider_refinements<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<Vec<CallSiteProviderRefinement>, SemanticNormalizationFailure<'db>> {
    let body = normalize_semantic_body_provisional(db, instance)?.body;
    let mut borrowck = Borrowck::new_with_body(
        db,
        instance,
        body,
        super::solver::BorrowSummaryMode::Provisional,
    )
    .map_err(SemanticNormalizationFailure::InternalFailure)?;
    borrowck
        .solve()
        .map_err(SemanticNormalizationFailure::InternalFailure)?;
    if let Some(blocked) = borrowck.blocked.clone() {
        return Err(SemanticNormalizationFailure::Blocked(blocked));
    }
    CallSiteProviderRefiner { borrowck }
        .refine()
        .map_err(SemanticNormalizationFailure::InternalFailure)
}

struct CallSiteProviderRefiner<'db> {
    borrowck: Borrowck<'db>,
}

impl<'db> CallSiteProviderRefiner<'db> {
    fn refine(&self) -> Result<Vec<CallSiteProviderRefinement>, SemanticBorrowDiagnostic<'db>> {
        let mut out = Vec::new();
        for (bb_idx, block) in self.borrowck.body.blocks.iter().enumerate() {
            for (statement, state) in block.statements.iter().zip(&self.borrowck.before[bb_idx]) {
                self.refine_statement(state, statement, &mut out)?;
            }
        }
        Ok(out)
    }

    fn refine_statement(
        &self,
        state: &BorrowState<'db>,
        statement: &NStatement<'db>,
        out: &mut Vec<CallSiteProviderRefinement>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let NStatementKind::Define {
            expr:
                NExpr::Call {
                    call_site,
                    callee,
                    effect_args,
                    ..
                },
            ..
        } = &statement.kind
        else {
            return Ok(());
        };
        for arg in effect_args {
            if matches!(arg.pass_mode, EffectPassMode::Unknown) {
                continue;
            }
            let Some(address_space) =
                self.effect_arg_address_space(state, statement.origin, arg)?
            else {
                continue;
            };
            out.push(CallSiteProviderRefinement {
                call_site: *call_site,
                binding_idx: arg.binding_idx,
                provider_idx: self.provider_idx_for_effect_arg(*callee, arg.binding_idx),
                address_space,
            });
        }
        Ok(())
    }

    fn effect_arg_address_space(
        &self,
        state: &BorrowState<'db>,
        origin: SemOrigin<'db>,
        arg: &NEffectArg<'db>,
    ) -> Result<Option<ProviderAddressSpace>, SemanticBorrowDiagnostic<'db>> {
        let targets = match &arg.arg {
            NEffectArgValue::Place(place) => self.borrowck.resolve_region(state, place),
            NEffectArgValue::Value(value) => self.value_targets(state, *value),
        };
        if targets.is_empty() {
            return Ok(arg.provider);
        }
        self.address_space_for_targets(&targets, self.effect_arg_origin(arg, origin))
    }

    fn value_targets(&self, state: &BorrowState<'db>, value: NOperand) -> RegionSet<'db> {
        self.borrowck
            .resolve_capability(state.value(value.value))
            .region
    }

    fn address_space_for_targets(
        &self,
        targets: &RegionSet<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<Option<ProviderAddressSpace>, SemanticBorrowDiagnostic<'db>> {
        let mut spaces = Vec::new();
        let mut symbolic = false;
        for target in targets.clauses() {
            let Some(space) = target.payload.root.address_space().known() else {
                symbolic = true;
                continue;
            };
            if !spaces.contains(&space) {
                spaces.push(space);
            }
        }
        if spaces.len() <= 1 && symbolic {
            return Ok(None);
        }
        if let [space] = spaces.as_slice() {
            return Ok(Some(*space));
        }
        spaces.sort_by_key(|space| address_space_rank(*space));
        Err(self.borrowck.diag(
            super::ir::SemanticBorrowDiagKind::ProviderProvenanceConflict,
            origin,
            format!(
                "effect argument may come from multiple address spaces: {}",
                spaces
                    .iter()
                    .map(|space| space.pretty())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        ))
    }

    fn provider_idx_for_effect_arg(
        &self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        binding_idx: u32,
    ) -> Option<u32> {
        match callee.key.owner(self.borrowck.db) {
            BodyOwner::Func(func) => provisional_provider_idx_for_requirement(
                self.borrowck.db,
                EffectParamSite::Func(func),
                binding_idx,
            ),
            BodyOwner::Const(_)
            | BodyOwner::AnonConstBody { .. }
            | BodyOwner::ContractInit { .. }
            | BodyOwner::ContractRecvArm { .. } => None,
        }
    }

    fn effect_arg_origin(&self, arg: &NEffectArg<'db>, fallback: SemOrigin<'db>) -> SemOrigin<'db> {
        match arg.arg {
            NEffectArgValue::Value(value) => operand_origin(value, fallback),
            NEffectArgValue::Place(_) => fallback,
        }
    }
}

fn address_space_rank(space: ProviderAddressSpace) -> u8 {
    match space {
        ProviderAddressSpace::Memory => 0,
        ProviderAddressSpace::Storage => 1,
        ProviderAddressSpace::Transient => 2,
        ProviderAddressSpace::Calldata => 3,
        ProviderAddressSpace::Code => 4,
    }
}
