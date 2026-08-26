use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    semantic::{
        CallSiteProviderRefinement, SemOrigin, SemanticInstance,
        normalized::{
            NBlockId, NEffectArg, NEffectArgValue, NExpr, NOperand, NStatement, NStatementKind,
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
    canon::{CanonPlace, State, address_space_for_borrow_root},
    check::Borrowck,
    diagnostics::operand_origin,
    ir::{SemanticBorrowDiagnostic, SemanticNormalizationFailure},
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
        super::analyses::BorrowSummaryMode::Provisional,
    )
    .map_err(SemanticNormalizationFailure::InternalFailure)?;
    borrowck.compute_entry_states();
    if let Some(blocked) = borrowck
        .compute_loan_targets()
        .map_err(SemanticNormalizationFailure::InternalFailure)?
    {
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
            let mut state = self.borrowck.entry_state[NBlockId::new(bb_idx)].clone();
            for statement in &block.statements {
                self.refine_statement(&state, statement, &mut out)?;
                self.borrowck
                    .canon()
                    .apply_statement_state(&mut state, statement);
            }
        }
        Ok(out)
    }

    fn refine_statement(
        &self,
        state: &State,
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
        state: &State,
        origin: SemOrigin<'db>,
        arg: &NEffectArg<'db>,
    ) -> Result<Option<ProviderAddressSpace>, SemanticBorrowDiagnostic<'db>> {
        let targets = match &arg.arg {
            NEffectArgValue::Place(place) => self
                .borrowck
                .canon()
                .canonicalize_place(state, place, origin)?,
            NEffectArgValue::Value(value) => self.value_targets(state, *value),
        };
        if targets.is_empty() {
            return Ok(arg.provider);
        }
        self.address_space_for_targets(&targets, self.effect_arg_origin(arg, origin))
            .map(Some)
    }

    fn value_targets(&self, state: &State, value: NOperand) -> FxHashSet<CanonPlace<'db>> {
        self.borrowck
            .canon()
            .canonicalize_value_base(state, value.value)
    }

    fn address_space_for_targets(
        &self,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<ProviderAddressSpace, SemanticBorrowDiagnostic<'db>> {
        let mut spaces = Vec::new();
        for target in targets {
            let space = address_space_for_borrow_root(
                self.borrowck.db,
                self.borrowck.instance,
                &self.borrowck.body,
                &target.root,
                origin,
            )?;
            if !spaces.contains(&space) {
                spaces.push(space);
            }
        }
        if let [space] = spaces.as_slice() {
            return Ok(*space);
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
