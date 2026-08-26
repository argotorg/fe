//! Definite-assignment analysis over normalized semantic bodies.
//!
//! Computes which caller-visible targets (contract fields, `uses` effect
//! params, `mut T` capability params) a body definitely writes on every
//! normal exit. The contract immutable-field init check consumes this to
//! require that every code-backed field is assigned before `init` returns.
//!
//! The analysis is a classical forward must-analysis over the same
//! normalized CFG borrowck uses: branch states merge by intersection and
//! loop bodies may execute zero times. The only value reasoning is folding
//! branches whose condition is a literal boolean constant (`if true`,
//! `while true { .. break }`), which is decided per-block without tracking
//! facts across joins.

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{JoinSemiLattice, try_solve_forward_cfg};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BlockedSemanticBody, BorrowDiagnosticId, SConst, SemConstScalar, SemConstValue,
            SemanticInstance, SemanticNormalizationFailure, get_or_build_semantic_instance,
            identity_semantic_instance_key,
            normalized::{
                NBlockId, NEffectArg, NEffectArgValue, NExpr, NOperand, NPlace, NPlaceBase,
                NRootKind, NStatementKind, NTerminatorKind, NValueId, NormalizedBody,
                SemanticBodyAdmission, semantic_body_admission,
            },
        },
        ty::{
            ty_check::{BodyOwner, EffectParamSite, EffectPassMode, LocalBinding, ParamSite},
            ty_def::{BorrowKind, CapabilityKind},
        },
    },
    hir_def::{Contract, Func, FuncParamMode},
    semantic::{ContractFieldId, ProviderSource},
};

/// A caller-visible write target a body definitely assigns (whole-value)
/// on every normal exit.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
enum AssignedTarget<'db> {
    /// A contract field bound as an effect provider (e.g. `uses (mut x)` in
    /// `init`).
    ContractField(ContractFieldId<'db>),
    /// A `uses` effect requirement of a function.
    FuncEffect {
        func: Func<'db>,
        requirement_idx: u32,
    },
    /// A `mut T` capability parameter of a function. Writes through `own`
    /// or non-capability params stay local to the callee and are excluded.
    FuncParam { func: Func<'db>, param_idx: u32 },
}

/// Field indices of `contract` definitely assigned on every normal exit of
/// its `init` body. `None` means no normal exit is reachable (the body
/// always diverges, so deployment can never succeed) or the body could not
/// be analyzed; callers should not require anything in that case.
pub fn contract_init_assigned_fields<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> Result<Option<FxHashSet<u32>>, SemanticNormalizationFailure<'db>> {
    let instance = get_or_build_semantic_instance(
        db,
        identity_semantic_instance_key(db, BodyOwner::ContractInit { contract }),
    );
    match instance_assigned_targets(db, instance) {
        AssignedTargetsResult::Ready(targets) => Ok(targets.as_ref().map(|targets| {
            targets
                .iter()
                .filter_map(|target| match target {
                    AssignedTarget::ContractField(field) if field.contract == contract => {
                        Some(field.index)
                    }
                    _ => None,
                })
                .collect()
        })),
        AssignedTargetsResult::Blocked(blocked) => {
            Err(SemanticNormalizationFailure::Blocked(blocked.clone()))
        }
        AssignedTargetsResult::InternalFailure(diag) => Err(
            SemanticNormalizationFailure::InternalFailure(diag.diag(db).clone()),
        ),
    }
}

/// Targets `instance`'s body definitely assigns on every normal exit.
/// `Ready(None)` means no normal exit is reachable. Admission failures remain
/// explicit so callers cannot mistake an unanalyzable body for divergence.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
enum AssignedTargetsResult<'db> {
    Ready(Option<Vec<AssignedTarget<'db>>>),
    Blocked(BlockedSemanticBody<'db>),
    InternalFailure(BorrowDiagnosticId<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum AssignedTargetsFailure<'db> {
    Blocked(BlockedSemanticBody<'db>),
    InternalFailure(BorrowDiagnosticId<'db>),
}

#[salsa::tracked(
    return_ref,
    cycle_fn=assigned_targets_cycle_recover,
    cycle_initial=assigned_targets_cycle_initial
)]
fn instance_assigned_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> AssignedTargetsResult<'db> {
    let body = match semantic_body_admission(db, instance) {
        SemanticBodyAdmission::Ready(body) => body.body(db).clone(),
        SemanticBodyAdmission::Blocked(blocked) => return AssignedTargetsResult::Blocked(blocked),
        SemanticBodyAdmission::InternalFailure(diag) => {
            return AssignedTargetsResult::InternalFailure(diag);
        }
    };
    if body.blocks.is_empty() {
        return AssignedTargetsResult::Ready(None);
    }

    let mut analysis = DefiniteAssignment::new(db, &body);
    let entry_states = match try_solve_forward_cfg(&mut analysis) {
        Ok(states) => states,
        Err(AssignedTargetsFailure::Blocked(blocked)) => {
            return AssignedTargetsResult::Blocked(blocked);
        }
        Err(AssignedTargetsFailure::InternalFailure(diag)) => {
            return AssignedTargetsResult::InternalFailure(diag);
        }
    };

    let mut exit_states = Vec::new();
    for (idx, block) in body.blocks.iter().enumerate() {
        let block_id = NBlockId::new(idx);
        if matches!(block.terminator.kind, NTerminatorKind::Return(_))
            && entry_states[block_id].reached
        {
            match analysis.transfer_state(block_id, &entry_states[block_id]) {
                Ok(state) => exit_states.push(state),
                Err(AssignedTargetsFailure::Blocked(blocked)) => {
                    return AssignedTargetsResult::Blocked(blocked);
                }
                Err(AssignedTargetsFailure::InternalFailure(diag)) => {
                    return AssignedTargetsResult::InternalFailure(diag);
                }
            }
        }
    }

    let Some(first) = exit_states.pop() else {
        return AssignedTargetsResult::Ready(None);
    };
    let assigned = exit_states
        .into_iter()
        .fold(first.assigned, |mut acc, state| {
            acc.retain(|target| state.assigned.contains(target));
            acc
        });
    AssignedTargetsResult::Ready(Some(assigned.into_iter().collect()))
}

fn assigned_targets_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _instance: SemanticInstance<'db>,
) -> AssignedTargetsResult<'db> {
    // Recursive calls initially contribute no writes; iteration refines.
    AssignedTargetsResult::Ready(Some(Vec::new()))
}

fn assigned_targets_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &AssignedTargetsResult<'db>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<AssignedTargetsResult<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

#[derive(Clone, Default, PartialEq, Eq)]
struct MustAssignState<'db> {
    reached: bool,
    assigned: FxHashSet<AssignedTarget<'db>>,
}

impl JoinSemiLattice for MustAssignState<'_> {
    fn join_into(&mut self, other: &Self) -> bool {
        if !other.reached {
            return false;
        }
        if !self.reached {
            *self = other.clone();
            return true;
        }
        let before = self.assigned.len();
        self.assigned
            .retain(|target| other.assigned.contains(target));
        before != self.assigned.len()
    }
}

struct DefiniteAssignment<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: &'a NormalizedBody<'db>,
    successors: SecondaryMap<NBlockId, Vec<NBlockId>>,
    definitions: FxHashMap<NValueId, &'a NExpr<'db>>,
}

impl<'a, 'db> DefiniteAssignment<'a, 'db> {
    fn new(db: &'db dyn HirAnalysisDb, body: &'a NormalizedBody<'db>) -> Self {
        let mut definitions = FxHashMap::default();
        for block in &body.blocks {
            for statement in &block.statements {
                if let NStatementKind::Define { result, expr } = &statement.kind {
                    definitions.insert(*result, expr);
                }
            }
        }

        let mut successors: SecondaryMap<NBlockId, Vec<NBlockId>> = SecondaryMap::new();
        successors.resize(body.blocks.len());
        for (idx, block) in body.blocks.iter().enumerate() {
            successors[NBlockId::new(idx)] = block_successors(db, body, &block.terminator.kind);
        }

        Self {
            db,
            body,
            successors,
            definitions,
        }
    }

    /// The place mut-borrowed into `value`, chasing exact SSA forwards such as
    /// `let r = mut x` back to the defining borrow.
    fn borrowed_place_of(&self, mut value: NValueId) -> Option<&'a NPlace<'db>> {
        let mut depth = 0;
        loop {
            match self.definitions.get(&value)? {
                NExpr::Borrow {
                    place,
                    kind: BorrowKind::Mut,
                    ..
                } => return Some(place),
                NExpr::Forward { src } => {
                    depth += 1;
                    if depth > 16 {
                        return None;
                    }
                    value = src.value;
                }
                _ => return None,
            }
        }
    }

    fn transfer_state(
        &self,
        block: NBlockId,
        in_state: &MustAssignState<'db>,
    ) -> Result<MustAssignState<'db>, AssignedTargetsFailure<'db>> {
        let mut state = in_state.clone();
        for statement in &self.body.blocks[block.index()].statements {
            match &statement.kind {
                NStatementKind::Store { destination, .. } => {
                    if let Some(target) = self.write_target_of_place(destination) {
                        state.assigned.insert(target);
                    }
                }
                NStatementKind::Define { expr, .. } => {
                    if let NExpr::Call {
                        callee,
                        args,
                        effect_args,
                        ..
                    } = expr
                    {
                        self.apply_call(callee.key, args, effect_args, &mut state)?;
                    }
                }
            }
        }
        Ok(state)
    }

    /// Resolves a whole-value store destination to a caller-visible target,
    /// looking through capability params and single-borrow local carriers.
    fn write_target_of_place(&self, place: &NPlace<'db>) -> Option<AssignedTarget<'db>> {
        if !place.path.is_empty() {
            return None;
        }
        match place.base {
            NPlaceBase::Root(root_id) => match &self.body.root(root_id)?.kind {
                NRootKind::Provider { binding } => match binding.source {
                    ProviderSource::ContractField { field } => {
                        Some(AssignedTarget::ContractField(field))
                    }
                    ProviderSource::UsesParam {
                        site: EffectParamSite::Func(func),
                        requirement_idx,
                    } => Some(AssignedTarget::FuncEffect {
                        func,
                        requirement_idx,
                    }),
                    _ => None,
                },
                NRootKind::LocalSlot { .. }
                | NRootKind::ParamPlace { .. }
                | NRootKind::CapabilityRepresentation { .. } => None,
            },
            NPlaceBase::CapabilityTarget { carrier } => self
                .target_of_param_carrier(carrier)
                .or_else(|| self.write_target_of_place(self.borrowed_place_of(carrier)?)),
        }
    }

    /// A capability-`mut` function parameter carried by `value`, if any.
    fn target_of_param_carrier(&self, value: NValueId) -> Option<AssignedTarget<'db>> {
        let Some(LocalBinding::Param {
            site: ParamSite::Func(func),
            idx,
            mode: FuncParamMode::View,
            ty,
            ..
        }) = self.body.value(value)?.source
        else {
            return None;
        };
        matches!(ty.as_capability(self.db), Some((CapabilityKind::Mut, _))).then(|| {
            AssignedTarget::FuncParam {
                func,
                param_idx: idx as u32,
            }
        })
    }

    /// Credits caller-side targets for writes the callee definitely performs
    /// through its effect requirements and capability params.
    fn apply_call(
        &self,
        callee_key: crate::analysis::semantic::SemanticInstanceKey<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        state: &mut MustAssignState<'db>,
    ) -> Result<(), AssignedTargetsFailure<'db>> {
        let BodyOwner::Func(callee_func) = callee_key.owner(self.db) else {
            return Ok(());
        };
        let callee = get_or_build_semantic_instance(self.db, callee_key);
        let summary = match instance_assigned_targets(self.db, callee) {
            AssignedTargetsResult::Ready(Some(summary)) => summary,
            AssignedTargetsResult::Ready(None) => return Ok(()),
            AssignedTargetsResult::Blocked(blocked) => {
                return Err(AssignedTargetsFailure::Blocked(blocked.clone()));
            }
            AssignedTargetsResult::InternalFailure(diag) => {
                return Err(AssignedTargetsFailure::InternalFailure(*diag));
            }
        };
        for target in summary {
            let mapped = match target {
                // Contract fields are absolute targets: when the callee
                // instance has the caller's effect providers substituted in,
                // its summary names the written field directly.
                AssignedTarget::ContractField { .. } => Some(*target),
                AssignedTarget::FuncEffect {
                    func,
                    requirement_idx,
                } if *func == callee_func => effect_args
                    .iter()
                    .find(|arg| {
                        arg.binding_idx == *requirement_idx
                            && arg.pass_mode == EffectPassMode::ByPlace
                    })
                    .and_then(|arg| match &arg.arg {
                        NEffectArgValue::Place(place) => self.write_target_of_place(place),
                        NEffectArgValue::Value(_) => None,
                    }),
                AssignedTarget::FuncParam { func, param_idx } if *func == callee_func => args
                    .get(*param_idx as usize)
                    .and_then(|arg| match self.borrowed_place_of(arg.value) {
                        Some(place) => self.write_target_of_place(place),
                        None => self.target_of_param_carrier(arg.value),
                    }),
                _ => None,
            };
            if let Some(mapped) = mapped {
                state.assigned.insert(mapped);
            }
        }
        Ok(())
    }
}

impl<'db> dataflow::ForwardCfgAnalysis for DefiniteAssignment<'_, 'db> {
    type Block = NBlockId;
    type State = MustAssignState<'db>;
    type Error = AssignedTargetsFailure<'db>;

    fn block_count(&self) -> usize {
        self.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        vec![self.body.entry]
    }

    fn bottom(&self) -> Self::State {
        MustAssignState::default()
    }

    fn initialize(
        &mut self,
        entry_states: &mut SecondaryMap<Self::Block, Self::State>,
    ) -> Result<(), Self::Error> {
        entry_states[self.body.entry].reached = true;
        Ok(())
    }

    fn transfer(
        &mut self,
        block: Self::Block,
        in_state: &Self::State,
    ) -> Result<Self::State, Self::Error> {
        self.transfer_state(block, in_state)
    }

    fn successors(&self, block: Self::Block) -> &[Self::Block] {
        &self.successors[block]
    }
}

fn block_successors<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    terminator: &NTerminatorKind<'db>,
) -> Vec<NBlockId> {
    match terminator {
        NTerminatorKind::Goto(target) => vec![target.block],
        NTerminatorKind::Branch {
            cond,
            then_target,
            else_target,
        } => match literal_bool_cond(db, body, cond.value) {
            Some(true) => vec![then_target.block],
            Some(false) => vec![else_target.block],
            None => vec![then_target.block, else_target.block],
        },
        NTerminatorKind::MatchEnum { cases, default, .. } => cases
            .iter()
            .map(|(_, target)| target.block)
            .chain(default.iter().map(|target| target.block))
            .collect(),
        NTerminatorKind::Assert { .. } | NTerminatorKind::Return(_) => Vec::new(),
    }
}

/// The literal boolean value of an immutable SSA definition, chasing exact
/// forwards. Loads remain unknown because roots may have changed.
fn literal_bool_cond<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
    mut value: NValueId,
) -> Option<bool> {
    for _ in 0..16 {
        let definition = body.value(value)?.definition;
        let crate::analysis::semantic::normalized::NValueDefinition::Statement { block, statement } =
            definition
        else {
            return None;
        };
        let NStatementKind::Define { expr, .. } =
            &body.block(block)?.statements.get(statement as usize)?.kind
        else {
            return None;
        };
        match expr {
            NExpr::Forward { src } => value = src.value,
            NExpr::Const(SConst::Value(value)) => {
                return match value.value(db) {
                    SemConstValue::Scalar {
                        value: SemConstScalar::Bool(value),
                        ..
                    } => Some(value),
                    _ => None,
                };
            }
            _ => return None,
        }
    }
    None
}
