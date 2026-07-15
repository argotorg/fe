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

use std::convert::Infallible;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{JoinSemiLattice, solve_forward_cfg};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SBlockId, SConst, SLocalId, SemConstScalar, SemConstValue, SemanticInstance,
            get_or_build_semantic_instance, identity_semantic_instance_key,
            normalize_semantic_body,
        },
        ty::{
            ty_check::{
                BodyOwner, EffectParamSite, EffectPassMode, LocalBinding, ParamSite,
                check_const_body, check_contract_init_body, check_contract_recv_arm_body,
                check_func_body,
            },
            ty_def::{BorrowKind, CapabilityKind},
        },
    },
    hir_def::{Contract, Func, FuncParamMode},
    semantic::{ContractFieldId, ProviderSource},
};

use super::borrowck::{
    NBorrowRoot, NEffectArg, NEffectArgValue, NExpr, NSPlace, NSPlaceRoot, NSStmtKind,
    NSTerminatorKind, NormalizedSemanticBody,
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
) -> Option<FxHashSet<u32>> {
    let instance = get_or_build_semantic_instance(
        db,
        identity_semantic_instance_key(db, BodyOwner::ContractInit { contract }),
    );
    instance_assigned_targets(db, instance)
        .as_ref()
        .map(|targets| {
            targets
                .iter()
                .filter_map(|target| match target {
                    AssignedTarget::ContractField(field) if field.contract == contract => {
                        Some(field.index)
                    }
                    _ => None,
                })
                .collect()
        })
}

/// Targets `instance`'s body definitely assigns on every normal exit.
/// `None` when no normal exit is reachable or the body fails to normalize.
#[salsa::tracked(
    return_ref,
    cycle_fn=assigned_targets_cycle_recover,
    cycle_initial=assigned_targets_cycle_initial
)]
fn instance_assigned_targets<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<Vec<AssignedTarget<'db>>> {
    // Bodies with type errors cannot be lowered to semantic MIR; credit no
    // writes instead of forcing a lowering that would panic.
    if !owner_body_is_clean(db, instance.key(db).owner(db)) {
        return Some(Vec::new());
    }
    let body = normalize_semantic_body(db, instance).ok()?;
    if body.blocks.is_empty() {
        return None;
    }

    let mut analysis = DefiniteAssignment::new(db, &body);
    let entry_states = solve_forward_cfg(&mut analysis);

    let mut exit_states = body
        .blocks
        .iter()
        .enumerate()
        .filter(|(_, block)| matches!(block.terminator.kind, NSTerminatorKind::Return(_)))
        .map(|(idx, _)| SBlockId::new(idx))
        .filter(|block| entry_states[*block].reached)
        .map(|block| {
            let Ok(state) = analysis.transfer_state(block, &entry_states[block]);
            state
        });

    let first = exit_states.next()?;
    let assigned = exit_states.fold(first.assigned, |mut acc, state| {
        acc.retain(|target| state.assigned.contains(target));
        acc
    });
    Some(assigned.into_iter().collect())
}

fn owner_body_is_clean<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> bool {
    match owner {
        BodyOwner::Func(func) => check_func_body(db, func).0.is_empty(),
        BodyOwner::Const(const_) => check_const_body(db, const_).0.is_empty(),
        BodyOwner::ContractInit { contract } => check_contract_init_body(db, contract).0.is_empty(),
        BodyOwner::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => check_contract_recv_arm_body(db, contract, recv_idx, arm_idx)
            .0
            .is_empty(),
        BodyOwner::AnonConstBody { .. } => false,
    }
}

fn assigned_targets_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _instance: SemanticInstance<'db>,
) -> Option<Vec<AssignedTarget<'db>>> {
    // Recursive calls initially contribute no writes; iteration refines.
    Some(Vec::new())
}

fn assigned_targets_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Option<Vec<AssignedTarget<'db>>>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<Option<Vec<AssignedTarget<'db>>>> {
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
    body: &'a NormalizedSemanticBody<'db>,
    successors: SecondaryMap<SBlockId, Vec<SBlockId>>,
    /// The unique defining expression of single-assignment locals, used to
    /// chase borrow carriers (`let r = mut x`, borrow-mode call arguments)
    /// back to the borrowed place.
    single_defs: FxHashMap<SLocalId, &'a NExpr<'db>>,
}

impl<'a, 'db> DefiniteAssignment<'a, 'db> {
    fn new(db: &'db dyn HirAnalysisDb, body: &'a NormalizedSemanticBody<'db>) -> Self {
        let mut assign_counts: FxHashMap<SLocalId, u32> = FxHashMap::default();
        let mut defs: FxHashMap<SLocalId, &'a NExpr<'db>> = FxHashMap::default();
        for block in &body.blocks {
            for stmt in &block.stmts {
                if let NSStmtKind::Assign { dst, expr } = &stmt.kind {
                    *assign_counts.entry(*dst).or_default() += 1;
                    defs.insert(*dst, expr);
                }
            }
        }
        let single_defs = defs
            .into_iter()
            .filter(|(local, _)| assign_counts.get(local) == Some(&1))
            .collect();

        let mut_aliased = mut_aliased_locals(body);

        let mut successors: SecondaryMap<SBlockId, Vec<SBlockId>> = SecondaryMap::new();
        successors.resize(body.blocks.len());
        for (idx, block) in body.blocks.iter().enumerate() {
            successors[SBlockId::new(idx)] =
                block_successors(db, body, idx, &block.terminator.kind, &mut_aliased);
        }

        Self {
            db,
            body,
            successors,
            single_defs,
        }
    }

    /// The place mut-borrowed into `local`, when `local` is a
    /// single-assignment carrier holding exactly one borrow (chasing copies
    /// like `let r = mut x`, which lowers to a borrow temp plus a `Use`).
    fn borrowed_place_of(&self, mut local: SLocalId) -> Option<&'a NSPlace<'db>> {
        let mut depth = 0;
        loop {
            match self.single_defs.get(&local)? {
                NExpr::Borrow {
                    place,
                    kind: BorrowKind::Mut,
                    ..
                } => return Some(place),
                NExpr::Use(op) => {
                    depth += 1;
                    if depth > 16 {
                        return None;
                    }
                    local = op.local;
                }
                _ => return None,
            }
        }
    }

    fn transfer_state(
        &self,
        block: SBlockId,
        in_state: &MustAssignState<'db>,
    ) -> Result<MustAssignState<'db>, Infallible> {
        let mut state = in_state.clone();
        for stmt in &self.body.blocks[block.index()].stmts {
            match &stmt.kind {
                NSStmtKind::Store { dst, .. } => {
                    if let Some(target) = self.write_target_of_place(dst) {
                        state.assigned.insert(target);
                    }
                }
                NSStmtKind::Assign { expr, .. } => {
                    if let NExpr::Call {
                        callee,
                        args,
                        effect_args,
                        ..
                    } = expr
                    {
                        self.apply_call(callee.key, args, effect_args, &mut state);
                    }
                }
            }
        }
        Ok(state)
    }

    /// Resolves a whole-value store destination to a caller-visible target,
    /// looking through capability params and single-borrow local carriers.
    fn write_target_of_place(&self, place: &NSPlace<'db>) -> Option<AssignedTarget<'db>> {
        if !place.path.is_empty() {
            return None;
        }
        match &place.root {
            NSPlaceRoot::Root(root_id) => match self.body.root(*root_id)? {
                NBorrowRoot::Provider { binding, .. } => match binding.source {
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
                NBorrowRoot::Param { .. } | NBorrowRoot::LocalSlot { .. } => None,
            },
            NSPlaceRoot::CarrierDerefLocal(local) => self
                .target_of_param_carrier(*local)
                .or_else(|| self.write_target_of_place(self.borrowed_place_of(*local)?)),
        }
    }

    /// A capability-`mut` function parameter carried by `local`, if any.
    fn target_of_param_carrier(&self, local: SLocalId) -> Option<AssignedTarget<'db>> {
        let Some(LocalBinding::Param {
            site: ParamSite::Func(func),
            idx,
            mode: FuncParamMode::View,
            ty,
            ..
        }) = self.body.local(local)?.source
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
        args: &[super::borrowck::NOperand],
        effect_args: &[NEffectArg<'db>],
        state: &mut MustAssignState<'db>,
    ) {
        let BodyOwner::Func(callee_func) = callee_key.owner(self.db) else {
            return;
        };
        let callee = get_or_build_semantic_instance(self.db, callee_key);
        let Some(summary) = instance_assigned_targets(self.db, callee) else {
            return;
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
                    .and_then(|arg| match self.borrowed_place_of(arg.local) {
                        Some(place) => self.write_target_of_place(place),
                        None => self.target_of_param_carrier(arg.local),
                    }),
                _ => None,
            };
            if let Some(mapped) = mapped {
                state.assigned.insert(mapped);
            }
        }
    }
}

impl<'db> dataflow::ForwardCfgAnalysis for DefiniteAssignment<'_, 'db> {
    type Block = SBlockId;
    type State = MustAssignState<'db>;
    type Error = Infallible;

    fn block_count(&self) -> usize {
        self.body.blocks.len()
    }

    fn seed_blocks(&self) -> Vec<Self::Block> {
        (!self.body.blocks.is_empty())
            .then_some(SBlockId::new(0))
            .into_iter()
            .collect()
    }

    fn bottom(&self) -> Self::State {
        MustAssignState::default()
    }

    fn initialize(
        &mut self,
        entry_states: &mut SecondaryMap<Self::Block, Self::State>,
    ) -> Result<(), Self::Error> {
        if !self.body.blocks.is_empty() {
            entry_states[SBlockId::new(0)].reached = true;
        }
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

/// Locals whose slot can be written other than by an `Assign` to the local
/// itself: mut-borrow targets, mut effect-provider places, and direct store
/// destinations. Branch folding must not trust `Assign`-visible constants
/// for these, since an aliasing write between the constant definition and
/// the branch would not show up as a later `Assign`.
fn mut_aliased_locals(body: &NormalizedSemanticBody<'_>) -> FxHashSet<SLocalId> {
    let mut aliased = FxHashSet::default();
    let note_place = |place: &NSPlace<'_>, aliased: &mut FxHashSet<SLocalId>| {
        if let NSPlaceRoot::Root(root_id) = &place.root
            && let Some(NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local }) =
                body.root(*root_id)
        {
            aliased.insert(*local);
        }
    };
    for block in &body.blocks {
        for stmt in &block.stmts {
            match &stmt.kind {
                NSStmtKind::Store { dst, .. } => note_place(dst, &mut aliased),
                NSStmtKind::Assign { expr, .. } => match expr {
                    NExpr::Borrow {
                        place,
                        kind: BorrowKind::Mut,
                        ..
                    } => note_place(place, &mut aliased),
                    NExpr::Call { effect_args, .. } => {
                        for arg in effect_args.iter().filter(|arg| arg.required_mut) {
                            if let NEffectArgValue::Place(place) = &arg.arg {
                                note_place(place, &mut aliased);
                            }
                        }
                    }
                    _ => {}
                },
            }
        }
    }
    aliased
}

fn block_successors<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'db>,
    block_idx: usize,
    terminator: &NSTerminatorKind<'db>,
    mut_aliased: &FxHashSet<SLocalId>,
) -> Vec<SBlockId> {
    match terminator {
        NSTerminatorKind::Goto(target) => vec![*target],
        NSTerminatorKind::Branch {
            cond,
            then_bb,
            else_bb,
        } => match literal_bool_cond(db, body, block_idx, cond.local, mut_aliased) {
            Some(true) => vec![*then_bb],
            Some(false) => vec![*else_bb],
            None => vec![*then_bb, *else_bb],
        },
        NSTerminatorKind::MatchEnum { cases, default, .. } => cases
            .iter()
            .map(|(_, target)| *target)
            .chain(*default)
            .collect(),
        NSTerminatorKind::Assert { .. } | NSTerminatorKind::Return(_) => Vec::new(),
    }
}

/// The boolean value of `local` at `block_idx`'s terminator, when its
/// dominating definition in the same block is a literal constant and the
/// local cannot be mutated through an alias.
fn literal_bool_cond<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'db>,
    block_idx: usize,
    local: SLocalId,
    mut_aliased: &FxHashSet<SLocalId>,
) -> Option<bool> {
    if mut_aliased.contains(&local) {
        return None;
    }
    let last_def = body.blocks[block_idx]
        .stmts
        .iter()
        .rev()
        .find_map(|stmt| match &stmt.kind {
            NSStmtKind::Assign { dst, expr } if *dst == local => Some(expr),
            _ => None,
        })?;
    let NExpr::Const(SConst::Value(value)) = last_def else {
        return None;
    };
    match value.value(db) {
        SemConstValue::Scalar {
            value: SemConstScalar::Bool(value),
            ..
        } => Some(value),
        _ => None,
    }
}
