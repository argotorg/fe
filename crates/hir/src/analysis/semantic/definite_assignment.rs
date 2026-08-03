//! Definite-assignment analysis over normalized semantic bodies.
//!
//! Computes which caller-visible targets (contract fields, `uses` effect
//! params, `mut T` capability params) a body definitely writes on every
//! normal exit. The contract immutable-field init check consumes this to
//! require that every code-backed field is assigned before `init` returns.
//!
//! The analysis is a classical forward must-analysis over the same
//! normalized CFG borrowck uses: branch states merge by intersection and
//! loop bodies may execute zero times. Borrowck's shared successor
//! refinement folds alias-safe boolean and enum constants through normalized
//! forwarding locals before this must-analysis runs.

use std::convert::Infallible;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::{JoinSemiLattice, solve_forward_cfg};
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SBlockId, SCallReturnProjectionStep, SConst, SLocalId, SemConstScalar, SemConstValue,
            SemanticInstance, get_or_build_semantic_instance, identity_semantic_instance_key,
            normalize_semantic_body_for_analysis,
        },
        ty::{
            adt_def::AdtRef,
            const_ty::CallableInputLayoutHoleOrigin,
            ty_check::{
                BodyOwner, EffectParamSite, EffectPassMode, LocalBinding, ParamSite,
                check_const_body, check_contract_init_body, check_contract_recv_arm_body,
                check_func_body,
            },
            ty_def::{BorrowKind, CapabilityKind, TyId},
            ty_reaches_mut_borrow,
        },
    },
    hir_def::{ClosureDef, Contract, Func, FuncParamMode},
    projection::{IndexSource, Projection},
    semantic::{ContractFieldId, ProviderSource},
};

use super::borrowck::{
    NBorrowRoot, NBorrowRootId, NCallReturnSources, NEffectArg, NEffectArgValue, NExpr, NSPlace,
    NSPlaceRoot, NSProjectionPath, NSStmtKind, NSTerminatorKind, NormalizedSemanticBody,
    cfg_reachable_blocks, normalized_cfg_successor_indices, semantic_projection_ty,
};

/// A caller-visible write target a body definitely assigns (whole-value)
/// on every normal exit.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
enum AssignedTarget<'db> {
    /// A contract field bound as an effect provider (e.g. `uses (mut x)` in
    /// `init`).
    ContractField(ContractFieldId<'db>),
    /// A `uses` effect requirement of a function.
    FuncEffect {
        func: Func<'db>,
        requirement_idx: u32,
        fields: Vec<CarrierProjection>,
    },
    /// A mutable capability carried by a function parameter. Ordinary
    /// by-value writes stay local to the callee and are excluded.
    ///
    /// `fields` identifies a statically projected capability nested inside
    /// the parameter. This lets a generic helper forward a concrete closure
    /// environment without losing the target of one of its captures.
    FuncParam {
        func: Func<'db>,
        param_idx: u32,
        fields: Vec<CarrierProjection>,
    },
    /// A capability stored in a closure environment capture.
    ClosureCapture {
        def: ClosureDef<'db>,
        capture_idx: u32,
        fields: Vec<CarrierProjection>,
    },
    /// A capability stored in a closure's tuple-packed logical arguments.
    ClosureArgument {
        def: ClosureDef<'db>,
        param_idx: u32,
        fields: Vec<CarrierProjection>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
enum CarrierProjection {
    Field(usize),
    VariantField {
        variant: u16,
        field: usize,
    },
    ConstantIndex(usize),
    /// All elements share one provenance (for an array repeat).
    AnyIndex,
    /// A runtime-selected element. Retaining the selector local distinguishes
    /// a later use of the same program-point value from an unrelated index.
    DynamicIndex(SLocalId),
}

fn is_carrier_index(step: &CarrierProjection) -> bool {
    matches!(
        step,
        CarrierProjection::ConstantIndex(_)
            | CarrierProjection::AnyIndex
            | CarrierProjection::DynamicIndex(_)
    )
}

/// Asymmetric match used when `pattern` comes from a summary or wildcard
/// override and `actual` is a concrete caller-side projection.
fn carrier_pattern_matches(pattern: &CarrierProjection, actual: &CarrierProjection) -> bool {
    pattern == actual || matches!(pattern, CarrierProjection::AnyIndex) && is_carrier_index(actual)
}

fn carrier_pattern_is_prefix(pattern: &[CarrierProjection], actual: &[CarrierProjection]) -> bool {
    pattern.len() <= actual.len()
        && pattern
            .iter()
            .zip(actual)
            .all(|(pattern, actual)| carrier_pattern_matches(pattern, actual))
}

/// Whether two runtime paths can select the same storage. Distinct constant
/// indices cannot alias; a dynamic selector can alias any index unless it is
/// proven to be the exact same selector by the stronger equality check.
fn carrier_paths_may_alias(left: &[CarrierProjection], right: &[CarrierProjection]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left == right
                || is_carrier_index(left)
                    && is_carrier_index(right)
                    && !matches!(
                        (left, right),
                        (
                            CarrierProjection::ConstantIndex(left),
                            CarrierProjection::ConstantIndex(right)
                        ) if left != right
                    )
        })
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct CarrierSlot {
    local: SLocalId,
    fields: Vec<CarrierProjection>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum CarrierProvenance<'db> {
    Known(FxHashSet<AssignedTarget<'db>>),
    Unknown,
}

impl<'db> CarrierProvenance<'db> {
    fn singleton(target: AssignedTarget<'db>) -> Self {
        Self::Known(FxHashSet::from_iter([target]))
    }

    fn definite_target(&self) -> Option<AssignedTarget<'db>> {
        let Self::Known(targets) = self else {
            return None;
        };
        (targets.len() == 1).then(|| targets.iter().next().expect("singleton target").clone())
    }

    fn join(&mut self, other: &Self) -> bool {
        match (&mut *self, other) {
            (Self::Unknown, _) => false,
            (_, Self::Unknown) => {
                *self = Self::Unknown;
                true
            }
            (Self::Known(targets), Self::Known(other_targets)) => {
                let before = targets.len();
                targets.extend(other_targets.iter().cloned());
                targets.len() != before
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
struct CarrierRebind<'db> {
    destination: AssignedTarget<'db>,
    /// `None` means the destination has no single reaching carrier target.
    source: Option<AssignedTarget<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
struct AssignmentSummary<'db> {
    assigned: Vec<AssignedTarget<'db>>,
    rebinds: Vec<CarrierRebind<'db>>,
    /// False when a cycle is unresolved or a caller-visible rebind cannot be
    /// represented exactly at the summary boundary.
    rebinds_complete: bool,
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
    instance_assignment_summary(db, instance)
        .as_ref()
        .map(|summary| {
            summary
                .assigned
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
fn instance_assignment_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<AssignmentSummary<'db>> {
    // Bodies with type errors cannot be lowered to semantic MIR; credit no
    // writes instead of forcing a lowering that would panic.
    if !owner_body_is_clean(db, instance.key(db).owner(db)) {
        return Some(AssignmentSummary {
            assigned: Vec::new(),
            rebinds: Vec::new(),
            rebinds_complete: false,
        });
    }
    let body = normalize_semantic_body_for_analysis(db, instance).ok()?;
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

    let mut exit_state = exit_states.next()?;
    for state in exit_states {
        exit_state.join_into(&state);
    }
    let (rebinds, rebinds_complete) = analysis.summary_rebinds(&exit_state);
    Some(AssignmentSummary {
        assigned: exit_state.assigned.iter().cloned().collect(),
        rebinds,
        rebinds_complete,
    })
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
        BodyOwner::Closure { def, .. } => {
            BodyOwner::from_body(db, def.body).is_some_and(|parent| owner_body_is_clean(db, parent))
        }
        BodyOwner::AnonConstBody { .. } => false,
    }
}

fn assigned_targets_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    _instance: SemanticInstance<'db>,
) -> Option<AssignmentSummary<'db>> {
    // Recursive calls initially contribute no writes; iteration refines.
    Some(AssignmentSummary {
        assigned: Vec::new(),
        rebinds: Vec::new(),
        rebinds_complete: false,
    })
}

fn assigned_targets_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &Option<AssignmentSummary<'db>>,
    _count: u32,
    _instance: SemanticInstance<'db>,
) -> salsa::CycleRecoveryAction<Option<AssignmentSummary<'db>>> {
    salsa::CycleRecoveryAction::Iterate
}

#[derive(Clone, Default, PartialEq, Eq)]
struct MustAssignState<'db> {
    reached: bool,
    assigned: FxHashSet<AssignedTarget<'db>>,
    carrier_overrides: FxHashMap<CarrierSlot, CarrierProvenance<'db>>,
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
        let mut changed = before != self.assigned.len();

        let keys = self
            .carrier_overrides
            .keys()
            .chain(other.carrier_overrides.keys())
            .cloned()
            .collect::<FxHashSet<_>>();
        for key in keys {
            match (
                self.carrier_overrides.get_mut(&key),
                other.carrier_overrides.get(&key),
            ) {
                (Some(current), Some(incoming)) => changed |= current.join(incoming),
                (Some(current), None) => {
                    if !matches!(current, CarrierProvenance::Unknown) {
                        *current = CarrierProvenance::Unknown;
                        changed = true;
                    }
                }
                (None, Some(_)) => {
                    self.carrier_overrides
                        .insert(key, CarrierProvenance::Unknown);
                    changed = true;
                }
                (None, None) => {}
            }
        }
        changed
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
    /// Canonical local whose backing storage is each effect-provider root.
    /// Building this once avoids a linear scan of all locals every time a
    /// projected provider carrier is resolved.
    provider_root_locals: FxHashMap<NBorrowRootId, SLocalId>,
}

fn carrier_path_from_semantic(path: &NSProjectionPath<'_>) -> Option<Vec<CarrierProjection>> {
    let mut result = Vec::with_capacity(path.len());
    for projection in path.iter() {
        let step = match projection {
            Projection::Field(field) => CarrierProjection::Field(*field),
            Projection::VariantField {
                variant, field_idx, ..
            } => CarrierProjection::VariantField {
                variant: variant.0,
                field: *field_idx,
            },
            Projection::Index(IndexSource::Constant(index)) => {
                CarrierProjection::ConstantIndex(*index)
            }
            Projection::Index(IndexSource::Dynamic(index)) => {
                CarrierProjection::DynamicIndex(*index)
            }
            // Capability dereferences are transport-only for structural
            // carrier identity. The selected type traversal independently
            // unwraps capability layers, so retaining Deref here would double
            // count the wrapper.
            Projection::Deref => continue,
            Projection::Discriminant => return None,
        };
        result.push(step);
    }
    Some(result)
}

fn canonical_forwarded_local(
    body: &NormalizedSemanticBody<'_>,
    single_defs: &FxHashMap<SLocalId, &NExpr<'_>>,
    mut local: SLocalId,
) -> SLocalId {
    let mut visiting = FxHashSet::default();
    while visiting.insert(local) {
        let Some(expr) = single_defs.get(&local) else {
            break;
        };
        let next = match expr {
            NExpr::Use(value) | NExpr::Cast { value, .. } => Some(value.local),
            NExpr::ReadPlace { place, .. } if place.path.is_empty() => match place.root {
                NSPlaceRoot::CarrierDerefLocal(source) => Some(source),
                NSPlaceRoot::Root(root) => match body.root(root) {
                    Some(NBorrowRoot::Param { local, .. })
                    | Some(NBorrowRoot::LocalSlot { local }) => Some(*local),
                    Some(NBorrowRoot::Provider { .. }) | None => None,
                },
            },
            _ => None,
        };
        let Some(next) = next else {
            break;
        };
        local = next;
    }
    local
}

fn canonicalize_dynamic_indices(
    db: &dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'_>,
    single_defs: &FxHashMap<SLocalId, &NExpr<'_>>,
    path: &mut [CarrierProjection],
) {
    for step in path {
        if let CarrierProjection::DynamicIndex(local) = step {
            *local = canonical_forwarded_local(body, single_defs, *local);
            if let Some(NExpr::Const(SConst::Value(value))) = single_defs.get(local)
                && let SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } = value.value(db)
                && let Some(index) = value.to_usize()
            {
                *step = CarrierProjection::ConstantIndex(index);
            }
        }
    }
}

fn append_carrier_path_to_projection<'db>(
    db: &'db dyn HirAnalysisDb,
    root_ty: TyId<'db>,
    path: &mut NSProjectionPath<'db>,
    carrier_path: &[CarrierProjection],
) -> Option<()> {
    for step in carrier_path {
        let current_ty = loop {
            let current_ty = semantic_projection_ty(db, root_ty, path)?.0;
            if current_ty.as_capability(db).is_some() {
                path.push(Projection::Deref);
            } else {
                break current_ty;
            }
        };
        let projection = match *step {
            CarrierProjection::Field(field) => Projection::Field(field),
            CarrierProjection::VariantField { variant, field } => Projection::VariantField {
                variant: crate::analysis::semantic::VariantIndex(variant),
                enum_ty: current_ty,
                field_idx: field,
            },
            CarrierProjection::ConstantIndex(index) => {
                Projection::Index(IndexSource::Constant(index))
            }
            CarrierProjection::DynamicIndex(index) => {
                Projection::Index(IndexSource::Dynamic(index))
            }
            // AnyIndex is used only for type traversal here. Call-site
            // mapping rejects it before treating this synthesized index as a
            // concrete storage identity.
            CarrierProjection::AnyIndex => Projection::Index(IndexSource::Constant(0)),
        };
        path.push(projection);
    }
    Some(())
}

fn append_carrier_path<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'db>,
    place: &mut NSPlace<'db>,
    carrier_path: &[CarrierProjection],
) -> Option<()> {
    append_carrier_path_to_projection(
        db,
        body.place_root_ty(&place.root)?,
        &mut place.path,
        carrier_path,
    )
}

/// Returns the logical local carrier graph exposed by `place`. Capability-
/// valued reads are chased so a write through a copied aggregate handle
/// updates the same descendant path later reads observe.
fn storage_exposed_local<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &NormalizedSemanticBody<'db>,
    single_defs: &FxHashMap<SLocalId, &NExpr<'db>>,
    provider_root_locals: &FxHashMap<NBorrowRootId, SLocalId>,
    place: &NSPlace<'db>,
    visiting: &mut FxHashSet<SLocalId>,
) -> Option<(SLocalId, Vec<CarrierProjection>)> {
    let Some(mut fields) = carrier_path_from_semantic(&place.path) else {
        return match place.root {
            NSPlaceRoot::Root(root) => match body.root(root)? {
                NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => {
                    Some((*local, Vec::new()))
                }
                NBorrowRoot::Provider { .. } => None,
            },
            NSPlaceRoot::CarrierDerefLocal(local) => Some((local, Vec::new())),
        };
    };
    canonicalize_dynamic_indices(db, body, single_defs, &mut fields);

    match place.root {
        NSPlaceRoot::Root(root) => match body.root(root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => {
                Some((*local, fields))
            }
            NBorrowRoot::Provider { .. } => provider_root_locals
                .get(&root)
                .copied()
                .map(|local| (local, fields)),
        },
        NSPlaceRoot::CarrierDerefLocal(local) => {
            if !visiting.insert(local) {
                return None;
            }
            match single_defs.get(&local) {
                Some(NExpr::AggregateMake { fields: values, .. }) => {
                    let (&field, rest) = fields.split_first()?;
                    let value = match field {
                        CarrierProjection::Field(field)
                        | CarrierProjection::ConstantIndex(field) => values.get(field)?,
                        CarrierProjection::VariantField { .. }
                        | CarrierProjection::AnyIndex
                        | CarrierProjection::DynamicIndex(_) => return None,
                    };
                    let mut source = NSPlace {
                        root: NSPlaceRoot::CarrierDerefLocal(value.local),
                        path: NSProjectionPath::default(),
                    };
                    append_carrier_path(db, body, &mut source, rest)?;
                    storage_exposed_local(
                        db,
                        body,
                        single_defs,
                        provider_root_locals,
                        &source,
                        visiting,
                    )
                }
                Some(NExpr::ArrayRepeat { value, .. }) => {
                    let (index, rest) = fields.split_first()?;
                    if !matches!(
                        index,
                        CarrierProjection::ConstantIndex(_)
                            | CarrierProjection::AnyIndex
                            | CarrierProjection::DynamicIndex(_)
                    ) {
                        return None;
                    }
                    let mut source = NSPlace {
                        root: NSPlaceRoot::CarrierDerefLocal(value.local),
                        path: NSProjectionPath::default(),
                    };
                    append_carrier_path(db, body, &mut source, rest)?;
                    storage_exposed_local(
                        db,
                        body,
                        single_defs,
                        provider_root_locals,
                        &source,
                        visiting,
                    )
                }
                Some(NExpr::EnumMake {
                    variant,
                    fields: values,
                    ..
                }) => {
                    let (projection, rest) = fields.split_first()?;
                    let CarrierProjection::VariantField {
                        variant: selected,
                        field,
                    } = projection
                    else {
                        return None;
                    };
                    if *selected != variant.0 {
                        return None;
                    }
                    let mut source = NSPlace {
                        root: NSPlaceRoot::CarrierDerefLocal(values.get(*field)?.local),
                        path: NSProjectionPath::default(),
                    };
                    append_carrier_path(db, body, &mut source, rest)?;
                    storage_exposed_local(
                        db,
                        body,
                        single_defs,
                        provider_root_locals,
                        &source,
                        visiting,
                    )
                }
                Some(NExpr::ExtractEnumField {
                    value,
                    variant,
                    field,
                }) => {
                    let mut source_fields = Vec::with_capacity(fields.len() + 1);
                    source_fields.push(CarrierProjection::VariantField {
                        variant: variant.0,
                        field: usize::from(field.0),
                    });
                    source_fields.extend_from_slice(&fields);
                    let mut source = NSPlace {
                        root: NSPlaceRoot::CarrierDerefLocal(value.local),
                        path: NSProjectionPath::default(),
                    };
                    append_carrier_path(db, body, &mut source, &source_fields)?;
                    storage_exposed_local(
                        db,
                        body,
                        single_defs,
                        provider_root_locals,
                        &source,
                        visiting,
                    )
                }
                Some(NExpr::Use(value) | NExpr::Cast { value, .. }) => {
                    let mut source = NSPlace {
                        root: NSPlaceRoot::CarrierDerefLocal(value.local),
                        path: NSProjectionPath::default(),
                    };
                    append_carrier_path(db, body, &mut source, &fields)?;
                    storage_exposed_local(
                        db,
                        body,
                        single_defs,
                        provider_root_locals,
                        &source,
                        visiting,
                    )
                }
                Some(NExpr::Borrow {
                    place,
                    kind: BorrowKind::Mut,
                    ..
                })
                | Some(NExpr::ReadPlace { place, .. }) => {
                    let mut source = place.clone();
                    append_carrier_path(db, body, &mut source, &fields)?;
                    storage_exposed_local(
                        db,
                        body,
                        single_defs,
                        provider_root_locals,
                        &source,
                        visiting,
                    )
                }
                _ => Some((local, fields)),
            }
        }
    }
}

impl<'a, 'db> DefiniteAssignment<'a, 'db> {
    fn new(db: &'db dyn HirAnalysisDb, body: &'a NormalizedSemanticBody<'db>) -> Self {
        let refined_successors = normalized_cfg_successor_indices(db, body);
        let reachable_blocks = cfg_reachable_blocks(&refined_successors);
        let mut assign_counts: FxHashMap<SLocalId, u32> = FxHashMap::default();
        let mut defs: FxHashMap<SLocalId, &'a NExpr<'db>> = FxHashMap::default();
        for (block_idx, block) in body.blocks.iter().enumerate() {
            if !reachable_blocks.contains(&SBlockId::new(block_idx)) {
                continue;
            }
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
        let mut provider_root_locals = FxHashMap::default();
        for (idx, local) in body.locals.iter().enumerate() {
            let Some(backing) = local.backing_place() else {
                continue;
            };
            let NSPlaceRoot::Root(root) = backing.root else {
                continue;
            };
            if backing.path.is_empty()
                && matches!(body.root(root), Some(NBorrowRoot::Provider { .. }))
            {
                provider_root_locals
                    .entry(root)
                    .or_insert_with(|| SLocalId::new(idx));
            }
        }
        let mut successors: SecondaryMap<SBlockId, Vec<SBlockId>> = SecondaryMap::new();
        successors.resize(body.blocks.len());
        for (block, refined) in refined_successors.iter() {
            successors[block].extend(refined.iter().copied());
        }

        Self {
            db,
            body,
            successors,
            single_defs,
            provider_root_locals,
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
                NSStmtKind::Store { dst, src } => {
                    self.apply_carrier_store(dst, *src, &mut state);
                    if let Some(target) = self.write_target_of_place(dst, &state) {
                        state.assigned.insert(target);
                    }
                }
                NSStmtKind::Assign { dst, expr } => {
                    self.apply_carrier_assign(*dst, expr, &mut state);
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

    fn apply_carrier_assign(
        &self,
        dst: SLocalId,
        expr: &NExpr<'db>,
        state: &mut MustAssignState<'db>,
    ) {
        let Some(dst_ty) = self.body.local(dst).map(|local| local.ty) else {
            return;
        };
        if !ty_reaches_mut_borrow(self.db, dst_ty) {
            return;
        }
        let Some(leaf_paths) = self.mut_carrier_leaf_paths(dst_ty) else {
            let root = CarrierSlot {
                local: dst,
                fields: Vec::new(),
            };
            let structured_replacements = match expr {
                NExpr::ArrayRepeat { value, .. } => self
                    .sparse_carrier_paths_of_local(value.local, &mut FxHashSet::default())
                    .map(|leaves| {
                        leaves
                            .into_iter()
                            .map(|leaf| {
                                let provenance = self
                                    .target_of_projected_local(
                                        value.local,
                                        &leaf,
                                        &mut FxHashSet::default(),
                                        state,
                                    )
                                    .map(CarrierProvenance::singleton)
                                    .unwrap_or(CarrierProvenance::Unknown);
                                let mut fields = Vec::with_capacity(leaf.len() + 1);
                                fields.push(CarrierProjection::AnyIndex);
                                fields.extend(leaf);
                                (CarrierSlot { local: dst, fields }, provenance)
                            })
                            .collect::<Vec<_>>()
                    }),
                NExpr::AggregateMake { fields: values, .. } if dst_ty.is_array(self.db) => {
                    let per_element = values
                        .iter()
                        .enumerate()
                        .map(|(index, value)| {
                            let leaves = self.sparse_carrier_paths_of_local(
                                value.local,
                                &mut FxHashSet::default(),
                            )?;
                            Some(
                                leaves
                                    .into_iter()
                                    .map(|leaf| {
                                        let provenance = self
                                            .target_of_projected_local(
                                                value.local,
                                                &leaf,
                                                &mut FxHashSet::default(),
                                                state,
                                            )
                                            .map(CarrierProvenance::singleton)
                                            .unwrap_or(CarrierProvenance::Unknown);
                                        (index, leaf, provenance)
                                    })
                                    .collect::<Vec<_>>(),
                            )
                        })
                        .collect::<Option<Vec<_>>>();
                    per_element.map(|per_element| {
                        let all_equal = per_element.first().is_some_and(|first| {
                            per_element.iter().all(|element| {
                                element.len() == first.len()
                                    && element.iter().zip(first).all(
                                        |(
                                            (_, leaf, provenance),
                                            (_, first_leaf, first_provenance),
                                        )| {
                                            leaf == first_leaf && provenance == first_provenance
                                        },
                                    )
                            })
                        });
                        if all_equal {
                            per_element
                                .first()
                                .into_iter()
                                .flatten()
                                .map(|(_, leaf, provenance)| {
                                    let mut fields = Vec::with_capacity(leaf.len() + 1);
                                    fields.push(CarrierProjection::AnyIndex);
                                    fields.extend_from_slice(leaf);
                                    (CarrierSlot { local: dst, fields }, provenance.clone())
                                })
                                .collect()
                        } else {
                            per_element
                                .into_iter()
                                .flatten()
                                .map(|(index, leaf, provenance)| {
                                    let mut fields = Vec::with_capacity(leaf.len() + 1);
                                    fields.push(CarrierProjection::ConstantIndex(index));
                                    fields.extend(leaf);
                                    (CarrierSlot { local: dst, fields }, provenance)
                                })
                                .collect()
                        }
                    })
                }
                NExpr::EnumMake {
                    variant,
                    fields: values,
                    ..
                } => values
                    .iter()
                    .enumerate()
                    .map(|(field, value)| {
                        let leaves = self.sparse_carrier_paths_of_local(
                            value.local,
                            &mut FxHashSet::default(),
                        )?;
                        Some(
                            leaves
                                .into_iter()
                                .map(|leaf| {
                                    let provenance = self
                                        .target_of_projected_local(
                                            value.local,
                                            &leaf,
                                            &mut FxHashSet::default(),
                                            state,
                                        )
                                        .map(CarrierProvenance::singleton)
                                        .unwrap_or(CarrierProvenance::Unknown);
                                    let mut fields = Vec::with_capacity(leaf.len() + 1);
                                    fields.push(CarrierProjection::VariantField {
                                        variant: variant.0,
                                        field,
                                    });
                                    fields.extend(leaf);
                                    (CarrierSlot { local: dst, fields }, provenance)
                                })
                                .collect::<Vec<_>>(),
                        )
                    })
                    .collect::<Option<Vec<_>>>()
                    .map(|fields| fields.into_iter().flatten().collect()),
                NExpr::Use(value) | NExpr::Cast { value, .. } => {
                    self.structured_replacements_from_local(dst, value.local, &[], state)
                }
                NExpr::ReadPlace { place, .. } => self.raw_storage_slot(place).and_then(|source| {
                    self.structured_replacements_from_local(
                        dst,
                        source.local,
                        &source.fields,
                        state,
                    )
                }),
                _ => None,
            };
            if let Some(replacements) = structured_replacements {
                Self::clear_carrier_subtree(state, &root);
                for (destination, provenance) in replacements {
                    self.strong_update_carrier(destination, provenance, state);
                }
                return;
            }
            Self::clear_carrier_subtree(state, &root);
            // A unique array/enum definition can be chased lazily at the
            // exact projection a later use requests. Multi-definition or
            // otherwise opaque assignments have no authoritative static
            // source, so invalidate the whole local.
            if !self
                .single_defs
                .get(&dst)
                .is_some_and(|single| std::ptr::eq(*single, expr))
            {
                Self::invalidate_carrier_subtree(root, state);
            }
            return;
        };

        let replacements = leaf_paths
            .iter()
            .map(|leaf| {
                let target = match expr {
                    NExpr::Use(value) | NExpr::Cast { value, .. } => self
                        .target_of_projected_local(
                            value.local,
                            leaf,
                            &mut FxHashSet::default(),
                            state,
                        ),
                    NExpr::Borrow { place, kind, .. }
                        if *kind == BorrowKind::Mut || !leaf.is_empty() =>
                    {
                        let mut source = place.clone();
                        append_carrier_path(self.db, self.body, &mut source, leaf).and_then(|()| {
                            self.target_of_carrier_source_place(
                                &source,
                                &mut FxHashSet::default(),
                                state,
                            )
                        })
                    }
                    NExpr::ReadPlace { place, .. } => {
                        let mut source = place.clone();
                        append_carrier_path(self.db, self.body, &mut source, leaf).and_then(|()| {
                            self.target_of_carrier_source_place(
                                &source,
                                &mut FxHashSet::default(),
                                state,
                            )
                        })
                    }
                    NExpr::AggregateMake { fields, .. } => leaf
                        .split_first()
                        .and_then(|(field, rest)| match field {
                            CarrierProjection::Field(field)
                            | CarrierProjection::ConstantIndex(field) => {
                                fields.get(*field).map(|value| (value, rest))
                            }
                            CarrierProjection::VariantField { .. }
                            | CarrierProjection::AnyIndex
                            | CarrierProjection::DynamicIndex(_) => None,
                        })
                        .and_then(|(value, rest)| {
                            self.target_of_projected_local(
                                value.local,
                                rest,
                                &mut FxHashSet::default(),
                                state,
                            )
                        }),
                    NExpr::ArrayRepeat { value, .. } => leaf
                        .split_first()
                        .and_then(|(index, rest)| {
                            matches!(
                                index,
                                CarrierProjection::ConstantIndex(_)
                                    | CarrierProjection::AnyIndex
                                    | CarrierProjection::DynamicIndex(_)
                            )
                            .then_some(rest)
                        })
                        .and_then(|rest| {
                            self.target_of_projected_local(
                                value.local,
                                rest,
                                &mut FxHashSet::default(),
                                state,
                            )
                        }),
                    NExpr::EnumMake {
                        variant, fields, ..
                    } => leaf
                        .split_first()
                        .and_then(|(projection, rest)| match projection {
                            CarrierProjection::VariantField {
                                variant: selected,
                                field,
                            } if *selected == variant.0 => {
                                fields.get(*field).map(|value| (value, rest))
                            }
                            _ => None,
                        })
                        .and_then(|(value, rest)| {
                            self.target_of_projected_local(
                                value.local,
                                rest,
                                &mut FxHashSet::default(),
                                state,
                            )
                        }),
                    NExpr::ExtractEnumField {
                        value,
                        variant,
                        field,
                    } => {
                        let mut source = Vec::with_capacity(leaf.len() + 1);
                        source.push(CarrierProjection::VariantField {
                            variant: variant.0,
                            field: usize::from(field.0),
                        });
                        source.extend_from_slice(leaf);
                        self.target_of_projected_local(
                            value.local,
                            &source,
                            &mut FxHashSet::default(),
                            state,
                        )
                    }
                    NExpr::Call {
                        args,
                        effect_args,
                        return_sources,
                        return_sources_complete,
                        ..
                    } => self.target_of_call_result(
                        args,
                        effect_args,
                        NCallReturnSources {
                            sources: return_sources,
                            complete: *return_sources_complete,
                        },
                        leaf,
                        &mut FxHashSet::default(),
                        state,
                    ),
                    _ => None,
                };
                (
                    CarrierSlot {
                        local: dst,
                        fields: leaf.clone(),
                    },
                    target
                        .map(CarrierProvenance::singleton)
                        .unwrap_or(CarrierProvenance::Unknown),
                )
            })
            .collect::<Vec<_>>();

        // Assign replaces the whole local. Sources were computed against the
        // old state above; clear every stale descendant, then keep only
        // differences from the authoritative unique initializer.
        Self::clear_carrier_subtree(
            state,
            &CarrierSlot {
                local: dst,
                fields: Vec::new(),
            },
        );
        for (destination, provenance) in replacements {
            self.strong_update_carrier(destination, provenance, state);
        }
    }

    fn mut_carrier_leaf_paths(&self, ty: TyId<'db>) -> Option<Vec<Vec<CarrierProjection>>> {
        fn collect<'db>(
            db: &'db dyn HirAnalysisDb,
            ty: TyId<'db>,
            prefix: &mut Vec<CarrierProjection>,
            visiting: &mut FxHashSet<TyId<'db>>,
            out: &mut Vec<Vec<CarrierProjection>>,
        ) -> bool {
            if let Some((kind, inner)) = ty.as_capability(db) {
                if kind == CapabilityKind::Mut && !out.contains(prefix) {
                    out.push(prefix.clone());
                }
                return !ty_reaches_mut_borrow(db, inner)
                    || collect(db, inner, prefix, visiting, out);
            }
            if !ty_reaches_mut_borrow(db, ty) {
                return true;
            }
            // Arrays and enums are resolved sparsely from their initializer
            // expression at the exact projection requested. Never enumerate
            // an array from its type-level length here: large const arrays
            // must remain O(body size), and an enum has no active variant
            // without value-level evidence.
            if !visiting.insert(ty)
                || ty.is_array(db)
                || ty
                    .adt_def(db)
                    .is_some_and(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))
            {
                return false;
            }
            let fields = ty.field_types(db);
            if fields.is_empty() {
                visiting.remove(&ty);
                return false;
            }
            let complete = fields.into_iter().enumerate().all(|(field, field_ty)| {
                prefix.push(CarrierProjection::Field(field));
                let complete = collect(db, field_ty, prefix, visiting, out);
                prefix.pop();
                complete
            });
            visiting.remove(&ty);
            complete
        }

        let mut out = Vec::new();
        collect(
            self.db,
            ty,
            &mut Vec::new(),
            &mut FxHashSet::default(),
            &mut out,
        )
        .then_some(out)
    }

    /// Discovers mutable carrier leaves from value-level constructor shape.
    /// Array repeats contribute one symbolic index regardless of their
    /// type-level length; explicit aggregates scale only with source operands.
    fn sparse_carrier_paths_of_local(
        &self,
        local: SLocalId,
        visiting: &mut FxHashSet<SLocalId>,
    ) -> Option<Vec<Vec<CarrierProjection>>> {
        if !visiting.insert(local) {
            return None;
        }
        let result = if let Some(local_data) = self.body.local(local)
            && let Some(leaves) = self.mut_carrier_leaf_paths(local_data.ty)
        {
            Some(leaves)
        } else {
            self.single_defs
                .get(&local)
                .and_then(|expr| self.sparse_carrier_paths_of_expr(expr, visiting))
        };
        visiting.remove(&local);
        result
    }

    fn sparse_carrier_paths_of_expr(
        &self,
        expr: &NExpr<'db>,
        visiting: &mut FxHashSet<SLocalId>,
    ) -> Option<Vec<Vec<CarrierProjection>>> {
        match expr {
            NExpr::Use(value) | NExpr::Cast { value, .. } => {
                self.sparse_carrier_paths_of_local(value.local, visiting)
            }
            NExpr::ReadPlace { place, .. } => {
                let source = self.raw_storage_slot(place)?;
                let paths = self.sparse_carrier_paths_of_local(source.local, visiting)?;
                if source.fields.is_empty() {
                    Some(paths)
                } else {
                    paths
                        .into_iter()
                        .map(|path| {
                            carrier_pattern_is_prefix(&source.fields, &path)
                                .then(|| path[source.fields.len()..].to_vec())
                        })
                        .collect()
                }
            }
            NExpr::ArrayRepeat { value, .. } => {
                let paths = self.sparse_carrier_paths_of_local(value.local, visiting)?;
                Some(
                    paths
                        .into_iter()
                        .map(|path| {
                            let mut result = Vec::with_capacity(path.len() + 1);
                            result.push(CarrierProjection::AnyIndex);
                            result.extend(path);
                            result
                        })
                        .collect(),
                )
            }
            NExpr::AggregateMake { ty, fields } => {
                let is_array = ty.is_array(self.db);
                fields
                    .iter()
                    .enumerate()
                    .map(|(field, value)| {
                        let paths = self.sparse_carrier_paths_of_local(value.local, visiting)?;
                        Some(paths.into_iter().map(move |path| {
                            let mut result = Vec::with_capacity(path.len() + 1);
                            result.push(if is_array {
                                CarrierProjection::ConstantIndex(field)
                            } else {
                                CarrierProjection::Field(field)
                            });
                            result.extend(path);
                            result
                        }))
                    })
                    .collect::<Option<Vec<_>>>()
                    .map(|paths| paths.into_iter().flatten().collect())
            }
            NExpr::EnumMake {
                variant, fields, ..
            } => fields
                .iter()
                .enumerate()
                .map(|(field, value)| {
                    let paths = self.sparse_carrier_paths_of_local(value.local, visiting)?;
                    Some(paths.into_iter().map(move |path| {
                        let mut result = Vec::with_capacity(path.len() + 1);
                        result.push(CarrierProjection::VariantField {
                            variant: variant.0,
                            field,
                        });
                        result.extend(path);
                        result
                    }))
                })
                .collect::<Option<Vec<_>>>()
                .map(|paths| paths.into_iter().flatten().collect()),
            NExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => {
                let prefix = CarrierProjection::VariantField {
                    variant: variant.0,
                    field: usize::from(field.0),
                };
                self.sparse_carrier_paths_of_local(value.local, visiting)?
                    .into_iter()
                    .map(|path| (path.first() == Some(&prefix)).then(|| path[1..].to_vec()))
                    .collect()
            }
            _ => None,
        }
    }

    fn structured_replacements_from_local(
        &self,
        destination: SLocalId,
        source: SLocalId,
        source_prefix: &[CarrierProjection],
        state: &MustAssignState<'db>,
    ) -> Option<Vec<(CarrierSlot, CarrierProvenance<'db>)>> {
        let replacements = self
            .sparse_carrier_paths_of_local(source, &mut FxHashSet::default())?
            .into_iter()
            .map(|source_path| {
                if !carrier_pattern_is_prefix(source_prefix, &source_path) {
                    return None;
                }
                let destination_path = source_path[source_prefix.len()..].to_vec();
                let provenance = self
                    .target_of_projected_local(
                        source,
                        &source_path,
                        &mut FxHashSet::default(),
                        state,
                    )
                    .map(CarrierProvenance::singleton)
                    .unwrap_or(CarrierProvenance::Unknown);
                Some((
                    CarrierSlot {
                        local: destination,
                        fields: destination_path,
                    },
                    provenance,
                ))
            })
            .collect::<Option<Vec<_>>>()?;
        Some(self.compact_uniform_array_replacements(replacements))
    }

    fn compact_uniform_array_replacements(
        &self,
        mut replacements: Vec<(CarrierSlot, CarrierProvenance<'db>)>,
    ) -> Vec<(CarrierSlot, CarrierProvenance<'db>)> {
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        enum UniformProvenanceKey<'db> {
            Unknown,
            Singleton(AssignedTarget<'db>),
        }

        let max_depth = replacements
            .iter()
            .map(|(slot, _)| slot.fields.len())
            .max()
            .unwrap_or_default();
        // Compact inner arrays first. Their AnyIndex paths can then be grouped
        // at the next outer array level in the same deepest-to-shallowest pass.
        for projection_idx in (0..max_depth).rev() {
            let mut groups: FxHashMap<
                (CarrierSlot, UniformProvenanceKey<'db>),
                Vec<(usize, usize)>,
            > = FxHashMap::default();
            for (replacement_idx, (slot, provenance)) in replacements.iter().enumerate() {
                let Some(CarrierProjection::ConstantIndex(index)) = slot.fields.get(projection_idx)
                else {
                    continue;
                };
                let provenance = match provenance {
                    CarrierProvenance::Unknown => UniformProvenanceKey::Unknown,
                    CarrierProvenance::Known(targets) if targets.len() == 1 => {
                        UniformProvenanceKey::Singleton(
                            targets.iter().next().expect("singleton provenance").clone(),
                        )
                    }
                    // A joined multi-target provenance is not uniform enough
                    // to canonicalize across array elements.
                    CarrierProvenance::Known(_) => continue,
                };
                let mut wildcard = slot.clone();
                wildcard.fields[projection_idx] = CarrierProjection::AnyIndex;
                groups
                    .entry((wildcard, provenance))
                    .or_default()
                    .push((replacement_idx, *index));
            }

            let mut candidates = Vec::new();
            let mut wildcard_counts: FxHashMap<CarrierSlot, usize> = FxHashMap::default();
            for ((wildcard, _), mut members) in groups {
                let Some(expected_len) =
                    self.projected_array_len(wildcard.local, &wildcard.fields[..projection_idx])
                else {
                    continue;
                };
                members.sort_by_key(|(_, index)| *index);
                if members.len() != expected_len
                    || !members
                        .iter()
                        .enumerate()
                        .all(|(expected, (_, actual))| expected == *actual)
                {
                    continue;
                }
                *wildcard_counts.entry(wildcard.clone()).or_default() += 1;
                candidates.push((wildcard, members));
            }

            let mut removed = vec![false; replacements.len()];
            let mut additions = Vec::new();
            for (wildcard, members) in candidates {
                // Duplicate full groups with different provenance should not
                // arise from a constructor, but declining to compact keeps a
                // malformed sparse source conservative.
                if wildcard_counts.get(&wildcard) != Some(&1) {
                    continue;
                }
                let provenance = replacements[members[0].0].1.clone();
                for (replacement_idx, _) in members {
                    removed[replacement_idx] = true;
                }
                additions.push((wildcard, provenance));
            }
            if !additions.is_empty() {
                replacements = replacements
                    .into_iter()
                    .enumerate()
                    .filter_map(|(idx, replacement)| (!removed[idx]).then_some(replacement))
                    .chain(additions)
                    .collect();
            }
        }
        replacements
    }

    fn projected_array_len(&self, local: SLocalId, prefix: &[CarrierProjection]) -> Option<usize> {
        let root_ty = self.body.local(local)?.ty;
        let mut path = NSProjectionPath::default();
        append_carrier_path_to_projection(self.db, root_ty, &mut path, prefix)?;
        let mut ty = semantic_projection_ty(self.db, root_ty, &path)?.0;
        while let Some((_, inner)) = ty.as_capability(self.db) {
            ty = inner;
        }
        ty.array_len(self.db)
    }

    fn raw_storage_slot(&self, place: &NSPlace<'db>) -> Option<CarrierSlot> {
        let mut fields = carrier_path_from_semantic(&place.path)?;
        canonicalize_dynamic_indices(self.db, self.body, &self.single_defs, &mut fields);
        let local = match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => local,
            NSPlaceRoot::Root(root) => match self.body.root(root)? {
                NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => *local,
                NBorrowRoot::Provider { .. } => self.provider_root_locals.get(&root).copied()?,
            },
        };
        Some(CarrierSlot { local, fields })
    }

    fn clear_carrier_subtree(state: &mut MustAssignState<'db>, destination: &CarrierSlot) {
        state.carrier_overrides.retain(|slot, _| {
            slot.local != destination.local
                || !carrier_pattern_is_prefix(&destination.fields, &slot.fields)
        });
    }

    fn effective_carrier_provenance(
        &self,
        slot: &CarrierSlot,
        state: &MustAssignState<'db>,
    ) -> CarrierProvenance<'db> {
        self.target_of_projected_local(slot.local, &slot.fields, &mut FxHashSet::default(), state)
            .map(CarrierProvenance::singleton)
            .unwrap_or(CarrierProvenance::Unknown)
    }

    /// Strong update relative to the unique static initializer. No-op
    /// rebinds stay absent from the sparse override map, which makes absence
    /// at a CFG join mean "unchanged" instead of "unknown".
    fn strong_update_carrier(
        &self,
        destination: CarrierSlot,
        provenance: CarrierProvenance<'db>,
        state: &mut MustAssignState<'db>,
    ) {
        if self.effective_carrier_provenance(&destination, state) == provenance {
            return;
        }
        Self::clear_carrier_subtree(state, &destination);
        state.carrier_overrides.retain(|slot, _| {
            slot.local != destination.local
                || !carrier_paths_may_alias(&slot.fields, &destination.fields)
                || !slot
                    .fields
                    .iter()
                    .any(|step| matches!(step, CarrierProjection::DynamicIndex(_)))
        });
        if self.effective_carrier_provenance(&destination, state) != provenance {
            state.carrier_overrides.insert(destination, provenance);
        }
    }

    /// Invalidating a structured prefix must remove every more-specific
    /// override first; otherwise a stale descendant would shadow Unknown.
    fn invalidate_carrier_subtree(destination: CarrierSlot, state: &mut MustAssignState<'db>) {
        Self::clear_carrier_subtree(state, &destination);
        state
            .carrier_overrides
            .insert(destination, CarrierProvenance::Unknown);
    }

    /// Strongly updates mutable-capability leaves replaced by a Store.
    /// Payload writes have a non-carrier source and leave provenance intact.
    fn apply_carrier_store(
        &self,
        dst: &NSPlace<'db>,
        src: super::borrowck::NOperand,
        state: &mut MustAssignState<'db>,
    ) {
        let Some(dst_ty) = self.body.place_ty(self.db, dst) else {
            return;
        };
        let Some(src_ty) = self.body.local(src.local).map(|local| local.ty) else {
            return;
        };
        if !ty_reaches_mut_borrow(self.db, dst_ty) || !ty_reaches_mut_borrow(self.db, src_ty) {
            return;
        }
        let raw_slot = self.raw_storage_slot(dst);
        let has_structured_selection = raw_slot.as_ref().is_some_and(|slot| {
            slot.fields.iter().any(|step| {
                matches!(
                    step,
                    CarrierProjection::VariantField { .. }
                        | CarrierProjection::ConstantIndex(_)
                        | CarrierProjection::AnyIndex
                        | CarrierProjection::DynamicIndex(_)
                )
            })
        });
        let destination = if has_structured_selection {
            raw_slot
        } else {
            storage_exposed_local(
                self.db,
                self.body,
                &self.single_defs,
                &self.provider_root_locals,
                dst,
                &mut FxHashSet::default(),
            )
            .map(|(local, fields)| CarrierSlot { local, fields })
            .or(raw_slot)
        };
        let Some(destination) = destination else {
            return;
        };
        let dynamic_wildcard = |slot: &CarrierSlot| {
            slot.fields
                .iter()
                .any(|step| matches!(step, CarrierProjection::DynamicIndex(_)))
                .then(|| CarrierSlot {
                    local: slot.local,
                    fields: slot
                        .fields
                        .iter()
                        .map(|step| match step {
                            CarrierProjection::DynamicIndex(_) => CarrierProjection::AnyIndex,
                            step => *step,
                        })
                        .collect(),
                })
        };
        let Some(leaf_paths) = self.mut_carrier_leaf_paths(dst_ty) else {
            if let Some(paths) =
                self.sparse_carrier_paths_of_local(src.local, &mut FxHashSet::default())
            {
                let replacements = paths
                    .into_iter()
                    .map(|path| {
                        let provenance = self
                            .target_of_projected_local(
                                src.local,
                                &path,
                                &mut FxHashSet::default(),
                                state,
                            )
                            .map(CarrierProvenance::singleton)
                            .unwrap_or(CarrierProvenance::Unknown);
                        let mut fields = destination.fields.clone();
                        fields.extend(path);
                        (
                            CarrierSlot {
                                local: destination.local,
                                fields,
                            },
                            provenance,
                        )
                    })
                    .collect::<Vec<_>>();
                Self::clear_carrier_subtree(state, &destination);
                for (destination, provenance) in replacements {
                    if let Some(wildcard) = dynamic_wildcard(&destination) {
                        Self::invalidate_carrier_subtree(wildcard, state);
                    }
                    self.strong_update_carrier(destination, provenance, state);
                }
                return;
            }
            Self::invalidate_carrier_subtree(
                dynamic_wildcard(&destination).unwrap_or(destination),
                state,
            );
            return;
        };

        let replacements = leaf_paths
            .iter()
            .map(|leaf| {
                let provenance = self
                    .target_of_projected_local(src.local, leaf, &mut FxHashSet::default(), state)
                    .map(CarrierProvenance::singleton)
                    .unwrap_or(CarrierProvenance::Unknown);
                let mut fields = destination.fields.clone();
                fields.extend(leaf);
                (
                    CarrierSlot {
                        local: destination.local,
                        fields,
                    },
                    provenance,
                )
            })
            .collect::<Vec<_>>();

        for (destination, provenance) in replacements {
            if let Some(wildcard) = dynamic_wildcard(&destination) {
                Self::invalidate_carrier_subtree(wildcard, state);
            }
            self.strong_update_carrier(destination, provenance, state);
        }
    }

    /// Resolves a whole-value store destination to a caller-visible target,
    /// looking through capability params and single-borrow local carriers.
    fn write_target_of_place(
        &self,
        place: &NSPlace<'db>,
        state: &MustAssignState<'db>,
    ) -> Option<AssignedTarget<'db>> {
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
                        fields: Vec::new(),
                    }),
                    _ => None,
                },
                NBorrowRoot::Param { .. } | NBorrowRoot::LocalSlot { .. } => None,
            },
            NSPlaceRoot::CarrierDerefLocal(local) => self.target_of_carrier_local(*local, state),
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
            let Some(LocalBinding::Param {
                site: ParamSite::Closure(def),
                idx,
                mode: FuncParamMode::View,
                ty,
                ..
            }) = self.body.local(local)?.source
            else {
                return None;
            };
            return matches!(ty.as_capability(self.db), Some((CapabilityKind::Mut, _))).then(
                || AssignedTarget::ClosureArgument {
                    def,
                    param_idx: idx as u32,
                    fields: Vec::new(),
                },
            );
        };
        matches!(ty.as_capability(self.db), Some((CapabilityKind::Mut, _))).then(|| {
            AssignedTarget::FuncParam {
                func,
                param_idx: idx as u32,
                fields: Vec::new(),
            }
        })
    }

    fn target_of_carrier_local(
        &self,
        local: SLocalId,
        state: &MustAssignState<'db>,
    ) -> Option<AssignedTarget<'db>> {
        self.target_of_projected_local(local, &[], &mut FxHashSet::default(), state)
    }

    /// `None` means no flow-sensitive override exists and static initializer
    /// provenance remains valid. `Some(None)` means the reaching provenance
    /// is unknown or has multiple possible targets.
    fn carrier_override_target(
        &self,
        state: &MustAssignState<'db>,
        local: SLocalId,
        fields: &[CarrierProjection],
    ) -> Option<Option<AssignedTarget<'db>>> {
        let exact = CarrierSlot {
            local,
            fields: fields.to_vec(),
        };
        if let Some(provenance) = state.carrier_overrides.get(&exact) {
            return Some(provenance.definite_target());
        }

        if let Some(provenance) =
            state
                .carrier_overrides
                .iter()
                .find_map(|(candidate, provenance)| {
                    (candidate.local == local
                        && candidate.fields.len() == fields.len()
                        && candidate
                            .fields
                            .iter()
                            .zip(fields)
                            .all(|(pattern, actual)| carrier_pattern_matches(pattern, actual)))
                    .then_some(provenance)
                })
        {
            return Some(provenance.definite_target());
        }

        // A dynamic/Any lookup can alias a sparse exact update even when the
        // selector identity differs. In that case no one target is definite.
        if fields.iter().any(|step| {
            matches!(
                step,
                CarrierProjection::AnyIndex | CarrierProjection::DynamicIndex(_)
            )
        }) && state.carrier_overrides.keys().any(|candidate| {
            candidate.local == local && carrier_paths_may_alias(&candidate.fields, fields)
        }) {
            return Some(None);
        }

        for prefix_len in (0..fields.len()).rev() {
            let slot = CarrierSlot {
                local,
                fields: fields[..prefix_len].to_vec(),
            };
            let provenance =
                state.carrier_overrides.get(&slot).or_else(|| {
                    state
                        .carrier_overrides
                        .iter()
                        .find_map(|(candidate, value)| {
                            (candidate.local == local
                                && candidate.fields.len() == prefix_len
                                && candidate.fields.iter().zip(&fields[..prefix_len]).all(
                                    |(pattern, actual)| carrier_pattern_matches(pattern, actual),
                                ))
                            .then_some(value)
                        })
                });
            if let Some(provenance) = provenance {
                let _ = provenance;
                return Some(None);
            }
        }
        None
    }

    /// Resolves a capability value nested under a statically known field path.
    ///
    /// Closure receivers are usually passed as a borrow of a local aggregate,
    /// while consuming calls pass the aggregate directly. Chasing both shapes
    /// through their unique definitions gives closure summaries one common,
    /// caller-relative target model. Multi-definition locals and non-field
    /// projections deliberately receive no credit.
    fn target_of_projected_local(
        &self,
        local: SLocalId,
        fields: &[CarrierProjection],
        visiting: &mut FxHashSet<(SLocalId, Vec<CarrierProjection>)>,
        state: &MustAssignState<'db>,
    ) -> Option<AssignedTarget<'db>> {
        if let Some(target) = self.carrier_override_target(state, local, fields) {
            return target;
        }
        let local_data = self.body.local(local)?;
        let mut selected_path = NSProjectionPath::default();
        append_carrier_path_to_projection(self.db, local_data.ty, &mut selected_path, fields)?;
        let selected_ty = semantic_projection_ty(self.db, local_data.ty, &selected_path)?.0;
        if !matches!(
            selected_ty.as_capability(self.db),
            Some((CapabilityKind::Mut, _))
        ) {
            return None;
        }

        if !visiting.insert((local, fields.to_vec())) {
            return None;
        }

        if fields.is_empty()
            && let Some(target) = self.target_of_param_carrier(local)
        {
            return Some(target);
        }

        if let Some(expr) = self.single_defs.get(&local) {
            let target = match expr {
                NExpr::Use(value) | NExpr::Cast { value, .. } => {
                    self.target_of_projected_local(value.local, fields, visiting, state)
                }
                NExpr::Borrow { place, kind, .. }
                    if *kind == BorrowKind::Mut || !fields.is_empty() =>
                {
                    self.target_of_source_place_with_projection(place, fields, visiting, state)
                }
                NExpr::ReadPlace { place, .. } => {
                    self.target_of_source_place_with_projection(place, fields, visiting, state)
                }
                NExpr::AggregateMake { fields: values, .. } => {
                    let (projection, rest) = fields.split_first()?;
                    match projection {
                        CarrierProjection::Field(field)
                        | CarrierProjection::ConstantIndex(field) => self
                            .target_of_projected_local(
                                values.get(*field)?.local,
                                rest,
                                visiting,
                                state,
                            ),
                        CarrierProjection::AnyIndex | CarrierProjection::DynamicIndex(_) => {
                            let mut targets = values.iter().map(|value| {
                                let mut branch_visiting = visiting.clone();
                                self.target_of_projected_local(
                                    value.local,
                                    rest,
                                    &mut branch_visiting,
                                    state,
                                )
                            });
                            let first = targets.next()??;
                            targets
                                .all(|target| target.as_ref() == Some(&first))
                                .then_some(first)
                        }
                        CarrierProjection::VariantField { .. } => None,
                    }
                }
                NExpr::ArrayRepeat { value, .. } => {
                    let (projection, rest) = fields.split_first()?;
                    if !matches!(
                        projection,
                        CarrierProjection::ConstantIndex(_)
                            | CarrierProjection::AnyIndex
                            | CarrierProjection::DynamicIndex(_)
                    ) {
                        return None;
                    }
                    self.target_of_projected_local(value.local, rest, visiting, state)
                }
                NExpr::EnumMake {
                    variant,
                    fields: values,
                    ..
                } => {
                    let (projection, rest) = fields.split_first()?;
                    let CarrierProjection::VariantField {
                        variant: selected,
                        field,
                    } = projection
                    else {
                        return None;
                    };
                    if *selected != variant.0 {
                        return None;
                    }
                    self.target_of_projected_local(values.get(*field)?.local, rest, visiting, state)
                }
                NExpr::ExtractEnumField {
                    value,
                    variant,
                    field,
                } => {
                    let mut source = Vec::with_capacity(fields.len() + 1);
                    source.push(CarrierProjection::VariantField {
                        variant: variant.0,
                        field: usize::from(field.0),
                    });
                    source.extend_from_slice(fields);
                    self.target_of_projected_local(value.local, &source, visiting, state)
                }
                NExpr::Call {
                    args,
                    effect_args,
                    return_sources,
                    return_sources_complete,
                    ..
                } => self.target_of_call_result(
                    args,
                    effect_args,
                    NCallReturnSources {
                        sources: return_sources,
                        complete: *return_sources_complete,
                    },
                    fields,
                    visiting,
                    state,
                ),
                _ => None,
            };
            // A unique definition is authoritative. Falling through to the
            // local's backing root after a structured initializer failed to
            // resolve could silently reinterpret a dynamic index as element
            // zero or an inactive enum variant.
            return target;
        }

        let root = self.body.local(local)?.lowering.root()?;
        let place = NSPlace {
            root: NSPlaceRoot::Root(root),
            path: NSProjectionPath::default(),
        };
        self.target_of_source_place_with_projection(&place, fields, visiting, state)
    }

    fn target_of_source_place_with_projection(
        &self,
        place: &NSPlace<'db>,
        fields: &[CarrierProjection],
        visiting: &mut FxHashSet<(SLocalId, Vec<CarrierProjection>)>,
        state: &MustAssignState<'db>,
    ) -> Option<AssignedTarget<'db>> {
        if fields
            .iter()
            .any(|step| matches!(step, CarrierProjection::AnyIndex))
        {
            // `AnyIndex` is a summary wildcard, not concrete element zero.
            // Keep it in the carrier model while forwarding through a place
            // so explicit caller aggregates must resolve unanimously.
            let source = self.raw_storage_slot(place)?;
            let mut source_fields = source.fields;
            source_fields.extend_from_slice(fields);
            return self.target_of_projected_local(source.local, &source_fields, visiting, state);
        }

        let mut place = place.clone();
        append_carrier_path(self.db, self.body, &mut place, fields)?;
        self.target_of_carrier_source_place(&place, visiting, state)
    }

    fn field_return_projection(
        projection: &[SCallReturnProjectionStep],
    ) -> Option<Vec<CarrierProjection>> {
        projection
            .iter()
            .map(|step| match step {
                SCallReturnProjectionStep::Field(field) => {
                    Some(CarrierProjection::Field(usize::from(*field)))
                }
                SCallReturnProjectionStep::VariantField { variant, field } => {
                    Some(CarrierProjection::VariantField {
                        variant: *variant,
                        field: usize::from(*field),
                    })
                }
                SCallReturnProjectionStep::ConstantIndex(index) => {
                    Some(CarrierProjection::ConstantIndex(*index))
                }
                // Analysis normalization has already rewritten this to the
                // caller-local selector snapshot recorded at the call site.
                SCallReturnProjectionStep::DynamicIndex(index) => {
                    Some(CarrierProjection::DynamicIndex(*index))
                }
                SCallReturnProjectionStep::AnyIndex => Some(CarrierProjection::AnyIndex),
            })
            .collect()
    }

    /// Maps a projected call result back through the callee's exact forwarded
    /// return sources. All applicable sources must resolve to the same target;
    /// fresh, unknown, mixed, indexed, and enum projections receive no credit.
    fn target_of_call_result(
        &self,
        args: &[super::borrowck::NOperand],
        effect_args: &[NEffectArg<'db>],
        return_sources: NCallReturnSources<'_>,
        fields: &[CarrierProjection],
        visiting: &mut FxHashSet<(SLocalId, Vec<CarrierProjection>)>,
        state: &MustAssignState<'db>,
    ) -> Option<AssignedTarget<'db>> {
        let NCallReturnSources { sources, complete } = return_sources;
        if !complete {
            return None;
        }
        let mut targets = Vec::new();
        for source in sources {
            let mut result_projection = Self::field_return_projection(&source.result_projection)?;
            canonicalize_dynamic_indices(
                self.db,
                self.body,
                &self.single_defs,
                &mut result_projection,
            );
            if !carrier_pattern_is_prefix(&result_projection, fields) {
                continue;
            }
            let mut input_projection = Self::field_return_projection(&source.projection)?;
            canonicalize_dynamic_indices(
                self.db,
                self.body,
                &self.single_defs,
                &mut input_projection,
            );
            input_projection.extend_from_slice(&fields[result_projection.len()..]);
            let mut branch_visiting = visiting.clone();

            let target = match source.origin {
                CallableInputLayoutHoleOrigin::Receiver => args.first().and_then(|arg| {
                    self.target_of_projected_local(
                        arg.local,
                        &input_projection,
                        &mut branch_visiting,
                        state,
                    )
                }),
                CallableInputLayoutHoleOrigin::ValueParam(param) => {
                    args.get(param).and_then(|arg| {
                        self.target_of_projected_local(
                            arg.local,
                            &input_projection,
                            &mut branch_visiting,
                            state,
                        )
                    })
                }
                CallableInputLayoutHoleOrigin::Effect(requirement_idx) => effect_args
                    .iter()
                    .find(|arg| arg.binding_idx as usize == requirement_idx)
                    .and_then(|arg| match &arg.arg {
                        NEffectArgValue::Value(value) => self.target_of_projected_local(
                            value.local,
                            &input_projection,
                            &mut branch_visiting,
                            state,
                        ),
                        NEffectArgValue::Place(place) => {
                            let mut place = place.clone();
                            append_carrier_path(self.db, self.body, &mut place, &input_projection)?;
                            self.target_of_carrier_source_place(&place, &mut branch_visiting, state)
                        }
                    }),
            }?;
            targets.push(target);
        }

        let first = targets.first()?.clone();
        targets
            .iter()
            .all(|target| *target == first)
            .then_some(first)
    }

    /// Interprets `place` as the storage location of a capability carrier,
    /// not as the carrier's referent. The returned target therefore describes
    /// what a later `CarrierDerefLocal` store initializes.
    fn target_of_carrier_source_place(
        &self,
        place: &NSPlace<'db>,
        visiting: &mut FxHashSet<(SLocalId, Vec<CarrierProjection>)>,
        state: &MustAssignState<'db>,
    ) -> Option<AssignedTarget<'db>> {
        let mut fields = carrier_path_from_semantic(&place.path)?;
        canonicalize_dynamic_indices(self.db, self.body, &self.single_defs, &mut fields);
        // A callee-local runtime selector has no stable identity at the
        // assignment-summary boundary. `AnyIndex` preserves the safe fact
        // that some element is written; caller mapping credits it only when
        // every possible element resolves to the same target. Rebind
        // summaries deliberately do not use this abstraction.
        let summary_fields = fields
            .iter()
            .map(|step| match step {
                CarrierProjection::DynamicIndex(_) => CarrierProjection::AnyIndex,
                step => *step,
            })
            .collect::<Vec<_>>();

        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) if !fields.is_empty() => {
                if let Some(target) = self.carrier_override_target(state, local, &fields) {
                    return target;
                }
                match self.body.local(local)?.source? {
                    LocalBinding::Param {
                        site: ParamSite::Func(func),
                        idx,
                        ..
                    } => Some(AssignedTarget::FuncParam {
                        func,
                        param_idx: idx as u32,
                        fields: summary_fields,
                    }),
                    LocalBinding::Param {
                        site: ParamSite::ClosureEnv(def),
                        ..
                    } => {
                        let (CarrierProjection::Field(capture_idx), rest) =
                            summary_fields.split_first()?
                        else {
                            return None;
                        };
                        Some(AssignedTarget::ClosureCapture {
                            def,
                            capture_idx: *capture_idx as u32,
                            fields: rest.to_vec(),
                        })
                    }
                    LocalBinding::Param {
                        site: ParamSite::ClosureArgs(def),
                        ..
                    } => {
                        let (CarrierProjection::Field(logical_idx), rest) =
                            summary_fields.split_first()?
                        else {
                            return None;
                        };
                        Some(AssignedTarget::ClosureArgument {
                            def,
                            param_idx: *logical_idx as u32,
                            fields: rest.to_vec(),
                        })
                    }
                    _ => None,
                }
            }
            NSPlaceRoot::CarrierDerefLocal(local) if fields.is_empty() => {
                self.target_of_projected_local(local, &[], visiting, state)
            }
            NSPlaceRoot::CarrierDerefLocal(_) => None,
            NSPlaceRoot::Root(root) => match self.body.root(root)? {
                NBorrowRoot::Provider { binding, .. } => {
                    if let Some((local, slot_fields)) = storage_exposed_local(
                        self.db,
                        self.body,
                        &self.single_defs,
                        &self.provider_root_locals,
                        place,
                        &mut FxHashSet::default(),
                    ) && let Some(target) =
                        self.carrier_override_target(state, local, &slot_fields)
                    {
                        return target;
                    }
                    match binding.source {
                        ProviderSource::ContractField { field } if fields.is_empty() => {
                            Some(AssignedTarget::ContractField(field))
                        }
                        ProviderSource::UsesParam {
                            site: EffectParamSite::Func(func),
                            requirement_idx,
                        } => Some(AssignedTarget::FuncEffect {
                            func,
                            requirement_idx,
                            fields: summary_fields,
                        }),
                        _ => None,
                    }
                }
                NBorrowRoot::LocalSlot { local } => {
                    self.target_of_projected_local(*local, &fields, visiting, state)
                }
                NBorrowRoot::Param { local, param_idx } => {
                    if let Some(target) = self.carrier_override_target(state, *local, &fields) {
                        return target;
                    }
                    let source = self.body.local(*local)?.source?;
                    match source {
                        LocalBinding::Param {
                            site: ParamSite::Func(func),
                            idx,
                            ..
                        } if idx == *param_idx as usize => Some(AssignedTarget::FuncParam {
                            func,
                            param_idx: *param_idx,
                            fields: summary_fields,
                        }),
                        LocalBinding::Param {
                            site: ParamSite::ClosureEnv(def),
                            ..
                        } if *param_idx == 0 => {
                            let (CarrierProjection::Field(capture_idx), rest) =
                                summary_fields.split_first()?
                            else {
                                return None;
                            };
                            Some(AssignedTarget::ClosureCapture {
                                def,
                                capture_idx: *capture_idx as u32,
                                fields: rest.to_vec(),
                            })
                        }
                        LocalBinding::Param {
                            site: ParamSite::ClosureArgs(def),
                            ..
                        } if *param_idx as usize
                            == crate::analysis::ty::ty_check::CLOSURE_ARGS_PARAM_IDX =>
                        {
                            let (CarrierProjection::Field(logical_idx), rest) =
                                summary_fields.split_first()?
                            else {
                                return None;
                            };
                            Some(AssignedTarget::ClosureArgument {
                                def,
                                param_idx: *logical_idx as u32,
                                fields: rest.to_vec(),
                            })
                        }
                        _ => None,
                    }
                }
            },
        }
    }

    fn abstract_target_of_slot(&self, slot: &CarrierSlot) -> Option<AssignedTarget<'db>> {
        if slot.fields.iter().any(|step| {
            matches!(
                step,
                CarrierProjection::AnyIndex | CarrierProjection::DynamicIndex(_)
            )
        }) {
            return None;
        }
        match self.body.local(slot.local)?.source? {
            LocalBinding::Param {
                site: ParamSite::Func(func),
                idx,
                ..
            } => Some(AssignedTarget::FuncParam {
                func,
                param_idx: idx as u32,
                fields: slot.fields.clone(),
            }),
            LocalBinding::Param {
                site: ParamSite::Closure(def),
                idx,
                ..
            } => Some(AssignedTarget::ClosureArgument {
                def,
                param_idx: idx as u32,
                fields: slot.fields.clone(),
            }),
            LocalBinding::Param {
                site: ParamSite::ClosureEnv(def),
                ..
            } => {
                let (CarrierProjection::Field(capture_idx), rest) = slot.fields.split_first()?
                else {
                    return None;
                };
                Some(AssignedTarget::ClosureCapture {
                    def,
                    capture_idx: *capture_idx as u32,
                    fields: rest.to_vec(),
                })
            }
            LocalBinding::Param {
                site: ParamSite::ClosureArgs(def),
                ..
            } => {
                let (CarrierProjection::Field(param_idx), rest) = slot.fields.split_first()? else {
                    return None;
                };
                Some(AssignedTarget::ClosureArgument {
                    def,
                    param_idx: *param_idx as u32,
                    fields: rest.to_vec(),
                })
            }
            LocalBinding::EffectParam {
                site: EffectParamSite::Func(func),
                idx,
                ..
            }
            | LocalBinding::Param {
                site: ParamSite::EffectField(EffectParamSite::Func(func)),
                idx,
                ..
            } => Some(AssignedTarget::FuncEffect {
                func,
                requirement_idx: idx as u32,
                fields: slot.fields.clone(),
            }),
            LocalBinding::Local { .. }
            | LocalBinding::Param {
                site:
                    ParamSite::ContractInit(_)
                    | ParamSite::EffectField(
                        EffectParamSite::Contract(_)
                        | EffectParamSite::ContractInit { .. }
                        | EffectParamSite::ContractRecvArm { .. },
                    ),
                ..
            }
            | LocalBinding::EffectParam {
                site:
                    EffectParamSite::Contract(_)
                    | EffectParamSite::ContractInit { .. }
                    | EffectParamSite::ContractRecvArm { .. },
                ..
            } => None,
        }
    }

    /// Rebinding a carrier stored in an owned/by-value aggregate is local to
    /// the callee. It becomes caller-visible only after crossing an existing
    /// mutable-capability boundary (or through a mutable effect place).
    fn slot_rebind_is_caller_visible(&self, slot: &CarrierSlot) -> bool {
        let Some(local) = self.body.local(slot.local) else {
            return false;
        };
        if matches!(
            local.source,
            Some(
                LocalBinding::EffectParam { is_mut: true, .. }
                    | LocalBinding::Param {
                        site: ParamSite::EffectField(_),
                        ..
                    }
            )
        ) {
            return true;
        }

        (0..slot.fields.len()).any(|prefix_len| {
            let mut path = NSProjectionPath::default();
            append_carrier_path_to_projection(
                self.db,
                local.ty,
                &mut path,
                &slot.fields[..prefix_len],
            )
            .is_some()
                && semantic_projection_ty(self.db, local.ty, &path).is_some_and(|(ty, _)| {
                    matches!(ty.as_capability(self.db), Some((CapabilityKind::Mut, _)))
                })
        })
    }

    fn summary_rebinds(&self, state: &MustAssignState<'db>) -> (Vec<CarrierRebind<'db>>, bool) {
        let mut rebinds = Vec::new();
        let mut complete = true;
        for (slot, provenance) in &state.carrier_overrides {
            if !self.slot_rebind_is_caller_visible(slot) {
                continue;
            }
            let Some(destination) = self.abstract_target_of_slot(slot) else {
                complete = false;
                continue;
            };
            let source = provenance.definite_target();
            if source.as_ref() != Some(&destination) {
                rebinds.push(CarrierRebind {
                    destination,
                    source,
                });
            }
        }
        (rebinds, complete)
    }

    fn map_callee_target(
        &self,
        target: &AssignedTarget<'db>,
        callee_owner: BodyOwner<'db>,
        args: &[super::borrowck::NOperand],
        effect_args: &[NEffectArg<'db>],
        state: &MustAssignState<'db>,
    ) -> Option<AssignedTarget<'db>> {
        match target {
            AssignedTarget::ContractField { .. } => Some(target.clone()),
            AssignedTarget::FuncEffect {
                func,
                requirement_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Func(callee_func) if *func == callee_func) => {
                effect_args
                    .iter()
                    .find(|arg| arg.binding_idx == *requirement_idx)
                    .and_then(|arg| match &arg.arg {
                        NEffectArgValue::Place(place)
                            if fields.is_empty() && arg.pass_mode == EffectPassMode::ByPlace =>
                        {
                            self.write_target_of_place(place, state)
                        }
                        NEffectArgValue::Place(place) if !fields.is_empty() => {
                            if fields.iter().any(|step| {
                                matches!(
                                    step,
                                    CarrierProjection::AnyIndex
                                        | CarrierProjection::DynamicIndex(_)
                                )
                            }) {
                                let source = self.raw_storage_slot(place)?;
                                let mut projection = source.fields;
                                projection.extend_from_slice(fields);
                                return self.target_of_projected_local(
                                    source.local,
                                    &projection,
                                    &mut FxHashSet::default(),
                                    state,
                                );
                            }
                            let mut source = place.clone();
                            append_carrier_path(self.db, self.body, &mut source, fields)?;
                            self.target_of_carrier_source_place(
                                &source,
                                &mut FxHashSet::default(),
                                state,
                            )
                        }
                        NEffectArgValue::Value(value) if !fields.is_empty() => self
                            .target_of_projected_local(
                                value.local,
                                fields,
                                &mut FxHashSet::default(),
                                state,
                            ),
                        NEffectArgValue::Place(_) | NEffectArgValue::Value(_) => None,
                    })
            }
            AssignedTarget::FuncParam {
                func,
                param_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Func(callee_func) if *func == callee_func) => {
                args.get(*param_idx as usize).and_then(|arg| {
                    self.target_of_projected_local(
                        arg.local,
                        fields,
                        &mut FxHashSet::default(),
                        state,
                    )
                })
            }
            AssignedTarget::ClosureCapture {
                def,
                capture_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Closure { def: callee_def, .. } if *def == callee_def) =>
            {
                let mut projection = Vec::with_capacity(fields.len() + 1);
                projection.push(CarrierProjection::Field(*capture_idx as usize));
                projection.extend_from_slice(fields);
                args.first().and_then(|arg| {
                    self.target_of_projected_local(
                        arg.local,
                        &projection,
                        &mut FxHashSet::default(),
                        state,
                    )
                })
            }
            AssignedTarget::ClosureArgument {
                def,
                param_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Closure { def: callee_def, .. } if *def == callee_def) =>
            {
                let mut projection = Vec::with_capacity(fields.len() + 1);
                projection.push(CarrierProjection::Field(*param_idx as usize));
                projection.extend_from_slice(fields);
                args.get(crate::analysis::ty::ty_check::CLOSURE_ARGS_PARAM_IDX)
                    .and_then(|arg| {
                        self.target_of_projected_local(
                            arg.local,
                            &projection,
                            &mut FxHashSet::default(),
                            state,
                        )
                    })
            }
            _ => None,
        }
    }

    fn rebind_destination_slot(
        &self,
        destination: &AssignedTarget<'db>,
        callee_owner: BodyOwner<'db>,
        args: &[super::borrowck::NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Option<CarrierSlot> {
        let (local, projection) = match destination {
            AssignedTarget::FuncParam {
                func,
                param_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Func(callee_func) if *func == callee_func) => {
                (args.get(*param_idx as usize)?.local, fields.clone())
            }
            AssignedTarget::ClosureCapture {
                def,
                capture_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Closure { def: callee_def, .. } if *def == callee_def) =>
            {
                let mut projection = Vec::with_capacity(fields.len() + 1);
                projection.push(CarrierProjection::Field(*capture_idx as usize));
                projection.extend_from_slice(fields);
                (args.first()?.local, projection)
            }
            AssignedTarget::ClosureArgument {
                def,
                param_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Closure { def: callee_def, .. } if *def == callee_def) =>
            {
                let mut projection = Vec::with_capacity(fields.len() + 1);
                projection.push(CarrierProjection::Field(*param_idx as usize));
                projection.extend_from_slice(fields);
                (
                    args.get(crate::analysis::ty::ty_check::CLOSURE_ARGS_PARAM_IDX)?
                        .local,
                    projection,
                )
            }
            AssignedTarget::FuncEffect {
                func,
                requirement_idx,
                fields,
            } if matches!(callee_owner, BodyOwner::Func(callee_func) if *func == callee_func) => {
                let effect_arg = effect_args
                    .iter()
                    .find(|arg| arg.binding_idx == *requirement_idx)?;
                match &effect_arg.arg {
                    NEffectArgValue::Value(value) => (value.local, fields.clone()),
                    NEffectArgValue::Place(place) => {
                        if fields.iter().any(|step| {
                            matches!(
                                step,
                                CarrierProjection::AnyIndex | CarrierProjection::DynamicIndex(_)
                            )
                        }) {
                            return None;
                        }
                        let mut place = place.clone();
                        append_carrier_path(self.db, self.body, &mut place, fields)?;
                        let (local, fields) = storage_exposed_local(
                            self.db,
                            self.body,
                            &self.single_defs,
                            &self.provider_root_locals,
                            &place,
                            &mut FxHashSet::default(),
                        )?;
                        return Some(CarrierSlot { local, fields });
                    }
                }
            }
            _ => return None,
        };
        let mut place = NSPlace {
            root: NSPlaceRoot::CarrierDerefLocal(local),
            path: NSProjectionPath::default(),
        };
        append_carrier_path(self.db, self.body, &mut place, &projection)?;
        let (local, fields) = storage_exposed_local(
            self.db,
            self.body,
            &self.single_defs,
            &self.provider_root_locals,
            &place,
            &mut FxHashSet::default(),
        )?;
        Some(CarrierSlot { local, fields })
    }

    fn invalidate_call_carrier_provenance(
        &self,
        args: &[super::borrowck::NOperand],
        effect_args: &[NEffectArg<'db>],
        state: &mut MustAssignState<'db>,
    ) {
        let mut places = args
            .iter()
            .map(|arg| NSPlace {
                root: NSPlaceRoot::CarrierDerefLocal(arg.local),
                path: NSProjectionPath::default(),
            })
            .collect::<Vec<_>>();
        places.extend(effect_args.iter().map(|arg| match &arg.arg {
            NEffectArgValue::Place(place) => place.clone(),
            NEffectArgValue::Value(value) => NSPlace {
                root: NSPlaceRoot::CarrierDerefLocal(value.local),
                path: NSProjectionPath::default(),
            },
        }));

        for place in places {
            let Some(ty) = self.body.place_ty(self.db, &place) else {
                continue;
            };
            let Some(leaves) = self.mut_carrier_leaf_paths(ty) else {
                let destination = storage_exposed_local(
                    self.db,
                    self.body,
                    &self.single_defs,
                    &self.provider_root_locals,
                    &place,
                    &mut FxHashSet::default(),
                )
                .map(|(local, fields)| CarrierSlot { local, fields })
                .or_else(|| self.raw_storage_slot(&place));
                if let Some(destination) = destination {
                    Self::invalidate_carrier_subtree(destination, state);
                }
                continue;
            };
            for leaf in leaves {
                let mut projected = place.clone();
                if append_carrier_path(self.db, self.body, &mut projected, &leaf).is_none() {
                    continue;
                }
                let destination = storage_exposed_local(
                    self.db,
                    self.body,
                    &self.single_defs,
                    &self.provider_root_locals,
                    &projected,
                    &mut FxHashSet::default(),
                )
                .map(|(local, fields)| CarrierSlot { local, fields })
                .or_else(|| self.raw_storage_slot(&projected));
                if let Some(destination) = destination {
                    Self::invalidate_carrier_subtree(destination, state);
                }
            }
        }
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
        let callee_owner = callee_key.owner(self.db);
        let callee = get_or_build_semantic_instance(self.db, callee_key);
        let Some(summary) = instance_assignment_summary(self.db, callee) else {
            return;
        };
        for target in &summary.assigned {
            if let Some(mapped) =
                self.map_callee_target(target, callee_owner, args, effect_args, state)
            {
                state.assigned.insert(mapped);
            }
        }

        if !summary.rebinds_complete {
            self.invalidate_call_carrier_provenance(args, effect_args, state);
            return;
        }

        let updates = summary
            .rebinds
            .iter()
            .filter_map(|rebind| {
                let destination = self.rebind_destination_slot(
                    &rebind.destination,
                    callee_owner,
                    args,
                    effect_args,
                )?;
                let provenance = rebind
                    .source
                    .as_ref()
                    .and_then(|source| {
                        self.map_callee_target(source, callee_owner, args, effect_args, state)
                    })
                    .map(CarrierProvenance::singleton)
                    .unwrap_or(CarrierProvenance::Unknown);
                Some((destination, provenance))
            })
            .collect::<Vec<_>>();
        for (destination, provenance) in updates {
            self.strong_update_carrier(destination, provenance, state);
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
