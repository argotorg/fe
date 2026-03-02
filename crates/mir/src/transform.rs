use hir::analysis::HirAnalysisDb;
use hir::analysis::ty::simplified_pattern::ConstructorKind;
use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
use hir::hir_def::EnumVariant;
use hir::projection::{IndexSource, Projection};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::CoreLib;
use crate::ir::{
    AddressSpaceKind, CallOrigin, LocalData, LocalId, MirBody, MirFunction, MirInst, MirModule,
    MirProjectionPath, Place, RuntimeAbi, RuntimeShape, Rvalue, SourceInfoId, TerminatingCall,
    Terminator, ValueData, ValueId, ValueOrigin, ValueRepr,
};
use crate::layout;

mod lower_capability_to_repr;

pub(crate) use lower_capability_to_repr::{
    lower_capability_to_repr, prepare_body_for_evm_yul_codegen,
};

struct StabilizeCtx<'db, 'a, 'b> {
    db: &'db dyn HirAnalysisDb,
    values: &'a [ValueData<'db>],
    value_use_counts: &'a [usize],
    bound_in_block: &'b mut FxHashSet<ValueId>,
    rewritten: &'b mut Vec<MirInst<'db>>,
}

impl<'db> StabilizeCtx<'db, '_, '_> {
    fn stabilize_terminator(&mut self, term: &Terminator<'db>) {
        match term {
            Terminator::Return {
                value: Some(value), ..
            } => self.stabilize_value(*value, true, false),
            Terminator::TerminatingCall { call, .. } => match call {
                TerminatingCall::Call(call) => {
                    for arg in call.args.iter().chain(call.effect_args.iter()) {
                        self.stabilize_value(*arg, true, false);
                    }
                }
                TerminatingCall::Intrinsic { args, .. } => {
                    for arg in args {
                        self.stabilize_value(*arg, true, false);
                    }
                }
            },
            Terminator::Branch { cond, .. } => self.stabilize_value(*cond, true, false),
            Terminator::Switch { discr, .. } => self.stabilize_value(*discr, true, false),
            Terminator::Return { value: None, .. }
            | Terminator::Goto { .. }
            | Terminator::Unreachable { .. } => {}
        }
    }

    fn stabilize_path(&mut self, path: &crate::ir::MirProjectionPath<'db>) {
        for proj in path.iter() {
            if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                self.stabilize_value(*value, true, false);
            }
        }
    }

    fn stabilize_value(&mut self, value: ValueId, bind_root: bool, force_root_bind: bool) {
        let mut visiting: FxHashSet<ValueId> = FxHashSet::default();
        self.stabilize_value_inner(value, bind_root, force_root_bind, &mut visiting);
    }

    fn stabilize_value_inner(
        &mut self,
        value: ValueId,
        bind_root: bool,
        force_root_bind: bool,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }

        let origin = &self.values[value.index()].origin;
        for dep in value_deps_in_eval_order(origin) {
            self.stabilize_value_inner(dep, true, false, visiting);
        }

        if bind_root
            && value_should_bind(
                self.db,
                value,
                &self.values[value.index()],
                origin,
                self.value_use_counts,
                force_root_bind,
            )
            && self.bound_in_block.insert(value)
        {
            self.rewritten.push(MirInst::BindValue {
                source: SourceInfoId::SYNTHETIC,
                value,
            });
        }
    }
}

pub(crate) fn insert_temp_binds<'db>(db: &'db dyn HirAnalysisDb, body: &mut MirBody<'db>) {
    let value_use_counts = compute_value_use_counts(body);
    let (blocks, values) = (&mut body.blocks, &body.values);
    for block in blocks {
        let mut bound_in_block: FxHashSet<ValueId> = FxHashSet::default();
        let mut rewritten: Vec<MirInst<'db>> = Vec::with_capacity(block.insts.len());
        {
            let mut ctx = StabilizeCtx {
                db,
                values,
                value_use_counts: &value_use_counts,
                bound_in_block: &mut bound_in_block,
                rewritten: &mut rewritten,
            };

            for inst in std::mem::take(&mut block.insts) {
                match inst {
                    MirInst::BindValue { value, .. } => {
                        ctx.stabilize_value(value, true, true);
                    }
                    MirInst::Assign {
                        source,
                        dest,
                        rvalue,
                    } => {
                        match &rvalue {
                            Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
                            Rvalue::Value(value) => {
                                ctx.stabilize_value(*value, dest.is_some(), false);
                            }
                            Rvalue::Call(call) => {
                                for arg in call.args.iter().chain(call.effect_args.iter()) {
                                    ctx.stabilize_value(*arg, true, false);
                                }
                            }
                            Rvalue::Intrinsic { args, .. } => {
                                for arg in args {
                                    ctx.stabilize_value(*arg, true, false);
                                }
                            }
                            Rvalue::Load { place } => {
                                ctx.stabilize_value(place.base, true, false);
                                ctx.stabilize_path(&place.projection);
                            }
                        }
                        ctx.rewritten.push(MirInst::Assign {
                            source,
                            dest,
                            rvalue,
                        });
                    }
                    MirInst::Store {
                        source,
                        place,
                        value,
                    } => {
                        ctx.stabilize_value(place.base, true, false);
                        ctx.stabilize_path(&place.projection);
                        ctx.stabilize_value(value, true, false);
                        ctx.rewritten.push(MirInst::Store {
                            source,
                            place,
                            value,
                        });
                    }
                    MirInst::InitAggregate {
                        source,
                        place,
                        inits,
                    } => {
                        ctx.stabilize_value(place.base, true, false);
                        ctx.stabilize_path(&place.projection);
                        for (path, value) in &inits {
                            ctx.stabilize_path(path);
                            ctx.stabilize_value(*value, true, false);
                        }
                        ctx.rewritten.push(MirInst::InitAggregate {
                            source,
                            place,
                            inits,
                        });
                    }
                    MirInst::SetDiscriminant {
                        source,
                        place,
                        variant,
                    } => {
                        ctx.stabilize_value(place.base, true, false);
                        ctx.stabilize_path(&place.projection);
                        ctx.rewritten.push(MirInst::SetDiscriminant {
                            source,
                            place,
                            variant,
                        });
                    }
                }
            }

            ctx.stabilize_terminator(&block.terminator);
        }
        block.insts = rewritten;
    }
}

pub fn prepare_module_for_evm_yul_codegen<'db>(
    db: &'db dyn HirAnalysisDb,
    module: &mut MirModule<'db>,
) {
    for func in &mut module.functions {
        let core = function_core_lib(db, func);
        prepare_body_for_evm_yul_codegen(db, &core, &mut func.body);
        canonicalize_transparent_newtypes(db, &mut func.body);
        insert_temp_binds(db, &mut func.body);
        canonicalize_zero_sized(db, &mut func.body);
    }
    normalize_runtime_shapes(db, module);
}

pub(crate) fn compute_live_values<'db>(body: &MirBody<'db>) -> Vec<bool> {
    fn mark_value_live<'db>(
        values: &[ValueData<'db>],
        live: &mut [bool],
        visiting: &mut FxHashSet<ValueId>,
        value: ValueId,
    ) {
        if !visiting.insert(value) {
            return;
        }
        let Some(slot) = live.get_mut(value.index()) else {
            return;
        };
        if *slot {
            return;
        }
        *slot = true;
        let Some(origin) = values.get(value.index()).map(|value| &value.origin) else {
            return;
        };
        for dep in value_deps_in_eval_order(origin) {
            mark_value_live(values, live, visiting, dep);
        }
        visiting.remove(&value);
    }

    let mut live = vec![false; body.values.len()];
    let values = &body.values;

    let mut mark_root = |value: ValueId| {
        let mut visiting = FxHashSet::default();
        mark_value_live(values, &mut live, &mut visiting, value);
    };

    for block in &body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::BindValue { value, .. } => mark_root(*value),
                MirInst::Assign { rvalue, .. } => match rvalue {
                    Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
                    Rvalue::Value(value) => mark_root(*value),
                    Rvalue::Call(call) => {
                        for arg in call.args.iter().chain(call.effect_args.iter()) {
                            mark_root(*arg);
                        }
                    }
                    Rvalue::Intrinsic { args, .. } => {
                        for arg in args {
                            mark_root(*arg);
                        }
                    }
                    Rvalue::Load { place } => {
                        mark_root(place.base);
                        bump_place_path(&mut mark_root, &place.projection);
                    }
                },
                MirInst::Store { place, value, .. } => {
                    mark_root(place.base);
                    bump_place_path(&mut mark_root, &place.projection);
                    mark_root(*value);
                }
                MirInst::InitAggregate { place, inits, .. } => {
                    mark_root(place.base);
                    bump_place_path(&mut mark_root, &place.projection);
                    for (path, value) in inits {
                        bump_place_path(&mut mark_root, path);
                        mark_root(*value);
                    }
                }
                MirInst::SetDiscriminant { place, .. } => {
                    mark_root(place.base);
                    bump_place_path(&mut mark_root, &place.projection);
                }
            }
        }

        match &block.terminator {
            Terminator::Return {
                value: Some(value), ..
            } => mark_root(*value),
            Terminator::TerminatingCall { call, .. } => match call {
                TerminatingCall::Call(call) => {
                    for arg in call.args.iter().chain(call.effect_args.iter()) {
                        mark_root(*arg);
                    }
                }
                TerminatingCall::Intrinsic { args, .. } => {
                    for arg in args {
                        mark_root(*arg);
                    }
                }
            },
            Terminator::Branch { cond, .. } => mark_root(*cond),
            Terminator::Switch { discr, .. } => mark_root(*discr),
            Terminator::Return { value: None, .. }
            | Terminator::Goto { .. }
            | Terminator::Unreachable { .. } => {}
        }
    }

    live
}

fn has_runtime_param_representation(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> bool {
    !layout::is_zero_sized_ty(db, ty)
        && !ty
            .as_capability(db)
            .is_some_and(|(_, inner)| layout::is_zero_sized_ty(db, inner))
}

fn is_contract_entry_function(func: &MirFunction<'_>) -> bool {
    use crate::ir::{ContractFunctionKind, MirFunctionOrigin, SyntheticId};

    func.contract_function.as_ref().is_some_and(|cf| {
        matches!(
            cf.kind,
            ContractFunctionKind::Init | ContractFunctionKind::Runtime
        )
    }) || matches!(
        func.origin,
        MirFunctionOrigin::Synthetic(
            SyntheticId::ContractInitEntrypoint(_) | SyntheticId::ContractRuntimeEntrypoint(_)
        )
    )
}

fn add_place_runtime_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &MirBody<'db>,
    place: &Place<'db>,
    live_locals: &mut FxHashSet<LocalId>,
    seen_values: &mut FxHashSet<ValueId>,
) {
    add_value_runtime_uses(db, body, place.base, live_locals, seen_values);
    for proj in place.projection.iter() {
        if let Projection::Index(IndexSource::Dynamic(value)) = proj {
            add_value_runtime_uses(db, body, *value, live_locals, seen_values);
        }
    }
}

fn add_value_runtime_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &MirBody<'db>,
    value: ValueId,
    live_locals: &mut FxHashSet<LocalId>,
    seen_values: &mut FxHashSet<ValueId>,
) {
    if !seen_values.insert(value) {
        return;
    }
    let Some(value_data) = body.values.get(value.index()) else {
        return;
    };
    if layout::is_zero_sized_ty(db, value_data.ty) {
        return;
    }

    match &value_data.origin {
        ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => {
            live_locals.insert(*local);
        }
        ValueOrigin::Unary { inner, .. } => {
            add_value_runtime_uses(db, body, *inner, live_locals, seen_values);
        }
        ValueOrigin::Binary { lhs, rhs, .. } => {
            add_value_runtime_uses(db, body, *lhs, live_locals, seen_values);
            add_value_runtime_uses(db, body, *rhs, live_locals, seen_values);
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            add_value_runtime_uses(db, body, field_ptr.base, live_locals, seen_values);
        }
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            add_place_runtime_uses(db, body, place, live_locals, seen_values);
        }
        ValueOrigin::TransparentCast { value } => {
            add_value_runtime_uses(db, body, *value, live_locals, seen_values);
        }
        ValueOrigin::Expr(..)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::ConstRegion(_)
        | ValueOrigin::Synthetic(..)
        | ValueOrigin::FuncItem(..) => {}
    }
}

fn add_runtime_call_arg_uses<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &MirBody<'db>,
    call: &CallOrigin<'db>,
    runtime_abis: &[RuntimeAbi<'db>],
    func_indices: &FxHashMap<String, usize>,
    live_locals: &mut FxHashSet<LocalId>,
    seen_values: &mut FxHashSet<ValueId>,
) {
    let Some(callee_name) = call.resolved_name.as_deref() else {
        for &arg in call.args.iter().chain(call.effect_args.iter()) {
            add_value_runtime_uses(db, body, arg, live_locals, seen_values);
        }
        return;
    };
    let Some(&func_idx) = func_indices.get(callee_name) else {
        for &arg in call.args.iter().chain(call.effect_args.iter()) {
            add_value_runtime_uses(db, body, arg, live_locals, seen_values);
        }
        return;
    };
    let Some(runtime_abi) = runtime_abis.get(func_idx) else {
        return;
    };

    for (idx, &arg) in call.args.iter().enumerate() {
        if runtime_abi.value_param_visible(idx) {
            add_value_runtime_uses(db, body, arg, live_locals, seen_values);
        }
    }
    for (idx, &arg) in call.effect_args.iter().enumerate() {
        if runtime_abi.effect_param_visible(idx) {
            add_value_runtime_uses(db, body, arg, live_locals, seen_values);
        }
    }
}

fn runtime_successors(term: &Terminator<'_>) -> Vec<crate::ir::BasicBlockId> {
    match term {
        Terminator::Goto { target, .. } => vec![*target],
        Terminator::Branch {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        Terminator::Switch {
            targets, default, ..
        } => {
            let mut out = Vec::with_capacity(targets.len() + 1);
            out.extend(targets.iter().map(|target| target.block));
            out.push(*default);
            out
        }
        Terminator::Return { .. }
        | Terminator::TerminatingCall { .. }
        | Terminator::Unreachable { .. } => Vec::new(),
    }
}

fn transfer_runtime_terminator<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &MirBody<'db>,
    term: &Terminator<'db>,
    runtime_abis: &[RuntimeAbi<'db>],
    func_indices: &FxHashMap<String, usize>,
    live: &mut FxHashSet<LocalId>,
) {
    let mut seen_values = FxHashSet::default();
    match term {
        Terminator::Return {
            value: Some(value), ..
        } => add_value_runtime_uses(db, body, *value, live, &mut seen_values),
        Terminator::TerminatingCall { call, .. } => match call {
            TerminatingCall::Call(call) => add_runtime_call_arg_uses(
                db,
                body,
                call,
                runtime_abis,
                func_indices,
                live,
                &mut seen_values,
            ),
            TerminatingCall::Intrinsic { op, args, .. } => {
                if !op.returns_value() || matches!(op, crate::ir::IntrinsicOp::Alloc) {
                    for &arg in args {
                        add_value_runtime_uses(db, body, arg, live, &mut seen_values);
                    }
                }
            }
        },
        Terminator::Branch { cond, .. } => {
            add_value_runtime_uses(db, body, *cond, live, &mut seen_values);
        }
        Terminator::Switch { discr, .. } => {
            add_value_runtime_uses(db, body, *discr, live, &mut seen_values);
        }
        Terminator::Return { value: None, .. }
        | Terminator::Goto { .. }
        | Terminator::Unreachable { .. } => {}
    }
}

fn transfer_runtime_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &MirBody<'db>,
    inst: &MirInst<'db>,
    runtime_abis: &[RuntimeAbi<'db>],
    func_indices: &FxHashMap<String, usize>,
    live: &mut FxHashSet<LocalId>,
) {
    let mut seen_values = FxHashSet::default();
    match inst {
        MirInst::Assign { dest, rvalue, .. } => {
            let dest_live = dest.is_some_and(|local| live.remove(&local));
            match rvalue {
                Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
                Rvalue::Value(value) => {
                    if dest.is_none() || dest_live {
                        add_value_runtime_uses(db, body, *value, live, &mut seen_values);
                    }
                }
                Rvalue::Load { place } => {
                    if dest.is_none() || dest_live {
                        add_place_runtime_uses(db, body, place, live, &mut seen_values);
                    }
                }
                Rvalue::Call(call) => add_runtime_call_arg_uses(
                    db,
                    body,
                    call,
                    runtime_abis,
                    func_indices,
                    live,
                    &mut seen_values,
                ),
                Rvalue::Intrinsic { op, args } => {
                    if dest.is_none()
                        || dest_live
                        || !op.returns_value()
                        || matches!(op, crate::ir::IntrinsicOp::Alloc)
                    {
                        for &arg in args {
                            add_value_runtime_uses(db, body, arg, live, &mut seen_values);
                        }
                    }
                }
            }
        }
        MirInst::Store { place, value, .. } => {
            add_place_runtime_uses(db, body, place, live, &mut seen_values);
            add_value_runtime_uses(db, body, *value, live, &mut seen_values);
        }
        MirInst::InitAggregate { place, inits, .. } => {
            add_place_runtime_uses(db, body, place, live, &mut seen_values);
            for (path, value) in inits {
                for proj in path.iter() {
                    if let Projection::Index(IndexSource::Dynamic(index)) = proj {
                        add_value_runtime_uses(db, body, *index, live, &mut seen_values);
                    }
                }
                add_value_runtime_uses(db, body, *value, live, &mut seen_values);
            }
        }
        MirInst::SetDiscriminant { place, .. } => {
            add_place_runtime_uses(db, body, place, live, &mut seen_values);
        }
        MirInst::BindValue { .. } => {}
    }
}

fn compute_entry_runtime_live_locals<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    runtime_abis: &[RuntimeAbi<'db>],
    func_indices: &FxHashMap<String, usize>,
) -> FxHashSet<LocalId> {
    let block_count = func.body.blocks.len();
    let mut live_in = vec![FxHashSet::default(); block_count];
    let mut changed = true;
    while changed {
        changed = false;
        for block_idx in (0..block_count).rev() {
            let block = &func.body.blocks[block_idx];
            let mut live = FxHashSet::default();
            for succ in runtime_successors(&block.terminator) {
                live.extend(live_in[succ.index()].iter().copied());
            }
            transfer_runtime_terminator(
                db,
                &func.body,
                &block.terminator,
                runtime_abis,
                func_indices,
                &mut live,
            );
            for inst in block.insts.iter().rev() {
                transfer_runtime_inst(db, &func.body, inst, runtime_abis, func_indices, &mut live);
            }
            if live != live_in[block_idx] {
                live_in[block_idx] = live;
                changed = true;
            }
        }
    }
    live_in
        .get(func.body.entry.index())
        .cloned()
        .unwrap_or_default()
}

fn compute_runtime_abi<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    runtime_abis: &[RuntimeAbi<'db>],
    func_indices: &FxHashMap<String, usize>,
) -> RuntimeAbi<'db> {
    if is_contract_entry_function(func) {
        return RuntimeAbi {
            value_params: vec![false; func.body.param_locals.len()],
            effect_params: vec![false; func.body.effect_param_locals.len()],
            effect_param_provider_tys: func.runtime_abi.effect_param_provider_tys.clone(),
        };
    }

    let live_at_entry = compute_entry_runtime_live_locals(db, func, runtime_abis, func_indices);
    let value_params = func
        .body
        .param_locals
        .iter()
        .copied()
        .map(|local| {
            live_at_entry.contains(&local)
                && has_runtime_param_representation(db, func.body.local(local).ty)
        })
        .collect();
    let effect_params = func
        .body
        .effect_param_locals
        .iter()
        .copied()
        .enumerate()
        .map(|(idx, local)| {
            live_at_entry.contains(&local)
                && func
                    .runtime_abi
                    .effect_param_provider_tys
                    .get(idx)
                    .copied()
                    .flatten()
                    .map(|ty| has_runtime_param_representation(db, ty))
                    .unwrap_or_else(|| {
                        has_runtime_param_representation(db, func.body.local(local).ty)
                    })
        })
        .collect();

    RuntimeAbi {
        value_params,
        effect_params,
        effect_param_provider_tys: func.runtime_abi.effect_param_provider_tys.clone(),
    }
}

fn rewrite_runtime_call_args<'db>(
    call: &mut CallOrigin<'db>,
    runtime_abis: &[RuntimeAbi<'db>],
    func_indices: &FxHashMap<String, usize>,
) {
    let Some(callee_name) = call.resolved_name.as_deref() else {
        return;
    };
    let Some(&func_idx) = func_indices.get(callee_name) else {
        return;
    };
    let Some(runtime_abi) = runtime_abis.get(func_idx) else {
        return;
    };

    call.args = call
        .args
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, arg)| runtime_abi.value_param_visible(idx).then_some(arg))
        .collect();
    call.effect_args = call
        .effect_args
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, arg)| runtime_abi.effect_param_visible(idx).then_some(arg))
        .collect();
}

fn path_has_deref<'db>(path: &MirProjectionPath<'db>) -> bool {
    path.iter().any(|proj| matches!(proj, Projection::Deref))
}

fn removable_memory_root_local_for_place<'db>(
    body: &MirBody<'db>,
    place: &Place<'db>,
) -> Option<LocalId> {
    let (local, prefix) = crate::ir::resolve_local_projection_root(&body.values, place.base)?;
    if path_has_deref(&prefix) || path_has_deref(&place.projection) {
        return None;
    }
    (crate::ir::try_place_address_space_in(&body.values, &body.locals, place)
        == Some(AddressSpaceKind::Memory))
    .then_some(local)
}

fn removable_assign_rvalue<'db>(_body: &MirBody<'db>, rvalue: &Rvalue<'db>) -> bool {
    match rvalue {
        Rvalue::ZeroInit | Rvalue::Alloc { .. } | Rvalue::Value(_) => true,
        Rvalue::Load { .. } => true,
        Rvalue::Call(_) | Rvalue::Intrinsic { .. } => false,
    }
}

fn mark_value_runtime_live<'db>(
    body: &MirBody<'db>,
    live_values: &mut [bool],
    live_locals: &mut FxHashSet<LocalId>,
    value: ValueId,
) -> bool {
    let Some(slot) = live_values.get_mut(value.index()) else {
        return false;
    };
    if *slot {
        return false;
    }
    *slot = true;

    let Some(data) = body.values.get(value.index()) else {
        return true;
    };
    let mut changed = true;
    match &data.origin {
        ValueOrigin::Unary { inner, .. } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *inner);
        }
        ValueOrigin::Binary { lhs, rhs, .. } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *lhs);
            changed |= mark_value_runtime_live(body, live_values, live_locals, *rhs);
        }
        ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => {
            changed |= live_locals.insert(*local);
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, field_ptr.base);
        }
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, place.base);
            for proj in place.projection.iter() {
                if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                    changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
                }
            }
        }
        ValueOrigin::TransparentCast { value } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
        }
        ValueOrigin::Expr(..)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::ConstRegion(_)
        | ValueOrigin::Synthetic(..)
        | ValueOrigin::FuncItem(..) => {}
    }
    changed
}

fn mark_place_runtime_live<'db>(
    body: &MirBody<'db>,
    live_values: &mut [bool],
    live_locals: &mut FxHashSet<LocalId>,
    place: &Place<'db>,
) -> bool {
    let mut changed = mark_value_runtime_live(body, live_values, live_locals, place.base);
    for proj in place.projection.iter() {
        if let Projection::Index(IndexSource::Dynamic(value)) = proj {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
        }
    }
    changed
}

fn instruction_is_runtime_root<'db>(
    body: &MirBody<'db>,
    inst: &MirInst<'db>,
    live_values: &[bool],
    live_locals: &FxHashSet<LocalId>,
) -> bool {
    match inst {
        MirInst::Assign { dest, rvalue, .. } => match dest {
            Some(local) if removable_assign_rvalue(body, rvalue) => live_locals.contains(local),
            None if removable_assign_rvalue(body, rvalue) => false,
            _ => true,
        },
        MirInst::Store { place, .. }
        | MirInst::InitAggregate { place, .. }
        | MirInst::SetDiscriminant { place, .. } => {
            removable_memory_root_local_for_place(body, place)
                .is_none_or(|local| live_locals.contains(&local))
        }
        MirInst::BindValue { value, .. } => {
            live_values.get(value.index()).copied().unwrap_or(false)
        }
    }
}

fn mark_runtime_inst_live_operands<'db>(
    body: &MirBody<'db>,
    inst: &MirInst<'db>,
    live_values: &mut [bool],
    live_locals: &mut FxHashSet<LocalId>,
) -> bool {
    let mut changed = false;
    match inst {
        MirInst::Assign { rvalue, .. } => match rvalue {
            Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
            Rvalue::Value(value) => {
                changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
            }
            Rvalue::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    changed |= mark_value_runtime_live(body, live_values, live_locals, *arg);
                }
            }
            Rvalue::Intrinsic { args, .. } => {
                for arg in args {
                    changed |= mark_value_runtime_live(body, live_values, live_locals, *arg);
                }
            }
            Rvalue::Load { place } => {
                changed |= mark_place_runtime_live(body, live_values, live_locals, place);
            }
        },
        MirInst::Store { place, value, .. } => {
            changed |= mark_place_runtime_live(body, live_values, live_locals, place);
            changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
        }
        MirInst::InitAggregate { place, inits, .. } => {
            changed |= mark_place_runtime_live(body, live_values, live_locals, place);
            for (path, value) in inits {
                for proj in path.iter() {
                    if let Projection::Index(IndexSource::Dynamic(index)) = proj {
                        changed |= mark_value_runtime_live(body, live_values, live_locals, *index);
                    }
                }
                changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
            }
        }
        MirInst::SetDiscriminant { place, .. } => {
            changed |= mark_place_runtime_live(body, live_values, live_locals, place);
        }
        MirInst::BindValue { value, .. } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
        }
    }
    changed
}

fn mark_runtime_terminator_live_operands<'db>(
    body: &MirBody<'db>,
    terminator: &Terminator<'db>,
    live_values: &mut [bool],
    live_locals: &mut FxHashSet<LocalId>,
) -> bool {
    let mut changed = false;
    match terminator {
        Terminator::Return {
            value: Some(value), ..
        } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *value);
        }
        Terminator::TerminatingCall { call, .. } => match call {
            TerminatingCall::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    changed |= mark_value_runtime_live(body, live_values, live_locals, *arg);
                }
            }
            TerminatingCall::Intrinsic { args, .. } => {
                for arg in args {
                    changed |= mark_value_runtime_live(body, live_values, live_locals, *arg);
                }
            }
        },
        Terminator::Branch { cond, .. } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *cond);
        }
        Terminator::Switch { discr, .. } => {
            changed |= mark_value_runtime_live(body, live_values, live_locals, *discr);
        }
        Terminator::Return { value: None, .. }
        | Terminator::Goto { .. }
        | Terminator::Unreachable { .. } => {}
    }
    changed
}

pub(crate) fn eliminate_dead_erased_arg_materializations<'db>(
    _db: &'db dyn HirAnalysisDb,
    module: &mut MirModule<'db>,
) {
    for func in &mut module.functions {
        let body = &func.body;
        let mut live_values = vec![false; body.values.len()];
        let mut live_locals = FxHashSet::default();

        loop {
            let mut changed = false;
            for block in &body.blocks {
                changed |= mark_runtime_terminator_live_operands(
                    body,
                    &block.terminator,
                    &mut live_values,
                    &mut live_locals,
                );
                for inst in &block.insts {
                    if instruction_is_runtime_root(body, inst, &live_values, &live_locals) {
                        changed |= mark_runtime_inst_live_operands(
                            body,
                            inst,
                            &mut live_values,
                            &mut live_locals,
                        );
                    }
                }
            }
            if !changed {
                break;
            }
        }

        let body_ref = &func.body;
        let keep_insts: Vec<Vec<bool>> = body_ref
            .blocks
            .iter()
            .map(|block| {
                block
                    .insts
                    .iter()
                    .map(|inst| {
                        instruction_is_runtime_root(body_ref, inst, &live_values, &live_locals)
                    })
                    .collect()
            })
            .collect();

        for (block, keep) in func.body.blocks.iter_mut().zip(keep_insts) {
            block.insts = std::mem::take(&mut block.insts)
                .into_iter()
                .zip(keep)
                .filter_map(|(inst, keep)| keep.then_some(inst))
                .collect();
        }
    }
}

pub(crate) fn normalize_runtime_abi<'db>(db: &'db dyn HirAnalysisDb, module: &mut MirModule<'db>) {
    let func_indices: FxHashMap<_, _> = module
        .functions
        .iter()
        .enumerate()
        .filter(|(_, func)| !func.symbol_name.is_empty())
        .map(|(idx, func)| (func.symbol_name.clone(), idx))
        .collect();

    loop {
        let current_abis: Vec<_> = module
            .functions
            .iter()
            .map(|func| func.runtime_abi.clone())
            .collect();
        let mut changed = false;
        for func in &mut module.functions {
            let next = compute_runtime_abi(db, func, &current_abis, &func_indices);
            if func.runtime_abi.value_params != next.value_params
                || func.runtime_abi.effect_params != next.effect_params
            {
                func.runtime_abi.value_params = next.value_params;
                func.runtime_abi.effect_params = next.effect_params;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let final_abis: Vec<_> = module
        .functions
        .iter()
        .map(|func| func.runtime_abi.clone())
        .collect();
    for func in &mut module.functions {
        for block in &mut func.body.blocks {
            for inst in &mut block.insts {
                match inst {
                    MirInst::Assign {
                        rvalue: Rvalue::Call(call),
                        ..
                    } => rewrite_runtime_call_args(call, &final_abis, &func_indices),
                    MirInst::Assign { .. }
                    | MirInst::Store { .. }
                    | MirInst::InitAggregate { .. }
                    | MirInst::SetDiscriminant { .. }
                    | MirInst::BindValue { .. } => {}
                }
            }
            if let Terminator::TerminatingCall {
                call: TerminatingCall::Call(call),
                ..
            } = &mut block.terminator
            {
                rewrite_runtime_call_args(call, &final_abis, &func_indices);
            }
        }
    }
}

fn function_core_lib<'db>(db: &'db dyn HirAnalysisDb, func: &MirFunction<'db>) -> CoreLib<'db> {
    let scope = match func.origin {
        crate::ir::MirFunctionOrigin::Hir(hir_func) => hir_func.scope(),
        crate::ir::MirFunctionOrigin::Synthetic(synth) => synth.contract().scope(),
    };
    CoreLib::new(db, scope)
}

fn merge_runtime_shapes<'db>(
    existing: RuntimeShape<'db>,
    next: RuntimeShape<'db>,
) -> Option<RuntimeShape<'db>> {
    match (existing, next) {
        (RuntimeShape::Unresolved, shape) | (shape, RuntimeShape::Unresolved) => Some(shape),
        (RuntimeShape::Erased, shape) | (shape, RuntimeShape::Erased) => Some(shape),
        (RuntimeShape::Word(lhs), RuntimeShape::Word(rhs)) if lhs == rhs => {
            Some(RuntimeShape::Word(lhs))
        }
        (
            RuntimeShape::MemoryPtr {
                target_ty: lhs_target,
            },
            RuntimeShape::MemoryPtr {
                target_ty: rhs_target,
            },
        ) => Some(RuntimeShape::MemoryPtr {
            target_ty: lhs_target.or(rhs_target),
        }),
        (RuntimeShape::AddressWord(lhs), RuntimeShape::AddressWord(rhs))
            if lhs.address_space == rhs.address_space =>
        {
            Some(RuntimeShape::AddressWord(crate::ir::PointerInfo {
                address_space: lhs.address_space,
                target_ty: lhs.target_ty.or(rhs.target_ty),
            }))
        }
        (
            RuntimeShape::Word(crate::ir::RuntimeWordKind::I256),
            RuntimeShape::MemoryPtr { target_ty },
        )
        | (
            RuntimeShape::MemoryPtr { target_ty },
            RuntimeShape::Word(crate::ir::RuntimeWordKind::I256),
        ) => Some(RuntimeShape::MemoryPtr { target_ty }),
        (RuntimeShape::Word(crate::ir::RuntimeWordKind::I256), RuntimeShape::AddressWord(info))
        | (RuntimeShape::AddressWord(info), RuntimeShape::Word(crate::ir::RuntimeWordKind::I256)) => {
            Some(RuntimeShape::AddressWord(info))
        }
        _ => None,
    }
}

fn assert_normalized_runtime_shapes<'db>(db: &'db dyn HirAnalysisDb, func: &MirFunction<'db>) {
    let live_values = compute_live_values(&func.body);

    for (idx, local) in func.body.locals.iter().enumerate() {
        if local.runtime_shape.is_unresolved() {
            panic!(
                "unresolved local runtime shape after MIR normalization in `{}` for local v{idx} `{}`: ty={}, address_space={:?}, pointer_leaf_infos={:?}",
                func.symbol_name,
                local.name,
                local.ty.pretty_print(db),
                local.address_space,
                local.pointer_leaf_infos,
            );
        }
    }

    for (idx, value) in func.body.values.iter().enumerate() {
        if !live_values.get(idx).copied().unwrap_or(false) {
            continue;
        }
        if value.runtime_shape.is_unresolved() {
            panic!(
                "unresolved value runtime shape after MIR normalization in `{}` for v{idx}: origin={:?}, ty={}, repr={:?}, pointer_info={:?}",
                func.symbol_name,
                value.origin,
                value.ty.pretty_print(db),
                value.repr,
                value.pointer_info,
            );
        }
        let Some(local) = (match value.origin {
            ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => Some(local),
            _ => None,
        }) else {
            continue;
        };
        let local_shape = func.body.local(local).runtime_shape;
        if merge_runtime_shapes(local_shape, value.runtime_shape).is_none() {
            panic!(
                "incompatible root local runtime shapes after MIR normalization in `{}` for v{idx}: local v{} `{}` has {:?}, value has {:?}, origin={:?}, ty={}, repr={:?}",
                func.symbol_name,
                local.index(),
                func.body.local(local).name,
                local_shape,
                value.runtime_shape,
                value.origin,
                value.ty.pretty_print(db),
                value.repr,
            );
        }
    }
}

pub(crate) fn normalize_runtime_shapes<'db>(
    db: &'db dyn HirAnalysisDb,
    module: &mut MirModule<'db>,
) {
    for func in &mut module.functions {
        let core = function_core_lib(db, func);
        let live_values = compute_live_values(&func.body);

        for local in &mut func.body.locals {
            local.runtime_shape = crate::repr::runtime_shape_for_local(db, &core, local);
        }

        for (idx, local_id) in func.body.param_locals.iter().copied().enumerate() {
            if func.runtime_abi.value_param_visible(idx) {
                continue;
            }
            if !live_values.iter().enumerate().any(|(value_idx, live)| {
                *live
                    && matches!(
                        func.body.values[value_idx].origin,
                        ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) if local == local_id
                    )
            }) {
                func.body.locals[local_id.index()].runtime_shape = RuntimeShape::Erased;
            }
        }
        for (idx, local_id) in func.body.effect_param_locals.iter().copied().enumerate() {
            if func.runtime_abi.effect_param_visible(idx) {
                continue;
            }
            if !live_values.iter().enumerate().any(|(value_idx, live)| {
                *live
                    && matches!(
                        func.body.values[value_idx].origin,
                        ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) if local == local_id
                    )
            }) {
                func.body.locals[local_id.index()].runtime_shape = RuntimeShape::Erased;
            }
        }

        for idx in 0..func.body.values.len() {
            let value_id = ValueId(idx as u32);
            let shape = crate::repr::runtime_shape_for_value(
                db,
                &core,
                &func.body.values,
                &func.body.locals,
                value_id,
            )
            .unwrap_or(RuntimeShape::Unresolved);
            func.body.values[idx].runtime_shape = shape;
        }

        let mut runtime_return_shape = if func.runtime_return_shape.is_unresolved() {
            crate::repr::runtime_return_shape_seed_for_ty(db, &core, func.ret_ty)
        } else {
            func.runtime_return_shape
        };

        for block in &func.body.blocks {
            let Terminator::Return {
                value: Some(value), ..
            } = &block.terminator
            else {
                continue;
            };
            let Some(returned) = func
                .body
                .values
                .get(value.index())
                .map(|value| value.runtime_shape)
            else {
                continue;
            };
            runtime_return_shape = merge_runtime_shapes(runtime_return_shape, returned)
                .unwrap_or_else(|| {
                    panic!(
                        "incompatible runtime return shapes in `{}`: {:?} vs {:?}",
                        func.symbol_name, runtime_return_shape, returned
                    )
                });
        }

        if runtime_return_shape.is_unresolved() {
            panic!(
                "failed to resolve runtime return shape in `{}` for `{}`",
                func.symbol_name,
                func.ret_ty.pretty_print(db)
            );
        }

        func.runtime_return_shape = runtime_return_shape;
        assert_normalized_runtime_shapes(db, func);
    }
}

/// Canonicalize transparent-newtype operations in MIR.
///
/// This pass enforces a single representation strategy for transparent single-field wrappers
/// (single-field structs and single-element tuples):
/// - Collapses chains of `ValueOrigin::TransparentCast` so downstream passes don't need to chase
///   multiple hops.
/// - Rewrites `Place` projection paths to peel `.0` field projections over transparent newtypes
///   by inserting type-only `TransparentCast`s on the base address value and removing the no-op
///   field projection.
///
/// This is intended as a post-lowering cleanup that reduces scattered newtype handling in later
/// passes and in codegen.
pub(crate) fn canonicalize_transparent_newtypes<'db>(
    db: &'db dyn HirAnalysisDb,
    body: &mut MirBody<'db>,
) {
    fn alloc_value<'db>(values: &mut Vec<ValueData<'db>>, data: ValueData<'db>) -> ValueId {
        let id = ValueId(values.len() as u32);
        values.push(data);
        id
    }

    fn flatten_transparent_cast_chains<'db>(values: &mut [ValueData<'db>]) {
        for idx in 0..values.len() {
            let ValueOrigin::TransparentCast { value: mut inner } = values[idx].origin else {
                continue;
            };
            // Bound the walk defensively (cycles should be impossible).
            for _ in 0..values.len() {
                match values.get(inner.index()).map(|v| &v.origin) {
                    Some(ValueOrigin::TransparentCast { value }) => inner = *value,
                    _ => break,
                }
            }
            values[idx].origin = ValueOrigin::TransparentCast { value: inner };
        }
    }

    fn apply_projection_to_ty<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        proj: &Projection<TyId<'db>, EnumVariant<'db>, ValueId>,
    ) -> Option<TyId<'db>> {
        match proj {
            Projection::Field(field_idx) => ty.field_types(db).get(*field_idx).copied(),
            Projection::VariantField {
                variant,
                enum_ty,
                field_idx,
            } => {
                let ctor = ConstructorKind::Variant(*variant, *enum_ty);
                ctor.field_types(db).get(*field_idx).copied()
            }
            Projection::Discriminant => {
                Some(TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256))))
            }
            Projection::Index(_idx) => {
                let (base, args) = ty.decompose_ty_app(db);
                (base.is_array(db) && !args.is_empty()).then(|| args[0])
            }
            Projection::Deref => None,
        }
    }

    fn canonicalize_place<'db>(
        db: &'db dyn HirAnalysisDb,
        values: &mut Vec<ValueData<'db>>,
        locals: &[LocalData<'db>],
        place: Place<'db>,
    ) -> Place<'db> {
        // Only attempt to canonicalize places rooted at pointer-like values. If the base is a
        // pure word with no address space, treating it as an address is a bug; this pass avoids
        // "fixing" such cases into memory loads/stores.
        if values
            .get(place.base.index())
            .is_none_or(|v| v.repr.address_space().is_none())
        {
            return place;
        }

        let mut base = place.base;
        let mut current_ty = values[base.index()].ty;
        let mut path = MirProjectionPath::new();

        for proj in place.projection.iter() {
            // Peel transparent-newtype field 0 projections by retyping the base address.
            if let Some(inner_ty) =
                crate::repr::transparent_field0_projection_step_ty(db, current_ty, proj)
            {
                let base_at_point = if path.is_empty() {
                    base
                } else {
                    let addr_space = crate::ir::try_value_address_space_in(values, locals, base)
                        .expect("pointer-like canonicalize base must carry an address space");
                    let prefix_place = Place::new(base, path);
                    alloc_value(
                        values,
                        ValueData {
                            ty: current_ty,
                            origin: ValueOrigin::PlaceRef(prefix_place),
                            source: SourceInfoId::SYNTHETIC,
                            repr: ValueRepr::Ref(addr_space),
                            pointer_info: Some(crate::ir::PointerInfo {
                                address_space: addr_space,
                                target_ty: Some(current_ty),
                            }),
                            runtime_shape: crate::ir::RuntimeShape::Unresolved,
                        },
                    )
                };

                let repr = values[base_at_point.index()].repr;
                base = alloc_value(
                    values,
                    ValueData {
                        ty: inner_ty,
                        origin: ValueOrigin::TransparentCast {
                            value: base_at_point,
                        },
                        source: SourceInfoId::SYNTHETIC,
                        repr,
                        pointer_info: crate::ir::try_value_pointer_info_in(
                            values,
                            locals,
                            base_at_point,
                        ),
                        runtime_shape: crate::ir::RuntimeShape::Unresolved,
                    },
                );
                current_ty = inner_ty;
                path = MirProjectionPath::new();
                continue;
            }

            path.push(proj.clone());
            if let Some(next) = apply_projection_to_ty(db, current_ty, proj) {
                current_ty = next;
            }
        }

        Place::new(base, path)
    }

    flatten_transparent_cast_chains(&mut body.values);

    let (values, blocks, locals) = (&mut body.values, &mut body.blocks, &body.locals);

    let initial_values_len = values.len();
    for idx in 0..initial_values_len {
        let (place, is_move_out) = match &values[idx].origin {
            ValueOrigin::PlaceRef(place) => (Some(place.clone()), false),
            ValueOrigin::MoveOut { place } => (Some(place.clone()), true),
            _ => (None, false),
        };
        if let Some(place) = place {
            let updated = canonicalize_place(db, values, locals, place);
            values[idx].origin = if is_move_out {
                ValueOrigin::MoveOut { place: updated }
            } else {
                ValueOrigin::PlaceRef(updated)
            };
        }
    }

    for block in blocks {
        for inst in &mut block.insts {
            match inst {
                MirInst::Assign { rvalue, .. } => {
                    if let Rvalue::Load { place } = rvalue {
                        *place = canonicalize_place(db, values, locals, place.clone());
                    }
                }
                MirInst::Store { place, .. } => {
                    *place = canonicalize_place(db, values, locals, place.clone());
                }
                MirInst::InitAggregate { place, .. } => {
                    *place = canonicalize_place(db, values, locals, place.clone());
                }
                MirInst::SetDiscriminant { place, .. } => {
                    *place = canonicalize_place(db, values, locals, place.clone());
                }
                MirInst::BindValue { .. } => {}
            }
        }
    }

    flatten_transparent_cast_chains(values);
}

/// Canonicalize zero-sized types (ZSTs) in MIR.
///
/// After this pass:
/// - ZST-returning ops do not produce runtime values; their values are rewritten to `Unit`.
/// - `Eval`/`EvalValue` instructions for ZST values are removed (their effects
///   are preserved by explicit effectful MIR instructions (`Call`/`Intrinsic`/`Load`/`Alloc`).
/// - `Store`/`InitAggregate` instructions of ZST values are removed, but any
///   dynamic index computations and RHS evaluations are preserved via inserted
///   `EvalValue`s to maintain evaluation order.
/// - `Return(Some(v))` where `v` is ZST is rewritten to `Return(None)`.
pub(crate) fn canonicalize_zero_sized<'db>(db: &'db dyn HirAnalysisDb, body: &mut MirBody<'db>) {
    fn is_zst(db: &dyn HirAnalysisDb, ty: hir::analysis::ty::ty_def::TyId<'_>) -> bool {
        layout::is_zero_sized_ty(db, ty)
    }

    fn mark_unit<'db>(values: &mut [ValueData<'db>], value: ValueId) {
        let data = &mut values[value.index()];
        data.origin = ValueOrigin::Unit;
        data.repr = ValueRepr::Word;
    }

    fn push_eval_value<'db>(
        db: &'db dyn HirAnalysisDb,
        values: &mut [ValueData<'db>],
        out: &mut Vec<MirInst<'db>>,
        value: ValueId,
    ) {
        let value_ty = values[value.index()].ty;
        if !is_zst(db, value_ty) {
            out.push(MirInst::Assign {
                source: SourceInfoId::SYNTHETIC,
                dest: None,
                rvalue: Rvalue::Value(value),
            });
            return;
        }

        // ZST: the value has no runtime representation.
        mark_unit(values, value);
    }

    fn push_place_eval<'db>(
        db: &'db dyn HirAnalysisDb,
        values: &mut [ValueData<'db>],
        out: &mut Vec<MirInst<'db>>,
        place: &crate::ir::Place<'db>,
    ) {
        push_eval_value(db, values, out, place.base);
        for proj in place.projection.iter() {
            if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                push_eval_value(db, values, out, *value);
            }
        }
    }

    fn push_path_eval<'db>(
        db: &'db dyn HirAnalysisDb,
        values: &mut [ValueData<'db>],
        out: &mut Vec<MirInst<'db>>,
        path: &crate::ir::MirProjectionPath<'db>,
    ) {
        for proj in path.iter() {
            if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                push_eval_value(db, values, out, *value);
            }
        }
    }

    let zst_locals: Vec<bool> = body
        .locals
        .iter()
        .map(|local| is_zst(db, local.ty))
        .collect();
    let (blocks, values) = (&mut body.blocks, &mut body.values);
    for block in blocks {
        let mut rewritten: Vec<MirInst<'db>> = Vec::with_capacity(block.insts.len());
        for inst in std::mem::take(&mut block.insts) {
            match inst {
                MirInst::Assign {
                    source,
                    dest,
                    rvalue,
                } => match dest {
                    Some(dest) if zst_locals.get(dest.index()).copied().unwrap_or(false) => {
                        // Dest is ZST: keep side effects, drop runtime write.
                        match rvalue {
                            Rvalue::Call(call) => rewritten.push(MirInst::Assign {
                                source,
                                dest: None,
                                rvalue: Rvalue::Call(call),
                            }),
                            Rvalue::Intrinsic { op, args } => rewritten.push(MirInst::Assign {
                                source,
                                dest: None,
                                rvalue: Rvalue::Intrinsic { op, args },
                            }),
                            Rvalue::Value(value) => {
                                // Pure value, no runtime representation needed.
                                if is_zst(db, values[value.index()].ty) {
                                    mark_unit(values, value);
                                }
                            }
                            Rvalue::Load { place } => {
                                // Even though the loaded value is ZST (so the write can be
                                // dropped), we must still evaluate the load's place to preserve
                                // any side effects in the base/index expressions.
                                push_place_eval(db, values, &mut rewritten, &place);
                            }
                            Rvalue::Alloc { .. } | Rvalue::ZeroInit => {}
                        }
                    }
                    _ => {
                        // Dest is non-ZST (or none): canonicalize ZST-valued evals.
                        if dest.is_none()
                            && let Rvalue::Value(value) = &rvalue
                        {
                            let value_ty = values[value.index()].ty;
                            if is_zst(db, value_ty) {
                                mark_unit(values, *value);
                                continue;
                            }
                        }
                        rewritten.push(MirInst::Assign {
                            source,
                            dest,
                            rvalue,
                        });
                    }
                },
                MirInst::BindValue { source, value } => {
                    let value_ty = values[value.index()].ty;
                    if is_zst(db, value_ty) {
                        push_eval_value(db, values, &mut rewritten, value);
                    } else {
                        rewritten.push(MirInst::BindValue { source, value });
                    }
                }
                MirInst::Store {
                    source,
                    place,
                    value,
                } => {
                    let value_ty = values[value.index()].ty;
                    if is_zst(db, value_ty) {
                        push_place_eval(db, values, &mut rewritten, &place);
                        push_eval_value(db, values, &mut rewritten, value);
                    } else {
                        rewritten.push(MirInst::Store {
                            source,
                            place,
                            value,
                        });
                    }
                }
                MirInst::InitAggregate {
                    source,
                    place,
                    inits,
                } => {
                    let base_ty = values[place.base.index()].ty;
                    if is_zst(db, base_ty) {
                        push_place_eval(db, values, &mut rewritten, &place);
                        for (path, value) in &inits {
                            push_path_eval(db, values, &mut rewritten, path);
                            push_eval_value(db, values, &mut rewritten, *value);
                        }
                    } else {
                        let mut kept: Vec<(crate::ir::MirProjectionPath<'db>, ValueId)> =
                            Vec::with_capacity(inits.len());
                        let mut removed_any = false;
                        for (path, value) in inits {
                            let value_ty = values[value.index()].ty;
                            if is_zst(db, value_ty) {
                                if !removed_any {
                                    push_place_eval(db, values, &mut rewritten, &place);
                                    removed_any = true;
                                }
                                push_path_eval(db, values, &mut rewritten, &path);
                                push_eval_value(db, values, &mut rewritten, value);
                            } else {
                                kept.push((path, value));
                            }
                        }
                        if kept.is_empty() {
                            if !removed_any {
                                // No inits to keep, but still preserve evaluation of the base.
                                push_place_eval(db, values, &mut rewritten, &place);
                            }
                        } else {
                            rewritten.push(MirInst::InitAggregate {
                                source,
                                place,
                                inits: kept,
                            });
                        }
                    }
                }
                other => rewritten.push(other),
            }
        }

        if let Terminator::Return {
            source,
            value: Some(value),
        } = &mut block.terminator
        {
            let ty = values[value.index()].ty;
            if is_zst(db, ty) {
                // Ensure any side effects are emitted before the return.
                push_eval_value(db, values, &mut rewritten, *value);
                block.terminator = Terminator::Return {
                    source: *source,
                    value: None,
                };
            }
        }

        block.insts = rewritten;
    }

    // Ensure no runtime value references a zero-sized local.
    //
    // The instruction forms that "define" locals (e.g. `Alloc`, `Load`, `Call` dests) can be
    // removed for ZSTs above. Any MIR `ValueId` that still points at such locals must be
    // canonicalized to `Unit` so codegen never needs to bind/resolve a runtime representation.
    for value in values.iter_mut() {
        if let ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) = &value.origin
            && zst_locals.get(local.index()).copied().unwrap_or(false)
        {
            value.origin = ValueOrigin::Unit;
            value.repr = ValueRepr::Word;
        }
    }

    let mut unit_values: Vec<bool> = values
        .iter()
        .map(|value| matches!(value.origin, ValueOrigin::Unit))
        .collect();
    let mut changed = true;
    while changed {
        changed = false;
        for (idx, value) in values.iter_mut().enumerate() {
            let collapses_to_unit = match &value.origin {
                ValueOrigin::TransparentCast { value } => {
                    unit_values.get(value.index()).copied().unwrap_or(false)
                }
                ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => unit_values
                    .get(place.base.index())
                    .copied()
                    .unwrap_or(false),
                _ => false,
            };
            if collapses_to_unit && !unit_values[idx] {
                value.origin = ValueOrigin::Unit;
                value.repr = ValueRepr::Word;
                unit_values[idx] = true;
                changed = true;
            }
        }
    }
}

fn value_should_bind(
    db: &dyn HirAnalysisDb,
    value_id: ValueId,
    value: &ValueData<'_>,
    origin: &ValueOrigin<'_>,
    value_use_counts: &[usize],
    force_root_bind: bool,
) -> bool {
    if force_root_bind {
        return true;
    }
    if layout::is_zero_sized_ty(db, value.ty) {
        return false;
    }
    value_use_counts.get(value_id.index()).copied().unwrap_or(0) > 1
        && !matches!(
            origin,
            ValueOrigin::Unit
                | ValueOrigin::Synthetic(..)
                | ValueOrigin::Local(..)
                | ValueOrigin::PlaceRoot(..)
                | ValueOrigin::FuncItem(..)
        )
}

fn value_deps_in_eval_order(origin: &ValueOrigin<'_>) -> Vec<ValueId> {
    match origin {
        ValueOrigin::Unary { inner, .. } => vec![*inner],
        ValueOrigin::Binary { lhs, rhs, .. } => vec![*lhs, *rhs],
        ValueOrigin::FieldPtr(field_ptr) => vec![field_ptr.base],
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            let mut deps = vec![place.base];
            for proj in place.projection.iter() {
                if let Projection::Index(IndexSource::Dynamic(value)) = proj {
                    deps.push(*value);
                }
            }
            deps
        }
        ValueOrigin::TransparentCast { value } => vec![*value],
        ValueOrigin::ConstRegion(_) => vec![],
        ValueOrigin::Expr(..)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::Synthetic(..)
        | ValueOrigin::Local(..)
        | ValueOrigin::PlaceRoot(..)
        | ValueOrigin::FuncItem(..) => Vec::new(),
    }
}

fn compute_value_use_counts<'db>(body: &MirBody<'db>) -> Vec<usize> {
    let mut counts = vec![0usize; body.values.len()];

    let mut bump = |value: ValueId| {
        if let Some(slot) = counts.get_mut(value.index()) {
            *slot += 1;
        }
    };

    for value in &body.values {
        for dep in value_deps_in_eval_order(&value.origin) {
            bump(dep);
        }
    }

    for block in &body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::BindValue { value, .. } => bump(*value),
                MirInst::Assign { rvalue, .. } => match rvalue {
                    Rvalue::Value(value) => bump(*value),
                    Rvalue::Call(call) => {
                        for arg in call.args.iter().chain(call.effect_args.iter()) {
                            bump(*arg);
                        }
                    }
                    Rvalue::Intrinsic { args, .. } => {
                        for arg in args {
                            bump(*arg);
                        }
                    }
                    Rvalue::Load { place } => {
                        bump(place.base);
                        bump_place_path(&mut bump, &place.projection);
                    }
                    Rvalue::Alloc { .. } | Rvalue::ZeroInit => {}
                },
                MirInst::Store { place, value, .. } => {
                    bump(place.base);
                    bump_place_path(&mut bump, &place.projection);
                    bump(*value);
                }
                MirInst::InitAggregate { place, inits, .. } => {
                    bump(place.base);
                    bump_place_path(&mut bump, &place.projection);
                    for (path, value) in inits {
                        bump_place_path(&mut bump, path);
                        bump(*value);
                    }
                }
                MirInst::SetDiscriminant { place, .. } => {
                    bump(place.base);
                    bump_place_path(&mut bump, &place.projection);
                }
            }
        }

        match &block.terminator {
            Terminator::Return {
                value: Some(value), ..
            } => bump(*value),
            Terminator::TerminatingCall { call, .. } => match call {
                TerminatingCall::Call(call) => {
                    for arg in call.args.iter().chain(call.effect_args.iter()) {
                        bump(*arg);
                    }
                }
                TerminatingCall::Intrinsic { args, .. } => {
                    for arg in args {
                        bump(*arg);
                    }
                }
            },
            Terminator::Branch { cond, .. } => bump(*cond),
            Terminator::Switch { discr, .. } => bump(*discr),
            Terminator::Return { value: None, .. }
            | Terminator::Goto { .. }
            | Terminator::Unreachable { .. } => {}
        }
    }

    counts
}

fn bump_place_path<'db>(bump: &mut impl FnMut(ValueId), path: &crate::ir::MirProjectionPath<'db>) {
    for proj in path.iter() {
        if let Projection::Index(IndexSource::Dynamic(value)) = proj {
            bump(*value);
        }
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_check::check_func_body;
    use hir::analysis::ty::ty_def::TyId;
    use url::Url;

    use crate::{
        MirInst,
        ir::{
            AddressSpaceKind, BasicBlock, LocalData, MirBody, MirProjectionPath, Place,
            PointerInfo, RuntimeShape, RuntimeWordKind, Rvalue, SourceInfoId, Terminator,
            ValueData, ValueOrigin, ValueRepr,
        },
        lower::{lower_function, lower_module},
    };

    fn lower_inline_module<'db>(
        db: &'db mut DriverDataBase,
        path: &str,
        src: &str,
    ) -> crate::MirModule<'db> {
        let url = Url::parse(path).expect("test url should be valid");
        let file = db.workspace().touch(db, url, Some(src.to_owned()));
        let top_mod = db.top_mod(file);
        lower_module(db, top_mod).expect("module should lower")
    }

    #[test]
    fn transparent_newtype_projection_keeps_non_memory_space() {
        let mut db = DriverDataBase::default();
        let url =
            Url::parse("file:///transparent_newtype_projection_keeps_non_memory_space.fe").unwrap();
        let src = r#"
struct Wrap {
    inner: u256,
}

pub fn transparent_newtype_projection_keeps_non_memory_space(w: mut Wrap) {}
"#;
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let module = lower_module(&db, top_mod).expect("module should lower");
        let wrap_fn = module
            .functions
            .iter()
            .find(|func| {
                func.symbol_name == "transparent_newtype_projection_keeps_non_memory_space"
            })
            .expect("function should exist");
        let wrap_ty = wrap_fn.body.local(wrap_fn.body.param_locals[0]).ty;
        let (_, wrap_inner_ty) = wrap_ty
            .as_capability(&db)
            .expect("parameter should be a capability type");
        let inner_ty = wrap_inner_ty.field_types(&db)[0];

        let mut body = MirBody::new();
        body.blocks.push(BasicBlock {
            insts: Vec::new(),
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });

        let wrap_local = body.alloc_local(LocalData {
            name: "w".to_string(),
            ty: wrap_inner_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Storage,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let base = body.alloc_value(ValueData {
            ty: wrap_inner_ty,
            origin: ValueOrigin::Local(wrap_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ref(AddressSpaceKind::Storage),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let value = body.alloc_value(ValueData {
            ty: inner_ty,
            origin: ValueOrigin::Synthetic(crate::ir::SyntheticValue::Int(1u8.into())),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        body.blocks[0].insts.push(crate::MirInst::Store {
            source: SourceInfoId::SYNTHETIC,
            place: Place::new(
                base,
                MirProjectionPath::from_projection(hir::projection::Projection::Field(0)),
            ),
            value,
        });

        super::canonicalize_transparent_newtypes(&db, &mut body);

        let crate::MirInst::Store { place, .. } = &body.blocks[0].insts[0] else {
            panic!("expected rewritten store");
        };
        assert!(
            place.projection.is_empty(),
            "transparent field projection should be peeled"
        );
        assert_eq!(
            body.value(place.base).repr.address_space(),
            Some(AddressSpaceKind::Storage),
            "peeled base must preserve non-memory address space",
        );
    }

    #[test]
    fn canonicalize_zero_sized_rewrites_place_values_over_unit_bases() {
        let db = DriverDataBase::default();
        let unit_ty = TyId::unit(&db);
        let mut body = MirBody::new();
        body.blocks.push(BasicBlock {
            insts: Vec::new(),
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });

        let local = body.alloc_local(LocalData {
            name: "zst".to_string(),
            ty: unit_ty,
            is_mut: false,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let root = body.alloc_value(ValueData {
            ty: unit_ty,
            origin: ValueOrigin::Local(local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let place = Place::new(root, MirProjectionPath::new());
        let place_ref = body.alloc_value(ValueData {
            ty: unit_ty,
            origin: ValueOrigin::PlaceRef(place.clone()),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let move_out = body.alloc_value(ValueData {
            ty: unit_ty,
            origin: ValueOrigin::MoveOut { place },
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        super::canonicalize_zero_sized(&db, &mut body);

        assert!(matches!(body.value(root).origin, ValueOrigin::Unit));
        assert!(matches!(body.value(place_ref).origin, ValueOrigin::Unit));
        assert!(matches!(body.value(move_out).origin, ValueOrigin::Unit));
    }

    #[test]
    fn runtime_abi_erases_compile_time_only_range_helper_params() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_abi_erases_compile_time_only_range_helper_params.fe",
            include_str!("../../codegen/tests/fixtures/range_bounds.fe"),
        );

        let helper = module
            .functions
            .iter()
            .find(|func| {
                func.symbol_name.contains("range_known_const")
                    && func.symbol_name.contains("len__0_4")
            })
            .expect("expected generated range helper");
        assert_eq!(
            helper.runtime_param_count(),
            0,
            "compile-time-only range helpers must not keep runtime params",
        );
        assert_eq!(
            helper.runtime_effect_param_count(),
            0,
            "compile-time-only range helpers must not keep runtime effect params",
        );
    }

    #[test]
    fn runtime_abi_rewrites_calls_to_erased_helpers() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_abi_rewrites_calls_to_erased_helpers.fe",
            include_str!("../../codegen/tests/fixtures/range_bounds.fe"),
        );

        let caller = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "sum_const")
            .expect("expected caller function");

        let call = caller
            .body
            .blocks
            .iter()
            .flat_map(|block| block.insts.iter())
            .find_map(|inst| match inst {
                crate::MirInst::Assign {
                    rvalue: Rvalue::Call(call),
                    ..
                } if call.resolved_name.as_deref().is_some_and(|name| {
                    name.contains("range_known_const") && name.contains("len__0_4")
                }) =>
                {
                    Some(call)
                }
                _ => None,
            })
            .expect("expected call to generated range helper");
        assert!(
            call.args.is_empty() && call.effect_args.is_empty(),
            "calls to erased helpers must be rewritten to the runtime ABI",
        );
    }

    #[test]
    fn runtime_abi_cleanup_removes_dead_zero_arg_create2_encoder_materialization() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_abi_cleanup_removes_dead_zero_arg_create2_encoder_materialization.fe",
            r#"
pub contract Counter {
    init() {}
}

#[test]
fn runtime_abi_cleanup_removes_dead_zero_arg_create2_encoder_materialization() uses (evm: mut Evm) {
    let addr = evm.create2<Counter>(value: 0, args: (), salt: 0)
    assert(addr.inner != 0)
}
"#,
        );

        let helper_body = module
            .functions
            .iter()
            .find(|func| {
                func.symbol_name.contains("create2_stor_arg0_root_stor")
                    && func.symbol_name.contains("Counter")
            })
            .expect("expected create2 helper")
            .body
            .clone();
        drop(module);

        let has_dead_materialization_chain = helper_body.blocks.iter().any(|block| {
            block.insts.iter().any(|inst| match inst {
                MirInst::Assign {
                    dest: Some(_),
                    rvalue: Rvalue::Alloc { .. },
                    ..
                } => true,
                MirInst::InitAggregate { .. } => true,
                MirInst::Store { .. }
                | MirInst::SetDiscriminant { .. }
                | MirInst::BindValue { .. }
                | MirInst::Assign { .. } => false,
            })
        });

        assert!(
            !has_dead_materialization_chain,
            "zero-arg create2 helpers should not retain dead alloc/init materialization chains after runtime ABI cleanup",
        );
    }

    #[test]
    fn runtime_abi_erases_contract_entry_params() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_abi_erases_contract_entry_params.fe",
            r#"
contract C:
    pub fn ping() -> u256:
        return 1
"#,
        );

        let entry = module
            .functions
            .iter()
            .find(|func| func.contract_function.is_some())
            .expect("expected contract entrypoint");
        assert_eq!(entry.runtime_param_count(), 0);
        assert_eq!(entry.runtime_effect_param_count(), 0);
    }

    #[test]
    fn runtime_shapes_preserve_pointer_effect_params() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_shapes_preserve_pointer_effect_params.fe",
            include_str!("../../codegen/tests/fixtures/pointer_field_aggregate.fe"),
        );

        assert!(
            module.functions.iter().any(|func| {
                func.symbol_name.contains("bump")
                    && func
                        .body
                        .effect_param_locals
                        .first()
                        .is_some_and(|effect_local| {
                            matches!(
                                func.body.local(*effect_local).runtime_shape,
                                RuntimeShape::MemoryPtr { target_ty: Some(_) }
                            )
                        })
            }),
            "expected at least one memory-specialized bump helper with a typed memory-pointer effect param",
        );
    }

    #[test]
    fn runtime_shapes_preserve_pointer_return_shapes() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_shapes_preserve_pointer_return_shapes.fe",
            include_str!("../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
        );

        let extract_ptr = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "extract_ptr")
            .expect("expected extract_ptr helper");
        let target_ty = match extract_ptr.runtime_return_shape {
            RuntimeShape::AddressWord(PointerInfo {
                address_space: AddressSpaceKind::Storage,
                target_ty,
            }) => target_ty,
            other => panic!("unexpected runtime return shape: {other:?}"),
        };
        assert!(
            target_ty.is_some(),
            "pointer return shape should retain pointee target metadata",
        );
    }

    #[test]
    fn runtime_shapes_resolve_storage_capability_return_shapes() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_shapes_resolve_storage_capability_return_shapes.fe",
            include_str!("../../fe/tests/fixtures/fe_test/mut_self_storage_receiver_regression.fe"),
        );

        let value_mut = module
            .functions
            .iter()
            .find(|func| func.symbol_name.contains("value_mut_stor_arg0_root_stor"))
            .expect("expected storage-specialized value_mut helper");
        let target_ty = match value_mut.runtime_return_shape {
            RuntimeShape::AddressWord(PointerInfo {
                address_space: AddressSpaceKind::Storage,
                target_ty,
            }) => target_ty,
            other => panic!("unexpected runtime return shape: {other:?}"),
        };
        assert!(
            target_ty.is_some(),
            "storage-backed capability returns must resolve from real return values and retain pointee metadata",
        );
    }

    #[test]
    fn runtime_shapes_recompute_storage_backed_wrapper_locals() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_shapes_recompute_storage_backed_wrapper_locals.fe",
            include_str!("../../codegen/tests/fixtures/effect_handle_field_deref.fe"),
        );

        let recv = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "__EffectHandleFieldDeref_recv_0_2")
            .expect("expected BumpMover receiver helper");
        let mover_local = recv
            .body
            .locals
            .iter()
            .find(|local| local.name == "mover")
            .expect("expected `mover` wrapper local");

        let target_ty = match mover_local.runtime_shape {
            RuntimeShape::AddressWord(PointerInfo {
                address_space: AddressSpaceKind::Storage,
                target_ty,
            }) => target_ty,
            other => panic!("unexpected wrapper local runtime shape: {other:?}"),
        };
        assert!(
            target_ty.is_some(),
            "storage-backed transparent wrapper locals must normalize to a storage address word with pointee metadata",
        );
    }

    #[test]
    fn runtime_shapes_preserve_alloc_backed_aug_assign_temporaries() {
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(
            &mut db,
            "file:///runtime_shapes_preserve_alloc_backed_aug_assign_temporaries.fe",
            include_str!("../../fe/tests/fixtures/fe_test/aug_assign_traits.fe"),
        );

        let func = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "add_assign_without_add_trait")
            .expect("expected add_assign_without_add_trait helper");
        let temp_local = func
            .body
            .locals
            .iter()
            .find(|local| local.name.starts_with("tmp_aug_assign_self"))
            .expect("expected aug-assign temp place local");

        let target_ty = match temp_local.runtime_shape {
            RuntimeShape::MemoryPtr { target_ty } => target_ty,
            other => panic!("unexpected aug-assign temp runtime shape: {other:?}"),
        };
        assert!(
            target_ty.is_some(),
            "alloc-backed aug-assign temp locals must normalize as typed memory pointers",
        );
    }

    #[test]
    fn runtime_shapes_preserve_scalar_param_shapes() {
        let src = include_str!("../../codegen/tests/fixtures/array_mut.fe");

        {
            let mut db = DriverDataBase::default();
            let url =
                Url::parse("file:///runtime_shapes_preserve_scalar_param_shapes_pre.fe").unwrap();
            let file = db.workspace().touch(&mut db, url, Some(src.to_owned()));
            let top_mod = db.top_mod(file);
            let hir_func = top_mod
                .all_funcs(&db)
                .iter()
                .copied()
                .find(|func| {
                    func.name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == "array_mut")
                })
                .expect("expected array_mut function");
            let source_arg_tys = hir_func.arg_tys(&db);
            let source_arg_ty = source_arg_tys[0].skip_binder();
            let (diags, typed_body) = check_func_body(&db, hir_func);
            assert!(diags.is_empty(), "unexpected diagnostics: {diags:#?}");
            let core = crate::CoreLib::new(&db, top_mod.scope());
            assert_eq!(
                crate::repr::runtime_shape_for_ty(
                    &db,
                    &core,
                    *source_arg_ty,
                    AddressSpaceKind::Memory,
                ),
                RuntimeShape::Word(RuntimeWordKind::I8),
                "source arg runtime shape should preserve the scalar width",
            );
            let lowered = lower_function(
                &db,
                hir_func,
                typed_body.clone(),
                None,
                Vec::new(),
                Vec::new(),
                Vec::new(),
            )
            .expect("array_mut should lower");
            assert_eq!(
                lowered
                    .body
                    .local(lowered.body.param_locals[0])
                    .runtime_shape,
                RuntimeShape::Unresolved,
                "raw lower_function output should leave local runtime shapes unresolved until normalization",
            );
        }

        {
            let mut db = DriverDataBase::default();
            let module = lower_inline_module(
                &mut db,
                "file:///runtime_shapes_preserve_scalar_param_shapes.fe",
                src,
            );

            let array_mut = module
                .functions
                .iter()
                .find(|func| func.symbol_name == "array_mut")
                .expect("expected array_mut function");
            let param_local = *array_mut
                .body
                .param_locals
                .first()
                .expect("array_mut should have one param");
            let local = array_mut.body.local(param_local);
            assert_eq!(
                local.runtime_shape,
                RuntimeShape::Word(RuntimeWordKind::I8),
                "scalar param runtime shape regressed after the full MIR pipeline",
            )
        }
    }

    #[test]
    fn zero_sized_contract_init_params_are_not_lowered_as_handler_params() {
        let src = r#"
msg ZeroMsg {
    #[selector = 0]
    Ping -> u256,
}

pub contract ZeroPayloadContractArgs {
    init(dummy: ()) {
        let kept = dummy
    }

    recv ZeroMsg {
        Ping -> u256 {
            7
        }
    }
}
"#;
        let mut db = DriverDataBase::default();
        let module = lower_inline_module(&mut db, "file:///zero_payload_contract_args.fe", src);

        let init_handler = module
            .functions
            .iter()
            .find(|func| func.symbol_name == "__ZeroPayloadContractArgs_init_contract")
            .expect("expected zero-payload init handler");
        assert!(
            init_handler.body.param_locals.is_empty(),
            "zero-sized init params should not remain in the lowered handler signature",
        );
        assert!(
            init_handler
                .body
                .locals
                .iter()
                .any(|local| local.name == "dummy"),
            "zero-sized init param should still be materialized as a body local when referenced",
        );
    }
}
