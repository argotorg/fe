use rustc_hash::FxHashMap;
use std::collections::VecDeque;

use crate::{
    LocalId, MirBody, MirInst, MirModule, Rvalue, TerminatingCall, Terminator, ValueId,
    ValueOrigin,
    analysis::{borrowck::BorrowSummary, build_call_graph},
    ir::{AddressSpaceKind, IntrinsicOp, Place},
};
use hir::{
    analysis::HirAnalysisDb,
    projection::{IndexSource, Projection},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MirPtrEscapeSummary {
    /// Parameter may escape via side effects in the callee body.
    ///
    /// Return-flow is tracked separately via `arg_may_be_returned` and propagated at callers
    /// through value dependency analysis.
    pub arg_may_escape: Vec<bool>,
    pub arg_may_be_returned: Vec<bool>,
    pub local_alloc_may_escape: Vec<bool>,
}

impl MirPtrEscapeSummary {
    fn new(arg_count: usize, local_count: usize) -> Self {
        Self {
            arg_may_escape: vec![false; arg_count],
            arg_may_be_returned: vec![false; arg_count],
            local_alloc_may_escape: vec![false; local_count],
        }
    }
}

pub type MirPtrEscapeSummaryMap = FxHashMap<String, MirPtrEscapeSummary>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum EscapeRoute {
    IncludeReturns,
    ExcludeReturns,
}

pub fn compute_ptr_escape_summaries<'db>(
    db: &'db dyn HirAnalysisDb,
    mir: &MirModule<'db>,
) -> MirPtrEscapeSummaryMap {
    let mut summaries: MirPtrEscapeSummaryMap = FxHashMap::default();
    let borrow_summaries =
        crate::analysis::borrowck::compute_borrow_summaries(db, &mir.functions).ok();

    for func in &mir.functions {
        if func.symbol_name.is_empty() {
            continue;
        }

        let arg_count = function_arg_locals(&func.body).len();
        summaries.insert(
            func.symbol_name.clone(),
            MirPtrEscapeSummary::new(arg_count, func.body.locals.len()),
        );
    }

    let symbol_to_idx: FxHashMap<String, usize> = mir
        .functions
        .iter()
        .enumerate()
        .filter(|(_, func)| !func.symbol_name.is_empty())
        .map(|(idx, func)| (func.symbol_name.clone(), idx))
        .collect();

    let call_graph = build_call_graph(&mir.functions);
    let mut callers_by_callee = vec![Vec::new(); mir.functions.len()];
    for (caller, callees) in call_graph {
        let Some(&caller_idx) = symbol_to_idx.get(&caller) else {
            continue;
        };
        for callee in callees {
            if let Some(&callee_idx) = symbol_to_idx.get(&callee) {
                callers_by_callee[callee_idx].push(caller_idx);
            }
        }
    }

    for callers in &mut callers_by_callee {
        callers.sort_unstable();
        callers.dedup();
    }

    let mut worklist: VecDeque<usize> = mir
        .functions
        .iter()
        .enumerate()
        .filter(|(_, func)| !func.symbol_name.is_empty())
        .map(|(idx, _)| idx)
        .collect();
    let mut in_worklist = vec![false; mir.functions.len()];
    for &idx in &worklist {
        in_worklist[idx] = true;
    }

    while let Some(func_idx) = worklist.pop_front() {
        in_worklist[func_idx] = false;
        let func = &mir.functions[func_idx];
        if func.symbol_name.is_empty() {
            continue;
        }

        let next = compute_ptr_escape_summary_for_function(
            &func.body,
            &summaries,
            borrow_summaries
                .as_ref()
                .and_then(|map| map.get(&func.symbol_name)),
        );
        if summaries.get(&func.symbol_name) != Some(&next) {
            summaries.insert(func.symbol_name.clone(), next);
            for &caller_idx in &callers_by_callee[func_idx] {
                if !in_worklist[caller_idx] {
                    in_worklist[caller_idx] = true;
                    worklist.push_back(caller_idx);
                }
            }
        }
    }

    summaries
}

fn compute_ptr_escape_summary_for_function<'db>(
    body: &MirBody<'db>,
    summaries: &MirPtrEscapeSummaryMap,
    borrow_summary: Option<&BorrowSummary<'db>>,
) -> MirPtrEscapeSummary {
    let args = function_arg_locals(body);
    let mut out = MirPtrEscapeSummary::new(args.len(), body.locals.len());

    for (idx, local) in args.iter().copied().enumerate() {
        out.arg_may_escape[idx] =
            local_may_escape(body, local, summaries, EscapeRoute::ExcludeReturns);
        out.arg_may_be_returned[idx] = borrow_summary
            .map(|summary| arg_is_returned_by_borrow_summary(summary, idx))
            .unwrap_or_else(|| local_may_be_returned(body, local, summaries));
    }

    for local_idx in 0..body.locals.len() {
        let local = LocalId(local_idx as u32);
        if local_has_memory_alloc(body, local) {
            out.local_alloc_may_escape[local_idx] =
                local_may_escape(body, local, summaries, EscapeRoute::IncludeReturns);
        }
    }

    out
}

fn arg_is_returned_by_borrow_summary(summary: &BorrowSummary<'_>, arg_index: usize) -> bool {
    summary
        .iter()
        .any(|transform| transform.param_index as usize == arg_index)
}

fn function_arg_locals<'db>(body: &MirBody<'db>) -> Vec<LocalId> {
    body.param_locals
        .iter()
        .chain(body.effect_param_locals.iter())
        .copied()
        .collect()
}

fn local_has_memory_alloc(body: &MirBody<'_>, local: LocalId) -> bool {
    for block in &body.blocks {
        for inst in &block.insts {
            if let MirInst::Assign {
                dest: Some(dest),
                rvalue,
                ..
            } = inst
                && *dest == local
                && rvalue_has_memory_alloc_source(rvalue)
            {
                return true;
            }
        }
    }
    false
}

fn rvalue_has_memory_alloc_source(rvalue: &Rvalue<'_>) -> bool {
    match rvalue {
        Rvalue::Alloc {
            address_space: AddressSpaceKind::Memory,
        } => true,
        Rvalue::Intrinsic {
            op: IntrinsicOp::Alloc,
            ..
        } => true,
        Rvalue::Call(call) => {
            call.effect_args.is_empty() && call.resolved_name.as_deref() == Some("alloc")
        }
        _ => false,
    }
}

struct LocalDependencyState {
    value_memo: Vec<Option<bool>>,
    value_visiting: Vec<bool>,
    local_memo: Vec<Option<bool>>,
    local_visiting: Vec<bool>,
}

impl LocalDependencyState {
    fn new<'db>(body: &MirBody<'db>) -> Self {
        Self {
            value_memo: vec![None; body.values.len()],
            value_visiting: vec![false; body.values.len()],
            local_memo: vec![None; body.locals.len()],
            local_visiting: vec![false; body.locals.len()],
        }
    }
}

fn local_may_be_returned<'db>(
    body: &MirBody<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
) -> bool {
    let mut state = LocalDependencyState::new(body);

    for block in &body.blocks {
        if let Terminator::Return {
            value: Some(returned),
            ..
        } = &block.terminator
            && value_can_carry_pointer(body, *returned)
            && value_depends_on_local(body, *returned, local, ptr_escape_summaries, &mut state)
        {
            return true;
        }
    }

    false
}

fn local_may_escape<'db>(
    body: &MirBody<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    escape_route: EscapeRoute,
) -> bool {
    let mut state = LocalDependencyState::new(body);

    for block in &body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::Assign { dest, rvalue, .. } => {
                    if rvalue_may_escape_local(
                        body,
                        *dest,
                        rvalue,
                        local,
                        ptr_escape_summaries,
                        &mut state,
                    ) {
                        return true;
                    }
                }
                MirInst::Store { place, value, .. } => {
                    if value_depends_on_local(body, *value, local, ptr_escape_summaries, &mut state)
                        && store_target_is_non_local(
                            body,
                            place,
                            local,
                            ptr_escape_summaries,
                            &mut state,
                        )
                    {
                        return true;
                    }
                }
                MirInst::InitAggregate { place, inits, .. } => {
                    for (_, value) in inits {
                        if value_depends_on_local(
                            body,
                            *value,
                            local,
                            ptr_escape_summaries,
                            &mut state,
                        ) && store_target_is_non_local(
                            body,
                            place,
                            local,
                            ptr_escape_summaries,
                            &mut state,
                        ) {
                            return true;
                        }
                    }
                }
                MirInst::SetDiscriminant { .. } | MirInst::BindValue { .. } => {}
            }
        }

        if terminator_may_escape_local(
            body,
            &block.terminator,
            local,
            ptr_escape_summaries,
            &mut state,
            escape_route,
        ) {
            return true;
        }
    }

    false
}

fn value_depends_on_local<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
) -> bool {
    if let Some(cached) = state.value_memo[value.index()] {
        return cached;
    }
    if state.value_visiting[value.index()] {
        // Conservatively treat recursive value dependency as escaping.
        return true;
    }

    state.value_visiting[value.index()] = true;
    let depends = match &body.value(value).origin {
        ValueOrigin::Local(dep_local) | ValueOrigin::PlaceRoot(dep_local) => {
            local_depends_on_local(body, *dep_local, local, ptr_escape_summaries, state)
        }
        ValueOrigin::Unary { inner, .. } => {
            value_depends_on_local(body, *inner, local, ptr_escape_summaries, state)
        }
        ValueOrigin::Binary { lhs, rhs, .. } => {
            value_depends_on_local(body, *lhs, local, ptr_escape_summaries, state)
                || value_depends_on_local(body, *rhs, local, ptr_escape_summaries, state)
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            value_depends_on_local(body, field_ptr.base, local, ptr_escape_summaries, state)
        }
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            let mut depends =
                value_depends_on_local(body, place.base, local, ptr_escape_summaries, state);
            if !depends {
                for projection in place.projection.iter() {
                    if let Projection::Index(IndexSource::Dynamic(index_val)) = projection
                        && value_depends_on_local(
                            body,
                            *index_val,
                            local,
                            ptr_escape_summaries,
                            state,
                        )
                    {
                        depends = true;
                        break;
                    }
                }
            }
            depends
        }
        ValueOrigin::TransparentCast { value } => {
            value_depends_on_local(body, *value, local, ptr_escape_summaries, state)
        }
        ValueOrigin::Expr(_)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::Synthetic(_)
        | ValueOrigin::FuncItem(_) => false,
    };
    state.value_visiting[value.index()] = false;
    state.value_memo[value.index()] = Some(depends);
    depends
}

fn rvalue_may_escape_local<'db>(
    body: &MirBody<'db>,
    _dest_local: Option<LocalId>,
    rvalue: &Rvalue<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
) -> bool {
    match rvalue {
        Rvalue::Call(call) => {
            let arg_escape_mask =
                call_escape_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
            if call_args_depend_on_local_with_mask(
                body,
                &call.args,
                local,
                ptr_escape_summaries,
                arg_escape_mask,
                0,
                state,
            ) || call_args_depend_on_local_with_mask(
                body,
                &call.effect_args,
                local,
                ptr_escape_summaries,
                arg_escape_mask,
                call.args.len(),
                state,
            ) {
                return true;
            }
            false
        }
        Rvalue::Intrinsic { op, args } => args.iter().copied().enumerate().any(|(idx, value)| {
            intrinsic_arg_may_escape(*op, idx)
                && value_depends_on_local(body, value, local, ptr_escape_summaries, state)
        }),
        Rvalue::ZeroInit | Rvalue::Value(_) | Rvalue::Load { .. } | Rvalue::Alloc { .. } => false,
    }
}

fn terminator_may_escape_local<'db>(
    body: &MirBody<'db>,
    terminator: &Terminator<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
    escape_route: EscapeRoute,
) -> bool {
    match terminator {
        Terminator::Return {
            value: Some(returned),
            ..
        } => {
            escape_route == EscapeRoute::IncludeReturns
                && value_can_carry_pointer(body, *returned)
                && value_depends_on_local(body, *returned, local, ptr_escape_summaries, state)
        }
        Terminator::TerminatingCall { call, .. } => match call {
            TerminatingCall::Call(call) => {
                let arg_escape_mask =
                    call_escape_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
                call_args_depend_on_local_with_mask(
                    body,
                    &call.args,
                    local,
                    ptr_escape_summaries,
                    arg_escape_mask,
                    0,
                    state,
                ) || call_args_depend_on_local_with_mask(
                    body,
                    &call.effect_args,
                    local,
                    ptr_escape_summaries,
                    arg_escape_mask,
                    call.args.len(),
                    state,
                )
            }
            TerminatingCall::Intrinsic { op, args } => {
                args.iter().copied().enumerate().any(|(idx, value)| {
                    intrinsic_arg_may_escape(*op, idx)
                        && value_depends_on_local(body, value, local, ptr_escape_summaries, state)
                })
            }
        },
        Terminator::Return { .. }
        | Terminator::Goto { .. }
        | Terminator::Branch { .. }
        | Terminator::Switch { .. }
        | Terminator::Unreachable { .. } => false,
    }
}

fn value_can_carry_pointer(body: &MirBody<'_>, value: ValueId) -> bool {
    body.value(value).repr.address_space().is_some()
}

fn intrinsic_arg_may_escape(op: IntrinsicOp, arg_idx: usize) -> bool {
    match op {
        IntrinsicOp::Mstore | IntrinsicOp::Mstore8 | IntrinsicOp::Sstore => arg_idx == 1,
        IntrinsicOp::ReturnData | IntrinsicOp::Revert => arg_idx == 0,
        IntrinsicOp::Mload
        | IntrinsicOp::Calldataload
        | IntrinsicOp::Calldatacopy
        | IntrinsicOp::Calldatasize
        | IntrinsicOp::Returndatacopy
        | IntrinsicOp::Returndatasize
        | IntrinsicOp::AddrOf
        | IntrinsicOp::Sload
        | IntrinsicOp::Codecopy
        | IntrinsicOp::Codesize
        | IntrinsicOp::CodeRegionOffset
        | IntrinsicOp::CodeRegionLen
        | IntrinsicOp::Keccak
        | IntrinsicOp::Caller
        | IntrinsicOp::Alloc => false,
    }
}

fn store_target_is_non_local<'db>(
    body: &MirBody<'db>,
    place: &Place<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
) -> bool {
    if !matches!(body.place_address_space(place), AddressSpaceKind::Memory) {
        return true;
    }
    !value_depends_on_local(body, place.base, local, ptr_escape_summaries, state)
}

fn values_depend_on_local<'db>(
    body: &MirBody<'db>,
    values: &[ValueId],
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
) -> bool {
    values
        .iter()
        .copied()
        .any(|value| value_depends_on_local(body, value, local, ptr_escape_summaries, state))
}

fn local_depends_on_local<'db>(
    body: &MirBody<'db>,
    candidate_local: LocalId,
    source_local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
) -> bool {
    if candidate_local == source_local {
        return true;
    }
    if let Some(cached) = state.local_memo[candidate_local.index()] {
        return cached;
    }
    if state.local_visiting[candidate_local.index()] {
        // Conservatively treat recursive local dependency as escaping.
        return true;
    }

    state.local_visiting[candidate_local.index()] = true;
    let mut depends = false;
    for block in &body.blocks {
        for inst in &block.insts {
            match inst {
                MirInst::Assign {
                    dest: Some(dest_local),
                    rvalue,
                    ..
                } if *dest_local == candidate_local => {
                    if rvalue_depends_on_local_value(
                        body,
                        rvalue,
                        candidate_local,
                        source_local,
                        ptr_escape_summaries,
                        state,
                    ) {
                        depends = true;
                        break;
                    }
                }
                MirInst::Assign { dest: Some(_), .. } => {}
                MirInst::Assign { dest: None, .. } => {}
                MirInst::Store { place, value, .. } => {
                    if place_targets_local(
                        body,
                        place,
                        candidate_local,
                        ptr_escape_summaries,
                        state,
                    ) && value_depends_on_local(
                        body,
                        *value,
                        source_local,
                        ptr_escape_summaries,
                        state,
                    ) {
                        depends = true;
                        break;
                    }
                }
                MirInst::InitAggregate { place, inits, .. } => {
                    if !place_targets_local(
                        body,
                        place,
                        candidate_local,
                        ptr_escape_summaries,
                        state,
                    ) {
                        continue;
                    }
                    if inits.iter().any(|(_, value)| {
                        value_depends_on_local(
                            body,
                            *value,
                            source_local,
                            ptr_escape_summaries,
                            state,
                        )
                    }) {
                        depends = true;
                        break;
                    }
                }
                MirInst::SetDiscriminant { .. } | MirInst::BindValue { .. } => {}
            }
        }
        if depends {
            break;
        }
    }

    state.local_visiting[candidate_local.index()] = false;
    state.local_memo[candidate_local.index()] = Some(depends);
    depends
}

fn place_targets_local<'db>(
    body: &MirBody<'db>,
    place: &Place<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
) -> bool {
    matches!(body.place_address_space(place), AddressSpaceKind::Memory)
        && value_depends_on_local(body, place.base, local, ptr_escape_summaries, state)
}

fn rvalue_depends_on_local_value<'db>(
    body: &MirBody<'db>,
    rvalue: &Rvalue<'db>,
    candidate_local: LocalId,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    state: &mut LocalDependencyState,
) -> bool {
    match rvalue {
        Rvalue::ZeroInit | Rvalue::Alloc { .. } => false,
        Rvalue::Value(value) => {
            value_depends_on_local(body, *value, local, ptr_escape_summaries, state)
        }
        Rvalue::Call(call) => {
            let arg_return_mask =
                call_return_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
            call_args_depend_on_local_with_mask(
                body,
                &call.args,
                local,
                ptr_escape_summaries,
                arg_return_mask,
                0,
                state,
            ) || call_args_depend_on_local_with_mask(
                body,
                &call.effect_args,
                local,
                ptr_escape_summaries,
                arg_return_mask,
                call.args.len(),
                state,
            )
        }
        Rvalue::Intrinsic { args, .. } => {
            values_depend_on_local(body, args, local, ptr_escape_summaries, state)
        }
        Rvalue::Load { place } => {
            if !local_can_carry_pointer(body, candidate_local) {
                return false;
            }
            value_depends_on_local(body, place.base, local, ptr_escape_summaries, state)
        }
    }
}

fn local_can_carry_pointer(body: &MirBody<'_>, local: LocalId) -> bool {
    body.values.iter().any(|value| {
        value.repr.address_space().is_some()
            && matches!(value.origin, ValueOrigin::Local(origin) | ValueOrigin::PlaceRoot(origin) if origin == local)
    })
}

fn call_escape_arg_mask<'a>(
    callee_name: Option<&str>,
    ptr_escape_summaries: &'a MirPtrEscapeSummaryMap,
) -> Option<&'a [bool]> {
    callee_name
        .and_then(|name| ptr_escape_summaries.get(name))
        .map(|summary| summary.arg_may_escape.as_slice())
}

fn call_return_arg_mask<'a>(
    callee_name: Option<&str>,
    ptr_escape_summaries: &'a MirPtrEscapeSummaryMap,
) -> Option<&'a [bool]> {
    callee_name
        .and_then(|name| ptr_escape_summaries.get(name))
        .map(|summary| summary.arg_may_be_returned.as_slice())
}

fn call_args_depend_on_local_with_mask<'db>(
    body: &MirBody<'db>,
    values: &[ValueId],
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    arg_mask: Option<&[bool]>,
    arg_offset: usize,
    state: &mut LocalDependencyState,
) -> bool {
    values.iter().copied().enumerate().any(|(index, value)| {
        if let Some(mask) = arg_mask
            && !mask.get(arg_offset + index).copied().unwrap_or(true)
        {
            return false;
        }

        value_depends_on_local(body, value, local, ptr_escape_summaries, state)
    })
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
    use num_bigint::BigUint;
    use url::Url;

    use crate::{
        MirFunction, MirInst, Rvalue,
        analysis::escape::compute_ptr_escape_summaries,
        ir::{
            AddressSpaceKind, BasicBlock, CallOrigin, LocalData, Place, SourceInfoId, Terminator,
            ValueData, ValueOrigin, ValueRepr,
        },
    };

    fn mutate_function<'db>(func: &mut MirFunction<'db>, ret_alloc: bool, db: &'db DriverDataBase) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let local = func.body.alloc_local(LocalData {
            name: "tmp".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
        });
        let local_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
        });

        func.body.push_block(BasicBlock {
            insts: vec![MirInst::Assign {
                source: SourceInfoId::SYNTHETIC,
                dest: Some(local),
                rvalue: Rvalue::Alloc {
                    address_space: AddressSpaceKind::Memory,
                },
            }],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: ret_alloc.then_some(local_value),
            },
        });
    }

    fn mutate_id_function<'db>(func: &mut MirFunction<'db>, db: &'db DriverDataBase) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let param = func.body.alloc_local(LocalData {
            name: "param".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
        });
        func.body.param_locals.push(param);
        let param_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(param),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
        });

        func.body.push_block(BasicBlock {
            insts: vec![],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(param_value),
            },
        });
    }

    fn mutate_caller_function<'db>(func: &mut MirFunction<'db>, db: &'db DriverDataBase) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let alloc_local = func.body.alloc_local(LocalData {
            name: "tmp".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
        });

        func.body.push_block(BasicBlock {
            insts: vec![
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(alloc_local),
                    rvalue: Rvalue::Alloc {
                        address_space: AddressSpaceKind::Memory,
                    },
                },
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: None,
                    rvalue: Rvalue::Call(CallOrigin {
                        expr: None,
                        hir_target: None,
                        args: vec![alloc_value],
                        effect_args: vec![],
                        resolved_name: Some("escape_id".to_string()),
                        receiver_space: None,
                    }),
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });
    }

    fn mutate_alloc_call_function<'db>(
        func: &mut MirFunction<'db>,
        ret_alloc: bool,
        db: &'db DriverDataBase,
    ) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let local = func.body.alloc_local(LocalData {
            name: "tmp_alloc".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
        });
        let local_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
        });
        let size_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Synthetic(crate::ir::SyntheticValue::Int(BigUint::from(32u64))),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
        });

        func.body.push_block(BasicBlock {
            insts: vec![MirInst::Assign {
                source: SourceInfoId::SYNTHETIC,
                dest: Some(local),
                rvalue: Rvalue::Call(CallOrigin {
                    expr: None,
                    hir_target: None,
                    args: vec![size_value],
                    effect_args: vec![],
                    resolved_name: Some("alloc".to_string()),
                    receiver_space: None,
                }),
            }],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: ret_alloc.then_some(local_value),
            },
        });
    }

    fn mutate_scalar_return_function<'db>(func: &mut MirFunction<'db>, db: &'db DriverDataBase) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let alloc_local = func.body.alloc_local(LocalData {
            name: "tmp_alloc".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
        });

        let loaded_local = func.body.alloc_local(LocalData {
            name: "loaded".to_string(),
            ty: u256_ty,
            is_mut: false,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
        });
        let loaded_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(loaded_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
        });

        func.body.push_block(BasicBlock {
            insts: vec![
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(alloc_local),
                    rvalue: Rvalue::Alloc {
                        address_space: AddressSpaceKind::Memory,
                    },
                },
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(loaded_local),
                    rvalue: Rvalue::Load {
                        place: Place::new(alloc_value, crate::MirProjectionPath::new()),
                    },
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(loaded_value),
            },
        });
    }

    #[test]
    fn returned_alloc_is_marked_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_return.fe").unwrap();
        let src = "pub fn escape_return() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_return")
            .expect("function should exist");
        mutate_function(func, true, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_return")
            .expect("summary should exist");
        assert!(
            summary.local_alloc_may_escape[0],
            "returned local alloc should be marked escaping"
        );
    }

    #[test]
    fn unused_alloc_is_not_marked_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_unused.fe").unwrap();
        let src = "pub fn escape_unused() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_unused")
            .expect("function should exist");
        mutate_function(func, false, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_unused")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[0],
            "unused local alloc should stay non-escaping"
        );
    }

    #[test]
    fn intrinsic_escape_masks_match_expected_roles() {
        assert!(super::intrinsic_arg_may_escape(
            super::IntrinsicOp::Mstore,
            1
        ));
        assert!(!super::intrinsic_arg_may_escape(
            super::IntrinsicOp::Mstore,
            0
        ));
        assert!(super::intrinsic_arg_may_escape(
            super::IntrinsicOp::ReturnData,
            0
        ));
        assert!(!super::intrinsic_arg_may_escape(
            super::IntrinsicOp::ReturnData,
            1
        ));
        assert!(!super::intrinsic_arg_may_escape(
            super::IntrinsicOp::Mload,
            0
        ));
    }

    #[test]
    fn call_that_only_returns_argument_does_not_mark_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_reborrow_call.fe").unwrap();
        let src = "pub fn escape_id(value: u256) -> u256 { value } pub fn escape_caller() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let id_func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_id")
            .expect("id function should exist");
        mutate_id_function(id_func, &db);

        let caller_func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_caller")
            .expect("caller function should exist");
        mutate_caller_function(caller_func, &db);

        let summaries = compute_ptr_escape_summaries(&db, &module);
        let id_summary = summaries.get("escape_id").expect("id summary should exist");
        assert!(
            !id_summary.arg_may_escape[0],
            "returned-only arguments should not count as side-effect escapes"
        );

        let caller_summary = summaries
            .get("escape_caller")
            .expect("caller summary should exist");
        assert!(
            !caller_summary.local_alloc_may_escape[0],
            "unused call result should not force caller alloc to escape"
        );
    }

    #[test]
    fn scalar_return_from_alloc_does_not_mark_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_scalar_from_alloc.fe").unwrap();
        let src = "pub fn escape_scalar_from_alloc() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_scalar_from_alloc")
            .expect("function should exist");
        mutate_scalar_return_function(func, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_scalar_from_alloc")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[0],
            "scalar return should not mark local alloc as escaping"
        );
    }

    #[test]
    fn returned_alloc_call_is_marked_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_returned_alloc_call.fe").unwrap();
        let src = "pub fn escape_returned_alloc_call() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_returned_alloc_call")
            .expect("function should exist");
        mutate_alloc_call_function(func, true, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_returned_alloc_call")
            .expect("summary should exist");
        assert!(
            summary.local_alloc_may_escape[0],
            "returned alloc call local should be marked escaping"
        );
    }

    #[test]
    fn unused_alloc_call_is_not_marked_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_unused_alloc_call.fe").unwrap();
        let src = "pub fn escape_unused_alloc_call() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_unused_alloc_call")
            .expect("function should exist");
        mutate_alloc_call_function(func, false, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_unused_alloc_call")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[0],
            "unused alloc call local should stay non-escaping"
        );
    }
}
