use rustc_hash::FxHashMap;
use std::collections::VecDeque;

use crate::{
    LocalId, MirBody, MirInst, MirModule, Rvalue, TerminatingCall, Terminator, ValueId,
    ValueOrigin,
    analysis::{borrowck::BorrowSummary, build_call_graph},
    ir::{AddressSpaceKind, BasicBlockId, IntrinsicOp, Place},
};
use hir::{
    analysis::HirAnalysisDb,
    projection::{IndexSource, Projection},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MirPtrEscapeSummary {
    /// Parameter cannot stay in a caller-local stack allocation.
    ///
    /// This includes side-effect escape in the callee body and any cross-frame use at call sites.
    /// Return-flow is tracked separately via `arg_may_be_returned` and propagated at callers
    /// through value dependency analysis.
    pub arg_may_escape: Vec<bool>,
    pub arg_may_be_returned: Vec<bool>,
    pub arg_value_may_escape: Vec<bool>,
    pub local_alloc_may_escape: Vec<bool>,
}

impl MirPtrEscapeSummary {
    fn new(arg_count: usize, local_count: usize) -> Self {
        Self {
            arg_may_escape: vec![false; arg_count],
            arg_may_be_returned: vec![false; arg_count],
            arg_value_may_escape: vec![false; arg_count],
            local_alloc_may_escape: vec![false; local_count],
        }
    }
}

pub type MirPtrEscapeSummaryMap = FxHashMap<String, MirPtrEscapeSummary>;

#[derive(Clone)]
struct LocalEscapeInfo {
    direct_side_effect: bool,
    direct_return: bool,
    edges: Vec<LocalId>,
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

    let call_graph = build_call_graph(db, &mir.functions);
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
    let alloc_flags: Vec<bool> = body
        .locals
        .iter()
        .enumerate()
        .map(|(idx, _)| local_has_memory_alloc(body, LocalId(idx as u32)))
        .collect();
    let must_alias_in_states = compute_must_alias_in_states(body, &alloc_flags);
    let mut alloc_direct_side_effect = vec![false; body.locals.len()];
    let mut alloc_direct_return = vec![false; body.locals.len()];
    let mut alloc_edges: Vec<Vec<LocalId>> = vec![Vec::new(); body.locals.len()];

    for (local_idx, is_alloc) in alloc_flags.iter().copied().enumerate() {
        if !is_alloc {
            continue;
        }
        let local = LocalId(local_idx as u32);
        let info = local_escape_info(body, local, summaries, &must_alias_in_states, &alloc_flags);
        alloc_direct_side_effect[local_idx] = info.direct_side_effect;
        alloc_direct_return[local_idx] = info.direct_return;
        alloc_edges[local_idx] = info.edges;
    }

    let mut reverse_edges: Vec<Vec<LocalId>> = vec![Vec::new(); body.locals.len()];
    for (source_idx, edges) in alloc_edges.iter().enumerate() {
        if !alloc_flags[source_idx] {
            continue;
        }
        let source = LocalId(source_idx as u32);
        for target in edges {
            reverse_edges[target.index()].push(source);
        }
    }

    let mut alloc_escape_side_effect = vec![false; body.locals.len()];
    let mut queue: VecDeque<LocalId> = VecDeque::new();
    for (local_idx, is_alloc) in alloc_flags.iter().copied().enumerate() {
        if !is_alloc || !alloc_direct_side_effect[local_idx] {
            continue;
        }
        alloc_escape_side_effect[local_idx] = true;
        queue.push_back(LocalId(local_idx as u32));
    }

    while let Some(escaped) = queue.pop_front() {
        for pred in &reverse_edges[escaped.index()] {
            if alloc_escape_side_effect[pred.index()] {
                continue;
            }
            alloc_escape_side_effect[pred.index()] = true;
            queue.push_back(*pred);
        }
    }

    let mut queue: VecDeque<LocalId> = VecDeque::new();
    for (local_idx, is_alloc) in alloc_flags.iter().copied().enumerate() {
        if !is_alloc || !(alloc_direct_side_effect[local_idx] || alloc_direct_return[local_idx]) {
            continue;
        }
        out.local_alloc_may_escape[local_idx] = true;
        queue.push_back(LocalId(local_idx as u32));
    }

    while let Some(escaped) = queue.pop_front() {
        for pred in &reverse_edges[escaped.index()] {
            if out.local_alloc_may_escape[pred.index()] {
                continue;
            }
            out.local_alloc_may_escape[pred.index()] = true;
            queue.push_back(*pred);
        }
    }

    for (idx, local) in args.iter().copied().enumerate() {
        let info = local_escape_info(body, local, summaries, &must_alias_in_states, &alloc_flags);
        let escapes_via_alloc = info
            .edges
            .iter()
            .any(|target| alloc_escape_side_effect[target.index()]);
        out.arg_may_escape[idx] = info.direct_side_effect || escapes_via_alloc;
        out.arg_may_be_returned[idx] = borrow_summary
            .is_some_and(|summary| arg_is_returned_by_borrow_summary(summary, idx))
            || local_may_be_returned(body, local, summaries);
        let value_info =
            local_value_escape_info(body, local, summaries, &must_alias_in_states, &alloc_flags);
        out.arg_value_may_escape[idx] = value_info.direct_side_effect || value_info.direct_return;
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
    origin_visiting: Vec<bool>,
    value_visiting: Vec<bool>,
}

impl LocalDependencyState {
    fn new<'db>(body: &MirBody<'db>) -> Self {
        Self {
            origin_visiting: vec![false; body.values.len()],
            value_visiting: vec![false; body.values.len()],
        }
    }
}

fn terminator_successors(term: &Terminator<'_>) -> Vec<BasicBlockId> {
    match term {
        Terminator::Goto { target, .. } => vec![*target],
        Terminator::Branch {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        Terminator::Switch {
            targets, default, ..
        } => targets
            .iter()
            .map(|target| target.block)
            .chain(std::iter::once(*default))
            .collect(),
        Terminator::Return { .. }
        | Terminator::TerminatingCall { .. }
        | Terminator::Unreachable { .. } => Vec::new(),
    }
}

fn compute_must_alias_in_states<'db>(
    body: &MirBody<'db>,
    alloc_flags: &[bool],
) -> Vec<Vec<Option<LocalId>>> {
    let local_count = body.locals.len();
    let block_count = body.blocks.len();
    if block_count == 0 {
        return Vec::new();
    }

    let mut preds = vec![Vec::new(); block_count];
    for (idx, block) in body.blocks.iter().enumerate() {
        let from = BasicBlockId(idx as u32);
        for succ in terminator_successors(&block.terminator) {
            preds[succ.index()].push(from);
        }
    }

    let mut in_states = vec![vec![None; local_count]; block_count];
    let mut out_states = vec![vec![None; local_count]; block_count];
    in_states[0] = vec![None; local_count];

    let mut worklist: VecDeque<_> = body
        .blocks
        .iter()
        .enumerate()
        .map(|(idx, _)| BasicBlockId(idx as u32))
        .collect();
    let mut value_visiting = vec![false; body.values.len()];

    while let Some(block_id) = worklist.pop_front() {
        let mut state = in_states[block_id.index()].clone();
        let block = &body.blocks[block_id.index()];
        for inst in &block.insts {
            if let MirInst::Assign {
                dest: Some(dest_local),
                rvalue,
                ..
            } = inst
            {
                let must_alias = rvalue_must_alias_local_alloc(
                    body,
                    rvalue,
                    *dest_local,
                    &state,
                    alloc_flags,
                    &mut value_visiting,
                );
                state[dest_local.index()] = must_alias;
            }
        }

        if state == out_states[block_id.index()] {
            continue;
        }

        out_states[block_id.index()] = state;
        for succ in terminator_successors(&block.terminator) {
            if succ.index() == 0 {
                continue;
            }
            let mut new_in = Vec::new();
            for (pred_idx, pred) in preds[succ.index()].iter().enumerate() {
                let pred_state = &out_states[pred.index()];
                if pred_idx == 0 {
                    new_in = pred_state.clone();
                    continue;
                }
                for (slot, pred_alias) in new_in.iter_mut().zip(pred_state.iter()) {
                    if slot.as_ref() != pred_alias.as_ref() {
                        *slot = None;
                    }
                }
            }
            if new_in.is_empty() {
                new_in = vec![None; local_count];
            }
            if new_in != in_states[succ.index()] {
                in_states[succ.index()] = new_in;
                worklist.push_back(succ);
            }
        }
    }

    in_states
}

fn initial_local_dependency_state(body: &MirBody<'_>, source_local: LocalId) -> Vec<bool> {
    let mut state = vec![false; body.locals.len()];
    if body.param_locals.contains(&source_local) || body.effect_param_locals.contains(&source_local)
    {
        state[source_local.index()] = true;
    }
    state
}

fn apply_local_dependency_effect<'db>(
    body: &MirBody<'db>,
    inst: &MirInst<'db>,
    source_local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    local_depends: &mut [bool],
    state: &mut LocalDependencyState,
) {
    match inst {
        MirInst::Assign {
            dest: Some(dest_local),
            rvalue,
            ..
        } => {
            local_depends[dest_local.index()] = (*dest_local == source_local
                && rvalue_has_memory_alloc_source(rvalue))
                || rvalue_depends_on_local_value(
                    body,
                    rvalue,
                    source_local,
                    ptr_escape_summaries,
                    local_depends,
                    *dest_local,
                    state,
                );
        }
        MirInst::Assign { dest: None, .. } => {}
        MirInst::Store { place, value, .. } => {
            if value_depends_on_local(
                body,
                *value,
                source_local,
                local_depends,
                &mut state.value_visiting,
            ) && !value_is_direct_ref_origin(body, *value, source_local, state)
                && let Some(root_local) =
                    value_origin_local(body, place.base, &mut state.origin_visiting)
            {
                local_depends[root_local.index()] = true;
            }
        }
        MirInst::InitAggregate { place, inits, .. } => {
            if inits.iter().any(|(_, value)| {
                value_depends_on_local(
                    body,
                    *value,
                    source_local,
                    local_depends,
                    &mut state.value_visiting,
                ) && !value_is_direct_ref_origin(body, *value, source_local, state)
            }) && let Some(root_local) =
                value_origin_local(body, place.base, &mut state.origin_visiting)
            {
                local_depends[root_local.index()] = true;
            }
        }
        MirInst::SetDiscriminant { .. } | MirInst::BindValue { .. } => {}
    }
}

fn compute_local_dependency_in_states<'db>(
    body: &MirBody<'db>,
    source_local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
) -> Vec<Vec<bool>> {
    let local_count = body.locals.len();
    let block_count = body.blocks.len();
    if block_count == 0 {
        return Vec::new();
    }

    let mut preds = vec![Vec::new(); block_count];
    for (idx, block) in body.blocks.iter().enumerate() {
        let from = BasicBlockId(idx as u32);
        for succ in terminator_successors(&block.terminator) {
            preds[succ.index()].push(from);
        }
    }

    let entry = body.entry.index();
    let mut in_states = vec![vec![false; local_count]; block_count];
    let mut out_states = vec![vec![false; local_count]; block_count];
    in_states[entry] = initial_local_dependency_state(body, source_local);

    let mut worklist: VecDeque<_> = body
        .blocks
        .iter()
        .enumerate()
        .map(|(idx, _)| BasicBlockId(idx as u32))
        .collect();

    while let Some(block_id) = worklist.pop_front() {
        let mut state = in_states[block_id.index()].clone();
        let mut dependency_state = LocalDependencyState::new(body);
        let block = &body.blocks[block_id.index()];
        for inst in &block.insts {
            apply_local_dependency_effect(
                body,
                inst,
                source_local,
                ptr_escape_summaries,
                &mut state,
                &mut dependency_state,
            );
        }

        if state == out_states[block_id.index()] {
            continue;
        }

        out_states[block_id.index()] = state;
        for succ in terminator_successors(&block.terminator) {
            if succ.index() == entry {
                continue;
            }
            let mut new_in = vec![false; local_count];
            for pred in &preds[succ.index()] {
                for (slot, pred_depends) in new_in.iter_mut().zip(out_states[pred.index()].iter()) {
                    *slot |= *pred_depends;
                }
            }
            if new_in != in_states[succ.index()] {
                in_states[succ.index()] = new_in;
                worklist.push_back(succ);
            }
        }
    }

    in_states
}

fn local_may_be_returned<'db>(
    body: &MirBody<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
) -> bool {
    let dependency_in_states =
        compute_local_dependency_in_states(body, local, ptr_escape_summaries);
    let mut dependency_state = LocalDependencyState::new(body);

    for (block_idx, block) in body.blocks.iter().enumerate() {
        let mut local_depends = dependency_in_states
            .get(block_idx)
            .cloned()
            .unwrap_or_else(|| initial_local_dependency_state(body, local));
        for inst in &block.insts {
            apply_local_dependency_effect(
                body,
                inst,
                local,
                ptr_escape_summaries,
                &mut local_depends,
                &mut dependency_state,
            );
        }
        if let Terminator::Return {
            value: Some(returned),
            ..
        } = &block.terminator
            && value_can_carry_pointer(body, *returned)
            && value_depends_on_local(
                body,
                *returned,
                local,
                &local_depends,
                &mut dependency_state.value_visiting,
            )
        {
            return true;
        }
    }

    false
}

fn value_depends_on_local<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    local: LocalId,
    local_depends: &[bool],
    value_visiting: &mut Vec<bool>,
) -> bool {
    if value_visiting[value.index()] {
        // Conservatively treat recursive value dependency as escaping.
        return true;
    }

    value_visiting[value.index()] = true;
    let depends = match &body.value(value).origin {
        ValueOrigin::Local(dep_local) | ValueOrigin::PlaceRoot(dep_local) => local_depends
            .get(dep_local.index())
            .copied()
            .unwrap_or(*dep_local == local),
        ValueOrigin::Unary { inner, .. } => {
            value_depends_on_local(body, *inner, local, local_depends, value_visiting)
        }
        ValueOrigin::Binary { lhs, rhs, .. } => {
            value_depends_on_local(body, *lhs, local, local_depends, value_visiting)
                || value_depends_on_local(body, *rhs, local, local_depends, value_visiting)
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            value_depends_on_local(body, field_ptr.base, local, local_depends, value_visiting)
        }
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            let mut depends =
                value_depends_on_local(body, place.base, local, local_depends, value_visiting);
            if !depends {
                for projection in place.projection.iter() {
                    if let Projection::Index(IndexSource::Dynamic(index_val)) = projection
                        && value_depends_on_local(
                            body,
                            *index_val,
                            local,
                            local_depends,
                            value_visiting,
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
            value_depends_on_local(body, *value, local, local_depends, value_visiting)
        }
        ValueOrigin::Expr(_)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::Synthetic(_)
        | ValueOrigin::FuncItem(_) => false,
    };
    value_visiting[value.index()] = false;
    depends
}

fn rvalue_may_escape_local<'db>(
    body: &MirBody<'db>,
    _dest_local: Option<LocalId>,
    rvalue: &Rvalue<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    local_depends: &[bool],
    value_visiting: &mut Vec<bool>,
) -> bool {
    match rvalue {
        Rvalue::Call(call) => {
            if values_pass_local_pointer(body, &call.args, local, local_depends, value_visiting)
                || values_pass_local_pointer(
                    body,
                    &call.effect_args,
                    local,
                    local_depends,
                    value_visiting,
                )
            {
                return true;
            }
            let arg_escape_mask =
                call_escape_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
            if call_args_depend_on_local_with_mask(
                body,
                &call.args,
                local,
                local_depends,
                arg_escape_mask,
                0,
                value_visiting,
            ) || call_args_depend_on_local_with_mask(
                body,
                &call.effect_args,
                local,
                local_depends,
                arg_escape_mask,
                call.args.len(),
                value_visiting,
            ) {
                return true;
            }
            false
        }
        Rvalue::Intrinsic { op, args } => args.iter().copied().enumerate().any(|(idx, value)| {
            intrinsic_arg_may_escape(*op, idx)
                && value_depends_on_local(body, value, local, local_depends, value_visiting)
        }),
        Rvalue::ZeroInit
        | Rvalue::Value(_)
        | Rvalue::Load { .. }
        | Rvalue::Alloc { .. }
        | Rvalue::ConstAggregate { .. } => false,
    }
}

fn terminator_may_escape_local<'db>(
    body: &MirBody<'db>,
    terminator: &Terminator<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    local_depends: &[bool],
    value_visiting: &mut Vec<bool>,
) -> bool {
    match terminator {
        Terminator::TerminatingCall { call, .. } => match call {
            TerminatingCall::Call(call) => {
                if values_pass_local_pointer(body, &call.args, local, local_depends, value_visiting)
                    || values_pass_local_pointer(
                        body,
                        &call.effect_args,
                        local,
                        local_depends,
                        value_visiting,
                    )
                {
                    return true;
                }
                let arg_escape_mask =
                    call_escape_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
                call_args_depend_on_local_with_mask(
                    body,
                    &call.args,
                    local,
                    local_depends,
                    arg_escape_mask,
                    0,
                    value_visiting,
                ) || call_args_depend_on_local_with_mask(
                    body,
                    &call.effect_args,
                    local,
                    local_depends,
                    arg_escape_mask,
                    call.args.len(),
                    value_visiting,
                )
            }
            TerminatingCall::Intrinsic { op, args } => {
                args.iter().copied().enumerate().any(|(idx, value)| {
                    intrinsic_arg_may_escape(*op, idx)
                        && value_depends_on_local(body, value, local, local_depends, value_visiting)
                })
            }
            TerminatingCall::DeployRuntime {
                runtime_offset,
                runtime_len,
                immutable_payload,
            } => {
                value_depends_on_local(body, *runtime_offset, local, local_depends, value_visiting)
                    || value_depends_on_local(
                        body,
                        *runtime_len,
                        local,
                        local_depends,
                        value_visiting,
                    )
                    || immutable_payload.is_some_and(|(ptr, _)| {
                        value_depends_on_local(body, ptr, local, local_depends, value_visiting)
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

fn local_can_carry_pointer(body: &MirBody<'_>, local: LocalId) -> bool {
    body.values.iter().any(|value| {
        matches!(value.origin, ValueOrigin::Local(origin) | ValueOrigin::PlaceRoot(origin) if origin == local)
            && value.repr.address_space().is_some()
    })
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
        | IntrinsicOp::CurrentCodeRegionLen
        | IntrinsicOp::Keccak
        | IntrinsicOp::Addmod
        | IntrinsicOp::Mulmod
        | IntrinsicOp::Caller
        | IntrinsicOp::Alloc => false,
    }
}

fn values_depend_on_local<'db>(
    body: &MirBody<'db>,
    values: &[ValueId],
    local: LocalId,
    local_depends: &[bool],
    value_visiting: &mut Vec<bool>,
) -> bool {
    values
        .iter()
        .copied()
        .any(|value| value_depends_on_local(body, value, local, local_depends, value_visiting))
}

fn values_pass_local_pointer<'db>(
    body: &MirBody<'db>,
    values: &[ValueId],
    local: LocalId,
    local_depends: &[bool],
    value_visiting: &mut Vec<bool>,
) -> bool {
    values.iter().copied().any(|value| {
        value_can_carry_pointer(body, value)
            && value_depends_on_local(body, value, local, local_depends, value_visiting)
    })
}

fn value_is_direct_ref_origin<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    local: LocalId,
    state: &mut LocalDependencyState,
) -> bool {
    body.value(value).repr.is_ref()
        && value_origin_local(body, value, &mut state.origin_visiting) == Some(local)
}

fn rvalue_depends_on_local_value<'db>(
    body: &MirBody<'db>,
    rvalue: &Rvalue<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    local_depends: &[bool],
    dest_local: LocalId,
    state: &mut LocalDependencyState,
) -> bool {
    match rvalue {
        Rvalue::ZeroInit | Rvalue::Alloc { .. } | Rvalue::ConstAggregate { .. } => false,
        Rvalue::Value(value) => value_depends_on_local(
            body,
            *value,
            local,
            local_depends,
            &mut state.value_visiting,
        ),
        Rvalue::Call(call) => {
            let arg_return_mask =
                call_return_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
            call_args_depend_on_local_with_mask(
                body,
                &call.args,
                local,
                local_depends,
                arg_return_mask,
                0,
                &mut state.value_visiting,
            ) || call_args_depend_on_local_with_mask(
                body,
                &call.effect_args,
                local,
                local_depends,
                arg_return_mask,
                call.args.len(),
                &mut state.value_visiting,
            )
        }
        Rvalue::Intrinsic { args, .. } => {
            values_depend_on_local(body, args, local, local_depends, &mut state.value_visiting)
        }
        Rvalue::Load { place } => {
            local_can_carry_pointer(body, dest_local)
                && value_origin_local(body, place.base, &mut state.origin_visiting).is_some_and(
                    |root_local| {
                        local_depends
                            .get(root_local.index())
                            .copied()
                            .unwrap_or(false)
                    },
                )
        }
    }
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

fn call_value_escape_arg_mask<'a>(
    callee_name: Option<&str>,
    ptr_escape_summaries: &'a MirPtrEscapeSummaryMap,
) -> Option<&'a [bool]> {
    callee_name
        .and_then(|name| ptr_escape_summaries.get(name))
        .map(|summary| summary.arg_value_may_escape.as_slice())
}

fn call_args_depend_on_local_with_mask<'db>(
    body: &MirBody<'db>,
    values: &[ValueId],
    local: LocalId,
    local_depends: &[bool],
    arg_mask: Option<&[bool]>,
    arg_offset: usize,
    value_visiting: &mut Vec<bool>,
) -> bool {
    values.iter().copied().enumerate().any(|(index, value)| {
        if let Some(mask) = arg_mask
            && !mask.get(arg_offset + index).copied().unwrap_or(true)
        {
            return false;
        }

        value_depends_on_local(body, value, local, local_depends, value_visiting)
    })
}

fn value_must_alias_local_alloc<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    must_alias_state: &[Option<LocalId>],
    value_visiting: &mut Vec<bool>,
) -> Option<LocalId> {
    if value_visiting[value.index()] {
        return None;
    }

    value_visiting[value.index()] = true;
    let must_alias = match &body.value(value).origin {
        ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => {
            must_alias_state.get(local.index()).copied().flatten()
        }
        ValueOrigin::TransparentCast { value } => {
            value_must_alias_local_alloc(body, *value, must_alias_state, value_visiting)
        }
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            value_must_alias_local_alloc(body, place.base, must_alias_state, value_visiting)
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            value_must_alias_local_alloc(body, field_ptr.base, must_alias_state, value_visiting)
        }
        ValueOrigin::Expr(_)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::Unary { .. }
        | ValueOrigin::Binary { .. }
        | ValueOrigin::Synthetic(_)
        | ValueOrigin::FuncItem(_) => None,
    };

    value_visiting[value.index()] = false;
    must_alias
}

fn value_origin_local<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    value_visiting: &mut Vec<bool>,
) -> Option<LocalId> {
    if value_visiting[value.index()] {
        return None;
    }

    value_visiting[value.index()] = true;
    let origin = match &body.value(value).origin {
        ValueOrigin::Local(local) | ValueOrigin::PlaceRoot(local) => Some(*local),
        ValueOrigin::TransparentCast { value } => value_origin_local(body, *value, value_visiting),
        ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
            value_origin_local(body, place.base, value_visiting)
        }
        ValueOrigin::FieldPtr(field_ptr) => {
            value_origin_local(body, field_ptr.base, value_visiting)
        }
        ValueOrigin::Expr(_)
        | ValueOrigin::ControlFlowResult { .. }
        | ValueOrigin::Unit
        | ValueOrigin::Unary { .. }
        | ValueOrigin::Binary { .. }
        | ValueOrigin::Synthetic(_)
        | ValueOrigin::FuncItem(_) => None,
    };

    value_visiting[value.index()] = false;
    origin
}

fn value_is_direct_ref_to_local<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    source_local: LocalId,
    must_alias_state: &[Option<LocalId>],
    value_visiting: &mut Vec<bool>,
) -> bool {
    if !body.value(value).repr.is_ref() {
        return false;
    }

    value_must_alias_local_alloc(body, value, must_alias_state, value_visiting)
        .or_else(|| value_origin_local(body, value, value_visiting))
        == Some(source_local)
}

fn rvalue_must_alias_local_alloc<'db>(
    body: &MirBody<'db>,
    rvalue: &Rvalue<'db>,
    dest_local: LocalId,
    must_alias_state: &[Option<LocalId>],
    alloc_flags: &[bool],
    value_visiting: &mut Vec<bool>,
) -> Option<LocalId> {
    match rvalue {
        Rvalue::Value(value) => {
            value_must_alias_local_alloc(body, *value, must_alias_state, value_visiting)
        }
        Rvalue::Alloc {
            address_space: AddressSpaceKind::Memory,
        } => alloc_flags
            .get(dest_local.index())
            .copied()
            .unwrap_or(false)
            .then_some(dest_local),
        Rvalue::Intrinsic {
            op: IntrinsicOp::Alloc,
            ..
        } => alloc_flags
            .get(dest_local.index())
            .copied()
            .unwrap_or(false)
            .then_some(dest_local),
        Rvalue::Call(call) => (alloc_flags
            .get(dest_local.index())
            .copied()
            .unwrap_or(false)
            && call.effect_args.is_empty()
            && call.resolved_name.as_deref() == Some("alloc"))
        .then_some(dest_local),
        Rvalue::ZeroInit
        | Rvalue::Load { .. }
        | Rvalue::Intrinsic { .. }
        | Rvalue::Alloc { .. }
        | Rvalue::ConstAggregate { .. } => None,
    }
}

fn store_target_alloc_local<'db>(
    body: &MirBody<'db>,
    place: &Place<'db>,
    must_alias_state: &[Option<LocalId>],
    value_visiting: &mut Vec<bool>,
) -> Option<LocalId> {
    if !matches!(body.place_address_space(place), AddressSpaceKind::Memory) {
        return None;
    }
    value_must_alias_local_alloc(body, place.base, must_alias_state, value_visiting)
}

fn local_escape_info<'db>(
    body: &MirBody<'db>,
    source_local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    must_alias_in_states: &[Vec<Option<LocalId>>],
    alloc_flags: &[bool],
) -> LocalEscapeInfo {
    let dependency_in_states =
        compute_local_dependency_in_states(body, source_local, ptr_escape_summaries);
    let mut dependency_state = LocalDependencyState::new(body);
    let mut direct_side_effect = false;
    let mut direct_return = false;
    let mut edges = Vec::new();
    let mut dependency_value_visiting = vec![false; body.values.len()];
    let mut must_alias_visiting = vec![false; body.values.len()];

    for (block_idx, block) in body.blocks.iter().enumerate() {
        let mut local_depends = dependency_in_states
            .get(block_idx)
            .cloned()
            .unwrap_or_else(|| initial_local_dependency_state(body, source_local));
        let mut must_alias_state = must_alias_in_states
            .get(block_idx)
            .cloned()
            .unwrap_or_else(|| vec![None; body.locals.len()]);
        for inst in &block.insts {
            match inst {
                MirInst::Assign { dest, rvalue, .. } => {
                    if rvalue_may_escape_local(
                        body,
                        *dest,
                        rvalue,
                        source_local,
                        ptr_escape_summaries,
                        &local_depends,
                        &mut dependency_value_visiting,
                    ) {
                        direct_side_effect = true;
                    }
                    if let Some(dest_local) = dest {
                        let must_alias = rvalue_must_alias_local_alloc(
                            body,
                            rvalue,
                            *dest_local,
                            &must_alias_state,
                            alloc_flags,
                            &mut must_alias_visiting,
                        );
                        must_alias_state[dest_local.index()] = must_alias;
                    }
                }
                MirInst::Store { place, value, .. } => {
                    if value_depends_on_local(
                        body,
                        *value,
                        source_local,
                        &local_depends,
                        &mut dependency_value_visiting,
                    ) && !value_is_direct_ref_to_local(
                        body,
                        *value,
                        source_local,
                        &must_alias_state,
                        &mut must_alias_visiting,
                    ) {
                        if value_origin_local(body, place.base, &mut must_alias_visiting)
                            == Some(source_local)
                        {
                            continue;
                        }
                        let target = store_target_alloc_local(
                            body,
                            place,
                            &must_alias_state,
                            &mut must_alias_visiting,
                        )
                        .and_then(|local| {
                            alloc_flags
                                .get(local.index())
                                .copied()
                                .unwrap_or(false)
                                .then_some(local)
                        });
                        if let Some(target) = target {
                            if target != source_local {
                                edges.push(target);
                            }
                        } else {
                            direct_side_effect = true;
                        }
                    }
                }
                MirInst::InitAggregate { place, inits, .. } => {
                    for (_, value) in inits {
                        if value_depends_on_local(
                            body,
                            *value,
                            source_local,
                            &local_depends,
                            &mut dependency_value_visiting,
                        ) && !value_is_direct_ref_to_local(
                            body,
                            *value,
                            source_local,
                            &must_alias_state,
                            &mut must_alias_visiting,
                        ) {
                            if value_origin_local(body, place.base, &mut must_alias_visiting)
                                == Some(source_local)
                            {
                                continue;
                            }
                            let target = store_target_alloc_local(
                                body,
                                place,
                                &must_alias_state,
                                &mut must_alias_visiting,
                            )
                            .and_then(|local| {
                                alloc_flags
                                    .get(local.index())
                                    .copied()
                                    .unwrap_or(false)
                                    .then_some(local)
                            });
                            if let Some(target) = target {
                                if target != source_local {
                                    edges.push(target);
                                }
                            } else {
                                direct_side_effect = true;
                            }
                        }
                    }
                }
                MirInst::SetDiscriminant { .. } | MirInst::BindValue { .. } => {}
            }

            apply_local_dependency_effect(
                body,
                inst,
                source_local,
                ptr_escape_summaries,
                &mut local_depends,
                &mut dependency_state,
            );
        }

        if terminator_may_escape_local(
            body,
            &block.terminator,
            source_local,
            ptr_escape_summaries,
            &local_depends,
            &mut dependency_value_visiting,
        ) {
            direct_side_effect = true;
        }

        if let Terminator::Return {
            value: Some(returned),
            ..
        } = &block.terminator
            && value_can_carry_pointer(body, *returned)
            && value_depends_on_local(
                body,
                *returned,
                source_local,
                &local_depends,
                &mut dependency_value_visiting,
            )
        {
            direct_return = true;
        }
    }

    LocalEscapeInfo {
        direct_side_effect,
        direct_return,
        edges,
    }
}

fn rvalue_may_escape_local_value<'db>(
    body: &MirBody<'db>,
    _dest_local: Option<LocalId>,
    rvalue: &Rvalue<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    local_depends: &[bool],
    value_visiting: &mut Vec<bool>,
) -> bool {
    match rvalue {
        Rvalue::Call(call) => {
            let arg_escape_mask =
                call_value_escape_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
            call_args_depend_on_local_with_mask(
                body,
                &call.args,
                local,
                local_depends,
                arg_escape_mask,
                0,
                value_visiting,
            ) || call_args_depend_on_local_with_mask(
                body,
                &call.effect_args,
                local,
                local_depends,
                arg_escape_mask,
                call.args.len(),
                value_visiting,
            )
        }
        Rvalue::Intrinsic { op, args } => args.iter().copied().enumerate().any(|(idx, value)| {
            intrinsic_arg_may_escape(*op, idx)
                && value_depends_on_local(body, value, local, local_depends, value_visiting)
        }),
        Rvalue::ZeroInit
        | Rvalue::Value(_)
        | Rvalue::Load { .. }
        | Rvalue::Alloc { .. }
        | Rvalue::ConstAggregate { .. } => false,
    }
}

fn terminator_may_escape_local_value<'db>(
    body: &MirBody<'db>,
    terminator: &Terminator<'db>,
    local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    local_depends: &[bool],
    value_visiting: &mut Vec<bool>,
) -> bool {
    match terminator {
        Terminator::TerminatingCall { call, .. } => match call {
            TerminatingCall::Call(call) => {
                let arg_escape_mask =
                    call_value_escape_arg_mask(call.resolved_name.as_deref(), ptr_escape_summaries);
                call_args_depend_on_local_with_mask(
                    body,
                    &call.args,
                    local,
                    local_depends,
                    arg_escape_mask,
                    0,
                    value_visiting,
                ) || call_args_depend_on_local_with_mask(
                    body,
                    &call.effect_args,
                    local,
                    local_depends,
                    arg_escape_mask,
                    call.args.len(),
                    value_visiting,
                )
            }
            TerminatingCall::Intrinsic { op, args } => {
                args.iter().copied().enumerate().any(|(idx, value)| {
                    intrinsic_arg_may_escape(*op, idx)
                        && value_depends_on_local(body, value, local, local_depends, value_visiting)
                })
            }
            TerminatingCall::DeployRuntime {
                runtime_offset,
                runtime_len,
                immutable_payload,
            } => {
                value_depends_on_local(body, *runtime_offset, local, local_depends, value_visiting)
                    || value_depends_on_local(
                        body,
                        *runtime_len,
                        local,
                        local_depends,
                        value_visiting,
                    )
                    || immutable_payload.is_some_and(|(ptr, _)| {
                        value_depends_on_local(body, ptr, local, local_depends, value_visiting)
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

fn local_value_escape_info<'db>(
    body: &MirBody<'db>,
    source_local: LocalId,
    ptr_escape_summaries: &MirPtrEscapeSummaryMap,
    must_alias_in_states: &[Vec<Option<LocalId>>],
    alloc_flags: &[bool],
) -> LocalEscapeInfo {
    let dependency_in_states =
        compute_local_dependency_in_states(body, source_local, ptr_escape_summaries);
    let mut dependency_state = LocalDependencyState::new(body);
    let mut direct_side_effect = false;
    let mut direct_return = false;
    let mut edges = Vec::new();
    let mut dependency_value_visiting = vec![false; body.values.len()];
    let mut must_alias_visiting = vec![false; body.values.len()];

    for (block_idx, block) in body.blocks.iter().enumerate() {
        let mut local_depends = dependency_in_states
            .get(block_idx)
            .cloned()
            .unwrap_or_else(|| initial_local_dependency_state(body, source_local));
        let mut must_alias_state = must_alias_in_states
            .get(block_idx)
            .cloned()
            .unwrap_or_else(|| vec![None; body.locals.len()]);
        for inst in &block.insts {
            match inst {
                MirInst::Assign { dest, rvalue, .. } => {
                    if rvalue_may_escape_local_value(
                        body,
                        *dest,
                        rvalue,
                        source_local,
                        ptr_escape_summaries,
                        &local_depends,
                        &mut dependency_value_visiting,
                    ) {
                        direct_side_effect = true;
                    }
                    if let Some(dest_local) = dest {
                        let must_alias = rvalue_must_alias_local_alloc(
                            body,
                            rvalue,
                            *dest_local,
                            &must_alias_state,
                            alloc_flags,
                            &mut must_alias_visiting,
                        );
                        must_alias_state[dest_local.index()] = must_alias;
                    }
                }
                MirInst::Store { place, value, .. } => {
                    if value_depends_on_local(
                        body,
                        *value,
                        source_local,
                        &local_depends,
                        &mut dependency_value_visiting,
                    ) && !value_is_direct_ref_to_local(
                        body,
                        *value,
                        source_local,
                        &must_alias_state,
                        &mut must_alias_visiting,
                    ) {
                        if value_origin_local(body, place.base, &mut must_alias_visiting)
                            == Some(source_local)
                        {
                            continue;
                        }
                        let target = store_target_alloc_local(
                            body,
                            place,
                            &must_alias_state,
                            &mut must_alias_visiting,
                        )
                        .and_then(|local| {
                            alloc_flags
                                .get(local.index())
                                .copied()
                                .unwrap_or(false)
                                .then_some(local)
                        });
                        if let Some(target) = target {
                            if target != source_local {
                                edges.push(target);
                            }
                        } else {
                            direct_side_effect = true;
                        }
                    }
                }
                MirInst::InitAggregate { place, inits, .. } => {
                    for (_, value) in inits {
                        if value_depends_on_local(
                            body,
                            *value,
                            source_local,
                            &local_depends,
                            &mut dependency_value_visiting,
                        ) && !value_is_direct_ref_to_local(
                            body,
                            *value,
                            source_local,
                            &must_alias_state,
                            &mut must_alias_visiting,
                        ) {
                            if value_origin_local(body, place.base, &mut must_alias_visiting)
                                == Some(source_local)
                            {
                                continue;
                            }
                            let target = store_target_alloc_local(
                                body,
                                place,
                                &must_alias_state,
                                &mut must_alias_visiting,
                            )
                            .and_then(|local| {
                                alloc_flags
                                    .get(local.index())
                                    .copied()
                                    .unwrap_or(false)
                                    .then_some(local)
                            });
                            if let Some(target) = target {
                                if target != source_local {
                                    edges.push(target);
                                }
                            } else {
                                direct_side_effect = true;
                            }
                        }
                    }
                }
                MirInst::SetDiscriminant { .. } | MirInst::BindValue { .. } => {}
            }

            apply_local_dependency_effect(
                body,
                inst,
                source_local,
                ptr_escape_summaries,
                &mut local_depends,
                &mut dependency_state,
            );
        }

        if terminator_may_escape_local_value(
            body,
            &block.terminator,
            source_local,
            ptr_escape_summaries,
            &local_depends,
            &mut dependency_value_visiting,
        ) {
            direct_side_effect = true;
        }

        if let Terminator::Return {
            value: Some(returned),
            ..
        } = &block.terminator
            && value_can_carry_pointer(body, *returned)
            && value_depends_on_local(
                body,
                *returned,
                source_local,
                &local_depends,
                &mut dependency_value_visiting,
            )
        {
            direct_return = true;
        }
    }

    LocalEscapeInfo {
        direct_side_effect,
        direct_return,
        edges,
    }
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::analysis::ty::ty_def::{PrimTy, TyBase, TyData, TyId};
    use num_bigint::BigUint;
    use url::Url;

    use crate::{
        LocalId, MirFunction, MirInst, Rvalue,
        analysis::escape::compute_ptr_escape_summaries,
        ir::{
            AddressSpaceKind, BasicBlock, BasicBlockId, CallOrigin, LocalData, Place, SourceInfoId,
            SyntheticValue, Terminator, ValueData, ValueOrigin, ValueRepr,
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
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let local_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        func.body.param_locals.push(param);
        let param_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(param),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        func.body.push_block(BasicBlock {
            insts: vec![],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(param_value),
            },
        });
    }

    fn mutate_effect_id_function<'db>(func: &mut MirFunction<'db>, db: &'db DriverDataBase) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let effect_param = func.body.alloc_local(LocalData {
            name: "effect_param".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        func.body.effect_param_locals.push(effect_param);
        let effect_param_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(effect_param),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        func.body.push_block(BasicBlock {
            insts: vec![],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(effect_param_value),
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
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                        checked_intrinsic: None,
                        builtin_terminator: None,
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

    fn mutate_caller_returning_call_result_function<'db>(
        func: &mut MirFunction<'db>,
        db: &'db DriverDataBase,
    ) {
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
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let call_result_local = func.body.alloc_local(LocalData {
            name: "result".to_string(),
            ty: u256_ty,
            is_mut: false,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let call_result_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(call_result_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                    dest: Some(call_result_local),
                    rvalue: Rvalue::Call(CallOrigin {
                        expr: None,
                        hir_target: None,
                        args: vec![alloc_value],
                        effect_args: vec![],
                        resolved_name: Some("escape_id".to_string()),
                        checked_intrinsic: None,
                        builtin_terminator: None,
                        receiver_space: None,
                    }),
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(call_result_value),
            },
        });
    }

    fn mutate_caller_returning_effect_call_result_function<'db>(
        func: &mut MirFunction<'db>,
        db: &'db DriverDataBase,
    ) {
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
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let call_result_local = func.body.alloc_local(LocalData {
            name: "result".to_string(),
            ty: u256_ty,
            is_mut: false,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let call_result_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(call_result_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                    dest: Some(call_result_local),
                    rvalue: Rvalue::Call(CallOrigin {
                        expr: None,
                        hir_target: None,
                        args: vec![],
                        effect_args: vec![alloc_value],
                        resolved_name: Some("escape_effect_id".to_string()),
                        checked_intrinsic: None,
                        builtin_terminator: None,
                        receiver_space: None,
                    }),
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(call_result_value),
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
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let local_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let size_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Synthetic(crate::ir::SyntheticValue::Int(BigUint::from(32u64))),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                    checked_intrinsic: None,
                    builtin_terminator: None,
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
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let loaded_local = func.body.alloc_local(LocalData {
            name: "loaded".to_string(),
            ty: u256_ty,
            is_mut: false,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let loaded_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(loaded_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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

    fn mutate_mixed_alias_store_function<'db>(
        func: &mut MirFunction<'db>,
        db: &'db DriverDataBase,
    ) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        let bool_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Bool)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let alloc_local = func.body.alloc_local(LocalData {
            name: "alloc".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let target_local = func.body.alloc_local(LocalData {
            name: "target".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let target_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(target_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let foreign_local = func.body.alloc_local(LocalData {
            name: "foreign".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let foreign_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(foreign_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let cond_value = func.body.alloc_value(ValueData {
            ty: bool_ty,
            origin: ValueOrigin::Synthetic(SyntheticValue::Bool(true)),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Word,
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let then_id = BasicBlockId(1);
        let else_id = BasicBlockId(2);
        let join_id = BasicBlockId(3);

        func.body.blocks = vec![
            BasicBlock {
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
                        dest: Some(foreign_local),
                        rvalue: Rvalue::Alloc {
                            address_space: AddressSpaceKind::Memory,
                        },
                    },
                ],
                terminator: Terminator::Branch {
                    source: SourceInfoId::SYNTHETIC,
                    cond: cond_value,
                    then_bb: then_id,
                    else_bb: else_id,
                },
            },
            BasicBlock {
                insts: vec![MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(target_local),
                    rvalue: Rvalue::Value(alloc_value),
                }],
                terminator: Terminator::Goto {
                    source: SourceInfoId::SYNTHETIC,
                    target: join_id,
                },
            },
            BasicBlock {
                insts: vec![MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(target_local),
                    rvalue: Rvalue::Value(foreign_value),
                }],
                terminator: Terminator::Goto {
                    source: SourceInfoId::SYNTHETIC,
                    target: join_id,
                },
            },
            BasicBlock {
                insts: vec![MirInst::Store {
                    source: SourceInfoId::SYNTHETIC,
                    place: Place::new(target_value, crate::MirProjectionPath::new()),
                    value: alloc_value,
                }],
                terminator: Terminator::Return {
                    source: SourceInfoId::SYNTHETIC,
                    value: None,
                },
            },
        ];
    }

    fn mutate_local_store_function<'db>(func: &mut MirFunction<'db>, db: &'db DriverDataBase) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let alloc_local = func.body.alloc_local(LocalData {
            name: "alloc".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                MirInst::Store {
                    source: SourceInfoId::SYNTHETIC,
                    place: Place::new(alloc_value, crate::MirProjectionPath::new()),
                    value: alloc_value,
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });
    }

    fn mutate_cast_local_store_function<'db>(func: &mut MirFunction<'db>, db: &'db DriverDataBase) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let alloc_local = func.body.alloc_local(LocalData {
            name: "alloc".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let cast_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::TransparentCast { value: alloc_value },
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                MirInst::Store {
                    source: SourceInfoId::SYNTHETIC,
                    place: Place::new(cast_value, crate::MirProjectionPath::new()),
                    value: alloc_value,
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });
    }

    fn mutate_store_before_overwrite_function<'db>(
        func: &mut MirFunction<'db>,
        db: &'db DriverDataBase,
    ) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let alloc_local = func.body.alloc_local(LocalData {
            name: "alloc".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let target_local = func.body.alloc_local(LocalData {
            name: "target".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let target_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(target_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let foreign_local = func.body.alloc_local(LocalData {
            name: "foreign".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let foreign_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(foreign_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                    dest: Some(target_local),
                    rvalue: Rvalue::Value(alloc_value),
                },
                MirInst::Store {
                    source: SourceInfoId::SYNTHETIC,
                    place: Place::new(target_value, crate::MirProjectionPath::new()),
                    value: alloc_value,
                },
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(foreign_local),
                    rvalue: Rvalue::Alloc {
                        address_space: AddressSpaceKind::Memory,
                    },
                },
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(target_local),
                    rvalue: Rvalue::Value(foreign_value),
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: None,
            },
        });
    }

    fn mutate_return_after_overwrite_function<'db>(
        func: &mut MirFunction<'db>,
        db: &'db DriverDataBase,
    ) -> (LocalId, LocalId) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let alloc_local = func.body.alloc_local(LocalData {
            name: "alloc".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let alloc_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(alloc_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let foreign_local = func.body.alloc_local(LocalData {
            name: "foreign".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let foreign_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(foreign_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let result_local = func.body.alloc_local(LocalData {
            name: "result".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let result_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(result_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ptr(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
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
                    dest: Some(result_local),
                    rvalue: Rvalue::Value(alloc_value),
                },
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(foreign_local),
                    rvalue: Rvalue::Alloc {
                        address_space: AddressSpaceKind::Memory,
                    },
                },
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(result_local),
                    rvalue: Rvalue::Value(foreign_value),
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(result_value),
            },
        });

        (alloc_local, foreign_local)
    }

    fn mutate_ref_store_into_returned_alloc_function<'db>(
        func: &mut MirFunction<'db>,
        db: &'db DriverDataBase,
    ) -> (LocalId, LocalId) {
        let u256_ty = TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::U256)));
        func.body.locals.clear();
        func.body.values.clear();
        func.body.blocks.clear();
        func.body.param_locals.clear();
        func.body.effect_param_locals.clear();

        let src_local = func.body.alloc_local(LocalData {
            name: "src".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let src_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(src_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ref(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        let dst_local = func.body.alloc_local(LocalData {
            name: "dst".to_string(),
            ty: u256_ty,
            is_mut: true,
            source: SourceInfoId::SYNTHETIC,
            address_space: AddressSpaceKind::Memory,
            pointer_leaf_infos: Vec::new(),
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });
        let dst_value = func.body.alloc_value(ValueData {
            ty: u256_ty,
            origin: ValueOrigin::Local(dst_local),
            source: SourceInfoId::SYNTHETIC,
            repr: ValueRepr::Ref(AddressSpaceKind::Memory),
            pointer_info: None,
            runtime_shape: crate::ir::RuntimeShape::Unresolved,
        });

        func.body.push_block(BasicBlock {
            insts: vec![
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(src_local),
                    rvalue: Rvalue::Alloc {
                        address_space: AddressSpaceKind::Memory,
                    },
                },
                MirInst::Assign {
                    source: SourceInfoId::SYNTHETIC,
                    dest: Some(dst_local),
                    rvalue: Rvalue::Alloc {
                        address_space: AddressSpaceKind::Memory,
                    },
                },
                MirInst::Store {
                    source: SourceInfoId::SYNTHETIC,
                    place: Place::new(dst_value, crate::MirProjectionPath::new()),
                    value: src_value,
                },
            ],
            terminator: Terminator::Return {
                source: SourceInfoId::SYNTHETIC,
                value: Some(dst_value),
            },
        });

        (src_local, dst_local)
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
    fn call_arg_marks_alloc_as_escaping_for_cross_frame_safety() {
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
            caller_summary.local_alloc_may_escape[0],
            "passing a local allocation across a call boundary must force malloc"
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

    #[test]
    fn returned_call_result_marks_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_returned_call_result.fe").unwrap();
        let src = "pub fn escape_id(value: u256) -> u256 { value } pub fn escape_returned_call_result() {}";
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
            .find(|func| func.symbol_name == "escape_returned_call_result")
            .expect("caller function should exist");
        mutate_caller_returning_call_result_function(caller_func, &db);

        let summaries = compute_ptr_escape_summaries(&db, &module);
        let id_summary = summaries.get("escape_id").expect("id summary should exist");
        assert!(
            id_summary.arg_may_be_returned[0],
            "returned argument should be tracked as return-propagating"
        );

        let caller_summary = summaries
            .get("escape_returned_call_result")
            .expect("caller summary should exist");
        assert!(
            caller_summary.local_alloc_may_escape[0],
            "alloc passed to a call whose result is returned should be marked escaping"
        );
    }

    #[test]
    fn returned_effect_call_result_marks_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_returned_effect_call_result.fe").unwrap();
        let src = "pub fn escape_effect_id() {} pub fn escape_returned_effect_call_result() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let effect_id_func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_effect_id")
            .expect("effect id function should exist");
        mutate_effect_id_function(effect_id_func, &db);

        let caller_func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_returned_effect_call_result")
            .expect("caller function should exist");
        mutate_caller_returning_effect_call_result_function(caller_func, &db);

        let summaries = compute_ptr_escape_summaries(&db, &module);
        let effect_id_summary = summaries
            .get("escape_effect_id")
            .expect("effect id summary should exist");
        assert!(
            effect_id_summary.arg_may_be_returned[0],
            "returned effect argument should be tracked as return-propagating"
        );

        let caller_summary = summaries
            .get("escape_returned_effect_call_result")
            .expect("caller summary should exist");
        assert!(
            caller_summary.local_alloc_may_escape[0],
            "alloc passed to returned effect-arg call result should be marked escaping"
        );
    }

    #[test]
    fn store_into_mixed_alias_target_marks_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_store_mixed_alias_target.fe").unwrap();
        let src = "pub fn escape_store_mixed_alias_target() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_store_mixed_alias_target")
            .expect("function should exist");
        mutate_mixed_alias_store_function(func, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_store_mixed_alias_target")
            .expect("summary should exist");
        assert!(
            summary.local_alloc_may_escape[0],
            "store target with mixed provenance must be treated as escaping"
        );
    }

    #[test]
    fn store_into_local_target_does_not_mark_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_store_local_target.fe").unwrap();
        let src = "pub fn escape_store_local_target() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_store_local_target")
            .expect("function should exist");
        mutate_local_store_function(func, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_store_local_target")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[0],
            "direct local store target must remain non-escaping"
        );
    }

    #[test]
    fn store_into_transparent_cast_local_target_does_not_mark_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_store_cast_local_target.fe").unwrap();
        let src = "pub fn escape_store_cast_local_target() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_store_cast_local_target")
            .expect("function should exist");
        mutate_cast_local_store_function(func, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_store_cast_local_target")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[0],
            "transparent cast local target must remain non-escaping"
        );
    }

    #[test]
    fn store_before_overwrite_does_not_mark_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_store_then_overwrite_target.fe").unwrap();
        let src = "pub fn escape_store_then_overwrite_target() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let func = module
            .functions
            .iter_mut()
            .find(|func| func.symbol_name == "escape_store_then_overwrite_target")
            .expect("function should exist");
        mutate_store_before_overwrite_function(func, &db);

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_store_then_overwrite_target")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[0],
            "store target should be treated as local before it is overwritten"
        );
    }

    #[test]
    fn return_after_overwrite_only_marks_final_alloc_as_escaping() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_return_after_overwrite.fe").unwrap();
        let src = "pub fn escape_return_after_overwrite() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let (alloc_local, foreign_local) = {
            let func = module
                .functions
                .iter_mut()
                .find(|func| func.symbol_name == "escape_return_after_overwrite")
                .expect("function should exist");
            mutate_return_after_overwrite_function(func, &db)
        };

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_return_after_overwrite")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[alloc_local.index()],
            "overwritten alloc should not remain marked as escaping"
        );
        assert!(
            summary.local_alloc_may_escape[foreign_local.index()],
            "final returned alloc should be marked as escaping"
        );
    }

    #[test]
    fn deep_copy_store_does_not_escape_source_alloc() {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///escape_deep_copy_store.fe").unwrap();
        let src = "pub fn escape_deep_copy_store() {}";
        let file = db.workspace().touch(&mut db, url, Some(src.to_string()));
        let top_mod = db.top_mod(file);
        let mut module = crate::lower_module(&db, top_mod).expect("module should lower");
        let (src_local, dst_local) = {
            let func = module
                .functions
                .iter_mut()
                .find(|func| func.symbol_name == "escape_deep_copy_store")
                .expect("function should exist");
            let (src_local, dst_local) = mutate_ref_store_into_returned_alloc_function(func, &db);
            (src_local, dst_local)
        };

        let summary = compute_ptr_escape_summaries(&db, &module)
            .remove("escape_deep_copy_store")
            .expect("summary should exist");
        assert!(
            !summary.local_alloc_may_escape[src_local.index()],
            "deep-copy store should not mark source alloc as escaping"
        );
        assert!(
            summary.local_alloc_may_escape[dst_local.index()],
            "returned alloc should still be marked as escaping"
        );
    }
}
