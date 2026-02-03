//! MIR borrow checking for `mut` / `ref` borrow handles.
//!
//! Borrow handles are pointer-like values (`mut T` / `ref T`) that carry NoEsc (stack-only)
//! restrictions. The handles themselves are copyable; soundness comes from enforcing aliasing
//! rules over the *places* they can point to.

use hir::analysis::ty::ty_def::BorrowKind;
use hir::analysis::{
    HirAnalysisDb,
    ty::{ty_is_borrow, ty_is_noesc},
};
use hir::hir_def::FuncParamMode;
use hir::projection::Aliasing;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{
    AddressSpaceKind, BasicBlockId, CallOrigin, LocalId, MirBody, MirFunction, MirInst, Place,
    Rvalue, Terminator, ValueId, ValueOrigin, ValueRepr,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LoanId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Root {
    Param(u32),
    Local(LocalId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CanonPlace<'db> {
    root: Root,
    proj: crate::MirProjectionPath<'db>,
}

#[derive(Debug, Clone)]
struct Loan<'db> {
    kind: BorrowKind,
    targets: FxHashSet<CanonPlace<'db>>,
    parents: FxHashSet<LoanId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BorrowTransform<'db> {
    pub param_index: u32,
    pub proj: crate::MirProjectionPath<'db>,
}

pub type BorrowSummary<'db> = FxHashSet<BorrowTransform<'db>>;
pub type BorrowSummaryMap<'db> = FxHashMap<String, BorrowSummary<'db>>;

#[derive(Debug)]
pub struct BorrowSummaryError {
    pub func_name: String,
    pub diagnostics: String,
}

pub fn compute_borrow_summaries<'db>(
    db: &'db dyn HirAnalysisDb,
    functions: &[MirFunction<'db>],
) -> Result<BorrowSummaryMap<'db>, BorrowSummaryError> {
    let mut summaries: BorrowSummaryMap<'db> = functions
        .iter()
        .map(|func| (func.symbol_name.clone(), FxHashSet::default()))
        .collect();

    loop {
        let mut changed = false;
        for func in functions {
            let func_name = match func.origin {
                crate::ir::MirFunctionOrigin::Hir(hir_func) => hir_func.pretty_print_signature(db),
                crate::ir::MirFunctionOrigin::Synthetic(_) => func.symbol_name.clone(),
            };

            let summary = Borrowck::new(db, func, &summaries)
                .borrow_summary()
                .map_err(|err| BorrowSummaryError {
                    func_name,
                    diagnostics: err,
                })?;

            let Some(summary) = summary else {
                continue;
            };
            let Some(existing) = summaries.get_mut(&func.symbol_name) else {
                panic!("borrow summary missing for {}", func.symbol_name);
            };

            let before = existing.len();
            existing.extend(summary);
            changed |= existing.len() != before;
        }
        if !changed {
            break;
        }
    }

    Ok(summaries)
}

pub fn check_borrows<'db>(
    db: &'db dyn HirAnalysisDb,
    func: &MirFunction<'db>,
    summaries: &BorrowSummaryMap<'db>,
) -> Option<String> {
    Borrowck::new(db, func, summaries).check()
}

struct Borrowck<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    func: &'a MirFunction<'db>,
    summaries: &'a BorrowSummaryMap<'db>,
    borrow_param: Vec<bool>,
    param_modes: Vec<FuncParamMode>,
    tracked_local_idx: Vec<Option<usize>>,
    tracked_locals: Vec<LocalId>,
    param_index_of_local: Vec<Option<u32>>,
    param_loan_for_local: Vec<Option<LoanId>>,
    loan_for_value: FxHashMap<ValueId, LoanId>,
    call_loan_for_value: FxHashMap<ValueId, LoanId>,
    loans: Vec<Loan<'db>>,
    entry_states: Vec<Vec<FxHashSet<LoanId>>>,
    moved_entry: Vec<FxHashSet<CanonPlace<'db>>>,
    live_before: Vec<Vec<FxHashSet<LocalId>>>,
    live_before_term: Vec<FxHashSet<LocalId>>,
}

impl<'db, 'a> Borrowck<'db, 'a> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        func: &'a MirFunction<'db>,
        summaries: &'a BorrowSummaryMap<'db>,
    ) -> Self {
        let body = &func.body;
        let mut tracked_local_idx = vec![None; body.locals.len()];
        let mut tracked_locals = Vec::new();
        for (idx, local) in body.locals.iter().enumerate() {
            let local_id = LocalId(idx as u32);
            if ty_is_noesc(db, local.ty) {
                tracked_local_idx[idx] = Some(tracked_locals.len());
                tracked_locals.push(local_id);
            }
        }

        let mut param_index_of_local = vec![None; body.locals.len()];
        for (idx, local) in body.param_locals.iter().enumerate() {
            param_index_of_local[local.index()] = Some(idx as u32);
        }
        let borrow_param: Vec<_> = body
            .param_locals
            .iter()
            .map(|local| ty_is_borrow(db, body.local(*local).ty).is_some())
            .collect();

        let param_modes = match func.origin {
            crate::ir::MirFunctionOrigin::Hir(hir_func) => {
                hir_func.params(db).map(|param| param.mode(db)).collect()
            }
            crate::ir::MirFunctionOrigin::Synthetic(_) => {
                vec![FuncParamMode::Move; body.param_locals.len()]
            }
        };
        if param_modes.len() != body.param_locals.len() {
            panic!("param modes length mismatch");
        }

        Self {
            db,
            func,
            summaries,
            borrow_param,
            param_modes,
            tracked_local_idx,
            tracked_locals,
            param_index_of_local,
            param_loan_for_local: vec![None; body.locals.len()],
            loan_for_value: FxHashMap::default(),
            call_loan_for_value: FxHashMap::default(),
            loans: Vec::new(),
            entry_states: Vec::new(),
            moved_entry: Vec::new(),
            live_before: Vec::new(),
            live_before_term: Vec::new(),
        }
    }

    fn analyze(&mut self) {
        self.entry_states = vec![
            vec![FxHashSet::default(); self.tracked_locals.len()];
            self.func.body.blocks.len()
        ];
        self.init_loans();
        self.seed_param_loans();
        self.compute_entry_states();
        self.compute_loan_targets_and_parents();
    }

    fn borrow_summary(mut self) -> Result<Option<BorrowSummary<'db>>, String> {
        if ty_is_borrow(self.db, self.func.ret_ty).is_none() {
            return Ok(None);
        }
        self.analyze();
        self.compute_return_summary().map(Some)
    }

    fn check(mut self) -> Option<String> {
        self.analyze();
        self.compute_moved_entry_states();
        self.compute_liveness();
        self.check_conflicts()
    }

    fn init_loans(&mut self) {
        // Borrow-handle params are treated as pre-existing loans rooted at `Param(i)`.
        for (idx, &local) in self.func.body.param_locals.iter().enumerate() {
            let ty = self.func.body.local(local).ty;
            let Some((kind, _)) = ty_is_borrow(self.db, ty) else {
                continue;
            };
            let loan = LoanId(self.loans.len() as u32);
            let mut targets = FxHashSet::default();
            targets.insert(CanonPlace {
                root: Root::Param(idx as u32),
                proj: crate::MirProjectionPath::new(),
            });
            self.loans.push(Loan {
                kind,
                targets,
                parents: FxHashSet::default(),
            });
            self.param_loan_for_local[local.index()] = Some(loan);
        }

        // Each `mut/ref <place>` expression becomes a distinct loan.
        for (idx, value) in self.func.body.values.iter().enumerate() {
            let value_id = ValueId(idx as u32);
            if !matches!(value.origin, ValueOrigin::PlaceRef(_)) {
                continue;
            }
            let Some((kind, _)) = ty_is_borrow(self.db, value.ty) else {
                continue;
            };
            let loan = LoanId(self.loans.len() as u32);
            self.loan_for_value.insert(value_id, loan);
            self.loans.push(Loan {
                kind,
                targets: FxHashSet::default(),
                parents: FxHashSet::default(),
            });
        }

        // Each borrow-handle call result becomes a distinct loan whose targets come from the callee
        // summary applied to the call arguments.
        for block in &self.func.body.blocks {
            for inst in &block.insts {
                let MirInst::Assign {
                    dest: Some(dest),
                    rvalue: Rvalue::Call(call),
                    ..
                } = inst
                else {
                    continue;
                };
                let Some((kind, _)) = ty_is_borrow(self.db, self.func.body.local(*dest).ty) else {
                    continue;
                };
                let Some(expr) = call.expr else {
                    panic!("borrow-handle call must carry its ExprId for analysis");
                };
                let Some(&call_value) = self.func.body.expr_values.get(&expr) else {
                    panic!("missing value id for call expr {expr:?}");
                };
                let loan = LoanId(self.loans.len() as u32);
                self.call_loan_for_value.insert(call_value, loan);
                self.loans.push(Loan {
                    kind,
                    targets: FxHashSet::default(),
                    parents: FxHashSet::default(),
                });
            }
        }
    }

    fn seed_param_loans(&mut self) {
        for &local in &self.func.body.param_locals {
            let Some(loan) = self.param_loan_for_local[local.index()] else {
                continue;
            };
            if let Some(tracked_idx) = self.tracked_local_idx.get(local.index()).copied().flatten()
            {
                self.entry_states[self.func.body.entry.index()][tracked_idx].insert(loan);
            }
        }
    }

    fn compute_entry_states(&mut self) {
        let body = &self.func.body;

        let mut worklist: Vec<BasicBlockId> = vec![body.entry];
        let mut in_worklist = FxHashSet::default();
        in_worklist.insert(body.entry);

        while let Some(bb) = worklist.pop() {
            in_worklist.remove(&bb);
            let mut state = self.entry_states[bb.index()].clone();
            for inst in &body.blocks[bb.index()].insts {
                self.update_state_for_inst(inst, &mut state);
            }

            for succ in successors(&body.blocks[bb.index()].terminator) {
                if self.join_entry_state(succ, &state) && in_worklist.insert(succ) {
                    worklist.push(succ);
                }
            }
        }
    }

    fn compute_loan_targets_and_parents(&mut self) {
        let body = &self.func.body;
        loop {
            let mut changed = false;
            for (bb_idx, block) in body.blocks.iter().enumerate() {
                let mut state = self.entry_states[bb_idx].clone();
                for inst in &block.insts {
                    self.update_loan_info_for_inst(inst, &state, &mut changed);
                    self.update_state_for_inst(inst, &mut state);
                }
                self.update_loan_info_for_terminator(&block.terminator, &state, &mut changed);
            }
            if !changed {
                break;
            }
        }
    }

    fn compute_return_summary(&self) -> Result<BorrowSummary<'db>, String> {
        let body = &self.func.body;
        let mut out = FxHashSet::default();
        for (bb_idx, block) in body.blocks.iter().enumerate() {
            let mut state = self.entry_states[bb_idx].clone();
            for inst in &block.insts {
                self.update_state_for_inst(inst, &mut state);
            }
            let Terminator::Return(Some(value)) = &block.terminator else {
                continue;
            };
            for place in self.canonicalize_base(&state, *value) {
                let Root::Param(param_index) = place.root else {
                    return Err(
                        "return borrow must be derived from explicit borrow params".to_string()
                    );
                };
                if !self
                    .borrow_param
                    .get(param_index as usize)
                    .copied()
                    .unwrap_or(false)
                {
                    return Err(
                        "return borrow must be derived from explicit borrow params".to_string()
                    );
                }
                for proj in place.proj.iter() {
                    if let hir::projection::Projection::Index(
                        hir::projection::IndexSource::Dynamic(_),
                    ) = proj
                    {
                        return Err(
                            "return borrows with dynamic indices are not supported".to_string()
                        );
                    }
                }
                out.insert(BorrowTransform {
                    param_index,
                    proj: place.proj,
                });
            }
        }
        Ok(out)
    }

    fn compute_liveness(&mut self) {
        let body = &self.func.body;
        let mut use_sets = vec![FxHashSet::default(); body.blocks.len()];
        let mut def_sets = vec![FxHashSet::default(); body.blocks.len()];

        for (bb_idx, block) in body.blocks.iter().enumerate() {
            let mut defs = FxHashSet::default();
            let mut uses = FxHashSet::default();
            for inst in &block.insts {
                let inst_uses = locals_used_by_inst(body, inst);
                for local in inst_uses {
                    if !defs.contains(&local) {
                        uses.insert(local);
                    }
                }
                if let MirInst::Assign {
                    dest: Some(dest), ..
                } = inst
                {
                    defs.insert(*dest);
                }
            }
            for local in locals_used_by_terminator(body, &block.terminator) {
                if !defs.contains(&local) {
                    uses.insert(local);
                }
            }
            use_sets[bb_idx] = uses;
            def_sets[bb_idx] = defs;
        }

        let mut live_in = vec![FxHashSet::default(); body.blocks.len()];
        let mut live_out = vec![FxHashSet::default(); body.blocks.len()];

        loop {
            let mut changed = false;
            for (bb_idx, block) in body.blocks.iter().enumerate().rev() {
                let mut out = FxHashSet::default();
                for succ in successors(&block.terminator) {
                    out.extend(live_in[succ.index()].iter().copied());
                }
                let mut input = use_sets[bb_idx].clone();
                for local in out.iter() {
                    if !def_sets[bb_idx].contains(local) {
                        input.insert(*local);
                    }
                }
                changed |= live_out[bb_idx] != out || live_in[bb_idx] != input;
                live_out[bb_idx] = out;
                live_in[bb_idx] = input;
            }
            if !changed {
                break;
            }
        }

        self.live_before = Vec::with_capacity(body.blocks.len());
        self.live_before_term = Vec::with_capacity(body.blocks.len());
        for (bb_idx, block) in body.blocks.iter().enumerate() {
            let mut live = live_out[bb_idx].clone();
            live.extend(locals_used_by_terminator(body, &block.terminator));
            self.live_before_term.push(live.clone());

            let mut per_inst = vec![FxHashSet::default(); block.insts.len()];
            for (idx, inst) in block.insts.iter().enumerate().rev() {
                if let MirInst::Assign {
                    dest: Some(dest), ..
                } = inst
                {
                    live.remove(dest);
                }
                live.extend(locals_used_by_inst(body, inst));
                per_inst[idx] = live.clone();
            }
            self.live_before.push(per_inst);
        }
    }

    fn compute_moved_entry_states(&mut self) {
        // Track which canonical places have been moved-out along each CFG edge. We use a simple
        // forward union dataflow: if a place is moved on any incoming path, it is treated as
        // moved after the join.
        let body = &self.func.body;
        self.moved_entry = vec![FxHashSet::default(); body.blocks.len()];

        let mut worklist: Vec<BasicBlockId> = vec![body.entry];
        let mut in_worklist = FxHashSet::default();
        in_worklist.insert(body.entry);

        while let Some(bb) = worklist.pop() {
            in_worklist.remove(&bb);

            let mut moved = self.moved_entry[bb.index()].clone();
            let mut state = self.entry_states[bb.index()].clone();
            for inst in &body.blocks[bb.index()].insts {
                self.update_moved_for_inst(inst, &state, &mut moved);
                self.update_state_for_inst(inst, &mut state);
            }
            self.update_moved_for_terminator(
                &body.blocks[bb.index()].terminator,
                &state,
                &mut moved,
            );

            for succ in successors(&body.blocks[bb.index()].terminator) {
                if self.join_moved_entry_state(succ, &moved) && in_worklist.insert(succ) {
                    worklist.push(succ);
                }
            }
        }
    }

    fn join_moved_entry_state(
        &mut self,
        succ: BasicBlockId,
        moved: &FxHashSet<CanonPlace<'db>>,
    ) -> bool {
        let before = self.moved_entry[succ.index()].len();
        self.moved_entry[succ.index()].extend(moved.iter().cloned());
        self.moved_entry[succ.index()].len() != before
    }

    fn update_moved_for_inst(
        &self,
        inst: &MirInst<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &mut FxHashSet<CanonPlace<'db>>,
    ) {
        for place in move_places_in_inst(&self.func.body, inst) {
            if !self.loans_for_place_base(state, place.base).is_empty() {
                continue;
            }
            moved.extend(self.canonicalize_place(state, &place));
        }

        match inst {
            MirInst::Assign {
                dest: Some(dest), ..
            } => {
                let root = self.root_for_local(*dest);
                moved.retain(|p| p.root != root);
            }
            MirInst::Store { place, .. } | MirInst::InitAggregate { place, .. } => {
                let written = self.canonicalize_place(state, place);
                moved.retain(|m| {
                    !written
                        .iter()
                        .any(|w| w.root == m.root && w.proj.is_prefix_of(&m.proj))
                });
            }
            _ => {}
        }
    }

    fn update_moved_for_terminator(
        &self,
        term: &Terminator<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &mut FxHashSet<CanonPlace<'db>>,
    ) {
        for place in move_places_in_terminator(&self.func.body, term) {
            if !self.loans_for_place_base(state, place.base).is_empty() {
                continue;
            }
            moved.extend(self.canonicalize_place(state, &place));
        }
    }

    fn check_moved_and_moves_in_inst(
        &self,
        inst: &MirInst<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &FxHashSet<CanonPlace<'db>>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        for place in move_places_in_inst(&self.func.body, inst) {
            if let Some(err) = self.check_move_out_place(&place, state, moved, active, suspended) {
                return Some(err);
            }
        }

        for value in borrow_values_in_inst(&self.func.body, inst) {
            if ty_is_borrow(self.db, self.func.body.value(value).ty).is_none() {
                continue;
            }
            let ValueOrigin::PlaceRef(place) = &self.func.body.value(value).origin else {
                continue;
            };
            let accessed = self.canonicalize_place(state, place);
            if place_set_overlaps(&accessed, moved) {
                return Some("move conflict: borrow of moved place".to_string());
            }
        }

        match inst {
            MirInst::Assign {
                rvalue: Rvalue::Load { place },
                ..
            } => {
                let accessed = self.canonicalize_place(state, place);
                if place_set_overlaps(&accessed, moved) {
                    return Some("move conflict: use after move".to_string());
                }
            }
            MirInst::Store { place, .. } | MirInst::InitAggregate { place, .. } => {
                let written = self.canonicalize_place(state, place);
                if let Some(err) = self.check_write_through_moved_parent(&written, moved) {
                    return Some(err);
                }
            }
            _ => {}
        }

        for value in value_operands_in_inst(inst) {
            if let Some(err) = self.check_value_reads_after_move(value, moved) {
                return Some(err);
            }
        }

        match inst {
            MirInst::Store { place, .. }
            | MirInst::InitAggregate { place, .. }
            | MirInst::SetDiscriminant { place, .. } => {
                self.check_place_path_indices_after_move(&place.projection, moved)
            }
            MirInst::Assign { rvalue, .. } => {
                if let Rvalue::Load { place } = rvalue {
                    self.check_place_path_indices_after_move(&place.projection, moved)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn check_moved_and_moves_in_terminator(
        &self,
        term: &Terminator<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &FxHashSet<CanonPlace<'db>>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        for place in move_places_in_terminator(&self.func.body, term) {
            if let Some(err) = self.check_move_out_place(&place, state, moved, active, suspended) {
                return Some(err);
            }
        }

        for value in borrow_values_in_terminator(&self.func.body, term) {
            if ty_is_borrow(self.db, self.func.body.value(value).ty).is_none() {
                continue;
            }
            let ValueOrigin::PlaceRef(place) = &self.func.body.value(value).origin else {
                continue;
            };
            let accessed = self.canonicalize_place(state, place);
            if place_set_overlaps(&accessed, moved) {
                return Some("move conflict: borrow of moved place".to_string());
            }
        }

        for value in value_operands_in_terminator(term) {
            if let Some(err) = self.check_value_reads_after_move(value, moved) {
                return Some(err);
            }
        }

        None
    }

    fn check_value_reads_after_move(
        &self,
        value: ValueId,
        moved: &FxHashSet<CanonPlace<'db>>,
    ) -> Option<String> {
        let mut locals = FxHashSet::default();
        collect_value_locals_in_value(&self.func.body, value, &mut locals);
        for local in locals {
            let accessed = CanonPlace {
                root: self.root_for_local(local),
                proj: crate::MirProjectionPath::new(),
            };
            if moved.iter().any(|m| places_overlap(m, &accessed)) {
                return Some("move conflict: use after move".to_string());
            }
        }
        None
    }

    fn check_place_path_indices_after_move(
        &self,
        path: &crate::MirProjectionPath<'db>,
        moved: &FxHashSet<CanonPlace<'db>>,
    ) -> Option<String> {
        for proj in path.iter() {
            if let hir::projection::Projection::Index(hir::projection::IndexSource::Dynamic(value)) =
                proj
                && let Some(err) = self.check_value_reads_after_move(*value, moved)
            {
                return Some(err);
            }
        }
        None
    }

    fn check_write_through_moved_parent(
        &self,
        written: &FxHashSet<CanonPlace<'db>>,
        moved: &FxHashSet<CanonPlace<'db>>,
    ) -> Option<String> {
        for w in written {
            for m in moved {
                if w.root == m.root && m.proj.is_prefix_of(&w.proj) && m.proj != w.proj {
                    return Some("move conflict: write through moved place".to_string());
                }
            }
        }
        None
    }

    fn check_move_out_place(
        &self,
        place: &Place<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &FxHashSet<CanonPlace<'db>>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        if !self.loans_for_place_base(state, place.base).is_empty() {
            return Some("move conflict: cannot move out through borrow handle".to_string());
        }

        let targets = self.canonicalize_place(state, place);
        if place_set_overlaps(&targets, moved) {
            return Some("move conflict: use after move".to_string());
        }

        for target in &targets {
            let Root::Param(idx) = target.root else {
                continue;
            };
            let mode = self
                .param_modes
                .get(idx as usize)
                .copied()
                .unwrap_or_else(|| panic!("missing param mode"));
            if mode == FuncParamMode::View {
                return Some("move conflict: cannot move out of view param".to_string());
            }
        }

        for &loan in active {
            if suspended.contains(&loan) {
                continue;
            }
            if place_set_overlaps(&self.loans[loan.0 as usize].targets, &targets) {
                return Some("move conflict: cannot move out of borrowed place".to_string());
            }
        }

        None
    }

    fn check_conflicts(self) -> Option<String> {
        let body = &self.func.body;
        for (bb_idx, block) in body.blocks.iter().enumerate() {
            let mut state = self.entry_states[bb_idx].clone();
            let mut moved = self.moved_entry[bb_idx].clone();
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let mut active = self.active_loans(&state, &self.live_before[bb_idx][inst_idx]);
                let temp = self.borrow_loans_in_inst(inst);
                active.extend(temp.iter().copied());
                if let Some(loan) = self.call_loan_in_inst(inst) {
                    active.insert(loan);
                }
                let suspended = self.suspended_loans(&active);
                let effective: Vec<_> = active
                    .iter()
                    .copied()
                    .filter(|l| !suspended.contains(l))
                    .collect();

                if let Some(err) =
                    self.check_moved_and_moves_in_inst(inst, &state, &moved, &active, &suspended)
                {
                    return Some(err);
                }
                if let Some(err) = self.check_active_set(&effective) {
                    return Some(err);
                }
                if let Some(err) = self.check_borrow_creations(inst, &active, &suspended) {
                    return Some(err);
                }
                if let Some(err) = self.check_accesses(inst, &state, &active, &suspended) {
                    return Some(err);
                }

                self.update_moved_for_inst(inst, &state, &mut moved);
                self.update_state_for_inst(inst, &mut state);
            }

            let mut active = self.active_loans(&state, &self.live_before_term[bb_idx]);
            let term_borrows = borrow_values_in_terminator(body, &block.terminator);
            for value in &term_borrows {
                if let Some(&loan) = self.loan_for_value.get(value) {
                    active.insert(loan);
                }
            }
            let suspended = self.suspended_loans(&active);
            let effective: Vec<_> = active
                .iter()
                .copied()
                .filter(|l| !suspended.contains(l))
                .collect();
            if let Some(err) = self.check_moved_and_moves_in_terminator(
                &block.terminator,
                &state,
                &moved,
                &active,
                &suspended,
            ) {
                return Some(err);
            }
            if let Some(err) = self.check_active_set(&effective) {
                return Some(err);
            }
            if let Some(err) =
                self.check_borrow_creations_in_values(&term_borrows, &active, &suspended)
            {
                return Some(err);
            }
        }
        None
    }

    fn join_entry_state(&mut self, succ: BasicBlockId, state: &[FxHashSet<LoanId>]) -> bool {
        let succ_state = &mut self.entry_states[succ.index()];
        let mut changed = false;
        for (idx, loans) in succ_state.iter_mut().enumerate() {
            let before = loans.len();
            loans.extend(state[idx].iter().copied());
            changed |= loans.len() != before;
        }
        changed
    }

    fn update_state_for_inst(&self, inst: &MirInst<'db>, state: &mut [FxHashSet<LoanId>]) {
        let body = &self.func.body;
        match inst {
            MirInst::Assign {
                dest: Some(dest),
                rvalue: Rvalue::Call(call),
                ..
            } => {
                let Some(idx) = self.tracked_local_idx.get(dest.index()).copied().flatten() else {
                    return;
                };
                let Some(expr) = call.expr else {
                    panic!("borrow-handle call must carry its ExprId for analysis");
                };
                let Some(&call_value) = body.expr_values.get(&expr) else {
                    panic!("missing value id for call expr {expr:?}");
                };
                let Some(&loan) = self.call_loan_for_value.get(&call_value) else {
                    panic!("missing loan id for borrow-handle call expr {expr:?}");
                };

                state[idx].clear();
                state[idx].insert(loan);
            }
            MirInst::Assign {
                dest: Some(dest),
                rvalue,
                ..
            } => {
                let Some(idx) = self.tracked_local_idx.get(dest.index()).copied().flatten() else {
                    return;
                };
                state[idx] = self.loans_in_rvalue(state, rvalue);
            }
            MirInst::Store { place, value } => {
                if let Some(root) = root_memory_local(body, place)
                    && let Some(idx) = self.tracked_local_idx.get(root.index()).copied().flatten()
                {
                    state[idx].extend(self.loans_in_value(state, *value));
                }
            }
            MirInst::InitAggregate { place, inits } => {
                if let Some(root) = root_memory_local(body, place)
                    && let Some(idx) = self.tracked_local_idx.get(root.index()).copied().flatten()
                {
                    for (_, value) in inits {
                        state[idx].extend(self.loans_in_value(state, *value));
                    }
                }
            }
            _ => {}
        }
    }

    fn loans_in_rvalue(
        &self,
        state: &[FxHashSet<LoanId>],
        rvalue: &Rvalue<'db>,
    ) -> FxHashSet<LoanId> {
        match rvalue {
            Rvalue::Value(value) => self.loans_in_value(state, *value),
            Rvalue::Load { .. }
            | Rvalue::ZeroInit
            | Rvalue::Call(_)
            | Rvalue::Intrinsic { .. }
            | Rvalue::Alloc { .. } => FxHashSet::default(),
        }
    }

    fn loans_in_value(&self, state: &[FxHashSet<LoanId>], value: ValueId) -> FxHashSet<LoanId> {
        let value_data = self.func.body.value(value);
        if !ty_is_noesc(self.db, value_data.ty) {
            return FxHashSet::default();
        }
        if let Some(&loan) = self.loan_for_value.get(&value) {
            let mut out = FxHashSet::default();
            out.insert(loan);
            return out;
        }
        match &value_data.origin {
            ValueOrigin::Local(local) => self
                .tracked_local_idx
                .get(local.index())
                .copied()
                .flatten()
                .map(|idx| state[idx].clone())
                .unwrap_or_default(),
            ValueOrigin::TransparentCast { value } => self.loans_in_value(state, *value),
            ValueOrigin::Unary { inner, .. } => self.loans_in_value(state, *inner),
            ValueOrigin::Binary { lhs, rhs, .. } => {
                let mut out = self.loans_in_value(state, *lhs);
                out.extend(self.loans_in_value(state, *rhs));
                out
            }
            _ => FxHashSet::default(),
        }
    }

    fn update_loan_info_for_inst(
        &mut self,
        inst: &MirInst<'db>,
        state: &[FxHashSet<LoanId>],
        changed: &mut bool,
    ) {
        if let MirInst::Assign {
            dest: Some(dest),
            rvalue: Rvalue::Call(call),
            ..
        } = inst
        {
            self.update_loan_from_call(state, *dest, call, changed);
        }
        for value in borrow_values_in_inst(&self.func.body, inst) {
            self.update_loan_from_value(state, value, changed);
        }
    }

    fn update_loan_info_for_terminator(
        &mut self,
        term: &Terminator<'db>,
        state: &[FxHashSet<LoanId>],
        changed: &mut bool,
    ) {
        for value in borrow_values_in_terminator(&self.func.body, term) {
            self.update_loan_from_value(state, value, changed);
        }
    }

    fn update_loan_from_value(
        &mut self,
        state: &[FxHashSet<LoanId>],
        value: ValueId,
        changed: &mut bool,
    ) {
        let Some(&loan_id) = self.loan_for_value.get(&value) else {
            return;
        };
        let ValueOrigin::PlaceRef(place) = &self.func.body.value(value).origin else {
            return;
        };

        if !matches!(
            self.func.body.place_address_space(place),
            AddressSpaceKind::Memory
        ) {
            panic!("borrow handles must point into memory");
        }

        let targets = self.canonicalize_place(state, place);
        let before = self.loans[loan_id.0 as usize].targets.len();
        self.loans[loan_id.0 as usize].targets.extend(targets);
        *changed |= self.loans[loan_id.0 as usize].targets.len() != before;

        let parents = self.mut_loans_for_handle_value(state, place.base);
        let before = self.loans[loan_id.0 as usize].parents.len();
        self.loans[loan_id.0 as usize].parents.extend(parents);
        *changed |= self.loans[loan_id.0 as usize].parents.len() != before;
    }

    fn update_loan_from_call(
        &mut self,
        state: &[FxHashSet<LoanId>],
        dest: LocalId,
        call: &CallOrigin<'db>,
        changed: &mut bool,
    ) {
        if ty_is_borrow(self.db, self.func.body.local(dest).ty).is_none() {
            return;
        }
        let Some(expr) = call.expr else {
            panic!("borrow-handle call must carry its ExprId for analysis");
        };
        let Some(&call_value) = self.func.body.expr_values.get(&expr) else {
            panic!("missing value id for call expr {expr:?}");
        };
        let Some(&loan_id) = self.call_loan_for_value.get(&call_value) else {
            panic!("missing loan id for borrow-handle call expr {expr:?}");
        };
        let Some(callee) = call.resolved_name.as_ref() else {
            panic!("borrow-handle call must have a resolved callee name");
        };
        let Some(summary) = self.summaries.get(callee) else {
            panic!("missing borrow summary for callee `{callee}`");
        };

        let mut targets = FxHashSet::default();
        let mut parents = FxHashSet::default();
        for transform in summary {
            let Some(&arg) = call.args.get(transform.param_index as usize) else {
                panic!("borrow summary index out of bounds for `{callee}`");
            };
            parents.extend(self.mut_loans_for_handle_value(state, arg));
            for base in self.canonicalize_base(state, arg) {
                targets.insert(CanonPlace {
                    root: base.root,
                    proj: base.proj.concat(&transform.proj),
                });
            }
        }

        let before = self.loans[loan_id.0 as usize].targets.len();
        self.loans[loan_id.0 as usize].targets.extend(targets);
        *changed |= self.loans[loan_id.0 as usize].targets.len() != before;

        let before = self.loans[loan_id.0 as usize].parents.len();
        self.loans[loan_id.0 as usize].parents.extend(parents);
        *changed |= self.loans[loan_id.0 as usize].parents.len() != before;
    }

    fn canonicalize_place(
        &self,
        state: &[FxHashSet<LoanId>],
        place: &Place<'db>,
    ) -> FxHashSet<CanonPlace<'db>> {
        let mut out = FxHashSet::default();
        for base in self.canonicalize_base(state, place.base) {
            out.insert(CanonPlace {
                root: base.root,
                proj: base.proj.concat(&place.projection),
            });
        }
        out
    }

    fn canonicalize_base(
        &self,
        state: &[FxHashSet<LoanId>],
        mut base: ValueId,
    ) -> FxHashSet<CanonPlace<'db>> {
        loop {
            match &self.func.body.value(base).origin {
                ValueOrigin::TransparentCast { value } => base = *value,
                ValueOrigin::PlaceRef(place) => return self.canonicalize_place(state, place),
                _ => break,
            }
        }

        let data = self.func.body.value(base);
        if ty_is_borrow(self.db, data.ty).is_some() {
            let mut out = FxHashSet::default();
            for loan in self.loans_for_handle_value(state, base) {
                out.extend(self.loans[loan.0 as usize].targets.iter().cloned());
            }
            return out;
        }

        match (&data.origin, data.repr) {
            (ValueOrigin::Local(local), ValueRepr::Ref(_)) => {
                let mut out = FxHashSet::default();
                out.insert(CanonPlace {
                    root: self.root_for_local(*local),
                    proj: crate::MirProjectionPath::new(),
                });
                out
            }
            _ => FxHashSet::default(),
        }
    }

    fn root_for_local(&self, local: LocalId) -> Root {
        self.param_index_of_local
            .get(local.index())
            .copied()
            .flatten()
            .map(Root::Param)
            .unwrap_or(Root::Local(local))
    }

    fn loans_for_handle_value(
        &self,
        state: &[FxHashSet<LoanId>],
        mut value: ValueId,
    ) -> FxHashSet<LoanId> {
        loop {
            match &self.func.body.value(value).origin {
                ValueOrigin::TransparentCast { value: inner } => value = *inner,
                _ => break,
            }
        }

        if let Some(&loan) = self.loan_for_value.get(&value) {
            let mut out = FxHashSet::default();
            out.insert(loan);
            return out;
        }

        let ValueOrigin::Local(local) = self.func.body.value(value).origin else {
            return FxHashSet::default();
        };
        self.tracked_local_idx
            .get(local.index())
            .copied()
            .flatten()
            .map(|idx| state[idx].clone())
            .unwrap_or_default()
    }

    fn mut_loans_for_handle_value(
        &self,
        state: &[FxHashSet<LoanId>],
        value: ValueId,
    ) -> FxHashSet<LoanId> {
        self.loans_for_handle_value(state, value)
            .into_iter()
            .filter(|loan| matches!(self.loans[loan.0 as usize].kind, BorrowKind::Mut))
            .collect()
    }

    fn active_loans(
        &self,
        state: &[FxHashSet<LoanId>],
        live: &FxHashSet<LocalId>,
    ) -> FxHashSet<LoanId> {
        let mut out = FxHashSet::default();
        for local in live {
            if let Some(idx) = self.tracked_local_idx.get(local.index()).copied().flatten() {
                out.extend(state[idx].iter().copied());
            }
        }
        out
    }

    fn suspended_loans(&self, active: &FxHashSet<LoanId>) -> FxHashSet<LoanId> {
        let mut suspended = FxHashSet::default();
        let mut worklist: Vec<_> = active.iter().copied().collect();
        while let Some(current) = worklist.pop() {
            for &parent in &self.loans[current.0 as usize].parents {
                if suspended.insert(parent) {
                    worklist.push(parent);
                }
            }
        }
        suspended
    }

    fn check_active_set(&self, active: &[LoanId]) -> Option<String> {
        for (idx, &a) in active.iter().enumerate() {
            for &b in &active[idx + 1..] {
                if self.loans_conflict(a, b) {
                    return Some("borrow conflict: overlapping active loans".to_string());
                }
            }
        }
        None
    }

    fn borrow_loans_in_inst(&self, inst: &MirInst<'db>) -> FxHashSet<LoanId> {
        let mut out = FxHashSet::default();
        for value in borrow_values_in_inst(&self.func.body, inst) {
            if let Some(&loan) = self.loan_for_value.get(&value) {
                out.insert(loan);
            }
        }
        out
    }

    fn call_loan_in_inst(&self, inst: &MirInst<'db>) -> Option<LoanId> {
        let MirInst::Assign {
            rvalue: Rvalue::Call(call),
            ..
        } = inst
        else {
            return None;
        };
        let expr = call.expr?;
        let call_value = *self.func.body.expr_values.get(&expr)?;
        self.call_loan_for_value.get(&call_value).copied()
    }

    fn check_borrow_creations(
        &self,
        inst: &MirInst<'db>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        if let Some(loan) = self.call_loan_in_inst(inst)
            && let Some(err) = self.check_loan_creation(loan, active, suspended)
        {
            return Some(err);
        }
        self.check_borrow_creations_in_values(
            &borrow_values_in_inst(&self.func.body, inst),
            active,
            suspended,
        )
    }

    fn check_loan_creation(
        &self,
        loan: LoanId,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        if suspended.contains(&loan) {
            return None;
        }
        let kind = self.loans[loan.0 as usize].kind;
        let parents = &self.loans[loan.0 as usize].parents;
        for &other in active {
            if other == loan || parents.contains(&other) || suspended.contains(&other) {
                continue;
            }
            if loans_overlap(&self.loans[loan.0 as usize], &self.loans[other.0 as usize])
                && match kind {
                    BorrowKind::Mut => true,
                    BorrowKind::Ref => matches!(self.loans[other.0 as usize].kind, BorrowKind::Mut),
                }
            {
                return Some("borrow conflict: cannot create overlapping loan".to_string());
            }
        }
        None
    }

    fn check_borrow_creations_in_values(
        &self,
        values: &FxHashSet<ValueId>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        for value in values {
            let Some(&loan) = self.loan_for_value.get(value) else {
                continue;
            };
            if let Some(err) = self.check_loan_creation(loan, active, suspended) {
                return Some(err);
            }
        }
        None
    }

    fn check_accesses(
        &self,
        inst: &MirInst<'db>,
        state: &[FxHashSet<LoanId>],
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        if let Some(err) = self.check_word_value_reads(inst, active, suspended) {
            return Some(err);
        }
        match inst {
            MirInst::Assign {
                dest: Some(dest), ..
            } => {
                let accessed = CanonPlace {
                    root: self.root_for_local(*dest),
                    proj: crate::MirProjectionPath::new(),
                };
                self.check_access_set(
                    &FxHashSet::from_iter([accessed]),
                    FxHashSet::default(),
                    AccessKind::Write,
                    active,
                    suspended,
                )
            }
            MirInst::Assign { rvalue, .. } => match rvalue {
                Rvalue::Load { place } => {
                    self.check_place_access(state, place, AccessKind::Read, active, suspended)
                }
                _ => None,
            },
            MirInst::Store { place, .. } => {
                self.check_place_access(state, place, AccessKind::Write, active, suspended)
            }
            MirInst::InitAggregate { place, .. } => {
                self.check_place_access(state, place, AccessKind::Write, active, suspended)
            }
            _ => None,
        }
    }

    fn check_place_access(
        &self,
        state: &[FxHashSet<LoanId>],
        place: &Place<'db>,
        access: AccessKind,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        let through = self.loans_for_place_base(state, place.base);
        let accessed = self.canonicalize_place(state, place);
        self.check_access_set(&accessed, through, access, active, suspended)
    }

    fn check_access_set(
        &self,
        accessed: &FxHashSet<CanonPlace<'db>>,
        through: FxHashSet<LoanId>,
        access: AccessKind,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        if through.iter().any(|loan| suspended.contains(loan)) {
            return Some("borrow conflict: use of reborrowed `mut` handle".to_string());
        }

        let effective: Vec<_> = active
            .iter()
            .copied()
            .filter(|l| !suspended.contains(l))
            .collect();
        for loan in effective {
            let loan_data = &self.loans[loan.0 as usize];
            if !place_set_overlaps(&loan_data.targets, accessed) {
                continue;
            }
            match loan_data.kind {
                BorrowKind::Ref => {
                    if matches!(access, AccessKind::Write) {
                        return Some(
                            "borrow conflict: write through active `ref` loan".to_string(),
                        );
                    }
                }
                BorrowKind::Mut => {
                    if !through.contains(&loan) {
                        return Some(
                            "borrow conflict: access overlaps active `mut` loan".to_string(),
                        );
                    }
                }
            }
        }
        None
    }

    fn loans_for_place_base(
        &self,
        state: &[FxHashSet<LoanId>],
        mut base: ValueId,
    ) -> FxHashSet<LoanId> {
        loop {
            match &self.func.body.value(base).origin {
                ValueOrigin::TransparentCast { value } => base = *value,
                _ => break,
            }
        }
        if ty_is_borrow(self.db, self.func.body.value(base).ty).is_none() {
            return FxHashSet::default();
        }
        self.loans_for_handle_value(state, base)
    }

    fn check_word_value_reads(
        &self,
        inst: &MirInst<'db>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        let mut locals = FxHashSet::default();
        for value in value_operands_in_inst(inst) {
            collect_word_locals_in_value(&self.func.body, value, &mut locals);
        }

        for local in locals {
            let accessed = CanonPlace {
                root: self.root_for_local(local),
                proj: crate::MirProjectionPath::new(),
            };
            if let Some(err) = self.check_access_set(
                &FxHashSet::from_iter([accessed]),
                FxHashSet::default(),
                AccessKind::Read,
                active,
                suspended,
            ) {
                return Some(err);
            }
        }
        None
    }

    fn loans_conflict(&self, a: LoanId, b: LoanId) -> bool {
        match (self.loans[a.0 as usize].kind, self.loans[b.0 as usize].kind) {
            (BorrowKind::Ref, BorrowKind::Ref) => false,
            _ => loans_overlap(&self.loans[a.0 as usize], &self.loans[b.0 as usize]),
        }
    }
}

#[derive(Clone, Copy)]
enum AccessKind {
    Read,
    Write,
}

fn successors(term: &Terminator<'_>) -> Vec<BasicBlockId> {
    match term {
        Terminator::Goto { target } => vec![*target],
        Terminator::Branch {
            then_bb, else_bb, ..
        } => vec![*then_bb, *else_bb],
        Terminator::Switch {
            targets, default, ..
        } => targets
            .iter()
            .map(|t| t.block)
            .chain(std::iter::once(*default))
            .collect(),
        Terminator::Return(_) | Terminator::TerminatingCall(_) | Terminator::Unreachable => {
            Vec::new()
        }
    }
}

fn locals_used_by_inst<'db>(body: &MirBody<'db>, inst: &MirInst<'db>) -> FxHashSet<LocalId> {
    let mut out = FxHashSet::default();
    match inst {
        MirInst::Assign { rvalue, .. } => match rvalue {
            Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
            Rvalue::Value(value) => collect_locals_in_value(body, *value, &mut out),
            Rvalue::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    collect_locals_in_value(body, *arg, &mut out);
                }
            }
            Rvalue::Intrinsic { args, .. } => {
                for arg in args {
                    collect_locals_in_value(body, *arg, &mut out);
                }
            }
            Rvalue::Load { place } => {
                collect_locals_in_value(body, place.base, &mut out);
                collect_locals_in_place_path(body, &place.projection, &mut out);
            }
        },
        MirInst::Store { place, value } => {
            collect_locals_in_value(body, place.base, &mut out);
            collect_locals_in_place_path(body, &place.projection, &mut out);
            collect_locals_in_value(body, *value, &mut out);
        }
        MirInst::InitAggregate { place, inits } => {
            collect_locals_in_value(body, place.base, &mut out);
            collect_locals_in_place_path(body, &place.projection, &mut out);
            for (path, value) in inits {
                collect_locals_in_place_path(body, path, &mut out);
                collect_locals_in_value(body, *value, &mut out);
            }
        }
        MirInst::SetDiscriminant { place, .. } => {
            collect_locals_in_value(body, place.base, &mut out);
            collect_locals_in_place_path(body, &place.projection, &mut out);
        }
        MirInst::BindValue { value } => collect_locals_in_value(body, *value, &mut out),
    }
    out
}

fn locals_used_by_terminator<'db>(
    body: &MirBody<'db>,
    term: &Terminator<'db>,
) -> FxHashSet<LocalId> {
    let mut out = FxHashSet::default();
    match term {
        Terminator::Return(Some(value)) => collect_locals_in_value(body, *value, &mut out),
        Terminator::TerminatingCall(call) => match call {
            crate::TerminatingCall::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    collect_locals_in_value(body, *arg, &mut out);
                }
            }
            crate::TerminatingCall::Intrinsic { args, .. } => {
                for arg in args {
                    collect_locals_in_value(body, *arg, &mut out);
                }
            }
        },
        Terminator::Branch { cond, .. } | Terminator::Switch { discr: cond, .. } => {
            collect_locals_in_value(body, *cond, &mut out);
        }
        Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => {}
    }
    out
}

fn collect_locals_in_value<'db>(body: &MirBody<'db>, value: ValueId, out: &mut FxHashSet<LocalId>) {
    fn inner<'db>(
        body: &MirBody<'db>,
        value: ValueId,
        out: &mut FxHashSet<LocalId>,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }
        match &body.value(value).origin {
            ValueOrigin::Local(local) => {
                out.insert(*local);
            }
            ValueOrigin::TransparentCast { value } => inner(body, *value, out, visiting),
            ValueOrigin::Unary { inner: dep, .. } => inner(body, *dep, out, visiting),
            ValueOrigin::Binary { lhs, rhs, .. } => {
                inner(body, *lhs, out, visiting);
                inner(body, *rhs, out, visiting);
            }
            ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
                inner(body, place.base, out, visiting);
                collect_locals_in_place_path(body, &place.projection, out);
            }
            _ => {}
        }
    }
    inner(body, value, out, &mut FxHashSet::default());
}

fn collect_value_locals_in_value<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    out: &mut FxHashSet<LocalId>,
) {
    fn inner<'db>(
        body: &MirBody<'db>,
        value: ValueId,
        out: &mut FxHashSet<LocalId>,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }
        match &body.value(value).origin {
            ValueOrigin::Local(local) => {
                out.insert(*local);
            }
            ValueOrigin::TransparentCast { value } => inner(body, *value, out, visiting),
            ValueOrigin::Unary { inner: dep, .. } => inner(body, *dep, out, visiting),
            ValueOrigin::Binary { lhs, rhs, .. } => {
                inner(body, *lhs, out, visiting);
                inner(body, *rhs, out, visiting);
            }
            ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
                for proj in place.projection.iter() {
                    if let hir::projection::Projection::Index(
                        hir::projection::IndexSource::Dynamic(idx),
                    ) = proj
                    {
                        inner(body, *idx, out, visiting);
                    }
                }
            }
            _ => {}
        }
    }
    inner(body, value, out, &mut FxHashSet::default());
}

fn collect_word_locals_in_value<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    out: &mut FxHashSet<LocalId>,
) {
    fn inner<'db>(
        body: &MirBody<'db>,
        value: ValueId,
        out: &mut FxHashSet<LocalId>,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }
        match (&body.value(value).origin, body.value(value).repr) {
            (ValueOrigin::Local(local), ValueRepr::Word) => {
                out.insert(*local);
            }
            (ValueOrigin::TransparentCast { value }, _) => inner(body, *value, out, visiting),
            (ValueOrigin::Unary { inner: dep, .. }, _) => inner(body, *dep, out, visiting),
            (ValueOrigin::Binary { lhs, rhs, .. }, _) => {
                inner(body, *lhs, out, visiting);
                inner(body, *rhs, out, visiting);
            }
            (ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place }, _) => {
                inner(body, place.base, out, visiting);
                for proj in place.projection.iter() {
                    if let hir::projection::Projection::Index(
                        hir::projection::IndexSource::Dynamic(idx),
                    ) = proj
                    {
                        inner(body, *idx, out, visiting);
                    }
                }
            }
            _ => {}
        }
    }
    inner(body, value, out, &mut FxHashSet::default());
}

fn value_operands_in_inst(inst: &MirInst<'_>) -> Vec<ValueId> {
    match inst {
        MirInst::Assign { rvalue, .. } => match rvalue {
            Rvalue::Value(value) => vec![*value],
            Rvalue::Call(call) => call
                .args
                .iter()
                .chain(call.effect_args.iter())
                .copied()
                .collect(),
            Rvalue::Intrinsic { args, .. } => args.clone(),
            Rvalue::Load { .. } | Rvalue::ZeroInit | Rvalue::Alloc { .. } => Vec::new(),
        },
        MirInst::Store { value, .. } => vec![*value],
        MirInst::InitAggregate { inits, .. } => inits.iter().map(|(_, v)| *v).collect(),
        MirInst::BindValue { value } => vec![*value],
        MirInst::SetDiscriminant { .. } => Vec::new(),
    }
}

fn value_operands_in_terminator(term: &Terminator<'_>) -> Vec<ValueId> {
    match term {
        Terminator::Return(Some(value)) => vec![*value],
        Terminator::TerminatingCall(call) => match call {
            crate::TerminatingCall::Call(call) => call
                .args
                .iter()
                .chain(call.effect_args.iter())
                .copied()
                .collect(),
            crate::TerminatingCall::Intrinsic { args, .. } => args.clone(),
        },
        Terminator::Branch { cond, .. } => vec![*cond],
        Terminator::Switch { discr, .. } => vec![*discr],
        Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => Vec::new(),
    }
}

fn collect_locals_in_place_path<'db>(
    body: &MirBody<'db>,
    path: &crate::MirProjectionPath<'db>,
    out: &mut FxHashSet<LocalId>,
) {
    for proj in path.iter() {
        if let hir::projection::Projection::Index(hir::projection::IndexSource::Dynamic(value)) =
            proj
        {
            collect_locals_in_value(body, *value, out);
        }
    }
}

fn borrow_values_in_inst<'db>(body: &MirBody<'db>, inst: &MirInst<'db>) -> FxHashSet<ValueId> {
    let mut out = FxHashSet::default();
    match inst {
        MirInst::Assign { rvalue, .. } => match rvalue {
            Rvalue::Value(value) => collect_borrow_values(body, *value, &mut out),
            Rvalue::Load { place } => {
                collect_borrow_values(body, place.base, &mut out);
                collect_borrow_values_in_place_path(body, &place.projection, &mut out);
            }
            Rvalue::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    collect_borrow_values(body, *arg, &mut out);
                }
            }
            Rvalue::Intrinsic { args, .. } => {
                for arg in args {
                    collect_borrow_values(body, *arg, &mut out);
                }
            }
            Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
        },
        MirInst::Store { place, value } => {
            collect_borrow_values(body, place.base, &mut out);
            collect_borrow_values_in_place_path(body, &place.projection, &mut out);
            collect_borrow_values(body, *value, &mut out);
        }
        MirInst::InitAggregate { place, inits } => {
            collect_borrow_values(body, place.base, &mut out);
            collect_borrow_values_in_place_path(body, &place.projection, &mut out);
            for (path, value) in inits {
                collect_borrow_values_in_place_path(body, path, &mut out);
                collect_borrow_values(body, *value, &mut out);
            }
        }
        MirInst::SetDiscriminant { place, .. } => {
            collect_borrow_values(body, place.base, &mut out);
            collect_borrow_values_in_place_path(body, &place.projection, &mut out);
        }
        MirInst::BindValue { value } => collect_borrow_values(body, *value, &mut out),
    }
    out
}

fn borrow_values_in_terminator<'db>(
    body: &MirBody<'db>,
    term: &Terminator<'db>,
) -> FxHashSet<ValueId> {
    let mut out = FxHashSet::default();
    match term {
        Terminator::Return(Some(value)) => collect_borrow_values(body, *value, &mut out),
        Terminator::TerminatingCall(call) => match call {
            crate::TerminatingCall::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    collect_borrow_values(body, *arg, &mut out);
                }
            }
            crate::TerminatingCall::Intrinsic { args, .. } => {
                for arg in args {
                    collect_borrow_values(body, *arg, &mut out);
                }
            }
        },
        Terminator::Branch { cond, .. } | Terminator::Switch { discr: cond, .. } => {
            collect_borrow_values(body, *cond, &mut out);
        }
        Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => {}
    }
    out
}

fn move_places_in_inst<'db>(body: &MirBody<'db>, inst: &MirInst<'db>) -> Vec<Place<'db>> {
    let mut out = Vec::new();
    match inst {
        MirInst::Assign { rvalue, .. } => match rvalue {
            Rvalue::Value(value) => collect_move_places(body, *value, &mut out),
            Rvalue::Load { place } => {
                collect_move_places(body, place.base, &mut out);
                collect_move_places_in_place_path(body, &place.projection, &mut out);
            }
            Rvalue::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    collect_move_places(body, *arg, &mut out);
                }
            }
            Rvalue::Intrinsic { args, .. } => {
                for arg in args {
                    collect_move_places(body, *arg, &mut out);
                }
            }
            Rvalue::ZeroInit | Rvalue::Alloc { .. } => {}
        },
        MirInst::Store { place, value } => {
            collect_move_places(body, place.base, &mut out);
            collect_move_places_in_place_path(body, &place.projection, &mut out);
            collect_move_places(body, *value, &mut out);
        }
        MirInst::InitAggregate { place, inits } => {
            collect_move_places(body, place.base, &mut out);
            collect_move_places_in_place_path(body, &place.projection, &mut out);
            for (path, value) in inits {
                collect_move_places_in_place_path(body, path, &mut out);
                collect_move_places(body, *value, &mut out);
            }
        }
        MirInst::SetDiscriminant { place, .. } => {
            collect_move_places(body, place.base, &mut out);
            collect_move_places_in_place_path(body, &place.projection, &mut out);
        }
        MirInst::BindValue { value } => collect_move_places(body, *value, &mut out),
    }
    out
}

fn move_places_in_terminator<'db>(body: &MirBody<'db>, term: &Terminator<'db>) -> Vec<Place<'db>> {
    let mut out = Vec::new();
    match term {
        Terminator::Return(Some(value)) => collect_move_places(body, *value, &mut out),
        Terminator::TerminatingCall(call) => match call {
            crate::TerminatingCall::Call(call) => {
                for arg in call.args.iter().chain(call.effect_args.iter()) {
                    collect_move_places(body, *arg, &mut out);
                }
            }
            crate::TerminatingCall::Intrinsic { args, .. } => {
                for arg in args {
                    collect_move_places(body, *arg, &mut out);
                }
            }
        },
        Terminator::Branch { cond, .. } | Terminator::Switch { discr: cond, .. } => {
            collect_move_places(body, *cond, &mut out);
        }
        Terminator::Return(None) | Terminator::Goto { .. } | Terminator::Unreachable => {}
    }
    out
}

fn collect_move_places_in_place_path<'db>(
    body: &MirBody<'db>,
    path: &crate::MirProjectionPath<'db>,
    out: &mut Vec<Place<'db>>,
) {
    for proj in path.iter() {
        if let hir::projection::Projection::Index(hir::projection::IndexSource::Dynamic(value)) =
            proj
        {
            collect_move_places(body, *value, out);
        }
    }
}

fn collect_move_places<'db>(body: &MirBody<'db>, value: ValueId, out: &mut Vec<Place<'db>>) {
    fn inner<'db>(
        body: &MirBody<'db>,
        value: ValueId,
        out: &mut Vec<Place<'db>>,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }
        match &body.value(value).origin {
            ValueOrigin::MoveOut { place } => {
                out.push(place.clone());
                inner(body, place.base, out, visiting);
                for proj in place.projection.iter() {
                    if let hir::projection::Projection::Index(
                        hir::projection::IndexSource::Dynamic(idx),
                    ) = proj
                    {
                        inner(body, *idx, out, visiting);
                    }
                }
            }
            ValueOrigin::TransparentCast { value } => inner(body, *value, out, visiting),
            ValueOrigin::Unary { inner: dep, .. } => inner(body, *dep, out, visiting),
            ValueOrigin::Binary { lhs, rhs, .. } => {
                inner(body, *lhs, out, visiting);
                inner(body, *rhs, out, visiting);
            }
            ValueOrigin::PlaceRef(place) => {
                inner(body, place.base, out, visiting);
                for proj in place.projection.iter() {
                    if let hir::projection::Projection::Index(
                        hir::projection::IndexSource::Dynamic(idx),
                    ) = proj
                    {
                        inner(body, *idx, out, visiting);
                    }
                }
            }
            _ => {}
        }
    }
    inner(body, value, out, &mut FxHashSet::default());
}

fn collect_borrow_values<'db>(body: &MirBody<'db>, value: ValueId, out: &mut FxHashSet<ValueId>) {
    fn inner<'db>(
        body: &MirBody<'db>,
        value: ValueId,
        out: &mut FxHashSet<ValueId>,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }
        if matches!(body.value(value).origin, ValueOrigin::PlaceRef(_)) {
            out.insert(value);
        }
        match &body.value(value).origin {
            ValueOrigin::TransparentCast { value } => inner(body, *value, out, visiting),
            ValueOrigin::Unary { inner: dep, .. } => inner(body, *dep, out, visiting),
            ValueOrigin::Binary { lhs, rhs, .. } => {
                inner(body, *lhs, out, visiting);
                inner(body, *rhs, out, visiting);
            }
            ValueOrigin::PlaceRef(place) | ValueOrigin::MoveOut { place } => {
                inner(body, place.base, out, visiting);
                collect_borrow_values_in_place_path(body, &place.projection, out);
            }
            _ => {}
        }
    }
    inner(body, value, out, &mut FxHashSet::default());
}

fn collect_borrow_values_in_place_path<'db>(
    body: &MirBody<'db>,
    path: &crate::MirProjectionPath<'db>,
    out: &mut FxHashSet<ValueId>,
) {
    for proj in path.iter() {
        if let hir::projection::Projection::Index(hir::projection::IndexSource::Dynamic(value)) =
            proj
        {
            collect_borrow_values(body, *value, out);
        }
    }
}

fn root_memory_local<'db>(body: &MirBody<'db>, place: &Place<'db>) -> Option<LocalId> {
    let mut base = place.base;
    loop {
        match &body.value(base).origin {
            ValueOrigin::TransparentCast { value } => base = *value,
            _ => break,
        }
    }
    match (&body.value(base).origin, body.value(base).repr) {
        (ValueOrigin::Local(local), ValueRepr::Ref(AddressSpaceKind::Memory)) => Some(*local),
        _ => None,
    }
}

fn loans_overlap<'db>(a: &Loan<'db>, b: &Loan<'db>) -> bool {
    place_set_overlaps(&a.targets, &b.targets)
}

fn place_set_overlaps<'db>(a: &FxHashSet<CanonPlace<'db>>, b: &FxHashSet<CanonPlace<'db>>) -> bool {
    a.iter().any(|p| b.iter().any(|q| places_overlap(p, q)))
}

fn places_overlap<'db>(a: &CanonPlace<'db>, b: &CanonPlace<'db>) -> bool {
    a.root == b.root && !matches!(a.proj.may_alias(&b.proj), Aliasing::No)
}
