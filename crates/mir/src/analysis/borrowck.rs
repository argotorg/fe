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
use hir::projection::Aliasing;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{
    AddressSpaceKind, BasicBlockId, LocalId, MirBody, MirFunction, MirInst, Place, Rvalue,
    Terminator, ValueId, ValueOrigin, ValueRepr,
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
    parent: Option<LoanId>,
    parent_candidates: FxHashSet<LoanId>,
}

pub fn check_borrows<'db>(db: &'db dyn HirAnalysisDb, func: &MirFunction<'db>) -> Option<String> {
    Borrowck::new(db, func).run()
}

struct Borrowck<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    func: &'a MirFunction<'db>,
    tracked_local_idx: Vec<Option<usize>>,
    tracked_locals: Vec<LocalId>,
    param_index_of_local: Vec<Option<u32>>,
    param_loan_for_local: Vec<Option<LoanId>>,
    loan_for_value: FxHashMap<ValueId, LoanId>,
    loans: Vec<Loan<'db>>,
    entry_states: Vec<Vec<FxHashSet<LoanId>>>,
    live_before: Vec<Vec<FxHashSet<LocalId>>>,
    live_before_term: Vec<FxHashSet<LocalId>>,
}

impl<'db, 'a> Borrowck<'db, 'a> {
    fn new(db: &'db dyn HirAnalysisDb, func: &'a MirFunction<'db>) -> Self {
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

        Self {
            db,
            func,
            tracked_local_idx,
            tracked_locals,
            param_index_of_local,
            param_loan_for_local: vec![None; body.locals.len()],
            loan_for_value: FxHashMap::default(),
            loans: Vec::new(),
            entry_states: Vec::new(),
            live_before: Vec::new(),
            live_before_term: Vec::new(),
        }
    }

    fn run(mut self) -> Option<String> {
        self.entry_states = vec![
            vec![FxHashSet::default(); self.tracked_locals.len()];
            self.func.body.blocks.len()
        ];
        self.init_loans();
        self.seed_param_loans();
        self.compute_entry_states();
        self.compute_loan_targets_and_parents();
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
                parent: None,
                parent_candidates: FxHashSet::default(),
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
                parent: None,
                parent_candidates: FxHashSet::default(),
            });
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

        for loan in &mut self.loans {
            loan.parent = (loan.parent_candidates.len() == 1)
                .then(|| *loan.parent_candidates.iter().next().unwrap());
        }
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

    fn check_conflicts(self) -> Option<String> {
        let body = &self.func.body;
        for (bb_idx, block) in body.blocks.iter().enumerate() {
            let mut state = self.entry_states[bb_idx].clone();
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let mut active = self.active_loans(&state, &self.live_before[bb_idx][inst_idx]);
                let temp = self.borrow_loans_in_inst(inst);
                active.extend(temp.iter().copied());
                let suspended = self.suspended_loans(&active);
                let effective: Vec<_> = active
                    .iter()
                    .copied()
                    .filter(|l| !suspended.contains(l))
                    .collect();

                if let Some(err) = self.check_active_set(&effective) {
                    return Some(err);
                }
                if let Some(err) = self.check_borrow_creations(inst, &active, &suspended) {
                    return Some(err);
                }
                if let Some(err) = self.check_accesses(inst, &state, &active, &suspended) {
                    return Some(err);
                }

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

        if let Some(parent) = self.parent_loan_for_place_base(state, place.base) {
            self.loans[loan_id.0 as usize]
                .parent_candidates
                .insert(parent);
        }
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

    fn parent_loan_for_place_base(
        &self,
        state: &[FxHashSet<LoanId>],
        base: ValueId,
    ) -> Option<LoanId> {
        let mut value = base;
        loop {
            match &self.func.body.value(value).origin {
                ValueOrigin::TransparentCast { value: inner } => value = *inner,
                _ => break,
            }
        }
        if !ty_is_borrow(self.db, self.func.body.value(value).ty).is_some() {
            return None;
        }
        let loans = self.loans_for_handle_value(state, value);
        if loans.len() != 1 {
            return None;
        }
        let parent = *loans.iter().next().unwrap();
        matches!(self.loans[parent.0 as usize].kind, BorrowKind::Mut).then_some(parent)
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
        for &loan in active {
            let mut current = loan;
            while let Some(parent) = self.loans[current.0 as usize].parent {
                if !suspended.insert(parent) {
                    break;
                }
                current = parent;
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

    fn check_borrow_creations(
        &self,
        inst: &MirInst<'db>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<String> {
        self.check_borrow_creations_in_values(
            &borrow_values_in_inst(&self.func.body, inst),
            active,
            suspended,
        )
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
            let kind = self.loans[loan.0 as usize].kind;
            let parent = self.loans[loan.0 as usize].parent;
            for &other in active {
                if other == loan || Some(other) == parent || suspended.contains(&other) {
                    continue;
                }
                if loans_overlap(&self.loans[loan.0 as usize], &self.loans[other.0 as usize])
                    && match kind {
                        BorrowKind::Mut => true,
                        BorrowKind::Ref => {
                            matches!(self.loans[other.0 as usize].kind, BorrowKind::Mut)
                        }
                    }
                {
                    return Some("borrow conflict: cannot create overlapping loan".to_string());
                }
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
