//! MIR borrow checking for `mut` / `ref` borrow handles.
//!
//! Borrow handles are pointer-like values (`mut T` / `ref T`) that carry NoEsc (stack-only)
//! restrictions. The handles themselves are copyable; soundness comes from enforcing aliasing
//! rules over the *places* they can point to.

use common::diagnostics::{
    CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
};
use hir::analysis::ty::ty_def::BorrowKind;
use hir::analysis::{
    HirAnalysisDb,
    ty::{ty_is_borrow, ty_is_noesc},
};
use hir::hir_def::{
    Body, ExprId, FuncParamMode, Partial,
    expr::{Expr, UnOp},
};
use hir::projection::Aliasing;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{
    AddressSpaceKind, BasicBlockId, CallOrigin, LocalId, MirBody, MirFunction, MirInst, Place,
    Rvalue, SourceInfoId, Terminator, ValueId, ValueOrigin, ValueRepr,
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
    // Diagnostics provenance for the expression that created this loan.
    origin_source: SourceInfoId,
    // Best-effort HIR ExprId for span selection (prefer underlining the borrowed place).
    origin_expr: Option<ExprId>,
}

#[derive(Debug, Clone, Copy)]
struct MoveOrigin {
    source: SourceInfoId,
    expr: Option<ExprId>,
}

#[derive(Debug, Clone)]
enum MoveConflictNote {
    Moved {
        moved: MoveOrigin,
        moved_name: Option<String>,
    },
    Loan(LoanId),
}

#[derive(Debug, Clone)]
struct MoveConflict {
    label: String,
    note: Option<MoveConflictNote>,
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
    pub diagnostic: CompleteDiagnostic,
}

pub fn compute_borrow_summaries<'db>(
    db: &'db dyn HirAnalysisDb,
    functions: &[MirFunction<'db>],
) -> Result<BorrowSummaryMap<'db>, Box<BorrowSummaryError>> {
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
                .map_err(|err| {
                    Box::new(BorrowSummaryError {
                        func_name,
                        diagnostic: err,
                    })
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
) -> Option<CompleteDiagnostic> {
    Borrowck::new(db, func, summaries).check()
}

struct Borrowck<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    func: &'a MirFunction<'db>,
    summaries: &'a BorrowSummaryMap<'db>,
    // Diagnostics-only: used to improve spans/messages in borrowck errors.
    hir_body: Option<Body<'db>>,
    // Inverse map of `MirBody::expr_values`, precomputed for diagnostics so we don't scan.
    value_to_expr: Vec<Option<ExprId>>,
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
    moved_entry: Vec<FxHashMap<CanonPlace<'db>, MoveOrigin>>,
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
        let hir_body = func.typed_body.as_ref().and_then(|typed| typed.body());
        // Pick one "best" HIR ExprId per MIR ValueId for diagnostics (prefer `mut/ref/move` ops).
        let mut value_to_expr = vec![None; body.values.len()];
        let unop_rank = |expr: ExprId| {
            let Some(body) = hir_body else {
                return 0;
            };
            match expr.data(db, body) {
                Partial::Present(Expr::Un(_, UnOp::Mut | UnOp::Ref)) => 2,
                Partial::Present(Expr::Un(_, UnOp::Move)) => 1,
                _ => 0,
            }
        };
        for (&expr, &value) in &body.expr_values {
            let slot = &mut value_to_expr[value.index()];
            let rank = unop_rank(expr);
            let replace = match slot {
                None => true,
                Some(existing) => {
                    rank > unop_rank(*existing)
                        || (rank == unop_rank(*existing) && expr < *existing)
                }
            };
            if replace {
                *slot = Some(expr);
            }
        }
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
            hir_body,
            value_to_expr,
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

    fn borrow_summary(mut self) -> Result<Option<BorrowSummary<'db>>, CompleteDiagnostic> {
        if ty_is_borrow(self.db, self.func.ret_ty).is_none() {
            return Ok(None);
        }
        self.analyze();
        self.compute_return_summary().map(Some)
    }

    fn check(mut self) -> Option<CompleteDiagnostic> {
        self.analyze();
        self.compute_moved_entry_states();
        self.compute_liveness();
        self.check_conflicts()
    }

    fn span_for_source(&self, source: SourceInfoId) -> common::diagnostics::Span {
        self.func
            .body
            .source_span(source)
            .or_else(|| {
                self.func
                    .body
                    .source_infos
                    .iter()
                    .find_map(|info| info.span.clone())
            })
            .expect("borrowck diagnostic missing a span")
    }

    fn diag_at_source(
        &self,
        local_code: u16,
        source: SourceInfoId,
        header: String,
        label: String,
    ) -> CompleteDiagnostic {
        let span = self.span_for_source(source);
        CompleteDiagnostic::new(
            Severity::Error,
            header,
            vec![SubDiagnostic::new(LabelStyle::Primary, label, Some(span))],
            Vec::new(),
            GlobalErrorCode::new(DiagnosticPass::Mir, local_code),
        )
    }

    fn diag_at_inst(
        &self,
        local_code: u16,
        inst: &MirInst<'db>,
        header: String,
        label: String,
    ) -> CompleteDiagnostic {
        self.diag_at_source(local_code, inst_source(inst), header, label)
    }

    fn diag_at_inst_with_loan(
        &self,
        local_code: u16,
        inst: &MirInst<'db>,
        header: String,
        label: String,
        loan: LoanId,
    ) -> CompleteDiagnostic {
        let mut diag = self.diag_at_inst(local_code, inst, header, label);
        self.push_loan_origin_label(&mut diag, loan);
        diag
    }

    fn diag_at_value(
        &self,
        local_code: u16,
        value: ValueId,
        header: String,
        label: String,
    ) -> CompleteDiagnostic {
        let source = self.func.body.value(value).source;
        self.diag_at_source(local_code, source, header, label)
    }

    fn diag_at_value_with_loan(
        &self,
        local_code: u16,
        value: ValueId,
        header: String,
        label: String,
        loan: LoanId,
    ) -> CompleteDiagnostic {
        let mut diag = self.diag_at_value(local_code, value, header, label);
        self.push_loan_origin_label(&mut diag, loan);
        diag
    }

    fn diag_at_terminator(
        &self,
        local_code: u16,
        term: &Terminator<'db>,
        header: String,
        label: String,
    ) -> CompleteDiagnostic {
        self.diag_at_source(local_code, terminator_source(term), header, label)
    }

    // Diagnostics convention: keep the primary label short, and put high-level context (category,
    // function) in the header.
    fn borrow_conflict_header(&self) -> String {
        format!("borrow conflict in `fn {}`", self.func.symbol_name)
    }

    fn move_conflict_header(&self) -> String {
        format!("move conflict in `fn {}`", self.func.symbol_name)
    }

    fn invalid_return_borrow_header(&self) -> String {
        format!("invalid return borrow in `fn {}`", self.func.symbol_name)
    }

    fn push_loan_origin_label(&self, diag: &mut CompleteDiagnostic, loan: LoanId) {
        if let Some(sub) = self.loan_origin_subdiag(loan) {
            diag.sub_diagnostics.push(sub);
        }
    }

    fn push_move_origin_label(
        &self,
        diag: &mut CompleteDiagnostic,
        moved: MoveOrigin,
        moved_name: Option<String>,
    ) {
        let span = moved
            .expr
            .and_then(|expr| self.moved_place_span(expr))
            .or_else(|| self.func.body.source_span(moved.source));
        let Some(span) = span else {
            return;
        };
        let msg = moved_name
            .map(|name| format!("`{name}` is moved here"))
            .unwrap_or_else(|| "value is moved here".to_string());
        diag.sub_diagnostics
            .push(SubDiagnostic::new(LabelStyle::Secondary, msg, Some(span)));
    }

    fn moved_overlap_origin(
        &self,
        accessed: &FxHashSet<CanonPlace<'db>>,
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
    ) -> Option<(MoveOrigin, Option<String>)> {
        moved
            .iter()
            .filter(|(moved_place, _)| accessed.iter().any(|p| places_overlap(p, moved_place)))
            .min_by_key(|(_, origin)| origin.source.0)
            .map(|(place, origin)| (*origin, self.canon_place_simple_name(place)))
    }

    fn loan_origin_subdiag(&self, loan: LoanId) -> Option<SubDiagnostic> {
        let span = self.loan_origin_span(loan)?;
        let msg = self
            .loan_simple_name(loan)
            .map(|name| format!("`{name}` is borrowed here"))
            .unwrap_or_else(|| "borrow created here".to_string());
        Some(SubDiagnostic::new(LabelStyle::Secondary, msg, Some(span)))
    }

    fn loan_origin_span(&self, loan: LoanId) -> Option<common::diagnostics::Span> {
        let loan = &self.loans[loan.0 as usize];
        if let Some(expr) = loan.origin_expr
            && let Some(span) = self.borrowed_place_span(expr)
        {
            return Some(span);
        }
        self.func.body.source_span(loan.origin_source)
    }

    fn borrowed_place_span(&self, expr: ExprId) -> Option<common::diagnostics::Span> {
        let body = self.hir_body?;
        match expr.data(self.db, body) {
            Partial::Present(Expr::Un(inner, UnOp::Mut | UnOp::Ref)) => {
                let &value = self.func.body.expr_values.get(inner)?;
                self.func
                    .body
                    .source_span(self.func.body.value(value).source)
            }
            _ => None,
        }
    }

    fn moved_place_span(&self, expr: ExprId) -> Option<common::diagnostics::Span> {
        let body = self.hir_body?;
        let target = match expr.data(self.db, body) {
            Partial::Present(Expr::Un(inner, UnOp::Move)) => *inner,
            _ => expr,
        };
        let &value = self.func.body.expr_values.get(&target)?;
        self.func
            .body
            .source_span(self.func.body.value(value).source)
    }

    fn loan_simple_name(&self, loan: LoanId) -> Option<String> {
        let targets = &self.loans[loan.0 as usize].targets;
        if targets.len() != 1 {
            return None;
        }
        let place = targets.iter().next()?;
        self.canon_place_simple_name(place)
    }

    fn canon_place_simple_name(&self, place: &CanonPlace<'db>) -> Option<String> {
        if place.proj.iter().next().is_some() {
            return None;
        }
        match place.root {
            Root::Param(param_index) => {
                let local = self
                    .func
                    .body
                    .param_locals
                    .get(param_index as usize)
                    .copied()?;
                Some(self.func.body.local(local).name.clone())
            }
            Root::Local(local) => Some(self.func.body.local(local).name.clone()),
        }
    }

    fn conflict_subject_name(&self, a: LoanId, b: LoanId) -> Option<String> {
        let a_name = self.loan_simple_name(a);
        let b_name = self.loan_simple_name(b);
        match (a_name, b_name) {
            (Some(a), Some(b)) => (a == b).then_some(a),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        }
    }

    fn overlapping_loans_msg(
        &self,
        a: LoanId,
        b: LoanId,
        created_a: bool,
        created_b: bool,
    ) -> String {
        let subject = self
            .conflict_subject_name(a, b)
            .map(|name| format!("`{name}`"))
            .unwrap_or_else(|| "this place".to_string());
        let kind_a = self.loans[a.0 as usize].kind;
        let kind_b = self.loans[b.0 as usize].kind;

        let creation_conflict = |new, active| match (new, active) {
            (BorrowKind::Mut, BorrowKind::Mut) => {
                format!("cannot mutably borrow {subject} while a mut borrow is active")
            }
            (BorrowKind::Mut, BorrowKind::Ref) => {
                format!("cannot mutably borrow {subject} while an immutable borrow is active")
            }
            (BorrowKind::Ref, BorrowKind::Mut) => {
                format!("cannot immutably borrow {subject} while a mutable borrow is active")
            }
            (BorrowKind::Ref, BorrowKind::Ref) => {
                panic!("two immutable borrows should not conflict")
            }
        };

        match (created_a, created_b) {
            (true, false) => creation_conflict(kind_a, kind_b),
            (false, true) => creation_conflict(kind_b, kind_a),
            _ => match (kind_a, kind_b) {
                (BorrowKind::Mut, BorrowKind::Mut) => {
                    format!("multiple mutable borrows of {subject} are active at the same time")
                }
                (BorrowKind::Mut, BorrowKind::Ref) | (BorrowKind::Ref, BorrowKind::Mut) => format!(
                    "a mutable and immutable borrow of {subject} are active at the same time"
                ),
                (BorrowKind::Ref, BorrowKind::Ref) => {
                    panic!("two immutable borrows should not conflict")
                }
            },
        }
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
                origin_source: self.func.body.local(local).source,
                origin_expr: None,
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
                origin_source: self.func.body.value(value_id).source,
                origin_expr: self.value_to_expr.get(value_id.index()).copied().flatten(),
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
                    origin_source: self.func.body.value(call_value).source,
                    origin_expr: Some(expr),
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

    fn compute_return_summary(&self) -> Result<BorrowSummary<'db>, CompleteDiagnostic> {
        let body = &self.func.body;
        let (ret_kind, _) = ty_is_borrow(self.db, self.func.ret_ty)
            .unwrap_or_else(|| panic!("borrow summary requires a borrow return type"));
        let borrow_kw = match ret_kind {
            BorrowKind::Ref => "ref",
            BorrowKind::Mut => "mut",
        };

        let mut out = FxHashSet::default();
        for (bb_idx, block) in body.blocks.iter().enumerate() {
            let mut state = self.entry_states[bb_idx].clone();
            for inst in &block.insts {
                self.update_state_for_inst(inst, &mut state);
            }
            let Terminator::Return {
                value: Some(value), ..
            } = &block.terminator
            else {
                continue;
            };
            for place in self.canonicalize_base(&state, *value) {
                let param_index = match place.root {
                    Root::Param(param_index) => param_index,
                    Root::Local(local) => {
                        let local_name = self.func.body.local(local).name.clone();
                        let local_span = self.span_for_source(self.func.body.local(local).source);
                        let mut diag = self.diag_at_value(
                            3,
                            *value,
                            self.invalid_return_borrow_header(),
                            format!("cannot return a borrow to local `{local_name}`"),
                        );
                        diag.sub_diagnostics.push(SubDiagnostic::new(
                            LabelStyle::Secondary,
                            format!("`{local_name}` is a local value created here"),
                            Some(local_span),
                        ));
                        return Err(diag);
                    }
                };

                if !self.borrow_param[param_index as usize] {
                    let param_local = *self
                        .func
                        .body
                        .param_locals
                        .get(param_index as usize)
                        .unwrap_or_else(|| panic!("missing param local"));
                    let param = self.func.body.local(param_local);
                    let mode = self.param_modes[param_index as usize];
                    let mode_str = match mode {
                        FuncParamMode::View => "view",
                        FuncParamMode::Move => "move",
                    };
                    let param_span = self.span_for_source(param.source);

                    let mut diag = self.diag_at_value(
                        3,
                        *value,
                        self.invalid_return_borrow_header(),
                        "return borrows must be derived from explicit borrow parameters"
                            .to_string(),
                    );
                    diag.sub_diagnostics.push(SubDiagnostic::new(
                        LabelStyle::Secondary,
                        format!("`{}` is a `{mode_str}` parameter", param.name),
                        Some(param_span),
                    ));
                    diag.notes.push(format!(
                        "help: consider changing `{}: {}` to `{}: {borrow_kw} {}`",
                        param.name,
                        param.ty.pretty_print(self.db),
                        param.name,
                        param.ty.pretty_print(self.db)
                    ));
                    return Err(diag);
                }

                for proj in place.proj.iter() {
                    if let hir::projection::Projection::Index(
                        hir::projection::IndexSource::Dynamic(_),
                    ) = proj
                    {
                        return Err(self.diag_at_value(
                            3,
                            *value,
                            self.invalid_return_borrow_header(),
                            "return borrows with dynamic indices are not supported".to_string(),
                        ));
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
        self.moved_entry = vec![FxHashMap::default(); body.blocks.len()];

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
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
    ) -> bool {
        let entry = &mut self.moved_entry[succ.index()];
        let before = entry.len();
        for (place, origin) in moved {
            entry
                .entry(place.clone())
                .and_modify(|existing| {
                    if origin.source.0 < existing.source.0 {
                        *existing = *origin;
                    }
                })
                .or_insert(*origin);
        }
        entry.len() != before
    }

    fn update_moved_for_inst(
        &self,
        inst: &MirInst<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &mut FxHashMap<CanonPlace<'db>, MoveOrigin>,
    ) {
        let source = inst_source(inst);
        let fallback_expr = match inst {
            MirInst::Assign {
                rvalue: Rvalue::Call(call),
                ..
            } => call.expr,
            _ => None,
        };
        for (place, move_value) in move_places_in_inst(&self.func.body, inst) {
            if !self.loans_for_place_base(state, place.base).is_empty() {
                continue;
            }
            let origin = MoveOrigin {
                source,
                expr: self
                    .value_to_expr
                    .get(move_value.index())
                    .copied()
                    .flatten()
                    .or(fallback_expr),
            };
            for canon_place in self.canonicalize_place(state, &place) {
                moved
                    .entry(canon_place)
                    .and_modify(|existing| {
                        if origin.source.0 < existing.source.0 {
                            *existing = origin;
                        }
                    })
                    .or_insert(origin);
            }
        }

        match inst {
            MirInst::Assign {
                dest: Some(dest), ..
            } => {
                let root = self.root_for_local(*dest);
                moved.retain(|p, _| p.root != root);
            }
            MirInst::Store { place, .. } | MirInst::InitAggregate { place, .. } => {
                let written = self.canonicalize_place(state, place);
                moved.retain(|m, _| {
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
        moved: &mut FxHashMap<CanonPlace<'db>, MoveOrigin>,
    ) {
        let source = terminator_source(term);
        let fallback_expr = match term {
            Terminator::TerminatingCall { call, .. } => match call {
                crate::TerminatingCall::Call(call) => call.expr,
                crate::TerminatingCall::Intrinsic { .. } => None,
            },
            _ => None,
        };
        for (place, move_value) in move_places_in_terminator(&self.func.body, term) {
            if !self.loans_for_place_base(state, place.base).is_empty() {
                continue;
            }
            let origin = MoveOrigin {
                source,
                expr: self
                    .value_to_expr
                    .get(move_value.index())
                    .copied()
                    .flatten()
                    .or(fallback_expr),
            };
            for canon_place in self.canonicalize_place(state, &place) {
                moved
                    .entry(canon_place)
                    .and_modify(|existing| {
                        if origin.source.0 < existing.source.0 {
                            *existing = origin;
                        }
                    })
                    .or_insert(origin);
            }
        }
    }

    fn check_moved_and_moves_in_inst(
        &self,
        inst: &MirInst<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<CompleteDiagnostic> {
        let mut no_note_conflicts = Vec::new();
        for (place, move_value) in move_places_in_inst(&self.func.body, inst) {
            if let Some(MoveConflict { label, note }) =
                self.check_move_out_place(&place, state, moved, active, suspended)
            {
                match note {
                    Some(MoveConflictNote::Moved { moved, moved_name }) => {
                        let mut diag =
                            self.diag_at_inst(2, inst, self.move_conflict_header(), label);
                        self.push_move_origin_label(&mut diag, moved, moved_name);
                        return Some(diag);
                    }
                    Some(MoveConflictNote::Loan(loan)) => {
                        let mut diag =
                            self.diag_at_inst(2, inst, self.move_conflict_header(), label);
                        self.push_loan_origin_label(&mut diag, loan);
                        return Some(diag);
                    }
                    None => {
                        no_note_conflicts.push((move_value, label));
                    }
                }
            }
        }
        if let Some((first_value, first_label)) = no_note_conflicts.first() {
            let mut diag = self.diag_at_value(
                2,
                *first_value,
                self.move_conflict_header(),
                first_label.clone(),
            );
            for (value, label) in no_note_conflicts.into_iter().skip(1) {
                let span = self.span_for_source(self.func.body.value(value).source);
                diag.sub_diagnostics.push(SubDiagnostic::new(
                    LabelStyle::Primary,
                    label,
                    Some(span),
                ));
            }
            return Some(diag);
        }

        for value in borrow_values_in_inst(&self.func.body, inst) {
            if ty_is_borrow(self.db, self.func.body.value(value).ty).is_none() {
                continue;
            }
            let ValueOrigin::PlaceRef(place) = &self.func.body.value(value).origin else {
                continue;
            };
            let accessed = self.canonicalize_place(state, place);
            if let Some((moved, moved_name)) = self.moved_overlap_origin(&accessed, moved) {
                let mut diag = self.diag_at_value(
                    2,
                    value,
                    self.move_conflict_header(),
                    "cannot borrow a moved value".to_string(),
                );
                self.push_move_origin_label(&mut diag, moved, moved_name);
                return Some(diag);
            }
        }

        match inst {
            MirInst::Assign {
                rvalue: Rvalue::Load { place },
                ..
            } => {
                let accessed = self.canonicalize_place(state, place);
                if let Some((moved, moved_name)) = self.moved_overlap_origin(&accessed, moved) {
                    let mut diag = self.diag_at_inst(
                        2,
                        inst,
                        self.move_conflict_header(),
                        "cannot use a value after it was moved".to_string(),
                    );
                    self.push_move_origin_label(&mut diag, moved, moved_name);
                    return Some(diag);
                }
            }
            MirInst::Store { place, .. } | MirInst::InitAggregate { place, .. } => {
                let written = self.canonicalize_place(state, place);
                if let Some((moved, moved_name)) =
                    self.check_write_through_moved_parent(&written, moved)
                {
                    let mut diag = self.diag_at_inst(
                        2,
                        inst,
                        self.move_conflict_header(),
                        "cannot write through a moved value".to_string(),
                    );
                    self.push_move_origin_label(&mut diag, moved, moved_name);
                    return Some(diag);
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
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<CompleteDiagnostic> {
        for (place, _) in move_places_in_terminator(&self.func.body, term) {
            if let Some(MoveConflict { label, note }) =
                self.check_move_out_place(&place, state, moved, active, suspended)
            {
                let mut diag = self.diag_at_terminator(2, term, self.move_conflict_header(), label);
                match note {
                    Some(MoveConflictNote::Moved { moved, moved_name }) => {
                        self.push_move_origin_label(&mut diag, moved, moved_name)
                    }
                    Some(MoveConflictNote::Loan(loan)) => {
                        self.push_loan_origin_label(&mut diag, loan)
                    }
                    None => {}
                }
                return Some(diag);
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
            if let Some((moved, moved_name)) = self.moved_overlap_origin(&accessed, moved) {
                let mut diag = self.diag_at_value(
                    2,
                    value,
                    self.move_conflict_header(),
                    "cannot borrow a moved value".to_string(),
                );
                self.push_move_origin_label(&mut diag, moved, moved_name);
                return Some(diag);
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
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
    ) -> Option<CompleteDiagnostic> {
        let mut locals = FxHashSet::default();
        collect_value_locals_in_value(&self.func.body, value, &mut locals);
        if locals.is_empty() {
            return None;
        }
        let accessed: FxHashSet<_> = locals
            .into_iter()
            .map(|local| CanonPlace {
                root: self.root_for_local(local),
                proj: crate::MirProjectionPath::new(),
            })
            .collect();
        if let Some((moved, moved_name)) = self.moved_overlap_origin(&accessed, moved) {
            let mut diag = self.diag_at_value(
                2,
                value,
                self.move_conflict_header(),
                "cannot use a value after it was moved".to_string(),
            );
            self.push_move_origin_label(&mut diag, moved, moved_name);
            return Some(diag);
        }
        None
    }

    fn check_place_path_indices_after_move(
        &self,
        path: &crate::MirProjectionPath<'db>,
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
    ) -> Option<CompleteDiagnostic> {
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
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
    ) -> Option<(MoveOrigin, Option<String>)> {
        moved
            .iter()
            .filter(|(m, _)| {
                written
                    .iter()
                    .any(|w| w.root == m.root && m.proj.is_prefix_of(&w.proj) && m.proj != w.proj)
            })
            .min_by_key(|(_, origin)| origin.source.0)
            .map(|(place, origin)| (*origin, self.canon_place_simple_name(place)))
    }

    fn check_move_out_place(
        &self,
        place: &Place<'db>,
        state: &[FxHashSet<LoanId>],
        moved: &FxHashMap<CanonPlace<'db>, MoveOrigin>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<MoveConflict> {
        let handle_loans = self.loans_for_place_base(state, place.base);
        if let Some(loan) = handle_loans.iter().copied().min_by_key(|loan| loan.0) {
            return Some(MoveConflict {
                label: "cannot move out through a borrow handle".to_string(),
                note: Some(MoveConflictNote::Loan(loan)),
            });
        }

        let targets = self.canonicalize_place(state, place);
        if let Some((moved, moved_name)) = self.moved_overlap_origin(&targets, moved) {
            return Some(MoveConflict {
                label: "cannot use a value after it was moved".to_string(),
                note: Some(MoveConflictNote::Moved { moved, moved_name }),
            });
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
                return Some(MoveConflict {
                    label: "cannot move out of a view parameter".to_string(),
                    note: None,
                });
            }
        }

        let mut effective: Vec<_> = active
            .iter()
            .copied()
            .filter(|loan| !suspended.contains(loan))
            .collect();
        effective.sort_by_key(|loan| loan.0);
        for loan in effective {
            if place_set_overlaps(&self.loans[loan.0 as usize].targets, &targets) {
                return Some(MoveConflict {
                    label: "cannot move out of a value while it is borrowed".to_string(),
                    note: Some(MoveConflictNote::Loan(loan)),
                });
            }
        }

        None
    }

    fn check_conflicts(self) -> Option<CompleteDiagnostic> {
        let body = &self.func.body;
        for (bb_idx, block) in body.blocks.iter().enumerate() {
            let mut state = self.entry_states[bb_idx].clone();
            let mut moved = self.moved_entry[bb_idx].clone();
            for (inst_idx, inst) in block.insts.iter().enumerate() {
                let mut active = self.active_loans(&state, &self.live_before[bb_idx][inst_idx]);
                let temp = self.borrow_loans_in_inst(inst);
                active.extend(temp.iter().copied());
                let mut call_loan = None;
                if let Some((_, loan)) = self.call_loan_in_inst(inst) {
                    active.insert(loan);
                    call_loan = Some(loan);
                }
                let suspended = self.suspended_loans(&active);
                let mut effective: Vec<_> = active
                    .iter()
                    .copied()
                    .filter(|l| !suspended.contains(l))
                    .collect();
                effective.sort_by_key(|loan| loan.0);

                if let Some(err) =
                    self.check_moved_and_moves_in_inst(inst, &state, &moved, &active, &suspended)
                {
                    return Some(err);
                }
                if let Some((a, b)) = self.active_set_conflict(&effective) {
                    let created_a = temp.contains(&a) || call_loan == Some(a);
                    let created_b = temp.contains(&b) || call_loan == Some(b);
                    let culprit = match (created_a, created_b) {
                        (true, false) => b,
                        (false, true) => a,
                        _ => a,
                    };
                    return Some(self.diag_at_inst_with_loan(
                        2,
                        inst,
                        self.borrow_conflict_header(),
                        self.overlapping_loans_msg(a, b, created_a, created_b),
                        culprit,
                    ));
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
            let mut effective: Vec<_> = active
                .iter()
                .copied()
                .filter(|l| !suspended.contains(l))
                .collect();
            effective.sort_by_key(|loan| loan.0);
            if let Some(err) = self.check_moved_and_moves_in_terminator(
                &block.terminator,
                &state,
                &moved,
                &active,
                &suspended,
            ) {
                return Some(err);
            }
            if let Some((a, b)) = self.active_set_conflict(&effective) {
                let mut diag = self.diag_at_terminator(
                    2,
                    &block.terminator,
                    self.borrow_conflict_header(),
                    self.overlapping_loans_msg(a, b, false, false),
                );
                self.push_loan_origin_label(&mut diag, a);
                return Some(diag);
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
            MirInst::Store { place, value, .. } => {
                if let Some(root) = root_memory_local(body, place)
                    && let Some(idx) = self.tracked_local_idx.get(root.index()).copied().flatten()
                {
                    state[idx].extend(self.loans_in_value(state, *value));
                }
            }
            MirInst::InitAggregate { place, inits, .. } => {
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
            // Generic templates may contain unresolved call targets; skip summary application in
            // that case so we can still report local borrow/move conflicts.
            return;
        };
        let Some(summary) = self.summaries.get(callee) else {
            // If we don't have a summary (e.g. callee not in this analysis set), treat it as not
            // contributing any borrow transforms.
            return;
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
        while let ValueOrigin::TransparentCast { value: inner } =
            &self.func.body.value(value).origin
        {
            value = *inner;
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

    fn active_set_conflict(&self, active: &[LoanId]) -> Option<(LoanId, LoanId)> {
        for (idx, &a) in active.iter().enumerate() {
            for &b in &active[idx + 1..] {
                if self.loans_conflict(a, b) {
                    return Some((a, b));
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

    fn call_loan_in_inst(&self, inst: &MirInst<'db>) -> Option<(ValueId, LoanId)> {
        let MirInst::Assign {
            rvalue: Rvalue::Call(call),
            ..
        } = inst
        else {
            return None;
        };
        let expr = call.expr?;
        let call_value = *self.func.body.expr_values.get(&expr)?;
        let loan = self.call_loan_for_value.get(&call_value).copied()?;
        Some((call_value, loan))
    }

    fn check_borrow_creations(
        &self,
        inst: &MirInst<'db>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<CompleteDiagnostic> {
        if let Some((call_value, loan)) = self.call_loan_in_inst(inst)
            && let Some(other) = self.loan_creation_conflicts(loan, active, suspended)
        {
            return Some(self.diag_at_value_with_loan(
                2,
                call_value,
                self.borrow_conflict_header(),
                self.overlapping_loans_msg(loan, other, true, false),
                other,
            ));
        }
        self.check_borrow_creations_in_values(
            &borrow_values_in_inst(&self.func.body, inst),
            active,
            suspended,
        )
    }

    fn loan_creation_conflicts(
        &self,
        loan: LoanId,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<LoanId> {
        if suspended.contains(&loan) {
            return None;
        }
        let kind = self.loans[loan.0 as usize].kind;
        let parents = &self.loans[loan.0 as usize].parents;
        let mut effective: Vec<_> = active
            .iter()
            .copied()
            .filter(|other| !suspended.contains(other))
            .collect();
        effective.sort_by_key(|loan| loan.0);
        for other in effective {
            if other == loan || parents.contains(&other) || suspended.contains(&other) {
                continue;
            }
            if loans_overlap(&self.loans[loan.0 as usize], &self.loans[other.0 as usize])
                && match kind {
                    BorrowKind::Mut => true,
                    BorrowKind::Ref => matches!(self.loans[other.0 as usize].kind, BorrowKind::Mut),
                }
            {
                return Some(other);
            }
        }
        None
    }

    fn check_borrow_creations_in_values(
        &self,
        values: &FxHashSet<ValueId>,
        active: &FxHashSet<LoanId>,
        suspended: &FxHashSet<LoanId>,
    ) -> Option<CompleteDiagnostic> {
        for value in values {
            let Some(&loan) = self.loan_for_value.get(value) else {
                continue;
            };
            if let Some(other) = self.loan_creation_conflicts(loan, active, suspended) {
                return Some(self.diag_at_value_with_loan(
                    2,
                    *value,
                    self.borrow_conflict_header(),
                    self.overlapping_loans_msg(loan, other, true, false),
                    other,
                ));
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
    ) -> Option<CompleteDiagnostic> {
        if let Some((loan, err)) = self.check_word_value_reads(inst, active, suspended) {
            return Some(self.diag_at_inst_with_loan(
                2,
                inst,
                self.borrow_conflict_header(),
                err,
                loan,
            ));
        }
        match inst {
            MirInst::Assign {
                dest: Some(dest),
                rvalue: Rvalue::Load { place },
                ..
            } => {
                // `check_word_value_reads` only tracks word locals inside value operands, so we
                // must explicitly validate reads performed via `Load { place }`.
                if let Some((loan, err)) =
                    self.check_place_access(state, place, AccessKind::Read, active, suspended)
                {
                    return Some(self.diag_at_inst_with_loan(
                        2,
                        inst,
                        self.borrow_conflict_header(),
                        err,
                        loan,
                    ));
                }
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
                .map(|(loan, err)| {
                    self.diag_at_inst_with_loan(2, inst, self.borrow_conflict_header(), err, loan)
                })
            }
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
                .map(|(loan, err)| {
                    self.diag_at_inst_with_loan(2, inst, self.borrow_conflict_header(), err, loan)
                })
            }
            MirInst::Assign {
                rvalue: Rvalue::Load { place },
                ..
            } => self
                .check_place_access(state, place, AccessKind::Read, active, suspended)
                .map(|(loan, err)| {
                    self.diag_at_inst_with_loan(2, inst, self.borrow_conflict_header(), err, loan)
                }),
            MirInst::Store { place, .. } => self
                .check_place_access(state, place, AccessKind::Write, active, suspended)
                .map(|(loan, err)| {
                    self.diag_at_inst_with_loan(2, inst, self.borrow_conflict_header(), err, loan)
                }),
            MirInst::InitAggregate { place, .. } => self
                .check_place_access(state, place, AccessKind::Write, active, suspended)
                .map(|(loan, err)| {
                    self.diag_at_inst_with_loan(2, inst, self.borrow_conflict_header(), err, loan)
                }),
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
    ) -> Option<(LoanId, String)> {
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
    ) -> Option<(LoanId, String)> {
        if let Some(loan) = through
            .iter()
            .copied()
            .filter(|loan| suspended.contains(loan))
            .min_by_key(|loan| loan.0)
        {
            return Some((loan, "cannot use reborrowed `mut` handle".to_string()));
        }

        let mut effective: Vec<_> = active
            .iter()
            .copied()
            .filter(|l| !suspended.contains(l))
            .collect();
        effective.sort_by_key(|loan| loan.0);
        for loan in effective {
            let loan_data = &self.loans[loan.0 as usize];
            if !place_set_overlaps(&loan_data.targets, accessed) {
                continue;
            }
            match loan_data.kind {
                BorrowKind::Ref => {
                    if matches!(access, AccessKind::Write) {
                        return Some((
                            loan,
                            "cannot write through an active `ref` borrow".to_string(),
                        ));
                    }
                }
                BorrowKind::Mut => {
                    if !through.contains(&loan) {
                        return Some((loan, "access overlaps an active `mut` borrow".to_string()));
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
        while let ValueOrigin::TransparentCast { value } = &self.func.body.value(base).origin {
            base = *value;
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
    ) -> Option<(LoanId, String)> {
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

fn inst_source(inst: &MirInst<'_>) -> SourceInfoId {
    match inst {
        MirInst::Assign { source, .. }
        | MirInst::BindValue { source, .. }
        | MirInst::Store { source, .. }
        | MirInst::InitAggregate { source, .. }
        | MirInst::SetDiscriminant { source, .. } => *source,
    }
}

fn terminator_source(term: &Terminator<'_>) -> SourceInfoId {
    match term {
        Terminator::Return { source, .. }
        | Terminator::TerminatingCall { source, .. }
        | Terminator::Goto { source, .. }
        | Terminator::Branch { source, .. }
        | Terminator::Switch { source, .. }
        | Terminator::Unreachable { source, .. } => *source,
    }
}

fn successors(term: &Terminator<'_>) -> Vec<BasicBlockId> {
    match term {
        Terminator::Goto { target, .. } => vec![*target],
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
        Terminator::Return { .. }
        | Terminator::TerminatingCall { .. }
        | Terminator::Unreachable { .. } => Vec::new(),
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
        MirInst::Store { place, value, .. } => {
            collect_locals_in_value(body, place.base, &mut out);
            collect_locals_in_place_path(body, &place.projection, &mut out);
            collect_locals_in_value(body, *value, &mut out);
        }
        MirInst::InitAggregate { place, inits, .. } => {
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
        MirInst::BindValue { value, .. } => collect_locals_in_value(body, *value, &mut out),
    }
    out
}

fn locals_used_by_terminator<'db>(
    body: &MirBody<'db>,
    term: &Terminator<'db>,
) -> FxHashSet<LocalId> {
    let mut out = FxHashSet::default();
    match term {
        Terminator::Return {
            value: Some(value), ..
        } => collect_locals_in_value(body, *value, &mut out),
        Terminator::TerminatingCall { call, .. } => match call {
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
        Terminator::Return { value: None, .. }
        | Terminator::Goto { .. }
        | Terminator::Unreachable { .. } => {}
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
        MirInst::BindValue { value, .. } => vec![*value],
        MirInst::SetDiscriminant { .. } => Vec::new(),
    }
}

fn value_operands_in_terminator(term: &Terminator<'_>) -> Vec<ValueId> {
    match term {
        Terminator::Return {
            value: Some(value), ..
        } => vec![*value],
        Terminator::TerminatingCall { call, .. } => match call {
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
        Terminator::Return { value: None, .. }
        | Terminator::Goto { .. }
        | Terminator::Unreachable { .. } => Vec::new(),
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
        MirInst::Store { place, value, .. } => {
            collect_borrow_values(body, place.base, &mut out);
            collect_borrow_values_in_place_path(body, &place.projection, &mut out);
            collect_borrow_values(body, *value, &mut out);
        }
        MirInst::InitAggregate { place, inits, .. } => {
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
        MirInst::BindValue { value, .. } => collect_borrow_values(body, *value, &mut out),
    }
    out
}

fn borrow_values_in_terminator<'db>(
    body: &MirBody<'db>,
    term: &Terminator<'db>,
) -> FxHashSet<ValueId> {
    let mut out = FxHashSet::default();
    match term {
        Terminator::Return {
            value: Some(value), ..
        } => collect_borrow_values(body, *value, &mut out),
        Terminator::TerminatingCall { call, .. } => match call {
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
        Terminator::Return { value: None, .. }
        | Terminator::Goto { .. }
        | Terminator::Unreachable { .. } => {}
    }
    out
}

fn move_places_in_inst<'db>(
    body: &MirBody<'db>,
    inst: &MirInst<'db>,
) -> Vec<(Place<'db>, ValueId)> {
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
        MirInst::Store { place, value, .. } => {
            collect_move_places(body, place.base, &mut out);
            collect_move_places_in_place_path(body, &place.projection, &mut out);
            collect_move_places(body, *value, &mut out);
        }
        MirInst::InitAggregate { place, inits, .. } => {
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
        MirInst::BindValue { value, .. } => collect_move_places(body, *value, &mut out),
    }
    out
}

fn move_places_in_terminator<'db>(
    body: &MirBody<'db>,
    term: &Terminator<'db>,
) -> Vec<(Place<'db>, ValueId)> {
    let mut out = Vec::new();
    match term {
        Terminator::Return {
            value: Some(value), ..
        } => collect_move_places(body, *value, &mut out),
        Terminator::TerminatingCall { call, .. } => match call {
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
        Terminator::Return { value: None, .. }
        | Terminator::Goto { .. }
        | Terminator::Unreachable { .. } => {}
    }
    out
}

fn collect_move_places_in_place_path<'db>(
    body: &MirBody<'db>,
    path: &crate::MirProjectionPath<'db>,
    out: &mut Vec<(Place<'db>, ValueId)>,
) {
    for proj in path.iter() {
        if let hir::projection::Projection::Index(hir::projection::IndexSource::Dynamic(value)) =
            proj
        {
            collect_move_places(body, *value, out);
        }
    }
}

fn collect_move_places<'db>(
    body: &MirBody<'db>,
    value: ValueId,
    out: &mut Vec<(Place<'db>, ValueId)>,
) {
    fn inner<'db>(
        body: &MirBody<'db>,
        value: ValueId,
        out: &mut Vec<(Place<'db>, ValueId)>,
        visiting: &mut FxHashSet<ValueId>,
    ) {
        if !visiting.insert(value) {
            return;
        }
        match &body.value(value).origin {
            ValueOrigin::MoveOut { place } => {
                out.push((place.clone(), value));
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
    while let ValueOrigin::TransparentCast { value } = &body.value(base).origin {
        base = *value;
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
