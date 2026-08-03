use common::diagnostics::CompleteDiagnostic;
use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
    semantic::{SemOrigin, SemanticCalleeRef, SemanticInstance},
    ty::{
        ProviderAddressSpace,
        ty_check::{BodyOwner, ClosureCapture, ClosureCaptureConstruction},
        ty_contains_borrow,
        ty_def::{ClosureTy, TyId},
        ty_is_noesc,
    },
};

use super::{
    canon::{BorrowRoot, CanonPlace, State, address_space_for_borrow_root},
    check::Borrowck,
    diagnostics::{normalized_body_internal_diag, operand_origin},
    ir::{
        BorrowDiagnosticId, NExpr, NOperand, NSStmt, NSStmtKind, ReadMode,
        SemanticBorrowCheckResult, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
        SemanticBorrowDiagnosticSpan, local_has_runtime_move_semantics,
    },
};

pub fn check_semantic_noesc<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), CompleteDiagnostic> {
    check_semantic_noesc_voucher(db, instance).map_err(|diag| diag.to_complete(db))
}

pub fn check_semantic_noesc_voucher<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    match semantic_noesc_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => Ok(()),
        SemanticBorrowCheckResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

#[salsa::tracked]
pub(super) fn semantic_noesc_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowCheckResult<'db> {
    match Borrowck::new_for_check(db, instance).and_then(NoEsc::check) {
        Ok(()) => SemanticBorrowCheckResult::Ok,
        Err(diag) => SemanticBorrowCheckResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

struct NoEsc<'db> {
    borrowck: Borrowck<'db>,
}

impl<'db> NoEsc<'db> {
    fn check(mut borrowck: Borrowck<'db>) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        borrowck.compute_entry_states();
        borrowck.compute_loan_targets()?;
        Self { borrowck }.check_body()
    }

    fn check_body(&self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for (bb_idx, block) in self.borrowck.body.blocks.iter().enumerate() {
            let mut state =
                self.borrowck.entry_state[crate::analysis::semantic::SBlockId::new(bb_idx)].clone();
            if !state.is_reachable() {
                continue;
            }
            for stmt in &block.stmts {
                self.check_stmt(&state, stmt)?;
                self.borrowck.apply_stmt_state(&mut state, stmt);
            }
        }
        Ok(())
    }

    fn check_stmt(
        &self,
        state: &State,
        stmt: &NSStmt<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &stmt.kind {
            NSStmtKind::Assign {
                expr: NExpr::Call { callee, args, .. },
                ..
            } => self.check_call_args(state, stmt.origin, *callee, args),
            NSStmtKind::Assign {
                dst,
                expr: NExpr::AggregateMake { fields, .. },
            } => {
                let Some(closure) = self
                    .borrowck
                    .body
                    .local(*dst)
                    .and_then(|local| local.ty.as_closure(self.borrowck.db))
                else {
                    return Ok(());
                };
                let plan = self
                    .borrowck
                    .instance
                    .key(self.borrowck.db)
                    .callable_body(self.borrowck.db)
                    .closure_capture_plan(self.borrowck.db, closure)
                    .ok_or_else(|| {
                        self.internal_diag(
                            stmt.origin,
                            format!(
                                "missing capture plan for closure {:?}",
                                closure.def(self.borrowck.db)
                            ),
                        )
                    })?;
                self.check_closure_captures(state, stmt.origin, closure, fields, &plan)
            }
            NSStmtKind::Store { dst, src } => self.check_store(state, stmt.origin, dst, *src),
            NSStmtKind::Assign { .. } => Ok(()),
        }
    }

    fn check_closure_captures(
        &self,
        state: &State,
        origin: SemOrigin<'db>,
        closure: ClosureTy<'db>,
        captures: &[NOperand],
        plan: &[ClosureCapture<'db>],
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let field_tys = closure.captures(self.borrowck.db);
        if captures.len() != plan.len() || plan.len() != field_tys.len() {
            return Err(self.internal_diag(
                origin,
                format!(
                    "closure capture arity mismatch: plan={}, fields={}, operands={}",
                    plan.len(),
                    field_tys.len(),
                    captures.len()
                ),
            ));
        }
        for ((capture, capture_plan), field_ty) in captures.iter().copied().zip(plan).zip(field_tys)
        {
            let capture_origin = operand_origin(capture, origin);
            let mode_matches_plan = match capture_plan.construction {
                ClosureCaptureConstruction::Copy => capture.mode != ReadMode::Move,
                ClosureCaptureConstruction::Deferred => {
                    return Err(self.internal_diag(
                        capture_origin,
                        format!(
                            "closure capture {:?} retained deferred construction",
                            capture_plan.binding
                        ),
                    ));
                }
                ClosureCaptureConstruction::Move => {
                    capture.mode == ReadMode::Move || self.logical_move_uses_runtime_copy(capture)
                }
            };
            if !mode_matches_plan {
                return Err(self.internal_diag(
                    capture_origin,
                    format!(
                        "closure capture {:?} has {:?} construction but {:?} operand mode",
                        capture_plan.binding, capture_plan.construction, capture.mode
                    ),
                ));
            }
            let ty = self.operand_ty(capture, capture_origin)?;
            let planned_ty = self
                .borrowck
                .instance
                .normalized_ty(self.borrowck.db, capture_plan.ty);
            let field_ty = self
                .borrowck
                .instance
                .normalized_ty(self.borrowck.db, *field_ty);
            if ty != planned_ty || planned_ty != field_ty {
                return Err(self.internal_diag(
                    capture_origin,
                    format!(
                        "closure capture {:?} type mismatch: operand={}, plan={}, field={}",
                        capture_plan.binding,
                        ty.pretty_print(self.borrowck.db),
                        planned_ty.pretty_print(self.borrowck.db),
                        field_ty.pretty_print(self.borrowck.db),
                    ),
                ));
            }
            if !ty_is_noesc(self.borrowck.db, ty) {
                continue;
            }
            let Some(space) = self.non_memory_operand_space(state, capture, capture_origin)? else {
                continue;
            };
            return Err(self.noesc_diag(
                capture_origin,
                format!(
                    "cannot capture `{}` from {} in closure",
                    ty.pretty_print(self.borrowck.db),
                    space.pretty()
                ),
            ));
        }
        Ok(())
    }

    /// A logical move may lower to a physical copy for an unpacked ABI
    /// carrier. The capture plan remains `Move`, but normalized runtime
    /// ownership intentionally does not attach move semantics to that carrier.
    fn logical_move_uses_runtime_copy(&self, capture: NOperand) -> bool {
        capture.mode == ReadMode::Copy
            && self
                .borrowck
                .body
                .local(capture.local)
                .is_some_and(|local| {
                    !local_has_runtime_move_semantics(
                        self.borrowck.db,
                        local,
                        &self.borrowck.body.borrow_roots,
                    )
                })
    }

    fn check_store(
        &self,
        state: &State,
        origin: SemOrigin<'db>,
        dst: &super::ir::NSPlace<'db>,
        src: NOperand,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let targets = self
            .borrowck
            .canon()
            .canonicalize_place(state, dst, origin)?;
        let spaces = self.address_spaces_for_targets(&targets, origin)?;
        if spaces.contains(&ProviderAddressSpace::Calldata) {
            return Err(self.noesc_diag(origin, "cannot write to calldata".to_string()));
        }
        if spaces.contains(&ProviderAddressSpace::Code) {
            return Err(self.noesc_diag(origin, "cannot write to code".to_string()));
        }

        let Some(space) = spaces.iter().copied().find(|space| {
            matches!(
                space,
                ProviderAddressSpace::Storage | ProviderAddressSpace::Transient
            )
        }) else {
            return Ok(());
        };
        let src_ty = self.operand_ty(src, origin)?;
        if ty_is_noesc(self.borrowck.db, src_ty) {
            return Err(self.noesc_diag(
                origin,
                format!(
                    "cannot store `{}` in {}",
                    src_ty.pretty_print(self.borrowck.db),
                    space.pretty()
                ),
            ));
        }
        Ok(())
    }

    fn check_call_args(
        &self,
        state: &State,
        origin: SemOrigin<'db>,
        callee: SemanticCalleeRef<'db>,
        args: &[NOperand],
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for arg in args.iter().copied().skip(self.receiver_arg_count(callee)) {
            let arg_origin = operand_origin(arg, origin);
            let ty = self.operand_ty(arg, arg_origin)?;
            if !ty_contains_borrow(self.borrowck.db, ty) {
                continue;
            }
            let Some(space) = self.non_memory_operand_space(state, arg, arg_origin)? else {
                continue;
            };
            return Err(self.noesc_diag(
                arg_origin,
                format!(
                    "cannot pass `{}` from {} as function argument",
                    ty.pretty_print(self.borrowck.db),
                    space.pretty()
                ),
            ));
        }
        Ok(())
    }

    fn non_memory_operand_space(
        &self,
        state: &State,
        operand: NOperand,
        origin: SemOrigin<'db>,
    ) -> Result<Option<ProviderAddressSpace>, SemanticBorrowDiagnostic<'db>> {
        let targets = self
            .borrowck
            .canon()
            .borrow_local_targets(state, operand.local);
        let spaces = self.address_spaces_for_targets(&targets, origin)?;
        Ok(spaces
            .into_iter()
            .find(|space| *space != ProviderAddressSpace::Memory))
    }

    fn receiver_arg_count(&self, callee: SemanticCalleeRef<'db>) -> usize {
        match callee.key.owner(self.borrowck.db) {
            BodyOwner::Func(func) if func.receiver_ty(self.borrowck.db).is_some() => 1,
            _ => 0,
        }
    }

    fn operand_ty(
        &self,
        operand: NOperand,
        origin: SemOrigin<'db>,
    ) -> Result<TyId<'db>, SemanticBorrowDiagnostic<'db>> {
        self.borrowck
            .body
            .local(operand.local)
            .map(|local| local.ty)
            .ok_or_else(|| {
                self.internal_diag(
                    origin,
                    format!(
                        "noesc operand local `%{}` is missing",
                        operand.local.index()
                    ),
                )
            })
    }

    fn address_spaces_for_targets(
        &self,
        targets: &FxHashSet<CanonPlace<'db>>,
        origin: SemOrigin<'db>,
    ) -> Result<Vec<ProviderAddressSpace>, SemanticBorrowDiagnostic<'db>> {
        let mut spaces = Vec::with_capacity(targets.len());
        for target in targets {
            let space = self.address_space_for_root(&target.root, origin)?;
            if !spaces.contains(&space) {
                spaces.push(space);
            }
        }
        spaces.sort_by_key(|space| address_space_rank(*space));
        Ok(spaces)
    }

    fn address_space_for_root(
        &self,
        root: &BorrowRoot<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<ProviderAddressSpace, SemanticBorrowDiagnostic<'db>> {
        address_space_for_borrow_root(
            self.borrowck.db,
            self.borrowck.instance,
            &self.borrowck.body,
            root,
            origin,
        )
    }

    fn noesc_diag(&self, origin: SemOrigin<'db>, message: String) -> SemanticBorrowDiagnostic<'db> {
        SemanticBorrowDiagnostic::new(
            self.borrowck.instance,
            SemanticBorrowDiagKind::NoEscViolation,
            message,
            SemanticBorrowDiagnosticSpan::Origin {
                owner: self
                    .borrowck
                    .instance
                    .key(self.borrowck.db)
                    .owner(self.borrowck.db),
                origin,
            },
        )
    }

    fn internal_diag(
        &self,
        origin: SemOrigin<'db>,
        message: String,
    ) -> SemanticBorrowDiagnostic<'db> {
        normalized_body_internal_diag(
            self.borrowck.db,
            self.borrowck.instance,
            &self.borrowck.body,
            origin,
            message,
        )
    }
}

fn address_space_rank(space: ProviderAddressSpace) -> u8 {
    match space {
        ProviderAddressSpace::Memory => 0,
        ProviderAddressSpace::Calldata => 1,
        ProviderAddressSpace::Code => 2,
        ProviderAddressSpace::Storage => 3,
        ProviderAddressSpace::Transient => 4,
    }
}
