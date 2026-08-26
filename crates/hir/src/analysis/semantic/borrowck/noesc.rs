use cranelift_entity::EntityRef;
use rustc_hash::FxHashSet;

use crate::analysis::{
    HirAnalysisDb,
    diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
    semantic::{
        SemOrigin, SemanticCalleeRef, SemanticInstance,
        normalized::{NBlockId, NExpr, NOperand, NPlace, NStatement, NStatementKind},
    },
    ty::{
        ProviderAddressSpace,
        ty_check::BodyOwner,
        ty_def::{BorrowKind, TyId},
        ty_is_noesc,
    },
};

use super::{
    canon::{BorrowRoot, CanonPlace, State, address_space_for_borrow_root},
    check::{Borrowck, SemanticAnalysisError},
    diagnostics::{normalized_body_internal_diag, operand_origin},
    ir::{
        BlockedSemanticBody, BorrowDiagnosticId, SemanticBorrowCheckResult, SemanticBorrowDiagKind,
        SemanticBorrowDiagnostic, SemanticBorrowDiagnosticSpan, SemanticNormalizationFailure,
    },
};

pub fn check_semantic_noesc<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), SemanticAnalysisError<'db>> {
    match semantic_noesc_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => Ok(()),
        SemanticBorrowCheckResult::Blocked(body) => Err(SemanticAnalysisError::Blocked(body)),
        SemanticBorrowCheckResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

#[salsa::tracked]
pub(super) fn semantic_noesc_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticBorrowCheckResult<'db> {
    let borrowck = match Borrowck::new(db, instance) {
        Ok(borrowck) => borrowck,
        Err(SemanticNormalizationFailure::Blocked(blocked)) => {
            return SemanticBorrowCheckResult::Blocked(blocked);
        }
        Err(SemanticNormalizationFailure::InternalFailure(diag)) => {
            return SemanticBorrowCheckResult::Err(BorrowDiagnosticId::new(db, diag));
        }
    };
    match NoEsc::check(borrowck) {
        Ok(Some(blocked)) => SemanticBorrowCheckResult::Blocked(blocked),
        Ok(None) => SemanticBorrowCheckResult::Ok,
        Err(diag) => SemanticBorrowCheckResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

struct NoEsc<'db> {
    borrowck: Borrowck<'db>,
}

impl<'db> NoEsc<'db> {
    fn check(
        mut borrowck: Borrowck<'db>,
    ) -> Result<Option<BlockedSemanticBody<'db>>, SemanticBorrowDiagnostic<'db>> {
        borrowck.compute_entry_states();
        if let Some(blocked) = borrowck.compute_loan_targets()? {
            return Ok(Some(blocked));
        }
        Self { borrowck }.check_body().map(|()| None)
    }

    fn check_body(&self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for (bb_idx, block) in self.borrowck.body.blocks.iter().enumerate() {
            let mut state = self.borrowck.entry_state[NBlockId::new(bb_idx)].clone();
            for statement in &block.statements {
                self.check_statement(&state, statement)?;
                self.borrowck
                    .canon()
                    .apply_statement_state(&mut state, statement);
            }
        }
        Ok(())
    }

    fn check_statement(
        &self,
        state: &State,
        statement: &NStatement<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        match &statement.kind {
            NStatementKind::Define {
                expr: NExpr::Call { callee, args, .. },
                ..
            } => self.check_call_args(state, statement.origin, *callee, args),
            NStatementKind::Store { destination, value } => {
                self.check_store(state, statement.origin, destination, *value)
            }
            NStatementKind::Define { .. } => Ok(()),
        }
    }

    fn check_store(
        &self,
        state: &State,
        origin: SemOrigin<'db>,
        dst: &NPlace<'db>,
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
            let ty = self.operand_ty(arg, origin)?;
            if !matches!(ty.as_borrow(self.borrowck.db), Some((BorrowKind::Mut, _))) {
                continue;
            }
            let targets = self.borrowck.canon().borrow_value_targets(state, arg.value);
            let spaces = self.address_spaces_for_targets(&targets, operand_origin(arg, origin))?;
            let Some(space) = spaces
                .iter()
                .copied()
                .find(|space| *space != ProviderAddressSpace::Memory)
            else {
                continue;
            };
            return Err(self.noesc_diag(
                operand_origin(arg, origin),
                format!(
                    "cannot pass `{}` from {} as function argument",
                    ty.pretty_print(self.borrowck.db),
                    space.pretty()
                ),
            ));
        }
        Ok(())
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
            .value(operand.value)
            .map(|value| value.ty)
            .ok_or_else(|| {
                self.internal_diag(
                    origin,
                    format!(
                        "noesc operand value `%v{}` is missing",
                        operand.value.index()
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
