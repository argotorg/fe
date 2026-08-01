use rustc_hash::{FxHashMap, FxHashSet};

use crate::analysis::semantic::{SemOrigin, SemanticCalleeRef, get_or_build_semantic_instance};

use super::{
    super::{
        analyses::BorrowSummaryMode,
        check::{provisional_memory_summary_voucher, semantic_memory_summary_voucher},
        ir::{
            FreshAllocSite, MemoryAccessKind, MemorySummaryId, MemorySummaryItem,
            MemorySummaryTarget, NEffectArg, NEffectArgValue, NOperand, NSProjectionPath,
            SemanticBorrowDiagnostic,
        },
    },
    BorrowCanonCx, BorrowRoot, CanonPlace, LoanId, PointerTargets, State,
};

pub(in super::super) struct ResolvedMemoryAccess<'db> {
    pub(in super::super) kind: MemoryAccessKind,
    pub(in super::super) targets: FxHashSet<CanonPlace<'db>>,
    pub(in super::super) authorized: FxHashSet<LoanId>,
    pub(in super::super) authorizer_targets: FxHashSet<CanonPlace<'db>>,
}

pub(super) struct ResolvedPointerStore<'db> {
    pub(super) written: FxHashSet<CanonPlace<'db>>,
    pub(super) targets: PointerTargets<'db>,
    pub(super) weak: bool,
}

pub(super) struct ResolvedCallUpdates<'db> {
    pub(super) summary: MemorySummaryId<'db>,
    pub(super) stores: Vec<ResolvedPointerStore<'db>>,
    pub(super) may_return: bool,
}

pub(in super::super) struct BorrowSummaryResolver<'a, 'db> {
    canon: BorrowCanonCx<'a, 'db>,
    summary_mode: BorrowSummaryMode,
}

impl<'a, 'db> BorrowSummaryResolver<'a, 'db> {
    pub(in super::super) fn new(
        canon: BorrowCanonCx<'a, 'db>,
        summary_mode: BorrowSummaryMode,
    ) -> Self {
        Self {
            canon,
            summary_mode,
        }
    }

    fn resolve_targets(
        &self,
        state: &State<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        summary: &[MemorySummaryTarget<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<PointerTargets<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut out = PointerTargets::default();
        for target in summary {
            out.join_into(
                &self
                    .resolve_target(state, args, effect_args, target, origin)?
                    .0,
            );
        }
        Ok(out)
    }

    fn resolve_target(
        &self,
        state: &State<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        target: &MemorySummaryTarget<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(PointerTargets<'db>, FxHashSet<LoanId>), SemanticBorrowDiagnostic<'db>> {
        Ok(match target {
            MemorySummaryTarget::Input { input, proj } => {
                let super::super::ir::BorrowInputRef::Param(idx) = input;
                let Some(arg) = args.get(*idx as usize) else {
                    return Err(self.canon.internal_diag(
                        origin,
                        format!("memory summary refers to missing parameter {idx}"),
                    ));
                };
                (
                    PointerTargets::known(
                        self.canon
                            .canonicalize_value_path(state, arg.local, proj, origin)?,
                    ),
                    self.canon.loans_for_value(state, arg.local),
                )
            }
            MemorySummaryTarget::Effect { binding_idx, proj } => {
                let Some(arg) = effect_args
                    .iter()
                    .find(|arg| arg.binding_idx == *binding_idx)
                else {
                    return Err(self.canon.internal_diag(
                        origin,
                        format!("memory summary refers to missing effect argument {binding_idx}"),
                    ));
                };
                match &arg.arg {
                    NEffectArgValue::Place(place) => {
                        let bases = self.canon.canonicalize_place(state, place, origin)?;
                        (
                            PointerTargets::known(
                                self.canon
                                    .canonicalize_path_from_places(&bases, proj, state, origin)?,
                            ),
                            self.canon.loans_for_place(state, place),
                        )
                    }
                    NEffectArgValue::Value(value) => (
                        PointerTargets::known(self.canon.canonicalize_value_path(
                            state,
                            value.local,
                            proj,
                            origin,
                        )?),
                        self.canon.loans_for_value(state, value.local),
                    ),
                }
            }
            MemorySummaryTarget::FreshAllocation {
                site,
                address_space,
            } => (
                PointerTargets::known(FxHashSet::from_iter([CanonPlace {
                    root: BorrowRoot::FreshAllocation {
                        site: FreshAllocSite::Call {
                            call: origin,
                            callee: match site {
                                FreshAllocSite::Direct(origin)
                                | FreshAllocSite::Call { call: origin, .. } => *origin,
                            },
                        },
                        address_space: *address_space,
                    },
                    proj: NSProjectionPath::default(),
                }])),
                FxHashSet::default(),
            ),
            MemorySummaryTarget::Unknown { address_spaces } => (
                PointerTargets::unknown(*address_spaces),
                FxHashSet::default(),
            ),
        })
    }

    fn callee_memory_summary(
        &self,
        callee: &SemanticCalleeRef<'db>,
    ) -> Result<MemorySummaryId<'db>, SemanticBorrowDiagnostic<'db>> {
        let callee_instance = get_or_build_semantic_instance(self.canon.db, callee.key);
        match self.summary_mode.for_callee(self.canon.db, callee_instance) {
            BorrowSummaryMode::Final => {
                semantic_memory_summary_voucher(self.canon.db, callee_instance)
            }
            BorrowSummaryMode::Provisional => {
                provisional_memory_summary_voucher(self.canon.db, callee_instance)
            }
        }
    }

    pub(in super::super) fn resolve_call_memory_accesses(
        &self,
        state: &State<'db>,
        callee: &SemanticCalleeRef<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<Vec<ResolvedMemoryAccess<'db>>, SemanticBorrowDiagnostic<'db>> {
        let summary = self.callee_memory_summary(callee)?;
        let mut out = Vec::new();
        for item in summary.items(self.canon.db) {
            let MemorySummaryItem::Access {
                target,
                kind,
                authorizers,
            } = item
            else {
                continue;
            };
            let (targets, loans) = self.resolve_target(state, args, effect_args, target, origin)?;
            // Direct accesses are authorized only by mutable loans. Explicit
            // authorizers identify the receiver/capability through which an
            // otherwise unknown effect occurs, so their immutable loans must
            // also be excluded from self-conflict checks.
            let mut authorized = self.canon.mut_loans(loans);
            let mut authorizer_targets = FxHashSet::default();
            for authorizer in authorizers {
                let (targets, loans) =
                    self.resolve_target(state, args, effect_args, authorizer, origin)?;
                authorized.extend(loans);
                authorizer_targets.extend(targets.places());
            }
            out.push(ResolvedMemoryAccess {
                kind: *kind,
                targets: targets.places(),
                authorized,
                authorizer_targets,
            });
        }
        Ok(out)
    }

    pub(super) fn resolve_call_updates(
        &self,
        state: &State<'db>,
        callee: &SemanticCalleeRef<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        origin: SemOrigin<'db>,
    ) -> Result<ResolvedCallUpdates<'db>, SemanticBorrowDiagnostic<'db>> {
        let summary = self.callee_memory_summary(callee)?;
        let mut stores = Vec::new();
        for item in summary.items(self.canon.db) {
            let MemorySummaryItem::StorePointer {
                target,
                targets,
                weak,
            } = item
            else {
                continue;
            };
            stores.push(ResolvedPointerStore {
                written: self
                    .resolve_target(state, args, effect_args, target, origin)?
                    .0
                    .places(),
                targets: self.resolve_targets(state, args, effect_args, targets, origin)?,
                weak: *weak,
            });
        }
        Ok(ResolvedCallUpdates {
            summary,
            stores,
            may_return: summary.may_return(self.canon.db),
        })
    }

    pub(super) fn resolve_return_pointer_targets(
        &self,
        state: &State<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        summary: MemorySummaryId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<FxHashMap<NSProjectionPath<'db>, PointerTargets<'db>>, SemanticBorrowDiagnostic<'db>>
    {
        let mut by_output: FxHashMap<NSProjectionPath<'db>, PointerTargets<'db>> =
            FxHashMap::default();
        for item in summary.items(self.canon.db) {
            let MemorySummaryItem::ReturnPointer { output, targets } = item else {
                continue;
            };
            let item_targets = self.resolve_targets(state, args, effect_args, targets, origin)?;
            by_output
                .entry(output.clone())
                .and_modify(|existing| {
                    existing.join_into(&item_targets);
                })
                .or_insert(item_targets);
        }
        Ok(by_output)
    }
}
