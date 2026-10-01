//! Structural return values and mutable-input poststates use the same algebra.
use super::validity::NativeValidity;
use crate::analysis::semantic::diagnostics::SemanticDiagnostic;
use std::collections::{BTreeMap, BTreeSet};

#[cfg(test)]
use std::cell::Cell;

#[cfg(test)]
thread_local! {
    static GUARD_INSTANTIATIONS: Cell<usize> = const { Cell::new(0) };
    static SOURCE_INSTANTIATIONS: Cell<usize> = const { Cell::new(0) };
}

use cranelift_entity::EntityRef;
use rustc_hash::FxHashMap;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BorrowActivation, FieldIndex, SemOrigin, SemanticInstance,
            capability::{
                birth::AllocationBirth,
                external::{
                    AddressProvenance, AliasBasis, ClobberCondition, ExternalOrigin,
                    ExternalSource, MemoryOffset, ReferentContract,
                },
                footprint::{AccessExtent, AccessFootprint},
                guard::{ChoiceKey, Guard, ValueOccurrence},
                handle::{
                    AddressOccurrence, HandleAddressSpace, OpaqueHandleContract, OpaqueHandleRef,
                    OpaqueWriteSite,
                },
                index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
                loan::{CapabilityRef, LoanDef, LoanId, LoanRef},
                opaque::OpaqueWrite,
                path::{Projection, RegionPath, StructuralPath},
                region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace, substitute_clause},
                semantics::{CapabilityClass, CapabilitySemantics},
                separation::{
                    SEPARATION_CLAUSE_LIMIT, SEPARATION_PAIR_LIMIT, Separation, SeparationSet,
                    validity_within_limits, within_limits,
                },
                shape::{ShapeChildren, ShapeId},
                source::{InputOrigin, SourceExpr},
                state::{BorrowState, CapabilityValue, CapabilityValues},
                value::{Guarded, IndexPayload, ValueId, ValueInterner, ValueLimits},
            },
            definite_assignment::{literal_bool_cond, literal_enum_variant},
            get_or_build_semantic_instance, instantiated_effect_env,
            normalized::{
                NEffectArg, NEffectArgValue, NExpr, NOperand, NStatement, NStatementKind,
                NTerminatorKind, NValueDefinition, NValueId,
            },
        },
        ty::{
            ProviderAddressSpace,
            corelib::{MemoryAccessKind, intrinsic_contract},
            provider::ProviderKind,
            ty_check::BodyOwner,
            ty_def::{BorrowKind, TyData, TyId},
        },
    },
    semantic::ProviderSource,
};

use super::{
    access::effect_occurrence,
    boundary::Boundary,
    check::{
        BorrowSummaryComputation, BorrowSummaryVoucher, provisional_borrow_summary_voucher,
        semantic_borrow_summary_voucher,
    },
    events::ConflictAnalysis,
    ir::{
        AvailabilityRequirement, AvailabilitySummary, BorrowSummary, BoundaryRequirement,
        CertifiedRangePoststate, InputPoststate, MemoryAccess, PendingSemanticValidation,
        ScalarInputPoststate, SeparationOrigin,
    },
    solver::{BorrowSummaryMode, Borrowck, Resolution},
    transport::{TransportRoute, referent_contract},
    validation::can_specialize,
};

pub type SourceValue<'db> = ValueId<'db, SourceExpr<'db>>;
type SourceValues<'db> = ValueInterner<'db, SourceExpr<'db>>;

impl<'db> BorrowSummary<'db> {
    /// Number choices only after assembling all summary components. Assigning
    /// numbers on first encounter separates related observations when poststates
    /// and access requirements encounter them in different orders. Preserve the
    /// local order through this injective renaming and subsequent call expansion.
    /// Loan requirements arrive separately with their origins, and leave as the
    /// summary component with origins in clause order.
    fn abstract_choices(
        mut self,
        db: &'db dyn HirAnalysisDb,
        values: &mut SourceValues<'db>,
        choices: BTreeSet<ValueOccurrence>,
        separations: Vec<(Guarded<'db, Separation<'db>>, SeparationOrigin<'db>)>,
    ) -> (Self, Vec<SeparationOrigin<'db>>) {
        let choices: BTreeMap<_, _> = choices
            .into_iter()
            .enumerate()
            .map(|(index, occurrence)| {
                (occurrence, index.try_into().expect("summary choice count"))
            })
            .collect();
        let rename = |occurrence: ValueOccurrence| match occurrence {
            ValueOccurrence::Argument(_) | ValueOccurrence::Summary => occurrence,
            _ => ValueOccurrence::SummaryChoice(choices[&occurrence]),
        };
        let map = |guard: &Guard<'db>| guard.map_occurrences(rename);
        for value in std::iter::once(&mut self.result)
            .chain(self.mutable_inputs.iter_mut().map(|input| &mut input.value))
            .chain(
                self.certified_ranges
                    .iter_mut()
                    .map(|range| &mut range.contents),
            )
        {
            *value = values.map_guards(value, map);
        }
        for range in &mut self.certified_ranges {
            range.coverage = map(&range.coverage).expect("injective summary choice renaming");
        }
        for region in self
            .requirements
            .iter_mut()
            .flat_map(|requirement| {
                std::iter::once(&mut requirement.region).chain(requirement.populated.iter_mut())
            })
            .chain(
                self.accesses
                    .iter_mut()
                    .flat_map(|access| [&mut access.region, &mut access.authorizers]),
            )
            .chain(
                self.availability
                    .incoming
                    .iter_mut()
                    .map(|requirement| &mut requirement.region),
            )
            .chain([
                &mut self.availability.reinitialized,
                &mut self.availability.unavailable,
                &mut self.native_requirements,
                &mut self.separation_validity,
            ])
        {
            *region = RegionSet::new(
                region.scope(),
                region.clauses().iter().map(|clause| Guarded {
                    guard: map(&clause.guard).expect("injective summary choice renaming"),
                    payload: clause.payload.clone(),
                }),
            );
        }
        self.separation_validity = self
            .separation_validity
            .forget_occurrences(|choice| matches!(choice, ValueOccurrence::SummaryChoice(_)));
        // Equal relations merge; the first origin in body order names each.
        let scope = BinderScope::default();
        let mut origins = BTreeMap::new();
        let mut clauses = Vec::new();
        for (clause, origin) in separations {
            // These are pre-call obligations. A caller must establish them for
            // every possible private execution of the callee; only argument
            // choices can restrict which executions it admits at the boundary.
            let renamed = SeparationSet::new(db, &scope, [clause])
                .map_occurrences(db, rename)
                .forget_occurrences(db, |choice| {
                    matches!(choice, ValueOccurrence::SummaryChoice(_))
                });
            for clause in renamed.clauses() {
                origins.entry(clause.payload.clone()).or_insert(origin);
                clauses.push(clause.clone());
            }
        }
        self.loan_requirements = SeparationSet::new(db, &scope, clauses);
        let provenance = self
            .loan_requirements
            .clauses()
            .iter()
            .map(|clause| origins[&clause.payload])
            .collect();
        self.accesses.sort();
        self.availability.incoming.sort();
        (self, provenance)
    }
}

#[derive(Clone, Copy)]
enum SummaryValueRole {
    Return,
    Retained,
    FreshInvalidPoststate,
    MirroredResult,
}

#[derive(Clone, Copy)]
pub(super) struct CallInputs<'a, 'db> {
    pub args: &'a [NOperand],
    pub effects: &'a [NEffectArg<'db>],
    pub origin: SemOrigin<'db>,
}

impl CallInputs<'_, '_> {
    pub fn occurrence(self, param: u32) -> Option<ValueOccurrence> {
        self.args
            .get(param as usize)
            .map(|arg| ValueOccurrence::Value(arg.value))
            .or_else(|| {
                self.effects
                    .iter()
                    .find(|effect| effect.binding_idx as usize + self.args.len() == param as usize)
                    .map(|effect| effect_occurrence(&effect.arg))
            })
    }
}

/// How a summary component may refer to the single object a call returns.
#[derive(Clone, Copy)]
enum PortUse {
    Result,
    Copy,
    Other,
}

fn contains_allocation_origin(source: &ExternalSource<'_>) -> bool {
    source.clobber.as_ref().is_some_and(|clobber| {
        contains_allocation_origin(&clobber.target.source)
            || contains_allocation_origin(&clobber.written.source)
    }) || match &source.origin {
        ExternalOrigin::Allocation(_) => true,
        ExternalOrigin::Memory { base, .. } => contains_allocation_origin(&base.source),
        _ => false,
    }
}

/// Resolutions share one immutable pre-call state and one argument mapping.
/// Inventory discovery and loan growth invalidate source resolutions; a
/// resolution computed while those facts change is never cached.
/// Guard substitutions depend only on the fixed body and call mapping, so they
/// remain reusable across inventory changes. Errors are never cached.
pub(super) struct SourceInstantiations<'a, 'db> {
    state: &'a BorrowState<'db>,
    result: NValueId,
    inputs: CallInputs<'a, 'db>,
    /// Separation that may drop a clobber-conditioned alternative. Resolutions
    /// are cached per session, so a basis never reuses another's results.
    basis: AliasBasis,
    generation: Option<usize>,
    resolved: BTreeMap<BinderScope, FxHashMap<SourceExpr<'db>, Resolution<'db>>>,
    guards: FxHashMap<Guard<'db>, Option<Guard<'db>>>,
    #[cfg(test)]
    evaluations: usize,
}

impl<'a, 'db> SourceInstantiations<'a, 'db> {
    pub(super) fn new(
        state: &'a BorrowState<'db>,
        result: NValueId,
        inputs: CallInputs<'a, 'db>,
    ) -> Self {
        Self::with_basis(state, result, inputs, AliasBasis::Assumed)
    }

    /// Resolution of separation endpoints, which may use only physical
    /// separation to drop an alternative.
    pub(super) fn physical(
        state: &'a BorrowState<'db>,
        result: NValueId,
        inputs: CallInputs<'a, 'db>,
    ) -> Self {
        Self::with_basis(state, result, inputs, AliasBasis::Physical)
    }

    fn with_basis(
        state: &'a BorrowState<'db>,
        result: NValueId,
        inputs: CallInputs<'a, 'db>,
        basis: AliasBasis,
    ) -> Self {
        Self {
            state,
            result,
            inputs,
            basis,
            generation: None,
            resolved: BTreeMap::new(),
            guards: FxHashMap::default(),
            #[cfg(test)]
            evaluations: 0,
        }
    }

    pub(super) fn guard(
        &mut self,
        checker: &Borrowck<'db>,
        guard: &Guard<'db>,
    ) -> Result<Option<Guard<'db>>, SemanticDiagnostic<'db>> {
        if let Some(instantiated) = self.guards.get(guard) {
            return Ok(instantiated.clone());
        }
        let instantiated = checker.instantiate_guard(guard, self.result, self.inputs)?;
        self.guards.insert(guard.clone(), instantiated.clone());
        Ok(instantiated)
    }

    pub(super) fn resolve(
        &mut self,
        checker: &mut Borrowck<'db>,
        source: &SourceExpr<'db>,
        scope: &BinderScope,
    ) -> Result<Resolution<'db>, SemanticDiagnostic<'db>> {
        let generation = checker.source_generation;
        if self.generation != Some(generation) {
            self.resolved.clear();
            self.generation = Some(generation);
        }
        if let Some(resolved) = self
            .resolved
            .get(scope)
            .and_then(|sources| sources.get(source))
        {
            return Ok(resolved.clone());
        }
        #[cfg(test)]
        {
            self.evaluations += 1;
            SOURCE_INSTANTIATIONS.set(SOURCE_INSTANTIATIONS.get() + 1);
        }
        let resolved = checker.instantiate_source_uncached(source, scope, self)?;
        if checker.source_generation == generation {
            self.resolved
                .entry(scope.clone())
                .or_default()
                .insert(source.clone(), resolved.clone());
        }
        Ok(resolved)
    }
}

#[derive(Clone)]
pub(super) struct CallSummary<'db> {
    pub instance: SemanticInstance<'db>,
    pub summary: BorrowSummary<'db>,
    /// Origins of the summary's loan requirements, when the callee has them.
    pub provenance: Vec<SeparationOrigin<'db>>,
    pub pending: bool,
    updates: Vec<CapabilityValue<'db>>,
    births: Vec<AllocationBirth<'db>>,
    single_result_port: bool,
}

impl<'db> Borrowck<'db> {
    fn instantiate_address_base(
        &self,
        mut source: ExternalSource<'db>,
        result: NValueId,
        origin: SemOrigin<'db>,
        single_result_port: bool,
    ) -> Result<ExternalSource<'db>, SemanticDiagnostic<'db>> {
        let mut invalid = false;
        let fresh = single_result_port && source.is_fresh_allocation();
        source.map_occurrences(&mut |occurrence, arguments| {
            let AddressOccurrence::Summary(choice) = *occurrence else {
                invalid = true;
                return;
            };
            *arguments = (if fresh { &[][..] } else { arguments.as_ref() })
                .iter()
                .copied()
                .chain(
                    self.inventory
                        .loops
                        .for_value(&self.body, result)
                        .map(IndexExpr::Iteration),
                )
                .collect();
            *occurrence = AddressOccurrence::Value {
                instance: self.instance,
                value: result,
                choice: if fresh { 0 } else { choice },
            };
        });
        if invalid {
            return Err(
                self.internal_diag(origin, "summary retains a local address occurrence".into())
            );
        }
        Ok(source)
    }

    pub fn prepare_calls(&mut self) -> Result<(), SemanticDiagnostic<'db>> {
        self.source_generation += 1;
        let calls: Vec<_> = self
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter_map(|statement| {
                if let NStatementKind::Define {
                    result,
                    expr: NExpr::Call { callee, .. },
                } = &statement.kind
                {
                    Some((*result, *callee, statement.origin))
                } else {
                    None
                }
            })
            .collect();
        let mut bases = Vec::new();
        for (result, callee, origin) in calls {
            let instance = get_or_build_semantic_instance(self.db, callee.key);
            // A default trait body is not the selected implementation of an
            // unresolved call: a concrete implementor can override that body.
            let voucher = if can_specialize(self.db, instance)
                && matches!(callee.key.owner(self.db), BodyOwner::Func(func)
                    if func.containing_trait(self.db).is_some()
                        && intrinsic_contract(self.db, func).is_none())
            {
                BorrowSummaryVoucher {
                    summary: Some(signature_summary(self.db, instance, true)?),
                    blocked: None,
                    pending: PendingSemanticValidation {
                        callees: [callee.key].into(),
                    },
                    provenance: Vec::new(),
                }
            } else {
                match self.summary_mode {
                    BorrowSummaryMode::Final => semantic_borrow_summary_voucher(self.db, instance),
                    BorrowSummaryMode::Provisional => {
                        provisional_borrow_summary_voucher(self.db, instance)
                    }
                }?
            };
            if self.blocked.is_none() {
                self.blocked = voucher.blocked;
            }
            let pending = !voucher.pending.callees.is_empty();
            self.pending.callees.extend(voucher.pending.callees);
            let Some(summary) = voucher.summary else {
                continue;
            };
            let updates = summary
                .mutable_inputs
                .iter()
                .map(|update| {
                    self.inventory.values.from_shape(
                        update.value.shape(),
                        update.value.scope(),
                        |semantics, _, scope| {
                            let payload = match semantics.class {
                                CapabilityClass::Borrow(kind) => {
                                    let (loan, args, _) = LoanDef::with_occurrence_arguments(
                                        kind,
                                        BorrowActivation::Immediate,
                                        origin,
                                        scope,
                                        self.inventory.loops.arguments(&self.body, result),
                                    );
                                    let id = LoanId(self.inventory.loans.len());
                                    assert_eq!(self.inventory.loan_seeds.len(), id.0);
                                    self.inventory.loan_seeds.push(loan.clone());
                                    self.inventory.loans.push(loan);
                                    CapabilityRef::borrow(kind, LoanRef { id, args })
                                }
                                CapabilityClass::View => {
                                    CapabilityRef::view(RegionSet::empty(scope), Vec::new())
                                }
                                CapabilityClass::Handle | CapabilityClass::Pointer => {
                                    CapabilityRef::Address(RegionSet::empty(scope))
                                }
                            };
                            vec![Guarded {
                                guard: Guard::always(scope),
                                payload,
                            }]
                        },
                    )
                })
                .collect();
            let sources = SourceValues::new(self.db, ValueLimits::default());
            let mut external = Vec::new();
            for leaf in sources.leaves(&summary.result, ValueOccurrence::Summary) {
                let scope = leaf.guard.scope().clone();
                external.push((
                    leaf.payload.source,
                    scope,
                    Some(leaf.guard),
                    PortUse::Result,
                ));
            }
            for update in &summary.mutable_inputs {
                let leaves = sources.leaves(&update.value, ValueOccurrence::Summary);
                let port = if update.value == summary.result
                    || (update.destination.source.fresh_allocation().is_some()
                        && leaves.iter().all(|leaf| leaf.payload.invalidated))
                {
                    PortUse::Copy
                } else {
                    PortUse::Other
                };
                for leaf in leaves {
                    let scope = leaf.guard.scope().clone();
                    external.push((leaf.payload.source, scope, Some(leaf.guard), port));
                }
                let destination = update.destination.source.clone();
                external.push((destination, update.value.scope().clone(), None, port));
            }
            for range in &summary.certified_ranges {
                for leaf in sources.leaves(&range.contents, ValueOccurrence::Summary) {
                    let scope = leaf.guard.scope().clone();
                    external.push((leaf.payload.source, scope, Some(leaf.guard), PortUse::Other));
                }
                external.push((
                    range.destination.source.clone(),
                    range.scope.clone(),
                    Some(range.coverage.clone()),
                    PortUse::Other,
                ));
            }
            let regions = summary
                .requirements
                .iter()
                .flat_map(|requirement| {
                    std::iter::once(&requirement.region).chain(requirement.populated.iter())
                })
                .chain(
                    summary
                        .accesses
                        .iter()
                        .flat_map(|access| [&access.region, &access.authorizers]),
                )
                .chain(
                    summary
                        .availability
                        .incoming
                        .iter()
                        .map(|requirement| &requirement.region),
                )
                .chain([
                    &summary.availability.reinitialized,
                    &summary.availability.unavailable,
                    &summary.native_requirements,
                    &summary.separation_validity,
                ]);
            external.extend(
                regions
                    .flat_map(|region| {
                        region
                            .clauses()
                            .iter()
                            .map(|clause| (&clause.guard, &clause.payload))
                    })
                    .chain(
                        summary
                            .loan_requirements
                            .clauses()
                            .iter()
                            .flat_map(|clause| {
                                [
                                    (&clause.guard, &clause.payload.protected),
                                    (&clause.guard, &clause.payload.access),
                                ]
                            }),
                    )
                    .filter_map(|(guard, place)| {
                        SourceExpr::from_place(place).map(|source| {
                            (
                                source.source,
                                guard.scope().clone(),
                                Some(guard.clone()),
                                PortUse::Other,
                            )
                        })
                    }),
            );
            // A direct capability result carries only one object in a call
            // evaluation. Its fresh alternatives therefore share one finite
            // call-result port when every other exported fresh object is that
            // same object: a stored copy of the result or invalid contents of
            // its fresh storage. Allocation arguments bound by a family or an
            // existential can name several objects in one evaluation, so an
            // equal abstract value does not identify a copy with the result.
            // The Value occurrence and enclosing loop arguments still
            // distinguish different calls.
            let single_result_port = summary.result.shape().direct(self.db).is_some()
                && summary.certified_ranges.is_empty()
                && summary.native_requirements.is_empty()
                && summary.separation_validity.is_empty()
                && external.iter().all(|(source, _, _, port)| match port {
                    PortUse::Result => true,
                    PortUse::Copy => source.fresh_allocation().is_none_or(|handle| {
                        handle.arguments.iter().all(|argument| {
                            !matches!(argument, IndexExpr::Bound(_) | IndexExpr::Iteration(_))
                        })
                    }),
                    PortUse::Other => !contains_allocation_origin(source),
                });
            let mut births = Vec::new();
            for (source, scope, guard, _) in external {
                if let Some(guard) = guard
                    && let Some(birth) = AllocationBirth::from_source(&source, guard)
                    && !births.contains(&birth)
                {
                    births.push(birth);
                }
                let base = if let Some(base) = source.address_base(self.db) {
                    self.instantiate_address_base(base, result, origin, single_result_port)?
                } else {
                    match source.origin {
                        // A conditional replacement's base is its whole family.
                        ExternalOrigin::OpaqueMemory => {
                            ExternalSource::opaque_memory(source.contract)
                        }
                        ExternalOrigin::Provider {
                            provider,
                            target_ty,
                            ..
                        } if !matches!(
                            provider.binding(self.db).source,
                            ProviderSource::UsesParam { .. }
                        ) =>
                        {
                            ExternalSource::provider(self.db, provider, target_ty)
                        }
                        ExternalOrigin::Input(_)
                        | ExternalOrigin::Memory { .. }
                        | ExternalOrigin::Provider { .. }
                        | ExternalOrigin::Local(_) => continue,
                        ExternalOrigin::Unknown { .. }
                        | ExternalOrigin::OpaqueHandle(_)
                        | ExternalOrigin::Allocation(_) => {
                            unreachable!("address bases handled above")
                        }
                    }
                };
                bases.push((base, scope));
            }
            self.calls.insert(
                result,
                CallSummary {
                    instance,
                    summary,
                    provenance: voucher.provenance,
                    pending,
                    updates,
                    births,
                    single_result_port,
                },
            );
        }
        self.inventory
            .add_external_sources(self.db, self.instance, bases)
            .map_err(|error| {
                self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    format!("unresolved external call storage: {error:?}"),
                )
            })?;
        Ok(())
    }

    pub fn borrow_summary(
        &mut self,
    ) -> Result<BorrowSummaryComputation<'db>, SemanticDiagnostic<'db>> {
        if let Some(computation) = self.unsolved_borrow_summary()? {
            return Ok(computation);
        }
        self.solve()?;
        self.solved_borrow_summary()
    }

    /// The summary of an intrinsic contract or a bodiless declaration, which
    /// is not solved.
    pub(super) fn unsolved_borrow_summary(
        &self,
    ) -> Result<Option<BorrowSummaryComputation<'db>>, SemanticDiagnostic<'db>> {
        if let Some(summary) = self.intrinsic_summary()? {
            self.verify_summary(&summary)?;
            return Ok(Some(BorrowSummaryComputation {
                summary: Some(summary),
                blocked: None,
                pending: Default::default(),
                provenance: Vec::new(),
            }));
        }
        if self
            .instance
            .key(self.db)
            .owner(self.db)
            .body(self.db)
            .is_none()
        {
            return Ok(Some(BorrowSummaryComputation {
                summary: Some(signature_summary(self.db, self.instance, true)?),
                blocked: None,
                pending: PendingSemanticValidation {
                    callees: [self.instance.key(self.db)].into(),
                },
                provenance: Vec::new(),
            }));
        }
        Ok(None)
    }

    /// The summary of the solved body.
    pub(super) fn solved_borrow_summary(
        &mut self,
    ) -> Result<BorrowSummaryComputation<'db>, SemanticDiagnostic<'db>> {
        // Requirements past a limit are incomplete; no fallback may hide that.
        if let ConflictAnalysis {
            exhausted: true,
            diagnostic: Some(diagnostic),
            ..
        } = self.conflicts()
        {
            return Err(diagnostic.clone());
        }
        let recursive_unresolved = !self.recursive_calls.is_empty()
            && (self.blocked.is_some() || !self.pending.callees.is_empty());
        let (summary, provenance) = if (!self.pending.callees.is_empty()
            && can_specialize(self.db, self.instance))
            || recursive_unresolved
        {
            if self.summary_mode == BorrowSummaryMode::Final && self.blocked.is_none() {
                if let Some(diagnostic) = self.availability_diagnostic() {
                    return Err(diagnostic);
                }
                self.boundary_requirements
                    .as_ref()
                    .expect("solved boundary requirements")
                    .clone()?;
                self.validate_pending_exports()?;
            }
            // A recursive component with unresolved callees has no validated
            // body contract to iterate. Keep its visible result opaque and
            // its Pending or Blocked status until those callees are resolved.
            // Independent final returning paths were checked above when the
            // component was pending rather than blocked.
            (signature_summary(self.db, self.instance, true)?, Vec::new())
        } else {
            self.build_summary()?
        };
        Ok(BorrowSummaryComputation {
            summary: Some(summary),
            blocked: self.blocked.clone(),
            pending: self.pending.clone(),
            provenance,
        })
    }

    /// Returning paths that never crossed an unresolved effect still have a
    /// complete boundary proof obligation, even in a pending template.
    fn validate_pending_exports(&self) -> Result<(), SemanticDiagnostic<'db>> {
        let mut values = SourceValues::new(self.db, ValueLimits::default());
        let mut choices = BTreeSet::new();
        let mut handles = BTreeMap::new();
        for (index, block) in self.body.blocks.iter().enumerate() {
            let NTerminatorKind::Return(returned) = block.terminator.kind else {
                continue;
            };
            let Some(state) = &self.terminal[index] else {
                continue;
            };
            if self.validation_dependencies[index][block.statements.len()] {
                continue;
            }
            let retained = self
                .inventory
                .inputs
                .iter()
                .filter(|input| {
                    input.writable && !matches!(input.source.origin, ExternalOrigin::Local(_))
                })
                .filter_map(|input| {
                    state
                        .storage()
                        .find(|(root, _)| **root == RegionRoot::External(input.source.clone()))
                })
                .map(|(_, value)| (value, SummaryValueRole::Retained));
            for (value, role) in returned
                .into_iter()
                .map(|returned| (state.value(returned.value), SummaryValueRole::Return))
                .chain(retained)
            {
                self.summarize_value(
                    value,
                    role,
                    block.terminator.origin,
                    &mut values,
                    &mut choices,
                    &mut handles,
                )?;
            }
        }
        Ok(())
    }

    /// The summary of the solved body, with its loan requirements' origins.
    pub fn build_summary(
        &self,
    ) -> Result<(BorrowSummary<'db>, Vec<SeparationOrigin<'db>>), SemanticDiagnostic<'db>> {
        #[cfg(feature = "borrowck-profile")]
        let profile = self.profile_scope("build_summary");
        // Provisional summaries supply provider facts needed for body admission
        // and definite assignment. Boundary policy must not suppress those facts.
        // A boundary violation is reported here; the local check reports its
        // ownership diagnostic.
        let pending = if self.summary_mode == BorrowSummaryMode::Final {
            self.boundary_requirements
                .as_ref()
                .expect("solved boundary requirements")
                .clone()?
        } else {
            Vec::new()
        };
        let ownership = self.analyze_availability();
        let _ = self
            .availability_diagnostic
            .set(ownership.diagnostic.clone());
        if self.summary_mode == BorrowSummaryMode::Final
            && self.blocked.is_none()
            && let Some(diagnostic) = ownership.diagnostic
        {
            return Err(diagnostic);
        }
        let scope = BinderScope::default();
        let result_shape = self.shape(self.instance.normalized_result_ty(self.db))?;
        let mut values = SourceValues::new(self.db, ValueLimits::default());
        let mut result = values.empty(result_shape, &scope);
        let mut updates = Vec::new();
        let inputs = self.inventory.inputs.clone();
        let pending_accesses = self.body_memory_accesses();
        // Control-flow restrictions and logical moves also change modeled
        // contents, but neither writes bytes. Publish typed poststates for write
        // destinations and newly created objects. Byte writes also expose their
        // corrupted interpretations. Typed alias invalidation is replayed from
        // the actual accesses, not as another store to each possible alias.
        let writes: Vec<_> = pending_accesses
            .iter()
            .filter(|access| access.kind == MemoryAccessKind::Write)
            .collect();
        for input in &inputs {
            if input.writable
                && input.shape.contains_capability(self.db)
                && !matches!(input.source.origin, ExternalOrigin::Local(_))
                && (input.source.is_fresh_allocation()
                    || writes.iter().any(|access| {
                        access.region.clauses().iter().any(|clause| {
                            matches!(&clause.payload.root, RegionRoot::External(source)
                                if input.source.match_instance(&input.scope, source, clause.guard.scope()).is_some())
                        }) || (access.extent != AccessExtent::Typed
                            && !matches!(
                                AccessFootprint::typed(&RegionSet::singleton(
                                    &input.scope,
                                    RegionRoot::External(input.source.clone()),
                                    RegionPath::default(),
                                ).quantify_into(self.db, access.region.scope()))
                                    .overlap(self.db, access.footprint()),
                                OverlapResult::Disjoint
                            ))
                    }))
            {
                let root = RegionRoot::External(input.source.clone());
                let initial = self
                    .inventory
                    .entry
                    .storage()
                    .find(|(key, _)| **key == root)
                    .expect("inventoried external entry")
                    .1;
                if self.terminal.iter().flatten().all(|state| {
                    state
                        .storage()
                        .find(|(key, _)| **key == root)
                        .is_some_and(|(_, value)| value == initial)
                }) {
                    continue;
                }
                updates.push(InputPoststate {
                    destination: SourceExpr {
                        invalidated: false,
                        views: Default::default(),
                        source: input.source.clone(),
                        path: RegionPath::default(),
                    },
                    value: values.empty(input.shape, &input.scope),
                });
            }
        }
        let mut may_return = false;
        let scalar_ty = self.instance.normalized_result_ty(self.db);
        let scalar_scope = BinderScope::default().bind(IndexNamespace::Result);
        let mut scalar_result = None;
        let mut choices = BTreeSet::new();
        let mut handles = BTreeMap::new();
        for index in 0..self.body.blocks.len() {
            let block = &self.body.blocks[index];
            let NTerminatorKind::Return(returned) = block.terminator.kind else {
                continue;
            };
            let Some(state) = self.terminal[index].clone() else {
                continue;
            };
            may_return = true;
            let origin = block.terminator.origin;
            // Facts over formal arguments hold on every normal return; an
            // integer result also relates to the value this path returns.
            let actual = returned
                .filter(|_| scalar_ty.is_integral(self.db))
                .map(|returned| self.index(returned.value));
            let uninformative = actual.is_some_and(|actual| {
                matches!(actual, IndexExpr::Runtime(value)
                    if !matches!(self.body.values[value.index()].definition,
                        NValueDefinition::EntryParam { .. })
                        && !state.guard().indices().contains(&actual))
            });
            let guard = if uninformative {
                Guard::always(&scalar_scope.0)
            } else {
                let guard = state.guard().in_scope(&scalar_scope.0);
                let guard = match actual {
                    Some(actual) => guard
                        .with_equality(scalar_scope.1, actual)
                        .expect("a fresh result can equal a returned scalar"),
                    None => guard,
                };
                let subst = IndexSubst::new(
                    &scalar_scope.0,
                    &scalar_scope.0,
                    guard.indices().into_iter().filter_map(|index| {
                        let IndexExpr::Runtime(value) = index else {
                            return None;
                        };
                        let replacement = match self.body.values[value.index()].definition {
                            NValueDefinition::EntryParam { param } => IndexExpr::FormalValue(param),
                            _ if Some(index) == actual => scalar_scope.1,
                            _ => return None,
                        };
                        Some((index, replacement))
                    }),
                )
                .expect("scalar result substitution");
                guard
                    .substitute(&subst)
                    .expect("renaming a feasible scalar return preserves feasibility")
                    .map_occurrences(|occurrence| match occurrence {
                        ValueOccurrence::Value(value)
                            if let NValueDefinition::EntryParam { param } =
                                self.body.values[value.index()].definition
                                && self
                                    .summary_param_ty(param)
                                    .is_some_and(|ty| ty.is_bool(self.db)) =>
                        {
                            ValueOccurrence::Argument(param)
                        }
                        other => other,
                    })
                    .expect("scalar choice renaming preserves feasibility")
                    .forget_occurrences(|occurrence| {
                        !matches!(occurrence, ValueOccurrence::Argument(_))
                    })
                    .forget_indices(|index| {
                        matches!(index, IndexExpr::Runtime(_) | IndexExpr::Iteration(_))
                    })
            };
            scalar_result = Some(
                scalar_result
                    .map_or_else(|| guard.clone(), |previous: Guard<'db>| previous.or(&guard)),
            );
            if let Some(returned) = returned {
                let returned = state.value(returned.value);
                if returned.shape() != result_shape {
                    return Err(self.internal_diag(
                        origin,
                        "return capability shape differs from its semantic signature".into(),
                    ));
                }
                let sources = self.summarize_value(
                    returned,
                    SummaryValueRole::Return,
                    origin,
                    &mut values,
                    &mut choices,
                    &mut handles,
                )?;
                result = values.join(&result, &sources);
            }
            for update in &mut updates {
                let source = &update.destination.source;
                let contents = state
                    .storage()
                    .find(|(root, _)| *root == &RegionRoot::External(source.clone()))
                    .expect("inventoried mutable input")
                    .1;
                let role =
                    if returned.is_some_and(|returned| contents == state.value(returned.value)) {
                        SummaryValueRole::MirroredResult
                    } else if source.fresh_allocation().is_some() {
                        SummaryValueRole::FreshInvalidPoststate
                    } else {
                        SummaryValueRole::Retained
                    };
                let sources = self.summarize_value(
                    contents,
                    role,
                    origin,
                    &mut values,
                    &mut choices,
                    &mut handles,
                )?;
                update.value = values.join(&update.value, &sources);
            }
        }
        for update in &mut updates {
            update
                .destination
                .source
                .map_occurrences(&mut |occurrence, _| {
                    let next = handles.len().try_into().expect("summary handle count");
                    *occurrence =
                        AddressOccurrence::Summary(*handles.entry(*occurrence).or_insert(next));
                });
        }
        let return_states: Vec<_> = self
            .body
            .blocks
            .iter()
            .zip(&self.terminal)
            .filter(|(block, _)| matches!(block.terminator.kind, NTerminatorKind::Return(_)))
            .filter_map(|(_, state)| state.as_ref())
            .collect();
        let scalar_inputs = self
            .inventory
            .inputs
            .iter()
            .filter_map(|input| {
                if !input.writable
                    || !input.source.contract.ty.is_integral(self.db)
                    || !matches!(&input.source.origin, ExternalOrigin::Input(_))
                {
                    return None;
                }
                let root = RegionRoot::External(input.source.clone());
                let (first, rest) = return_states.split_first()?;
                let value = first.scalar_constant(&root)?;
                if !rest
                    .iter()
                    .all(|state| state.scalar_constant(&root) == Some(value))
                {
                    return None;
                }
                Some(ScalarInputPoststate {
                    destination: SourceExpr::whole(input.source.clone()),
                    value,
                })
            })
            .collect();
        let mut requirements: Vec<BoundaryRequirement<'db>> = Vec::new();
        for mut requirement in pending {
            requirement.region = self.summarize_region(
                &requirement.region,
                requirement.origin,
                &mut choices,
                &mut handles,
            )?;
            requirement.populated = requirement
                .populated
                .as_ref()
                .map(|region| {
                    self.summarize_region(region, requirement.origin, &mut choices, &mut handles)
                })
                .transpose()?;
            if requirement.region.is_empty() {
                continue;
            }
            if let Some(existing) = requirements.iter_mut().find(|existing| {
                existing.rule == requirement.rule
                    && existing.instance == requirement.instance
                    && existing.origin == requirement.origin
                    && existing.populated == requirement.populated
            }) {
                existing.region = existing.region.union(&requirement.region);
            } else {
                requirements.push(requirement);
            }
        }
        let mut accesses = Vec::new();
        for access in pending_accesses {
            // Eliding fresh-object effects relies on the raw API's valid-range
            // precondition for the whole footprint. A cast/offset does not prove
            // containment, and out-of-allocation spans are outside that contract.
            // Occurrences exposed by results, poststates, and requirements stay
            // shared. An address used only by this effect is existential within
            // the effect: retaining call-depth identities would grow recursive
            // summaries forever without distinguishing their possible targets.
            let mut access_handles = handles.clone();
            let external = |region: &RegionSet<'db>| {
                RegionSet::new(
                    region.scope(),
                    region
                        .clauses()
                        .iter()
                        .filter(|clause| {
                            SourceExpr::from_place(&clause.payload)
                                .is_some_and(|source| !source.source.is_fresh_allocation())
                        })
                        .cloned(),
                )
            };
            let region = self.summarize_region(
                &external(&access.region)
                    .forget_occurrences(|occurrence| self.recursive_call_choice(occurrence)),
                SemOrigin::Body(self.body.template_owner),
                &mut choices,
                &mut access_handles,
            )?;
            if region.is_empty() {
                continue;
            }
            let authorizers = self.summarize_region(
                &external(&access.authorizers),
                SemOrigin::Body(self.body.template_owner),
                &mut choices,
                &mut access_handles,
            )?;
            let access = MemoryAccess {
                kind: access.kind,
                extent: self.summarize_extent(access.extent),
                region,
                authorizers,
            };
            if !accesses.contains(&access) {
                accesses.push(access);
            }
        }
        accesses.sort();
        accesses.dedup();
        let mut availability = AvailabilitySummary::empty();
        let origin = SemOrigin::Body(self.body.template_owner);
        let external = |region: &RegionSet<'db>, incoming: bool| {
            RegionSet::new(
                region.scope(),
                region
                    .clauses()
                    .iter()
                    .filter(|clause| {
                        SourceExpr::from_place(&clause.payload).is_some_and(|source| {
                            !matches!(source.source.origin, ExternalOrigin::Local(_))
                                && (!incoming || !source.source.is_fresh_allocation())
                        })
                    })
                    .cloned(),
            )
        };
        for requirement in ownership.summary.incoming {
            let extent = self.summarize_extent(requirement.extent);
            let region = self.summarize_availability_region(
                &external(&requirement.region, true),
                origin,
                &mut choices,
                &handles,
                false,
            )?;
            if !region.is_empty() {
                if let Some(existing) = availability
                    .incoming
                    .iter_mut()
                    .find(|old| old.kind == requirement.kind && old.extent == extent)
                {
                    existing.region = existing.region.union(&region);
                } else {
                    availability.incoming.push(AvailabilityRequirement {
                        kind: requirement.kind,
                        extent,
                        region,
                    });
                }
            }
        }
        availability.incoming.sort();
        availability.reinitialized = self.summarize_availability_region(
            &external(&ownership.summary.reinitialized, false),
            origin,
            &mut choices,
            &handles,
            true,
        )?;
        availability.unavailable = self.summarize_availability_region(
            &external(&ownership.summary.unavailable, false),
            origin,
            &mut choices,
            &handles,
            false,
        )?;
        let mut returns = return_states.iter();
        let mut common_ranges: Vec<_> = returns
            .next()
            .into_iter()
            .flat_map(|state| state.certified())
            .map(|certified| {
                (
                    certified.family.clone(),
                    certified.scope.clone(),
                    certified.coverage.clone(),
                    vec![&certified.contents],
                )
            })
            .collect();
        for state in returns {
            common_ranges.retain_mut(|(family, scope, coverage, contents)| {
                let Some(incoming) = state
                    .certified()
                    .iter()
                    .find(|incoming| incoming.family == *family && incoming.scope == *scope)
                else {
                    return false;
                };
                let Some(shared) = coverage.and(&incoming.coverage) else {
                    return false;
                };
                *coverage = shared;
                contents.push(&incoming.contents);
                true
            });
        }
        let mut certified_ranges = Vec::new();
        for (family, scope, coverage, contents) in common_ranges {
            if contents.iter().any(|value| {
                self.inventory
                    .values
                    .leaves(value, ValueOccurrence::Summary)
                    .iter()
                    .any(|leaf| {
                        !matches!(
                            leaf.semantics.class,
                            CapabilityClass::Pointer | CapabilityClass::Handle
                        )
                    })
            }) {
                continue;
            }
            let RegionRoot::External(source) = family else {
                continue;
            };
            let destination = SourceExpr {
                source,
                path: RegionPath::default(),
                views: Default::default(),
                invalidated: false,
            };
            let Some(summarized) =
                self.summarize_source(destination, &coverage, &mut choices, &mut handles)
            else {
                continue;
            };
            if summarized.guard.scope() != &scope {
                continue;
            }
            let mut combined = values.empty(contents[0].shape(), &scope);
            for value in contents {
                let mapped = self.summarize_value(
                    value,
                    SummaryValueRole::Retained,
                    origin,
                    &mut values,
                    &mut choices,
                    &mut handles,
                )?;
                combined = values.join(&combined, &mapped);
            }
            certified_ranges.push(CertifiedRangePoststate {
                destination: summarized.payload,
                scope,
                coverage: summarized.guard,
                contents: combined,
            });
        }
        let scalar_result =
            scalar_result.filter(|guard: &Guard<'db>| !Guard::always(guard.scope()).implies(guard));
        let mut separations =
            self.summarize_separations(&self.conflicts().deferred, &mut choices, &handles);
        // Equal relations take the first origin: prefer one forwarded from a
        // callee, which names the borrow and access where the relation began.
        let owner = self.instance.key(self.db).owner(self.db);
        separations.sort_by_key(|(_, origin)| origin.owner == owner);
        let summary = BorrowSummary {
            native_requirements: self.summarize_availability_region(
                &ownership.native_validity.requirements,
                origin,
                &mut choices,
                &handles,
                false,
            )?,
            loan_requirements: SeparationSet::empty(&BinderScope::default()),
            separation_validity: self.summarize_availability_region(
                &self.conflicts().validity.requirements,
                origin,
                &mut choices,
                &handles,
                false,
            )?,
            accesses,
            availability,
            may_return,
            result,
            scalar_result,
            mutable_inputs: updates,
            certified_ranges,
            scalar_inputs,
            requirements,
        };
        let (summary, provenance) =
            summary.abstract_choices(self.db, &mut values, choices, separations);
        // Merging equal relations ORs their guards, so check the final clauses.
        let requirements = &summary.loan_requirements;
        let validity = &summary.separation_validity;
        if requirements.clauses().len() > SEPARATION_CLAUSE_LIMIT
            || !requirements
                .clauses()
                .iter()
                .all(|clause| within_limits(clause, requirements.scope()))
            || validity.clauses().len() > SEPARATION_CLAUSE_LIMIT
            || !validity
                .clauses()
                .iter()
                .all(|clause| validity_within_limits(clause, validity.scope()))
        {
            return Err(self.separation_limit_diag(SemOrigin::Body(self.body.template_owner)));
        }
        self.verify_summary(&summary)?;
        #[cfg(feature = "borrowck-profile")]
        drop(profile);
        Ok((summary, provenance))
    }

    /// Export the separation callers owe. Each relation is abstracted as a
    /// whole, so its endpoints, extent and slices keep their shared selectors
    /// and address identities. There is no fresh-effect elision of an endpoint.
    fn summarize_separations(
        &self,
        requirements: &[(Guarded<'db, Separation<'db>>, SeparationOrigin<'db>)],
        choices: &mut BTreeSet<ValueOccurrence>,
        exposed: &BTreeMap<AddressOccurrence<'db>, u32>,
    ) -> Vec<(Guarded<'db, Separation<'db>>, SeparationOrigin<'db>)> {
        // Repeated accesses may export the same relation thousands of times.
        // The clause budget applies after abstract_choices merges equal
        // relations, alongside the final guard and validity checks.
        let mut clauses = Vec::new();
        for (clause, origin) in requirements {
            let subst = self.abstract_local_indices(
                clause.guard.scope(),
                clause
                    .guard
                    .indices()
                    .into_iter()
                    .chain(clause.payload.indices()),
            );
            let Some(guard) = clause.guard.substitute(&subst) else {
                continue;
            };
            let mut relation = clause.payload.substitute(self.db, &subst);
            // An address used only by this relation is named by its family and
            // an independent witness per distinct argument list, so recursion
            // cannot grow its arguments, and equal arguments stay shared.
            let mut handles = exposed.clone();
            let mut witnesses = BTreeMap::new();
            let mut scope = guard.scope().clone();
            for place in [&mut relation.protected, &mut relation.access] {
                let RegionRoot::External(source) = &mut place.root else {
                    unreachable!("representable separation endpoint")
                };
                source.map_occurrences(&mut |occurrence, arguments| {
                    if !exposed.contains_key(occurrence) {
                        let witness = *witnesses
                            .entry((*occurrence, arguments.clone()))
                            .or_insert_with(|| {
                                let (nested, witness) = scope.bind(IndexNamespace::Existential);
                                scope = nested;
                                witness
                            });
                        *arguments = Box::new([witness]);
                    }
                    let next = handles.len().try_into().expect("summary handle count");
                    *occurrence =
                        AddressOccurrence::Summary(*handles.entry(*occurrence).or_insert(next));
                });
            }
            // Forgetting a recursive choice widens the possible conflict. A
            // suspension that depended on one is dropped, never widened.
            let Some(guard) = guard
                .in_scope(&scope)
                .forget_occurrences(|occurrence| self.recursive_call_choice(occurrence))
                .map_occurrences(|occurrence| self.summary_occurrence(occurrence, choices))
            else {
                continue;
            };
            relation.suspended = relation
                .suspended
                .iter()
                .filter(|slice| {
                    !slice
                        .guard
                        .occurrences()
                        .into_iter()
                        .any(|occurrence| self.recursive_call_choice(occurrence))
                })
                .filter_map(|slice| {
                    Some(Guarded {
                        guard: slice.guard.map_occurrences(|occurrence| {
                            self.summary_occurrence(occurrence, choices)
                        })?,
                        payload: slice.payload.clone(),
                    })
                })
                .collect();
            let clause = Guarded {
                guard,
                payload: relation,
            };
            clauses.push((clause, *origin));
        }
        clauses
    }

    fn summarize_availability_region(
        &self,
        region: &RegionSet<'db>,
        origin: SemOrigin<'db>,
        choices: &mut BTreeSet<ValueOccurrence>,
        exposed_handles: &BTreeMap<AddressOccurrence<'db>, u32>,
        definite: bool,
    ) -> Result<RegionSet<'db>, SemanticDiagnostic<'db>> {
        let mut summaries = Vec::new();
        let region = if definite {
            region.clone()
        } else {
            region.forget_occurrences(|occurrence| self.recursive_call_choice(occurrence))
        };
        for clause in region.clauses() {
            let mut source =
                SourceExpr::from_place(&clause.payload).expect("filtered availability source");
            let mut guard = clause.guard.clone();
            let mut unexposed = false;
            source.source.map_occurrences(&mut |occurrence, arguments| {
                if !exposed_handles.contains_key(occurrence) {
                    unexposed = true;
                    // An effect-only address has no exported identity. Quantify
                    // it within this clause, so recursion cannot accumulate
                    // call-depth identities or correlate independent effects.
                    let (scope, witness) = guard.scope().bind(IndexNamespace::Existential);
                    guard = guard.in_scope(&scope);
                    *arguments = Box::new([witness]);
                }
            });
            if definite && unexposed {
                continue;
            }
            let region = RegionSet::new(
                region.scope(),
                [Guarded {
                    guard,
                    payload: SymbolicPlace {
                        root: RegionRoot::External(source.source),
                        path: source.path,
                        views: source.views,
                    },
                }],
            );
            summaries.push(self.summarize_region(
                &region,
                origin,
                choices,
                &mut exposed_handles.clone(),
            )?);
        }
        Ok(RegionSet::union_all(&BinderScope::default(), summaries))
    }

    fn summarize_region(
        &self,
        region: &RegionSet<'db>,
        origin: SemOrigin<'db>,
        choices: &mut BTreeSet<ValueOccurrence>,
        handles: &mut BTreeMap<AddressOccurrence<'db>, u32>,
    ) -> Result<RegionSet<'db>, SemanticDiagnostic<'db>> {
        let scope = BinderScope::default();
        let mut clauses = Vec::new();
        for clause in region.clauses() {
            let source = SourceExpr::from_place(&clause.payload).ok_or_else(|| {
                self.internal_diag(
                    origin,
                    "boundary requirement has no external provenance".into(),
                )
            })?;
            // Requirements quantify over all selected regions, independent
            // of any result shape that might otherwise bind array families.
            let fresh = clause.guard.scope().freshening(&scope);
            let guard = clause
                .guard
                .substitute(&fresh)
                .expect("boundary source scope");
            if let Some(source) =
                self.summarize_source(source.substitute(self.db, &fresh), &guard, choices, handles)
            {
                clauses.push(Guarded {
                    guard: source.guard,
                    payload: SymbolicPlace {
                        root: RegionRoot::External(source.payload.source),
                        path: source.payload.path,
                        views: source.payload.views,
                    },
                });
            }
        }
        Ok(RegionSet::new(&scope, clauses))
    }

    fn summarize_value(
        &self,
        value: &CapabilityValue<'db>,
        role: SummaryValueRole,
        origin: SemOrigin<'db>,
        values: &mut SourceValues<'db>,
        choices: &mut BTreeSet<ValueOccurrence>,
        handles: &mut BTreeMap<AddressOccurrence<'db>, u32>,
    ) -> Result<SourceValue<'db>, SemanticDiagnostic<'db>> {
        let boundary = if matches!(role, SummaryValueRole::Return) {
            Boundary::Return
        } else {
            Boundary::Retained
        };
        let mut failure = None;
        let cache = self.inventory.values.guard_cache();
        let result =
            self.inventory
                .values
                .map_payloads(value, values, |semantics, _, entry, domain| {
                    let region = entry
                        .payload
                        .region(self.db, &self.inventory.loans, entry.guard.scope())
                        .restricted(domain, |left, right| cache.borrow_mut().and(left, right));
                    if matches!(boundary, Boundary::Return)
                        && matches!(entry.payload, CapabilityRef::Invalidated { .. })
                        && !NativeValidity::from_region(&region).invalid
                    {
                        // The return's validity obligation excludes this branch.
                        // Poststates still retain corruption that was never used.
                        return Vec::new();
                    }
                    let mut sources = Vec::new();
                    for clause in region.clauses() {
                        let mut payload = match super::boundary::escape_source(
                            self,
                            value,
                            semantics,
                            &clause.payload,
                            boundary,
                            origin,
                        ) {
                            Ok(source) => source,
                            Err(diag) => {
                                failure.get_or_insert(diag);
                                continue;
                            }
                        };
                        payload.invalidated =
                            matches!(entry.payload, CapabilityRef::Invalidated { .. });
                        // A direct result contains one address per call evaluation.
                        // The finite call-result port names that fresh address;
                        // deeper recursive choices only select which fresh
                        // alternative reached it. Project those choices from
                        // the may-source, while the call result and loop
                        // generation still isolate this birth from older ones.
                        // Forwarded inputs remain may-aliases. Invalid native
                        // contents of that new object also stay invalid when
                        // their deeper selection is projected.
                        let input = matches!(&payload.source.origin, ExternalOrigin::Input(_));
                        let result = input || payload.source.is_fresh_allocation();
                        let projected = match role {
                            SummaryValueRole::Return => result,
                            SummaryValueRole::MirroredResult => result || payload.invalidated,
                            SummaryValueRole::FreshInvalidPoststate => payload.invalidated,
                            SummaryValueRole::Retained => false,
                        };
                        let guard = if projected {
                            clause.guard.forget_occurrences(|occurrence| {
                                self.recursive_call_choice(occurrence)
                                    && (input
                                        || matches!(occurrence, ValueOccurrence::CallChoice { result, .. }
                                            if self.calls[&result].single_result_port))
                            })
                        } else {
                            clause.guard.clone()
                        };
                        if let Some(source) =
                            self.summarize_source(payload, &guard, choices, handles)
                        {
                            sources.push(source);
                        }
                    }
                    sources
                });
        if let Some(failure) = failure {
            return Err(failure);
        }
        Ok(result)
    }

    fn summarize_source(
        &self,
        mut payload: SourceExpr<'db>,
        guard: &Guard<'db>,
        choices: &mut BTreeSet<ValueOccurrence>,
        handles: &mut BTreeMap<AddressOccurrence<'db>, u32>,
    ) -> Option<Guarded<'db, SourceExpr<'db>>> {
        if payload.invalidated
            && let ExternalOrigin::OpaqueHandle(handle) = &mut payload.source.origin
        {
            // Native markers establish neither an address nor loan identity.
            // Retain their overlap predicates and independent witnesses, while
            // avoiding an ever-growing call chain of marker identities.
            handle.occurrence = AddressOccurrence::NativeInvalidity;
        }
        payload.source.map_occurrences(&mut |occurrence, _| {
            let next = handles.len().try_into().expect("summary handle count");
            *occurrence = AddressOccurrence::Summary(*handles.entry(*occurrence).or_insert(next));
        });
        let subst = self.abstract_local_indices(
            guard.scope(),
            guard.indices().into_iter().chain(payload.indices()),
        );
        let guard = guard.substitute(&subst).and_then(|guard| {
            guard.map_occurrences(|occurrence| self.summary_occurrence(occurrence, choices))
        })?;
        Some(Guarded {
            guard,
            payload: payload.substitute(self.db, &subst),
        })
    }

    /// Abstract indices local to this body: an entry parameter becomes a
    /// formal value, and each other runtime value or loop iteration a fresh
    /// witness of `scope`.
    fn abstract_local_indices(
        &self,
        scope: &BinderScope,
        indices: impl IntoIterator<Item = IndexExpr<'db>>,
    ) -> IndexSubst<'db> {
        let mut destination = scope.clone();
        let mut substitutions = BTreeMap::new();
        for index in indices {
            if matches!(index, IndexExpr::Runtime(_) | IndexExpr::Iteration(_)) {
                substitutions.entry(index).or_insert_with(|| {
                    let actual = match index {
                        IndexExpr::Runtime(value) => self.index(value),
                        index => index,
                    };
                    if let IndexExpr::Runtime(actual) = actual
                        && let NValueDefinition::EntryParam { param } =
                            self.body.values[actual.index()].definition
                    {
                        return IndexExpr::FormalValue(param);
                    }
                    if !matches!(actual, IndexExpr::Runtime(_) | IndexExpr::Iteration(_)) {
                        return actual;
                    }
                    let (nested, witness) = destination.bind(IndexNamespace::Existential);
                    destination = nested;
                    witness
                });
            }
        }
        IndexSubst::new(scope, &destination, substitutions)
            .expect("summary local index abstraction")
    }

    /// Name a guard occurrence at the summary boundary. Entry parameters are
    /// arguments; other choices are collected for summary-wide renaming.
    fn summary_occurrence(
        &self,
        occurrence: ValueOccurrence,
        choices: &mut BTreeSet<ValueOccurrence>,
    ) -> ValueOccurrence {
        if let ValueOccurrence::Value(value) = occurrence
            && let NValueDefinition::EntryParam { param } =
                self.body.values[value.index()].definition
        {
            return ValueOccurrence::Argument(param);
        }
        if !matches!(
            occurrence,
            ValueOccurrence::Argument(_) | ValueOccurrence::Summary
        ) {
            choices.insert(occurrence);
        }
        occurrence
    }

    pub(super) fn summary_param_ty(&self, param: u32) -> Option<TyId<'db>> {
        self.inventory.transport.param_ty(param)
    }

    fn reachable_source_class(
        &self,
        external: &ExternalSource<'db>,
        requested: Option<CapabilitySemantics<'db>>,
        writable: bool,
    ) -> Option<CapabilityClass> {
        // A widened source denotes any reachable referent. Its final type can
        // differ from the abstract input that provides its authority.
        let classes = self
            .inventory
            .inputs
            .iter()
            .filter(|target| match (&target.source.origin, &external.origin) {
                (ExternalOrigin::Input(left), ExternalOrigin::Input(right)) => {
                    left.param() == right.param()
                }
                (ExternalOrigin::Provider { .. }, ExternalOrigin::Provider { .. }) => {
                    target.source.origin == external.origin
                }
                _ => false,
            })
            .flat_map(|target| target.classes.iter().copied());
        classes
            .filter(|class| {
                !writable
                    || matches!(
                        class,
                        CapabilityClass::Borrow(BorrowKind::Mut)
                            | CapabilityClass::Handle
                            | CapabilityClass::Pointer
                    )
            })
            .find(|class| {
                requested.is_none_or(|semantics| class.can_supply_result(semantics.class))
            })
    }

    fn verify_source(
        &self,
        source: &SourceExpr<'db>,
        scope: &BinderScope,
        requested: Option<CapabilitySemantics<'db>>,
        writable: bool,
    ) -> Result<TyId<'db>, SemanticDiagnostic<'db>> {
        let invalid = |message: &str| {
            self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                format!("invalid structural summary: {message}"),
            )
        };
        let db = self.db;
        if let Some(clobber) = &source.source.clobber {
            self.verify_source(&clobber.target, scope, None, false)?;
            self.verify_source(&clobber.written, scope, None, false)?;
        }
        if source.invalidated
            && (writable
                || !requested.is_some_and(|semantics| {
                    matches!(
                        semantics.class,
                        CapabilityClass::Borrow(_) | CapabilityClass::View
                    )
                }))
        {
            return Err(invalid(
                "invalidated contents cannot establish an address or authority",
            ));
        }
        for index in source.indices() {
            if scope.validate(index).is_err() {
                return Err(invalid("source contains a free binder"));
            }
            match index {
                IndexExpr::Runtime(_) | IndexExpr::Iteration(_) => {
                    return Err(invalid("source contains a callee-local index"));
                }
                IndexExpr::FormalValue(param)
                    if !self
                        .summary_param_ty(param)
                        .is_some_and(|ty| ty.as_view(db).unwrap_or(ty).is_integral(db)) =>
                {
                    return Err(invalid(
                        "source refers to a missing or nonintegral scalar parameter",
                    ));
                }
                _ => {}
            }
        }
        let external = &source.source;
        if external.contract
            != ReferentContract::new(db, external.contract.ty, external.contract.address_space)
        {
            return Err(invalid("referent addressability does not match its type"));
        }
        let (mut ty, mut class, mut space) = match &external.origin {
            ExternalOrigin::OpaqueMemory => (
                external.contract.ty,
                CapabilityClass::Pointer,
                external.contract.address_space,
            ),
            ExternalOrigin::Unknown {
                contract,
                occurrence,
                provenance,
                ..
            } => {
                if !matches!(occurrence, AddressOccurrence::Summary(_)) {
                    return Err(invalid(
                        "unknown address contains a callee-local occurrence",
                    ));
                }
                if *provenance == AddressProvenance::HashedStorageSlot
                    && *contract
                        != ReferentContract::new(
                            db,
                            TyId::u256(db),
                            HandleAddressSpace::Known(ProviderAddressSpace::Storage),
                        )
                {
                    return Err(invalid("hashed slot is not a storage word address"));
                }
                (
                    contract.ty,
                    CapabilityClass::Pointer,
                    contract.address_space,
                )
            }
            ExternalOrigin::Local(_) => return Err(invalid("source contains local storage")),
            ExternalOrigin::Input(input) => {
                self.summary_param_ty(input.param())
                    .ok_or_else(|| invalid("source parameter does not exist"))?;
                if external.is_reachable() {
                    let class = self
                        .reachable_source_class(external, requested, writable)
                        .ok_or_else(|| {
                            invalid("reachable source has no compatible input authority")
                        })?;
                    // A widened source keeps only an address space that an
                    // entry route from the same parameter established.
                    if external.contract.address_space != HandleAddressSpace::Unspecified
                        && !self.inventory.inputs.iter().any(|target| {
                            matches!(&target.source.origin, ExternalOrigin::Input(entry)
                                if entry.param() == input.param())
                                && target.source.contract == external.contract
                        })
                    {
                        return Err(invalid("widened source address space has no entry route"));
                    }
                    (external.contract.ty, class, external.contract.address_space)
                } else {
                    let (semantics, contract) = self
                        .inventory
                        .transport
                        .route(db, external)
                        .map_err(|_| invalid("input referent contract is unresolved"))?
                        .and_then(|route| route.selected)
                        .ok_or_else(|| invalid("input source does not select a capability"))?;
                    (contract.ty, semantics.class, contract.address_space)
                }
            }
            ExternalOrigin::Provider {
                provider,
                target_ty,
                ..
            } => {
                // Declared providers are built canonically, so matching the
                // complete origin also verifies its storage classification.
                if !self
                    .inventory
                    .inputs
                    .iter()
                    .any(|input| input.source.origin == external.origin)
                {
                    return Err(invalid("provider is not declared by the body or a call"));
                }
                if external.is_reachable() {
                    let class = self
                        .reachable_source_class(external, requested, writable)
                        .ok_or_else(|| invalid("reachable provider has no compatible authority"))?;
                    (external.contract.ty, class, external.contract.address_space)
                } else {
                    let binding = provider.binding(db);
                    let class = if binding.provider_ty.as_capability(db).is_some()
                        || binding.semantics.kind == ProviderKind::RootObject
                    {
                        CapabilityClass::Borrow(if binding.is_mut {
                            BorrowKind::Mut
                        } else {
                            BorrowKind::Ref
                        })
                    } else {
                        CapabilityClass::Handle
                    };
                    let base = ExternalSource::provider(db, *provider, *target_ty);
                    (*target_ty, class, base.contract.address_space)
                }
            }
            ExternalOrigin::Memory {
                base,
                offset,
                target_ty,
            } => {
                self.verify_source(base, scope, None, false)?;
                if offset
                    .index()
                    .is_some_and(|index| scope.validate(index).is_err())
                {
                    return Err(invalid("memory element has a free selector"));
                }
                (
                    *target_ty,
                    CapabilityClass::Pointer,
                    base.source.contract.address_space,
                )
            }
            ExternalOrigin::OpaqueHandle(handle) | ExternalOrigin::Allocation(handle) => {
                if !matches!(handle.occurrence, AddressOccurrence::Summary(_)) {
                    return Err(invalid("opaque source contains a callee-local occurrence"));
                }
                let declared = OpaqueHandleContract::for_ty(
                    db,
                    self.instance.key(db).impl_env(db).normalization_scope(db),
                    self.instance.assumptions(db),
                    handle.contract.handle_ty,
                )
                .map_err(|_| invalid("opaque source has an unresolved handle contract"))?;
                if declared != Some(handle.contract) {
                    return Err(invalid(
                        "opaque source differs from its declared target or address space",
                    ));
                }
                (
                    handle.contract.target_ty,
                    if handle.contract.handle_ty.as_ptr(db).is_some() {
                        CapabilityClass::Pointer
                    } else {
                        CapabilityClass::Handle
                    },
                    handle.contract.address_space,
                )
            }
        };
        if !external.is_reachable() {
            if !matches!(&external.origin, ExternalOrigin::Input(_))
                && let Some((semantics, contract)) = self
                    .inventory
                    .transport
                    .follow(
                        db,
                        TransportRoute::untransported(ty),
                        external.dereferences().iter().map(RegionPath::as_slice),
                    )
                    .map_err(|_| invalid("followed referent contract is unresolved"))?
                    .ok_or_else(|| invalid("followed source does not select a stored capability"))?
                    .selected
            {
                ty = contract.ty;
                class = semantics.class;
                space = contract.address_space;
            }
            if ty != external.contract.ty || space != external.contract.address_space {
                return Err(invalid("source contract differs from its final referent"));
            }
        }
        if writable
            && !matches!(
                class,
                CapabilityClass::Borrow(BorrowKind::Mut)
                    | CapabilityClass::Handle
                    | CapabilityClass::Pointer
            )
        {
            return Err(invalid("poststate destination is immutable"));
        }
        if let Some(requested) = requested
            && matches!(requested.class, CapabilityClass::Borrow(BorrowKind::Mut))
            && !matches!(
                class,
                CapabilityClass::Borrow(BorrowKind::Mut)
                    | CapabilityClass::Handle
                    | CapabilityClass::Pointer
            )
        {
            return Err(invalid("mutable result comes from shared authority"));
        }
        let target = source
            .referent_ty(db, self.instance)
            .ok_or_else(|| invalid("referent projection or conversion is invalid"))?;
        if let Some(requested) = requested {
            if target != requested.target_ty {
                return Err(invalid("source target differs from its capability slot"));
            }
            if matches!(
                requested.class,
                CapabilityClass::Handle | CapabilityClass::Pointer
            ) {
                let contract = referent_contract(db, self.instance, requested)
                    .map_err(|_| invalid("result handle has an unresolved contract"))?;
                if contract.address_space != external.contract.address_space {
                    return Err(invalid("result handle changes address space"));
                }
            }
        }
        Ok(target)
    }

    fn verify_summary(&self, summary: &BorrowSummary<'db>) -> Result<(), SemanticDiagnostic<'db>> {
        let values = SourceValues::new(self.db, ValueLimits::default());
        if summary.may_return && summary.result.has_missing_native_result(self.db) {
            let parameters: Vec<_> = self
                .body
                .values
                .iter()
                .filter(|value| matches!(value.definition, NValueDefinition::EntryParam { .. }))
                .map(|value| value.ty.pretty_print(self.db).as_str())
                .collect();
            return Err(self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                format!(
                    "returning native result `{}` has no represented referent (parameters: {})",
                    self.instance
                        .normalized_result_ty(self.db)
                        .pretty_print(self.db),
                    parameters.join(", "),
                ),
            ));
        }
        if summary.result.shape() != self.shape(self.instance.normalized_result_ty(self.db))?
            || summary.result.scope() != &BinderScope::default()
        {
            return Err(self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                "summary result shape or scope differs from its signature".into(),
            ));
        }
        if let Some(guard) = &summary.scalar_result {
            let (scope, _) = BinderScope::default().bind(IndexNamespace::Result);
            // Only an integer result can be named by the postcondition.
            let integral = self
                .instance
                .normalized_result_ty(self.db)
                .is_integral(self.db);
            if guard.scope() != &scope
                || guard.occurrences().into_iter().any(|occurrence| {
                    !matches!(occurrence, ValueOccurrence::Argument(param) if self.summary_param_ty(param).is_some_and(|ty| ty.is_bool(self.db)))
                })
                || guard.indices().into_iter().any(|index| match index {
                    IndexExpr::Bound(_) => !integral || scope.validate(index).is_err(),
                    IndexExpr::FormalValue(param) => !self
                        .summary_param_ty(param)
                        .is_some_and(|ty| ty.as_view(self.db).unwrap_or(ty).is_integral(self.db)),
                    IndexExpr::Const(_) | IndexExpr::TypeConst(_) => false,
                    IndexExpr::Runtime(_) | IndexExpr::Iteration(_) => true,
                })
            {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "scalar postcondition retains a callee-local fact".into(),
                ));
            }
        }
        for update in &summary.mutable_inputs {
            let ty = self.verify_source(&update.destination, update.value.scope(), None, true)?;
            if self.shape(ty)? != update.value.shape() {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "summary poststate differs from its destination shape".into(),
                ));
            }
        }
        for update in &summary.scalar_inputs {
            let ExternalOrigin::Input(_) = &update.destination.source.origin else {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "scalar poststate retains a non-input destination".into(),
                ));
            };
            if !self.inventory.inputs.iter().any(|target| {
                target.writable
                    && target.source == update.destination.source
                    && target.source.contract.ty.is_integral(self.db)
            }) || !update.destination.path.is_empty()
                || update.destination.views.iter().next().is_some()
                || update.destination.invalidated
            {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "scalar poststate has an invalid destination".into(),
                ));
            }
        }
        for extent in summary.accesses.iter().map(|access| access.extent).chain(
            summary
                .availability
                .incoming
                .iter()
                .map(|requirement| requirement.extent),
        ) {
            if extent.indices().any(|index| match index {
                IndexExpr::FormalValue(param) => !self
                    .summary_param_ty(param)
                    .is_some_and(|ty| ty.as_view(self.db).unwrap_or(ty).is_integral(self.db)),
                IndexExpr::Const(_) | IndexExpr::TypeConst(_) => false,
                IndexExpr::Runtime(_) | IndexExpr::Iteration(_) | IndexExpr::Bound(_) => true,
            }) {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "memory extent retains a local index or invalid scalar parameter".into(),
                ));
            }
        }
        for access in &summary.accesses {
            for region in [&access.region, &access.authorizers] {
                for clause in region.clauses() {
                    let source = SourceExpr::from_place(&clause.payload).ok_or_else(|| {
                        self.internal_diag(
                            SemOrigin::Body(self.body.template_owner),
                            "memory summary retains local storage".into(),
                        )
                    })?;
                    self.verify_source(&source, clause.guard.scope(), None, false)?;
                    self.verify_summary_guard(&clause.guard, &source)?;
                }
            }
        }
        self.verify_loan_requirements(&summary.loan_requirements)?;
        for region in summary
            .availability
            .incoming
            .iter()
            .map(|requirement| &requirement.region)
            .chain([
                &summary.availability.reinitialized,
                &summary.availability.unavailable,
                &summary.native_requirements,
                &summary.separation_validity,
            ])
        {
            if region.scope() != &BinderScope::default() {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "availability retains a lexical scope".into(),
                ));
            }
            for clause in region.clauses() {
                let source = SourceExpr::from_place(&clause.payload).ok_or_else(|| {
                    self.internal_diag(
                        SemOrigin::Body(self.body.template_owner),
                        "availability retains local storage".into(),
                    )
                })?;
                self.verify_source(&source, clause.guard.scope(), None, false)?;
                self.verify_summary_guard(&clause.guard, &source)?;
            }
        }
        for requirement in &summary.requirements {
            for region in std::iter::once(&requirement.region).chain(requirement.populated.iter()) {
                if region.scope() != &BinderScope::default() {
                    return Err(self.internal_diag(
                        requirement.origin,
                        "boundary requirement retains a lexical scope".into(),
                    ));
                }
                for clause in region.clauses() {
                    let source = SourceExpr::from_place(&clause.payload).ok_or_else(|| {
                        self.internal_diag(
                            requirement.origin,
                            "boundary requirement retains local storage".into(),
                        )
                    })?;
                    self.verify_source(&source, clause.guard.scope(), None, false)?;
                    self.verify_summary_guard(&clause.guard, &source)?;
                }
            }
        }
        for value in std::iter::once(&summary.result)
            .chain(summary.mutable_inputs.iter().map(|input| &input.value))
        {
            for leaf in values.leaves(value, ValueOccurrence::Summary) {
                self.verify_source(
                    &leaf.payload,
                    leaf.guard.scope(),
                    Some(leaf.semantics),
                    false,
                )?;
                self.verify_summary_guard(&leaf.guard, &leaf.payload)?;
            }
        }
        for range in &summary.certified_ranges {
            if range.coverage.scope() != &range.scope || range.contents.scope() != &range.scope {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "certified range has inconsistent binders".into(),
                ));
            }
            self.verify_source(&range.destination, &range.scope, None, true)?;
            self.verify_summary_guard(&range.coverage, &range.destination)?;
            for leaf in values.leaves(&range.contents, ValueOccurrence::Summary) {
                self.verify_source(
                    &leaf.payload,
                    leaf.guard.scope(),
                    Some(leaf.semantics),
                    false,
                )?;
                self.verify_summary_guard(&leaf.guard, &leaf.payload)?;
            }
        }
        Ok(())
    }

    /// Each relation is closed over its own witnesses: both endpoints are
    /// caller-visible sources, its kinds can conflict, its extent uses no local
    /// index, and every suspension slice projects its protected place.
    fn verify_loan_requirements(
        &self,
        requirements: &SeparationSet<'db>,
    ) -> Result<(), SemanticDiagnostic<'db>> {
        let invalid = |message: &str| {
            self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                format!("invalid separation requirement: {message}"),
            )
        };
        if requirements.scope() != &BinderScope::default() {
            return Err(invalid("it retains a lexical scope"));
        }
        for clause in requirements.clauses() {
            let separation = &clause.payload;
            let scope = clause.guard.scope();
            if separation.protected_kind == BorrowKind::Ref
                && separation.access_kind == BorrowKind::Ref
            {
                return Err(invalid("shared kinds cannot conflict"));
            }
            let endpoint = |place| {
                let source = SourceExpr::from_place(place)
                    .ok_or_else(|| invalid("it retains local storage"))?;
                self.verify_source(&source, scope, None, false)?;
                self.verify_summary_guard(&clause.guard, &source)?;
                Ok::<_, SemanticDiagnostic<'db>>(source)
            };
            let protected = endpoint(&separation.protected)?;
            endpoint(&separation.access)?;
            let local = |scope: &BinderScope, index: IndexExpr<'db>| match index {
                IndexExpr::FormalValue(param) => !self
                    .summary_param_ty(param)
                    .is_some_and(|ty| ty.as_view(self.db).unwrap_or(ty).is_integral(self.db)),
                IndexExpr::Const(_) | IndexExpr::TypeConst(_) => false,
                IndexExpr::Bound(_) => scope.validate(index).is_err(),
                IndexExpr::Runtime(_) | IndexExpr::Iteration(_) => true,
            };
            if separation.extent.indices().any(|index| local(scope, index)) {
                return Err(invalid("its extent retains a local index"));
            }
            for slice in &separation.suspended {
                // Suspension conditions lie in a prefix of the clause scope.
                let prefix = slice.guard.scope();
                if scope.existential_extension_of(prefix).is_none()
                    || slice.payload.indices().any(|index| local(prefix, index))
                {
                    return Err(invalid("a suspension slice is not in the clause scope"));
                }
                self.verify_summary_guard(&slice.guard, &protected)?;
                let projected = SourceExpr {
                    path: protected.path.concat(&slice.payload),
                    ..protected.clone()
                };
                if projected.referent_ty(self.db, self.instance).is_none() {
                    return Err(invalid(
                        "a suspension slice does not project its protected place",
                    ));
                }
            }
        }
        Ok(())
    }

    fn verify_summary_guard(
        &self,
        guard: &Guard<'db>,
        source: &SourceExpr<'db>,
    ) -> Result<(), SemanticDiagnostic<'db>> {
        if guard
            .occurrences()
            .iter()
            .any(|occurrence| match occurrence {
                ValueOccurrence::Value(_)
                | ValueOccurrence::Root(_)
                | ValueOccurrence::CallChoice { .. } => true,
                ValueOccurrence::Argument(param) => self.summary_param_ty(*param).is_none(),
                ValueOccurrence::Summary | ValueOccurrence::SummaryChoice(_) => false,
            })
            || guard.indices().iter().any(|index| match index {
                IndexExpr::FormalValue(param) => !self
                    .summary_param_ty(*param)
                    .is_some_and(|ty| ty.as_view(self.db).unwrap_or(ty).is_integral(self.db)),
                _ => guard.scope().validate(*index).is_err(),
            })
        {
            return Err(self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                "summary guard contains an invalid occurrence or scalar parameter".into(),
            ));
        }
        if matches!(&source.source.origin, ExternalOrigin::OpaqueHandle(source) if !matches!(source.occurrence, AddressOccurrence::Summary(_)))
        {
            return Err(self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                "summary retains a callee-local handle occurrence".into(),
            ));
        }
        if guard
            .indices()
            .into_iter()
            .chain(source.indices())
            .any(|index| matches!(index, IndexExpr::Runtime(_) | IndexExpr::Iteration(_)))
        {
            return Err(self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                "summary retains a callee-local value index".into(),
            ));
        }
        Ok(())
    }

    pub(super) fn call_births(
        &self,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Vec<AllocationBirth<'db>>, SemanticDiagnostic<'db>> {
        let Some(call) = self
            .calls
            .get(&result)
            .filter(|call| call.summary.may_return)
        else {
            return Ok(Vec::new());
        };
        let mut births = Vec::new();
        for template in &call.births {
            let Some(guard) = self.instantiate_guard(&template.guard, result, inputs)? else {
                continue;
            };
            let subst = self.formal_substitution(template.guard.scope(), guard.scope(), inputs);
            let source = ExternalSource::allocation(self.db, template.allocation.clone())
                .substitute(self.db, &subst);
            let source = self.instantiate_address_base(
                source,
                result,
                inputs.origin,
                call.single_result_port,
            )?;
            let birth =
                AllocationBirth::from_source(&source, guard).expect("allocation birth origin");
            if !births.contains(&birth) {
                births.push(birth);
            }
        }
        Ok(births)
    }

    pub fn transfer_call(
        &mut self,
        state: &mut BorrowState<'db>,
        result: NValueId,
        statement: &NStatement<'db>,
    ) -> Result<CapabilityValue<'db>, SemanticDiagnostic<'db>> {
        let NStatementKind::Define {
            expr: NExpr::Call {
                args, effect_args, ..
            },
            ..
        } = &statement.kind
        else {
            unreachable!()
        };
        let Some(call) = self.calls.get(&result).cloned() else {
            let shape = self.inventory.shapes[result.index()];
            if shape.contains_capability(self.db) {
                return Err(self.internal_diag(
                    statement.origin,
                    "capability-returning call has no summary".into(),
                ));
            }
            return Ok(self.inventory.values.empty(shape, &BinderScope::default()));
        };
        let template = self.inventory.definitions[&result].clone();
        let inputs = CallInputs {
            args,
            effects: effect_args,
            origin: statement.origin,
        };
        let returned =
            self.instantiate_value(state, &call.summary.result, &template, result, inputs)?;
        let mut updates: Vec<(RegionSet<'db>, CapabilityValue<'db>)> = Vec::new();
        for (update, template) in call.summary.mutable_inputs.iter().zip(&call.updates) {
            let value = self.instantiate_value(state, &update.value, template, result, inputs)?;
            let target = self.instantiate_source(
                state,
                &update.destination,
                result,
                update.value.scope(),
                inputs,
            )?;
            if let Some((_, existing)) = updates
                .iter_mut()
                .find(|(region, _)| *region == target.region)
            {
                *existing = self.inventory.values.join(existing, &value);
            } else {
                updates.push((target.region, value));
            }
        }
        let mut certified_ranges = Vec::new();
        for range in &call.summary.certified_ranges {
            let Some(coverage) = self.instantiate_guard(&range.coverage, result, inputs)? else {
                continue;
            };
            let target =
                self.instantiate_source(state, &range.destination, result, &range.scope, inputs)?;
            let template = self
                .inventory
                .values
                .empty(range.contents.shape(), &range.scope);
            let contents =
                self.instantiate_value(state, &range.contents, &template, result, inputs)?;
            certified_ranges.push((target.region, coverage, contents));
        }
        let mut scalar_updates = BTreeMap::new();
        for update in &call.summary.scalar_inputs {
            let resolved = self.instantiate_source(
                state,
                &update.destination,
                result,
                &BinderScope::default(),
                inputs,
            )?;
            let [clause] = resolved.region.clauses() else {
                continue;
            };
            if !clause.payload.path.is_empty()
                || clause.payload.views.iter().next().is_some()
                || clause.guard.scope() != state.guard().scope()
                || !state.guard().implies(&clause.guard)
            {
                continue;
            }
            scalar_updates
                .entry(clause.payload.root.clone())
                .and_modify(|value: &mut Option<usize>| {
                    if *value != Some(update.value) {
                        *value = None;
                    }
                })
                .or_insert(Some(update.value));
        }
        let accesses = self.call_memory_accesses(state, result, inputs)?;
        let consumed = if call.summary.availability.unavailable.is_empty() {
            Vec::new()
        } else {
            self.call_availability(state, result, inputs)?
                .expect("prepared call")
                .consumed
        };
        let storage = updates
            .iter()
            .map(|(region, _)| region)
            .chain(consumed.iter().map(|(region, _)| region))
            .flat_map(RegionSet::clauses)
            .filter_map(|clause| match &clause.payload.root {
                RegionRoot::External(source) => {
                    Some((source.clone(), clause.guard.scope().clone()))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        self.ensure_storage(state, storage, statement.origin)?;
        // Every source above was substituted against the same pre-call state.
        // Birth seeds precede final poststates, never caller-source reads.
        let births = self.call_births(result, inputs)?;
        state.birth_allocations(
            &mut self.inventory.values,
            &self.inventory.entry,
            &self.inventory.allocation_cells,
            &births,
        );
        let overwrite = self.opaque_write(statement.id);
        for access in accesses {
            if access.access.kind == MemoryAccessKind::Write {
                state
                    .invalidate_memory(
                        &mut self.inventory.values,
                        access.access.footprint(),
                        overwrite,
                    )
                    .map_err(|error| {
                        self.internal_diag(
                            statement.origin,
                            format!("unresolved opaque write: {error:?}"),
                        )
                    })?;
            }
        }
        // Final typed poststates can restore portions of a consumed aggregate.
        for (region, shape) in consumed {
            state
                .move_out(&mut self.inventory.values, &region, shape)
                .map_err(|error| {
                    self.internal_diag(
                        statement.origin,
                        format!("unresolved call consumption: {error:?}"),
                    )
                })?;
        }
        state
            .write_regions(
                overwrite,
                &mut self.inventory.values,
                &updates
                    .iter()
                    .map(|(region, value)| (region, value))
                    .collect::<Vec<_>>(),
            )
            .map_err(|error| {
                self.internal_diag(
                    statement.origin,
                    format!("unresolved call poststate: {error:?}"),
                )
            })?;
        for (region, coverage, contents) in certified_ranges {
            let [clause] = region.clauses() else {
                continue;
            };
            let Some(coverage) = coverage
                .and(&clause.guard)
                .and_then(|guard| guard.and(&state.guard().in_scope(region.scope())))
            else {
                continue;
            };
            state.certify_family_contents(
                &clause.payload.root,
                region.scope(),
                &coverage,
                &contents,
            );
        }
        for (root, value) in scalar_updates {
            if let Some(value) = value {
                state.store_scalar(root, IndexExpr::Const(value));
            }
        }
        if let Some(postcondition) = &call.summary.scalar_result {
            let (_, returned) = BinderScope::default().bind(IndexNamespace::Result);
            let subst = IndexSubst::new(
                postcondition.scope(),
                &BinderScope::default(),
                [(returned, IndexExpr::Runtime(result))],
            )
            .expect("scalar call result substitution");
            if let Some(postcondition) = postcondition.substitute(&subst)
                && let Some(postcondition) =
                    self.instantiate_guard(&postcondition, result, inputs)?
            {
                state.constrain(&postcondition, &mut self.inventory.values);
            }
        }
        Ok(returned)
    }

    fn instantiate_value(
        &mut self,
        state: &BorrowState<'db>,
        value: &SourceValue<'db>,
        template: &CapabilityValue<'db>,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<CapabilityValue<'db>, SemanticDiagnostic<'db>> {
        let cache = self.inventory.values.guard_cache();
        let mut values = CapabilityValues::sharing(&self.inventory.values, ValueLimits::default());
        let sources = SourceValues::sharing(&values, ValueLimits::default());
        let mut instantiations = SourceInstantiations::new(state, result, inputs);
        let mut error = None;
        let instantiated =
            sources.map_payloads(value, &mut values, |semantics, path, entry, domain| {
                let guard = match instantiations.guard(self, domain) {
                    Ok(Some(guard)) => guard,
                    Ok(None) => return Vec::new(),
                    Err(failure) => {
                        error.get_or_insert(failure);
                        return Vec::new();
                    }
                };
                let resolved = match instantiations.resolve(self, &entry.payload, guard.scope()) {
                    Ok(resolved) => resolved,
                    Err(failure) => {
                        error.get_or_insert(failure);
                        return Vec::new();
                    }
                };
                let region = if resolved.invalidated.invalid {
                    let requirements = &resolved.invalidated.requirements;
                    let lift = IndexSubst::new(requirements.scope(), guard.scope(), [])
                        .expect("native requirement scope");
                    requirements.substitute(self.db, &lift).with_guard(&guard)
                } else {
                    resolved
                        .region
                        .restricted(&guard, |left, right| cache.borrow_mut().and(left, right))
                };
                if region.is_empty() {
                    return Vec::new();
                }
                let payload = if entry.payload.invalidated || resolved.invalidated.invalid {
                    if !matches!(
                        semantics.class,
                        CapabilityClass::Borrow(_) | CapabilityClass::View
                    ) {
                        error.get_or_insert_with(|| self.invalidated_diag(inputs.origin));
                        return Vec::new();
                    }
                    CapabilityRef::Invalidated {
                        class: semantics.class,
                        region,
                    }
                } else {
                    match semantics.class {
                        CapabilityClass::Borrow(kind) => {
                            let lift = IndexSubst::new(template.scope(), guard.scope(), [])
                                .expect("result template scope");
                            let template = self.inventory.values.substitute(template, &lift);
                            let selected = self
                                .inventory
                                .values
                                .project(&template, path, ValueOccurrence::Value(result))
                                .expect("a result leaf selects an inventoried template member");
                            let reference = selected
                                .direct()
                                .first()
                                .and_then(|entry| entry.payload.loan())
                                .expect("inventoried call result loan")
                                .clone();
                            let parents = resolved.parents.into_iter().filter_map(|parent| {
                                let guard =
                                    parent.guard.and(&guard.in_scope(parent.guard.scope()))?;
                                Some(Guarded {
                                    guard,
                                    payload: parent.payload,
                                })
                            });
                            self.extend_loan(result, &reference, &region, parents.collect());
                            CapabilityRef::borrow(kind, reference)
                        }
                        CapabilityClass::View => CapabilityRef::view(region, resolved.parents),
                        CapabilityClass::Handle | CapabilityClass::Pointer => {
                            CapabilityRef::Address(region)
                        }
                    }
                };
                vec![Guarded { guard, payload }]
            });
        if let Some(error) = error {
            return Err(error);
        }
        Ok(instantiated)
    }

    pub(super) fn summarize_extent(&self, extent: AccessExtent<'db>) -> AccessExtent<'db> {
        let AccessExtent::Bytes(index) = extent else {
            return extent;
        };
        let index = match index {
            IndexExpr::Runtime(value) => self.index(value),
            index => index,
        };
        match index {
            IndexExpr::Runtime(value) => match self.body.values[value.index()].definition {
                NValueDefinition::EntryParam { param } => {
                    AccessExtent::Bytes(IndexExpr::FormalValue(param))
                }
                NValueDefinition::BlockParam { .. } | NValueDefinition::Statement { .. } => {
                    AccessExtent::Unknown
                }
            },
            IndexExpr::Bound(_) | IndexExpr::Iteration(_) => AccessExtent::Unknown,
            IndexExpr::Const(_) | IndexExpr::TypeConst(_) | IndexExpr::FormalValue(_) => {
                AccessExtent::Bytes(index)
            }
        }
    }

    pub(super) fn instantiate_extent(
        &self,
        extent: AccessExtent<'db>,
        inputs: CallInputs<'_, 'db>,
    ) -> AccessExtent<'db> {
        if let AccessExtent::Bytes(IndexExpr::FormalValue(param)) = extent {
            inputs
                .args
                .get(param as usize)
                .map_or(AccessExtent::Unknown, |arg| {
                    AccessExtent::Bytes(self.index(arg.value))
                })
        } else {
            extent
        }
    }

    /// Replace formal scalar parameters with this call's actual arguments.
    pub(super) fn formal_substitution(
        &self,
        source: &BinderScope,
        destination: &BinderScope,
        inputs: CallInputs<'_, 'db>,
    ) -> IndexSubst<'db> {
        IndexSubst::new(
            source,
            destination,
            inputs.args.iter().enumerate().map(|(param, arg)| {
                (
                    IndexExpr::FormalValue(param.try_into().expect("parameter count")),
                    self.index(arg.value),
                )
            }),
        )
        .expect("call scalar substitution")
    }

    pub(super) fn instantiate_guard(
        &self,
        guard: &Guard<'db>,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Option<Guard<'db>>, SemanticDiagnostic<'db>> {
        #[cfg(test)]
        GUARD_INSTANTIATIONS.set(GUARD_INSTANTIATIONS.get() + 1);
        let subst = self.formal_substitution(guard.scope(), guard.scope(), inputs);
        let mut invalid = false;
        let guard = guard
            .substitute(&subst)
            .and_then(|guard| {
                guard.map_occurrences(|occurrence| match occurrence {
                    ValueOccurrence::Argument(param) => inputs.occurrence(param).map_or(
                        ValueOccurrence::CallChoice {
                            result,
                            choice: param,
                        },
                        |occurrence| match occurrence {
                            ValueOccurrence::Value(value) => {
                                ValueOccurrence::Value(self.forwarded_value(value))
                            }
                            other => other,
                        },
                    ),
                    ValueOccurrence::SummaryChoice(choice) => {
                        ValueOccurrence::CallChoice { result, choice }
                    }
                    ValueOccurrence::Summary => ValueOccurrence::Value(result),
                    ValueOccurrence::Value(_)
                    | ValueOccurrence::Root(_)
                    | ValueOccurrence::CallChoice { .. } => {
                        invalid = true;
                        occurrence
                    }
                })
            })
            .and_then(|mut guard| {
                for arg in inputs.args {
                    let occurrence = ValueOccurrence::Value(self.forwarded_value(arg.value));
                    if let Some(value) = literal_bool_cond(self.db, &self.body, arg.value) {
                        guard = guard.with_boolean(
                            ChoiceKey::new(occurrence, StructuralPath::default()),
                            value,
                        )?;
                        guard = guard.forget_occurrences(|candidate| candidate == occurrence);
                    } else if let Some(variant) =
                        literal_enum_variant(self.db, &self.body, arg.value)
                    {
                        // Payload choices of the literal stay; only its variant is known.
                        guard = guard.with_variant(
                            ChoiceKey::new(occurrence, StructuralPath::default()),
                            variant,
                        )?;
                    }
                }
                Some(guard)
            });
        if invalid {
            return Err(self.internal_diag(
                inputs.origin,
                "summary retains a local choice occurrence".into(),
            ));
        }
        Ok(guard)
    }

    pub(super) fn instantiate_requirement(
        &mut self,
        state: &BorrowState<'db>,
        region: &RegionSet<'db>,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<RegionSet<'db>, SemanticDiagnostic<'db>> {
        let cache = self.inventory.values.guard_cache();
        let mut instantiated = RegionSet::empty(region.scope());
        for clause in region.clauses() {
            let source = SourceExpr::from_place(&clause.payload).ok_or_else(|| {
                self.internal_diag(
                    inputs.origin,
                    "summary boundary requirement retains local storage".into(),
                )
            })?;
            if let Some(guard) = self.instantiate_guard(&clause.guard, result, inputs)? {
                let resolved =
                    self.instantiate_source(state, &source, result, guard.scope(), inputs)?;
                instantiated = instantiated.union(
                    &resolved
                        .region
                        .restricted(&guard, |left, right| cache.borrow_mut().and(left, right))
                        .close_existentials(region.scope()),
                );
            }
        }
        Ok(instantiated)
    }

    pub(super) fn instantiate_source(
        &mut self,
        state: &BorrowState<'db>,
        source: &SourceExpr<'db>,
        result: NValueId,
        scope: &BinderScope,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Resolution<'db>, SemanticDiagnostic<'db>> {
        SourceInstantiations::new(state, result, inputs).resolve(self, source, scope)
    }

    /// A callee's conditional replacement, once for every pair of caller
    /// target and written places that can still overlap.
    fn conditional_region(
        &self,
        scope: &BinderScope,
        source: ExternalSource<'db>,
        clobber: Option<(RegionSet<'db>, RegionSet<'db>, AccessExtent<'db>)>,
        path: RegionPath<IndexExpr<'db>>,
        disjoint: impl Fn(&RegionSet<'db>, &RegionSet<'db>, AccessExtent<'db>) -> bool,
    ) -> RegionSet<'db> {
        if let Some((target, written, extent)) = clobber {
            let mut clauses = Vec::new();
            for target_clause in target.clauses() {
                for written_clause in written.clauses() {
                    let target_subst = target_clause
                        .guard
                        .scope()
                        .open_existentials(target.scope(), scope);
                    let written_subst = written_clause
                        .guard
                        .scope()
                        .open_existentials(written.scope(), target_subst.destination());
                    let target_subst = target_subst
                        .then(
                            &IndexSubst::new(
                                target_subst.destination(),
                                written_subst.destination(),
                                [],
                            )
                            .expect("clobber witness scope"),
                        )
                        .expect("fresh clobber witnesses");
                    let target_clause = substitute_clause(target_clause, &target_subst);
                    let written_clause = substitute_clause(written_clause, &written_subst);
                    let Some(guard) = target_clause.guard.and(&written_clause.guard) else {
                        continue;
                    };
                    if disjoint(
                        &RegionSet::new(guard.scope(), [target_clause.clone()]),
                        &RegionSet::new(guard.scope(), [written_clause.clone()]),
                        extent.substitute(&written_subst),
                    ) {
                        continue;
                    }
                    let mut alternative = source.clone();
                    alternative.clobber = SourceExpr::from_place(&target_clause.payload)
                        .zip(SourceExpr::from_place(&written_clause.payload))
                        .map(|(target, written)| {
                            Box::new(ClobberCondition::new(
                                target,
                                written,
                                extent.substitute(&written_subst),
                            ))
                        });
                    clauses.push(Guarded {
                        guard,
                        payload: SymbolicPlace {
                            root: RegionRoot::External(alternative),
                            path: path.clone(),
                            views: Default::default(),
                        },
                    });
                }
            }
            RegionSet::new(scope, clauses)
        } else {
            RegionSet::singleton(scope, RegionRoot::External(source), path)
        }
    }

    fn instantiate_source_uncached(
        &mut self,
        source: &SourceExpr<'db>,
        scope: &BinderScope,
        instantiations: &mut SourceInstantiations<'_, 'db>,
    ) -> Result<Resolution<'db>, SemanticDiagnostic<'db>> {
        let state = instantiations.state;
        let result = instantiations.result;
        let inputs = instantiations.inputs;
        let CallInputs {
            args,
            effects,
            origin,
        } = inputs;
        let subst = self.formal_substitution(scope, scope, inputs);
        let source = source.substitute(self.db, &subst);
        let invalidated = source.invalidated;
        let path = &source.path;
        let external = &source.source;
        let basis = instantiations.basis;
        // A clobber applies only where its target and written span overlap.
        let disjoint = |target: &RegionSet<'db>, written: &RegionSet<'db>, extent| {
            let written = AccessFootprint {
                region: written,
                extent,
            };
            match basis {
                AliasBasis::Assumed => matches!(
                    AccessFootprint::typed(target).overlap(self.db, written),
                    OverlapResult::Disjoint
                ),
                AliasBasis::Physical => written.physical_pairs(self.db, target).is_empty(),
            }
        };
        // A separation endpoint keeps its dependencies' native validity,
        // including where a dependency resolves to no valid alternative.
        let mut dependencies = NativeValidity::default();
        let clobber = if let Some(clobber) = &external.clobber {
            let target = instantiations.resolve(self, &clobber.target, scope)?;
            let written = instantiations.resolve(self, &clobber.written, scope)?;
            if basis == AliasBasis::Physical {
                if target.region.clauses().len() * written.region.clauses().len()
                    > SEPARATION_PAIR_LIMIT
                {
                    return Err(self.separation_limit_diag(origin));
                }
                dependencies |= target.invalidated;
                dependencies |= written.invalidated;
            }
            if disjoint(&target.region, &written.region, clobber.extent) {
                return Ok(Resolution {
                    invalidated: dependencies,
                    ..Resolution::empty(scope)
                });
            }
            Some((target.region, written.region, clobber.extent))
        } else {
            None
        };
        if let ExternalOrigin::Input(input) = &external.origin
            && external.is_reachable()
        {
            let mut resolved =
                self.reachable_input(state, input.param(), external.contract, path, scope, inputs)?;
            resolved.region =
                resolved
                    .region
                    .with_relative_views(self.db, &source.views, path.as_slice().len());
            return Ok(resolved);
        }
        let (mut resolved, mut target_ty) = match &external.origin {
            ExternalOrigin::OpaqueMemory => {
                // The callee's condition is restated over caller places.
                let family = ExternalSource::opaque_memory(external.contract);
                let region = self
                    .conditional_region(scope, family, clobber, path.clone(), disjoint)
                    .with_relative_views(self.db, &source.views, path.as_slice().len());
                if invalidated {
                    dependencies |= NativeValidity::from_region(&region);
                }
                return Ok(Resolution {
                    invalidated: dependencies,
                    region,
                    parents: Vec::new(),
                    traversed: Vec::new(),
                });
            }
            ExternalOrigin::Local(_) => {
                return Err(self.internal_diag(origin, "summary retains local storage".into()));
            }
            ExternalOrigin::Memory {
                base,
                offset,
                target_ty,
            } => {
                let base = instantiations.resolve(self, base, scope)?;
                let region = self.memory_region(&base.region, *target_ty, *offset, origin)?;
                (
                    Resolution {
                        invalidated: base.invalidated,
                        region,
                        parents: Vec::new(),
                        traversed: base.traversed.into_iter().chain(base.parents).collect(),
                    },
                    *target_ty,
                )
            }
            ExternalOrigin::OpaqueHandle(_)
            | ExternalOrigin::Allocation(_)
            | ExternalOrigin::Unknown { .. } => {
                let source = self.instantiate_address_base(
                    external.address_base(self.db).expect("address origin"),
                    result,
                    origin,
                    self.calls[&result].single_result_port,
                )?;
                let ty = source.contract.ty;
                let region = self.conditional_region(
                    scope,
                    source,
                    clobber,
                    RegionPath::default(),
                    disjoint,
                );
                (
                    Resolution {
                        invalidated: NativeValidity::default(),
                        region,
                        parents: Vec::new(),
                        traversed: Vec::new(),
                    },
                    ty,
                )
            }
            // A formal provider resolves to its actual effect argument, which
            // carries its own storage classification; a concrete binding is
            // rebuilt canonically. Neither copies the formal's classification.
            ExternalOrigin::Provider {
                provider,
                target_ty,
                ..
            } => {
                let source = ExternalSource::provider(self.db, *provider, *target_ty);
                let ty = source.contract.ty;
                if let ProviderSource::UsesParam {
                    requirement_idx, ..
                } = provider.binding(self.db).source
                {
                    let callee = self.calls.get(&result).expect("prepared call").instance;
                    let binding = provider.binding(self.db);
                    let local_requirement = instantiated_effect_env(self.db, callee)
                        .and_then(|env| {
                            let local_provider =
                                env.providers(self.db).iter().find(|candidate| {
                                    candidate.source == binding.source
                                        && candidate.provider_ty == binding.provider_ty
                                })?;
                            env.resolutions(self.db)
                                .iter()
                                .find(|resolution| {
                                    resolution.provider_idx == local_provider.provider_idx
                                })
                                .map(|resolution| resolution.requirement_idx)
                        })
                        .unwrap_or(requirement_idx);
                    let effect = effects
                        .iter()
                        .find(|effect| effect.binding_idx == local_requirement)
                        .ok_or_else(|| {
                            self.internal_diag(
                                origin,
                                format!("summary effect source has no call argument for binding {local_requirement}"),
                            )
                        })?;
                    let mut resolved = match &effect.arg {
                        NEffectArgValue::Place(place) => self.resolve_place(state, place),
                        NEffectArgValue::Value(value)
                            if self.body.values[value.value.index()].ty == ty =>
                        {
                            // A by-value provider can expose its own fields as
                            // well as a separate handle target. Reading the
                            // copied representation must use its structural value.
                            Resolution {
                                invalidated: NativeValidity::default(),
                                region: RegionSet::singleton(
                                    scope,
                                    RegionRoot::Value(value.value),
                                    RegionPath::default(),
                                ),
                                parents: Vec::new(),
                                traversed: Vec::new(),
                            }
                        }
                        NEffectArgValue::Value(value) => {
                            self.resolve_capability(state.value(value.value))
                        }
                    };
                    let lift = IndexSubst::new(resolved.region.scope(), scope, [])
                        .expect("effect region scope");
                    resolved.region = resolved.region.substitute(self.db, &lift);
                    resolved.parents = resolved
                        .parents
                        .into_iter()
                        .map(|parent| {
                            let subst = lift.under_existentials(parent.guard.scope());
                            Guarded {
                                guard: parent
                                    .guard
                                    .substitute(&subst)
                                    .expect("effect parent scope"),
                                payload: parent.payload.substitute(&subst),
                            }
                        })
                        .collect();
                    (resolved, ty)
                } else {
                    (
                        Resolution {
                            invalidated: NativeValidity::default(),
                            region: RegionSet::singleton(
                                scope,
                                RegionRoot::External(source),
                                RegionPath::default(),
                            ),
                            parents: Vec::new(),
                            traversed: Vec::new(),
                        },
                        ty,
                    )
                }
            }
            ExternalOrigin::Input(input) => {
                let param = input.param();
                let occurrence = inputs.occurrence(param).ok_or_else(|| {
                    self.internal_diag(origin, "summary input has no caller occurrence".into())
                })?;
                let (mut value, direct_place) = if let Some(arg) = args.get(param as usize) {
                    (state.value(arg.value).clone(), None)
                } else {
                    let binding = param as usize - args.len();
                    let effect = effects
                        .iter()
                        .find(|effect| effect.binding_idx as usize == binding)
                        .ok_or_else(|| {
                            self.internal_diag(
                                origin,
                                "summary input has no actual argument".into(),
                            )
                        })?;
                    match &effect.arg {
                        NEffectArgValue::Value(arg) => (state.value(arg.value).clone(), None),
                        NEffectArgValue::Place(place) => {
                            let region = self.resolve_region(state, place);
                            let shape = self.shape(place.ty)?;
                            let value =
                                self.read_region(state, &region, shape, occurrence, origin)?;
                            (value, Some((self.resolve_place(state, place), place.ty)))
                        }
                    }
                };
                let lift =
                    IndexSubst::new(value.scope(), scope, []).expect("actual argument scope");
                value = self.inventory.values.substitute(&value, &lift);
                match input.origin() {
                    InputOrigin::Place(_) => {
                        if let Some((mut resolved, ty)) = direct_place {
                            resolved.region = resolved.region.substitute(self.db, &lift);
                            (resolved, ty)
                        } else {
                            let semantics = value.shape().direct(self.db).ok_or_else(|| {
                                self.internal_diag(
                                    origin,
                                    "input place requires an explicit view or capability argument"
                                        .into(),
                                )
                            })?;
                            (self.resolve_capability(&value), semantics.target_ty)
                        }
                    }
                    InputOrigin::Slot { slot, .. } => {
                        let mut traversed = Vec::new();
                        let mut invalidated = NativeValidity::default();
                        if let Some(semantics) = value.shape().direct(self.db)
                            && semantics.class == CapabilityClass::View
                        {
                            let resolved = self.resolve_capability(&value);
                            invalidated |= resolved.invalidated;
                            traversed.extend(resolved.parents);
                            let region = resolved.region;
                            let shape = self.shape(semantics.target_ty)?;
                            value = self.read_region(state, &region, shape, occurrence, origin)?;
                        }
                        let Some(selected) =
                            self.inventory.values.project(&value, slot, occurrence)
                        else {
                            return Ok(Resolution::empty(scope));
                        };
                        let semantics = selected.shape().direct(self.db).ok_or_else(|| {
                            self.internal_diag(
                                origin,
                                "summary slot does not select a capability".into(),
                            )
                        })?;
                        let mut resolved = self.resolve_capability(&selected);
                        resolved.invalidated |= invalidated;
                        resolved.traversed = traversed;
                        (resolved, semantics.target_ty)
                    }
                }
            }
        };
        if external.is_reachable() {
            let invalidated = resolved.invalidated;
            resolved = self.reachable_region(
                state,
                &resolved.region,
                target_ty,
                external.contract,
                scope,
                origin,
            )?;
            resolved.invalidated |= invalidated;
        } else {
            let input_steps = match &external.origin {
                ExternalOrigin::Input(input) => input.dereferences(),
                _ => &[],
            };
            for step in input_steps.iter().chain(external.dereferences()) {
                let shape = self.shape(target_ty)?;
                let contents = self.read_region(
                    state,
                    &resolved.region,
                    shape,
                    ValueOccurrence::Value(result),
                    origin,
                )?;
                let Some(selected) = self.inventory.values.project(
                    &contents,
                    &StructuralPath::new(step.as_slice()),
                    ValueOccurrence::Value(result),
                ) else {
                    return Ok(Resolution::empty(scope));
                };
                let semantics = selected.shape().direct(self.db).ok_or_else(|| {
                    self.internal_diag(
                        origin,
                        "summary dereference does not select a stored capability".into(),
                    )
                })?;
                let mut selected = self.resolve_capability(&selected);
                selected.invalidated |= resolved.invalidated;
                selected.traversed.extend(resolved.traversed);
                selected.traversed.extend(resolved.parents);
                resolved = selected;
                target_ty = semantics.target_ty;
            }
        }
        resolved.region = resolved.region.project(path).with_relative_views(
            self.db,
            &source.views,
            path.as_slice().len(),
        );
        if invalidated {
            resolved.invalidated |= NativeValidity::from_region(&resolved.region);
        }
        resolved.invalidated |= dependencies;
        Ok(resolved)
    }
}

#[derive(Clone)]
struct Candidate<'db> {
    source: SourceExpr<'db>,
    guard: Guard<'db>,
    ty: TyId<'db>,
    class: CapabilityClass,
}

struct SignatureValues<'a, 'db> {
    checker: &'a Borrowck<'db>,
    candidates: Vec<Candidate<'db>>,
    values: SourceValues<'db>,
    choice: u32,
}

impl<'db> SignatureValues<'_, 'db> {
    /// A returning native capability has a valid result loan, even when its
    /// referent was freshly allocated or explicitly borrowed from raw memory.
    fn result(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
    ) -> Result<SourceValue<'db>, SemanticDiagnostic<'db>> {
        let checker = self.checker;
        let db = checker.db;
        let instance = checker.instance;
        let mut failure = None;
        let value = self.values.from_shape(shape, scope, |semantics, _, scope| {
            let mut sources: Vec<_> = self
                .candidates
                .iter()
                .filter(|candidate| {
                    (candidate.ty == semantics.target_ty
                        || matches!(
                            candidate.ty.base_ty(db).data(db),
                            TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
                        ))
                        && candidate.class.can_supply_result(semantics.class)
                })
                .filter_map(|candidate| {
                    let subst = candidate.guard.scope().freshening(scope);
                    let mut source = candidate.source.substitute(db, &subst);
                    if candidate.ty != semantics.target_ty
                        && matches!(
                            candidate.class,
                            CapabilityClass::Pointer | CapabilityClass::Handle
                        )
                    {
                        // Reinterpreting a raw address preserves that address;
                        // it does not inherit loans reachable through its bytes.
                        // Arbitrary other referents have their own alternative.
                        source = SourceExpr::whole(ExternalSource::memory(
                            db,
                            source,
                            semantics.target_ty,
                            MemoryOffset::Zero,
                        ));
                    } else if candidate.ty != semantics.target_ty {
                        source.source = source.source.widen();
                        source.source.contract = ReferentContract::new(
                            db,
                            semantics.target_ty,
                            source.source.contract.address_space,
                        );
                        source.path = RegionPath::default();
                        source.views = Default::default();
                    }
                    Some(Guarded {
                        guard: candidate.guard.substitute(&subst)?,
                        payload: source,
                    })
                })
                .collect();
            let occurrence = AddressOccurrence::Summary(self.choice);
            self.choice += 1;
            let source = if matches!(
                semantics.class,
                CapabilityClass::Borrow(_) | CapabilityClass::View
            ) {
                // Native results obey the language's memory-transport contract.
                // Instantiation creates a result loan; this is no inherited parent.
                Some(ExternalSource::unknown(
                    ReferentContract::memory(db, semantics.target_ty),
                    occurrence,
                    scope.variables().collect(),
                    AddressProvenance::Raw,
                ))
            } else {
                if let Ok(Some(contract)) = OpaqueHandleContract::for_ty(
                    db,
                    instance.key(db).impl_env(db).normalization_scope(db),
                    instance.assumptions(db),
                    semantics.representation_ty,
                ) {
                    Some(ExternalSource::opaque(
                        db,
                        OpaqueHandleRef {
                            contract,
                            occurrence,
                            arguments: scope.variables().collect(),
                        },
                    ))
                } else {
                    failure.get_or_insert_with(|| {
                        checker.internal_diag(
                            SemOrigin::Body(checker.body.template_owner),
                            "opaque result has an unresolved address contract".into(),
                        )
                    });
                    None
                }
            };
            if let Some(source) = source {
                sources.push(Guarded {
                    guard: Guard::always(scope),
                    payload: SourceExpr {
                        source,
                        path: RegionPath::default(),
                        views: Default::default(),
                        invalidated: false,
                    },
                });
            }
            sources
        });
        failure.map_or(Ok(value), Err)
    }

    /// Writable storage may preserve, replace, or raw-clobber its entry contents.
    /// Its native bytes have no return-value validity guarantee.
    fn contents(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
    ) -> Result<SourceValue<'db>, SemanticDiagnostic<'db>> {
        let valid = self.result(shape, scope)?;
        let checker = self.checker;
        let mut contents =
            CapabilityValues::sharing(&checker.inventory.values, ValueLimits::default());
        let overwrite = OpaqueWrite {
            site: OpaqueWriteSite::Summary(self.choice),
            scope: checker
                .instance
                .key(checker.db)
                .impl_env(checker.db)
                .normalization_scope(checker.db),
            assumptions: checker.instance.assumptions(checker.db),
        };
        self.choice += 1;
        let arbitrary = overwrite
            .contents(&mut contents, shape, scope, None)
            .map_err(|error| {
                checker.internal_diag(
                    SemOrigin::Body(checker.body.template_owner),
                    format!("opaque poststate has an unresolved capability: {error:?}"),
                )
            })?;
        let mut identities = BTreeMap::new();
        let arbitrary =
            contents.map_payloads(&arbitrary, &mut self.values, |_, _, entry, domain| {
                let region = entry
                    .payload
                    .region(checker.db, &checker.inventory.loans, entry.guard.scope())
                    .with_guard(domain);
                region
                    .clauses()
                    .iter()
                    .map(|clause| {
                        let mut source = SourceExpr::from_place(&clause.payload)
                            .expect("opaque external contents");
                        source.invalidated =
                            matches!(entry.payload, CapabilityRef::Invalidated { .. });
                        source.source.map_occurrences(&mut |occurrence, _| {
                            *occurrence = AddressOccurrence::Summary(
                                *identities.entry(*occurrence).or_insert_with(|| {
                                    let choice = self.choice;
                                    self.choice += 1;
                                    choice
                                }),
                            );
                        });
                        Guarded {
                            guard: clause.guard.clone(),
                            payload: source,
                        }
                    })
                    .collect()
            });
        Ok(self.values.join(&valid, &arbitrary))
    }
}

pub(super) fn signature_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    opaque: bool,
) -> Result<BorrowSummary<'db>, SemanticDiagnostic<'db>> {
    let body = super::inventory::signature_body(db, instance);
    let checker = Borrowck::new_with_body(db, instance, body, BorrowSummaryMode::Provisional)?;
    let values = SourceValues::new(db, ValueLimits::default());
    let shape = checker.shape(instance.normalized_result_ty(db))?;
    let mut candidates = Vec::new();
    if opaque {
        for input in &checker.inventory.inputs {
            let mut pending = vec![(input.ty, RegionPath::default(), Guard::always(&input.scope))];
            while let Some((ty, path, guard)) = pending.pop() {
                candidates.extend(input.classes.iter().map(|class| Candidate {
                    source: SourceExpr {
                        invalidated: false,
                        views: Default::default(),
                        source: input.source.clone(),
                        path: path.clone(),
                    },
                    guard: guard.clone(),
                    ty,
                    class: *class,
                }));
                let shape = checker.shape(ty)?;
                if shape.direct(db).is_some() {
                    continue;
                }
                match shape.children(db) {
                    ShapeChildren::None | ShapeChildren::EmptyArray => {}
                    ShapeChildren::Product(fields) => {
                        let types = instance.normalized_field_types(db, ty);
                        pending.extend(fields.iter().zip(types.iter().copied()).map(
                            |((field, _), ty)| {
                                (ty, path.appended(Projection::Field(*field)), guard.clone())
                            },
                        ));
                    }
                    ShapeChildren::Sum(variants) => {
                        for (variant, _) in variants {
                            pending.extend(
                                instance
                                    .normalized_enum_variant_field_tys(db, ty, *variant)
                                    .iter()
                                    .copied()
                                    .enumerate()
                                    .map(|(field, ty)| {
                                        (
                                            ty,
                                            path.appended(Projection::VariantField {
                                                variant: *variant,
                                                field: FieldIndex(
                                                    field.try_into().expect("verified field count"),
                                                ),
                                            }),
                                            guard.clone(),
                                        )
                                    }),
                            );
                        }
                    }
                    ShapeChildren::Array { len, .. } => {
                        let (scope, witness) = guard.scope().bind(IndexNamespace::Existential);
                        if let Some(guard) = guard.in_scope(&scope).with_bound(witness, len.index())
                        {
                            pending.push((
                                ty.generic_args(db)[0],
                                path.appended(Projection::Index(witness)),
                                guard,
                            ));
                        }
                    }
                }
            }
        }
    }
    let mut builder = SignatureValues {
        checker: &checker,
        candidates,
        values,
        choice: 0,
    };
    let result = if opaque {
        builder.result(shape, &BinderScope::default())?
    } else {
        builder.values.empty(shape, &BinderScope::default())
    };
    let mut mutable_inputs = Vec::new();
    if opaque {
        for input in &checker.inventory.inputs {
            if input.writable && input.shape.contains_capability(db) {
                mutable_inputs.push(InputPoststate {
                    destination: SourceExpr {
                        invalidated: false,
                        views: Default::default(),
                        source: input.source.clone(),
                        path: RegionPath::default(),
                    },
                    value: builder.contents(input.shape, &input.scope)?,
                });
            }
        }
    }
    let accesses = if opaque {
        checker.signature_memory_accesses(builder.choice)?
    } else {
        Vec::new()
    };
    let mut availability = AvailabilitySummary::empty();
    for access in &accesses {
        availability.incoming.push(AvailabilityRequirement {
            kind: MemoryAccessKind::Read,
            extent: access.extent,
            region: access.region.clone(),
        });
        // Raw non-Copy pointees permit ownership extraction. Native receivers
        // and byte-memory writes do not themselves consume.
        if access.kind == MemoryAccessKind::Move {
            availability.unavailable = availability.unavailable.union(&access.region);
        }
    }
    let summary = BorrowSummary {
        native_requirements: RegionSet::empty(&BinderScope::default()),
        loan_requirements: SeparationSet::empty(&BinderScope::default()),
        separation_validity: RegionSet::empty(&BinderScope::default()),
        may_return: opaque && !instance.is_intrinsically_never_returning(db),
        result,
        scalar_result: None,
        mutable_inputs,
        certified_ranges: Vec::new(),
        scalar_inputs: Vec::new(),
        requirements: Vec::new(),
        accesses,
        availability,
    };
    checker.verify_summary(&summary)?;
    Ok(summary)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{
            semantic::{
                VariantIndex,
                capability::{
                    DECISION_INTERN_ATTEMPTS,
                    external::ProviderStorage,
                    guard::ChoiceKey,
                    handle::HandleAddressSpace,
                    region::CANONICALIZED_REGION_CLAUSES,
                    repack::{ReferentRepackId, ReferentViews},
                    separation::{SEPARATION_GUARD_NODE_LIMIT, SEPARATION_WITNESS_LIMIT},
                    source::InputSource,
                    test_roots,
                },
                identity_semantic_instance_key,
                normalized::{NRootId, ReadMode},
                root_semantic_instance_key,
            },
            ty::{
                corelib::{resolve_core_trait, resolve_lib_func_path},
                trait_resolution::PredicateListId,
                ty_check::{BodyOwner, EffectArgLayoutView, EffectPassMode},
            },
        },
        hir_def::ItemKind,
        semantic::ContractFieldId,
        test_db::{HirAnalysisTestDb, find_func},
    };

    fn source_region<'db>(source: &SourceExpr<'db>, guard: &Guard<'db>) -> RegionSet<'db> {
        RegionSet::new(
            &BinderScope::default(),
            [Guarded {
                guard: guard.clone(),
                payload: SymbolicPlace {
                    root: RegionRoot::External(source.source.clone()),
                    path: source.path.clone(),
                    views: source.views.clone(),
                },
            }],
        )
    }

    #[test]
    fn recursive_read_only_summaries_preserve_input_cells() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "recursive_frame.fe".into(),
            r#"
struct Node { value: u256, next: Option<*Node> }
enum Link { Nil, Cons(*Node) }
fn read_next_ptr(_ start: *Node) -> Option<*Node> { start.next }
fn optional_next_value(_ start: *Node) -> u256 {
    if let Option::Some(node) = start.next { node.value } else { 0 }
}
fn is_cons(_ link: Link) -> bool {
    match link { Link::Nil => false, Link::Cons(_) => true }
}
fn relink(_ node: *Node, _ next: Option<*Node>) { node.next = next }
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        for name in ["read_next_ptr", "optional_next_value", "is_cons"] {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let summary = Borrowck::new(&db, instance)
                .unwrap()
                .borrow_summary()
                .unwrap()
                .summary
                .unwrap();
            assert!(
                summary.mutable_inputs.is_empty(),
                "{name} publishes writes to unchanged input cells: {summary:#?}"
            );
        }
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "relink"))),
        );
        let summary = Borrowck::new(&db, instance)
            .unwrap()
            .borrow_summary()
            .unwrap()
            .summary
            .unwrap();
        let [update] = summary.mutable_inputs.as_slice() else {
            panic!("relink must publish only its written cell: {summary:#?}");
        };
        assert_eq!(update.destination.source.param(), Some(0));
        assert!(!update.destination.source.is_reachable());
        assert!(
            summary
                .accesses
                .iter()
                .any(|access| access.kind == MemoryAccessKind::Write)
        );
    }

    #[test]
    fn followed_input_transport_matches_entry_and_summary_verification() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "followed_input_transport.fe".into(),
            r#"
struct Inner { cursor: mut u256 }
struct Outer { inner: mut Inner }
fn nested(_ outer: mut Outer) {}
fn raw(_ ptr: *Outer) {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        for (name, expected) in [
            (
                "nested",
                HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            ),
            ("raw", HandleAddressSpace::Unspecified),
        ] {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let checker = Borrowck::new(&db, instance).unwrap();
            let target = checker
                .inventory
                .inputs
                .iter()
                .find(|target| {
                    target.ty == TyId::u256(&db)
                        && target
                            .classes
                            .contains(&CapabilityClass::Borrow(BorrowKind::Mut))
                        && matches!(&target.source.origin, ExternalOrigin::Input(input)
                            if !input.dereferences().is_empty()
                                || !target.source.dereferences().is_empty())
                })
                .expect("nested mutable input target");
            assert_eq!(target.source.contract.address_space, expected, "{name}");
            let mut source = SourceExpr::whole(target.source.clone());
            assert_eq!(
                checker
                    .verify_source(&source, &target.scope, None, false)
                    .unwrap(),
                TyId::u256(&db),
                "{name}"
            );
            let forged = if expected == HandleAddressSpace::Unspecified {
                HandleAddressSpace::Known(ProviderAddressSpace::Memory)
            } else {
                HandleAddressSpace::Unspecified
            };
            source.source.contract.address_space = forged;
            assert!(
                checker
                    .verify_source(&source, &target.scope, None, false)
                    .is_err(),
                "{name}: forged transport contract was accepted"
            );
            // Widening loses the route, so only an entry-established space survives.
            source.source = target.source.clone().widen();
            assert!(
                checker
                    .verify_source(&source, &BinderScope::default(), None, false)
                    .is_ok(),
                "{name}: widened entry contract was rejected"
            );
            source.source.contract.address_space = forged;
            assert_eq!(
                checker
                    .verify_source(&source, &BinderScope::default(), None, false)
                    .is_err(),
                forged != HandleAddressSpace::Unspecified,
                "{name}: widened forged transport contract"
            );
        }
    }

    #[test]
    fn loan_requirements_are_verified_as_closed_relations() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "separation_verification.fe".into(),
            r#"
struct Pair { left: u256, right: u256 }
fn relate(_ held: mut Pair, _ other: mut Pair, _ count: u256) {}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "relate"))),
        );
        let checker = Borrowck::new(&db, instance).unwrap();
        let referent = |param| {
            checker
                .inventory
                .inputs
                .iter()
                .find(|target| {
                    matches!(&target.source.origin, ExternalOrigin::Input(input)
                        if input.param() == param && input.dereferences().is_empty())
                        && target.source.dereferences().is_empty()
                })
                .expect("direct input referent")
                .source
                .clone()
        };
        let field = |index| Projection::Field(FieldIndex(index));
        let scope = BinderScope::default();
        let always = Guard::always(&scope);
        let place = |root, path: Vec<_>| SymbolicPlace {
            root,
            path: RegionPath::new(path),
            views: Default::default(),
        };
        let valid = Separation {
            protected: place(RegionRoot::External(referent(0)), vec![]),
            protected_kind: BorrowKind::Mut,
            access: place(RegionRoot::External(referent(1)), vec![field(0)]),
            access_kind: BorrowKind::Ref,
            extent: AccessExtent::Typed,
            suspended: Box::new([Guarded {
                guard: always.clone(),
                payload: RegionPath::new([field(1)]),
            }]),
        };
        let requirements = |guard, separation| {
            SeparationSet::new(
                &db,
                &scope,
                [Guarded {
                    guard,
                    payload: separation,
                }],
            )
        };
        let slice = |guard| {
            Box::new([Guarded {
                guard,
                payload: RegionPath::new([field(1)]),
            }])
        };
        let (witnessed, witness) = scope.bind(IndexNamespace::Existential);
        for (name, guard, separation) in [
            ("valid", always.clone(), valid.clone()),
            (
                "integral formal extent",
                always.clone(),
                Separation {
                    extent: AccessExtent::Bytes(IndexExpr::FormalValue(2)),
                    ..valid.clone()
                },
            ),
            (
                "clause-owned extent",
                Guard::always(&witnessed)
                    .with_disequality(witness, IndexExpr::Const(0))
                    .unwrap(),
                // Slice conditions live in the clause scope.
                Separation {
                    extent: AccessExtent::Bytes(witness),
                    suspended: slice(Guard::always(&witnessed)),
                    ..valid.clone()
                },
            ),
            (
                "guard-only witness",
                Guard::always(&witnessed)
                    .with_boolean(
                        ChoiceKey::new(
                            ValueOccurrence::SummaryChoice(0),
                            StructuralPath::new([Projection::Index(witness)]),
                        ),
                        true,
                    )
                    .unwrap(),
                valid.clone(),
            ),
        ] {
            let requirements = requirements(guard, separation);
            assert!(
                checker.verify_loan_requirements(&requirements).is_ok(),
                "{name}: {requirements:#?}"
            );
        }
        let runtime = IndexExpr::Runtime(NValueId::from_u32(0));
        let local_choice = ChoiceKey::new(
            ValueOccurrence::Value(NValueId::from_u32(0)),
            StructuralPath::default(),
        );
        let mut malformed = referent(1);
        malformed.contract =
            ReferentContract::new(&db, TyId::u256(&db), malformed.contract.address_space);
        let mut views = ReferentViews::default();
        views.append(
            &db,
            0,
            ReferentRepackId::new(
                &db,
                TyId::u8(&db),
                TyId::u256(&db),
                module.scope(),
                PredicateListId::empty_list(&db),
            ),
        );
        for (name, guard, invalid) in [
            (
                "shared kinds",
                always.clone(),
                Separation {
                    protected_kind: BorrowKind::Ref,
                    ..valid.clone()
                },
            ),
            (
                "local endpoint",
                always.clone(),
                Separation {
                    access: place(test_roots::local(&db, NRootId::from_u32(0)), vec![]),
                    ..valid.clone()
                },
            ),
            (
                "malformed endpoint contract",
                always.clone(),
                Separation {
                    access: place(RegionRoot::External(malformed), vec![field(0)]),
                    ..valid.clone()
                },
            ),
            (
                "invalid endpoint view",
                always.clone(),
                Separation {
                    protected: SymbolicPlace {
                        views,
                        ..valid.protected.clone()
                    },
                    ..valid.clone()
                },
            ),
            (
                "local extent",
                always.clone(),
                Separation {
                    extent: AccessExtent::Bytes(runtime),
                    ..valid.clone()
                },
            ),
            (
                "nonintegral formal extent",
                always.clone(),
                Separation {
                    extent: AccessExtent::Bytes(IndexExpr::FormalValue(0)),
                    ..valid.clone()
                },
            ),
            (
                "missing formal extent",
                always.clone(),
                Separation {
                    extent: AccessExtent::Bytes(IndexExpr::FormalValue(9)),
                    ..valid.clone()
                },
            ),
            (
                "local guard occurrence",
                always.with_boolean(local_choice, true).unwrap(),
                valid.clone(),
            ),
            (
                "local guard index",
                always
                    .with_disequality(runtime, IndexExpr::Const(1))
                    .unwrap(),
                valid.clone(),
            ),
            (
                "nonintegral guard index",
                always
                    .with_disequality(IndexExpr::FormalValue(0), IndexExpr::Const(1))
                    .unwrap(),
                valid.clone(),
            ),
            (
                "local slice condition",
                always.clone(),
                Separation {
                    suspended: slice(
                        always
                            .with_disequality(runtime, IndexExpr::Const(1))
                            .unwrap(),
                    ),
                    ..valid.clone()
                },
            ),
            (
                "unprojected slice",
                always.clone(),
                Separation {
                    suspended: Box::new([Guarded {
                        guard: always.clone(),
                        payload: RegionPath::new([field(7)]),
                    }]),
                    ..valid.clone()
                },
            ),
        ] {
            assert!(
                checker
                    .verify_loan_requirements(&requirements(guard, invalid))
                    .is_err(),
                "{name}"
            );
        }
        let (lexical, _) = scope.bind(IndexNamespace::Value);
        assert!(
            checker
                .verify_loan_requirements(&SeparationSet::empty(&lexical))
                .is_err(),
            "lexical scope"
        );
    }

    #[test]
    fn provider_storage_classification_and_hashed_slot_contracts_are_verified() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "provider_storage_verification.fe".into(),
            r#"
use std::evm::{Address, StorageMap}
pub struct Ledger {
    pub balances: StorageMap<Address, u256>,
    pub total_supply: u256,
}
msg M {
    #[selector = 1]
    Supply -> u256,
}
pub contract C {
    mut ledger: Ledger,
    recv M {
        Supply -> u256 uses (ledger) { ledger.total_supply }
    }
}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let [contract] = module.all_contracts(&db).as_slice() else {
            panic!("expected one contract");
        };
        let owner = BodyOwner::ContractRecvArm {
            contract: *contract,
            recv_idx: 0,
            arm_idx: 0,
        };
        let instance =
            get_or_build_semantic_instance(&db, root_semantic_instance_key(&db, owner).unwrap());
        let checker = Borrowck::new(&db, instance).unwrap();
        let target = checker
            .inventory
            .inputs
            .iter()
            .find(|target| matches!(target.source.origin, ExternalOrigin::Provider { .. }))
            .expect("contract-field provider target");
        let ExternalOrigin::Provider {
            provider,
            target_ty,
            storage,
        } = target.source.origin.clone()
        else {
            unreachable!();
        };
        let ProviderStorage::AllocatedField(field) = storage else {
            panic!("allocated contract field");
        };
        let mut source = SourceExpr::whole(target.source.clone());
        assert!(
            checker
                .verify_source(&source, &target.scope, None, false)
                .is_ok()
        );
        let other_field = ContractFieldId {
            index: field.index + 1,
            ..field
        };
        for storage in [
            ProviderStorage::Other,
            ProviderStorage::AllocatedField(other_field),
        ] {
            source.source.origin = ExternalOrigin::Provider {
                provider,
                target_ty,
                storage,
            };
            assert!(
                checker
                    .verify_source(&source, &target.scope, None, false)
                    .is_err(),
                "a forged provider classification was accepted: {storage:?}"
            );
        }

        // Only a hashed slot is constrained to the storage word contract.
        let scope = BinderScope::default();
        for (space, provenance, valid) in [
            (
                ProviderAddressSpace::Storage,
                AddressProvenance::HashedStorageSlot,
                true,
            ),
            (
                ProviderAddressSpace::Memory,
                AddressProvenance::HashedStorageSlot,
                false,
            ),
            (ProviderAddressSpace::Memory, AddressProvenance::Raw, true),
        ] {
            let address = SourceExpr::whole(ExternalSource::unknown(
                ReferentContract::new(&db, TyId::u256(&db), HandleAddressSpace::Known(space)),
                AddressOccurrence::Summary(0),
                Box::new([]),
                provenance,
            ));
            assert_eq!(
                checker.verify_source(&address, &scope, None, false).is_ok(),
                valid,
                "{space:?} {provenance:?}"
            );
        }
    }

    #[test]
    fn availability_summary_canonicalizes_regions_in_linear_work() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "availability_union.fe".into(),
            "fn inspect(_ pointer: *u256) {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "inspect"))),
        );
        let checker = Borrowck::new(&db, instance).unwrap();
        let scope = BinderScope::default();
        let ty = TyId::u256(&db);
        let base = SourceExpr {
            invalidated: false,
            source: ExternalSource::input(
                InputSource::slot(0, StructuralPath::default()),
                ReferentContract::new(
                    &db,
                    ty,
                    HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                ),
                false,
            ),
            path: RegionPath::default(),
            views: Default::default(),
        };
        let count = 256;
        let region = RegionSet::new(
            &scope,
            (0..count).map(|index| Guarded {
                guard: Guard::always(&scope),
                payload: SymbolicPlace {
                    root: RegionRoot::External(ExternalSource::memory(
                        &db,
                        base.clone(),
                        ty,
                        MemoryOffset::Element(ty, IndexExpr::Const(index)),
                    )),
                    path: RegionPath::default(),
                    views: Default::default(),
                },
            }),
        );
        assert_eq!(region.clauses().len(), count);
        for definite in [false, true] {
            let before = CANONICALIZED_REGION_CLAUSES.get();
            let summarized = checker
                .summarize_availability_region(
                    &region,
                    SemOrigin::Body(checker.body.template_owner),
                    &mut BTreeSet::new(),
                    &BTreeMap::new(),
                    definite,
                )
                .unwrap();
            let visits = CANONICALIZED_REGION_CLAUSES.get() - before;
            assert_eq!(summarized, region);
            assert!(
                visits <= count * 4,
                "processed {visits} clauses for {count} alternatives"
            );
        }
    }

    #[test]
    fn call_memory_and_availability_regions_preserve_guards_authority_and_linear_work() {
        let mut db = HirAnalysisTestDb::default();
        let flags = (0..32)
            .map(|index| format!("_ flag{index}: bool"))
            .collect::<Vec<_>>()
            .join(", ");
        let file = db.new_stand_alone(
            "call_memory_unions.fe".into(),
            &format!(
                "fn native(_ value: mut u256, _ index: u256, {flags}) {{}}\nfn raw(_ value: *u256, _ index: u256, {flags}) {{}}"
            ),
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let scope = BinderScope::default();
        let ty = TyId::u256(&db);
        for (count, name, invalidated, conditional) in [64, 128, 256]
            .into_iter()
            .flat_map(|count| {
                [("native", false), ("native", true), ("raw", false)]
                    .map(|(name, invalidated)| (count, name, invalidated))
            })
            .flat_map(|(count, name, invalidated)| {
                [false, true].map(move |conditional| (count, name, invalidated, conditional))
            })
        {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let mut summary = Borrowck::new(&db, instance)
                .unwrap()
                .borrow_summary()
                .unwrap()
                .summary
                .unwrap();
            let mut checker = Borrowck::new(&db, instance).unwrap();
            let params: BTreeMap<_, _> = checker
                .body
                .values
                .iter()
                .enumerate()
                .filter_map(|(index, value)| {
                    if let NValueDefinition::EntryParam { param } = value.definition {
                        Some((param, NValueId::new(index)))
                    } else {
                        None
                    }
                })
                .collect();
            let args: Vec<_> = params
                .values()
                .copied()
                .map(|value| NOperand {
                    value,
                    origin: None,
                    mode: ReadMode::Read,
                })
                .collect();
            let result = args[0].value;
            let mut state = checker.inventory.entry.clone();
            if invalidated {
                // A weak byte overwrite retains the valid alternative and
                // adds invalidated native provenance without loan authority.
                let original = state.value(result).clone();
                let region = checker.resolve_capability(&original).region;
                let damaged = checker.inventory.values.from_shape(
                    original.shape(),
                    original.scope(),
                    |semantics, _, scope| {
                        vec![Guarded {
                            guard: Guard::always(scope),
                            payload: CapabilityRef::Invalidated {
                                class: semantics.class,
                                region: region.clone(),
                            },
                        }]
                    },
                );
                let mixed = checker.inventory.values.join(&original, &damaged);
                state.set_value(result, mixed);
            }
            let caller_guard = if conditional {
                params
                    .range(2..)
                    .try_fold(Guard::always(&scope), |guard, (_, value)| {
                        guard.with_boolean(
                            ChoiceKey::new(
                                ValueOccurrence::Value(*value),
                                StructuralPath::default(),
                            ),
                            true,
                        )
                    })
                    .unwrap()
            } else {
                Guard::always(&scope)
            };
            let guarded = checker
                .inventory
                .values
                .with_guard(state.value(result), &caller_guard);
            state.set_value(result, guarded);
            let origin = SemOrigin::Body(checker.body.template_owner);
            let inputs = CallInputs {
                args: &args,
                effects: &[],
                origin,
            };
            let base = SourceExpr {
                invalidated: false,
                source: ExternalSource::input(
                    InputSource::slot(0, StructuralPath::default()),
                    ReferentContract::new(
                        &db,
                        ty,
                        HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                    ),
                    false,
                ),
                path: RegionPath::default(),
                views: Default::default(),
            };
            let region = RegionSet::new(
                &scope,
                (0..count).map(|index| Guarded {
                    guard: Guard::always(&scope)
                        .with_equality(IndexExpr::FormalValue(1), IndexExpr::Const(index % 2))
                        .unwrap(),
                    payload: SymbolicPlace {
                        root: RegionRoot::External(ExternalSource::memory(
                            &db,
                            base.clone(),
                            ty,
                            MemoryOffset::Element(ty, IndexExpr::Const(index)),
                        )),
                        path: RegionPath::default(),
                        views: Default::default(),
                    },
                }),
            );
            assert_eq!(region.clauses().len(), count);
            let base = checker
                .instantiate_source(&state, &base, result, &scope, inputs)
                .unwrap();
            assert_eq!(base.invalidated.invalid, invalidated);
            let expected_regions = (0..count).map(|index| {
                let guard = Guard::always(&scope)
                    .with_equality(checker.index(args[1].value), IndexExpr::Const(index % 2))
                    .unwrap();
                checker
                    .memory_region(
                        &base.region,
                        ty,
                        MemoryOffset::Element(ty, IndexExpr::Const(index)),
                        origin,
                    )
                    .unwrap()
                    .with_guard(&guard)
            });
            let mut expected_authority = Vec::new();
            for clause in region.clauses() {
                let guard = checker
                    .instantiate_guard(&clause.guard, result, inputs)
                    .unwrap()
                    .unwrap();
                expected_authority.extend(base.traversed.iter().chain(&base.parents).map(
                    |parent| Guarded {
                        guard: parent.guard.and(&guard).unwrap(),
                        payload: parent.payload.clone(),
                    },
                ));
            }
            let expected_region = RegionSet::union_all(&scope, expected_regions);
            assert_eq!(expected_region.clauses().len(), count);
            assert_eq!(expected_authority.is_empty(), name == "raw");
            expected_authority.extend(expected_authority.clone());
            summary.accesses = vec![MemoryAccess {
                kind: MemoryAccessKind::Write,
                extent: AccessExtent::Bytes(IndexExpr::FormalValue(1)),
                region: region.clone(),
                authorizers: region.clone(),
            }];
            summary.availability = AvailabilitySummary {
                incoming: vec![
                    AvailabilityRequirement {
                        kind: MemoryAccessKind::Write,
                        extent: AccessExtent::Bytes(IndexExpr::FormalValue(1)),
                        region: region.clone(),
                    },
                    AvailabilityRequirement {
                        kind: MemoryAccessKind::Read,
                        extent: AccessExtent::Typed,
                        region: region.clone(),
                    },
                ],
                reinitialized: region.clone(),
                unavailable: region,
            };
            checker.calls.insert(
                result,
                CallSummary {
                    instance,
                    summary,
                    provenance: Vec::new(),
                    pending: false,
                    updates: Vec::new(),
                    births: Vec::new(),
                    single_result_port: false,
                },
            );
            let before = CANONICALIZED_REGION_CLAUSES.get();
            let guards_before = GUARD_INSTANTIATIONS.get();
            let decisions_before = DECISION_INTERN_ATTEMPTS.get();
            let resolved = checker
                .call_memory_accesses(&state, result, inputs)
                .unwrap();
            let visits = CANONICALIZED_REGION_CLAUSES.get() - before;
            let memory_decisions = DECISION_INTERN_ATTEMPTS.get() - decisions_before;
            assert_eq!(resolved.len(), 1);
            let resolved = &resolved[0];
            assert_eq!(resolved.access.region, expected_region);
            assert_eq!(resolved.access.authorizers, expected_region);
            assert_eq!(resolved.access.kind, MemoryAccessKind::Write);
            assert_eq!(
                resolved.access.extent,
                AccessExtent::Bytes(checker.index(args[1].value))
            );
            assert_eq!(resolved.authority, expected_authority);
            assert_eq!(resolved.invalidated.invalid, invalidated);
            assert_eq!(
                resolved.invalidated.requirements,
                base.invalidated.requirements
            );
            assert!(
                visits <= count * 64,
                "{name}, invalidated={invalidated}: processed {visits} clauses for {count} alternatives"
            );
            assert_eq!(
                GUARD_INSTANTIATIONS.get() - guards_before,
                2,
                "each distinct call guard should be instantiated once"
            );

            let before = CANONICALIZED_REGION_CLAUSES.get();
            let guards_before = GUARD_INSTANTIATIONS.get();
            let sources_before = SOURCE_INSTANTIATIONS.get();
            let decisions_before = DECISION_INTERN_ATTEMPTS.get();
            let availability = checker
                .call_availability(&state, result, inputs)
                .unwrap()
                .unwrap();
            let visits = CANONICALIZED_REGION_CLAUSES.get() - before;
            let guards = GUARD_INSTANTIATIONS.get() - guards_before;
            let sources = SOURCE_INSTANTIATIONS.get() - sources_before;
            let availability_decisions = DECISION_INTERN_ATTEMPTS.get() - decisions_before;
            assert_eq!(availability.incoming.len(), 2);
            for (requirement, validity) in &availability.incoming {
                assert_eq!(requirement.region, expected_region);
                assert_eq!(validity.invalid, invalidated);
                assert_eq!(validity.requirements, base.invalidated.requirements);
            }
            assert_eq!(availability.incoming[0].0.kind, MemoryAccessKind::Write);
            assert_eq!(
                availability.incoming[0].0.extent,
                AccessExtent::Bytes(checker.index(args[1].value))
            );
            assert_eq!(availability.incoming[1].0.kind, MemoryAccessKind::Read);
            assert_eq!(availability.incoming[1].0.extent, AccessExtent::Typed);
            assert_eq!(availability.reinitialized, expected_region);
            assert_eq!(availability.unavailable, expected_region);
            assert!(
                availability.consumed.is_empty(),
                "scalar memory has no capabilities to retire"
            );
            assert!(
                visits <= count * 64,
                "{name}, invalidated={invalidated}: availability processed {visits} clauses, {guards} guards, {sources} sources for {count} alternatives"
            );
            assert_eq!(guards, 2, "reuse each call guard across availability roles");
            assert_eq!(
                sources, count,
                "reuse the shared source prefix and repeated targets"
            );
            let budget = count * 32 + caller_guard.node_count() * 32;
            assert!(
                memory_decisions <= budget,
                "{name}, invalidated={invalidated}, conditional={conditional}: memory effects interned {memory_decisions} decision nodes for {count} alternatives (budget {budget})"
            );
            assert!(
                availability_decisions <= budget,
                "{name}, invalidated={invalidated}, conditional={conditional}: availability interned {availability_decisions} decision nodes for {count} alternatives (budget {budget})"
            );
        }
    }

    #[test]
    fn call_availability_preserves_targets_validity_consumption_and_call_isolation() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "call_availability.fe".into(),
            "struct Holder { value: mut u256 }\nfn inspect(_ left: mut Holder, _ right: mut Holder, _ selector: u256) -> u256 { 0 }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "inspect"))),
        );
        let summary = Borrowck::new(&db, instance)
            .unwrap()
            .borrow_summary()
            .unwrap()
            .summary
            .unwrap();
        let scope = BinderScope::default();
        let field = StructuralPath::new([Projection::Field(FieldIndex(0))]);
        for (damaged, ambiguous) in [(false, false), (false, true), (true, false), (true, true)] {
            let mut checker = Borrowck::new(&db, instance).unwrap();
            let params: BTreeMap<_, _> = checker
                .body
                .values
                .iter()
                .enumerate()
                .filter_map(|(index, value)| {
                    if let NValueDefinition::EntryParam { param } = value.definition {
                        Some((param, NValueId::new(index)))
                    } else {
                        None
                    }
                })
                .collect();
            let result = params[&0];
            let zero = checker
                .body
                .values
                .iter()
                .enumerate()
                .find_map(|(index, _)| {
                    let value = NValueId::new(index);
                    (checker.index(value) == IndexExpr::Const(0)).then_some(value)
                })
                .unwrap();
            let entry = checker.inventory.entry.clone();
            let holder_ty = entry.value(result).shape().direct(&db).unwrap().target_ty;
            let holder_shape = checker.shape(holder_ty).unwrap();
            let mut storage: Vec<_> = entry
                .storage()
                .map(|(root, value)| (root.clone(), value.clone()))
                .collect();
            for (_, value) in &mut storage {
                if damaged && value.shape() == holder_shape {
                    let held = checker
                        .inventory
                        .values
                        .project(value, &field, ValueOccurrence::Value(result))
                        .unwrap();
                    let region = checker.resolve_capability(&held).region;
                    assert!(!region.is_empty());
                    *value = checker.inventory.values.from_shape(
                        holder_shape,
                        value.scope(),
                        |semantics, _, scope| {
                            vec![Guarded {
                                guard: Guard::always(scope),
                                payload: CapabilityRef::Invalidated {
                                    class: semantics.class,
                                    region: region.clone(),
                                },
                            }]
                        },
                    );
                }
            }
            let mut state = BorrowState::new(
                &mut checker.inventory.values,
                entry.holders().map(|(id, value)| (id, value.shape())),
                storage,
            );
            for (id, value) in entry.holders() {
                state.set_value(id, value.clone());
            }
            if ambiguous {
                let alternatives = checker
                    .inventory
                    .values
                    .join(state.value(result), state.value(params[&1]));
                state.set_value(result, alternatives);
            }
            let origin = SemOrigin::Body(checker.body.template_owner);
            let source = SourceExpr {
                invalidated: false,
                source: ExternalSource::input(
                    InputSource::slot(0, StructuralPath::default()),
                    ReferentContract::memory(&db, holder_ty),
                    false,
                ),
                path: RegionPath::new([Projection::Field(FieldIndex(0))]),
                views: Default::default(),
            };
            let region = source_region(&source, &Guard::always(&scope));
            let mut summary = summary.clone();
            summary.availability = AvailabilitySummary {
                incoming: vec![
                    AvailabilityRequirement {
                        kind: MemoryAccessKind::Write,
                        extent: AccessExtent::Typed,
                        region: region.clone(),
                    },
                    AvailabilityRequirement {
                        kind: MemoryAccessKind::Read,
                        extent: AccessExtent::Typed,
                        region: region.clone(),
                    },
                ],
                reinitialized: region.clone(),
                unavailable: region,
            };
            checker.calls.insert(
                result,
                CallSummary {
                    instance,
                    summary,
                    provenance: Vec::new(),
                    pending: false,
                    updates: Vec::new(),
                    births: Vec::new(),
                    single_result_port: false,
                },
            );
            let shape = checker
                .shape(source.referent_ty(&db, instance).unwrap())
                .unwrap();
            assert!(shape.contains_capability(&db));
            // A second call mapping and a moved snapshot must not reuse the first call's facts.
            for actuals in [
                [params[&0], params[&1], params[&2]],
                [params[&1], params[&0], params[&2]],
            ] {
                let args = actuals.map(|value| NOperand {
                    value,
                    origin: None,
                    mode: ReadMode::Read,
                });
                let inputs = CallInputs {
                    args: &args,
                    effects: &[],
                    origin,
                };
                let target = checker
                    .instantiate_source(&state, &source, result, &scope, inputs)
                    .unwrap();
                let expected = target.region.close_existentials(&scope);
                assert_eq!(
                    expected.clauses().len(),
                    if ambiguous && actuals[0] == params[&0] {
                        2
                    } else {
                        1
                    }
                );
                assert!(
                    !target.invalidated.invalid,
                    "the outer references are valid"
                );
                let contents = checker
                    .read_region(
                        &state,
                        &target.region,
                        shape,
                        ValueOccurrence::Value(result),
                        origin,
                    )
                    .unwrap();
                let validity = checker.value_validity(&contents, ValueOccurrence::Value(result));
                assert_eq!(validity.invalid, damaged);
                let resolved = checker
                    .call_availability(&state, result, inputs)
                    .unwrap()
                    .unwrap();
                assert_eq!(resolved.incoming.len(), 2);
                assert_eq!(resolved.incoming[0].0.region, expected);
                assert!(
                    !resolved.incoming[0].1.invalid,
                    "writing does not require valid contents"
                );
                assert!(resolved.incoming[0].1.requirements.is_empty());
                assert_eq!(resolved.incoming[1].0.region, expected);
                assert_eq!(resolved.incoming[1].1.invalid, validity.invalid);
                assert_eq!(resolved.incoming[1].1.requirements, validity.requirements);
                assert_eq!(
                    resolved.reinitialized,
                    if expected.definite_write().is_some() {
                        expected.clone()
                    } else {
                        RegionSet::empty(&scope)
                    }
                );
                assert_eq!(resolved.unavailable, expected);
                assert_eq!(resolved.consumed, [(expected, shape)]);
            }
            let args = [params[&0], params[&1], zero].map(|value| NOperand {
                value,
                origin: None,
                mode: ReadMode::Read,
            });
            let inputs = CallInputs {
                args: &args,
                effects: &[],
                origin,
            };
            let mut moved = state.clone();
            let empty = checker
                .inventory
                .values
                .empty(state.value(result).shape(), &scope);
            moved.set_value(result, empty);
            let resolved = checker
                .call_availability(&moved, result, inputs)
                .unwrap()
                .unwrap();
            assert!(resolved.incoming.iter().all(|(requirement, validity)| {
                requirement.region.is_empty()
                    && !validity.invalid
                    && validity.requirements.is_empty()
            }));
            assert!(resolved.reinitialized.is_empty());
            assert!(resolved.unavailable.is_empty());
            assert!(resolved.consumed.is_empty());

            let impossible = Guard::always(&scope)
                .with_equality(IndexExpr::FormalValue(2), IndexExpr::Const(1))
                .unwrap();
            let region = source_region(&source, &impossible);
            let availability = &mut checker.calls.get_mut(&result).unwrap().summary.availability;
            for requirement in &mut availability.incoming {
                requirement.region = region.clone();
            }
            availability.reinitialized = region.clone();
            availability.unavailable = region;
            let guards_before = GUARD_INSTANTIATIONS.get();
            let sources_before = SOURCE_INSTANTIATIONS.get();
            let resolved = checker
                .call_availability(&state, result, inputs)
                .unwrap()
                .unwrap();
            assert!(
                resolved
                    .incoming
                    .iter()
                    .all(|(requirement, _)| requirement.region.is_empty())
            );
            assert!(resolved.reinitialized.is_empty());
            assert!(resolved.unavailable.is_empty());
            assert!(resolved.consumed.is_empty());
            assert_eq!(GUARD_INSTANTIATIONS.get() - guards_before, 1);
            assert_eq!(SOURCE_INSTANTIATIONS.get() - sources_before, 0);

            let local = Guard::always(&scope)
                .with_boolean(
                    ChoiceKey::new(ValueOccurrence::Value(result), StructuralPath::default()),
                    true,
                )
                .unwrap();
            checker
                .calls
                .get_mut(&result)
                .unwrap()
                .summary
                .availability
                .incoming[0]
                .region = source_region(&source, &local);
            let guards_before = GUARD_INSTANTIATIONS.get();
            for _ in 0..2 {
                assert!(
                    checker.call_availability(&state, result, inputs).is_err(),
                    "invalid local summary choices must be rejected"
                );
            }
            assert_eq!(GUARD_INSTANTIATIONS.get() - guards_before, 2);
        }
    }

    #[test]
    fn call_guard_reuse_preserves_scopes_mappings_and_errors() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "call_guard_cache.fe".into(),
            "fn inspect(_ first: u256, _ second: u256) -> u256 { 1 }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "inspect"))),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        let params: BTreeMap<_, _> = checker
            .body
            .values
            .iter()
            .enumerate()
            .filter_map(|(index, value)| {
                if let NValueDefinition::EntryParam { param } = value.definition {
                    Some((param, NValueId::new(index)))
                } else {
                    None
                }
            })
            .collect();
        let constant = checker
            .body
            .values
            .iter()
            .enumerate()
            .find_map(|(index, _)| {
                let value = NValueId::new(index);
                (checker.index(value) == IndexExpr::Const(1)).then_some(value)
            })
            .unwrap();
        let scope = BinderScope::default();
        let (nested, _) = scope.bind(IndexNamespace::Existential);
        let state = checker.inventory.entry.clone();
        for argument in [params[&0], constant] {
            let args = [NOperand {
                value: argument,
                origin: None,
                mode: ReadMode::Read,
            }];
            for effect in [params[&0], params[&1]] {
                let effects = [NEffectArg {
                    binding_idx: 0,
                    arg: NEffectArgValue::Value(NOperand {
                        value: effect,
                        origin: None,
                        mode: ReadMode::Read,
                    }),
                    pass_mode: EffectPassMode::ByValue,
                    layout_view: EffectArgLayoutView::Direct,
                    required_mut: false,
                    provider_target_ty: None,
                    provider: None,
                }];
                for result in [params[&0], params[&1]] {
                    let inputs = CallInputs {
                        args: &args,
                        effects: &effects,
                        origin: SemOrigin::Body(checker.body.template_owner),
                    };
                    let mut instantiations = SourceInstantiations::new(&state, result, inputs);
                    for scope in [&scope, &nested] {
                        let scalar = Guard::always(scope)
                            .with_equality(IndexExpr::FormalValue(0), IndexExpr::Const(0))
                            .unwrap();
                        let expected = Guard::always(scope)
                            .with_equality(checker.index(argument), IndexExpr::Const(0));
                        let before = GUARD_INSTANTIATIONS.get();
                        for _ in 0..4 {
                            assert_eq!(instantiations.guard(&checker, &scalar).unwrap(), expected);
                        }
                        assert_eq!(
                            GUARD_INSTANTIATIONS.get() - before,
                            1,
                            "cache feasible and infeasible guards separately in each scope"
                        );
                        for (formal, actual) in [
                            (
                                ValueOccurrence::Argument(0),
                                ValueOccurrence::Value(argument),
                            ),
                            (ValueOccurrence::Argument(1), ValueOccurrence::Value(effect)),
                            (
                                ValueOccurrence::Argument(9),
                                ValueOccurrence::CallChoice { result, choice: 9 },
                            ),
                            (ValueOccurrence::Summary, ValueOccurrence::Value(result)),
                            (
                                ValueOccurrence::SummaryChoice(7),
                                ValueOccurrence::CallChoice { result, choice: 7 },
                            ),
                        ] {
                            let guard = Guard::always(scope)
                                .with_variant(
                                    ChoiceKey::new(
                                        formal,
                                        StructuralPath::new([Projection::Index(
                                            IndexExpr::FormalValue(0),
                                        )]),
                                    ),
                                    VariantIndex(1),
                                )
                                .unwrap();
                            let expected = Guard::always(scope).with_variant(
                                ChoiceKey::new(
                                    actual,
                                    StructuralPath::new([Projection::Index(
                                        checker.index(argument),
                                    )]),
                                ),
                                VariantIndex(1),
                            );
                            let before = GUARD_INSTANTIATIONS.get();
                            assert_eq!(instantiations.guard(&checker, &guard).unwrap(), expected);
                            checker.source_generation += 1;
                            assert_eq!(instantiations.guard(&checker, &guard).unwrap(), expected);
                            assert_eq!(
                                GUARD_INSTANTIATIONS.get() - before,
                                1,
                                "inventory changes do not change pure guard substitution"
                            );
                        }
                        for occurrence in [
                            ValueOccurrence::Value(argument),
                            ValueOccurrence::Root(NRootId::from_u32(0)),
                            ValueOccurrence::CallChoice { result, choice: 0 },
                        ] {
                            let guard = Guard::always(scope)
                                .with_variant(
                                    ChoiceKey::new(occurrence, StructuralPath::default()),
                                    VariantIndex(0),
                                )
                                .unwrap();
                            let before = GUARD_INSTANTIATIONS.get();
                            assert!(instantiations.guard(&checker, &guard).is_err());
                            assert!(instantiations.guard(&checker, &guard).is_err());
                            assert_eq!(
                                GUARD_INSTANTIATIONS.get() - before,
                                2,
                                "invalid local occurrences must still be rejected, without caching errors"
                            );
                            assert!(!instantiations.guards.contains_key(&guard));
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn call_source_reuse_tracks_state_scopes_and_inventory_growth() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "call_source_cache.fe".into(),
            "fn native(_ value: mut u256) {}\nfn raw(_ value: *u256) {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        for name in ["native", "raw"] {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let mut checker = Borrowck::new(&db, instance).unwrap();
            let value = checker
                .body
                .values
                .iter()
                .enumerate()
                .find_map(|(index, value)| {
                    matches!(value.definition, NValueDefinition::EntryParam { param: 0 })
                        .then(|| NValueId::new(index))
                })
                .unwrap();
            let state = checker.inventory.entry.clone();
            let scope = BinderScope::default();
            let origin = SemOrigin::Body(checker.body.template_owner);
            let args = [NOperand {
                value,
                origin: None,
                mode: ReadMode::Read,
            }];
            let inputs = CallInputs {
                args: &args,
                effects: &[],
                origin,
            };
            let contract = ReferentContract::new(
                &db,
                TyId::u256(&db),
                HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            );
            let source = SourceExpr {
                invalidated: false,
                source: ExternalSource::input(
                    InputSource::slot(0, StructuralPath::default()),
                    contract,
                    false,
                ),
                path: RegionPath::default(),
                views: Default::default(),
            };
            let mut sources = SourceInstantiations::new(&state, value, inputs);
            let initial = sources.resolve(&mut checker, &source, &scope).unwrap();
            assert!(!initial.region.is_empty());
            let evaluations = sources.evaluations;
            let reused = sources.resolve(&mut checker, &source, &scope).unwrap();
            assert_eq!(
                sources.evaluations, evaluations,
                "identical source was evaluated twice"
            );
            assert_eq!(reused.region, initial.region);
            assert_eq!(reused.parents, initial.parents);
            assert_eq!(reused.traversed, initial.traversed);

            let (nested, _) = scope.bind(IndexNamespace::Existential);
            let lifted = sources.resolve(&mut checker, &source, &nested).unwrap();
            assert_eq!(lifted.region.scope(), &nested);
            assert!(
                sources.evaluations > evaluations,
                "different scopes shared a resolution"
            );
            let evaluations = sources.evaluations;
            sources.resolve(&mut checker, &source, &scope).unwrap();
            assert_eq!(sources.evaluations, evaluations);

            if name == "native" {
                let reference = state.value(value).direct()[0]
                    .payload
                    .loan()
                    .unwrap()
                    .clone();
                let extra = RegionSet::singleton(
                    &scope,
                    test_roots::local(&db, NRootId::from_u32(0)),
                    RegionPath::default(),
                );
                checker.extend_loan(value, &reference, &extra, Vec::new());
                let changed = sources.resolve(&mut checker, &source, &scope).unwrap();
                assert!(
                    sources.evaluations > evaluations,
                    "loan growth retained a stale resolution"
                );
                assert_eq!(changed.region, initial.region.union(&extra));
                let evaluations = sources.evaluations;
                checker.extend_loan(value, &reference, &extra, Vec::new());
                sources.resolve(&mut checker, &source, &scope).unwrap();
                assert_eq!(
                    sources.evaluations, evaluations,
                    "unchanged loan facts invalidated reuse"
                );
            } else {
                // Following a newly discovered pointer cell changes the storage
                // inventory during resolution. Cache only a subsequent stable read.
                let memory = SourceExpr {
                    source: ExternalSource::memory(
                        &db,
                        source.clone(),
                        TyId::ptr_to(&db, contract.ty),
                        MemoryOffset::Zero,
                    )
                    .follow(RegionPath::default(), contract, false),
                    ..source.clone()
                };
                let generation = checker.source_generation;
                sources.resolve(&mut checker, &memory, &scope).unwrap();
                assert!(checker.source_generation > generation);
                assert!(
                    sources
                        .resolved
                        .get(&scope)
                        .is_none_or(|entries| !entries.contains_key(&memory))
                );
                let evaluations = sources.evaluations;
                let discovered = sources.resolve(&mut checker, &memory, &scope).unwrap();
                assert!(sources.evaluations > evaluations);
                let evaluations = sources.evaluations;
                assert_eq!(
                    sources
                        .resolve(&mut checker, &memory, &scope)
                        .unwrap()
                        .region,
                    discovered.region
                );
                assert_eq!(sources.evaluations, evaluations);
            }

            // A new pre-call state always starts a new cache, even with the same
            // source expression, arguments, and unchanged inventory generation.
            let mut moved = state.clone();
            let empty = checker
                .inventory
                .values
                .empty(state.value(value).shape(), &scope);
            moved.set_value(value, empty);
            let resolved = SourceInstantiations::new(&moved, value, inputs)
                .resolve(&mut checker, &source, &scope)
                .unwrap();
            assert!(resolved.region.is_empty());
        }
    }

    #[test]
    fn summary_choice_order_stays_compact_across_poststates_and_accesses() {
        for arms in [4, 8, 16] {
            let branches = (0..arms)
                .map(|index| format!("if op == {index} {{ frame.memory.set(value: {index}) }}"))
                .collect::<Vec<_>>()
                .join(" else ");
            let source = format!(
                r#"
use core::Option
struct Buffer {{ allocation: Option<*u256> }}
impl Buffer {{
    fn data(self) -> *u256 {{
        match self.allocation {{
            Option::Some(address) => address
            Option::None => core::panic()
        }}
    }}
    fn set(mut self, value: u256) {{ *self.data() = value }}
}}
struct Frame {{ memory: Buffer, output: Buffer }}
fn dispatch(frame: mut Frame, op: u256) {{ {branches} }}
fn caller(frame: mut Frame, op: u256) {{ dispatch(frame, op) }}
"#
            );
            let mut db = HirAnalysisTestDb::default();
            let file = db.new_stand_alone("summary_choice_order.fe".into(), &source);
            let (module, _) = db.top_mod(file);
            db.assert_no_diags(module);
            for name in ["dispatch", "caller"] {
                let instance = get_or_build_semantic_instance(
                    &db,
                    identity_semantic_instance_key(
                        &db,
                        BodyOwner::Func(find_func(&db, module, name)),
                    ),
                );
                let mut checker = Borrowck::new(&db, instance).unwrap();
                let summary = checker.borrow_summary().unwrap().summary.unwrap();
                assert!(!summary.accesses.is_empty());
                let maximum = summary
                    .accesses
                    .iter()
                    .flat_map(|access| [&access.region, &access.authorizers])
                    .chain(
                        summary
                            .availability
                            .incoming
                            .iter()
                            .map(|requirement| &requirement.region),
                    )
                    .chain([
                        &summary.availability.reinitialized,
                        &summary.availability.unavailable,
                    ])
                    .flat_map(|region| region.clauses())
                    .map(|clause| clause.guard.node_count())
                    .max()
                    .unwrap();
                assert!(
                    maximum <= arms * 128,
                    "{name} with {arms} arms: {maximum} nodes"
                );
            }
        }
    }

    #[test]
    fn core_signature_summaries_respect_native_and_copied_transport() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "core_signatures.fe".into(),
            "fn anchor() {}\nfn bytes(value: u256) -> [u8; 32] { core::intrinsic::__as_bytes(value) }\nfn aggregate(value: [u256; 2]) -> [u8; 64] { core::intrinsic::__as_bytes(value) }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let scope = find_func(&db, module, "anchor").scope();
        let mut failures = Vec::new();
        let index_mut = resolve_core_trait(&db, scope, &["ops", "IndexMut"])
            .unwrap()
            .methods(&db)
            .next()
            .unwrap();
        let pointer_module = resolve_lib_func_path(&db, scope, "core::ptr::array_elem")
            .unwrap()
            .top_mod(&db);
        let pointer_methods: Vec<_> = pointer_module
            .all_funcs(&db)
            .iter()
            .copied()
            .filter_map(|func| {
                func.name(&db)
                    .to_opt()
                    .is_some_and(|name| name.data(&db) == "index_mut")
                    .then_some(("core::ptr::index_mut", func))
            })
            .collect();
        assert!(!pointer_methods.is_empty());
        for (path, func) in [
            "core::intrinsic::__as_bytes",
            "core::intrinsic::__keccak256",
        ]
        .into_iter()
        .map(|path| (path, resolve_lib_func_path(&db, scope, path).expect(path)))
        .chain([
            ("core::ops::IndexMut::index_mut", index_mut),
            ("bytes", find_func(&db, module, "bytes")),
            ("aggregate", find_func(&db, module, "aggregate")),
        ])
        .chain(pointer_methods)
        {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(func)),
            );
            let mut checker = Borrowck::new(&db, instance).unwrap();
            match checker.borrow_summary() {
                Ok(result) => {
                    let summary = result.summary.unwrap();
                    if path == "bytes" {
                        assert!(summary.accesses.is_empty());
                    } else if path == "aggregate" {
                        assert!(summary.accesses.iter().any(|access| {
                            access.kind == MemoryAccessKind::Read && !access.region.is_empty()
                        }));
                    }
                }
                Err(error) => failures.push((path, error)),
            }
        }
        assert!(failures.is_empty(), "{failures:#?}");
    }

    #[test]
    fn signature_fallback_covers_checked_result_effect_and_poststate_contracts() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "signature_contract_matrix.fe".into(),
            r#"
use core::ptr
struct Item { n: u256 }
fn raw(pointer: *u256) -> mut u256 { mut *pointer }
fn cast_borrow<T>(pointer: *T) -> mut u256 { mut *ptr::cast<T, u256>(pointer) }
fn cast_shared<T>(pointer: *T) -> ref u256 { ref *ptr::cast<T, u256>(pointer) }
fn shared(value: ref u256) -> ref u256 { value }
fn exclusive(value: mut u256) -> mut u256 { value }
fn fresh() -> mut u256 {
    let pointer = ptr::alloc<u256>()
    *pointer = 1
    mut *pointer
}
fn unchanged(slot: *ref u256) {}
fn replaced(slot: *ref u256, value: ref u256) { *slot = value }
fn clobbered(slot: *ref u256) { ptr::zero_bytes(ptr::byte_ptr(slot), 32) }
fn take(pointer: *Item) -> Item { *pointer }
fn restore(pointer: *Item) -> Item {
    let value = *pointer
    *pointer = Item { n: 2 }
    value
}
fn empty() -> [ref u256; 0] { [] }
enum Maybe { None, Some(ref u256) }
fn absent() -> Maybe { Maybe::None }
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let values = SourceValues::new(&db, ValueLimits::default());
        let mut normalization_failures = Vec::new();
        for name in [
            "raw",
            "cast_borrow",
            "cast_shared",
            "shared",
            "exclusive",
            "fresh",
            "unchanged",
            "replaced",
            "clobbered",
            "take",
            "restore",
            "empty",
            "absent",
        ] {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let mut checker = match Borrowck::new(&db, instance) {
                Ok(checker) => checker,
                Err(error) => {
                    normalization_failures.push((name, error));
                    continue;
                }
            };
            checker.solve().unwrap();
            let (concrete, _) = checker.build_summary().unwrap();
            let fallback = signature_summary(&db, instance, true).unwrap();
            assert_eq!(
                fallback,
                signature_summary(&db, instance, true).unwrap(),
                "unstable {name}"
            );
            let possible = values.leaves(&fallback.result, ValueOccurrence::Summary);
            for leaf in values.leaves(&concrete.result, ValueOccurrence::Summary) {
                assert!(
                    possible.iter().any(|candidate| candidate.path == leaf.path
                        && !candidate.payload.invalidated
                        && !matches!(
                            source_region(&candidate.payload, &candidate.guard)
                                .overlap(&db, &source_region(&leaf.payload, &leaf.guard)),
                            OverlapResult::Disjoint
                        )),
                    "lost result in {name}"
                );
            }
            for access in &concrete.accesses {
                assert!(
                    fallback.accesses.iter().any(|possible| {
                        (possible.kind == access.kind
                            || (access.kind == MemoryAccessKind::MutAccess
                                && possible.kind == MemoryAccessKind::Write))
                            && !matches!(
                                possible.footprint().overlap(&db, access.footprint()),
                                OverlapResult::Disjoint
                            )
                    }),
                    "lost {name} access {access:?}"
                );
            }
            for contents in &concrete.mutable_inputs {
                for leaf in values.leaves(&contents.value, ValueOccurrence::Summary) {
                    assert!(
                        fallback
                            .mutable_inputs
                            .iter()
                            .filter(|possible| possible.destination == contents.destination)
                            .any(|possible| {
                                values
                                    .leaves(&possible.value, ValueOccurrence::Summary)
                                    .iter()
                                    .any(|candidate| {
                                        candidate.path == leaf.path
                                            && candidate.payload.invalidated
                                                == leaf.payload.invalidated
                                            && !matches!(
                                                source_region(&candidate.payload, &candidate.guard)
                                                    .overlap(
                                                        &db,
                                                        &source_region(&leaf.payload, &leaf.guard)
                                                    ),
                                                OverlapResult::Disjoint
                                            )
                                    })
                            }),
                        "lost {name} poststate {leaf:?}"
                    );
                }
            }
            assert!(fallback.availability.reinitialized.is_empty());
            let mut bottom = fallback.clone();
            let mut interner = SourceValues::new(&db, ValueLimits::default());
            bottom.result = interner.empty(bottom.result.shape(), bottom.result.scope());
            if matches!(name, "raw" | "shared" | "exclusive" | "fresh") {
                assert!(checker.verify_summary(&bottom).is_err());
            }
            bottom.may_return = false;
            assert!(checker.verify_summary(&bottom).is_ok());
        }
        assert!(
            normalization_failures.is_empty(),
            "{normalization_failures:#?}"
        );
    }

    #[test]
    fn returning_native_summary_invariant_checks_required_enum_payloads() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "required_native_results.fe".into(),
            r#"
enum Required { First(ref u256), Second(mut u256) }
enum Optional { None, Some(ref u256) }
struct Wrapped { value: Required }
fn required(value: ref u256) -> Required { Required::First(value) }
fn wrapped(value: ref u256) -> Wrapped { Wrapped { value: Required::First(value) } }
fn array(value: ref u256) -> [Required; 1] { [Required::First(value)] }
fn optional() -> Optional { Optional::None }
fn empty() -> [Required; 0] { [] }
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        for (name, requires_native) in [
            ("required", true),
            ("wrapped", true),
            ("array", true),
            ("optional", false),
            ("empty", false),
        ] {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let checker = Borrowck::new(&db, instance).unwrap();
            let mut summary = signature_summary(&db, instance, true).unwrap();
            checker.verify_summary(&summary).unwrap();
            let mut values = SourceValues::new(&db, ValueLimits::default());
            summary.result = values.empty(summary.result.shape(), summary.result.scope());
            assert_eq!(
                checker.verify_summary(&summary).is_err(),
                requires_native,
                "{name}"
            );
            summary.may_return = false;
            checker.verify_summary(&summary).unwrap();
        }
    }

    #[test]
    fn signature_fallback_represents_native_results_and_corrupted_contents() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "opaque_native_contracts.fe".into(),
            r#"
use core::ptr
fn lend(pointer: *u256) -> mut u256 { mut *pointer }
fn clobber(slot: *ref u256) {
    let bytes = ptr::alloc_bytes(32)
    ptr::copy_raw(ptr::byte_ptr(slot), bytes, 32)
}
"#,
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let values = SourceValues::new(&db, ValueLimits::default());
        for name in ["lend", "clobber"] {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let mut checker = Borrowck::new(&db, instance).unwrap();
            checker.solve().unwrap();
            let (concrete, _) = checker.build_summary().unwrap();
            let fallback = signature_summary(&db, instance, true).unwrap();
            if name == "lend" {
                assert!(
                    !values
                        .leaves(&concrete.result, ValueOccurrence::Summary)
                        .is_empty()
                );
                assert!(
                    !values
                        .leaves(&fallback.result, ValueOccurrence::Summary)
                        .is_empty(),
                    "opaque native result vanished"
                );
            } else {
                // The interner is invariant over its database lifetime, so name it.
                fn invalid<'db>(values: &SourceValues<'db>, summary: &BorrowSummary<'db>) -> bool {
                    summary.mutable_inputs.iter().any(|poststate| {
                        values
                            .leaves(&poststate.value, ValueOccurrence::Summary)
                            .iter()
                            .any(|leaf| leaf.payload.invalidated)
                    })
                }
                assert!(invalid(&values, &concrete));
                assert!(
                    invalid(&values, &fallback),
                    "opaque native poststate restored entry validity"
                );
            }
        }
    }

    #[test]
    fn signature_fallback_preserves_both_mutation_and_consumption() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "opaque_ownership_effects.fe".into(),
            "struct Item { n: u256 }\nfn take(pointer: *Item) -> Item { *pointer }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "take"))),
        );
        let fallback = signature_summary(&db, instance, true).unwrap();
        for kind in [MemoryAccessKind::Move, MemoryAccessKind::Write] {
            assert!(
                fallback.accesses.iter().any(|access| access.kind == kind),
                "missing {kind:?}"
            );
        }
    }

    #[test]
    fn summary_verification_rejects_invalid_sources_and_authority() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "summary_verification.fe".into(),
            "fn forward(value: ref u256) -> ref u256 { value }",
        );
        let (top_mod, _) = db.top_mod(file);
        let func = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| {
                if let ItemKind::Func(func) = item {
                    Some(*func)
                } else {
                    None
                }
            })
            .unwrap();
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(func)),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        let (summary, _) = checker.build_summary().unwrap();
        let values = SourceValues::new(&db, ValueLimits::default());
        let leaf = values
            .leaves(&summary.result, ValueOccurrence::Summary)
            .pop()
            .unwrap();
        let check = |source| {
            checker
                .verify_source(source, leaf.guard.scope(), Some(leaf.semantics), false)
                .is_ok()
        };
        assert!(check(&leaf.payload));
        let mut source = leaf.payload.clone();
        source.source.origin =
            ExternalOrigin::Input(InputSource::slot(9, StructuralPath::default()));
        assert!(!check(&source));
        let mut source = leaf.payload.clone();
        source.path = RegionPath::new([Projection::Field(FieldIndex(0))]);
        assert!(!check(&source));
        let mut source = leaf.payload.clone();
        source.source.contract.ty = TyId::bool(&db);
        assert!(!check(&source));
        let mut source = leaf.payload.clone();
        source.source.contract.address_space =
            HandleAddressSpace::Known(ProviderAddressSpace::Storage);
        assert!(!check(&source));
        let mut requested = leaf.semantics;
        requested.class = CapabilityClass::Borrow(BorrowKind::Mut);
        assert!(
            checker
                .verify_source(&leaf.payload, leaf.guard.scope(), Some(requested), false)
                .is_err()
        );
        assert!(
            checker
                .verify_source(&leaf.payload, leaf.guard.scope(), None, true)
                .is_err()
        );
        let (_, unowned) = BinderScope::default().bind(IndexNamespace::Result);
        let mut source = leaf.payload.clone();
        source.path = RegionPath::new([Projection::Index(unowned)]);
        assert!(!check(&source));
    }

    /// The first call in a solved body: its position, result and arguments.
    fn first_call(checker: &Borrowck<'_>) -> ((usize, usize), NValueId, Vec<NOperand>) {
        checker
            .body
            .blocks
            .iter()
            .enumerate()
            .find_map(|(block, data)| {
                data.statements
                    .iter()
                    .enumerate()
                    .find_map(|(index, statement)| match &statement.kind {
                        NStatementKind::Define {
                            result,
                            expr: NExpr::Call { args, .. },
                        } => Some(((block, index), *result, args.to_vec())),
                        _ => None,
                    })
            })
            .unwrap()
    }

    fn summary_input<'db>(db: &'db dyn HirAnalysisDb, param: u32) -> SourceExpr<'db> {
        SourceExpr::whole(ExternalSource::input(
            InputSource::slot(param, StructuralPath::default()),
            ReferentContract::new(db, TyId::u256(db), HandleAddressSpace::Unspecified),
            false,
        ))
    }

    /// Arbitrary bytes at an unknown address, present only where `target` and
    /// `written` overlap.
    fn clobbered<'db>(
        db: &'db dyn HirAnalysisDb,
        target: SourceExpr<'db>,
        written: SourceExpr<'db>,
    ) -> SymbolicPlace<'db> {
        let mut source = ExternalSource::unknown(
            ReferentContract::new(
                db,
                TyId::u256(db),
                HandleAddressSpace::Known(ProviderAddressSpace::Memory),
            ),
            AddressOccurrence::Summary(0),
            Box::new([]),
            AddressProvenance::Raw,
        );
        source.clobber = Some(Box::new(ClobberCondition::new(
            target,
            written,
            AccessExtent::Typed,
        )));
        SymbolicPlace {
            root: RegionRoot::External(source),
            path: RegionPath::default(),
            views: Default::default(),
        }
    }

    /// The caller parameters each alternative's clobber condition compares.
    fn clobber_params(region: &RegionSet<'_>) -> BTreeSet<(Option<u32>, Option<u32>)> {
        region
            .clauses()
            .iter()
            .map(|clause| {
                let RegionRoot::External(source) = &clause.payload.root else {
                    panic!("clobbered alternative without a source");
                };
                let clobber = source.clobber.as_ref().expect("clobber condition");
                (
                    clobber.target.source.param(),
                    clobber.written.source.param(),
                )
            })
            .collect()
    }

    /// A value of `like`'s shape whose every leaf is `payload`.
    fn uniform<'db>(
        checker: &mut Borrowck<'db>,
        like: &CapabilityValue<'db>,
        payload: impl Fn(CapabilitySemantics<'db>) -> CapabilityRef<'db>,
    ) -> CapabilityValue<'db> {
        checker
            .inventory
            .values
            .from_shape(like.shape(), like.scope(), |semantics, _, scope| {
                vec![Guarded {
                    guard: Guard::always(scope),
                    payload: payload(semantics),
                }]
            })
    }

    #[test]
    fn physical_resolution_keeps_clobbers_between_distinct_certain_inputs() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "physical_clobbers.fe".into(),
            "fn leaf(_ a: ref u256, _ b: ref u256, _ c: *u256, _ flag: bool) {}\n\
             fn wrapper(_ first: ref u256, _ second: ref u256, _ third: *u256, _ flag: bool) {\n    \
                 leaf(first, second, third, flag)\n}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "wrapper"))),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        let ((block, index), result, args) = first_call(&checker);
        let state = checker.before[block][index].clone();
        let scope = BinderScope::default();
        let inputs = CallInputs {
            args: &args,
            effects: &[],
            origin: SemOrigin::Body(checker.body.template_owner),
        };
        let input = |param| summary_input(&db, param);
        let source = |place| SourceExpr::from_place(&place).unwrap();
        let referents: Vec<_> = args
            .iter()
            .map(|arg| checker.resolve_capability(state.value(arg.value)).region)
            .collect();

        // Distinct certain inputs are separate only by the entry assumption:
        // its first filter drops the alternative, the physical one keeps it.
        let between = source(clobbered(&db, input(0), input(1)));
        let mut assumed = SourceInstantiations::new(&state, result, inputs);
        let mut physical = SourceInstantiations::physical(&state, result, inputs);
        assert!(
            assumed
                .resolve(&mut checker, &between, &scope)
                .unwrap()
                .region
                .is_empty()
        );
        let kept = physical.resolve(&mut checker, &between, &scope).unwrap();
        assert_eq!(clobber_params(&kept.region), [(Some(0), Some(1))].into());
        // Each session reuses only its own resolutions.
        let evaluations = physical.evaluations;
        assert!(
            assumed
                .resolve(&mut checker, &between, &scope)
                .unwrap()
                .region
                .is_empty()
        );
        assert_eq!(
            physical
                .resolve(&mut checker, &between, &scope)
                .unwrap()
                .region,
            kept.region
        );
        assert_eq!(physical.evaluations, evaluations);

        // A pointer that may also hold the first input's address. The
        // per-alternative filter keeps only its uncertain target under the
        // assumption, and both physically.
        let first = &referents[0];
        let pointer = state.value(args[2].value).clone();
        let alias = uniform(&mut checker, &pointer, |_| {
            CapabilityRef::Address(first.clone())
        });
        let mut mixed = state.clone();
        mixed.set_value(
            args[2].value,
            checker.inventory.values.join(&pointer, &alias),
        );
        let either = source(clobbered(&db, input(2), input(1)));
        let assumed = SourceInstantiations::new(&mixed, result, inputs)
            .resolve(&mut checker, &either, &scope)
            .unwrap();
        assert_eq!(clobber_params(&assumed.region), [(Some(2), Some(1))].into());
        let kept = SourceInstantiations::physical(&mixed, result, inputs)
            .resolve(&mut checker, &either, &scope)
            .unwrap();
        assert_eq!(
            clobber_params(&kept.region),
            [(Some(0), Some(1)), (Some(2), Some(1))].into()
        );

        // A dependency resolved through memory keeps its session's basis.
        let mut redirected = state.clone();
        redirected.set_value(args[2].value, alias.clone());
        let element = SourceExpr::whole(ExternalSource::memory(
            &db,
            input(2),
            TyId::u256(&db),
            MemoryOffset::Element(TyId::u256(&db), IndexExpr::Const(1)),
        ));
        let through = source(clobbered(&db, element, input(1)));
        assert!(
            SourceInstantiations::new(&redirected, result, inputs)
                .resolve(&mut checker, &through, &scope)
                .unwrap()
                .region
                .is_empty()
        );
        assert_eq!(
            clobber_params(
                &SourceInstantiations::physical(&redirected, result, inputs)
                    .resolve(&mut checker, &through, &scope)
                    .unwrap()
                    .region
            ),
            [(Some(0), Some(1))].into()
        );

        // A clobber whose dependencies resolve to 17 places each exceeds the
        // pair limit before any comparison; the assumed basis compares no
        // pairs and needs no limit.
        let pointee = SourceExpr::from_place(&referents[2].clauses()[0].payload).unwrap();
        let cells = (0..17).fold(pointer.clone(), |value, cell| {
            let region = RegionSet::singleton(
                &scope,
                RegionRoot::External(ExternalSource::memory(
                    &db,
                    pointee.clone(),
                    TyId::u256(&db),
                    MemoryOffset::Element(TyId::u256(&db), IndexExpr::Const(cell + 1)),
                )),
                RegionPath::default(),
            );
            let cell = uniform(&mut checker, &pointer, |_| {
                CapabilityRef::Address(region.clone())
            });
            checker.inventory.values.join(&value, &cell)
        });
        let mut wide = state.clone();
        wide.set_value(args[2].value, cells);
        let product = source(clobbered(&db, input(2), input(2)));
        assert!(
            SourceInstantiations::new(&wide, result, inputs)
                .resolve(&mut checker, &product, &scope)
                .is_ok()
        );
        let exhausted = SourceInstantiations::physical(&wide, result, inputs)
            .resolve(&mut checker, &product, &scope)
            .unwrap_err();
        assert_eq!(
            exhausted.primary.message,
            "borrow separation requirements exceed the analysis limits"
        );

        // A requirement holds only where the call executes, even where the
        // resolved endpoints do not carry that condition themselves.
        let flag = |occurrence| ChoiceKey::new(occurrence, StructuralPath::default());
        let relation = Separation {
            protected: SymbolicPlace {
                root: RegionRoot::External(input(0).source),
                path: RegionPath::default(),
                views: Default::default(),
            },
            protected_kind: BorrowKind::Ref,
            access: SymbolicPlace {
                root: RegionRoot::External(input(1).source),
                path: RegionPath::default(),
                views: Default::default(),
            },
            access_kind: BorrowKind::Mut,
            extent: AccessExtent::Typed,
            suspended: Box::new([]),
        };
        let touched = Guard::always(&scope)
            .with_boolean(flag(ValueOccurrence::Argument(3)), true)
            .unwrap();
        checker
            .calls
            .get_mut(&result)
            .unwrap()
            .summary
            .loan_requirements = SeparationSet::new(
            &db,
            &scope,
            [Guarded {
                guard: touched.clone(),
                payload: relation,
            }],
        );
        // Execute the call only where the requirement cannot hold, leaving
        // the argument values unconstrained.
        let instantiated = checker
            .instantiate_guard(&touched, result, inputs)
            .unwrap()
            .unwrap();
        let mut skipped = state.clone();
        assert!(skipped.constrain(
            &Guard::always(&scope).difference(&instantiated).unwrap(),
            &mut checker.inventory.values,
        ));
        for arg in &args {
            skipped.set_value(arg.value, state.value(arg.value).clone());
        }
        let (requirements, _) = checker
            .call_loan_requirements(&state, result, inputs)
            .unwrap();
        assert_eq!(requirements.len(), 1);
        let (requirements, _) = checker
            .call_loan_requirements(&skipped, result, inputs)
            .unwrap();
        assert!(requirements.is_empty());
        checker
            .calls
            .get_mut(&result)
            .unwrap()
            .summary
            .loan_requirements = SeparationSet::empty(&scope);

        // Dependencies keep their native validity, immediate or deferred,
        // where nothing valid remains to compare.
        let deferred = source(clobbered(
            &db,
            SourceExpr::from_place(&first.clauses()[0].payload).unwrap(),
            SourceExpr::from_place(&referents[2].clauses()[0].payload).unwrap(),
        ));
        for (param, immediate) in [(0u32, true), (1, true), (0, false), (1, false)] {
            let arg = args[param as usize].value;
            let region = if immediate {
                referents[param as usize].clone()
            } else {
                source_region(&deferred, &Guard::always(&scope))
            };
            let original = state.value(arg).clone();
            let invalid = uniform(&mut checker, &original, |semantics| {
                CapabilityRef::Invalidated {
                    class: semantics.class,
                    region: region.clone(),
                }
            });
            let mut damaged = state.clone();
            damaged.set_value(arg, invalid);
            let resolved = SourceInstantiations::physical(&damaged, result, inputs)
                .resolve(&mut checker, &between, &scope)
                .unwrap();
            assert!(resolved.region.is_empty());
            assert_eq!(resolved.invalidated.invalid, immediate);
            assert!(!resolved.invalidated.requirements.is_empty());

            // The access's validity reaches the caller with the relation's.
            // An invalid protected referent holds no loan to protect, and its
            // use fails through its own validity contract.
            let call = &mut checker.calls.get_mut(&result).unwrap().summary;
            let place = |param| SymbolicPlace {
                root: RegionRoot::External(input(param).source),
                path: RegionPath::default(),
                views: Default::default(),
            };
            let relation = |protected, access| Separation {
                protected: place(protected),
                protected_kind: BorrowKind::Ref,
                access: place(access),
                access_kind: BorrowKind::Mut,
                extent: AccessExtent::Typed,
                suspended: Box::new([]),
            };
            call.loan_requirements = SeparationSet::new(
                &db,
                &scope,
                [Guarded {
                    guard: Guard::always(&scope),
                    payload: relation(param, 1 - param),
                }],
            );
            call.native_requirements = RegionSet::empty(&scope);
            let (requirements, validity) = checker
                .call_loan_requirements(&damaged, result, inputs)
                .unwrap();
            assert!(requirements[0].protected.is_empty());
            assert!(!requirements[0].access.is_empty());
            assert!(!validity.invalid && validity.requirements.is_empty());
            assert!(
                !checker
                    .call_native_validity(&damaged, result, inputs)
                    .unwrap()
                    .invalid
            );
            checker
                .calls
                .get_mut(&result)
                .unwrap()
                .summary
                .native_requirements = RegionSet::new(
                &scope,
                [Guarded {
                    guard: Guard::always(&scope),
                    payload: place(param),
                }],
            );
            let used = checker
                .call_native_validity(&damaged, result, inputs)
                .unwrap();
            assert_eq!(used.invalid, immediate);
            assert!(!used.requirements.is_empty());

            let call = &mut checker.calls.get_mut(&result).unwrap().summary;
            call.native_requirements = RegionSet::empty(&scope);
            call.loan_requirements = SeparationSet::new(
                &db,
                &scope,
                [Guarded {
                    guard: Guard::always(&scope),
                    payload: relation(1 - param, param),
                }],
            );
            let (requirements, validity) = checker
                .call_loan_requirements(&damaged, result, inputs)
                .unwrap();
            assert!(requirements[0].access.is_empty());
            assert_eq!(validity.invalid, immediate);
            assert!(!validity.requirements.is_empty());
        }
    }

    #[test]
    fn separation_validity_keeps_its_physical_basis_through_export() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "separation_validity_export.fe".into(),
            "fn leaf(_ a: ref u256, _ b: ref u256) {}\n\
             fn wrapper(_ first: ref u256, _ second: ref u256) { leaf(first, second) }\n\
             fn alias(_ value: ref u256) { wrapper(value, value) }\n\
             fn locals() {\n    let a: u256 = 1\n    let b: u256 = 2\n    wrapper(ref a, ref b)\n}\n\
             fn inputs(_ x: ref u256, _ y: ref u256) { wrapper(x, y) }",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let solved = |name| {
            let instance = get_or_build_semantic_instance(
                &db,
                identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, name))),
            );
            let mut checker = Borrowck::new(&db, instance).unwrap();
            checker.solve().unwrap();
            checker
        };
        /// Give the wrapper's callee a relation from its first input to
        /// `validity`, and that access's validity, then settle the analysis.
        fn inject<'db>(
            db: &'db dyn HirAnalysisDb,
            checker: &mut Borrowck<'db>,
            validity: SymbolicPlace<'db>,
        ) {
            let scope = BinderScope::default();
            let (_, result, _) = first_call(checker);
            let call = &mut checker.calls.get_mut(&result).unwrap().summary;
            call.separation_validity = RegionSet::new(
                &scope,
                [Guarded {
                    guard: Guard::always(&scope),
                    payload: validity.clone(),
                }],
            );
            call.loan_requirements = SeparationSet::new(
                db,
                &scope,
                [Guarded {
                    guard: Guard::always(&scope),
                    payload: Separation {
                        protected: SymbolicPlace {
                            root: RegionRoot::External(summary_input(db, 0).source),
                            path: RegionPath::default(),
                            views: Default::default(),
                        },
                        protected_kind: BorrowKind::Ref,
                        access: validity,
                        access_kind: BorrowKind::Mut,
                        extent: AccessExtent::Typed,
                        suspended: Box::new([]),
                    },
                }],
            );
            checker.resolve_operations().unwrap();
            checker.conflicts = Some(checker.analyze_conflicts());
        }
        let input = |param| summary_input(&db, param);
        let place = |param| SymbolicPlace {
            root: RegionRoot::External(input(param).source),
            path: RegionPath::default(),
            views: Default::default(),
        };

        // An access whose validity fails at this call is diagnosed here.
        let mut wrapper = solved("wrapper");
        inject(&db, &mut wrapper, clobbered(&db, input(0), input(0)));
        assert!(wrapper.conflicts().diagnostic.is_some());

        // An access through a borrow that is invalid where the wrapper's
        // inputs overlap leaves no relation; its validity is exported alone.
        let mut wrapper = solved("wrapper");
        let ((block, index), result, args) = first_call(&wrapper);
        let state = wrapper.before[block][index].clone();
        let [first, second] = [0, 1].map(|arg| {
            let region = wrapper
                .resolve_capability(state.value(args[arg].value))
                .region;
            SourceExpr::from_place(&region.clauses()[0].payload).unwrap()
        });
        let mut overwritten = ExternalSource::unknown(
            first.source.contract,
            AddressOccurrence::Value {
                instance: wrapper.instance,
                value: result,
                choice: 0,
            },
            Box::new([]),
            AddressProvenance::Raw,
        );
        overwritten.clobber = Some(Box::new(ClobberCondition::new(
            first,
            second,
            AccessExtent::Typed,
        )));
        let damaged = RegionSet::singleton(
            &BinderScope::default(),
            RegionRoot::External(overwritten),
            RegionPath::default(),
        );
        let original = state.value(args[1].value).clone();
        let invalid = uniform(&mut wrapper, &original, |semantics| {
            CapabilityRef::Invalidated {
                class: semantics.class,
                region: damaged.clone(),
            }
        });
        wrapper.before[block][index].set_value(args[1].value, invalid);
        inject(&db, &mut wrapper, place(1));
        wrapper
            .calls
            .get_mut(&result)
            .unwrap()
            .summary
            .separation_validity = RegionSet::empty(&BinderScope::default());
        wrapper.resolve_operations().unwrap();
        wrapper.conflicts = Some(wrapper.analyze_conflicts());
        assert!(wrapper.conflicts().diagnostic.is_none());
        let (summary, _) = wrapper.build_summary().unwrap();
        assert!(summary.loan_requirements.is_empty());
        assert_eq!(
            clobber_params(&summary.separation_validity),
            [(Some(0), Some(1))].into()
        );

        // Over distinct certain inputs, the obligations are exported with the
        // clobber condition intact.
        let mut wrapper = solved("wrapper");
        inject(&db, &mut wrapper, clobbered(&db, input(0), input(1)));
        assert!(wrapper.conflicts().diagnostic.is_none());
        let (summary, _) = wrapper.build_summary().unwrap();
        assert_eq!(
            clobber_params(&summary.separation_validity),
            [(Some(0), Some(1))].into()
        );
        let [relation] = summary.loan_requirements.clauses() else {
            panic!("one forwarded relation");
        };
        assert_eq!(
            clobber_params(&RegionSet::new(
                relation.guard.scope(),
                [Guarded {
                    guard: relation.guard.clone(),
                    payload: relation.payload.access.clone(),
                }]
            )),
            [(Some(0), Some(1))].into()
        );

        // A further caller refines them physically.
        for name in ["alias", "locals", "inputs"] {
            let mut caller = solved(name);
            let ((block, index), result, args) = first_call(&caller);
            let state = caller.before[block][index].clone();
            let call = &mut caller.calls.get_mut(&result).unwrap().summary;
            call.separation_validity = summary.separation_validity.clone();
            call.loan_requirements = summary.loan_requirements.clone();
            let inputs = CallInputs {
                args: &args,
                effects: &[],
                origin: SemOrigin::Body(caller.body.template_owner),
            };
            let (requirements, validity) = caller
                .call_loan_requirements(&state, result, inputs)
                .unwrap();
            let [requirement] = &requirements[..] else {
                panic!("{name}: one requirement");
            };
            match name {
                "alias" => assert!(validity.invalid, "{name}"),
                "locals" => {
                    assert!(!validity.invalid && validity.requirements.is_empty());
                    assert!(requirement.access.is_empty());
                }
                _ => {
                    assert!(!validity.invalid && !validity.requirements.is_empty());
                    assert_eq!(
                        clobber_params(&requirement.access),
                        [(Some(0), Some(1))].into()
                    );
                }
            }
        }
    }

    #[test]
    fn separation_export_names_unexposed_addresses_by_family_and_selector() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "separation_export.fe".into(),
            "struct Pair { a: u256, b: u256 }\n\
             fn holder(_ value: mut Pair, _ first: u256, _ second: u256, _ flag: bool) {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "holder"))),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        let params: BTreeMap<_, _> = checker
            .body
            .values
            .iter()
            .enumerate()
            .filter_map(|(index, value)| match value.definition {
                NValueDefinition::EntryParam { param } => Some((param, NValueId::new(index))),
                _ => None,
            })
            .collect();
        let scope = BinderScope::default();
        let u256 = TyId::u256(&db);
        // One allocation family, observed at two selectors that may be equal.
        let family = AddressOccurrence::Value {
            instance,
            value: params[&0],
            choice: 0,
        };
        let allocation = |selector: NValueId| {
            ExternalSource::allocation(
                &db,
                OpaqueHandleRef {
                    contract: OpaqueHandleContract {
                        handle_ty: TyId::ptr_to(&db, u256),
                        target_ty: u256,
                        address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                    },
                    occurrence: family,
                    arguments: Box::new([checker.index(selector)]),
                },
            )
        };
        let mut access = allocation(params[&1]);
        access.clobber = Some(Box::new(ClobberCondition::new(
            SourceExpr::whole(allocation(params[&1])),
            SourceExpr::whole(allocation(params[&2])),
            AccessExtent::Typed,
        )));
        // The clause and one slice observe a choice of a recursive call; the
        // other slice observes a choice only it mentions.
        let (recursive, other) = (params[&1], params[&2]);
        checker.recursive_calls.insert(recursive);
        let choice = |occurrence| ChoiceKey::new(occurrence, StructuralPath::default());
        let call_choice = |result, choice| ValueOccurrence::CallChoice { result, choice };
        let guard = Guard::always(&scope)
            .with_boolean(choice(call_choice(recursive, 0)), true)
            .and_then(|guard| guard.with_boolean(choice(ValueOccurrence::Value(params[&3])), true))
            .unwrap();
        let slice = |field, occurrence| Guarded {
            guard: Guard::always(&scope)
                .with_boolean(choice(occurrence), true)
                .unwrap(),
            payload: RegionPath::new([Projection::Field(FieldIndex(field))]),
        };
        let pair = ReferentContract::new(
            &db,
            checker.summary_param_ty(0).unwrap(),
            HandleAddressSpace::Unspecified,
        );
        let clause = Guarded {
            guard,
            payload: Separation {
                protected: SymbolicPlace {
                    root: RegionRoot::External(ExternalSource::input(
                        InputSource::slot(0, StructuralPath::default()),
                        pair,
                        false,
                    )),
                    path: RegionPath::default(),
                    views: Default::default(),
                },
                protected_kind: BorrowKind::Mut,
                access: SymbolicPlace {
                    root: RegionRoot::External(access),
                    path: RegionPath::default(),
                    views: Default::default(),
                },
                access_kind: BorrowKind::Mut,
                extent: AccessExtent::Typed,
                suspended: Box::new([
                    slice(0, call_choice(other, 1)),
                    slice(1, call_choice(recursive, 1)),
                ]),
            },
        };
        let origin = SeparationOrigin {
            owner: instance.key(&db).owner(&db),
            template_owner: checker.body.template_owner,
            borrow: SemOrigin::Body(checker.body.template_owner),
            access: SemOrigin::Body(checker.body.template_owner),
        };
        /// Every handle the source mentions, in visit order.
        fn handles<'db>(
            source: &ExternalSource<'db>,
        ) -> Vec<(AddressOccurrence<'db>, Vec<IndexExpr<'db>>)> {
            let mut handles = Vec::new();
            source
                .clone()
                .map_occurrences(&mut |occurrence, arguments| {
                    handles.push((*occurrence, arguments.to_vec()));
                });
            handles
        }
        for exposed in [BTreeMap::new(), BTreeMap::from([(family, 5)])] {
            let mut choices = BTreeSet::new();
            let [(exported, _)] =
                &checker.summarize_separations(&[(clause.clone(), origin)], &mut choices, &exposed)
                    [..]
            else {
                panic!("one exported relation");
            };
            let RegionRoot::External(access) = &exported.payload.access.root else {
                panic!("external access");
            };
            let handles = handles(access);
            assert_eq!(handles.len(), 3);
            let number = if exposed.is_empty() { 0 } else { 5 };
            assert!(
                handles
                    .iter()
                    .all(|(occurrence, _)| *occurrence == AddressOccurrence::Summary(number))
            );
            // The written selector and the access's own are distinct names
            // that may still denote one address.
            let arguments: BTreeSet<_> = handles.iter().map(|(_, arguments)| arguments).collect();
            assert_eq!(arguments.len(), 2);
            if exposed.is_empty() {
                let witnesses: Vec<_> = arguments.iter().map(|arguments| arguments[0]).collect();
                assert!(arguments.iter().all(|arguments| arguments.len() == 1));
                assert!(
                    exported
                        .guard
                        .with_equality(witnesses[0], witnesses[1])
                        .is_some()
                );
            } else {
                assert_eq!(
                    arguments,
                    [
                        vec![IndexExpr::FormalValue(1)],
                        vec![IndexExpr::FormalValue(2)]
                    ]
                    .iter()
                    .collect()
                );
            }
            // The recursive choice is forgotten, and the slice it conditions
            // is dropped; the other slice keeps its choice for renaming.
            assert_eq!(
                exported.guard.occurrences(),
                [ValueOccurrence::Argument(3)].into()
            );
            let [suspended] = &*exported.payload.suspended else {
                panic!("{exported:#?}");
            };
            assert_eq!(
                suspended.payload,
                RegionPath::new([Projection::Field(FieldIndex(0))])
            );
            assert_eq!(
                suspended.guard.occurrences(),
                [call_choice(other, 1)].into()
            );
            assert!(choices.contains(&call_choice(other, 1)));
        }
    }

    #[test]
    fn separation_limits_apply_to_merged_relations() {
        // Two clauses of one relation, or of one access's validity, each fit
        // the guard limit; merging them ORs their guards past it, which only
        // the final check sees.
        let count = 44;
        let params: Vec<_> = (0..count)
            .map(|index| format!("_ x{index}: u256"))
            .collect();
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "separation_merge.fe".into(),
            &format!(
                "fn holder(_ value: mut u256, _ saved: *u256, {}) {{}}",
                params.join(", ")
            ),
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "holder"))),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        let params: BTreeMap<_, _> = checker
            .body
            .values
            .iter()
            .enumerate()
            .filter_map(|(index, value)| match value.definition {
                NValueDefinition::EntryParam { param } => Some((param, NValueId::new(index))),
                _ => None,
            })
            .collect();
        let scope = BinderScope::default();
        let index = |position: usize| checker.index(params[&(position as u32 + 2)]);
        // Equal neighbors, paired from the first or the second scalar.
        let paired = |offset: usize| {
            (offset..count - 1)
                .step_by(2)
                .try_fold(Guard::always(&scope), |guard, position| {
                    guard.with_equality(index(position), index(position + 1))
                })
                .unwrap()
        };
        let (first, second) = (paired(0), paired(1));
        assert!(first.node_count() <= SEPARATION_GUARD_NODE_LIMIT);
        assert!(second.node_count() <= SEPARATION_GUARD_NODE_LIMIT);
        assert!(first.or(&second).node_count() > SEPARATION_GUARD_NODE_LIMIT);
        let referent = |param| {
            checker
                .resolve_capability(checker.inventory.entry.value(params[&param]))
                .region
                .clauses()[0]
                .payload
                .clone()
        };
        let relation = Separation {
            protected: referent(0),
            protected_kind: BorrowKind::Mut,
            access: referent(1),
            access_kind: BorrowKind::Mut,
            extent: AccessExtent::Typed,
            suspended: Box::new([]),
        };
        let origin = SeparationOrigin {
            owner: instance.key(&db).owner(&db),
            template_owner: checker.body.template_owner,
            borrow: SemOrigin::Body(checker.body.template_owner),
            access: SemOrigin::Body(checker.body.template_owner),
        };
        for (guards, merged) in [
            (vec![first.clone()], false),
            (vec![second.clone()], false),
            (vec![first, second], true),
        ] {
            // Either relation clauses, or the validity of an access with no
            // relation left.
            for validity in [false, true] {
                checker.conflicts = Some(ConflictAnalysis {
                    diagnostic: None,
                    exhausted: false,
                    deferred: guards
                        .iter()
                        .filter(|_| !validity)
                        .map(|guard| {
                            let clause = Guarded {
                                guard: guard.clone(),
                                payload: relation.clone(),
                            };
                            (clause, origin)
                        })
                        .collect(),
                    validity: NativeValidity {
                        invalid: false,
                        requirements: RegionSet::new(
                            &scope,
                            guards.iter().filter(|_| validity).map(|guard| Guarded {
                                guard: guard.clone(),
                                payload: relation.access.clone(),
                            }),
                        ),
                    },
                });
                match checker.build_summary() {
                    Ok((summary, _)) => {
                        assert!(!merged);
                        let exported = if validity {
                            summary.separation_validity.clauses().len()
                        } else {
                            summary.loan_requirements.clauses().len()
                        };
                        assert_eq!(exported, 1);
                    }
                    Err(diagnostic) => {
                        assert!(merged);
                        assert_eq!(
                            diagnostic.primary.message,
                            "borrow separation requirements exceed the analysis limits"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn separation_limits_apply_after_projecting_guard_only_witnesses() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "separation_projected_budget.fe".into(),
            "fn holder(_ value: mut u256, _ pointer: *u256) {}",
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "holder"))),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        let referents: Vec<_> = checker
            .body
            .values
            .iter()
            .enumerate()
            .filter(|(_, value)| matches!(value.definition, NValueDefinition::EntryParam { .. }))
            .map(|(index, _)| {
                checker
                    .resolve_capability(checker.inventory.entry.value(NValueId::new(index)))
                    .region
                    .clauses()[0]
                    .payload
                    .clone()
            })
            .collect();
        let owner = BinderScope::default();
        let (scope, witnesses) =
            (0..44).fold((owner.clone(), Vec::new()), |(scope, mut witnesses), _| {
                let (scope, witness) = scope.bind(IndexNamespace::Existential);
                witnesses.push(witness);
                (scope, witnesses)
            });
        let paired = |offset: usize| {
            witnesses[offset..]
                .as_chunks::<2>()
                .0
                .iter()
                .try_fold(Guard::always(&scope), |guard, pair| {
                    guard.with_equality(pair[0], pair[1])
                })
                .unwrap()
        };
        let guard = paired(0).or(&paired(1));
        assert!(guard.node_count() > SEPARATION_GUARD_NODE_LIMIT);
        assert!(witnesses.len() > SEPARATION_WITNESS_LIMIT as usize);
        let private = |number| {
            ChoiceKey::new(
                ValueOccurrence::SummaryChoice(number),
                StructuralPath::default(),
            )
        };
        let private_guard = (0..14).fold(Guard::always(&owner), |guard, index| {
            let equal = [false, true].map(|value| {
                Guard::always(&owner)
                    .with_boolean(private(index), value)
                    .unwrap()
                    .with_boolean(private(index + 14), value)
                    .unwrap()
            });
            guard.and(&equal[0].or(&equal[1])).unwrap()
        });
        assert!(private_guard.node_count() > SEPARATION_GUARD_NODE_LIMIT);
        let origin = SeparationOrigin {
            owner: instance.key(&db).owner(&db),
            template_owner: checker.body.template_owner,
            borrow: SemOrigin::Body(checker.body.template_owner),
            access: SemOrigin::Body(checker.body.template_owner),
        };
        for guard in [guard, private_guard] {
            checker.conflicts = Some(ConflictAnalysis {
                diagnostic: None,
                exhausted: false,
                deferred: vec![(
                    Guarded {
                        guard,
                        payload: Separation {
                            protected: referents[0].clone(),
                            protected_kind: BorrowKind::Mut,
                            access: referents[1].clone(),
                            access_kind: BorrowKind::Mut,
                            extent: AccessExtent::Typed,
                            suspended: Box::new([]),
                        },
                    },
                    origin,
                )],
                validity: NativeValidity {
                    invalid: false,
                    requirements: RegionSet::empty(&owner),
                },
            });
            let (summary, provenance) = checker.build_summary().unwrap();
            let [clause] = summary.loan_requirements.clauses() else {
                panic!("one canonical relation")
            };
            assert_eq!(clause.guard, Guard::always(&owner));
            assert_eq!(provenance, [origin]);
        }
    }

    #[test]
    fn separation_validity_limits_bound_retained_witnesses() {
        // The validity of an access with no relation left keeps every witness
        // its place names; past the witness limit it fails like a relation.
        let depth = SEPARATION_WITNESS_LIMIT as usize + 1;
        let ty = (0..depth).fold("u256".to_string(), |ty, _| format!("[{ty}; 2]"));
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "separation_witnesses.fe".into(),
            &format!("fn holder(_ cells: *{ty}) {{}}"),
        );
        let (module, _) = db.top_mod(file);
        db.assert_no_diags(module);
        let instance = get_or_build_semantic_instance(
            &db,
            identity_semantic_instance_key(&db, BodyOwner::Func(find_func(&db, module, "holder"))),
        );
        let mut checker = Borrowck::new(&db, instance).unwrap();
        checker.solve().unwrap();
        let param = checker
            .body
            .values
            .iter()
            .position(|value| matches!(value.definition, NValueDefinition::EntryParam { .. }))
            .unwrap();
        let cells = checker
            .resolve_capability(checker.inventory.entry.value(NValueId::new(param)))
            .region
            .clauses()[0]
            .payload
            .clone();
        let owner = BinderScope::default();
        for witnesses in [depth - 1, depth] {
            let (scope, selectors) =
                (0..witnesses).fold((owner.clone(), Vec::new()), |(scope, mut selectors), _| {
                    let (scope, selector) = scope.bind(IndexNamespace::Existential);
                    selectors.push(Projection::Index(selector));
                    (scope, selectors)
                });
            checker.conflicts = Some(ConflictAnalysis {
                diagnostic: None,
                exhausted: false,
                deferred: Vec::new(),
                validity: NativeValidity {
                    invalid: false,
                    requirements: RegionSet::new(
                        &owner,
                        [Guarded {
                            guard: Guard::always(&scope),
                            payload: SymbolicPlace {
                                path: RegionPath::new(selectors),
                                ..cells.clone()
                            },
                        }],
                    ),
                },
            });
            match checker.build_summary() {
                Ok((summary, _)) => {
                    assert!(witnesses <= SEPARATION_WITNESS_LIMIT as usize);
                    let [clause] = summary.separation_validity.clauses() else {
                        panic!("one validity requirement");
                    };
                    assert_eq!(
                        clause.guard.scope().existential_extension_of(&owner),
                        Some(witnesses as u32)
                    );
                }
                Err(diagnostic) => {
                    assert!(witnesses > SEPARATION_WITNESS_LIMIT as usize);
                    assert_eq!(
                        diagnostic.primary.message,
                        "borrow separation requirements exceed the analysis limits"
                    );
                }
            }
        }
    }
}
