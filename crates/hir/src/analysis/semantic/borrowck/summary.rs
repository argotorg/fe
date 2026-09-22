//! Structural return values and mutable-input poststates use the same algebra.
use super::validity::NativeValidity;
use crate::analysis::semantic::diagnostics::SemanticDiagnostic;
use std::collections::BTreeMap;

use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BorrowActivation, FieldIndex, SemOrigin, SemanticInstance,
            capability::{
                birth::AllocationBirth,
                external::{ClobberCondition, ExternalOrigin, ExternalSource, ReferentContract},
                footprint::{AccessExtent, AccessFootprint},
                guard::{Guard, ValueOccurrence},
                handle::{
                    AddressOccurrence, HandleAddressSpace, OpaqueHandleContract, OpaqueHandleRef,
                    OpaqueWriteSite,
                },
                index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
                loan::{CapabilityRef, LoanDef, LoanId, LoanRef},
                opaque::OpaqueWrite,
                path::{Projection, RegionPath, StructuralPath, project_referent_ty},
                region::{OverlapResult, RegionRoot, RegionSet, SymbolicPlace, substitute_clause},
                semantics::{CapabilityClass, CapabilitySemantics},
                shape::{ShapeChildren, ShapeId},
                source::{InputOrigin, SourceExpr},
                state::{BorrowState, CapabilityValue, CapabilityValues},
                value::{Guarded, IndexPayload, ValueId, ValueInterner, ValueLimits},
            },
            get_or_build_semantic_instance, instantiated_effect_env,
            normalized::{
                NEffectArg, NEffectArgValue, NExpr, NOperand, NRootKind, NStatement,
                NStatementKind, NTerminatorKind, NValueDefinition, NValueId,
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
    inventory::referent_contract,
    ir::{
        AvailabilityRequirement, AvailabilitySummary, BorrowSummary, BoundaryRequirement,
        InputPoststate, MemoryAccess, PendingSemanticValidation,
    },
    solver::{BorrowSummaryMode, Borrowck, Resolution},
    validation::can_specialize,
};

pub type SourceValue<'db> = ValueId<'db, SourceExpr<'db>>;
type SourceValues<'db> = ValueInterner<'db, SourceExpr<'db>>;

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

#[derive(Clone)]
pub(super) struct CallSummary<'db> {
    pub instance: SemanticInstance<'db>,
    pub summary: BorrowSummary<'db>,
    pub pending: bool,
    updates: Vec<CapabilityValue<'db>>,
    births: Vec<AllocationBirth<'db>>,
}

impl<'db> Borrowck<'db> {
    fn instantiate_address_base(
        &self,
        mut source: ExternalSource<'db>,
        result: NValueId,
        origin: SemOrigin<'db>,
    ) -> Result<ExternalSource<'db>, SemanticDiagnostic<'db>> {
        let mut invalid = false;
        source.map_occurrences(&mut |occurrence, arguments| {
            let AddressOccurrence::Summary(choice) = *occurrence else {
                invalid = true;
                return;
            };
            *arguments = arguments
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
                choice,
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
                                    let id = LoanId(self.inventory.loans.len());
                                    let (loan, args, _) = LoanDef::with_occurrence_arguments(
                                        kind,
                                        BorrowActivation::Immediate,
                                        origin,
                                        scope,
                                        self.inventory.loops.arguments(&self.body, result),
                                    );
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
            for value in std::iter::once(&summary.result)
                .chain(summary.mutable_inputs.iter().map(|update| &update.value))
            {
                for leaf in sources.leaves(value, ValueOccurrence::Summary) {
                    external.push((
                        leaf.payload.source,
                        leaf.guard.scope().clone(),
                        Some(leaf.guard),
                    ));
                }
            }
            external.extend(summary.mutable_inputs.iter().map(|update| {
                (
                    update.destination.source.clone(),
                    update.value.scope().clone(),
                    None,
                )
            }));
            external.extend(summary.requirements.iter().flat_map(|requirement| {
                std::iter::once(&requirement.region)
                    .chain(requirement.populated.iter())
                    .flat_map(|region| region.clauses())
                    .filter_map(|clause| {
                        SourceExpr::from_place(&clause.payload).map(|source| {
                            (
                                source.source,
                                clause.guard.scope().clone(),
                                Some(clause.guard.clone()),
                            )
                        })
                    })
            }));
            external.extend(
                summary
                    .accesses
                    .iter()
                    .flat_map(|access| [&access.region, &access.authorizers])
                    .flat_map(|region| region.clauses())
                    .filter_map(|clause| {
                        SourceExpr::from_place(&clause.payload).map(|source| {
                            (
                                source.source,
                                clause.guard.scope().clone(),
                                Some(clause.guard.clone()),
                            )
                        })
                    }),
            );
            external.extend(
                summary
                    .availability
                    .incoming
                    .iter()
                    .map(|requirement| &requirement.region)
                    .chain([
                        &summary.availability.reinitialized,
                        &summary.availability.unavailable,
                        &summary.native_requirements,
                    ])
                    .flat_map(|region| region.clauses())
                    .filter_map(|clause| {
                        SourceExpr::from_place(&clause.payload).map(|source| {
                            (
                                source.source,
                                clause.guard.scope().clone(),
                                Some(clause.guard.clone()),
                            )
                        })
                    }),
            );
            let mut births = Vec::new();
            for (source, scope, guard) in external {
                if let Some(guard) = guard
                    && let Some(birth) = AllocationBirth::from_source(&source, guard)
                    && !births.contains(&birth)
                {
                    births.push(birth);
                }
                let base = if let Some(base) = source.address_base(self.db) {
                    self.instantiate_address_base(base, result, origin)?
                } else {
                    match source.origin {
                        ExternalOrigin::Provider {
                            provider,
                            target_ty,
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
                    pending,
                    updates,
                    births,
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
        mut self,
    ) -> Result<BorrowSummaryComputation<'db>, SemanticDiagnostic<'db>> {
        if let Some(summary) = self.intrinsic_summary()? {
            self.verify_summary(&summary)?;
            return Ok(BorrowSummaryComputation {
                summary: Some(summary),
                blocked: None,
                pending: Default::default(),
            });
        }
        if self
            .instance
            .key(self.db)
            .owner(self.db)
            .body(self.db)
            .is_none()
        {
            return Ok(BorrowSummaryComputation {
                summary: Some(signature_summary(self.db, self.instance, true)?),
                blocked: None,
                pending: PendingSemanticValidation {
                    callees: [self.instance.key(self.db)].into(),
                },
            });
        }
        self.solve()?;
        let summary = if !self.pending.callees.is_empty() && can_specialize(self.db, self.instance)
        {
            if self.summary_mode == BorrowSummaryMode::Final && self.blocked.is_none() {
                if let Some(diagnostic) = self.analyze_availability().diagnostic {
                    return Err(diagnostic);
                }
                self.boundary_requirements
                    .as_ref()
                    .expect("solved boundary requirements")
                    .clone()?;
                self.validate_pending_exports()?;
            }
            signature_summary(self.db, self.instance, true)?
        } else {
            self.build_summary()?
        };
        Ok(BorrowSummaryComputation {
            summary: Some(summary),
            blocked: self.blocked,
            pending: self.pending,
        })
    }

    /// Returning paths that never crossed an unresolved effect still have a
    /// complete boundary proof obligation, even in a pending template.
    fn validate_pending_exports(&self) -> Result<(), SemanticDiagnostic<'db>> {
        let mut values = SourceValues::new(self.db, ValueLimits::default());
        let mut choices = BTreeMap::new();
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
                .map(|(_, value)| (value, Boundary::Retained));
            for (value, boundary) in returned
                .into_iter()
                .map(|returned| (state.value(returned.value), Boundary::Return))
                .chain(retained)
            {
                self.summarize_value(
                    value,
                    boundary,
                    block.terminator.origin,
                    &mut values,
                    &mut choices,
                    &mut handles,
                )?;
            }
        }
        Ok(())
    }

    pub fn build_summary(&self) -> Result<BorrowSummary<'db>, SemanticDiagnostic<'db>> {
        let ownership = self.analyze_availability();
        if self.summary_mode == BorrowSummaryMode::Final
            && self.blocked.is_none()
            && let Some(diagnostic) = ownership.diagnostic
        {
            return Err(diagnostic);
        }
        // Provisional summaries supply provider facts needed for body admission
        // and definite assignment. Boundary policy must not suppress those facts.
        let pending = if self.summary_mode == BorrowSummaryMode::Final {
            self.boundary_requirements
                .as_ref()
                .expect("solved boundary requirements")
                .clone()?
        } else {
            Vec::new()
        };
        let scope = BinderScope::default();
        let result_shape = self.shape(self.instance.normalized_result_ty(self.db))?;
        let mut values = SourceValues::new(self.db, ValueLimits::default());
        let mut result = values.empty(result_shape, &scope);
        let mut updates = Vec::new();
        let inputs = self.inventory.inputs.clone();
        for input in &inputs {
            if input.writable
                && input.shape.contains_capability(self.db)
                && !matches!(input.source.origin, ExternalOrigin::Local(_))
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
        let mut choices = BTreeMap::new();
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
                    Boundary::Return,
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
                let sources = self.summarize_value(
                    contents,
                    Boundary::Retained,
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
        let pending_accesses = self.body_memory_accesses();
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
                &external(&access.region),
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
        let summary = BorrowSummary {
            native_requirements: self.summarize_availability_region(
                &ownership.native_validity.requirements,
                origin,
                &mut choices,
                &handles,
                false,
            )?,
            accesses,
            availability,
            may_return,
            result,
            mutable_inputs: updates,
            requirements,
        };
        self.verify_summary(&summary)?;
        Ok(summary)
    }

    fn summarize_availability_region(
        &self,
        region: &RegionSet<'db>,
        origin: SemOrigin<'db>,
        choices: &mut BTreeMap<ValueOccurrence, u32>,
        exposed_handles: &BTreeMap<AddressOccurrence<'db>, u32>,
        definite: bool,
    ) -> Result<RegionSet<'db>, SemanticDiagnostic<'db>> {
        let mut summary = RegionSet::empty(&BinderScope::default());
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
            summary = summary.union(&self.summarize_region(
                &region,
                origin,
                choices,
                &mut exposed_handles.clone(),
            )?);
        }
        Ok(summary)
    }

    fn summarize_region(
        &self,
        region: &RegionSet<'db>,
        origin: SemOrigin<'db>,
        choices: &mut BTreeMap<ValueOccurrence, u32>,
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
        boundary: Boundary,
        origin: SemOrigin<'db>,
        values: &mut SourceValues<'db>,
        choices: &mut BTreeMap<ValueOccurrence, u32>,
        handles: &mut BTreeMap<AddressOccurrence<'db>, u32>,
    ) -> Result<SourceValue<'db>, SemanticDiagnostic<'db>> {
        let mut failure = None;
        let result =
            self.inventory
                .values
                .map_payloads(value, values, |semantics, _, entry, domain| {
                    let region = entry
                        .payload
                        .region(self.db, &self.inventory.loans, entry.guard.scope())
                        .with_guard(domain);
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
                        if let Some(source) =
                            self.summarize_source(payload, &clause.guard, choices, handles)
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
        choices: &mut BTreeMap<ValueOccurrence, u32>,
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
        let mut scope = guard.scope().clone();
        let mut substitutions = BTreeMap::new();
        for index in guard.indices().into_iter().chain(payload.indices()) {
            if matches!(index, IndexExpr::Runtime(_) | IndexExpr::Iteration(_)) {
                substitutions.entry(index).or_insert_with(|| {
                    match match index {
                        IndexExpr::Runtime(value) => self.index(value),
                        index => index,
                    } {
                        IndexExpr::Runtime(actual) => {
                            match self.body.values[actual.index()].definition {
                                NValueDefinition::EntryParam { param } => {
                                    IndexExpr::FormalValue(param)
                                }
                                NValueDefinition::BlockParam { .. }
                                | NValueDefinition::Statement { .. } => {
                                    let (nested, witness) = scope.bind(IndexNamespace::Existential);
                                    scope = nested;
                                    witness
                                }
                            }
                        }
                        IndexExpr::Iteration(_) => {
                            let (nested, witness) = scope.bind(IndexNamespace::Existential);
                            scope = nested;
                            witness
                        }
                        index => index,
                    }
                });
            }
        }
        let subst = IndexSubst::new(guard.scope(), &scope, substitutions)
            .expect("summary local index abstraction");
        let guard = guard.substitute(&subst).and_then(|guard| {
            guard.map_occurrences(|occurrence| {
                if let ValueOccurrence::Value(value) = occurrence
                    && let NValueDefinition::EntryParam { param } =
                        self.body.values[value.index()].definition
                {
                    return ValueOccurrence::Argument(param);
                }
                if matches!(
                    occurrence,
                    ValueOccurrence::Argument(_) | ValueOccurrence::Summary
                ) {
                    return occurrence;
                }
                let next = choices.len().try_into().expect("summary choice count");
                ValueOccurrence::SummaryChoice(*choices.entry(occurrence).or_insert(next))
            })
        })?;
        Some(Guarded {
            guard,
            payload: payload.substitute(self.db, &subst),
        })
    }

    pub(super) fn summary_param_ty(&self, param: u32) -> Option<TyId<'db>> {
        self.body
            .values
            .iter()
            .find_map(|value| {
                matches!(value.definition,
            NValueDefinition::EntryParam { param: actual } if actual == param)
                .then_some(value.ty)
            })
            .or_else(|| {
                self.body.roots.iter().find_map(|root| {
                    matches!(root.kind, NRootKind::ParamPlace { param: actual } if actual == param)
                        .then_some(root.ty)
                })
            })
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
            ExternalOrigin::Unknown {
                contract,
                occurrence,
                ..
            } => {
                if !matches!(occurrence, AddressOccurrence::Summary(_)) {
                    return Err(invalid(
                        "unknown address contains a callee-local occurrence",
                    ));
                }
                (
                    contract.ty,
                    CapabilityClass::Pointer,
                    contract.address_space,
                )
            }
            ExternalOrigin::Local(_) => return Err(invalid("source contains local storage")),
            ExternalOrigin::Input(input) => {
                let param_ty = self
                    .summary_param_ty(input.param())
                    .ok_or_else(|| invalid("source parameter does not exist"))?;
                if external.is_reachable() {
                    let class = self
                        .reachable_source_class(external, requested, writable)
                        .ok_or_else(|| {
                            invalid("reachable source has no compatible input authority")
                        })?;
                    (external.contract.ty, class, external.contract.address_space)
                } else {
                    let semantics = match input.origin() {
                        InputOrigin::Place(_) => self.shape(param_ty)?.direct(db),
                        InputOrigin::Slot { slot, .. } => {
                            let param_ty = param_ty.as_view(db).unwrap_or(param_ty);
                            let ty =
                                project_referent_ty(db, self.instance, param_ty, slot.as_slice())
                                    .ok_or_else(|| invalid("input slot path is invalid"))?;
                            self.shape(ty)?.direct(db)
                        }
                    }
                    .ok_or_else(|| invalid("input source does not select a capability"))?;
                    let contract = super::inventory::input_referent_contract(
                        db,
                        self.instance,
                        semantics,
                        input.param(),
                    )
                    .map_err(|_| invalid("input referent contract is unresolved"))?;
                    (contract.ty, semantics.class, contract.address_space)
                }
            }
            ExternalOrigin::Provider {
                provider,
                target_ty,
            } => {
                if !self.inventory.inputs.iter().any(|input| matches!(input.source.origin,
                    ExternalOrigin::Provider { provider: actual, target_ty: actual_ty } if actual == *provider && actual_ty == *target_ty)) {
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
                element,
                target_ty,
            } => {
                self.verify_source(base, scope, None, false)?;
                if element.is_some_and(|(_, index)| scope.validate(index).is_err()) {
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
            let steps = match &external.origin {
                ExternalOrigin::Input(input) => input.dereferences(),
                _ => &[],
            };
            for path in steps.iter().chain(external.dereferences()) {
                let slot = project_referent_ty(db, self.instance, ty, path.as_slice())
                    .ok_or_else(|| invalid("followed source path is invalid"))?;
                let semantics = self.shape(slot)?.direct(db).ok_or_else(|| {
                    invalid("followed source does not select a stored capability")
                })?;
                let contract = referent_contract(db, self.instance, semantics)
                    .map_err(|_| invalid("followed referent contract is unresolved"))?;
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
        for update in &summary.mutable_inputs {
            let ty = self.verify_source(&update.destination, update.value.scope(), None, true)?;
            if self.shape(ty)? != update.value.shape() {
                return Err(self.internal_diag(
                    SemOrigin::Body(self.body.template_owner),
                    "summary poststate differs from its destination shape".into(),
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
        for region in summary
            .availability
            .incoming
            .iter()
            .map(|requirement| &requirement.region)
            .chain([
                &summary.availability.reinitialized,
                &summary.availability.unavailable,
                &summary.native_requirements,
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
            let subst = IndexSubst::new(
                template.guard.scope(),
                guard.scope(),
                inputs.args.iter().enumerate().map(|(param, arg)| {
                    (
                        IndexExpr::FormalValue(param.try_into().expect("parameter count")),
                        self.index(arg.value),
                    )
                }),
            )
            .expect("birth scalar substitution");
            let source = ExternalSource::allocation(self.db, template.allocation.clone())
                .substitute(self.db, &subst);
            let source = self.instantiate_address_base(source, result, inputs.origin)?;
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
        let mut updates = Vec::new();
        for (update, template) in call.summary.mutable_inputs.iter().zip(&call.updates) {
            let value = self.instantiate_value(state, &update.value, template, result, inputs)?;
            let target = self.instantiate_source(
                state,
                &update.destination,
                result,
                update.value.scope(),
                inputs,
            )?;
            updates.push((target.region, value));
        }
        let storage = updates
            .iter()
            .flat_map(|(region, _)| region.clauses())
            .filter_map(|clause| match &clause.payload.root {
                RegionRoot::External(source) => {
                    Some((source.clone(), clause.guard.scope().clone()))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        let accesses = self.call_memory_accesses(state, result, inputs)?;
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
        let mut values = CapabilityValues::new(self.db, ValueLimits::default());
        let sources = SourceValues::new(self.db, ValueLimits::default());
        let mut error = None;
        let instantiated =
            sources.map_payloads(value, &mut values, |semantics, path, entry, domain| {
                let guard = match self.instantiate_guard(domain, result, inputs) {
                    Ok(Some(guard)) => guard,
                    Ok(None) => return Vec::new(),
                    Err(failure) => {
                        error.get_or_insert(failure);
                        return Vec::new();
                    }
                };
                let resolved = match self.instantiate_source(
                    state,
                    &entry.payload,
                    result,
                    guard.scope(),
                    inputs,
                ) {
                    Ok(resolved) => resolved,
                    Err(failure) => {
                        error.get_or_insert(failure);
                        return Vec::new();
                    }
                };
                let region = if resolved.invalidated.invalid {
                    resolved.invalidated.requirements.with_guard(&guard)
                } else {
                    resolved.region.with_guard(&guard)
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

    pub(super) fn instantiate_guard(
        &self,
        guard: &Guard<'db>,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Option<Guard<'db>>, SemanticDiagnostic<'db>> {
        let subst = IndexSubst::new(
            guard.scope(),
            guard.scope(),
            inputs.args.iter().enumerate().map(|(param, arg)| {
                (
                    IndexExpr::FormalValue(param.try_into().expect("parameter count")),
                    self.index(arg.value),
                )
            }),
        )
        .expect("call guard substitution");
        let mut invalid = false;
        let guard = guard.substitute(&subst).and_then(|guard| {
            guard.map_occurrences(|occurrence| match occurrence {
                ValueOccurrence::Argument(param) => {
                    inputs
                        .occurrence(param)
                        .unwrap_or(ValueOccurrence::CallChoice {
                            result,
                            choice: param,
                        })
                }
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
        });
        if invalid {
            return Err(self.internal_diag(
                inputs.origin,
                "summary retains a local enum occurrence".into(),
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
                        .with_guard(&guard)
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
        let CallInputs {
            args,
            effects,
            origin,
        } = inputs;
        let subst = IndexSubst::new(
            scope,
            scope,
            args.iter().enumerate().map(|(param, arg)| {
                (
                    IndexExpr::FormalValue(param.try_into().expect("parameter count")),
                    self.index(arg.value),
                )
            }),
        )
        .expect("summary destination scalar substitution");
        let source = source.substitute(self.db, &subst);
        let invalidated = source.invalidated;
        let path = &source.path;
        let external = &source.source;
        let clobber = if let Some(clobber) = &external.clobber {
            let target = self.instantiate_source(state, &clobber.target, result, scope, inputs)?;
            let written =
                self.instantiate_source(state, &clobber.written, result, scope, inputs)?;
            if matches!(
                AccessFootprint::typed(&target.region).overlap(
                    self.db,
                    AccessFootprint {
                        region: &written.region,
                        extent: clobber.extent
                    }
                ),
                OverlapResult::Disjoint
            ) {
                return Ok(Resolution::empty(scope));
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
            ExternalOrigin::Local(_) => {
                return Err(self.internal_diag(origin, "summary retains local storage".into()));
            }
            ExternalOrigin::Memory {
                base,
                element,
                target_ty,
            } => {
                let base = self.instantiate_source(state, base, result, scope, inputs)?;
                let region = self.memory_region(&base.region, *target_ty, *element, origin)?;
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
                )?;
                let ty = source.contract.ty;
                let region = if let Some((target, written, extent)) = clobber {
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
                            if matches!(
                                AccessFootprint::typed(&RegionSet::new(
                                    guard.scope(),
                                    [target_clause.clone()]
                                ))
                                .overlap(
                                    self.db,
                                    AccessFootprint {
                                        region: &RegionSet::new(
                                            guard.scope(),
                                            [written_clause.clone()]
                                        ),
                                        extent: extent.substitute(&written_subst)
                                    }
                                ),
                                OverlapResult::Disjoint
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
                                    path: RegionPath::default(),
                                    views: Default::default(),
                                },
                            });
                        }
                    }
                    RegionSet::new(scope, clauses)
                } else {
                    RegionSet::singleton(scope, RegionRoot::External(source), RegionPath::default())
                };
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
            ExternalOrigin::Provider {
                provider,
                target_ty,
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
                        source = SourceExpr {
                            source: ExternalSource::memory(db, source, semantics.target_ty, None),
                            path: RegionPath::default(),
                            views: Default::default(),
                            invalidated: false,
                        };
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
                    ReferentContract::new(
                        db,
                        semantics.target_ty,
                        HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                    ),
                    occurrence,
                    scope.variables().collect(),
                ))
            } else {
                match OpaqueHandleContract::for_ty(
                    db,
                    instance.key(db).impl_env(db).normalization_scope(db),
                    instance.assumptions(db),
                    semantics.representation_ty,
                ) {
                    Ok(Some(contract)) => Some(ExternalSource::opaque(
                        db,
                        OpaqueHandleRef {
                            contract,
                            occurrence,
                            arguments: scope.variables().collect(),
                        },
                    )),
                    Ok(None) | Err(_) => {
                        failure.get_or_insert_with(|| {
                            checker.internal_diag(
                                SemOrigin::Body(checker.body.template_owner),
                                "opaque result has an unresolved address contract".into(),
                            )
                        });
                        None
                    }
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
        let mut contents = CapabilityValues::new(checker.db, ValueLimits::default());
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
        may_return: opaque && !instance.is_intrinsically_never_returning(db),
        result,
        mutable_inputs,
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
                capability::{handle::HandleAddressSpace, source::InputSource},
                identity_semantic_instance_key,
            },
            ty::{
                ProviderAddressSpace,
                corelib::{resolve_core_trait, resolve_lib_func_path},
                ty_check::BodyOwner,
            },
        },
        hir_def::ItemKind,
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
            let checker = Borrowck::new(&db, instance).unwrap();
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
            let concrete = checker.build_summary().unwrap();
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
            let concrete = checker.build_summary().unwrap();
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
                let invalid = |summary: &BorrowSummary<'_>| {
                    summary.mutable_inputs.iter().any(|poststate| {
                        values
                            .leaves(&poststate.value, ValueOccurrence::Summary)
                            .iter()
                            .any(|leaf| leaf.payload.invalidated)
                    })
                };
                assert!(invalid(&concrete));
                assert!(
                    invalid(&fallback),
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
        let summary = checker.build_summary().unwrap();
        let values = SourceValues::new(&db, ValueLimits::default());
        let leaf = values
            .leaves(&summary.result, ValueOccurrence::Summary)
            .pop()
            .unwrap();
        let check = |source: &SourceExpr<'_>| {
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
}
