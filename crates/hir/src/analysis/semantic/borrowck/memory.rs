//! Memory effects use the same regions and input substitution as borrow results.
use std::cmp::Reverse;

use super::validity::NativeValidity;
use super::{
    ir::{AvailabilityRequirement, AvailabilitySummary, BorrowSummary, MemoryAccess},
    solver::Borrowck,
    summary::{CallInputs, SourceInstantiations},
};
use crate::analysis::semantic::diagnostics::SemanticDiagnostic;
use crate::analysis::{
    semantic::{
        FieldIndex, SemOrigin,
        capability::{
            external::{ExternalOrigin, ExternalSource, ReferentContract},
            footprint::AccessExtent,
            guard::Guard,
            handle::{
                AddressOccurrence, HandleAddressSpace, OpaqueHandleContract, OpaqueHandleRef,
            },
            index::{BinderScope, IndexExpr},
            loan::LoanRef,
            path::{Projection, RegionPath, StructuralPath},
            region::{RegionRoot, RegionSet},
            semantics::CapabilityClass,
            source::{InputOrigin, SourceExpr},
            state::BorrowState,
            value::{Guarded, ValueInterner, ValueLimits},
        },
        normalized::NValueId,
    },
    ty::{
        corelib::{
            IntrinsicContract, IntrinsicMemoryExtent, IntrinsicMemoryTarget,
            IntrinsicPointerReturn, MemoryAccessKind, contract_metadata_kind, intrinsic_contract,
        },
        ty_check::BodyOwner,
        ty_def::{BorrowKind, TyId},
        ty_is_copy,
    },
};

#[derive(Clone)]
pub(super) struct ResolvedMemoryAccess<'db> {
    pub invalidated: NativeValidity<'db>,
    pub access: MemoryAccess<'db>,
    pub authority: Vec<Guarded<'db, LoanRef<'db>>>,
}

impl<'db> Borrowck<'db> {
    pub fn call_memory_accesses(
        &mut self,
        state: &BorrowState<'db>,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Vec<ResolvedMemoryAccess<'db>>, SemanticDiagnostic<'db>> {
        let Some(call) = self.calls.get(&result).cloned() else {
            return Ok(Vec::new());
        };
        let mut resolved = Vec::new();
        let mut instantiations = SourceInstantiations::new(state, result, inputs);
        for access in &call.summary.accesses {
            let mut invalidated = NativeValidity::default();
            let mut authority = Vec::new();
            let mut regions = Vec::new();
            for source_region in [&access.region, &access.authorizers] {
                let mut region = RegionSet::empty(source_region.scope());
                for clause in source_region.clauses() {
                    let Some(guard) = self.instantiate_guard(&clause.guard, result, inputs)? else {
                        continue;
                    };
                    let source = SourceExpr::from_place(&clause.payload)
                        .expect("verified memory summary source");
                    if let ExternalOrigin::Input(input) = &source.source.origin
                        && matches!(input.origin(), InputOrigin::Place(_))
                        && input.dereferences().is_empty()
                        && source.source.dereferences().is_empty()
                        && let Some(arg) = inputs.args.get(input.param() as usize)
                        && state.value(arg.value).shape().direct(self.db).is_none()
                    {
                        continue;
                    }

                    let target = instantiations.resolve(self, &source, guard.scope())?;
                    invalidated |= target.invalidated;
                    authority.extend(
                        target
                            .parents
                            .into_iter()
                            .chain(target.traversed)
                            .filter_map(|parent| {
                                if access.kind.borrow_kind() == BorrowKind::Mut
                                    && self.inventory.loans[parent.payload.id.0].kind()
                                        != BorrowKind::Mut
                                    && source_region == &access.region
                                {
                                    return None;
                                }
                                Some(Guarded {
                                    guard: parent
                                        .guard
                                        .and(&guard.in_scope(parent.guard.scope()))?,
                                    payload: parent.payload,
                                })
                            }),
                    );
                    region = region.union(
                        &target
                            .region
                            .with_guard(&guard)
                            .close_existentials(source_region.scope()),
                    );
                }
                regions.push(region);
            }
            let authorizers = regions.pop().expect("authorizers");
            let region = regions.pop().expect("access target");
            resolved.push(ResolvedMemoryAccess {
                invalidated,
                access: MemoryAccess {
                    kind: access.kind,
                    extent: self.instantiate_extent(access.extent, inputs),
                    region,
                    authorizers,
                },
                authority,
            });
        }
        Ok(resolved)
    }

    pub fn body_memory_accesses(&self) -> Vec<MemoryAccess<'db>> {
        let mut accesses = Vec::new();
        for operation in self.operations.iter().flatten() {
            accesses.extend(operation.accesses.iter().map(|access| MemoryAccess {
                kind: access.kind,
                extent: AccessExtent::Typed,
                region: access.region.clone(),
                authorizers: RegionSet::empty(access.region.scope()),
            }));
            for resolved in &operation.calls {
                let mut access = resolved.access.clone();
                for parent in self.ancestors(resolved.authority.iter().cloned()) {
                    let region = self.inventory.loans[parent.payload.id.0]
                        .region(self.db, &parent.payload, parent.guard.scope())
                        .with_guard(&parent.guard);
                    access.authorizers = access
                        .authorizers
                        .union(&region.close_existentials(access.authorizers.scope()));
                }
                accesses.push(access);
            }
        }
        accesses
    }
}

impl<'db> Borrowck<'db> {
    /// Trusted intrinsic identities supply precise effects even when the source
    /// declaration is bodyless, or its implementation uses raw address arithmetic.
    pub fn intrinsic_summary(&self) -> Result<Option<BorrowSummary<'db>>, SemanticDiagnostic<'db>> {
        let BodyOwner::Func(func) = self.instance.key(self.db).owner(self.db) else {
            return Ok(None);
        };
        let contract = intrinsic_contract(self.db, func).or_else(|| {
            (contract_metadata_kind(self.db, func).is_some()
                && self
                    .instance
                    .key(self.db)
                    .subst(self.db)
                    .generic_args(self.db)
                    .iter()
                    .any(|ty| ty.as_contract(self.db).is_some()))
            .then_some(IntrinsicContract {
                pointer_return: None,
                memory: Some(&[]),
            })
        });
        let Some(contract) = contract else {
            return Ok(None);
        };
        let scope = BinderScope::default();
        let origin = SemOrigin::Body(self.body.template_owner);
        let input = |param, pointer_field: bool| {
            let slot = if pointer_field {
                StructuralPath::new([Projection::Field(FieldIndex(0))])
            } else {
                StructuralPath::default()
            };
            self.inventory
                .inputs
                .iter()
                .find(|target| match &target.source.origin {
                    ExternalOrigin::Input(input) => {
                        input.param() == param
                            && input.dereferences().is_empty()
                            && target.source.dereferences().is_empty()
                            && match input.origin() {
                                InputOrigin::Slot { slot: actual, .. } => actual == &slot,
                                InputOrigin::Place(_) => slot.is_empty(),
                            }
                    }
                    _ => false,
                })
                .map(|target| SourceExpr {
                    invalidated: false,
                    source: target.source.clone(),
                    path: RegionPath::default(),
                    views: Default::default(),
                })
        };
        let mut values = ValueInterner::new(self.db, ValueLimits::default());
        let shape = self.shape(self.instance.normalized_result_ty(self.db))?;
        let mut result = values.empty(shape, &scope);
        if let Some(pointer_return) = contract.pointer_return {
            let mut source = match pointer_return {
                IntrinsicPointerReturn::FreshMemory => {
                    let handle = OpaqueHandleContract::for_ty(
                        self.db,
                        self.instance
                            .key(self.db)
                            .impl_env(self.db)
                            .normalization_scope(self.db),
                        self.instance.assumptions(self.db),
                        self.instance.normalized_result_ty(self.db),
                    )
                    .map_err(|_| {
                        self.internal_diag(origin, "invalid allocation result type".into())
                    })?
                    .expect("allocation returns a raw pointer");
                    SourceExpr {
                        invalidated: false,
                        source: ExternalSource::allocation(
                            self.db,
                            OpaqueHandleRef {
                                contract: handle,
                                occurrence: AddressOccurrence::Summary(0),
                                arguments: Box::new([]),
                            },
                        ),
                        path: RegionPath::default(),
                        views: Default::default(),
                    }
                }
                other => input(0, other == IntrinsicPointerReturn::InputMemArrayElem).ok_or_else(
                    || self.internal_diag(origin, "intrinsic has no pointer input".into()),
                )?,
            };
            let target_ty = shape
                .direct(self.db)
                .expect("intrinsic pointer result")
                .target_ty;
            match pointer_return {
                IntrinsicPointerReturn::InputArrayElem => {
                    source.path = source
                        .path
                        .appended(Projection::Index(IndexExpr::FormalValue(1)))
                }
                IntrinsicPointerReturn::InputMemArrayElem => {
                    source = SourceExpr {
                        invalidated: false,
                        source: ExternalSource::memory(
                            self.db,
                            source,
                            target_ty,
                            Some((target_ty, IndexExpr::FormalValue(1))),
                        ),
                        path: RegionPath::default(),
                        views: Default::default(),
                    };
                }
                IntrinsicPointerReturn::InputPointee => {
                    source = SourceExpr {
                        invalidated: false,
                        source: ExternalSource::memory(
                            self.db,
                            source,
                            target_ty,
                            Some((TyId::u8(self.db), IndexExpr::FormalValue(1))),
                        ),
                        path: RegionPath::default(),
                        views: Default::default(),
                    };
                }
                IntrinsicPointerReturn::FreshMemory => {}
            }
            result = values.with_direct(
                &result,
                vec![Guarded {
                    guard: Guard::always(&scope),
                    payload: source,
                }],
            );
        }
        let mut accesses = Vec::new();
        let contracts = contract.memory.unwrap_or_default();
        let authorizers = contracts
            .iter()
            .filter_map(|access| match access.target {
                IntrinsicMemoryTarget::Value(param) => input(param, false),
                _ => None,
            })
            .fold(RegionSet::empty(&scope), |region, source| {
                region.union(&RegionSet::singleton(
                    &scope,
                    RegionRoot::External(source.source),
                    source.path,
                ))
            });
        for access in contracts {
            if let IntrinsicMemoryTarget::Value(param) = access.target {
                let ty = self.summary_param_ty(param).ok_or_else(|| {
                    self.internal_diag(origin, "intrinsic access has no input parameter".into())
                })?;
                // A value access reads through native argument transport. Plain
                // copied values are already checked by the caller's operand
                // access; a copied raw address does not read its pointee.
                if ty.as_capability(self.db).is_none() {
                    continue;
                }
            }
            let source = match access.target {
                IntrinsicMemoryTarget::Address { input, space } => Some(SourceExpr {
                    source: ExternalSource::unknown(
                        ReferentContract::new(
                            self.db,
                            TyId::u256(self.db),
                            HandleAddressSpace::Known(space),
                        ),
                        AddressOccurrence::Summary(input),
                        Box::new([IndexExpr::FormalValue(input)]),
                    ),
                    path: RegionPath::default(),
                    views: Default::default(),
                    invalidated: false,
                }),
                IntrinsicMemoryTarget::WholeSpace(space) => Some(SourceExpr {
                    // This uncertain source ranges over every compatible slot.
                    // Unknown extent prevents field/offset separation, and this
                    // may-write can never establish definite initialization.
                    source: ExternalSource::unknown(
                        ReferentContract::new(
                            self.db,
                            TyId::u256(self.db),
                            HandleAddressSpace::Known(space),
                        ),
                        AddressOccurrence::Summary(0),
                        Box::new([]),
                    ),
                    path: RegionPath::default(),
                    views: Default::default(),
                    invalidated: false,
                }),
                IntrinsicMemoryTarget::Value(param) | IntrinsicMemoryTarget::Pointee(param) => {
                    input(param, false)
                }
            }
            .ok_or_else(|| {
                self.internal_diag(
                    origin,
                    "intrinsic memory access has no input referent".into(),
                )
            })?;
            accesses.push(MemoryAccess {
                kind: access.kind,
                extent: match access.extent {
                    IntrinsicMemoryExtent::Typed => AccessExtent::Typed,
                    IntrinsicMemoryExtent::Bytes(len) => AccessExtent::Bytes(IndexExpr::Const(len)),
                    IntrinsicMemoryExtent::Argument(param) => {
                        AccessExtent::Bytes(IndexExpr::FormalValue(param))
                    }
                    IntrinsicMemoryExtent::Unknown => AccessExtent::Unknown,
                },
                region: RegionSet::singleton(
                    &scope,
                    RegionRoot::External(source.source),
                    source.path,
                ),
                authorizers: if matches!(
                    access.target,
                    IntrinsicMemoryTarget::Value(_) | IntrinsicMemoryTarget::WholeSpace(_)
                ) {
                    // A capability to issue an external call supplies no
                    // authority over native loans into arbitrary current state.
                    RegionSet::empty(&scope)
                } else {
                    authorizers.clone()
                },
            });
        }
        Ok(Some(BorrowSummary {
            native_requirements: RegionSet::empty(&scope),
            may_return: !self.instance.is_intrinsically_never_returning(self.db),
            result,
            mutable_inputs: Vec::new(),
            requirements: Vec::new(),
            availability: AvailabilitySummary {
                incoming: accesses
                    .iter()
                    .map(|access| AvailabilityRequirement {
                        kind: access.kind,
                        extent: access.extent,
                        region: access.region.clone(),
                    })
                    .collect(),
                ..AvailabilitySummary::empty()
            },
            accesses,
        }))
    }
}

impl<'db> Borrowck<'db> {
    pub fn signature_memory_accesses(
        &self,
        choice: u32,
    ) -> Result<Vec<MemoryAccess<'db>>, SemanticDiagnostic<'db>> {
        let scope = BinderScope::default();
        let mut accesses = Vec::new();
        let mut read_authorizers = RegionSet::empty(&scope);
        let mut write_authorizers = RegionSet::empty(&scope);
        for input in &self.inventory.inputs {
            if input.source.param().is_none() {
                continue;
            }
            let region = RegionSet::singleton(
                &input.scope,
                RegionRoot::External(input.source.clone()),
                RegionPath::default(),
            )
            .substitute(self.db, &input.scope.freshening(&scope))
            .close_existentials(&scope);
            let native = input
                .classes
                .iter()
                .any(|class| matches!(class, CapabilityClass::Borrow(_) | CapabilityClass::View));
            let mutable = input
                .classes
                .contains(&CapabilityClass::Borrow(BorrowKind::Mut));
            if native {
                read_authorizers = read_authorizers.union(&region);
            }
            if mutable {
                write_authorizers = write_authorizers.union(&region);
            }
            let pointer = input.classes.contains(&CapabilityClass::Pointer);
            let consume = pointer
                && !ty_is_copy(
                    self.db,
                    self.instance
                        .key(self.db)
                        .impl_env(self.db)
                        .normalization_scope(self.db),
                    input.ty,
                    self.instance.assumptions(self.db),
                );
            for (kind, possible) in [
                (MemoryAccessKind::Read, native || pointer),
                (MemoryAccessKind::Write, mutable || pointer),
                (MemoryAccessKind::Move, consume),
            ] {
                if possible {
                    accesses.push(MemoryAccess {
                        kind,
                        extent: AccessExtent::Typed,
                        region: region.clone(),
                        authorizers: RegionSet::empty(&scope),
                    });
                }
            }
        }
        // A bare signature bounds neither raw addresses nor their extent. This
        // contract may touch existing storage in any space, even without inputs.
        // Compiler-defined intrinsics bypass this fallback with explicit effects.
        let unknown = RegionSet::singleton(
            &scope,
            RegionRoot::External(ExternalSource::unknown(
                ReferentContract::new(self.db, TyId::u8(self.db), HandleAddressSpace::Unspecified),
                AddressOccurrence::Summary(choice),
                Box::new([]),
            )),
            RegionPath::default(),
        );
        for kind in [
            MemoryAccessKind::Read,
            MemoryAccessKind::Write,
            MemoryAccessKind::Move,
        ] {
            accesses.push(MemoryAccess {
                kind,
                extent: AccessExtent::Unknown,
                region: unknown.clone(),
                authorizers: if kind == MemoryAccessKind::Read {
                    read_authorizers.clone()
                } else {
                    write_authorizers.clone()
                },
            });
        }
        // Prefer the strongest conflict diagnostic without joining distinct effects.
        accesses.sort_by_key(|access| Reverse(access.kind));
        Ok(accesses)
    }
}
