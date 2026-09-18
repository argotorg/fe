//! Memory effects use the same regions and input substitution as borrow results.
use super::{
    ir::{BorrowSummary, MemoryAccess, SemanticBorrowDiagnostic},
    solver::Borrowck,
    summary::CallInputs,
};
use crate::analysis::{
    semantic::{
        FieldIndex, SemOrigin,
        capability::{
            external::{ExternalOrigin, ExternalSource},
            guard::Guard,
            handle::{AddressOccurrence, OpaqueHandleContract, OpaqueHandleRef},
            index::{BinderScope, IndexExpr},
            loan::LoanRef,
            path::{Projection, RegionPath, StructuralPath},
            region::{RegionRoot, RegionSet},
            semantics::CapabilityClass,
            source::{InputOrigin, SourceExpr},
            state::BorrowState,
            value::{Guarded, ValueInterner, ValueLimits},
        },
        normalized::{NExpr, NStatementKind, NValueId},
    },
    ty::{
        corelib::{
            IntrinsicMemoryProjection, IntrinsicPointerReturn, MemoryAccessKind,
            intrinsic_contract, is_std_evm_effect_method,
        },
        ty_check::BodyOwner,
        ty_def::{BorrowKind, TyId},
    },
};

pub(super) struct ResolvedMemoryAccess<'db> {
    pub access: MemoryAccess<'db>,
    pub authority: Vec<Guarded<'db, LoanRef<'db>>>,
}

impl<'db> Borrowck<'db> {
    pub fn call_memory_accesses(
        &mut self,
        state: &BorrowState<'db>,
        result: NValueId,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Vec<ResolvedMemoryAccess<'db>>, SemanticBorrowDiagnostic<'db>> {
        let Some(call) = self.calls.get(&result).cloned() else {
            return Ok(Vec::new());
        };
        let mut resolved = Vec::new();
        for access in &call.summary.accesses {
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

                    let target =
                        self.instantiate_source(state, &source, result, guard.scope(), inputs)?;
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
                access: MemoryAccess {
                    kind: access.kind,
                    region,
                    authorizers,
                },
                authority,
            });
        }
        Ok(resolved)
    }

    pub fn body_memory_accesses(
        &mut self,
    ) -> Result<Vec<MemoryAccess<'db>>, SemanticBorrowDiagnostic<'db>> {
        let mut accesses = Vec::new();
        for block in 0..self.body.blocks.len() {
            let statements = self.body.blocks[block].statements.clone();
            for (index, statement) in statements.iter().enumerate() {
                let Some(state) = self.before[block].get(index).cloned() else {
                    break;
                };
                let (kind, place) = match &statement.kind {
                    NStatementKind::Store { destination, .. } => {
                        (MemoryAccessKind::Write, destination)
                    }
                    NStatementKind::Define {
                        expr: NExpr::Load { place, .. } | NExpr::MakeView { place, .. },
                        ..
                    } => (MemoryAccessKind::Read, place),
                    NStatementKind::Define {
                        expr: NExpr::Borrow { place, kind, .. },
                        ..
                    } => (
                        if *kind == crate::analysis::ty::ty_def::BorrowKind::Mut {
                            MemoryAccessKind::MutAccess
                        } else {
                            MemoryAccessKind::Read
                        },
                        place,
                    ),
                    NStatementKind::Define {
                        result,
                        expr:
                            NExpr::Call {
                                args, effect_args, ..
                            },
                    } => {
                        accesses.extend(
                            self.call_memory_accesses(
                                &state,
                                *result,
                                CallInputs {
                                    args,
                                    effects: effect_args,
                                    origin: statement.origin,
                                },
                            )?
                            .into_iter()
                            .map(|mut resolved| {
                                for parent in self.ancestors(resolved.authority) {
                                    let region = self.inventory.loans[parent.payload.id.0]
                                        .region(self.db, &parent.payload, parent.guard.scope())
                                        .with_guard(&parent.guard);
                                    resolved.access.authorizers = resolved
                                        .access
                                        .authorizers
                                        .union(&region.close_existentials(
                                            resolved.access.authorizers.scope(),
                                        ));
                                }
                                resolved.access
                            }),
                        );
                        continue;
                    }
                    _ => continue,
                };
                let region = self
                    .resolve_region(&state, place)
                    .with_guard(&state.guard().in_scope(&BinderScope::default()));
                accesses.push(MemoryAccess {
                    kind,
                    region,
                    authorizers: RegionSet::empty(&BinderScope::default()),
                });
            }
        }
        Ok(accesses)
    }
}

impl<'db> Borrowck<'db> {
    /// Trusted intrinsic identities supply precise effects even when the source
    /// declaration is bodyless, or its implementation uses raw address arithmetic.
    pub fn intrinsic_summary(
        &self,
    ) -> Result<Option<BorrowSummary<'db>>, SemanticBorrowDiagnostic<'db>> {
        let BodyOwner::Func(func) = self.instance.key(self.db).owner(self.db) else {
            return Ok(None);
        };
        let Some(contract) = intrinsic_contract(self.db, func) else {
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
            .filter(|access| access.projection == IntrinsicMemoryProjection::Value)
            .filter_map(|access| input(access.input, false))
            .fold(RegionSet::empty(&scope), |region, source| {
                region.union(&RegionSet::singleton(
                    &scope,
                    RegionRoot::External(source.source),
                    source.path,
                ))
            });
        for access in contracts {
            let source = input(access.input, false).ok_or_else(|| {
                self.internal_diag(
                    origin,
                    "intrinsic memory access has no input referent".into(),
                )
            })?;
            accesses.push(MemoryAccess {
                kind: access.kind,
                region: RegionSet::singleton(
                    &scope,
                    RegionRoot::External(source.source),
                    source.path,
                ),
                authorizers: if access.projection == IntrinsicMemoryProjection::Pointee {
                    authorizers.clone()
                } else {
                    RegionSet::empty(&scope)
                },
            });
        }
        Ok(Some(BorrowSummary {
            may_return: !self.instance.is_intrinsically_never_returning(self.db),
            result,
            mutable_inputs: Vec::new(),
            requirements: Vec::new(),
            accesses,
        }))
    }
}

impl<'db> Borrowck<'db> {
    pub fn signature_memory_accesses(
        &self,
        mut choice: u32,
    ) -> Result<Vec<MemoryAccess<'db>>, SemanticBorrowDiagnostic<'db>> {
        let scope = BinderScope::default();
        let evm_receiver = matches!(self.instance.key(self.db).owner(self.db), BodyOwner::Func(func) if is_std_evm_effect_method(self.db, func));
        let mut accesses = Vec::new();
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
            let Some(kind) = input
                .classes
                .iter()
                .filter_map(|class| match class {
                    CapabilityClass::Borrow(BorrowKind::Ref) | CapabilityClass::View => {
                        Some(MemoryAccessKind::Read)
                    }
                    CapabilityClass::Borrow(BorrowKind::Mut) | CapabilityClass::Pointer => {
                        Some(MemoryAccessKind::Write)
                    }
                    CapabilityClass::Handle => None,
                })
                .max()
            else {
                continue;
            };
            accesses.push(MemoryAccess {
                kind,
                region: region.clone(),
                authorizers: RegionSet::empty(&scope),
            });
            if input.source.contract.is_abstract(self.db)
                && !(evm_receiver && input.source.param() == Some(0))
            {
                let handle_ty = TyId::ptr_to(self.db, TyId::u8(self.db));
                let contract = OpaqueHandleContract::for_ty(
                    self.db,
                    self.instance
                        .key(self.db)
                        .impl_env(self.db)
                        .normalization_scope(self.db),
                    self.instance.assumptions(self.db),
                    handle_ty,
                )
                .expect("builtin pointer contract")
                .expect("raw memory pointer");
                let unknown = ExternalSource::opaque(
                    self.db,
                    OpaqueHandleRef {
                        contract,
                        occurrence: AddressOccurrence::Summary(choice),
                        arguments: Box::new([]),
                    },
                );
                choice += 1;
                accesses.push(MemoryAccess {
                    kind: MemoryAccessKind::Write,
                    region: RegionSet::singleton(
                        &scope,
                        RegionRoot::External(unknown),
                        RegionPath::default(),
                    ),
                    authorizers: region,
                });
            }
        }
        Ok(accesses)
    }
}
