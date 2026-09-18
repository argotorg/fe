//! Transport, storage and escape policy over solved capability values.
use crate::analysis::{
    HirAnalysisDb,
    diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
    semantic::{
        SemOrigin, SemanticInstance,
        capability::{
            external::{ExternalOrigin, ExternalSource},
            guard::ValueOccurrence,
            index::IndexSubst,
            path::Projection,
            region::{RegionRoot, RegionSet, SymbolicPlace},
            semantics::{CapabilitySemantics, StorageClass, TransportClass},
            source::{InputOrigin, SourceExpr},
            state::{BorrowState, CapabilityValue},
        },
        normalized::{NEffectArg, NEffectArgValue, NExpr, NOperand, NRootKind, NStatementKind},
    },
    ty::{
        ProviderAddressSpace,
        ty_check::{BodyOwner, EffectPassMode},
        ty_def::{BorrowKind, TyId},
    },
};
use cranelift_entity::EntityRef;

use super::{
    check::SemanticAnalysisError,
    diagnostics::operand_origin,
    events::CapabilityTraversal,
    ir::{
        BlockedSemanticBody, BorrowDiagnosticId, BoundaryRequirement, BoundaryRule,
        SemanticBorrowCheckResult, SemanticBorrowDiagKind, SemanticBorrowDiagnostic,
        SemanticBorrowDiagnosticSpan, SemanticNormalizationFailure,
    },
    solver::Borrowck,
    summary::CallInputs,
};

pub fn check_semantic_boundaries<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), SemanticAnalysisError<'db>> {
    match semantic_boundary_check_query(db, instance) {
        SemanticBorrowCheckResult::Ok => Ok(()),
        SemanticBorrowCheckResult::Blocked(body) => Err(SemanticAnalysisError::Blocked(body)),
        SemanticBorrowCheckResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

#[salsa::tracked]
pub(super) fn semantic_boundary_check_query<'db>(
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
    match check(borrowck) {
        Ok(Some(blocked)) => SemanticBorrowCheckResult::Blocked(blocked),
        Ok(None) => SemanticBorrowCheckResult::Ok,
        Err(diag) => SemanticBorrowCheckResult::Err(BorrowDiagnosticId::new(db, diag)),
    }
}

/// Ordinary parameters cannot carry native mutable access to provider storage.
/// A receiver explicitly supports provider transport. Read-only views transport
/// their contents; they do not turn nested native handles into plain values.
#[derive(Clone, Copy)]
pub(super) enum ParamTransportMode {
    Ordinary,
    Receiver,
}

#[derive(Clone, Copy)]
pub(super) enum Boundary {
    CallArg(ParamTransportMode),
    EffectArg {
        mode: EffectPassMode,
        required_mut: bool,
    },
    Return,
    Retained,
}

fn check<'db>(
    mut borrowck: Borrowck<'db>,
) -> Result<Option<BlockedSemanticBody<'db>>, SemanticBorrowDiagnostic<'db>> {
    borrowck.solve()?;
    if let Some(blocked) = borrowck.blocked.clone() {
        return Ok(Some(blocked));
    }
    // Export validates return values and caller-visible poststates as well as
    // each statement boundary. Provisional exports remain policy-independent.
    borrowck.build_summary()?;
    Ok(None)
}

pub(super) fn check_solved_body<'db>(
    borrowck: &mut Borrowck<'db>,
) -> Result<Vec<BoundaryRequirement<'db>>, SemanticBorrowDiagnostic<'db>> {
    BoundaryCheck {
        borrowck,
        requirements: Vec::new(),
    }
    .run()
}

struct BoundaryCheck<'a, 'db> {
    borrowck: &'a mut Borrowck<'db>,
    requirements: Vec<BoundaryRequirement<'db>>,
}

impl<'db> BoundaryCheck<'_, 'db> {
    fn run(mut self) -> Result<Vec<BoundaryRequirement<'db>>, SemanticBorrowDiagnostic<'db>> {
        for index in 0..self.borrowck.body.blocks.len() {
            let statements = self.borrowck.body.blocks[index].statements.clone();
            let states = self.borrowck.before[index].clone();
            for (statement, state) in statements.iter().zip(&states) {
                match &statement.kind {
                    NStatementKind::Define {
                        result,
                        expr:
                            NExpr::Call {
                                callee,
                                args,
                                effect_args,
                                ..
                            },
                    } => {
                        for (param, arg) in args.iter().copied().enumerate() {
                            let mode = match callee.key.owner(self.borrowck.db) {
                                BodyOwner::Func(func)
                                    if param == 0
                                        && func.receiver_ty(self.borrowck.db).is_some() =>
                                {
                                    ParamTransportMode::Receiver
                                }
                                _ => ParamTransportMode::Ordinary,
                            };
                            self.check_call_arg(
                                state,
                                statement.origin,
                                arg,
                                Boundary::CallArg(mode),
                            )?;
                        }
                        for effect in effect_args {
                            self.check_effect_arg(state, statement.origin, effect)?;
                        }
                        // Instantiate against the pre-call state: a call's
                        // poststate must not erase the inputs it required.
                        let requirements = self
                            .borrowck
                            .calls
                            .get(result)
                            .map(|call| call.summary.requirements.clone())
                            .unwrap_or_default();
                        for mut requirement in requirements {
                            requirement.region = self
                                .borrowck
                                .instantiate_requirement(
                                    state,
                                    &requirement.region,
                                    *result,
                                    CallInputs {
                                        args,
                                        effects: effect_args,
                                        origin: statement.origin,
                                    },
                                )?
                                .with_guard(state.guard());
                            if let Some(populated) = &requirement.populated {
                                requirement.populated =
                                    Some(self.borrowck.instantiate_requirement(
                                        state,
                                        populated,
                                        *result,
                                        CallInputs {
                                            args,
                                            effects: effect_args,
                                            origin: statement.origin,
                                        },
                                    )?);
                            }
                            self.check_requirement(requirement)?;
                        }
                    }
                    NStatementKind::Store { destination, value } => {
                        let region = self
                            .borrowck
                            .resolve_region(state, destination)
                            .with_guard(state.guard());
                        self.check_write(statement.origin, &region, destination.ty)?;
                        self.check_stored_value(
                            state.value(value.value),
                            statement.origin,
                            *value,
                            &region,
                        )?;
                    }
                    NStatementKind::Define { .. } => {}
                }
            }
        }
        Ok(self.requirements)
    }

    fn require(
        &mut self,
        rule: BoundaryRule<'db>,
        origin: SemOrigin<'db>,
        region: RegionSet<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        self.check_requirement(BoundaryRequirement {
            rule,
            instance: self.borrowck.instance,
            origin,
            region,
            populated: None,
        })
    }

    fn check_requirement(
        &mut self,
        mut requirement: BoundaryRequirement<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        if let Some(populated) = requirement.populated.take() {
            for clause in populated.clauses() {
                // Source and destination families are independently quantified.
                // Scalar formal indices and enum occurrences still correlate.
                let fresh = clause.guard.scope().freshening(requirement.region.scope());
                let guard = clause
                    .guard
                    .substitute(&fresh)
                    .expect("populated source scope");
                let lift = IndexSubst::new(requirement.region.scope(), guard.scope(), [])
                    .expect("boundary condition scope");
                let region = requirement
                    .region
                    .substitute(self.borrowck.db, &lift)
                    .with_guard(&guard)
                    .close_existentials(requirement.region.scope());
                let mut conditional = requirement.clone();
                conditional.region = region;
                if let RegionRoot::External(source) = &clause.payload.root
                    && let ExternalOrigin::Input(input) = &source.origin
                    && (source.is_reachable()
                        || matches!(input.origin(), InputOrigin::Slot { slot, .. }
                            if slot.as_slice().iter().any(|step| matches!(step, Projection::VariantField { .. })))
                        || input.dereferences().iter().any(|path| {
                            path.as_slice()
                                .iter()
                                .any(|step| matches!(step, Projection::VariantField { .. }))
                        }))
                {
                    conditional.populated =
                        Some(RegionSet::new(populated.scope(), [clause.clone()]));
                    // Optional input slots may disappear when the caller
                    // supplies an empty variant. A direct native input is
                    // necessarily populated and must satisfy the rule now.
                    self.check_spaces(conditional, true)?;
                } else {
                    self.check_spaces(conditional, false)?;
                }
            }
            return Ok(());
        }
        self.check_spaces(requirement, false)
    }

    fn check_spaces(
        &mut self,
        mut requirement: BoundaryRequirement<'db>,
        conditional: bool,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let mut unresolved = Vec::new();
        for clause in requirement.region.clauses() {
            let Some(space) = clause.payload.root.address_space().known() else {
                unresolved.push(clause.clone());
                continue;
            };
            let violation = match requirement.rule {
                BoundaryRule::MemoryTransport(ty) if space != ProviderAddressSpace::Memory => {
                    Some((
                        SemanticBorrowDiagKind::TransportViolation,
                        format!(
                            "cannot pass `{}` from {} as function argument",
                            ty.pretty_print(self.borrowck.db),
                            space.pretty()
                        ),
                    ))
                }
                BoundaryRule::Writable
                    if matches!(
                        space,
                        ProviderAddressSpace::Calldata | ProviderAddressSpace::Code
                    ) =>
                {
                    Some((
                        SemanticBorrowDiagKind::StorageViolation,
                        format!("cannot write to {}", space.pretty()),
                    ))
                }
                BoundaryRule::BorrowedStore(ty)
                    if matches!(
                        space,
                        ProviderAddressSpace::Storage | ProviderAddressSpace::Transient
                    ) =>
                {
                    Some((
                        SemanticBorrowDiagKind::NoEscViolation,
                        format!(
                            "cannot store `{}` in {}",
                            ty.pretty_print(self.borrowck.db),
                            space.pretty()
                        ),
                    ))
                }
                _ => None,
            };
            if let Some((kind, message)) = violation {
                if conditional {
                    unresolved.push(clause.clone());
                    continue;
                }
                return Err(SemanticBorrowDiagnostic::new(
                    requirement.instance,
                    kind,
                    message,
                    SemanticBorrowDiagnosticSpan::Origin {
                        owner: requirement
                            .instance
                            .key(self.borrowck.db)
                            .owner(self.borrowck.db),
                        origin: requirement.origin,
                    },
                ));
            }
        }
        // Known spaces have discharged the contract. Unknown spaces remain
        // explicit input preconditions, including through receiver forwarding.
        requirement.region = RegionSet::new(requirement.region.scope(), unresolved);
        if !requirement.region.is_empty() {
            self.requirements.push(requirement);
        }
        Ok(())
    }

    fn check_call_arg(
        &mut self,
        state: &BorrowState<'db>,
        origin: SemOrigin<'db>,
        operand: NOperand,
        boundary: Boundary,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let capabilities = self.borrowck.capabilities(
            state,
            state.value(operand.value),
            ValueOccurrence::Value(operand.value),
            origin,
            CapabilityTraversal::Held,
        )?;
        for capability in capabilities {
            if capability.semantics.transport != TransportClass::MemoryBorrow {
                continue;
            }
            let region = capability.region.with_guard(state.guard());
            if matches!(boundary, Boundary::CallArg(ParamTransportMode::Receiver)) {
                self.check_write(origin, &region, capability.semantics.target_ty)?;
            } else {
                let ty = self.borrowck.body.values[operand.value.index()].ty;
                self.require(
                    BoundaryRule::MemoryTransport(ty),
                    operand_origin(operand, origin),
                    region,
                )?;
            }
        }
        Ok(())
    }

    fn check_effect_arg(
        &mut self,
        state: &BorrowState<'db>,
        origin: SemOrigin<'db>,
        effect: &NEffectArg<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let boundary = Boundary::EffectArg {
            mode: effect.pass_mode,
            required_mut: effect.required_mut,
        };
        let Boundary::EffectArg { mode, required_mut } = boundary else {
            unreachable!()
        };
        let kind = if required_mut {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        };
        let (value, traversal) = match (&effect.arg, mode) {
            (NEffectArgValue::Place(place), EffectPassMode::ByPlace) => {
                let region = self
                    .borrowck
                    .resolve_region(state, place)
                    .with_guard(state.guard());
                if required_mut {
                    self.check_write(origin, &region, place.ty)?;
                }
                let shape = self.borrowck.shape(place.ty)?;
                (
                    self.borrowck.read_region(
                        state,
                        &region,
                        shape,
                        ValueOccurrence::Argument(effect.binding_idx),
                        origin,
                    )?,
                    CapabilityTraversal::Held,
                )
            }
            (NEffectArgValue::Value(value), EffectPassMode::ByValue) => (
                state.value(value.value).clone(),
                CapabilityTraversal::Effect(kind),
            ),
            // The temporary is fresh memory; its native handles retain their
            // referent spaces and authority.
            (NEffectArgValue::Value(value), EffectPassMode::ByTempPlace) => {
                (state.value(value.value).clone(), CapabilityTraversal::Held)
            }
            _ => {
                return Err(self
                    .borrowck
                    .internal_diag(origin, "invalid effect boundary transport".into()));
            }
        };
        for capability in self.borrowck.capabilities(
            state,
            &value,
            ValueOccurrence::Argument(effect.binding_idx),
            origin,
            traversal,
        )? {
            if capability.access == Some(BorrowKind::Mut) {
                self.check_write(
                    origin,
                    &capability.region.with_guard(state.guard()),
                    capability.semantics.target_ty,
                )?;
            }
        }
        Ok(())
    }

    fn check_write(
        &mut self,
        origin: SemOrigin<'db>,
        region: &RegionSet<'db>,
        target: TyId<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        // An empty receiver has no bytes to modify. Its methods' other regions
        // keep their own effect and store contracts.
        if target.is_zero_sized(self.borrowck.db) {
            return Ok(());
        }
        self.require(BoundaryRule::Writable, origin, region.clone())
    }

    fn check_stored_value(
        &mut self,
        value: &CapabilityValue<'db>,
        origin: SemOrigin<'db>,
        operand: NOperand,
        region: &RegionSet<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        for leaf in self
            .borrowck
            .inventory
            .values
            .leaves(value, ValueOccurrence::Value(operand.value))
        {
            if leaf.semantics.storage != StorageClass::Borrowed {
                continue;
            }
            // Source families and destination families quantify independently.
            // Preserve enum guards so an empty variant introduces no contract.
            let fresh = leaf.guard.scope().freshening(region.scope());
            let guard = leaf.guard.substitute(&fresh).expect("stored leaf scope");
            let lift = IndexSubst::new(region.scope(), guard.scope(), [])
                .expect("store destination scope");
            let region = region
                .substitute(self.borrowck.db, &lift)
                .with_guard(&guard)
                .close_existentials(region.scope());
            let ty = self.borrowck.body.values[operand.value.index()].ty;
            let populated = leaf
                .payload
                .region(
                    self.borrowck.db,
                    &self.borrowck.inventory.loans,
                    leaf.guard.scope(),
                )
                .substitute(self.borrowck.db, &fresh)
                .with_guard(&guard)
                .close_existentials(region.scope());
            self.check_requirement(BoundaryRequirement {
                rule: BoundaryRule::BorrowedStore(ty),
                instance: self.borrowck.instance,
                origin,
                region,
                populated: Some(populated),
            })?;
        }
        Ok(())
    }
}

/// Validate actual outgoing leaves before converting their roots to summary
/// sources. This also applies to caller-visible storage retained after return.
pub(super) fn escape_source<'db>(
    borrowck: &Borrowck<'db>,
    value: &CapabilityValue<'db>,
    semantics: CapabilitySemantics<'db>,
    place: &SymbolicPlace<'db>,
    boundary: Boundary,
    origin: SemOrigin<'db>,
) -> Result<SourceExpr<'db>, SemanticBorrowDiagnostic<'db>> {
    let message = match &place.root {
        RegionRoot::Root { root, .. } => {
            let name = match &borrowck.body.roots[root.index()].kind {
                NRootKind::LocalSlot {
                    binding: Some(binding),
                } => borrowck
                    .body
                    .template_owner
                    .body(borrowck.db)
                    .map(|body| binding.pretty_name_in_body(borrowck.db, body))
                    .unwrap_or_else(|| format!("%r{}", root.index())),
                _ => format!("%r{}", root.index()),
            };
            if matches!(boundary, Boundary::Retained) {
                format!("cannot leave a borrow of local `{name}` in caller-accessible storage")
            } else if value.shape().direct(borrowck.db).is_some() {
                format!("cannot return a borrow to local `{name}`")
            } else {
                format!("cannot return a value that holds a borrow of local `{name}`")
            }
        }
        RegionRoot::Value(_)
        | RegionRoot::External(ExternalSource {
            origin: ExternalOrigin::Local(_),
            ..
        }) => {
            if matches!(boundary, Boundary::Retained) {
                "cannot leave a borrow of local storage in caller-accessible storage".into()
            } else {
                "cannot return a borrow to local storage".into()
            }
        }
        RegionRoot::External(source)
            if matches!(boundary, Boundary::Return)
                && semantics.storage == StorageClass::Borrowed
                && matches!(source.origin, ExternalOrigin::Provider { .. }) =>
        {
            "cannot return a borrow derived from an effect parameter".into()
        }
        RegionRoot::External(_) => {
            return SourceExpr::from_place(place).ok_or_else(|| {
                borrowck.internal_diag(
                    origin,
                    "outgoing capability lacks an external source".into(),
                )
            });
        }
    };
    Err(borrowck.diag(SemanticBorrowDiagKind::InvalidReturnBorrow, origin, message))
}
