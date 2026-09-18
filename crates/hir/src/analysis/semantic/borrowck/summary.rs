//! Structural return values and mutable-input poststates use the same algebra.
use std::collections::BTreeMap;

use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BorrowActivation, FieldIndex, SemOrigin, SemanticInstance,
            capability::{
                external::{ExternalOrigin, ExternalSource, ReferentContract},
                guard::{Guard, ValueOccurrence},
                handle::{OpaqueHandleContract, OpaqueHandleOccurrence, OpaqueHandleRef},
                index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
                loan::{CapabilityRef, LoanDef, LoanId, LoanRef},
                path::{Projection, RegionPath, StructuralPath},
                region::{RegionRoot, RegionSet},
                semantics::{CapabilityClass, CapabilitySemantics},
                shape::ShapeChildren,
                source::{InputOrigin, SourceExpr},
                state::{BorrowState, CapabilityValue, CapabilityValues},
                value::{Guarded, IndexPayload, ValueId, ValueInterner, ValueLimits},
            },
            get_or_build_semantic_instance,
            normalized::{
                NEffectArg, NEffectArgValue, NExpr, NOperand, NStatement, NStatementKind,
                NTerminatorKind, NValueDefinition, NValueId,
            },
        },
        ty::{
            provider::ProviderKind,
            ty_def::{BorrowKind, CapabilityKind, TyData, TyId},
        },
    },
    semantic::ProviderSource,
};

use super::{
    check::{
        BorrowSummaryComputation, provisional_borrow_summary_voucher,
        semantic_borrow_summary_voucher,
    },
    inventory::referent_contract,
    ir::{BorrowSummary, InputPoststate, SemanticBorrowDiagKind, SemanticBorrowDiagnostic},
    solver::{BorrowSummaryMode, Borrowck, Resolution},
};

pub type SourceValue<'db> = ValueId<'db, SourceExpr<'db>>;
type SourceValues<'db> = ValueInterner<'db, SourceExpr<'db>>;

#[derive(Clone, Copy)]
pub(super) struct CallInputs<'a, 'db> {
    pub args: &'a [NOperand],
    pub effects: &'a [NEffectArg<'db>],
    pub origin: SemOrigin<'db>,
}

#[derive(Clone)]
pub(super) struct CallSummary<'db> {
    pub summary: BorrowSummary<'db>,
    updates: Vec<CapabilityValue<'db>>,
}

impl<'db> Borrowck<'db> {
    pub fn prepare_calls(&mut self) -> Result<(), SemanticBorrowDiagnostic<'db>> {
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
            let voucher = match self.summary_mode {
                BorrowSummaryMode::Final => semantic_borrow_summary_voucher(self.db, instance),
                BorrowSummaryMode::Provisional => {
                    provisional_borrow_summary_voucher(self.db, instance)
                }
            }?;
            if self.blocked.is_none() {
                self.blocked = voucher.blocked;
            }
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
                                CapabilityClass::Handle => {
                                    CapabilityRef::Handle(RegionSet::empty(scope))
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
                    external.push((leaf.payload.source, leaf.guard.scope().clone()));
                }
            }
            external.extend(summary.mutable_inputs.iter().map(|update| {
                (
                    update.destination.source.clone(),
                    update.value.scope().clone(),
                )
            }));
            for (source, scope) in external {
                let base = match source.origin {
                    ExternalOrigin::OpaqueHandle(mut handle) => {
                        let OpaqueHandleOccurrence::Summary(choice) = handle.occurrence else {
                            return Err(self.internal_diag(
                                origin,
                                "summary retains a local handle occurrence".into(),
                            ));
                        };
                        handle.arguments = handle
                            .arguments
                            .iter()
                            .copied()
                            .chain(
                                self.inventory
                                    .loops
                                    .for_value(&self.body, result)
                                    .map(IndexExpr::Iteration),
                            )
                            .collect();
                        handle.occurrence = OpaqueHandleOccurrence::Value {
                            instance: self.instance,
                            value: result,
                            choice,
                        };
                        ExternalSource::opaque(self.db, handle)
                    }
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
                    | ExternalOrigin::Provider { .. }
                    | ExternalOrigin::Local(_) => continue,
                };
                bases.push((base, scope));
            }
            self.calls.insert(result, CallSummary { summary, updates });
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
    ) -> Result<BorrowSummaryComputation<'db>, SemanticBorrowDiagnostic<'db>> {
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
            });
        }
        self.solve()?;
        let summary = self.build_summary()?;
        Ok(BorrowSummaryComputation {
            summary: Some(summary),
            blocked: self.blocked,
        })
    }

    pub fn build_summary(&mut self) -> Result<BorrowSummary<'db>, SemanticBorrowDiagnostic<'db>> {
        // Provisional summaries supply provider facts needed for body admission
        // and definite assignment. Boundary policy must not suppress those facts.
        if self.summary_mode == BorrowSummaryMode::Final {
            super::noesc::check_solved_body(self)?;
        }
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
                    true,
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
                    false,
                    origin,
                    &mut values,
                    &mut choices,
                    &mut handles,
                )?;
                update.value = values.join(&update.value, &sources);
            }
        }
        for update in &mut updates {
            if let ExternalOrigin::OpaqueHandle(handle) = &mut update.destination.source.origin {
                let next = handles.len().try_into().expect("summary handle count");
                handle.occurrence = OpaqueHandleOccurrence::Summary(
                    *handles.entry(handle.occurrence).or_insert(next),
                );
            }
        }
        let summary = BorrowSummary {
            may_return,
            result,
            mutable_inputs: updates,
        };
        self.verify_summary(&summary)?;
        Ok(summary)
    }

    fn summarize_value(
        &self,
        value: &CapabilityValue<'db>,
        returning: bool,
        origin: SemOrigin<'db>,
        values: &mut SourceValues<'db>,
        choices: &mut BTreeMap<ValueOccurrence, u32>,
        handles: &mut BTreeMap<OpaqueHandleOccurrence<'db>, u32>,
    ) -> Result<SourceValue<'db>, SemanticBorrowDiagnostic<'db>> {
        let mut failure = None;
        let result = self.inventory.values.map_payloads(value, values, |semantics, _, entry, domain| {
            let region = entry.payload.region(self.db, &self.inventory.loans, entry.guard.scope()).with_guard(domain);
            let mut sources = Vec::new();
            for clause in region.clauses() {
                let Some(mut payload) = SourceExpr::from_place(&clause.payload) else {
                    failure.get_or_insert_with(|| self.diag(SemanticBorrowDiagKind::InvalidReturnBorrow, origin,
                        match &clause.payload.root {
                            RegionRoot::Root { root, .. } => {
                                let name = match &self.body.roots[root.index()].kind {
                                    crate::analysis::semantic::normalized::NRootKind::LocalSlot { binding: Some(binding) } => self.body.template_owner.body(self.db)
                                        .map(|body| binding.pretty_name_in_body(self.db, body)).unwrap_or_else(|| format!("%r{}", root.index())),
                                    _ => format!("%r{}", root.index()),
                                };
                                if !returning { format!("cannot leave a borrow of local `{name}` in caller-accessible storage") }
                                else if value.shape().direct(self.db).is_some() { format!("cannot return a borrow to local `{name}`") }
                                else { format!("cannot return a value that holds a borrow of local `{name}`") }
                            }
                            _ if !returning => "cannot leave a borrow of local storage in caller-accessible storage".into(),
                            _ => "cannot return a borrow to local storage".into(),
                        }));
                    continue;
                };
                if returning && matches!(semantics.class, CapabilityClass::Borrow(_)) && matches!(payload.source.origin, ExternalOrigin::Provider { .. }) {
                    failure.get_or_insert_with(|| self.diag(SemanticBorrowDiagKind::InvalidReturnBorrow, origin,
                        "cannot return a borrow derived from an effect parameter".into()));
                    continue;
                }
                if let ExternalOrigin::OpaqueHandle(source) = &mut payload.source.origin {
                    let next = handles.len().try_into().expect("summary handle count");
                    source.occurrence = OpaqueHandleOccurrence::Summary(*handles.entry(source.occurrence).or_insert(next));
                }
                let mut scope = clause.guard.scope().clone();
                let mut substitutions = BTreeMap::new();
                for index in clause.guard.indices().into_iter().chain(payload.indices()) {
                    if matches!(index, IndexExpr::Runtime(_) | IndexExpr::Iteration(_)) {
                        substitutions.entry(index).or_insert_with(|| {
                            match match index { IndexExpr::Runtime(value) => self.index(value), index => index } {
                                IndexExpr::Runtime(actual) => match self.body.values[actual.index()].definition {
                                    NValueDefinition::EntryParam { param } => IndexExpr::FormalValue(param),
                                    NValueDefinition::BlockParam { .. } | NValueDefinition::Statement { .. } => {
                                        let (nested, witness) = scope.bind(IndexNamespace::Existential);
                                        scope = nested;
                                        witness
                                    }
                                },
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
                let subst = IndexSubst::new(clause.guard.scope(), &scope, substitutions).expect("summary local index abstraction");
                let Some(guard) = clause.guard.substitute(&subst).and_then(|guard| guard.map_occurrences(|occurrence| {
                    if let ValueOccurrence::Value(value) = occurrence
                        && let NValueDefinition::EntryParam { param } = self.body.values[value.index()].definition {
                        return ValueOccurrence::Argument(param);
                    }
                    if matches!(occurrence, ValueOccurrence::Argument(_) | ValueOccurrence::Summary) { return occurrence }
                    let next = choices.len().try_into().expect("summary choice count");
                    ValueOccurrence::SummaryChoice(*choices.entry(occurrence).or_insert(next))
                })) else { continue };
                sources.push(Guarded { guard, payload: payload.substitute(self.db, &subst) });
            }
            sources
        });
        if let Some(failure) = failure {
            return Err(failure);
        }
        Ok(result)
    }

    fn summary_param_ty(&self, param: u32) -> Option<TyId<'db>> {
        self.body.values.iter().find_map(|value| {
            matches!(value.definition,
            NValueDefinition::EntryParam { param: actual } if actual == param)
            .then_some(value.ty)
        })
    }

    fn summary_project_ty(
        &self,
        mut ty: TyId<'db>,
        path: &[Projection<IndexExpr<'db>>],
    ) -> Option<TyId<'db>> {
        for step in path {
            if let Some((CapabilityKind::View, target)) = ty.as_capability(self.db) {
                ty = target;
            }
            ty = match step {
                Projection::Field(field) => *self
                    .instance
                    .normalized_field_types(self.db, ty)
                    .get(usize::from(field.0))?,
                Projection::VariantField { variant, field } => *self
                    .instance
                    .normalized_enum_variant_field_tys(self.db, ty, *variant)
                    .get(usize::from(field.0))?,
                Projection::Index(_) if ty.is_array(self.db) => {
                    *ty.generic_args(self.db).first()?
                }
                Projection::Index(_) => return None,
            };
        }
        Some(ty)
    }

    fn verify_source(
        &self,
        source: &SourceExpr<'db>,
        scope: &BinderScope,
        requested: Option<CapabilitySemantics<'db>>,
        writable: bool,
    ) -> Result<TyId<'db>, SemanticBorrowDiagnostic<'db>> {
        let invalid = |message: &str| {
            self.internal_diag(
                SemOrigin::Body(self.body.template_owner),
                format!("invalid structural summary: {message}"),
            )
        };
        let db = self.db;
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
                        .is_some_and(|ty| ty.is_integral(db)) =>
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
            ExternalOrigin::Local(_) => return Err(invalid("source contains local storage")),
            ExternalOrigin::Input(input) => {
                let param_ty = self
                    .summary_param_ty(input.param())
                    .ok_or_else(|| invalid("source parameter does not exist"))?;
                if external.is_reachable() {
                    let candidates = self
                        .inventory
                        .inputs
                        .iter()
                        .filter(|target| target.source.param() == Some(input.param()));
                    let can_write = candidates.clone().any(|target| target.writable);
                    if writable && !can_write {
                        return Err(invalid("reachable destination is immutable"));
                    }
                    let class = requested
                        .map_or(CapabilityClass::Borrow(BorrowKind::Mut), |semantics| {
                            semantics.class
                        });
                    let compatible = candidates.clone().any(|target| match class {
                        CapabilityClass::Borrow(BorrowKind::Mut) => {
                            target.class == CapabilityClass::Borrow(BorrowKind::Mut)
                        }
                        CapabilityClass::Borrow(BorrowKind::Ref) | CapabilityClass::View => {
                            target.class != CapabilityClass::Handle
                        }
                        CapabilityClass::Handle => target.class == CapabilityClass::Handle,
                    });
                    if !compatible {
                        return Err(invalid(
                            "reachable source has no compatible input authority",
                        ));
                    }
                    (external.contract.ty, class, external.contract.address_space)
                } else {
                    let semantics = match input.origin() {
                        InputOrigin::Place(_) => self.shape(param_ty)?.direct(db),
                        InputOrigin::Slot { slot, .. } => {
                            let param_ty = param_ty.as_view(db).unwrap_or(param_ty);
                            let ty = self
                                .summary_project_ty(param_ty, slot.as_slice())
                                .ok_or_else(|| invalid("input slot path is invalid"))?;
                            self.shape(ty)?.direct(db)
                        }
                    }
                    .ok_or_else(|| invalid("input source does not select a capability"))?;
                    let contract = referent_contract(db, self.instance, semantics)
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
            ExternalOrigin::OpaqueHandle(handle) => {
                if !matches!(handle.occurrence, OpaqueHandleOccurrence::Summary(_)) {
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
                    CapabilityClass::Handle,
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
                let slot = self
                    .summary_project_ty(ty, path.as_slice())
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
                CapabilityClass::Borrow(BorrowKind::Mut) | CapabilityClass::Handle
            )
        {
            return Err(invalid("poststate destination is immutable"));
        }
        if let Some(requested) = requested
            && matches!(requested.class, CapabilityClass::Borrow(BorrowKind::Mut))
            && !matches!(
                class,
                CapabilityClass::Borrow(BorrowKind::Mut) | CapabilityClass::Handle
            )
        {
            return Err(invalid("mutable result comes from shared authority"));
        }
        let mut target = self
            .summary_project_ty(external.contract.ty, source.path.as_slice())
            .ok_or_else(|| invalid("referent projection is invalid"))?;
        for view in source.views.iter() {
            let suffix = source
                .path
                .as_slice()
                .get(view.depth..)
                .ok_or_else(|| invalid("referent view anchor is invalid"))?;
            let original = self
                .summary_project_ty(view.repack.source_ty(db), suffix)
                .ok_or_else(|| invalid("referent view source is invalid"))?;
            if target != original {
                return Err(invalid("referent view starts at the wrong type"));
            }
            target = self
                .summary_project_ty(view.repack.target_ty(db), suffix)
                .ok_or_else(|| invalid("referent view target is invalid"))?;
        }
        if let Some(requested) = requested {
            if target != requested.target_ty {
                return Err(invalid("source target differs from its capability slot"));
            }
            if requested.class == CapabilityClass::Handle {
                let contract = referent_contract(db, self.instance, requested)
                    .map_err(|_| invalid("result handle has an unresolved contract"))?;
                if contract.address_space != external.contract.address_space {
                    return Err(invalid("result handle changes address space"));
                }
            }
        }
        Ok(target)
    }

    fn verify_summary(
        &self,
        summary: &BorrowSummary<'db>,
    ) -> Result<(), SemanticBorrowDiagnostic<'db>> {
        let values = SourceValues::new(self.db, ValueLimits::default());
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
                if leaf
                    .guard
                    .occurrences()
                    .iter()
                    .any(|occurrence| match occurrence {
                        ValueOccurrence::Value(_)
                        | ValueOccurrence::Root(_)
                        | ValueOccurrence::CallChoice { .. } => true,
                        ValueOccurrence::Argument(param) => self.summary_param_ty(*param).is_none(),
                        ValueOccurrence::Summary | ValueOccurrence::SummaryChoice(_) => false,
                    })
                    || leaf.guard.indices().iter().any(|index| match index {
                        IndexExpr::FormalValue(param) => !self
                            .summary_param_ty(*param)
                            .is_some_and(|ty| ty.is_integral(self.db)),
                        _ => leaf.guard.scope().validate(*index).is_err(),
                    })
                {
                    return Err(self.internal_diag(
                        SemOrigin::Body(self.body.template_owner),
                        "summary guard contains an invalid occurrence or scalar parameter".into(),
                    ));
                }
                if matches!(&leaf.payload.source.origin, ExternalOrigin::OpaqueHandle(source) if !matches!(source.occurrence, OpaqueHandleOccurrence::Summary(_)))
                {
                    return Err(self.internal_diag(
                        SemOrigin::Body(self.body.template_owner),
                        "summary retains a callee-local handle occurrence".into(),
                    ));
                }
                if leaf
                    .guard
                    .indices()
                    .into_iter()
                    .chain(leaf.payload.indices())
                    .any(|index| matches!(index, IndexExpr::Runtime(_) | IndexExpr::Iteration(_)))
                {
                    return Err(self.internal_diag(
                        SemOrigin::Body(self.body.template_owner),
                        "summary retains a callee-local value index".into(),
                    ));
                }
            }
        }
        Ok(())
    }

    pub fn transfer_call(
        &mut self,
        state: &mut BorrowState<'db>,
        result: NValueId,
        statement: &NStatement<'db>,
    ) -> Result<CapabilityValue<'db>, SemanticBorrowDiagnostic<'db>> {
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
        state
            .write_regions(
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
    ) -> Result<CapabilityValue<'db>, SemanticBorrowDiagnostic<'db>> {
        let CallInputs { args, origin, .. } = inputs;
        let mut values = CapabilityValues::new(self.db, ValueLimits::default());
        let sources = SourceValues::new(self.db, ValueLimits::default());
        let mut error = None;
        let instantiated =
            sources.map_payloads(value, &mut values, |semantics, path, entry, domain| {
                let subst = IndexSubst::new(
                    entry.guard.scope(),
                    entry.guard.scope(),
                    args.iter().enumerate().map(|(param, arg)| {
                        (
                            IndexExpr::FormalValue(param.try_into().expect("parameter count")),
                            self.index(arg.value),
                        )
                    }),
                )
                .expect("call index substitution");
                let source = entry.payload.substitute(self.db, &subst);
                let Some(guard) = domain.substitute(&subst).and_then(|guard| {
                    guard.map_occurrences(|occurrence| match occurrence {
                        ValueOccurrence::Argument(param) => args.get(param as usize).map_or(
                            ValueOccurrence::CallChoice {
                                result,
                                choice: param,
                            },
                            |arg| ValueOccurrence::Value(arg.value),
                        ),
                        ValueOccurrence::SummaryChoice(choice) => {
                            ValueOccurrence::CallChoice { result, choice }
                        }
                        ValueOccurrence::Summary => ValueOccurrence::Value(result),
                        ValueOccurrence::Value(_)
                        | ValueOccurrence::Root(_)
                        | ValueOccurrence::CallChoice { .. } => {
                            error.get_or_insert_with(|| {
                                self.internal_diag(
                                    origin,
                                    "summary retains a local enum occurrence".into(),
                                )
                            });
                            occurrence
                        }
                    })
                }) else {
                    return Vec::new();
                };
                let resolved =
                    match self.instantiate_source(state, &source, result, guard.scope(), inputs) {
                        Ok(resolved) => resolved,
                        Err(failure) => {
                            error.get_or_insert(failure);
                            return Vec::new();
                        }
                    };
                let region = resolved.region.with_guard(&guard);
                let payload = match semantics.class {
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
                            let guard = parent.guard.and(&guard.in_scope(parent.guard.scope()))?;
                            Some(Guarded {
                                guard,
                                payload: parent.payload,
                            })
                        });
                        self.extend_loan(result, &reference, &region, parents.collect());
                        CapabilityRef::borrow(kind, reference)
                    }
                    CapabilityClass::View => CapabilityRef::view(region, resolved.parents),
                    CapabilityClass::Handle => CapabilityRef::Handle(region),
                };
                vec![Guarded { guard, payload }]
            });
        if let Some(error) = error {
            return Err(error);
        }
        Ok(instantiated)
    }

    fn instantiate_source(
        &mut self,
        state: &BorrowState<'db>,
        source: &SourceExpr<'db>,
        result: NValueId,
        scope: &BinderScope,
        inputs: CallInputs<'_, 'db>,
    ) -> Result<Resolution<'db>, SemanticBorrowDiagnostic<'db>> {
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
        let path = &source.path;
        let external = &source.source;
        if let ExternalOrigin::Input(input) = &external.origin
            && external.is_reachable()
        {
            let mut resolved = self.reachable_input(
                state,
                input.param(),
                external.contract.ty,
                path,
                scope,
                inputs,
            )?;
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
            ExternalOrigin::OpaqueHandle(handle) => {
                let OpaqueHandleOccurrence::Summary(choice) = handle.occurrence else {
                    return Err(self.internal_diag(
                        origin,
                        "summary retains a local handle occurrence".into(),
                    ));
                };
                let mut handle = handle.clone();
                handle.arguments = handle
                    .arguments
                    .iter()
                    .copied()
                    .chain(
                        self.inventory
                            .loops
                            .for_value(&self.body, result)
                            .map(IndexExpr::Iteration),
                    )
                    .collect();
                handle.occurrence = OpaqueHandleOccurrence::Value {
                    instance: self.instance,
                    value: result,
                    choice,
                };
                let source = ExternalSource::opaque(self.db, handle);
                let ty = source.contract.ty;
                (
                    Resolution {
                        region: RegionSet::singleton(
                            scope,
                            RegionRoot::External(source),
                            RegionPath::default(),
                        ),
                        parents: Vec::new(),
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
                    let effect = effects
                        .iter()
                        .find(|effect| effect.binding_idx == requirement_idx)
                        .ok_or_else(|| {
                            self.internal_diag(
                                origin,
                                "summary effect source has no call argument".into(),
                            )
                        })?;
                    let mut resolved = match &effect.arg {
                        NEffectArgValue::Place(place) => self.resolve_place(state, place),
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
                            region: RegionSet::singleton(
                                scope,
                                RegionRoot::External(source),
                                RegionPath::default(),
                            ),
                            parents: Vec::new(),
                        },
                        ty,
                    )
                }
            }
            ExternalOrigin::Input(input) => {
                let param = input.param();
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
                            let value = self.read_region(
                                state,
                                &region,
                                shape,
                                ValueOccurrence::Argument(param),
                                origin,
                            )?;
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
                        if let Some(semantics) = value.shape().direct(self.db)
                            && semantics.class == CapabilityClass::View
                        {
                            let region = self.resolve_capability(&value).region;
                            let shape = self.shape(semantics.target_ty)?;
                            value = self.read_region(
                                state,
                                &region,
                                shape,
                                ValueOccurrence::Argument(param),
                                origin,
                            )?;
                        }
                        let Some(selected) = self.inventory.values.project(
                            &value,
                            slot,
                            ValueOccurrence::Argument(param),
                        ) else {
                            return Ok(Resolution::empty(scope));
                        };
                        let semantics = selected.shape().direct(self.db).ok_or_else(|| {
                            self.internal_diag(
                                origin,
                                "summary slot does not select a capability".into(),
                            )
                        })?;
                        (self.resolve_capability(&selected), semantics.target_ty)
                    }
                }
            }
        };
        if external.is_reachable() {
            resolved = self.reachable_region(
                state,
                &resolved.region,
                target_ty,
                external.contract.ty,
                scope,
                origin,
            )?;
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
                resolved = self.resolve_capability(&selected);
                target_ty = semantics.target_ty;
            }
        }
        resolved.region = resolved.region.project(path).with_relative_views(
            self.db,
            &source.views,
            path.as_slice().len(),
        );
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

pub(super) fn signature_summary<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    opaque: bool,
) -> Result<BorrowSummary<'db>, SemanticBorrowDiagnostic<'db>> {
    let body = super::inventory::signature_body(db, instance);
    let checker = Borrowck::new_with_body(db, instance, body, BorrowSummaryMode::Provisional)?;
    let mut values = SourceValues::new(db, ValueLimits::default());
    let shape = checker.shape(instance.normalized_result_ty(db))?;
    let mut candidates = Vec::new();
    if opaque {
        for input in &checker.inventory.inputs {
            let mut pending = vec![(input.ty, RegionPath::default(), Guard::always(&input.scope))];
            while let Some((ty, path, guard)) = pending.pop() {
                candidates.push(Candidate {
                    source: SourceExpr {
                        views: Default::default(),
                        source: input.source.clone(),
                        path: path.clone(),
                    },
                    guard: guard.clone(),
                    ty,
                    class: input.class,
                });
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
    let mut opaque_choice = 0;
    let mut failure = None;
    let mut build = |shape, scope: &BinderScope| {
        values.from_shape(shape, scope, |semantics, _, scope| {
            let mut sources: Vec<_> = candidates
                .iter()
                .filter(|candidate| {
                    (candidate.ty == semantics.target_ty
                        || matches!(
                            candidate.ty.base_ty(db).data(db),
                            TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
                        ))
                        && match semantics.class {
                            CapabilityClass::Borrow(BorrowKind::Mut) => {
                                candidate.class == CapabilityClass::Borrow(BorrowKind::Mut)
                            }
                            CapabilityClass::Borrow(BorrowKind::Ref) | CapabilityClass::View => {
                                candidate.class != CapabilityClass::Handle
                            }
                            CapabilityClass::Handle => candidate.class == CapabilityClass::Handle,
                        }
                })
                .filter_map(|candidate| {
                    let subst = candidate.guard.scope().freshening(scope);
                    Some(Guarded {
                        guard: candidate.guard.substitute(&subst)?,
                        payload: if candidate.ty != semantics.target_ty {
                            let mut source = candidate.source.source.clone().widen();
                            source.contract = ReferentContract::new(
                                db,
                                semantics.target_ty,
                                source.contract.address_space,
                            );
                            SourceExpr {
                                views: Default::default(),
                                source,
                                path: RegionPath::default(),
                            }
                        } else {
                            candidate.source.substitute(db, &subst)
                        },
                    })
                })
                .collect();
            if opaque && semantics.class == CapabilityClass::Handle {
                match OpaqueHandleContract::for_ty(
                    db,
                    instance.key(db).impl_env(db).normalization_scope(db),
                    instance.assumptions(db),
                    semantics.representation_ty,
                ) {
                    Ok(Some(contract)) => {
                        let choice = opaque_choice;
                        opaque_choice += 1;
                        sources.push(Guarded {
                            guard: Guard::always(scope),
                            payload: SourceExpr {
                                views: Default::default(),
                                source: ExternalSource::opaque(
                                    db,
                                    OpaqueHandleRef {
                                        contract,
                                        occurrence: OpaqueHandleOccurrence::Summary(choice),
                                        arguments: scope.variables().collect(),
                                    },
                                ),
                                path: RegionPath::default(),
                            },
                        });
                    }
                    Ok(None) | Err(_) => {
                        failure.get_or_insert_with(|| {
                            checker.internal_diag(
                                SemOrigin::Body(checker.body.template_owner),
                                "opaque signature has an unresolved handle origin".into(),
                            )
                        });
                    }
                }
            }
            sources
        })
    };
    let result = build(shape, &BinderScope::default());
    let mutable_inputs = checker
        .inventory
        .inputs
        .iter()
        .filter(|input| input.writable && input.shape.contains_capability(db))
        .map(|input| InputPoststate {
            destination: SourceExpr {
                views: Default::default(),
                source: input.source.clone(),
                path: RegionPath::default(),
            },
            value: build(input.shape, &input.scope),
        })
        .collect();
    if let Some(failure) = failure {
        return Err(failure);
    }
    let summary = BorrowSummary {
        may_return: opaque && !instance.is_intrinsically_never_returning(db),
        result,
        mutable_inputs,
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
            ty::{ProviderAddressSpace, ty_check::BodyOwner},
        },
        hir_def::ItemKind,
        test_db::HirAnalysisTestDb,
    };

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
