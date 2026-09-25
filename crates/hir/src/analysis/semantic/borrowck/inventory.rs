//! Immutable structural input and borrow-occurrence inventory.
use std::collections::{BTreeMap, BTreeSet, HashSet};

use super::control::LoopRegions;

use cranelift_entity::EntityRef;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            BorrowActivation, Mutability, SemOrigin, SemanticInstance,
            capability::{
                external::{ExternalOrigin, ExternalSource, ReferentContract},
                guard::Guard,
                handle::{
                    AddressOccurrence, HandleAddressSpace, OpaqueHandleContract, OpaqueHandleRef,
                    OpaqueWriteSite, SeedOrigin,
                },
                index::{BinderScope, IndexExpr, IndexNamespace, IndexSubst},
                loan::{CapabilityRef, LoanDef, LoanId, LoanRef},
                opaque::OpaqueWrite,
                path::{RegionPath, StructuralPath},
                region::{ProviderRegionId, RegionRoot, RegionSet},
                semantics::{CapabilityClass, CapabilitySemantics},
                shape::{ShapeError, ShapeId, capability_shape},
                source::InputSource,
                state::{BorrowState, CapabilityValue, CapabilityValues},
                value::{Guarded, ValueLimits},
            },
            instantiated_effect_env,
            normalized::{
                HandleOrigin, NBlock, NBlockId, NExpr, NRootId, NRootKind, NStatementKind,
                NTerminator, NTerminatorKind, NValue, NValueDefinition, NValueId, NormalizedBody,
                copied_scalar_ty,
            },
        },
        ty::{
            ProviderAddressSpace,
            corelib::{is_std_evm_effect_method, is_std_evm_effect_trait},
            ty_check::BodyOwner,
            ty_def::{BorrowKind, TyId},
        },
    },
    hir_def::FuncParamMode,
};

#[derive(Clone)]
pub(super) struct InputTarget<'db> {
    pub source: ExternalSource<'db>,
    pub scope: BinderScope,
    pub ty: TyId<'db>,
    pub shape: ShapeId<'db>,
    pub writable: bool,
    pub classes: Vec<CapabilityClass>,
}

pub(super) struct Inventory<'db> {
    pub loops: LoopRegions,
    pub values: CapabilityValues<'db>,
    pub shapes: Vec<ShapeId<'db>>,
    pub roots: Vec<RegionRoot<'db>>,
    pub loans: Vec<LoanDef<'db>>,
    /// Native input loans carry a separation precondition. Calls discharge it
    /// against the exported accesses using the caller's concrete provenance.
    pub input_loans: BTreeSet<LoanId>,
    pub definitions: BTreeMap<NValueId, CapabilityValue<'db>>,
    pub inputs: Vec<InputTarget<'db>>,
    pub entry: BorrowState<'db>,
    /// Rebuilt with entry storage after discovery; call transfer visits only
    /// the physical cells belonging to its allocation occurrences.
    pub allocation_cells: BTreeMap<AddressOccurrence<'db>, Vec<RegionRoot<'db>>>,
    external_loans: BTreeMap<(ExternalSource<'db>, bool), LoanId>,
}

#[derive(Clone)]
enum InputOrigin<'db> {
    Parameter(u32),
    Referent(ExternalSource<'db>),
}

/// These classify initial contents, independently of inventory/discovery order.
#[derive(Clone, Copy)]
enum CellSeed {
    EntryContents,
    FreshUninitialized,
    UnknownBytes,
}

impl CellSeed {
    fn for_source(source: &ExternalSource<'_>) -> Self {
        if source.is_fresh_allocation() {
            return Self::FreshUninitialized;
        }
        match &source.origin {
            ExternalOrigin::Input(_) | ExternalOrigin::Provider { .. } => Self::EntryContents,
            ExternalOrigin::Memory { base, .. }
                if matches!(Self::for_source(&base.source), Self::EntryContents) =>
            {
                Self::EntryContents
            }
            _ => Self::UnknownBytes,
        }
    }
}

struct InputBuilder<'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    values: CapabilityValues<'db>,
    loans: Vec<LoanDef<'db>>,
    input_loans: BTreeMap<(ExternalSource<'db>, bool), LoanId>,
    targets: BTreeMap<ExternalSource<'db>, InputTarget<'db>>,
    storage: BTreeMap<RegionRoot<'db>, CapabilityValue<'db>>,
    pending: Vec<(InputTarget<'db>, InputOrigin<'db>, Vec<TyId<'db>>)>,
}

impl<'db> Inventory<'db> {
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        body: &NormalizedBody<'db>,
    ) -> Result<Self, ShapeError<'db>> {
        let instance = body.owner;
        let loops = LoopRegions::new(body);
        let mut inputs = InputBuilder {
            db,
            instance,
            values: CapabilityValues::new(db, ValueLimits::default()),
            loans: Vec::new(),
            input_loans: BTreeMap::new(),
            targets: BTreeMap::new(),
            storage: BTreeMap::new(),
            pending: Vec::new(),
        };
        let shapes = body
            .values
            .iter()
            .map(|value| inputs.shape(value.ty))
            .collect::<Result<Vec<_>, _>>()?;
        let scope = BinderScope::default();
        let mut roots = Vec::new();
        let param_modes: Vec<_> = match body.template_owner {
            BodyOwner::Func(func) => func.params(db).map(|param| param.mode(db)).collect(),
            _ => Vec::new(),
        };
        for (index, root) in body.roots.iter().enumerate() {
            let shape = inputs.shape(root.ty)?;
            let region = match &root.kind {
                NRootKind::ParamPlace { param }
                    if param_modes.get(*param as usize) != Some(&FuncParamMode::Own) =>
                {
                    RegionRoot::External(ExternalSource::input(
                        InputSource::place(*param),
                        ReferentContract::new(db, root.ty, HandleAddressSpace::Unspecified),
                        false,
                    ))
                }
                NRootKind::Provider { binding } => RegionRoot::External(ExternalSource::provider(
                    db,
                    ProviderRegionId::new(db, binding.clone()),
                    root.ty,
                )),
                NRootKind::Temporary { .. }
                | NRootKind::LocalSlot { .. }
                | NRootKind::ParamPlace { .. }
                | NRootKind::CapabilityRepresentation { .. } => RegionRoot::Root {
                    root: NRootId::new(index),
                    contract: ReferentContract::new(
                        db,
                        root.ty,
                        HandleAddressSpace::Known(root.address_space),
                    ),
                },
            };
            let contents = if let NRootKind::ParamPlace { param } = root.kind {
                inputs.value(shape, &scope, InputOrigin::Parameter(param), &[root.ty])?
            } else if let RegionRoot::External(source) = &region {
                assert_eq!(
                    shape,
                    inputs.shape(source.contract.ty)?,
                    "provider root target mismatch: {} vs {}; {:?}",
                    root.ty.pretty_print(db),
                    source.contract.ty.pretty_print(db),
                    root.kind
                );
                inputs.register(
                    source.clone(),
                    scope.clone(),
                    CapabilityClass::Handle,
                    true,
                    &[root.ty],
                )?;
                inputs.value(
                    shape,
                    &scope,
                    InputOrigin::Referent(source.clone()),
                    &[root.ty],
                )?
            } else {
                inputs.values.empty(shape, &scope)
            };
            inputs.storage.insert(region.clone(), contents);
            roots.push(region);
        }
        let mut entry_values = Vec::new();
        for (index, value) in body.values.iter().enumerate() {
            if let NValueDefinition::EntryParam { param } = value.definition {
                let value = inputs.value(
                    shapes[index],
                    &scope,
                    InputOrigin::Parameter(param),
                    &[value.ty],
                )?;
                entry_values.push((NValueId::new(index), value));
            }
        }
        for statement in body.blocks.iter().flat_map(|block| &block.statements) {
            if let NStatementKind::Define {
                result,
                expr:
                    NExpr::MakeHandle {
                        origin: HandleOrigin::Opaque(contract),
                        ..
                    },
            } = &statement.kind
            {
                let source = ExternalSource::opaque(
                    db,
                    OpaqueHandleRef {
                        contract: *contract,
                        occurrence: AddressOccurrence::Value {
                            instance,
                            value: *result,
                            choice: 0,
                        },
                        arguments: loops
                            .for_value(body, *result)
                            .map(IndexExpr::Iteration)
                            .into_iter()
                            .collect(),
                    },
                );
                inputs.register(source, scope.clone(), CapabilityClass::Handle, true, &[])?;
            }
        }
        inputs.finish_storage()?;
        let mut definitions = BTreeMap::new();
        for block in &body.blocks {
            for statement in &block.statements {
                let NStatementKind::Define { result, expr } = &statement.kind else {
                    continue;
                };
                let activation = match expr {
                    NExpr::Borrow { activation, .. } => *activation,
                    NExpr::Call { .. } => BorrowActivation::Immediate,
                    _ => continue,
                };
                let template = inputs.values.from_shape(
                    shapes[result.index()],
                    &scope,
                    |semantics, _, scope| {
                        let payload = match semantics.class {
                            CapabilityClass::Borrow(kind) => {
                                let id = LoanId(inputs.loans.len());
                                let (loan, args, _) = LoanDef::with_occurrence_arguments(
                                    kind,
                                    activation,
                                    statement.origin,
                                    scope,
                                    loops.arguments(body, *result),
                                );
                                inputs.loans.push(loan);
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
                );
                definitions.insert(*result, template);
            }
        }
        let entry = BorrowState::new(
            &mut inputs.values,
            shapes
                .iter()
                .enumerate()
                .map(|(index, shape)| (NValueId::new(index), *shape)),
            inputs.storage,
        );
        let mut result = Self {
            loops,
            values: inputs.values,
            shapes,
            roots,
            loans: inputs.loans,
            input_loans: inputs
                .input_loans
                .iter()
                .filter_map(|((source, _), loan)| source.is_incoming().then_some(*loan))
                .collect(),
            definitions,
            inputs: inputs.targets.into_values().collect(),
            external_loans: inputs.input_loans,
            entry,
            allocation_cells: BTreeMap::new(),
        };
        for (id, value) in entry_values {
            result.entry.set_value(id, value);
        }
        Ok(result)
    }
}

impl<'db> Inventory<'db> {
    /// Complete call-created external storage before starting the fixed point.
    pub fn add_external_sources(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        sources: impl IntoIterator<Item = (ExternalSource<'db>, BinderScope)>,
    ) -> Result<(), ShapeError<'db>> {
        let mut builder = InputBuilder {
            db,
            instance,
            values: std::mem::replace(
                &mut self.values,
                CapabilityValues::new(db, ValueLimits::default()),
            ),
            loans: std::mem::take(&mut self.loans),
            input_loans: std::mem::take(&mut self.external_loans),
            targets: std::mem::take(&mut self.inputs)
                .into_iter()
                .map(|target| (target.source.clone(), target))
                .collect(),
            storage: self
                .entry
                .storage()
                .map(|(root, value)| (root.clone(), value.clone()))
                .collect(),
            pending: Vec::new(),
        };
        for (source, scope) in sources {
            builder.register(source, scope, CapabilityClass::Handle, true, &[])?;
        }
        builder.finish_storage()?;
        let mut contracts = Vec::new();
        let mut shapes = HashSet::new();
        for shape in &self.shapes {
            if !shapes.insert(*shape) {
                continue;
            }
            let mut failure = None;
            builder
                .values
                .from_shape(*shape, &BinderScope::default(), |semantics, _, _| {
                    match referent_contract(db, instance, semantics) {
                        Ok(contract) => {
                            let contract = (contract, semantics.class);
                            if !contracts.contains(&contract) {
                                contracts.push(contract);
                            }
                        }
                        Err(error) => {
                            failure = Some(error);
                        }
                    }
                    Vec::new()
                });
            if let Some(error) = failure {
                return Err(error);
            }
        }
        let normalization_scope = instance.key(db).impl_env(db).normalization_scope(db);
        // Sealed EVM effect traits admit only the zero-sized EVM token. A
        // symbolic effect witness must not invent hidden pointer-bearing fields.
        let effect_env = instantiated_effect_env(db, instance);
        let zero_sized_providers: HashSet<_> = instance
            .assumptions(db)
            .list(db)
            .iter()
            .chain(
                effect_env
                    .iter()
                    .flat_map(|env| env.forwarded_witnesses(db)),
            )
            .filter(|predicate| is_std_evm_effect_trait(db, normalization_scope, predicate.def(db)))
            .map(|predicate| predicate.self_ty(db))
            .collect();
        let evm_receiver = matches!(instance.key(db).owner(db), BodyOwner::Func(func) if is_std_evm_effect_method(db, func));
        let abstract_roots: Vec<_> = builder
            .storage
            .iter()
            .filter(|(root, _)| {
                root.contract().is_some_and(|contract| {
                    contract.is_abstract(db) && !zero_sized_providers.contains(&contract.ty)
                }) && !(evm_receiver
                    && matches!(root, RegionRoot::External(source) if source.param() == Some(0)))
            })
            .map(|(root, value)| (root.clone(), value.scope().clone()))
            .collect();
        for (root, scope) in abstract_roots {
            for (contract, class) in &contracts {
                let mut contract = *contract;
                if contract.address_space == HandleAddressSpace::Unspecified {
                    contract.address_space = root.address_space();
                }
                builder.register(
                    ExternalSource::abstract_target(&root, contract),
                    scope.clone(),
                    *class,
                    matches!(
                        class,
                        CapabilityClass::Borrow(BorrowKind::Mut)
                            | CapabilityClass::Handle
                            | CapabilityClass::Pointer
                    ),
                    &[],
                )?;
            }
        }
        builder.finish_storage()?;
        let holders: Vec<_> = self
            .entry
            .holders()
            .map(|(id, value)| (id, value.clone()))
            .collect();
        self.entry = BorrowState::new(
            &mut builder.values,
            self.shapes
                .iter()
                .enumerate()
                .map(|(index, shape)| (NValueId::new(index), *shape)),
            builder.storage,
        );
        for (id, value) in holders {
            self.entry.set_value(id, value);
        }
        self.values = builder.values;
        self.loans = builder.loans;
        self.inputs = builder.targets.into_values().collect();
        self.external_loans = builder.input_loans;
        self.allocation_cells.clear();
        for (root, value) in self.entry.storage() {
            if value.shape().contains_capability(db)
                && let RegionRoot::External(source) = root
                && let Some(allocation) = source.fresh_allocation()
            {
                self.allocation_cells
                    .entry(allocation.occurrence)
                    .or_default()
                    .push(root.clone());
            }
        }
        Ok(())
    }
}

impl<'db> InputBuilder<'db> {
    fn shape(&self, ty: TyId<'db>) -> Result<ShapeId<'db>, ShapeError<'db>> {
        capability_shape(
            self.db,
            self.instance
                .key(self.db)
                .impl_env(self.db)
                .normalization_scope(self.db),
            self.instance.assumptions(self.db),
            ty,
        )
    }

    fn register(
        &mut self,
        source: ExternalSource<'db>,
        scope: BinderScope,
        class: CapabilityClass,
        writable: bool,
        ancestry: &[TyId<'db>],
    ) -> Result<(), ShapeError<'db>> {
        let (source, scope, _) = canonical_source(self.db, &source, &scope);
        if let Some(target) = self.targets.get_mut(&source) {
            target.writable |= writable;
            if !target.classes.contains(&class) {
                target.classes.push(class);
            }
        } else {
            let shape = self.shape(source.contract.ty)?;
            let target = InputTarget {
                ty: source.contract.ty,
                source: source.clone(),
                scope,
                shape,
                writable,
                classes: vec![class],
            };
            self.targets.insert(source.clone(), target.clone());
            self.pending
                .push((target, InputOrigin::Referent(source), ancestry.to_vec()));
        }
        Ok(())
    }

    fn finish_storage(&mut self) -> Result<(), ShapeError<'db>> {
        while let Some((target, origin, mut ancestry)) = self.pending.pop() {
            let root = RegionRoot::External(target.source.clone());
            if self.storage.contains_key(&root) {
                continue;
            }
            ancestry.push(target.ty);
            // Reserve the cell before traversing recursively followed handles.
            self.storage
                .insert(root.clone(), self.values.empty(target.shape, &target.scope));
            let value = match CellSeed::for_source(&target.source) {
                CellSeed::EntryContents => {
                    self.value(target.shape, &target.scope, origin, &ancestry)?
                }
                CellSeed::FreshUninitialized | CellSeed::UnknownBytes => {
                    let mut base = &target.source;
                    while let ExternalOrigin::Memory { base: next, .. } = &base.origin {
                        base = &next.source;
                    }
                    let origin = match &base.origin {
                        ExternalOrigin::Allocation(handle)
                        | ExternalOrigin::OpaqueHandle(handle) => {
                            SeedOrigin::Address(handle.occurrence)
                        }
                        ExternalOrigin::Unknown { occurrence, .. } => {
                            SeedOrigin::Address(*occurrence)
                        }
                        ExternalOrigin::Local(root) => SeedOrigin::Local(*root),
                        _ => unreachable!("entry contents have symbolic input seeds"),
                    };
                    // Following an arbitrary pointer seed must not grow an
                    // unbounded chain of seed identities during discovery.
                    let site = if let SeedOrigin::Address(AddressOccurrence::Overwrite(id)) = origin
                        && let site @ OpaqueWriteSite::Seed { .. } = id.site(self.db)
                    {
                        site
                    } else {
                        OpaqueWriteSite::Seed {
                            instance: self.instance,
                            origin,
                        }
                    };
                    OpaqueWrite {
                        site,
                        scope: self
                            .instance
                            .key(self.db)
                            .impl_env(self.db)
                            .normalization_scope(self.db),
                        assumptions: self.instance.assumptions(self.db),
                    }
                    // No conditional clobber: fresh/arbitrary bytes have no
                    // native authority even if all raw writes were disjoint.
                    .contents(&mut self.values, target.shape, &target.scope, None)
                    .map_err(|error| ShapeError::UnresolvedCapability(error.0))?
                }
            };
            self.storage.insert(root, value);
        }
        Ok(())
    }

    fn value(
        &mut self,
        shape: ShapeId<'db>,
        scope: &BinderScope,
        origin: InputOrigin<'db>,
        ancestry: &[TyId<'db>],
    ) -> Result<CapabilityValue<'db>, ShapeError<'db>> {
        let mut requests = Vec::new();
        let mut views: Vec<(StructuralPath<IndexExpr<'db>>, ExternalSource<'db>)> = Vec::new();
        let mut failure = None;
        let db = self.db;
        let instance = self.instance;
        let value = self
            .values
            .from_shape(shape, scope, |semantics, path, scope| {
                let contract = match match &origin {
                    InputOrigin::Parameter(param) => {
                        input_referent_contract(db, instance, semantics, *param)
                    }
                    InputOrigin::Referent(_) => referent_contract(db, instance, semantics),
                } {
                    Ok(contract) => contract,
                    Err(error) => {
                        failure = Some(error);
                        return Vec::new();
                    }
                };
                let uncertain = matches!(
                    semantics.class,
                    CapabilityClass::Handle | CapabilityClass::Pointer
                );
                let outer_view = matches!(origin, InputOrigin::Parameter(_))
                    && path.is_empty()
                    && semantics.class == CapabilityClass::View;
                let mut source = if let Some((prefix, source)) =
                    views.iter().rev().find(|(prefix, _)| {
                        !prefix.is_empty()
                            && path.as_slice().starts_with(prefix.as_slice())
                            && prefix != path
                    }) {
                    source.follow(
                        RegionPath::new(&path.as_slice()[prefix.as_slice().len()..]),
                        contract,
                        uncertain,
                    )
                } else {
                    match &origin {
                        InputOrigin::Parameter(param) => ExternalSource::input(
                            if outer_view {
                                InputSource::place(*param)
                            } else {
                                InputSource::slot(*param, path.clone())
                            },
                            contract,
                            uncertain,
                        ),
                        InputOrigin::Referent(source) => {
                            source.follow(RegionPath::new(path.as_slice()), contract, uncertain)
                        }
                    }
                };
                if ancestry.contains(&semantics.target_ty) {
                    source = source.widen();
                }
                if semantics.class == CapabilityClass::View {
                    views.push((path.clone(), source.clone()));
                }
                let region = RegionSet::singleton(
                    scope,
                    RegionRoot::External(source.clone()),
                    RegionPath::default(),
                );
                let payload = match semantics.class {
                    CapabilityClass::Borrow(kind) => CapabilityRef::borrow(
                        kind,
                        input_loan(
                            db,
                            &mut self.loans,
                            &mut self.input_loans,
                            &source,
                            scope,
                            kind,
                            SemOrigin::Body(instance.key(db).owner(db)),
                        ),
                    ),
                    CapabilityClass::View => CapabilityRef::view(region, Vec::new()),
                    CapabilityClass::Handle | CapabilityClass::Pointer => {
                        CapabilityRef::Address(region)
                    }
                };
                requests.push((source, scope.clone(), semantics, outer_view));
                vec![Guarded {
                    guard: Guard::always(scope),
                    payload,
                }]
            });
        if let Some(error) = failure {
            return Err(error);
        }
        for (source, scope, semantics, outer_view) in requests {
            let writable = matches!(
                semantics.class,
                CapabilityClass::Borrow(BorrowKind::Mut)
                    | CapabilityClass::Handle
                    | CapabilityClass::Pointer
            );
            self.register(source.clone(), scope, semantics.class, writable, ancestry)?;
            if outer_view {
                let param = source.param().expect("outer parameter view");
                if let Some((_, origin, _)) = self
                    .pending
                    .iter_mut()
                    .rev()
                    .find(|(target, _, _)| target.source == source)
                {
                    *origin = InputOrigin::Parameter(param);
                }
            }
        }
        Ok(value)
    }
}

/// Ordinary mutable arguments carry the memory transport precondition checked
/// at each call. Receivers and effects retain their provider's address space.
pub(super) fn input_referent_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    semantics: CapabilitySemantics<'db>,
    param: u32,
) -> Result<ReferentContract<'db>, ShapeError<'db>> {
    let mut contract = referent_contract(db, instance, semantics)?;
    if semantics.class == CapabilityClass::Borrow(BorrowKind::Mut)
        && let BodyOwner::Func(func) = instance.key(db).owner(db)
        && (param as usize) < func.params(db).count()
        && !(param == 0 && func.receiver_ty(db).is_some())
    {
        contract.address_space = HandleAddressSpace::Known(ProviderAddressSpace::Memory);
    }
    Ok(contract)
}

pub(super) fn referent_contract<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    semantics: CapabilitySemantics<'db>,
) -> Result<ReferentContract<'db>, ShapeError<'db>> {
    let space = if matches!(
        semantics.class,
        CapabilityClass::Handle | CapabilityClass::Pointer
    ) {
        OpaqueHandleContract::for_ty(
            db,
            instance.key(db).impl_env(db).normalization_scope(db),
            instance.assumptions(db),
            semantics.representation_ty,
        )
        .map_err(|error| ShapeError::UnresolvedCapability(error.0))?
        .ok_or(ShapeError::UnresolvedCapability(
            semantics.representation_ty,
        ))?
        .address_space
    } else {
        HandleAddressSpace::Unspecified
    };
    Ok(ReferentContract::new(db, semantics.target_ty, space))
}

fn canonical_source<'db>(
    db: &'db dyn HirAnalysisDb,
    source: &ExternalSource<'db>,
    scope: &BinderScope,
) -> (ExternalSource<'db>, BinderScope, Box<[IndexExpr<'db>]>) {
    let mut parameters = BinderScope::default();
    let mut bindings = BTreeMap::new();
    let mut arguments = Vec::new();
    for index in source.indices() {
        if matches!(
            index,
            IndexExpr::Bound(_) | IndexExpr::Runtime(_) | IndexExpr::Iteration(_)
        ) && !bindings.contains_key(&index)
        {
            let (nested, parameter) = parameters.bind(IndexNamespace::InputSlot);
            parameters = nested;
            bindings.insert(index, parameter);
            arguments.push(index);
        }
    }
    for index in scope.variables() {
        bindings.entry(index).or_insert(IndexExpr::Const(0));
    }
    let subst = IndexSubst::new(scope, &parameters, bindings).expect("input family abstraction");
    (source.substitute(db, &subst), parameters, arguments.into())
}

fn input_loan<'db>(
    db: &'db dyn HirAnalysisDb,
    loans: &mut Vec<LoanDef<'db>>,
    inventory: &mut BTreeMap<(ExternalSource<'db>, bool), LoanId>,
    source: &ExternalSource<'db>,
    scope: &BinderScope,
    kind: BorrowKind,
    origin: SemOrigin<'db>,
) -> LoanRef<'db> {
    let (source, parameters, args) = canonical_source(db, source, scope);
    let id = *inventory
        .entry((source.clone(), kind == BorrowKind::Mut))
        .or_insert_with(|| {
            let id = LoanId(loans.len());
            let (mut loan, _, abstraction) =
                LoanDef::new(kind, BorrowActivation::Immediate, origin, &parameters);
            loan.extend(
                &RegionSet::singleton(
                    &parameters,
                    RegionRoot::External(source),
                    RegionPath::default(),
                )
                .substitute(db, &abstraction),
                [],
            );
            loans.push(loan);
            id
        });
    LoanRef { id, args }
}

/// Signature-only inventory has no body definitions, layout facts, or call queries.
pub(super) fn signature_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> NormalizedBody<'db> {
    let owner = instance.key(db).owner(db);
    let typed = instance.key(db).typed_body(db);
    let mut values = Vec::new();
    while let Some(binding) = typed.param_binding(values.len()) {
        values.push(NValue {
            ty: copied_scalar_ty(db, instance.normalized_binding_ty(db, binding)),
            mutability: if binding.is_mut() {
                Mutability::Mutable
            } else {
                Mutability::Immutable
            },
            origin: SemOrigin::Body(owner),
            definition: NValueDefinition::EntryParam {
                param: values.len().try_into().expect("parameter count"),
            },
            source: Some(binding),
        });
    }
    NormalizedBody {
        owner: instance,
        template_owner: owner,
        values,
        roots: Vec::new(),
        entry: NBlockId::new(0),
        blocks: vec![NBlock {
            params: Box::new([]),
            statements: Vec::new(),
            terminator: NTerminator {
                origin: SemOrigin::Body(owner),
                kind: NTerminatorKind::Return(None),
            },
        }],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::{
            semantic::{
                capability::guard::ValueOccurrence, get_or_build_semantic_instance,
                identity_semantic_instance_key,
            },
            ty::ProviderAddressSpace,
        },
        test_db::{HirAnalysisTestDb, find_func},
    };

    #[test]
    fn fresh_cell_discovery_does_not_manufacture_native_input_loans() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone("fresh_inventory.fe".into(), "fn anchor() {}");
        let (module, _) = db.top_mod(file);
        let owner = BodyOwner::Func(find_func(&db, module, "anchor"));
        let instance =
            get_or_build_semantic_instance(&db, identity_semantic_instance_key(&db, owner));
        let mut inventory = Inventory::new(&db, &signature_body(&db, instance)).unwrap();
        for native in [
            TyId::borrow_ref_of(&db, TyId::u256(&db)),
            TyId::borrow_mut_of(&db, TyId::u256(&db)),
        ] {
            let source = ExternalSource::allocation(
                &db,
                OpaqueHandleRef {
                    contract: OpaqueHandleContract {
                        handle_ty: TyId::ptr_to(&db, native),
                        target_ty: native,
                        address_space: HandleAddressSpace::Known(ProviderAddressSpace::Memory),
                    },
                    occurrence: AddressOccurrence::Summary(0),
                    arguments: Box::new([]),
                },
            );
            inventory
                .add_external_sources(&db, instance, [(source.clone(), BinderScope::default())])
                .unwrap();
            let (_, value) = inventory
                .entry
                .storage()
                .find(|(root, _)| **root == RegionRoot::External(source.clone()))
                .unwrap();
            let leaves = inventory.values.leaves(value, ValueOccurrence::Summary);
            assert!(!leaves.is_empty());
            assert!(
                leaves
                    .iter()
                    .all(|leaf| matches!(leaf.payload, CapabilityRef::Invalidated { .. })),
                "fresh bytes must not manufacture native input loans: {leaves:#?}"
            );
        }
    }
}
