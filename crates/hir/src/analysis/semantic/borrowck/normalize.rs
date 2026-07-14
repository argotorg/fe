use cranelift_entity::EntityRef;
use rustc_hash::FxHashMap;
use std::cell::RefCell;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            LayoutBackingPlace, LayoutBackingProjection, PlaceProvenance, SExpr, SLocalId,
            SOperand, SPlace, SStmtKind, STerminatorKind, SemanticBody, SemanticInstance,
            SemanticLocalKind, SemanticLocalRole, SemanticProjectionPath, ValueProvenance,
            ctfe::{canonicalize_semantic_const_refs_from_body, canonicalize_semantic_consts},
            semantic_instance_base_assumptions_for_key,
        },
        ty::{
            normalize::normalize_ty,
            ty_check::{EffectPassMode, LocalBinding, ParamSite},
            ty_def::{BorrowKind, TyId},
            ty_is_copy,
        },
    },
    hir_def::ExprId,
    projection::{IndexSource, Projection, ProjectionPath},
};

use super::diagnostics::normalize_error_to_diag;
use super::ir::{
    NBorrowRoot, NBorrowRootId, NEffectArg, NEffectArgValue, NExpr, NLayoutBackingSource,
    NLocalFacts, NLocalOrigin, NLocalRootDemand, NOperand, NSBlock, NSLocal, NSPlace, NSPlaceRoot,
    NSStmt, NSStmtKind, NSTerminator, NSTerminatorKind, NormalizedBindingLowering,
    NormalizedSemanticBody, NormalizedSemanticBodyId, ReadMode, SemanticBorrowDiagnostic,
    SemanticNormalizeError, SemanticNormalizeResult, empty_normalized_body,
    local_has_runtime_move_semantics,
};

pub fn normalize_semantic_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<NormalizedSemanticBody<'db>, SemanticBorrowDiagnostic<'db>> {
    match normalized_semantic_body_query(db, instance) {
        SemanticNormalizeResult::Ok(body) => Ok(body.body(db).clone()),
        SemanticNormalizeResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

/// Normalizes semantic places and ownership without folding value-producing
/// expressions. Layout evidence consumes this view so its dataflow is stable
/// across runtime constant-folding decisions.
pub fn normalize_semantic_body_for_layout_evidence<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<NormalizedSemanticBody<'db>, SemanticBorrowDiagnostic<'db>> {
    match layout_normalized_semantic_body_query(db, instance) {
        SemanticNormalizeResult::Ok(body) => Ok(body.body(db).clone()),
        SemanticNormalizeResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

pub(crate) fn normalize_provisional_semantic_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<NormalizedSemanticBody<'db>, SemanticBorrowDiagnostic<'db>> {
    match provisional_normalized_semantic_body_query(db, instance) {
        SemanticNormalizeResult::Ok(body) => Ok(body.body(db).clone()),
        SemanticNormalizeResult::Err(diag) => Err(diag.diag(db).clone()),
    }
}

fn layout_backing_source_projection_matches(
    pattern: LayoutBackingProjection,
    candidate: LayoutBackingProjection,
) -> bool {
    pattern == candidate
        || matches!(
            (pattern, candidate),
            (
                LayoutBackingProjection::Index(None),
                LayoutBackingProjection::Index(_)
            )
        )
}

fn layout_backing_source_path_is_prefix(
    prefix: &[LayoutBackingProjection],
    path: &[LayoutBackingProjection],
) -> bool {
    prefix.len() <= path.len()
        && prefix
            .iter()
            .copied()
            .zip(path.iter().copied())
            .all(|(pattern, candidate)| {
                layout_backing_source_projection_matches(pattern, candidate)
            })
}

fn semantic_layout_query<'db>(
    path: &SemanticProjectionPath<'db>,
) -> Option<(Vec<LayoutBackingProjection>, SemanticProjectionPath<'db>)> {
    let mut target = Vec::new();
    let mut filtered = SemanticProjectionPath::new();
    for projection in path.iter() {
        let step = match projection {
            Projection::Field(field) => LayoutBackingProjection::Field(
                crate::analysis::semantic::FieldIndex(u16::try_from(*field).ok()?),
            ),
            Projection::VariantField {
                variant, field_idx, ..
            } => LayoutBackingProjection::VariantField {
                variant: *variant,
                field: crate::analysis::semantic::FieldIndex(u16::try_from(*field_idx).ok()?),
            },
            Projection::Index(IndexSource::Constant(index)) => {
                LayoutBackingProjection::Index(Some(*index))
            }
            Projection::Index(IndexSource::Dynamic(_)) => LayoutBackingProjection::Index(None),
            Projection::Deref => continue,
            Projection::Discriminant => return None,
        };
        target.push(step);
        filtered.push(projection.clone());
    }
    Some((target, filtered))
}

fn resolve_normalized_layout_backing_source<'db>(
    sources: &[NLayoutBackingSource<'db>],
    requested: &SemanticProjectionPath<'db>,
) -> Option<NSPlace<'db>> {
    let (target, path) = semantic_layout_query(requested)?;
    let mut resolved = Vec::new();
    for source in sources {
        if !layout_backing_source_path_is_prefix(&source.target, &target) {
            continue;
        }
        let mut suffix = SemanticProjectionPath::new();
        for projection in path.iter().skip(source.target.len()) {
            suffix.push(projection.clone());
        }
        let mut place = source.source.clone();
        place.path = place.path.concat(&suffix);
        if !resolved.contains(&place) {
            resolved.push(place);
        }
    }
    let [place] = resolved.as_slice() else {
        return None;
    };
    Some(place.clone())
}

#[salsa::tracked]
fn normalized_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticNormalizeResult<'db> {
    if let Some(diag) = instance.call_site_finalization_diagnostic(db) {
        return SemanticNormalizeResult::Err(diag);
    }
    let raw = canonicalize_semantic_consts(db, instance);
    normalize_semantic_body_result(db, instance, raw, instance.assumptions(db))
}

#[salsa::tracked]
fn layout_normalized_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticNormalizeResult<'db> {
    if let Some(diag) = instance.call_site_finalization_diagnostic(db) {
        return SemanticNormalizeResult::Err(diag);
    }
    let raw = canonicalize_semantic_const_refs_from_body(db, instance, instance.body(db).clone());
    normalize_semantic_body_result(db, instance, raw, instance.assumptions(db))
}

#[salsa::tracked]
fn provisional_normalized_semantic_body_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticNormalizeResult<'db> {
    let raw =
        canonicalize_semantic_const_refs_from_body(db, instance, instance.provisional_body(db));
    let assumptions = semantic_instance_base_assumptions_for_key(db, instance.key(db));
    normalize_semantic_body_result(db, instance, raw, assumptions)
}

fn normalize_semantic_body_result<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    raw: SemanticBody<'db>,
    assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
) -> SemanticNormalizeResult<'db> {
    match NormalizeCtxt::new(db, instance, raw, assumptions).normalize() {
        Ok(body) => SemanticNormalizeResult::Ok(NormalizedSemanticBodyId::new(db, body)),
        Err(err) => {
            SemanticNormalizeResult::Err(crate::analysis::semantic::BorrowDiagnosticId::new(
                db,
                normalize_error_to_diag(db, instance, err),
            ))
        }
    }
}

struct NormalizeCtxt<'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    raw: SemanticBody<'db>,
    assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
    locals: Vec<Option<NSLocal<'db>>>,
    local_state: Vec<LocalNormState>,
    root_demands: Vec<NLocalRootDemand>,
    borrow_roots: Vec<NBorrowRoot<'db>>,
    copy_cache: RefCell<FxHashMap<TyId<'db>, bool>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum LocalNormState {
    Unseen,
    Visiting,
    Done,
}

impl<'db> NormalizeCtxt<'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        raw: SemanticBody<'db>,
        assumptions: crate::analysis::ty::trait_resolution::PredicateListId<'db>,
    ) -> Self {
        let local_capacity = raw.locals.len();
        Self {
            db,
            instance,
            raw,
            assumptions,
            locals: vec![None; local_capacity],
            local_state: vec![LocalNormState::Unseen; local_capacity],
            root_demands: vec![NLocalRootDemand::default(); local_capacity],
            borrow_roots: Vec::new(),
            copy_cache: RefCell::new(FxHashMap::default()),
        }
    }

    fn normalize(mut self) -> Result<NormalizedSemanticBody<'db>, SemanticNormalizeError<'db>> {
        self.normalize_locals()?;
        if self.raw.blocks.is_empty() {
            let locals = self.take_normalized_locals();
            let borrow_roots = std::mem::take(&mut self.borrow_roots);
            return Ok(empty_normalized_body(&self.raw, locals, borrow_roots));
        }

        let mut blocks = Vec::with_capacity(self.raw.blocks.len());
        let raw_blocks = self.raw.blocks.clone();
        for block in &raw_blocks {
            let stmts = block
                .stmts
                .iter()
                .map(|stmt| {
                    Ok(NSStmt {
                        id: stmt.id,
                        origin: stmt.origin,
                        kind: self.normalize_stmt(stmt.origin, &stmt.kind)?,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            let terminator = NSTerminator {
                origin: block.terminator.origin,
                kind: self.normalize_terminator(block.terminator.origin, &block.terminator.kind),
            };
            blocks.push(NSBlock { stmts, terminator });
        }
        let locals = self.take_normalized_locals();

        Ok(NormalizedSemanticBody {
            owner: self.instance,
            template_owner: self.raw.template_owner,
            entry_locals: self.raw.entry_locals.clone(),
            locals,
            blocks,
            borrow_roots: self.borrow_roots,
        })
    }

    fn take_normalized_locals(&mut self) -> Vec<NSLocal<'db>> {
        std::mem::take(&mut self.locals)
            .into_iter()
            .enumerate()
            .map(|(idx, local)| {
                let mut local = local.expect("all locals normalized");
                local.facts.root_demand = self.root_demands[idx];
                local
            })
            .collect()
    }

    fn normalize_locals(&mut self) -> Result<(), SemanticNormalizeError<'db>> {
        let raw_locals = self.raw.locals.clone();
        for (idx, local) in raw_locals.iter().enumerate() {
            let local_id = SLocalId::from_u32(idx as u32);
            self.ensure_local_normalized(local_id, local)?;
        }
        Ok(())
    }

    fn ensure_local_normalized(
        &mut self,
        local: SLocalId,
        raw_local: &crate::analysis::semantic::SLocal<'db>,
    ) -> Result<(), SemanticNormalizeError<'db>> {
        match self.local_state[local.index()] {
            LocalNormState::Done => return Ok(()),
            LocalNormState::Visiting => {
                return Err(SemanticNormalizeError::LocalProvenanceCycle {
                    owner: self.instance,
                    local,
                });
            }
            LocalNormState::Unseen => {}
        }

        self.local_state[local.index()] = LocalNormState::Visiting;
        let lowering = self.normalize_local_lowering(local, raw_local)?;
        let facts = self.normalize_local_facts(local, raw_local, &lowering)?;
        self.mark_local_root_demand(
            local,
            &lowering,
            facts.snapshot_source_place.as_ref(),
            &facts.layout_backing_sources,
        );
        self.locals[local.index()] = Some(NSLocal {
            ty: raw_local.ty,
            mutability: raw_local.mutability,
            source: raw_local.source,
            lowering,
            facts,
        });
        self.local_state[local.index()] = LocalNormState::Done;
        Ok(())
    }

    fn normalize_local_lowering(
        &mut self,
        local: SLocalId,
        raw_local: &crate::analysis::semantic::SLocal<'db>,
    ) -> Result<NormalizedBindingLowering<'db>, SemanticNormalizeError<'db>> {
        match raw_local.role.clone() {
            SemanticLocalRole::Erased => Ok(NormalizedBindingLowering::Erased),
            SemanticLocalRole::DirectValue { provenance } => {
                let place = self.normalize_value_provenance(
                    local,
                    raw_local.source,
                    provenance,
                    raw_local.ty,
                )?;
                Ok(NormalizedBindingLowering::ValueLocal { place })
            }
            SemanticLocalRole::PlaceBoundValue {
                provenance,
                value_ty,
            } => {
                let place = self.normalize_place_provenance(local, provenance, value_ty)?;
                Ok(NormalizedBindingLowering::PlaceBoundValue { place, value_ty })
            }
            SemanticLocalRole::PlaceCarrier { provider, value_ty } => {
                Ok(NormalizedBindingLowering::CarrierLocal {
                    root: Some(self.push_local_root(local, raw_local.source)),
                    provider,
                    target_ty: value_ty,
                })
            }
            SemanticLocalRole::DirectCarrier {
                provider: None,
                target_ty: value_ty,
            } => Ok(NormalizedBindingLowering::CarrierLocal {
                root: Some(self.push_local_root(local, raw_local.source)),
                provider: None,
                target_ty: value_ty,
            }),
            SemanticLocalRole::DirectCarrier {
                provider,
                target_ty,
            } => Ok(NormalizedBindingLowering::CarrierLocal {
                root: Some(self.push_local_root(local, raw_local.source)),
                provider,
                target_ty,
            }),
        }
    }

    fn normalize_local_facts(
        &mut self,
        local: SLocalId,
        raw_local: &crate::analysis::semantic::SLocal<'db>,
        lowering: &NormalizedBindingLowering<'db>,
    ) -> Result<NLocalFacts<'db>, SemanticNormalizeError<'db>> {
        let (interface, origin) = match &raw_local.role {
            SemanticLocalRole::Erased => (SemanticLocalKind::Erased, NLocalOrigin::SelfRooted),
            SemanticLocalRole::DirectValue { provenance } => (
                SemanticLocalKind::DirectValue,
                match provenance {
                    ValueProvenance::Ordinary => NLocalOrigin::SelfRooted,
                    ValueProvenance::RootProvider(provider) => {
                        NLocalOrigin::RootProvider(provider.clone())
                    }
                },
            ),
            SemanticLocalRole::PlaceBoundValue { provenance, .. } => (
                SemanticLocalKind::PlaceBoundValue,
                match provenance {
                    PlaceProvenance::RootProvider(provider) => {
                        NLocalOrigin::RootProvider(provider.clone())
                    }
                    PlaceProvenance::Derived(_) => NLocalOrigin::AliasedPlace,
                },
            ),
            SemanticLocalRole::PlaceCarrier { provider, .. } => (
                SemanticLocalKind::PlaceCarrier,
                provider
                    .clone()
                    .map_or(NLocalOrigin::SelfRooted, NLocalOrigin::RootProvider),
            ),
            SemanticLocalRole::DirectCarrier { provider, .. } => (
                SemanticLocalKind::DirectCarrier,
                provider
                    .clone()
                    .map_or(NLocalOrigin::SelfRooted, NLocalOrigin::RootProvider),
            ),
        };
        let snapshot_source_place = raw_local
            .snapshot_source
            .clone()
            .map(|snapshot_source| {
                self.normalize_snapshot_source(
                    local,
                    snapshot_source,
                    raw_local.role.layout_ty(raw_local.ty),
                )
            })
            .transpose()?;
        let layout_backing_sources = raw_local
            .layout_backing_sources
            .iter()
            .cloned()
            .map(|layout_backing_source| {
                Ok(NLayoutBackingSource {
                    target: layout_backing_source.target,
                    source: self.normalize_layout_backing_source(
                        local,
                        lowering,
                        layout_backing_source.source,
                    )?,
                })
            })
            .collect::<Result<Vec<_>, SemanticNormalizeError<'db>>>()?;
        let mut root_demand = NLocalRootDemand::default();
        if matches!(interface, SemanticLocalKind::PlaceCarrier)
            || (matches!(interface, SemanticLocalKind::PlaceBoundValue)
                && !matches!(origin, NLocalOrigin::AliasedPlace))
        {
            root_demand.always_rooted = true;
        }
        self.root_demands[local.index()] = root_demand;
        Ok(NLocalFacts {
            interface,
            origin,
            snapshot_source_place,
            layout_backing_sources,
            root_demand,
        })
    }

    fn mark_local_root_demand(
        &mut self,
        local: SLocalId,
        lowering: &NormalizedBindingLowering<'db>,
        snapshot_source_place: Option<&NSPlace<'db>>,
        layout_backing_sources: &[NLayoutBackingSource<'db>],
    ) {
        if let NormalizedBindingLowering::ValueLocal { place } = lowering
            && !self.is_self_rooted_value_place(local, place)
        {
            self.mark_place_root_demand(place, |demand| {
                demand.nonself_backing_place = true;
            });
        }
        if let Some(place) = snapshot_source_place {
            self.mark_place_root_demand(place, |demand| {
                demand.nonself_backing_place = true;
            });
        }
        for source in layout_backing_sources {
            if Some(&source.source) == snapshot_source_place
                || lowering.place() == Some(&source.source)
            {
                continue;
            }
            self.mark_place_root_demand(&source.source, |demand| {
                demand.nonself_backing_place = true;
            });
        }
    }

    fn mark_stmt_root_demand(&mut self, stmt: &NSStmtKind<'db>) {
        match stmt {
            NSStmtKind::Assign { expr, .. } => self.mark_expr_root_demand(expr),
            NSStmtKind::Store { dst, .. } => {
                self.mark_place_root_demand(dst, |demand| {
                    demand.written_by_place = true;
                });
            }
        }
    }

    fn mark_expr_root_demand(&mut self, expr: &NExpr<'db>) {
        match expr {
            NExpr::Use(_)
            | NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::Cast { .. }
            | NExpr::ArrayRepeat { .. }
            | NExpr::AggregateMake { .. }
            | NExpr::EnumMake { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::ExtractEnumField { .. }
            | NExpr::CodeRegionRef { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => {}
            NExpr::ReadPlace { place, .. } => {
                self.mark_place_root_demand(place, |demand| {
                    demand.read_by_place = true;
                });
            }
            NExpr::Borrow { kind, place, .. } => {
                self.mark_place_root_demand(place, |demand| {
                    demand.borrowed_or_addr_taken = true;
                    if matches!(kind, BorrowKind::Mut) {
                        demand.mut_borrowed_or_addr_taken = true;
                    }
                });
            }
            NExpr::Call { effect_args, .. } => {
                for arg in effect_args {
                    match &arg.arg {
                        NEffectArgValue::Place(place) => {
                            self.mark_place_root_demand(place, |demand| {
                                demand.passed_by_place = true;
                                if arg.required_mut {
                                    demand.mut_borrowed_or_addr_taken = true;
                                }
                            });
                        }
                        NEffectArgValue::Value(value)
                            if arg.required_mut
                                && matches!(arg.pass_mode, EffectPassMode::ByTempPlace) =>
                        {
                            if let Some(demand) = self.root_demands.get_mut(value.local.index()) {
                                demand.passed_by_place = true;
                                demand.mut_borrowed_or_addr_taken = true;
                            }
                        }
                        NEffectArgValue::Value(_) => {}
                    }
                }
            }
        }
    }

    fn mark_place_root_demand(
        &mut self,
        place: &NSPlace<'db>,
        mut mark: impl FnMut(&mut NLocalRootDemand),
    ) {
        let local = match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => Some(local),
            NSPlaceRoot::Root(root) => match self.borrow_roots.get(root.index()) {
                Some(NBorrowRoot::Param { local, .. }) | Some(NBorrowRoot::LocalSlot { local }) => {
                    Some(*local)
                }
                Some(NBorrowRoot::Provider { .. }) | None => None,
            },
        };
        if let Some(local) = local
            && let Some(demand) = self.root_demands.get_mut(local.index())
        {
            mark(demand);
        }
    }

    fn is_self_rooted_value_place(&self, local: SLocalId, place: &NSPlace<'db>) -> bool {
        if !place.path.is_empty() {
            return false;
        }
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(root_local) => root_local == local,
            NSPlaceRoot::Root(root) => matches!(
                self.borrow_roots.get(root.index()),
                Some(NBorrowRoot::Param { local: root_local, .. }
                    | NBorrowRoot::LocalSlot { local: root_local }) if *root_local == local
            ),
        }
    }

    fn propagated_place(&mut self, local: SLocalId) -> Option<NSPlace<'db>> {
        let local_data = self.locals.get(local.index())?.as_ref()?;
        match &local_data.lowering {
            NormalizedBindingLowering::ValueLocal { place }
            | NormalizedBindingLowering::PlaceBoundValue { place, .. } => Some(place.clone()),
            NormalizedBindingLowering::CarrierLocal {
                root,
                provider,
                target_ty,
            } => {
                let root = *root;
                let provider = provider.clone();
                let target_ty = *target_ty;
                let is_capability = local_data.ty.as_capability(self.db).is_some();
                provider
                    .map(|provider| self.provider_root_place(provider, target_ty))
                    .or_else(|| {
                        is_capability.then_some(NSPlace {
                            root: NSPlaceRoot::CarrierDerefLocal(local),
                            path: ProjectionPath::default(),
                        })
                    })
                    .or_else(|| {
                        root.map(|root| NSPlace {
                            root: NSPlaceRoot::Root(root),
                            path: ProjectionPath::default(),
                        })
                    })
            }
            NormalizedBindingLowering::Erased => None,
        }
    }

    fn normalize_value_provenance(
        &mut self,
        local: SLocalId,
        source: Option<LocalBinding<'db>>,
        provenance: ValueProvenance<'db>,
        value_ty: TyId<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        match provenance {
            ValueProvenance::Ordinary => Ok(self.local_root_place(local, source)),
            ValueProvenance::RootProvider(binding) => {
                Ok(self.provider_root_place(binding, value_ty))
            }
        }
    }

    fn normalize_place_provenance(
        &mut self,
        local: SLocalId,
        provenance: PlaceProvenance<'db>,
        value_ty: TyId<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        match provenance {
            PlaceProvenance::RootProvider(binding) => {
                Ok(self.provider_root_place(binding, value_ty))
            }
            PlaceProvenance::Derived(place) => self.normalize_derived_place(local, &place),
        }
    }

    fn normalize_snapshot_source(
        &mut self,
        local: SLocalId,
        snapshot_source: PlaceProvenance<'db>,
        value_ty: TyId<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        match snapshot_source {
            PlaceProvenance::RootProvider(binding) => {
                Ok(self.provider_root_place(binding, value_ty))
            }
            PlaceProvenance::Derived(place) => self.normalize_snapshot_derived_place(local, &place),
        }
    }

    fn normalize_layout_backing_source(
        &mut self,
        local: SLocalId,
        lowering: &NormalizedBindingLowering<'db>,
        layout_backing_source: LayoutBackingPlace<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        match layout_backing_source {
            LayoutBackingPlace::RootProvider {
                provider,
                value_ty,
                path,
            } => {
                let mut place = self.provider_root_place(provider, value_ty);
                place.path = place.path.concat(&path);
                Ok(place)
            }
            LayoutBackingPlace::Local(place) => {
                self.normalize_layout_derived_place(local, lowering, &place)
            }
        }
    }

    fn normalize_layout_derived_place(
        &mut self,
        local: SLocalId,
        lowering: &NormalizedBindingLowering<'db>,
        source_place: &SPlace<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        let base = source_place.local;
        let raw_base = self.raw.locals[base.index()].clone();
        let (selected_source, self_source) = if base == local {
            let source = lowering.place().cloned().or_else(|| {
                lowering.root().map(|root| NSPlace {
                    root: NSPlaceRoot::Root(root),
                    path: ProjectionPath::default(),
                })
            });
            (None, source)
        } else {
            self.ensure_local_normalized(base, &raw_base)?;
            (
                self.locals[base.index()].as_ref().and_then(|local| {
                    resolve_normalized_layout_backing_source(
                        local.layout_backing_sources(),
                        &source_place.path,
                    )
                }),
                None,
            )
        };
        let mut place = selected_source
            .clone()
            .or(self_source)
            .or_else(|| self.propagated_place(base))
            .or_else(|| {
                matches!(
                    raw_base.source,
                    Some(
                        LocalBinding::Param {
                            site: ParamSite::Func(_) | ParamSite::EffectField(_),
                            ..
                        } | LocalBinding::EffectParam { .. }
                    )
                )
                .then(|| self.local_root_place(base, raw_base.source))
            })
            .ok_or(SemanticNormalizeError::NonPlaceDerivedValue {
                owner: self.instance,
                local,
                base,
            })?;
        if selected_source.is_none() {
            place.path = place.path.concat(&source_place.path);
        }
        Ok(place)
    }

    fn normalize_snapshot_derived_place(
        &mut self,
        local: SLocalId,
        source_place: &SPlace<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        let base = source_place.local;
        let raw_base = self.raw.locals[base.index()].clone();
        self.ensure_local_normalized(base, &raw_base)?;
        let mut place = self.snapshot_source_base_place(local, base)?;
        place.path = place.path.concat(&source_place.path);
        Ok(place)
    }

    fn snapshot_source_base_place(
        &mut self,
        local: SLocalId,
        base: SLocalId,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        self.locals
            .get(base.index())
            .and_then(|local| local.as_ref())
            .and_then(|local| local.snapshot_source_place().cloned())
            .or_else(|| self.propagated_place(base))
            .ok_or(SemanticNormalizeError::NonPlaceDerivedValue {
                owner: self.instance,
                local,
                base,
            })
    }

    fn normalize_derived_place(
        &mut self,
        local: SLocalId,
        source_place: &SPlace<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        let base = source_place.local;
        let raw_base = self.raw.locals[base.index()].clone();
        self.ensure_local_normalized(base, &raw_base)?;
        let mut place =
            self.propagated_place(base)
                .ok_or(SemanticNormalizeError::NonPlaceDerivedValue {
                    owner: self.instance,
                    local,
                    base,
                })?;
        place.path = place.path.concat(&source_place.path);
        Ok(place)
    }

    fn push_local_root(
        &mut self,
        local: SLocalId,
        source: Option<LocalBinding<'db>>,
    ) -> NBorrowRootId {
        let root = NBorrowRootId::from_u32(self.borrow_roots.len() as u32);
        let param_idx = source.and_then(|binding| match binding {
            LocalBinding::Param { idx, .. } => Some(idx as u32),
            _ => None,
        });
        self.borrow_roots.push(if let Some(param_idx) = param_idx {
            NBorrowRoot::Param { local, param_idx }
        } else {
            NBorrowRoot::LocalSlot { local }
        });
        root
    }

    fn local_root_place(
        &mut self,
        local: SLocalId,
        source: Option<LocalBinding<'db>>,
    ) -> NSPlace<'db> {
        NSPlace {
            root: NSPlaceRoot::Root(self.push_local_root(local, source)),
            path: ProjectionPath::default(),
        }
    }

    fn push_provider_root(
        &mut self,
        binding: crate::semantic::ProviderBinding<'db>,
        value_ty: TyId<'db>,
    ) -> NBorrowRootId {
        let root = NBorrowRootId::from_u32(self.borrow_roots.len() as u32);
        self.borrow_roots
            .push(NBorrowRoot::Provider { binding, value_ty });
        root
    }

    fn provider_root_place(
        &mut self,
        binding: crate::semantic::ProviderBinding<'db>,
        value_ty: TyId<'db>,
    ) -> NSPlace<'db> {
        NSPlace {
            root: NSPlaceRoot::Root(self.push_provider_root(binding, value_ty)),
            path: ProjectionPath::default(),
        }
    }

    fn normalize_stmt(
        &mut self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        stmt: &SStmtKind<'db>,
    ) -> Result<NSStmtKind<'db>, SemanticNormalizeError<'db>> {
        let stmt = match stmt {
            SStmtKind::Assign { dst, expr } => Ok(NSStmtKind::Assign {
                dst: *dst,
                expr: self.normalize_expr(origin, *dst, expr)?,
            }),
            SStmtKind::Store { dst, src } => Ok(NSStmtKind::Store {
                dst: self.normalize_place(dst)?,
                src: self.normalize_operand(*src, origin),
            }),
        }?;
        self.mark_stmt_root_demand(&stmt);
        Ok(stmt)
    }

    fn normalize_terminator(
        &mut self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        term: &STerminatorKind<'db>,
    ) -> NSTerminatorKind<'db> {
        match term {
            STerminatorKind::Goto(bb) => NSTerminatorKind::Goto(*bb),
            STerminatorKind::Branch {
                cond,
                then_bb,
                else_bb,
            } => NSTerminatorKind::Branch {
                cond: self.normalize_operand(*cond, origin),
                then_bb: *then_bb,
                else_bb: *else_bb,
            },
            STerminatorKind::MatchEnum {
                value,
                enum_ty,
                cases,
                default,
            } => NSTerminatorKind::MatchEnum {
                value: self.normalize_operand(*value, origin),
                enum_ty: *enum_ty,
                cases: cases.clone(),
                default: *default,
            },
            STerminatorKind::Assert { message } => NSTerminatorKind::Assert { message: *message },
            STerminatorKind::Return(value) => {
                NSTerminatorKind::Return(value.map(|value| self.normalize_operand(value, origin)))
            }
        }
    }

    fn normalize_expr(
        &mut self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        dst: SLocalId,
        expr: &SExpr<'db>,
    ) -> Result<NExpr<'db>, SemanticNormalizeError<'db>> {
        let dst_ty = self.locals[dst.index()]
            .as_ref()
            .expect("all locals normalized before block lowering")
            .ty;
        Ok(match expr {
            SExpr::Forward(value) => NExpr::Use(self.normalize_operand(*value, origin)),
            SExpr::UseValue(value) => self
                .normalize_direct_read(origin, *value, dst_ty)?
                .unwrap_or(NExpr::Use(self.normalize_operand(*value, origin))),
            SExpr::CodeRegionRef { region } => NExpr::CodeRegionRef {
                region: region.clone(),
            },
            SExpr::Const(const_) => NExpr::Const(const_.clone()),
            SExpr::Unary { op, value } => NExpr::Unary {
                op: *op,
                value: self.normalize_operand(*value, origin),
            },
            SExpr::Binary { op, lhs, rhs } => NExpr::Binary {
                op: *op,
                lhs: self.normalize_operand(*lhs, origin),
                rhs: self.normalize_operand(*rhs, origin),
            },
            SExpr::Cast { value, to } => NExpr::Cast {
                value: self.normalize_operand(*value, origin),
                to: *to,
            },
            SExpr::ArrayRepeat { ty, value } => NExpr::ArrayRepeat {
                ty: *ty,
                value: self.normalize_operand(*value, origin),
            },
            SExpr::AggregateMake { ty, fields } => NExpr::AggregateMake {
                ty: *ty,
                fields: fields
                    .iter()
                    .map(|field| self.normalize_operand(*field, origin))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            SExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => NExpr::EnumMake {
                enum_ty: *enum_ty,
                variant: *variant,
                fields: fields
                    .iter()
                    .map(|field| self.normalize_operand(*field, origin))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            SExpr::ReadPlace { place } => {
                let place = self.normalize_place(place)?;
                NExpr::ReadPlace {
                    mode: self.read_mode_for_place(origin, dst_ty, &place),
                    place,
                }
            }
            SExpr::Field { base, field } => {
                let place =
                    self.project_local_place(base.value, Projection::Field(field.0 as usize))?;
                NExpr::ReadPlace {
                    mode: self.read_mode_for_place(origin, dst_ty, &place),
                    place,
                }
            }
            SExpr::Index { base, index } => {
                let place = self.project_local_place(
                    base.value,
                    Projection::Index(IndexSource::Dynamic(index.value)),
                )?;
                NExpr::ReadPlace {
                    mode: self.read_mode_for_place(origin, dst_ty, &place),
                    place,
                }
            }
            SExpr::Borrow {
                place,
                kind,
                provider,
            } => NExpr::Borrow {
                place: self.normalize_place(place)?,
                kind: *kind,
                provider: *provider,
            },
            SExpr::GetEnumTag { value } => NExpr::GetEnumTag {
                value: self.normalize_copy_operand(*value, origin),
            },
            SExpr::IsEnumVariant { value, variant } => NExpr::IsEnumVariant {
                value: self.normalize_copy_operand(*value, origin),
                variant: *variant,
            },
            SExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => NExpr::ExtractEnumField {
                value: self.normalize_operand(*value, origin),
                variant: *variant,
                field: *field,
            },
            SExpr::CodeRegionOffset { target } => NExpr::CodeRegionOffset {
                target: target.clone(),
            },
            SExpr::CodeRegionLen { target } => NExpr::CodeRegionLen {
                target: target.clone(),
            },
            SExpr::Call {
                call_site,
                callee,
                args,
                effect_args,
            } => NExpr::Call {
                call_site: *call_site,
                callee: *callee,
                args: args
                    .iter()
                    .enumerate()
                    .map(|(idx, arg)| self.normalize_call_arg(*callee, idx, *arg, origin))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                effect_args: effect_args
                    .iter()
                    .map(|arg| self.normalize_effect_arg(arg, origin))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_boxed_slice(),
            },
        })
    }

    fn normalize_effect_arg(
        &mut self,
        arg: &crate::analysis::semantic::SEffectArg<'db>,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> Result<NEffectArg<'db>, SemanticNormalizeError<'db>> {
        Ok(NEffectArg {
            binding_idx: arg.binding_idx,
            arg: match &arg.arg {
                crate::analysis::semantic::SEffectArgValue::Place(place) => {
                    NEffectArgValue::Place(self.normalize_place(place)?)
                }
                crate::analysis::semantic::SEffectArgValue::Value(value)
                    if matches!(arg.pass_mode, EffectPassMode::ByTempPlace) =>
                {
                    NEffectArgValue::Value(self.normalize_copy_operand(*value, origin))
                }
                crate::analysis::semantic::SEffectArgValue::Value(value) => {
                    NEffectArgValue::Value(self.normalize_operand(*value, origin))
                }
            },
            pass_mode: arg.pass_mode,
            required_mut: arg.required_mut,
            target_ty: arg.target_ty,
            provider: arg.provider,
        })
    }

    fn normalize_direct_read(
        &mut self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        operand: SOperand,
        ty: TyId<'db>,
    ) -> Result<Option<NExpr<'db>>, SemanticNormalizeError<'db>> {
        let origin = operand.sem_origin(origin);
        let Some(crate::analysis::semantic::SemOrigin::Expr(_)) = Some(origin) else {
            return Ok(None);
        };
        let Some(place) = self.local_read_place(operand.value, false)? else {
            return Ok(None);
        };
        let mode = self.read_mode_for_place(origin, ty, &place);
        Ok(Some(NExpr::ReadPlace { place, mode }))
    }

    fn normalize_operand(
        &self,
        operand: SOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> NOperand {
        let origin = operand.sem_origin(origin);
        let local = operand.value;
        let ty = self.locals[local.index()]
            .as_ref()
            .expect("all locals normalized before operand lowering")
            .ty;
        NOperand {
            local,
            origin: Self::origin_expr(origin),
            mode: self.read_mode_for_operand(local, origin, ty),
        }
    }

    fn normalize_copy_operand(
        &self,
        operand: SOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> NOperand {
        let origin = operand.sem_origin(origin);
        NOperand {
            local: operand.value,
            origin: Self::origin_expr(origin),
            mode: ReadMode::Copy,
        }
    }

    fn normalize_call_arg(
        &self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        idx: usize,
        operand: SOperand,
        origin: crate::analysis::semantic::SemOrigin<'db>,
    ) -> NOperand {
        let origin = operand.sem_origin(origin);
        let local = operand.value;
        let ty = self.locals[local.index()]
            .as_ref()
            .expect("all locals normalized before call arg lowering")
            .ty;
        let mode = match callee.key.owner(self.db) {
            crate::analysis::ty::ty_check::BodyOwner::Func(func) => func
                .params(self.db)
                .nth(idx)
                .map(|param| param.mode(self.db))
                .filter(|mode| *mode == crate::hir_def::FuncParamMode::View)
                .map(|_| self.read_mode_for_view_call_arg(ty))
                .unwrap_or_else(|| self.read_mode_for_operand(local, origin, ty)),
            _ => self.read_mode_for_operand(local, origin, ty),
        };
        NOperand {
            local,
            origin: Self::origin_expr(origin),
            mode,
        }
    }

    fn origin_expr(origin: crate::analysis::semantic::SemOrigin<'db>) -> Option<ExprId> {
        match origin {
            crate::analysis::semantic::SemOrigin::Expr(expr) => Some(expr),
            crate::analysis::semantic::SemOrigin::Stmt(_)
            | crate::analysis::semantic::SemOrigin::Body(_)
            | crate::analysis::semantic::SemOrigin::Synthetic => None,
        }
    }

    fn project_local_place(
        &mut self,
        local: SLocalId,
        projection: Projection<TyId<'db>, crate::analysis::semantic::VariantIndex, SLocalId>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        let mut place = self
            .local_read_place(local, true)?
            .ok_or(SemanticNormalizeError::MissingBorrowRoot { local })?;
        place.path.push(projection);
        Ok(place)
    }

    fn normalize_place(
        &mut self,
        place: &SPlace<'db>,
    ) -> Result<NSPlace<'db>, SemanticNormalizeError<'db>> {
        let mut lowered = self
            .local_read_place(place.local, true)?
            .ok_or(SemanticNormalizeError::MissingBorrowRoot { local: place.local })?;
        lowered.path = lowered.path.concat(&place.path);
        Ok(lowered)
    }

    fn local_read_place(
        &mut self,
        local: SLocalId,
        allow_carrier: bool,
    ) -> Result<Option<NSPlace<'db>>, SemanticNormalizeError<'db>> {
        let Some(local_data) = self
            .locals
            .get(local.index())
            .and_then(|local| local.as_ref())
        else {
            return Ok(None);
        };
        Ok(match &local_data.lowering {
            NormalizedBindingLowering::Erased => None,
            NormalizedBindingLowering::ValueLocal { place }
            | NormalizedBindingLowering::PlaceBoundValue { place, .. } => Some(place.clone()),
            NormalizedBindingLowering::CarrierLocal { .. } if !allow_carrier => None,
            NormalizedBindingLowering::CarrierLocal {
                root,
                provider,
                target_ty,
            } => {
                let root = *root;
                let provider = provider.clone();
                let target_ty = *target_ty;
                let is_capability = local_data.ty.as_capability(self.db).is_some();
                provider
                    .map(|provider| self.provider_root_place(provider, target_ty))
                    .or_else(|| {
                        is_capability.then_some(NSPlace {
                            root: NSPlaceRoot::CarrierDerefLocal(local),
                            path: ProjectionPath::default(),
                        })
                    })
                    .or_else(|| {
                        root.map(|root| NSPlace {
                            root: NSPlaceRoot::Root(root),
                            path: ProjectionPath::default(),
                        })
                    })
            }
        })
    }

    fn read_mode_for_view_call_arg(&self, ty: TyId<'db>) -> ReadMode {
        self.copy_or_read_mode(ty)
    }

    fn ty_is_copy(&self, ty: TyId<'db>) -> bool {
        if let Some(is_copy) = self.copy_cache.borrow().get(&ty).copied() {
            return is_copy;
        }
        let is_copy = ty_is_copy(
            self.db,
            self.raw.template_owner.scope(),
            ty,
            self.assumptions,
        );
        self.copy_cache.borrow_mut().insert(ty, is_copy);
        is_copy
    }

    fn copy_or_read_mode(&self, ty: TyId<'db>) -> ReadMode {
        let scope = self.raw.template_owner.scope();
        let ty = normalize_ty(self.db, ty, scope, self.assumptions);
        let value_ty = ty
            .as_capability(self.db)
            .map(|(_, inner)| normalize_ty(self.db, inner, scope, self.assumptions))
            .unwrap_or(ty);
        if self.ty_is_copy(value_ty) {
            ReadMode::Copy
        } else {
            ReadMode::Read
        }
    }

    fn origin_is_implicit_move(&self, origin: crate::analysis::semantic::SemOrigin<'db>) -> bool {
        matches!(
            origin,
            crate::analysis::semantic::SemOrigin::Expr(expr)
                if self
                    .instance
                    .key(self.db)
                    .instantiate_typed_body(self.db)
                    .is_implicit_move(expr)
        )
    }

    fn read_mode_for_capability_place(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        ty: TyId<'db>,
    ) -> ReadMode {
        if self.origin_is_implicit_move(origin) {
            ReadMode::Move
        } else {
            self.copy_or_read_mode(ty)
        }
    }

    fn read_mode(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        ty: TyId<'db>,
    ) -> ReadMode {
        if self.origin_is_implicit_move(origin) || !self.ty_is_copy(ty) {
            ReadMode::Move
        } else {
            ReadMode::Copy
        }
    }

    fn read_mode_for_operand(
        &self,
        local: SLocalId,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        ty: TyId<'db>,
    ) -> ReadMode {
        let Some(local) = self
            .locals
            .get(local.index())
            .and_then(|local| local.as_ref())
        else {
            return self.read_mode(origin, ty);
        };
        if !local_has_runtime_move_semantics(self.db, local, &self.borrow_roots) {
            return ReadMode::Copy;
        }
        if !self.ty_is_copy(ty) {
            return ReadMode::Move;
        }
        match origin {
            crate::analysis::semantic::SemOrigin::Expr(expr)
                if self
                    .instance
                    .key(self.db)
                    .instantiate_typed_body(self.db)
                    .is_implicit_move(expr) =>
            {
                ReadMode::Move
            }
            _ => ReadMode::Copy,
        }
    }

    fn read_mode_for_place(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        ty: TyId<'db>,
        place: &NSPlace<'db>,
    ) -> ReadMode {
        match place.root {
            NSPlaceRoot::CarrierDerefLocal(local) => {
                self.read_mode_for_carrier_deref_local(origin, ty, local)
            }
            NSPlaceRoot::Root(root) => self.read_mode_for_root(origin, ty, root),
        }
    }

    fn read_mode_for_carrier_deref_local(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        ty: TyId<'db>,
        local: SLocalId,
    ) -> ReadMode {
        let Some(local) = self
            .locals
            .get(local.index())
            .and_then(|local| local.as_ref())
        else {
            return ReadMode::Copy;
        };
        if local.ty.as_capability(self.db).is_none() {
            return ReadMode::Copy;
        }
        self.read_mode_for_capability_place(origin, ty)
    }

    fn read_mode_for_root(
        &self,
        origin: crate::analysis::semantic::SemOrigin<'db>,
        ty: TyId<'db>,
        root: NBorrowRootId,
    ) -> ReadMode {
        match self.borrow_roots.get(root.index()) {
            Some(NBorrowRoot::Provider { .. }) => ReadMode::Copy,
            Some(NBorrowRoot::Param { param_idx, .. })
                if self
                    .instance
                    .key(self.db)
                    .instantiate_typed_body(self.db)
                    .param_binding(*param_idx as usize)
                    .is_some_and(|binding| {
                        matches!(
                            binding,
                            LocalBinding::Param {
                                mode: crate::hir_def::FuncParamMode::View,
                                ..
                            }
                        )
                    }) =>
            {
                self.read_mode_for_capability_place(origin, ty)
            }
            Some(NBorrowRoot::Param { .. }) | Some(NBorrowRoot::LocalSlot { .. }) | None => {
                self.read_mode(origin, ty)
            }
        }
    }
}
