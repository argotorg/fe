use std::cell::RefCell;

use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            EvalOutcome, FieldIndex, LayoutBackingPlace, LayoutBackingProjection, PlaceProvenance,
            SBlockId, SConst, SExpr, SLocal, SLocalId, SOperand, SPlace, SStmtId, SStmtKind,
            STerminatorKind, SemConstValue, SemOrigin, SemanticBody, SemanticInstance,
            SemanticLocalRole, ValueProvenance, VariantIndex,
            capability::array::ArrayLength,
            capability::semantics::contains_capability,
            eval_const_ref,
            lower::layout_backing_source_path_is_prefix,
            normalized::{
                NBlock, NBlockId, NDataPath, NDataProjection, NDropTarget, NEffectArg,
                NEffectArgValue, NExpr, NIndex, NOperand, NPlace, NPlaceBase, NRoot, NRootId,
                NRootKind, NStatement, NStatementId, NStatementKind, NStructuralPath, NSuccessor,
                NTerminator, NTerminatorKind, NValue, NValueDefinition, NValueId, NormalizedBody,
                ReadMode, StructuralRepack, ViewAccess, copied_scalar_ty, is_literal_allocation,
                layout_plan::{
                    NLayoutBackingSource, NLayoutPlan, NLayoutProjection, NLayoutSourcePath,
                    NLayoutUseBacking, NRootRepresentation, NValueRepresentation,
                },
                verify::project_path_ty,
            },
            sem_const_ty,
        },
        ty::{
            adt_def::AdtRef,
            provider::{AddressSpace, ProviderKind},
            trait_resolution::PredicateListId,
            ty_check::{BindingAccess, BodyOwner, LocalBinding},
            ty_def::{BorrowKind, CapabilityKind, TyData, TyId},
            ty_is_copy,
        },
    },
    projection::{IndexSource, Projection},
    semantic::ProviderBinding,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NormalizeError<'db> {
    MissingValue(SLocalId),
    MissingRoot(SLocalId),
    MissingProviderAddressSpace(ProviderBinding<'db>),
    InvalidProjection,
    UnresolvedCapabilityConst(TyId<'db>),
    UnsupportedPlaceProjection,
    UnsupportedCapabilityCast { from: TyId<'db>, to: TyId<'db> },
    InvalidControlFlow,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct NormalizedArtifacts<'db> {
    pub body: NormalizedBody<'db>,
    pub layout_plan: NLayoutPlan<'db>,
}

pub fn normalize_raw_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    raw: &SemanticBody<'db>,
    assumptions: PredicateListId<'db>,
) -> Result<NormalizedArtifacts<'db>, NormalizeError<'db>> {
    NormalizeCx::new(db, instance, raw, assumptions).normalize()
}

struct NormalizeCx<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    raw: &'a SemanticBody<'db>,
    assumptions: PredicateListId<'db>,
    roots: Vec<NRoot<'db>>,
    root_for_local: Vec<Option<NRootId>>,
    provider_target_root_for_local: Vec<Option<NRootId>>,
    provider_roots: FxHashMap<(ProviderBinding<'db>, TyId<'db>), NRootId>,
    phi_locals: Vec<Vec<SLocalId>>,
    phi_values: Vec<Vec<NValueId>>,
    values: Vec<NValue<'db>>,
    value_sources: Vec<NValueRepresentation>,
    root_sources: Vec<NRootRepresentation>,
    use_backings: Vec<NLayoutUseBacking<'db>>,
    blocks: Vec<NBlock<'db>>,
    statement_count: usize,
    current_values: Vec<Vec<NValueId>>,
    projection_values: Vec<Option<NValueId>>,
    copy_cache: RefCell<FxHashMap<TyId<'db>, bool>>,
    /// The carrier each raw statement defined, so `End` can name the access.
    defined: FxHashMap<SStmtId, NValueId>,
}

impl<'a, 'db> NormalizeCx<'a, 'db> {
    fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        raw: &'a SemanticBody<'db>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let local_count = raw.locals.len();
        Self {
            db,
            instance,
            raw,
            assumptions,
            roots: Vec::new(),
            root_for_local: vec![None; local_count],
            provider_target_root_for_local: vec![None; local_count],
            provider_roots: FxHashMap::default(),
            phi_locals: vec![Vec::new(); raw.blocks.len()],
            phi_values: vec![Vec::new(); raw.blocks.len()],
            values: Vec::new(),
            value_sources: Vec::new(),
            root_sources: Vec::new(),
            use_backings: Vec::new(),
            blocks: Vec::new(),
            statement_count: 0,
            current_values: vec![Vec::new(); local_count],
            projection_values: vec![None; local_count],
            copy_cache: RefCell::new(FxHashMap::default()),
            defined: FxHashMap::default(),
        }
    }

    fn normalize(mut self) -> Result<NormalizedArtifacts<'db>, NormalizeError<'db>> {
        if self.raw.blocks.is_empty() {
            return self.normalize_empty_body();
        }

        self.classify_roots()?;
        let cfg = RawCfg::new(self.raw);
        self.phi_locals = cfg.phi_locals(self.raw, &self.root_for_local);
        self.blocks = (0..self.raw.blocks.len())
            .map(|_| NBlock {
                params: Box::default(),
                statements: Vec::new(),
                terminator: NTerminator {
                    origin: crate::analysis::semantic::SemOrigin::Synthetic,
                    kind: NTerminatorKind::Assert { message: None },
                },
            })
            .collect();

        self.allocate_entry_values()?;
        self.allocate_phi_values();

        for root in cfg.dominator_roots() {
            self.rename_block(root, &cfg)?;
        }

        let unsafe_splits = self.unsafe_splits();
        let body = NormalizedBody {
            owner: self.instance,
            template_owner: self.raw.template_owner,
            values: self.values,
            roots: self.roots,
            blocks: self.blocks,
            entry: NBlockId::new(0),
            unsafe_splits,
        };
        Ok(NormalizedArtifacts {
            body,
            layout_plan: NLayoutPlan {
                value_representations: self.value_sources,
                root_representations: self.root_sources,
                use_backings: self.use_backings,
            },
        })
    }

    /// The carriers the raw body's unsafe split components define.
    fn unsafe_splits(&self) -> Vec<Vec<NValueId>> {
        let defining = |local: SLocalId| {
            self.raw
                .blocks
                .iter()
                .flat_map(|block| &block.stmts)
                .find_map(|stmt| {
                    matches!(stmt.kind, SStmtKind::Assign { dst, .. } if dst == local)
                        .then(|| self.defined.get(&stmt.id).copied())
                        .flatten()
                })
        };
        self.raw
            .unsafe_splits
            .iter()
            .map(|split| split.iter().filter_map(|&local| defining(local)).collect())
            .collect()
    }

    fn normalize_empty_body(mut self) -> Result<NormalizedArtifacts<'db>, NormalizeError<'db>> {
        self.classify_roots()?;
        self.blocks.push(NBlock {
            params: Box::default(),
            statements: Vec::new(),
            terminator: NTerminator {
                origin: crate::analysis::semantic::SemOrigin::Body(self.raw.template_owner),
                kind: NTerminatorKind::Return(None),
            },
        });
        self.allocate_entry_values()?;
        Ok(NormalizedArtifacts {
            body: NormalizedBody {
                owner: self.instance,
                template_owner: self.raw.template_owner,
                values: self.values,
                roots: self.roots,
                blocks: self.blocks,
                entry: NBlockId::new(0),
                unsafe_splits: Vec::new(),
            },
            layout_plan: NLayoutPlan {
                value_representations: self.value_sources,
                root_representations: self.root_sources,
                use_backings: self.use_backings,
            },
        })
    }

    fn classify_roots(&mut self) -> Result<(), NormalizeError<'db>> {
        let mut needs_slot = vec![false; self.raw.locals.len()];
        for (index, local) in self.raw.locals.iter().enumerate() {
            let direct_capability = local.ty.as_capability(self.db).is_some();
            needs_slot[index] =
                !direct_capability && local.source.is_some_and(|binding| binding.is_mut());
        }
        for block in &self.raw.blocks {
            for statement in &block.stmts {
                match &statement.kind {
                    SStmtKind::Assign { dst, expr } => {
                        for_each_address_required_local(expr, |local| {
                            self.mark_address_root(local, &mut needs_slot)
                        });
                        if let SExpr::ReadPlace { place } = expr
                            && self.normalized_local_ty(*dst).as_view(self.db).is_some()
                            && !matches!(place.path.iter().next(), Some(Projection::Deref))
                        {
                            // A contextual view borrows the projected storage;
                            // it must not materialize an owned field snapshot.
                            self.mark_address_root(place.local, &mut needs_slot);
                        }
                        if let SExpr::Call { callee, args, .. } = expr {
                            for (index, argument) in args.iter().enumerate() {
                                if self
                                    .call_view_target(*callee, index, argument.value)
                                    .is_some()
                                {
                                    self.mark_address_root(argument.value, &mut needs_slot);
                                }
                            }
                        }
                    }
                    SStmtKind::Store { dst, .. } => {
                        if !matches!(dst.path.iter().next(), Some(Projection::Deref)) {
                            self.mark_address_root(dst.local, &mut needs_slot);
                        }
                    }
                    SStmtKind::End { .. } | SStmtKind::Drop { .. } => {}
                }
            }
        }

        for (index, needs_slot) in needs_slot.into_iter().enumerate() {
            let local_id = SLocalId::new(index);
            let local = &self.raw.locals[index];
            let direct_capability = local.ty.as_capability(self.db).is_some();
            let provider = semantic_root_provider(&local.role, local.ty);
            let root = if let Some((provider, value_ty)) = provider {
                let root = self.provider_root(provider, value_ty)?;
                if direct_capability {
                    self.provider_target_root_for_local[index] = Some(root);
                    None
                } else {
                    Some(root)
                }
            } else if needs_slot {
                Some(self.local_slot_root(local_id))
            } else {
                None
            };
            self.root_for_local[index] = root;
        }
        Ok(())
    }

    fn provider_root(
        &mut self,
        binding: ProviderBinding<'db>,
        value_ty: TyId<'db>,
    ) -> Result<NRootId, NormalizeError<'db>> {
        let value_ty = self.instance.normalized_ty(self.db, value_ty);
        if let Some(root) = self.provider_roots.get(&(binding.clone(), value_ty)) {
            return Ok(*root);
        }
        let address_space = binding.semantics.address_space.or_else(|| {
            matches!(binding.semantics.kind, ProviderKind::RootObject)
                .then_some(AddressSpace::Memory)
        });
        let Some(address_space) = address_space else {
            return Err(NormalizeError::MissingProviderAddressSpace(binding));
        };
        let root = NRootId::new(self.roots.len());
        self.roots.push(NRoot {
            kind: NRootKind::Provider {
                binding: binding.clone(),
            },
            ty: value_ty,
            address_space,
            mutability: crate::analysis::semantic::Mutability::Mutable,
            origin: crate::analysis::semantic::SemOrigin::Body(self.raw.template_owner),
        });
        self.root_sources.push(NRootRepresentation {
            root,
            source_local: None,
        });
        self.provider_roots.insert((binding, value_ty), root);
        Ok(root)
    }

    fn local_slot_root(&mut self, local: SLocalId) -> NRootId {
        let local_data = &self.raw.locals[local.index()];
        let param = local_data.source.and_then(|binding| match binding {
            LocalBinding::Param { idx, .. } => Some(idx as u32),
            _ => None,
        });
        let root = NRootId::new(self.roots.len());
        self.roots.push(NRoot {
            kind: param.map_or_else(
                || NRootKind::LocalSlot {
                    binding: local_data.source,
                },
                |param| NRootKind::ParamPlace { param },
            ),
            ty: self.normalized_local_ty(local),
            address_space: AddressSpace::Memory,
            mutability: local_data.mutability,
            origin: crate::analysis::semantic::SemOrigin::Body(self.raw.template_owner),
        });
        self.root_sources.push(NRootRepresentation {
            root,
            source_local: Some(local),
        });
        root
    }

    fn allocate_entry_values(&mut self) -> Result<(), NormalizeError<'db>> {
        let mut values = Vec::new();
        for (param, local) in self.raw.entry_locals.iter().copied().enumerate() {
            if self.root_for_local[local.index()].is_some() {
                continue;
            }
            let local_data = &self.raw.locals[local.index()];
            let value = self.push_value(
                self.normalized_local_ty(local),
                crate::analysis::semantic::SemOrigin::Body(self.raw.template_owner),
                NValueDefinition::EntryParam {
                    param: param as u32,
                },
                local_data.source,
                local,
            );
            self.current_values[local.index()].push(value);
            values.push((value, local));
        }
        for (value, local) in values {
            self.prepare_layout_backing_indices(
                SBlockId::new(0),
                crate::analysis::semantic::SemOrigin::Body(self.raw.template_owner),
                local,
            )?;
            self.record_layout_backings(
                value,
                local,
                SBlockId::new(0),
                crate::analysis::semantic::SemOrigin::Body(self.raw.template_owner),
            )?;
        }
        Ok(())
    }

    fn allocate_phi_values(&mut self) {
        for block_index in 0..self.phi_locals.len() {
            let block = NBlockId::new(block_index);
            let locals = self.phi_locals[block_index].clone();
            let mut params = Vec::with_capacity(locals.len());
            for (index, local) in locals.into_iter().enumerate() {
                let local_data = &self.raw.locals[local.index()];
                params.push(self.push_value(
                    self.normalized_local_ty(local),
                    crate::analysis::semantic::SemOrigin::Synthetic,
                    NValueDefinition::BlockParam {
                        block,
                        index: index as u32,
                    },
                    local_data.source,
                    local,
                ));
            }
            self.phi_values[block_index] = params.clone();
            self.blocks[block_index].params = params.into_boxed_slice();
        }
    }

    fn rename_block(
        &mut self,
        raw_block: SBlockId,
        cfg: &RawCfg,
    ) -> Result<(), NormalizeError<'db>> {
        let block_index = raw_block.index();
        let mut pushed = Vec::new();
        for (local, value) in self.phi_locals[block_index]
            .iter()
            .copied()
            .zip(self.phi_values[block_index].iter().copied())
        {
            self.current_values[local.index()].push(value);
            pushed.push(local);
        }
        for statement in &self.raw.blocks[block_index].stmts {
            self.projection_values.fill(None);
            match &statement.kind {
                SStmtKind::Assign { dst, expr } => {
                    let mut normalized =
                        self.normalize_expr(raw_block, statement.origin, *dst, expr)?;
                    let mut source = Some(statement.id);
                    if let NExpr::Call { callee, .. } = &normalized {
                        let callee = SemanticInstance::new(self.db, callee.key);
                        let returned_ty = callee.normalized_result_ty(self.db);
                        let target_ty = self.normalized_local_ty(*dst);
                        if returned_ty.as_borrow(self.db).is_some()
                            && target_ty.as_capability(self.db).is_none()
                        {
                            // A contextual Copy read does not change the callee's
                            // result type. Bind the native carrier first, then
                            // perform the same explicit read as other operands.
                            let returned = self.emit_define(
                                raw_block,
                                source.take(),
                                statement.origin,
                                returned_ty,
                                *dst,
                                normalized,
                            )?;
                            let returned = self.operand(returned, statement.origin, ReadMode::Copy);
                            normalized = NExpr::Forward {
                                src: self.coerce_operand_as(
                                    raw_block,
                                    statement.origin,
                                    *dst,
                                    returned,
                                    target_ty,
                                )?,
                            };
                        }
                    }
                    if let NExpr::Call { callee, .. } = &normalized
                        && let Some(target) = self.normalized_local_ty(*dst).as_view(self.db)
                    {
                        let callee = SemanticInstance::new(self.db, callee.key);
                        let returned_ty = callee.normalized_result_ty(self.db);
                        if returned_ty.as_capability(self.db).is_none()
                            && !returned_ty.is_never(self.db)
                        {
                            if !structural_types_are_boundary_compatible(
                                self.db,
                                self.instance,
                                returned_ty,
                                target,
                            ) {
                                return Err(NormalizeError::InvalidProjection);
                            }
                            let returned = self.emit_define(
                                raw_block,
                                source.take(),
                                statement.origin,
                                returned_ty,
                                *dst,
                                normalized,
                            )?;
                            normalized = NExpr::MakeView {
                                place: self.materialize_value(returned, statement.origin),
                                access: ViewAccess::Read,
                            };
                        }
                    }
                    let result_ty = match &normalized {
                        NExpr::Borrow { place, kind, .. } => match kind {
                            crate::analysis::ty::ty_def::BorrowKind::Mut => {
                                TyId::borrow_mut_of(self.db, place.ty)
                            }
                            crate::analysis::ty::ty_def::BorrowKind::Ref => {
                                TyId::borrow_ref_of(self.db, place.ty)
                            }
                        },
                        NExpr::MakeView { place, .. } => TyId::view_of(self.db, place.ty),
                        NExpr::Forward { src } => self.values[src.value.index()].ty,
                        NExpr::Load { place, .. } => place.ty,
                        NExpr::ProjectValue { value, path } => project_path_ty(
                            self.db,
                            self.instance,
                            &self.values,
                            self.values[value.value.index()].ty,
                            &path.0,
                        )
                        .map_err(|_| NormalizeError::InvalidProjection)?,
                        NExpr::ScalarCast { to, .. } | NExpr::PointerCast { to, .. } => *to,
                        _ => self.normalized_local_ty(*dst),
                    };
                    // Reading a stored mutable handle creates a reborrow. Keep the
                    // structural extraction exact and represent that access explicitly.
                    if matches!(normalized, NExpr::Load { .. } | NExpr::ProjectValue { .. })
                        && let Some((BorrowKind::Mut, target)) = result_ty.as_borrow(self.db)
                    {
                        let carrier = self.emit_define(
                            raw_block,
                            None,
                            statement.origin,
                            result_ty,
                            *dst,
                            normalized,
                        )?;
                        normalized = NExpr::Borrow {
                            place: NPlace {
                                base: NPlaceBase::CapabilityTarget { carrier },
                                path: NDataPath::empty(),
                                ty: target,
                                origin: statement.origin,
                            },
                            kind: BorrowKind::Mut,
                            provider: None,
                        };
                    }
                    if let Some(root) = self.root_for_local[dst.index()] {
                        let result = self.emit_define(
                            raw_block,
                            source,
                            statement.origin,
                            result_ty,
                            *dst,
                            normalized,
                        )?;
                        let destination = self.root_place(root, statement.origin);
                        let mut value = self.operand(result, statement.origin, ReadMode::Move);
                        if result_ty != destination.ty {
                            let mapping = structural_repack_mapping(
                                self.db,
                                self.instance,
                                result_ty,
                                destination.ty,
                            )
                            .ok_or(
                                NormalizeError::UnsupportedCapabilityCast {
                                    from: result_ty,
                                    to: destination.ty,
                                },
                            )?;
                            let result = self.emit_define(
                                raw_block,
                                None,
                                statement.origin,
                                destination.ty,
                                *dst,
                                NExpr::StructuralRepack { value, mapping },
                            )?;
                            value = self.operand(result, statement.origin, ReadMode::Move);
                        }
                        self.emit_store(raw_block, None, statement.origin, destination, value);
                    } else {
                        let value = self.emit_define(
                            raw_block,
                            source,
                            statement.origin,
                            result_ty,
                            *dst,
                            normalized,
                        )?;
                        self.defined.insert(statement.id, value);
                        self.current_values[dst.index()].push(value);
                        pushed.push(*dst);
                    }
                }
                SStmtKind::Drop { place } => {
                    let target = if place.path.is_empty() && !self.local_has_place(place.local) {
                        NDropTarget::Value(self.current_value(place.local)?)
                    } else {
                        let mut target =
                            self.normalize_place(raw_block, statement.origin, place)?;
                        if target.ty.as_capability(self.db).is_some() {
                            target = self.dereference_place(
                                raw_block,
                                statement.origin,
                                place.local,
                                target,
                            )?;
                        }
                        NDropTarget::Place(target)
                    };
                    let id = NStatementId::new(self.statement_count);
                    self.statement_count += 1;
                    self.blocks[block_index].statements.push(NStatement {
                        id,
                        source: Some(statement.id),
                        origin: statement.origin,
                        kind: NStatementKind::Drop { target },
                    });
                }
                SStmtKind::End { access } => {
                    // An access the normalization folded away has nothing to close.
                    if let Some(&access) = self.defined.get(access) {
                        let id = NStatementId::new(self.statement_count);
                        self.statement_count += 1;
                        self.blocks[block_index].statements.push(NStatement {
                            id,
                            source: Some(statement.id),
                            origin: statement.origin,
                            kind: NStatementKind::End { access },
                        });
                    }
                }
                SStmtKind::Store { dst, src } => {
                    let destination = self.normalize_place(raw_block, statement.origin, dst)?;
                    let value = self.read_operand(raw_block, statement.origin, *src, None)?;
                    let destination = self.dereference_place_to(
                        raw_block,
                        statement.origin,
                        dst.local,
                        destination,
                        self.values[value.value.index()].ty,
                    )?;
                    self.emit_store(
                        raw_block,
                        Some(statement.id),
                        statement.origin,
                        destination,
                        value,
                    );
                }
            }
        }

        let terminator = self.normalize_terminator(raw_block)?;
        self.blocks[block_index].terminator = terminator;

        for child in cfg.dom_children[block_index].iter().copied() {
            self.rename_block(SBlockId::new(child), cfg)?;
        }
        for local in pushed.into_iter().rev() {
            self.current_values[local.index()].pop();
        }
        Ok(())
    }

    fn normalize_expr(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        dst: SLocalId,
        expr: &SExpr<'db>,
    ) -> Result<NExpr<'db>, NormalizeError<'db>> {
        let dst_ty = self.normalized_local_ty(dst);
        let normalized = match expr {
            SExpr::Forward(value) => NExpr::Forward {
                src: self.read_operand(block, origin, *value, Some(ReadMode::Copy))?,
            },
            SExpr::UseValue(value) => {
                let source_ty = self.normalized_local_ty(value.value);
                // A place alias has no carrier of its own: reading one as a
                // carrier accesses the aliased place.
                let alias = matches!(
                    self.raw.locals[value.value.index()].role,
                    SemanticLocalRole::PlaceBoundValue {
                        provenance: PlaceProvenance::Derived(_),
                        ..
                    }
                );
                if (source_ty != dst_ty || alias) && self.local_has_place(value.value) {
                    let place =
                        self.place_for_local(block, value.sem_origin(origin), value.value)?;
                    self.load_or_borrow_place(
                        block,
                        value.sem_origin(origin),
                        value.value,
                        dst_ty,
                        place,
                        None,
                    )?
                } else {
                    NExpr::Forward {
                        src: self.read_operand(block, origin, *value, None)?,
                    }
                }
            }
            SExpr::ReadPlace { place } | SExpr::InspectPlace { place } => {
                // An inspection reads the value in place; any other read takes
                // it.
                let mode = matches!(expr, SExpr::InspectPlace { .. }).then(|| {
                    if self.ty_is_copy(dst_ty) {
                        ReadMode::Copy
                    } else {
                        ReadMode::Read
                    }
                });
                if self.local_has_place(place.local)
                    || place
                        .path
                        .iter()
                        .any(|projection| matches!(projection, Projection::Deref))
                {
                    let normalized = self.normalize_place(block, origin, place)?;
                    self.load_or_borrow_place(
                        block,
                        origin,
                        place.local,
                        dst_ty,
                        normalized,
                        mode,
                    )?
                } else {
                    let value = self.read_operand(
                        block,
                        origin,
                        SOperand::inherited(place.local),
                        mode,
                    )?;
                    let path = self.normalize_path(block, origin, &place.path)?;
                    self.normalize_value_projection(
                        block,
                        origin,
                        place.local,
                        value,
                        path,
                        dst_ty,
                    )?
                }
            }
            SExpr::Field { base, field } => self.normalize_projection_expr(
                block,
                origin,
                *base,
                NDataProjection::Field(*field),
                dst_ty,
            )?,
            SExpr::Index { base, index } => {
                let index_local = index.value;
                let index =
                    self.read_scalar_operand(block, origin, *index, Some(ReadMode::Copy))?;
                self.projection_values[index_local.index()] = Some(index.value);
                self.normalize_projection_expr(
                    block,
                    origin,
                    *base,
                    NDataProjection::Index(NIndex::Value(index.value)),
                    dst_ty,
                )?
            }
            SExpr::ExtractEnumField {
                value,
                variant,
                field,
            } => self.normalize_projection_expr(
                block,
                origin,
                *value,
                NDataProjection::VariantField {
                    variant: *variant,
                    field: *field,
                },
                dst_ty,
            )?,
            SExpr::Borrow {
                place,
                kind,
                provider,
            } => {
                let mut normalized = self.normalize_place(block, origin, place)?;
                if let Some((_, target)) = dst_ty.as_borrow(self.db) {
                    normalized =
                        self.dereference_place_to(block, origin, place.local, normalized, target)?;
                }
                NExpr::Borrow {
                    place: normalized,
                    kind: *kind,
                    provider: *provider,
                }
            }
            SExpr::CodeRegionRef { region } => NExpr::CodeRegionRef {
                region: region.clone(),
            },
            SExpr::Const(value) => {
                self.normalize_constant(block, origin, dst, dst_ty, value.clone())?
            }
            SExpr::Unary { op, value } => NExpr::Unary {
                op: *op,
                value: self.read_scalar_operand(block, origin, *value, None)?,
            },
            SExpr::Binary { op, lhs, rhs } => NExpr::Binary {
                op: *op,
                lhs: self.read_scalar_operand(block, origin, *lhs, None)?,
                rhs: self.read_scalar_operand(block, origin, *rhs, None)?,
            },
            SExpr::Cast { value, to } => {
                let to = self.instance.normalized_ty(self.db, *to);
                let raw_value = *value;
                let materialize_place = matches!(
                    self.raw.locals[raw_value.value.index()].role,
                    SemanticLocalRole::PlaceCarrier { .. }
                        | SemanticLocalRole::PlaceBoundValue { .. }
                ) && to.as_capability(self.db).is_none()
                    && self.local_has_place(raw_value.value);
                let (value, from) = if materialize_place {
                    let place =
                        self.place_for_local(block, raw_value.sem_origin(origin), raw_value.value)?;
                    let mode = self.read_mode_for_place(place.ty);
                    let loaded = self.emit_define(
                        block,
                        None,
                        origin,
                        place.ty,
                        raw_value.value,
                        NExpr::Load { place, mode },
                    )?;
                    (
                        self.operand(loaded, origin, mode),
                        self.values[loaded.index()].ty,
                    )
                } else {
                    let value = self.read_operand(block, origin, raw_value, None)?;
                    let from = self.values[value.value.index()].ty;
                    (value, from)
                };
                // Lowering left only the leaves of a newtype cast.
                if from == to {
                    NExpr::Forward { src: value }
                } else if from.as_ptr(self.db).is_some() || to.as_ptr(self.db).is_some() {
                    NExpr::PointerCast { value, to }
                } else {
                    NExpr::ScalarCast { value, to }
                }
            }
            SExpr::ArrayRepeat { ty, value } => {
                let ty = self.instance.normalized_ty(self.db, *ty);
                let element_ty = ty
                    .decompose_ty_app(self.db)
                    .1
                    .first()
                    .copied()
                    .map(|ty| self.instance.normalized_ty(self.db, ty))
                    .filter(|_| ty.is_array(self.db))
                    .ok_or(NormalizeError::InvalidProjection)?;
                NExpr::ArrayRepeat {
                    ty,
                    value: self.read_operand_as(block, origin, *value, element_ty)?,
                }
            }
            SExpr::AggregateMake { ty, fields } => {
                let ty = self.instance.normalized_ty(self.db, *ty);
                let field_tys = if ty.is_array(self.db) {
                    let element_ty = ty
                        .decompose_ty_app(self.db)
                        .1
                        .first()
                        .copied()
                        .map(|ty| self.instance.normalized_ty(self.db, ty))
                        .ok_or(NormalizeError::InvalidProjection)?;
                    vec![element_ty; fields.len()]
                } else if ty.is_product(self.db) {
                    self.instance.normalized_field_types(self.db, ty).to_vec()
                } else {
                    return Err(NormalizeError::InvalidProjection);
                };
                if field_tys.len() != fields.len() {
                    return Err(NormalizeError::InvalidProjection);
                }
                NExpr::AggregateMake {
                    ty,
                    fields: fields
                        .iter()
                        .copied()
                        .zip(field_tys)
                        .map(|(field, field_ty)| {
                            self.read_operand_as(block, origin, field, field_ty)
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into_boxed_slice(),
                }
            }
            SExpr::EnumMake {
                enum_ty,
                variant,
                fields,
            } => {
                let enum_ty = self.instance.normalized_ty(self.db, *enum_ty);
                let adt = enum_ty
                    .adt_def(self.db)
                    .filter(|adt| matches!(adt.adt_ref(self.db), AdtRef::Enum(_)))
                    .ok_or(NormalizeError::InvalidProjection)?;
                let expected = adt
                    .fields(self.db)
                    .get(variant.0 as usize)
                    .ok_or(NormalizeError::InvalidProjection)?;
                if expected.num_types() != fields.len() {
                    return Err(NormalizeError::InvalidProjection);
                }
                NExpr::EnumMake {
                    enum_ty,
                    variant: *variant,
                    fields: fields
                        .iter()
                        .copied()
                        .enumerate()
                        .map(|(index, field)| {
                            self.read_operand_as(
                                block,
                                origin,
                                field,
                                self.instance
                                    .normalized_enum_variant_field_tys(self.db, enum_ty, *variant)
                                    [index],
                            )
                        })
                        .collect::<Result<Vec<_>, _>>()?
                        .into_boxed_slice(),
                }
            }
            SExpr::GetEnumTag { value } => NExpr::GetEnumTag {
                value: self.read_operand(block, origin, *value, Some(ReadMode::Copy))?,
            },
            SExpr::IsEnumVariant { value, variant } => NExpr::IsEnumVariant {
                value: self.read_operand(block, origin, *value, Some(ReadMode::Copy))?,
                variant: *variant,
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
                    .map(|(index, arg)| {
                        if let Some(target) = self.call_view_target(*callee, index, arg.value) {
                            let place =
                                self.place_for_local(block, arg.sem_origin(origin), arg.value)?;
                            let mut place = self.dereference_place_to(
                                block,
                                arg.sem_origin(origin),
                                arg.value,
                                place,
                                target,
                            )?;
                            // A `Copy` view argument is passed by copy. Copying
                            // a provider's place is observable when the callee's
                            // effects could write it, or when the copy validates
                            // untrusted enum tags.
                            let provider_copy = self.ty_is_copy(target)
                                && matches!(place.base, NPlaceBase::Root(root)
                                    if matches!(self.roots[root.index()].kind, NRootKind::Provider { .. })
                                        && match self.roots[root.index()].address_space {
                                            AddressSpace::Code => false,
                                            AddressSpace::Calldata => {
                                                ty_has_enum(self.db, target)
                                            }
                                            AddressSpace::Memory
                                            | AddressSpace::Storage
                                            | AddressSpace::Transient => true,
                                        });
                            if provider_copy
                                || structural_repack_mapping(self.db, self.instance, place.ty, target)
                                    .is_none()
                            {
                                let value =
                                    self.read_operand(block, origin, *arg, Some(ReadMode::Copy))?;
                                let from = self.values[value.value.index()].ty;
                                let mapping =
                                    structural_repack_mapping(self.db, self.instance, from, target)
                                        .ok_or(NormalizeError::UnsupportedCapabilityCast {
                                            from,
                                            to: target,
                                        })?;
                                let value_expr = if from == target {
                                    NExpr::Forward { src: value }
                                } else {
                                    NExpr::StructuralRepack { value, mapping }
                                };
                                let result = self.emit_define(
                                    block,
                                    None,
                                    arg.sem_origin(origin),
                                    target,
                                    arg.value,
                                    value_expr,
                                )?;
                                place = self.materialize_value(result, arg.sem_origin(origin));
                            }
                            let from = TyId::view_of(self.db, place.ty);
                            let to = TyId::view_of(self.db, target);
                            let mut result = self.emit_define(
                                block,
                                None,
                                origin,
                                from,
                                arg.value,
                                NExpr::MakeView {
                                    place,
                                    access: ViewAccess::Read,
                                },
                            )?;
                            if from != to {
                                let mapping =
                                    structural_repack_mapping(self.db, self.instance, from, to)
                                        .ok_or(NormalizeError::UnsupportedCapabilityCast {
                                            from,
                                            to,
                                        })?;
                                let value = self.operand(result, origin, ReadMode::Read);
                                result = self.emit_define(
                                    block,
                                    None,
                                    origin,
                                    to,
                                    arg.value,
                                    NExpr::StructuralRepack { value, mapping },
                                )?;
                            }
                            Ok(self.operand(result, origin, ReadMode::Read))
                        } else {
                            let value = self.read_operand(
                                block,
                                origin,
                                *arg,
                                self.call_arg_mode(*callee, index, arg.value),
                            )?;
                            let expected = self
                                .call_param_ty(*callee, index)
                                .ok_or(NormalizeError::InvalidProjection)?;
                            let actual = self.values[value.value.index()].ty;
                            let target = match (
                                actual.as_capability(self.db),
                                expected.as_capability(self.db),
                            ) {
                                (Some((kind, _)), Some((_, target))) => match kind {
                                    CapabilityKind::Mut => TyId::borrow_mut_of(self.db, target),
                                    CapabilityKind::Ref => TyId::borrow_ref_of(self.db, target),
                                    CapabilityKind::View => TyId::view_of(self.db, target),
                                },
                                (None, Some((_, target))) => target,
                                (_, None) => expected,
                            };
                            self.coerce_operand_as(block, origin, arg.value, value, target)
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?
                    .into_boxed_slice(),
                effect_args: effect_args
                    .iter()
                    .map(|arg| {
                        Ok(NEffectArg {
                            binding_idx: arg.binding_idx,
                            arg: match &arg.arg {
                                crate::analysis::semantic::SEffectArgValue::Place(place) => {
                                    NEffectArgValue::Place(
                                        self.normalize_place(block, origin, place)?,
                                    )
                                }
                                // A provider passed by value lends the effect
                                // for the call; it does not move.
                                crate::analysis::semantic::SEffectArgValue::Value(value) => {
                                    let mode = if self.ty_is_copy(self.normalized_local_ty(value.value))
                                    {
                                        ReadMode::Copy
                                    } else {
                                        ReadMode::Read
                                    };
                                    NEffectArgValue::Value(self.read_operand(
                                        block,
                                        origin,
                                        *value,
                                        Some(mode),
                                    )?)
                                }
                            },
                            pass_mode: arg.pass_mode,
                            required_mut: arg.required_mut,
                            provider_target_ty: arg
                                .provider_target_ty
                                .map(|ty| self.instance.normalized_ty(self.db, ty)),
                            provider: arg.provider,
                        })
                    })
                    .collect::<Result<Vec<_>, NormalizeError<'db>>>()?
                    .into_boxed_slice(),
            },
        };
        Ok(normalized)
    }

    fn contains_capability(&self, ty: TyId<'db>) -> bool {
        contains_capability(
            self.db,
            self.instance
                .key(self.db)
                .impl_env(self.db)
                .normalization_scope(self.db),
            self.assumptions,
            ty,
        )
    }

    fn normalize_constant(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source: SLocalId,
        ty: TyId<'db>,
        constant: SConst<'db>,
    ) -> Result<NExpr<'db>, NormalizeError<'db>> {
        if let Some((kind, target)) = ty.as_capability(self.db) {
            if kind == CapabilityKind::Mut {
                return Err(NormalizeError::InvalidProjection);
            }
            let expr = self.normalize_constant(block, origin, source, target, constant)?;
            let value = self.emit_define(block, None, origin, target, source, expr)?;
            let place = self.materialize_value(value, origin);
            return Ok(match kind {
                CapabilityKind::View => NExpr::MakeView {
                    place,
                    access: ViewAccess::Read,
                },
                CapabilityKind::Ref => NExpr::Borrow {
                    place,
                    kind: BorrowKind::Ref,
                    provider: None,
                },
                CapabilityKind::Mut => unreachable!(),
            });
        }
        if !self.contains_capability(ty) {
            return Ok(NExpr::Const(constant));
        }
        let value = match constant {
            SConst::Value(value) => value.value(),
            SConst::Description(value) | SConst::Evidence(value) | SConst::Invalid(value) => value,
            SConst::Ref(reference) => match eval_const_ref(self.db, reference) {
                EvalOutcome::Ready(value) => value,
                EvalOutcome::Blocked(_) | EvalOutcome::Failed(_) => {
                    return Err(NormalizeError::UnresolvedCapabilityConst(ty));
                }
            },
        };
        let constant = SConst::from_trusted_source(self.db, value);
        if is_literal_allocation(self.db, ty, &constant) {
            return Ok(NExpr::Const(constant));
        }
        let (variant, fields) = match value.value(self.db) {
            SemConstValue::Struct { fields, .. } => (None, fields),
            SemConstValue::Tuple { elems, .. } | SemConstValue::Array { elems, .. } => {
                (None, elems)
            }
            SemConstValue::Enum {
                variant, fields, ..
            } => (Some(*variant), fields),
            SemConstValue::Scalar { .. } | SemConstValue::Unit | SemConstValue::Description(..) => {
                return Err(NormalizeError::UnresolvedCapabilityConst(ty));
            }
        };
        let fields = fields
            .iter()
            .map(|field| {
                let ty = self
                    .instance
                    .normalized_ty(self.db, sem_const_ty(self.db, *field));
                let expr = self.normalize_constant(
                    block,
                    origin,
                    source,
                    ty,
                    SConst::from_trusted_source(self.db, *field),
                )?;
                let value = self.emit_define(block, None, origin, ty, source, expr)?;
                Ok(self.operand(value, origin, ReadMode::Move))
            })
            .collect::<Result<Vec<_>, NormalizeError<'db>>>()?
            .into_boxed_slice();
        Ok(match variant {
            Some(variant) => NExpr::EnumMake {
                enum_ty: ty,
                variant,
                fields,
            },
            None => NExpr::AggregateMake { ty, fields },
        })
    }

    fn load_or_borrow_place(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source_local: SLocalId,
        result_ty: TyId<'db>,
        place: NPlace<'db>,
        load_mode: Option<ReadMode>,
    ) -> Result<NExpr<'db>, NormalizeError<'db>> {
        if result_ty == place.ty {
            return Ok(NExpr::Load {
                mode: load_mode.unwrap_or_else(|| self.read_mode_for_place(result_ty)),
                place,
            });
        }
        if let Some((kind, target)) = result_ty.as_capability(self.db)
            && (target == place.ty
                || structural_repack_mapping(self.db, self.instance, place.ty, target).is_some())
        {
            if kind == CapabilityKind::View {
                return Ok(NExpr::MakeView {
                    place,
                    access: ViewAccess::Read,
                });
            }
            return Ok(NExpr::Borrow {
                place,
                kind: match kind {
                    CapabilityKind::Mut => crate::analysis::ty::ty_def::BorrowKind::Mut,
                    CapabilityKind::Ref => crate::analysis::ty::ty_def::BorrowKind::Ref,
                    CapabilityKind::View => unreachable!("views are explicit"),
                },
                provider: None,
            });
        }
        if structural_repack_mapping(self.db, self.instance, place.ty, result_ty).is_some() {
            Ok(NExpr::Load {
                mode: load_mode.unwrap_or_else(|| self.read_mode_for_place(place.ty)),
                place,
            })
        } else if place.ty.as_capability(self.db).is_some()
            && result_ty.as_capability(self.db).is_none()
        {
            // Contextual reads can copy the referent of a stored capability.
            // Load its carrier first, then use the ordinary operand coercion so
            // place reads enforce the same Copy and conversion requirements.
            let carrier = self.emit_define(
                block,
                None,
                origin,
                place.ty,
                source_local,
                NExpr::Load {
                    place,
                    mode: ReadMode::Copy,
                },
            )?;
            let value = self.operand(carrier, origin, ReadMode::Copy);
            let src = self.coerce_operand_as(block, origin, source_local, value, result_ty)?;
            Ok(NExpr::Forward { src })
        } else {
            Err(NormalizeError::InvalidProjection)
        }
    }

    fn normalize_projection_expr(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        base: SOperand,
        projection: NDataProjection,
        result_ty: TyId<'db>,
    ) -> Result<NExpr<'db>, NormalizeError<'db>> {
        if self.local_has_place(base.value) {
            let mut place = self.place_for_local(block, base.sem_origin(origin), base.value)?;
            if place.ty.as_capability(self.db).is_some() {
                place =
                    self.dereference_place(block, base.sem_origin(origin), base.value, place)?;
            }
            place.path = place.path.appended(projection);
            place.ty = project_path_ty(
                self.db,
                self.instance,
                &self.values,
                self.place_base_ty(place.base)?,
                &place.path,
            )
            .map_err(|_| NormalizeError::InvalidProjection)?;
            self.load_or_borrow_place(block, origin, base.value, result_ty, place, None)
        } else {
            let value = self.read_operand(block, origin, base, None)?;
            self.normalize_value_projection(
                block,
                origin,
                base.value,
                value,
                NDataPath::new(vec![projection].into_boxed_slice()),
                result_ty,
            )
        }
    }

    fn normalize_value_projection(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source_local: SLocalId,
        mut value: NOperand,
        path: NDataPath,
        result_ty: TyId<'db>,
    ) -> Result<NExpr<'db>, NormalizeError<'db>> {
        if path.is_empty() {
            return Ok(NExpr::Forward { src: value });
        }
        let projections = path.iter().copied().collect::<Vec<_>>();
        let mut projected_ty = self.values[value.value.index()].ty;
        for (index, projection) in projections.iter().copied().enumerate() {
            projected_ty = project_path_ty(
                self.db,
                self.instance,
                &self.values,
                projected_ty,
                &NDataPath::new(vec![projection].into_boxed_slice()),
            )
            .map_err(|_| NormalizeError::InvalidProjection)?;
            let Some((_, target)) = projected_ty.as_capability(self.db) else {
                continue;
            };
            if index + 1 == projections.len() {
                break;
            }
            if self.ty_is_copy(projected_ty) {
                value.mode = ReadMode::Copy;
            }
            let carrier = self.emit_define(
                block,
                None,
                origin,
                projected_ty,
                source_local,
                NExpr::ProjectValue {
                    value,
                    path: NStructuralPath(NDataPath::new(
                        projections[..=index].to_vec().into_boxed_slice(),
                    )),
                },
            )?;
            let mut place = NPlace {
                base: NPlaceBase::CapabilityTarget { carrier },
                path: NDataPath::empty(),
                ty: target,
                origin,
            };
            for (remaining_index, projection) in
                projections[index + 1..].iter().copied().enumerate()
            {
                place.path.push(projection);
                place.ty = project_path_ty(
                    self.db,
                    self.instance,
                    &self.values,
                    self.place_base_ty(place.base)?,
                    &place.path,
                )
                .map_err(|_| NormalizeError::InvalidProjection)?;
                if remaining_index + index + 2 < projections.len()
                    && place.ty.as_capability(self.db).is_some()
                {
                    place = self.dereference_place(block, origin, source_local, place)?;
                }
            }
            return self.load_or_borrow_place(block, origin, source_local, result_ty, place, None);
        }
        if self.ty_is_copy(projected_ty) {
            value.mode = ReadMode::Copy;
        }
        Ok(NExpr::ProjectValue {
            value,
            path: NStructuralPath(path),
        })
    }

    fn normalize_place(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        raw: &SPlace<'db>,
    ) -> Result<NPlace<'db>, NormalizeError<'db>> {
        let mut projections = raw.path.iter().peekable();
        let mut place = if matches!(projections.peek(), Some(Projection::Deref)) {
            projections.next();
            let value = self.read_scalar_operand(
                block,
                origin,
                SOperand::inherited(raw.local),
                Some(ReadMode::Copy),
            )?;
            let (ty, _) = self.values[value.value.index()]
                .ty
                .as_ptr(self.db)
                .ok_or(NormalizeError::InvalidProjection)?;
            NPlace {
                base: NPlaceBase::Deref {
                    pointer: value.value,
                },
                path: NDataPath::empty(),
                ty,
                origin,
            }
        } else {
            self.place_for_local(block, origin, raw.local)?
        };
        for projection in projections {
            if matches!(projection, Projection::Deref) {
                place = self.dereference_place(block, origin, raw.local, place)?;
                continue;
            }
            // A raw dereference can expose a stored native carrier. Follow it
            // before applying data projections, just as for carriers in fields.
            if place.ty.as_capability(self.db).is_some() {
                place = self.dereference_place(block, origin, raw.local, place)?;
            }
            let suffix = self.normalize_path(
                block,
                origin,
                &crate::analysis::semantic::SemanticProjectionPath::from_projection(
                    projection.clone(),
                ),
            )?;
            for projection in suffix.iter().copied() {
                place.path.push(projection);
            }
            let base_ty = self.place_base_ty(place.base)?;
            place.ty = project_path_ty(self.db, self.instance, &self.values, base_ty, &place.path)
                .map_err(|_| NormalizeError::InvalidProjection)?;
        }
        Ok(place)
    }

    /// A typed payload write or reborrow may select the target of the terminal
    /// capability. Ordinary place traversal preserves its structural slot, so
    /// materialize that final target transition according to the operation type.
    fn dereference_place_to(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source_local: SLocalId,
        place: NPlace<'db>,
        target_ty: TyId<'db>,
    ) -> Result<NPlace<'db>, NormalizeError<'db>> {
        if place.ty != target_ty
            && place
                .ty
                .as_capability(self.db)
                .is_some_and(|(_, target)| target == target_ty)
        {
            self.dereference_place(block, origin, source_local, place)
        } else {
            Ok(place)
        }
    }

    fn dereference_place(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source_local: SLocalId,
        place: NPlace<'db>,
    ) -> Result<NPlace<'db>, NormalizeError<'db>> {
        let pointer_target = place.ty.as_ptr(self.db).map(|(target, _)| target);
        let target = pointer_target
            .or_else(|| place.ty.as_capability(self.db).map(|(_, target)| target))
            .ok_or(NormalizeError::InvalidProjection)?;
        let value = self.emit_define(
            block,
            None,
            origin,
            place.ty,
            source_local,
            NExpr::Load {
                place,
                mode: ReadMode::Copy,
            },
        )?;
        Ok(NPlace {
            base: if pointer_target.is_some() {
                NPlaceBase::Deref { pointer: value }
            } else {
                NPlaceBase::CapabilityTarget { carrier: value }
            },
            path: NDataPath::empty(),
            ty: target,
            origin,
        })
    }

    fn materialize_value(&mut self, value: NValueId, origin: SemOrigin<'db>) -> NPlace<'db> {
        let root = NRootId::new(self.roots.len());
        self.roots.push(NRoot {
            kind: NRootKind::Temporary { value },
            ty: self.values[value.index()].ty,
            address_space: AddressSpace::Memory,
            mutability: self.values[value.index()].mutability,
            origin,
        });
        self.root_sources.push(NRootRepresentation {
            root,
            source_local: None,
        });
        self.root_place(root, origin)
    }

    fn place_for_local(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        local: SLocalId,
    ) -> Result<NPlace<'db>, NormalizeError<'db>> {
        if let Some(root) = self.root_for_local[local.index()] {
            return Ok(self.root_place(root, origin));
        }
        if let Some(root) = self.provider_target_root_for_local[local.index()] {
            return Ok(self.root_place(root, origin));
        }
        let local_data = &self.raw.locals[local.index()];
        match &local_data.role {
            SemanticLocalRole::PlaceBoundValue {
                provenance: PlaceProvenance::Derived(place),
                ..
            } => self.normalize_place(block, origin, place),
            _ if self
                .normalized_local_ty(local)
                .as_capability(self.db)
                .is_some() =>
            {
                let carrier = self.current_value(local)?;
                let ty = self.values[carrier.index()]
                    .ty
                    .as_capability(self.db)
                    .map(|(_, target)| target)
                    .ok_or(NormalizeError::MissingRoot(local))?;
                Ok(NPlace {
                    base: NPlaceBase::CapabilityTarget { carrier },
                    path: NDataPath::empty(),
                    ty,
                    origin,
                })
            }
            _ => Err(NormalizeError::MissingRoot(local)),
        }
    }

    fn normalize_path(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        path: &crate::analysis::semantic::SemanticProjectionPath<'db>,
    ) -> Result<NDataPath, NormalizeError<'db>> {
        let mut projections = Vec::with_capacity(path.len());
        for projection in path.iter() {
            projections.push(match projection {
                Projection::Field(field) => {
                    NDataProjection::Field(crate::analysis::semantic::FieldIndex(
                        u16::try_from(*field).map_err(|_| NormalizeError::InvalidProjection)?,
                    ))
                }
                Projection::VariantField {
                    variant, field_idx, ..
                } => NDataProjection::VariantField {
                    variant: *variant,
                    field: crate::analysis::semantic::FieldIndex(
                        u16::try_from(*field_idx).map_err(|_| NormalizeError::InvalidProjection)?,
                    ),
                },
                Projection::Index(IndexSource::Constant(index)) => {
                    NDataProjection::Index(NIndex::Const(*index))
                }
                Projection::Index(IndexSource::Dynamic(local)) => {
                    let index = self.read_scalar_operand(
                        block,
                        origin,
                        SOperand::inherited(*local),
                        Some(ReadMode::Copy),
                    )?;
                    self.projection_values[local.index()] = Some(index.value);
                    NDataProjection::Index(NIndex::Value(index.value))
                }
                Projection::Entry(local) => {
                    let key = self.read_scalar_operand(
                        block,
                        origin,
                        SOperand::inherited(*local),
                        Some(ReadMode::Copy),
                    )?;
                    self.projection_values[local.index()] = Some(key.value);
                    NDataProjection::Entry(key.value)
                }
                Projection::Index(IndexSource::Any)
                | Projection::Deref
                | Projection::Discriminant => {
                    return Err(NormalizeError::UnsupportedPlaceProjection);
                }
            });
        }
        Ok(NDataPath::new(projections.into_boxed_slice()))
    }

    fn read_operand(
        &mut self,
        block: SBlockId,
        fallback: SemOrigin<'db>,
        operand: SOperand,
        forced_mode: Option<ReadMode>,
    ) -> Result<NOperand, NormalizeError<'db>> {
        self.read_local(
            block,
            operand.sem_origin(fallback),
            operand.value,
            forced_mode,
        )
    }

    fn read_scalar_operand(
        &mut self,
        block: SBlockId,
        fallback: SemOrigin<'db>,
        operand: SOperand,
        forced_mode: Option<ReadMode>,
    ) -> Result<NOperand, NormalizeError<'db>> {
        let origin = operand.sem_origin(fallback);
        if !self.local_has_place(operand.value) {
            return self.read_local(block, origin, operand.value, forced_mode);
        }
        let place = self.place_for_local(block, origin, operand.value)?;
        let mode = forced_mode.unwrap_or_else(|| self.read_mode_for_place(place.ty));
        let value = self.emit_define(
            block,
            None,
            origin,
            place.ty,
            operand.value,
            NExpr::Load { place, mode },
        )?;
        Ok(self.operand(value, origin, mode))
    }

    fn read_operand_as(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        operand: SOperand,
        target_ty: TyId<'db>,
    ) -> Result<NOperand, NormalizeError<'db>> {
        let value = self.read_operand(block, origin, operand, None)?;
        self.coerce_operand_as(block, origin, operand.value, value, target_ty)
    }

    fn coerce_operand_as(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source_local: SLocalId,
        mut value: NOperand,
        target_ty: TyId<'db>,
    ) -> Result<NOperand, NormalizeError<'db>> {
        if let Some((kind, inner)) = self.values[value.value.index()].ty.as_capability(self.db)
            && target_ty.as_capability(self.db).is_none()
            && (kind == CapabilityKind::View || self.ty_is_copy(inner))
            && structural_repack_mapping(self.db, self.instance, inner, target_ty).is_some()
        {
            let place = NPlace {
                base: NPlaceBase::CapabilityTarget {
                    carrier: value.value,
                },
                path: NDataPath::empty(),
                ty: inner,
                origin,
            };
            let mode = self.read_mode_for_place(inner);
            let result = self.emit_define(
                block,
                None,
                origin,
                inner,
                source_local,
                NExpr::Load { place, mode },
            )?;
            value = self.operand(result, origin, mode);
        }
        self.repack_operand_as(block, origin, source_local, value, target_ty)
    }

    fn repack_operand_as(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source_local: SLocalId,
        value: NOperand,
        target_ty: TyId<'db>,
    ) -> Result<NOperand, NormalizeError<'db>> {
        let source_ty = self.values[value.value.index()].ty;
        if source_ty == target_ty {
            return Ok(value);
        }
        let mapping = structural_repack_mapping(self.db, self.instance, source_ty, target_ty)
            .ok_or(NormalizeError::UnsupportedCapabilityCast {
                from: source_ty,
                to: target_ty,
            })?;
        let mode = value.mode;
        let value = self.emit_define(
            block,
            None,
            origin,
            target_ty,
            source_local,
            NExpr::StructuralRepack { value, mapping },
        )?;
        Ok(self.operand(value, origin, mode))
    }

    fn read_local(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        local: SLocalId,
        forced_mode: Option<ReadMode>,
    ) -> Result<NOperand, NormalizeError<'db>> {
        if self.root_for_local[local.index()].is_some() {
            let place = self.place_for_local(block, origin, local)?;
            let mode = forced_mode
                .unwrap_or_else(|| self.read_mode_for_place(self.raw.locals[local.index()].ty));
            let value = self.emit_define(
                block,
                None,
                origin,
                place.ty,
                local,
                NExpr::Load { place, mode },
            )?;
            return Ok(self.operand(value, origin, mode));
        }
        let value = self.current_value(local)?;
        let mode = forced_mode.unwrap_or_else(|| self.read_mode_for_value(local));
        Ok(self.operand(value, origin, mode))
    }

    fn normalize_terminator(
        &mut self,
        block: SBlockId,
    ) -> Result<NTerminator<'db>, NormalizeError<'db>> {
        let raw = self.raw.blocks[block.index()].terminator.clone();
        let kind = match &raw.kind {
            STerminatorKind::Goto(target) => {
                NTerminatorKind::Goto(self.successor(block, raw.origin, *target)?)
            }
            STerminatorKind::Branch {
                cond,
                then_bb,
                else_bb,
            } => NTerminatorKind::Branch {
                cond: self.read_scalar_operand(block, raw.origin, *cond, Some(ReadMode::Copy))?,
                then_target: self.successor(block, raw.origin, *then_bb)?,
                else_target: self.successor(block, raw.origin, *else_bb)?,
            },
            STerminatorKind::MatchEnum {
                value,
                enum_ty,
                cases,
                default,
            } => NTerminatorKind::MatchEnum {
                value: self.read_operand(block, raw.origin, *value, Some(ReadMode::Copy))?,
                enum_ty: self.instance.normalized_ty(self.db, *enum_ty),
                cases: cases
                    .iter()
                    .map(|(variant, target)| {
                        Ok((*variant, self.successor(block, raw.origin, *target)?))
                    })
                    .collect::<Result<Vec<_>, NormalizeError<'db>>>()?
                    .into_boxed_slice(),
                default: default
                    .map(|target| self.successor(block, raw.origin, target))
                    .transpose()?,
            },
            STerminatorKind::Assert { message } => NTerminatorKind::Assert { message: *message },
            STerminatorKind::Return(value) => NTerminatorKind::Return(
                value
                    .map(|value| self.read_result(block, raw.origin, value))
                    .transpose()?,
            ),
            STerminatorKind::Yield { value, resume } => NTerminatorKind::Yield {
                value: self.read_result(block, raw.origin, *value)?,
                resume: self.successor(block, raw.origin, *resume)?,
            },
        };
        Ok(NTerminator {
            origin: raw.origin,
            kind,
        })
    }

    /// Reads a returned or yielded value as the instance's result.
    fn read_result(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        value: SOperand,
    ) -> Result<NOperand, NormalizeError<'db>> {
        let target = self.instance.normalized_result_ty(self.db);
        if target.has_invalid(self.db) {
            self.read_operand(block, origin, value, None)
        } else {
            self.read_operand_as(block, origin, value, target)
        }
    }

    fn successor(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        target: SBlockId,
    ) -> Result<NSuccessor, NormalizeError<'db>> {
        let locals = self.phi_locals[target.index()].clone();
        let params = self.phi_values[target.index()].clone();
        let args = locals
            .into_iter()
            .zip(params)
            .map(|(local, param)| {
                let value = self.current_value(local)?;
                let value = self.operand(value, origin, ReadMode::Copy);
                self.repack_operand_as(block, origin, local, value, self.values[param.index()].ty)
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(NSuccessor {
            block: NBlockId::new(target.index()),
            args: args.into_boxed_slice(),
        })
    }

    fn emit_define(
        &mut self,
        block: SBlockId,
        source: Option<SStmtId>,
        origin: SemOrigin<'db>,
        ty: TyId<'db>,
        source_local: SLocalId,
        expr: NExpr<'db>,
    ) -> Result<NValueId, NormalizeError<'db>> {
        // A freshly decomposed constant has its own structural representation.
        // Raw-local backing paths describe the enclosing value, not its new fields.
        let constructed = source.is_none()
            && matches!(
                &expr,
                NExpr::Const(_)
                    | NExpr::AggregateMake { .. }
                    | NExpr::EnumMake { .. }
                    | NExpr::ArrayRepeat { .. }
            );
        let forwarded = match &expr {
            NExpr::Forward { src } => Some(src.value),
            _ => None,
        };
        let projected = if source.is_none() {
            match &expr {
                NExpr::Load { place, .. } => Some((place.base, place.path.clone())),
                NExpr::ProjectValue { value, path } => Some((
                    NPlaceBase::CapabilityTarget {
                        carrier: value.value,
                    },
                    path.0.clone(),
                )),
                _ => None,
            }
        } else {
            None
        };
        if forwarded.is_none() && !constructed {
            self.prepare_layout_backing_indices(block, origin, source_local)?;
        }
        let statement = self.blocks[block.index()].statements.len() as u32;
        let source_binding = forwarded
            .and_then(|source| self.values.get(source.index())?.source)
            .or(self.raw.locals[source_local.index()].source);
        let value = self.push_value(
            ty,
            origin,
            NValueDefinition::Statement {
                block: NBlockId::new(block.index()),
                statement,
            },
            source_binding,
            source_local,
        );
        let id = NStatementId::new(self.statement_count);
        self.statement_count += 1;
        self.blocks[block.index()].statements.push(NStatement {
            id,
            source,
            origin,
            kind: NStatementKind::Define {
                result: value,
                expr,
            },
        });
        if let Some(source) = forwarded {
            let backings = self
                .use_backings
                .iter()
                .filter(|backing| backing.value == source)
                .cloned()
                .map(|mut backing| {
                    backing.value = value;
                    backing.origin = origin;
                    backing
                })
                .collect::<Vec<_>>();
            self.use_backings.extend(backings);
        } else if let Some((base, path)) = projected {
            self.record_projected_layout_backings(value, base, &path, block, origin)?;
        } else if !constructed {
            self.record_layout_backings(value, source_local, block, origin)?;
        }
        Ok(value)
    }

    fn emit_store(
        &mut self,
        block: SBlockId,
        source: Option<SStmtId>,
        origin: SemOrigin<'db>,
        destination: NPlace<'db>,
        value: NOperand,
    ) {
        let id = NStatementId::new(self.statement_count);
        self.statement_count += 1;
        self.blocks[block.index()].statements.push(NStatement {
            id,
            source,
            origin,
            kind: NStatementKind::Store { destination, value },
        });
    }

    fn record_layout_backings(
        &mut self,
        value: NValueId,
        source_local: SLocalId,
        block: SBlockId,
        origin: SemOrigin<'db>,
    ) -> Result<(), NormalizeError<'db>> {
        let backings = self.normalize_layout_backings(value, source_local, block, origin)?;
        self.use_backings.extend(backings);
        Ok(())
    }

    fn normalize_layout_backings(
        &mut self,
        value: NValueId,
        source_local: SLocalId,
        block: SBlockId,
        origin: SemOrigin<'db>,
    ) -> Result<Vec<NLayoutUseBacking<'db>>, NormalizeError<'db>> {
        let backings = self.raw.locals[source_local.index()]
            .layout_backing_sources
            .clone();
        let mut normalized = Vec::new();
        for backing in backings {
            let source = match backing.source {
                LayoutBackingPlace::Local(place) => {
                    let path = self.normalize_layout_path(block, origin, &place.path)?;
                    if let Some(root) = self.root_for_local[place.local.index()] {
                        NLayoutBackingSource::Root { root, path }
                    } else {
                        let source = if place.local == source_local {
                            value
                        } else {
                            self.current_value(place.local)?
                        };
                        NLayoutBackingSource::Value {
                            value: source,
                            path,
                        }
                    }
                }
                LayoutBackingPlace::RootProvider {
                    provider,
                    value_ty,
                    path,
                } => NLayoutBackingSource::Root {
                    root: self.provider_root(provider, value_ty)?,
                    path: self.normalize_layout_path(block, origin, &path)?,
                },
            };
            normalized.push(NLayoutUseBacking {
                value,
                target: backing.target.into_boxed_slice(),
                source,
                origin,
            });
        }
        Ok(normalized)
    }

    fn record_projected_layout_backings(
        &mut self,
        value: NValueId,
        base: NPlaceBase,
        path: &NDataPath,
        block: SBlockId,
        origin: SemOrigin<'db>,
    ) -> Result<(), NormalizeError<'db>> {
        let backings = match base {
            NPlaceBase::CapabilityTarget { carrier } => self
                .use_backings
                .iter()
                .filter(|backing| backing.value == carrier)
                .cloned()
                .collect(),
            // A pointee lies outside the pointer value's layout.
            NPlaceBase::Deref { .. } => Vec::new(),
            NPlaceBase::Root(root) => match self.root_sources[root.index()].source_local {
                Some(local) => self.normalize_layout_backings(value, local, block, origin)?,
                None => Vec::new(),
            },
        };
        // An entry lies at a slot computed at runtime, outside its base's
        // layout, so no backing reaches through it.
        let Some(target) = path
            .iter()
            .map(|projection| match projection {
                NDataProjection::Field(field) => Some(LayoutBackingProjection::Field(*field)),
                NDataProjection::VariantField { variant, field } => {
                    Some(LayoutBackingProjection::VariantField {
                        variant: *variant,
                        field: *field,
                    })
                }
                NDataProjection::Index(index) => {
                    Some(LayoutBackingProjection::Index(match index {
                        NIndex::Const(index) => Some(*index),
                        NIndex::Value(_) => None,
                    }))
                }
                NDataProjection::Entry(_) => None,
            })
            .collect::<Option<Vec<_>>>()
        else {
            return Ok(());
        };
        for mut backing in backings {
            if layout_backing_source_path_is_prefix(&backing.target, &target) {
                let suffix = NDataPath::new(
                    path.iter()
                        .skip(backing.target.len())
                        .copied()
                        .collect::<Vec<_>>(),
                );
                match &mut backing.source {
                    NLayoutBackingSource::Value { path, .. }
                    | NLayoutBackingSource::Root { path, .. } => *path = path.concat_data(&suffix),
                }
                backing.target = Box::new([]);
            } else if layout_backing_source_path_is_prefix(&target, &backing.target) {
                backing.target = backing.target[target.len()..].into();
            } else {
                continue;
            }
            backing.value = value;
            backing.origin = origin;
            if !self.use_backings.contains(&backing) {
                self.use_backings.push(backing);
            }
        }
        Ok(())
    }

    fn prepare_layout_backing_indices(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        source_local: SLocalId,
    ) -> Result<(), NormalizeError<'db>> {
        let backings = self.raw.locals[source_local.index()]
            .layout_backing_sources
            .clone();
        for backing in backings {
            let path = match backing.source {
                LayoutBackingPlace::Local(place) => place.path,
                LayoutBackingPlace::RootProvider { path, .. } => path,
            };
            self.normalize_layout_path(block, origin, &path)?;
        }
        Ok(())
    }

    fn normalize_layout_path(
        &mut self,
        block: SBlockId,
        origin: SemOrigin<'db>,
        path: &crate::analysis::semantic::SemanticProjectionPath<'db>,
    ) -> Result<NLayoutSourcePath, NormalizeError<'db>> {
        let mut normalized = Vec::with_capacity(path.len());
        for projection in path.iter() {
            if matches!(projection, Projection::Deref) {
                normalized.push(NLayoutProjection::PointerTarget);
                continue;
            }
            normalized.push(NLayoutProjection::Data(match projection {
                Projection::Field(field) => NDataProjection::Field(
                    u16::try_from(*field)
                        .map(crate::analysis::semantic::FieldIndex)
                        .map_err(|_| NormalizeError::InvalidProjection)?,
                ),
                Projection::VariantField {
                    variant, field_idx, ..
                } => NDataProjection::VariantField {
                    variant: *variant,
                    field: crate::analysis::semantic::FieldIndex(
                        u16::try_from(*field_idx).map_err(|_| NormalizeError::InvalidProjection)?,
                    ),
                },
                Projection::Index(IndexSource::Constant(index)) => {
                    NDataProjection::Index(NIndex::Const(*index))
                }
                Projection::Index(IndexSource::Dynamic(local)) => {
                    let value = if let Some(value) = self.projection_values[local.index()] {
                        value
                    } else {
                        let value = self
                            .read_scalar_operand(
                                block,
                                origin,
                                SOperand::inherited(*local),
                                Some(ReadMode::Copy),
                            )?
                            .value;
                        self.projection_values[local.index()] = Some(value);
                        value
                    };
                    NDataProjection::Index(NIndex::Value(value))
                }
                // A layout source never reaches through an entry, which lies at
                // a slot computed at runtime.
                Projection::Index(IndexSource::Any)
                | Projection::Entry(_)
                | Projection::Deref
                | Projection::Discriminant => {
                    return Err(NormalizeError::UnsupportedPlaceProjection);
                }
            }));
        }
        Ok(NLayoutSourcePath(normalized.into_boxed_slice()))
    }

    fn push_value(
        &mut self,
        ty: TyId<'db>,
        origin: SemOrigin<'db>,
        definition: NValueDefinition,
        source: Option<LocalBinding<'db>>,
        source_local: SLocalId,
    ) -> NValueId {
        let ty = self.instance.normalized_ty(self.db, ty);
        let value = NValueId::new(self.values.len());
        self.values.push(NValue {
            ty,
            mutability: self.raw.locals[source_local.index()].mutability,
            origin,
            definition,
            source,
        });
        self.value_sources.push(NValueRepresentation {
            value,
            source_local,
        });
        value
    }

    fn current_value(&self, local: SLocalId) -> Result<NValueId, NormalizeError<'db>> {
        self.current_values[local.index()]
            .last()
            .copied()
            .ok_or(NormalizeError::MissingValue(local))
    }

    fn normalized_local_ty(&self, local: SLocalId) -> TyId<'db> {
        normalized_source_local_value_ty(self.db, self.instance, &self.raw.locals[local.index()])
    }

    /// Address demand follows semantic derived places before allocating storage.
    /// A projected snapshot must not receive a new root for its old referent.
    fn mark_address_root(&self, mut local: SLocalId, needs_slot: &mut [bool]) {
        let mut visited = FxHashSet::default();
        while visited.insert(local) {
            if let SemanticLocalRole::PlaceBoundValue {
                provenance: PlaceProvenance::Derived(place),
                ..
            } = &self.raw.locals[local.index()].role
            {
                local = place.local;
            } else {
                if self
                    .normalized_local_ty(local)
                    .as_capability(self.db)
                    .is_none()
                {
                    needs_slot[local.index()] = true;
                }
                return;
            }
        }
    }

    fn call_view_target(
        &self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        index: usize,
        local: SLocalId,
    ) -> Option<TyId<'db>> {
        if self.normalized_local_ty(local).as_view(self.db).is_some() {
            return None;
        }
        self.call_param_ty(callee, index)?.as_view(self.db)
    }

    fn call_param_ty(
        &self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        index: usize,
    ) -> Option<TyId<'db>> {
        let instance = SemanticInstance::new(self.db, callee.key);
        let binding = *instance
            .key(self.db)
            .typed_body(self.db)
            .input_bindings()
            .get(index)?;
        Some(copied_scalar_ty(
            self.db,
            instance.normalized_binding_ty(self.db, binding),
        ))
    }

    fn call_arg_mode(
        &self,
        callee: crate::analysis::semantic::SemanticCalleeRef<'db>,
        index: usize,
        local: SLocalId,
    ) -> Option<ReadMode> {
        let BodyOwner::Func(..) = callee.key.owner(self.db) else {
            return None;
        };
        let typed_body = callee.key.typed_body(self.db);
        let binding = *typed_body.input_bindings().get(index)?;
        matches!(
            typed_body.binding_access(binding),
            Some(BindingAccess::View)
        )
        .then(|| {
            if self.ty_is_copy(self.normalized_local_ty(local)) {
                ReadMode::Copy
            } else {
                ReadMode::Read
            }
        })
    }

    fn operand(&self, value: NValueId, origin: SemOrigin<'db>, mode: ReadMode) -> NOperand {
        NOperand {
            value,
            origin: match origin {
                crate::analysis::semantic::SemOrigin::Expr(expr) => Some(expr),
                crate::analysis::semantic::SemOrigin::Stmt(_)
                | crate::analysis::semantic::SemOrigin::Pat(_)
                | crate::analysis::semantic::SemOrigin::Body(_)
                | crate::analysis::semantic::SemOrigin::Synthetic => None,
            },
            mode,
        }
    }

    fn root_place(&self, root: NRootId, origin: SemOrigin<'db>) -> NPlace<'db> {
        NPlace {
            base: NPlaceBase::Root(root),
            path: NDataPath::empty(),
            ty: self.roots[root.index()].ty,
            origin,
        }
    }

    fn place_base_ty(&self, base: NPlaceBase) -> Result<TyId<'db>, NormalizeError<'db>> {
        match base {
            NPlaceBase::Root(root) => self
                .roots
                .get(root.index())
                .map(|root| root.ty)
                .ok_or(NormalizeError::InvalidProjection),
            NPlaceBase::CapabilityTarget { carrier } => self
                .values
                .get(carrier.index())
                .and_then(|value| value.ty.as_capability(self.db).map(|(_, target)| target))
                .ok_or(NormalizeError::InvalidProjection),
            NPlaceBase::Deref { pointer } => self
                .values
                .get(pointer.index())
                .and_then(|value| value.ty.as_ptr(self.db))
                .map(|(target, _)| target)
                .ok_or(NormalizeError::InvalidProjection),
        }
    }

    fn local_has_place(&self, local: SLocalId) -> bool {
        self.root_for_local[local.index()].is_some()
            || self
                .normalized_local_ty(local)
                .as_capability(self.db)
                .is_some()
            || matches!(
                self.raw.locals[local.index()].role,
                SemanticLocalRole::PlaceBoundValue {
                    provenance: PlaceProvenance::Derived(_),
                    ..
                }
            )
    }

    fn ty_is_copy(&self, ty: TyId<'db>) -> bool {
        if let Some(result) = self.copy_cache.borrow().get(&ty) {
            return *result;
        }
        let result = ty_is_copy(
            self.db,
            self.raw.template_owner.scope(),
            ty,
            self.assumptions,
        );
        self.copy_cache.borrow_mut().insert(ty, result);
        result
    }

    /// A value read with no mode of its own: lowering emits one only where an
    /// expression is evaluated as a value, which takes it, so a non-`Copy`
    /// value moves. A capability carrier names its place and copies. The
    /// reads that leave a value in place pass their mode: inspections, view
    /// arguments and lent effects.
    fn read_mode_for_value(&self, local: SLocalId) -> ReadMode {
        let ty = self.normalized_local_ty(local);
        if ty.as_capability(self.db).is_some() || self.ty_is_copy(ty) {
            ReadMode::Copy
        } else {
            ReadMode::Move
        }
    }

    /// A place read with no mode of its own, as `read_mode_for_value`: a
    /// non-`Copy` value moves out of its place, through any access, which the
    /// access checker rejects where the access does not own it.
    fn read_mode_for_place(&self, ty: TyId<'db>) -> ReadMode {
        if self.ty_is_copy(ty) {
            ReadMode::Copy
        } else {
            ReadMode::Move
        }
    }
}

fn semantic_root_provider<'db>(
    role: &SemanticLocalRole<'db>,
    local_ty: TyId<'db>,
) -> Option<(ProviderBinding<'db>, TyId<'db>)> {
    match role {
        SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::RootProvider(provider),
        } => Some((provider.clone(), local_ty)),
        SemanticLocalRole::PlaceCarrier {
            provider: Some(provider),
            value_ty,
        }
        | SemanticLocalRole::PlaceBoundValue {
            provenance: PlaceProvenance::RootProvider(provider),
            value_ty,
        } => Some((provider.clone(), *value_ty)),
        SemanticLocalRole::Erased
        | SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::Ordinary,
        }
        | SemanticLocalRole::PlaceCarrier { provider: None, .. }
        | SemanticLocalRole::PlaceBoundValue {
            provenance: PlaceProvenance::Derived(_),
            ..
        } => None,
    }
}

fn for_each_address_required_local(expr: &SExpr<'_>, mut f: impl FnMut(SLocalId)) {
    match expr {
        SExpr::Borrow { place, .. } => {
            if !place
                .path
                .iter()
                .any(|projection| matches!(projection, Projection::Deref))
            {
                f(place.local);
            }
        }
        SExpr::Call { effect_args, .. } => {
            for arg in effect_args {
                if let crate::analysis::semantic::SEffectArgValue::Place(place) = &arg.arg {
                    f(place.local);
                }
            }
        }
        SExpr::Forward(_)
        | SExpr::UseValue(_)
        | SExpr::ReadPlace { .. }
        | SExpr::InspectPlace { .. }
        | SExpr::CodeRegionRef { .. }
        | SExpr::Const(_)
        | SExpr::Unary { .. }
        | SExpr::Binary { .. }
        | SExpr::Cast { .. }
        | SExpr::ArrayRepeat { .. }
        | SExpr::AggregateMake { .. }
        | SExpr::EnumMake { .. }
        | SExpr::Field { .. }
        | SExpr::Index { .. }
        | SExpr::GetEnumTag { .. }
        | SExpr::IsEnumVariant { .. }
        | SExpr::ExtractEnumField { .. }
        | SExpr::CodeRegionOffset { .. }
        | SExpr::CodeRegionLen { .. } => {}
    }
}

struct StructuralRepackCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    mapping: Vec<(NDataPath, NDataPath)>,
    visiting: FxHashSet<(TyId<'db>, TyId<'db>)>,
    allow_deferred_leaves: bool,
}

impl<'db> StructuralRepackCollector<'db> {
    fn collect(
        &mut self,
        mut source: TyId<'db>,
        mut target: TyId<'db>,
        source_path: NDataPath,
        target_path: NDataPath,
    ) -> bool {
        let db = self.db;
        let instance = self.instance;
        source = instance.normalized_ty(db, source);
        target = instance.normalized_ty(db, target);
        if !self.visiting.insert((source, target)) {
            return true;
        }
        let deferred_runtime_leaf = |ty: TyId<'db>| {
            matches!(
                ty.data(db),
                TyData::TyVar(_) | TyData::TyParam(_) | TyData::AssocTy(_) | TyData::QualifiedTy(_)
            )
        };
        let deferred_const_leaf = |ty: TyId<'db>| {
            matches!(ty.data(db), TyData::ConstTy(_)) && (ty.has_param(db) || ty.has_var(db))
        };
        let result = if deferred_const_leaf(source)
            || deferred_const_leaf(target)
            || self.allow_deferred_leaves
                && (deferred_runtime_leaf(source) || deferred_runtime_leaf(target))
        {
            true
        } else if source.as_capability(db).is_some() || target.as_capability(db).is_some() {
            let compatible = source
                .as_capability(db)
                .zip(target.as_capability(db))
                .is_some_and(
                    |((source_kind, source_target), (target_kind, target_target))| {
                        let mapping_len = self.mapping.len();
                        let compatible = source_kind == target_kind
                            && self.collect(
                                source_target,
                                target_target,
                                source_path.clone(),
                                target_path.clone(),
                            );
                        self.mapping.truncate(mapping_len);
                        compatible
                    },
                );
            if compatible {
                self.mapping.push((target_path, source_path));
            }
            compatible
        } else if source.is_array(db) && target.is_array(db) {
            let source_elem = source.decompose_ty_app(db).1.first().copied();
            let target_elem = target.decompose_ty_app(db).1.first().copied();
            // Verifier-approved repacks must use the same length identity as
            // capability shapes and their subsequent value transfers.
            let [source_len, target_len] =
                [source, target].map(|ty| ArrayLength::from_ty(db, ty.generic_args(db)[1]));
            if source_len.is_none() || source_len != target_len {
                false
            } else if source_len == Some(ArrayLength::Known(0)) {
                true
            } else {
                source_elem
                    .zip(target_elem)
                    .is_some_and(|(source_elem, target_elem)| {
                        self.collect(
                            instance.normalized_ty(db, source_elem),
                            instance.normalized_ty(db, target_elem),
                            source_path.appended(NDataProjection::Index(NIndex::Const(0))),
                            target_path.appended(NDataProjection::Index(NIndex::Const(0))),
                        )
                    })
            }
        } else if let (Some(source_adt), Some(target_adt)) =
            (source.adt_def(db), target.adt_def(db))
            && matches!(source_adt.adt_ref(db), AdtRef::Enum(_))
            && matches!(target_adt.adt_ref(db), AdtRef::Enum(_))
        {
            let source_variants = source_adt.fields(db);
            let target_variants = target_adt.fields(db);
            source_variants.len() == target_variants.len()
                && source_variants.iter().zip(target_variants).enumerate().all(
                    |(variant, (source_fields, target_fields))| {
                        source_fields.num_types() == target_fields.num_types()
                            && (0..source_fields.num_types()).all(|field| {
                                self.collect(
                                    instance.normalized_enum_variant_field_tys(
                                        db,
                                        source,
                                        VariantIndex(variant as u16),
                                    )[field],
                                    instance.normalized_enum_variant_field_tys(
                                        db,
                                        target,
                                        VariantIndex(variant as u16),
                                    )[field],
                                    source_path.appended(NDataProjection::VariantField {
                                        variant: VariantIndex(variant as u16),
                                        field: FieldIndex(field as u16),
                                    }),
                                    target_path.appended(NDataProjection::VariantField {
                                        variant: VariantIndex(variant as u16),
                                        field: FieldIndex(field as u16),
                                    }),
                                )
                            })
                    },
                )
        } else if source
            .adt_def(db)
            .is_some_and(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))
            || target
                .adt_def(db)
                .is_some_and(|adt| matches!(adt.adt_ref(db), AdtRef::Enum(_)))
        {
            false
        } else if instance.normalized_field_types(db, source).is_empty()
            && instance.normalized_field_types(db, target).is_empty()
        {
            if source == target {
                if !source.is_zero_sized(db) {
                    self.mapping.push((target_path, source_path));
                }
                true
            } else if source.base_ty(db) == target.base_ty(db) {
                let source_args = source.generic_args(db);
                let target_args = target.generic_args(db);
                let mapping_len = self.mapping.len();
                let compatible = !source_args.is_empty()
                    && source_args.len() == target_args.len()
                    && source_args.iter().zip(target_args).all(|(source, target)| {
                        self.collect(*source, *target, source_path.clone(), target_path.clone())
                    });
                self.mapping.truncate(mapping_len);
                if compatible && !source.is_zero_sized(db) {
                    self.mapping.push((target_path, source_path));
                }
                compatible
            } else {
                source.is_zero_sized(db)
                    && target.is_zero_sized(db)
                    && source.adt_def(db) == target.adt_def(db)
            }
        } else {
            let source_fields = instance.normalized_field_types(db, source);
            let target_fields = instance.normalized_field_types(db, target);
            !source_fields.is_empty()
                && source_fields.len() == target_fields.len()
                && source_fields
                    .into_iter()
                    .zip(target_fields)
                    .enumerate()
                    .all(|(index, (source_field, target_field))| {
                        let field = crate::analysis::semantic::FieldIndex(index as u16);
                        self.collect(
                            *source_field,
                            *target_field,
                            source_path.appended(NDataProjection::Field(field)),
                            target_path.appended(NDataProjection::Field(field)),
                        )
                    })
        };
        self.visiting.remove(&(source, target));
        result
    }
}

pub(super) fn structural_repack_mapping<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    source: TyId<'db>,
    target: TyId<'db>,
) -> Option<StructuralRepack> {
    let mut collector = StructuralRepackCollector {
        db,
        instance,
        mapping: Vec::new(),
        visiting: FxHashSet::default(),
        allow_deferred_leaves: false,
    };
    collector
        .collect(source, target, NDataPath::empty(), NDataPath::empty())
        .then(|| StructuralRepack {
            fields: collector.mapping.into_boxed_slice(),
        })
}

pub(super) fn structural_types_are_boundary_compatible<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    source: TyId<'db>,
    target: TyId<'db>,
) -> bool {
    StructuralRepackCollector {
        db,
        instance,
        mapping: Vec::new(),
        visiting: FxHashSet::default(),
        allow_deferred_leaves: true,
    }
    .collect(source, target, NDataPath::empty(), NDataPath::empty())
}

struct RawCfg {
    successors: Vec<Vec<usize>>,
    reachable: Vec<bool>,
    dom_children: Vec<Vec<usize>>,
    dominance_frontier: Vec<FxHashSet<usize>>,
}

impl RawCfg {
    fn new(body: &SemanticBody<'_>) -> Self {
        let successors = body
            .blocks
            .iter()
            .map(|block| {
                block
                    .terminator
                    .kind
                    .successors()
                    .into_iter()
                    .map(|block| block.index())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();
        let mut predecessors = vec![Vec::new(); body.blocks.len()];
        for (block, targets) in successors.iter().enumerate() {
            for target in targets {
                predecessors[*target].push(block);
            }
        }
        let mut reachable = vec![false; body.blocks.len()];
        let mut postorder = Vec::new();
        let mut pending = vec![(0usize, false)];
        while let Some((block, finished)) = pending.pop() {
            if finished {
                postorder.push(block);
                continue;
            }
            if std::mem::replace(&mut reachable[block], true) {
                continue;
            }
            pending.push((block, true));
            pending.extend(successors[block].iter().map(|next| (*next, false)));
        }
        let reachable_set = reachable
            .iter()
            .enumerate()
            .filter_map(|(block, reachable)| reachable.then_some(block))
            .collect::<FxHashSet<_>>();
        let mut dominators = reachable
            .iter()
            .enumerate()
            .map(|(block, reachable)| {
                if !reachable {
                    FxHashSet::from_iter([block])
                } else if block == 0 {
                    FxHashSet::from_iter([0])
                } else {
                    reachable_set.clone()
                }
            })
            .collect::<Vec<_>>();
        loop {
            let mut changed = false;
            for block in 1..body.blocks.len() {
                if !reachable[block] {
                    continue;
                }
                let mut incoming = predecessors[block]
                    .iter()
                    .copied()
                    .filter(|pred| reachable[*pred]);
                let mut next = incoming
                    .next()
                    .map(|pred| dominators[pred].clone())
                    .unwrap_or_default();
                for pred in incoming {
                    next.retain(|dom| dominators[pred].contains(dom));
                }
                next.insert(block);
                changed |= next != dominators[block];
                dominators[block] = next;
            }
            if !changed {
                break;
            }
        }
        let mut idom = vec![None; body.blocks.len()];
        for block in 1..body.blocks.len() {
            if reachable[block] {
                idom[block] = dominators[block]
                    .iter()
                    .copied()
                    .filter(|dom| *dom != block)
                    .max_by_key(|dom| dominators[*dom].len());
            }
        }
        let mut dom_children = vec![Vec::new(); body.blocks.len()];
        // Renaming still follows the dominator tree, but visit siblings in CFG
        // reverse postorder. Block allocation order can put a short-circuit
        // continuation before its RHS, giving later calls lower value IDs than
        // their controlling conditions and interleaving unrelated guard choices.
        for block in postorder.into_iter().rev() {
            if let Some(parent) = idom[block] {
                dom_children[parent].push(block);
            }
        }
        let mut dominance_frontier = vec![FxHashSet::default(); body.blocks.len()];
        for block in 0..body.blocks.len() {
            let reachable_preds = predecessors[block]
                .iter()
                .copied()
                .filter(|pred| reachable[*pred])
                .collect::<Vec<_>>();
            if reachable_preds.len() < 2 {
                continue;
            }
            for pred in reachable_preds {
                let mut runner = Some(pred);
                while runner != idom[block] {
                    let Some(current) = runner else {
                        break;
                    };
                    dominance_frontier[current].insert(block);
                    runner = idom[current];
                }
            }
        }
        Self {
            successors,
            reachable,
            dom_children,
            dominance_frontier,
        }
    }

    fn dominator_roots(&self) -> Vec<SBlockId> {
        let mut roots = vec![SBlockId::new(0)];
        roots.extend(
            self.reachable
                .iter()
                .enumerate()
                .skip(1)
                .filter_map(|(block, reachable)| (!reachable).then_some(SBlockId::new(block))),
        );
        roots
    }

    fn phi_locals(
        &self,
        body: &SemanticBody<'_>,
        root_for_local: &[Option<NRootId>],
    ) -> Vec<Vec<SLocalId>> {
        let (uses, defs) = block_uses_and_defs(body);
        let mut live_in = vec![FxHashSet::default(); body.blocks.len()];
        let mut live_out = vec![FxHashSet::default(); body.blocks.len()];
        loop {
            let mut changed = false;
            for block in (0..body.blocks.len()).rev() {
                let next_out = self.successors[block]
                    .iter()
                    .flat_map(|successor| live_in[*successor].iter().copied())
                    .collect::<FxHashSet<_>>();
                let mut next_in = uses[block].clone();
                next_in.extend(next_out.difference(&defs[block]).copied());
                changed |= next_in != live_in[block] || next_out != live_out[block];
                live_in[block] = next_in;
                live_out[block] = next_out;
            }
            if !changed {
                break;
            }
        }

        let mut def_blocks = vec![FxHashSet::default(); body.locals.len()];
        for local in &body.entry_locals {
            def_blocks[local.index()].insert(0);
        }
        for (block_index, block_defs) in defs.iter().enumerate() {
            for local in block_defs {
                def_blocks[local.index()].insert(block_index);
            }
        }
        let mut phis = vec![FxHashSet::default(); body.blocks.len()];
        for local_index in 0..body.locals.len() {
            if root_for_local[local_index].is_some() {
                continue;
            }
            let local = SLocalId::new(local_index);
            let mut work = def_blocks[local_index].iter().copied().collect::<Vec<_>>();
            let mut seen = def_blocks[local_index].clone();
            while let Some(block) = work.pop() {
                for frontier in &self.dominance_frontier[block] {
                    if live_in[*frontier].contains(&local)
                        && phis[*frontier].insert(local)
                        && seen.insert(*frontier)
                    {
                        work.push(*frontier);
                    }
                }
            }
        }
        phis.into_iter()
            .map(|locals| {
                let mut locals = locals.into_iter().collect::<Vec<_>>();
                locals.sort_by_key(|local| local.index());
                locals
            })
            .collect()
    }
}

/// Whether a value of `ty` holds an enum, whose tag a copy validates.
fn ty_has_enum<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    if ty.as_enum(db).is_some() {
        return true;
    }
    if ty.is_array(db) {
        return ty_has_enum(db, ty.generic_args(db)[0]);
    }
    ty.is_product(db)
        && ty
            .field_types(db)
            .into_iter()
            .any(|field| ty_has_enum(db, field))
}

fn block_uses_and_defs(
    body: &SemanticBody<'_>,
) -> (Vec<FxHashSet<SLocalId>>, Vec<FxHashSet<SLocalId>>) {
    let mut uses = Vec::with_capacity(body.blocks.len());
    let mut defs = Vec::with_capacity(body.blocks.len());
    for block in &body.blocks {
        let mut block_uses = FxHashSet::default();
        let mut block_defs = FxHashSet::default();
        for statement in &block.stmts {
            match &statement.kind {
                SStmtKind::Assign { dst, expr } => {
                    for local in expr.used_locals() {
                        if !block_defs.contains(&local) {
                            block_uses.insert(local);
                        }
                    }
                    block_defs.insert(*dst);
                }
                SStmtKind::Store { dst, src } => {
                    for local in dst
                        .used_locals()
                        .into_iter()
                        .chain(std::iter::once(src.value))
                    {
                        if !block_defs.contains(&local) {
                            block_uses.insert(local);
                        }
                    }
                }
                SStmtKind::Drop { place } => {
                    for local in place.used_locals() {
                        if !block_defs.contains(&local) {
                            block_uses.insert(local);
                        }
                    }
                }
                SStmtKind::End { .. } => {}
            }
        }
        for local in block.terminator.kind.used_locals() {
            if !block_defs.contains(&local) {
                block_uses.insert(local);
            }
        }
        uses.push(block_uses);
        defs.push(block_defs);
    }
    (uses, defs)
}

pub(super) fn normalized_source_local_value_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    local: &SLocal<'db>,
) -> TyId<'db> {
    let ty = if matches!(
        local.role,
        SemanticLocalRole::DirectValue {
            provenance: ValueProvenance::Ordinary,
        }
    ) && let Some((_, target)) = local.ty.as_capability(db)
    {
        target
    } else {
        local.ty
    };
    copied_scalar_ty(db, instance.normalized_ty(db, ty))
}

#[cfg(test)]
mod tests {
    use cranelift_entity::EntityRef;

    use crate::{
        analysis::{
            semantic::{
                BinaryOp, FieldIndex, Mutability, Overflow, SConst, SStmtId, SemanticInstance,
                UnaryOp, VariantIndex, identity_semantic_instance_key,
                normalized::{
                    NDataPath, NDataProjection, NEffectArgValue, NExpr, NIndex, NPlace, NPlaceBase,
                    NRootId, NStatementKind, NTerminatorKind, NValueDefinition, NValueId,
                    NormalizedBodyVerifyError, NormalizedLayoutPlanVerifyError, StructuralRepack,
                    normalize_raw_body, verify_normalized_body, verify_normalized_layout_plan,
                },
                unit_const,
            },
            ty::{
                ty_check::{BodyOwner, EffectPassMode},
                ty_def::TyId,
            },
        },
        hir_def::ItemKind,
        test_db::HirAnalysisTestDb,
    };

    fn normalized_func<'db>(
        db: &'db HirAnalysisTestDb,
        top_mod: crate::hir_def::TopLevelMod<'db>,
        name: &str,
    ) -> super::NormalizedArtifacts<'db> {
        let func = top_mod
            .all_items(db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(db)
                        .to_opt()
                        .is_some_and(|func_name| func_name.data(db) == name) =>
                {
                    Some(*func)
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing function `{name}`"));
        let instance = SemanticInstance::new(
            db,
            identity_semantic_instance_key(db, BodyOwner::Func(func, None)),
        );
        let raw = instance
            .admitted_body(db)
            .expect("test body should be admitted");
        normalize_raw_body(db, instance, raw, instance.assumptions(db))
            .unwrap_or_else(|error| panic!("test body should normalize: {error:?}\n{raw:#?}"))
    }

    #[test]
    fn function_item_values_normalize_without_constant_folding() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "function_item_values.fe".into(),
            r#"
fn answer(value: u256) -> u256 { value }

fn stored(_ x: u256) -> u256 {
    let f = answer
    let pair = (f, answer)
    pair.0(value: x) + f(value: x)
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let artifacts = normalized_func(&db, top_mod, "stored");
        verify_normalized_body(&db, &artifacts.body).expect("function item values must verify");
    }

    #[test]
    fn branch_results_are_ssa_block_parameters() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "normalized.fe".into(),
            r#"
fn choose(flag: bool, lhs: u256, rhs: u256) -> u256 {
    if flag { lhs } else { rhs }
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let artifacts = normalized_func(&db, top_mod, "choose");
        verify_normalized_body(&db, &artifacts.body).unwrap_or_else(|error| {
            panic!(
                "normalized body should verify: {error:?}\n{:#?}",
                artifacts.body
            )
        });
        assert!(
            artifacts
                .body
                .blocks
                .iter()
                .any(|block| !block.params.is_empty()),
            "branch result must cross its join through a block parameter"
        );
    }

    #[test]
    fn mutable_index_rereads_produce_distinct_values() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "normalized.fe".into(),
            r#"
fn read_twice(var _ index: own usize, values: [u256; 2]) -> u256 {
    let first = values[index]
    index = 1
    first + values[index]
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let artifacts = normalized_func(&db, top_mod, "read_twice");
        verify_normalized_body(&db, &artifacts.body).unwrap_or_else(|error| {
            panic!(
                "normalized body should verify: {error:?}\n{:#?}",
                artifacts.body
            )
        });
        let indices = artifacts
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    expr: NExpr::ProjectValue { path, .. },
                    ..
                } => path.0.iter().find_map(|projection| match projection {
                    NDataProjection::Index(NIndex::Value(value)) => Some(*value),
                    NDataProjection::Field(_)
                    | NDataProjection::VariantField { .. }
                    | NDataProjection::Entry(_)
                    | NDataProjection::Index(NIndex::Const(_)) => None,
                }),
                NStatementKind::Define {
                    expr: NExpr::Load { place, .. },
                    ..
                } => place.path.iter().find_map(|projection| match projection {
                    NDataProjection::Index(NIndex::Value(value)) => Some(*value),
                    NDataProjection::Field(_)
                    | NDataProjection::VariantField { .. }
                    | NDataProjection::Entry(_)
                    | NDataProjection::Index(NIndex::Const(_)) => None,
                }),
                NStatementKind::Define { .. }
                | NStatementKind::Store { .. }
                | NStatementKind::End { .. }
                | NStatementKind::Drop { .. } => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            indices.len(),
            2,
            "unexpected normalized indices: {indices:?}"
        );
        assert_ne!(
            indices[0], indices[1],
            "an index reread after mutation must have fresh value identity"
        );

        let mut invalid = artifacts.body.clone();
        let mut replaced = false;
        'blocks: for block in &mut invalid.blocks {
            for statement in &mut block.statements {
                let NStatementKind::Define { expr, .. } = &mut statement.kind else {
                    continue;
                };
                let path = match expr {
                    NExpr::ProjectValue { path, .. } => Some(&mut path.0),
                    NExpr::Load { place, .. } => Some(&mut place.path),
                    _ => None,
                };
                if let Some(path) = path
                    && path.iter().any(|projection| {
                        matches!(projection, NDataProjection::Index(NIndex::Value(value)) if *value == indices[0])
                    })
                {
                    *path = NDataPath::new(
                        path.iter()
                            .map(|projection| match projection {
                                NDataProjection::Index(NIndex::Value(value))
                                    if *value == indices[0] =>
                                {
                                    NDataProjection::Index(NIndex::Value(indices[1]))
                                }
                                projection => *projection,
                            })
                            .collect::<Vec<_>>(),
                    );
                    replaced = true;
                    break 'blocks;
                }
            }
        }
        assert!(replaced, "missing first dynamic index projection");
        assert!(matches!(
            verify_normalized_body(&db, &invalid),
            Err(NormalizedBodyVerifyError::UseBeforeDefinition { value, .. })
                if value == indices[1]
        ));
    }

    #[test]
    fn scalar_casts_reject_capabilities_and_invalid_repacks() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "normalized.fe".into(),
            r#"
fn widen(_ value: own u8) -> u256 {
    value as u256
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let artifacts = normalized_func(&db, top_mod, "widen");
        verify_normalized_body(&db, &artifacts.body).expect("scalar cast should verify");
        let (result, source) = artifacts
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::ScalarCast { value, .. },
                } => Some((*result, value.value)),
                _ => None,
            })
            .expect("widening cast must emit ScalarCast");
        assert!(
            artifacts
                .body
                .value(source)
                .expect("cast source")
                .ty
                .is_integral(&db)
        );
        assert!(
            artifacts
                .body
                .value(result)
                .expect("cast result")
                .ty
                .is_integral(&db)
        );

        let mut capability_operand = artifacts.body.clone();
        let source_ty = capability_operand.values[source.index()].ty;
        capability_operand.values[source.index()].ty = TyId::borrow_mut_of(&db, source_ty);
        assert!(matches!(
            verify_normalized_body(&db, &capability_operand),
            Err(NormalizedBodyVerifyError::ScalarOperandCapability(value)) if value == source
        ));

        let mut invalid_repack = artifacts.body.clone();
        for block in &mut invalid_repack.blocks {
            for statement in &mut block.statements {
                if let NStatementKind::Define {
                    result: candidate,
                    expr,
                } = &mut statement.kind
                    && *candidate == result
                {
                    let value = match expr {
                        NExpr::ScalarCast { value, .. } => *value,
                        _ => unreachable!("located scalar cast changed shape"),
                    };
                    *expr = NExpr::StructuralRepack {
                        value,
                        mapping: StructuralRepack {
                            fields: Box::new([]),
                        },
                    };
                }
            }
        }
        assert_eq!(
            verify_normalized_body(&db, &invalid_repack),
            Err(NormalizedBodyVerifyError::InvalidRepack)
        );
    }

    #[test]
    fn symbolic_array_defaults_normalize_and_verify() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "symbolic_array_defaults.fe".into(),
            include_str!(
                "../../../../../uitest/fixtures/ty_check/generic_type_default_environments.fe"
            ),
        );
        let (top_mod, _) = db.top_mod(file);
        for name in ["forward_nested", "recursive_nested"] {
            let artifacts = normalized_func(&db, top_mod, name);
            verify_normalized_body(&db, &artifacts.body)
                .unwrap_or_else(|error| panic!("{name}: {error:?}"));
            let raw = artifacts.body.owner.admitted_body(&db).unwrap();
            verify_normalized_layout_plan(&db, &artifacts.body, raw, &artifacts.layout_plan)
                .unwrap_or_else(|error| panic!("{name}: {error:?}"));
        }
    }

    #[test]
    fn scalar_expressions_require_runtime_scalar_operator_signatures() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "normalized.fe".into(),
            r#"
struct Wide {
    left: u256,
    right: u256,
}

fn unary(wide: own Wide, value: u256) -> u256 {
    ~value
}

fn compare(flag: bool, value: u256) -> u8 {
    match value {
        0 => 0,
        _ => 1,
    }
}

fn widen(wide: own Wide, value: u8) -> u256 {
    value as u256
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);

        let unary = normalized_func(&db, top_mod, "unary").body;
        verify_normalized_body(&db, &unary).expect("primitive unary expression should verify");
        let (unary_result, wide_value) = {
            let unary_result = unary
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .find_map(|statement| match statement.kind {
                    NStatementKind::Define {
                        result,
                        expr:
                            NExpr::Unary {
                                op: UnaryOp::BitNot,
                                ..
                            },
                    } => Some(result),
                    _ => None,
                })
                .expect("unary plus expression");
            let wide_value = unary
                .values
                .iter()
                .enumerate()
                .find_map(|(index, value)| {
                    matches!(value.definition, NValueDefinition::EntryParam { param: 0 })
                        .then_some(NValueId::new(index))
                })
                .expect("wide entry value");
            (unary_result, wide_value)
        };

        let mut aggregate_operand = unary.clone();
        let unary_value = aggregate_operand
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Unary { value, .. },
                    ..
                } => Some(value),
                _ => None,
            })
            .expect("unary expression");
        unary_value.value = wide_value;
        assert_eq!(
            verify_normalized_body(&db, &aggregate_operand),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut bool_operator = unary.clone();
        let unary_op = bool_operator
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Unary { op, .. },
                    ..
                } => Some(op),
                _ => None,
            })
            .expect("unary expression");
        *unary_op = UnaryOp::Not;
        assert_eq!(
            verify_normalized_body(&db, &bool_operator),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut unary_result_mismatch = unary.clone();
        unary_result_mismatch.values[unary_result.index()].ty = TyId::bool(&db);
        assert_eq!(
            verify_normalized_body(&db, &unary_result_mismatch),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let comparison = normalized_func(&db, top_mod, "compare").body;
        verify_normalized_body(&db, &comparison).expect("literal comparison should verify");
        let comparison_result = comparison
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| match statement.kind {
                NStatementKind::Define {
                    result,
                    expr:
                        NExpr::Binary {
                            op: BinaryOp::Comp(_),
                            ..
                        },
                } => Some(result),
                _ => None,
            })
            .expect("literal comparison expression");

        let mut arithmetic_result_mismatch = comparison.clone();
        let comparison_op = arithmetic_result_mismatch
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Binary { op, .. },
                    ..
                } if matches!(op, BinaryOp::Comp(_)) => Some(op),
                _ => None,
            })
            .expect("literal comparison expression");
        *comparison_op = BinaryOp::Add(Overflow::Checked);
        assert_eq!(
            verify_normalized_body(&db, &arithmetic_result_mismatch),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut comparison_result_mismatch = comparison.clone();
        comparison_result_mismatch.values[comparison_result.index()].ty = TyId::u256(&db);
        assert_eq!(
            verify_normalized_body(&db, &comparison_result_mismatch),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut aggregate_cast = normalized_func(&db, top_mod, "widen").body;
        let wide_value = aggregate_cast
            .values
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                matches!(value.definition, NValueDefinition::EntryParam { param: 0 })
                    .then_some(NValueId::new(index))
            })
            .expect("wide entry value");
        let cast_value = aggregate_cast
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::ScalarCast { value, .. },
                    ..
                } => Some(value),
                _ => None,
            })
            .expect("scalar cast expression");
        cast_value.value = wide_value;
        assert_eq!(
            verify_normalized_body(&db, &aggregate_cast),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );
    }

    #[test]
    fn verifier_rejects_boundary_type_and_mutability_mismatches() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "normalized.fe".into(),
            r#"
enum Choice {
    A,
    B,
}

enum One {
    A,
}

impl Copy for One {}

struct Empty {}

struct Pair {
    first: u256,
}

fn constant() -> u256 {
    1
}

fn identity(value: u256) -> u256 {
    value
}

fn read_effect() -> u256 uses (value: u256) {
    value
}

fn call_effect() -> u256 uses (value: u256) {
    read_effect()
}

fn call_identity() -> u256 {
    identity(value: 1)
}

fn call_identity_from(flag: bool) -> u256 {
    identity(value: 1)
}

fn return_value(flag: bool, value: u256) -> u256 {
    value
}

fn borrow_owned(var _ value: own u256) -> mut u256 {
    mut value
}

fn classify(choice: Choice) -> u8 {
    match choice {
        Choice::A => 0,
        Choice::B => 1,
    }
}

fn make_one() -> One {
    One::A
}

fn classify_one(choice: One) -> u8 {
    match choice {
        One::A => 0,
    }
}

fn empty() -> Empty {
    Empty {}
}

fn project_pair(value: own Pair) -> u256 {
    value.first
}

fn generic_boundaries<T>(pair: (T, T), array: [T; 2]) -> (T, T) {
    pair
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);

        let mut borrowed_const = normalized_func(&db, top_mod, "constant").body;
        let borrowed_const_result = borrowed_const
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| match statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::Const(_),
                } => Some(result),
                _ => None,
            })
            .expect("constant expression");
        borrowed_const.values[borrowed_const_result.index()].ty =
            TyId::view_of(&db, TyId::u256(&db));
        assert_eq!(
            verify_normalized_body(&db, &borrowed_const),
            Err(NormalizedBodyVerifyError::ScalarCapability)
        );

        let mut invalid_mutable_const = borrowed_const.clone();
        invalid_mutable_const.values[borrowed_const_result.index()].ty =
            TyId::borrow_mut_of(&db, TyId::u256(&db));
        assert_eq!(
            verify_normalized_body(&db, &invalid_mutable_const),
            Err(NormalizedBodyVerifyError::ScalarCapability)
        );

        let mut invalid_const = normalized_func(&db, top_mod, "constant").body;
        let const_value = invalid_const
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Const(value),
                    ..
                } => Some(value),
                _ => None,
            })
            .expect("constant expression");
        *const_value = SConst::from_trusted_source(&db, unit_const(&db));
        assert_eq!(
            verify_normalized_body(&db, &invalid_const),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_call = normalized_func(&db, top_mod, "call_identity").body;
        let call_result = invalid_call
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::Call { .. },
                } => Some(*result),
                _ => None,
            })
            .expect("call expression");
        invalid_call.values[call_result.index()].ty = TyId::bool(&db);
        assert_eq!(
            verify_normalized_body(&db, &invalid_call),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_call_arg = normalized_func(&db, top_mod, "call_identity_from").body;
        let bool_value = invalid_call_arg
            .values
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                matches!(value.definition, NValueDefinition::EntryParam { param: 0 })
                    .then_some(NValueId::new(index))
            })
            .expect("bool entry value");
        let call_args = invalid_call_arg
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { args, .. },
                    ..
                } => Some(args),
                _ => None,
            })
            .expect("call expression");
        call_args[0].value = bool_value;
        assert_eq!(
            verify_normalized_body(&db, &invalid_call_arg),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_call_arity = normalized_func(&db, top_mod, "call_identity").body;
        let call_args = invalid_call_arity
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { args, .. },
                    ..
                } => Some(args),
                _ => None,
            })
            .expect("call expression");
        *call_args = Box::new([]);
        assert_eq!(
            verify_normalized_body(&db, &invalid_call_arity),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_generic_boundary = normalized_func(&db, top_mod, "generic_boundaries").body;
        let array_value = invalid_generic_boundary
            .values
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                matches!(value.definition, NValueDefinition::EntryParam { param: 1 })
                    .then_some(NValueId::new(index))
            })
            .expect("generic array entry value");
        let return_operand = invalid_generic_boundary
            .blocks
            .iter_mut()
            .find_map(|block| match &mut block.terminator.kind {
                NTerminatorKind::Return(Some(value)) => Some(value),
                _ => None,
            })
            .expect("generic return operand");
        return_operand.value = array_value;
        assert_eq!(
            verify_normalized_body(&db, &invalid_generic_boundary),
            Err(NormalizedBodyVerifyError::OperandType)
        );

        let mut invalid_effect_binding = normalized_func(&db, top_mod, "call_effect").body;
        let effect_args = invalid_effect_binding
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { effect_args, .. },
                    ..
                } => Some(effect_args),
                _ => None,
            })
            .expect("effectful call");
        assert_eq!(effect_args.len(), 1);
        effect_args[0].binding_idx = u32::MAX;
        assert_eq!(
            verify_normalized_body(&db, &invalid_effect_binding),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut missing_effect_arg = normalized_func(&db, top_mod, "call_effect").body;
        let effect_args = missing_effect_arg
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { effect_args, .. },
                    ..
                } => Some(effect_args),
                _ => None,
            })
            .expect("effectful call");
        *effect_args = Box::new([]);
        assert_eq!(
            verify_normalized_body(&db, &missing_effect_arg),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_effect_shape = normalized_func(&db, top_mod, "call_effect").body;
        let effect_arg = invalid_effect_shape
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { effect_args, .. },
                    ..
                } => effect_args.first_mut(),
                _ => None,
            })
            .expect("effectful call argument");
        effect_arg.pass_mode = match effect_arg.arg {
            NEffectArgValue::Place(_) => EffectPassMode::ByValue,
            NEffectArgValue::Value(_) => EffectPassMode::ByPlace,
        };
        assert_eq!(
            verify_normalized_body(&db, &invalid_effect_shape),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut unknown_effect_mode = normalized_func(&db, top_mod, "call_effect").body;
        let effect_arg = unknown_effect_mode
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { effect_args, .. },
                    ..
                } => effect_args.first_mut(),
                _ => None,
            })
            .expect("effectful call argument");
        effect_arg.pass_mode = EffectPassMode::Unknown;
        assert_eq!(
            verify_normalized_body(&db, &unknown_effect_mode),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_effect_target = normalized_func(&db, top_mod, "call_effect").body;
        let effect_arg = invalid_effect_target
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { effect_args, .. },
                    ..
                } => effect_args.first_mut(),
                _ => None,
            })
            .expect("effectful call argument");
        effect_arg.provider_target_ty = Some(TyId::bool(&db));
        assert_eq!(
            verify_normalized_body(&db, &invalid_effect_target),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_return = normalized_func(&db, top_mod, "return_value").body;
        let bool_value = invalid_return
            .values
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                matches!(value.definition, NValueDefinition::EntryParam { param: 0 })
                    .then_some(NValueId::new(index))
            })
            .expect("bool entry value");
        let return_operand = invalid_return
            .blocks
            .iter_mut()
            .find_map(|block| match &mut block.terminator.kind {
                NTerminatorKind::Return(Some(value)) => Some(value),
                _ => None,
            })
            .expect("return operand");
        return_operand.value = bool_value;
        assert_eq!(
            verify_normalized_body(&db, &invalid_return),
            Err(NormalizedBodyVerifyError::OperandType)
        );

        let mut missing_return = normalized_func(&db, top_mod, "return_value").body;
        let return_terminator = missing_return
            .blocks
            .iter_mut()
            .find_map(|block| match &mut block.terminator.kind {
                terminator @ NTerminatorKind::Return(Some(_)) => Some(terminator),
                _ => None,
            })
            .expect("return terminator");
        *return_terminator = NTerminatorKind::Return(None);
        assert_eq!(
            verify_normalized_body(&db, &missing_return),
            Err(NormalizedBodyVerifyError::OperandType)
        );

        let mut invalid_borrow = normalized_func(&db, top_mod, "borrow_owned").body;
        let borrow_root = invalid_borrow
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    expr:
                        NExpr::Borrow {
                            place:
                                NPlace {
                                    base: NPlaceBase::Root(root),
                                    ..
                                },
                            ..
                        },
                    ..
                } => Some(*root),
                _ => None,
            })
            .expect("mutable borrow root");
        invalid_borrow.roots[borrow_root.index()].mutability = Mutability::Immutable;
        assert!(matches!(
            verify_normalized_body(&db, &invalid_borrow),
            Err(NormalizedBodyVerifyError::ImmutableMutation {
                place: NPlaceBase::Root(root),
                ..
            }) if root == borrow_root
        ));

        let mut invalid_variant_test = normalized_func(&db, top_mod, "classify").body;
        let enum_value = invalid_variant_test
            .blocks
            .iter()
            .find_map(|block| match block.terminator.kind {
                NTerminatorKind::MatchEnum { value, .. } => Some(value),
                _ => None,
            })
            .expect("enum match value");
        let replaced = invalid_variant_test
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: expr @ NExpr::Const(_),
                    ..
                } => {
                    *expr = NExpr::IsEnumVariant {
                        value: enum_value,
                        variant: VariantIndex(0),
                    };
                    Some(())
                }
                _ => None,
            });
        assert!(replaced.is_some(), "enum arm constant");
        assert_eq!(
            verify_normalized_body(&db, &invalid_variant_test),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let empty_ty = normalized_func(&db, top_mod, "empty")
            .body
            .values
            .iter()
            .find_map(|value| {
                matches!(value.definition, NValueDefinition::Statement { .. }).then_some(value.ty)
            })
            .expect("empty aggregate type");

        let mut invalid_aggregate = normalized_func(&db, top_mod, "empty").body;
        let (aggregate_result, aggregate_ty) = invalid_aggregate
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::AggregateMake { ty, .. },
                } => Some((*result, ty)),
                _ => None,
            })
            .expect("empty aggregate construction");
        *aggregate_ty = TyId::u256(&db);
        invalid_aggregate.values[aggregate_result.index()].ty = TyId::u256(&db);
        assert_eq!(
            verify_normalized_body(&db, &invalid_aggregate),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_enum_make = normalized_func(&db, top_mod, "make_one").body;
        let (make_result, enum_ty) = invalid_enum_make
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::EnumMake { enum_ty, .. },
                } => Some((*result, enum_ty)),
                _ => None,
            })
            .expect("enum construction");
        *enum_ty = empty_ty;
        invalid_enum_make.values[make_result.index()].ty = empty_ty;
        assert_eq!(
            verify_normalized_body(&db, &invalid_enum_make),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let mut invalid_enum_match = normalized_func(&db, top_mod, "classify_one").body;
        let (match_value, enum_ty) = invalid_enum_match
            .blocks
            .iter_mut()
            .find_map(|block| match &mut block.terminator.kind {
                NTerminatorKind::MatchEnum { value, enum_ty, .. } => Some((value.value, enum_ty)),
                _ => None,
            })
            .expect("enum match");
        *enum_ty = empty_ty;
        invalid_enum_match.values[match_value.index()].ty = empty_ty;
        assert_eq!(
            verify_normalized_body(&db, &invalid_enum_match),
            Err(NormalizedBodyVerifyError::OperandType)
        );

        let mut invalid_variant_projection = normalized_func(&db, top_mod, "project_pair").body;
        let path = invalid_variant_projection
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::ProjectValue { path, .. },
                    ..
                } => Some(path),
                _ => None,
            })
            .expect("struct field projection");
        path.0 = NDataPath::new(
            vec![NDataProjection::VariantField {
                variant: VariantIndex(0),
                field: FieldIndex(0),
            }]
            .into_boxed_slice(),
        );
        assert_eq!(
            verify_normalized_body(&db, &invalid_variant_projection),
            Err(NormalizedBodyVerifyError::InvalidProjection)
        );
    }

    #[test]
    fn layout_plan_verifier_requires_exact_statement_provenance() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            "normalized.fe".into(),
            r#"
fn identity(value: u256) -> u256 {
    value
}

fn caller() -> u256 {
    identity(value: 1)
}

fn mutate(var _ value: own u256) {
    value = 1
}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        let artifacts = normalized_func(&db, top_mod, "caller");
        let source = artifacts
            .body
            .owner
            .admitted_body(&db)
            .expect("test body should be admitted");
        let mut invalid_value_mutability = artifacts.body.clone();
        invalid_value_mutability.values[0].mutability = Mutability::Mutable;
        assert_eq!(
            verify_normalized_layout_plan(
                &db,
                &invalid_value_mutability,
                source,
                &artifacts.layout_plan,
            ),
            Err(NormalizedLayoutPlanVerifyError::ValueMutability(
                NValueId::new(0),
            ))
        );

        let call_result = artifacts
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| match statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::Call { .. },
                } => Some(result),
                _ => None,
            })
            .expect("call result");
        let mut invalid_value_type = artifacts.body.clone();
        invalid_value_type.values[call_result.index()].ty = TyId::view_of(&db, TyId::u256(&db));
        assert_eq!(
            verify_normalized_body(&db, &invalid_value_type),
            Err(NormalizedBodyVerifyError::ExpressionType)
        );

        let constant_result = artifacts
            .body
            .blocks
            .iter()
            .flat_map(|block| &block.statements)
            .find_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    result,
                    expr: NExpr::Const(_),
                } if statement.source.is_some() => Some(*result),
                _ => None,
            })
            .expect("source constant");
        let mut invalid_value_type = artifacts.body.clone();
        invalid_value_type.values[constant_result.index()].ty = TyId::bool(&db);
        assert_eq!(
            verify_normalized_layout_plan(&db, &invalid_value_type, source, &artifacts.layout_plan),
            Err(NormalizedLayoutPlanVerifyError::ValueType(constant_result))
        );

        let mut invalid_root_mutability = normalized_func(&db, top_mod, "mutate");
        let root = NRootId::new(0);
        invalid_root_mutability.body.roots[root.index()].mutability = Mutability::Immutable;
        let root_source = invalid_root_mutability
            .body
            .owner
            .admitted_body(&db)
            .expect("test body should be admitted");
        assert_eq!(
            verify_normalized_layout_plan(
                &db,
                &invalid_root_mutability.body,
                root_source,
                &invalid_root_mutability.layout_plan,
            ),
            Err(NormalizedLayoutPlanVerifyError::RootMutability(root))
        );

        let mut invalid_root_type = normalized_func(&db, top_mod, "mutate");
        invalid_root_type.body.roots[root.index()].ty = TyId::bool(&db);
        assert_eq!(
            verify_normalized_layout_plan(
                &db,
                &invalid_root_type.body,
                root_source,
                &invalid_root_type.layout_plan,
            ),
            Err(NormalizedLayoutPlanVerifyError::RootType(root))
        );

        let mut missing = artifacts.body.clone();
        let statement = missing
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .find_map(|statement| match &mut statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { .. },
                    ..
                } => statement.source.take(),
                _ => None,
            })
            .expect("call statement source");
        assert_eq!(
            verify_normalized_layout_plan(&db, &missing, source, &artifacts.layout_plan),
            Err(NormalizedLayoutPlanVerifyError::MissingStatementSource(
                statement,
            ))
        );

        let mut unknown = artifacts.body.clone();
        let unknown_id = SStmtId::new(source.blocks.iter().flat_map(|block| &block.stmts).count());
        unknown.blocks[0].statements[0].source = Some(unknown_id);
        assert_eq!(
            verify_normalized_layout_plan(&db, &unknown, source, &artifacts.layout_plan),
            Err(NormalizedLayoutPlanVerifyError::UnknownStatementSource(
                unknown_id,
            ))
        );

        let mut duplicate = artifacts.body.clone();
        let mut sourced = duplicate
            .blocks
            .iter_mut()
            .flat_map(|block| &mut block.statements)
            .filter(|statement| statement.source.is_some())
            .take(2)
            .collect::<Vec<_>>();
        let first = sourced[0].source.expect("first statement source");
        sourced[1].source = Some(first);
        assert_eq!(
            verify_normalized_layout_plan(&db, &duplicate, source, &artifacts.layout_plan),
            Err(NormalizedLayoutPlanVerifyError::DuplicateStatementSource(
                first,
            ))
        );
    }
}
