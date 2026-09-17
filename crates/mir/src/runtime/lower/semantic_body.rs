use cranelift_entity::EntityRef;
use hir::analysis::{
    HirAnalysisDb,
    semantic::{
        PlaceProvenance, SLocal, SLocalId, SemanticBody, SemanticInstance, SemanticLocalRole,
        SemanticNormalizationFailure, ValueProvenance,
        normalized::{
            NEffectArgValue, NExpr, NLayoutBackingSource, NLayoutPlan, NOperand, NPlace,
            NPlaceBase, NRootId, NStatementKind, NValueDefinition, NValueId, NormalizedBody,
            normalize_semantic_body,
        },
    },
};
use hir::hir_def::ExprId;

/// The admitted semantic and representation artifacts consumed by runtime lowering.
///
/// `NormalizedBody` is the semantic authority. `NLayoutPlan` records source and
/// backing metadata; `value_locals` assigns runtime storage independently of that
/// source identity. Synthetic values never overwrite their containing source local.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RuntimeSemanticBody<'db> {
    pub(crate) normalized: NormalizedBody<'db>,
    pub(crate) layout_plan: NLayoutPlan<'db>,
    pub(crate) source: SemanticBody<'db>,
    /// Source locals retain their indices for bindings and root metadata. Fresh
    /// direct locals follow them for values introduced by normalization.
    pub(crate) locals: Vec<SLocal<'db>>,
    value_locals: Vec<SLocalId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct RuntimeOperand {
    pub(crate) local: SLocalId,
    pub(crate) value: Option<NValueId>,
    pub(crate) origin: Option<ExprId>,
    pub(crate) mode: hir::analysis::semantic::normalized::ReadMode,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct RuntimeRootDemand {
    read_by_place: bool,
    written_by_place: bool,
    borrowed_or_addr_taken: bool,
    mut_borrowed_or_addr_taken: bool,
    passed_by_place: bool,
    nonself_backing_place: bool,
    always_rooted: bool,
}

impl RuntimeRootDemand {
    pub(crate) fn needs_runtime_root(self) -> bool {
        self.read_by_place
            || self.written_by_place
            || self.borrowed_or_addr_taken
            || self.mut_borrowed_or_addr_taken
            || self.passed_by_place
            || self.nonself_backing_place
            || self.always_rooted
    }

    pub(crate) fn needs_projectable_owned_storage(self) -> bool {
        self.read_by_place
            || self.written_by_place
            || self.borrowed_or_addr_taken
            || self.mut_borrowed_or_addr_taken
            || self.passed_by_place
    }

    pub(crate) fn permits_unrooted_value_projection_reads(self) -> bool {
        !self.written_by_place
            && !self.borrowed_or_addr_taken
            && !self.mut_borrowed_or_addr_taken
            && !self.passed_by_place
    }
}

impl<'db> RuntimeSemanticBody<'db> {
    pub(crate) fn admitted(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Result<Self, SemanticNormalizationFailure<'db>> {
        let artifacts = normalize_semantic_body(db, instance)?;
        let source = instance.body(db).clone();
        let mut locals = source.locals.clone();
        let value_locals = artifacts
            .body
            .values
            .iter()
            .enumerate()
            .map(|(index, value)| {
                if let NValueDefinition::Statement { block, statement } = value.definition
                    && artifacts.body.blocks[block.index()].statements[statement as usize]
                        .source
                        .is_none()
                {
                    let local = SLocalId::new(locals.len());
                    locals.push(SLocal {
                        ty: value.ty,
                        mutability: value.mutability,
                        source: None,
                        role: SemanticLocalRole::DirectValue {
                            provenance: ValueProvenance::Ordinary,
                        },
                        snapshot_source: None,
                        layout_backing_sources: Vec::new(),
                    });
                    local
                } else {
                    artifacts
                        .layout_plan
                        .value_source(NValueId::new(index))
                        .expect("verified normalized value must have source metadata")
                }
            })
            .collect();
        Ok(Self {
            normalized: artifacts.body,
            layout_plan: artifacts.layout_plan,
            source,
            locals,
            value_locals,
        })
    }

    pub(crate) fn owner(&self) -> SemanticInstance<'db> {
        self.normalized.owner
    }

    pub(crate) fn local(&self, local: SLocalId) -> Option<&SLocal<'db>> {
        self.locals.get(local.index())
    }

    pub(crate) fn value_local(&self, value: NValueId) -> Option<SLocalId> {
        self.value_locals.get(value.index()).copied()
    }

    pub(crate) fn operand_local(&self, operand: NOperand) -> Option<SLocalId> {
        self.value_local(operand.value)
    }

    pub(crate) fn runtime_operand(&self, operand: NOperand) -> Option<RuntimeOperand> {
        Some(RuntimeOperand {
            local: self.operand_local(operand)?,
            value: Some(operand.value),
            origin: operand.origin,
            mode: operand.mode,
        })
    }

    pub(crate) fn root_local(&self, root: NRootId) -> Option<SLocalId> {
        self.layout_plan.root_source(root)
    }

    pub(crate) fn root_demand(&self, local: SLocalId) -> RuntimeRootDemand {
        let mut demand = RuntimeRootDemand {
            always_rooted: self.local(local).is_some_and(|local| {
                matches!(
                    local.role,
                    SemanticLocalRole::PlaceCarrier { .. }
                        | SemanticLocalRole::PlaceBoundValue {
                            provenance: PlaceProvenance::RootProvider(_),
                            ..
                        }
                )
            }),
            ..RuntimeRootDemand::default()
        };
        for block in &self.normalized.blocks {
            for statement in &block.statements {
                match &statement.kind {
                    NStatementKind::Define { expr, .. } => {
                        self.mark_expr_root_demand(local, expr, &mut demand)
                    }
                    NStatementKind::Store { destination, .. } => {
                        if self.place_source(destination) == Some(local) {
                            demand.written_by_place = true;
                        }
                    }
                }
            }
        }
        for backing in &self.layout_plan.use_backings {
            let source = match backing.source {
                NLayoutBackingSource::Value { value, .. } => self.value_local(value),
                NLayoutBackingSource::Root { root, .. } => self.root_local(root),
            };
            if source == Some(local) && self.value_local(backing.value) != Some(local) {
                demand.nonself_backing_place = true;
            }
        }
        demand
    }

    fn mark_expr_root_demand(
        &self,
        local: SLocalId,
        expr: &NExpr<'db>,
        demand: &mut RuntimeRootDemand,
    ) {
        match expr {
            NExpr::Load { place, .. } => {
                if self.place_source(place) == Some(local) {
                    demand.read_by_place = true;
                }
            }
            NExpr::Borrow { place, kind, .. } => {
                if self.place_source(place) == Some(local) {
                    demand.borrowed_or_addr_taken = true;
                    demand.mut_borrowed_or_addr_taken =
                        matches!(kind, hir::analysis::ty::ty_def::BorrowKind::Mut);
                }
            }
            NExpr::Call { effect_args, .. } => {
                for arg in effect_args {
                    match &arg.arg {
                        NEffectArgValue::Place(place)
                            if self.place_source(place) == Some(local) =>
                        {
                            demand.passed_by_place = true;
                            demand.mut_borrowed_or_addr_taken |= arg.required_mut;
                        }
                        NEffectArgValue::Value(value)
                            if arg.required_mut
                                && matches!(
                                    arg.pass_mode,
                                    hir::analysis::ty::ty_check::EffectPassMode::ByTempPlace
                                )
                                && self.operand_local(*value) == Some(local) =>
                        {
                            demand.passed_by_place = true;
                            demand.mut_borrowed_or_addr_taken = true;
                        }
                        NEffectArgValue::Place(_) | NEffectArgValue::Value(_) => {}
                    }
                }
            }
            NExpr::Forward { .. }
            | NExpr::ProjectValue { .. }
            | NExpr::StructuralRepack { .. }
            | NExpr::CodeRegionRef { .. }
            | NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::ArrayRepeat { .. }
            | NExpr::AggregateMake { .. }
            | NExpr::EnumMake { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => {}
        }
    }

    fn place_source(&self, place: &NPlace<'db>) -> Option<SLocalId> {
        match place.base {
            NPlaceBase::Root(root) => self.root_local(root),
            NPlaceBase::CapabilityTarget { carrier } => self.value_local(carrier),
        }
    }
}
