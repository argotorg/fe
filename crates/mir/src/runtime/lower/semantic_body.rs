use hir::analysis::{
    HirAnalysisDb,
    semantic::{
        SLocal, SLocalId, SemanticBody, SemanticInstance, SemanticNormalizationFailure,
        normalized::{
            NEffectArgValue, NExpr, NLayoutBackingSource, NLayoutPlan, NOperand, NPlace,
            NPlaceBase, NRootId, NStatementKind, NValueId, NormalizedBody, normalize_semantic_body,
        },
    },
};
use hir::hir_def::ExprId;

/// The admitted semantic and representation artifacts consumed by runtime lowering.
///
/// `NormalizedBody` is the semantic authority. `NLayoutPlan` maps its immutable
/// values and roots onto the mutable runtime-local representation retained by rMIR.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct RuntimeSemanticBody<'db> {
    pub(crate) normalized: NormalizedBody<'db>,
    pub(crate) layout_plan: NLayoutPlan<'db>,
    pub(crate) source: SemanticBody<'db>,
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
        Ok(Self {
            normalized: artifacts.body,
            layout_plan: artifacts.layout_plan,
            source,
        })
    }

    pub(crate) fn owner(&self) -> SemanticInstance<'db> {
        self.normalized.owner
    }

    pub(crate) fn local(&self, local: SLocalId) -> Option<&SLocal<'db>> {
        self.source.local(local)
    }

    pub(crate) fn value_source(&self, value: NValueId) -> Option<SLocalId> {
        self.layout_plan.value_source(value)
    }

    pub(crate) fn operand_source(&self, operand: NOperand) -> Option<SLocalId> {
        self.value_source(operand.value)
    }

    pub(crate) fn runtime_operand(&self, operand: NOperand) -> Option<RuntimeOperand> {
        Some(RuntimeOperand {
            local: self.operand_source(operand)?,
            value: Some(operand.value),
            origin: operand.origin,
            mode: operand.mode,
        })
    }

    pub(crate) fn root_source(&self, root: NRootId) -> Option<SLocalId> {
        self.layout_plan.root_source(root)
    }

    pub(crate) fn root_demand(&self, local: SLocalId) -> RuntimeRootDemand {
        use hir::analysis::semantic::{PlaceProvenance, SemanticLocalRole};

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
                NLayoutBackingSource::Value { value, .. } => self.value_source(value),
                NLayoutBackingSource::Root { root, .. } => self.root_source(root),
            };
            if source == Some(local) && self.value_source(backing.value) != Some(local) {
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
                                && self.operand_source(*value) == Some(local) =>
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
            NPlaceBase::Root(root) => self.root_source(root),
            NPlaceBase::CapabilityTarget { carrier } => self.value_source(carrier),
        }
    }
}
