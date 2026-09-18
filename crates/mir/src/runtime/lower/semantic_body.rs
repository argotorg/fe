use cranelift_entity::EntityRef;
use hir::analysis::{
    HirAnalysisDb,
    semantic::{
        PlaceProvenance, SLocal, SLocalId, SemanticBody, SemanticInstance, SemanticLocalRole,
        SemanticNormalizationFailure,
        normalized::{
            NEffectArgValue, NExpr, NLayoutBackingSource, NLayoutLocals, NLayoutPlan, NOperand,
            NPlace, NPlaceBase, NRootId, NRootKind, NStatementKind, NValueId, NormalizedBody,
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
        let representations = NLayoutLocals::new(&artifacts.body, &artifacts.layout_plan, &source);
        Ok(Self {
            normalized: artifacts.body,
            layout_plan: artifacts.layout_plan,
            source,
            locals: representations.locals,
            value_locals: representations.value_locals,
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
        match self.normalized.root(root)?.kind {
            NRootKind::Temporary { value } => self.value_local(value),
            _ => self.layout_plan.root_source(root),
        }
    }

    pub(crate) fn root_demand(
        &self,
        db: &'db dyn HirAnalysisDb,
        local: SLocalId,
    ) -> RuntimeRootDemand {
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
                        self.mark_expr_root_demand(db, local, expr, &mut demand)
                    }
                    NStatementKind::Store { destination, .. } => {
                        if self.place_source(db, destination) == Some(local) {
                            // Whole-local assignments update its value binding.
                            // They need physical storage only when another use
                            // requires an address, including across loop iterations.
                            let assigns_local = statement.source.is_none()
                                && destination.path.is_empty()
                                && matches!(destination.base, NPlaceBase::Root(root)
                                    if matches!(self.normalized.roots[root.index()].kind,
                                        NRootKind::LocalSlot { .. }));
                            demand.written_by_place |= !assigns_local;
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
        db: &'db dyn HirAnalysisDb,
        local: SLocalId,
        expr: &NExpr<'db>,
        demand: &mut RuntimeRootDemand,
    ) {
        match expr {
            NExpr::Load { place, .. } => {
                if self.place_source(db, place) == Some(local) {
                    demand.read_by_place = true;
                }
            }
            NExpr::Borrow { place, kind, .. } => {
                if self.place_source(db, place) == Some(local) {
                    demand.borrowed_or_addr_taken = true;
                    demand.mut_borrowed_or_addr_taken =
                        matches!(kind, hir::analysis::ty::ty_def::BorrowKind::Mut);
                }
            }
            NExpr::MakeView { place, .. } => {
                if self.place_source(db, place) == Some(local) {
                    // Read-only views may carry an immutable value directly.
                    // Other writes, borrows, or address consumers still demand storage.
                    demand.read_by_place = true;
                }
            }
            NExpr::Call { effect_args, .. } => {
                for arg in effect_args {
                    match &arg.arg {
                        NEffectArgValue::Place(place)
                            if self.place_source(db, place) == Some(local) =>
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
            | NExpr::PointerCast { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::ArrayRepeat { .. }
            | NExpr::AggregateMake { .. }
            | NExpr::MakeHandle { .. }
            | NExpr::EnumMake { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => {}
        }
    }

    fn place_source(&self, db: &'db dyn HirAnalysisDb, place: &NPlace<'db>) -> Option<SLocalId> {
        match place.base {
            NPlaceBase::Root(root) => self.root_local(root),
            NPlaceBase::CapabilityTarget { carrier } => {
                // Accessing a raw pointee does not take the address of the
                // pointer value. Its own binding needs storage only for uses
                // of a Root place, such as borrowing or replacing that binding.
                if self.normalized.value(carrier)?.ty.as_ptr(db).is_some() {
                    None
                } else {
                    self.value_local(carrier)
                }
            }
        }
    }
}
