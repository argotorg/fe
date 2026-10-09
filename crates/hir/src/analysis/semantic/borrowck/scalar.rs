//! Trusted scalar predicates share the borrow checker's scoped guard algebra.
use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SemOrigin,
            capability::{
                external::ExternalOrigin,
                guard::Guard,
                index::{BinderScope, IndexExpr},
                region::{RegionRoot, RegionSet},
                state::BorrowState,
            },
            definite_assignment::literal_bool_cond,
            diagnostics::{SemanticDiagnostic, normalized_body_internal_diag},
            normalized::{
                NDataPath, NDataProjection, NEffectArgValue, NExpr, NIndex, NPlace, NPlaceBase,
                NRootKind, NStatement, NStatementKind, NTerminatorKind, NValueDefinition, NValueId,
                NormalizedBody, copied_scalar_ty,
            },
        },
        ty::{
            corelib::{PrimitiveWrapperCallKind, core_primitive_wrapper_call_kind},
            ty_check::BodyOwner,
            ty_def::{TyBase, TyData, TyId, prim_int_bits},
        },
    },
    hir_def::{BinOp, CompBinOp, LogicalBinOp, UnOp},
};

use super::{ir::ObservedParams, loop_certificate::frontier_candidates, solver::Borrowck};

/// Nested boolean operations followed when deriving a branch condition.
pub(super) const CONDITION_BUDGET: u8 = 16;

/// Dependency policies share transfers, while only selectors admit bounds and
/// predicates demand comparison operands within the condition budget.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum ScalarUse {
    Live,
    Value,
    Selector,
    Predicate(u8),
}

/// Scalar facts are generated on demand: for index selectors, loop frontiers,
/// representable integer returns, returned boolean equality predicates, and
/// parameters an assertion constrains on every normal return.
#[derive(Default)]
pub(super) struct ScalarDemand<'db> {
    /// Values whose trusted comparisons contribute guard facts.
    pub values: FxHashSet<NValueId>,
    pub indices: FxHashSet<IndexExpr<'db>>,
    /// Selector indices may also carry unsigned bounds.
    pub selectors: FxHashSet<IndexExpr<'db>>,
    /// Block parameters that keep their incoming equalities.
    pub phis: FxHashSet<NValueId>,
    /// Exact scalar cells whose store versions are tracked.
    pub cells: FxHashSet<NPlaceBase>,
    /// Reader-loop conditions whose bounds apply once a fill is certified.
    pub bounded_readers: FxHashSet<NValueId>,
    /// Values whose scalar facts a check, a region, an export or a callee can
    /// read; see [`Borrowck::scalar_liveness`].
    pub live: FxHashSet<NValueId>,
    /// The parameters among them, as this body's summary exports them.
    pub observed: ObservedParams,
    /// Unconditional observations other than control-flow choices. Scalar-only
    /// summaries can refine branch observations once their postcondition is known.
    pub non_branch_params: BTreeSet<u32>,
    readers: Vec<(NValueId, Vec<NValueId>)>,
}

pub(super) fn integer_model(db: &dyn HirAnalysisDb, ty: TyId<'_>) -> Option<(usize, bool)> {
    let TyData::TyBase(TyBase::Prim(primitive)) = copied_scalar_ty(db, ty).data(db) else {
        return None;
    };
    Some((prim_int_bits(*primitive)?, primitive.is_signed_int()))
}

pub(super) fn lossless_integer_cast(
    db: &dyn HirAnalysisDb,
    source: TyId<'_>,
    target: TyId<'_>,
) -> bool {
    integer_model(db, source)
        .zip(integer_model(db, target))
        .is_some_and(
            |((source_bits, source_signed), (target_bits, target_signed))| {
                source_signed == target_signed
                    && source_bits <= target_bits
                    && (!source_signed || source_bits == target_bits)
            },
        )
}

impl<'db> Borrowck<'db> {
    pub(super) fn prepare_scalar_demand(&mut self) -> Result<(), SemanticDiagnostic<'db>> {
        let (mut selectors, cells, values) = scalar_seeds(self);
        if self.inventory.loops.has_cycle()
            && self.body.blocks.iter().flat_map(|block| &block.statements).any(|statement| {
                self.stores_capability(statement)
                    || matches!(&statement.kind, NStatementKind::Define { result, expr: NExpr::Call { .. } }
                        if self.inventory.shapes[result.index()].contains_capability(self.db))
            })
        {
            self.frontiers = frontier_candidates(self.db, &self.body)
                .map_err(|error| {
                    normalized_body_internal_diag(
                        self.db,
                        self.instance,
                        &self.body,
                        SemOrigin::Body(self.body.template_owner),
                        format!("invalid normalized loop control flow: {error:?}"),
                    )
                })?
                .into_iter()
                .flatten()
                .collect();
        }
        let mut readers = Vec::new();
        for candidate in &self.frontiers {
            let mut frontier: Vec<_> = candidate
                .loop_region
                .blocks
                .iter()
                .flat_map(|block| &self.body.blocks[block.index()].statements)
                .filter_map(|statement| match &statement.kind {
                    NStatementKind::Define {
                        result,
                        expr: NExpr::Load { place, .. },
                    } if place.base == candidate.frontier_root && place.path.is_empty() => {
                        Some(*result)
                    }
                    _ => None,
                })
                .collect();
            let indices: FxHashSet<_> = frontier.iter().map(|value| self.index(*value)).collect();
            let statements = &self.body.blocks[candidate.body.index()].statements;
            let reads = statements.iter().any(|statement| {
                matches!(&statement.kind,
                    NStatementKind::Define { result, expr: NExpr::Call { args, .. } }
                        if self.inventory.shapes[result.index()].contains_capability(self.db)
                            && args.iter().any(|argument| indices.contains(&self.index(argument.value))))
            });
            frontier.extend([candidate.header_value, candidate.bound]);
            if statements
                .iter()
                .any(|statement| self.stores_capability(statement))
            {
                selectors.extend(frontier);
            } else if reads {
                readers.push((candidate.condition, frontier));
            }
        }
        self.scalar.readers = readers;
        self.add_scalar_demand(selectors, values, cells);
        Ok(())
    }

    /// Values whose scalar facts something can read, and those read only through
    /// a scalar result: the result relation is the one fact a caller forgets
    /// when the call's result is dead. Every use counts unless it is known to
    /// relate no fact to its operands: arithmetic, a lossy cast, an unused or
    /// fact-free result, a cell nothing reads, or an argument the callee's
    /// summary never observes. A call result outside the live set occurs in no
    /// other fact, so forgetting its relation to the arguments is exact.
    fn scalar_liveness(
        &self,
        stored: &FxHashMap<NValueId, FxHashSet<NValueId>>,
    ) -> (FxHashSet<NValueId>, ObservedParams, BTreeSet<u32>) {
        let mut pending = Vec::new();
        let mut branches = Vec::new();
        let mut returned = Vec::new();
        for block in &self.body.blocks {
            for statement in &block.statements {
                match &statement.kind {
                    NStatementKind::Define { result, expr } => {
                        expr.for_each_place_operand(|place| {
                            pending.extend(self.body.place_values(place));
                        });
                        if let NExpr::ProjectValue { path, .. } = expr {
                            pending.extend(path.0.iter().filter_map(
                                |projection| match projection {
                                    NDataProjection::Index(NIndex::Value(value)) => Some(*value),
                                    _ => None,
                                },
                            ));
                        }
                        if let NExpr::Call { effect_args, .. } = expr
                            && primitive_operator(self.db, &self.body, *result).is_none()
                        {
                            pending.extend(self.observed_arguments(*result, false));
                            pending.extend(effect_args.iter().filter_map(|arg| match arg.arg {
                                NEffectArgValue::Value(value) => Some(value.value),
                                NEffectArgValue::Place(_) => None,
                            }));
                        }
                    }
                    NStatementKind::Store { destination, .. } => {
                        pending.extend(self.body.place_values(destination));
                    }
                }
            }
            match &block.terminator.kind {
                NTerminatorKind::Branch { cond: value, .. }
                | NTerminatorKind::MatchEnum { value, .. } => branches.push(value.value),
                NTerminatorKind::Return(Some(value)) => returned.push(value.value),
                NTerminatorKind::Goto(_)
                | NTerminatorKind::Assert { .. }
                | NTerminatorKind::Return(None) => {}
            }
        }
        let params = |live: &FxHashSet<NValueId>| {
            live.iter()
                .filter_map(|value| match self.body.values[value.index()].definition {
                    NValueDefinition::EntryParam { param } => Some(param),
                    _ => None,
                })
                .collect::<BTreeSet<_>>()
        };
        let mut live = FxHashSet::default();
        self.propagate_liveness(&mut live, pending, stored);
        let non_branch = params(&live);
        self.propagate_liveness(&mut live, branches, stored);
        let unconditional = live.clone();
        self.propagate_liveness(&mut live, returned, stored);
        // Integral and boolean result relations are projected when dead;
        // choices of other results stay, along with the arguments they name.
        let result_ty = self.instance.normalized_result_ty(self.db);
        let observed = if result_ty.is_integral(self.db) || result_ty.is_bool(self.db) {
            let unconditional = params(&unconditional);
            ObservedParams {
                through_result: params(&live).difference(&unconditional).copied().collect(),
                unconditional,
            }
        } else {
            ObservedParams {
                unconditional: params(&live),
                through_result: BTreeSet::new(),
            }
        };
        (live, observed, non_branch)
    }

    fn propagate_liveness(
        &self,
        live: &mut FxHashSet<NValueId>,
        mut pending: Vec<NValueId>,
        stored: &FxHashMap<NValueId, FxHashSet<NValueId>>,
    ) {
        while let Some(value) = pending.pop() {
            if !live.insert(value) {
                continue;
            }
            let (dependencies, _) = self.scalar_dependencies(value, ScalarUse::Live, stored);
            pending.extend(dependencies.into_iter().map(|(value, _)| value));
        }
    }

    /// The shared transfer rules for observation and fact generation. Calls
    /// follow exactly their observed arguments; loads follow reaching stores,
    /// never stores killed by a later whole-cell write.
    fn scalar_dependencies(
        &self,
        value: NValueId,
        usage: ScalarUse,
        stored: &FxHashMap<NValueId, FxHashSet<NValueId>>,
    ) -> (Vec<(NValueId, ScalarUse)>, Option<NPlaceBase>) {
        let budget = match usage {
            ScalarUse::Predicate(0) => return (Vec::new(), None),
            ScalarUse::Predicate(budget) => budget - 1,
            _ => CONDITION_BUDGET,
        };
        let inherited = if matches!(usage, ScalarUse::Predicate(_)) {
            ScalarUse::Predicate(budget)
        } else {
            usage
        };
        if let NValueDefinition::BlockParam { block, index } =
            self.body.values[value.index()].definition
        {
            let incoming = self
                .body
                .blocks
                .iter()
                .flat_map(|predecessor| predecessor.terminator.kind.successors())
                .filter(|successor| successor.block == block)
                .filter_map(|successor| successor.args.get(index as usize))
                .map(|argument| (argument.value, inherited))
                .collect();
            return (incoming, None);
        }
        let Some((_, expr)) = self.body.defining_expr(value) else {
            return (Vec::new(), None);
        };
        match expr {
            NExpr::Forward { src } => return (vec![(src.value, inherited)], None),
            NExpr::ScalarCast { value: source, to } => {
                let dependencies = self
                    .lossless_scalar_cast(source.value, *to)
                    .then_some((source.value, inherited))
                    .into_iter()
                    .collect();
                return (dependencies, None);
            }
            NExpr::Load { place, .. } => {
                let tracked = place.path.is_empty()
                    && if usage == ScalarUse::Live {
                        self.scalar.cells.contains(&place.base)
                    } else {
                        place.ty.is_integral(self.db)
                    };
                let dependencies = if tracked {
                    stored
                        .get(&value)
                        .into_iter()
                        .flatten()
                        .map(|value| (*value, inherited))
                        .collect()
                } else {
                    Vec::new()
                };
                let cell = (tracked && usage != ScalarUse::Live).then_some(place.base);
                return (dependencies, cell);
            }
            _ => {}
        }
        let operator = match expr {
            NExpr::Binary { op, .. } => Some(PrimitiveWrapperCallKind::Binary(*op)),
            NExpr::Unary { op, .. } => Some(PrimitiveWrapperCallKind::Unary(*op)),
            NExpr::Call { .. } => primitive_operator(self.db, &self.body, value),
            _ => None,
        };
        if matches!(expr, NExpr::Call { .. }) && operator.is_none() {
            let arguments = self.observed_arguments(
                value,
                usage == ScalarUse::Live || self.scalar.live.contains(&value),
            );
            let dependencies = arguments
                .into_iter()
                .filter_map(|value| {
                    let usage = if usage == ScalarUse::Live {
                        Some(ScalarUse::Live)
                    } else {
                        self.scalar_use(value, budget)
                    }?;
                    Some((value, usage))
                })
                .collect();
            return (dependencies, None);
        }
        let mut operands = Vec::new();
        expr.for_each_value_operand(|operand| operands.push(operand.value));
        let dependencies = if usage == ScalarUse::Live {
            let comparison = || {
                operands
                    .iter()
                    .any(|value| self.scalar.indices.contains(&self.index(*value)))
                    || operands.iter().all(|value| {
                        copied_scalar_ty(self.db, self.body.values[value.index()].ty)
                            .is_bool(self.db)
                    })
            };
            let relates = match operator {
                Some(PrimitiveWrapperCallKind::Binary(BinOp::Comp(_))) => comparison(),
                Some(PrimitiveWrapperCallKind::Binary(BinOp::Arith(_)))
                | Some(PrimitiveWrapperCallKind::Unary(UnOp::Plus | UnOp::Minus | UnOp::BitNot)) => {
                    false
                }
                _ if matches!(expr, NExpr::Call { .. }) => matches!(
                    operator,
                    Some(
                        PrimitiveWrapperCallKind::Binary(BinOp::Logical(_))
                            | PrimitiveWrapperCallKind::Unary(UnOp::Not)
                    )
                ),
                _ => !matches!(
                    expr,
                    NExpr::AggregateMake { .. }
                        | NExpr::EnumMake { .. }
                        | NExpr::ArrayRepeat { .. }
                        | NExpr::MakeHandle { .. }
                ),
            };
            if relates {
                operands
                    .into_iter()
                    .map(|value| (value, ScalarUse::Live))
                    .collect()
            } else {
                Vec::new()
            }
        } else if matches!(usage, ScalarUse::Predicate(_)) {
            let equality = matches!(
                operator,
                Some(PrimitiveWrapperCallKind::Binary(BinOp::Comp(
                    CompBinOp::Eq | CompBinOp::NotEq
                )))
            );
            let relates = equality
                || matches!(
                    operator,
                    Some(
                        PrimitiveWrapperCallKind::Unary(UnOp::Not)
                            | PrimitiveWrapperCallKind::Binary(BinOp::Logical(_))
                    )
                );
            if relates {
                operands
                    .into_iter()
                    .filter_map(|value| {
                        let usage = self.scalar_use(value, budget)?;
                        (equality || matches!(usage, ScalarUse::Predicate(_)))
                            .then_some((value, usage))
                    })
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        (dependencies, None)
    }

    fn scalar_use(&self, value: NValueId, budget: u8) -> Option<ScalarUse> {
        let ty = self.body.values[value.index()].ty;
        if ty.is_bool(self.db) {
            Some(ScalarUse::Predicate(budget))
        } else {
            ty.is_integral(self.db).then_some(ScalarUse::Value)
        }
    }

    /// Unknown summaries conservatively observe every argument unconditionally.
    /// Result-dependent arguments enter only when the result is observable.
    fn observed_arguments(&self, value: NValueId, through_result: bool) -> Vec<NValueId> {
        if let Some((_, NExpr::Call { args, .. })) = self.body.defining_expr(value)
            && primitive_operator(self.db, &self.body, value).is_none()
        {
            let observed = self.observed_params(value);
            args.iter()
                .enumerate()
                .filter_map(|(param, arg)| {
                    let param = u32::try_from(param).unwrap();
                    observed
                        .is_none_or(|observed| {
                            observed.unconditional.contains(&param)
                                || (through_result && observed.through_result.contains(&param))
                        })
                        .then_some(arg.value)
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    /// For each load of a tracked whole cell, the stored values that can reach
    /// it: a later whole-cell store replaces earlier ones on every path.
    fn reaching_cell_stores(&self) -> FxHashMap<NValueId, FxHashSet<NValueId>> {
        type Stores = FxHashMap<NPlaceBase, FxHashSet<NValueId>>;
        let tracked =
            |place: &NPlace<'db>| place.path.is_empty() && self.scalar.cells.contains(&place.base);
        let mut predecessors = vec![Vec::new(); self.body.blocks.len()];
        for (index, block) in self.body.blocks.iter().enumerate() {
            for successor in block.terminator.kind.successors() {
                predecessors[successor.block.index()].push(index);
            }
        }
        let mut exits = vec![Stores::default(); self.body.blocks.len()];
        let mut reaching = FxHashMap::default();
        // The pass after the exits settle records what reaches each load.
        let mut settled = false;
        loop {
            let mut changed = false;
            for &index in self.inventory.loops.reverse_postorder() {
                let mut current = Stores::default();
                for predecessor in &predecessors[index] {
                    for (base, values) in &exits[*predecessor] {
                        current.entry(*base).or_default().extend(values);
                    }
                }
                for statement in &self.body.blocks[index].statements {
                    match &statement.kind {
                        NStatementKind::Store { destination, value } if tracked(destination) => {
                            current.insert(destination.base, FxHashSet::from_iter([value.value]));
                        }
                        NStatementKind::Define {
                            result,
                            expr: NExpr::Load { place, .. },
                        } if settled && tracked(place) => {
                            reaching.insert(
                                *result,
                                current.get(&place.base).cloned().unwrap_or_default(),
                            );
                        }
                        _ => {}
                    }
                }
                if exits[index] != current {
                    exits[index] = current;
                    changed = true;
                }
            }
            if settled {
                return reaching;
            }
            settled = !changed;
        }
    }

    /// The parameters the callee of the call defining `value` observes, or
    /// `None` when its summary does not record them.
    fn observed_params(&self, value: NValueId) -> Option<&ObservedParams> {
        self.calls
            .get(&value)
            .and_then(|call| call.summary.observed_params.as_ref())
    }

    /// A reader loop uses its unsigned bound only once a fill is certified.
    pub(super) fn enable_bounded_readers(&mut self) {
        let mut selectors = FxHashSet::default();
        for (condition, frontier) in std::mem::take(&mut self.scalar.readers) {
            self.scalar.bounded_readers.insert(condition);
            selectors.extend(frontier);
        }
        if !selectors.is_empty() {
            self.add_scalar_demand(selectors, [], FxHashSet::default());
        }
    }

    /// Close observations and typed fact demand together. Every newly demanded
    /// cell or live result revisits the same dependency rules before export.
    fn add_scalar_demand(
        &mut self,
        selectors: FxHashSet<NValueId>,
        values: impl IntoIterator<Item = (NValueId, ScalarUse)>,
        cells: FxHashSet<NPlaceBase>,
    ) {
        let mut roots: Vec<_> = values.into_iter().collect();
        roots.extend(
            selectors
                .into_iter()
                .map(|value| (value, ScalarUse::Selector)),
        );
        self.scalar.cells.extend(cells);
        let mut stored = FxHashMap::default();
        let mut stored_cells = 0;
        loop {
            let previous = (
                self.scalar.values.len(),
                self.scalar.cells.len(),
                self.scalar.selectors.len(),
                self.scalar.live.len(),
            );
            let (selectors, cells, values) = scalar_seeds(self);
            self.scalar.cells.extend(cells);
            if stored_cells != self.scalar.cells.len() {
                stored = self.reaching_cell_stores();
                stored_cells = self.scalar.cells.len();
            }
            let mut pending = roots.clone();
            pending.extend(values);
            pending.extend(
                selectors
                    .into_iter()
                    .map(|value| (value, ScalarUse::Selector)),
            );
            pending.extend(self.scalar.values.iter().map(|value| {
                let usage = if self.scalar.selectors.contains(&self.index(*value)) {
                    ScalarUse::Selector
                } else {
                    ScalarUse::Value
                };
                (*value, usage)
            }));
            self.close_scalar_demand(pending, &stored);
            self.scalar.indices = self
                .scalar
                .values
                .iter()
                .map(|value| self.index(*value))
                .collect();
            if stored_cells != self.scalar.cells.len() {
                stored = self.reaching_cell_stores();
                stored_cells = self.scalar.cells.len();
            }
            (
                self.scalar.live,
                self.scalar.observed,
                self.scalar.non_branch_params,
            ) = self.scalar_liveness(&stored);
            if previous
                == (
                    self.scalar.values.len(),
                    self.scalar.cells.len(),
                    self.scalar.selectors.len(),
                    self.scalar.live.len(),
                )
            {
                break;
            }
        }
        self.scalar.phis = self
            .scalar
            .values
            .iter()
            .copied()
            .filter(|value| {
                matches!(
                    self.body.values[value.index()].definition,
                    NValueDefinition::BlockParam { .. }
                )
            })
            .collect();
    }

    fn close_scalar_demand(
        &mut self,
        mut pending: Vec<(NValueId, ScalarUse)>,
        stored: &FxHashMap<NValueId, FxHashSet<NValueId>>,
    ) {
        let mut visited = FxHashSet::default();
        let mut predicates = FxHashMap::default();
        while let Some((value, usage)) = pending.pop() {
            if let ScalarUse::Predicate(budget) = usage {
                if budget == 0
                    || predicates
                        .get(&value)
                        .is_some_and(|previous| *previous >= budget)
                {
                    continue;
                }
                predicates.insert(value, budget);
            } else if !visited.insert((value, usage)) {
                continue;
            }
            if matches!(usage, ScalarUse::Value | ScalarUse::Selector) {
                self.scalar.values.insert(value);
                if usage == ScalarUse::Selector {
                    self.scalar.selectors.insert(self.index(value));
                }
            }
            let (dependencies, cell) = self.scalar_dependencies(value, usage, stored);
            pending.extend(dependencies);
            if let Some(cell) = cell {
                self.scalar.cells.insert(cell);
                // All SSA loads of a demanded cell share its versioned identity.
                // The shared transfer follows each load's own reaching stores.
                pending.extend(
                    self.body
                        .blocks
                        .iter()
                        .flat_map(|block| &block.statements)
                        .filter_map(|statement| {
                            if let NStatementKind::Define {
                                result,
                                expr: NExpr::Load { place, .. },
                            } = &statement.kind
                                && place.path.is_empty()
                                && place.base == cell
                            {
                                Some((*result, usage))
                            } else {
                                None
                            }
                        }),
                );
            }
        }
    }

    pub(super) fn stores_capability(&self, statement: &NStatement<'db>) -> bool {
        matches!(&statement.kind,
            NStatementKind::Store { destination, value }
                if matches!(destination.base, NPlaceBase::CapabilityTarget { .. })
                    && self.inventory.shapes[value.value.index()].contains_capability(self.db))
    }

    fn compact_scalar_phi(&self, value: NValueId) -> bool {
        let NValueDefinition::BlockParam { block, index } =
            self.body.values[value.index()].definition
        else {
            return false;
        };
        let incoming: Vec<_> = self
            .body
            .blocks
            .iter()
            .flat_map(|predecessor| predecessor.terminator.kind.successors())
            .filter(|successor| successor.block == block)
            .filter_map(|successor| successor.args.get(index as usize))
            .map(|argument| self.index(argument.value))
            .collect();
        let Some(first) = incoming.first() else {
            return false;
        };
        incoming
            .iter()
            .all(|index| matches!(index, IndexExpr::Const(_)))
            || incoming.iter().all(|index| index == first)
    }

    fn supported_scalar_return(&self, mut value: NValueId) -> bool {
        loop {
            match self.body.defining_expr(value) {
                Some((_, NExpr::Forward { src })) => value = src.value,
                Some((_, NExpr::ScalarCast { value: src, to }))
                    if self.lossless_scalar_cast(src.value, *to) =>
                {
                    value = src.value;
                }
                _ => {
                    return self.compact_scalar_phi(value)
                        || !matches!(
                            self.body.values[value.index()].definition,
                            NValueDefinition::BlockParam { .. }
                        );
                }
            }
        }
    }

    pub(super) fn scalar_cell(
        &self,
        place: &NPlace<'db>,
        region: &RegionSet<'db>,
        state: &BorrowState<'db>,
    ) -> Option<RegionRoot<'db>> {
        if !place.path.is_empty()
            || !self.scalar.cells.contains(&place.base)
            || integer_model(self.db, place.ty).is_none()
        {
            return None;
        }
        let [clause] = region.clauses() else {
            return None;
        };
        if !clause.payload.path.is_empty()
            || clause.payload.views.iter().next().is_some()
            || clause.guard.scope() != state.guard().scope()
            || !state.guard().implies(&clause.guard)
        {
            return None;
        }
        match &clause.payload.root {
            RegionRoot::Root { root, .. }
                if matches!(
                    self.body.roots[root.index()].kind,
                    NRootKind::LocalSlot { .. } | NRootKind::ParamPlace { .. }
                ) =>
            {
                Some(clause.payload.root.clone())
            }
            RegionRoot::External(source)
                if matches!(&source.origin, ExternalOrigin::Input(input) if !input.is_reachable() && input.dereferences().is_empty())
                    && !source.uncertain()
                    && source.dereferences().is_empty() =>
            {
                Some(clause.payload.root.clone())
            }
            _ => None,
        }
    }

    pub(super) fn lossless_scalar_cast(&self, source: NValueId, target: TyId<'db>) -> bool {
        lossless_integer_cast(self.db, self.body.values[source.index()].ty, target)
    }

    pub(super) fn scalar_bounds_enabled(&self, value: NValueId) -> bool {
        self.inventory.loops.for_value(&self.body, value).is_none()
            || (self.scalar.bounded_readers.contains(&value)
                && (!self.prefix_certificates.is_empty()
                    || self
                        .calls
                        .values()
                        .any(|call| !call.summary.certified_ranges.is_empty())))
    }

    /// The guard under which `value` equals `expected`, following trusted
    /// boolean operations within a budget of nested conditions.
    pub(super) fn condition_guard(
        &self,
        value: NValueId,
        expected: bool,
        include_bounds: bool,
        budget: u8,
    ) -> Option<Guard<'db>> {
        let always = Guard::always(&BinderScope::default());
        if let Some(actual) = literal_bool_cond(self.db, &self.body, value) {
            return (actual == expected).then_some(always);
        }
        let selected = always.with_boolean(self.boolean_choice(value), expected)?;
        if budget == 0 {
            return Some(selected);
        }
        let Some((_, expr)) = self.body.defining_expr(value) else {
            return Some(selected);
        };
        let relation = match expr {
            NExpr::Forward { src } => {
                self.condition_guard(src.value, expected, include_bounds, budget - 1)
            }
            NExpr::Unary {
                op: UnOp::Not,
                value,
            } => self.condition_guard(value.value, !expected, include_bounds, budget - 1),
            NExpr::Binary {
                op: BinOp::Logical(operator),
                lhs,
                rhs,
            } => self.logical_guard(
                *operator,
                lhs.value,
                rhs.value,
                expected,
                include_bounds,
                budget - 1,
            ),
            NExpr::Binary {
                op: BinOp::Comp(operator),
                lhs,
                rhs,
            } => self.comparison_guard(
                *operator,
                lhs.value,
                rhs.value,
                expected,
                include_bounds,
                budget - 1,
            ),
            NExpr::Call { callee, args, .. } => {
                let BodyOwner::Func(function) = callee.key.owner(self.db) else {
                    return Some(selected);
                };
                match (
                    core_primitive_wrapper_call_kind(
                        self.db,
                        function,
                        self.body.values[value.index()].ty,
                    ),
                    args.as_ref(),
                ) {
                    (Some(PrimitiveWrapperCallKind::Unary(UnOp::Not)), [operand]) => {
                        self.condition_guard(operand.value, !expected, include_bounds, budget - 1)
                    }
                    (Some(PrimitiveWrapperCallKind::Binary(BinOp::Comp(operator))), [lhs, rhs]) => {
                        self.comparison_guard(
                            operator,
                            lhs.value,
                            rhs.value,
                            expected,
                            include_bounds,
                            budget - 1,
                        )
                    }
                    (
                        Some(PrimitiveWrapperCallKind::Binary(BinOp::Logical(operator))),
                        [lhs, rhs],
                    ) => self.logical_guard(
                        operator,
                        lhs.value,
                        rhs.value,
                        expected,
                        include_bounds,
                        budget - 1,
                    ),
                    _ => Some(always),
                }
            }
            _ => Some(always),
        }?;
        selected.and(&relation)
    }

    fn logical_guard(
        &self,
        operator: LogicalBinOp,
        lhs: NValueId,
        rhs: NValueId,
        expected: bool,
        include_bounds: bool,
        budget: u8,
    ) -> Option<Guard<'db>> {
        let both = matches!(operator, LogicalBinOp::And) == expected;
        let left = self.condition_guard(lhs, expected, include_bounds, budget);
        let right = self.condition_guard(rhs, expected, include_bounds, budget);
        if both {
            left?.and(&right?)
        } else {
            match (left, right) {
                (Some(left), Some(right)) => Some(left.or(&right)),
                (left, right) => left.or(right),
            }
        }
    }

    fn comparison_guard(
        &self,
        operator: CompBinOp,
        lhs: NValueId,
        rhs: NValueId,
        expected: bool,
        include_bounds: bool,
        budget: u8,
    ) -> Option<Guard<'db>> {
        let always = Guard::always(&BinderScope::default());
        let lhs_ty = copied_scalar_ty(self.db, self.body.values[lhs.index()].ty);
        let rhs_ty = copied_scalar_ty(self.db, self.body.values[rhs.index()].ty);
        if lhs_ty.is_bool(self.db)
            && lhs_ty == rhs_ty
            && matches!(operator, CompBinOp::Eq | CompBinOp::NotEq)
        {
            let equal = expected == matches!(operator, CompBinOp::Eq);
            return [false, true]
                .into_iter()
                .filter_map(|value| {
                    self.condition_guard(lhs, value, include_bounds, budget)?
                        .and(&self.condition_guard(
                            rhs,
                            if equal { value } else { !value },
                            include_bounds,
                            budget,
                        )?)
                })
                .reduce(|left, right| left.or(&right));
        }
        let lhs_index = self.index(lhs);
        let rhs_index = self.index(rhs);
        if !self.scalar.indices.contains(&lhs_index) && !self.scalar.indices.contains(&rhs_index) {
            return Some(always);
        }
        let Some((_, signed)) = integer_model(self.db, lhs_ty) else {
            return Some(always);
        };
        if lhs_ty != rhs_ty
            || ((!include_bounds
                || signed
                || (!self.scalar.selectors.contains(&lhs_index)
                    && !self.scalar.selectors.contains(&rhs_index)))
                && !matches!(operator, CompBinOp::Eq | CompBinOp::NotEq))
        {
            return Some(always);
        }
        let (condition, positive) = match operator {
            CompBinOp::Eq => (always.with_equality(lhs_index, rhs_index), true),
            CompBinOp::NotEq => (always.with_equality(lhs_index, rhs_index), false),
            CompBinOp::Lt => (always.with_bound(lhs_index, rhs_index), true),
            CompBinOp::LtEq => (always.with_bound(rhs_index, lhs_index), false),
            CompBinOp::Gt => (always.with_bound(rhs_index, lhs_index), true),
            CompBinOp::GtEq => (always.with_bound(lhs_index, rhs_index), false),
        };
        if expected == positive {
            condition
        } else {
            condition.map_or(Some(always.clone()), |condition| {
                always.difference(&condition)
            })
        }
    }
}

/// The primitive operator a core wrapper method call defining `value` stands for.
fn primitive_operator(
    db: &dyn HirAnalysisDb,
    body: &NormalizedBody<'_>,
    value: NValueId,
) -> Option<PrimitiveWrapperCallKind> {
    let (_, NExpr::Call { callee, .. }) = body.defining_expr(value)? else {
        return None;
    };
    let BodyOwner::Func(function) = callee.key.owner(db) else {
        return None;
    };
    core_primitive_wrapper_call_kind(db, function, body.values[value.index()].ty)
}

fn scalar_seeds<'db>(
    checker: &Borrowck<'db>,
) -> (
    FxHashSet<NValueId>,
    FxHashSet<NPlaceBase>,
    Vec<(NValueId, ScalarUse)>,
) {
    let db = checker.db;
    let body = &checker.body;
    let mut selectors = FxHashSet::default();
    let mut cells = FxHashSet::default();
    let mut values = Vec::new();
    let returns_boolean = checker.instance.normalized_result_ty(db).is_bool(db);
    for block in &body.blocks {
        for statement in &block.statements {
            let mut add_indices = |path: &NDataPath| {
                selectors.extend(path.iter().filter_map(|projection| match projection {
                    NDataProjection::Index(NIndex::Value(value)) => Some(*value),
                    _ => None,
                }));
            };
            match &statement.kind {
                NStatementKind::Define { result, expr } => {
                    expr.for_each_place_operand(|place| add_indices(&place.path));
                    if let NExpr::ProjectValue { path, .. } = expr {
                        add_indices(&path.0);
                    }
                    if matches!(expr, NExpr::Call { .. }) {
                        values.extend(
                            checker
                                .observed_arguments(*result, checker.scalar.live.contains(result))
                                .into_iter()
                                .filter_map(|value| {
                                    Some((value, checker.scalar_use(value, CONDITION_BUDGET)?))
                                }),
                        );
                    }
                }
                NStatementKind::Store { destination, value } => {
                    add_indices(&destination.path);
                    if destination.path.is_empty()
                        && destination.ty.is_integral(db)
                        && matches!(destination.base, NPlaceBase::CapabilityTarget { carrier }
                            if matches!(body.values[carrier.index()].definition, NValueDefinition::EntryParam { .. }))
                        && matches!(body.defining_expr(value.value), Some((_, NExpr::Const(_))))
                    {
                        cells.insert(destination.base);
                    }
                }
            }
        }
        match block.terminator.kind {
            NTerminatorKind::Return(Some(value)) => {
                if let Some(usage) = checker.scalar_use(value.value, CONDITION_BUDGET)
                    && (usage != ScalarUse::Value || checker.supported_scalar_return(value.value))
                {
                    values.push((value.value, usage));
                }
            }
            NTerminatorKind::Branch { cond, .. } if returns_boolean => {
                values.push((cond.value, ScalarUse::Predicate(CONDITION_BUDGET)));
            }
            _ => {}
        }
    }
    // Constant stores to writable scalar inputs also demand their load versions.
    values.extend(
        body.blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter_map(|statement| {
                if let NStatementKind::Define {
                    result,
                    expr: NExpr::Load { place, .. },
                } = &statement.kind
                    && place.path.is_empty()
                    && cells.contains(&place.base)
                {
                    Some((*result, ScalarUse::Value))
                } else {
                    None
                }
            }),
    );
    // Failed assertions can constrain integer parameters on every normal return.
    if body.blocks.iter().any(|block| {
        block.terminator.kind.successors().iter().any(|successor| {
            matches!(
                body.blocks[successor.block.index()].terminator.kind,
                NTerminatorKind::Assert { .. }
            )
        })
    }) {
        values.extend(
            body.values
                .iter()
                .enumerate()
                .filter(|(_, value)| {
                    matches!(value.definition, NValueDefinition::EntryParam { .. })
                        && value.ty.is_integral(db)
                })
                .map(|(index, _)| (NValueId::new(index), ScalarUse::Value)),
        );
    }
    (selectors, cells, values)
}
