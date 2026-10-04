//! Trusted scalar predicates share the borrow checker's scoped guard algebra.
use cranelift_entity::EntityRef;
use num_traits::ToPrimitive;
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeSet;

use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SConst, SemConstScalar, SemConstValue, SemOrigin,
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
                NDataPath, NDataProjection, NEffectArgValue, NExpr, NIndex, NOperand, NPlace,
                NPlaceBase, NRootKind, NStatement, NStatementKind, NTerminatorKind,
                NValueDefinition, NValueId, NormalizedBody, copied_scalar_ty,
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

/// Scalar facts are generated on demand: for index selectors, loop frontiers,
/// representable integer returns, and parameters an assertion constrains on
/// every normal return.
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
    readers: Vec<(NValueId, Vec<NValueId>)>,
}

/// Integer literals represented exactly by the index algebra.
pub(super) fn constant_index<'db>(
    db: &'db dyn HirAnalysisDb,
    constant: &SConst<'db>,
) -> Option<usize> {
    if let SConst::Value(constant) = constant
        && let SemConstValue::Scalar {
            value: SemConstScalar::Int { value },
            ..
        } = constant.value().value(db)
    {
        value.to_usize()
    } else {
        None
    }
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
        let (mut selectors, cells, mut values) = scalar_seeds(self.db, &self.body);
        values.retain(|value| self.supported_scalar_return(*value));
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
    /// an integer result: the result relation is the one fact a caller forgets
    /// when the call's result is dead. Every use counts unless it is known to
    /// relate no fact to its operands: arithmetic, a lossy cast, an unused or
    /// fact-free result, a cell nothing reads, or an argument the callee's
    /// summary never observes. A call result outside the live set occurs in no
    /// other fact, so forgetting its relation to the arguments is exact.
    pub(super) fn scalar_liveness(&self) -> (FxHashSet<NValueId>, ObservedParams) {
        let mut pending = Vec::new();
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
                        // A primitive operator relates its operands only through
                        // its result, below.
                        if let NExpr::Call {
                            args, effect_args, ..
                        } = expr
                            && self.primitive_operator(*result).is_none()
                        {
                            let observed = self.observed_params(*result);
                            pending.extend(args.iter().enumerate().filter_map(|(param, arg)| {
                                observed
                                    .is_none_or(|observed| {
                                        observed
                                            .unconditional
                                            .contains(&u32::try_from(param).unwrap())
                                    })
                                    .then_some(arg.value)
                            }));
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
                | NTerminatorKind::MatchEnum { value, .. } => pending.push(value.value),
                NTerminatorKind::Return(Some(value)) => returned.push(value.value),
                NTerminatorKind::Goto(_)
                | NTerminatorKind::Assert { .. }
                | NTerminatorKind::Return(None) => {}
            }
        }
        let mut live = FxHashSet::default();
        let stored = self.reaching_cell_stores();
        self.propagate_liveness(&mut live, pending, &stored);
        let unconditional = live.clone();
        self.propagate_liveness(&mut live, returned, &stored);
        let params = |live: &FxHashSet<NValueId>| {
            live.iter()
                .filter_map(|value| match self.body.values[value.index()].definition {
                    NValueDefinition::EntryParam { param } => Some(param),
                    _ => None,
                })
                .collect::<BTreeSet<_>>()
        };
        // Only an integer result's relation is forgotten; choices of other
        // results stay, along with the arguments they name.
        let observed = if self
            .instance
            .normalized_result_ty(self.db)
            .is_integral(self.db)
        {
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
        (live, observed)
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
            match self.body.values[value.index()].definition {
                NValueDefinition::BlockParam { block, index } => pending.extend(
                    self.body
                        .blocks
                        .iter()
                        .flat_map(|predecessor| predecessor.terminator.kind.successors())
                        .filter(|successor| successor.block == block)
                        .filter_map(|successor| successor.args.get(index as usize))
                        .map(|argument| argument.value),
                ),
                NValueDefinition::Statement { .. } => {
                    let Some((_, expr)) = self.body.defining_expr(value) else {
                        continue;
                    };
                    // Arithmetic, lossy casts and constructors relate no fact
                    // to their operands, and a comparison relates them only
                    // with a tracked side. A call relates the arguments its
                    // summary observes through its result, unless it is a
                    // primitive operator. A tracked cell relates a load to the
                    // stores that can reach it.
                    let tracked = |operands: &[NOperand]| {
                        operands
                            .iter()
                            .any(|operand| self.scalar.indices.contains(&self.index(operand.value)))
                    };
                    let relates = match expr {
                        NExpr::Binary {
                            op: BinOp::Comp(_),
                            lhs,
                            rhs,
                        } => tracked(&[*lhs, *rhs]),
                        NExpr::Binary { op, .. } => !matches!(op, BinOp::Arith(_)),
                        NExpr::Unary { op, .. } => {
                            !matches!(op, UnOp::Plus | UnOp::Minus | UnOp::BitNot)
                        }
                        NExpr::ScalarCast { value: source, to } => {
                            self.lossless_scalar_cast(source.value, *to)
                        }
                        NExpr::Load { .. } => {
                            pending.extend(stored.get(&value).into_iter().flatten());
                            false
                        }
                        // Constructors hold scalar fields only as values; no fact
                        // relates a field to its projection.
                        NExpr::AggregateMake { .. }
                        | NExpr::EnumMake { .. }
                        | NExpr::ArrayRepeat { .. }
                        | NExpr::MakeHandle { .. } => false,
                        NExpr::Call { args, .. } => match self.primitive_operator(value) {
                            Some(PrimitiveWrapperCallKind::Binary(BinOp::Comp(_))) => tracked(args),
                            Some(
                                PrimitiveWrapperCallKind::Binary(BinOp::Logical(_))
                                | PrimitiveWrapperCallKind::Unary(UnOp::Not),
                            ) => true,
                            Some(_) => false,
                            None => {
                                if let Some(observed) = self.observed_params(value) {
                                    pending.extend(args.iter().enumerate().filter_map(
                                        |(param, arg)| {
                                            observed
                                                .through_result
                                                .contains(&u32::try_from(param).unwrap())
                                                .then_some(arg.value)
                                        },
                                    ));
                                }
                                false
                            }
                        },
                        _ => true,
                    };
                    if relates {
                        expr.for_each_value_operand(|operand| pending.push(operand.value));
                    }
                }
                NValueDefinition::EntryParam { .. } => {}
            }
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

    /// The primitive operator a core wrapper method call defining `value` stands for.
    fn primitive_operator(&self, value: NValueId) -> Option<PrimitiveWrapperCallKind> {
        let (_, NExpr::Call { callee, .. }) = self.body.defining_expr(value)? else {
            return None;
        };
        let BodyOwner::Func(function) = callee.key.owner(self.db) else {
            return None;
        };
        core_primitive_wrapper_call_kind(self.db, function, self.body.values[value.index()].ty)
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

    /// Track scalar facts for new selectors and values, and everything they load.
    fn add_scalar_demand(
        &mut self,
        mut selectors: FxHashSet<NValueId>,
        values: impl IntoIterator<Item = NValueId>,
        cells: FxHashSet<NPlaceBase>,
    ) {
        let mut selector_cells = FxHashSet::default();
        close_scalar_values(self.db, &self.body, &mut selectors, &mut selector_cells);
        let selector_indices: Vec<_> = selectors.iter().map(|value| self.index(*value)).collect();
        self.scalar.selectors.extend(selector_indices);
        self.scalar
            .values
            .extend(selectors.into_iter().chain(values));
        self.scalar
            .cells
            .extend(cells.into_iter().chain(selector_cells));
        close_scalar_values(
            self.db,
            &self.body,
            &mut self.scalar.values,
            &mut self.scalar.cells,
        );
        self.scalar.indices = self
            .scalar
            .values
            .iter()
            .map(|value| self.index(*value))
            .collect();
        self.scalar.phis = self
            .scalar
            .values
            .iter()
            .copied()
            .filter(|value| self.compact_scalar_phi(*value))
            .collect();
        (self.scalar.live, self.scalar.observed) = self.scalar_liveness();
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
            } => self.comparison_guard(*operator, lhs.value, rhs.value, expected, include_bounds),
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
                        )
                    }
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
    ) -> Option<Guard<'db>> {
        let always = Guard::always(&BinderScope::default());
        let lhs_index = self.index(lhs);
        let rhs_index = self.index(rhs);
        if !self.scalar.indices.contains(&lhs_index) && !self.scalar.indices.contains(&rhs_index) {
            return Some(always);
        }
        let lhs_ty = copied_scalar_ty(self.db, self.body.values[lhs.index()].ty);
        let rhs_ty = copied_scalar_ty(self.db, self.body.values[rhs.index()].ty);
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

fn scalar_seeds<'db>(
    db: &dyn HirAnalysisDb,
    body: &NormalizedBody<'db>,
) -> (FxHashSet<NValueId>, FxHashSet<NPlaceBase>, Vec<NValueId>) {
    let mut selectors = FxHashSet::default();
    let mut cells = FxHashSet::default();
    let mut values = Vec::new();
    for block in &body.blocks {
        for statement in &block.statements {
            let mut add_indices = |path: &NDataPath| {
                selectors.extend(path.iter().filter_map(|projection| match projection {
                    NDataProjection::Index(NIndex::Value(value)) => Some(*value),
                    _ => None,
                }));
            };
            match &statement.kind {
                NStatementKind::Define { expr, .. } => {
                    expr.for_each_place_operand(|place| add_indices(&place.path));
                    if let NExpr::ProjectValue { path, .. } = expr {
                        add_indices(&path.0);
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
        if let NTerminatorKind::Return(Some(value)) = block.terminator.kind
            && body.values[value.value.index()].ty.is_integral(db)
        {
            values.push(value.value);
        }
    }
    // A branch around a failed assertion constrains every normal return, which
    // a summary exports over the integer parameters it compares.
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
                .map(|(index, _)| NValueId::new(index)),
        );
    }
    (selectors, cells, values)
}

fn close_scalar_values(
    db: &dyn HirAnalysisDb,
    body: &NormalizedBody<'_>,
    values: &mut FxHashSet<NValueId>,
    cells: &mut FxHashSet<NPlaceBase>,
) {
    loop {
        let previous = (values.len(), cells.len());
        for value in values.iter().copied().collect::<Vec<_>>() {
            match body.defining_expr(value) {
                Some((_, NExpr::Load { place, .. })) if place.path.is_empty() => {
                    cells.insert(place.base);
                }
                Some((_, NExpr::Forward { src })) => {
                    values.insert(src.value);
                }
                Some((_, NExpr::ScalarCast { value: src, to }))
                    if lossless_integer_cast(db, body.values[src.value.index()].ty, *to) =>
                {
                    values.insert(src.value);
                }
                _ => {}
            }
            if let NValueDefinition::BlockParam { block, index } =
                body.values[value.index()].definition
            {
                values.extend(
                    body.blocks
                        .iter()
                        .flat_map(|predecessor| predecessor.terminator.kind.successors())
                        .filter(|successor| successor.block == block)
                        .filter_map(|successor| successor.args.get(index as usize))
                        .map(|argument| argument.value),
                );
            }
        }
        for statement in body.blocks.iter().flat_map(|block| &block.statements) {
            if let NStatementKind::Define {
                result,
                expr: NExpr::Load { place, .. },
            } = &statement.kind
                && place.path.is_empty()
                && cells.contains(&place.base)
            {
                values.insert(*result);
            }
        }
        if previous == (values.len(), cells.len()) {
            break;
        }
    }
}
