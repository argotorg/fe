//! Definite-initialization checking over normalized semantic control flow.
//!
//! Normalization is the first stage that preserves evaluation order, exposes
//! control-flow joins, and has the final closure-capture operations. Runtime
//! lowering must therefore only receive bodies whose value reads are known to
//! follow a definition on every reachable path.

use std::collections::VecDeque;

use cranelift_entity::{EntityRef, SecondaryMap};
use dataflow::JoinSemiLattice;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    analysis::{
        semantic::{
            NBorrowRoot, NEffectArgValue, NExpr, NOperand, NSPlace, NSPlaceRoot, NSProjectionPath,
            NSStmtKind, NSTerminatorKind, SBlockId, SLocalId, SemOrigin, SemanticBorrowDiagKind,
            SemanticBorrowDiagnostic, SemanticBorrowDiagnosticSpan,
        },
        ty::adt_def::AdtRef,
    },
    projection::{IndexSource, Projection},
};

use super::{
    check::{Borrowck, IndexPhiSource},
    diagnostics::operand_origin,
    ir::{NormalizedSemanticBody, semantic_projection_ty},
};

#[derive(Clone, Default, PartialEq, Eq)]
struct InitializedPlaces<'db> {
    reached: bool,
    places: FxHashMap<SLocalId, FxHashSet<NSProjectionPath<'db>>>,
}

impl<'db> InitializedPlaces<'db> {
    fn mark(&mut self, local: SLocalId, path: NSProjectionPath<'db>) {
        let initialized = self.places.entry(local).or_default();
        if initialized
            .iter()
            .any(|existing| existing.is_prefix_of(&path))
        {
            return;
        }
        initialized.retain(|existing| !path.is_prefix_of(existing));
        initialized.insert(path);
    }

    fn contains_prefix(&self, local: SLocalId, path: &NSProjectionPath<'db>) -> bool {
        self.places.get(&local).is_some_and(|initialized| {
            initialized
                .iter()
                .any(|existing| existing.is_prefix_of(path))
        })
    }
}

impl<'db> JoinSemiLattice for InitializedPlaces<'db> {
    fn join_into(&mut self, other: &Self) -> bool {
        if !other.reached {
            return false;
        }
        if !self.reached {
            *self = other.clone();
            return true;
        }

        let mut intersection = FxHashMap::default();
        for (&local, lhs_paths) in &self.places {
            let Some(rhs_paths) = other.places.get(&local) else {
                continue;
            };
            let mut paths = FxHashSet::default();
            for lhs in lhs_paths {
                for rhs in rhs_paths {
                    if lhs.is_prefix_of(rhs) {
                        paths.insert(rhs.clone());
                    } else if rhs.is_prefix_of(lhs) {
                        paths.insert(lhs.clone());
                    }
                }
            }
            if !paths.is_empty() {
                intersection.insert(local, paths);
            }
        }
        if intersection == self.places {
            return false;
        }
        self.places = intersection;
        true
    }
}

struct DefiniteInitializationAnalysis<'a, 'db> {
    checker: &'a Borrowck<'db>,
}

impl<'a, 'db> DefiniteInitializationAnalysis<'a, 'db> {
    fn new(checker: &'a Borrowck<'db>) -> Self {
        Self { checker }
    }

    fn apply_definition(&self, state: &mut InitializedPlaces<'db>, stmt: &NSStmtKind<'db>) {
        match stmt {
            NSStmtKind::Assign { dst, .. } => {
                state.mark(*dst, NSProjectionPath::new());
            }
            NSStmtKind::Store { dst, .. } => {
                if let Some((local, path)) = stored_local_path(self.checker, dst) {
                    state.mark(local, path);
                }
            }
        }
    }
}

fn substitute_index_phis<'db>(
    checker: &Borrowck<'db>,
    predecessor: SBlockId,
    block: SBlockId,
    state: &InitializedPlaces<'db>,
) -> InitializedPlaces<'db> {
    let Some(substitutions) = checker.index_phi_substitutions(predecessor, block) else {
        return state.clone();
    };
    let mut out = InitializedPlaces {
        reached: state.reached,
        ..InitializedPlaces::default()
    };
    for (&local, paths) in &state.places {
        for path in paths {
            let mut expanded = vec![NSProjectionPath::new()];
            for projection in path.iter() {
                let alternatives = match projection {
                    Projection::Index(index) => {
                        let source = match index {
                            IndexSource::Constant(index) => IndexPhiSource::Constant(*index),
                            IndexSource::Dynamic(index) => IndexPhiSource::Dynamic(*index),
                        };
                        substitutions
                            .replacements
                            .get(&source)
                            .into_iter()
                            .flatten()
                            .copied()
                            .map(|index| Projection::Index(IndexSource::Dynamic(index)))
                            // A loop-phi result in an incoming state denotes
                            // the previous iteration's selected value. It
                            // cannot flow into the newly selected header value
                            // unless this edge maps that exact source back to
                            // the result. Acyclic phi results are normally
                            // absent before the join, so the same rule is inert
                            // for them.
                            .chain(
                                (!matches!(
                                    index,
                                    IndexSource::Dynamic(index)
                                        if substitutions.results.contains(index)
                                ))
                                .then(|| projection.clone()),
                            )
                            .collect::<Vec<_>>()
                    }
                    _ => vec![projection.clone()],
                };
                expanded = expanded
                    .into_iter()
                    .flat_map(|prefix| {
                        alternatives.iter().cloned().map(move |projection| {
                            let mut path = prefix.clone();
                            path.push(projection);
                            path
                        })
                    })
                    .collect();
            }
            for path in expanded {
                out.mark(local, path);
            }
        }
    }
    out
}

fn definite_initialization_entry_states<'db>(
    checker: &Borrowck<'db>,
) -> SecondaryMap<SBlockId, InitializedPlaces<'db>> {
    let mut entry_states = SecondaryMap::new();
    entry_states.resize(checker.body.blocks.len());
    if checker.body.blocks.is_empty() {
        return entry_states;
    }
    let entry = SBlockId::new(0);
    let mut initial = InitializedPlaces {
        reached: true,
        ..InitializedPlaces::default()
    };
    for local in checker.body.entry_locals.iter().copied() {
        initial.mark(local, NSProjectionPath::new());
    }
    entry_states[entry] = initial.clone();

    let analysis = DefiniteInitializationAnalysis::new(checker);
    let successors = checker.cfg_successor_indices();
    let mut edge_states = FxHashMap::<(SBlockId, SBlockId), InitializedPlaces<'db>>::default();
    let mut pending = VecDeque::from([entry]);
    let mut queued = vec![false; checker.body.blocks.len()];
    queued[entry.index()] = true;
    while let Some(block) = pending.pop_front() {
        queued[block.index()] = false;
        let mut exit = entry_states[block].clone();
        for stmt in &checker.body.blocks[block.index()].stmts {
            analysis.apply_definition(&mut exit, &stmt.kind);
        }
        for successor in successors[block].iter().copied() {
            if edge_states.get(&(block, successor)) == Some(&exit) {
                continue;
            }
            edge_states.insert((block, successor), exit.clone());
            let mut merged = InitializedPlaces::default();
            if successor == entry {
                merged.join_into(&initial);
            }
            for predecessor_idx in 0..checker.body.blocks.len() {
                let predecessor = SBlockId::new(predecessor_idx);
                if let Some(state) = edge_states.get(&(predecessor, successor)) {
                    let state = substitute_index_phis(checker, predecessor, successor, state);
                    merged.join_into(&state);
                }
            }
            if entry_states[successor] != merged {
                entry_states[successor] = merged;
                if !queued[successor.index()] {
                    pending.push_back(successor);
                    queued[successor.index()] = true;
                }
            }
        }
    }
    entry_states
}

pub(super) fn check_definite_initialization<'db>(
    checker: &Borrowck<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    if checker.body.blocks.is_empty() {
        return Ok(());
    }

    let entry_states = definite_initialization_entry_states(checker);
    for (block_idx, block) in checker.body.blocks.iter().enumerate() {
        let mut state = entry_states[SBlockId::new(block_idx)].clone();
        if !state.reached {
            continue;
        }

        for stmt in &block.stmts {
            check_stmt(checker, &state, stmt)?;
            match &stmt.kind {
                NSStmtKind::Assign { dst, .. } => {
                    state.mark(*dst, NSProjectionPath::new());
                }
                NSStmtKind::Store { dst, .. } => {
                    if let Some((local, path)) = stored_local_path(checker, dst) {
                        state.mark(local, path);
                    }
                }
            }
        }
        check_terminator(checker, &state, &block.terminator)?;
    }
    Ok(())
}

fn check_stmt<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    stmt: &super::ir::NSStmt<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    match &stmt.kind {
        NSStmtKind::Assign { expr, .. } => check_expr(checker, state, expr, stmt.origin),
        NSStmtKind::Store { dst, src } => {
            check_operand(checker, state, *src, stmt.origin)?;
            check_place_address(checker, state, dst, stmt.origin)
        }
    }
}

fn check_expr<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    expr: &NExpr<'db>,
    origin: SemOrigin<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    expr.try_for_each_value_operand(|operand| check_operand(checker, state, operand, origin))?;

    match expr {
        NExpr::ReadPlace { place, .. } => check_place_read(checker, state, place, origin),
        NExpr::Borrow { place, .. } => check_place_read(checker, state, place, origin),
        NExpr::Call { effect_args, .. } => {
            for effect_arg in effect_args {
                if let NEffectArgValue::Place(place) = &effect_arg.arg {
                    check_place_read(checker, state, place, origin)?;
                }
            }
            Ok(())
        }
        NExpr::Use(_)
        | NExpr::CodeRegionRef { .. }
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
        | NExpr::CodeRegionOffset { .. }
        | NExpr::CodeRegionLen { .. } => Ok(()),
    }
}

fn check_terminator<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    term: &super::ir::NSTerminator<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    match &term.kind {
        NSTerminatorKind::Branch { cond, .. }
        | NSTerminatorKind::MatchEnum { value: cond, .. }
        | NSTerminatorKind::Return(Some(cond)) => check_operand(checker, state, *cond, term.origin),
        NSTerminatorKind::Goto(_)
        | NSTerminatorKind::Assert { .. }
        | NSTerminatorKind::Return(None) => Ok(()),
    }
}

fn check_operand<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    operand: NOperand,
    fallback: SemOrigin<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    check_local_path(
        checker,
        state,
        operand.local,
        &NSProjectionPath::new(),
        operand_origin(operand, fallback),
    )
}

fn check_place_read<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    place: &NSPlace<'db>,
    origin: SemOrigin<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    if let Some((local, path)) = place_local_read_path(checker, place) {
        check_local_path(checker, state, local, &path, origin)?;
    }
    check_place_indices(checker, state, place, origin)
}

fn check_place_address<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    place: &NSPlace<'db>,
    origin: SemOrigin<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    if let NSPlaceRoot::CarrierDerefLocal(local) = place.root {
        check_local_path(checker, state, local, &NSProjectionPath::new(), origin)?;
    } else if let Some(path) = path_before_first_deref(&place.path)
        && let Some(local) = root_local(&checker.body, &place.root)
    {
        let path = canonical_initialization_path(checker, local, &path, PathUse::Read)
            .expect("read paths always have a conservative initialization form");
        check_local_path(checker, state, local, &path, origin)?;
    }
    check_place_indices(checker, state, place, origin)
}

fn check_place_indices<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    place: &NSPlace<'db>,
    origin: SemOrigin<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    for local in place.dynamic_index_locals() {
        check_local_path(checker, state, local, &NSProjectionPath::new(), origin)?;
    }
    Ok(())
}

fn check_local_path<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    local: SLocalId,
    path: &NSProjectionPath<'db>,
    origin: SemOrigin<'db>,
) -> Result<(), SemanticBorrowDiagnostic<'db>> {
    if path_is_initialized(checker, state, local, path) {
        return Ok(());
    }

    let name = checker
        .instance
        .key(checker.db)
        .owner(checker.db)
        .body(checker.db)
        .zip(checker.body.local(local).and_then(|local| local.source))
        .map(|(body, source)| source.pretty_name_in_body(checker.db, body))
        .unwrap_or_else(|| format!("%{}", local.index()));
    let mut diag = SemanticBorrowDiagnostic::new(
        checker.instance,
        SemanticBorrowDiagKind::UninitializedLocal,
        format!("local `{name}` may be used before it is initialized"),
        SemanticBorrowDiagnosticSpan::Origin {
            owner: checker.instance.key(checker.db).owner(checker.db),
            origin,
        },
    );
    diag.push_secondary(
        format!("`{name}` is declared without an initial value"),
        SemanticBorrowDiagnosticSpan::LocalSourceOrBody {
            instance: checker.instance,
            local,
        },
    );
    Err(diag)
}

fn path_is_initialized<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    local: SLocalId,
    path: &NSProjectionPath<'db>,
) -> bool {
    let mut candidate = path.clone();
    loop {
        if region_is_initialized(checker, state, local, &candidate) {
            return true;
        }
        let Some(parent) = candidate.parent() else {
            return false;
        };
        candidate = parent;
    }
}

fn region_is_initialized<'db>(
    checker: &Borrowck<'db>,
    state: &InitializedPlaces<'db>,
    local: SLocalId,
    path: &NSProjectionPath<'db>,
) -> bool {
    if state.contains_prefix(local, path) {
        return true;
    }
    let Some(local_data) = checker.body.local(local) else {
        return false;
    };
    let Some((mut ty, traverses_capability)) =
        semantic_projection_ty(checker.db, local_data.ty, path)
    else {
        return false;
    };
    if traverses_capability {
        return false;
    }
    while let Some((_, inner)) = ty.as_capability(checker.db) {
        ty = inner;
    }

    if ty.is_array(checker.db) {
        let Some(len) = ty.array_len(checker.db) else {
            return false;
        };
        // An empty array has no child writes from which to reconstruct a
        // whole-value definition. It is initialized only by an explicit
        // whole-value assignment, handled by `contains_prefix` above.
        if len == 0 {
            return false;
        }
        if len > state.places.get(&local).map_or(0, FxHashSet::len) {
            return false;
        }
        return (0..len).all(|index| {
            let mut child = path.clone();
            child.push(Projection::Index(IndexSource::Constant(index)));
            region_is_initialized(checker, state, local, &child)
        });
    }

    let is_struct = ty.is_tuple(checker.db)
        || ty
            .adt_def(checker.db)
            .is_some_and(|adt| matches!(adt.adt_ref(checker.db), AdtRef::Struct(_)));
    if !is_struct {
        return false;
    }
    let fields = ty.field_types(checker.db);
    // Like an empty array, an empty struct/tuple has no field definitions that
    // can establish initialization of the parent value.
    !fields.is_empty()
        && fields.into_iter().enumerate().all(|(field, _)| {
            let mut child = path.clone();
            child.push(Projection::Field(field));
            region_is_initialized(checker, state, local, &child)
        })
}

fn place_local_read_path<'db>(
    checker: &Borrowck<'db>,
    place: &NSPlace<'db>,
) -> Option<(SLocalId, NSProjectionPath<'db>)> {
    match place.root {
        NSPlaceRoot::CarrierDerefLocal(local) => Some((local, NSProjectionPath::new())),
        NSPlaceRoot::Root(_) => {
            let local = root_local(&checker.body, &place.root)?;
            let path = path_before_first_deref(&place.path).unwrap_or_else(|| place.path.clone());
            Some((
                local,
                canonical_initialization_path(checker, local, &path, PathUse::Read)
                    .expect("read paths always have a conservative initialization form"),
            ))
        }
    }
}

fn root_local(body: &NormalizedSemanticBody<'_>, root: &NSPlaceRoot) -> Option<SLocalId> {
    match *root {
        NSPlaceRoot::Root(root) => match body.root(root)? {
            NBorrowRoot::Param { local, .. } | NBorrowRoot::LocalSlot { local } => Some(*local),
            NBorrowRoot::Provider { .. } => None,
        },
        NSPlaceRoot::CarrierDerefLocal(local) => Some(local),
    }
}

fn path_before_first_deref<'db>(path: &NSProjectionPath<'db>) -> Option<NSProjectionPath<'db>> {
    let mut prefix = NSProjectionPath::new();
    for projection in path.iter() {
        if matches!(projection, Projection::Deref) {
            return Some(prefix);
        }
        prefix.push(projection.clone());
    }
    None
}

fn stored_local_path<'db>(
    checker: &Borrowck<'db>,
    place: &NSPlace<'db>,
) -> Option<(SLocalId, NSProjectionPath<'db>)> {
    let NSPlaceRoot::Root(_) = place.root else {
        return None;
    };
    let local = root_local(&checker.body, &place.root)?;
    Some((
        local,
        canonical_initialization_path(checker, local, &place.path, PathUse::Store)?,
    ))
}

#[derive(Clone, Copy)]
enum PathUse {
    Read,
    Store,
}

/// Canonicalizes dynamic indices for initialization facts without equating
/// unrelated runtime values.
///
/// A constant index is exact. An index into a singleton array is also exact on
/// every path that continues past the bounds check. Otherwise, repeated reads
/// of one immutable source place share a representative local. Mutable or
/// provider-backed sources stay conservative: reads retain their original
/// projection, while stores do not establish a reusable fact.
fn canonical_initialization_path<'db>(
    checker: &Borrowck<'db>,
    root_local: SLocalId,
    path: &NSProjectionPath<'db>,
    use_: PathUse,
) -> Option<NSProjectionPath<'db>> {
    let mut canonical = NSProjectionPath::new();
    for projection in path.iter() {
        let projection = match projection {
            Projection::Deref => return None,
            Projection::Index(IndexSource::Dynamic(index)) => {
                if let Some(index) = checker.constant_index(*index) {
                    Projection::Index(IndexSource::Constant(index))
                } else if projected_array_len(checker, root_local, &canonical) == Some(1) {
                    Projection::Index(IndexSource::Constant(0))
                } else if let Some(index) = stable_index_identity(checker, *index) {
                    Projection::Index(IndexSource::Dynamic(index))
                } else {
                    match use_ {
                        PathUse::Read => projection.clone(),
                        PathUse::Store => return None,
                    }
                }
            }
            projection => projection.clone(),
        };
        canonical.push(projection);
    }
    Some(canonical)
}

fn projected_array_len<'db>(
    checker: &Borrowck<'db>,
    root_local: SLocalId,
    path: &NSProjectionPath<'db>,
) -> Option<usize> {
    let local = checker.body.local(root_local)?;
    let (mut ty, _) = semantic_projection_ty(checker.db, local.layout_ty(), path)?;
    while let Some((_, inner)) = ty.as_capability(checker.db) {
        ty = inner;
    }
    ty.array_len(checker.db)
}

/// Returns the shared identity of snapshots whose source has the same
/// reaching value on every executable path.
fn stable_index_identity(checker: &Borrowck<'_>, local: SLocalId) -> Option<SLocalId> {
    checker.index_value_identity(local)
}
