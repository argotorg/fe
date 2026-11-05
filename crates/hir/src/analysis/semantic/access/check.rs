//! The access analysis of one normalized body.
//!
//! An access is open from the statement that creates it to the `end` that
//! liveness elaboration placed after its last use: a borrow or view of a
//! place, or a projection call's session. A session reserves the places its
//! arguments name and the domains its `uses` clause names, and grants one
//! access per yielded component. Every operation is checked against the
//! accesses open around it: overlapping accesses conflict unless both read,
//! except that an access through a carrier is authorized by the accesses the
//! carrier was derived from.

// Diagnostics and CTFE stops are returned on the slow path; salsa's 8-byte ids
// put them just past clippy's 128-byte `Err` threshold.
#![allow(clippy::result_large_err)]
use std::collections::BTreeSet;

use common::ingot::IngotKind;
use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::SmallVec;

use super::{
    DropElaboration, DropFlagPoint, DropFlagUpdate, DropPart, DropStep,
    domain::Domains,
    place::{AbsPlace, Base, Path, Step, path_of, paths_overlap},
    space::{PathSpace, PlaceStep},
};
use crate::{
    analysis::{
        HirAnalysisDb,
        semantic::{
            SemOrigin, SemanticInstance, SemanticInstanceKey, VariantIndex,
            access::{control::semantic_may_return, projection_result_spaces},
            definite_assignment::literal_index,
            diagnostics::{
                SemanticDiagnostic, SemanticDiagnosticKind, SemanticDiagnosticSpan, operand_origin,
            },
            effect_param_site,
            instance::row_expansion_for_key,
            normalized::{
                NBlockId, NDataPath, NDataProjection, NDropTarget, NEffectArg, NEffectArgValue,
                NExpr, NOperand, NPlace, NPlaceBase, NRootKind, NStatement, NStatementId,
                NStatementKind, NStructuralPath, NTerminatorKind, NValueDefinition, NValueId,
                NormalizedBody, ReadMode, access::AccessTarget,
            },
        },
        ty::{
            adt_def::AdtRef,
            corelib::{
                MemoryAccessKind, bounded_by_sealed_capabilities, effect_key_state_access,
                external_call_state_access, is_sealed_evm_capability,
            },
            drop_glue::{adt_drop_impl, implements_drop, ty_has_drop_glue},
            provider::{AddressSpace, provider_semantics},
            shape::Shape,
            trait_resolution::PredicateListId,
            ty_check::{BindingAccess, BodyOwner, LocalBinding, infer_body},
            ty_def::{BorrowKind, CapabilityKind, ShapeMode, TyData, TyId},
            ty_is_copy, ty_is_snapshot,
        },
    },
    hir_def::{Partial, Stmt, scope_graph::ScopeId},
    semantic::{EffectRequirementKey, ProviderSource, effect_requirements_for_site},
};

type Diag<'db> = SemanticDiagnostic<'db>;

/// The fix for a plain binding that moves a value out of an access it could
/// use in place.
const IN_PLACE_FIX: &str = "; bind it with `mut` or `ref` to use it in place";
pub(super) type TokenId = u32;
type TokenSet = SmallVec<TokenId, 4>;

/// The tokens a value holds, each at the path of the value where its carrier
/// sits: a projection's tuple or sum shape holds one grant per component.
pub(super) type Held = BTreeSet<(TokenId, Path)>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum TokenKind {
    /// A data parameter, open for the whole body.
    Input,
    /// A borrow or view this body opened.
    Access,
    /// A session's reservation of an argument or effect domain.
    Reservation,
    /// A component a session grants.
    Grant,
    /// A provider handle: names a domain, confers no exclusivity.
    Handle,
}

#[derive(Clone, Debug)]
pub(super) struct Token<'db> {
    kind: TokenKind,
    mode: BorrowKind,
    pub regions: Vec<AbsPlace>,
    /// The tokens this one was derived through, which authorize it.
    parents: TokenSet,
    origin: SemOrigin<'db>,
    /// An access of a zero-sized place protects no data.
    zero_sized: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum MoveKey {
    Value(NValueId),
    Root(u32),
    Param(u32),
    /// The referent of a `mut` access, which may hold a hole while the
    /// access is open.
    Access(TokenId),
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct State {
    /// Accesses, reservations and grants opened and not yet ended.
    open: BTreeSet<TokenId>,
    /// Possibly moved (or uninitialized) values, roots and parameter paths.
    moved: BTreeSet<(MoveKey, Path)>,
    /// The entries of `moved` moved on every path, which a drop skips.
    definitely_moved: BTreeSet<(MoveKey, Path)>,
}

impl State {
    fn join(&mut self, other: &Self) -> bool {
        let before = (
            self.open.len(),
            self.moved.len(),
            self.definitely_moved.len(),
        );
        self.open.extend(other.open.iter().copied());
        self.moved.extend(other.moved.iter().cloned());
        self.definitely_moved
            .retain(|entry| other.definitely_moved.contains(entry));
        before
            != (
                self.open.len(),
                self.moved.len(),
                self.definitely_moved.len(),
            )
    }

    fn record_move(&mut self, entry: (MoveKey, Path)) {
        self.definitely_moved.insert(entry.clone());
        self.moved.insert(entry);
    }

    /// Initializes the entries `initialized` selects.
    fn initialize(&mut self, initialized: impl Fn(&(MoveKey, Path)) -> bool) {
        self.moved.retain(|entry| !initialized(entry));
        self.definitely_moved.retain(|entry| !initialized(entry));
    }
}

/// The places an effect of a call reaches, with the mode it takes and the
/// tokens of the carrier its place is reached through.
struct Footprint {
    regions: Vec<AbsPlace>,
    mode: BorrowKind,
    parents: TokenSet,
}

/// Where an access through a place goes, and which tokens authorize it.
pub(super) struct Resolved {
    pub regions: Vec<AbsPlace>,
    /// The tokens of the carrier the place is reached through.
    direct: TokenSet,
    authority: TokenSet,
}

/// The kinds of a statement's token creation sites, for stable identities
/// across fixed-point iterations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Site {
    Open,
    Handle,
    Effect(usize),
    Arg(usize),
    Grant(usize),
}

pub(super) struct Analysis<'a, 'db> {
    pub db: &'db dyn HirAnalysisDb,
    pub instance: SemanticInstance<'db>,
    pub body: &'a NormalizedBody<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    param_count: u32,
    pub domains: Domains<'db>,
    /// The domain of each provider root.
    root_domain: Vec<Option<u32>>,
    pub tokens: Vec<Token<'db>>,
    pub values: Vec<Held>,
    /// Tokens each root's contents may hold.
    contents: Vec<Held>,
    sites: FxHashMap<(NValueId, Site), TokenId>,
    /// The tokens the statement defining a value opens, which its `end`
    /// closes.
    opened: FxHashMap<NValueId, TokenSet>,
    /// The values whose accesses an `end` closes. Normalization opens
    /// others for a single call, which closes them.
    ended: FxHashSet<NValueId>,
    /// The values a call takes as arguments, and those they forward.
    call_args: FxHashSet<NValueId>,
    /// The projection each session calls.
    sessions: FxHashMap<NValueId, SemanticInstance<'db>>,
    /// Each unsafe split component's split and position in it.
    split_of: FxHashMap<NValueId, (usize, usize)>,
    entry: Vec<Option<State>>,
    moved_at: FxHashMap<(MoveKey, Path), SemOrigin<'db>>,
    /// The statement at which each block diverges into a call that never returns.
    divergence: Vec<Option<usize>>,
    /// Whether transfer functions report violations.
    checking: bool,
    /// The parts each drop destroys, as the check finds them.
    drop_parts: Vec<(NStatementId, Vec<DropPart>)>,
    /// The paths whose initialization a flag tracks, by flag.
    drop_flags: Vec<(MoveKey, Path)>,
}

impl<'a, 'db> Analysis<'a, 'db> {
    /// `divergence` cuts each block at a call that never returns. Provisional
    /// analyses run while callee instances are still being built, so they
    /// keep every path.
    pub fn new(
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
        body: &'a NormalizedBody<'db>,
        divergence: bool,
    ) -> Self {
        let typed_body = instance.key(db).typed_body(db);
        let param_count = typed_body.input_bindings().len() as u32;
        let mut analysis = Self {
            db,
            instance,
            body,
            scope: instance.key(db).impl_env(db).normalization_scope(db),
            assumptions: instance.assumptions(db),
            param_count,
            domains: Domains::default(),
            root_domain: vec![None; body.roots.len()],
            tokens: Vec::new(),
            values: vec![Held::new(); body.values.len()],
            contents: vec![Held::new(); body.roots.len()],
            sites: FxHashMap::default(),
            opened: FxHashMap::default(),
            ended: body
                .blocks
                .iter()
                .flat_map(|block| &block.statements)
                .filter_map(|statement| match statement.kind {
                    NStatementKind::End { access } => Some(access),
                    _ => None,
                })
                .collect(),
            call_args: call_args(body),
            sessions: FxHashMap::default(),
            split_of: body
                .unsafe_splits
                .iter()
                .enumerate()
                .flat_map(|(split, components)| {
                    components
                        .iter()
                        .enumerate()
                        .map(move |(position, &component)| (component, (split, position)))
                })
                .collect(),
            entry: vec![None; body.blocks.len()],
            moved_at: FxHashMap::default(),
            divergence: vec![None; body.blocks.len()],
            checking: false,
            drop_parts: Vec::new(),
            drop_flags: Vec::new(),
        };
        if divergence {
            for (block, data) in body.blocks.iter().enumerate() {
                analysis.divergence[block] =
                    data.statements
                        .iter()
                        .position(|statement| match &statement.kind {
                            NStatementKind::Define {
                                expr: NExpr::Call { callee, .. },
                                ..
                            } => !semantic_may_return(db, SemanticInstance::new(db, callee.key)),
                            _ => false,
                        });
            }
        }
        for (index, root) in body.roots.iter().enumerate() {
            if let NRootKind::Provider { binding } = &root.kind {
                analysis.root_domain[index] = Some(analysis.domains.provider(binding));
            }
        }
        analysis.seed_entry();
        analysis
    }

    fn token(&mut self, value: NValueId, site: Site, token: Token<'db>) -> TokenId {
        if let Some(&id) = self.sites.get(&(value, site)) {
            return id;
        }
        self.tokens.push(token);
        let id = (self.tokens.len() - 1) as TokenId;
        self.sites.insert((value, site), id);
        if matches!(
            self.tokens[id as usize].kind,
            TokenKind::Access | TokenKind::Reservation | TokenKind::Grant
        ) {
            self.opened.entry(value).or_default().push(id);
        }
        id
    }

    fn new_token(&self, kind: TokenKind, mode: BorrowKind, origin: SemOrigin<'db>) -> Token<'db> {
        Token {
            kind,
            mode,
            regions: Vec::new(),
            parents: TokenSet::new(),
            origin,
            zero_sized: false,
        }
    }

    /// The address space of data parameter `param`'s place in this
    /// instantiation.
    fn param_space(&self, param: u32) -> AddressSpace {
        self.instance.param_space(self.db, param)
    }

    /// Whether a place of `ty` holds nothing: a zero-sized type, or a sealed
    /// std EVM capability, whose only implementor is zero-sized.
    fn holds_nothing(&self, ty: TyId<'db>) -> bool {
        ty.is_zero_sized(self.db)
            || is_sealed_evm_capability(self.db, self.scope, self.assumptions, ty)
    }

    fn seed_entry(&mut self) {
        let body = self.body;
        for (index, value) in body.values.iter().enumerate() {
            let NValueDefinition::EntryParam { param } = value.definition else {
                continue;
            };
            let value_id = NValueId::new(index);
            let origin = SemOrigin::Body(body.template_owner);
            let held = if let Some((kind, target)) = value.ty.as_capability(self.db)
                && param < self.param_count
            {
                let mode = match kind {
                    CapabilityKind::Mut => BorrowKind::Mut,
                    CapabilityKind::Ref | CapabilityKind::View => BorrowKind::Ref,
                };
                let mut input = self.new_token(TokenKind::Input, mode, origin);
                input.regions.push(AbsPlace::new(Base::Param(param)));
                input.zero_sized = self.holds_nothing(target);
                self.token(value_id, Site::Open, input)
            } else {
                // An effect handle names its provider's domain.
                let domain = match value.source {
                    Some(LocalBinding::EffectParam { site, idx, .. }) => {
                        Some(self.domains.provider_source(
                            ProviderSource::UsesParam {
                                site: site.slot_site(self.db, idx as u32),
                                requirement_idx: idx as u32,
                            },
                            provider_semantics(self.db, value.ty).address_space,
                        ))
                    }
                    _ => None,
                };
                let Some(domain) = domain else {
                    continue;
                };
                let mut handle = self.new_token(TokenKind::Handle, BorrowKind::Mut, origin);
                handle.regions.push(AbsPlace::new(Base::Domain(domain)));
                self.token(value_id, Site::Handle, handle)
            };
            self.values[index] = Held::from([(held, Path::new())]);
        }
        let mut state = State::default();
        // Entry bindings, such as recv arm fields, initialize their slots. The
        // provisional body has the same entry locals and does not depend on
        // the call-site refinements this analysis may be computing.
        let raw = self.instance.provisional_body(self.db);
        let entry_bindings: Vec<_> = raw
            .entry_locals
            .iter()
            .filter_map(|local| raw.locals[local.index()].source)
            .collect();
        for (index, root) in body.roots.iter().enumerate() {
            if let NRootKind::LocalSlot { binding } = &root.kind
                && !binding.is_some_and(|binding| entry_bindings.contains(&binding))
            {
                state.record_move((MoveKey::Root(index as u32), Path::new()));
            }
        }
        self.entry[body.entry.index()] = Some(state);
    }

    /// The steps of a normalized path, resolving literal index values.
    fn path(&self, path: &NDataPath) -> Path {
        path_of(path, |value| literal_index(self.db, self.body, value))
    }

    /// `tokens` and every token they were derived from.
    fn ancestors(&self, tokens: &[TokenId]) -> TokenSet {
        let mut closure: TokenSet = tokens.iter().copied().collect();
        let mut pending = closure.to_vec();
        while let Some(token) = pending.pop() {
            for &parent in &self.tokens[token as usize].parents {
                if !closure.contains(&parent) {
                    closure.push(parent);
                    pending.push(parent);
                }
            }
        }
        closure.sort_unstable();
        closure
    }

    /// The tokens a value's carrier holds at its own position.
    fn direct(&self, value: NValueId) -> TokenSet {
        self.values[value.index()]
            .iter()
            .filter(|(_, rest)| rest.is_empty())
            .map(|(token, _)| *token)
            .collect()
    }

    /// The places a carrier argument names.
    pub fn passed_regions(&self, value: NValueId) -> Vec<AbsPlace> {
        self.passed(value)
            .iter()
            .flat_map(|token| self.tokens[*token as usize].regions.clone())
            .collect()
    }

    /// The accesses a carrier argument for an input taken by `access` passes.
    /// A handle opens none, and a snapshot view argument is passed by copy.
    fn passed_to(&self, value: NValueId, access: Option<BindingAccess>) -> TokenSet {
        let ty = self.body.values[value.index()].ty;
        let ty = ty.as_capability(self.db).map_or(ty, |(_, target)| target);
        if access == Some(BindingAccess::View)
            && ty_is_snapshot(self.db, self.scope, ty, self.assumptions)
        {
            return TokenSet::new();
        }
        self.passed(value)
    }

    /// The accesses a carrier argument passes: passing a handle opens none.
    fn passed(&self, value: NValueId) -> TokenSet {
        self.direct(value)
            .into_iter()
            .filter(|token| self.tokens[*token as usize].kind != TokenKind::Handle)
            .collect()
    }

    /// Where the path of `place` takes it from `start`, the space of the
    /// place the path is relative to.
    fn path_space(&self, place: &NPlace<'db>, start: Option<AddressSpace>) -> PathSpace {
        let db = self.db;
        let ty_at = |len| {
            self.body
                .place_prefix_ty(db, place, len)
                .map(|ty| self.instance.normalized_ty(db, ty))
        };
        let Some(root) = ty_at(0) else {
            return PathSpace {
                moved: None,
                lane: false,
            };
        };
        let mut walk = PathSpace::new(db, root);
        let mut container = root;
        for (index, projection) in place.path.as_slice().iter().enumerate() {
            let Some(result) = ty_at(index + 1) else {
                break;
            };
            let step = match projection {
                NDataProjection::Field(field) => PlaceStep::Field(field.0 as usize),
                NDataProjection::VariantField { .. } => PlaceStep::Variant,
                NDataProjection::Index(_) => PlaceStep::Index,
                NDataProjection::Entry(_) => PlaceStep::Entry,
            };
            walk.step(db, self.instance, start, (container, step, result));
            container = result;
        }
        walk
    }

    pub fn resolve(&mut self, place: &NPlace<'db>) -> Resolved {
        let path = self.path(&place.path);
        let rooted = |this: &Self, base: Base, path: Path| {
            let PathSpace { moved, lane } = this.path_space(place, this.space(base));
            AbsPlace {
                base,
                path,
                space: moved,
                lane,
            }
        };
        let (regions, direct) = match place.base {
            NPlaceBase::Root(root) => {
                let base = self.root_domain[root.index()].map_or(Base::Root(root), Base::Domain);
                (vec![rooted(self, base, path)], TokenSet::new())
            }
            // A carrier with no access names no known place: none until the
            // flow gives it one (`check_places_known`).
            NPlaceBase::CapabilityTarget { carrier } => {
                let direct = self.direct(carrier);
                let mut regions: Vec<AbsPlace> = direct
                    .iter()
                    .flat_map(|token| &self.tokens[*token as usize].regions)
                    .map(|region| {
                        let PathSpace { moved, lane } =
                            self.path_space(place, self.grant_space(region));
                        region.extended(&path, moved, lane)
                    })
                    .collect();
                regions.sort();
                regions.dedup();
                (regions, direct)
            }
            // A pointer holds no access. A memory pointee is raw memory; a
            // state pointee may be any place of its space.
            NPlaceBase::Deref { pointer } => {
                let base =
                    self.body.values[pointer.index()]
                        .ty
                        .as_ptr(self.db)
                        .map(|(_, space)| match space {
                            AddressSpace::Memory => Base::Raw,
                            space => Base::Domain(self.domains.dynamic(Some(space))),
                        });
                (
                    base.map(|base| rooted(self, base, path))
                        .into_iter()
                        .collect(),
                    TokenSet::new(),
                )
            }
        };
        Resolved {
            regions,
            authority: self.ancestors(&direct),
            direct,
        }
    }

    /// The space `region` lies in: its base's, unless a pinned type or an
    /// entry along its path moved it.
    pub fn region_space(&self, region: &AbsPlace) -> Option<AddressSpace> {
        region.space.or_else(|| self.space(region.base))
    }

    /// The space a retained grant to `region` lies in: memory for a lane,
    /// whose grant is a memory copy, else the region's own.
    pub fn grant_space(&self, region: &AbsPlace) -> Option<AddressSpace> {
        if region.lane {
            Some(AddressSpace::Memory)
        } else {
            self.region_space(region)
        }
    }

    pub fn space(&self, base: Base) -> Option<AddressSpace> {
        match base {
            Base::Root(root) => Some(self.body.roots[root.index()].address_space),
            Base::Param(param) => Some(self.param_space(param)),
            Base::Domain(domain) => self.domains.space(domain),
            Base::State(space) => Some(space),
            // A grant lies in the space its projection's contract exports.
            Base::Grant { session, component } => {
                projection_result_spaces(self.db, *self.sessions.get(&session)?)
                    .get(component as usize)
                    .copied()
                    .flatten()
            }
            Base::Raw => Some(AddressSpace::Memory),
        }
    }

    /// Whether two abstract places may share storage. Raw memory is
    /// unchecked; a grant is covered by its session's reservations. Places
    /// in different spaces, each that of its root unless a pinned type or an
    /// entry along its path moved it, are disjoint.
    fn overlaps(&self, lhs: &AbsPlace, rhs: &AbsPlace) -> bool {
        let same_space = || match (self.region_space(lhs), self.region_space(rhs)) {
            (Some(lhs), Some(rhs)) => lhs == rhs,
            _ => true,
        };
        match (lhs.base, rhs.base) {
            (Base::Raw, _)
            | (_, Base::Raw)
            | (Base::State(_), Base::Grant { .. })
            | (Base::Grant { .. }, Base::State(_)) => false,
            (Base::State(_), _) | (_, Base::State(_)) => same_space(),
            (Base::Domain(lhs_domain), Base::Domain(rhs_domain)) if lhs_domain != rhs_domain => {
                self.domains.may_alias(lhs_domain, rhs_domain) && same_space()
            }
            (lhs_base, rhs_base) => lhs_base == rhs_base && paths_overlap(&lhs.path, &rhs.path),
        }
    }

    fn any_overlap(&self, lhs: &[AbsPlace], rhs: &[AbsPlace]) -> bool {
        lhs.iter()
            .any(|lhs| rhs.iter().any(|rhs| self.overlaps(lhs, rhs)))
    }

    // --- value flow -------------------------------------------------------

    fn union(&mut self, value: NValueId, held: Held, changed: &mut bool) {
        let target = &mut self.values[value.index()];
        let before = target.len();
        target.extend(held);
        *changed |= target.len() != before;
    }

    /// What a value holds below `prefix`.
    fn prefixed(held: &Held, prefix: Step) -> Held {
        held.iter()
            .map(|(token, rest)| {
                let path = [prefix].into_iter().chain(rest.iter().copied()).collect();
                (*token, path)
            })
            .collect()
    }

    /// What the part of a value at `path` holds.
    fn projected(held: &Held, path: &[Step]) -> Held {
        held.iter()
            .filter(|(_, rest)| paths_overlap(rest, path))
            .map(|(token, rest)| {
                let rest = rest.get(path.len()..).unwrap_or_default();
                (*token, rest.iter().copied().collect())
            })
            .collect()
    }

    fn root_contents(&self, root: usize) -> Held {
        let mut held = self.contents[root].clone();
        if let NRootKind::CapabilityRepresentation { carrier: value }
        | NRootKind::Temporary { value } = self.body.roots[root].kind
        {
            held.extend(self.values[value.index()].iter().cloned());
        }
        held
    }

    /// Run the flow-insensitive fixed point over the tokens values and root
    /// contents hold, then the forward fixed point over open accesses and
    /// moves.
    pub fn solve(&mut self) {
        loop {
            let before: Vec<_> = self
                .tokens
                .iter()
                .map(|token| token.regions.clone())
                .collect();
            let mut changed = false;
            for block in 0..self.body.blocks.len() {
                let block = NBlockId::new(block);
                let statements: Vec<_> = self.statements(block).collect();
                for (_, statement) in statements {
                    self.flow(statement, &mut changed);
                }
                self.flow_terminator(block, &mut changed);
            }
            let regions_changed = self.tokens.len() != before.len()
                || self
                    .tokens
                    .iter()
                    .zip(&before)
                    .any(|(token, before)| token.regions != *before);
            if !changed && !regions_changed {
                break;
            }
        }
        let order = self.reverse_postorder();
        loop {
            let mut changed = false;
            for &block in &order {
                if let Some(state) = self.run_block(block) {
                    changed |= self.propagate(block, &state);
                }
            }
            if !changed {
                break;
            }
        }
    }

    fn flow(&mut self, statement: &NStatement<'db>, changed: &mut bool) {
        match &statement.kind {
            NStatementKind::End { .. } | NStatementKind::Drop { .. } => {}
            NStatementKind::Store { destination, value } => {
                let held = self.values[value.value.index()].clone();
                if held.is_empty() {
                    return;
                }
                let path = self.path(&destination.path);
                for region in self.resolve(destination).regions {
                    if let Base::Root(root) = region.base {
                        let contents = &mut self.contents[root.index()];
                        let before = contents.len();
                        contents.extend(held.iter().map(|(token, rest)| {
                            let mut full = path.clone();
                            full.extend_from_slice(rest);
                            (*token, full)
                        }));
                        *changed |= contents.len() != before;
                    }
                }
            }
            NStatementKind::Define { result, expr } => {
                let held = self.define(*result, statement, expr);
                self.union(*result, held, changed);
            }
        }
    }

    fn flow_terminator(&mut self, block: NBlockId, changed: &mut bool) {
        if self.diverges(block) {
            return;
        }
        for successor in self.body.blocks[block.index()].terminator.kind.successors() {
            let target = &self.body.blocks[successor.block.index()];
            for (param, arg) in target.params.iter().zip(&successor.args) {
                let held = self.values[arg.value.index()].clone();
                self.union(*param, held, changed);
            }
        }
    }

    /// The tokens a defining statement's result holds, creating the tokens
    /// it opens.
    fn define(&mut self, result: NValueId, statement: &NStatement<'db>, expr: &NExpr<'db>) -> Held {
        let origin = statement.origin;
        let of = |this: &Self, operand: &NOperand| this.values[operand.value.index()].clone();
        match expr {
            NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. } => of(self, src),
            NExpr::ProjectValue { value, path } => {
                Self::projected(&of(self, value), &self.path(&path.0))
            }
            NExpr::ArrayRepeat { value, .. } => Self::prefixed(&of(self, value), Step::Index(None)),
            NExpr::AggregateMake { fields, .. } => fields
                .iter()
                .enumerate()
                .flat_map(|(field, operand)| {
                    Self::prefixed(&of(self, operand), Step::Field(field as u16))
                })
                .collect(),
            NExpr::EnumMake {
                variant, fields, ..
            } => fields
                .iter()
                .enumerate()
                .flat_map(|(field, operand)| {
                    Self::prefixed(
                        &of(self, operand),
                        Step::Variant {
                            variant: variant.0,
                            field: field as u16,
                        },
                    )
                })
                .collect(),
            NExpr::Load { place, .. } => {
                let mut loaded = Held::new();
                for region in self.resolve(place).regions {
                    if let Base::Root(root) = region.base {
                        loaded.extend(Self::projected(
                            &self.root_contents(root.index()),
                            &region.path,
                        ));
                    }
                }
                loaded
            }
            // A borrow no `end` closes is not an access of its own: through
            // a carrier it names part of the carrier's access, and otherwise
            // it is normalization's view of a call argument, open for that
            // call. A call argument reached through a carrier is opened for
            // the call too, derived from the carrier, so a call's arguments
            // and receiver are checked against each other whatever their
            // places are reached through. A snapshot argument is passed by
            // copy: its read is the whole access.
            NExpr::Borrow { place, .. } | NExpr::MakeView { place, .. }
                if !self.ended.contains(&result) =>
            {
                let snapshot = ty_is_snapshot(self.db, self.scope, place.ty, self.assumptions);
                match place.base {
                    NPlaceBase::Deref { .. } => Held::new(),
                    NPlaceBase::CapabilityTarget { carrier }
                        if !self.call_args.contains(&result)
                            || snapshot && matches!(expr, NExpr::Borrow { .. }) =>
                    {
                        self.direct(carrier)
                            .into_iter()
                            .map(|token| (token, Path::new()))
                            .collect()
                    }
                    _ if snapshot => Held::new(),
                    NPlaceBase::Root(_) | NPlaceBase::CapabilityTarget { .. } => {
                        let mode = match expr {
                            NExpr::Borrow { kind, .. } => *kind,
                            _ => BorrowKind::Ref,
                        };
                        self.open_access(result, origin, place, mode)
                    }
                }
            }
            NExpr::Borrow { place, kind, .. } => self.open_access(result, origin, place, *kind),
            NExpr::MakeView { place, .. } => {
                self.open_access(result, origin, place, BorrowKind::Ref)
            }
            NExpr::Call {
                callee,
                args,
                effect_args,
                ..
            } => {
                if let BodyOwner::Func(func, mode) = callee.key.owner(self.db)
                    && let Some(shape) = func.return_shape(self.db, mode)
                {
                    self.sessions
                        .insert(result, SemanticInstance::new(self.db, callee.key));
                    return self.open_session(result, origin, callee.key, shape, args, effect_args);
                }
                Held::new()
            }
            NExpr::CodeRegionRef { .. }
            | NExpr::Const(_)
            | NExpr::Unary { .. }
            | NExpr::Binary { .. }
            | NExpr::PointerCast { .. }
            | NExpr::ScalarCast { .. }
            | NExpr::GetEnumTag { .. }
            | NExpr::IsEnumVariant { .. }
            | NExpr::CodeRegionOffset { .. }
            | NExpr::CodeRegionLen { .. } => Held::new(),
        }
    }

    fn open_access(
        &mut self,
        result: NValueId,
        origin: SemOrigin<'db>,
        place: &NPlace<'db>,
        mode: BorrowKind,
    ) -> Held {
        let resolved = self.resolve(place);
        let mut access = self.new_token(TokenKind::Access, mode, origin);
        access.zero_sized = self.holds_nothing(place.ty);
        let id = self.token(result, Site::Open, access);
        let token = &mut self.tokens[id as usize];
        token.regions = resolved.regions;
        token.parents = resolved.direct;
        Held::from([(id, Path::new())])
    }

    /// Opens a projection call's session: a reservation per argument
    /// carrier and per effect footprint, and a grant per access component,
    /// derived from all reservations.
    fn open_session(
        &mut self,
        result: NValueId,
        origin: SemOrigin<'db>,
        callee: SemanticInstanceKey<'db>,
        shape: &Shape<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
    ) -> Held {
        let BodyOwner::Func(..) = callee.owner(self.db) else {
            return Held::new();
        };
        let mut reservations = TokenSet::new();
        for (position, arg) in args.iter().enumerate() {
            let access = callee_input_access(self.db, callee, position);
            let direct = self.passed_to(arg.value, access);
            if direct.is_empty() {
                continue;
            }
            let mode = match access {
                Some(BindingAccess::Mut) => BorrowKind::Mut,
                Some(BindingAccess::View | BindingAccess::Ref) | None => BorrowKind::Ref,
            };
            let regions: Vec<AbsPlace> = direct
                .iter()
                .flat_map(|token| self.tokens[*token as usize].regions.clone())
                .collect();
            let zero_sized = direct
                .iter()
                .all(|token| self.tokens[*token as usize].zero_sized);
            let id = self.token(
                result,
                Site::Arg(position),
                self.new_token(TokenKind::Reservation, mode, origin),
            );
            let token = &mut self.tokens[id as usize];
            token.regions = regions;
            token.parents = direct;
            token.zero_sized = zero_sized;
            reservations.push(id);
        }
        let mut footprints: Vec<_> = args
            .iter()
            .enumerate()
            .filter_map(|(position, arg)| self.capability_arg_footprint(callee, position, arg))
            .collect();
        for effect in effect_args {
            footprints.extend(self.effect_footprint(callee, effect));
        }
        for (position, footprint) in footprints.into_iter().enumerate() {
            let id = self.token(
                result,
                Site::Effect(position),
                self.new_token(TokenKind::Reservation, footprint.mode, origin),
            );
            let token = &mut self.tokens[id as usize];
            token.regions = footprint.regions;
            token.parents = footprint.parents;
            reservations.push(id);
        }
        let mut held = Held::new();
        for (component, (path, mode, _)) in
            access_components(self.db, shape).into_iter().enumerate()
        {
            let mut grant = self.new_token(TokenKind::Grant, mode, origin);
            grant.regions.push(AbsPlace::new(Base::Grant {
                session: result,
                component: component as u16,
            }));
            grant.parents = reservations.clone();
            held.insert((self.token(result, Site::Grant(component), grant), path));
        }
        // An unsafe split is one session: each component's grants derive
        // from every component's reservations.
        let siblings = self.split_siblings(result);
        let of_kind = |this: &Self, tokens: &[TokenId], kind| -> TokenSet {
            tokens
                .iter()
                .copied()
                .filter(|token| this.tokens[*token as usize].kind == kind)
                .collect()
        };
        let sibling_reservations = of_kind(self, &siblings, TokenKind::Reservation);
        let sibling_grants = of_kind(self, &siblings, TokenKind::Grant);
        for (grant, _) in held.iter() {
            add_parents(
                &mut self.tokens[*grant as usize].parents,
                &sibling_reservations,
            );
        }
        for grant in sibling_grants {
            add_parents(&mut self.tokens[grant as usize].parents, &reservations);
        }
        held
    }

    /// The tokens the components of `value`'s unsafe split before it opened:
    /// they and `value` are one session, so never conflict.
    fn split_siblings(&self, value: NValueId) -> TokenSet {
        let Some(&(split, position)) = self.split_of.get(&value) else {
            return TokenSet::new();
        };
        self.body.unsafe_splits[split][..position]
            .iter()
            .flat_map(|component| self.opened.get(component).into_iter().flatten().copied())
            .collect()
    }

    /// Every persistent and transient slot, when the data argument at
    /// `position` of a call of `callee` is a reentrant or raw storage
    /// capability: the callee may start an external execution or address
    /// slots through it, as through an effect. Read through a view. The
    /// methods `core` and `std` define on the capabilities say what they
    /// reach themselves (`external_call_state_access`), so `ctx.caller()`
    /// reaches nothing.
    fn capability_arg_footprint(
        &self,
        callee: SemanticInstanceKey<'db>,
        position: usize,
        arg: &NOperand,
    ) -> Option<Footprint> {
        let owner = callee.owner(self.db);
        if let BodyOwner::Func(func, _) = owner
            && matches!(
                func.top_mod(self.db).ingot(self.db).kind(self.db),
                IngotKind::Core | IngotKind::Std
            )
        {
            return None;
        }
        // The declared type, before the callee's generic arguments. A
        // parameter a spread lists declares none.
        let typed_body = &infer_body(self.db, owner).1;
        let (input, declared) = match typed_body.input_bindings().get(position) {
            Some(&input) => (
                input,
                matches!(owner, BodyOwner::Func(..)).then(|| typed_body.binding_ty(self.db, input)),
            ),
            None => (
                *callee.typed_body(self.db).input_bindings().get(position)?,
                None,
            ),
        };
        let ty = self.body.values[arg.value.index()].ty;
        let ty = ty.as_capability(self.db).map_or(ty, |(_, inner)| inner);
        // A view or a `ref` part only reads; an owned or `mut` input may write.
        let reads = matches!(
            typed_body.binding_access(input),
            Some(BindingAccess::View | BindingAccess::Ref)
        );
        let access = self.capability_state_access(owner, declared, ty, !reads)?;
        Some(Footprint {
            regions: state_regions(),
            mode: access.borrow_kind(),
            parents: TokenSet::new(),
        })
    }

    /// The state a capability of type `ty`, passed to `owner` where it
    /// declares `declared`, lets the call reach. A parameter whose bounds
    /// are all sealed std capability traits reaches only what they give the
    /// callee, which is checked generically, so `C: Ctx` reaches nothing
    /// even for an `Evm` argument; any other parameter reaches what the
    /// argument's own type does, since an unsealed bound may be implemented
    /// or routed to reach it.
    fn capability_state_access(
        &self,
        owner: BodyOwner<'db>,
        declared: Option<TyId<'db>>,
        ty: TyId<'db>,
        is_mut: bool,
    ) -> Option<MemoryAccessKind> {
        if let Some(declared) = declared
            && let BodyOwner::Func(_, _) = owner
        {
            let assumptions = infer_body(self.db, owner).1.assumptions();
            if bounded_by_sealed_capabilities(self.db, owner.scope(), assumptions, declared) {
                return effect_key_state_access(
                    self.db,
                    owner.scope(),
                    assumptions,
                    EffectRequirementKey::Type(declared),
                    is_mut,
                );
            }
        }
        effect_key_state_access(
            self.db,
            self.scope,
            self.assumptions,
            EffectRequirementKey::Type(ty),
            is_mut,
        )
    }

    /// The domains an effect argument confers authority over, with its mode:
    /// every persistent and transient slot for a reentrant or raw storage
    /// capability, and the supplied place or handle's domain otherwise. The
    /// requirement of a row's component is the one the callee's instance
    /// expands it to.
    fn effect_footprint(
        &mut self,
        callee: SemanticInstanceKey<'db>,
        effect: &NEffectArg<'db>,
    ) -> Option<Footprint> {
        let mode = if effect.required_mut {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        };
        let arg_ty = match &effect.arg {
            NEffectArgValue::Place(place) => place.ty,
            NEffectArgValue::Value(value) => self.body.values[value.value.index()].ty,
        };
        let requirements = effect_param_site(callee.owner(self.db))
            .map(|site| effect_requirements_for_site(self.db, site))
            .unwrap_or_default();
        let requirement = requirements
            .iter()
            .find(|requirement| requirement.binding_idx == effect.binding_idx)
            .or_else(|| {
                row_expansion_for_key(self.db, callee)
                    .components
                    .iter()
                    .map(|component| &component.requirement)
                    .find(|requirement| requirement.binding_idx == effect.binding_idx)
            });
        let access = requirement.and_then(|requirement| match &requirement.key {
            EffectRequirementKey::Type(declared) => self.capability_state_access(
                callee.owner(self.db),
                Some(*declared),
                arg_ty,
                effect.required_mut,
            ),
            key => effect_key_state_access(
                self.db,
                self.scope,
                self.assumptions,
                key.clone(),
                effect.required_mut,
            ),
        });
        if let Some(access) = access {
            return Some(Footprint {
                regions: state_regions(),
                mode: access.borrow_kind(),
                parents: TokenSet::new(),
            });
        }
        let (regions, parents) = match &effect.arg {
            // A place that holds nothing has no footprint.
            NEffectArgValue::Place(place) if self.holds_nothing(place.ty) => return None,
            NEffectArgValue::Place(place) => {
                let resolved = self.resolve(place);
                (resolved.regions, resolved.direct)
            }
            NEffectArgValue::Value(value) => (
                self.values[value.value.index()]
                    .iter()
                    .filter(|(token, _)| self.tokens[*token as usize].kind == TokenKind::Handle)
                    .flat_map(|(token, _)| self.tokens[*token as usize].regions.clone())
                    .collect(),
                TokenSet::new(),
            ),
        };
        (!regions.is_empty()).then_some(Footprint {
            regions,
            mode,
            parents,
        })
    }

    /// The regions of the place a value was read from or borrowed, through
    /// forwards and temporaries, or else the places its own carriers name,
    /// with the tokens that authorize it.
    fn source_place(&mut self, value: NValueId) -> (Vec<AbsPlace>, TokenSet) {
        let body = self.body;
        let mut source = value;
        let (regions, parents) = loop {
            match body.defining_expr(source) {
                Some((_, NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. })) => {
                    source = src.value
                }
                Some((
                    _,
                    NExpr::Borrow { place, .. }
                    | NExpr::MakeView { place, .. }
                    | NExpr::Load { place, .. },
                )) => {
                    // A temporary holds the value it was initialized from.
                    if let NPlaceBase::Root(root) = place.base
                        && let NRootKind::Temporary { value } = body.roots[root.index()].kind
                    {
                        source = value;
                        continue;
                    }
                    let resolved = self.resolve(place);
                    break (resolved.regions, resolved.direct);
                }
                _ => {
                    let direct = self.direct(source);
                    let regions: Vec<AbsPlace> = direct
                        .iter()
                        .flat_map(|token| self.tokens[*token as usize].regions.clone())
                        .collect();
                    break (regions, direct);
                }
            }
        };
        if regions.is_empty() {
            (vec![AbsPlace::new(Base::Raw)], parents)
        } else {
            (regions, parents)
        }
    }

    // --- control flow -----------------------------------------------------

    /// The state on entry to `block`, after its parameters are (re)defined.
    fn block_entry(&self, block: NBlockId) -> Option<State> {
        let mut state = self.entry[block.index()].clone()?;
        for param in &self.body.blocks[block.index()].params {
            state.initialize(|(key, _)| *key == MoveKey::Value(*param));
        }
        Some(state)
    }

    /// The statements of `block` that can execute.
    fn statements(
        &self,
        block: NBlockId,
    ) -> impl Iterator<Item = (usize, &'a NStatement<'db>)> + use<'a, 'db> {
        let statements = &self.body.blocks[block.index()].statements;
        let end = self.divergence[block.index()].map_or(statements.len(), |end| end + 1);
        statements[..end].iter().enumerate()
    }

    fn diverges(&self, block: NBlockId) -> bool {
        self.divergence[block.index()].is_some()
    }

    fn successors(&self, block: NBlockId) -> Vec<NBlockId> {
        if self.diverges(block) {
            return Vec::new();
        }
        self.body.blocks[block.index()]
            .terminator
            .kind
            .successors()
            .into_iter()
            .map(|successor| successor.block)
            .collect()
    }

    fn reverse_postorder(&self) -> Vec<NBlockId> {
        let mut visited = vec![false; self.body.blocks.len()];
        let mut postorder = Vec::new();
        let mut stack = vec![(self.body.entry, 0)];
        visited[self.body.entry.index()] = true;
        while let Some((block, next)) = stack.last_mut() {
            let successors = self.successors(*block);
            if let Some(&successor) = successors.get(*next) {
                *next += 1;
                if !visited[successor.index()] {
                    visited[successor.index()] = true;
                    stack.push((successor, 0));
                }
            } else {
                postorder.push(*block);
                stack.pop();
            }
        }
        postorder.reverse();
        postorder
    }

    /// Runs a block's statements; returns the state at its terminator.
    fn run_block(&mut self, block: NBlockId) -> Option<State> {
        let mut state = self.block_entry(block)?;
        let statements: Vec<_> = self.statements(block).collect();
        for (_, statement) in statements {
            if self.transfer(statement, &mut state).is_err() {
                break;
            }
        }
        Some(state)
    }

    fn propagate(&mut self, block: NBlockId, state: &State) -> bool {
        if self.diverges(block) {
            return false;
        }
        let mut changed = false;
        let kind = &self.body.blocks[block.index()].terminator.kind;
        let mut moved = state.clone();
        if let Some(access) = kind.access(self.db, self.body)
            && let AccessTarget::Value { operand, .. } = access.target
            && access.kind == MemoryAccessKind::Move
        {
            moved.record_move((MoveKey::Value(operand.value), Path::new()));
        }
        for successor in kind.successors() {
            let mut edge = moved.clone();
            for arg in &successor.args {
                if arg.mode == ReadMode::Move {
                    edge.record_move((MoveKey::Value(arg.value), Path::new()));
                }
            }
            match &mut self.entry[successor.block.index()] {
                Some(existing) => changed |= existing.join(&edge),
                slot @ None => {
                    *slot = Some(edge);
                    changed = true;
                }
            }
        }
        changed
    }

    // --- checks -----------------------------------------------------------

    /// Checks the body; returns its result spaces.
    pub fn check(&mut self) -> Result<Vec<Option<AddressSpace>>, Diag<'db>> {
        self.checking = true;
        for block in self.reverse_postorder() {
            let Some(mut state) = self.block_entry(block) else {
                continue;
            };
            let statements: Vec<_> = self.statements(block).collect();
            for (_, statement) in statements {
                self.check_places_known(statement)?;
                self.transfer(statement, &mut state)?;
            }
            if !self.diverges(block) {
                self.check_terminator(block, &state)?;
            }
        }
        self.check_yield_paths()?;
        self.check_raw_place()?;
        self.check_projection_recursion()?;
        self.check_yields()
    }

    /// Every place a reachable operation names is known once the flow is
    /// solved: its carrier holds the access it reaches the place through, and
    /// a dereference is of a pointer. A place nothing traces is an error,
    /// never memory.
    fn check_places_known(&self, statement: &NStatement<'db>) -> Result<(), Diag<'db>> {
        let known = |place: &NPlace<'db>| match place.base {
            NPlaceBase::Root(_) => true,
            NPlaceBase::CapabilityTarget { carrier } => !self.direct(carrier).is_empty(),
            NPlaceBase::Deref { pointer } => self.body.values[pointer.index()]
                .ty
                .as_ptr(self.db)
                .is_some(),
        };
        let mut unknown = None;
        match &statement.kind {
            NStatementKind::Store { destination, .. }
            | NStatementKind::Drop {
                target: NDropTarget::Place(destination),
            } if !known(destination) => {
                unknown = Some(destination.origin);
            }
            NStatementKind::Define { expr, .. } => expr.for_each_place_operand(|place| {
                if unknown.is_none() && !known(place) {
                    unknown = Some(place.origin);
                }
            }),
            NStatementKind::Store { .. }
            | NStatementKind::End { .. }
            | NStatementKind::Drop { .. } => {}
        }
        match unknown {
            Some(origin) => Err(self.diag(
                SemanticDiagnosticKind::Internal,
                specific_origin(origin, statement.origin),
                "cannot tell which place this access names".into(),
            )),
            None => Ok(()),
        }
    }

    /// A `#[raw_place]` projection yields a place computed from its inputs,
    /// with nothing to resume: after each yield it only closes the accesses
    /// its grant derives from, a session among them is a `#[raw_place]`
    /// call's, and the place is not one of its own frame.
    fn check_raw_place(&self) -> Result<(), Diag<'db>> {
        let owner = self.instance.key(self.db).owner(self.db);
        let BodyOwner::Func(func, mode) = owner else {
            return Ok(());
        };
        if !func.is_raw_place(self.db) {
            return Ok(());
        }
        let violation = |origin, message: &str| {
            self.diag(
                SemanticDiagnosticKind::RawPlaceViolation,
                origin,
                message.into(),
            )
        };
        if func.return_shape(self.db, mode).is_none() {
            return Err(violation(
                SemOrigin::Body(owner),
                "a `#[raw_place]` function must be a projection",
            ));
        }
        for block in &self.body.blocks {
            let NTerminatorKind::Yield { value, resume } = &block.terminator.kind else {
                continue;
            };
            let origin = operand_origin(*value, block.terminator.origin);
            if self.values[value.value.index()].iter().any(|(token, _)| {
                self.tokens[*token as usize]
                    .regions
                    .iter()
                    .any(|region| matches!(region.base, Base::Root(_)))
            }) {
                return Err(violation(
                    origin,
                    "a `#[raw_place]` projection yields a place of its own frame",
                ));
            }
            let mut derived = FxHashSet::default();
            let mut work = vec![value.value];
            while let Some(value) = work.pop() {
                if derived.insert(value)
                    && let Some((_, expr)) = self.body.defining_expr(value)
                {
                    expr.for_each_value_operand(|operand| work.push(operand.value));
                }
            }
            let mut next = resume.block;
            loop {
                let data = &self.body.blocks[next.index()];
                for statement in &data.statements {
                    let NStatementKind::End { access } = statement.kind else {
                        return Err(violation(
                            statement.origin,
                            "a `#[raw_place]` projection runs no code after its yield",
                        ));
                    };
                    let raw_session = match self.body.defining_expr(access) {
                        Some((_, NExpr::Call { callee, .. })) => matches!(
                            callee.key.owner(self.db),
                            BodyOwner::Func(callee, _) if callee.is_raw_place(self.db)
                        ),
                        _ => true,
                    };
                    if !derived.contains(&access) || !raw_session {
                        // Point at what opened the access rather than its end.
                        let opened = match self.body.value(access).map(|value| value.definition) {
                            Some(NValueDefinition::Statement { block, statement }) => {
                                self.body.blocks[block.index()].statements[statement as usize]
                                    .origin
                            }
                            _ => statement.origin,
                        };
                        return Err(violation(
                            opened,
                            "a `#[raw_place]` projection keeps no access or session of its own \
                             open across its yield",
                        ));
                    }
                }
                match &data.terminator.kind {
                    NTerminatorKind::Goto(successor) => next = successor.block,
                    NTerminatorKind::Return(_) => break,
                    _ => {
                        return Err(violation(
                            data.terminator.origin,
                            "a `#[raw_place]` projection runs no code after its yield",
                        ));
                    }
                }
            }
        }
        Ok(())
    }

    /// Projections are inlined into their callers, so a projection may not
    /// reach itself through projection calls. Another instance of its
    /// function is another projection, as `Enumerate<Enumerate<D>>::at` calls
    /// `Enumerate<D>::at`; instantiation limits keep their chains finite.
    fn check_projection_recursion(&self) -> Result<(), Diag<'db>> {
        if !self.instance.is_projection(self.db) {
            return Ok(());
        }
        let mut explored = FxHashSet::default();
        for statement in self.body.blocks.iter().flat_map(|block| &block.statements) {
            if let NStatementKind::Define {
                expr: NExpr::Call { callee, .. },
                ..
            } = &statement.kind
                && reaches_instance(
                    self.db,
                    SemanticInstance::new(self.db, callee.key),
                    self.instance,
                    &mut explored,
                )
            {
                return Err(self.diag(
                    SemanticDiagnosticKind::ProjectionRecursion,
                    statement.origin,
                    "this call reaches the projection again through projection calls, which \
                     cannot be inlined"
                        .into(),
                ));
            }
        }
        Ok(())
    }

    /// A projection yields exactly once on each path that completes.
    fn check_yield_paths(&self) -> Result<(), Diag<'db>> {
        const NOT_YET: u8 = 1;
        const YIELDED: u8 = 2;
        let BodyOwner::Func(func, mode) = self.instance.key(self.db).owner(self.db) else {
            return Ok(());
        };
        if func.body(self.db).is_none() || func.return_shape(self.db, mode).is_none() {
            return Ok(());
        }
        let mut entry = vec![0; self.body.blocks.len()];
        entry[self.body.entry.index()] = NOT_YET;
        let mut work = vec![self.body.entry];
        while let Some(block) = work.pop() {
            if self.diverges(block) {
                continue;
            }
            let state = entry[block.index()];
            let terminator = &self.body.blocks[block.index()].terminator;
            let after = match terminator.kind {
                NTerminatorKind::Yield { value, .. } if state & YIELDED != 0 => {
                    // A sum shape's empty variant grants nothing.
                    let message = if self.values[value.value.index()].is_empty() {
                        "a projection returns its empty variant only before it yields"
                    } else {
                        "this path has already yielded"
                    };
                    return Err(self.diag(
                        SemanticDiagnosticKind::YieldViolation,
                        operand_origin(value, terminator.origin),
                        message.into(),
                    ));
                }
                NTerminatorKind::Yield { .. } => YIELDED,
                NTerminatorKind::Return(_) if state & NOT_YET != 0 => {
                    return Err(self.diag(
                        SemanticDiagnosticKind::YieldViolation,
                        terminator.origin,
                        "a projection must yield on every path that completes".into(),
                    ));
                }
                _ => state,
            };
            for successor in self.successors(block) {
                let merged = entry[successor.index()] | after;
                if merged != entry[successor.index()] {
                    entry[successor.index()] = merged;
                    work.push(successor);
                }
            }
        }
        Ok(())
    }

    /// `mut` yields of session-owned places with no slide after them: writes
    /// through them are discarded when the session finishes. A view
    /// descriptor is exempt, since its writes reach its owner.
    fn discarded_writes(&self) -> Vec<Diag<'db>> {
        let BodyOwner::Func(func, mode) = self.instance.key(self.db).owner(self.db) else {
            return Vec::new();
        };
        let Some(shape) = func.return_shape(self.db, mode) else {
            return Vec::new();
        };
        let components = access_components(self.db, shape);
        self.body
            .blocks
            .iter()
            .filter_map(|block| match block.terminator.kind {
                NTerminatorKind::Yield { value, ref resume } if !self.has_slide(resume.block) => {
                    Some((value, block.terminator.origin))
                }
                _ => None,
            })
            .filter(|(value, _)| {
                let held = &self.values[value.value.index()];
                components.iter().any(|(path, kind, ty)| {
                    let mut tokens = held.iter().filter(|(_, rest)| rest == path).peekable();
                    *kind == BorrowKind::Mut
                        && !ty.is_view(self.db)
                        && tokens.peek().is_some()
                        && tokens.all(|(token, _)| {
                            self.tokens[*token as usize]
                                .regions
                                .iter()
                                .all(|region| matches!(region.base, Base::Root(_)))
                        })
                })
            })
            .map(|(value, origin)| {
                self.diag(
                    SemanticDiagnosticKind::DiscardedWrites,
                    operand_origin(value, origin),
                    "this yields a place of the projection's own frame, and no code after the \
                     yield writes it back"
                        .into(),
                )
            })
            .collect()
    }

    /// Whether code that can write runs between resuming at `block` and
    /// returning.
    fn has_slide(&self, mut block: NBlockId) -> bool {
        loop {
            let data = &self.body.blocks[block.index()];
            if data.statements.iter().any(|statement| {
                matches!(
                    statement.kind,
                    NStatementKind::Store { .. }
                        | NStatementKind::Define {
                            expr: NExpr::Call { .. },
                            ..
                        }
                )
            }) {
                return true;
            }
            match &data.terminator.kind {
                NTerminatorKind::Goto(next) => block = next.block,
                NTerminatorKind::Return(_) => return false,
                _ => return true,
            }
        }
    }

    fn diag(
        &self,
        kind: SemanticDiagnosticKind,
        origin: SemOrigin<'db>,
        message: String,
    ) -> Diag<'db> {
        SemanticDiagnostic::new(self.instance, kind, message, self.span(origin))
    }

    /// Whether `origin` is a pattern binding or a `let` initializer: a value
    /// bound by name.
    fn binds_by_value(&self, origin: SemOrigin<'db>) -> bool {
        match origin {
            SemOrigin::Pat(_) => true,
            SemOrigin::Expr(expr) => self.body.template_owner.body(self.db).is_some_and(|body| {
                body.stmts(self.db).values().any(|stmt| {
                    matches!(stmt, Partial::Present(Stmt::Let(_, _, Some(init), _)) if *init == expr)
                })
            }),
            _ => false,
        }
    }

    fn span(&self, origin: SemOrigin<'db>) -> SemanticDiagnosticSpan<'db> {
        SemanticDiagnosticSpan::OriginWithTemplateFallback {
            owner: self.instance.key(self.db).owner(self.db),
            template_owner: self.body.template_owner,
            origin,
        }
    }

    /// Reports `result` only while checking; the fixed point keeps going.
    fn report(&self, result: Result<(), Diag<'db>>) -> Result<(), Diag<'db>> {
        if self.checking { result } else { Ok(()) }
    }

    fn transfer(
        &mut self,
        statement: &NStatement<'db>,
        state: &mut State,
    ) -> Result<(), Diag<'db>> {
        let origin = statement.origin;
        if let NStatementKind::End { access } = &statement.kind {
            let ended = self.opened.get(access).cloned().unwrap_or_default();
            for token in &ended {
                state.open.remove(token);
            }
            // An access ends with its referent restored.
            let hole = ended.iter().find_map(|token| {
                Some((*token, self.hole(state, MoveKey::Access(*token))?.clone()))
            });
            state.initialize(
                |(key, _)| matches!(key, MoveKey::Access(token) if ended.contains(token)),
            );
            return match hole {
                Some((token, hole)) => {
                    let mut diag = self.moved_diag(
                        "this access ends while its referent is moved out",
                        self.tokens[token as usize].origin,
                        &hole,
                    );
                    // A binding that moved out of the access can use it in
                    // place instead.
                    if let Some(moved_at) = self.moved_at.get(&hole)
                        && self.binds_by_value(*moved_at)
                        && let Some(label) = diag.secondaries.last_mut()
                    {
                        label.message.push_str(IN_PLACE_FIX);
                    }
                    self.report(Err(diag))
                }
                None => Ok(()),
            };
        }
        // A statement that opens accesses again opens new instances of them.
        if let NStatementKind::Define { result, .. } = &statement.kind
            && let Some(opened) = self.opened.get(result)
        {
            for token in opened {
                state.open.remove(token);
            }
        }
        // Operands are checked in order, so one operation cannot consume the
        // same value twice.
        for access in statement.kind.accesses(self.db, self.body) {
            match access.target {
                AccessTarget::Value { operand, path } => {
                    let consume = access.kind == MemoryAccessKind::Move;
                    let result = self.use_value(operand, path, state, consume, origin);
                    self.report(result)?;
                }
                AccessTarget::Place(place) => {
                    let origin = specific_origin(place.origin, origin);
                    let result = self.access_place(place, access.kind, state, origin);
                    self.report(result)?;
                }
            }
        }
        match &statement.kind {
            NStatementKind::Store { destination, .. } => {
                let path = self.path(&destination.path);
                let key = match destination.base {
                    NPlaceBase::Root(root) => Some(MoveKey::Root(root.index() as u32)),
                    NPlaceBase::CapabilityTarget { carrier } => self.carrier_key(carrier),
                    NPlaceBase::Deref { .. } => None,
                };
                if let Some(key) = key
                    && !path.contains(&Step::Index(None))
                {
                    state.initialize(|(moved, moved_path)| {
                        *moved == key && moved_path.starts_with(&path)
                    });
                }
            }
            NStatementKind::Define { result, expr } => {
                state.initialize(|(key, _)| *key == MoveKey::Value(*result));
                // A value forwarded by copy is handed on when it owns
                // something to destroy: two owners would destroy it twice. An
                // inspection reads it in place.
                if let NExpr::Forward { src } = expr
                    && src.mode == ReadMode::Copy
                    && self.owns_drop_glue(self.body.values[src.value.index()].ty)
                {
                    let moved = (MoveKey::Value(src.value), Path::new());
                    self.moved_at.entry(moved.clone()).or_insert(origin);
                    state.record_move(moved);
                }
                if let NExpr::Call {
                    callee,
                    args,
                    effect_args,
                    ..
                } = expr
                {
                    let siblings = self.split_siblings(*result);
                    let result =
                        self.check_call(callee.key, args, effect_args, state, &siblings, origin);
                    self.report(result)?;
                }
                let opened = self.opened.get(result).cloned().unwrap_or_default();
                if self.checking {
                    let siblings = self.split_siblings(*result);
                    for &token in &opened {
                        self.check_opened(token, state, &siblings)?;
                    }
                }
                // Accesses no `end` closes last for the operation using them.
                let mut used = Vec::new();
                expr.for_each_value_operand(|operand| used.push(operand.value));
                for value in used {
                    for (opener, tokens) in &self.opened {
                        if !self.ended.contains(opener)
                            && self.values[value.index()]
                                .iter()
                                .any(|(token, _)| tokens.contains(token))
                        {
                            for token in tokens {
                                state.open.remove(token);
                            }
                        }
                    }
                }
                state.open.extend(opened);
            }
            NStatementKind::Drop { target } => {
                let result = self.drop_target(statement.id, target, state, origin);
                self.report(result)?;
            }
            NStatementKind::End { .. } => {}
        }
        Ok(())
    }

    /// Whether a value of `ty` may own something to destroy. A `Copy` value
    /// owns nothing.
    fn owns_drop_glue(&self, ty: TyId<'db>) -> bool {
        ty_has_drop_glue(self.db, ty) && !ty_is_copy(self.db, self.scope, ty, self.assumptions)
    }

    /// A value whose type implements `Drop` is destroyed whole, so no part
    /// of it moves out; its own `drop` destroys `self` field by field, and
    /// never moves it whole. `prefix_ty` is the type after a path prefix of
    /// a length.
    fn check_drop_move(
        &self,
        key: MoveKey,
        prefix_ty: impl Fn(usize) -> Option<TyId<'db>>,
        len: usize,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let in_own_drop = self.is_drop_self(key);
        if in_own_drop && len == 0 {
            return Err(self.diag(
                SemanticDiagnosticKind::MoveConflict,
                origin,
                "`drop` cannot move `self` whole: it is destroyed field by field".into(),
            ));
        }
        let skip = usize::from(in_own_drop);
        for ty in (skip..len).filter_map(prefix_ty) {
            if ty
                .adt_def(self.db)
                .is_some_and(|adt| adt_drop_impl(self.db, adt.adt_ref(self.db)).is_some())
            {
                return Err(self.diag(
                    SemanticDiagnosticKind::MoveConflict,
                    origin,
                    format!(
                        "cannot move out of `{}`, which implements `Drop`",
                        ty.pretty_print(self.db)
                    ),
                ));
            }
        }
        Ok(())
    }

    /// Whether `key` is the `self` of the `Drop::drop` being checked.
    fn is_drop_self(&self, key: MoveKey) -> bool {
        let BodyOwner::Func(func, _) = self.instance.key(self.db).owner(self.db) else {
            return false;
        };
        let self_param = match key {
            MoveKey::Value(value) => matches!(
                self.body.values[value.index()].definition,
                NValueDefinition::EntryParam { param: 0 }
            ),
            MoveKey::Root(root) => matches!(
                self.body.roots[root as usize].kind,
                NRootKind::ParamPlace { param: 0 }
            ),
            MoveKey::Param(_) | MoveKey::Access(_) => false,
        };
        self_param && implements_drop(self.db, func)
    }

    /// Whether `value` owns what it holds, rather than reading a place or
    /// a value in place.
    fn value_owns(&self, value: NValueId) -> bool {
        !matches!(
            self.body.defining_expr(value),
            Some((
                _,
                NExpr::Load {
                    mode: ReadMode::Read,
                    ..
                } | NExpr::Forward {
                    src: NOperand {
                        mode: ReadMode::Read,
                        ..
                    },
                } | NExpr::ProjectValue {
                    value: NOperand {
                        mode: ReadMode::Read,
                        ..
                    },
                    ..
                }
            ))
        )
    }

    /// The parts of what `key` holds at `path` that a drop destroys: those
    /// initialized on some path, each with the flags of the paths on its way
    /// that only some paths move. A place no key tracks is initialized.
    fn drop_parts(
        &mut self,
        key: Option<MoveKey>,
        path: &Path,
        ty: TyId<'db>,
        state: &State,
    ) -> Vec<DropPart> {
        let Some(key) = key else {
            return vec![DropPart {
                path: Vec::new(),
                flags: Vec::new(),
            }];
        };
        let mut flags = Vec::new();
        for len in 0..path.len() {
            let enclosing = (key, path[..len].iter().copied().collect());
            if state.definitely_moved.contains(&enclosing) {
                return Vec::new();
            }
            if state.moved.contains(&enclosing) {
                flags.push(self.drop_flag(enclosing));
            }
        }
        let mut parts = Vec::new();
        self.collect_drop_parts(key, path, path.len(), ty, flags, state, &mut parts);
        parts
    }

    #[allow(clippy::too_many_arguments)]
    fn collect_drop_parts(
        &mut self,
        key: MoveKey,
        path: &Path,
        base: usize,
        ty: TyId<'db>,
        mut flags: Vec<u32>,
        state: &State,
        parts: &mut Vec<DropPart>,
    ) {
        let entry = (key, path.clone());
        if !self.owns_drop_glue(ty) || state.definitely_moved.contains(&entry) {
            return;
        }
        if state.moved.contains(&entry) {
            flags.push(self.drop_flag(entry));
        }
        let partly_moved = state.moved.iter().any(|(moved, moved_path)| {
            *moved == key && moved_path.len() > path.len() && moved_path.starts_with(path)
        });
        if !partly_moved {
            parts.push(DropPart {
                path: path[base..].iter().map(|step| drop_step(*step)).collect(),
                flags,
            });
            return;
        }
        for (step, component_ty) in self.drop_components(ty) {
            let mut component = path.clone();
            component.push(step);
            self.collect_drop_parts(
                key,
                &component,
                base,
                component_ty,
                flags.clone(),
                state,
                parts,
            );
        }
    }

    /// The components of a value of `ty` its drop destroys one by one when
    /// some are moved out: fields, every variant's fields, or elements.
    fn drop_components(&self, ty: TyId<'db>) -> Vec<(Step, TyId<'db>)> {
        let (db, instance) = (self.db, self.instance);
        if let Some(adt) = ty.adt_def(db)
            && matches!(adt.adt_ref(db), AdtRef::Enum(_))
        {
            return (0..adt.fields(db).len() as u16)
                .flat_map(|variant| {
                    instance
                        .normalized_enum_variant_field_tys(db, ty, VariantIndex(variant))
                        .into_iter()
                        .enumerate()
                        .map(move |(field, &ty)| {
                            (
                                Step::Variant {
                                    variant,
                                    field: field as u16,
                                },
                                ty,
                            )
                        })
                })
                .collect();
        }
        if ty.is_array(db)
            && let Some(len) = ty.array_len(db)
        {
            let element = instance.normalized_ty(db, ty.generic_args(db)[0]);
            return (0..len)
                .map(|index| (Step::Index(Some(index)), element))
                .collect();
        }
        instance
            .normalized_field_types(db, ty)
            .iter()
            .enumerate()
            .map(|(field, &ty)| (Step::Field(field as u16), ty))
            .collect()
    }

    /// The flag tracking whether `entry` is initialized.
    fn drop_flag(&mut self, entry: (MoveKey, Path)) -> u32 {
        let index = self
            .drop_flags
            .iter()
            .position(|flag| *flag == entry)
            .unwrap_or_else(|| {
                self.drop_flags.push(entry);
                self.drop_flags.len() - 1
            });
        index as u32
    }

    /// What the body's drops destroy, with the flags they consult set where
    /// the flow moves or initializes each flagged path.
    fn drop_elaboration(&mut self) -> Result<DropElaboration, Diag<'db>> {
        let mut drops = std::mem::take(&mut self.drop_parts);
        drops.sort_by_key(|(statement, _)| *statement);
        let entry = self.entry[self.body.entry.index()]
            .clone()
            .unwrap_or_default();
        let flags = self
            .drop_flags
            .iter()
            .map(|flag| !self.flag_moved(&entry.moved, flag))
            .collect();
        let flag_updates = if self.drop_flags.is_empty() {
            Vec::new()
        } else {
            self.drop_flag_updates()?
        };
        Ok(DropElaboration {
            drops,
            flags,
            flag_updates,
        })
    }

    /// Whether an entry of `moved` covers the flagged path.
    fn flag_moved(&self, moved: &BTreeSet<(MoveKey, Path)>, flag: &(MoveKey, Path)) -> bool {
        moved
            .iter()
            .any(|(key, path)| *key == flag.0 && flag.1.starts_with(path))
    }

    /// Replays the solved flow, recording where each flagged path is moved
    /// or initialized.
    fn drop_flag_updates(&mut self) -> Result<Vec<DropFlagUpdate>, Diag<'db>> {
        self.checking = false;
        let mut updates = Vec::new();
        for block in self.reverse_postorder() {
            let Some(before) = self.entry[block.index()].clone() else {
                continue;
            };
            let mut state = self
                .block_entry(block)
                .expect("a reached block has an entry state");
            self.record_flag_updates(
                &before,
                &state,
                DropFlagPoint::BlockStart(block),
                &mut updates,
            );
            let statements: Vec<_> = self.statements(block).collect();
            for (_, statement) in statements {
                let before = state.clone();
                let _ = self.transfer(statement, &mut state);
                self.record_flag_updates(
                    &before,
                    &state,
                    DropFlagPoint::After(statement.id),
                    &mut updates,
                );
            }
            if self.diverges(block) {
                continue;
            }
            // A terminator's moves happen on its way out.
            let kind = &self.body.blocks[block.index()].terminator.kind;
            let successors = kind.successors();
            let mut after = state.clone();
            if let Some(access) = kind.access(self.db, self.body)
                && let AccessTarget::Value { operand, .. } = access.target
                && access.kind == MemoryAccessKind::Move
            {
                after.record_move((MoveKey::Value(operand.value), Path::new()));
            }
            for successor in &successors {
                let mut edge = after.clone();
                for arg in &successor.args {
                    if arg.mode == ReadMode::Move {
                        edge.record_move((MoveKey::Value(arg.value), Path::new()));
                    }
                }
                let mut moves = Vec::new();
                self.record_flag_updates(
                    &state,
                    &edge,
                    DropFlagPoint::BeforeTerminator(block),
                    &mut moves,
                );
                let mut everywhere = Vec::new();
                self.record_flag_updates(
                    &state,
                    &after,
                    DropFlagPoint::BeforeTerminator(block),
                    &mut everywhere,
                );
                // Only a successor argument moves on one edge alone, and
                // normalization passes them in `Copy` mode, so this does not
                // happen today; a flag update per edge would be needed if it did.
                if moves.len() != everywhere.len() && successors.len() > 1 {
                    return Err(self.diag(
                        SemanticDiagnosticKind::Internal,
                        self.body.blocks[block.index()].terminator.origin,
                        "a value is moved into one branch only and destroyed after it".into(),
                    ));
                }
                if successor.block == successors[0].block {
                    updates.extend(moves);
                }
            }
        }
        self.checking = true;
        Ok(updates)
    }

    fn record_flag_updates(
        &self,
        before: &State,
        after: &State,
        at: DropFlagPoint,
        updates: &mut Vec<DropFlagUpdate>,
    ) {
        for (flag, tracked) in self.drop_flags.iter().enumerate() {
            let covers =
                |entry: &(MoveKey, Path)| entry.0 == tracked.0 && tracked.1.starts_with(&entry.1);
            let moved = after
                .definitely_moved
                .iter()
                .any(|entry| covers(entry) && !before.definitely_moved.contains(entry));
            let initialized = before
                .moved
                .iter()
                .any(|entry| covers(entry) && !after.moved.contains(entry));
            if moved || initialized {
                updates.push(DropFlagUpdate {
                    at,
                    flag: flag as u32,
                    initialized: !moved,
                });
            }
        }
    }

    /// A drop destroys whatever of its target is initialized, so a part
    /// moved out is no error. Destroying a place writes it: no other access
    /// may be open on it.
    fn drop_target(
        &mut self,
        statement: NStatementId,
        target: &NDropTarget<'db>,
        state: &mut State,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let place = match target {
            NDropTarget::Value(value) => {
                let entry = (MoveKey::Value(*value), Path::new());
                if self.checking {
                    let ty = self.body.values[value.index()].ty;
                    let parts = if self.value_owns(*value) {
                        self.drop_parts(Some(entry.0), &entry.1, ty, state)
                    } else {
                        Vec::new()
                    };
                    self.record_drop(statement, parts, ty, origin)?;
                }
                state.record_move(entry);
                return Ok(());
            }
            NDropTarget::Place(place) => place,
        };
        let resolved = self.resolve(place);
        let path = self.path(&place.path);
        let key = match place.base {
            NPlaceBase::Root(root) if self.root_domain[root.index()].is_none() => {
                Some(MoveKey::Root(root.index() as u32))
            }
            NPlaceBase::Root(_) | NPlaceBase::Deref { .. } => None,
            NPlaceBase::CapabilityTarget { carrier } => self.carrier_key(carrier),
        };
        // `[*]` names no definite element, so the drop may reach one moved
        // out and destroy again what was moved, unless that owns nothing.
        if let Some(key) = key
            && path.contains(&Step::Index(None))
            && let Some(found) = state.moved.iter().find(|(moved, moved_path)| {
                *moved == key
                    && paths_overlap(moved_path, &path)
                    && moved_path
                        .get(path.len()..)
                        .unwrap_or_default()
                        .iter()
                        .try_fold(place.ty, |ty, step| {
                            self.drop_components(ty)
                                .into_iter()
                                .find_map(|(component, ty)| (component == *step).then_some(ty))
                        })
                        .is_none_or(|ty| self.owns_drop_glue(ty))
            })
        {
            return Err(self.moved_diag(
                "cannot assign at a dynamic index while an element may be moved out",
                origin,
                found,
            ));
        }
        if self.checking {
            let parts = self.drop_parts(key, &path, place.ty, state);
            self.record_drop(statement, parts, place.ty, origin)?;
        }
        // `[*]` names no definite element; the store a drop precedes
        // initializes it again.
        if let Some(key) = key
            && !path.contains(&Step::Index(None))
        {
            state.record_move((key, path));
        }
        if self.holds_nothing(place.ty) {
            return Ok(());
        }
        self.check_conflicts(
            state,
            &resolved.regions,
            BorrowKind::Mut,
            &resolved.authority,
            &[],
            MemoryAccessKind::Write,
            origin,
        )
    }

    /// Records the parts the drop `statement` destroys. Compile-time code
    /// runs no `Drop` implementation, so there a drop may destroy nothing
    /// whose destruction might run one.
    fn record_drop(
        &mut self,
        statement: NStatementId,
        parts: Vec<DropPart>,
        ty: TyId<'db>,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        if !parts.is_empty()
            && match self.instance.key(self.db).owner(self.db) {
                BodyOwner::Func(func, _) => func.is_const(self.db),
                BodyOwner::Const(_) | BodyOwner::AnonConstBody { .. } => true,
                BodyOwner::ContractInit { .. }
                | BodyOwner::ContractRecvArm { .. }
                | BodyOwner::Closure { .. } => false,
            }
        {
            return Err(self.diag(
                SemanticDiagnosticKind::ConstDestructor,
                origin,
                format!(
                    "destroying `{}` may run a `Drop` implementation, which compile-time code cannot",
                    ty.pretty_print(self.db)
                ),
            ));
        }
        self.drop_parts.push((statement, parts));
        Ok(())
    }

    /// What tracks the initialization of a carrier's referent: the data
    /// parameter it is the input of, or the one `mut` access it holds.
    fn carrier_key(&self, carrier: NValueId) -> Option<MoveKey> {
        let [token] = self.direct(carrier)[..] else {
            return None;
        };
        let data = &self.tokens[token as usize];
        match (data.kind, data.regions.as_slice()) {
            (
                TokenKind::Input,
                [
                    AbsPlace {
                        base: Base::Param(param),
                        ..
                    },
                ],
            ) => Some(MoveKey::Param(*param)),
            (TokenKind::Access | TokenKind::Grant, _) if data.mode == BorrowKind::Mut => {
                Some(MoveKey::Access(token))
            }
            _ => None,
        }
    }

    /// The first hole in a referent an operation needs initialized.
    fn hole<'s>(&'s self, state: &'s State, key: MoveKey) -> Option<&'s (MoveKey, Path)> {
        state.moved.iter().find(|(moved, _)| *moved == key)
    }

    /// Reject a use of a possibly moved value, then record its move.
    fn use_value(
        &mut self,
        operand: NOperand,
        structural: Option<&NStructuralPath>,
        state: &mut State,
        consume: bool,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let path = structural
            .map(|path| self.path(&path.0))
            .unwrap_or_default();
        let key = MoveKey::Value(operand.value);
        if let Some(found) = state
            .moved
            .iter()
            .find(|(moved, moved_path)| *moved == key && paths_overlap(moved_path, &path))
        {
            return Err(self.moved_diag(
                "cannot use a value after it was moved",
                operand_origin(operand, origin),
                found,
            ));
        }
        if consume
            && self.body.values[operand.value.index()]
                .ty
                .as_capability(self.db)
                .is_none()
        {
            let body = self.body;
            self.check_drop_move(
                key,
                |len| match structural {
                    Some(structural) => {
                        body.value_prefix_ty(self.db, operand.value, &structural.0, len)
                    }
                    None => body.value(operand.value).map(|value| value.ty),
                },
                path.len(),
                operand_origin(operand, origin),
            )?;
            self.moved_at
                .entry((key, path.clone()))
                .or_insert(operand_origin(operand, origin));
            state.record_move((key, path));
        }
        Ok(())
    }

    fn moved_diag(
        &self,
        message: &str,
        origin: SemOrigin<'db>,
        moved: &(MoveKey, Path),
    ) -> Diag<'db> {
        let mut diag = self.diag(SemanticDiagnosticKind::MoveConflict, origin, message.into());
        if let Some(moved_at) = self.moved_at.get(moved) {
            diag.push_secondary("value is moved here".into(), self.span(*moved_at));
        }
        diag
    }

    fn access_place(
        &mut self,
        place: &NPlace<'db>,
        kind: MemoryAccessKind,
        state: &mut State,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let resolved = self.resolve(place);
        let path = self.path(&place.path);
        let key = match place.base {
            NPlaceBase::Root(root) if self.root_domain[root.index()].is_none() => {
                Some(MoveKey::Root(root.index() as u32))
            }
            NPlaceBase::Root(_) | NPlaceBase::Deref { .. } => None,
            NPlaceBase::CapabilityTarget { carrier } => self.carrier_key(carrier),
        };
        // A closure's environment, data parameter 0, holds its captures, and
        // each call of the closure only views them.
        if kind == MemoryAccessKind::Move
            && matches!(self.body.template_owner, BodyOwner::Closure { .. })
            && resolved
                .regions
                .iter()
                .any(|region| region.base == Base::Param(0))
        {
            return Err(self.diag(
                SemanticDiagnosticKind::MoveConflict,
                origin,
                "cannot move out of a closure's captures, which each call of the closure only views"
                    .into(),
            ));
        }
        if kind == MemoryAccessKind::Move
            && let Some(space @ (AddressSpace::Storage | AddressSpace::Transient)) = resolved
                .regions
                .iter()
                .find_map(|region| self.region_space(region))
        {
            return Err(self.diag(
                SemanticDiagnosticKind::MoveConflict,
                origin,
                format!(
                    "cannot move out of {}, which never holds a hole",
                    space.pretty()
                ),
            ));
        }
        if let Some(key) = key {
            let found = state.moved.iter().find(|(moved, moved_path)| {
                *moved == key
                    && if kind == MemoryAccessKind::Write {
                        moved_path.len() < path.len() && path.starts_with(moved_path)
                    } else {
                        paths_overlap(moved_path, &path)
                    }
            });
            if let Some(found) = found {
                let message = if kind == MemoryAccessKind::Write {
                    "cannot assign to part of a moved value"
                } else if self.moved_at.contains_key(found) {
                    "cannot use a value after it was moved"
                } else {
                    "cannot use a value before it is initialized"
                };
                return Err(self.moved_diag(message, origin, found));
            }
            if kind == MemoryAccessKind::Move {
                let body = self.body;
                self.check_drop_move(
                    key,
                    |len| body.place_prefix_ty(self.db, place, len),
                    path.len(),
                    origin,
                )?;
                // `[*]` names no definite element to restore.
                if path.contains(&Step::Index(None)) {
                    return Err(self.diag(
                        SemanticDiagnosticKind::MoveConflict,
                        origin,
                        "cannot move out of an element at a dynamic index".into(),
                    ));
                }
                // A view's descriptor stays tied to the session that
                // reserves its owner, so it never leaves a hole.
                if place.ty.is_view(self.db) {
                    return Err(self.diag(
                        SemanticDiagnosticKind::MoveConflict,
                        origin,
                        format!(
                            "cannot move out of `{}`, a `#[view]` value, which never leaves a hole",
                            place.ty.pretty_print(self.db)
                        ),
                    ));
                }
                self.moved_at.entry((key, path.clone())).or_insert(origin);
                state.record_move((key, path));
            }
        }
        let writes = kind != MemoryAccessKind::Read;
        if writes
            && resolved
                .direct
                .iter()
                .any(|token| self.tokens[*token as usize].mode == BorrowKind::Ref)
        {
            let diag_kind = if kind == MemoryAccessKind::Move {
                SemanticDiagnosticKind::MoveConflict
            } else {
                SemanticDiagnosticKind::AccessConflict
            };
            // A plain binding takes a value: one that cannot move needs
            // `ref` to be read in place.
            let fix = if kind == MemoryAccessKind::Move && self.binds_by_value(origin) {
                "; bind it with `ref` to read it in place"
            } else {
                ""
            };
            return Err(self.diag(
                diag_kind,
                origin,
                format!(
                    "cannot {} a place reached through a `ref` access{fix}",
                    verb(kind)
                ),
            ));
        }
        if kind == MemoryAccessKind::Move
            && key.is_none()
            && resolved
                .regions
                .iter()
                .any(|region| region.base != Base::Raw)
        {
            let fix = if self.binds_by_value(origin) {
                "; bind it with `ref` to read it in place"
            } else {
                ""
            };
            return Err(self.diag(
                SemanticDiagnosticKind::MoveConflict,
                origin,
                format!("cannot move out of a place reached through an access{fix}"),
            ));
        }
        if writes {
            self.check_writable(&resolved.regions, origin)?;
        }
        if self.holds_nothing(place.ty) {
            return Ok(());
        }
        let mode = if writes {
            BorrowKind::Mut
        } else {
            BorrowKind::Ref
        };
        self.check_conflicts(
            state,
            &resolved.regions,
            mode,
            &resolved.authority,
            &[],
            kind,
            origin,
        )
    }

    fn check_writable(
        &self,
        regions: &[AbsPlace],
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        for region in regions {
            if let Some(space @ (AddressSpace::Calldata | AddressSpace::Code)) =
                self.region_space(region)
            {
                return Err(self.diag(
                    SemanticDiagnosticKind::StorageViolation,
                    origin,
                    format!("cannot write to {}", space.pretty()),
                ));
            }
        }
        Ok(())
    }

    /// The tokens open at a point: this body's open accesses and its data
    /// parameters.
    fn active<'s>(&'s self, state: &'s State) -> impl Iterator<Item = TokenId> + 's {
        state.open.iter().copied().chain(
            self.tokens
                .iter()
                .enumerate()
                .filter(|(_, token)| token.kind == TokenKind::Input)
                .map(|(id, _)| id as TokenId),
        )
    }

    /// Reject an access that overlaps an open access it is not derived from.
    #[allow(clippy::too_many_arguments)]
    fn check_conflicts(
        &self,
        state: &State,
        regions: &[AbsPlace],
        mode: BorrowKind,
        authority: &[TokenId],
        own: &[TokenId],
        kind: MemoryAccessKind,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        for token in self.active(state) {
            let data = &self.tokens[token as usize];
            if own.contains(&token)
                || authority.contains(&token)
                || data.zero_sized
                || mode == BorrowKind::Ref && data.mode == BorrowKind::Ref
                || !self.any_overlap(regions, &data.regions)
            {
                continue;
            }
            let held = match data.mode {
                BorrowKind::Mut => "a `mut`",
                BorrowKind::Ref => "a `ref`",
            };
            let what = match data.kind {
                TokenKind::Input => "parameter",
                TokenKind::Reservation | TokenKind::Grant => "projection",
                TokenKind::Access | TokenKind::Handle => "access",
            };
            let action = match regions.iter().find_map(|region| match region.base {
                Base::State(space) => Some(space),
                _ => None,
            }) {
                Some(space) => format!(
                    "this call may {} {}",
                    if mode == BorrowKind::Mut {
                        "write"
                    } else {
                        "read"
                    },
                    space.pretty()
                ),
                None => format!("cannot {} this place", verb(kind)),
            };
            let mut diag = self.diag(
                SemanticDiagnosticKind::AccessConflict,
                origin,
                format!("{action} while {held} {what} of it is open"),
            );
            diag.push_secondary(format!("{what} opened here"), self.span(data.origin));
            return Err(diag);
        }
        Ok(())
    }

    /// Check a token a statement opens against the accesses open around it.
    /// Checks a reservation `token` opens against the open accesses, apart
    /// from those `siblings`, the earlier components of its unsafe split.
    fn check_opened(
        &self,
        token: TokenId,
        state: &State,
        siblings: &[TokenId],
    ) -> Result<(), Diag<'db>> {
        let data = &self.tokens[token as usize];
        if data.zero_sized || data.kind != TokenKind::Reservation {
            return Ok(());
        }
        let authority = self.ancestors(&data.parents);
        let kind = match data.mode {
            BorrowKind::Mut => MemoryAccessKind::MutAccess,
            BorrowKind::Ref => MemoryAccessKind::Read,
        };
        let own: TokenSet = siblings.iter().copied().chain([token]).collect();
        self.check_conflicts(
            state,
            &data.regions,
            data.mode,
            &authority,
            &own,
            kind,
            data.origin,
        )
    }

    /// A call's interference footprint: its carrier arguments for the call's
    /// duration, its effects, and the state its external executions reach.
    /// `siblings` are the tokens of the earlier components of the call's
    /// unsafe split, which it never conflicts with.
    fn check_call(
        &mut self,
        callee: SemanticInstanceKey<'db>,
        args: &[NOperand],
        effect_args: &[NEffectArg<'db>],
        state: &State,
        siblings: &[TokenId],
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        // A closure is called as a function whose first parameter is its
        // environment.
        let owner = callee.owner(self.db);
        let func = match owner {
            BodyOwner::Func(func, mode) => Some((func, mode)),
            BodyOwner::Closure { .. } => None,
            _ => return Ok(()),
        };
        // The places each argument passes, as the callee takes them.
        let mut passed = Vec::new();
        for (position, arg) in args.iter().enumerate() {
            if callee
                .typed_body(self.db)
                .input_bindings()
                .get(position)
                .is_none()
            {
                continue;
            }
            let access = callee_input_access(self.db, callee, position);
            let arg_origin = operand_origin(*arg, origin);
            let direct = self.passed_to(arg.value, access);
            if direct.is_empty() {
                // An aggregate passed by value is still viewed in place: the
                // call reads the place it came from.
                let ty = self.body.values[arg.value.index()].ty;
                if access == Some(BindingAccess::View)
                    && !ty_is_snapshot(self.db, self.scope, ty, self.assumptions)
                {
                    let (regions, parents) = self.source_place(arg.value);
                    let authority = self.ancestors(&parents);
                    self.check_conflicts(
                        state,
                        &regions,
                        BorrowKind::Ref,
                        &authority,
                        siblings,
                        MemoryAccessKind::Read,
                        arg_origin,
                    )?;
                    passed.push((regions, BorrowKind::Ref, arg_origin));
                }
                continue;
            }
            let (mode, kind) = match access {
                Some(BindingAccess::Mut) => (BorrowKind::Mut, MemoryAccessKind::MutAccess),
                Some(BindingAccess::View | BindingAccess::Ref) | None => {
                    (BorrowKind::Ref, MemoryAccessKind::Read)
                }
            };
            let regions: Vec<AbsPlace> = direct
                .iter()
                .filter(|token| !self.tokens[**token as usize].zero_sized)
                .flat_map(|token| self.tokens[*token as usize].regions.clone())
                .collect();
            let authority = self.ancestors(&direct);
            // The callee may read its argument at once.
            if let Some(key) = self.carrier_key(arg.value)
                && let Some(hole) = self.hole(state, key)
            {
                return Err(self.moved_diag(
                    "this argument's referent is moved out",
                    arg_origin,
                    hole,
                ));
            }
            self.check_conflicts(
                state, &regions, mode, &authority, siblings, kind, arg_origin,
            )?;
            passed.push((regions, mode, arg_origin));
        }
        let mut footprints: Vec<_> = args
            .iter()
            .enumerate()
            .filter_map(|(position, arg)| self.capability_arg_footprint(callee, position, arg))
            .collect();
        for effect in effect_args {
            footprints.extend(self.effect_footprint(callee, effect));
        }
        if let Some((func, mode)) = func
            && let Some(access) = external_call_state_access(self.db, func, mode)
        {
            footprints.push(Footprint {
                regions: state_regions(),
                mode: access.borrow_kind(),
                parents: TokenSet::new(),
            });
        }
        for footprint in &footprints {
            let kind = match footprint.mode {
                BorrowKind::Mut => MemoryAccessKind::Write,
                BorrowKind::Ref => MemoryAccessKind::Read,
            };
            let authority = self.ancestors(&footprint.parents);
            self.check_conflicts(
                state,
                &footprint.regions,
                footprint.mode,
                &authority,
                siblings,
                kind,
                origin,
            )?;
        }
        // The callee takes its data parameters to be compatible with each
        // other and with its effects, so the parts of one call never
        // authorize each other: a `mut` one excludes any other of the same
        // place, whatever accesses they are reached through. Two effects may
        // alias, which the callee allows for.
        let conflict = |(lhs, lhs_mode): (&[AbsPlace], BorrowKind),
                        (rhs, rhs_mode): (&[AbsPlace], BorrowKind)| {
            (lhs_mode == BorrowKind::Mut || rhs_mode == BorrowKind::Mut)
                && self.any_overlap(lhs, rhs)
        };
        for (idx, (regions, mode, arg_origin)) in passed.iter().enumerate() {
            if let Some((_, _, other)) = passed[idx + 1..]
                .iter()
                .find(|(other, other_mode, _)| conflict((regions, *mode), (other, *other_mode)))
            {
                let mut diag = self.diag(
                    SemanticDiagnosticKind::AccessConflict,
                    *other,
                    "this argument and an earlier one of the same call reach one place, and one of them is `mut`".into(),
                );
                diag.push_secondary("the earlier argument".into(), self.span(*arg_origin));
                return Err(diag);
            }
            if footprints
                .iter()
                .any(|footprint| conflict((regions, *mode), (&footprint.regions, footprint.mode)))
            {
                return Err(self.diag(
                    SemanticDiagnosticKind::AccessConflict,
                    *arg_origin,
                    "this argument and an effect of the same call reach one place, and one of them is `mut`".into(),
                ));
            }
        }
        Ok(())
    }

    fn check_terminator(&mut self, block: NBlockId, state: &State) -> Result<(), Diag<'db>> {
        let terminator = &self.body.blocks[block.index()].terminator;
        let mut state = state.clone();
        if let Some(access) = terminator.kind.access(self.db, self.body)
            && let AccessTarget::Value { operand, path } = access.target
        {
            let consume = access.kind == MemoryAccessKind::Move;
            self.use_value(operand, path, &mut state, consume, terminator.origin)?;
        }
        for successor in terminator.kind.successors() {
            let mut edge = state.clone();
            for arg in &successor.args {
                let consume = arg.mode == ReadMode::Move;
                self.use_value(*arg, None, &mut edge, consume, terminator.origin)?;
            }
        }
        if let NTerminatorKind::Yield { value, .. } = terminator.kind {
            self.check_grant_creation(value, &state, terminator.origin)?;
        }
        // A frame a yield suspends may hold a parameter's hole it restores in
        // its slide, which ends in a return; the grant may not reach it.
        if matches!(terminator.kind, NTerminatorKind::Return(_))
            && let Some(moved) = state
                .moved
                .iter()
                .find(|(key, _)| matches!(key, MoveKey::Param(_)))
        {
            let origin = self
                .moved_at
                .get(moved)
                .copied()
                .unwrap_or(terminator.origin);
            let fix = if self.binds_by_value(origin) {
                IN_PLACE_FIX
            } else {
                ""
            };
            return Err(self.diag(
                SemanticDiagnosticKind::MoveConflict,
                origin,
                format!(
                    "a `mut` parameter moved out of here must be reinitialized before the function returns{fix}"
                ),
            ));
        }
        Ok(())
    }

    /// Creating a grant is an access of its component's mode on the yielded
    /// place, checked against every access the suspended frame retains: only
    /// the grant's own ancestors are exempt.
    fn check_grant_creation(
        &self,
        value: NOperand,
        state: &State,
        origin: SemOrigin<'db>,
    ) -> Result<(), Diag<'db>> {
        let BodyOwner::Func(func, mode) = self.instance.key(self.db).owner(self.db) else {
            return Ok(());
        };
        let Some(shape) = func.return_shape(self.db, mode) else {
            return Ok(());
        };
        let held = &self.values[value.value.index()];
        for (path, mode, _) in access_components(self.db, shape) {
            let own: TokenSet = held
                .iter()
                .filter(|(_, rest)| *rest == path)
                .map(|(token, _)| *token)
                .collect();
            let regions: Vec<AbsPlace> = own
                .iter()
                .flat_map(|token| self.tokens[*token as usize].regions.clone())
                .collect();
            let kind = match mode {
                BorrowKind::Mut => MemoryAccessKind::MutAccess,
                BorrowKind::Ref => MemoryAccessKind::Read,
            };
            let authority = self.ancestors(&own);
            let origin = operand_origin(value, origin);
            // The caller receives the referent whole: no part of it may be
            // moved out, under the grant's own key or an ancestor's.
            if let Some(hole) = state.moved.iter().find(|(key, hole)| match key {
                MoveKey::Access(token) => {
                    authority.contains(token)
                        && self.tokens[*token as usize].regions.iter().any(|region| {
                            let moved: Path = region.path.iter().chain(hole).cloned().collect();
                            regions.iter().any(|granted| {
                                granted.base == region.base && paths_overlap(&moved, &granted.path)
                            })
                        })
                }
                MoveKey::Param(param) => regions.iter().any(|granted| {
                    granted.base == Base::Param(*param) && paths_overlap(hole, &granted.path)
                }),
                MoveKey::Value(_) | MoveKey::Root(_) => false,
            }) {
                return Err(self.moved_diag(
                    "cannot yield an access while its referent is moved out",
                    origin,
                    hole,
                ));
            }
            self.check_conflicts(state, &regions, mode, &authority, &own, kind, origin)?;
        }
        Ok(())
    }

    /// A projection's yields: each component of every yield site names a
    /// place in one address space, its result space, and the access
    /// components of a split are disjoint. A component no yield grants has
    /// none: every place but a grant has a space, and a grant of a callee's
    /// component that no yield of the callee grants is never live.
    fn check_yields(&self) -> Result<Vec<Option<AddressSpace>>, Diag<'db>> {
        let BodyOwner::Func(func, mode) = self.instance.key(self.db).owner(self.db) else {
            return Ok(Vec::new());
        };
        let Some(shape) = func.return_shape(self.db, mode) else {
            return Ok(Vec::new());
        };
        let components = access_components(self.db, shape);
        let mut spaces: Vec<Option<(AddressSpace, SemOrigin<'db>)>> = vec![None; components.len()];
        for block in &self.body.blocks {
            let NTerminatorKind::Yield { value, .. } = &block.terminator.kind else {
                continue;
            };
            let origin = operand_origin(*value, block.terminator.origin);
            let held = &self.values[value.value.index()];
            let regions: Vec<Vec<AbsPlace>> = components
                .iter()
                .map(|(path, _, _)| {
                    held.iter()
                        .filter(|(_, rest)| rest == path)
                        .flat_map(|(token, _)| self.tokens[*token as usize].regions.clone())
                        .collect()
                })
                .collect();
            for (index, component) in regions.iter().enumerate() {
                for region in component {
                    let Some(space) = self.grant_space(region) else {
                        continue;
                    };
                    match spaces[index] {
                        Some((known, first)) if known != space => {
                            let mut diag = self.diag(
                                SemanticDiagnosticKind::TransportViolation,
                                origin,
                                format!(
                                    "this yield names {}, but another yield names {}",
                                    space.pretty(),
                                    known.pretty()
                                ),
                            );
                            diag.push_secondary(
                                format!("yields {}", known.pretty()),
                                self.span(first),
                            );
                            return Err(diag);
                        }
                        Some(_) => {}
                        None => spaces[index] = Some((space, origin)),
                    }
                }
                for (other, other_regions) in regions.iter().enumerate().skip(index + 1) {
                    if (components[index].1 == BorrowKind::Mut
                        || components[other].1 == BorrowKind::Mut)
                        && self.any_overlap(component, other_regions)
                    {
                        return Err(self.diag(
                            SemanticDiagnosticKind::AccessConflict,
                            origin,
                            "the components of a split must be disjoint when one of them is `mut`"
                                .into(),
                        ));
                    }
                }
            }
        }
        Ok(spaces
            .into_iter()
            .map(|space| space.map(|(space, _)| space))
            .collect())
    }
}

/// Whether `instance`, a callee, is a projection that reaches `target`
/// through projection calls. An instance is explored once, and an
/// instantiation past the limit has no callee, which bounds polymorphic
/// recursion.
fn reaches_instance<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    target: SemanticInstance<'db>,
    explored: &mut FxHashSet<SemanticInstance<'db>>,
) -> bool {
    if !instance.is_projection(db) || !explored.insert(instance) {
        return false;
    }
    instance == target
        || instance.callees(db).iter().any(|callee| {
            reaches_instance(db, SemanticInstance::new(db, callee.key), target, explored)
        })
}

/// The access components of a shape, by their path in its carrier, with
/// their mode and type. An opaque part is one abstract component, at its
/// mode, or `mut` when that is not known.
pub(super) fn access_components<'db>(
    db: &'db dyn HirAnalysisDb,
    shape: &Shape<'db>,
) -> Vec<(Path, BorrowKind, TyId<'db>)> {
    fn walk<'db>(
        db: &'db dyn HirAnalysisDb,
        shape: &Shape<'db>,
        path: &mut Path,
        out: &mut Vec<(Path, BorrowKind, TyId<'db>)>,
    ) {
        match shape {
            Shape::Owned(_) => {}
            Shape::Access(kind, ty) => out.push((path.clone(), *kind, *ty)),
            Shape::Opaque(term) => {
                let kind = match term.data(db) {
                    TyData::AssocTy(assoc) => assoc.mode.and_then(ShapeMode::kind),
                    _ => None,
                };
                out.push((
                    path.clone(),
                    kind.unwrap_or(BorrowKind::Mut),
                    shape.erased_ty(db),
                ));
            }
            Shape::Tuple(elems) => {
                for (field, elem) in elems.iter().enumerate() {
                    path.push(Step::Field(field as u16));
                    walk(db, elem, path, out);
                    path.pop();
                }
            }
            Shape::Sum {
                variant, payload, ..
            } => {
                path.push(Step::Variant {
                    variant: *variant,
                    field: 0,
                });
                walk(db, payload, path, out);
                path.pop();
            }
        }
    }
    let mut out = Vec::new();
    walk(db, shape, &mut Path::new(), &mut out);
    out
}

/// Every persistent and transient slot.
/// The values a call of `body` takes as arguments, and the values they
/// forward.
fn call_args(body: &NormalizedBody<'_>) -> FxHashSet<NValueId> {
    let mut args: FxHashSet<NValueId> = body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter_map(|statement| match &statement.kind {
            NStatementKind::Define {
                expr: NExpr::Call { args, .. },
                ..
            } => Some(args),
            _ => None,
        })
        .flatten()
        .map(|arg| arg.value)
        .collect();
    let mut pending: Vec<_> = args.iter().copied().collect();
    while let Some(value) = pending.pop() {
        if let Some((_, NExpr::Forward { src } | NExpr::StructuralRepack { value: src, .. })) =
            body.defining_expr(value)
            && args.insert(src.value)
        {
            pending.push(src.value);
        }
    }
    args
}

/// How the callee `callee` takes its input at `position`: a parameter, or a
/// product parameter's part; a closure's first input is its environment, in
/// the mode the closure is called with. `None` for an owned input.
pub(super) fn callee_input_access<'db>(
    db: &'db dyn HirAnalysisDb,
    callee: SemanticInstanceKey<'db>,
    position: usize,
) -> Option<BindingAccess> {
    let typed_body = callee.typed_body(db);
    typed_body.binding_access(*typed_body.input_bindings().get(position)?)
}

fn state_regions() -> Vec<AbsPlace> {
    [AddressSpace::Storage, AddressSpace::Transient]
        .map(|space| AbsPlace::new(Base::State(space)))
        .to_vec()
}

fn verb(kind: MemoryAccessKind) -> &'static str {
    match kind {
        MemoryAccessKind::Read => "read",
        MemoryAccessKind::MutAccess => "mutably access",
        MemoryAccessKind::Write => "write to",
        MemoryAccessKind::Move => "move out of",
    }
}

/// The more precise of two origins for a diagnostic: an expression, then a
/// statement, then the body.
fn specific_origin<'db>(first: SemOrigin<'db>, second: SemOrigin<'db>) -> SemOrigin<'db> {
    match (first, second) {
        (SemOrigin::Expr(_), _)
        | (SemOrigin::Stmt(_), SemOrigin::Body(_) | SemOrigin::Synthetic) => first,
        _ => second,
    }
}

/// Checks a body, returning its warnings and its result spaces.
/// What checking a body establishes for its callers and for runtime
/// lowering.
pub(super) struct CheckedBody<'db> {
    pub warnings: Vec<Diag<'db>>,
    /// A projection's result space per access component.
    pub spaces: Vec<Option<AddressSpace>>,
    pub drops: DropElaboration,
}

pub(super) fn check_body<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
) -> Result<CheckedBody<'db>, Diag<'db>> {
    let mut analysis = Analysis::new(db, instance, body, true);
    analysis.solve();
    let spaces = analysis.check()?;
    let drops = analysis.drop_elaboration()?;
    Ok(CheckedBody {
        warnings: analysis.discarded_writes(),
        spaces,
        drops,
    })
}

/// Adds `extra` to `parents`, once each.
fn add_parents(parents: &mut TokenSet, extra: &[TokenId]) {
    for &token in extra {
        if !parents.contains(&token) {
            parents.push(token);
        }
    }
}

fn drop_step(step: Step) -> DropStep {
    match step {
        Step::Field(field) => DropStep::Field(field),
        Step::Variant { variant, field } => DropStep::Variant { variant, field },
        Step::Index(index) => {
            DropStep::Index(index.expect("a part of a drop lies at constant indices"))
        }
    }
}
