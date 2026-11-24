//! Access checking of normalized semantic bodies.
//!
//! Mutable value semantics: references exist only as parameter modes, named
//! access bindings and projection results. The checker tracks the accesses a
//! body opens (borrows, views and projection sessions) between their opening
//! and the `end` that liveness elaboration placed after their last use, and
//! enforces one rule: overlapping accesses conflict unless both read. Calls
//! are checked against signatures only: a call's interference footprint is
//! its data arguments plus the footprints of its declared effects. See
//! `docs/architecture/access-checker.md`.
mod check;
mod control;
mod domain;
mod implicit;
mod place;
mod refine;
mod space;
mod trust;

use std::{collections::VecDeque, fmt, iter};

use common::diagnostics::CompleteDiagnostic;
use cranelift_entity::EntityRef;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::SalsaValue;

pub use self::control::{
    ExecutableBlock, ExecutableControlFlow, semantic_executable_control_flow, semantic_may_return,
};
pub(crate) use self::domain::keys_may_share;
pub(crate) use self::refine::{CallSiteRefinements, provisional_call_site_provider_refinements};
use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            SemOrigin, SemanticCodeRegionRef, SemanticCodeRegionTarget, SemanticInstance,
            SemanticInstanceKey,
            diagnostics::{
                BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticId,
                SemanticDiagnosticKind, SemanticDiagnosticSpan, SemanticNormalizationFailure,
            },
            identity_semantic_instance_key,
            normalized::{
                NBlockId, NDataProjection, NDropTarget, NExpr, NPlace, NStatementId,
                NStatementKind, NormalizedBody, SemanticBodyAdmission, normalize_semantic_body,
                runtime_semantic_body_admission, semantic_body_admission,
            },
            root_semantic_instance_key,
        },
        ty::{
            provider::AddressSpace,
            ty_check::{BodyOwner, ReturnProjectionStep},
            ty_def::TyId,
        },
    },
    hir_def::{Contract, ExprId, HirIngot, ItemKind, StmtId, TopLevelMod},
};

use self::{
    check::access_components,
    place::Step,
    trust::{UnsafeTrust, UntrustedUnsafeUse},
};

#[derive(Clone, Debug, PartialEq, Eq, SalsaValue)]
pub enum SemanticAccessCheckResult<'db> {
    /// Checked, with the body's warnings, for a projection the address
    /// space each access component yields (its result spaces), and what its
    /// drops destroy.
    Ok {
        warnings: Vec<SemanticDiagnosticId<'db>>,
        spaces: Vec<Option<AddressSpace>>,
        drops: DropElaboration,
    },
    Blocked(BlockedSemanticBody<'db>),
    Err(SemanticDiagnosticId<'db>),
}

/// What a body's drops destroy, as the moves before each decide.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, SalsaValue)]
pub struct DropElaboration {
    /// The parts each `Drop` statement destroys, by statement; a drop with
    /// nothing initialized destroys none.
    pub drops: Vec<(NStatementId, Vec<DropPart>)>,
    /// Each flag's value where the body begins: whether the path it tracks
    /// is initialized there.
    pub flags: Vec<bool>,
    /// Where the moves and initializations of a flagged path happen.
    pub flag_updates: Vec<DropFlagUpdate>,
}

impl DropElaboration {
    /// This elaboration of `checked`, keyed by the statements of `body`,
    /// another normalization of the same raw body, as constant folding makes
    /// for runtime lowering. Folding renumbers normalized statements, but
    /// each keeps the raw statement it comes from, and the blocks stay the
    /// same.
    fn rekeyed(&self, checked: &NormalizedBody<'_>, body: &NormalizedBody<'_>) -> Self {
        // Where each raw statement's normalized statements begin in `body`,
        // and its last one.
        let mut before = FxHashMap::default();
        let mut last = FxHashMap::default();
        for (block, data) in body.blocks.iter().enumerate() {
            let mut point = DropFlagPoint::BlockStart(NBlockId::new(block));
            for statement in &data.statements {
                if let Some(source) = statement.source {
                    before.entry(source).or_insert(point);
                    last.insert(source, statement.id);
                }
                point = DropFlagPoint::After(statement.id);
            }
        }
        // The point after each checked statement, in `body`: after its raw
        // statement, or, for one normalization introduced, before the next
        // raw statement of its block.
        let mut after = FxHashMap::default();
        for (block, data) in checked.blocks.iter().enumerate() {
            let mut next = DropFlagPoint::BeforeTerminator(NBlockId::new(block));
            for statement in data.statements.iter().rev() {
                match statement
                    .source
                    .and_then(|source| Some((before.get(&source)?, last[&source])))
                {
                    Some((&first, last)) => {
                        after.insert(statement.id, DropFlagPoint::After(last));
                        next = first;
                    }
                    None => {
                        after.insert(statement.id, next);
                    }
                }
            }
        }
        let mut drops: Vec<_> = self
            .drops
            .iter()
            .map(|(statement, parts)| {
                let DropFlagPoint::After(statement) = after[statement] else {
                    unreachable!("a drop is a raw statement that folding keeps")
                };
                (statement, parts.clone())
            })
            .collect();
        drops.sort_by_key(|(statement, _)| *statement);
        let flag_updates = self
            .flag_updates
            .iter()
            .map(|update| DropFlagUpdate {
                at: match update.at {
                    DropFlagPoint::After(statement) => after[&statement],
                    point => point,
                },
                ..*update
            })
            .collect();
        Self {
            drops,
            flags: self.flags.clone(),
            flag_updates,
        }
    }

    pub fn parts(&self, statement: NStatementId) -> &[DropPart] {
        self.drops
            .binary_search_by_key(&statement, |(id, _)| *id)
            .map_or(&[], |index| &self.drops[index].1)
    }
}

/// A part of a drop's target to destroy, by its path from the target. It is
/// destroyed where all its flags hold: one per path on its way that only
/// some paths through the body move.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct DropPart {
    pub path: Vec<DropStep>,
    pub flags: Vec<u32>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum DropStep {
    Field(u16),
    /// A variant's field, which the value holds only as that variant.
    Variant {
        variant: u16,
        field: u16,
    },
    Index(usize),
}

/// A flagged path becomes initialized, or is moved, at `at`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct DropFlagUpdate {
    pub at: DropFlagPoint,
    pub flag: u32,
    pub initialized: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum DropFlagPoint {
    After(NStatementId),
    /// The start of a block, whose parameters are defined there.
    BlockStart(NBlockId),
    /// The end of a block, whose terminator moves its operand or, on every
    /// edge, an argument.
    BeforeTerminator(NBlockId),
}

/// What `instance`'s drops destroy, keyed by the statements of the body
/// runtime lowering uses.
#[salsa::tracked]
pub fn runtime_drop_elaboration<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<DropElaboration> {
    let elaboration = drop_elaboration(db, instance)?;
    let (SemanticBodyAdmission::Ready(checked), SemanticBodyAdmission::Ready(runtime)) = (
        semantic_body_admission(db, instance),
        runtime_semantic_body_admission(db, instance),
    ) else {
        return None;
    };
    Some(if checked == runtime {
        elaboration.clone()
    } else {
        elaboration.rekeyed(checked.body(db), runtime.body(db))
    })
}

/// What `instance`'s drops destroy, once its body is checked.
pub fn drop_elaboration<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<&'db DropElaboration> {
    match body_check(db, instance) {
        SemanticAccessCheckResult::Ok { drops, .. } => Some(drops),
        SemanticAccessCheckResult::Blocked(_) | SemanticAccessCheckResult::Err(_) => None,
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SemanticAnalysisError<'db> {
    Blocked(BlockedSemanticBody<'db>),
    Diagnostic(CompleteDiagnostic),
}

impl SemanticAnalysisError<'_> {
    pub fn diagnostic(&self) -> Option<&CompleteDiagnostic> {
        match self {
            Self::Blocked(_) => None,
            Self::Diagnostic(diag) => Some(diag),
        }
    }
}

impl fmt::Display for SemanticAnalysisError<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Blocked(blocked) => write!(
                formatter,
                "semantic body is blocked by upstream causes: {:?}",
                blocked.causes
            ),
            Self::Diagnostic(diag) => formatter.write_str(&diag.message),
        }
    }
}

/// A projection's result space, per access component, as its body infers
/// it: callers depend on this exported fact, never on the body.
pub(crate) fn projection_result_spaces<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> &'db [Option<AddressSpace>] {
    match body_check(db, instance) {
        SemanticAccessCheckResult::Ok { spaces, .. } => spaces,
        SemanticAccessCheckResult::Blocked(_) | SemanticAccessCheckResult::Err(_) => &[],
    }
}

/// A projection's result space per access component, with the steps from its
/// return to the component: `None` where no yield grants the component, as
/// for the payload of a variant the projection never returns, and for an
/// instance whose body is not checked.
pub fn projection_component_spaces<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Vec<(Vec<ReturnProjectionStep>, Option<AddressSpace>)> {
    let Some(shape) = instance.return_shape(db) else {
        return Vec::new();
    };
    let spaces = projection_result_spaces(db, instance);
    access_components(db, shape)
        .into_iter()
        .enumerate()
        .map(|(index, (path, _, _))| {
            let steps = path
                .iter()
                .map(|step| match *step {
                    Step::Field(field) => ReturnProjectionStep::Field(field),
                    Step::Variant { variant, field } => {
                        ReturnProjectionStep::VariantField { variant, field }
                    }
                    Step::Index(_) | Step::Entry { .. } => {
                        unreachable!("an access component lies at fields and variant fields")
                    }
                })
                .collect();
            (steps, spaces.get(index).copied().flatten())
        })
        .collect()
}

// A projection's check reads its projection callees' result spaces, so
// recursion through projections, which the check rejects, is a cycle.
#[salsa::tracked(cycle_initial = body_check_cycle_initial)]
fn body_check<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> SemanticAccessCheckResult<'db> {
    let artifacts = match normalize_semantic_body(db, instance) {
        Ok(artifacts) => artifacts,
        Err(SemanticNormalizationFailure::Blocked(blocked)) => {
            return SemanticAccessCheckResult::Blocked(blocked);
        }
        Err(
            SemanticNormalizationFailure::Rejected(diag)
            | SemanticNormalizationFailure::InternalFailure(diag),
        ) => return SemanticAccessCheckResult::Err(SemanticDiagnosticId::new(db, diag)),
    };
    match check::check_body(db, instance, &artifacts.body) {
        Ok(check::CheckedBody {
            warnings,
            spaces,
            drops,
        }) => SemanticAccessCheckResult::Ok {
            warnings: warnings
                .into_iter()
                .map(|diag| SemanticDiagnosticId::new(db, diag))
                .collect(),
            spaces,
            drops,
        },
        Err(diag) => SemanticAccessCheckResult::Err(SemanticDiagnosticId::new(db, diag)),
    }
}

fn body_check_cycle_initial<'db>(
    _: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    _: SemanticInstance<'db>,
) -> SemanticAccessCheckResult<'db> {
    SemanticAccessCheckResult::Ok {
        warnings: Vec::new(),
        spaces: Vec::new(),
        drops: DropElaboration::default(),
    }
}

pub fn semantic_access_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> &'db SemanticAccessCheckResult<'db> {
    body_check(db, instance)
}

pub fn check_semantic_accesses<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Result<(), SemanticAnalysisError<'db>> {
    match body_check(db, instance) {
        SemanticAccessCheckResult::Ok { .. } => Ok(()),
        SemanticAccessCheckResult::Blocked(body) => {
            Err(SemanticAnalysisError::Blocked(body.clone()))
        }
        SemanticAccessCheckResult::Err(diag) => {
            Err(SemanticAnalysisError::Diagnostic(diag.to_complete(db)))
        }
    }
}

pub struct SemanticAccessAnalysisPass;

impl ModuleAnalysisPass for SemanticAccessAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        collect_semantic_access_diagnostic_vouchers(db, top_mod)
    }
}

/// Checks each item's identity instance and, transitively, the concrete
/// instances it calls. From a trust root's module, the walk also reports
/// calls that reach an untrusted dependency's unsafe code (`trust`).
pub fn collect_semantic_access_diagnostic_vouchers<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
    collect_semantic_access_with_blocked(db, top_mod, &mut Vec::new())
}

/// Returns blocked bodies reached by the module's semantic access analysis.
/// Drivers use their owning modules to recover otherwise suppressed HIR errors.
pub fn collect_blocked_semantic_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<BlockedSemanticBody<'db>> {
    let mut blocked = Vec::new();
    collect_semantic_access_with_blocked(db, top_mod, &mut blocked);
    blocked
}

fn collect_semantic_access_with_blocked<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    blocked: &mut Vec<BlockedSemanticBody<'db>>,
) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
    let mut owners = Vec::new();
    for item in top_mod
        .all_items(db)
        .iter()
        .filter(|item| item.top_mod(db) == top_mod)
    {
        owners.extend(BodyOwner::const_predicates_of(db, *item));
        match item {
            ItemKind::Func(func) => owners.extend(BodyOwner::func_bodies(db, *func)),
            ItemKind::Const(const_) => owners.push(BodyOwner::Const(*const_)),
            ItemKind::Contract(contract) => owners.extend(contract_bodies(db, *contract)),
            ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
            | ItemKind::StaticAssert(_)
            | ItemKind::Use(_)
            | ItemKind::TopMod(_)
            | ItemKind::Body(_) => {}
        }
    }
    let trust = UnsafeTrust::for_root(db, top_mod);
    // Each instance carries the call in the module's own code that reached
    // it, `None` for the module's own bodies. Code that a provider of an
    // untrusted package wrote is that package's, although preparation
    // published it into this module. It is checked like the module's own
    // bodies, but a call is judged only when it comes from code that a
    // trusted package wrote, and is judged at that call: the walk from the
    // trusted bodies comes first, and the untrusted ones are checked after it,
    // for what it did not reach.
    let (trusted, untrusted): (Vec<_>, Vec<_>) = owners.into_iter().partition(|owner| {
        trust
            .as_ref()
            .is_none_or(|trust| trust.trusts_authors_of(db, *owner))
    });
    let roots = |owners: Vec<BodyOwner<'db>>| -> VecDeque<_> {
        owners
            .into_iter()
            .map(|owner| {
                let key = identity_semantic_instance_key(db, owner);
                (
                    SemanticInstance::new(db, key),
                    None::<SemanticDiagnosticSpan>,
                )
            })
            .collect()
    };
    // A `#[test]` function runs when `fe test` runs, though no call names it.
    // One with an untrusted contributor, a generated one, is a use of that
    // contributor's code, judged at the function.
    let test_uses: Vec<_> = trust
        .iter()
        .flat_map(|_| {
            untrusted
                .iter()
                .copied()
                .filter(|owner| is_test(db, *owner))
        })
        .collect();
    let (mut trusted, untrusted) = (roots(trusted), roots(untrusted));
    // Warnings are reported once, for the module's own bodies.
    let own: FxHashSet<_> = trusted
        .iter()
        .chain(&untrusted)
        .map(|(instance, _)| *instance)
        .collect();
    let mut test_calls = FxHashSet::default();
    for owner in test_uses {
        let call = SemanticDiagnosticSpan::Origin {
            owner,
            origin: SemOrigin::Body(owner),
        };
        test_calls.insert(call.clone());
        let key = identity_semantic_instance_key(db, owner);
        trusted.push_back((SemanticInstance::new(db, key), Some(call)));
    }
    let mut seen = FxHashSet::default();
    let mut seen_diags = FxHashSet::default();
    let mut seen_untrusted = FxHashSet::default();
    let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = Vec::new();
    for (mut pending, judge) in [(trusted, trust.as_ref()), (untrusted, None)] {
        while let Some((instance, call)) = pending.pop_front() {
            if !seen.insert(instance) {
                continue;
            }
            let owner = instance.key(db).owner(db);
            if let (Some(trust), Some(call)) = (judge, &call)
                && let Some((site, author)) = trust.untrusted_site(db, owner)
                && seen_untrusted.insert((author, call.clone()))
            {
                diags.push(Box::new(UntrustedUnsafeUse {
                    root: top_mod.ingot(db),
                    call: call.clone(),
                    runs_as_test: test_calls.contains(call),
                    user: owner,
                    site,
                    author,
                }));
            }
            match body_check(db, instance) {
                SemanticAccessCheckResult::Ok { warnings, .. } => {
                    if own.contains(&instance) {
                        diags.extend(
                            warnings
                                .iter()
                                .map(|diag| Box::new(*diag) as Box<dyn DiagnosticVoucher + 'db>),
                        );
                    }
                    let generated = generated_callees(db, instance);
                    for diag in generated.failures {
                        if seen_diags.insert(diag) {
                            diags.push(Box::new(diag));
                        }
                    }
                    let callees: Vec<_> = match call {
                        Some(call) => instance
                            .callees(db)
                            .iter()
                            .map(|callee| callee.key)
                            .chain(generated.callees.into_iter().map(|(key, _)| key))
                            .map(|key| (key, call.clone()))
                            .collect(),
                        None => {
                            let at = |origin| SemanticDiagnosticSpan::Origin { owner, origin };
                            let calls = instance.call_sites(db).iter().enumerate().filter_map(
                                |(idx, site)| {
                                    Some((
                                        site.as_ref()?.callee?.key,
                                        at(SemOrigin::Expr(ExprId::new(idx))),
                                    ))
                                },
                            );
                            let loops = instance
                                .for_loop_call_sites(db)
                                .iter()
                                .enumerate()
                                .flat_map(|(idx, sites)| {
                                    sites.iter().flat_map(move |sites| {
                                        sites.sites.iter().filter_map(move |site| {
                                            Some((
                                                site.callee?.key,
                                                at(SemOrigin::Stmt(StmtId::new(idx))),
                                            ))
                                        })
                                    })
                                });
                            let generated = generated
                                .callees
                                .into_iter()
                                .map(|(key, origin)| (key, at(origin)));
                            calls.chain(loops).chain(generated).collect()
                        }
                    };
                    pending.extend(
                        callees
                            .into_iter()
                            .map(|(key, call)| (SemanticInstance::new(db, key), Some(call))),
                    );
                }
                SemanticAccessCheckResult::Blocked(body) => blocked.push(body.clone()),
                SemanticAccessCheckResult::Err(diag) => {
                    if seen_diags.insert(*diag) {
                        diags.push(Box::new(*diag));
                    }
                }
            }
        }
    }
    // An `unsafe impl` without a function has nothing for a call to reach, so it
    // is judged where it is declared, once for the program of a trust root.
    if let Some(trust) = &trust
        && top_mod == top_mod.ingot(db).root_mod(db)
    {
        diags.extend(
            trust
                .untrusted_unsafe_impls(db, top_mod.ingot(db))
                .into_iter()
                .map(|unsafe_impl| Box::new(unsafe_impl) as Box<dyn DiagnosticVoucher + 'db>),
        );
    }
    diags
}

/// A contract's bodies: its `init` and each recv arm.
fn contract_bodies<'db>(
    db: &'db dyn HirAnalysisDb,
    contract: Contract<'db>,
) -> impl Iterator<Item = BodyOwner<'db>> {
    let arms = contract
        .recvs(db)
        .data(db)
        .iter()
        .enumerate()
        .flat_map(move |(recv_idx, recv)| {
            (0..recv.arms.data(db).len()).map(move |arm_idx| BodyOwner::ContractRecvArm {
                contract,
                recv_idx: recv_idx as u32,
                arm_idx: arm_idx as u32,
            })
        });
    iter::once(BodyOwner::ContractInit { contract }).chain(arms)
}

/// The instances the compiler runs for `instance` that no semantic call site
/// records, each with the statement or body that makes it run:
/// - for a contract's `init` and each `recv` arm, the dispatcher's decode of
///   the arguments and encode of the return value (`implicit`);
/// - for each drop, the `Drop::drop` implementations that drop elaboration
///   inserts for the parts it destroys (`implicit`);
/// - for each entry of a place it names, the collection's
///   `StateIndex::locate` and, for a collection that packs its elements
///   into lanes, its codec's `get` and `set`;
/// - the bodies of a contract whose code it embeds to create it: a
///   high-level contract's, named by a `Contract` metadata builtin's type
///   argument, or a manual contract root named by a code region.
///
/// A drop of a type whose `Drop` implementation has no proof is reported in
/// `failures`.
fn generated_callees<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> GeneratedCallees<'db> {
    let root = |owner| root_semantic_instance_key(db, owner).ok();
    if let Some((_, contract)) = instance.contract_metadata(db) {
        let owner = instance.key(db).owner(db);
        return GeneratedCallees {
            callees: contract_bodies(db, contract)
                .filter_map(root)
                .map(|key| (key, SemOrigin::Body(owner)))
                .collect(),
            failures: Vec::new(),
        };
    }
    let owner = instance.key(db).owner(db);
    let mut callees: Vec<_> = implicit::dispatcher_callees(db, owner)
        .into_iter()
        .map(|key| (key, SemOrigin::Body(owner)))
        .collect();
    let mut failures = Vec::new();
    let SemanticBodyAdmission::Ready(admitted) = semantic_body_admission(db, instance) else {
        return GeneratedCallees { callees, failures };
    };
    let body = admitted.body(db);
    for statement in body.blocks.iter().flat_map(|block| &block.statements) {
        if let NStatementKind::Drop { target } = &statement.kind {
            let origin = implicit::drop_origin(owner, body, statement.origin, target);
            let drops = implicit::drop_callees(db, instance, body, statement.id, target);
            callees.extend(drops.keys.into_iter().map(|key| (key, origin)));
            failures.extend(
                drops
                    .unproved
                    .into_iter()
                    .map(|ty| unproved_drop_diagnostic(db, instance, ty, origin)),
            );
        }
        let mut visit = |place: &NPlace<'db>| {
            for (idx, projection) in place.path.iter().enumerate() {
                let NDataProjection::Entry(_, entry) = projection else {
                    continue;
                };
                let Some(collection) = body.place_prefix_ty(db, place, idx) else {
                    continue;
                };
                let collection = instance.normalized_ty(db, collection);
                let locate = instance.call_sites(db)[entry.index()]
                    .as_ref()
                    .and_then(|site| site.callee)
                    .map(|callee| callee.key);
                let codec = instance
                    .state_index_lanes(db, collection)
                    .and_then(|_| instance.entry_codec_callees(db, *entry).ok())
                    .into_iter()
                    .flatten();
                callees.extend(
                    locate
                        .into_iter()
                        .chain(codec)
                        .map(|key| (key, statement.origin)),
                );
            }
        };
        match &statement.kind {
            NStatementKind::Define { expr, .. } => {
                expr.for_each_place_operand(&mut visit);
                if let NExpr::CodeRegionRef { region }
                | NExpr::CodeRegionOffset {
                    target: SemanticCodeRegionTarget::Resolved(region),
                }
                | NExpr::CodeRegionLen {
                    target: SemanticCodeRegionTarget::Resolved(region),
                } = expr
                {
                    let SemanticCodeRegionRef::ManualContractRoot { func } = region;
                    callees.extend(
                        root(BodyOwner::Func(*func, None)).map(|key| (key, statement.origin)),
                    );
                }
            }
            NStatementKind::Store { destination, .. }
            | NStatementKind::Drop {
                target: NDropTarget::Place(destination),
            } => visit(destination),
            NStatementKind::End { .. }
            | NStatementKind::CheckIndex { .. }
            | NStatementKind::Drop {
                target: NDropTarget::Value(_),
            } => {}
        }
    }
    GeneratedCallees { callees, failures }
}

/// The result of `generated_callees`.
struct GeneratedCallees<'db> {
    callees: Vec<(SemanticInstanceKey<'db>, SemOrigin<'db>)>,
    failures: Vec<SemanticDiagnosticId<'db>>,
}

/// A value of `ty` is destroyed at `origin`, but no `Drop` implementation of
/// it has an ordinary proof. A choice made by a `with` around the value does
/// not reach its drop, so a bound that only a chosen implementation satisfies
/// is not proved there.
fn unproved_drop_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    ty: TyId<'db>,
    origin: SemOrigin<'db>,
) -> SemanticDiagnosticId<'db> {
    SemanticDiagnosticId::new(
        db,
        SemanticDiagnostic::new(
            instance,
            SemanticDiagnosticKind::UnprovedDrop,
            format!(
                "`{}` is destroyed here, but its `Drop` implementation cannot be proved without the implementations that a `with` chose; a drop does not read a `with`",
                ty.pretty_print(db),
            ),
            SemanticDiagnosticSpan::Origin {
                owner: instance.key(db).owner(db),
                origin,
            },
        ),
    )
}

/// Whether `owner` is a function that `fe test` runs: one with `#[test]`.
fn is_test<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> bool {
    matches!(owner, BodyOwner::Func(func, _)
        if ItemKind::from(func)
            .attrs(db)
            .is_some_and(|attrs| attrs.has_attr(db, "test")))
}
