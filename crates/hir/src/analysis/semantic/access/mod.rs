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
pub use self::implicit::dispatcher_callees;
pub(crate) use self::refine::{CallSiteRefinements, provisional_call_site_provider_refinements};
use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{
            CallSiteId, SemOrigin, SemanticCodeRegionRef, SemanticCodeRegionTarget,
            SemanticInstance, SemanticInstanceKey,
            diagnostics::{
                BlockedSemanticBody, SemanticDiagnostic, SemanticDiagnosticId,
                SemanticDiagnosticKind, SemanticDiagnosticSpan, SemanticNormalizationFailure,
                checker_name,
            },
            identity_semantic_instance_key,
            normalized::{
                NBlockId, NDataProjection, NDropTarget, NExpr, NPlace, NStatementId,
                NStatementKind, NormalizedBody, SemanticBodyAdmission, normalize_semantic_body,
                runtime_semantic_body_admission, semantic_body_admission,
            },
            panic_revert_edges, root_semantic_instance_key,
        },
        ty::{
            provider::AddressSpace,
            ty_check::{
                AssocConstBody, BodyOwner, ForLoopStep, ReturnProjectionStep, assoc_const_bodies_of,
            },
            ty_def::TyId,
        },
    },
    hir_def::{Contract, ExprId, ItemKind, StmtId, TopLevelMod},
};

use self::{
    check::access_components,
    implicit::UnprovedDrop,
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
        // The initializer of a constant that an implementation or a trait holds
        // is a body like a module constant's. It is checked where it is
        // written, whether or not anything reads it, by the enumeration that
        // the type checker's own pass over these bodies uses. The constants of
        // implementations and traits that the compiler generates (`#[event]`,
        // `#[error]`, `#[abi]`) are roots like written ones.
        owners.extend(
            assoc_const_bodies_of(db, *item)
                .into_iter()
                .filter(|constant| !constant.expected.has_invalid(db))
                .map(AssocConstBody::owner),
        );
        match item {
            ItemKind::Func(func) => owners.extend(BodyOwner::func_bodies(db, *func)),
            ItemKind::Const(const_) => owners.push(BodyOwner::Const(*const_)),
            ItemKind::Contract(contract) => owners.extend(contract_bodies(db, *contract)),
            // A static assertion is evaluated when its package is checked.
            ItemKind::StaticAssert(assert_) => owners.push(BodyOwner::AnonConstBody {
                body: assert_.condition(db),
                expected: TyId::bool(db),
            }),
            ItemKind::Mod(_)
            | ItemKind::Struct(_)
            | ItemKind::Enum(_)
            | ItemKind::Trait(_)
            | ItemKind::Impl(_)
            | ItemKind::ImplTrait(_)
            | ItemKind::TypeAlias(_)
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
    // The walk runs twice over a root that runs: along every call that the
    // typed body records, which is what the trust policy judges, and along the
    // calls that can run (`live`), which is what the drop diagnostic judges. A
    // call that no path reaches does not run, and neither does its drop; nor
    // does a function that no runtime root (`Func::is_runtime_root`: a test, a
    // manual contract root, an export or `main`), contract body or live call
    // reaches.
    let roots = |owners: Vec<BodyOwner<'db>>| -> VecDeque<_> {
        owners
            .into_iter()
            .flat_map(|owner| {
                let key = identity_semantic_instance_key(db, owner);
                let runs = match owner {
                    BodyOwner::Func(func, _) => func.is_runtime_root(db),
                    BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => true,
                    _ => false,
                };
                [false, true]
                    .into_iter()
                    .filter(move |&live| runs || !live)
                    .map(move |live| {
                        (
                            SemanticInstance::new(db, key),
                            None::<SemanticDiagnosticSpan>,
                            live,
                        )
                    })
            })
            .collect()
    };
    // A body that runs without a call (see `entry_kind`) runs when `fe test`
    // or a build runs it, though no call names it. One with an untrusted
    // contributor, a generated one, is a use of that contributor's code,
    // judged at the body.
    let entry_uses: Vec<_> = trust
        .iter()
        .flat_map(|_| {
            untrusted
                .iter()
                .copied()
                .filter_map(|owner| Some((owner, entry_kind(db, owner)?)))
        })
        .collect();
    let (mut trusted, untrusted) = (roots(trusted), roots(untrusted));
    // Warnings are reported once, for the module's own bodies.
    let own: FxHashSet<_> = trusted
        .iter()
        .chain(&untrusted)
        .map(|(instance, _, _)| *instance)
        .collect();
    let mut entry_calls = FxHashMap::default();
    for (owner, kind) in entry_uses {
        let call = SemanticDiagnosticSpan::Origin {
            owner,
            origin: SemOrigin::Body(owner),
        };
        entry_calls.insert(call.clone(), kind);
        let key = identity_semantic_instance_key(db, owner);
        trusted.push_back((SemanticInstance::new(db, key), Some(call.clone()), false));
        trusted.push_back((SemanticInstance::new(db, key), Some(call), true));
    }
    let mut seen = FxHashSet::default();
    let mut seen_diags = FxHashSet::default();
    let mut seen_inherited_sites = FxHashSet::default();
    let mut seen_untrusted = FxHashSet::default();
    let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = Vec::new();
    for (mut pending, judge) in [(trusted, trust.as_ref()), (untrusted, None)] {
        while let Some((instance, call, live)) = pending.pop_front() {
            if !seen.insert((instance, live)) {
                continue;
            }
            let owner = instance.key(db).owner(db);
            if let (Some(trust), Some(call)) = (judge, &call)
                && !live
                && let Some((site, author)) = trust.untrusted_site(db, instance)
                && seen_untrusted.insert((author, call.clone()))
            {
                diags.push(Box::new(UntrustedUnsafeUse {
                    root: top_mod.ingot(db),
                    call: call.clone(),
                    entry: entry_calls.get(call).copied(),
                    user: owner,
                    site,
                    author,
                }));
            }
            match body_check(db, instance) {
                SemanticAccessCheckResult::Ok { warnings, .. } => {
                    if own.contains(&instance) && !live {
                        diags.extend(
                            warnings
                                .iter()
                                .map(|diag| Box::new(*diag) as Box<dyn DiagnosticVoucher + 'db>),
                        );
                    }
                    let edges = run_edges(db, instance, live);
                    for (drop, origin) in edges.failures.iter().filter(|_| live) {
                        let diag =
                            unproved_drop_diagnostic(db, instance, drop, *origin, call.as_ref());
                        if seen_diags.insert(diag) {
                            diags.push(Box::new(diag));
                        }
                    }
                    // A live walk follows the calls that a statement that runs
                    // makes, when the body says which those are.
                    let running = if live {
                        running_call_sites(db, instance)
                    } else {
                        None
                    };
                    let runs = |site| {
                        running
                            .as_ref()
                            .is_none_or(|running| running.contains(&site))
                    };
                    let calls =
                        instance
                            .call_sites(db)
                            .iter()
                            .enumerate()
                            .filter_map(|(idx, site)| {
                                let expr = ExprId::new(idx);
                                Some((
                                    site.as_ref()?.callee?.key,
                                    CallSiteId::Expr(expr),
                                    SemOrigin::Expr(expr),
                                ))
                            });
                    let loops = instance
                        .for_loop_call_sites(db)
                        .iter()
                        .enumerate()
                        .flat_map(|(idx, sites)| {
                            sites.iter().flat_map(move |sites| {
                                ForLoopStep::ALL.into_iter().filter_map(move |step| {
                                    let stmt = StmtId::new(idx);
                                    Some((
                                        sites.site(step).callee?.key,
                                        CallSiteId::ForLoop(stmt, step),
                                        SemOrigin::Stmt(stmt),
                                    ))
                                })
                            })
                        });
                    let at = |origin| SemanticDiagnosticSpan::Origin { owner, origin };
                    let callees: Vec<_> = calls
                        .chain(loops)
                        .filter(|&(_, site, _)| runs(site))
                        .map(|(key, _, origin)| (key, origin))
                        .chain(edges.callees)
                        .map(|(key, origin)| (key, call.clone().unwrap_or_else(|| at(origin))))
                        .collect();
                    pending.extend(
                        callees
                            .into_iter()
                            .map(|(key, call)| (SemanticInstance::new(db, key), Some(call), live)),
                    );
                }
                SemanticAccessCheckResult::Blocked(body) if !live => blocked.push(body.clone()),
                SemanticAccessCheckResult::Err(diag) if !live => {
                    // The body of a default is walked once for each
                    // implementation that inherits it, and its text is one:
                    // an error in it is reported once, where the trait wrote
                    // it.
                    let inherited_site = {
                        let diag = diag.diag(db);
                        match &diag.primary.span {
                            SemanticDiagnosticSpan::Origin {
                                owner: owner @ BodyOwner::InheritedConst { .. },
                                origin,
                            } => owner.body(db).map(|body| {
                                (diag.kind, body, *origin, diag.primary.message.clone())
                            }),
                            _ => None,
                        }
                    };
                    if seen_diags.insert(*diag)
                        && inherited_site.is_none_or(|site| seen_inherited_sites.insert(site))
                    {
                        diags.push(Box::new(*diag));
                    }
                }
                SemanticAccessCheckResult::Blocked(_) | SemanticAccessCheckResult::Err(_) => {}
            }
        }
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
/// records, each with the statement, expression, pattern or body that makes it
/// run. This is the one list of such edges; the trust walk follows only it and
/// the call sites:
/// - for each constant the body reads, in an expression or a pattern, the
///   constant's initializer, which evaluating the constant runs;
/// - for each call of `panic_with_value`, the `Encode` implementation that
///   builds the revert data of the value (`panic_payload_revert_key`);
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
/// `failures`. With `live`, the statements that no path of the body runs
/// contribute nothing (see `unrun_statements`).
fn run_edges<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    live: bool,
) -> RunEdges<'db> {
    let root = |owner| root_semantic_instance_key(db, owner).ok();
    if let Some((_, contract)) = instance.contract_metadata(db) {
        let owner = instance.key(db).owner(db);
        return RunEdges {
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
    // The trust walk follows every constant and every codec callee that the
    // typed body records. A walk over what runs follows the constants, and
    // takes the codec methods of an entry from the statements that use it.
    callees.extend(if live {
        instance.constant_dependencies_with_origins(db)
    } else {
        instance.implicit_dependencies_with_origins(db)
    });
    // A panic that no statement which runs makes builds no revert data.
    let running = if live {
        running_call_sites(db, instance)
    } else {
        None
    };
    callees.extend(
        panic_revert_edges(db, instance)
            .into_iter()
            .filter(|(_, origin)| match (origin, &running) {
                (SemOrigin::Expr(expr), Some(running)) => {
                    running.contains(&CallSiteId::Expr(*expr))
                }
                _ => true,
            }),
    );
    let SemanticBodyAdmission::Ready(admitted) = semantic_body_admission(db, instance) else {
        return RunEdges { callees, failures };
    };
    let body = admitted.body(db);
    let unrun = if live {
        unrun_statements(db, instance, body)
    } else {
        FxHashSet::default()
    };
    for statement in body
        .blocks
        .iter()
        .flat_map(|block| &block.statements)
        .filter(|statement| !unrun.contains(&statement.id))
    {
        if let NStatementKind::Drop { target } = &statement.kind {
            let origin = implicit::drop_origin(owner, body, statement.origin, target);
            let drops = implicit::drop_callees(db, instance, body, statement.id, target);
            callees.extend(drops.keys.into_iter().map(|key| (key, origin)));
            failures.extend(drops.unproved.into_iter().map(|drop| (drop, origin)));
        }
        let mut visit = |place: &NPlace<'db>, usage: PlaceUse| {
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
                // A lane that the place ends at is read or written by the
                // method for that use; one that a projection continues from
                // is held as a copy and stored back.
                let usage = if idx + 1 == place.path.len() {
                    usage
                } else {
                    PlaceUse::Address
                };
                let codec = instance
                    .state_index_lanes(db, collection)
                    .and_then(|_| instance.entry_codec_callees(db, *entry).ok())
                    .into_iter()
                    .flat_map(|[get, set]| match usage {
                        PlaceUse::Read => vec![get],
                        PlaceUse::Write => vec![set],
                        PlaceUse::Address => vec![get, set],
                    });
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
                match expr {
                    NExpr::Load { place, .. } => visit(place, PlaceUse::Read),
                    _ => expr.for_each_place_operand(|place| visit(place, PlaceUse::Address)),
                }
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
            NStatementKind::Store { destination, .. } => visit(destination, PlaceUse::Write),
            NStatementKind::Drop {
                target: NDropTarget::Place(destination),
            } => visit(destination, PlaceUse::Address),
            NStatementKind::End { .. }
            | NStatementKind::CheckIndex { .. }
            | NStatementKind::Drop {
                target: NDropTarget::Value(_),
            } => {}
        }
    }
    RunEdges { callees, failures }
}

/// How a statement uses a place that ends at, or runs through, an entry of a
/// collection that packs its elements into lanes. Runtime lowering decides the
/// codec method of the entry by the use: a value read calls the getter
/// (`read_lane`), a store the setter (`write_lane`), and an address, a view, an
/// effect argument, a drop or a place that a projection continues from holds a
/// copy of the lane, which it reads with the getter and stores back with the
/// setter (`hold_lane`).
#[derive(Clone, Copy)]
enum PlaceUse {
    Read,
    Write,
    Address,
}

/// The statements of `body`, a body of `instance`, that no run executes: those
/// of blocks that no path reaches, those after a call that never returns and
/// those after the anchor of a trap.
/// It is the control flow that runtime lowering emits. Where the flow is not
/// known, every statement runs.
fn unrun_statements<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    body: &NormalizedBody<'db>,
) -> FxHashSet<NStatementId> {
    let Some(flow) = semantic_executable_control_flow(db, instance) else {
        return FxHashSet::default();
    };
    let mut unrun = FxHashSet::default();
    for (block, executable) in body.blocks.iter().zip(&flow.0) {
        let from = match executable {
            ExecutableBlock::Unreachable => 0,
            // A trap ends the block after its anchor, as a call that does not
            // return does; with no anchor nothing of the block runs.
            ExecutableBlock::Diverges(anchor) | ExecutableBlock::TrapsAfter(Some(anchor)) => block
                .statements
                .iter()
                .position(|statement| statement.source == Some(*anchor))
                .map_or(block.statements.len(), |at| at + 1),
            ExecutableBlock::TrapsAfter(None) => 0,
            ExecutableBlock::Continues(_) => block.statements.len(),
        };
        unrun.extend(
            block.statements[from..]
                .iter()
                .map(|statement| statement.id),
        );
    }
    unrun
}

/// The call sites of `instance` that a statement which runs makes, or `None`
/// where the body does not say which statements run. The typed body records
/// every call written, also one after a `return`, which has no statement
/// that runs.
fn running_call_sites<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
) -> Option<FxHashSet<CallSiteId>> {
    let SemanticBodyAdmission::Ready(admitted) = semantic_body_admission(db, instance) else {
        return None;
    };
    semantic_executable_control_flow(db, instance)?;
    let body = admitted.body(db);
    let unrun = unrun_statements(db, instance, body);
    Some(
        body.blocks
            .iter()
            .flat_map(|block| &block.statements)
            .filter(|statement| !unrun.contains(&statement.id))
            .filter_map(|statement| match &statement.kind {
                NStatementKind::Define {
                    expr: NExpr::Call { call_site, .. },
                    ..
                } => Some(*call_site),
                _ => None,
            })
            .collect(),
    )
}

/// The result of `run_edges`.
struct RunEdges<'db> {
    callees: Vec<(SemanticInstanceKey<'db>, SemOrigin<'db>)>,
    failures: Vec<(UnprovedDrop<'db>, SemOrigin<'db>)>,
}

/// A value of `drop.ty` is destroyed at `origin`, but no `Drop` implementation
/// of it has an ordinary proof. A choice made by a `with` around the value
/// does not reach its drop, so a bound that only a chosen implementation
/// satisfies is not proved there. `call` is the call in the module's own code
/// that instantiated the body with that type, when the body is not the
/// module's own; it is where the error is reported, and the drop is the
/// secondary label.
fn unproved_drop_diagnostic<'db>(
    db: &'db dyn HirAnalysisDb,
    instance: SemanticInstance<'db>,
    drop: &UnprovedDrop<'db>,
    origin: SemOrigin<'db>,
    call: Option<&SemanticDiagnosticSpan<'db>>,
) -> SemanticDiagnosticId<'db> {
    let ty = drop.ty.pretty_print(db);
    let needs = drop.unmet.as_ref().map_or_else(
        || "an implementation that no ordinary proof gives".to_string(),
        |unmet| {
            format!(
                "`{}`, which no ordinary implementation proves",
                unmet.needs()
            )
        },
    );
    let reason = format!(
        "its `Drop` implementation needs {needs} (only one that a `with` chose does); a drop does not read a `with`"
    );
    let owner = instance.key(db).owner(db);
    let site = SemanticDiagnosticSpan::Origin { owner, origin };
    let diagnostic = match call {
        Some(call) => {
            let mut diagnostic = SemanticDiagnostic::new(
                instance,
                SemanticDiagnosticKind::UnprovedDrop,
                format!(
                    "this call reaches `{}`, which destroys `{ty}`; {reason}",
                    checker_name(db, instance)
                ),
                call.clone(),
            );
            diagnostic.push_secondary(format!("`{ty}` is destroyed here"), site);
            diagnostic
        }
        None => SemanticDiagnostic::new(
            instance,
            SemanticDiagnosticKind::UnprovedDrop,
            format!("`{ty}` is destroyed here, and {reason}"),
            site,
        ),
    };
    SemanticDiagnosticId::new(db, diagnostic)
}

/// Why a body runs without a call written in the package being checked.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum EntryKind {
    /// A `#[test]` function, which `fe test` runs.
    Test,
    /// An `#[unsafe(export)]` function, which a native object or a
    /// WebAssembly module defines for its caller.
    Export,
    /// The package's `main` entry.
    Main,
    /// A contract's `init` or one of its `recv` arms.
    Contract,
}

/// The reason `owner` runs without a call written in its package, if it does:
/// a test, an export, the `main` entry, or a contract body. Provider output
/// may only fill these in where the answer is the same predicate that code
/// generation uses, so a generated one is judged at the body like a call.
fn entry_kind<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Option<EntryKind> {
    match owner {
        BodyOwner::Func(func, _) if func.is_test(db) => Some(EntryKind::Test),
        BodyOwner::Func(func, _) if func.is_exported(db) => Some(EntryKind::Export),
        BodyOwner::Func(func, _) if func.is_main_entry(db) => Some(EntryKind::Main),
        BodyOwner::ContractInit { .. } | BodyOwner::ContractRecvArm { .. } => {
            Some(EntryKind::Contract)
        }
        _ => None,
    }
}
