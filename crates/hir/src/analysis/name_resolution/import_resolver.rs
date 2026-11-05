//! This module implements import and export resolution for HIR.
//!
//! Uses settle in rounds, as imports do in rustc. A round resolves the pending
//! uses against the imports of the settled ones, and a use settles unless a
//! lookup on its path could still change: a pending named import of the name
//! could shadow what the lookup found, or, if it found nothing or only a
//! lexical parent's name, a pending glob import, or a pending use in a scope
//! that a glob import reads, could bring the name in. What a settled use
//! imports doesn't change, so no resolution rests on one that a later import
//! shadows, and the result doesn't depend on the order of the uses.
//!
//! When no use can settle, the next round forces the cycles of uses that wait
//! on each other and on no other use: their lookups take the forced uses they
//! wait on to import nothing. The forced uses that resolve settle, or if none
//! does, the ones that fail. Last, each use is resolved again against the
//! settled imports, to report the ones whose resolution changed after they
//! settled, at the first lookup that changed: a glob import that settled later
//! made a name ambiguous, or a forced use imports something after all.
use std::{cell::RefCell, mem};

use common::{indexmap::IndexMap, ingot::Ingot};
use petgraph::{algo::tarjan_scc, graphmap::DiGraphMap};
use rustc_hash::{FxBuildHasher, FxHashMap, FxHashSet};
use salsa::SalsaValue;
use thin_vec::ThinVec;

use super::{
    EarlyNameQueryId,
    diagnostics::ImportDiag,
    name_resolver::{
        NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind, NameResolutionError,
        NameResolver, QueryDirective,
    },
    visibility_checker::is_use_visible,
};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{
        HirIngot, IdentId, Partial, Use, UsePathSegment, prim_ty::PrimTy, scope_graph::ScopeId,
    },
};

/// The names a glob import brings into its scope.
type GlobNames<'db> = FxHashMap<IdentId<'db>, Vec<NameRes<'db>>>;

/// A failed lookup: the index of the path segment, and the error.
type SegmentError<'db> = (usize, NameResolutionError<'db>);

pub(crate) struct ImportResolver<'db> {
    db: &'db dyn HirAnalysisDb,

    /// The ingot that is being resolved.
    ingot: Ingot<'db>,

    /// The imports of the settled uses, which lookups see.
    imports: ResolvedImports<'db>,

    /// The bucket that each settled use other than a glob imports.
    buckets: FxHashMap<Use<'db>, NameResBucket<'db>>,

    /// The scope that each settled glob imports from.
    glob_targets: FxHashMap<Use<'db>, ScopeId<'db>>,

    /// The named uses that import each name into each scope.
    named_uses: IndexMap<(ScopeId<'db>, IdentId<'db>), Vec<Use<'db>>>,

    /// The uses that haven't settled.
    pending: FxHashSet<Use<'db>>,

    /// The scopes of the ingot whose imports lookups have read since this was
    /// last taken.
    reads: RefCell<FxHashSet<ScopeId<'db>>>,
}

/// What a use imports.
#[derive(PartialEq, Eq)]
enum Imported<'db> {
    Glob(GlobNames<'db>),
    Bucket(NameResBucket<'db>),
}

impl<'db> Imported<'db> {
    fn none(db: &'db dyn HirAnalysisDb, use_: Use<'db>) -> Self {
        if use_.is_glob(db) {
            Self::Glob(GlobNames::default())
        } else {
            Self::Bucket(NameResBucket::default())
        }
    }
}

/// What the path of a use resolves to.
#[derive(PartialEq, Eq)]
enum Resolved<'db> {
    /// The scope that a glob imports from.
    Glob(ScopeId<'db>),
    /// What a use other than a glob imports.
    Bucket(NameResBucket<'db>),
}

/// Why the resolution of the path of a use stopped.
enum Stop<'db> {
    Failed(SegmentError<'db>),
    /// The lookup of a segment could still change, because of the pending
    /// uses. Each is `true` if it's a named import of the name in the scope
    /// that it's looked up in, or a lexical parent.
    Blocked(Vec<(Use<'db>, bool)>),
}

/// The uses that lookups take to import nothing, to break cycles of uses
/// that wait on each other.
#[derive(Default)]
struct Forced<'db> {
    uses: FxHashSet<Use<'db>>,
    /// Whether that includes the named imports of the name in the scope that
    /// it's looked up in, which shadow what glob imports and lexical parents
    /// bring in.
    named: bool,
}

impl<'db> Forced<'db> {
    fn ignores(&self, (use_, named): (Use<'db>, bool)) -> bool {
        self.uses.contains(&use_) && (self.named || !named)
    }
}

impl<'db> From<SegmentError<'db>> for Stop<'db> {
    fn from(err: SegmentError<'db>) -> Self {
        Self::Failed(err)
    }
}

impl<'db> ImportResolver<'db> {
    pub(crate) fn new(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        Self {
            db,
            ingot,
            imports: ResolvedImports::default(),
            buckets: FxHashMap::default(),
            glob_targets: FxHashMap::default(),
            named_uses: IndexMap::default(),
            pending: FxHashSet::default(),
            reads: RefCell::default(),
        }
    }

    pub(crate) fn resolve_imports(mut self) -> (ResolvedImports<'db>, Vec<ImportDiag<'db>>) {
        let db = self.db;
        let uses: Vec<_> = self
            .ingot
            .module_tree(db)
            .all_modules()
            .flat_map(|top_mod| top_mod.scope_graph(db).unresolved_uses.iter().copied())
            .collect();
        let index: FxHashMap<_, _> = uses
            .iter()
            .enumerate()
            .map(|(idx, &use_)| (use_, idx))
            .collect();
        for &use_ in &uses {
            let scope = use_scope(db, use_);
            if use_.is_glob(db) {
                // Making every glob's entry up front fixes the order in which
                // lookups visit globs.
                self.imports
                    .glob_resolved
                    .entry(scope)
                    .or_default()
                    .imported
                    .insert(use_, GlobNames::default());
            } else if let Some(name) = use_.imported_name(db) {
                self.named_uses.entry((scope, name)).or_default().push(use_);
            }
        }
        self.pending = uses.iter().copied().collect();

        let mut outcomes = vec![Ok(()); uses.len()];
        // The scopes that the path of each use led through when it settled,
        // the pending uses that each blocked use waits on, and the blocked
        // uses that wait on each use.
        let mut traces = vec![Vec::new(); uses.len()];
        let mut waits = vec![Vec::new(); uses.len()];
        let mut waiters = vec![Vec::new(); uses.len()];
        // The settled globs that read the imports of each scope, and the ones
        // whose names are out of date.
        let mut readers: FxHashMap<ScopeId<'db>, FxHashSet<usize>> = FxHashMap::default();
        let mut stale: FxHashSet<usize> = FxHashSet::default();
        let mut ready: FxHashSet<usize> = (0..uses.len()).collect();
        // The uses forced since a use last settled.
        let mut forced = Forced::default();
        loop {
            // The names that glob imports bring in only grow, so updating them
            // in any order reaches the same fixed point.
            while !stale.is_empty() {
                for idx in mem::take(&mut stale) {
                    let use_ = uses[idx];
                    let names = self.glob_names(use_, self.glob_targets[&use_]);
                    for scope in self.reads.take() {
                        readers.entry(scope).or_default().insert(idx);
                    }
                    let scope = use_scope(db, use_);
                    let old = &mut self.imports.glob_resolved[&scope].imported[&use_];
                    if *old != names {
                        *old = names;
                        stale.extend(readers.get(&scope).into_iter().flatten());
                    }
                }
            }

            let (mut settled, mut failed) = (Vec::new(), Vec::new());
            for idx in mem::take(&mut ready) {
                let use_ = uses[idx];
                if !self.pending.contains(&use_) {
                    continue;
                }
                let (trace, resolved) = self.resolve(use_, &forced);
                self.reads.take();
                match resolved {
                    Err(Stop::Blocked(blockers)) => {
                        for (blocker, _) in &blockers {
                            waiters[index[blocker]].push(idx);
                        }
                        waits[idx] = blockers;
                    }
                    // A forced use that fails might have resolved had it
                    // waited, so it only settles if no forced use resolves.
                    Err(Stop::Failed(_)) if forced.uses.contains(&use_) => {
                        failed.push((idx, trace, resolved));
                    }
                    resolved => settled.push((idx, trace, resolved)),
                }
            }

            if !settled.is_empty() {
                ready.extend(failed.iter().map(|&(idx, ..)| idx));
            } else if !failed.is_empty() {
                settled = failed;
            } else if self.pending.is_empty() {
                break;
            } else {
                // Each pending use waits on another, so some cycles of uses
                // that wait on each other wait on no other use. The next round
                // forces their uses, first taking only the glob imports they
                // wait on to import nothing, and then the named imports too.
                let mut graph = DiGraphMap::<_, (), FxBuildHasher>::default();
                for &use_ in &self.pending {
                    graph.add_node(use_);
                    for &blocker in &waits[index[&use_]] {
                        if !forced.uses.contains(&use_) || !forced.ignores(blocker) {
                            graph.add_edge(use_, blocker.0, ());
                        }
                    }
                }
                let sinks: Vec<_> = tarjan_scc(&graph)
                    .into_iter()
                    .filter(|scc| {
                        (scc.iter().flat_map(|&use_| graph.neighbors(use_)))
                            .all(|next| scc.contains(&next))
                    })
                    .flatten()
                    .collect();
                if sinks.iter().all(|use_| forced.uses.contains(use_)) {
                    forced.named = true;
                }
                forced.uses.extend(&sinks);
                ready.extend(sinks.iter().map(|use_| index[use_]));
                continue;
            }
            forced = Forced::default();

            for (idx, trace, resolved) in settled {
                let use_ = uses[idx];
                self.pending.remove(&use_);
                traces[idx] = trace;
                let imported = match resolved {
                    Ok(Resolved::Glob(target)) => {
                        self.glob_targets.insert(use_, target);
                        stale.insert(idx);
                        Imported::none(db, use_)
                    }
                    Ok(Resolved::Bucket(bucket)) => Imported::Bucket(bucket),
                    Err(Stop::Failed(err)) => {
                        outcomes[idx] = Err(err);
                        Imported::none(db, use_)
                    }
                    Err(Stop::Blocked(_)) => unreachable!(),
                };
                self.replace(use_, imported);
                stale.extend(readers.get(&use_scope(db, use_)).into_iter().flatten());
                ready.extend(mem::take(&mut waiters[idx]));
            }
        }

        // Each use is resolved again, to report the ones whose resolution
        // changed after they settled, at the first lookup that changed. A
        // lookup that finds what it didn't when the use failed leaves the
        // error as it was.
        for (idx, &use_) in uses.iter().enumerate() {
            let settled = match &outcomes[idx] {
                Err(err) => Err(err.clone()),
                Ok(()) if use_.is_glob(db) => Ok(Resolved::Glob(self.glob_targets[&use_])),
                Ok(()) => Ok(Resolved::Bucket(self.buckets[&use_].clone())),
            };
            let (trace, resolved) = self.resolve(use_, &Forced::default());
            self.reads.take();
            let resolved = resolved.map_err(|stop| match stop {
                Stop::Failed(err) => err,
                Stop::Blocked(_) => unreachable!(),
            });
            let old = &traces[idx];
            if resolved == settled && trace == *old {
                continue;
            }
            let res = |scope| {
                NameRes::new_from_scope(
                    scope,
                    NameDomain::from_scope(db, scope),
                    NameDerivation::Def,
                )
            };
            let at = old.iter().zip(&trace).position(|(old, new)| old != new);
            outcomes[idx] = match (at, resolved, settled) {
                (Some(at), ..) => Err((
                    at,
                    NameResolutionError::Ambiguous(ThinVec::from([res(old[at]), res(trace[at])])),
                )),
                _ if trace.len() > old.len() => continue,
                (None, Err(err), _) => Err(err),
                (None, Ok(_), Err(_)) => continue,
                (None, Ok(new), Ok(old)) => {
                    let buckets = [old, new]
                        .into_iter()
                        .filter_map(|resolved| match resolved {
                            Resolved::Bucket(bucket) => Some(bucket),
                            Resolved::Glob(_) => None,
                        });
                    let mut cands = ThinVec::new();
                    for res in
                        buckets.flat_map(|bucket| bucket.iter_ok().cloned().collect::<Vec<_>>())
                    {
                        if cands.iter().all(|cand: &NameRes| cand.kind != res.kind) {
                            cands.push(res);
                        }
                    }
                    Err((trace.len(), NameResolutionError::Ambiguous(cands)))
                }
            };
        }

        let mut diags: Vec<_> = uses
            .iter()
            .zip(outcomes)
            .filter(|(use_, _)| !use_.is_synthetic_use(db))
            .filter_map(|(&use_, outcome)| {
                let (idx, err) = outcome.err()?;
                self.diag(use_, idx, err)
            })
            .collect();
        diags.extend(self.conflicts());

        for &use_ in &uses {
            if !use_.is_glob(db)
                && use_.imported_name(db).is_none()
                && let Some(bucket) = self.buckets.remove(&use_)
                && !bucket.is_empty()
            {
                let unnamed = &mut self.imports.unnamed_resolved;
                unnamed.entry(use_scope(db, use_)).or_default().push(bucket);
            }
        }

        (self.imports, diags)
    }

    /// Resolves the path of `use_` against the imports of the settled uses,
    /// and returns the scopes that it leads through. What a use other than a
    /// glob imports is withdrawn, so it can't name itself. Lookups take the
    /// uses that `forced` ignores to import nothing.
    fn resolve(
        &mut self,
        use_: Use<'db>,
        forced: &Forced<'db>,
    ) -> (Vec<ScopeId<'db>>, Result<Resolved<'db>, Stop<'db>>) {
        let db = self.db;
        let own = (!use_.is_glob(db)).then(|| self.replace(use_, Imported::none(db, use_)));
        let mut trace = Vec::new();
        let resolved = self
            .resolve_base(use_, forced, &mut trace)
            .and_then(|target| {
                if use_.is_glob(db) {
                    return Ok(Resolved::Glob(target));
                }
                let segments = use_path(db, use_)?;
                let idx = segments.len() - 1;
                let domain = NameDomain::TYPE | NameDomain::VALUE;
                let mut bucket = self.lookup(use_, target, idx, &segments[idx], domain, forced)?;
                bucket.set_derivation(NameDerivation::NamedImported(use_));
                Ok(Resolved::Bucket(bucket))
            });
        if let Some(own) = own {
            self.replace(use_, own);
        }
        (trace, resolved)
    }

    /// Collects the names that the glob `use_` brings in from `target`. The
    /// names that a pending named import in `target` could shadow aren't
    /// brought in from `target`'s glob imports.
    fn glob_names(&self, use_: Use<'db>, target: ScopeId<'db>) -> GlobNames<'db> {
        let db = self.db;
        let scope = use_scope(db, use_);
        let mut names = NameResolver::new(db, self).collect_all_resolutions_for_glob(target, scope);
        names.retain(|&name, resolutions| {
            if self
                .pending_named(target, name)
                .any(|named| is_use_visible(db, scope, named))
            {
                resolutions
                    .retain(|res| !matches!(res.derivation, NameDerivation::GlobImported(_)));
            }
            // Sorted, so that equal contents compare equal whatever order the
            // resolutions were found in.
            resolutions.sort_unstable_by_key(|res| res.kind);
            for res in resolutions.iter_mut() {
                res.derivation = NameDerivation::GlobImported(use_);
            }
            !resolutions.is_empty()
        });
        names
    }

    /// Replaces what `use_` imports, and returns what it imported before.
    fn replace(&mut self, use_: Use<'db>, imported: Imported<'db>) -> Imported<'db> {
        let scope = use_scope(self.db, use_);
        match imported {
            Imported::Glob(names) => {
                let old = &mut self.imports.glob_resolved[&scope].imported[&use_];
                Imported::Glob(mem::replace(old, names))
            }
            Imported::Bucket(bucket) => {
                let old = self.buckets.insert(use_, bucket).unwrap_or_default();
                if let Some(name) = use_.imported_name(self.db) {
                    self.merge_named(scope, name);
                }
                Imported::Bucket(old)
            }
        }
    }

    /// Merges the buckets of the named uses importing `name` into `scope`.
    fn merge_named(&mut self, scope: ScopeId<'db>, name: IdentId<'db>) {
        let mut merged = NameResBucket::default();
        for use_ in &self.named_uses[&(scope, name)] {
            if let Some(bucket) = self.buckets.get(use_) {
                merged.merge(self.db, bucket);
            }
        }
        let named = self.imports.named_resolved.entry(scope).or_default();
        if merged.bucket.is_empty() {
            named.remove(&name);
        } else {
            named.insert(name, merged);
        }
    }

    /// Returns the pending named uses that import `name` into `scope`.
    fn pending_named(
        &self,
        scope: ScopeId<'db>,
        name: IdentId<'db>,
    ) -> impl Iterator<Item = Use<'db>> + '_ {
        let uses = self.named_uses.get(&(scope, name)).into_iter().flatten();
        uses.copied().filter(|use_| self.pending.contains(use_))
    }

    /// Resolves all segments of the path of `use_` but the last one into
    /// `trace`, and returns the scope that the last segment is resolved in.
    fn resolve_base(
        &self,
        use_: Use<'db>,
        forced: &Forced<'db>,
        trace: &mut Vec<ScopeId<'db>>,
    ) -> Result<ScopeId<'db>, Stop<'db>> {
        let segments = use_path(self.db, use_)?;
        segments[..segments.len() - 1].iter().enumerate().try_fold(
            use_scope(self.db, use_),
            |target, (idx, segment)| {
                let bucket = self.lookup(use_, target, idx, segment, NameDomain::TYPE, forced)?;
                let res = bucket
                    .pick(NameDomain::TYPE)
                    .as_ref()
                    .unwrap_or_else(|_| bucket.iter_ok().next().unwrap());
                if res.is_mod(self.db) || res.is_enum(self.db) {
                    let scope = res.scope().unwrap();
                    trace.push(scope);
                    Ok(scope)
                } else {
                    Err(Stop::Failed((
                        idx,
                        NameResolutionError::InvalidPathSegment(res.clone()),
                    )))
                }
            },
        )
    }

    /// Looks up the `idx`th segment of the path of `use_` in `scope`, for what
    /// it names in `domain`. Only the first segment is looked up in lexical
    /// parents and external ingots.
    fn lookup(
        &self,
        use_: Use<'db>,
        scope: ScopeId<'db>,
        idx: usize,
        segment: &Partial<UsePathSegment<'db>>,
        domain: NameDomain,
        forced: &Forced<'db>,
    ) -> Result<NameResBucket<'db>, Stop<'db>> {
        let db = self.db;
        let use_scope = use_scope(db, use_);
        let name = segment
            .to_opt()
            .and_then(UsePathSegment::ident)
            .ok_or((idx, NameResolutionError::Invalid))?;
        // A prelude use names an ingot dependency, whatever the names of the
        // scope it's in.
        if idx == 0
            && use_.is_synthetic_use(db)
            && let Some((_, ingot)) = self
                .ingot
                .resolved_external_ingots(db)
                .iter()
                .find(|(ingot_name, _)| *ingot_name == name)
        {
            let root = ScopeId::from_item(ingot.root_mod(db).into());
            return Ok(
                NameRes::new_from_scope(root, NameDomain::TYPE, NameDerivation::External).into(),
            );
        }
        let directive = if idx == 0 {
            QueryDirective::new()
        } else {
            QueryDirective::new().disallow_lex().disallow_external()
        };
        let query = EarlyNameQueryId::new(db, name, scope, directive);
        let mut bucket = NameResolver::new(db, self).resolve_query(query);

        let mut invisible = None;
        bucket.bucket.retain(|(_, res)| match res {
            Ok(res) if !res.is_importable() => false,
            Ok(res) if !res.is_visible(db, use_scope) => {
                if invisible.is_none() {
                    invisible = res.derived_from(db);
                }
                false
            }
            _ => true,
        });

        // How the name was found in each domain, if it was.
        let found: Option<Vec<_>> = domain
            .iter()
            .map(|domain| match bucket.pick(domain) {
                Ok(res) => Some(&res.derivation),
                Err(NameResolutionError::Ambiguous(cands)) => Some(&cands[0].derivation),
                Err(_) => None,
            })
            .collect();
        let found = found.and_then(|found| found.into_iter().min_by(|a, b| a.cmp_precedence(b)));
        let mut blockers = Vec::new();
        self.blockers(use_, scope, name, idx == 0, found, &mut blockers);
        blockers.retain(|&blocker| !forced.ignores(blocker));
        if !blockers.is_empty() {
            return Err(Stop::Blocked(blockers));
        }

        if let Some((_, err)) = bucket.errors().find(|&(err_domain, err)| {
            domain.intersects(err_domain)
                && !matches!(
                    err,
                    NameResolutionError::NotFound | NameResolutionError::Invalid
                )
        }) {
            return Err(Stop::Failed((idx, err.clone())));
        }
        if bucket.is_empty() {
            let err = invisible.map_or(NameResolutionError::NotFound, |span| {
                NameResolutionError::Invisible(Some(span))
            });
            return Err(Stop::Failed((idx, err)));
        }

        // A name that a glob imports shadows an external ingot or a builtin
        // type of the same name, so a use path that starts with it is
        // ambiguous.
        let glob_shadows_external = idx == 0
            && bucket.pick(NameDomain::TYPE).as_ref().is_ok_and(|res| {
                matches!(res.derivation.origin(), NameDerivation::GlobImported(_))
                    && !res.is_external(db, self.ingot)
            })
            && (PrimTy::from_name(db, name).is_some()
                || self
                    .ingot
                    .resolved_external_ingots(db)
                    .iter()
                    .any(|(ingot_name, _)| *ingot_name == name));
        if glob_shadows_external {
            let err = NameResolutionError::Ambiguous(ThinVec::new());
            return Err(Stop::Failed((idx, err)));
        }
        Ok(bucket)
    }

    /// Collects the pending uses other than `use_` that could still change a
    /// lookup of `name` in `scope`, given what it found with the lowest
    /// precedence, if anything: the named imports of the name, which shadow
    /// glob imports and lexical parents, and the glob imports and the pending
    /// uses of the scopes they read, which shadow lexical parents. A glob
    /// import that brings in the name of an ingot or builtin type makes it
    /// ambiguous, which the last pass reports, so they don't wait for that.
    /// Each is `true` if it's a named import.
    fn blockers(
        &self,
        use_: Use<'db>,
        scope: ScopeId<'db>,
        name: IdentId<'db>,
        allow_lex: bool,
        found: Option<&NameDerivation<'db>>,
        blockers: &mut Vec<(Use<'db>, bool)>,
    ) {
        let shadows = |derivation: NameDerivation<'db>| {
            found.is_none_or(|found| derivation.cmp_precedence(found).is_gt())
        };
        if shadows(NameDerivation::NamedImported(use_)) {
            let named = self
                .pending_named(scope, name)
                .filter(|&named| named != use_);
            blockers.extend(named.map(|named| (named, true)));
        }
        let parent_found = match found {
            None => None,
            Some(NameDerivation::Lex(found)) => Some(&**found),
            Some(found @ (NameDerivation::External | NameDerivation::Prim)) => Some(found),
            Some(_) => return,
        };
        if matches!(found, None | Some(NameDerivation::Lex(_))) {
            self.glob_blockers(use_, scope, None, name, &mut FxHashSet::default(), blockers);
        }
        if allow_lex && let Some(parent) = scope.lex_parent(self.db) {
            self.blockers(use_, parent, name, true, parent_found, blockers);
        }
    }

    /// Collects the pending glob imports of `scope` and, through its settled
    /// ones, the pending uses of the scopes they read that could bring `name`
    /// in. `from` is the scope that imports `scope`'s names, if not `scope`
    /// itself.
    fn glob_blockers(
        &self,
        use_: Use<'db>,
        scope: ScopeId<'db>,
        from: Option<ScopeId<'db>>,
        name: IdentId<'db>,
        visited: &mut FxHashSet<(ScopeId<'db>, Option<ScopeId<'db>>)>,
        blockers: &mut Vec<(Use<'db>, bool)>,
    ) {
        if scope.ingot(self.db) != self.ingot || !visited.insert((scope, from)) {
            return;
        }
        let visible = |other: Use<'db>| {
            other != use_ && from.is_none_or(|from| is_use_visible(self.db, from, other))
        };
        if from.is_some() {
            let named = self
                .pending_named(scope, name)
                .filter(|&named| visible(named));
            blockers.extend(named.map(|named| (named, false)));
        }
        let globs = self.imports.glob_resolved.get(&scope).into_iter();
        for &glob in globs.flat_map(|globs| globs.imported.keys()) {
            if !visible(glob) {
                continue;
            }
            if self.pending.contains(&glob) {
                blockers.push((glob, false));
            } else if let Some(&target) = self.glob_targets.get(&glob) {
                self.glob_blockers(use_, target, Some(scope), name, visited, blockers);
            }
        }
    }

    /// Returns a diagnostic for each name that several named uses import into
    /// the same scope and domain.
    fn conflicts(&self) -> impl Iterator<Item = ImportDiag<'db>> + '_ {
        self.named_uses
            .iter()
            .filter(|(_, uses)| uses.len() > 1)
            .filter_map(|((scope, name), _)| {
                let bucket = self.imports.named_resolved.get(scope)?.get(name)?;
                let mut spans = ThinVec::new();
                for (_, err) in bucket.errors() {
                    let NameResolutionError::Ambiguous(cands) = err else {
                        continue;
                    };
                    for span in cands.iter().filter_map(|cand| cand.derived_from(self.db)) {
                        if !spans.contains(&span) {
                            spans.push(span);
                        }
                    }
                }
                (!spans.is_empty()).then(|| ImportDiag::Conflict(*name, spans))
            })
    }

    fn diag(
        &self,
        use_: Use<'db>,
        idx: usize,
        err: NameResolutionError<'db>,
    ) -> Option<ImportDiag<'db>> {
        let db = self.db;
        let span = use_.span().path().segment(idx).into();
        let ident = use_path(db, use_).ok()?.get(idx)?.to_opt()?.ident()?;
        Some(match err {
            NameResolutionError::NotFound => ImportDiag::NotFound(span, ident),
            // The parser has already reported an invalid segment.
            NameResolutionError::Invalid => return None,
            NameResolutionError::Ambiguous(cands) => ImportDiag::ambiguous(db, span, ident, cands),
            NameResolutionError::InvalidPathSegment(res) => {
                ImportDiag::InvalidPathSegment(span, ident, res.kind.name_span(db))
            }
            NameResolutionError::Invisible(def_span) => {
                ImportDiag::Invisible(span, ident, def_span)
            }
            NameResolutionError::Conflict(ident, spans) => ImportDiag::Conflict(ident, spans),
        })
    }
}

/// The scope that `use_` imports into.
fn use_scope<'db>(db: &'db dyn HirAnalysisDb, use_: Use<'db>) -> ScopeId<'db> {
    ScopeId::from_item(use_.into()).lex_parent(db).unwrap()
}

/// The segments of the path of `use_`, of which there is at least one.
fn use_path<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: Use<'db>,
) -> Result<&'db [Partial<UsePathSegment<'db>>], SegmentError<'db>> {
    use_.path(db)
        .to_opt()
        .map(|path| path.data(db).as_slice())
        .filter(|segments| !segments.is_empty())
        .ok_or((0, NameResolutionError::Invalid))
}

#[derive(Clone, Debug, Default, PartialEq, Eq, SalsaValue)]
pub struct ResolvedImports<'db> {
    pub named_resolved: IndexMap<ScopeId<'db>, NamedImportSet<'db>>,
    pub glob_resolved: IndexMap<ScopeId<'db>, GlobImportSet<'db>>,
    pub unnamed_resolved: IndexMap<ScopeId<'db>, Vec<NameResBucket<'db>>>,
}

pub(super) trait Importer<'db> {
    /// Returns the resolved imports of the ingot containing `scope`.
    fn imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> &'a ResolvedImports<'db>;

    fn named_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a NamedImportSet<'db>> {
        self.imports(db, scope).named_resolved.get(&scope)
    }

    fn glob_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a GlobImportSet<'db>> {
        self.imports(db, scope).glob_resolved.get(&scope)
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct DefaultImporter;

impl<'db> Importer<'db> for DefaultImporter {
    fn imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> &'a ResolvedImports<'db> {
        &super::resolve_imports(db, scope.ingot(db)).1
    }
}

impl<'db> Importer<'db> for ImportResolver<'db> {
    fn imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> &'a ResolvedImports<'db> {
        if scope.ingot(db) == self.ingot {
            self.reads.borrow_mut().insert(scope);
            &self.imports
        } else {
            DefaultImporter.imports(db, scope)
        }
    }
}

pub type NamedImportSet<'db> = FxHashMap<IdentId<'db>, NameResBucket<'db>>;

#[derive(Clone, Debug, Default, PartialEq, Eq, SalsaValue)]
pub struct GlobImportSet<'db> {
    imported: IndexMap<Use<'db>, GlobNames<'db>>,
}
impl<'db> GlobImportSet<'db> {
    /// Returns imported resolutions for the given `name`.
    pub fn name_res_for(&self, name: IdentId<'db>) -> impl Iterator<Item = &NameRes<'db>> {
        self.imported
            .values()
            .flat_map(move |v| v.get(&name).into_iter().flatten())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Use<'db>, &GlobNames<'db>)> {
        self.imported.iter()
    }
}

impl NameRes<'_> {
    /// Returns true if the resolution is a builtin or is in another ingot.
    fn is_external(&self, db: &dyn HirAnalysisDb, ingot: Ingot) -> bool {
        match self.kind {
            NameResKind::Scope(scope) => scope.ingot(db) != ingot,
            NameResKind::Prim(_) => true,
        }
    }
}
