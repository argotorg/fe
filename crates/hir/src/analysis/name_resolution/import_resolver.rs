//! This module implements import and export resolution for HIR.
use std::{
    collections::{VecDeque, hash_map::Entry},
    mem,
};

use crate::{
    hir_def::{HirIngot, IdentId, Use, prim_ty::PrimTy, scope_graph::ScopeId},
    span::DynLazySpan,
};
use common::{indexmap::IndexMap, ingot::Ingot};
use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;
use thin_vec::ThinVec;

use super::{
    EarlyNameQueryId,
    diagnostics::ImportDiag,
    name_resolver::{
        NameDerivation, NameDomain, NameRes, NameResBucket, NameResKind, NameResolutionError,
        NameResolutionResult, NameResolver, QueryDirective,
    },
};
use crate::analysis::{HirAnalysisDb, name_resolution::visibility_checker::is_use_visible};

pub(crate) struct ImportResolver<'db> {
    db: &'db dyn HirAnalysisDb,

    /// The ingot that is being resolved.
    ingot: Ingot<'db>,

    /// The set of imports that have been resolved.
    resolved_imports: IntermediateResolvedImports<'db>,

    /// The uses that have resolution is work in progress.
    intermediate_uses: FxHashMap<ScopeId<'db>, VecDeque<IntermediateUse<'db>>>,

    /// The errors that have been accumulated during the import resolution.
    accumulated_errors: Vec<ImportDiag<'db>>,

    /// The number of imported resolutions.
    /// This is used to judge if a import resolution doesn't change in each
    /// iteration of fixed point calculation.
    /// This check rely on the fact that the number of resolutions is
    /// monotonically increasing.
    num_imported_res: FxHashMap<Use<'db>, usize>,

    /// The set of imports that are suspicious to be ambiguous.
    /// In this case, the use will turns out to be ambiguous after the import
    /// resolution reaches the fixed point.
    suspicious_imports: FxHashSet<Use<'db>>,
}
impl<'db> ImportResolver<'db> {
    pub(crate) fn new(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        Self {
            db,
            ingot,
            resolved_imports: IntermediateResolvedImports::new(ingot),
            intermediate_uses: FxHashMap::default(),
            accumulated_errors: Vec::default(),
            num_imported_res: FxHashMap::default(),
            suspicious_imports: FxHashSet::default(),
        }
    }

    pub(crate) fn resolve_imports(mut self) -> (ResolvedImports<'db>, Vec<ImportDiag<'db>>) {
        self.initialize_i_uses();

        let mut changed = true;
        let mut unresolved_scope: VecDeque<_> =
            self.intermediate_uses.keys().copied().dedup().collect();
        while changed {
            changed = false;
            let n_unresolved_scope = unresolved_scope.len();
            let mut scope_counter = 0;

            while scope_counter < n_unresolved_scope {
                scope_counter += 1;
                let scope = unresolved_scope.pop_front().unwrap();

                let n_i_uses = self.intermediate_uses[&scope].len();
                let mut i_use_counter = 0;
                while i_use_counter < n_i_uses {
                    i_use_counter += 1;
                    let i_use = self
                        .intermediate_uses
                        .get_mut(&scope)
                        .unwrap()
                        .pop_front()
                        .unwrap();

                    match self.resolve_i_use(i_use) {
                        (Some(updated_i_use), i_use_changed) => {
                            changed |= i_use_changed;
                            self.intermediate_uses
                                .get_mut(&scope)
                                .unwrap()
                                .push_back(updated_i_use);
                        }

                        (None, i_use_changed) => {
                            changed |= i_use_changed;
                        }
                    }
                }

                if !self.scope_state(scope).is_closed() {
                    unresolved_scope.push_back(scope);
                }
            }
        }

        for i_use in std::mem::take(&mut self.intermediate_uses)
            .into_values()
            .flatten()
        {
            // If the unresolved use is a glob and the base path is fully resolved, then we
            // can regard the resolution for the glob as completed.
            // This happens if the scope that glob is referring to is not closed, e.g., if
            // there is a cycle in the import graph.
            if i_use.is_glob(self.db) && i_use.is_base_resolved(self.db) {
                continue;
            }

            // If the unresolved use is not a glob and the number of imported bucket is
            // not 0, then we can regard the resolution for the use as completed.
            // This happens if the scope that the use is referring to is not closed.
            if !i_use.is_glob(self.db) && *self.num_imported_res.entry(i_use.use_).or_default() != 0
            {
                continue;
            }

            self.register_error(&i_use, NameResolutionError::NotFound);
        }

        for suspicious in mem::take(&mut self.suspicious_imports) {
            self.verify_ambiguity(suspicious);
        }

        (
            self.resolved_imports.resolved_imports,
            self.accumulated_errors,
        )
    }

    /// Try to resolve the given `IntermediateUse`.
    ///
    /// The first value of the returned tuple is the updated `IntermediateUse`
    /// if the resolution is not fully completed.
    ///
    /// The second value of the returned tuple indicates whether the resolution
    /// is progressed from the passed `IntermediateUse`.
    fn resolve_i_use(
        &mut self,
        i_use: IntermediateUse<'db>,
    ) -> (Option<IntermediateUse<'db>>, bool) {
        if i_use.is_glob(self.db) {
            self.resolve_glob(i_use)
        } else {
            self.resolve_named(i_use)
        }
    }

    /// Try to resolve the given named `IntermediateUse`.
    fn resolve_named(
        &mut self,
        i_use: IntermediateUse<'db>,
    ) -> (Option<IntermediateUse<'db>>, bool) {
        let Some(i_use_res) = self.resolve_base_path(i_use.clone()) else {
            return (None, true);
        };

        match i_use_res {
            IUseResolution::Full(_) => unreachable!(),

            IUseResolution::BasePath(base_path_resolved) => {
                if self.try_finalize_named_use(base_path_resolved.clone()) {
                    (None, true)
                } else {
                    let changed = !i_use.is_base_resolved(self.db);
                    (Some(base_path_resolved), changed)
                }
            }

            IUseResolution::Partial(i_use) => (Some(i_use), true),

            IUseResolution::Unchanged(i_use) => (Some(i_use), false),
        }
    }

    /// Try to resolve the given glob `IntermediateUse`.
    ///
    /// The first value of the returned tuple is the updated `IntermediateUse`
    /// if the resolution is not fully completed.
    ///
    /// The second value of the returned tuple indicates whether the resolution
    /// is progressed from the passed `IntermediateUse`.
    fn resolve_glob(
        &mut self,
        i_use: IntermediateUse<'db>,
    ) -> (Option<IntermediateUse<'db>>, bool) {
        let (base_path_resolved, changed) = {
            if i_use.is_base_resolved(self.db) {
                (i_use, false)
            } else {
                let Some(i_use_res) = self.resolve_base_path(i_use) else {
                    return (None, true);
                };
                match i_use_res {
                    IUseResolution::Full(_) => unreachable!(),

                    IUseResolution::BasePath(resolved) => (resolved, true),

                    IUseResolution::Partial(i_use) => {
                        return (Some(i_use), true);
                    }

                    IUseResolution::Unchanged(i_use) => {
                        return (Some(i_use), false);
                    }
                }
            }
        };

        let Some(target_scope) = base_path_resolved.current_scope() else {
            return (None, true);
        };

        let original_scope = base_path_resolved.original_scope;
        let use_ = base_path_resolved.use_;

        // Collect all unresolved named imports in the target scope to avoid binding a
        // name to a wrong resolution being brought by a glob.
        let unresolved_named_imports = match self.intermediate_uses.get(&target_scope) {
            Some(i_uses) => i_uses
                .iter()
                .filter_map(|i_use_in_target| {
                    if !i_use_in_target.is_glob(self.db)
                        && is_use_visible(self.db, original_scope, use_)
                    {
                        i_use_in_target.imported_name(self.db)
                    } else {
                        None
                    }
                })
                .collect(),

            None => FxHashSet::default(),
        };

        // Collect all bucket in the target scope.
        let mut resolver = NameResolver::new(self.db, &self.resolved_imports);
        let resolutions = resolver.collect_all_resolutions_for_glob(
            target_scope,
            original_scope,
            unresolved_named_imports,
        );

        let is_decidable = self.is_decidable(&base_path_resolved);
        let n_res = resolutions.iter().fold(0, |acc, res| acc + res.1.len());
        if *self.num_imported_res.entry(use_).or_default() == n_res {
            if is_decidable {
                return (None, true);
            } else {
                return (Some(base_path_resolved), changed);
            }
        }

        self.num_imported_res.insert(base_path_resolved.use_, n_res);
        self.resolved_imports
            .set_glob_resolutions(&base_path_resolved, resolutions);

        if is_decidable {
            (None, true)
        } else {
            (Some(base_path_resolved), true)
        }
    }

    /// Resolves all segments of the given `IntermediateUse` except for the last
    /// segment.
    /// NOTE: `IUseResolution::Full` is never returned from this function.
    ///
    /// # Returns
    /// - `Some(IUseResolution::BasePath(_))` if the base path is fully
    ///   resolved.
    /// - `Some(IUseResolution::Partial(_))` if the base path is partially
    ///   resolved and the `IntermediateUse` is updated.
    /// - `Some(IUseResolution::Unchanged(_))` if the resulted `IntermediateUse`
    ///   is unchanged.
    /// - `None` if the error happens during the resolution, the error is
    ///   accumulated in the function.
    fn resolve_base_path(
        &mut self,
        mut i_use: IntermediateUse<'db>,
    ) -> Option<IUseResolution<'db>> {
        let mut changed = false;
        if i_use.is_base_resolved(self.db) {
            return Some(IUseResolution::BasePath(i_use));
        }

        loop {
            match self.resolve_segment(&i_use)? {
                IUseResolution::Full(_) => unreachable!(),

                IUseResolution::BasePath(resolved) => {
                    return Some(IUseResolution::BasePath(resolved));
                }

                IUseResolution::Partial(updated_i_use) => {
                    changed = true;
                    i_use = updated_i_use;
                }

                IUseResolution::Unchanged(i_use) => {
                    return if changed {
                        Some(IUseResolution::Partial(i_use))
                    } else {
                        Some(IUseResolution::Unchanged(i_use))
                    };
                }
            }
        }
    }

    /// Resolves the segments of the given `IntermediateUse` one by one.
    ///
    /// # Returns
    /// - `Some(IUseResolution::Full(_))` if the given use is fully resolved.
    /// - `Some(IUseResolution::BasePath(_))` if the base path is fully
    ///   resolved.
    /// - `Some(IUseResolution::Partial(_))` if the base path is partially
    ///   resolved and the `IntermediateUse` is updated.
    /// - `Some(IUseResolution::Unchanged(_))` if the resulted `IntermediateUse`
    ///   is unchanged.
    /// - `None` if the error happens during the resolution, the error is
    ///   accumulated in the function.
    fn resolve_segment(&mut self, i_use: &IntermediateUse<'db>) -> Option<IUseResolution<'db>> {
        // The segment is syntactically invalid. We can't perform name resolution
        // anymore.
        // We don't need to report the error here because the parser should have already
        // reported it.
        let query = match self.make_query(i_use) {
            Ok(query) => query,
            Err(err) => {
                self.register_error(i_use, err);
                return None;
            }
        };

        let mut resolver = NameResolver::new(self.db, &self.resolved_imports);
        let mut bucket = resolver.resolve_query(query);
        // Filter out invisible resolutions.
        let mut invisible_span = None;
        bucket.bucket.retain(|(_, res)| {
            let Ok(res) = res else {
                return true;
            };

            if !res.is_importable() {
                return false;
            }
            if res.is_visible(self.db, i_use.original_scope) {
                true
            } else {
                if let Some(span) = res.derived_from(self.db) {
                    invisible_span.get_or_insert(span);
                }
                false
            }
        });

        for (_, err) in bucket.errors() {
            if !matches!(
                err,
                NameResolutionError::NotFound | NameResolutionError::Invalid
            ) {
                self.register_error(i_use, err.clone());
                return None;
            }
        }
        if bucket.is_empty() {
            if self.is_decidable(i_use) {
                let err = if let Some(invisible_span) = invisible_span {
                    NameResolutionError::Invisible(invisible_span.into())
                } else {
                    NameResolutionError::NotFound
                };
                self.register_error(i_use, err);
                return None;
            } else {
                return Some(IUseResolution::Unchanged(i_use.clone()));
            };
        }

        // If the resolution is derived from glob import or external crate, we have to
        // insert the use into the `suspicious_imports` set to verify the ambiguity
        // after the algorithm reaches the fixed point.
        if i_use.is_first_segment() {
            for res in bucket.iter_ok() {
                if res.is_builtin()
                    || res.is_external(self.db, self.ingot)
                    || res.is_derived_from_glob()
                {
                    self.suspicious_imports.insert(i_use.use_);
                    break;
                }
            }
        }

        if i_use.is_base_resolved(self.db) {
            Some(IUseResolution::Full(bucket))
        } else {
            let next_i_use = match i_use.proceed(self.db, bucket) {
                Ok(next_i_use) => next_i_use,
                Err(err) => {
                    self.register_error(i_use, err);
                    return None;
                }
            };

            if next_i_use.is_base_resolved(self.db) {
                Some(IUseResolution::BasePath(next_i_use))
            } else {
                Some(IUseResolution::Partial(next_i_use))
            }
        }
    }

    fn initialize_i_uses(&mut self) {
        let m_tree = self.ingot.module_tree(self.db);

        for top_mod in m_tree.all_modules() {
            let s_graph = top_mod.scope_graph(self.db);
            for &use_ in &s_graph.unresolved_uses {
                let i_use = IntermediateUse::new(self.db, use_);
                self.intermediate_uses
                    .entry(i_use.original_scope)
                    .or_default()
                    .push_back(i_use);
            }
        }
    }

    /// Returns `true` if the given `IntermediateUse` reaches the fixed point.
    fn try_finalize_named_use(&mut self, i_use: IntermediateUse<'db>) -> bool {
        debug_assert!(i_use.is_base_resolved(self.db));

        let bucket = match self.resolve_segment(&i_use) {
            Some(IUseResolution::Full(bucket)) => bucket,
            Some(IUseResolution::Unchanged(_)) => {
                return false;
            }

            Some(_) => unreachable!(),

            None => {
                return true;
            }
        };

        let n_res = bucket.len();
        let is_decidable = self.is_decidable(&i_use);
        if *self.num_imported_res.entry(i_use.use_).or_default() == n_res {
            return is_decidable;
        }

        self.num_imported_res.insert(i_use.use_, n_res);
        if let Err(err) = self
            .resolved_imports
            .set_named_bucket(self.db, &i_use, bucket)
        {
            self.register_error(&i_use, err);
            return true;
        }

        is_decidable
    }

    /// Check the ambiguity of the given suspicious `IntermediateUse` and report
    /// an error if it is ambiguous.
    /// An additional ambiguity check should be performed after the import
    /// resolution reaches a fixed point.
    //
    // The ambiguity in the first segment possibly occurs when the segment is
    // resolved to either a glob imported derived resolution or an external ingot in
    // the `i_use` resolution.
    //
    // This is because:
    // 1. the resolution of the first segment changes depending on whether the const
    //    glob is resolved or not at the time of `i_use` resolution,
    // 2. the order in which uses are resolved is nondeterministic.
    //
    // In normal name resolution rules, the name brought in by a glob always shadows
    // the external ingot, so this ambiguity is inherent in import resolution.
    // As a result, we need to add additional verification to check this kind of
    // ambiguity.
    fn verify_ambiguity(&mut self, use_: Use<'db>) {
        let i_use = IntermediateUse::new(self.db, use_);
        let first_segment_ident = i_use.current_segment_ident(self.db).unwrap();

        let res = match self.resolve_segment(&i_use) {
            Some(IUseResolution::Full(bucket)) => match bucket.pick(NameDomain::TYPE) {
                Ok(res) => res.clone(),
                _ => {
                    return;
                }
            },

            Some(IUseResolution::BasePath(i_use) | IUseResolution::Partial(i_use)) => {
                i_use.current_res.unwrap()
            }

            Some(IUseResolution::Unchanged(_)) | None => return,
        };

        // The resolved scope is shadowed by an glob imports while originally
        // the use might be resolved to an external ingot or builtin. This means there
        // is an ambiguity between the external ingot and the name
        // imported by the glob import.
        if !res.is_external(self.db, self.ingot)
            && (self
                .ingot
                .resolved_external_ingots(self.db)
                .iter()
                .any(|(ingot_name, _)| *ingot_name == first_segment_ident)
                || PrimTy::all_types()
                    .iter()
                    .any(|ty| ty.name(self.db) == first_segment_ident))
        {
            self.register_error(&i_use, NameResolutionError::Ambiguous(ThinVec::new()));
        }
    }

    fn register_error(&mut self, i_use: &IntermediateUse<'db>, err: NameResolutionError<'db>) {
        self.suspicious_imports.remove(&i_use.use_);

        if i_use.use_.is_synthetic_use(self.db) {
            return;
        }

        match err {
            NameResolutionError::NotFound => {
                self.accumulated_errors.push(ImportDiag::NotFound(
                    i_use.current_segment_span(),
                    i_use.current_segment_ident(self.db).unwrap(),
                ));
            }

            NameResolutionError::Invalid => {
                // Do nothing because the error is already reported in the
                // parsing phase.
            }

            NameResolutionError::Ambiguous(cands) => {
                self.accumulated_errors.push(ImportDiag::ambiguous(
                    self.db,
                    i_use.current_segment_span(),
                    i_use.current_segment_ident(self.db).unwrap(),
                    cands,
                ));
            }

            NameResolutionError::InvalidPathSegment(res) => {
                self.accumulated_errors.push(ImportDiag::InvalidPathSegment(
                    i_use.current_segment_span(),
                    i_use.current_segment_ident(self.db).unwrap(),
                    res.kind.name_span(self.db),
                ))
            }

            NameResolutionError::Invisible(invisible_span) => {
                self.accumulated_errors.push(ImportDiag::Invisible(
                    i_use.current_segment_span(),
                    i_use.current_segment_ident(self.db).unwrap(),
                    invisible_span,
                ));
            }

            NameResolutionError::Conflict(ident, spans) => {
                self.accumulated_errors
                    .push(ImportDiag::Conflict(ident, spans));
            }
        }
    }

    /// Makes a query for the current segment of the intermediate use to be
    /// resolved.
    fn make_query(
        &self,
        i_use: &IntermediateUse<'db>,
    ) -> NameResolutionResult<'db, EarlyNameQueryId<'db>> {
        let Some(seg_name) = i_use.current_segment_ident(self.db) else {
            return Err(NameResolutionError::Invalid);
        };

        let Some(current_scope) = i_use.current_scope() else {
            return Err(NameResolutionError::NotFound);
        };

        // In the middle of the use path, disallow lexically scoped names and
        // external names.
        let directive = if !i_use.is_first_segment() {
            QueryDirective::new().disallow_lex().disallow_external()
        } else if self.contains_unresolved_named_use(
            seg_name,
            current_scope,
            i_use.is_first_segment(),
        ) {
            QueryDirective::new().disallow_glob().disallow_external()
        } else {
            QueryDirective::new()
        };

        Ok(EarlyNameQueryId::new(
            self.db,
            seg_name,
            current_scope,
            directive,
        ))
    }

    /// Returns `true` if there is an unresolved named import for the given name
    /// in the given scope or its lexical parents(if `allow_lex` is `true`).
    fn contains_unresolved_named_use(
        &self,
        name: IdentId,
        scope: ScopeId,
        allow_lex: bool,
    ) -> bool {
        let mut current_scope = Some(scope);

        while let Some(scope) = current_scope {
            for i_use in self.intermediate_uses.get(&scope).into_iter().flatten() {
                if i_use.imported_name(self.db) == Some(name) {
                    return true;
                }
            }
            if !allow_lex {
                break;
            }
            current_scope = scope.lex_parent(self.db);
        }

        false
    }

    /// Returns the current state of the scope.
    fn scope_state(&self, scope: ScopeId) -> ScopeState {
        if scope.ingot(self.db) != self.ingot {
            return ScopeState::Closed;
        }

        let Some(i_uses) = self.intermediate_uses.get(&scope) else {
            return ScopeState::Closed;
        };

        if i_uses.is_empty() {
            ScopeState::Closed
        } else {
            ScopeState::Open
        }
    }

    /// Returns `true` if the next segment of the intermediate use is
    /// deterministically resolvable.
    fn is_decidable(&self, i_use: &IntermediateUse) -> bool {
        let Some(target_scope) = i_use.current_scope() else {
            return true;
        };

        if i_use.is_first_segment() {
            let mut target_scope = Some(target_scope);
            while let Some(scope) = target_scope {
                if self.scope_state(scope) != ScopeState::Closed {
                    return false;
                }
                target_scope = scope.lex_parent(self.db);
            }
            true
        } else {
            self.scope_state(target_scope) != ScopeState::Open
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Update)]
pub struct ResolvedImports<'db> {
    pub named_resolved: IndexMap<ScopeId<'db>, NamedImportSet<'db>>,
    pub glob_resolved: IndexMap<ScopeId<'db>, GlobImportSet<'db>>,
    pub unnamed_resolved: IndexMap<ScopeId<'db>, Vec<NameResBucket<'db>>>,
}

pub(super) trait Importer<'db> {
    fn named_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a NamedImportSet<'db>>;

    fn glob_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a GlobImportSet<'db>>;
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct DefaultImporter;

impl<'db> Importer<'db> for DefaultImporter {
    fn named_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a NamedImportSet<'db>> {
        resolved_imports_for_scope(db, scope)
            .named_resolved
            .get(&scope)
    }

    fn glob_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a GlobImportSet<'db>> {
        resolved_imports_for_scope(db, scope)
            .glob_resolved
            .get(&scope)
    }
}

pub type NamedImportSet<'db> = FxHashMap<IdentId<'db>, NameResBucket<'db>>;

#[derive(Clone, Debug, Default, PartialEq, Eq, Update)]
pub struct GlobImportSet<'db> {
    imported: FxHashMap<Use<'db>, FxHashMap<IdentId<'db>, Vec<NameRes<'db>>>>,
}
impl<'db> GlobImportSet<'db> {
    /// Returns imported resolutions for the given `name`.
    pub fn name_res_for(&self, name: IdentId<'db>) -> impl Iterator<Item = &NameRes<'db>> {
        self.imported
            .values()
            .flat_map(move |v| v.get(&name).into_iter().flatten())
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&Use<'db>, &FxHashMap<IdentId<'db>, Vec<NameRes<'db>>>)> {
        self.imported.iter()
    }
}

/// This is the state of import resolution for a given scope.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum ScopeState {
    // The scope is open, meaning that the scope needs further resolution.
    Open,

    /// The scope is closed, meaning that the all imports in the scope is fully
    /// resolved.
    Closed,
}

impl ScopeState {
    fn is_closed(self) -> bool {
        matches!(self, ScopeState::Closed)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct IntermediateUse<'db> {
    use_: Use<'db>,
    current_res: Option<NameRes<'db>>,
    original_scope: ScopeId<'db>,
    unresolved_from: usize,
}

impl<'db> IntermediateUse<'db> {
    fn new(db: &'db dyn HirAnalysisDb, use_: Use<'db>) -> Self {
        let scope = ScopeId::from_item(use_.into()).lex_parent(db).unwrap();
        Self {
            use_,
            current_res: None,
            original_scope: scope,
            unresolved_from: 0,
        }
    }

    /// Returns the scope that the current resolution is pointed to.
    fn current_scope(&self) -> Option<ScopeId<'db>> {
        if let Some(current_res) = self.current_res.as_ref() {
            match current_res.kind {
                NameResKind::Scope(scope) => Some(scope),
                NameResKind::Prim(_) => None,
            }
        } else {
            self.original_scope.into()
        }
    }

    fn is_glob(&self, db: &dyn HirAnalysisDb) -> bool {
        self.use_.is_glob(db)
    }

    /// Proceed the resolution of the use path to the next segment.
    /// Returns an error if the bucket doesn't contain appropriate resolution
    /// for use path segment. # Panics
    /// - Panics if the the base path is already resolved.
    /// - Panics if the bucket is empty.
    fn proceed(
        &self,
        db: &'db dyn HirAnalysisDb,
        bucket: NameResBucket<'db>,
    ) -> NameResolutionResult<'db, Self> {
        debug_assert!(!bucket.is_empty());
        debug_assert!(!self.is_base_resolved(db));

        let next_res = match bucket.pick(NameDomain::TYPE) {
            Ok(res) => res.clone(),
            Err(_) => {
                let res = bucket.iter_ok().next().unwrap();
                return Err(NameResolutionError::InvalidPathSegment(res.clone()));
            }
        };

        if next_res.is_mod(db) || next_res.is_enum(db) {
            Ok(Self {
                use_: self.use_,
                current_res: next_res.into(),
                original_scope: self.original_scope,
                unresolved_from: self.unresolved_from + 1,
            })
        } else {
            Err(NameResolutionError::InvalidPathSegment(next_res))
        }
    }

    /// Returns the span of the current segment of the use.
    fn current_segment_span(&self) -> DynLazySpan<'db> {
        self.use_.span().path().segment(self.unresolved_from).into()
    }

    fn current_segment_ident(&self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        let segments = self.use_.path(db).to_opt()?.data(db);

        let seg_idx = self.unresolved_from;
        let segment = segments[seg_idx].to_opt()?;
        segment.ident()
    }

    fn imported_name(&self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        self.use_.imported_name(db)
    }

    fn segment_len(&self, db: &dyn HirAnalysisDb) -> Option<usize> {
        self.use_.path(db).to_opt().map(|p| p.segment_len(db))
    }

    /// Returns `true` if the segment that should be resolved next is the first
    /// segment.
    fn is_first_segment(&self) -> bool {
        self.unresolved_from == 0
    }

    /// Returns `true` if the use path except the last segment is fully
    /// resolved.
    fn is_base_resolved(&self, db: &dyn HirAnalysisDb) -> bool {
        let Some(segment_len) = self.segment_len(db) else {
            return false;
        };

        self.unresolved_from + 1 == segment_len
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IUseResolution<'db> {
    /// The all segments are resolved.
    Full(NameResBucket<'db>),

    /// The all path segments except the last one are resolved.
    BasePath(IntermediateUse<'db>),

    /// The intermediate use was partially resolved, but still needs further
    /// resolution.
    Partial(IntermediateUse<'db>),

    /// There was no change to the intermediate use.
    Unchanged(IntermediateUse<'db>),
}

struct IntermediateResolvedImports<'db> {
    resolved_imports: ResolvedImports<'db>,
    ingot: Ingot<'db>,
}

impl<'db> IntermediateResolvedImports<'db> {
    fn new(ingot: Ingot<'db>) -> Self {
        Self {
            resolved_imports: ResolvedImports::default(),
            ingot,
        }
    }

    fn set_named_bucket(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        i_use: &IntermediateUse<'db>,
        mut bucket: NameResBucket<'db>,
    ) -> NameResolutionResult<'db, ()> {
        let scope = i_use.original_scope;
        bucket.set_derivation(NameDerivation::NamedImported(i_use.use_));

        let imported_name = match i_use.imported_name(db) {
            Some(name) => name,
            None => {
                self.resolved_imports
                    .unnamed_resolved
                    .entry(scope)
                    .or_default()
                    .push(bucket);
                return Ok(());
            }
        };

        let imported_set = self
            .resolved_imports
            .named_resolved
            .entry(scope)
            .or_default();

        match imported_set.entry(imported_name) {
            Entry::Occupied(mut e) => {
                let old_bucket = e.get_mut();
                old_bucket.merge(&bucket);
                for (_, err) in old_bucket.errors() {
                    let NameResolutionError::Ambiguous(cands) = err else {
                        continue;
                    };
                    for cand in cands {
                        let NameDerivation::NamedImported(use_) = cand.derivation else {
                            continue;
                        };

                        if i_use.use_ != use_ {
                            return Err(NameResolutionError::Conflict(
                                imported_name,
                                ThinVec::from([
                                    i_use.use_.imported_name_span(db).unwrap(),
                                    cand.derived_from(db).unwrap(),
                                ]),
                            ));
                        }
                    }
                }
                Ok(())
            }

            Entry::Vacant(e) => {
                e.insert(bucket);
                Ok(())
            }
        }
    }

    fn set_glob_resolutions(
        &mut self,
        i_use: &IntermediateUse<'db>,
        mut resolutions: FxHashMap<IdentId<'db>, Vec<NameRes<'db>>>,
    ) {
        let scope = i_use.original_scope;
        for res in resolutions.values_mut().flatten() {
            res.derivation = NameDerivation::GlobImported(i_use.use_);
        }

        self.resolved_imports
            .glob_resolved
            .entry(scope)
            .or_default()
            .imported
            .insert(i_use.use_, resolutions);
    }
}

impl<'db> Importer<'db> for IntermediateResolvedImports<'db> {
    fn named_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a NamedImportSet<'db>> {
        if scope.top_mod(db).ingot(db) != self.ingot {
            resolved_imports_for_scope(db, scope)
                .named_resolved
                .get(&scope)
        } else {
            self.resolved_imports.named_resolved.get(&scope)
        }
    }

    fn glob_imports<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
    ) -> Option<&'a GlobImportSet<'db>> {
        if scope.top_mod(db).ingot(db) != self.ingot {
            resolved_imports_for_scope(db, scope)
                .glob_resolved
                .get(&scope)
        } else {
            self.resolved_imports.glob_resolved.get(&scope)
        }
    }
}

fn resolved_imports_for_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> &'db ResolvedImports<'db> {
    let ingot = scope.ingot(db);
    &super::resolve_imports(db, ingot).1
}

impl NameRes<'_> {
    /// Returns true if the bucket contains an resolution that is not in the
    /// same ingot as the current resolution of the `i_use`.
    fn is_external(&self, db: &dyn HirAnalysisDb, ingot: Ingot) -> bool {
        match self.kind {
            NameResKind::Scope(scope) => scope.ingot(db) != ingot,
            NameResKind::Prim(_) => true,
        }
    }

    fn is_builtin(&self) -> bool {
        matches!(self.kind, NameResKind::Prim(_))
    }

    /// Returns true if the bucket contains a glob import.
    fn is_derived_from_glob(&self) -> bool {
        matches!(self.derivation, NameDerivation::GlobImported(_))
    }
}
