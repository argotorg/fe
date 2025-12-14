use std::{cmp, fmt, mem};

use crate::{
    core::{
        hir_def::{
            Enum, EnumVariant, GenericParam, GenericParamOwner, HirIngot, IdentId, ItemKind, Mod,
            TopLevelMod, Trait, Use,
            prim_ty::PrimTy,
            scope_graph::{
                AnonEdge, EdgeKind, FieldEdge, GenericParamEdge, IngotEdge, LexEdge, ModEdge,
                ScopeId, SelfEdge, SelfTyEdge, SuperEdge, TraitEdge, TraitTypeEdge, TypeEdge,
                ValueEdge, VariantEdge,
            },
        },
        span::DynLazySpan,
    },
    hir_def::scope_graph::ScopeEdge,
};
use bitflags::bitflags;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;
use thin_vec::ThinVec;

use super::{
    import_resolver::Importer,
    visibility_checker::{is_scope_visible_from, is_use_visible},
};
use crate::analysis::HirAnalysisDb;

#[salsa::interned]
#[derive(Debug)]
pub struct EarlyNameQueryId<'db> {
    /// The name to be resolved.
    name: IdentId<'db>,
    /// The scope where the name is resolved.
    scope: ScopeId<'db>,
    directive: QueryDirective,
}

/// The query directive is used to control the name resolution behavior, such as
/// whether to lookup the name in the lexical scope or not.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct QueryDirective {
    /// If `allow_lex` is `true`, then the query will be propagated to the
    /// lexical scope if the name is not found in the current scope.
    allow_lex: bool,

    /// If `allow_external` is `true`, then the query will be propagated to the
    /// external ingot and builtin types as well.
    allow_external: bool,

    /// If `allow_glob` is `true`, then the resolver uses the glob import to
    /// resolve the name.
    allow_glob: bool,
}

impl QueryDirective {
    /// Make a new query directive for resolving names from `scope`.
    pub fn for_scope<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        let _ = (db, scope);
        Self::new()
    }

    /// Make a new query directive with the default settings.
    /// The default setting is to lookup the name in the lexical scope and all
    /// imports and external ingots.
    pub fn new() -> Self {
        Self {
            allow_lex: true,
            allow_external: true,
            allow_glob: true,
        }
    }

    /// Disallow lexical scope lookup.
    pub fn disallow_lex(mut self) -> Self {
        self.allow_lex = false;
        self
    }

    pub(super) fn disallow_external(mut self) -> Self {
        self.allow_external = false;
        self
    }

    pub(super) fn disallow_glob(mut self) -> Self {
        self.allow_glob = false;
        self
    }
}

impl Default for QueryDirective {
    fn default() -> Self {
        Self::new()
    }
}

/// The struct contains the lookup result of a name query.
/// The results can contain more than one name resolutions which belong to
/// different name domains.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, Update)]
pub struct NameResBucket<'db> {
    // Contains a maximum of 3 entries (one for each distinct NameDomain)
    pub(super) bucket: ThinVec<(NameDomain, NameResolutionResult<'db, NameRes<'db>>)>,
}

impl<'db> NameResBucket<'db> {
    /// Returns the number of resolutions in the bucket.
    pub fn len(&self) -> usize {
        self.iter_ok().count()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = &NameResolutionResult<'db, NameRes<'db>>> {
        self.bucket.iter().map(|(_, res)| res)
    }

    pub fn iter_ok(&self) -> impl Iterator<Item = &NameRes<'db>> {
        self.bucket.iter().filter_map(|(_, res)| res.as_ref().ok())
    }

    pub fn iter_ok_mut(&mut self) -> impl Iterator<Item = &mut NameRes<'db>> {
        self.bucket
            .iter_mut()
            .filter_map(|(_, res)| res.as_mut().ok())
    }

    pub fn errors(&self) -> impl Iterator<Item = (NameDomain, &NameResolutionError<'db>)> {
        self.bucket
            .iter()
            .filter_map(|(domain, res)| res.as_ref().err().map(|err| (*domain, err)))
    }

    /// Returns the resolution of the given `domain`.
    pub fn pick(&self, domain: NameDomain) -> &NameResolutionResult<'db, NameRes<'db>> {
        for domain in domain.iter() {
            if let Some((_, res)) = self.bucket.iter().find(|(d, _)| *d == domain) {
                return res;
            }
        }

        &Err(NameResolutionError::NotFound)
    }

    pub fn pick_any(&self, from: &[NameDomain]) -> &NameResolutionResult<'db, NameRes<'db>> {
        let mut res = &Err(NameResolutionError::NotFound);
        for domain in from {
            res = self.pick(*domain);
            if res.is_ok() {
                return res;
            }
        }
        res
    }

    pub fn filter_by_domain(&mut self, domain: NameDomain) {
        for domain in domain.iter() {
            self.bucket.retain(|(d, _)| *d == domain);
        }
    }

    pub(super) fn merge(&mut self, bucket: &NameResBucket<'db>) {
        for (domain, err) in bucket.errors() {
            if let Err(NameResolutionError::NotFound) = self.pick(domain) {
                self.bucket.push((domain, Err(err.clone())));
            }
        }
        for res in bucket.iter_ok() {
            self.push(res);
        }
    }

    pub(super) fn set_derivation(&mut self, derivation: NameDerivation<'db>) {
        for res in self.iter_ok_mut() {
            res.derivation = derivation.clone();
        }
    }

    /// Push the `res` into the set.
    fn push(&mut self, res: &NameRes<'db>) {
        for domain in res.domain.iter() {
            let existing_idx = self.bucket.iter().position(|(d, _)| *d == domain);

            let Some(idx) = existing_idx else {
                self.bucket.push((domain, Ok(res.clone())));
                continue;
            };
            let (_, existing_res) = &mut self.bucket[idx];
            match existing_res {
                Ok(old_res) => {
                    let old_derivation = old_res.derivation.clone();
                    match res.derivation.cmp(&old_derivation) {
                        cmp::Ordering::Less => {}
                        cmp::Ordering::Equal => {
                            // Only mark as ambiguous if the two resolutions actually
                            // refer to different underlying scopes/kinds (i.e. true
                            // duplicates like two generic params with the same name
                            // under the same owner). If they are identical (same
                            // scope/kind), keep the existing resolution to avoid
                            // spurious ambiguity.
                            if old_res.kind != res.kind {
                                *existing_res =
                                    Err(NameResolutionError::Ambiguous(ThinVec::from([
                                        old_res.clone(),
                                        res.clone(),
                                    ])));
                            }
                        }
                        cmp::Ordering::Greater => {
                            *existing_res = Ok(res.clone());
                        }
                    }
                }
                Err(NameResolutionError::NotFound) => {
                    *existing_res = Ok(res.clone());
                }
                Err(NameResolutionError::Ambiguous(ambiguous_set)) => {
                    if ambiguous_set[0].derivation == res.derivation {
                        ambiguous_set.push(res.clone());
                    }
                }
                Err(_) => {}
            }
        }
    }

    fn set_lexed_derivation(&mut self) {
        for res in self.iter_ok_mut() {
            res.derivation.lexed()
        }
    }
}

impl<'db> From<NameRes<'db>> for NameResBucket<'db> {
    fn from(res: NameRes<'db>) -> Self {
        Self {
            bucket: ThinVec::from([(res.domain, Ok(res))]),
        }
    }
}

/// The struct contains the lookup result of a name query.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub struct NameRes<'db> {
    /// The kind of the resolution.
    pub kind: NameResKind<'db>,
    /// The domain of the name resolution.
    pub domain: NameDomain,
    /// Where the resolution is derived from. (e.g, via `use` or item definition
    /// in the same scope).
    pub derivation: NameDerivation<'db>,
}

impl<'db> NameRes<'db> {
    /// Returns `true` if the name is visible from the given `scope`.
    pub fn is_visible(&self, db: &'db dyn HirAnalysisDb, from: ScopeId<'db>) -> bool {
        match self.derivation {
            NameDerivation::Def | NameDerivation::Prim | NameDerivation::External => {
                match self.kind {
                    NameResKind::Scope(scope) => is_scope_visible_from(db, scope, from),
                    NameResKind::Prim(_) => true,
                }
            }
            NameDerivation::NamedImported(use_) | NameDerivation::GlobImported(use_) => {
                is_use_visible(db, from, use_)
            }
            NameDerivation::Lex(ref inner) => {
                let mut inner = inner;
                while let NameDerivation::Lex(parent) = inner.as_ref() {
                    inner = parent;
                }

                Self {
                    derivation: inner.as_ref().clone(),
                    ..self.clone()
                }
                .is_visible(db, from)
            }
        }
    }

    /// Returns `true` if the resolution is a type.
    pub fn is_type(&self) -> bool {
        match self.kind {
            NameResKind::Prim(_) => true,
            NameResKind::Scope(scope) => scope.is_type(),
        }
    }

    pub fn trait_(&self) -> Option<Trait<'db>> {
        match self.kind {
            NameResKind::Scope(ScopeId::Item(ItemKind::Trait(trait_))) => Some(trait_),
            _ => None,
        }
    }

    pub fn enum_variant(&self) -> Option<EnumVariant<'_>> {
        match self.kind {
            NameResKind::Scope(ScopeId::Variant(v)) => Some(v),
            _ => None,
        }
    }

    /// Returns `true` if the resolution is a trait.
    pub fn is_trait(&self) -> bool {
        self.trait_().is_some()
    }

    pub fn is_enum(&self, db: &dyn HirAnalysisDb) -> bool {
        match self.kind {
            NameResKind::Prim(_) => false,
            NameResKind::Scope(scope) => scope.resolve_to::<Enum>(db).is_some(),
        }
    }

    pub fn is_mod(&self, db: &dyn HirAnalysisDb) -> bool {
        match self.kind {
            NameResKind::Prim(_) => false,
            NameResKind::Scope(scope) => {
                scope.resolve_to::<TopLevelMod>(db).is_some()
                    || scope.resolve_to::<Mod>(db).is_some()
            }
        }
    }

    pub fn is_value(&self) -> bool {
        !self.is_type() && !self.is_trait()
    }

    /// Returns the scope of the name resolution if the name is not a builtin
    /// type.
    pub fn scope(&self) -> Option<ScopeId<'db>> {
        match self.kind {
            NameResKind::Scope(scope) => Some(scope),
            NameResKind::Prim(_) => None,
        }
    }

    pub fn pretty_path(&self, db: &dyn HirAnalysisDb) -> Option<String> {
        match self.kind {
            NameResKind::Scope(scope) => scope.pretty_path(db),
            NameResKind::Prim(prim) => prim.name(db).data(db).clone().into(),
        }
    }

    pub(super) fn derived_from(&self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        match self.derivation {
            NameDerivation::Def | NameDerivation::Prim | NameDerivation::External => {
                self.kind.name_span(db)
            }
            NameDerivation::NamedImported(use_) => use_.imported_name_span(db),
            NameDerivation::GlobImported(use_) => use_.glob_span(db),
            NameDerivation::Lex(ref inner) => {
                let mut inner = inner;
                while let NameDerivation::Lex(parent) = inner.as_ref() {
                    inner = parent;
                }
                Self {
                    derivation: inner.as_ref().clone(),
                    ..self.clone()
                }
                .derived_from(db)
            }
        }
    }

    pub(super) fn new_from_scope(
        scope: ScopeId<'db>,
        domain: NameDomain,
        derivation: NameDerivation<'db>,
    ) -> Self {
        Self {
            kind: scope.into(),
            derivation,
            domain,
        }
    }

    pub(super) fn kind_name(&self) -> &'static str {
        match self.kind {
            NameResKind::Scope(scope) => scope.kind_name(),
            NameResKind::Prim(_) => "type",
        }
    }

    pub(super) fn is_importable(&self) -> bool {
        matches!(self.domain, NameDomain::TYPE | NameDomain::VALUE)
    }

    fn new_prim(prim: PrimTy) -> Self {
        Self {
            kind: prim.into(),
            derivation: NameDerivation::Prim,
            domain: NameDomain::TYPE,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, derive_more::From, Update)]
pub enum NameResKind<'db> {
    /// The name is resolved to a scope.
    Scope(ScopeId<'db>),
    /// The name is resolved to a primitive type.
    Prim(PrimTy),
}

impl<'db> NameResKind<'db> {
    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> Option<DynLazySpan<'db>> {
        match self {
            NameResKind::Scope(scope) => scope.name_span(db),
            NameResKind::Prim(_) => None,
        }
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
        match self {
            NameResKind::Scope(scope) => scope.name(db).unwrap(),
            NameResKind::Prim(prim) => prim.name(db),
        }
    }
}

/// The name derivation indicates where a name resolution comes from.
/// Name derivation is used to track the origin of a resolution, and to
/// determine the shadowing rules.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Update)]
pub enum NameDerivation<'db> {
    /// Derived from a definition in the current scope.
    Def,
    /// Derived from a named import in the current scope.
    NamedImported(Use<'db>),
    /// Derived from a glob import in the current scope.
    GlobImported(Use<'db>),
    /// Derived from lexical parent scope.
    Lex(Box<NameDerivation<'db>>),
    /// Derived from an external ingot.
    External,
    /// Derived from a builtin primitive.
    Prim,
}

impl NameDerivation<'_> {
    fn lexed(&mut self) {
        let inner = mem::replace(self, NameDerivation::Def);
        *self = NameDerivation::Lex(Box::new(inner));
    }

    pub fn use_stmt(&self) -> Option<Use<'_>> {
        match self {
            NameDerivation::NamedImported(u) | NameDerivation::GlobImported(u) => Some(*u),
            NameDerivation::Lex(deriv) => deriv.use_stmt(),
            _ => None,
        }
    }
}

impl PartialOrd for NameDerivation<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NameDerivation<'_> {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (NameDerivation::Def, NameDerivation::Def) => cmp::Ordering::Equal,
            (NameDerivation::Def, _) => cmp::Ordering::Greater,
            (_, NameDerivation::Def) => cmp::Ordering::Less,

            (NameDerivation::NamedImported(_), NameDerivation::NamedImported(_)) => {
                cmp::Ordering::Equal
            }
            (NameDerivation::NamedImported(_), _) => cmp::Ordering::Greater,
            (_, NameDerivation::NamedImported(_)) => cmp::Ordering::Less,

            (NameDerivation::GlobImported(_), NameDerivation::GlobImported(_)) => {
                cmp::Ordering::Equal
            }
            (NameDerivation::GlobImported(_), _) => cmp::Ordering::Greater,
            (_, NameDerivation::GlobImported(_)) => cmp::Ordering::Less,

            (NameDerivation::Lex(lhs), NameDerivation::Lex(rhs)) => lhs.cmp(rhs),
            (NameDerivation::Lex(_), _) => cmp::Ordering::Greater,
            (_, NameDerivation::Lex(_)) => cmp::Ordering::Less,

            (NameDerivation::External, NameDerivation::External) => cmp::Ordering::Equal,
            (NameDerivation::External, _) => cmp::Ordering::Greater,
            (_, NameDerivation::External) => cmp::Ordering::Less,

            (NameDerivation::Prim, NameDerivation::Prim) => cmp::Ordering::Equal,
        }
    }
}

pub(crate) struct NameResolver<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    importer: &'a dyn Importer<'db>,
}

impl<'db, 'a> NameResolver<'db, 'a> {
    pub(super) fn new(db: &'db dyn HirAnalysisDb, importer: &'a dyn Importer<'db>) -> Self {
        Self { db, importer }
    }

    pub(crate) fn resolve_query(&mut self, query: EarlyNameQueryId<'db>) -> NameResBucket<'db> {
        let mut bucket = NameResBucket::default();

        // The shadowing rule is
        // `$ > NamedImports > GlobImports > Lex > external ingot > builtin types`,
        // where `$` means current scope.
        // This ordering means that greater one shadows lower ones in the same domain.
        let mut parent = None;

        // 1. Look for the name in the current scope.
        let mut found_scopes = FxHashSet::default();
        let scope = query.scope(self.db);
        let top_mod = scope.top_mod(self.db);
        let s_graph = top_mod.scope_graph(self.db);

        {
            let mut process_edge = |edge: &ScopeEdge<'db>| match edge.kind.propagate(self.db, query)
            {
                PropagationResult::Terminated => {
                    if found_scopes.insert(edge.dest) {
                        let res = NameRes::new_from_scope(
                            edge.dest,
                            NameDomain::from_scope(self.db, edge.dest),
                            NameDerivation::Def,
                        );
                        bucket.push(&res);
                    }
                }

                PropagationResult::Continuation => {
                    debug_assert!(parent.is_none());
                    parent = Some(edge.dest);
                }

                PropagationResult::UnPropagated => {}
            };

            for edge in s_graph.edges(scope) {
                process_edge(edge);
            }
        }

        // 2. Look for the name in the named imports of the current scope.
        if let Some(imported) = self
            .importer
            .named_imports(self.db, query.scope(self.db))
            .and_then(|imports| imports.get(&query.name(self.db)))
        {
            bucket.merge(imported);
        }

        // 3. Look for the name in the glob imports.
        if query.directive(self.db).allow_glob
            && let Some(imported) = self.importer.glob_imports(self.db, query.scope(self.db))
        {
            for res in imported.name_res_for(query.name(self.db)) {
                bucket.push(res);
            }
        }

        // 4. Look for the name in the lexical scope if it exists.
        if let Some(parent) = parent {
            let directive = query.directive(self.db).disallow_external();
            let query_for_parent =
                EarlyNameQueryId::new(self.db, query.name(self.db), parent, directive);

            let mut resolved = self.resolve_query(query_for_parent);
            resolved.set_lexed_derivation();
            bucket.merge(&resolved);
        }

        if !query.directive(self.db).allow_external {
            return bucket;
        }

        // 5. Look for the name in the external ingots.
        query
            .scope(self.db)
            .top_mod(self.db)
            .ingot(self.db)
            .resolved_external_ingots(self.db)
            .iter()
            .for_each(|(name, ingot)| {
                if *name == query.name(self.db) {
                    // We don't care about the result of `push` because we assume ingots are
                    // guaranteed to be unique.
                    bucket.push(&NameRes::new_from_scope(
                        ScopeId::from_item((ingot.root_mod(self.db)).into()),
                        NameDomain::TYPE,
                        NameDerivation::External,
                    ))
                }
            });

        // 6. Look for the name in the builtin types.
        for &prim in PrimTy::all_types() {
            // We don't care about the result of `push` because we assume builtin types are
            // guaranteed to be unique.
            if query.name(self.db) == prim.name(self.db) {
                bucket.push(&NameRes::new_prim(prim));
            }
        }

        bucket
    }

    /// Collect all visible resolutions in the given `target` scope.
    ///
    /// The function follows the shadowing rule, meaning the same name in the
    /// same domain is properly shadowed. Also, this function guarantees that
    /// the collected resolutions are unique in terms of its name and resolved
    /// scope.
    ///
    /// On the other hand, the function doesn't cause any error and collect all
    /// resolutions even if they are in the same domain. The reason
    /// for this is
    /// - Ambiguous error should be reported lazily, meaning it should be
    ///   reported when the resolution is actually used.
    /// - The function is used for glob imports, so it's necessary to return
    ///   monotonously increasing results. Also, we can't arbitrarily choose the
    ///   possible resolution from multiple candidates to avoid hiding
    ///   ambiguity. That's also the reason why we can't use [`ResBucket`] and
    ///   [`ResBucket::merge`] in this function.
    ///
    /// The below examples demonstrates the second point.
    /// We need to report ambiguous error at `const C: S = S` because `S` is
    /// ambiguous, on the other hand, we need NOT to report ambiguous error in
    /// `foo` modules because `S` is not referred to in the module.
    ///
    /// ```fe
    /// use foo::*
    /// const C: S = S
    ///
    /// mod foo {
    ///     pub use inner1::*
    ///     pub use inner2::*
    ///
    ///     mod inner1 {
    ///           pub struct S {}
    ///     }
    ///     mod inner2 {
    ///        pub struct S {}
    ///     }
    /// }
    /// ```
    pub(super) fn collect_all_resolutions_for_glob(
        &mut self,
        target: ScopeId<'db>,
        use_scope: ScopeId<'db>,
        unresolved_named_imports: FxHashSet<IdentId<'db>>,
    ) -> FxHashMap<IdentId<'db>, Vec<NameRes<'db>>> {
        let mut res_collection: FxHashMap<IdentId, Vec<NameRes>> = FxHashMap::default();
        let mut found_domains: FxHashMap<IdentId, NameDomain> = FxHashMap::default();
        let mut found_kinds: FxHashSet<(IdentId, NameResKind)> = FxHashSet::default();

        for edge in target.edges(self.db) {
            let scope = match edge.kind.propagate_glob() {
                PropagationResult::Terminated => edge.dest,
                _ => {
                    continue;
                }
            };

            let name = scope.name(self.db).unwrap();
            if !found_kinds.insert((name, scope.into())) {
                continue;
            }
            let res = NameRes::new_from_scope(
                scope,
                NameDomain::from_scope(self.db, scope),
                NameDerivation::Def,
            );

            if res.is_visible(self.db, use_scope) {
                *found_domains.entry(name).or_default() |= res.domain;
                res_collection.entry(name).or_default().push(res);
            }
        }

        let mut found_domains_after_named = found_domains.clone();
        if let Some(named_imports) = self.importer.named_imports(self.db, target) {
            for (&name, import) in named_imports {
                let found_domain = found_domains.get(&name).copied().unwrap_or_default();
                for res in import
                    .iter_ok()
                    .filter(|res| res.is_visible(self.db, use_scope))
                {
                    if (found_domain & res.domain != NameDomain::Invalid)
                        || !found_kinds.insert((name, res.kind))
                    {
                        continue;
                    }

                    *found_domains_after_named.entry(name).or_default() |= res.domain;
                    res_collection.entry(name).or_default().push(res.clone());
                }
            }
        }

        if let Some(glob_imports) = self.importer.glob_imports(self.db, target) {
            for (_, resolutions) in glob_imports.iter() {
                // if !is_use_visible(self.db, ref_scope, use_) {
                //     continue;
                // }
                for (&name, res_for_name) in resolutions.iter() {
                    if unresolved_named_imports.contains(&name) {
                        continue;
                    }

                    for res in res_for_name
                        .iter()
                        .filter(|res| res.is_visible(self.db, use_scope))
                    {
                        let seen_domain = found_domains_after_named
                            .get(&name)
                            .copied()
                            .unwrap_or_default();

                        if (seen_domain & res.domain != NameDomain::Invalid)
                            || !found_kinds.insert((name, res.kind))
                        {
                            continue;
                        }
                        res_collection.entry(name).or_default().push(res.clone());
                    }
                }
            }
        }

        res_collection
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum NameResolutionError<'db> {
    /// The name is not found.
    NotFound,

    /// The name is invalid in parsing. Basically, no need to report it because
    /// the error is already emitted from parsing phase.
    Invalid,

    /// The name is found, but it's not visible from the reference site.
    Invisible(Option<DynLazySpan<'db>>),

    /// The name is found, but it's ambiguous.
    Ambiguous(ThinVec<NameRes<'db>>),

    /// The name is found, but it can't be used in the middle of a use path.
    InvalidPathSegment(NameRes<'db>),

    /// The definition conflicts with other definitions.
    Conflict(IdentId<'db>, ThinVec<DynLazySpan<'db>>),
}

pub type NameResolutionResult<'db, T> = Result<T, NameResolutionError<'db>>;

impl fmt::Display for NameResolutionError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NameResolutionError::NotFound => write!(f, "name not found"),
            NameResolutionError::Invalid => write!(f, "invalid name"),
            NameResolutionError::Invisible(_) => write!(f, "name is not visible"),
            NameResolutionError::Ambiguous(_) => write!(f, "name is ambiguous"),
            NameResolutionError::InvalidPathSegment(_) => write!(
                f,
                "the found resolution can't be used in the middle of a path"
            ),
            NameResolutionError::Conflict(_, _) => write!(f, "name conflicts with other names"),
        }
    }
}

impl std::error::Error for NameResolutionError<'_> {}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
    /// Each resolved name is associated with a domain that indicates which domain
    /// the name belongs to.
    /// The multiple same names can be introduced in a same scope as long as they
    /// are in different domains.
    ///
    /// E.g., A `Foo` in the below example can be introduced in the same scope as a
    /// type and variant at the same time.
    /// ```fe
    /// struct Foo {}
    /// enum MyEnum {
    ///     Foo
    /// }
    /// use MyEnum::Foo
    /// ```
    pub struct NameDomain: u8
        {
            const TYPE = 0b00000001;
            const VALUE = 0b00000010;
            const FIELD = 0b100;
            const Invalid = 0b0;
        }
}

#[allow(non_upper_case_globals)]
impl NameDomain {
    pub(super) fn from_scope(db: &dyn HirAnalysisDb, scope: ScopeId) -> Self {
        match scope {
            ScopeId::Item(ItemKind::Func(_) | ItemKind::Const(_))
            | ScopeId::FuncParam(..)
            | ScopeId::Block(..) => Self::VALUE,
            ScopeId::Item(_) => Self::TYPE,
            ScopeId::GenericParam(parent, idx) => {
                let parent = GenericParamOwner::from_item_opt(parent).unwrap();
                match parent.param_view(db, idx as usize).param {
                    GenericParam::Type(_) => NameDomain::TYPE,
                    GenericParam::Const(_) => NameDomain::TYPE | NameDomain::VALUE,
                }
            }
            ScopeId::TraitType(..) => Self::TYPE,
            ScopeId::TraitConst(..) => Self::VALUE,
            ScopeId::Field(..) => Self::FIELD,
            ScopeId::Variant(..) => Self::VALUE,
        }
    }
}

impl Default for NameDomain {
    fn default() -> Self {
        Self::Invalid
    }
}

/// The propagator controls how the name query is propagated to the next scope.
trait QueryPropagator<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult;
    fn propagate_glob(self) -> PropagationResult;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PropagationResult {
    /// The query is resolved to the next scope(edge's destination).
    Terminated,
    /// The query resolution should be continued, i.e., the query is propagated
    /// to the next scope and the next scope should be searched for the query.
    Continuation,
    /// The query can't be propagated to the next scope.
    UnPropagated,
}

impl<'db> QueryPropagator<'db> for LexEdge {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if query.directive(db).allow_lex {
            PropagationResult::Continuation
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for ModEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl<'db> QueryPropagator<'db> for TypeEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl<'db> QueryPropagator<'db> for TraitEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl<'db> QueryPropagator<'db> for TraitTypeEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl<'db> QueryPropagator<'db> for ValueEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl<'db> QueryPropagator<'db> for GenericParamEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for FieldEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for VariantEdge<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if self.0 == query.name(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::Terminated
    }
}

impl<'db> QueryPropagator<'db> for SuperEdge {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if query.name(db).is_super(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for IngotEdge {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if query.name(db).is_ingot(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for SelfTyEdge {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if query.name(db).is_self_ty(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for SelfEdge {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        if query.name(db).is_self(db) {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for AnonEdge {
    fn propagate(
        self,
        _db: &'db dyn HirAnalysisDb,
        _query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        PropagationResult::UnPropagated
    }

    fn propagate_glob(self) -> PropagationResult {
        PropagationResult::UnPropagated
    }
}

impl<'db> QueryPropagator<'db> for EdgeKind<'db> {
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        query: EarlyNameQueryId<'db>,
    ) -> PropagationResult {
        match self {
            EdgeKind::Lex(edge) => edge.propagate(db, query),
            EdgeKind::Mod(edge) => edge.propagate(db, query),
            EdgeKind::Type(edge) => edge.propagate(db, query),
            EdgeKind::Trait(edge) => edge.propagate(db, query),
            EdgeKind::TraitType(edge) => edge.propagate(db, query),
            EdgeKind::GenericParam(edge) => edge.propagate(db, query),
            EdgeKind::Value(edge) => edge.propagate(db, query),
            EdgeKind::Field(edge) => edge.propagate(db, query),
            EdgeKind::Variant(edge) => edge.propagate(db, query),
            EdgeKind::Super(edge) => edge.propagate(db, query),
            EdgeKind::Ingot(edge) => edge.propagate(db, query),
            EdgeKind::Self_(edge) => edge.propagate(db, query),
            EdgeKind::SelfTy(edge) => edge.propagate(db, query),
            EdgeKind::Anon(edge) => edge.propagate(db, query),
        }
    }

    fn propagate_glob(self) -> PropagationResult {
        match self {
            EdgeKind::Lex(edge) => edge.propagate_glob(),
            EdgeKind::Mod(edge) => edge.propagate_glob(),
            EdgeKind::Type(edge) => edge.propagate_glob(),
            EdgeKind::Trait(edge) => edge.propagate_glob(),
            EdgeKind::TraitType(edge) => edge.propagate_glob(),
            EdgeKind::GenericParam(edge) => edge.propagate_glob(),
            EdgeKind::Value(edge) => edge.propagate_glob(),
            EdgeKind::Field(edge) => edge.propagate_glob(),
            EdgeKind::Variant(edge) => edge.propagate_glob(),
            EdgeKind::Super(edge) => edge.propagate_glob(),
            EdgeKind::Ingot(edge) => edge.propagate_glob(),
            EdgeKind::Self_(edge) => edge.propagate_glob(),
            EdgeKind::SelfTy(edge) => edge.propagate_glob(),
            EdgeKind::Anon(edge) => edge.propagate_glob(),
        }
    }
}
