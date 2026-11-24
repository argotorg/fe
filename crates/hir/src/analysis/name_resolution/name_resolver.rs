use std::{cmp, fmt, mem};

use common::ingot::Ingot;

use crate::{
    core::{
        hir_def::{
            Enum, EnumVariant, GenericParam, GenericParamOwner, HirIngot, IdentId, ItemKind, Mod,
            TopLevelMod, Trait, Use, Visibility,
            prim_ty::PrimTy,
            scope_graph::{
                EdgeKind, FieldEdge, GenericParamEdge, ImplEdge, ModEdge, ScopeId, TraitEdge,
                TraitTypeEdge, TypeEdge, ValueEdge, VariantEdge,
            },
        },
        span::DynLazySpan,
    },
    hir_def::scope_graph::ScopeEdge,
};
use bitflags::bitflags;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::SalsaValue;
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
    #[returns(copy)]
    name: IdentId<'db>,
    /// The scope where the name is resolved.
    #[returns(copy)]
    pub(super) scope: ScopeId<'db>,
    #[returns(copy)]
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

    /// A quote-local probe follows lexical parents only within generated code.
    stay_inside_quote: bool,

    /// If `allow_assoc_fn` is `false`, functions defined in an `impl`,
    /// `impl trait` or `trait` scope are not visible. This is the case for
    /// queries propagated to a lexical parent: like in Rust, associated
    /// functions are not in scope as bare names inside their own impl or
    /// trait and must be referred to via `Self::f` or `self.f()`.
    allow_assoc_fn: bool,

    /// If `allow_enclosing_item_bindings` is `false`, parameters, `Self`, and
    /// associated items of enclosing functions, impls, or traits are not
    /// visible when resolving names through a generated function's lexical
    /// origin.
    allow_enclosing_item_bindings: bool,
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
            stay_inside_quote: false,
            allow_assoc_fn: true,
            allow_enclosing_item_bindings: true,
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

    pub(super) fn quoted_locals(self) -> Self {
        Self {
            stay_inside_quote: true,
            ..self.disallow_external()
        }
    }

    pub(super) fn allows_lex_at(self, db: &dyn HirAnalysisDb, scope: ScopeId) -> bool {
        self.allow_lex && !(self.stay_inside_quote && scope.is_published(db))
    }

    fn disallow_assoc_fn(mut self) -> Self {
        self.allow_assoc_fn = false;
        self
    }

    pub(crate) fn disallow_enclosing_item_bindings(mut self) -> Self {
        self.allow_enclosing_item_bindings = false;
        self
    }
}

impl Default for QueryDirective {
    fn default() -> Self {
        Self::new()
    }
}

/// Which items of its own ingot a lookup may see.
///
/// Inside the ingot that generates them, generated items only fill gaps: a
/// name, member or implementation that the ingot's own code or a dependency
/// already answers keeps that answer, and generated items are consulted only
/// where nothing else answers. A lookup procedure therefore runs in the
/// `Authored` tier first, and in the `All` tier only if the whole procedure
/// found nothing (see `authored_first`). A dependency's sealed generated items
/// are ordinary items in both tiers.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum Tier {
    /// Without the items generated in the ingot the lookup starts in.
    Authored,
    /// Every item.
    All,
}

impl Tier {
    /// The tier to use for a lookup that starts in `ingot`: an ingot without
    /// generated items has one tier, so its lookups share their cache entries
    /// whichever tier asked.
    pub fn in_ingot<'db>(self, db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        match self {
            Tier::Authored if *ingot_has_generated_items(db, ingot) => Tier::Authored,
            _ => Tier::All,
        }
    }

    /// Like [`Tier::in_ingot`], for the lookup of a name or an import. Only an
    /// item that generation published into a module is bound by its name there
    /// (see `ScopeId::is_generated_in`); the members that a provider gives to
    /// a derive header join the header, not a module. A package whose only
    /// generation is derive headers therefore has one answer for every name and
    /// every import, and shares its cache entries between the tiers.
    pub fn in_ingot_for_names<'db>(self, db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> Self {
        match self {
            Tier::Authored if *ingot_publishes_items(db, ingot) => Tier::Authored,
            _ => Tier::All,
        }
    }

    /// The tier to use in the tables of `table` for a lookup that starts in
    /// `origin`: only the origin ingot's own tables hide its generated items,
    /// because the generated items of any other ingot are sealed.
    pub fn for_table<'db>(
        self,
        db: &'db dyn HirAnalysisDb,
        origin: Ingot<'db>,
        table: Ingot<'db>,
    ) -> Self {
        if origin == table {
            self.in_ingot(db, table)
        } else {
            Tier::All
        }
    }

    /// The ingot whose generated items the tier hides for a lookup that
    /// starts in `origin`.
    pub fn hides<'db>(self, origin: Ingot<'db>) -> Option<Ingot<'db>> {
        (self == Tier::Authored).then_some(origin)
    }
}

/// Whether generation produced code in a file of `ingot`: an item published
/// into a module, or members given to an authored derive header, which can hold
/// nested items such as implementations. Generation is described by the
/// database's generation inputs, so this does not depend on lowering any
/// module.
#[salsa::tracked]
pub fn ingot_has_generated_items<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> bool {
    let inputs = db.generation_inputs();
    let functions = inputs.functions(db);
    let requests = inputs.requests(db);
    if functions.is_empty() && requests.is_empty() {
        return false;
    }
    ingot
        .files(db)
        .iter()
        .any(|(_, file)| common::generation::file_generation(db, file).is_some())
}

/// Whether generation published an item into a module of `ingot`: a
/// module-level generated item or the named implementation of a scoped
/// derive. The members of a derive header are not such items.
#[salsa::tracked]
pub fn ingot_publishes_items<'db>(db: &'db dyn HirAnalysisDb, ingot: Ingot<'db>) -> bool {
    ingot.files(db).iter().any(|(_, file)| {
        common::generation::file_generation(db, file)
            .as_ref()
            .is_some_and(|generation| {
                !generation.functions.is_empty() || !generation.items.is_empty()
            })
    })
}

/// Runs a lookup procedure in the authored tier and, only if it found nothing
/// there (`found` is false), in the full tier. This is the one place where the
/// full tier is reached as a fallback; every procedure that chooses among
/// names, members or implementations goes through it.
///
/// The procedure must contain every lookup it needs to decide, so that a
/// generated item never wins part of a decision that authored items made: a
/// lookup that is one step of a longer walk (a block, its enclosing blocks and
/// then the module) belongs inside the closure, not around each step.
pub(crate) fn authored_first<'db, T>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    procedure: impl FnMut(Tier) -> T,
    found: impl Fn(&T) -> bool,
) -> T {
    authored_first_filling(db, ingot, procedure, found, |_authored, all| all)
}

/// Like [`authored_first`], for a procedure whose authored result still
/// matters when the full tier supplies nothing: `fill` chooses between the two
/// results when the authored tier found nothing.
pub(crate) fn authored_first_filling<'db, T>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    mut procedure: impl FnMut(Tier) -> T,
    found: impl Fn(&T) -> bool,
    fill: impl FnOnce(T, T) -> T,
) -> T {
    if Tier::Authored.in_ingot(db, ingot) == Tier::Authored {
        let authored = procedure(Tier::Authored);
        if found(&authored) {
            return authored;
        }
        let all = procedure(Tier::All);
        return fill(authored, all);
    }
    procedure(Tier::All)
}

/// A decision that prefers one implementation to another, made in both tiers.
pub(crate) struct TieredChoice<T> {
    /// The answer the program takes: the authored tier's if it answers, and
    /// the full tier's only where the authored tier found nothing.
    pub chosen: T,
    /// The full tier's answer, when the authored tier answered and the full
    /// tier answers differently: generated implementations would change the
    /// choice. The program keeps `chosen`; the generated implementations that
    /// would change it are refused where they are requested.
    pub changed_to: Option<T>,
}

/// Makes a decision that prefers one implementation to another (an ordered
/// choice, such as the first of several traits that applies) in the authored
/// tier, and in the full tier to learn whether generated implementations would
/// change it. Unlike [`authored_first`], which stops at the first tier that
/// finds an answer, this also runs the full tier when the authored tier
/// answered, because a changed choice is an error to report and not a gap to
/// fill. An ingot without generated code has one tier and no change.
///
/// `procedure` is given the tier and whether its answer is only compared (the
/// full tier after an authored answer): a procedure that changes the state of
/// a checker must undo that for a comparison. `same` says whether two answers
/// are the same choice.
pub(crate) fn choose_in_tiers<'db, T>(
    db: &'db dyn HirAnalysisDb,
    ingot: Ingot<'db>,
    mut procedure: impl FnMut(Tier, bool) -> T,
    found: impl Fn(&T) -> bool,
    same: impl Fn(&T, &T) -> bool,
) -> TieredChoice<T> {
    if Tier::Authored.in_ingot(db, ingot) == Tier::All {
        return TieredChoice {
            chosen: procedure(Tier::All, false),
            changed_to: None,
        };
    }
    let authored = procedure(Tier::Authored, false);
    if !found(&authored) {
        return TieredChoice {
            chosen: procedure(Tier::All, false),
            changed_to: None,
        };
    }
    let all = procedure(Tier::All, true);
    let changed = !same(&all, &authored);
    TieredChoice {
        chosen: authored,
        changed_to: changed.then_some(all),
    }
}

/// The struct contains the lookup result of a name query.
/// The results can contain more than one name resolutions which belong to
/// different name domains.
#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, SalsaValue)]
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

    /// Whether the lookup answered in any domain: it found a name, or the name
    /// is ambiguous, invisible or in conflict. Only a lookup that found
    /// nothing leaves a name to the generated items of a lower tier.
    pub fn answers(&self) -> bool {
        self.bucket
            .iter()
            .any(|(_, res)| !matches!(res, Err(NameResolutionError::NotFound)))
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

    pub(super) fn merge(&mut self, db: &dyn HirAnalysisDb, bucket: &NameResBucket<'db>) {
        for (domain, err) in bucket.errors() {
            if let Err(NameResolutionError::NotFound) = self.pick(domain) {
                self.bucket.push((domain, Err(err.clone())));
            }
        }
        for res in bucket.iter_ok() {
            self.push(db, res);
        }
    }

    pub(super) fn set_derivation(&mut self, derivation: NameDerivation<'db>) {
        for res in self.iter_ok_mut() {
            res.derivation = derivation.clone();
        }
    }

    /// Pushes `res` into the bucket. In each of its domains, `res` shadows a
    /// resolution of lower precedence, and is ambiguous with a resolution of a
    /// different scope of the same precedence. Of two imports of the same
    /// scope, the more visible one is kept.
    fn push(&mut self, db: &dyn HirAnalysisDb, res: &NameRes<'db>) {
        for domain in res.domain.iter() {
            let Some((_, existing)) = self.bucket.iter_mut().find(|(d, _)| *d == domain) else {
                self.bucket.push((domain, Ok(res.clone())));
                continue;
            };
            match existing {
                Ok(old) => match res.derivation.cmp_precedence(&old.derivation) {
                    cmp::Ordering::Less => {}
                    cmp::Ordering::Equal if old.kind == res.kind => {
                        if res.import_vis_rank(db) > old.import_vis_rank(db) {
                            *existing = Ok(res.clone())
                        }
                    }
                    cmp::Ordering::Equal => {
                        *existing = Err(NameResolutionError::Ambiguous(ThinVec::from([
                            old.clone(),
                            res.clone(),
                        ])))
                    }
                    cmp::Ordering::Greater => *existing = Ok(res.clone()),
                },
                Err(NameResolutionError::Ambiguous(cands)) => {
                    match res.derivation.cmp_precedence(&cands[0].derivation) {
                        cmp::Ordering::Less => {}
                        cmp::Ordering::Equal => {
                            if cands.iter().all(|cand| cand.kind != res.kind) {
                                cands.push(res.clone());
                            }
                        }
                        cmp::Ordering::Greater => *existing = Ok(res.clone()),
                    }
                }
                Err(NameResolutionError::NotFound) => *existing = Ok(res.clone()),
                Err(_) => {}
            }
        }
    }

    fn set_lexed_derivation(&mut self) {
        for (_, res) in &mut self.bucket {
            match res {
                Ok(res) => res.derivation.lexed(),
                Err(NameResolutionError::Ambiguous(cands)) => {
                    cands.iter_mut().for_each(|cand| cand.derivation.lexed())
                }
                Err(_) => {}
            }
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
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
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
    /// A local module alias can give the rest of a quoted path producer
    /// access only for another path copied from the same quotation.
    pub(super) fn quoted_module_alias(
        &self,
        db: &'db dyn HirAnalysisDb,
        origin: crate::hir_def::QuotedPathOrigin<'db>,
    ) -> bool {
        let NameDerivation::NamedImported(import) = self.derivation.origin() else {
            return false;
        };
        import
            .path(db)
            .to_opt()
            .and_then(|path| path.quote_origin(db))
            == Some(origin)
            && self
                .scope()
                .is_some_and(|scope| super::visibility_checker::is_authored_module(db, scope))
    }

    /// Returns `true` if the name is visible from the given `scope`.
    pub fn is_visible(&self, db: &'db dyn HirAnalysisDb, from: ScopeId<'db>) -> bool {
        match self.derivation.origin() {
            NameDerivation::NamedImported(use_) | NameDerivation::GlobImported(use_) => {
                is_use_visible(db, from, *use_)
            }
            _ => match self.kind {
                NameResKind::Scope(scope) => is_scope_visible_from(db, scope, from),
                NameResKind::Prim(_) => true,
            },
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
        match self.derivation.origin() {
            NameDerivation::NamedImported(use_) => use_.imported_name_span(db),
            NameDerivation::GlobImported(use_) => use_.glob_span(db),
            _ => self.kind.name_span(db),
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

    /// Returns how widely the use that the name is imported through is
    /// visible, from private up to `pub`.
    fn import_vis_rank(&self, db: &dyn HirAnalysisDb) -> Option<u8> {
        self.derivation.use_stmt().map(|use_| match use_.vis(db) {
            Visibility::Private => 0,
            Visibility::PubSuper => 1,
            Visibility::PubIngot => 2,
            Visibility::Public => 3,
        })
    }

    pub(super) fn is_importable(&self) -> bool {
        matches!(
            self.domain,
            NameDomain::TYPE | NameDomain::VALUE | NameDomain::IMPL
        )
    }

    fn new_prim(prim: PrimTy) -> Self {
        Self {
            kind: prim.into(),
            derivation: NameDerivation::Prim,
            domain: NameDomain::TYPE,
        }
    }
}

#[derive(
    Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, derive_more::From, SalsaValue,
)]
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
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
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

impl<'db> NameDerivation<'db> {
    fn lexed(&mut self) {
        let inner = mem::replace(self, NameDerivation::Def);
        *self = NameDerivation::Lex(Box::new(inner));
    }

    /// Returns the derivation in the scope that the name is found in, looking
    /// through lexical parents.
    pub fn origin(&self) -> &Self {
        match self {
            NameDerivation::Lex(inner) => inner.origin(),
            _ => self,
        }
    }

    pub fn use_stmt(&self) -> Option<Use<'db>> {
        match self.origin() {
            NameDerivation::NamedImported(use_) | NameDerivation::GlobImported(use_) => Some(*use_),
            _ => None,
        }
    }

    /// Compares which of two derivations of a name shadows the other: in a
    /// scope, a definition shadows a named import, which shadows a glob
    /// import, and those shadow the names of lexical parents, external ingots
    /// and builtin types, in that order.
    pub(super) fn cmp_precedence(&self, other: &Self) -> cmp::Ordering {
        let rank = |derivation: &Self| match derivation {
            NameDerivation::Prim => 0,
            NameDerivation::External => 1,
            NameDerivation::Lex(_) => 2,
            NameDerivation::GlobImported(_) => 3,
            NameDerivation::NamedImported(_) => 4,
            NameDerivation::Def => 5,
        };
        match (self, other) {
            (NameDerivation::Lex(lhs), NameDerivation::Lex(rhs)) => lhs.cmp_precedence(rhs),
            _ => rank(self).cmp(&rank(other)),
        }
    }
}

pub(crate) struct NameResolver<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    importer: &'a dyn Importer<'db>,
    /// The ingot whose generated items this resolver does not see: the authored
    /// tier of a lookup that starts in it. Items generated in other ingots are
    /// ordinary.
    hide: Option<Ingot<'db>>,
}

impl<'db, 'a> NameResolver<'db, 'a> {
    pub(super) fn new(
        db: &'db dyn HirAnalysisDb,
        importer: &'a dyn Importer<'db>,
        hide: Option<Ingot<'db>>,
    ) -> Self {
        Self { db, importer, hide }
    }

    /// Whether the resolver does not see `scope`: a generated item of the
    /// ingot whose generated items the lookup does not use.
    fn hides(&self, scope: ScopeId<'db>) -> bool {
        self.hide
            .is_some_and(|ingot| scope.is_generated_in(self.db, ingot))
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
        let s_graph = scope.scope_graph(self.db);

        {
            // Edges only need the query's name and whether lexical scopes apply.
            let name = query.name(self.db);
            let allow_lex = query.directive(self.db).allows_lex_at(self.db, scope);
            let mut process_edge =
                |edge: &ScopeEdge<'db>| match edge.kind.propagate(self.db, name, allow_lex) {
                    PropagationResult::Terminated => {
                        if !self.hides(edge.dest) && found_scopes.insert(edge.dest) {
                            let res = NameRes::new_from_scope(
                                edge.dest,
                                NameDomain::from_edge(self.db, edge),
                                NameDerivation::Def,
                            );
                            bucket.push(self.db, &res);
                        }
                    }

                    PropagationResult::Continuation => {
                        debug_assert!(parent.is_none());
                        parent = Some(edge.dest);
                    }

                    PropagationResult::UnPropagated => {}
                };

            let hide_assoc_fns = !query.directive(self.db).allow_assoc_fn
                && matches!(
                    scope,
                    ScopeId::Item(ItemKind::Impl(_) | ItemKind::ImplTrait(_) | ItemKind::Trait(_))
                );
            let only_lex = !query.directive(self.db).allow_enclosing_item_bindings
                && !matches!(
                    scope,
                    ScopeId::Block(..) | ScopeId::Item(ItemKind::Mod(_) | ItemKind::TopMod(_))
                );
            for edge in s_graph.edges(scope) {
                if only_lex && !matches!(edge.kind, EdgeKind::Lex(_)) {
                    continue;
                }
                if hide_assoc_fns && matches!(edge.dest, ScopeId::Item(ItemKind::Func(_))) {
                    continue;
                }
                process_edge(edge);
            }
        }

        // 2. Look for the name in the named imports of the current scope.
        if let Some(imported) = self
            .importer
            .named_imports(self.db, query.scope(self.db))
            .and_then(|imports| imports.get(&query.name(self.db)))
        {
            bucket.merge(self.db, imported);
        }

        // 3. Look for the name in the glob imports.
        if let Some(imported) = self.importer.glob_imports(self.db, query.scope(self.db)) {
            for res in imported.name_res_for(query.name(self.db)) {
                bucket.push(self.db, res);
            }
        }

        // 4. Look for the name in the lexical scope if it exists.
        if let Some(parent) = parent {
            let mut directive = query
                .directive(self.db)
                .disallow_external()
                .disallow_assoc_fn();
            if scope.data(self.db).publication_parent.is_some() {
                directive = directive.disallow_enclosing_item_bindings();
            }
            let query_for_parent =
                EarlyNameQueryId::new(self.db, query.name(self.db), parent, directive);

            let mut resolved = self.resolve_query(query_for_parent);
            resolved.set_lexed_derivation();
            bucket.merge(self.db, &resolved);
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
                    bucket.push(
                        self.db,
                        &NameRes::new_from_scope(
                            ScopeId::from_item((ingot.root_mod(self.db)).into()),
                            NameDomain::TYPE,
                            NameDerivation::External,
                        ),
                    )
                }
            });

        // 6. Look for the name in the builtin types.
        if let Some(prim) = PrimTy::from_name(self.db, query.name(self.db)) {
            // We don't care about the result of `push` because we assume builtin types are
            // guaranteed to be unique.
            bucket.push(self.db, &NameRes::new_prim(prim));
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
    /// - We can't arbitrarily choose one of several candidates without hiding
    ///   the ambiguity, which is why [`NameResBucket::merge`] isn't used here.
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
    ) -> FxHashMap<IdentId<'db>, Vec<NameRes<'db>>> {
        let mut res_collection: FxHashMap<IdentId, Vec<NameRes>> = FxHashMap::default();
        let mut found_domains: FxHashMap<IdentId, NameDomain> = FxHashMap::default();
        let mut found_kinds: FxHashSet<(IdentId, NameResKind)> = FxHashSet::default();

        for edge in target.edges(self.db) {
            if !edge.kind.is_glob_imported() {
                continue;
            }
            let scope = edge.dest;
            if self.hides(scope) {
                continue;
            }

            let name = scope.name(self.db).unwrap();
            if !found_kinds.insert((name, scope.into())) {
                continue;
            }
            let res = NameRes::new_from_scope(
                scope,
                NameDomain::from_edge(self.db, edge),
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
                for (&name, res_for_name) in resolutions.iter() {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, SalsaValue)]
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
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SalsaValue)]
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
            /// A named implementation's name.
            const IMPL = 0b1000;
            const Invalid = 0b0;
        }
}

#[allow(non_upper_case_globals)]
impl NameDomain {
    /// The domain of a name found through `edge`: a named implementation's
    /// name is in its own domain, while its scope is a type's (where `Self`
    /// resolves inside it).
    pub(super) fn from_edge(db: &dyn HirAnalysisDb, edge: &ScopeEdge<'_>) -> Self {
        match edge.kind {
            EdgeKind::Impl(_) => Self::IMPL,
            _ => Self::from_scope(db, edge.dest),
        }
    }

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
            ScopeId::ImplConst(..) => Self::VALUE,
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

impl<'db> EdgeKind<'db> {
    /// Returns how a query for `name` propagates along an edge of this kind.
    fn propagate(
        self,
        db: &'db dyn HirAnalysisDb,
        name: IdentId<'db>,
        allow_lex: bool,
    ) -> PropagationResult {
        let terminated = match self {
            EdgeKind::Lex(_) if allow_lex => return PropagationResult::Continuation,
            EdgeKind::Mod(ModEdge(ident))
            | EdgeKind::Type(TypeEdge(ident))
            | EdgeKind::Trait(TraitEdge(ident))
            | EdgeKind::TraitType(TraitTypeEdge(ident))
            | EdgeKind::GenericParam(GenericParamEdge(ident))
            | EdgeKind::Value(ValueEdge(ident))
            | EdgeKind::Impl(ImplEdge(ident))
            | EdgeKind::Field(FieldEdge(ident))
            | EdgeKind::Variant(VariantEdge(ident)) => ident == name,
            EdgeKind::Super(_) => name.is_super(db),
            EdgeKind::Ingot(_) => name.is_ingot(db),
            EdgeKind::Self_(_) => name.is_self(db),
            EdgeKind::SelfTy(_) => name.is_self_ty(db),
            EdgeKind::Lex(_) | EdgeKind::Anon(_) => false,
        };
        if terminated {
            PropagationResult::Terminated
        } else {
            PropagationResult::UnPropagated
        }
    }

    /// Returns `true` if a glob import of a scope imports the destinations of
    /// its edges of this kind.
    fn is_glob_imported(self) -> bool {
        matches!(
            self,
            EdgeKind::Mod(_)
                | EdgeKind::Type(_)
                | EdgeKind::Trait(_)
                | EdgeKind::TraitType(_)
                | EdgeKind::Value(_)
                | EdgeKind::Impl(_)
                | EdgeKind::Variant(_)
        )
    }
}
