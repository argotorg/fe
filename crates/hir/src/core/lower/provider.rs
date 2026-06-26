//! Derive-provider discovery, validation, and selection.
//!
//! Derive providers (`impl Derive<Goal> for Provider { const fn derive .. }`)
//! are ordinary HIR items written in Fe. The expansion stage discovers them
//! across the requesting ingot and its dependencies, validates their shape,
//! selects one per derive request (canonical core provider for bare
//! `#[derive(..)]`/`derive ..` requests, named lookup for `using ..`
//! selections), and runs the selected provider's body through the
//! command-language executor ([`super::provider_executor`]).
//!
//! Stratification: everything here reads *base* scope graphs only
//! ([`base_scope_graph_impl`]), never the merged
//! [`scope_graph_impl`](super::scope_graph_impl) — reading the merged graph
//! of any module of the requesting ingot would cycle back into expansion.

use common::ingot::{Ingot, IngotKind};
use rustc_hash::FxHashSet;

use super::base_scope_graph_impl;
use crate::{
    HirDb,
    hir_def::{
        Body, Func, GenericArg, HirIngot, IdentId, ImplTrait, ItemKind, PathId, TopLevelMod, Trait,
        TraitRefId, TypeId, TypeKind, UsePathSegment,
        scope_graph::{ScopeGraph, ScopeId},
    },
    span::{DesugaredOrigin, DynLazySpan, HirOrigin},
};

/// The single function a provider must define.
const DERIVE_FN: &str = "derive";
/// The `core::derive` module that holds the canonical capability types
/// (`Reflect`, `ImplBuilder`), the `Evidence` witness, the unforgeable `ImplPermit`
/// one-of-a-kind capability, and the `Derive` provider trait, used for
/// resolved-identity recognition.
const DERIVE_MODULE: &str = "derive";
/// The canonical last-segment names of the `core::derive` items. These are the
/// *identity* the recognized canonical path / def scope must end in — they are
/// matched only behind the `core::derive` module qualifier (path side, see
/// [`path_core_derive_item`]) or `core`-ingot `derive` module (scope side, see
/// `provider_goal::scope_is_core_derive_item`), never as a bare head-identifier
/// authority.
const REFLECT_TY: &str = "Reflect";
const IMPL_BUILDER_TY: &str = "ImplBuilder";
const EVIDENCE_TY: &str = "Evidence";
const IMPL_PERMIT_TY: &str = "ImplPermit";
const PERMIT_AUTHORITY_TY: &str = "PermitAuthority";
/// The canonical last-segment name of the `core::derive` provider trait. Like
/// the capability types, it is matched only behind the `core::derive` module
/// qualifier — the trait ref `Derive<Goal>` of an `impl Derive<Goal> for P` is
/// recognized by its resolved `core::derive::Derive` identity, never by a bare
/// string.
const DERIVE_TRAIT_TY: &str = "Derive";

/// A canonical `core::derive` item recognized by RESOLVED IDENTITY (its name +
/// the `core`-ingot `derive` module qualifier), NOT by a bare head-identifier
/// string. This is the single recognized SET shared by both resolved-identity
/// recognizers: the base-graph path-keyed one here ([`path_core_derive_item`])
/// and the merged-graph scope-keyed one in `provider_goal`
/// (`scope_is_core_derive_item`). A user type merely *named* `Reflect` /
/// `Evidence` / `ImplPermit`, without `use core::derive::..`, does not resolve to the
/// canonical item and is granted ZERO authority.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CoreDeriveItem {
    /// `core::derive::Derive` — the provider trait (the marker after `:`).
    Derive,
    /// `core::derive::Evidence` — the witness type.
    Evidence,
    /// `core::derive::ImplBuilder` — the generated-impl builder capability.
    ImplBuilder,
    /// `core::derive::Reflect` — reflection-over-the-target capability.
    Reflect,
    /// `core::derive::ImplPermit`: the unforgeable one-of-a-kind capability,
    /// permission to establish the one allowed impl of a one-of-a-kind goal.
    /// Recognition is live (this item is part of the recognized SET); the gate
    /// that turns recognition into establish authority is a later increment.
    ImplPermit,
    /// `core::derive::PermitAuthority`: authority to mint an `ImplPermit<G>` for a
    /// single-impl goal. Recognized by identity; inert (nothing mints or consumes
    /// it yet).
    PermitAuthority,
}

impl CoreDeriveItem {
    /// The canonical last-segment name of this item.
    pub(crate) fn name(self) -> &'static str {
        match self {
            CoreDeriveItem::Derive => DERIVE_TRAIT_TY,
            CoreDeriveItem::Evidence => EVIDENCE_TY,
            CoreDeriveItem::ImplBuilder => IMPL_BUILDER_TY,
            CoreDeriveItem::Reflect => REFLECT_TY,
            CoreDeriveItem::ImplPermit => IMPL_PERMIT_TY,
            CoreDeriveItem::PermitAuthority => PERMIT_AUTHORITY_TY,
        }
    }

    /// Every canonical item, in declaration order — the recognized SET.
    const ALL: [CoreDeriveItem; 6] = [
        CoreDeriveItem::Derive,
        CoreDeriveItem::Evidence,
        CoreDeriveItem::ImplBuilder,
        CoreDeriveItem::Reflect,
        CoreDeriveItem::ImplPermit,
        CoreDeriveItem::PermitAuthority,
    ];
}

/// A compile-time capability a provider's `derive` fn consumes, declared in its
/// `uses (..)` clause (`reflect: Reflect<T>`, `builder: mut ImplBuilder<Goal>`).
/// The variant carries the binding name introduced for the capability.
///
/// Capabilities are recognized (K04a-C3, landed) by the capability type's
/// resolved canonical-path identity — `core::derive::Reflect` /
/// `core::derive::ImplBuilder` — established through the provider module's base-
/// graph `use` items (see [`path_core_derive_item`]). There is no string-key
/// fallback: a user type merely *named* `Reflect`, without importing the
/// canonical type, grants NO capability authority. This enum is the home for the
/// recognized capability (and any future grade/scope).
#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub(super) enum Capability<'db> {
    /// `reflect: Reflect<T>` — reflection over the derive target.
    Reflect(IdentId<'db>),
    /// `builder: mut ImplBuilder<Goal>` — the generated-impl builder.
    ImplBuilder(IdentId<'db>),
}

impl<'db> Capability<'db> {
    /// The binding name introduced for this capability in `uses (..)`.
    pub(super) fn binding(self) -> IdentId<'db> {
        match self {
            Capability::Reflect(name) | Capability::ImplBuilder(name) => name,
        }
    }

    pub(super) fn is_reflect(self) -> bool {
        matches!(self, Capability::Reflect(_))
    }

    pub(super) fn is_builder(self) -> bool {
        matches!(self, Capability::ImplBuilder(_))
    }
}

/// A derive provider that passed the HIR-level shape validation and can be
/// selected and executed by the expansion stage.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) struct ValidatedProvider<'db> {
    /// The ordinary `impl Derive<Goal> for Provider` declaration that this
    /// validated provider was lowered from. Identified by stable identity (its
    /// `top_mod` plus the underlying salsa [`ImplTrait`] node), not by name
    /// string.
    pub(super) provider: ImplTrait<'db>,
    /// The provider's `derive` function.
    pub(super) func: Func<'db>,
    /// The function's body (validated present).
    pub(super) body: Body<'db>,
    /// The provider's name (`StableEq` in `impl Derive<Eq> for StableEq`).
    pub(super) name: IdentId<'db>,
    /// The last segment of the head trait path (`Eq`). Used for
    /// diagnostics and for the cross-module-safe bare-ident selection adapter
    /// (`goal_matches_provider`); provider selection otherwise keys on
    /// [`Self::trait_path`] identity, not on this string (W-C).
    pub(super) head_name: IdentId<'db>,
    /// The canonical path of the head trait, resolved against the provider
    /// module's `use` items (e.g. `core::ops::Eq`). Generated impls name the
    /// trait through this path so resolution does not depend on imports at
    /// the derive site. W-C: it is also the provider's selection *identity* —
    /// a derive request whose goal canonicalizes to this same path selects
    /// this provider, regardless of how the goal was spelled at the site.
    pub(super) trait_path: PathId<'db>,
    /// The compile-time capabilities the provider consumes, in `uses (..)`
    /// declaration order (`Reflect<..>`, `mut ImplBuilder<..>`).
    pub(super) capabilities: Vec<Capability<'db>>,
    /// Names of the function's ordinary parameters (e.g. `ev`); bound as
    /// opaque evidence values during execution.
    pub(super) param_names: Vec<IdentId<'db>>,
}

/// A provider shape error, reported on the module that declares the
/// provider.
#[derive(Debug, Clone)]
pub(super) struct ProviderShapeError {
    pub(super) message: String,
    pub(super) range: parser::TextRange,
}

/// The func-level fields a validated provider carries, plus any shape errors
/// found while extracting them.
struct ProviderFnValidation<'db> {
    func: Option<Func<'db>>,
    body: Option<Body<'db>>,
    capabilities: Vec<Capability<'db>>,
    param_names: Vec<IdentId<'db>>,
    errors: Vec<ProviderShapeError>,
}

/// Validates the `derive` function of a provider `decl`: it finds the one
/// `derive` fn among the declaration's base-graph children, checks it is a
/// `const fn` with a body, and recognizes the `uses (..)` capabilities by
/// resolved `core::derive` identity. `error` builds a [`ProviderShapeError`]
/// with the declaration's fallback range.
///
/// All reads are *base* scope graphs (the expansion stage must not touch the
/// merged graph).
fn validate_provider_fn<'db>(
    db: &'db dyn HirDb,
    decl: ImplTrait<'db>,
    error: &impl Fn(String) -> ProviderShapeError,
) -> ProviderFnValidation<'db> {
    let mut errors = Vec::new();
    let top_mod = decl.top_mod(db);

    // The provider's `derive` function, found through the *base* scope graph of
    // the declaring module (the merged-graph `methods` reader must not run in
    // the expansion stage).
    let base = base_scope_graph_impl(db, top_mod);
    let mut derive_fns = base
        .child_items(decl.scope())
        .filter_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == DERIVE_FN) =>
            {
                Some(func)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let func = match derive_fns.len() {
        1 => Some(derive_fns.remove(0)),
        0 => {
            errors.push(error(
                "derive provider declarations must contain one `derive` function".into(),
            ));
            None
        }
        _ => {
            errors.push(error(
                "derive provider declarations may contain only one `derive` function".into(),
            ));
            None
        }
    };

    let mut body = None;
    let mut capabilities = Vec::new();
    let mut param_names = Vec::new();
    if let Some(func) = func {
        if !func.is_const(db) {
            errors.push(error("derive provider functions must be `const fn`".into()));
        }
        body = func.body(db);
        if body.is_none() {
            errors.push(error("derive provider functions must have a body".into()));
        }

        for param in func.effects(db).data(db) {
            let Some(param_name) = param.name else {
                continue;
            };
            let Some(key_path) = param.key_path.to_opt() else {
                continue;
            };
            let Some(key_head) = last_path_ident(db, key_path) else {
                continue;
            };
            // Recognize the capability by RESOLVED IDENTITY only (K04a-C3): the
            // capability key path is canonicalized (base-graph `use` resolution
            // only — provider validation runs in the expansion stage and must not
            // touch the merged graph) and accepted only when it names a
            // `core::derive` capability type by its canonical-path identity
            // (`derive::Reflect` / `derive::ImplBuilder`). A user type merely named
            // `Reflect`, with no `use core::derive::Reflect`, no longer collides
            // with the capability — the bare head-identifier string fallback is
            // gone, so the name alone grants no authority.
            let canonical = if key_path.parent(db).is_some() {
                key_path
            } else {
                canonical_trait_path(db, top_mod, PathId::from_ident(db, key_head))
            };
            match path_core_derive_item(db, canonical) {
                Some(CoreDeriveItem::Reflect) => capabilities.push(Capability::Reflect(param_name)),
                Some(CoreDeriveItem::ImplBuilder) if param.is_mut => {
                    capabilities.push(Capability::ImplBuilder(param_name))
                }
                _ => {}
            }
        }
        // The minimal capability check: a provider must declare the
        // capabilities it consumes. The full key/grade capability system is
        // a later milestone.
        if !capabilities
            .iter()
            .any(|capability| capability.is_reflect())
        {
            errors.push(error(
                "derive provider functions must declare a `Reflect<..>` capability in `uses (..)`"
                    .into(),
            ));
        }
        if !capabilities
            .iter()
            .any(|capability| capability.is_builder())
        {
            errors.push(error(
                "derive provider functions must declare a `mut ImplBuilder<..>` capability in `uses (..)`"
                    .into(),
            ));
        }

        if let Some(params) = func.params_list(db).to_opt() {
            for param in params.data(db) {
                if let Some(crate::hir_def::FuncParamName::Ident(ident)) = param.name.to_opt() {
                    param_names.push(ident);
                }
            }
        }
    }

    ProviderFnValidation {
        func,
        body,
        capabilities,
        param_names,
        errors,
    }
}

/// Validates the shape of a provider declared in the ORDINARY form
/// `impl Derive<Goal> for Provider`, returning the validated form or shape
/// errors. The recognition that this impl IS a `core::derive::Derive<..>`
/// provider (and the extraction of the goal path) is
/// [`impl_trait_provider_goal_path`]; this function assumes that has already
/// matched and fills in the rest of the [`ValidatedProvider`]:
///
/// * `name` — the implementor (`Self`) type's last-segment ident
///   (`StableEq` in `impl Derive<Eq> for StableEq`).
/// * goal — recovered again from the recognizer; `head_name` is its last
///   segment and `trait_path` its canonicalization.
/// * the `Derive` marker is implied (the trait ref already IS
///   `core::derive::Derive`).
/// * `func` / `body` / `capabilities` / `param_names` — via
///   [`validate_provider_fn`].
pub(super) fn validate_impl_provider<'db>(
    db: &'db dyn HirDb,
    impl_trait: ImplTrait<'db>,
) -> Result<ValidatedProvider<'db>, Vec<ProviderShapeError>> {
    let mut errors = Vec::new();
    let fallback_range = impl_provider_name_range(db, impl_trait);
    let error = |message: String| ProviderShapeError {
        message,
        range: fallback_range,
    };

    // The provider name is the implementor (`Self`) type's last-segment ident.
    let name = impl_trait
        .type_ref(db)
        .to_opt()
        .and_then(|ty| type_head_path(db, ty))
        .and_then(|path| last_path_ident(db, path));
    if name.is_none() {
        errors.push(error(
            "derive provider declarations must have a provider name".into(),
        ));
    }

    // The goal trait path, recovered from the `Derive<Goal>` argument by the
    // same recognizer that classified this impl as a provider.
    let goal_path = impl_trait_provider_goal_path(db, impl_trait);
    let head_name = goal_path.and_then(|path| last_path_ident(db, path));
    if head_name.is_none() {
        errors.push(error(
            "derive provider declarations must name a goal trait as the `Derive<..>` argument"
                .into(),
        ));
    }

    let func_level = validate_provider_fn(db, impl_trait, &error);
    errors.extend(func_level.errors);

    if !errors.is_empty() {
        return Err(errors);
    }
    let (Some(name), Some(head_name), Some(goal_path), Some(func), Some(body)) =
        (name, head_name, goal_path, func_level.func, func_level.body)
    else {
        return Err(errors);
    };

    let trait_path = canonical_trait_path(db, impl_trait.top_mod(db), goal_path);

    Ok(ValidatedProvider {
        provider: impl_trait,
        func,
        body,
        name,
        head_name,
        trait_path,
        capabilities: func_level.capabilities,
        param_names: func_level.param_names,
    })
}

/// The primary range for shape errors on an ordinary-form provider
/// (`impl Derive<Goal> for Provider`): the implementor (`Self`) type, or the
/// whole `impl` when the type is missing.
pub(super) fn impl_provider_name_range<'db>(
    db: &'db dyn HirDb,
    impl_trait: ImplTrait<'db>,
) -> parser::TextRange {
    use parser::ast::prelude::*;
    let root = super::top_mod_ast(db, impl_trait.top_mod(db));
    let crate::span::HirOrigin::Raw(ptr) = impl_trait.origin(db) else {
        return parser::TextRange::new(0.into(), 0.into());
    };
    let Some(ast) = ptr
        .syntax_node_ptr()
        .try_to_node(root.syntax())
        .and_then(parser::ast::ImplTrait::cast)
    else {
        return parser::TextRange::new(0.into(), 0.into());
    };
    ast.ty()
        .map(|ty| ty.syntax().text_range())
        .unwrap_or_else(|| ast.syntax().text_range())
}

/// The last identifier segment of `path`, if every segment is present.
fn last_path_ident<'db>(db: &'db dyn HirDb, path: PathId<'db>) -> Option<IdentId<'db>> {
    path.ident(db).to_opt()
}

/// Which [`CoreDeriveItem`] `path` names by resolved (canonical-path) identity,
/// or `None`. A path matches only when its parent segment is the `derive` module
/// AND its last segment is a canonical item name (`Derive` / `Evidence` /
/// `ImplBuilder` / `Reflect` / `ImplPermit` / `PermitAuthority`). This is the
/// base-graph-safe form of
/// identity recognition — it requires the `derive` module qualifier (so a bare
/// user `struct Reflect`, not imported from `core::derive`, does not match)
/// without needing the merged scope graph (unavailable during the expansion
/// stage). The caller canonicalizes a bare ident through the provider module's
/// `use` items before calling this, so an imported/aliased item is recognized
/// while a like-named local type is not. There is no bare-name fallback: the
/// name alone grants no authority.
///
/// This is the base-graph path-keyed sibling of the merged-graph scope-keyed
/// `provider_goal::scope_is_core_derive_item`; both recognize the one
/// [`CoreDeriveItem`] SET by the same (name + `core::derive` qualifier)
/// resolved identity.
fn path_core_derive_item<'db>(db: &'db dyn HirDb, path: PathId<'db>) -> Option<CoreDeriveItem> {
    let parent_is_derive = path
        .parent(db)
        .and_then(|parent| parent.ident(db).to_opt())
        .is_some_and(|ident| ident.data(db) == DERIVE_MODULE);
    if !parent_is_derive {
        return None;
    }
    let last = path.ident(db).to_opt()?;
    CoreDeriveItem::ALL
        .into_iter()
        .find(|item| last.data(db) == item.name())
}

/// The head [`PathId`] of `ty`, peeling any leading mode wrapper (`own`/`mut`/
/// `ref`). A goal type argument is an ordinary `TypeKind::Path` (`Eq`,
/// `core::ops::Eq`); other shapes (tuple, pointer, ..) cannot name a goal trait
/// and yield `None`.
fn type_head_path<'db>(db: &'db dyn HirDb, ty: TypeId<'db>) -> Option<PathId<'db>> {
    match ty.data(db) {
        TypeKind::Path(path) => path.to_opt(),
        TypeKind::Mode(_, inner) => inner.to_opt().and_then(|inner| type_head_path(db, inner)),
        _ => None,
    }
}

/// Recognizes the ORDINARY derive-provider form `impl Derive<Goal> for Provider`
/// and returns the GOAL trait path (the first generic arg of the `Derive<..>`
/// trait ref), or `None` when `impl_trait` is not a provider in this form.
///
/// Base-graph-safe (the expansion stage must not read the merged scope graph):
/// the trait-ref head path is canonicalized the SAME way the legacy `: Derive`
/// marker is ([`canonical_trait_path`] + [`path_core_derive_item`]) and accepted
/// ONLY when it resolves to `core::derive::Derive` by identity. A like-named
/// local `trait Derive`, not imported from `core::derive`, does not match.
///
/// The goal is the first `GenericArg::Type` of the trait ref, reduced to its
/// underlying head path (`Derive<Eq>` → `Eq`, `Derive<core::ops::Eq>` →
/// `core::ops::Eq`). Returns `None` if the trait ref has no type argument.
pub(crate) fn impl_trait_provider_goal_path<'db>(
    db: &'db dyn HirDb,
    impl_trait: ImplTrait<'db>,
) -> Option<PathId<'db>> {
    let trait_ref = impl_trait.hir_trait_ref(db).to_opt()?;
    let trait_path = trait_ref.path(db).to_opt()?;

    // The trait-ref head must resolve to `core::derive::Derive` by identity —
    // same canonicalization the legacy marker uses (base-graph `use` resolution).
    // Strip generic args first so the head is keyed on its name, then
    // canonicalize a bare ident through the impl module's `use` items.
    let head = trait_path.strip_generic_args(db);
    let canonical = if head.parent(db).is_some() {
        head
    } else {
        canonical_trait_path(db, impl_trait.top_mod(db), head)
    };
    if path_core_derive_item(db, canonical) != Some(CoreDeriveItem::Derive) {
        return None;
    }

    // The goal is the first type argument of `Derive<..>`.
    trait_path
        .generic_args(db)
        .data(db)
        .iter()
        .find_map(|arg| match arg {
            GenericArg::Type(type_arg) => type_arg.ty.to_opt(),
            _ => None,
        })
        .and_then(|ty| type_head_path(db, ty))
}

/// Resolves `path` (the head trait of a provider, or the trait argument of a
/// `require<Trait>` command) to a canonical path usable from *any* module:
///
/// * multi-segment paths are taken as written (assumed to start at an ingot
///   alias such as `core`);
/// * a single identifier is looked up among the provider module's `use`
///   items (e.g. `use core::ops::Eq` canonicalizes `Eq` to `core::ops::Eq`);
/// * otherwise the path is used as written — correct whenever the trait is
///   declared next to the derive target (same-module user providers).
///
/// This is a deliberately small, base-graph-only resolver: full import
/// resolution reads the merged scope graph and cannot run inside the
/// expansion stage.
pub(super) fn canonical_trait_path<'db>(
    db: &'db dyn HirDb,
    top_mod: TopLevelMod<'db>,
    path: PathId<'db>,
) -> PathId<'db> {
    if path.len(db) > 1 {
        return path;
    }
    let Some(name) = path.as_ident(db) else {
        return path;
    };

    let base = base_scope_graph_impl(db, top_mod);
    for item in base.items_dfs(db) {
        let ItemKind::Use(use_) = item else {
            continue;
        };
        let Some(use_path) = use_.path(db).to_opt() else {
            continue;
        };
        let segments = use_path.data(db);
        let Some(last) = segments
            .last()
            .and_then(|seg| seg.to_opt())
            .and_then(UsePathSegment::ident)
        else {
            continue;
        };
        let matches = match use_.alias(db) {
            Some(alias) => alias
                .to_opt()
                .map(|alias| match alias {
                    crate::hir_def::UseAlias::Ident(ident) => ident == name,
                    crate::hir_def::UseAlias::Underscore => false,
                })
                .unwrap_or(false),
            None => last == name,
        };
        if !matches {
            continue;
        }
        // Rebuild the use path as a value path ending in the *original*
        // (pre-alias) name.
        let mut idents = Vec::new();
        let mut ok = true;
        for seg in segments {
            match seg.to_opt() {
                Some(UsePathSegment::Ident(ident)) => idents.push(ident),
                _ => {
                    ok = false;
                    break;
                }
            }
        }
        if !ok || idents.is_empty() {
            continue;
        }
        let mut canonical = PathId::from_ident(db, idents[0]);
        for ident in &idents[1..] {
            canonical = canonical.push_ident(db, *ident);
        }
        return canonical;
    }
    path
}

/// A module reached while walking a trait path's leading segments. Submodules
/// are either separate files (their own [`TopLevelMod`], reached through the
/// module tree) or inline `mod` items (children of an enclosing top mod's
/// scope, reached through that top mod's base scope graph). Either way the
/// final-segment trait lookup reads only *base* scope graphs, so this is
/// stratification-safe for the expansion stage.
#[derive(Clone, Copy)]
enum NavModule<'db> {
    /// A top-level module (file root or file-based submodule).
    Top(TopLevelMod<'db>),
    /// An inline `mod` item, with the top mod whose base graph holds it.
    Inline(crate::hir_def::Mod<'db>, TopLevelMod<'db>),
}

impl<'db> NavModule<'db> {
    /// The top mod whose *base* scope graph holds this module's direct child
    /// items (the module's own graph for a top mod; the enclosing top mod's
    /// graph for an inline mod).
    fn graph_owner(self) -> TopLevelMod<'db> {
        match self {
            NavModule::Top(top) => top,
            NavModule::Inline(_, owner) => owner,
        }
    }

    /// This module's scope (the key under which its child items hang in the
    /// owner's base scope graph).
    fn scope(self) -> ScopeId<'db> {
        match self {
            NavModule::Top(top) => ScopeId::Item(top.into()),
            NavModule::Inline(mod_, _) => ScopeId::Item(mod_.into()),
        }
    }
}

/// Resolves a *canonical* trait path (an ingot-rooted multi-segment path, or a
/// bare trait ident local to `from`) to the `Trait` *item* it names, reading
/// only base scope graphs and the module tree — never the merged
/// [`scope_graph_impl`](super::scope_graph_impl) of any module of the
/// requesting ingot. This is the stratification-safe replacement for
/// last-segment string matching in provider selection: selection identity is
/// the resolved `Trait` def, not the path's spelling.
///
/// Inputs are the output of [`canonical_trait_path`], so the leading segment is
/// either an ingot alias (`core`, an external-dependency ingot), an ingot-self
/// keyword (`ingot`), or — for a bare path — a trait/submodule directly in
/// `from`. Generic args on any segment are ignored (a goal's `Eq<T>` names the
/// same trait as the head's `Eq`). Returns `None` when the path does not name a
/// reachable `Trait` (e.g. a not-yet-imported bare ident, or a non-trait
/// target); callers treat an unresolved path as "no identity match".
pub(super) fn resolve_trait_def<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
    path: PathId<'db>,
) -> Option<Trait<'db>> {
    // Collect the path's identifier segments (root .. last), generic args
    // stripped. A `QualifiedType` segment has no ident and cannot name a trait
    // path here, so bail.
    let len = path.len(db);
    let mut idents = Vec::with_capacity(len);
    for idx in 0..len {
        let seg = path.segment(db, idx)?;
        idents.push(seg.ident(db).to_opt()?);
    }
    let (&last, leading) = idents.split_last()?;

    // Establish the module the leading segments walk from, and the submodule
    // steps that remain to walk (the final trait segment is excluded).
    let (mut module, walk) = resolve_path_root(db, from, &idents, leading)?;

    // Walk the leading segments as submodules.
    for &segment in walk {
        module = nav_child_module(db, module, segment)?;
    }

    // Final segment: a `Trait` directly in the resolved module's base graph.
    let owner = module.graph_owner();
    let base = base_scope_graph_impl(db, owner);
    base.child_items(module.scope())
        .find_map(|item| match item {
            ItemKind::Trait(trait_) if trait_.name(db).to_opt() == Some(last) => Some(trait_),
            _ => None,
        })
}

/// Resolves the *root* segment of a trait path to the module its leading
/// segments walk from, returning that module and the submodule steps still to
/// walk (everything between that starting module and the final trait segment).
///
/// * a bare single-ident path: start at `from`, no submodule steps — the trait
///   is looked up directly in `from`.
/// * `ingot`: start at the requesting ingot's root; walk all leading segments
///   after `ingot`.
/// * an external-ingot alias (`core`, ..): start at that dependency ingot's
///   root module; walk the leading segments after the alias. Reading a
///   dependency ingot's base graphs never cycles back into this ingot's
///   expansion.
/// * otherwise (root is a submodule of `from`): start at `from` and walk all
///   leading segments (the root included).
///
/// All reads are the module tree (content-agnostic) and `resolved_external_ingots`
/// (project structure) — base/stratification-safe.
fn resolve_path_root<'db, 'a>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
    idents: &'a [IdentId<'db>],
    leading: &'a [IdentId<'db>],
) -> Option<(NavModule<'db>, &'a [IdentId<'db>])> {
    let (&root, _) = idents.split_first()?;
    // A bare single-ident path names a trait directly in `from`.
    if leading.is_empty() {
        return Some((NavModule::Top(from), leading));
    }

    // `leading` is `[root, mid...]` (the final trait segment is already split
    // off by the caller). The submodule steps after the root are `leading[1..]`.
    let after_root = &leading[1..];

    if root.is_ingot(db) {
        return Some((NavModule::Top(from.ingot(db).root_mod(db)), after_root));
    }

    for &(alias, ingot) in from.ingot(db).resolved_external_ingots(db) {
        if alias == root {
            return Some((NavModule::Top(ingot.root_mod(db)), after_root));
        }
    }

    // The root names a submodule directly in `from`: walk from `from`,
    // consuming the root as the first submodule step (all of `leading`).
    Some((NavModule::Top(from), leading))
}

/// The submodule named `segment` directly under `module`: a file-based child
/// top mod (module tree) or an inline `mod` item (base scope graph). Base/
/// module-tree reads only.
fn nav_child_module<'db>(
    db: &'db dyn HirDb,
    module: NavModule<'db>,
    segment: IdentId<'db>,
) -> Option<NavModule<'db>> {
    // File-based submodules of a top mod live in the module tree.
    if let NavModule::Top(top) = module {
        for child in top.child_top_mods(db) {
            if child.name(db) == segment {
                return Some(NavModule::Top(child));
            }
        }
    }

    // Inline `mod` items hang off the module's scope in the owner's base graph.
    let owner = module.graph_owner();
    let base = base_scope_graph_impl(db, owner);
    base.child_items(module.scope())
        .find_map(|item| match item {
            ItemKind::Mod(mod_) if mod_.name(db).to_opt() == Some(segment) => {
                Some(NavModule::Inline(mod_, owner))
            }
            _ => None,
        })
}

/// All validated derive providers declared in `ingot`, discovered through
/// the base scope graphs of its modules. Providers that fail shape
/// validation are silently excluded here; their diagnostics are reported
/// when the module that declares them is expanded.
#[salsa::tracked(return_ref)]
pub(super) fn validated_providers_in_ingot<'db>(
    db: &'db dyn HirDb,
    ingot: Ingot<'db>,
) -> Vec<ValidatedProvider<'db>> {
    let mut providers = Vec::new();
    for &top_mod in ingot.all_modules(db) {
        let base = base_scope_graph_impl(db, top_mod);
        for item in base.items_dfs(db) {
            // Ordinary form (`impl Derive<Goal> for Provider`): an ordinary
            // `impl Trait` whose trait ref resolves to `core::derive::Derive`
            // by identity. A non-provider `impl SomeTrait for T` (the goal
            // recognizer returns `None`) is left untouched.
            if let ItemKind::ImplTrait(impl_trait) = item
                && impl_trait_provider_goal_path(db, impl_trait).is_some()
                && let Ok(validated) = validate_impl_provider(db, impl_trait)
            {
                providers.push(validated);
            }
        }
    }
    providers
}

/// All providers visible from `from`: the requesting ingot's own providers
/// plus those of its (transitive) dependencies, in a deterministic order.
pub(super) fn visible_providers<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
) -> Vec<&'db ValidatedProvider<'db>> {
    let mut providers = Vec::new();
    let mut visited = FxHashSet::default();
    collect_visible(db, from.ingot(db), &mut visited, &mut providers);
    providers
}

fn collect_visible<'db>(
    db: &'db dyn HirDb,
    ingot: Ingot<'db>,
    visited: &mut FxHashSet<Ingot<'db>>,
    providers: &mut Vec<&'db ValidatedProvider<'db>>,
) {
    if !visited.insert(ingot) {
        return;
    }
    providers.extend(validated_providers_in_ingot(db, ingot));
    for &(_, dependency) in ingot.resolved_external_ingots(db) {
        collect_visible(db, dependency, visited, providers);
    }
}

/// The canonical core providers visible from `from`: providers declared in
/// `core_derives` ingots. Bare `#[derive(Trait)]` attributes and
/// `derive Trait for T` declarations select among these.
pub(super) fn core_providers<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
) -> Vec<&'db ValidatedProvider<'db>> {
    visible_providers(db, from)
        .into_iter()
        .filter(|provider| {
            provider.provider.top_mod(db).ingot(db).kind(db) == IngotKind::CoreDerives
        })
        .collect()
}

/// The trait names the canonical core providers can derive, for
/// unknown-trait diagnostics.
pub(super) fn core_derivable_trait_names<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
) -> Vec<String> {
    let mut names: Vec<String> = core_providers(db, from)
        .iter()
        .map(|provider| provider.head_name.data(db).to_string())
        .collect();
    names.dedup();
    names
}

/// Whether the bare identifier `name` already names a trait, type, or `use`
/// import in `from`'s **base** scope graph — i.e. a *competing* definition
/// that would shadow the implicit core-derive convention. Used as the
/// cross-module safety guard for the bare-ident selection adapter: a bare
/// `derive Eq` only falls back to the canonical core `Eq` provider when no
/// such local/imported `Eq` exists, so a user trait `Eq` (or `use other::Eq`)
/// never silently selects the core provider.
///
/// Base-graph only (`base_scope_graph_impl`); the expansion stage must not
/// read the merged graph.
fn names_competing_definition<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
    name: IdentId<'db>,
) -> bool {
    let base = base_scope_graph_impl(db, from);
    base.items_dfs(db).any(|item| match item {
        // A locally declared trait/type of the same name is a distinct
        // identity from the canonical core trait.
        ItemKind::Trait(_)
        | ItemKind::Struct(_)
        | ItemKind::Enum(_)
        | ItemKind::TypeAlias(_)
        | ItemKind::Contract(_) => {
            item
            .name(db)
            .is_some_and(|item_name| item_name == name)
            // Only definitions directly in the requesting module shadow the
            // bare convention; items nested in inner modules do not.
            && item.top_mod(db) == from
            && is_top_level_item(base, item)
        }
        // A `use` binding the name resolves the bare ident to its imported
        // target instead (handled by canonical-path identity); its presence
        // disqualifies the bare convention here too.
        ItemKind::Use(use_) => use_binds_name(db, use_, name),
        _ => false,
    })
}

/// Whether `item` is a direct child of the top-level module scope (not nested
/// inside an inner `mod`).
fn is_top_level_item<'db>(base: &ScopeGraph<'db>, item: ItemKind<'db>) -> bool {
    base.child_items(ScopeId::Item(base.top_mod.into()))
        .any(|child| child == item)
}

/// Whether the `use` item introduces the binding `name` (through its alias,
/// or — absent an alias — its final path segment).
fn use_binds_name<'db>(
    db: &'db dyn HirDb,
    use_: crate::hir_def::Use<'db>,
    name: IdentId<'db>,
) -> bool {
    match use_.alias(db) {
        Some(alias) => matches!(
            alias.to_opt(),
            Some(crate::hir_def::UseAlias::Ident(ident)) if ident == name
        ),
        None => use_
            .path(db)
            .to_opt()
            .and_then(|path| path.data(db).last().cloned())
            .and_then(|seg| seg.to_opt())
            .and_then(UsePathSegment::ident)
            .is_some_and(|ident| ident == name),
    }
}

/// Whether the goal trait named by `goal_path` (as written at the derive
/// site) is the same trait as `provider`'s head trait (W-C). Selection keys on
/// resolved trait-*def* identity, not on the head-name string, so an
/// aliased/qualified/imported goal still selects the canonical provider and a
/// same-named trait from another module does not.
///
/// Identity is established (in priority order):
///
/// 1. **Resolved trait-def identity (the SSOT).** The goal path is resolved
///    against `from`, and the provider's head trait path against the provider's
///    own module, each through [`resolve_trait_def`] (base scope graphs + module
///    tree only — stratification-safe). When *both* resolve, identity is exactly
///    `goal_def == head_def`: equal defs match, distinct defs do not — even when
///    the last segments spell the same name. This single rule covers a qualified
///    goal (`core::ops::Eq`), an aliased goal (`use core::ops::Eq as MyEq; derive
///    MyEq`), a bare goal imported with `use core::ops::Eq`, and a same-named
///    local trait (which resolves to a *different* def and is rejected).
///
/// 2. **Bare-ident "derive prelude" convention (COMPAT SHIM, residual).** A
///    *bare* goal ident with no `use` import and no competing local trait/type
///    of that name does not resolve to any trait def from `from` (step 1 yields
///    `None` for the goal) — it relies on an implicit "derive prelude" for
///    canonical core traits the prelude does not re-export (e.g. `Eq`/`Ord`).
///    Such a goal is matched against the provider's *resolved head def name*,
///    guarded by [`names_competing_definition`] so a user trait `Eq` or
///    `use other::Eq` disqualifies it. This is the one residual name comparison;
///    see the COMPAT SHIM note at its site.
///
/// 3. **Named-provider fallback (COMPAT SHIM, residual).** Under explicit
///    `derive .. using Provider` / `with Provider { .. }` (`named_provider`),
///    when step 1 could not establish identity because the goal or the head did
///    not resolve to a def, fall back to the goal's last segment matching the
///    provider's head last segment. The user named the provider explicitly, so
///    this is not a cross-module *core*-aliasing risk. See the COMPAT SHIM note
///    at its site.
fn goal_matches_provider<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
    goal_path: PathId<'db>,
    provider: &ValidatedProvider<'db>,
    named_provider: bool,
) -> bool {
    // (1) Resolved trait-def identity — the selection SSOT.
    let goal_def = resolve_trait_def(db, from, canonical_trait_path(db, from, goal_path));
    let head_def = resolve_trait_def(db, provider.provider.top_mod(db), provider.trait_path);
    if let (Some(goal_def), Some(head_def)) = (goal_def, head_def) {
        // Both ends resolved: identity is decided purely by def equality.
        // Distinct defs with the same last-segment name fall through to `false`
        // here — name spelling never overrides a resolved-def mismatch.
        return goal_def == head_def;
    }

    if named_provider {
        // COMPAT SHIM — removal target: named-selection last-segment fallback.
        // Blocker: reached only when step (1) could not resolve the goal or the
        // head to a `Trait` def (e.g. a provider whose head trait is declared in
        // its own module and not reachable as a canonical/importable path from
        // here, or a goal that does not resolve). The user named this provider
        // explicitly, so a last-segment match restores the pre-W-C named-goal
        // semantics without cross-module *core*-aliasing risk. Retire once every
        // provider head and named goal resolve to a def via `resolve_trait_def`.
        return last_path_ident(db, goal_path) == Some(provider.head_name);
    }

    // (2) Bare-ident "derive prelude" convention.
    let Some(goal_ident) = goal_path.as_ident(db) else {
        return false;
    };
    if goal_def.is_some() {
        // The bare goal resolved to *some* trait def (a local trait, or one
        // imported with `use`); step (1) already decided identity against it, so
        // do not fall back to a name match that would ignore that resolution.
        return false;
    }
    if names_competing_definition(db, from, goal_ident) {
        return false;
    }
    // COMPAT SHIM — removal target: bare-ident derive-prelude convention.
    // Blocker: a bare `derive Eq` with no `use core::ops::Eq` import resolves to
    // no trait def from `from` (there is no base-graph import to follow), yet
    // must still select the canonical core `Eq` provider — the implicit
    // "derive prelude" for core traits the prelude does not re-export. Matching
    // is against the provider's *resolved head def name* (not the raw
    // `head_name` string) so it is anchored to the real core trait def; the
    // `names_competing_definition` guard keeps it cross-module-safe. Retire once
    // the canonical core derive traits are in the prelude (so a bare goal
    // resolves to the def) or a derive-prelude is expressible as a base-graph
    // import.
    match head_def {
        Some(head_def) => head_def.name(db).to_opt() == Some(goal_ident),
        // Head did not resolve to a def: last-segment string match, the most
        // residual case (e.g. a core provider whose head path is not yet
        // resolvable here). Same removal target as above.
        None => provider.head_name == goal_ident,
    }
}

/// Whether the goal trait named by `goal_path` (bare, aliased, or qualified)
/// is derivable through a canonical core provider, by resolved identity
/// (W-C). Used for the up-front `13-0002 cannot derive` / "derivable traits
/// are .." check on `#[derive(Trait)]` arguments and bare `derive Trait for
/// T` declarations, so that check keys on identity exactly like selection.
pub(super) fn is_core_derivable<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
    goal_path: PathId<'db>,
) -> bool {
    core_providers(db, from)
        .iter()
        .any(|provider| goal_matches_provider(db, from, goal_path, provider, false))
}

/// How a derive request selects its provider.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ProviderSelection<'db> {
    /// Bare `#[derive(Trait)]` / `derive Trait for T`: the canonical core
    /// provider for the trait.
    Canonical,
    /// `derive Trait for T using Provider`, or a declaration inside a
    /// `with Provider { .. }` scope.
    Named(PathId<'db>),
}

/// The result of provider selection for one derive request.
pub(super) enum SelectionOutcome<'db> {
    Found(&'db ValidatedProvider<'db>),
    /// No matching provider; `wrong_goal_heads` lists the trait names of
    /// same-named providers when the name exists but provides other traits.
    NotFound {
        wrong_goal_heads: Vec<String>,
    },
    Ambiguous {
        provider_names: Vec<String>,
    },
}

/// Selects the provider for a request whose goal trait is named by
/// `goal_path` (the head path exactly as written at the derive site: bare,
/// aliased, or qualified).
///
/// W-C: the goal is matched against each candidate by *resolved identity*
/// ([`goal_matches_provider`]), not by the head-name string. An aliased or
/// qualified goal therefore still selects the canonical provider, and a
/// same-named trait from another module does not.
pub(super) fn select_provider<'db>(
    db: &'db dyn HirDb,
    from: TopLevelMod<'db>,
    goal_path: PathId<'db>,
    selection: ProviderSelection<'db>,
) -> SelectionOutcome<'db> {
    let candidates: Vec<&'db ValidatedProvider<'db>> = match selection {
        ProviderSelection::Canonical => core_providers(db, from)
            .into_iter()
            .filter(|provider| goal_matches_provider(db, from, goal_path, provider, false))
            .collect(),
        ProviderSelection::Named(path) => {
            let Some(selected) = path.as_ident(db) else {
                return SelectionOutcome::NotFound {
                    wrong_goal_heads: Vec::new(),
                };
            };
            let named: Vec<_> = visible_providers(db, from)
                .into_iter()
                .filter(|provider| provider.name == selected)
                .collect();
            let matching: Vec<_> = named
                .iter()
                .copied()
                .filter(|provider| goal_matches_provider(db, from, goal_path, provider, true))
                .collect();
            if matching.is_empty() {
                return SelectionOutcome::NotFound {
                    wrong_goal_heads: named
                        .iter()
                        .map(|provider| provider.head_name.data(db).to_string())
                        .collect(),
                };
            }
            matching
        }
    };

    match candidates.len() {
        0 => SelectionOutcome::NotFound {
            wrong_goal_heads: Vec::new(),
        },
        1 => SelectionOutcome::Found(candidates[0]),
        _ => SelectionOutcome::Ambiguous {
            provider_names: candidates
                .iter()
                .map(|provider| provider.name.data(db).to_string())
                .collect(),
        },
    }
}

/// Reflection data for a derive target, extracted from the target's HIR by
/// the expansion stage and exposed to provider bodies through the
/// `Reflect<T>` capability.
#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) struct TargetReflection<'db> {
    pub(super) shape: TargetShape<'db>,
}

#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) enum TargetShape<'db> {
    Struct {
        fields: Vec<ReflectedField<'db>>,
    },
    Enum {
        variants: Vec<ReflectedVariant<'db>>,
    },
}

/// A reflected field of the target: a struct field, or a payload field of
/// an enum variant (`variant` is the variant index in that case).
#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub(super) struct ReflectedField<'db> {
    pub(super) variant: Option<usize>,
    pub(super) index: usize,
    pub(super) name: FieldName<'db>,
    pub(super) ty: TypeId<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub(super) enum FieldName<'db> {
    /// A named (record) field.
    Named(IdentId<'db>),
    /// A positional (tuple variant) field.
    Positional(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub(super) struct ReflectedVariant<'db> {
    pub(super) index: usize,
    pub(super) name: IdentId<'db>,
    pub(super) kind: ReflectedVariantKind,
    pub(super) is_default: bool,
    pub(super) fields: Vec<ReflectedField<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub(super) enum ReflectedVariantKind {
    Unit,
    Tuple,
    Record,
}

impl<'db> TargetReflection<'db> {
    pub(super) fn is_struct(&self) -> bool {
        matches!(self.shape, TargetShape::Struct { .. })
    }

    pub(super) fn is_enum(&self) -> bool {
        matches!(self.shape, TargetShape::Enum { .. })
    }

    pub(super) fn struct_fields(&self) -> &[ReflectedField<'db>] {
        match &self.shape {
            TargetShape::Struct { fields } => fields,
            TargetShape::Enum { .. } => &[],
        }
    }

    pub(super) fn variants(&self) -> &[ReflectedVariant<'db>] {
        match &self.shape {
            TargetShape::Struct { .. } => &[],
            TargetShape::Enum { variants } => variants,
        }
    }

    pub(super) fn variant(&self, index: usize) -> Option<&ReflectedVariant<'db>> {
        self.variants().get(index)
    }

    pub(super) fn field(
        &self,
        variant: Option<usize>,
        index: usize,
    ) -> Option<&ReflectedField<'db>> {
        match variant {
            None => self.struct_fields().get(index),
            Some(v) => self.variant(v)?.fields.get(index),
        }
    }
}

/// Observable provenance for a provider-generated `impl Trait for Type`:
/// the link `derive site → provider → generated impl → goal`.
///
/// This is *reconstructed* — not stored — from data already on the generated
/// impl (its [`HirOrigin::Desugared`] derive-site origin and its goal trait
/// ref), so it adds no field to [`ImplTrait`] and no new salsa stored input.
/// The query [`derived_impl_provenance`] re-runs provider selection (the same
/// resolved-identity selection used during expansion) to recover the provider
/// that produced the impl. It therefore turns provider generation from an
/// opaque "better macro" into evidence-carrying metaprogramming: every link in
/// the chain is recoverable and assertable.
///
/// All fields are salsa ids (tracked structs / interned ids), so the value is
/// `salsa::Update` and the query can be `#[salsa::tracked]`; the derive-site
/// span is computed on demand from the impl via [`Self::derive_site`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub struct DerivedImplProvenance<'db> {
    /// The provider whose body produced the generated impl: the ordinary
    /// `impl Derive<G> for P` declaration, identified by stable identity (its
    /// `top_mod` plus the underlying salsa [`ImplTrait`] node), not by name
    /// string. The cascade's default-tier decision
    /// (`implementor_is_default_marked` in `trait_resolution`) reads its
    /// `top_mod`'s ingot kind off this field.
    pub provider: ImplTrait<'db>,
    /// The generated `impl Trait for Type` item. Its `TrackedItemId` is the
    /// generated-impl id; `generated_impl.into()` recovers it.
    pub generated_impl: ImplTrait<'db>,
    /// The goal trait reference of the generated impl (the canonical head path
    /// the provider provides for, e.g. `core::ops::Ord`).
    pub goal: TraitRefId<'db>,
}

impl<'db> DerivedImplProvenance<'db> {
    /// The derive site span: the `#[derive(..)]` target or the standalone
    /// `derive Trait for Type` declaration that requested this impl. The
    /// generated impl's origin is the derive site (a `Desugared(Derive(..))`
    /// origin), so its own span already resolves there.
    pub fn derive_site(&self) -> DynLazySpan<'db> {
        self.generated_impl.span().into()
    }
}

/// Reconstructs the provenance of a provider-generated impl: the provider that
/// produced it, the generated impl itself, and its goal trait. Returns `None`
/// for a hand-written (non-generated) impl, or when the provider cannot be
/// uniquely re-identified.
///
/// Reconstruction (no stored provenance, no `ImplTrait` schema change):
///
/// 1. Read `impl_trait.origin` — only a `Desugared(Derive(..))` origin (an
///    impl expanded from `#[derive(..)]` or a `derive Trait for T`
///    declaration) is provider-generated; everything else is `None`.
/// 2. Recover the goal trait path from the impl's `trait_ref` (synthesis put
///    the provider's canonical head path there).
/// 3. Find, among the providers visible from the impl's module, the unique one
///    whose head trait matches the goal by *resolved identity*
///    ([`goal_matches_provider`]) — the same selection rule expansion used.
///    This recovers the provider for both canonical (`derive Trait`) and named
///    (`using Provider`) selections; ambiguity (more than one matching
///    provider) yields `None` rather than guessing.
///
/// Anti-vacuous: a hand-written impl has no `Desugared(Derive(..))` origin and
/// returns `None`, so the query never claims a non-generated impl is
/// provider-generated.
///
/// KNOWN LIMITATION (reconstruction gap): when the goal is provided by MORE THAN
/// ONE visible provider — e.g. `derive Clone for Point using MyClone` while the
/// canonical core `StableClone` also provides `Clone` — reconstruction cannot
/// recover WHICH provider ran from the impl alone, so it returns `None`. The
/// unique-provider case (the common one) is exact. To make the `using`-override
/// case exact, either read the `using Provider` name from the impl's
/// `Desugared(Derive(Decl(..)))` AST, or record the selected provider id at
/// synthesis — additive follow-ups, not done here (kept to pure reconstruction).
#[salsa::tracked]
pub fn derived_impl_provenance<'db>(
    db: &'db dyn HirDb,
    impl_trait: ImplTrait<'db>,
) -> Option<DerivedImplProvenance<'db>> {
    // (1) Only impls desugared from a derive site are provider-generated.
    if !matches!(
        impl_trait.origin(db),
        HirOrigin::Desugared(DesugaredOrigin::Derive(_))
    ) {
        return None;
    }

    // (2) Recover the goal trait path the provider provided for.
    let goal = impl_trait.hir_trait_ref(db).to_opt()?;
    let goal_path = goal.path(db).to_opt()?;

    let from = impl_trait.top_mod(db);

    // (3) Re-identify the provider by the same resolved-identity match used at
    // selection time. We search all visible providers (not just the canonical
    // core ones) so that `using Provider` selections — whose provider lives
    // outside `core_derives` — are recovered too. `named_provider = true`
    // keeps the last-segment compat shim available for heads that do not
    // resolve to a def, matching `select_provider`'s named path.
    let mut matching = visible_providers(db, from)
        .into_iter()
        .filter(|provider| goal_matches_provider(db, from, goal_path, provider, true));

    let provider = matching.next()?;
    // Unique match only: if two providers both provide this goal we cannot
    // tell which one generated the impl from the reconstructed data, so report
    // honestly rather than guess.
    if matching.next().is_some() {
        return None;
    }

    // Provenance carries the discovered provider `ImplTrait` directly (the
    // ordinary `impl Derive<G> for P` declaration, identified by `top_mod` plus
    // its underlying salsa node).
    Some(DerivedImplProvenance {
        provider: provider.provider,
        generated_impl: impl_trait,
        goal,
    })
}

#[cfg(test)]
mod tests {
    use super::{core_providers, goal_matches_provider, is_core_derivable, resolve_trait_def};
    use crate::{hir_def::PathId, lower::map_file_to_mod, test_db::TestDb};

    /// The selection SSOT — [`resolve_trait_def`] — keys on the resolved
    /// `Trait` *def*, so two traits that *spell* the same last segment in
    /// different modules resolve to two DISTINCT defs (and the core trait of the
    /// same name to a third). This is the property last-segment string matching
    /// could not express; it is the foundation of identity-based selection.
    #[test]
    fn resolve_trait_def_distinguishes_same_named_traits() {
        let mut db = TestDb::default();
        let text = r#"
            mod a {
                pub trait Eq {
                    fn eq(self, other: Self) -> bool
                }
            }
            mod b {
                pub trait Eq {
                    fn eq(self, other: Self) -> bool
                }
            }
        "#;
        let file = db.standalone_file(text);
        let top_mod = map_file_to_mod(&db, file);

        let a_eq = resolve_trait_def(&db, top_mod, PathId::from_segments(&db, &["a", "Eq"]))
            .expect("a::Eq resolves to a trait def");
        let b_eq = resolve_trait_def(&db, top_mod, PathId::from_segments(&db, &["b", "Eq"]))
            .expect("b::Eq resolves to a trait def");
        let core_eq = resolve_trait_def(
            &db,
            top_mod,
            PathId::from_segments(&db, &["core", "ops", "Eq"]),
        )
        .expect("core::ops::Eq resolves to the core trait def");

        // Same last segment (`Eq`) everywhere, three distinct defs.
        assert_ne!(a_eq, b_eq, "same-named local traits must be distinct defs");
        assert_ne!(a_eq, core_eq);
        assert_ne!(b_eq, core_eq);
    }

    /// End-to-end: provider selection keys on resolved def identity, NOT on the
    /// head-name string. A goal that resolves to a *local* trait named `Eq` does
    /// not match the canonical core `Eq` provider, while a goal that resolves to
    /// `core::ops::Eq` does — even though both goals spell `Eq` as their last
    /// segment. This is the anti-vacuous guard for the bridge removal: it proves
    /// the resolved-identity path (not the string path) is what fires.
    #[test]
    fn goal_matches_provider_keys_on_def_not_name() {
        let mut db = TestDb::default();
        // A user trait *also* named `Eq`, a distinct def from `core::ops::Eq`.
        let text = r#"
            mod local {
                pub trait Eq {
                    fn eq(self, other: Self) -> bool
                }
            }
        "#;
        let file = db.standalone_file(text);
        let top_mod = map_file_to_mod(&db, file);

        // The canonical core `Eq` provider (`StableEq`, from core_derives).
        let core_eq_provider = core_providers(&db, top_mod)
            .into_iter()
            .find(|p| p.head_name.data(&db) == "Eq")
            .expect("a canonical core `Eq` provider is visible");

        let core_goal = PathId::from_segments(&db, &["core", "ops", "Eq"]);
        let local_goal = PathId::from_segments(&db, &["local", "Eq"]);

        // Identity match: `core::ops::Eq` resolves to the same def as the
        // provider head, so it selects the core provider.
        assert!(
            goal_matches_provider(&db, top_mod, core_goal, core_eq_provider, false),
            "core::ops::Eq goal must match the core Eq provider by def"
        );
        assert!(is_core_derivable(&db, top_mod, core_goal));

        // Identity MISMATCH: `local::Eq` resolves to a DIFFERENT def, so it must
        // NOT match the core provider — despite the identical `Eq` spelling.
        assert!(
            !goal_matches_provider(&db, top_mod, local_goal, core_eq_provider, false),
            "local::Eq goal must NOT match the core Eq provider — distinct defs"
        );
        assert!(
            !is_core_derivable(&db, top_mod, local_goal),
            "a goal resolving to a local same-named trait is not core-derivable"
        );
    }

    /// The single ordinary-form provider (`impl Derive<G> for P`) in `text`,
    /// validated.
    fn validate_only_provider(text: &str) -> Result<(), Vec<String>> {
        use super::{impl_trait_provider_goal_path, validate_impl_provider};
        use crate::hir_def::{HirIngot, ItemKind};

        let mut db = TestDb::default();
        let file = db.standalone_file(text);
        let top_mod = map_file_to_mod(&db, file);
        let provider = top_mod
            .ingot(&db)
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::ImplTrait(impl_trait)
                    if impl_trait_provider_goal_path(&db, *impl_trait).is_some() =>
                {
                    Some(*impl_trait)
                }
                _ => None,
            })
            .expect("one provider is present");
        validate_impl_provider(&db, provider)
            .map(|_| ())
            .map_err(|errors| errors.into_iter().map(|e| e.message).collect())
    }

    /// Positive control for the anti-vacuous guard below: a provider whose `uses`
    /// clause names the CANONICAL capability types (`use core::derive::Reflect` /
    /// `ImplBuilder`) is recognized by identity and validates.
    #[test]
    fn provider_capability_recognized_when_canonical_imported() {
        let result = validate_only_provider(
            r#"
            use core::ops::Eq
            use core::derive::Derive
            use core::derive::Evidence
            use core::derive::Reflect
            use core::derive::ImplBuilder

            struct ImportedEq {}

            impl Derive<Eq> for ImportedEq {
                const fn derive<T>(ev: own Evidence<Eq<T>>) -> Evidence<Eq<T>>
                    uses (
                        reflect: Reflect<T>,
                        builder: mut ImplBuilder<Eq<T>>,
                    )
                {
                    ev
                }
            }
            "#,
        );
        assert!(
            result.is_ok(),
            "a provider importing the canonical capability types must validate; got {result:?}"
        );
    }

    /// Anti-vacuous guard for the K04a-C3 string-fallback removal: provider
    /// capability authority keys on the resolved canonical identity
    /// (`core::derive::Reflect` / `::ImplBuilder`), NOT on the bare name.
    ///
    /// This provider's `uses` clause names LOCAL `struct Reflect` / `ImplBuilder`
    /// declared in the SAME file, with NO `use core::derive::Reflect`/`ImplBuilder`
    /// import — so the names cannot canonicalize to `core::derive::*`. The
    /// capability is therefore NOT recognized, and the provider fails shape
    /// validation with the missing-capability errors. If the deleted bare
    /// head-identifier string fallback ever returns, these local-named types would
    /// be granted authority and this test fails. (Mirrors the spirit of
    /// `derive_local_trait_no_alias` from burn-down #1.) The `Derive` provider
    /// trait is imported so the test isolates the capability check, not the
    /// trait-ref recognition.
    #[test]
    fn provider_capability_keys_on_identity_not_name() {
        let errors = validate_only_provider(
            r#"
            use core::ops::Eq
            use core::derive::Derive
            use core::derive::Evidence

            // A user type merely NAMED `Reflect` / `ImplBuilder`, with no
            // `use core::derive::Reflect`/`ImplBuilder`. The name alone must grant
            // no authority.
            struct Reflect<T> { x: T }
            struct ImplBuilder<G> { x: G }

            struct LocalNamedEq {}

            impl Derive<Eq> for LocalNamedEq {
                const fn derive<T>(ev: own Evidence<Eq<T>>) -> Evidence<Eq<T>>
                    uses (
                        reflect: Reflect<T>,
                        builder: mut ImplBuilder<Eq<T>>,
                    )
                {
                    ev
                }
            }
            "#,
        )
        .expect_err(
            "a provider whose `uses` clause names a like-named LOCAL type (not \
             `core::derive::Reflect`/`ImplBuilder`) must NOT be granted the \
             capability by name",
        );
        let messages = errors.join("\n");
        assert!(
            messages.contains("must declare a `Reflect<..>` capability"),
            "expected the missing-Reflect-capability error (identity, not name, \
             fires); got:\n{messages}"
        );
        assert!(
            messages.contains("must declare a `mut ImplBuilder<..>` capability"),
            "expected the missing-ImplBuilder-capability error; got:\n{messages}"
        );
    }

    /// FCO #87: the `impl Derive<Goal> for Provider` form is discovered and
    /// validated as a derive provider, with the goal extracted from the
    /// `Derive<..>` argument and the provider name from the `Self` (implementor)
    /// type. The recognition keys on the `core::derive::Derive` IDENTITY of the
    /// trait ref (an `impl Derive_<Eq>` with a like-named local trait would not
    /// match).
    #[test]
    fn ordinary_impl_form_is_recognized_as_provider() {
        use super::{impl_trait_provider_goal_path, validate_impl_provider};

        let mut db = TestDb::default();
        let text = r#"
            use core::ops::Eq
            use core::derive::Derive
            use core::derive::Evidence
            use core::derive::Reflect
            use core::derive::ImplBuilder

            struct StableEq {}

            impl Derive<Eq> for StableEq {
                const fn derive<T>(ev: own Evidence<Eq<T>>) -> Evidence<Eq<T>>
                    uses (
                        reflect: Reflect<T>,
                        builder: mut ImplBuilder<Eq<T>>,
                    )
                {
                    ev
                }
            }
        "#;
        let file = db.standalone_file(text);
        let top_mod = map_file_to_mod(&db, file);

        // Find the `impl Derive<Eq> for StableEq` item and confirm the recognizer
        // extracts the goal `Eq` from the `Derive<..>` argument.
        let impl_trait = top_mod
            .all_impl_traits(&db)
            .iter()
            .copied()
            .find(|it| impl_trait_provider_goal_path(&db, *it).is_some())
            .expect("the ordinary-form provider impl is recognized as a Derive provider");
        let goal_path = impl_trait_provider_goal_path(&db, impl_trait)
            .expect("goal path is recovered from the Derive<..> argument");
        assert_eq!(
            goal_path
                .ident(&db)
                .to_opt()
                .map(|i| i.data(&db).to_string()),
            Some("Eq".to_string()),
            "the goal extracted from `Derive<Eq>` is `Eq`"
        );

        // Validation yields a ValidatedProvider whose name is the implementor
        // (`StableEq`), goal head is `Eq`, and capabilities are recognized.
        let validated =
            validate_impl_provider(&db, impl_trait).expect("the ordinary-form provider validates");
        assert_eq!(validated.name.data(&db), "StableEq");
        assert_eq!(validated.head_name.data(&db), "Eq");
        assert_eq!(validated.provider, impl_trait);
        assert!(
            validated.capabilities.iter().any(|c| c.is_reflect()),
            "the Reflect capability is recognized in the ordinary form"
        );
        assert!(
            validated.capabilities.iter().any(|c| c.is_builder()),
            "the ImplBuilder capability is recognized in the ordinary form"
        );

        // It also shows up through ingot discovery (`visible_providers`), proving
        // `validated_providers_in_ingot` walks the ordinary-impl branch.
        let discovered = super::visible_providers(&db, top_mod)
            .into_iter()
            .any(|p| p.name.data(&db) == "StableEq" && p.provider == impl_trait);
        assert!(
            discovered,
            "the ordinary-form provider is discovered by `validated_providers_in_ingot`"
        );

        // Anti-vacuous: an `impl Derive_<Eq> for X` over a LIKE-NAMED local trait
        // (`Derive_`, not `core::derive::Derive`) is NOT recognized.
        let mut db2 = TestDb::default();
        let not_provider = r#"
            use core::ops::Eq

            trait Derive_<P: * -> Constraint> {
                const fn derive<T>(ev: Eq<T>) -> Eq<T>
            }

            struct StableEqK {}

            impl Derive_<Eq> for StableEqK {
                const fn derive<T>(ev: Eq<T>) -> Eq<T> { ev }
            }
        "#;
        let file2 = db2.standalone_file(not_provider);
        let top_mod2 = map_file_to_mod(&db2, file2);
        assert!(
            top_mod2
                .all_impl_traits(&db2)
                .iter()
                .all(|it| impl_trait_provider_goal_path(&db2, *it).is_none()),
            "an `impl Derive_<..>` over a like-named local trait must NOT be \
             recognized as a `core::derive::Derive` provider"
        );
    }

    /// P50 provenance proof: a provider-generated impl is linked back to the
    /// provider that produced it (`derive site → provider → generated impl →
    /// goal`), reconstructed by [`super::derived_impl_provenance`] with no
    /// stored provenance — and a HAND-WRITTEN impl has `None` provenance, so the
    /// query never claims a non-generated impl is provider-generated.
    ///
    /// Anti-vacuous on both ends: the generated `impl Eq for Point` resolves to
    /// the fixture-local `ConcreteEq` provider by identity; the hand-written
    /// `impl Marker for Point` resolves to nothing.
    #[test]
    fn derived_impl_provenance_links_provider_and_none_for_handwritten() {
        use super::derived_impl_provenance;
        use crate::span::{DesugaredOrigin, HirOrigin};
        use crate::test_db::HirAnalysisTestDb;
        use camino::Utf8PathBuf;

        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("p50_provenance.fe"),
            r#"
use core::derive::Derive
use core::derive::Evidence
use core::derive::ImplBuilder
use core::derive::Reflect

struct Point {
    x: u256,
    y: u256,
}

// A fixture-LOCAL goal trait, so the only provider that provides it is the
// fixture-local one below — reconstruction is unambiguous. (A goal that a core
// provider ALSO provides, e.g. `Eq`, is intentionally ambiguous under pure
// reconstruction; see the doc on `derived_impl_provenance`.)
trait Taggable {
    fn tag(self) -> bool
}

// Fixture-local provider for the local goal `Taggable<T>` (concrete goal).
struct TagProv {}

impl Derive<Taggable> for TagProv {
    const fn derive<T>(ev: own Evidence<Taggable<T>>) -> Evidence<Taggable<T>>
        uses (
            reflect: Reflect<T>,
            builder: mut ImplBuilder<Taggable<T>>,
        )
    {
        builder.emit_method("tag", builder.bool(true))
        builder.finish()
        ev
    }
}

derive Taggable for Point using TagProv

// GATE-ISOLATING control: a HAND-WRITTEN impl of the PROVIDER-BACKED trait
// `Taggable` (TagProv provides it) for a different type. Its provenance must be
// `None` ONLY because its origin is not a derive site — if the origin gate were
// removed, reconstruction WOULD find TagProv for it (the goal resolves to a
// real provider), so this control fails when the gate is deleted. (The `Marker`
// control below does NOT isolate the gate: `Marker` has no provider, so it would
// return `None` via the empty provider-filter regardless of the gate.)
struct Other {}
impl Taggable for Other {
    fn tag(self) -> bool {
        false
    }
}

// A hand-written impl of a non-provider-backed trait: a second control.
trait Marker {}
impl Marker for Point {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        // Run the analysis pass to trigger derive expansion (the generated impl
        // is synthesized and merged) and to prove provenance SURVIVES the impl
        // re-entering ordinary checking.
        let _ = db.run_on_top_mod(top_mod);

        let impls = top_mod.all_impl_traits(&db);
        let generated: Vec<_> = impls
            .iter()
            .copied()
            .filter(|it| {
                matches!(
                    it.origin(&db),
                    HirOrigin::Desugared(DesugaredOrigin::Derive(_))
                )
            })
            .collect();
        let handwritten: Vec<_> = impls
            .iter()
            .copied()
            .filter(|it| {
                !matches!(
                    it.origin(&db),
                    HirOrigin::Desugared(DesugaredOrigin::Derive(_))
                )
            })
            .collect();

        assert_eq!(
            generated.len(),
            1,
            "exactly one generated impl (the derived `Eq for Point`)"
        );
        let gen_impl = generated[0];

        // The generated impl has provenance pointing at the fixture-local provider.
        let prov = derived_impl_provenance(&db, gen_impl)
            .expect("a provider-generated impl must have reconstructable provenance");
        assert_eq!(
            prov.generated_impl, gen_impl,
            "provenance names the generated impl itself"
        );
        // `prov.provider` is the provider `ImplTrait`; compare it directly to the
        // discovered provider's declaration (the fixture-local `TagProv`, the
        // unique provider visible from this module), by stable identity.
        let expected_provider = super::visible_providers(&db, top_mod)
            .into_iter()
            .find(|p| p.name.data(&db) == "TagProv")
            .map(|p| p.provider)
            .expect("the fixture declares one provider (TagProv)");
        assert_eq!(
            prov.provider, expected_provider,
            "provenance resolves to the fixture-local `TagProv` provider by identity"
        );

        // Anti-vacuous: a hand-written impl has NO provenance.
        assert!(
            !handwritten.is_empty(),
            "the fixture has a hand-written `impl Marker for Point`"
        );
        for hw in handwritten {
            assert!(
                derived_impl_provenance(&db, hw).is_none(),
                "a hand-written impl must have no provider provenance"
            );
        }
    }

    /// BRIDGE BURN-DOWN #5 / H10a reify-path — provenance for a generated
    /// `impl core::abi::AbiSize` resolves to the NAMED `StableAbiSize` provider.
    ///
    /// This is the reify-path analogue of
    /// [`derived_impl_provenance_links_provider_and_none_for_handwritten`], but
    /// over the REAL core trait `core::abi::AbiSize` (an EVM layout trait) rather
    /// than a fixture-local goal. Reconstruction is unambiguous *precisely
    /// because* AbiSize-derivation is NOT canonicalized into `core_derives`:
    /// there is no canonical AbiSize provider competing with the fixture-local
    /// `StableAbiSize`, so `derived_impl_provenance` finds exactly one matching
    /// provider and returns `Some(StableAbiSize)`. (If a canonical AbiSize
    /// provider were ever added — the multi-backend PAUSE condition — this would
    /// become ambiguous and the query would honestly return `None`; this test
    /// would then catch that policy change.)
    #[test]
    fn derived_impl_provenance_links_named_abi_size_provider() {
        use super::derived_impl_provenance;
        use crate::span::{DesugaredOrigin, HirOrigin};
        use crate::test_db::HirAnalysisTestDb;
        use camino::Utf8PathBuf;

        let mut db = HirAnalysisTestDb::default();
        // FCO #5b: `StableAbiSize` is the std-resident provider
        // (`ingots/std/src/abi.fe`), promoted out of fixtures. It lives in `std`
        // (not `core`) so its goal canonicalizes to the absolute path
        // `core::abi::AbiSize` and the generated impls resolve everywhere; it is
        // NOT in `core_derives`, so it is never a canonical AbiSize competitor.
        // This file selects it by NAME — the unique provider of that name — so
        // reconstruction is unambiguous.
        let file = db.new_stand_alone(
            Utf8PathBuf::from("reify_abi_size_provenance.fe"),
            r#"
use core::abi::AbiSize

struct Point {
    x: u256,
    y: u256,
}

derive AbiSize for Point using StableAbiSize

// A hand-written impl: the anti-vacuous control (provenance must be `None`).
trait Marker {}
impl Marker for Point {}
"#,
        );
        let (top_mod, _) = db.top_mod(file);
        // Run analysis to trigger derive expansion and prove provenance SURVIVES
        // the generated impl re-entering ordinary checking.
        let _ = db.run_on_top_mod(top_mod);

        let impls = top_mod.all_impl_traits(&db);
        let generated: Vec<_> = impls
            .iter()
            .copied()
            .filter(|it| {
                matches!(
                    it.origin(&db),
                    HirOrigin::Desugared(DesugaredOrigin::Derive(_))
                )
            })
            .collect();
        assert_eq!(
            generated.len(),
            1,
            "exactly one generated impl (the derived `AbiSize for Point`)"
        );
        let gen_impl = generated[0];

        // The generated impl's provenance points at the NAMED `StableAbiSize`
        // provider — reconstructed (no stored provenance) and UNIQUE because no
        // canonical AbiSize competitor exists.
        let prov = derived_impl_provenance(&db, gen_impl)
            .expect("a NAMED-only AbiSize derive must have unambiguous reconstructable provenance");
        assert_eq!(
            prov.generated_impl, gen_impl,
            "provenance names the generated impl itself"
        );
        // The expected provider is the std-resident `StableAbiSize`, found
        // among the providers visible from this module (its own ingot + std + core).
        // `prov.provider` is the provider `ImplTrait`, so we compare the
        // discovered provider's declaration directly by stable identity —
        // proving provenance is first-class for the `impl Derive<AbiSize> for
        // StableAbiSize` form.
        let expected_provider = super::visible_providers(&db, top_mod)
            .into_iter()
            .find(|p| p.name.data(&db) == "StableAbiSize")
            .map(|p| p.provider)
            .expect("the std-resident `StableAbiSize` provider is visible");
        assert_eq!(
            prov.provider, expected_provider,
            "provenance resolves to the NAMED `StableAbiSize` provider by identity"
        );

        // Anti-vacuous: the hand-written `impl Marker for Point` has NO
        // provenance, so the query never claims a non-generated impl is derived.
        let handwritten: Vec<_> = impls
            .iter()
            .copied()
            .filter(|it| {
                !matches!(
                    it.origin(&db),
                    HirOrigin::Desugared(DesugaredOrigin::Derive(_))
                )
            })
            .collect();
        assert!(
            !handwritten.is_empty(),
            "the fixture has a hand-written `impl Marker for Point`"
        );
        for hw in handwritten {
            assert!(
                derived_impl_provenance(&db, hw).is_none(),
                "a hand-written impl must have no provider provenance"
            );
        }
    }
}
