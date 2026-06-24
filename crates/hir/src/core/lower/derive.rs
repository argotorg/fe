//! Derive request parsing, validation, and provider routing.
//!
//! `#[derive(..)]` attributes and `derive Trait for Type` declarations
//! produce [`DeriveRequest`]s. Each request selects a derive *provider* — a
//! Fe `impl Derive<Trait> for Provider { const fn derive .. }` item — whose body
//! executes at compile time ([`super::provider_executor`]) and whose builder
//! commands are replayed into a real HIR `impl Trait for Target` item
//! ([`super::provider_synthesis`]) via [`HirBuilder`], exactly like
//! `#[event]` / `#[error]` desugaring. Generated items carry
//! [`DeriveDesugared`] origins so diagnostics and IDE features can map them
//! back to the request site.
//!
//! The compiler itself has no knowledge of how any specific trait is
//! derived: the canonical `Eq` / `Default` providers live in the
//! `core_derives` ingot, written in Fe.
//!
//! Synthesis runs in the post-lowering expansion stage
//! ([`super::expansion`]), not during AST lowering: the entry points here
//! are invoked with an expansion [`FileLowerCtxt`] positioned at a shim for
//! the target item's lexical parent scope, so the generated impls become
//! siblings of the target once the expansion graph is merged into the base
//! scope graph.

use parser::ast::{self, prelude::*};
use salsa::Accumulator as _;

use super::{
    FileLowerCtxt,
    attr::{AttrForm, AttrRule, AttrTarget, has_named_attr, named_attr_specs, validate_attr_rules},
    hir_builder::HirBuilder,
    provider::{
        self, FieldName, ProviderSelection, ReflectedField, ReflectedVariant, ReflectedVariantKind,
        SelectionOutcome, TargetReflection, TargetShape, ValidatedProvider,
    },
    provider_executor::ProviderExecutor,
    provider_synthesis::synthesize_provider_impl,
};
use crate::{
    hir_def::{
        Enum, GenericArg, GenericArgListId, GenericParam, GenericParamListId, IdentId, Partial,
        PathId, PathKind, Struct, TraitRefId, TypeGenericArg, TypeGenericParam, TypeId, TypeKind,
        VariantKind, WhereClauseId, WherePredicate,
    },
    span::DeriveDesugared,
};

/// The targets on which a `#[default]` attribute is meaningful. Used both
/// here and by the item-level attribute validation in `item.rs`.
pub(super) const DEFAULT_ATTR_TARGETS: &str = "variants of `#[derive(Default)]` enums";

/// Derive-related errors accumulated during derive expansion / validation.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeriveError {
    pub kind: DeriveErrorKind,
    pub file: common::file::File,
    /// Range of the primary span (attribute, argument, or generic params).
    pub primary_range: parser::TextRange,
    pub item_name: Option<String>,
    /// An optional secondary span, e.g. the failing expression inside a
    /// provider body for [`DeriveErrorKind::ProviderFailed`].
    pub secondary: Option<DeriveSecondarySpan>,
}

/// A secondary labeled span of a [`DeriveError`]; may point into a
/// different file than the primary (e.g. into the provider's module).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DeriveSecondarySpan {
    pub file: common::file::File,
    pub range: parser::TextRange,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DeriveErrorKind {
    /// `#[derive(..)]` on an item with const generic parameters is not yet
    /// supported (type generic parameters are).
    ConstGeneric { item_kind: &'static str },
    /// The derive argument has no canonical core provider. `available`
    /// lists the traits the visible core providers can derive.
    UnknownTrait {
        name: String,
        available: Vec<String>,
    },
    /// The attribute has an invalid form, e.g. bare `#[derive]`,
    /// `#[derive = Eq]`, or `#[derive(Eq = 1)]`.
    InvalidForm,
    /// The same trait is requested more than once, e.g. `#[derive(Eq, Eq)]`.
    DuplicateTrait { name: String },
    /// `#[derive(..)]` cannot be combined with `#[event]` / `#[error]`.
    EventErrorStruct,
    /// `#[derive(Default)]` on an enum with no `#[default]` variant.
    MissingDefaultVariant,
    /// More than one variant is marked `#[default]`.
    MultipleDefaultVariants { first_variant_name: Option<String> },
    /// The target path of a standalone `derive Trait for Type` declaration
    /// cannot be resolved to an item declared in the same file.
    UnresolvedDeclTarget { path: String },
    /// The target path of a standalone `derive Trait for Type` declaration
    /// resolves to an item that is not a struct or enum.
    InvalidDeclTarget { path: String, actual: &'static str },
    /// The same trait is derived for the same target more than once, either
    /// by combining `#[derive(..)]` with a `derive` declaration or by two
    /// identical declarations.
    ConflictingDerive { trait_name: String, target: String },
    /// A `derive .. using Provider` selection names no visible provider for
    /// the requested trait. `wrong_goal_heads` lists the traits provided by
    /// same-named providers, when any exist.
    ProviderNotFound {
        provider: String,
        trait_name: String,
        wrong_goal_heads: Vec<String>,
    },
    /// A `derive .. using Provider` selection matches more than one visible
    /// provider for the requested trait.
    ProviderAmbiguous {
        provider: String,
        trait_name: String,
        count: usize,
    },
    /// A bare derive request matches more than one canonical core provider.
    CanonicalProviderAmbiguous {
        trait_name: String,
        providers: Vec<String>,
    },
    /// The selected provider's body failed to execute (unsupported
    /// construct, missing `finish`, budget exceeded, ..). The secondary
    /// span points at the offending construct inside the provider.
    ProviderFailed {
        provider: String,
        trait_name: String,
        message: String,
    },
    /// A derive provider declaration is malformed (shape validation),
    /// reported on the module that declares the provider.
    InvalidProvider { message: String },
}

pub(super) fn has_derive_attr(ast: &ast::Struct) -> bool {
    has_named_attr(ast.attr_list(), "derive")
}

pub(super) fn enum_has_derive_attr(ast: &ast::Enum) -> bool {
    has_named_attr(ast.attr_list(), "derive")
}

pub(super) fn accumulate_error<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    item_name: &Option<String>,
    kind: DeriveErrorKind,
    primary_range: parser::TextRange,
) {
    accumulate_error_with(ctxt, item_name, kind, primary_range, None)
}

pub(super) fn accumulate_error_with<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    item_name: &Option<String>,
    kind: DeriveErrorKind,
    primary_range: parser::TextRange,
    secondary: Option<DeriveSecondarySpan>,
) {
    let db = ctxt.db();
    DeriveError {
        kind,
        file: ctxt.top_mod().file(db),
        primary_range,
        item_name: item_name.clone(),
        secondary,
    }
    .accumulate(db);
}

/// Reports an error for `#[derive(..)]` combined with `#[event]` or
/// `#[error]`; derive desugaring is skipped for such structs.
pub(super) fn report_derive_on_event_or_error_struct<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
) {
    let item_name = ast.name().map(|n| n.text().to_string());
    for attr in derive_attrs(ast.attr_list()) {
        let range = attr.syntax().text_range();
        accumulate_error(ctxt, &item_name, DeriveErrorKind::EventErrorStruct, range);
    }
}

fn derive_attrs(attrs: Option<ast::AttrList>) -> Vec<ast::NormalAttr> {
    attrs
        .into_iter()
        .flat_map(|attrs| attrs.normal_attrs_named("derive").collect::<Vec<_>>())
        .collect()
}

/// Parses the `#[derive(..)]` attributes of an item into the list of traits
/// to derive, reporting malformed arguments, unknown traits (no canonical
/// core provider), and duplicates.
pub(super) fn parse_derive_traits<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    attrs: Option<ast::AttrList>,
    item_name: &Option<String>,
) -> Vec<(IdentId<'db>, parser::TextRange)> {
    let mut traits: Vec<(IdentId<'db>, parser::TextRange)> = Vec::new();

    for attr in derive_attrs(attrs) {
        let attr_range = attr.syntax().text_range();

        // `#[derive = ..]` or bare `#[derive]`.
        let args = match attr.args() {
            Some(args) if attr.value().is_none() => args,
            _ => {
                accumulate_error(ctxt, item_name, DeriveErrorKind::InvalidForm, attr_range);
                continue;
            }
        };

        let mut has_arg = false;
        for arg in args {
            has_arg = true;
            let arg_range = arg.syntax().text_range();
            let (Some(key), None) = (arg.key(), arg.value()) else {
                accumulate_error(ctxt, item_name, DeriveErrorKind::InvalidForm, arg_range);
                continue;
            };

            let name = key.text().to_string();
            let ident = IdentId::new(ctxt.db(), name.clone());
            let goal_path = PathId::from_ident(ctxt.db(), ident);
            if !provider::is_core_derivable(ctxt.db(), ctxt.top_mod(), goal_path) {
                accumulate_error(
                    ctxt,
                    item_name,
                    DeriveErrorKind::UnknownTrait {
                        name,
                        available: provider::core_derivable_trait_names(ctxt.db(), ctxt.top_mod()),
                    },
                    arg_range,
                );
                continue;
            }

            if traits.iter().any(|(t, _)| *t == ident) {
                accumulate_error(
                    ctxt,
                    item_name,
                    DeriveErrorKind::DuplicateTrait { name },
                    arg_range,
                );
                continue;
            }
            traits.push((ident, arg_range));
        }

        // `#[derive()]` derives nothing; treat it as malformed to avoid
        // silently accepting a no-op attribute.
        if !has_arg {
            accumulate_error(ctxt, item_name, DeriveErrorKind::InvalidForm, attr_range);
        }
    }

    traits
}

/// One derive request for one trait on one target item: an argument of a
/// `#[derive(..)]` attribute, or a standalone `derive Trait for Type`
/// declaration (optionally with a `using Provider` selection).
#[derive(Debug, Clone)]
pub(super) struct DeriveRequest<'db> {
    /// The goal trait's last path segment (`Eq` in `core::ops::Eq` or
    /// `MyEq`). Retained for diagnostics, `Default`-marker handling, and
    /// `#[derive(..)]`/declaration conflict deduplication. Provider
    /// *selection* keys on [`Self::trait_path`] (resolved identity), not on
    /// this string, so an aliased or qualified goal still selects the right
    /// provider and a same-named trait from another module does not.
    pub(super) trait_name: IdentId<'db>,
    /// The goal trait path exactly as written at the derive site: a bare
    /// ident (`Eq`), an alias (`MyEq`), or a qualified path
    /// (`core::ops::Eq`). Provider selection canonicalizes this against the
    /// requesting module's `use` items (base-graph only) to recover the
    /// goal's resolved identity (W-C). For `#[derive(Trait)]` attributes the
    /// argument is always a single ident, so this is `PathId::from_ident`.
    pub(super) trait_path: PathId<'db>,
    /// The request site: the derive attribute argument or the declaration's
    /// trait path. Primary span for request-level diagnostics.
    pub(super) primary_range: parser::TextRange,
    pub(super) selection: ProviderSelection<'db>,
    /// The `using ..` path range, for selection diagnostics; falls back to
    /// `primary_range` when absent.
    pub(super) selection_range: Option<parser::TextRange>,
    /// The origin attached to the generated items.
    pub(super) desugared: DeriveDesugared,
}

impl<'db> DeriveRequest<'db> {
    fn selection_range(&self) -> parser::TextRange {
        self.selection_range.unwrap_or(self.primary_range)
    }
}

/// Generic-parameter information for a derive target, used to put real
/// generic params, generic args, and where clauses on the synthesized
/// impls. For `struct Pair<A, B>` a derived `Eq` impl is
///
/// ```text
/// impl<A, B> Eq for Pair<A, B> where A: Eq, B: Eq { .. }
/// ```
///
/// The where-clause predicates come from the provider's `require` commands
/// (see [`super::provider_synthesis`]); the item's own inline param bounds
/// and where-clause predicates are carried over so the impl self type stays
/// well-formed.
pub(super) struct DeriveGenerics<'db> {
    /// The impl's generic param list: the item's params with default types
    /// stripped (defaults are not meaningful on an impl). Empty for
    /// non-generic items.
    pub(super) impl_params: GenericParamListId<'db>,
    /// The item's own where-clause predicates, copied onto each impl.
    pub(super) inherited_preds: Vec<WherePredicate<'db>>,
    /// Each type param as a path type (`A`, `B`, ..). A provider `require`
    /// whose type mentions one of these (a generic-param requirement) becomes a
    /// real where-predicate on the impl; a fully-concrete requirement does not
    /// (it is a concrete obligation discharged at the generated body's use
    /// site). See [`super::provider_synthesis::requirement_where_clause`].
    pub(super) param_tys: Vec<TypeId<'db>>,
    /// `<A, B>` argument list applied to the item's name in the impl self
    /// type; `GenericArgListId::none` for non-generic items.
    pub(super) self_ty_args: GenericArgListId<'db>,
}

/// Builds the [`DeriveGenerics`] for an item with the given generic params
/// and where clause. Returns `None` (skipping impl generation) when the item
/// has const generic params — reported with a precise diagnostic — or when a
/// param name is missing due to a parser error (reported elsewhere).
fn derive_generics<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    item_name: &Option<String>,
    item_kind: &'static str,
    generic_params: GenericParamListId<'db>,
    where_clause: WhereClauseId<'db>,
    generic_params_range: impl FnOnce() -> parser::TextRange,
) -> Option<DeriveGenerics<'db>> {
    let db = ctxt.db();

    let mut impl_params = Vec::new();
    let mut param_tys = Vec::new();
    let mut args = Vec::new();
    for param in generic_params.data(db) {
        match param {
            GenericParam::Type(ty_param) => {
                let name = ty_param.name.to_opt()?;
                let ty = TypeId::new(
                    db,
                    TypeKind::Path(Partial::Present(PathId::from_ident(db, name))),
                );
                impl_params.push(GenericParam::Type(TypeGenericParam {
                    name: ty_param.name,
                    bounds: ty_param.bounds.clone(),
                    default_ty: None,
                }));
                param_tys.push(ty);
                args.push(GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(ty),
                }));
            }
            GenericParam::Const(_) => {
                accumulate_error(
                    ctxt,
                    item_name,
                    DeriveErrorKind::ConstGeneric { item_kind },
                    generic_params_range(),
                );
                return None;
            }
        }
    }

    let self_ty_args = if args.is_empty() {
        GenericArgListId::none(db)
    } else {
        GenericArgListId::given(db, args)
    };

    Some(DeriveGenerics {
        impl_params: GenericParamListId::new(db, impl_params),
        inherited_preds: where_clause.data(db).clone(),
        param_tys,
        self_ty_args,
    })
}

/// The impl self type: the item's name applied to its own generic params,
/// e.g. `Pair<A, B>` (or just `Pair` for non-generic items).
fn derive_self_ty<'db>(
    ctxt: &FileLowerCtxt<'db>,
    name: IdentId<'db>,
    generics: &DeriveGenerics<'db>,
) -> TypeId<'db> {
    let db = ctxt.db();
    let path = PathId::new(
        db,
        PathKind::Ident {
            ident: Partial::Present(name),
            generic_args: generics.self_ty_args,
        },
        None,
    );
    TypeId::new(db, TypeKind::Path(Partial::Present(path)))
}

/// Executes the derive requests targeting `struct_`. Called from the
/// post-lowering expansion stage with `ctxt` positioned at the struct's
/// lexical parent scope, so the generated impls become siblings of the
/// struct in the merged scope graph.
pub(super) fn lower_struct_derives<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Struct,
    struct_: Struct<'db>,
    requests: &[DeriveRequest<'db>],
) {
    let item_name = ast.name().map(|n| n.text().to_string());
    lower_struct_derives_inner(ctxt, item_name, struct_, requests, || {
        ast.generic_params()
            .map(|g| g.syntax().text_range())
            .unwrap_or_else(|| ast.syntax().text_range())
    });
}

/// Like [`lower_struct_derives`] but for a SYNTHETIC struct that has no source
/// `ast::Struct` — e.g. a `#[msg]` variant struct (FCO #5c). The item name comes
/// from the HIR struct, and `anchor` is the diagnostic span used only as the
/// generics-diagnostic fallback (synthetic structs have no generics, so it never
/// actually fires). Reflection is sourced from the struct's HIR fields, exactly
/// as in [`lower_struct_derives`] — the `ast::Struct` was only ever used for the
/// item name and that fallback span.
pub(super) fn lower_synthetic_struct_derives<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    anchor: parser::TextRange,
    struct_: Struct<'db>,
    requests: &[DeriveRequest<'db>],
) {
    let db = ctxt.db();
    let item_name = struct_.name(db).to_opt().map(|n| n.data(db).to_string());
    lower_struct_derives_inner(ctxt, item_name, struct_, requests, || anchor);
}

fn lower_struct_derives_inner<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    item_name: Option<String>,
    struct_: Struct<'db>,
    requests: &[DeriveRequest<'db>],
    generics_range: impl FnOnce() -> parser::TextRange,
) {
    let db = ctxt.db();

    let Some(generics) = derive_generics(
        ctxt,
        &item_name,
        "struct",
        struct_.generic_params(db),
        struct_.where_clause(db),
        generics_range,
    ) else {
        return;
    };

    let Some(struct_name_ident) = struct_.name(db).to_opt() else {
        // Parser error: missing name token. Avoid panics/cascades.
        return;
    };

    // Collect every field; skip generation when the struct is not
    // well-formed enough (parser errors are reported elsewhere).
    let mut fields = Vec::new();
    for (index, field) in struct_.fields(db).data(db).iter().enumerate() {
        let (Some(name), Some(ty)) = (field.name.to_opt(), field.type_ref.to_opt()) else {
            return;
        };
        fields.push(ReflectedField {
            variant: None,
            index,
            name: FieldName::Named(name),
            ty,
        });
    }

    let reflection = TargetReflection {
        shape: TargetShape::Struct { fields },
    };

    execute_requests(
        ctxt,
        &item_name,
        struct_name_ident,
        &generics,
        &reflection,
        requests,
    );
}

/// Validates `#[default]` attributes on the variants of `ast`:
/// * on an enum that derives `Default` the attribute must be bare and
///   unique per variant;
/// * on any other enum the attribute is reported as misplaced.
fn validate_variant_default_attrs<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Enum,
    derives_default: bool,
) {
    let Some(variants) = ast.variants() else {
        return;
    };
    let rule = if derives_default {
        AttrRule::supported("default", AttrForm::Bare, "`#[default]`")
    } else {
        AttrRule::unsupported("default", DEFAULT_ATTR_TARGETS)
    };
    for variant in variants {
        let target = AttrTarget::new("variant", variant.name().map(|n| n.text().to_string()));
        validate_attr_rules(ctxt, variant.attr_list(), target, &[rule]);
    }
}

/// Reports `#[default]` markers on the variants of an enum that has no
/// `Default` derive request; the marker only means something on enums
/// deriving `Default`.
pub(super) fn report_misplaced_default_attrs<'db>(ctxt: &mut FileLowerCtxt<'db>, ast: &ast::Enum) {
    validate_variant_default_attrs(ctxt, ast, false);
}

/// The index of the first variant marked `#[default]`, without diagnostics.
fn first_default_variant_index(ast: &ast::Enum) -> Option<usize> {
    ast.variants()
        .into_iter()
        .flatten()
        .position(|variant| !named_attr_specs(variant.attr_list(), "default").is_empty())
}

/// Finds the variant marked `#[default]`, reporting a missing marker and any
/// extra markers. Returns the index of the first marked variant so partial
/// code can still be generated alongside the diagnostics.
fn resolve_default_variant<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Enum,
    item_name: &Option<String>,
) -> Option<usize> {
    let mut first: Option<(usize, Option<String>)> = None;

    for (idx, variant) in ast.variants().into_iter().flatten().enumerate() {
        let specs = named_attr_specs(variant.attr_list(), "default");
        let Some(spec) = specs.first() else {
            continue;
        };
        if let Some((_, first_variant_name)) = &first {
            accumulate_error(
                ctxt,
                item_name,
                DeriveErrorKind::MultipleDefaultVariants {
                    first_variant_name: first_variant_name.clone(),
                },
                spec.range,
            );
        } else {
            first = Some((idx, variant.name().map(|n| n.text().to_string())));
        }
    }

    if first.is_none() {
        let range = derive_attrs(ast.attr_list())
            .first()
            .map(|attr| attr.syntax().text_range())
            .unwrap_or_else(|| ast.syntax().text_range());
        accumulate_error(
            ctxt,
            item_name,
            DeriveErrorKind::MissingDefaultVariant,
            range,
        );
    }

    first.map(|(idx, _)| idx)
}

/// Executes the derive requests targeting `enum_`. Called from the
/// post-lowering expansion stage with `ctxt` positioned at the enum's
/// lexical parent scope, so the generated impls become siblings of the enum
/// in the merged scope graph.
pub(super) fn lower_enum_derives<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: &ast::Enum,
    enum_: Enum<'db>,
    requests: &[DeriveRequest<'db>],
) {
    let item_name = ast.name().map(|n| n.text().to_string());
    let db = ctxt.db();

    // The `#[default]` variant marker is compiler attribute machinery, keyed
    // on whether any request derives `Default` (through any provider).
    let derives_default = requests
        .iter()
        .any(|request| request.trait_name.data(db) == "Default");
    validate_variant_default_attrs(ctxt, ast, derives_default);
    let default_variant = derives_default
        .then(|| resolve_default_variant(ctxt, ast, &item_name))
        .flatten();

    let Some(generics) = derive_generics(
        ctxt,
        &item_name,
        "enum",
        enum_.generic_params(db),
        enum_.where_clause(db),
        || {
            ast.generic_params()
                .map(|g| g.syntax().text_range())
                .unwrap_or_else(|| ast.syntax().text_range())
        },
    ) else {
        return;
    };

    let Some(enum_name_ident) = enum_.name(db).to_opt() else {
        // Parser error: missing name token. Avoid panics/cascades.
        return;
    };

    // Collect the name and payload of every variant; skip generation when
    // the enum is not well-formed enough (parser errors are reported
    // elsewhere).
    let mut variants = Vec::new();
    for (index, variant) in enum_.variants_list(db).data(db).iter().enumerate() {
        let Some(name) = variant.name.to_opt() else {
            return;
        };
        let (kind, fields) = match &variant.kind {
            VariantKind::Unit => (ReflectedVariantKind::Unit, Vec::new()),
            VariantKind::Tuple(tup) => {
                let mut fields = Vec::new();
                for (field_idx, ty) in tup.data(db).iter().enumerate() {
                    let Some(ty) = ty.to_opt() else {
                        return;
                    };
                    fields.push(ReflectedField {
                        variant: Some(index),
                        index: field_idx,
                        name: FieldName::Positional(field_idx),
                        ty,
                    });
                }
                (ReflectedVariantKind::Tuple, fields)
            }
            VariantKind::Record(record_fields) => {
                let mut fields = Vec::new();
                for (field_idx, field) in record_fields.data(db).iter().enumerate() {
                    let (Some(name), Some(ty)) = (field.name.to_opt(), field.type_ref.to_opt())
                    else {
                        return;
                    };
                    fields.push(ReflectedField {
                        variant: Some(index),
                        index: field_idx,
                        name: FieldName::Named(name),
                        ty,
                    });
                }
                (ReflectedVariantKind::Record, fields)
            }
        };
        variants.push(ReflectedVariant {
            index,
            name,
            kind,
            is_default: default_variant == Some(index)
                || (!derives_default && first_default_variant_index(ast) == Some(index)),
            fields,
        });
    }

    let reflection = TargetReflection {
        shape: TargetShape::Enum { variants },
    };

    // When a `Default` request has no `#[default]` variant the request is
    // skipped (the missing-marker diagnostic was reported above); other
    // requests still run.
    let runnable: Vec<DeriveRequest<'db>> = requests
        .iter()
        .filter(|request| !(request.trait_name.data(db) == "Default" && default_variant.is_none()))
        .cloned()
        .collect();

    execute_requests(
        ctxt,
        &item_name,
        enum_name_ident,
        &generics,
        &reflection,
        &runnable,
    );
}

/// Selects, executes, and replays the provider for each request targeting
/// one item. Failures accumulate diagnostics; they never abort the other
/// requests.
fn execute_requests<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    item_name: &Option<String>,
    target_name: IdentId<'db>,
    generics: &DeriveGenerics<'db>,
    reflection: &TargetReflection<'db>,
    requests: &[DeriveRequest<'db>],
) {
    let db = ctxt.db();
    let self_ty = derive_self_ty(ctxt, target_name, generics);

    for request in requests {
        let trait_name = request.trait_name.data(db).to_string();

        let provider: &ValidatedProvider<'db> = match provider::select_provider(
            db,
            ctxt.top_mod(),
            request.trait_path,
            request.selection,
        ) {
            SelectionOutcome::Found(provider) => provider,
            SelectionOutcome::NotFound { wrong_goal_heads } => {
                let kind = match request.selection {
                    ProviderSelection::Canonical => DeriveErrorKind::UnknownTrait {
                        name: trait_name,
                        available: provider::core_derivable_trait_names(db, ctxt.top_mod()),
                    },
                    ProviderSelection::Named(path) => DeriveErrorKind::ProviderNotFound {
                        provider: path.pretty_print(db),
                        trait_name,
                        wrong_goal_heads,
                    },
                };
                accumulate_error(ctxt, item_name, kind, request.selection_range());
                continue;
            }
            SelectionOutcome::Ambiguous { provider_names } => {
                let kind = match request.selection {
                    ProviderSelection::Canonical => DeriveErrorKind::CanonicalProviderAmbiguous {
                        trait_name,
                        providers: provider_names,
                    },
                    ProviderSelection::Named(path) => DeriveErrorKind::ProviderAmbiguous {
                        provider: path.pretty_print(db),
                        trait_name,
                        count: provider_names.len(),
                    },
                };
                accumulate_error(ctxt, item_name, kind, request.selection_range());
                continue;
            }
        };

        match ProviderExecutor::run(db, provider, reflection, self_ty, target_name) {
            Ok(output) => {
                let trait_ref = TraitRefId::new(db, Partial::Present(provider.trait_path));
                let mut builder = HirBuilder::new(ctxt, request.desugared.clone());
                synthesize_provider_impl(
                    &mut builder,
                    target_name,
                    self_ty,
                    generics,
                    reflection,
                    trait_ref,
                    &output,
                );
            }
            Err(err) => {
                let provider_file = provider.provider.top_mod(db).file(db);
                let secondary = DeriveSecondarySpan {
                    file: provider_file,
                    range: err.range,
                    label: err.kind.message(),
                };
                accumulate_error_with(
                    ctxt,
                    item_name,
                    DeriveErrorKind::ProviderFailed {
                        provider: provider.name.data(db).to_string(),
                        trait_name,
                        message: err.kind.message(),
                    },
                    request.primary_range,
                    Some(secondary),
                );
            }
        }
    }
}
