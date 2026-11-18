//! Generated items that an authored binding hides.
//!
//! Inside the package that generates them, generated items only fill gaps: a
//! name that the package's own code, a dependency, an import or the prelude
//! already binds keeps that binding, and a generated item is found only where
//! nothing else answers the name. A generated item published into a module
//! that already binds its name would therefore never be found there. That is
//! reported where the item is requested, so that the behaviour stays
//! explainable, instead of leaving an item that silently does nothing.

use parser::TextRange;

use super::{
    EarlyNameQueryId, NameDerivation, NameDomain, NameRes, NameResKind, NameResolutionError,
    QueryDirective, Tier, ingot_has_generated_items,
    method_selection::MethodCandidate,
    path_resolver::{AuthoredMember, authored_member},
    resolve_query_in,
};
use crate::{
    analysis::{
        diagnostics::SpannedHirAnalysisDb,
        ty::ty_check::{UseTarget, item_target, target_at},
    },
    hir_def::{CallableDef, IdentId, Impl, ItemKind, TopLevelMod, Use, scope_graph::ScopeId},
    span::{HirOrigin, LazySpan},
};

/// A place in source, with the text found there.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Located {
    pub file: url::Url,
    pub range: TextRange,
    pub text: String,
}

/// How a module reaches the item a name is bound to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Via {
    /// The module declares the item.
    Item,
    NamedImport(Option<Located>),
    GlobImport(Option<Located>),
    /// The names every module has without importing them.
    Prelude,
    /// An ingot the package depends on, named by its alias.
    Ingot,
    /// A builtin type.
    Builtin,
    /// The name is found in an enclosing module.
    Enclosing(Box<Via>),
}

/// What binds a name that a generated item would take.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    /// The domain of the binding: a type, a value or an implementation.
    pub domain: &'static str,
    /// The bound item, if it is an item of a package.
    pub target: Option<UseTarget>,
    pub via: Via,
    /// Several items bind the name equally; the first is described.
    pub ambiguous: bool,
}

/// A generated item that is hidden in the module it was published into.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HiddenGeneratedItem {
    pub name: String,
    /// The domain the generated item would bind the name in.
    pub domain: &'static str,
    /// The generated item, with the place of the quotation that wrote it.
    pub generated: UseTarget,
    pub file: url::Url,
    /// The `mod` item by its source range, or `None` for the file's root.
    pub module: Option<TextRange>,
    pub hidden_by: Binding,
    /// For an authored named import that names the generated item: the import.
    /// The import imports nothing in the authored tier, so the name means what
    /// `hidden_by` says in the scope of the import.
    pub imported_by: Option<Located>,
}

/// The generated items of the root module of `top_mod` and of each `mod`
/// written in it, whose names are bound in their module by something other
/// than themselves.
pub fn hidden_generated_items<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<HiddenGeneratedItem> {
    let ingot = top_mod.ingot(db);
    let Some(file) = top_mod.file(db).url(db) else {
        return Vec::new();
    };
    if !*ingot_has_generated_items(db, ingot) {
        return Vec::new();
    }
    let mut modules = vec![(None, top_mod.scope())];
    for module in top_mod.all_mods(db) {
        if let HirOrigin::Raw(pointer) = module.origin(db) {
            modules.push((Some(pointer.syntax_node_ptr().text_range()), module.scope()));
        }
    }
    let mut hidden = Vec::new();
    for (range, scope) in modules {
        for child in scope.child_items(db) {
            let child_scope = ScopeId::from_item(child);
            if !child_scope.is_generated_in(db, ingot) {
                continue;
            }
            let Some(name) = child.name(db) else {
                continue;
            };
            let label = if matches!(child, ItemKind::ImplTrait(_)) {
                domain_label(NameDomain::IMPL)
            } else {
                domain_label(NameDomain::from_scope(db, child_scope))
            };
            // The lookup a name in the module gets without the generated
            // items, through the module's own bindings, its enclosing modules
            // and the prelude.
            let query = EarlyNameQueryId::new(db, name, scope, QueryDirective::new());
            let bucket = resolve_query_in(db, query, Tier::Authored);
            let Some(binding) = describe_binding(db, bucket) else {
                continue;
            };
            let name = name.data(db).to_string();
            // An implementation has no name span of its own; the span of the
            // whole implementation says where the provider's quotation wrote it.
            let generated = match child {
                ItemKind::ImplTrait(imp) => {
                    target_at(db, child_scope, imp.span().resolve(db), name.clone())
                }
                _ => item_target(db, child_scope, name.clone()),
            };
            let Some(generated) = generated else {
                continue;
            };
            hidden.push(HiddenGeneratedItem {
                name,
                domain: label,
                generated,
                file: file.clone(),
                module: range,
                hidden_by: binding,
                imported_by: None,
            });
        }
    }
    hidden.extend(hidden_imports(db, top_mod, &file));
    hidden
}

/// The authored named imports of `top_mod` that name an item generated in the
/// ingot while something else binds the name in the scope of the import. In the
/// authored tier the import imports nothing, so the other binding wins: the
/// import would not find the generated item.
fn hidden_imports<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    file: &url::Url,
) -> Vec<HiddenGeneratedItem> {
    let ingot = top_mod.ingot(db);
    let mut hidden = Vec::new();
    for (&scope, named) in &super::resolve_imports(db, ingot, Tier::All)
        .1
        .named_resolved
    {
        if scope.top_mod(db) != top_mod {
            continue;
        }
        for (&name, bucket) in named {
            for res in bucket.iter_ok() {
                let NameResKind::Scope(target) = res.kind else {
                    continue;
                };
                let Some(use_) = res.derivation.use_stmt() else {
                    continue;
                };
                if !target.is_generated_in(db, ingot) || use_.is_synthetic_use(db) {
                    continue;
                }
                let query = EarlyNameQueryId::new(db, name, scope, QueryDirective::new());
                let authored = resolve_query_in(db, query, Tier::Authored);
                let Some(binding) = describe_binding(db, authored) else {
                    continue;
                };
                let name = name.data(db).to_string();
                let Some(generated) = item_target(db, target, name.clone()) else {
                    continue;
                };
                let module = match scope {
                    ScopeId::Item(ItemKind::Mod(_)) => Some(scope),
                    ScopeId::Item(ItemKind::TopMod(_)) => None,
                    other => other.parent_module(db),
                }
                .and_then(|module| match module {
                    ScopeId::Item(ItemKind::Mod(module)) => match module.origin(db) {
                        HirOrigin::Raw(pointer) => Some(pointer.syntax_node_ptr().text_range()),
                        _ => None,
                    },
                    _ => None,
                });
                hidden.push(HiddenGeneratedItem {
                    name,
                    domain: domain_label(res.domain),
                    generated,
                    file: file.clone(),
                    module,
                    hidden_by: binding,
                    imported_by: located(db, use_),
                });
            }
        }
    }
    hidden
}

fn domain_label(domain: NameDomain) -> &'static str {
    if domain == NameDomain::TYPE {
        "type"
    } else if domain == NameDomain::VALUE {
        "value"
    } else if domain == NameDomain::IMPL {
        "implementation"
    } else {
        "name"
    }
}

/// What the lookup found, or `None` if it found nothing.
fn describe_binding<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    bucket: &super::NameResBucket<'db>,
) -> Option<Binding> {
    for (domain, result) in &bucket.bucket {
        let (res, ambiguous) = match result {
            Ok(res) => (res, false),
            Err(NameResolutionError::Ambiguous(candidates)) if !candidates.is_empty() => {
                (&candidates[0], true)
            }
            Err(_) => continue,
        };
        return Some(binding_of(db, *domain, res, ambiguous));
    }
    // An answer that is an error other than ambiguity, such as a name that is
    // not visible, still binds the name.
    bucket.answers().then_some(Binding {
        domain: "name",
        target: None,
        via: Via::Item,
        ambiguous: false,
    })
}

fn binding_of<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    domain: NameDomain,
    res: &NameRes<'db>,
    ambiguous: bool,
) -> Binding {
    let target = match (res.kind, res.derivation.origin()) {
        (NameResKind::Scope(scope), derivation)
            if !matches!(derivation, NameDerivation::External | NameDerivation::Prim) =>
        {
            scope
                .name(db)
                .and_then(|name| item_target(db, scope, name.data(db).to_string()))
        }
        _ => None,
    };
    Binding {
        domain: domain_label(domain),
        target,
        via: via(db, &res.derivation),
        ambiguous,
    }
}

fn via<'db>(db: &'db dyn SpannedHirAnalysisDb, derivation: &NameDerivation<'db>) -> Via {
    match derivation {
        NameDerivation::Lex(inner) => Via::Enclosing(Box::new(via(db, inner))),
        NameDerivation::NamedImported(use_) => Via::NamedImport(located(db, *use_)),
        NameDerivation::GlobImported(use_) if use_.is_synthetic_use(db) => Via::Prelude,
        NameDerivation::GlobImported(use_) => Via::GlobImport(located(db, *use_)),
        NameDerivation::Def => Via::Item,
        NameDerivation::External => Via::Ingot,
        NameDerivation::Prim => Via::Builtin,
    }
}

fn located<'db>(db: &'db dyn SpannedHirAnalysisDb, use_: Use<'db>) -> Option<Located> {
    let span = use_.span().resolve(db)?;
    let file = span.file.url(db)?;
    let text = span.file.text(db);
    let (start, end) = (
        usize::from(span.range.start()),
        usize::from(span.range.end()),
    );
    Some(Located {
        file,
        range: span.range,
        text: text.get(start..end)?.to_string(),
    })
}

/// What already has the name of a generated member on its type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberBinding {
    /// For example "the method `m` of the trait `Tr`".
    pub description: String,
    pub target: Option<UseTarget>,
}

/// A generated member of an inherent implementation that the type already has
/// under that name, through an inherent member or a trait, in the authored
/// tier. A path or call that names it finds the existing member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HiddenGeneratedMember {
    pub name: String,
    /// "constant" or "method".
    pub kind: &'static str,
    pub self_ty: String,
    pub generated: UseTarget,
    /// Where the inherent header `impl Type with (P)` whose provider wrote the
    /// member is written; `None` for a member of a generated implementation.
    /// The header is the request: several headers can run one provider, so
    /// the quotation that holds the member's text does not tell them apart.
    pub header: Option<(url::Url, TextRange)>,
    pub file: url::Url,
    pub hidden_by: MemberBinding,
}

/// The generated members, of the inherent implementations that generation
/// added to the modules of `top_mod` and of its inherent headers, that their
/// type already has.
pub fn hidden_generated_members<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<HiddenGeneratedMember> {
    let ingot = top_mod.ingot(db);
    let Some(file) = top_mod.file(db).url(db) else {
        return Vec::new();
    };
    if !*ingot_has_generated_items(db, ingot) {
        return Vec::new();
    }
    let mut hidden = Vec::new();
    for &imp in top_mod.all_impls(db) {
        // A provider wrote every member of a generated implementation, and
        // every member of an authored inherent header `impl Type with (P)`.
        if !imp.members_generated_in(db, ingot) {
            continue;
        }
        let generated_impl = imp.scope().is_within_generated(db, ingot);
        let Some(ty) = imp.admissible_inherent_impl_ty(db) else {
            continue;
        };
        let header = if generated_impl {
            None
        } else {
            imp.span()
                .resolve(db)
                .and_then(|span| Some((span.file.url(db)?, span.range)))
        };
        // Each member with where its text is written, in the provider's
        // quotation.
        let members = imp
            .hir_consts(db)
            .iter()
            .filter_map(|c| {
                let written = c.generated.and_then(|syntax| {
                    Some((syntax.top_mod.file(db).url(db)?, syntax.node.text_range()))
                });
                Some(("constant", c.name.to_opt()?, written))
            })
            .chain(imp.funcs(db).filter_map(|func| {
                let written = func
                    .span()
                    .resolve(db)
                    .and_then(|span| Some((span.file.url(db)?, span.range)));
                Some(("method", func.name(db).to_opt()?, written))
            }))
            .collect::<Vec<_>>();
        for (kind, name, written) in members {
            let Some(found) = authored_member_of(db, imp, name) else {
                continue;
            };
            let name = name.data(db).to_string();
            let generated = if generated_impl {
                target_at(db, imp.scope(), imp.span().resolve(db), name.clone())
            } else {
                Some(UseTarget::Generated {
                    name: name.clone(),
                    ingot: imp.scope().ingot(db).base(db).clone(),
                    source: written,
                })
            };
            let Some(generated) = generated else {
                continue;
            };
            hidden.push(HiddenGeneratedMember {
                self_ty: ty.pretty_print(db).to_string(),
                hidden_by: describe_member(db, &found, &name, ty.pretty_print(db)),
                name,
                kind,
                generated,
                header: header.clone(),
                file: file.clone(),
            });
        }
    }
    hidden
}

fn authored_member_of<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    imp: Impl<'db>,
    name: IdentId<'db>,
) -> Option<AuthoredMember<'db>> {
    let ty = imp.admissible_inherent_impl_ty(db)?;
    let assumptions =
        crate::analysis::ty::trait_resolution::constraint::collect_constraints(db, imp.into())
            .instantiate_identity();
    authored_member(db, ty, name, imp.scope(), assumptions)
}

fn describe_member<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    found: &AuthoredMember<'db>,
    name: &str,
    ty: &str,
) -> MemberBinding {
    let in_trait = |func_trait: Option<crate::hir_def::Trait<'db>>, what: &str| {
        let trait_name = func_trait
            .and_then(|trait_| trait_.name(db).to_opt())
            .map_or_else(
                || "a trait".to_string(),
                |ident| format!("the trait `{}`", ident.data(db)),
            );
        (
            format!("the {what} `{name}` of {trait_name}"),
            func_trait.and_then(|trait_| item_target(db, trait_.scope(), name.to_string())),
        )
    };
    let (description, target) = match found {
        AuthoredMember::InherentConst(other) => (
            format!("an inherent constant `{name}` of `{ty}`"),
            target_at(
                db,
                other.scope(),
                other.span().resolve(db),
                name.to_string(),
            ),
        ),
        AuthoredMember::TraitConst(inst) => in_trait(Some(inst.def(db)), "constant"),
        AuthoredMember::Method(MethodCandidate::InherentMethod(probed)) => (
            format!("an inherent method `{name}` of `{ty}`"),
            match probed.def {
                CallableDef::Func(func) => item_target(db, func.scope(), name.to_string()),
                _ => None,
            },
        ),
        AuthoredMember::Method(
            MethodCandidate::TraitMethod(cand) | MethodCandidate::NeedsConfirmation(cand),
        ) => in_trait(cand.method.containing_trait(db), "method"),
        AuthoredMember::Unclear => (format!("several members named `{name}` of `{ty}`"), None),
    };
    MemberBinding {
        description,
        target,
    }
}
