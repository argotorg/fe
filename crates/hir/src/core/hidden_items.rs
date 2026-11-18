use common::{indexmap::IndexSet, ingot::IngotKind};
use parser::TextRange;

use crate::{
    analysis::{
        diagnostics::SpannedHirAnalysisDb,
        name_resolution::{
            method_selection::select_trait_method_for_type,
            path_resolver::{inherent_impl_self_types_unify, select_trait_const_for_type},
        },
        ty::{
            canonical::Canonicalized,
            trait_lower::lower_impl_trait_header,
            trait_resolution::{PredicateListId, constraint::collect_constraints},
            ty_def::TyId,
        },
    },
    core::{
        hir_def::{HirIngot, IdentId, Impl, ItemKind, TopLevelMod, Trait, scope_graph::ScopeId},
        lower::source_location,
    },
    span::LazySpan,
};

/// A generated member of an inherent header that its type already has under
/// that name, through a written inherent member or a trait member.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HiddenGeneratedMember {
    pub name: String,
    /// `"constant"` or `"method"`.
    pub kind: &'static str,
    pub self_ty: String,
    pub existing: String,
    pub file: url::Url,
    pub range: TextRange,
}

impl HiddenGeneratedMember {
    pub fn diagnostic_message(&self) -> String {
        format!(
            "The generated {kind} `{name}` of `{ty}` would never be selected: `{ty}` already has {existing}",
            kind = self.kind,
            name = self.name,
            ty = self.self_ty,
            existing = self.existing,
        )
    }
}

/// Returns generated members of inherent headers in `top_mod` whose name the
/// target type already has through a written member or a trait member.
pub fn hidden_generated_members<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<HiddenGeneratedMember> {
    let Some(default_url) = top_mod.file(db).url(db) else {
        return Vec::new();
    };
    let mut hidden = Vec::new();
    let all_impls = top_mod.all_impls(db);
    for (imp_idx, &imp) in all_impls.iter().enumerate() {
        if imp.derive_provider(db).is_none() {
            continue;
        }
        let Some(ty) = imp.admissible_inherent_impl_ty(db) else {
            continue;
        };
        let assumptions = collect_constraints(db, imp.into()).instantiate_identity();
        let self_ty = ty.pretty_print(db).to_string();
        let (header_file, header_range) = imp
            .span()
            .resolve(db)
            .and_then(|span| Some((span.file.url(db)?, span.range)))
            .unwrap_or_else(|| (default_url.clone(), TextRange::empty(0.into())));

        let mut seen_in_self: Vec<(IdentId<'db>, Option<String>)> = Vec::new();
        let members = imp
            .assoc_consts(db)
            .map(|c| {
                let loc = c
                    .span()
                    .name()
                    .resolve(db)
                    .map(|span| source_location(db, span.file, span.range));
                ("constant", c.name(db), loc)
            })
            .chain(imp.funcs(db).map(|func| {
                let loc = func
                    .span()
                    .name()
                    .resolve(db)
                    .map(|span| source_location(db, span.file, span.range));
                ("method", func.name(db).to_opt(), loc)
            }))
            .collect::<Vec<_>>();

        for (kind, name_opt, own_loc) in members {
            let Some(name) = name_opt else {
                continue;
            };
            let existing = existing_member_of_type(
                db,
                top_mod,
                imp,
                imp_idx,
                ty,
                name,
                assumptions,
                &seen_in_self,
            );
            seen_in_self.push((name, own_loc));
            let Some(existing) = existing else {
                continue;
            };
            hidden.push(HiddenGeneratedMember {
                name: name.data(db).to_string(),
                kind,
                self_ty: self_ty.clone(),
                existing,
                file: header_file.clone(),
                range: header_range,
            });
        }
    }
    hidden
}

#[allow(clippy::too_many_arguments)]
fn existing_member_of_type<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    imp: Impl<'db>,
    imp_idx: usize,
    ty: TyId<'db>,
    name: IdentId<'db>,
    assumptions: PredicateListId<'db>,
    seen_in_self: &[(IdentId<'db>, Option<String>)],
) -> Option<String> {
    if let Some((_, prev_loc)) = seen_in_self.iter().find(|(prev, _)| *prev == name) {
        let loc_suffix = prev_loc
            .as_ref()
            .map(|loc| format!(" ({loc})"))
            .unwrap_or_default();
        return Some(format!("a written `{}`{loc_suffix}", name.data(db)));
    }

    let own_ingot = top_mod.ingot(db);
    let mut search_ingots: IndexSet<common::ingot::Ingot<'db>> = IndexSet::default();
    for (_, ext) in own_ingot.resolved_external_ingots(db) {
        search_ingots.insert(*ext);
    }
    search_ingots.insert(own_ingot);

    for ingot in search_ingots {
        for &other_imp in ingot.all_impls(db).iter() {
            if other_imp == imp {
                continue;
            }
            // A later header of the same file reports the earlier one.
            if other_imp.top_mod(db) == top_mod
                && other_imp.derive_provider(db).is_some()
                && top_mod.all_impls(db).iter().position(|&i| i == other_imp) > Some(imp_idx)
            {
                continue;
            }
            if !inherent_impl_self_types_unify(db, imp, other_imp) {
                continue;
            }
            if let Some(c) = other_imp
                .assoc_consts(db)
                .find(|c| c.name(db) == Some(name))
            {
                let loc_suffix = c
                    .span()
                    .name()
                    .resolve(db)
                    .map(|span| format!(" ({})", source_location(db, span.file, span.range)))
                    .unwrap_or_default();
                return Some(format!("a written `{}`{loc_suffix}", name.data(db)));
            }
            if let Some(func) = other_imp
                .funcs(db)
                .find(|f| f.name(db).to_opt() == Some(name))
            {
                let loc_suffix = func
                    .span()
                    .name()
                    .resolve(db)
                    .map(|span| format!(" ({})", source_location(db, span.file, span.range)))
                    .unwrap_or_default();
                return Some(format!("a written `{}`{loc_suffix}", name.data(db)));
            }
        }
    }

    if let Some(trait_) = select_trait_const_for_type(db, ty, name, imp.scope(), assumptions) {
        let trait_name = trait_
            .name(db)
            .to_opt()
            .map_or_else(|| "<trait>".to_string(), |ident| ident.data(db).to_string());
        let loc_suffix = trait_
            .const_(db, name)
            .and_then(|c| c.span().name().resolve(db))
            .map(|span| format!(" ({})", source_location(db, span.file, span.range)))
            .unwrap_or_default();
        return Some(format!(
            "the constant `{}` of the trait `{trait_name}`{loc_suffix}",
            name.data(db)
        ));
    }

    if let Some((trait_, method)) = select_trait_method_for_type(
        db,
        &Canonicalized::new(db, ty),
        name,
        imp.scope(),
        assumptions,
    ) {
        let trait_name = trait_
            .name(db)
            .to_opt()
            .map_or_else(|| "<trait>".to_string(), |ident| ident.data(db).to_string());
        let loc_suffix = method
            .span()
            .name()
            .resolve(db)
            .map(|span| format!(" ({})", source_location(db, span.file, span.range)))
            .unwrap_or_default();
        return Some(format!(
            "the method `{}` of the trait `{trait_name}`{loc_suffix}",
            name.data(db)
        ));
    }

    None
}

/// Refuses deciding-trait implementations nested inside generated members or
/// generated functions in `top_mod`.
pub fn nested_generated_deciding_impl_errors<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<(url::Url, TextRange, String)> {
    let Some(default_url) = top_mod.file(db).url(db) else {
        return Vec::new();
    };
    let mut errors = Vec::new();
    for &imp in top_mod.all_impl_traits(db) {
        if imp.selection_name(db).is_some() {
            continue;
        }
        let Some(enclosing_generated) = enclosing_generated_item(db, imp.scope()) else {
            continue;
        };
        let Some(implementor) = lower_impl_trait_header(db, imp) else {
            continue;
        };
        let trait_ = implementor.trait_def(db);
        let Some(decision) = deciding_trait_description(db, trait_) else {
            continue;
        };
        let trait_name = implementor.trait_(db).pretty_print(db, false);
        let subject = implementor.self_ty(db).pretty_print(db);
        let (file, range) = enclosing_request_span(db, enclosing_generated)
            .or_else(|| {
                imp.span()
                    .resolve(db)
                    .and_then(|span| Some((span.file.url(db)?, span.range)))
            })
            .unwrap_or_else(|| (default_url.clone(), TextRange::empty(0.into())));
        let message = format!(
            "the generated implementation of `{trait_name}` for `{subject}` is refused: it could change {decision} for `{subject}`. The compiler decides this by whether a type implements `{trait_name}`, so only an implementation that the package's own code or a dependency wrote counts."
        );
        errors.push((file, range, message));
    }
    errors
}

fn enclosing_generated_item<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    scope: ScopeId<'db>,
) -> Option<ItemKind<'db>> {
    let mut current = scope.parent(db);
    while let Some(parent) = current {
        if let ScopeId::Item(item) = parent
            && parent.data(db).publication_parent.is_some()
        {
            return Some(item);
        }
        current = parent.parent(db);
    }
    None
}

fn enclosing_request_span<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    generated_item: ItemKind<'db>,
) -> Option<(url::Url, TextRange)> {
    match generated_item.scope().parent(db) {
        Some(ScopeId::Item(ItemKind::Impl(imp))) => {
            let span = imp.span().resolve(db)?;
            Some((span.file.url(db)?, span.range))
        }
        Some(ScopeId::Item(ItemKind::ImplTrait(imp_trait))) => {
            let span = imp_trait.span().resolve(db)?;
            Some((span.file.url(db)?, span.range))
        }
        _ => None,
    }
}

fn deciding_trait_description<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    trait_: Trait<'db>,
) -> Option<&'static str> {
    let name = trait_.name(db).to_opt()?.data(db).as_str();
    match (trait_.ingot(db).kind(db), name) {
        (IngotKind::Core, "Drop") => Some("what runs where a value goes out of use"),
        (IngotKind::Core, "Copy") => Some("whether a value is copied or moved"),
        (IngotKind::Core, "Snapshot") => Some("whether a view takes a value by copy"),
        (IngotKind::Core, "Pinned") => Some("where a value may be kept"),
        (IngotKind::Core, "StateIndex") => {
            Some("whether indexing a collection is a call or an entry")
        }
        (IngotKind::Core, "LaneCodec") => Some("how a collection packs its elements into lanes"),
        (IngotKind::Core, "ErrorVariant") => Some("what data a revert with a value carries"),
        (IngotKind::Core, "Fn" | "FnMut") => Some("which implementation a call of a value runs"),
        (IngotKind::Core, "Producer" | "Driver") => Some("which protocol a `for` loop runs"),
        (IngotKind::Std, "SolPacked") => Some("how a stored value is packed"),
        _ => None,
    }
}
