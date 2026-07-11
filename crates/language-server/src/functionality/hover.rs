use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{
    HirDb,
    analysis::ty::ty_check::{EffectParamSite, LocalBinding, ParamSite},
    core::semantic::{
        ContractFieldId, EffectEnvView, FieldStorageLayout, FieldView, ProviderSource,
        reference::{ReferenceView, Target},
    },
    hir_def::{FieldParent, ItemKind, PathId, scope_graph::ScopeId},
    lower::map_file_to_mod,
    span::LazySpan,
};
use tracing::debug;

use super::{
    goto::Cursor,
    item_info::{get_docstring, get_item_definition_markdown, get_item_path_markdown},
};
use crate::util::{to_lsp_range_from_span, to_offset_from_position};
use driver::DriverDataBase;

/// Returns `(hover_result, doc_path)`.
///
/// `doc_path` is the documentation URL path for the first resolved scope target
/// (e.g. `"mylib::Foo/struct"`), used for `fe/navigate` notifications.
fn local_name_from_reference<'db>(
    db: &'db dyn HirDb,
    reference: &ReferenceView<'db>,
) -> Option<String> {
    let ReferenceView::Path(path_view) = reference else {
        return None;
    };
    let ident = path_view.path.ident(db).to_opt()?;
    Some(ident.data(db).to_string())
}

fn contract_from_effect_site<'db>(
    db: &'db DriverDataBase,
    site: EffectParamSite<'db>,
) -> Option<hir::hir_def::Contract<'db>> {
    match site {
        EffectParamSite::Contract(contract)
        | EffectParamSite::ContractInit { contract }
        | EffectParamSite::ContractRecvArm { contract, .. } => Some(contract),
        EffectParamSite::Func(func) => match func.scope().parent_item(db) {
            Some(ItemKind::Contract(contract)) => Some(contract),
            _ => None,
        },
    }
}

fn effect_key_path_at_site<'db>(
    db: &'db DriverDataBase,
    site: EffectParamSite<'db>,
    idx: usize,
) -> Option<PathId<'db>> {
    match site {
        EffectParamSite::Func(func) => func.effect_params(db).nth(idx)?.key_path(db),
        EffectParamSite::Contract(contract) => contract.effect_params(db).nth(idx)?.key_path(db),
        EffectParamSite::ContractInit { contract } => contract
            .init(db)?
            .effects(db)
            .data(db)
            .get(idx)?
            .key_path
            .to_opt(),
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => contract
            .recv(db, recv_idx)?
            .arm(db, arm_idx)?
            .effects(db)
            .data(db)
            .get(idx)?
            .key_path
            .to_opt(),
    }
}

fn effect_binding_provider_source_at_site<'db>(
    db: &'db DriverDataBase,
    site: EffectParamSite<'db>,
    idx: usize,
) -> Option<ProviderSource<'db>> {
    let view = EffectEnvView::new(site);
    let provider_idx = view
        .resolutions(db)
        .into_iter()
        .find(|resolution| resolution.requirement_idx as usize == idx)?
        .provider_idx;
    view.providers(db)
        .into_iter()
        .find(|provider| provider.provider_idx == provider_idx)
        .map(|provider| provider.source)
}

fn contract_field_id_by_index<'db>(
    db: &'db DriverDataBase,
    contract: hir::hir_def::Contract<'db>,
    field_idx: u32,
) -> Option<ContractFieldId<'db>> {
    hir::hir_def::FieldParent::Contract(contract)
        .fields(db)
        .filter_map(|field| field.name(db))
        .nth(field_idx as usize)?;
    Some(ContractFieldId {
        contract,
        index: field_idx,
    })
}

fn contract_field_id_from_scope<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
) -> Option<ContractFieldId<'db>> {
    let ScopeId::Field(FieldParent::Contract(contract), idx) = scope else {
        return None;
    };
    FieldView {
        parent: FieldParent::Contract(contract),
        idx: usize::from(idx),
    }
    .contract_field_id(db)
}

fn contract_field_id_from_local_binding<'db>(
    db: &'db DriverDataBase,
    binding: LocalBinding<'db>,
) -> Option<ContractFieldId<'db>> {
    match binding {
        LocalBinding::Param {
            site: ParamSite::EffectField(effect_site),
            idx,
            ..
        } => {
            let contract = contract_from_effect_site(db, effect_site)?;
            let key_path = effect_key_path_at_site(db, effect_site, idx)?;
            let name = key_path.ident(db).to_opt()?;
            contract
                .storage_layout(db)
                .values()
                .find(|field| field.name == name)
                .map(|field| field.field)
                .or_else(|| {
                    FieldParent::Contract(contract)
                        .fields(db)
                        .filter_map(|field| field.name(db))
                        .position(|field_name| field_name == name)
                        .and_then(|idx| contract_field_id_by_index(db, contract, idx as u32))
                })
        }
        LocalBinding::EffectParam { site, idx, .. } => {
            let ProviderSource::ContractField { field_idx, .. } =
                effect_binding_provider_source_at_site(db, site, idx)?
            else {
                return None;
            };
            let contract = contract_from_effect_site(db, site)?;
            contract_field_id_by_index(db, contract, field_idx)
        }
        _ => None,
    }
}

fn allocated_layout_footer(db: &DriverDataBase, field: &FieldStorageLayout<'_>) -> String {
    let end = field.slot_offset + field.slot_count;
    let mut lines = vec![
        format!(
            "slot: {} (range: {}..{}, count: {})",
            field.slot_offset, field.slot_offset, end, field.slot_count
        ),
        format!("space: {}", field.address_space.pretty()),
    ];
    let mut explicit = Vec::new();
    for occurrence in &field.concrete_occurrences {
        let root = (occurrence.space, occurrence.value);
        if explicit.contains(&root) {
            continue;
        }
        explicit.push(root);
        lines.push(format!(
            "explicit root: slot {} ({})",
            occurrence.value.data(db),
            occurrence.space.pretty()
        ));
    }
    for cell in &field.cells {
        if let Some(allocation) = cell.allocation {
            lines.push(format!(
                "scalar root: slot {} ({})",
                allocation.slot,
                allocation.space.pretty()
            ));
        }
    }
    for family in &field.families {
        let Some(allocation) = family.allocation else {
            continue;
        };
        let end = allocation.slot + family.extent;
        let formula = family
            .strides
            .iter()
            .enumerate()
            .map(|(idx, stride)| {
                if *stride == 1 {
                    format!("i{idx}")
                } else {
                    format!("i{idx}*{stride}")
                }
            })
            .collect::<Vec<_>>()
            .join(" + ");
        let dimensions = family
            .dimensions
            .iter()
            .map(|dimension| dimension.len.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        lines.push(format!(
            "root family: slots {}..{} ({}, dimensions [{}], root = {} + {})",
            allocation.slot,
            end,
            allocation.space.pretty(),
            dimensions,
            allocation.slot,
            formula,
        ));
    }
    lines.join("\n")
}

fn contract_field_layout_footer_for_id<'db>(
    db: &'db DriverDataBase,
    field: ContractFieldId<'db>,
) -> Option<String> {
    let result = field.contract.storage_layout(db);
    if let Some(layout) = result.values().find(|layout| layout.field == field) {
        return Some(allocated_layout_footer(db, layout));
    }
    if let Some(errors) = result.field_errors_for_id(field) {
        return Some(format!(
            "layout: invalid ({})",
            errors
                .first()
                .map_or("unknown layout error", |error| error.summary())
        ));
    }
    Some(
        "layout: allocation unavailable because another contract field has an invalid layout"
            .to_string(),
    )
}

fn contract_field_layout_footer<'db>(
    db: &'db DriverDataBase,
    target: &Target<'db>,
) -> Option<String> {
    let field = match target {
        Target::Scope(scope) => contract_field_id_from_scope(db, *scope)?,
        Target::Local { binding, .. } => contract_field_id_from_local_binding(db, *binding)?,
    };
    contract_field_layout_footer_for_id(db, field)
}

fn contract_field_definition_hover(
    db: &DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'_>,
    cursor: Cursor,
) -> Option<Hover> {
    for item in top_mod.scope_graph(db).items_dfs(db) {
        let ItemKind::Contract(contract) = item else {
            continue;
        };
        for field in FieldParent::Contract(contract).fields(db) {
            let scope = ScopeId::Field(FieldParent::Contract(contract), field.idx as u16);
            let Some(span) = scope.name_span(db).and_then(|span| span.resolve(db)) else {
                continue;
            };
            if !span.range.contains(cursor) {
                continue;
            }
            let field = contract_field_id_from_scope(db, scope)?;
            let footer = contract_field_layout_footer_for_id(db, field)?;
            return Some(Hover {
                contents: async_lsp::lsp_types::HoverContents::Markup(
                    async_lsp::lsp_types::MarkupContent {
                        kind: async_lsp::lsp_types::MarkupKind::Markdown,
                        value: footer,
                    },
                ),
                range: to_lsp_range_from_span(span, db).ok(),
            });
        }
    }
    None
}

fn hover_markdown_for_target<'db>(
    db: &'db DriverDataBase,
    reference: &ReferenceView<'db>,
    target: &Target<'db>,
) -> Option<String> {
    let mut body = match target {
        Target::Scope(scope) => {
            let item = scope.item();
            let pretty_path = get_item_path_markdown(db, item);
            let definition_source = get_item_definition_markdown(db, item);
            let docs = get_docstring(db, *scope);

            [pretty_path, definition_source, docs]
                .iter()
                .filter_map(|info| info.clone().map(|info| format!("{info}\n")))
                .collect::<Vec<String>>()
                .join("\n")
        }
        Target::Local { ty, .. } => {
            let name = local_name_from_reference(db, reference)?;
            let ty_str = ty.pretty_print(db);
            format!("```fe\nlet {name}: {ty_str}\n```")
        }
    };

    if let Some(layout_footer) = contract_field_layout_footer(db, target) {
        body.push('\n');
        body.push_str(&layout_footer);
        body.push('\n');
    }

    Some(body)
}

pub fn hover_helper(
    db: &DriverDataBase,
    file: File,
    params: async_lsp::lsp_types::HoverParams,
) -> Result<(Option<Hover>, Option<String>), Error> {
    debug!("handling hover");
    let file_text = file.text(db);

    let cursor: Cursor = to_offset_from_position(
        params.text_document_position_params.position,
        file_text.as_str(),
    );

    let top_mod = map_file_to_mod(db, file);

    // Get the reference at cursor and resolve it
    let Some(r) = top_mod.reference_at(db, cursor) else {
        return Ok((contract_field_definition_hover(db, top_mod, cursor), None));
    };

    let resolution = r.target_at(db, cursor);

    // Extract doc path from the first scope target (for fe/navigate)
    let doc_path = resolution
        .as_slice()
        .iter()
        .find_map(|target| match target {
            Target::Scope(scope) => hir::semantic::scope_to_doc_path(db, *scope),
            Target::Local { .. } => None,
        });

    // Compute the hover range from the reference span at the cursor position.
    // For paths, use the specific segment span containing the cursor.
    let hover_range = match &r {
        ReferenceView::Path(pv) => {
            let mut seg_range = None;
            for idx in 0..=pv.path.segment_index(db) {
                if let Some(resolved) = pv.span.clone().segment(idx).resolve(db)
                    && resolved.range.contains(cursor)
                {
                    seg_range = to_lsp_range_from_span(resolved, db).ok();
                    break;
                }
            }
            seg_range
        }
        _ => r
            .span()
            .resolve(db)
            .and_then(|s| to_lsp_range_from_span(s, db).ok()),
    };

    // Build hover content
    let info = if resolution.is_ambiguous() {
        let mut sections = vec!["**Multiple definitions**\n\n".to_string()];

        for (i, target) in resolution.as_slice().iter().enumerate() {
            if let Some(section) = hover_markdown_for_target(db, r, target) {
                sections.push(format!("{section}\n\n"));
            }

            if i < resolution.as_slice().len() - 1 {
                sections.push("---\n\n".to_string());
            }
        }

        sections.join("")
    } else {
        let Some(target) = resolution.first() else {
            return Ok((None, doc_path));
        };
        let Some(info) = hover_markdown_for_target(db, r, target) else {
            return Ok((None, doc_path));
        };
        info
    };

    let result = async_lsp::lsp_types::Hover {
        contents: async_lsp::lsp_types::HoverContents::Markup(
            async_lsp::lsp_types::MarkupContent {
                kind: async_lsp::lsp_types::MarkupKind::Markdown,
                value: info,
            },
        ),
        range: hover_range,
    };
    Ok((Some(result), doc_path))
}
