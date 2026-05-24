use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{
    HirDb,
    analysis::ty::{
        ty_check::{EffectParamSite, LocalBinding, ParamSite},
        ty_def::TyId,
    },
    core::semantic::{
        EffectEnvView, ProviderSource,
        reference::{ReferenceView, Target},
    },
    hir_def::{FieldParent, ItemKind, PathId, scope_graph::ScopeId},
    lower::map_file_to_mod,
    span::LazySpan,
};
use trace_query::{
    ExplainLocalRequest, GasBreakdownRequest, IntrospectionService, TraceIntrospectionService,
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

fn contract_field_layout_by_index<'db>(
    db: &'db DriverDataBase,
    contract: hir::hir_def::Contract<'db>,
    field_idx: u32,
) -> Option<(usize, usize, TyId<'db>)> {
    let field = contract
        .field_layout(db)
        .values()
        .find(|field| field.index == field_idx)?;
    Some((field.slot_offset, field.slot_count, field.address_space))
}

fn contract_field_layout_from_scope<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
) -> Option<(usize, usize, TyId<'db>)> {
    let ScopeId::Field(FieldParent::Contract(contract), idx) = scope else {
        return None;
    };

    if let Some(name) = scope.name(db)
        && let Some(field) = contract.field_layout(db).get(&name)
    {
        return Some((field.slot_offset, field.slot_count, field.address_space));
    }

    contract_field_layout_by_index(db, contract, idx as u32)
}

fn contract_field_layout_from_local_binding<'db>(
    db: &'db DriverDataBase,
    binding: LocalBinding<'db>,
) -> Option<(usize, usize, TyId<'db>)> {
    match binding {
        LocalBinding::Param {
            site: ParamSite::EffectField(effect_site),
            idx,
            ..
        } => {
            let contract = contract_from_effect_site(db, effect_site)?;
            let key_path = effect_key_path_at_site(db, effect_site, idx)?;
            let name = key_path.ident(db).to_opt()?;
            let field = contract.field_layout(db).get(&name)?;
            Some((field.slot_offset, field.slot_count, field.address_space))
        }
        LocalBinding::EffectParam { site, idx, .. } => {
            let ProviderSource::ContractField { field_idx, .. } =
                effect_binding_provider_source_at_site(db, site, idx)?
            else {
                return None;
            };
            let contract = contract_from_effect_site(db, site)?;
            contract_field_layout_by_index(db, contract, field_idx)
        }
        _ => None,
    }
}

fn contract_field_layout_footer<'db>(
    db: &'db DriverDataBase,
    target: &Target<'db>,
) -> Option<String> {
    let (slot_offset, slot_count, address_space) = match target {
        Target::Scope(scope) => contract_field_layout_from_scope(db, *scope)?,
        Target::Local { binding, .. } => contract_field_layout_from_local_binding(db, *binding)?,
    };
    Some(format!(
        "slot: {slot_offset} (count: {slot_count})\nspace: {}",
        address_space.pretty_print(db)
    ))
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
    trace_service: Option<&TraceIntrospectionService>,
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
        return Ok((None, None));
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
    let mut info = if resolution.is_ambiguous() {
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
    if let Some(trace_markdown) = trace_hover_markdown(db, trace_service, r, resolution.first()) {
        info.push_str("\n---\n");
        info.push_str(&trace_markdown);
    }

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

fn trace_hover_markdown<'db>(
    db: &'db DriverDataBase,
    trace_service: Option<&TraceIntrospectionService>,
    reference: &ReferenceView<'db>,
    target: Option<&Target<'db>>,
) -> Option<String> {
    let trace_service = trace_service?;
    let Target::Local { .. } = target? else {
        return None;
    };
    let local = local_name_from_reference(db, reference)?;
    let report = trace_service
        .explain_local(ExplainLocalRequest {
            local: local.clone(),
            local_key: None,
        })
        .ok()?;
    let mut out = String::new();
    out.push_str("**Trace**\n\n");
    out.push_str(&format!(
        "data source: `{}`\n\n",
        report.metadata.data_source
    ));
    if !report.available {
        out.push_str(
            report
                .unavailable_reason
                .as_deref()
                .unwrap_or("trace facts do not include enough local attribution for this symbol"),
        );
        return Some(out);
    }
    if !report.storage_history.is_empty() {
        out.push_str("Storage history:\n");
        for storage in report.storage_history.iter().take(4) {
            out.push_str(&format!(
                "- {}: {} ({})\n",
                storage.phase, storage.location, storage.reason
            ));
        }
    }
    if !report.zero_extends.is_empty() {
        out.push_str(&format!(
            "\nZero-extends attributed to `{local}`: {}\n",
            report.zero_extends.len()
        ));
    }
    if let Ok(gas) = trace_service.gas_breakdown(GasBreakdownRequest::default())
        && gas.available
        && let Some(total) = gas.total_gas
    {
        out.push_str(&format!(
            "\nStatic gas estimate: `{total}` under `{}` schedule. This is opcode-table static gas, not runtime transaction gas.\n",
            gas.schedule
        ));
    }
    Some(out)
}
