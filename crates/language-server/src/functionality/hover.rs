use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{
    HirDb,
    analysis::ty::ty_check::{EffectParamSite, LocalBinding},
    core::semantic::reference::{ReferenceView, Target},
    core::semantic::{ContractFieldInfo, EffectSource},
    hir_def::{FieldDef, FieldParent, scope_graph::ScopeId},
    lower::map_file_to_mod,
    span::LazySpan,
};
use tracing::info;

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

fn effect_binding_source_at_site<'db>(
    db: &'db DriverDataBase,
    site: EffectParamSite<'db>,
    idx: usize,
) -> Option<EffectSource<'db>> {
    match site {
        EffectParamSite::Func(func) => func
            .effect_bindings(db)
            .iter()
            .find(|binding| binding.binding_idx as usize == idx)
            .map(|binding| binding.source),
        EffectParamSite::Contract(contract) => contract
            .effect_bindings(db)
            .iter()
            .find(|binding| binding.binding_idx as usize == idx)
            .map(|binding| binding.source),
        EffectParamSite::ContractInit { contract } => contract
            .init_effect_bindings(db)
            .iter()
            .find(|binding| binding.binding_idx as usize == idx)
            .map(|binding| binding.source),
        EffectParamSite::ContractRecvArm {
            contract,
            recv_idx,
            arm_idx,
        } => {
            let recv = contract.recv(db, recv_idx)?;
            let arm = recv.arm(db, arm_idx)?;
            arm.effective_effect_bindings(db)
                .iter()
                .find(|binding| binding.binding_idx as usize == idx)
                .map(|binding| binding.source)
        }
    }
}

fn contract_field_layout_by_index<'db>(
    db: &'db DriverDataBase,
    contract: hir::hir_def::Contract<'db>,
    field_idx: u32,
) -> Option<ContractFieldInfo<'db>> {
    contract
        .field_layout(db)
        .values()
        .find(|field| field.index == field_idx)
        .copied()
}

fn contract_field_layout_from_scope<'db>(
    db: &'db DriverDataBase,
    scope: ScopeId<'db>,
) -> Option<ContractFieldInfo<'db>> {
    let ScopeId::Field(FieldParent::Contract(contract), idx) = scope else {
        return None;
    };

    if let Some(name) = scope.name(db)
        && let Some(field) = contract.field_layout(db).get(&name)
    {
        return Some(*field);
    }

    contract_field_layout_by_index(db, contract, idx as u32)
}

fn contract_field_layout_from_local_binding<'db>(
    db: &'db DriverDataBase,
    binding: LocalBinding<'db>,
) -> Option<ContractFieldInfo<'db>> {
    match binding {
        LocalBinding::EffectParam { site, idx, .. } => {
            let EffectSource::Field(field) = effect_binding_source_at_site(db, site, idx)? else {
                return None;
            };
            Some(field.field)
        }
        LocalBinding::ContractField {
            contract,
            field_idx,
            ..
        } => contract_field_layout_by_index(db, contract, field_idx),
        _ => None,
    }
}

fn contract_field_layout_footer_from_info<'db>(
    db: &'db DriverDataBase,
    field: ContractFieldInfo<'db>,
) -> Option<String> {
    match field.kind {
        hir::semantic::ContractFieldKind::MutableStorage => {
            let layout = field.storage_layout()?;
            Some(format!(
                "slot: {} (count: {})\nspace: {}",
                layout.slot_offset,
                layout.slot_count,
                field.address_space.pretty_print(db)
            ))
        }
        hir::semantic::ContractFieldKind::ImmutableCode => {
            let layout = field.immutable_layout()?;
            Some(format!(
                "bytes: {byte_offset}..{byte_end} (len: {byte_len})\nspace: {space}",
                byte_offset = layout.byte_offset,
                byte_end = layout.byte_offset + layout.byte_len,
                byte_len = layout.byte_len,
                space = field.address_space.pretty_print(db)
            ))
        }
    }
}

fn contract_field_layout_footer<'db>(
    db: &'db DriverDataBase,
    target: &Target<'db>,
) -> Option<String> {
    let field = match target {
        Target::Scope(scope) => contract_field_layout_from_scope(db, *scope)?,
        Target::Local { binding, .. } => contract_field_layout_from_local_binding(db, *binding)?,
    };
    contract_field_layout_footer_from_info(db, field)
}

fn contract_field_markdown<'db>(db: &'db DriverDataBase, scope: ScopeId<'db>) -> Option<String> {
    let field: &FieldDef<'db> = scope.resolve_to(db)?;
    let path = scope.pretty_path(db)?;
    let name = field.name.to_opt()?.data(db);
    let ty = field.type_ref().to_opt()?.pretty_print(db);
    let vis = field.vis.pretty_print();
    let mut_prefix = if field.is_mut { "mut " } else { "" };

    Some(format!(
        "```fe\n{path}\n```\n\n```fe\n{vis}{mut_prefix}{name}: {ty}\n```"
    ))
}

fn hover_markdown_for_target<'db>(
    db: &'db DriverDataBase,
    reference: &ReferenceView<'db>,
    target: &Target<'db>,
) -> Option<String> {
    let mut body = match target {
        Target::Scope(scope @ ScopeId::Field(FieldParent::Contract(_), _)) => {
            let docs = get_docstring(db, *scope);
            [contract_field_markdown(db, *scope), docs]
                .iter()
                .filter_map(|info| info.clone().map(|info| format!("{info}\n")))
                .collect::<Vec<String>>()
                .join("\n")
        }
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
    info!("handling hover");
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
