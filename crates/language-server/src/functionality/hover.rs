use anyhow::Error;
use async_lsp::lsp_types::Hover;

use common::file::File;
use hir::{
    HirDb,
    analysis::ty::{
        ProviderAddressSpace,
        ty_check::{EffectParamSite, LocalBinding, ParamSite},
    },
    core::semantic::{
        ContractFieldId, ContractLayoutEntry, ContractLayoutEntryKind,
        ContractLayoutParameterOrigin, ContractLayoutValue, EffectEnvView, FieldView,
        ProviderSource,
        reference::{ReferenceView, Target},
    },
    hir_def::{Contract, FieldParent, ItemKind, PathId, scope_graph::ScopeId},
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
                        .find(|field| field.name(db) == Some(name))
                        .and_then(|field| field.contract_field_id(db))
                })
        }
        LocalBinding::EffectParam { site, idx, .. } => {
            let ProviderSource::ContractField { field } =
                effect_binding_provider_source_at_site(db, site, idx)?
            else {
                return None;
            };
            Some(field)
        }
        _ => None,
    }
}

fn address_space_heading(space: ProviderAddressSpace) -> &'static str {
    match space {
        ProviderAddressSpace::Memory => "Memory",
        ProviderAddressSpace::Storage => "Storage",
        ProviderAddressSpace::Transient => "Transient Storage",
        ProviderAddressSpace::Calldata => "Calldata",
        ProviderAddressSpace::Code => "Immutable (Code)",
    }
}

fn layout_entry_markdown(db: &DriverDataBase, entry: &ContractLayoutEntry<'_>) -> String {
    let (value, value_dimensions) = match &entry.value {
        ContractLayoutValue::Scalar(value) => (value.data(db).to_string(), Vec::new()),
        ContractLayoutValue::Indexed {
            base,
            dimensions,
            strides,
            ..
        } => {
            let mut terms = vec![base.data(db).to_string()];
            terms.extend(strides.iter().enumerate().map(|(index, stride)| {
                if *stride == 1 {
                    format!("i{index}")
                } else {
                    format!("i{index}*{stride}")
                }
            }));
            (terms.join(" + "), dimensions.clone())
        }
    };
    let mut dimensions = entry
        .path
        .index_dimensions()
        .map(|(index, len)| (index as usize, len))
        .collect::<Vec<_>>();
    if dimensions.is_empty() {
        dimensions.extend(value_dimensions.into_iter().enumerate());
    }
    let dimensions = (!dimensions.is_empty()).then(|| {
        format!(
            " ({})",
            dimensions
                .into_iter()
                .map(|(index, len)| format!("i{index}: 0..{len}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    });
    let kind = match entry.kind {
        ContractLayoutEntryKind::InlineField => "inline field",
        ContractLayoutEntryKind::EnumTag => "enum tag",
        ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Explicit) => {
            "explicit parameter"
        }
        ContractLayoutEntryKind::Parameter(ContractLayoutParameterOrigin::Inferred) => {
            "inferred parameter"
        }
    };
    format!(
        "- `{value}`{}: `{}` ({kind}, `{}`)",
        dimensions.unwrap_or_default(),
        entry.path.display(db),
        entry.ty.pretty_print(db),
    )
}

fn allocated_layout_markdown<'db>(
    db: &'db DriverDataBase,
    title: &str,
    entries: impl Iterator<Item = &'db ContractLayoutEntry<'db>>,
) -> String {
    let entries = entries.collect::<Vec<_>>();
    let mut markdown = format!("### {title}");
    for space in [
        ProviderAddressSpace::Storage,
        ProviderAddressSpace::Transient,
        ProviderAddressSpace::Code,
        ProviderAddressSpace::Memory,
        ProviderAddressSpace::Calldata,
    ] {
        let entries = entries
            .iter()
            .filter(|entry| entry.address_space == space)
            .copied()
            .collect::<Vec<_>>();
        if entries.is_empty() {
            continue;
        }
        markdown.push_str(&format!("\n\n#### {}\n\n", address_space_heading(space)));
        markdown.push_str(
            &entries
                .into_iter()
                .map(|entry| layout_entry_markdown(db, entry))
                .collect::<Vec<_>>()
                .join("\n"),
        );
    }
    if entries.is_empty() {
        markdown.push_str("\n\nNo address-space values are assigned.");
    }
    markdown
}

fn contract_field_layout_markdown<'db>(
    db: &'db DriverDataBase,
    field: ContractFieldId<'db>,
) -> Option<String> {
    let result = field.contract.storage_layout(db);
    if let Some(report) = field.contract.layout_report(db) {
        return Some(allocated_layout_markdown(
            db,
            "Field Layout",
            report.entries_for_field(field),
        ));
    }
    if let Some(errors) = result.field_errors_for_id(field) {
        return Some(format!(
            "### Field Layout\n\nInvalid: {}",
            errors
                .first()
                .map_or("unknown layout error", |error| error.summary())
        ));
    }
    Some(
        "### Field Layout\n\nUnavailable because another contract field has an invalid layout."
            .to_string(),
    )
}

fn contract_layout_markdown<'db>(db: &'db DriverDataBase, contract: Contract<'db>) -> String {
    let result = contract.storage_layout(db);
    if let Some(report) = contract.layout_report(db) {
        return allocated_layout_markdown(db, "Contract Layout", report.entries.iter());
    }
    let errors = result
        .field_results
        .iter()
        .filter_map(|field| {
            Some((
                field.name.data(db),
                field.result.as_ref().err()?.first()?.summary(),
            ))
        })
        .map(|(name, error)| format!("- `{name}`: {error}"))
        .collect::<Vec<_>>();
    if errors.is_empty() {
        "### Contract Layout\n\nUnavailable because the contract layout is invalid.".to_string()
    } else {
        format!(
            "### Contract Layout\n\nUnavailable because the contract has invalid field layouts:\n\n{}",
            errors.join("\n")
        )
    }
}

fn layout_markdown_for_target<'db>(
    db: &'db DriverDataBase,
    target: &Target<'db>,
) -> Option<String> {
    match target {
        Target::Scope(scope) => {
            if let Some(field) = contract_field_id_from_scope(db, *scope) {
                contract_field_layout_markdown(db, field)
            } else if let ItemKind::Contract(contract) = scope.item() {
                Some(contract_layout_markdown(db, contract))
            } else {
                None
            }
        }
        Target::Local { binding, .. } => contract_field_id_from_local_binding(db, *binding)
            .and_then(|field| contract_field_layout_markdown(db, field)),
    }
}

fn layout_definition_hover(
    db: &DriverDataBase,
    top_mod: hir::hir_def::TopLevelMod<'_>,
    cursor: Cursor,
) -> Option<Hover> {
    for item in top_mod.scope_graph(db).items_dfs(db) {
        let ItemKind::Contract(contract) = item else {
            continue;
        };
        if let Some(span) = contract
            .scope()
            .name_span(db)
            .and_then(|span| span.resolve(db))
            && span.range.contains(cursor)
        {
            return Some(Hover {
                contents: async_lsp::lsp_types::HoverContents::Markup(
                    async_lsp::lsp_types::MarkupContent {
                        kind: async_lsp::lsp_types::MarkupKind::Markdown,
                        value: contract_layout_markdown(db, contract),
                    },
                ),
                range: to_lsp_range_from_span(span, db).ok(),
            });
        }
        for field in FieldParent::Contract(contract).fields(db) {
            let scope = ScopeId::Field(FieldParent::Contract(contract), field.idx as u16);
            let Some(span) = scope.name_span(db).and_then(|span| span.resolve(db)) else {
                continue;
            };
            if !span.range.contains(cursor) {
                continue;
            }
            let field = contract_field_id_from_scope(db, scope)?;
            let markdown = contract_field_layout_markdown(db, field)?;
            return Some(Hover {
                contents: async_lsp::lsp_types::HoverContents::Markup(
                    async_lsp::lsp_types::MarkupContent {
                        kind: async_lsp::lsp_types::MarkupKind::Markdown,
                        value: markdown,
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

    if let Some(layout_markdown) = layout_markdown_for_target(db, target) {
        body.push('\n');
        body.push_str(&layout_markdown);
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
        return Ok((layout_definition_hover(db, top_mod, cursor), None));
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

#[cfg(test)]
mod tests {
    use async_lsp::lsp_types::{
        HoverContents, HoverParams, MarkedString, Position, TextDocumentIdentifier,
        TextDocumentPositionParams, WorkDoneProgressParams,
    };
    use common::InputDb;
    use dir_test::{Fixture, dir_test};
    use test_utils::{normalize::normalize_newlines, snap_test};
    use url::Url;

    use super::hover_helper;
    use driver::DriverDataBase;

    fn extract_hover_markers(source: &str) -> (String, Vec<(String, usize)>) {
        let mut cleaned = String::new();
        let mut markers = Vec::new();
        let mut remaining = source;
        while let Some(start) = remaining.find("<|") {
            cleaned.push_str(&remaining[..start]);
            let marker = &remaining[start + 2..];
            let end = marker.find("|>").expect("unterminated hover marker");
            let label = &marker[..end];
            assert!(!label.is_empty(), "hover markers must be named");
            markers.push((label.to_string(), cleaned.len()));
            remaining = &marker[end + 2..];
        }
        cleaned.push_str(remaining);
        (cleaned, markers)
    }

    fn lsp_position(source: &str, offset: usize) -> Position {
        let prefix = &source[..offset];
        Position::new(
            prefix.bytes().filter(|byte| *byte == b'\n').count() as u32,
            prefix
                .rsplit_once('\n')
                .map_or(prefix, |(_, line)| line)
                .encode_utf16()
                .count() as u32,
        )
    }

    fn hover_contents(contents: HoverContents) -> String {
        match contents {
            HoverContents::Markup(content) => content.value,
            HoverContents::Scalar(marked) => match marked {
                MarkedString::String(text) => text,
                MarkedString::LanguageString(text) => text.value,
            },
            HoverContents::Array(contents) => contents
                .into_iter()
                .map(|marked| match marked {
                    MarkedString::String(text) => text,
                    MarkedString::LanguageString(text) => text.value,
                })
                .collect::<Vec<_>>()
                .join("\n"),
        }
    }

    #[dir_test(
        dir: "$CARGO_MANIFEST_DIR/test_files",
        glob: "layout_hover.fe"
    )]
    fn layout_hover_snapshot(fixture: Fixture<&str>) {
        let source = normalize_newlines(fixture.content()).into_owned();
        let (source, markers) = extract_hover_markers(&source);
        let uri = Url::from_file_path(fixture.path()).unwrap();
        let mut db = DriverDataBase::default();
        let file = db
            .workspace()
            .touch(&mut db, uri.clone(), Some(source.clone()));
        let mut snapshot = String::new();
        for (label, offset) in markers {
            let (hover, _) = hover_helper(
                &db,
                file,
                HoverParams {
                    text_document_position_params: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier { uri: uri.clone() },
                        position: lsp_position(&source, offset),
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                },
            )
            .unwrap();
            let hover = hover.unwrap_or_else(|| panic!("missing hover for marker `{label}`"));
            snapshot.push_str(&format!("## {label}\n"));
            if let Some(range) = hover.range {
                snapshot.push_str(&format!(
                    "range: {}:{}..{}:{}\n\n",
                    range.start.line, range.start.character, range.end.line, range.end.character,
                ));
            }
            snapshot.push_str(&hover_contents(hover.contents));
            snapshot.push_str("\n\n---\n\n");
        }
        snap_test!(snapshot, fixture.path());
    }
}
