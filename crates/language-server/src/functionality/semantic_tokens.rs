use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensResult,
    SemanticTokensServerCapabilities,
};
use common::InputDb;
use hir::{
    core::semantic::reference::{HasReferences, Target},
    hir_def::ItemKind,
    lower::map_file_to_mod,
    span::LazySpan,
};

use crate::{backend::Backend, util::calculate_line_offsets};

/// Supported semantic token types
pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::NAMESPACE,   // 0 - modules
    SemanticTokenType::TYPE,        // 1 - structs, enums
    SemanticTokenType::FUNCTION,    // 2 - functions
    SemanticTokenType::VARIABLE,    // 3 - local variables
    SemanticTokenType::PARAMETER,   // 4 - function parameters
    SemanticTokenType::PROPERTY,    // 5 - struct fields
    SemanticTokenType::ENUM_MEMBER, // 6 - enum variants
];

/// Supported semantic token modifiers
pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DEFINITION,  // 0 - at definition site
    SemanticTokenModifier::DECLARATION, // 1 - at declaration site
];

/// Create the semantic tokens capability
pub fn semantic_tokens_options() -> SemanticTokensServerCapabilities {
    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        legend: SemanticTokensLegend {
            token_types: TOKEN_TYPES.to_vec(),
            token_modifiers: TOKEN_MODIFIERS.to_vec(),
        },
        full: Some(SemanticTokensFullOptions::Bool(true)),
        range: None,
        work_done_progress_options: Default::default(),
    })
}

/// Map an item kind to a semantic token type index
fn item_kind_to_token_type(item: ItemKind) -> Option<u32> {
    match item {
        ItemKind::TopMod(_) | ItemKind::Mod(_) => Some(0), // namespace
        ItemKind::Struct(_) | ItemKind::Enum(_) | ItemKind::TypeAlias(_) => Some(1), // type
        ItemKind::Func(_) => Some(2),                      // function
        ItemKind::Const(_) => Some(3),                     // variable (constants are variables)
        ItemKind::Trait(_) | ItemKind::Impl(_) | ItemKind::ImplTrait(_) => Some(1), // type
        ItemKind::Contract(_) => Some(1),                  // type
        _ => None,
    }
}

pub async fn handle_semantic_tokens_full(
    backend: &Backend,
    params: async_lsp::lsp_types::SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>, ResponseError> {
    let path_str = params.text_document.uri.path();

    let Ok(url) = url::Url::from_file_path(path_str) else {
        return Ok(None);
    };

    let Some(file) = backend.db.workspace().get(&backend.db, &url) else {
        return Ok(None);
    };

    let file_text = file.text(&backend.db);
    let line_offsets = calculate_line_offsets(file_text.as_str());

    let top_mod = map_file_to_mod(&backend.db, file);

    let mut tokens: Vec<(u32, u32, u32, u32, u32)> = vec![]; // (line, col, length, type, modifiers)

    // Collect tokens from all items in the module
    let scope_graph = top_mod.scope_graph(&backend.db);
    for item in scope_graph.items_dfs(&backend.db) {
        for reference in item.references(&backend.db) {
            // Get the span of the reference
            let Some(span) = reference.span().resolve(&backend.db) else {
                continue;
            };

            // Only include tokens from this file
            if span.file.url(&backend.db) != Some(url.clone()) {
                continue;
            }

            // Determine the token type from the resolved target
            let token_type = match reference.target(&backend.db).first() {
                Some(Target::Scope(scope)) => item_kind_to_token_type(scope.item()),
                Some(Target::Local { .. }) => Some(3), // variable
                None => None,
            };

            let Some(token_type) = token_type else {
                continue;
            };

            // Calculate line and column from offset
            let start_offset: usize = span.range.start().into();
            let end_offset: usize = span.range.end().into();
            let length = (end_offset - start_offset) as u32;

            // Find the line containing this offset
            let line = line_offsets
                .binary_search(&start_offset)
                .unwrap_or_else(|x| x.saturating_sub(1));
            let col = start_offset - line_offsets.get(line).copied().unwrap_or(0);

            tokens.push((line as u32, col as u32, length, token_type, 0));
        }
    }

    // Sort tokens by position
    tokens.sort_by_key(|(line, col, _, _, _)| (*line, *col));

    // Convert to delta-encoded format
    let mut result = vec![];
    let mut prev_line = 0u32;
    let mut prev_col = 0u32;

    for (line, col, length, token_type, modifiers) in tokens {
        let delta_line = line - prev_line;
        let delta_col = if delta_line == 0 { col - prev_col } else { col };

        result.push(SemanticToken {
            delta_line,
            delta_start: delta_col,
            length,
            token_type,
            token_modifiers_bitset: modifiers,
        });

        prev_line = line;
        prev_col = col;
    }

    Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: result,
    })))
}
