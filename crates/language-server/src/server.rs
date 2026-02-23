use crate::fallback::WithFallbackService;
use crate::functionality::handlers::{FileChange, FilesNeedDiagnostics, NeedsDiagnostics};
use crate::logging;
use crate::lsp_actor::LspActor;
use crate::lsp_actor::service::LspActorService;
use crate::lsp_streams::RouterStreams;
use act_locally::builder::ActorBuilder;
use async_lsp::ClientSocket;
use async_lsp::lsp_types::notification::{
    self, DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument, DidSaveTextDocument,
    Initialized,
};

use async_lsp::lsp_types::request::{
    CallHierarchyIncomingCalls, CallHierarchyOutgoingCalls, CallHierarchyPrepare,
    CodeActionRequest, CodeLensRequest, Completion, DocumentHighlightRequest,
    DocumentSymbolRequest, ExecuteCommand, FoldingRangeRequest, Formatting, GotoDeclaration,
    GotoDefinition, GotoImplementation, GotoTypeDefinition, HoverRequest, InlayHintRequest,
    References, Rename, SelectionRangeRequest, SemanticTokensFullRequest, Shutdown,
    SignatureHelpRequest, TypeHierarchyPrepare, TypeHierarchySubtypes, TypeHierarchySupertypes,
    WorkspaceSymbolRequest,
};
use async_std::stream::StreamExt;
use futures_batch::ChunksTimeoutStreamExt;
// use serde_json::Value;
use tracing::instrument::WithSubscriber;
use tracing::{info, warn};

use crate::backend::Backend;
use crate::functionality::{
    call_hierarchy, code_actions, code_lens, codegen_view, completion, declaration,
    document_symbols, folding_range, goto, handlers, highlight, implementations, inlay_hints,
    references, rename, selection_range, semantic_tokens, signature_help, type_definition,
    type_hierarchy, workspace_symbols,
};
use crate::ws_notify::WsBroadcast;
use async_lsp::lsp_types::request::Initialize;
use async_lsp::router::Router;

pub(crate) fn setup(
    client: ClientSocket,
    name: String,
    ws_broadcast: Option<WsBroadcast>,
) -> WithFallbackService<LspActorService<Backend>, Router<()>> {
    info!("Setting up server");
    let client_for_actor = client.clone();
    let client_for_logging = client.clone();
    let backend_actor = ActorBuilder::new()
        .with_name(name)
        .with_state_init(move || Ok(Backend::new(client_for_actor, ws_broadcast)))
        .with_subscriber_init(logging::init_fn(client_for_logging))
        .spawn()
        .expect("Failed to spawn backend actor");

    let mut lsp_actor_service = LspActorService::with(backend_actor);

    lsp_actor_service
        // mutating handlers
        .handle_request_mut::<Initialize>(handlers::initialize)
        .handle_request_mut::<GotoDefinition>(goto::handle_goto_definition)
        .handle_event_mut::<FileChange>(handlers::handle_file_change)
        .handle_event::<FilesNeedDiagnostics>(handlers::handle_files_need_diagnostics)
        // non-mutating handlers
        .handle_notification::<Initialized>(handlers::initialized)
        .handle_request::<HoverRequest>(handlers::handle_hover_request)
        .handle_request::<Completion>(completion::handle_completion)
        .handle_request::<SignatureHelpRequest>(signature_help::handle_signature_help)
        .handle_request::<CodeActionRequest>(code_actions::handle_code_action)
        .handle_request::<References>(references::handle_references)
        .handle_request::<DocumentHighlightRequest>(highlight::handle_document_highlight)
        .handle_request::<GotoTypeDefinition>(type_definition::handle_goto_type_definition)
        .handle_request::<GotoImplementation>(implementations::handle_goto_implementation)
        .handle_request::<Rename>(rename::handle_rename)
        .handle_request::<SemanticTokensFullRequest>(semantic_tokens::handle_semantic_tokens_full)
        .handle_request::<Formatting>(handlers::handle_formatting)
        .handle_request::<InlayHintRequest>(inlay_hints::handle_inlay_hints)
        .handle_request::<DocumentSymbolRequest>(document_symbols::handle_document_symbols)
        .handle_request::<WorkspaceSymbolRequest>(workspace_symbols::handle_workspace_symbols)
        // call hierarchy
        .handle_request::<CallHierarchyPrepare>(call_hierarchy::handle_prepare)
        .handle_request::<CallHierarchyIncomingCalls>(call_hierarchy::handle_incoming_calls)
        .handle_request::<CallHierarchyOutgoingCalls>(call_hierarchy::handle_outgoing_calls)
        // type hierarchy
        .handle_request::<TypeHierarchyPrepare>(type_hierarchy::handle_prepare)
        .handle_request::<TypeHierarchySupertypes>(type_hierarchy::handle_supertypes)
        .handle_request::<TypeHierarchySubtypes>(type_hierarchy::handle_subtypes)
        // code lens
        .handle_request::<CodeLensRequest>(code_lens::handle_code_lens)
        // execute command (codegen views)
        .handle_request_mut::<ExecuteCommand>(codegen_view::handle_execute_command)
        // selection range
        .handle_request::<SelectionRangeRequest>(selection_range::handle_selection_range)
        // folding range
        .handle_request::<FoldingRangeRequest>(folding_range::handle_folding_range)
        // go to declaration
        .handle_request::<GotoDeclaration>(declaration::handle_goto_declaration)
        .handle_notification::<DidOpenTextDocument>(handlers::handle_did_open_text_document)
        .handle_notification::<DidChangeTextDocument>(handlers::handle_did_change_text_document)
        .handle_notification::<DidChangeWatchedFiles>(handlers::handle_did_change_watched_files)
        .handle_notification::<DidSaveTextDocument>(handlers::handle_did_save_text_document)
        .handle_notification::<notification::Exit>(handlers::handle_exit)
        .handle_request::<Shutdown>(handlers::handle_shutdown);

    let mut streaming_router = Router::new(());
    setup_streams(client.clone(), &mut streaming_router);
    setup_unhandled(&mut streaming_router);

    WithFallbackService::new(lsp_actor_service, streaming_router)
}

fn setup_streams(client: ClientSocket, router: &mut Router<()>) {
    info!("setting up streams");

    let mut diagnostics_stream = router
        .event_stream::<NeedsDiagnostics>()
        .chunks_timeout(500, std::time::Duration::from_millis(30))
        .map(FilesNeedDiagnostics)
        .fuse();

    tokio::spawn(
        async move {
            while let Some(files_need_diagnostics) = diagnostics_stream.next().await {
                let _ = client.emit(files_need_diagnostics);
            }
        }
        .with_current_subscriber(),
    );
}

fn setup_unhandled(router: &mut Router<()>) {
    router
        .unhandled_notification(|_, params| {
            warn!("Unhandled notification: {:?}", params);
            std::ops::ControlFlow::Continue(())
        })
        .unhandled_event(|_, params| {
            warn!("Unhandled event: {:?}", params);
            std::ops::ControlFlow::Continue(())
        });
}
