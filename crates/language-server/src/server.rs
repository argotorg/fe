use crate::fallback::WithFallbackService;
use crate::functionality::handlers::{DocNavigate, FileChange, FilesNeedDiagnostics, NeedsDiagnostics};
use crate::logging;
use crate::lsp_actor::LspActor;
use crate::lsp_actor::service::LspActorService;
use crate::lsp_streams::{RouterStreams, StreamPipeExt};
use act_locally::builder::ActorBuilder;
use async_lsp::ClientSocket;
use async_lsp::lsp_types::notification::{
    self, DidChangeTextDocument, DidChangeWatchedFiles, DidOpenTextDocument, DidSaveTextDocument,
    Initialized,
};
use async_lsp::lsp_types::request::{
    CodeActionRequest, Completion, DocumentHighlightRequest, DocumentSymbolRequest,
    ExecuteCommand, GotoDefinition, GotoImplementation, GotoTypeDefinition, HoverRequest,
    InlayHintRequest, References, Rename, SemanticTokensFullRequest, Shutdown,
    SignatureHelpRequest, WorkspaceSymbolRequest,
};
use futures::StreamExt;
use futures_batch::ChunksTimeoutStreamExt;
use tracing::{info, warn};

use crate::backend::Backend;
use crate::functionality::{
    code_actions, completion, document_symbols, goto, handlers, highlight, implementations,
    inlay_hints, references, rename, semantic_tokens, signature_help, type_definition,
    workspace_symbols,
};
use async_lsp::lsp_types::request::Initialize;
use async_lsp::router::Router;

pub(crate) fn setup(
    client: ClientSocket,
    name: String,
) -> WithFallbackService<LspActorService<Backend>, Router<()>> {
    info!("Setting up server");
    let client_for_actor = client.clone();
    let client_for_logging = client.clone();
    let backend_actor = ActorBuilder::new()
        .with_name(name)
        .with_state_init(move || Ok(Backend::new(client_for_actor)))
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
        .handle_event_mut::<DocNavigate>(handlers::handle_doc_navigate)
        // doc server starts on initialization
        .handle_notification_mut::<Initialized>(handlers::initialized)
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
        .handle_request::<InlayHintRequest>(inlay_hints::handle_inlay_hints)
        .handle_request::<DocumentSymbolRequest>(document_symbols::handle_document_symbols)
        .handle_request::<WorkspaceSymbolRequest>(workspace_symbols::handle_workspace_symbols)
        .handle_notification::<DidOpenTextDocument>(handlers::handle_did_open_text_document)
        .handle_notification::<DidChangeTextDocument>(handlers::handle_did_change_text_document)
        .handle_notification::<DidChangeWatchedFiles>(handlers::handle_did_change_watched_files)
        .handle_notification::<DidSaveTextDocument>(handlers::handle_did_save_text_document)
        .handle_notification::<notification::Exit>(handlers::handle_exit)
        .handle_request::<Shutdown>(handlers::handle_shutdown)
        .handle_request_mut::<ExecuteCommand>(handlers::handle_execute_command);

    let mut streaming_router = Router::new(());
    setup_streams(client.clone(), &mut streaming_router);
    setup_unhandled(&mut streaming_router);

    WithFallbackService::new(lsp_actor_service, streaming_router)
}

fn setup_streams(client: ClientSocket, router: &mut Router<()>) {
    info!("setting up streams");

    // Batch diagnostics events and re-emit as FilesNeedDiagnostics
    router
        .event_stream::<NeedsDiagnostics>()
        .chunks_timeout(500, std::time::Duration::from_millis(30))
        .map(FilesNeedDiagnostics)
        .pipe_emit(client);
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
