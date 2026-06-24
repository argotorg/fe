//! Integration tests using a mock LSP client connected to the real server.
//!
//! These tests exercise the full pipeline: actor → diagnostics batching →
//! request handling, verifying the server survives malformed edits.

use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::notification::{LogMessage, PublishDiagnostics};
use async_lsp::lsp_types::*;
use async_lsp::server::LifecycleLayer;
use async_lsp::{LanguageServer, MainLoop};
use futures::AsyncReadExt;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio_util::compat::TokioAsyncReadCompatExt;
use tower::ServiceBuilder;
use tracing::instrument::WithSubscriber;

use crate::server::setup;

const DUPLEX_BUF: usize = 64 << 10;

// ---------------------------------------------------------------------------
// Fixture helpers
// ---------------------------------------------------------------------------

fn fixture_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("test_files")
        .join("single_ingot")
}

fn fixture_url() -> Url {
    Url::from_directory_path(fixture_path()).unwrap()
}

fn lib_url() -> Url {
    Url::from_file_path(fixture_path().join("src").join("lib.fe")).unwrap()
}

// ---------------------------------------------------------------------------
// MockLspClient — a test harness for driving the real fe language server
// ---------------------------------------------------------------------------

/// Collected diagnostics for a single URI publish event.
#[derive(Debug, Clone)]
pub struct PublishedDiagnostics {
    pub uri: Url,
    pub diagnostics: Vec<Diagnostic>,
}

/// A mock LSP client connected to the real fe language server via duplex pipe.
///
/// Use `MockLspClient::start()` to spin up, then call helper methods to drive
/// the protocol. Diagnostics and server log messages are collected automatically
/// for inspection.
pub struct MockLspClient {
    server: async_lsp::ServerSocket,
    diagnostics: Arc<Mutex<Vec<PublishedDiagnostics>>>,
    log_messages: Arc<Mutex<Vec<LogMessageParams>>>,
    _srv_handle: tokio::task::JoinHandle<()>,
    _cli_handle: tokio::task::JoinHandle<()>,
    // Holds the tracing subscriber default-guard installed for this test. Dropped
    // when the client is dropped, which resets the thread-local dispatch.
    _logging_guard: Option<tracing::subscriber::DefaultGuard>,
}

impl MockLspClient {
    /// Spawn the real server and connect a mock client.
    pub async fn start() -> Self {
        let (server_main, server_client_socket) = MainLoop::new_server(|client| {
            let lsp_service = setup(client.clone(), "test-actor".to_string());
            ServiceBuilder::new()
                .layer(LifecycleLayer::default())
                .layer(ConcurrencyLayer::default())
                .service(lsp_service)
        });

        // Install the production tracing subscriber on the current (test) thread
        // using the server's ClientSocket. This routes `tracing::warn!` /
        // `tracing::info!` calls from the LSP main loop (including
        // `setup_unhandled`) through `ClientSocketWriter`, which emits them as
        // `window/logMessage` notifications the mock client captures below.
        //
        // `set_default` is thread-local; `#[tokio::test]` uses a current-thread
        // runtime by default, and we use `.with_current_subscriber()` on the
        // spawned server task below to ensure the dispatch follows the future.
        let logging_guard = crate::logging::setup_default_subscriber(server_client_socket);

        let (server_stream, client_stream) = tokio::io::duplex(DUPLEX_BUF);
        let (srv_rx, srv_tx) = server_stream.compat().split();
        let srv_handle = tokio::spawn(
            async move {
                if let Err(e) = server_main.run_buffered(srv_rx, srv_tx).await {
                    tracing::debug!("test server loop exited: {e:?}");
                }
            }
            .with_current_subscriber(),
        );

        let diagnostics: Arc<Mutex<Vec<PublishedDiagnostics>>> = Arc::new(Mutex::new(Vec::new()));
        let diag_collector = diagnostics.clone();

        let log_messages: Arc<Mutex<Vec<LogMessageParams>>> = Arc::new(Mutex::new(Vec::new()));
        let log_collector = log_messages.clone();

        let (client_main, server_socket) = MainLoop::new_client(move |_| {
            let diags = diag_collector.clone();
            let logs = log_collector.clone();
            let mut router = async_lsp::router::Router::new(());
            router
                .notification::<PublishDiagnostics>(move |_, params| {
                    diags.lock().unwrap().push(PublishedDiagnostics {
                        uri: params.uri,
                        diagnostics: params.diagnostics,
                    });
                    ControlFlow::Continue(())
                })
                // Capture server-side tracing output delivered via window/logMessage
                // so tests can assert on the log levels and messages the server emits.
                .notification::<LogMessage>(move |_, params| {
                    logs.lock().unwrap().push(params);
                    ControlFlow::Continue(())
                })
                // Silently absorb any other server notifications
                .unhandled_notification(|_, _| ControlFlow::Continue(()));
            ServiceBuilder::new().service(router)
        });
        let (cli_rx, cli_tx) = client_stream.compat().split();
        let cli_handle = tokio::spawn(async move {
            let _ = client_main.run_buffered(cli_rx, cli_tx).await;
        });

        Self {
            server: server_socket,
            diagnostics,
            log_messages,
            _srv_handle: srv_handle,
            _cli_handle: cli_handle,
            _logging_guard: logging_guard,
        }
    }

    /// Run the initialize + initialized handshake with the test fixture workspace.
    pub async fn initialize(&mut self) {
        self.server
            .initialize(InitializeParams {
                workspace_folders: Some(vec![WorkspaceFolder {
                    uri: fixture_url(),
                    name: "test".into(),
                }]),
                ..Default::default()
            })
            .await
            .expect("initialize failed");
        self.server
            .initialized(InitializedParams {})
            .expect("initialized failed");
        // Give the actor time to load ingot files
        self.settle(500).await;
    }

    /// Open a text document.
    pub fn did_open(&mut self, uri: &Url, text: &str) {
        self.server
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "fe".into(),
                    version: 1,
                    text: text.into(),
                },
            })
            .expect("didOpen failed");
    }

    /// Send a full-content text change.
    pub fn did_change(&mut self, uri: &Url, version: i32, text: &str) {
        self.server
            .did_change(DidChangeTextDocumentParams {
                text_document: VersionedTextDocumentIdentifier {
                    uri: uri.clone(),
                    version,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: text.into(),
                }],
            })
            .expect("didChange failed");
    }

    /// Request formatting and return the result.
    pub async fn format(&mut self, uri: &Url) -> Result<Option<Vec<TextEdit>>, async_lsp::Error> {
        self.server
            .formatting(DocumentFormattingParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                options: FormattingOptions {
                    tab_size: 4,
                    insert_spaces: true,
                    ..Default::default()
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
    }

    /// Request hover.
    pub async fn hover(
        &mut self,
        uri: &Url,
        line: u32,
        character: u32,
    ) -> Result<Option<Hover>, async_lsp::Error> {
        self.server
            .hover(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position { line, character },
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
    }

    /// Return a snapshot of all diagnostics received so far.
    pub fn diagnostics(&self) -> Vec<PublishedDiagnostics> {
        self.diagnostics.lock().unwrap().clone()
    }

    /// Clear collected diagnostics.
    pub fn clear_diagnostics(&self) {
        self.diagnostics.lock().unwrap().clear();
    }

    /// Return a snapshot of all `window/logMessage` notifications received so far.
    #[allow(dead_code)]
    pub fn log_messages(&self) -> Vec<LogMessageParams> {
        self.log_messages.lock().unwrap().clone()
    }

    /// Return captured log messages filtered by MessageType.
    pub fn log_messages_of_type(&self, typ: MessageType) -> Vec<LogMessageParams> {
        self.log_messages
            .lock()
            .unwrap()
            .iter()
            .filter(|m| m.typ == typ)
            .cloned()
            .collect()
    }

    /// Clear captured log messages.
    pub fn clear_log_messages(&self) {
        self.log_messages.lock().unwrap().clear();
    }

    /// Wait for the server to settle (process queued events).
    pub async fn settle(&self, ms: u64) {
        tokio::time::sleep(Duration::from_millis(ms)).await;
    }

    /// Request goto definition.
    pub async fn goto_definition(
        &mut self,
        uri: &Url,
        line: u32,
        character: u32,
    ) -> Result<Option<GotoDefinitionResponse>, async_lsp::Error> {
        self.server
            .definition(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position { line, character },
                },
                partial_result_params: PartialResultParams::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
    }

    /// Request completion.
    pub async fn completion(
        &mut self,
        uri: &Url,
        line: u32,
        character: u32,
    ) -> Result<Option<CompletionResponse>, async_lsp::Error> {
        self.server
            .completion(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position { line, character },
                },
                context: None,
                partial_result_params: PartialResultParams::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
    }

    /// Request document symbols.
    pub async fn document_symbols(
        &mut self,
        uri: &Url,
    ) -> Result<Option<DocumentSymbolResponse>, async_lsp::Error> {
        self.server
            .document_symbol(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                partial_result_params: PartialResultParams::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
    }

    /// Request references.
    pub async fn references(
        &mut self,
        uri: &Url,
        line: u32,
        character: u32,
    ) -> Result<Option<Vec<Location>>, async_lsp::Error> {
        self.server
            .references(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position { line, character },
                },
                context: ReferenceContext {
                    include_declaration: true,
                },
                partial_result_params: PartialResultParams::default(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
    }

    /// Replay a sequence of edits with settle time between each.
    /// Each entry is the full file content at that point in time.
    pub async fn replay_edits(&mut self, uri: &Url, edits: &[&str], settle_ms: u64) {
        for (i, text) in edits.iter().enumerate() {
            self.did_change(uri, i as i32 + 2, text);
            if settle_ms > 0 {
                self.settle(settle_ms).await;
            }
        }
    }

    /// Replay edits without settling between them (simulates fast typing).
    pub fn replay_edits_burst(&mut self, uri: &Url, edits: &[&str]) {
        for (i, text) in edits.iter().enumerate() {
            self.did_change(uri, i as i32 + 2, text);
        }
    }

    /// Poll until a diagnostic with the given code appears for the URI, or give up.
    pub async fn wait_for_diagnostic_code(&self, uri: &Url, code: &str) -> bool {
        for _ in 0..20 {
            self.settle(500).await;
            let found = self
                .diagnostics()
                .iter()
                .filter(|d| &d.uri == uri)
                .flat_map(|d| &d.diagnostics)
                .any(|d| match &d.code {
                    Some(NumberOrString::String(s)) => s == code,
                    _ => false,
                });
            if found {
                return true;
            }
        }
        false
    }

    /// Return all diagnostics for a specific URI from the latest publish.
    pub fn diagnostics_for_uri(&self, uri: &Url) -> Vec<Diagnostic> {
        self.diagnostics()
            .into_iter()
            .filter(|d| &d.uri == uri)
            .flat_map(|d| d.diagnostics)
            .collect()
    }

    /// Shut down the server cleanly.
    pub async fn shutdown(mut self) {
        let _ = self.server.shutdown(()).await;
        let _ = self.server.exit(());
    }
}

// ---------------------------------------------------------------------------
// Tests — all scenarios share one server to avoid repeated cold salsa costs.
//
// Each `diagnostics_for_ingot()` call takes 1-7s on a salsa cache miss (full
// analysis pipeline). With 7 independent tests each spinning up their own
// server, the suite took ~160s. Sharing one server lets the first scenario
// warm the cache; subsequent scenarios hit it in ~5ms.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn mock_lsp_scenarios() {
    let mut client = MockLspClient::start().await;
    client.initialize().await;

    let uri = lib_url();
    client.did_open(&uri, "");
    client.settle(500).await;

    scenario_survives_malformed_edits_and_formats(&mut client, &uri).await;
    scenario_self_to_mut_self_keystrokes(&mut client, &uri).await;
    scenario_features_work_after_malformed_edits(&mut client, &uri).await;
    scenario_format_during_malformed_intermediate_states(&mut client, &uri).await;
    scenario_format_concurrent_with_diagnostics(&mut client, &uri).await;
    scenario_format_during_generic_struct_keystroke_sequence(&mut client, &uri).await;
    scenario_diagnostics_published_for_broken_code(&mut client).await;
    scenario_arithmetic_attr_reports_errors(&mut client, &uri).await;
    scenario_contract_analysis_reports_errors(&mut client, &uri).await;
    scenario_payable_attr_reports_errors(&mut client, &uri).await;
    scenario_errors_reported_after_panic_recovery(&mut client, &uri).await;
    scenario_unhandled_notifications_do_not_warn(&mut client).await;

    client.shutdown().await;
}

/// Send malformed edits via replay_edits_burst, then verify formatting works.
async fn scenario_survives_malformed_edits_and_formats(client: &mut MockLspClient, uri: &Url) {
    let valid = "struct Foo { x: u256 }";
    client.did_change(uri, 100, valid);
    client.settle(500).await;

    // Burst of malformed edits — no settling between them
    client.replay_edits_burst(
        uri,
        &[
            "struct Foo { x: }",
            "struct",
            "",
            "}{}{}{",
            "fn (",
            "impl { fn set(mself) }",
            "struct S<T, const N:",
        ],
    );

    client.settle(1000).await;

    // Restore valid text and verify formatting still works
    client.clear_diagnostics();
    client.did_change(uri, 110, valid);
    client.settle(500).await;

    let result = client.format(uri).await;
    assert!(
        result.is_ok(),
        "formatting should succeed after malformed edits, got: {result:?}",
    );
}

/// Verify the server survives the `self` -> `mut self` keystroke sequence.
async fn scenario_self_to_mut_self_keystrokes(client: &mut MockLspClient, uri: &Url) {
    let template = |param: &str| {
        format!(
            "struct Foo {{ x: u256 }}\nimpl Foo {{\n    fn set({param}, val: u256) {{\n        self.x = val\n    }}\n}}"
        )
    };

    client.did_change(uri, 200, &template("self"));
    client.settle(500).await;

    let edits: Vec<String> = ["mself", "muself", "mutself", "mut self"]
        .iter()
        .map(|s| template(s))
        .collect();
    let edit_refs: Vec<&str> = edits.iter().map(|s| s.as_str()).collect();
    client.replay_edits(uri, &edit_refs, 100).await;

    client.settle(1000).await;

    let result = client.format(uri).await;
    assert!(result.is_ok(), "server should survive keystroke sequence");
}

/// Verify hover, goto, and completion all work after broken edits.
async fn scenario_features_work_after_malformed_edits(client: &mut MockLspClient, uri: &Url) {
    let code = "struct Foo {\n    x: u256\n}\nfn bar() -> Foo {\n    return Foo(x: 1)\n}";
    client.did_change(uri, 300, code);
    client.settle(500).await;

    // Break it, then fix it
    client.replay_edits_burst(uri, &["}{}{", ""]);
    client.settle(500).await;
    client.did_change(uri, 310, code);
    client.settle(500).await;

    // Hover on "Foo" in the return type (line 3, char 13)
    let hover_result = client.hover(uri, 3, 13).await;
    assert!(hover_result.is_ok(), "hover should work after recovery");

    // Goto definition on "Foo" in the return type
    let goto_result = client.goto_definition(uri, 3, 13).await;
    assert!(goto_result.is_ok(), "goto should work after recovery");

    // Completion at end of "return Foo(" (line 4)
    let comp_result = client.completion(uri, 4, 15).await;
    assert!(comp_result.is_ok(), "completion should work after recovery");

    // Document symbols
    let syms_result = client.document_symbols(uri).await;
    assert!(
        syms_result.is_ok(),
        "document_symbols should work after recovery"
    );

    // References on "Foo" (line 0, char 7)
    let refs_result = client.references(uri, 0, 7).await;
    assert!(refs_result.is_ok(), "references should work after recovery");
}

/// Format during malformed intermediate states (reproduces Sean's crash).
async fn scenario_format_during_malformed_intermediate_states(
    client: &mut MockLspClient,
    uri: &Url,
) {
    let template = |param: &str| {
        format!(
            "struct Foo {{ x: u256 }}\nimpl Foo {{\n    fn set({param}, val: u256) {{\n        self.x = val\n    }}\n}}"
        )
    };

    client.did_change(uri, 400, &template("self"));
    client.settle(500).await;

    // Simulate typing while formatting is triggered at each step
    let intermediates = ["mself", "muself", "mutself", "mut self"];
    for (i, param) in intermediates.iter().enumerate() {
        client.did_change(uri, 401 + i as i32, &template(param));
        let fmt_result = client.format(uri).await;
        assert!(
            fmt_result.is_ok(),
            "formatting should not crash on intermediate state '{param}', got: {fmt_result:?}"
        );
    }

    // Also test partial generic definitions with format interleaved
    let generic_steps = [
        "struct S<",
        "struct S<T,",
        "struct S<T, const",
        "struct S<T, const N:",
    ];
    for (i, code) in generic_steps.iter().enumerate() {
        client.did_change(uri, 410 + i as i32, code);
        let fmt_result = client.format(uri).await;
        assert!(
            fmt_result.is_ok(),
            "formatting should not crash on partial generic '{code}', got: {fmt_result:?}"
        );
    }
}

/// Format races with diagnostics computation (format-on-save scenario).
async fn scenario_format_concurrent_with_diagnostics(client: &mut MockLspClient, uri: &Url) {
    client.did_change(uri, 500, "struct Foo { x: u256 }");
    client.settle(500).await;

    // Send broken edits and IMMEDIATELY format without settling —
    // diagnostics are still being computed when format arrives
    for i in 0..5i32 {
        let broken_code = match i % 5 {
            0 => "struct Foo { x: }",
            1 => "}{}{}{",
            2 => "impl { fn set(mself) }",
            3 => "struct S<T, const N:",
            _ => "fn (",
        };
        client.did_change(uri, 501 + i, broken_code);
        // No settle — format races with diagnostics computation
        let fmt_result = client.format(uri).await;
        assert!(
            fmt_result.is_ok(),
            "formatting should survive concurrent diagnostics, iteration {i}, got: {fmt_result:?}"
        );
    }
}

/// Type `struct S<T, const N: usize>` character by character with format
/// requests at each step (reproduces Sean's generic struct crash).
async fn scenario_format_during_generic_struct_keystroke_sequence(
    client: &mut MockLspClient,
    uri: &Url,
) {
    client.did_change(uri, 600, "");
    client.settle(500).await;

    let steps = [
        "s",
        "st",
        "str",
        "stru",
        "struc",
        "struct",
        "struct ",
        "struct S",
        "struct S<",
        "struct S<T",
        "struct S<T,",
        "struct S<T, ",
        "struct S<T, c",
        "struct S<T, co",
        "struct S<T, con",
        "struct S<T, cons",
        "struct S<T, const",
        "struct S<T, const ",
        "struct S<T, const N",
        "struct S<T, const N:",
        "struct S<T, const N: ",
        "struct S<T, const N: u",
        "struct S<T, const N: us",
        "struct S<T, const N: usi",
        "struct S<T, const N: usiz",
        "struct S<T, const N: usize",
        "struct S<T, const N: usize>",
    ];

    for (i, code) in steps.iter().enumerate() {
        client.did_change(uri, 601 + i as i32, code);
        let fmt_result = client.format(uri).await;
        assert!(
            fmt_result.is_ok(),
            "format crashed at step {i} '{code}': {fmt_result:?}"
        );
    }
}

/// Regression test: AttrMisusePass was absent from initialize_analysis_pass().
/// That caused invalid arithmetic attributes to surface in CLI diagnostics while
/// silently disappearing from the language server.
async fn scenario_arithmetic_attr_reports_errors(client: &mut MockLspClient, uri: &Url) {
    client.clear_diagnostics();
    client.did_change(
        uri,
        750,
        r#"#[arithmetic = checked]
fn bad() {}"#,
    );

    let found = client.wait_for_diagnostic_code(uri, "12-0002").await;
    assert!(
        found,
        "expected error 12-0002 (invalid arithmetic attribute form) — \
         AttrMisusePass must be registered in initialize_analysis_pass(); \
         got: {:?}",
        client.diagnostics_for_uri(uri),
    );
}

/// Regression test: ContractAnalysisPass was absent from initialize_analysis_pass().
/// BodyAnalysisPass explicitly skips contract bodies ("contract-specific analysis is
/// handled separately"), so errors in init/recv blocks were never reported by the LSP.
async fn scenario_contract_analysis_reports_errors(client: &mut MockLspClient, uri: &Url) {
    client.clear_diagnostics();
    // Contract with an unresolved effect: ContractAnalysisPass produces error 8-0051.
    // BodyAnalysisPass will NOT catch this — it only visits all_funcs(), not contract bodies.
    client.did_change(
        uri,
        800,
        r#"struct Store { value: u256 }

pub contract Broken {
    mut store: Store

    init() uses (mut nonexistent_effect) {
        store.value = 0
    }
}"#,
    );

    let found = client.wait_for_diagnostic_code(uri, "8-0051").await;
    assert!(
        found,
        "expected error 8-0051 (unresolved effect in contract init) — \
         ContractAnalysisPass must be registered in initialize_analysis_pass(); \
         got: {:?}",
        client.diagnostics_for_uri(uri),
    );
}

/// Regression test: AttrMisusePass was absent from initialize_analysis_pass().
/// Invalid `#[payable]` usages must be reported by the LSP, not just by `fe check`.
async fn scenario_payable_attr_reports_errors(client: &mut MockLspClient, uri: &Url) {
    client.clear_diagnostics();
    // `#[payable(foo)]` is invalid — the attribute takes no arguments (error 12-0002).
    client.did_change(
        uri,
        850,
        r#"struct Store { value: u256 }

pub contract BadPayable {
    mut store: Store

    #[payable(foo)]
    init() {
        store.value = 0
    }
}"#,
    );

    let found = client.wait_for_diagnostic_code(uri, "12-0002").await;
    assert!(
        found,
        "expected error 12-0002 (invalid #[payable] form) — \
         AttrMisusePass must be registered in initialize_analysis_pass(); \
         got: {:?}",
        client.diagnostics_for_uri(uri),
    );
}

/// Test that error diagnostics are still reported after a panic inside
/// `diagnostics_for_ingot`.
///
/// The outer `catch_unwind` in `handle_files_need_diagnostics` must absorb the
/// panic without killing the server actor. After recovery the diagnostics
/// pipeline must remain fully functional — error codes must still be emitted
/// for code sent after the panic.
async fn scenario_errors_reported_after_panic_recovery(client: &mut MockLspClient, uri: &Url) {
    // Arm the latch: the next handle_files_need_diagnostics call will panic.
    crate::lsp_diagnostics::FORCE_DIAGNOSTIC_PANIC.store(true, std::sync::atomic::Ordering::SeqCst);

    // Trigger a diagnostics run; the handler will hit the panic and the outer
    // catch_unwind in handle_files_need_diagnostics must absorb it.
    client.did_change(uri, 900, "struct TriggerDiag {}");
    client.settle(500).await;

    // Verify the latch was actually consumed by this server and not stolen
    // by a concurrent test. If this fires, the test is racy.
    assert!(
        !crate::lsp_diagnostics::FORCE_DIAGNOSTIC_PANIC.load(std::sync::atomic::Ordering::SeqCst),
        "FORCE_DIAGNOSTIC_PANIC was not consumed — another test may have stolen it"
    );

    // After the panic, send code with a deterministic error code.
    // If the panic killed the diagnostics actor, this will time out.
    client.clear_diagnostics();
    client.did_change(
        uri,
        901,
        r#"pub contract StillReports {
    init() uses (mut ghost_effect) {
    }
}"#,
    );

    let found = client.wait_for_diagnostic_code(uri, "8-0051").await;
    assert!(
        found,
        "server must still emit error 8-0051 after a panic in diagnostics_for_ingot — \
         the outer catch_unwind in handle_files_need_diagnostics must keep \
         the actor alive; got: {:?}",
        client.diagnostics_for_uri(uri),
    );
}

/// Regression test: notifications for methods that Fe doesn't handle must not
/// produce WARN-level log messages to the client. Editors (Zed, VS Code, …)
/// routinely send `$/cancelRequest`, `$/setTrace`, `workspace/didChangeConfiguration`,
/// custom `experimental/*` methods, etc. These arriving at the server is
/// completely normal — it is not a warning condition and should not clutter the
/// client log stream.
///
/// At the time of writing, `server.rs::setup_unhandled` logs these at WARN, which
/// turns every editor session into a wall of noise that drowns out real errors.
async fn scenario_unhandled_notifications_do_not_warn(client: &mut MockLspClient) {
    // Custom notification type for a method no layer in the stack handles.
    // Using `experimental/*` keeps us clear of reserved LSP namespace.
    enum UnhandledTestNotification {}
    impl async_lsp::lsp_types::notification::Notification for UnhandledTestNotification {
        type Params = serde_json::Value;
        const METHOD: &'static str = "experimental/fe_test_unhandled";
    }

    client.clear_log_messages();

    client
        .server
        .notify::<UnhandledTestNotification>(serde_json::Value::Null)
        .expect("notify failed");
    client.settle(200).await;

    let warns = client.log_messages_of_type(MessageType::WARNING);
    let offending: Vec<&LogMessageParams> = warns
        .iter()
        .filter(|m| m.message.contains("Unhandled notification"))
        .collect();

    assert!(
        offending.is_empty(),
        "sending an unknown notification must not emit WARN-level log messages — \
         `setup_unhandled` in server.rs should log at DEBUG so normal LSP traffic \
         (e.g. $/cancelRequest, experimental/*) does not flood the editor log. \
         got: {offending:?}"
    );
}

/// Verify diagnostics are published via the async pipeline.
async fn scenario_diagnostics_published_for_broken_code(client: &mut MockLspClient) {
    // The fixture's foo.fe produces diagnostics. Wait for them to arrive
    // through the full pipeline: actor -> event batching -> publish.
    client.clear_diagnostics();
    let mut found = false;
    for _ in 0..40 {
        client.settle(500).await;
        let diags = client.diagnostics();
        if diags.iter().any(|d| !d.diagnostics.is_empty()) {
            found = true;
            break;
        }
    }
    assert!(
        found,
        "should receive non-empty diagnostics from the fixture ingot"
    );

    // Verify the diagnostics struct fields are populated
    let diags = client.diagnostics();
    let nonempty = diags.iter().find(|d| !d.diagnostics.is_empty()).unwrap();
    assert!(nonempty.uri.scheme() == "file");
    assert!(!nonempty.diagnostics[0].message.is_empty());
}

fn hover_text(hover: &Hover) -> String {
    match &hover.contents {
        HoverContents::Markup(content) => content.value.clone(),
        HoverContents::Scalar(marked) => match marked {
            MarkedString::String(text) => text.clone(),
            MarkedString::LanguageString(lang) => lang.value.clone(),
        },
        HoverContents::Array(items) => items
            .iter()
            .map(|item| match item {
                MarkedString::String(text) => text.clone(),
                MarkedString::LanguageString(lang) => lang.value.clone(),
            })
            .collect::<Vec<_>>()
            .join("\n"),
    }
}

async fn hover_eventually(
    client: &mut MockLspClient,
    uri: &Url,
    line: u32,
    character: u32,
) -> Hover {
    for _ in 0..20 {
        if let Ok(Some(hover)) = client.hover(uri, line, character).await {
            return hover;
        }
        client.settle(200).await;
    }
    panic!("expected hover result at {line}:{character}");
}

#[tokio::test]
async fn mock_lsp_hover_shows_contract_field_layout_info() {
    let mut client = MockLspClient::start().await;
    client.initialize().await;

    let uri = lib_url();
    client.did_open(&uri, "");
    client.settle(500).await;

    let code = r#"msg M {
  #[selector = 0x01]
  Ping { amount: i32 } -> bool,
}

pub contract C {
  supply: i32

  recv M {
    Ping { amount } -> bool uses (mut supply) {
      supply += amount
      true
    }
  }
}
"#;
    client.did_change(&uri, 700, code);
    client.settle(1000).await;

    let field_hover = hover_eventually(&mut client, &uri, 9, 39).await;
    let field_text = hover_text(&field_hover);
    assert!(
        field_text.contains("slot:") && field_text.contains("space:"),
        "expected field hover to include layout info, got:\n{field_text}"
    );

    let alias_hover = hover_eventually(&mut client, &uri, 10, 7).await;
    let alias_text = hover_text(&alias_hover);
    assert!(
        alias_text.contains("slot:") && alias_text.contains("space:"),
        "expected alias hover to include layout info, got:\n{alias_text}"
    );

    client.shutdown().await;
}

#[tokio::test]
async fn mock_lsp_hover_shows_discharged_trait_obligations() {
    let mut client = MockLspClient::start().await;
    client.initialize().await;

    let uri = lib_url();
    client.did_open(&uri, "");
    client.settle(500).await;

    let code = r#"trait Marker {}

struct Point {}

impl Marker for Point {}

struct Holder {}

impl Holder {
  fn accept<T: Marker>(self, _ t: T) {}
}

fn requires_marker<T: Marker>(_ t: T) {}

fn caller(p: Point, h: Holder) {
  requires_marker(p)
  h.accept(p)
}
"#;
    client.did_change(&uri, 700, code);
    client.settle(1000).await;

    // Hovering the callee path of `requires_marker(p)` reports the trait
    // obligation its constraints raised and the impl that discharged it.
    let call_hover = hover_eventually(&mut client, &uri, 15, 5).await;
    let call_text = hover_text(&call_hover);
    assert!(
        call_text.contains("Trait obligations discharged:")
            && call_text.contains("`Point: Marker` discharged by `impl Marker for Point`"),
        "expected call hover to include discharge evidence, got:\n{call_text}"
    );

    // The same evidence is reported when hovering the method name of a
    // method call whose constraints were discharged.
    let method_hover = hover_eventually(&mut client, &uri, 16, 5).await;
    let method_text = hover_text(&method_hover);
    assert!(
        method_text.contains("Trait obligations discharged:")
            && method_text.contains("`Point: Marker` discharged by `impl Marker for Point`"),
        "expected method hover to include discharge evidence, got:\n{method_text}"
    );

    client.shutdown().await;
}

#[tokio::test]
async fn mock_lsp_hover_shows_discharged_const_predicates() {
    let mut client = MockLspClient::start().await;
    client.initialize().await;

    let uri = lib_url();
    client.did_open(&uri, "");
    client.settle(500).await;

    let code = r#"trait Platform {
    const WORD_BITS: u256
}

struct Evm {}

impl Platform for Evm {
    const WORD_BITS: u256 = 256
}

fn word_op<B: Platform>() where B::WORD_BITS == 256 {
}

fn caller() {
    word_op<Evm>()
}
"#;
    client.did_change(&uri, 700, code);
    client.settle(1000).await;

    // Hovering the callee `word_op` reports the const predicate its `where`
    // clause raised and the route (CTFE) that discharged it, reconstructed from
    // the recorded evidence.
    let call_hover = hover_eventually(&mut client, &uri, 14, 5).await;
    let call_text = hover_text(&call_hover);
    assert!(
        call_text.contains("Const predicates discharged:")
            && call_text.contains("discharged by CTFE")
            && call_text.contains("Evm"),
        "expected call hover to include const-predicate discharge evidence, got:\n{call_text}"
    );

    client.shutdown().await;
}

/// Regression test for the "goto def → freeze" bug observed in Zed.
///
/// # History
///
/// Log `2061701.log`, 2026-04-06: a burst of ~14 concurrent
/// `textDocument/definition` + `textDocument/typeDefinition` requests
/// arrived at the LSP within ~400µs after a goto-def click. The LSP
/// actor thread went to `futex_do_wait` and never came back. No panic,
/// no response.
///
/// # Root cause (fixed)
///
/// The bug was in `async-lsp::MainLoop::run`. When an incoming message
/// arrived, the outer `select_biased!` entered the `incoming.next()` arm
/// body, which ran a nested inner loop awaiting `dispatch_fut`.
/// `dispatch_fut` awaited `service.poll_ready`. If `ConcurrencyLayer`'s
/// semaphore was saturated, `poll_ready` returned `Pending`. The inner
/// loop only selected on `dispatch_fut` and `flush_fut` — it did NOT
/// poll `self.tasks`. For `poll_ready` to become ready a permit had to
/// release, which required an in-flight task to complete, which required
/// `self.tasks` to be polled, which the main loop couldn't do because it
/// was stuck inside the arm body. Classic "waker trapped behind a nested
/// await" deadlock.
///
/// The fix (in the local `async-lsp` checkout at
/// `~/hacker-stuff-2023/async-lsp`): `dispatch_message` now drains
/// `self.tasks` concurrently with `poll_ready` via disjoint field
/// borrows. Drained responses flow back to the outer loop alongside the
/// dispatch outcome as a `Vec<Message>`, and the outer loop feeds them
/// all to the outgoing sink before the next iteration. Details in the
/// `dispatch_message` doc comment in `async-lsp/src/lib.rs`.
///
/// # What this test does
///
/// Fires 72 concurrent LSP requests (24 positions × 3 request types)
/// against the mock client. `ConcurrencyLayer::default() == nproc`
/// means 16 permits on a typical dev box, so the burst pegs the
/// semaphore and forces `dispatch_message` down the drain-while-waiting
/// code path. If a regression re-introduces the inner-loop deadlock,
/// `tokio::time::timeout` catches it and the assertion fires with a
/// pointer to the exact lines in `async-lsp/src/lib.rs` that need
/// inspection.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn repro_concurrent_goto_burst_does_not_deadlock() {
    let mut client = MockLspClient::start().await;
    client.initialize().await;

    let uri = lib_url();
    // Seed lib.fe with content that has multiple cross-file references
    // to `Why` defined in foo.fe — gives the LSP real cross-file targets
    // to chase when we fire goto-def.
    let lib_content = r#"use ingot::foo::Why

mod who {
  use super::Why
  pub mod what {
    pub fn how() {}
    pub mod how {
      use ingot::Why
      pub struct When {
        x: Why
      }
    }
  }
  pub struct Bar {
    x: Why
  }
}

fn bar() -> () {
    let y: Why
    let z = who::what::how
    let z: who::what::how::When
}"#;
    client.did_open(&uri, lib_content);
    client.settle(500).await;

    // Fire a burst modeled on the Zed log: definition + hover + references
    // across several cursor positions. Each position hits a `Why`
    // identifier somewhere in lib.fe. The LSP should happily process all
    // of them; if the actor deadlocks, the outer timeout catches it.
    //
    // Positions are (line, col) pairs landing on identifiers in
    // `lib_content`. We re-use them multiple times to get 20+ requests
    // without calibrating precise offsets.
    let probe_positions = [
        (0, 16),  // Why in the top-level `use`
        (3, 13),  // Why in `use super::Why`
        (7, 17),  // Why in `use ingot::Why`
        (9, 11),  // Why in `x: Why` inside When
        (14, 9),  // Why in `x: Why` inside Bar
        (19, 11), // Why in `let y: Why`
        (20, 15), // how in `let z = who::what::how`
        (21, 20), // When in `let z: who::what::how::When`
    ];

    // Fire all the requests TRULY CONCURRENTLY, not sequentially. This
    // is what Zed does on a goto-def click — the log shows 14 requests
    // arriving within 400µs, well before any of them complete. Each
    // request goes over the wire as its own JSON-RPC message and the
    // actor's dispatch loop spawns one detached task per message.
    //
    // We clone the `ServerSocket` so we can issue multiple requests
    // without an `&mut self` bottleneck, then collect the resulting
    // futures and `join_all` them. The timeout catches the hang if the
    // actor deadlocks.
    // Fire >16 concurrent goto-definition requests at the server. 16 is the
    // default ConcurrencyLayer limit (nproc on our test box), so 18 requests
    // overflows the semaphore by 2 and triggers the `MainLoop::run` dispatch
    // deadlock described in the docstring above.
    let socket = client.server.clone();
    let mut request_futures: Vec<std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send>>> =
        Vec::new();
    for _ in 0..3 {
        for &(line, col) in &probe_positions {
            let s1 = socket.clone();
            let uri1 = uri.clone();
            request_futures.push(Box::pin(async move {
                let _ = s1
                    .request::<async_lsp::lsp_types::request::GotoDefinition>(
                        GotoDefinitionParams {
                            text_document_position_params: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: uri1 },
                                position: Position {
                                    line,
                                    character: col,
                                },
                            },
                            partial_result_params: PartialResultParams::default(),
                            work_done_progress_params: WorkDoneProgressParams::default(),
                        },
                    )
                    .await;
            }));
            let s2 = socket.clone();
            let uri2 = uri.clone();
            request_futures.push(Box::pin(async move {
                let _ = s2
                    .request::<async_lsp::lsp_types::request::GotoTypeDefinition>(
                        async_lsp::lsp_types::request::GotoTypeDefinitionParams {
                            text_document_position_params: TextDocumentPositionParams {
                                text_document: TextDocumentIdentifier { uri: uri2 },
                                position: Position {
                                    line,
                                    character: col,
                                },
                            },
                            partial_result_params: PartialResultParams::default(),
                            work_done_progress_params: WorkDoneProgressParams::default(),
                        },
                    )
                    .await;
            }));
            let s3 = socket.clone();
            let uri3 = uri.clone();
            request_futures.push(Box::pin(async move {
                let _ = s3
                    .request::<async_lsp::lsp_types::request::HoverRequest>(HoverParams {
                        text_document_position_params: TextDocumentPositionParams {
                            text_document: TextDocumentIdentifier { uri: uri3 },
                            position: Position {
                                line,
                                character: col,
                            },
                        },
                        work_done_progress_params: WorkDoneProgressParams::default(),
                    })
                    .await;
            }));
        }
    }

    let total = request_futures.len();
    let burst = tokio::time::timeout(
        Duration::from_secs(15),
        futures::future::join_all(request_futures),
    )
    .await;

    assert!(
        burst.is_ok(),
        "{total} concurrent LSP requests deadlocked (no responses within 15s). \
         Root cause lives in async-lsp MainLoop::run at src/lib.rs:554-568: \
         the inner dispatch loop doesn't poll self.tasks while awaiting \
         dispatch_fut/poll_ready, so a saturated ConcurrencyLayer semaphore \
         never releases. See this test's docstring for the full analysis."
    );

    client.shutdown().await;
}
