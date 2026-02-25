//! Integration tests using a mock LSP client connected to the real server.
//!
//! These tests exercise the full pipeline: actor → diagnostics batching →
//! request handling, verifying the server survives malformed edits.

use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::lsp_types::notification::PublishDiagnostics;
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
/// the protocol. Diagnostics are collected automatically for inspection.
pub struct MockLspClient {
    server: async_lsp::ServerSocket,
    diagnostics: Arc<Mutex<Vec<PublishedDiagnostics>>>,
    _srv_handle: tokio::task::JoinHandle<()>,
    _cli_handle: tokio::task::JoinHandle<()>,
}

impl MockLspClient {
    /// Spawn the real server and connect a mock client.
    pub async fn start() -> Self {
        let (server_main, _client_socket) = MainLoop::new_server(|client| {
            let lsp_service = setup(client.clone(), "test-actor".to_string());
            ServiceBuilder::new()
                .layer(LifecycleLayer::default())
                .layer(ConcurrencyLayer::default())
                .service(lsp_service)
        });

        let (server_stream, client_stream) = tokio::io::duplex(DUPLEX_BUF);
        let (srv_rx, srv_tx) = server_stream.compat().split();
        let srv_handle = tokio::spawn(async move {
            if let Err(e) = server_main.run_buffered(srv_rx, srv_tx).await {
                tracing::debug!("test server loop exited: {e:?}");
            }
        });

        let diagnostics: Arc<Mutex<Vec<PublishedDiagnostics>>> = Arc::new(Mutex::new(Vec::new()));
        let diag_collector = diagnostics.clone();

        let (client_main, server_socket) = MainLoop::new_client(move |_| {
            let diags = diag_collector.clone();
            let mut router = async_lsp::router::Router::new(());
            router
                .notification::<PublishDiagnostics>(move |_, params| {
                    diags.lock().unwrap().push(PublishedDiagnostics {
                        uri: params.uri,
                        diagnostics: params.diagnostics,
                    });
                    ControlFlow::Continue(())
                })
                // Silently absorb server log/show messages
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
            _srv_handle: srv_handle,
            _cli_handle: cli_handle,
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
