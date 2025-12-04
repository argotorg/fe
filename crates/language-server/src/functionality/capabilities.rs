use async_lsp::lsp_types::{HoverProviderCapability, ServerCapabilities};

#[cfg(target_arch = "wasm32")]
use crate::util::DummyFilePathConversion;

pub(crate) fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        // full sync mode for now
        text_document_sync: Some(async_lsp::lsp_types::TextDocumentSyncCapability::Kind(
            async_lsp::lsp_types::TextDocumentSyncKind::FULL,
        )),
        // goto definition
        definition_provider: Some(async_lsp::lsp_types::OneOf::Left(true)),
        // find all references
        references_provider: Some(async_lsp::lsp_types::OneOf::Left(true)),
        // document highlight
        document_highlight_provider: Some(async_lsp::lsp_types::OneOf::Left(true)),
        // go to type definition
        type_definition_provider: Some(async_lsp::lsp_types::TypeDefinitionProviderCapability::Simple(true)),
        // support for workspace add/remove changes
        workspace: Some(async_lsp::lsp_types::WorkspaceServerCapabilities {
            workspace_folders: Some(async_lsp::lsp_types::WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(async_lsp::lsp_types::OneOf::Left(true)),
            }),
            file_operations: Some(
                async_lsp::lsp_types::WorkspaceFileOperationsServerCapabilities {
                    did_create: Some(async_lsp::lsp_types::FileOperationRegistrationOptions {
                        filters: vec![async_lsp::lsp_types::FileOperationFilter {
                            scheme: Some(String::from("file")),
                            pattern: async_lsp::lsp_types::FileOperationPattern {
                                glob: String::from("**/*"),
                                options: None,
                                // options: Some(async_lsp::lsp_types::FileOperationPatternOptions {
                                //     ignore_case: Some(true),
                                // }),
                                matches: None,
                            },
                        }],
                    }),
                    did_rename: Some(async_lsp::lsp_types::FileOperationRegistrationOptions {
                        filters: vec![async_lsp::lsp_types::FileOperationFilter {
                            scheme: Some(String::from("file")),
                            pattern: async_lsp::lsp_types::FileOperationPattern {
                                glob: String::from("**/*"),
                                options: None,
                                // options: Some(async_lsp::lsp_types::FileOperationPatternOptions {
                                //     ignore_case: Some(true),
                                // }),
                                matches: None,
                            },
                        }],
                    }),
                    did_delete: Some(async_lsp::lsp_types::FileOperationRegistrationOptions {
                        filters: vec![async_lsp::lsp_types::FileOperationFilter {
                            scheme: Some(String::from("file")),
                            pattern: async_lsp::lsp_types::FileOperationPattern {
                                glob: String::from("**/*"),
                                options: None,
                                // options: Some(async_lsp::lsp_types::FileOperationPatternOptions {
                                //     ignore_case: Some(true),
                                // }),
                                matches: None,
                            },
                        }],
                    }),
                    will_create: None,
                    will_rename: None,
                    will_delete: None,
                    // TODO: implement file operation refactors and workspace cache updates
                    // will_create: Some(async_lsp::lsp_types::FileOperationRegistrationOptions {
                    //     filters: vec![async_lsp::lsp_types::FileOperationFilter {
                    //         scheme: Some(String::from("file")),
                    //         pattern: async_lsp::lsp_types::FileOperationPattern {
                    //             glob: String::from("**/*"),
                    //             options: None,
                    //             matches: None,
                    //         },
                    //     }],
                    // }),
                    // will_rename: Some(async_lsp::lsp_types::FileOperationRegistrationOptions {
                    //     filters: vec![async_lsp::lsp_types::FileOperationFilter {
                    //         scheme: Some(String::from("file")),
                    //         pattern: async_lsp::lsp_types::FileOperationPattern {
                    //             glob: String::from("**/*"),
                    //             options: None,
                    //             matches: None,
                    //         },
                    //     }],
                    // }),
                    // will_delete: Some(async_lsp::lsp_types::FileOperationRegistrationOptions {
                    //     filters: vec![async_lsp::lsp_types::FileOperationFilter {
                    //         scheme: Some(String::from("file")),
                    //         pattern: async_lsp::lsp_types::FileOperationPattern {
                    //             glob: String::from("**/*"),
                    //             options: None,
                    //             matches: None,
                    //         },
                    //     }],
                    // }),
                },
            ),
        }),
        ..Default::default()
    }
}
