use async_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range, Url};
use camino::Utf8Path;
use codespan_reporting::files as cs_files;
use common::{
    diagnostics::{CompleteDiagnostic, cmp_complete_diagnostics},
    file::{File, IngotFileKind},
    ingot::IngotKind,
};
use driver::{DriverDataBase, MirDiagnosticsMode};
use hir::Ingot;
use hir::analysis::analysis_pass::{
    AnalysisPassManager, EventLowerPass, MsgLowerPass, ParsingPass,
};
use hir::analysis::name_resolution::ImportAnalysisPass;
use hir::analysis::ty::{
    AdtDefAnalysisPass, BodyAnalysisPass, ContractAnalysisPass, DefConflictAnalysisPass,
    FuncAnalysisPass, ImplAnalysisPass, ImplTraitAnalysisPass, MsgSelectorAnalysisPass,
    TraitAnalysisPass, TypeAliasAnalysisPass,
};
use hir::hir_def::HirIngot;
use hir::lower::map_file_to_mod;
use rustc_hash::FxHashMap;

use crate::util::diag_to_lsp;

/// Test-only latch: set to `true` to force the next
/// `handle_files_need_diagnostics` call to panic (simulating an analysis-pass
/// crash). The flag is consumed atomically (swap to false), so only one call
/// panics per arm.
///
/// The check lives in `handle_files_need_diagnostics` (handlers.rs), not in
/// `diagnostics_for_ingot`, so unit tests that call `diagnostics_for_ingot`
/// directly never interact with the latch — only the mock LSP test path does.
#[cfg(test)]
pub(crate) static FORCE_DIAGNOSTIC_PANIC: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(false);

/// Wrapper type to implement codespan Files trait
#[allow(dead_code)]
pub struct LspDb<'a>(pub &'a DriverDataBase);

/// Extension trait for LSP-specific functionality on DriverDataBase
pub trait LspDiagnostics {
    fn diagnostics_for_ingot(&self, ingot: Ingot) -> FxHashMap<Url, Vec<Diagnostic>>;
    #[allow(dead_code)]
    fn file_line_starts(&self, file: File) -> Vec<usize>;
}

impl LspDiagnostics for DriverDataBase {
    fn diagnostics_for_ingot(&self, ingot: Ingot) -> FxHashMap<Url, Vec<Diagnostic>> {
        let t_total = std::time::Instant::now();
        let mut result = FxHashMap::<Url, Vec<Diagnostic>>::default();
        let mut pass_manager = initialize_analysis_pass();
        let ingot_files = ingot.files(self);
        let is_standalone = ingot.kind(self) == IngotKind::StandAlone;
        let file_count = ingot_files
            .iter()
            .filter(|(_, f)| matches!(f.kind(self), Some(IngotFileKind::Source)))
            .count();

        let mut hir_has_errors = false;
        for (url, file) in ingot_files.iter() {
            if !matches!(file.kind(self), Some(IngotFileKind::Source)) {
                continue;
            }

            // initialize an empty diagnostic list for this file
            // (to clear any previous diagnostics)
            let file_diags = result.entry(url.clone()).or_default();

            // Add warning for standalone files (files outside of a proper ingot)
            if is_standalone {
                file_diags.push(standalone_file_warning());
            }

            let t_file = std::time::Instant::now();
            let top_mod = map_file_to_mod(self, file);
            let diagnostics = pass_manager.run_on_module(self, top_mod);
            tracing::debug!("[fe:timing]  file {url}: {:?}", t_file.elapsed());
            let mut finalized_diags: Vec<CompleteDiagnostic> = diagnostics
                .iter()
                .map(|d| d.to_complete(self).clone())
                .collect();
            if finalized_diags
                .iter()
                .any(|d| d.severity == common::diagnostics::Severity::Error)
            {
                hir_has_errors = true;
            }
            finalized_diags.sort_by(cmp_complete_diagnostics);
            for diag in finalized_diags {
                let lsp_diags = diag_to_lsp(self, diag).clone();
                for (uri, more_diags) in lsp_diags {
                    let diags = result.entry(uri.clone()).or_insert_with(Vec::new);
                    diags.extend(more_diags);
                }
            }
        }

        let t_mir = std::time::Instant::now();
        // Skip MIR diagnostics when HIR already has errors: MIR assumes HIR is
        // sound and panics on broken input. Also skip for ingots with no modules.
        let mut mir_diags = if !hir_has_errors && ingot.module_tree(self).root_data().is_some() {
            match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                self.mir_diagnostics_for_ingot(ingot, MirDiagnosticsMode::TemplatesOnly)
            })) {
                Ok(diags) => diags,
                Err(panic_info) => {
                    if panic_info.is::<salsa::Cancelled>() {
                        std::panic::resume_unwind(panic_info);
                    }
                    let msg = panic_info
                        .downcast_ref::<&str>()
                        .copied()
                        .or_else(|| panic_info.downcast_ref::<String>().map(|s| s.as_str()))
                        .unwrap_or("<non-string panic>");
                    tracing::error!("MIR diagnostics panicked (skipping): {msg}");
                    // Intentional degradation: return empty MIR diagnostics rather
                    // than letting the panic propagate to the outer handler and
                    // losing the HIR diagnostics we already collected. The outer
                    // catch_unwind in handle_files_need_diagnostics is the safety
                    // net for panics that originate before we have any HIR results.
                    Vec::new()
                }
            }
        } else {
            Vec::new()
        };
        tracing::debug!("[fe:timing]  MIR diagnostics: {:?}", t_mir.elapsed());
        mir_diags.sort_by(cmp_complete_diagnostics);
        for diag in mir_diags {
            let lsp_diags = diag_to_lsp(self, diag).clone();
            for (uri, more_diags) in lsp_diags {
                let diags = result.entry(uri.clone()).or_insert_with(Vec::new);
                diags.extend(more_diags);
            }
        }

        tracing::debug!(
            "[fe:timing] diagnostics_for_ingot ({file_count} files): {:?}",
            t_total.elapsed()
        );
        result
    }

    fn file_line_starts(&self, file: File) -> Vec<usize> {
        cs_files::line_starts(file.text(self)).collect()
    }
}

impl<'a> cs_files::Files<'a> for LspDb<'a> {
    type FileId = File;
    type Name = &'a Utf8Path;
    type Source = &'a str;

    fn name(&'a self, file_id: Self::FileId) -> Result<Self::Name, cs_files::Error> {
        file_id
            .path(self.0)
            .as_deref()
            .ok_or(cs_files::Error::FileMissing)
    }

    fn source(&'a self, file_id: Self::FileId) -> Result<Self::Source, cs_files::Error> {
        Ok(file_id.text(self.0))
    }

    fn line_index(
        &'a self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, cs_files::Error> {
        let starts = self.0.file_line_starts(file_id);
        Ok(starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'a self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, cs_files::Error> {
        let line_starts = self.0.file_line_starts(file_id);

        let start = *line_starts
            .get(line_index)
            .ok_or(cs_files::Error::LineTooLarge {
                given: line_index,
                max: line_starts.len() - 1,
            })?;

        let end = if line_index == line_starts.len() - 1 {
            file_id.text(self.0).len()
        } else {
            *line_starts
                .get(line_index + 1)
                .ok_or(cs_files::Error::LineTooLarge {
                    given: line_index,
                    max: line_starts.len() - 1,
                })?
        };

        Ok(std::ops::Range { start, end })
    }
}

fn initialize_analysis_pass() -> AnalysisPassManager {
    let mut pass_manager = AnalysisPassManager::new();
    pass_manager.add_module_pass("Parsing", Box::new(ParsingPass {}));
    pass_manager.add_module_pass("MsgLower", Box::new(MsgLowerPass {}));
    pass_manager.add_module_pass("EventLower", Box::new(EventLowerPass {}));
    pass_manager.add_module_pass("MsgSelector", Box::new(MsgSelectorAnalysisPass {}));
    pass_manager.add_module_pass("DefConflict", Box::new(DefConflictAnalysisPass {}));
    pass_manager.add_module_pass("Import", Box::new(ImportAnalysisPass {}));
    pass_manager.add_module_pass("AdtDef", Box::new(AdtDefAnalysisPass {}));
    pass_manager.add_module_pass("TypeAlias", Box::new(TypeAliasAnalysisPass {}));
    pass_manager.add_module_pass("Trait", Box::new(TraitAnalysisPass {}));
    pass_manager.add_module_pass("Impl", Box::new(ImplAnalysisPass {}));
    pass_manager.add_module_pass("ImplTrait", Box::new(ImplTraitAnalysisPass {}));
    pass_manager.add_module_pass("Func", Box::new(FuncAnalysisPass {}));
    pass_manager.add_module_pass("Body", Box::new(BodyAnalysisPass {}));
    pass_manager.add_module_pass("Contract", Box::new(ContractAnalysisPass {}));
    pass_manager
}

/// Creates a warning diagnostic for standalone files (files outside of a proper ingot).
fn standalone_file_warning() -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 0,
            },
        },
        severity: Some(DiagnosticSeverity::WARNING),
        code: None,
        source: Some("fe".to_string()),
        message: "This file is not part of an ingot and should be considered isolated from other .fe files."
            .to_string(),
        related_information: None,
        tags: None,
        code_description: None,
        data: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::load_ingot_from_directory;
    use common::InputDb;
    use std::path::PathBuf;

    const FIXTURE: &str = "single_ingot";

    fn fixture_path() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("test_files")
            .join(FIXTURE)
    }

    fn lib_url() -> Url {
        Url::from_file_path(fixture_path().join("src").join("lib.fe")).unwrap()
    }

    /// Set up the DB from the fixture directory.
    fn setup_db() -> DriverDataBase {
        let mut db = DriverDataBase::default();
        load_ingot_from_directory(&mut db, &fixture_path());
        db
    }

    /// Resolve the ingot for lib.fe and run diagnostics_for_ingot.
    fn run_diagnostics(db: &DriverDataBase) -> FxHashMap<Url, Vec<Diagnostic>> {
        let ingot = db
            .workspace()
            .containing_ingot(db, lib_url())
            .expect("ingot not found");
        db.diagnostics_for_ingot(ingot)
    }

    /// Update file text then run diagnostics (re-resolves ingot after mutation).
    fn update_and_diagnose(
        db: &mut DriverDataBase,
        new_text: &str,
    ) -> FxHashMap<Url, Vec<Diagnostic>> {
        db.workspace().update(db, lib_url(), new_text.to_string());
        run_diagnostics(db)
    }

    /// Regression test: diagnostics_for_ingot must not panic on valid code.
    #[test]
    fn diagnostics_for_valid_ingot_does_not_panic() {
        let db = setup_db();
        let _diags = run_diagnostics(&db);
    }

    /// Regression test: diagnostics must survive incomplete/truncated source text,
    /// simulating a user mid-edit (e.g., typing `struct S<T, const N: usize>`).
    #[test]
    fn diagnostics_survive_truncated_source() {
        let mut db = setup_db();
        let _diags = update_and_diagnose(&mut db, "struct S<T, const");
    }

    /// Regression test: diagnostics must survive completely empty file content.
    #[test]
    fn diagnostics_survive_empty_file() {
        let mut db = setup_db();
        let _diags = update_and_diagnose(&mut db, "");
    }

    /// Regression test: diagnostics must survive garbage/non-Fe content.
    #[test]
    fn diagnostics_survive_garbage_content() {
        let mut db = setup_db();
        let _diags = update_and_diagnose(&mut db, "}{}{}{{{{}}}}}(((");
    }

    /// Regression test: simulates the exact "mself" scenario Sean reported —
    /// an intermediate editing state where `self` is being changed to `mut self`.
    #[test]
    fn diagnostics_survive_intermediate_self_edit() {
        let mut db = setup_db();
        let _diags = update_and_diagnose(
            &mut db,
            r#"
struct Foo {
    x: u256
}

impl Foo {
    fn set(mself, val: u256) {
        self.x = val
    }
}
"#,
        );
    }

    /// Regression test: simulates partial generic struct definition.
    #[test]
    fn diagnostics_survive_partial_generic_definition() {
        let mut db = setup_db();
        let _diags = update_and_diagnose(&mut db, "struct S<T, const N:");
    }

    /// Regression test: diagnostics must not crash on incomplete function bodies
    /// during mid-edit states.
    #[test]
    fn diagnostics_survive_partial_function_body() {
        let mut db = setup_db();
        let _diags = update_and_diagnose(
            &mut db,
            r#"
fn foo() -> u256 {
    let x: u256 = 42
    let y: u256 =
"#,
        );
    }

    /// Simulate character-by-character typing of `self` → `mut self` as Sean reported.
    /// Each intermediate state must not panic.
    #[test]
    fn diagnostics_survive_self_to_mut_self_keystroke_sequence() {
        let mut db = setup_db();
        let template = |param: &str| {
            format!(
                "struct Foo {{ x: u256 }}\nimpl Foo {{\n    fn set({param}, val: u256) {{\n        self.x = val\n    }}\n}}"
            )
        };
        // Simulate cursor before "self", typing "mut " one char at a time
        for intermediate in &[
            "self",     // original
            "mself",    // typed 'm' before 'self'
            "muself",   // typed 'u'
            "mutself",  // typed 't'
            "mut self", // typed ' '
        ] {
            let _diags = update_and_diagnose(&mut db, &template(intermediate));
        }
    }

    /// Simulate typing `struct S<T, const N: usize>` character by character.
    #[test]
    fn diagnostics_survive_generic_struct_keystroke_sequence() {
        let mut db = setup_db();
        let steps = [
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
        for step in &steps {
            let _diags = update_and_diagnose(&mut db, step);
        }
    }

    /// Test rapid successive edits: valid → broken → valid → broken.
    /// This catches bugs where stale salsa cache state causes panics
    /// when transitioning between valid and invalid states.
    #[test]
    fn diagnostics_survive_valid_invalid_transitions() {
        let mut db = setup_db();
        let valid = "struct Foo { x: u256 }\nfn bar() -> u256 { return 1 }";
        let broken_states = [
            "",                  // empty
            "struct",            // keyword only
            "struct {",          // missing name
            "fn (",              // broken fn
            "impl {",            // impl without type
            "struct Foo { x: }", // missing type
            "fn bar() -> { }",   // missing return type
            "use ",              // incomplete use
            "pub ",              // dangling pub
            "let x =",           // top-level let
        ];
        for broken in &broken_states {
            let _diags = update_and_diagnose(&mut db, valid);
            let _diags = update_and_diagnose(&mut db, broken);
        }
    }

    /// Test that formatting doesn't panic on malformed inputs.
    #[test]
    fn format_survives_malformed_inputs() {
        let inputs = [
            "",
            "}{",
            "struct",
            "fn (",
            "struct Foo { x: u256 }",
            "impl Foo { fn set(mself) {} }",
            "struct S<T, const N:",
        ];
        for input in &inputs {
            // format_str should return an error, never panic
            let _ = fmt::format_str(input, &fmt::Config::default());
        }
    }

    /// Fuzz diagnostics with many malformed inputs to find panics.
    /// Uses catch_unwind to detect rather than crash.
    #[test]
    fn fuzz_diagnostics_for_panics() {
        let mut db = setup_db();
        let inputs = [
            // Sean's reported scenarios
            "impl Foo { fn set(mself, val: u256) { self.x = val } }",
            "struct S<T, const N: usize>",
            "struct S<T, const N:",
            // Edge cases in generics
            "struct S<>",
            "struct S<,>",
            "struct S<T,>",
            "fn f<>() {}",
            "fn f<T: >() {}",
            "fn f() -> <T> {}",
            // Unclosed delimiters
            "fn f() {",
            "fn f() { {",
            "fn f() { { }",
            "struct S { x: u256",
            "impl Foo {",
            "impl Foo { fn f(",
            // Invalid positions
            "return 5",
            "self.x",
            "use ",
            "use foo::",
            "use foo::bar::",
            "pub",
            "pub fn",
            "pub fn f",
            "pub fn f(",
            "pub fn f(x",
            "pub fn f(x:",
            "pub fn f(x: u256",
            "pub fn f(x: u256)",
            "pub fn f(x: u256) {",
            "pub fn f(x: u256) { }",
            // Weird token sequences
            ":::",
            "...",
            "<<<",
            ">>>",
            "+++",
            "***",
            "@@@",
            "###",
            "$$$",
            "%%%",
            // Mixed valid/invalid
            "struct Foo { x: u256 }\nimpl",
            "struct Foo { x: u256 }\nimpl Foo",
            "struct Foo { x: u256 }\nimpl Foo {",
            "struct Foo { x: u256 }\nimpl Foo { fn",
            "struct Foo { x: u256 }\nimpl Foo { fn f",
            "struct Foo { x: u256 }\nimpl Foo { fn f(",
            "struct Foo { x: u256 }\nimpl Foo { fn f(self",
            "struct Foo { x: u256 }\nimpl Foo { fn f(self)",
            "struct Foo { x: u256 }\nimpl Foo { fn f(self) {",
            // Contract-related
            "pub contract",
            "pub contract C",
            "pub contract C {",
            "pub contract C { pub fn",
            // Trait-related
            "trait",
            "trait T",
            "trait T {",
            "trait T { fn f(",
            "impl trait",
            // Enum-related
            "enum",
            "enum E",
            "enum E {",
            "enum E { A(",
        ];

        let mut panics = Vec::new();
        for input in &inputs {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                update_and_diagnose(&mut db, input);
            }));
            if let Err(e) = result {
                let msg = e
                    .downcast_ref::<&str>()
                    .copied()
                    .or_else(|| e.downcast_ref::<String>().map(|s| s.as_str()))
                    .unwrap_or("<non-string panic>");
                panics.push(format!("PANIC on '{input}': {msg}"));
                // Reset DB after panic since state may be corrupted
                db = setup_db();
            }
        }

        if !panics.is_empty() {
            panic!(
                "Found {} panic(s) in diagnostics:\n{}",
                panics.len(),
                panics.join("\n")
            );
        }
    }
}
