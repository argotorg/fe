//! Test database utilities for HIR analysis.
//!
//! This module is only available when the `testutils` feature is enabled.

// TODO tracing::error doesn't log. set up default logger?
#![allow(clippy::print_stderr)]

use std::collections::BTreeMap;
use std::ops::Range;

use crate::analysis::{
    analysis_pass::AnalysisPassManager,
    diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
    initialize_pre_contract_analysis_pass,
};
use crate::{
    Ingot, SpannedHirDb,
    hir_def::{Contract, Func, ItemKind, TopLevelMod, scope_graph::ScopeGraph},
    lower::{self, map_file_to_mod, module_tree, scope_graph},
    span::{DynLazySpan, LazySpan},
};
use camino::{Utf8Path, Utf8PathBuf};
use codespan_reporting::diagnostic as cs_diag;
use codespan_reporting::files as cs_files;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{BufferWriter, ColorChoice},
    },
};
use common::{
    InputDb, define_input_db,
    diagnostics::{LabelStyle, Severity, cmp_complete_diagnostics, trim_trailing_line_whitespace},
    file::File,
    indexmap::IndexMap,
    paths::absolute_utf8,
    stdlib::{BUILTIN_STD_BASE_URL, HasBuiltinCore, HasBuiltinStd},
};
use derive_more::TryIntoError;
use rustc_hash::FxHashMap;
use test_utils::url_utils::UrlExt;
use url::Url;

type CodeSpanFileId = usize;

// --- Codespan diagnostic helpers (inlined from driver::diagnostics) ---

trait ToCsDiag {
    fn to_cs(&self, db: &dyn SpannedInputDb) -> cs_diag::Diagnostic<File>;
}

trait SpannedInputDb: SpannedHirAnalysisDb + InputDb {}
impl<T> SpannedInputDb for T where T: SpannedHirAnalysisDb + InputDb {}

impl<T> ToCsDiag for T
where
    T: DiagnosticVoucher,
{
    fn to_cs(&self, db: &dyn SpannedInputDb) -> cs_diag::Diagnostic<File> {
        let complete = self.to_complete(db);

        let severity = match complete.severity {
            Severity::Error => cs_diag::Severity::Error,
            Severity::Warning => cs_diag::Severity::Warning,
            Severity::Note => cs_diag::Severity::Note,
        };
        let code = Some(complete.error_code.to_string());
        let message = complete.message;

        let labels = complete
            .sub_diagnostics
            .into_iter()
            .filter_map(|sub_diag| {
                let span = sub_diag.span?;
                match sub_diag.style {
                    LabelStyle::Primary => {
                        cs_diag::Label::new(cs_diag::LabelStyle::Primary, span.file, span.range)
                    }
                    LabelStyle::Secondary => {
                        cs_diag::Label::new(cs_diag::LabelStyle::Secondary, span.file, span.range)
                    }
                }
                .with_message(sub_diag.message)
                .into()
            })
            .collect();

        cs_diag::Diagnostic {
            severity,
            code,
            message,
            labels,
            notes: complete.notes,
        }
    }
}

fn file_line_starts(db: &dyn SpannedHirAnalysisDb, file: File) -> Vec<usize> {
    codespan_reporting::files::line_starts(file.text(db)).collect()
}

struct CsDbWrapper<'a>(&'a dyn SpannedHirAnalysisDb);

impl<'db> cs_files::Files<'db> for CsDbWrapper<'db> {
    type FileId = File;
    type Name = &'db Utf8Path;
    type Source = &'db str;

    fn name(&'db self, file_id: Self::FileId) -> Result<Self::Name, cs_files::Error> {
        match file_id.path(self.0) {
            Some(path) => Ok(path.as_path()),
            None => Err(cs_files::Error::FileMissing),
        }
    }

    fn source(&'db self, file_id: Self::FileId) -> Result<Self::Source, cs_files::Error> {
        Ok(file_id.text(self.0))
    }

    fn line_index(
        &'db self,
        file_id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, cs_files::Error> {
        let starts = file_line_starts(self.0, file_id);
        Ok(starts
            .binary_search(&byte_index)
            .unwrap_or_else(|next_line| next_line - 1))
    }

    fn line_range(
        &'db self,
        file_id: Self::FileId,
        line_index: usize,
    ) -> Result<Range<usize>, cs_files::Error> {
        let line_starts = file_line_starts(self.0, file_id);

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

        Ok(Range { start, end })
    }
}

// --- End codespan helpers ---

pub fn emit_diagnostics<'db, DB>(db: &'db DB, diags: &[Box<dyn DiagnosticVoucher + 'db>])
where
    DB: SpannedHirAnalysisDb + InputDb,
{
    let writer = BufferWriter::stderr(ColorChoice::Auto);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    for diag in finalized_diags(db, diags) {
        term::emit(&mut buffer, &config, &CsDbWrapper(db), &diag.to_cs(db)).unwrap();
    }

    writer
        .print(&buffer)
        .expect("Failed to write diagnostics to stderr");
}

pub fn format_diagnostics<'db, DB>(
    db: &'db DB,
    diags: &[Box<dyn DiagnosticVoucher + 'db>],
) -> String
where
    DB: SpannedHirAnalysisDb + InputDb,
{
    let writer = BufferWriter::stderr(ColorChoice::Never);
    let mut buffer = writer.buffer();
    let config = term::Config::default();

    for diag in finalized_diags(db, diags) {
        term::emit(&mut buffer, &config, &CsDbWrapper(db), &diag.to_cs(db)).unwrap();
    }

    trim_trailing_line_whitespace(std::str::from_utf8(buffer.as_slice()).unwrap())
}

fn finalized_diags<'db, DB>(
    db: &'db DB,
    diags: &[Box<dyn DiagnosticVoucher + 'db>],
) -> Vec<common::diagnostics::CompleteDiagnostic>
where
    DB: SpannedHirAnalysisDb + InputDb,
{
    let mut complete_diags: Vec<_> = diags.iter().map(|diag| diag.to_complete(db)).collect();
    complete_diags.sort_by(cmp_complete_diagnostics);
    complete_diags
}

define_input_db!(HirAnalysisTestDb);

pub fn find_contract<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    name: &str,
) -> Contract<'db> {
    top_mod
        .children_non_nested(db)
        .find_map(|item| match item {
            ItemKind::Contract(contract)
                if contract
                    .name(db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(db) == name) =>
            {
                Some(contract)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing contract `{name}`"))
}

pub fn find_func<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
    name: &str,
) -> Func<'db> {
    top_mod
        .children_non_nested(db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func
                    .name(db)
                    .to_opt()
                    .is_some_and(|ident| ident.data(db) == name) =>
            {
                Some(func)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing function `{name}`"))
}

// https://github.com/rust-lang/rust/issues/46379
#[allow(dead_code)]
impl HirAnalysisTestDb {
    pub fn new_stand_alone(&mut self, file_path: Utf8PathBuf, text: &str) -> File {
        let file_path =
            absolute_utf8(file_path.as_path()).expect("resolve absolute standalone path");
        let index = self.workspace();
        self.initialize_builtin_core();
        self.initialize_builtin_std();
        index.touch(
            self,
            <Url as UrlExt>::from_file_path_lossy(file_path.as_std_path()),
            Some(text.to_string()),
        )
    }

    pub fn new_trusted_effect_handle_module(&mut self, file_path: Utf8PathBuf, text: &str) -> File {
        self.initialize_builtin_core();
        self.initialize_builtin_std();
        let base_url = Url::parse("test-effect-handle:/").expect("valid trusted handle test URL");
        self.workspace().touch(
            self,
            base_url.join("fe.toml").expect("valid test std config URL"),
            Some(
                r#"
[ingot]
name = "effect-handle-test"
version = "0.0.0"
"#
                .to_string(),
            ),
        );
        self.dependency_graph().add_dependency(
            self,
            &base_url,
            &Url::parse(BUILTIN_STD_BASE_URL).expect("valid builtin std URL"),
            "std".into(),
            Default::default(),
        );
        let url = base_url
            .join(&format!("src/__test__/{}", file_path.as_str()))
            .expect("valid std test module URL");
        self.workspace().touch(self, url, Some(text.to_string()))
    }

    pub fn top_mod(&self, input: File) -> (TopLevelMod<'_>, HirPropertyFormatter<'_>) {
        let mut prop_formatter = HirPropertyFormatter::default();
        let top_mod = self.register_file(&mut prop_formatter, input);
        (top_mod, prop_formatter)
    }

    pub fn assert_no_diags(&self, top_mod: TopLevelMod) {
        let mut manager = initialize_test_analysis_pass();
        let diags = manager.run_on_module(self, top_mod);

        if !diags.is_empty() {
            eprintln!("{}", format_diagnostics(self, &diags));

            panic!("this module contains errors");
        }
    }

    pub fn run_on_top_mod<'db>(
        &'db self,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut manager = initialize_test_analysis_pass();
        manager.run_on_module(self, top_mod)
    }

    pub fn run_on_ingot<'db>(
        &'db self,
        ingot: Ingot<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut manager = initialize_test_analysis_pass();
        let tree = module_tree(self, ingot);
        manager.run_on_module_tree(self, tree)
    }

    fn register_file<'db>(
        &'db self,
        prop_formatter: &mut HirPropertyFormatter<'db>,
        input_file: File,
    ) -> TopLevelMod<'db> {
        let top_mod = lower::map_file_to_mod(self, input_file);
        let path = input_file
            .path(self)
            .as_ref()
            .expect("Failed to get file path");
        let text = input_file.text(self);
        prop_formatter.register_top_mod(path.as_str(), text, top_mod);
        top_mod
    }
}

pub struct HirPropertyFormatter<'db> {
    // https://github.com/rust-lang/rust/issues/46379
    #[allow(dead_code)]
    properties: IndexMap<TopLevelMod<'db>, Vec<(String, DynLazySpan<'db>)>>,
    top_mod_to_file: FxHashMap<TopLevelMod<'db>, CodeSpanFileId>,
    code_span_files: SimpleFiles<String, String>,
}

// https://github.com/rust-lang/rust/issues/46379
#[allow(dead_code)]
impl<'db> HirPropertyFormatter<'db> {
    pub fn push_prop(&mut self, top_mod: TopLevelMod<'db>, span: DynLazySpan<'db>, prop: String) {
        self.properties
            .entry(top_mod)
            .or_default()
            .push((prop, span));
    }

    pub fn finish(&mut self, db: &'db dyn SpannedHirDb) -> String {
        let writer = BufferWriter::stderr(ColorChoice::Never);
        let mut buffer = writer.buffer();
        let config = term::Config::default();

        for top_mod in self.top_mod_to_file.keys() {
            if !self.properties.contains_key(top_mod) {
                continue;
            }

            let diags =
                self.properties[top_mod]
                    .iter()
                    .fold(BTreeMap::new(), |mut diags, (prop, span)| {
                        let span = span.resolve(db).unwrap();
                        diags
                            .entry((span.file, span.range.start(), span.range.end()))
                            .or_insert_with(Vec::new)
                            .push(prop.clone());
                        diags
                    });

            for ((_, start, end), mut props) in diags {
                props.sort_unstable();
                props.dedup();
                let diag =
                    self.property_to_diag(*top_mod, &props.join(" | "), start.into()..end.into());
                term::emit(&mut buffer, &config, &self.code_span_files, &diag).unwrap();
            }
        }

        trim_trailing_line_whitespace(std::str::from_utf8(buffer.as_slice()).unwrap())
    }

    fn property_to_diag(
        &self,
        top_mod: TopLevelMod<'db>,
        prop: &str,
        range: Range<usize>,
    ) -> Diagnostic<usize> {
        let file_id = self.top_mod_to_file[&top_mod];
        Diagnostic::note().with_labels(vec![Label::primary(file_id, range).with_message(prop)])
    }

    pub fn register_top_mod(&mut self, path: &str, text: &str, top_mod: TopLevelMod<'db>) {
        let file_id = self.code_span_files.add(path.to_string(), text.to_string());
        self.top_mod_to_file.insert(top_mod, file_id);
    }
}

impl Default for HirPropertyFormatter<'_> {
    fn default() -> Self {
        Self {
            properties: Default::default(),
            top_mod_to_file: Default::default(),
            code_span_files: SimpleFiles::new(),
        }
    }
}

/// The focused HIR-test pipeline, which deliberately stops before contract
/// policy checks so type-check fixtures can contain partial contracts. Tests
/// of user-facing diagnostics must use [`crate::analysis::initialize_analysis_pass`].
pub fn initialize_test_analysis_pass() -> AnalysisPassManager {
    initialize_pre_contract_analysis_pass()
}

// --- Simple test database for unit tests ---

define_input_db!(TestDb);

#[allow(dead_code)]
impl TestDb {
    pub fn parse_source(&self, file: File) -> &ScopeGraph<'_> {
        let top_mod = map_file_to_mod(self, file);
        scope_graph(self, top_mod)
    }

    /// Parses the given source text and returns the first inner item in the file.
    pub fn expect_item<'db, T>(&'db self, file: File) -> T
    where
        ItemKind<'db>: TryInto<T, Error = TryIntoError<ItemKind<'db>>>,
    {
        let tree = self.parse_source(file);
        tree.items_dfs(self)
            .find_map(|it| it.try_into().ok())
            .unwrap()
    }

    pub fn expect_items<'db, T>(&'db self, file: File) -> Vec<T>
    where
        ItemKind<'db>: TryInto<T, Error = TryIntoError<ItemKind<'db>>>,
    {
        let tree = self.parse_source(file);
        tree.items_dfs(self)
            .filter_map(|it| it.try_into().ok())
            .collect()
    }

    pub fn text_at(&self, top_mod: TopLevelMod, span: &impl LazySpan) -> &str {
        let range = span.resolve(self).unwrap().range;
        let file = top_mod.file(self);
        let text = file.text(self);
        &text[range.start().into()..range.end().into()]
    }

    pub fn standalone_file(&mut self, text: &str) -> File {
        self.workspace().touch(
            self,
            Url::parse("file:///hir_test/test_file.fe").unwrap(),
            Some(text.into()),
        )
    }
}
