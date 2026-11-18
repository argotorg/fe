//! Generation for derive headers: finds the headers of a source file or of a
//! package with its dependencies, evaluates each provider at compile time
//! and admits the generated items into one prepared view, which checks, builds
//! and the language server read in place of the written file.
use crate::DriverDataBase;
use common::{InputDb, file::File, ingot::IngotKind};
use hir::{
    analysis::{
        diagnostics::DiagnosticVoucher,
        semantic::{CtfeError, EvalFailure, EvalOutcome, SemConstValue, SemOrigin},
        ty::{
            corelib::resolve_lib_type_path,
            ty_check::{BodyOwner, check_func_body, checked_body_demands},
            ty_def::TyId,
        },
    },
    core::meta::RecordInput,
    span::LazySpan,
};
use parser::ast::{self, prelude::*};
use salsa::Setter;

#[derive(Clone, Debug)]
pub struct GenerationError {
    pub file: url::Url,
    pub range: parser::TextRange,
    pub cause: String,
}

impl std::fmt::Display for GenerationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}..{}: {}",
            self.file,
            u32::from(self.range.start()),
            u32::from(self.range.end()),
            self.cause
        )
    }
}

/// The one view every provider runs in, whatever channel requested it.
const PROVIDER_VIEW_RULE: &str = "a provider runs against authored text and the implementation headers an author wrote; what generation produces in its own package is not visible to it. Share an ordinary helper function between the two providers instead";

/// Provider diagnostics that may come from reading something generation has
/// not produced in the input view carry the rule that fixes the view.
fn with_view_note(mut error: GenerationError) -> GenerationError {
    error.cause = format!("{}\nnote: {PROVIDER_VIEW_RULE}", error.cause);
    error
}

/// Whether `diagnostic` lies in written code: its first location is outside
/// every quotation of its file. Generated code is spliced from quotations.
fn in_written_code(
    db: &DriverDataBase,
    diagnostic: &common::diagnostics::CompleteDiagnostic,
) -> bool {
    let Some(span) = diagnostic.first_span() else {
        return false;
    };
    let (green, _) =
        parser::parse_source_file(span.file.text(db).as_str(), parser::RecoveryMode::Recover);
    let root = ast::Root::cast(parser::SyntaxNode::new_root(green)).unwrap();
    !root
        .syntax()
        .descendants()
        .filter_map(ast::QuoteExpr::cast)
        .any(|quote| quote.syntax().text_range().contains_range(span.range))
}

fn diagnostic_error(
    db: &DriverDataBase,
    diagnostics: &[common::diagnostics::CompleteDiagnostic],
    mut fallback: GenerationError,
) -> GenerationError {
    if let Some(span) = diagnostics
        .iter()
        .find_map(|diagnostic| diagnostic.first_span())
    {
        fallback.file = span.file.url(db).unwrap_or(fallback.file);
        fallback.range = span.range;
    }
    // Labels carry expected/given types even when a source transition lacks a
    // printable span. Do not let terminal rendering erase the original cause.
    let details = diagnostics
        .iter()
        .flat_map(|diagnostic| {
            std::iter::once(diagnostic.message.clone())
                .chain(
                    diagnostic
                        .sub_diagnostics
                        .iter()
                        .map(|label| label.message.clone()),
                )
                .chain(diagnostic.notes.iter().cloned())
        })
        .filter(|message| !message.is_empty())
        .collect::<Vec<_>>()
        .join("; ");
    fallback.cause = format!(
        "{}: {}\n{}",
        fallback.cause,
        details,
        db.format_complete_diagnostics(diagnostics)
    );
    fallback
}

fn body_errors(
    db: &DriverDataBase,
    function: hir::hir_def::Func<'_>,
) -> Vec<common::diagnostics::CompleteDiagnostic> {
    check_func_body(db, function)
        .0
        .iter()
        .map(|diagnostic| diagnostic.to_complete(db))
        .filter(|diagnostic| diagnostic.severity == common::diagnostics::Severity::Error)
        .collect()
}

fn evaluation_error<'db>(
    db: &'db DriverDataBase,
    mut owner: BodyOwner<'db>,
    error: &CtfeError<'db>,
    fallback: GenerationError,
) -> GenerationError {
    let mut error = error;
    while let CtfeError::CalleeError { callee, source, .. } = error {
        owner = callee.key(db).owner(db);
        error = source;
    }
    if let BodyOwner::Func(function, _) = owner {
        let complete = body_errors(db, function);
        if !complete.is_empty() {
            return with_view_note(diagnostic_error(
                db,
                &complete,
                GenerationError {
                    cause: "authored const helper diagnostics".into(),
                    ..fallback
                },
            ));
        }
    }
    let (origin, cause) = match error {
        CtfeError::DivisionByZero { origin } => {
            (*origin, "division by zero in const context".to_string())
        }
        CtfeError::ArithmeticOverflow { origin } => {
            (*origin, "arithmetic overflow in const context".to_string())
        }
        CtfeError::InvalidOperation { origin, message } => (*origin, message.clone()),
        CtfeError::OutOfBounds { origin } => {
            (*origin, "constant index outside record or aggregate".into())
        }
        CtfeError::NonConstCall { origin } => (
            *origin,
            "cannot call non-const function in const context".to_string(),
        ),
        CtfeError::InvalidBorrow { origin } => (
            *origin,
            "invalid borrow in const evaluation context".to_string(),
        ),
        CtfeError::InvalidProviderUse { origin } => (
            *origin,
            "invalid effect provider use in const context".to_string(),
        ),
        CtfeError::InvalidBody { origin } => (
            *origin,
            "invalid body in const evaluation context".to_string(),
        ),
        CtfeError::NotConstEvaluable { origin } => (
            *origin,
            "expression cannot be evaluated at compile time".to_string(),
        ),
        CtfeError::RecursiveConst { origin } => {
            (*origin, "recursive const evaluation detected".to_string())
        }
        CtfeError::StepLimitExceeded { origin } => {
            (*origin, "existing CTFE step limit exceeded".into())
        }
        CtfeError::RecursionLimitExceeded { origin } => {
            (*origin, "existing CTFE recursion limit exceeded".into())
        }
        _ => return fallback,
    };
    let Some(body) = owner.body(db) else {
        return fallback;
    };
    let SemOrigin::Expr(expr) = origin else {
        return fallback;
    };
    let Some(span) = expr.span(body).resolve(db) else {
        return fallback;
    };
    GenerationError {
        file: span.file.url(db).unwrap_or(fallback.file),
        range: span.range,
        cause,
    }
}

/// The authored requests of one source file. Explicit requests are validated
/// against the authored view and kept as syntax; each is frozen later, against
/// the output view of the transaction that admits it.
struct FileRequests {
    url: url::Url,
    /// Each trait header `impl Trait for Type with (Provider)`, with its range
    /// and the range of its provider path.
    explicit_requests: Vec<(ast::ImplTrait, parser::TextRange, parser::TextRange)>,
    /// Each inherent header `impl Type with (Provider)`, with its range and
    /// the range of its provider path.
    inherent_requests: Vec<(ast::Impl, parser::TextRange, parser::TextRange)>,
    has_scoped_opening: bool,
    root_range: parser::TextRange,
}

impl FileRequests {
    fn is_empty(&self) -> bool {
        self.explicit_requests.is_empty() && self.inherent_requests.is_empty()
    }
}

/// Nothing is published when any input request fails. The returned database
/// owns the unchanged source text and the complete frozen output plan.
pub fn prepare_file(
    db: &DriverDataBase,
    file: File,
) -> Result<Option<DriverDataBase>, GenerationError> {
    prepare_file_within(db, file, MAX_PREPARATION_ROUNDS)
}

/// `prepare_file` with the number of rounds a transaction may build.
fn prepare_file_within(
    db: &DriverDataBase,
    file: File,
    max_rounds: usize,
) -> Result<Option<DriverDataBase>, GenerationError> {
    let url = file.url(db).expect("indexed generation input");
    let error = |range, cause| GenerationError {
        file: url.clone(),
        range,
        cause,
    };
    if !db.generation_inputs().functions(db).is_empty()
        || !db.generation_inputs().requests(db).is_empty()
    {
        return Err(error(
            parser::TextRange::empty(0.into()),
            "generation requires the authored input view; output is checked once".into(),
        ));
    }
    let Some(syntax) = scan_syntax(&url, file.text(db))? else {
        return Ok(None);
    };
    // A standalone file has no dependencies whose output could be read, so the
    // authored view shows its openings.
    let Some(scan) = scan_file(db, file, syntax, true)? else {
        return Ok(None);
    };
    // The one view every provider is admitted and evaluated in: the authored
    // text, with the implementation headers an author wrote counted as facts.
    let basis = ViewBasis::Standalone(file);
    let input_db = input_view(db, &basis, &[]);
    let db = &input_db;
    let file = db
        .workspace()
        .get(db, &url)
        .expect("the input view holds the authored file");
    let found = file_requests(db, file, scan)?;
    // Lowering dropped every header of the file: nothing is requested.
    if found.is_empty() && !found.has_scoped_opening {
        return Ok(None);
    }
    let root_range = found.root_range;
    admit_requests(
        db,
        &Transaction {
            basis: ViewBasis::Standalone(file),
            modules: &[url],
            standalone_root: Some(root_range),
            max_rounds,
        },
        Vec::new(),
        &[found],
    )
    .map(|(output, _)| Some(output))
}

/// One ingot to prepare: its authored source files and the ingots it depends
/// on (directly or not) among the ones to prepare.
struct IngotPlan {
    files: Vec<url::Url>,
    /// Indices into the plan list, ascending. Dependencies come first.
    dependencies: Vec<usize>,
}

/// The ingots containing `roots` and the ingots they depend on, dependencies
/// first. The builtin libraries carry no generation requests; standalone files
/// are prepared one at a time by `prepare_file`.
fn plan_ingots(db: &DriverDataBase, roots: &[url::Url]) -> Vec<IngotPlan> {
    use hir::hir_def::HirIngot;
    fn visit<'db>(
        db: &'db DriverDataBase,
        ingot: common::ingot::Ingot<'db>,
        index: &mut std::collections::HashMap<url::Url, usize>,
        in_progress: &mut std::collections::HashSet<url::Url>,
        plans: &mut Vec<IngotPlan>,
    ) -> Option<usize> {
        if matches!(
            ingot.kind(db),
            IngotKind::Core | IngotKind::Std | IngotKind::StandAlone
        ) {
            return None;
        }
        let base = ingot.base(db).clone();
        if let Some(&done) = index.get(&base) {
            return Some(done);
        }
        // Dependency cycles are refused when the workspace is resolved; do not
        // follow one here.
        if !in_progress.insert(base.clone()) {
            return None;
        }
        let mut dependencies = std::collections::BTreeSet::new();
        for (_, dependency) in ingot.resolved_external_ingots(db).iter() {
            if let Some(done) = visit(db, *dependency, index, in_progress, plans) {
                dependencies.insert(done);
                dependencies.extend(plans[done].dependencies.iter().copied());
            }
        }
        let files = ingot
            .files(db)
            .iter()
            .filter(|(_, file)| matches!(file.kind(db), Some(common::file::IngotFileKind::Source)))
            .map(|(url, _)| url.clone())
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        plans.push(IngotPlan {
            files,
            dependencies: dependencies.into_iter().collect(),
        });
        let done = plans.len() - 1;
        index.insert(base, done);
        Some(done)
    }
    let mut index = std::collections::HashMap::new();
    let mut in_progress = std::collections::HashSet::new();
    let mut plans = Vec::new();
    for root in roots {
        if let Some(ingot) = db.workspace().containing_ingot(db, root.clone()) {
            visit(db, ingot, &mut index, &mut in_progress, &mut plans);
        }
    }
    plans
}

/// Prepare every authored request of the given ingots and of the ingots they
/// depend on, one ingot at a time, dependencies first. Each ingot is prepared
/// once, in a view that holds the sealed output of the ingots it depends on as
/// ordinary code, and its providers read nothing else that generation
/// produced. The authored database is unchanged, also on failure. `None`
/// means there is nothing to generate, so consumers keep using the authored
/// view.
///
/// Requests are evaluated against authored input exactly as for a standalone
/// file; scoped openings use the same dependency frontier. Output diagnostics
/// stay in the returned view, where each consumer reports them with their own
/// spans, instead of being collapsed into one preparation error.
pub fn prepare_ingots(
    db: &DriverDataBase,
    roots: &[url::Url],
) -> Result<Option<DriverDataBase>, GenerationError> {
    let Some(first) = roots.first() else {
        return Ok(None);
    };
    if !db.generation_inputs().functions(db).is_empty()
        || !db.generation_inputs().requests(db).is_empty()
    {
        return Err(GenerationError {
            file: first.clone(),
            range: parser::TextRange::empty(0.into()),
            cause: "generation requires the authored input view; output is checked once".into(),
        });
    }
    let plans = plan_ingots(db, roots);
    // The sealed output of each prepared ingot, by its index in `plans`. A
    // seal lasts for this preparation only: it is the output of the ingot's
    // text, of the seals of its dependencies and of the settings the database
    // holds now, and none of these is recorded with it. A seal kept for a
    // later call would need a key that holds all of them, such as the hashes of
    // the files, the keys of the dependencies' seals and the settings.
    let mut seals: Vec<Option<Vec<common::generation::FrozenGenerationRequest>>> =
        vec![None; plans.len()];
    let mut last = None;
    for (index, plan) in plans.iter().enumerate() {
        // A first look at the text decides which ingots can need preparing
        // before any view is copied.
        let mut syntaxes = Vec::new();
        for url in &plan.files {
            let file = db
                .workspace()
                .get(db, url)
                .expect("a planned ingot file is in the database");
            if let Some(syntax) = scan_syntax(url, file.text(db))? {
                syntaxes.push((url.clone(), syntax));
            }
        }
        if syntaxes.is_empty() {
            continue;
        }
        let mut sealed = Vec::new();
        for &dependency in &plan.dependencies {
            if let Some(requests) = &seals[dependency] {
                sealed.extend(requests.iter().cloned());
            }
        }
        // The one view this ingot's providers are admitted and evaluated in:
        // authored text, the sealed output of its dependencies, and the
        // implementation headers an author wrote counted as facts. The `with`
        // blocks that need a provider are found in it too, since a block can
        // name what a dependency generated. Without a sealed dependency and
        // without a request of its own, the authored view shows the same
        // blocks and no copy is made until a block needs preparing.
        let wants_view =
            !sealed.is_empty() || syntaxes.iter().any(|(_, syntax)| syntax.has_requests);
        let mut input_db = wants_view.then(|| input_view(db, &ViewBasis::Package, &sealed));
        let mut scans = Vec::new();
        for (url, syntax) in syntaxes {
            let scanned_in = input_db.as_ref().unwrap_or(db);
            let file = scanned_in
                .workspace()
                .get(scanned_in, &url)
                .expect("the view holds every authored file");
            if let Some(scan) = scan_file(scanned_in, file, syntax, false)? {
                scans.push((url, scan));
            }
        }
        if scans.is_empty() {
            continue;
        }
        let input_db = input_db
            .take()
            .unwrap_or_else(|| input_view(db, &ViewBasis::Package, &sealed));
        let view = &input_db;
        let mut found_requests = Vec::new();
        for (url, scan) in scans {
            let file = view
                .workspace()
                .get(view, &url)
                .expect("the input view holds every authored file");
            found_requests.push(file_requests(view, file, scan)?);
        }
        let (output, own) = admit_requests(
            view,
            &Transaction {
                basis: ViewBasis::Package,
                modules: &plan.files,
                standalone_root: None,
                max_rounds: MAX_PREPARATION_ROUNDS,
            },
            sealed,
            &found_requests,
        )?;
        seals[index] = Some(own);
        last = Some(output);
    }
    // No ingot needed preparing: consumers keep using the authored view.
    let Some(output) = last else {
        return Ok(None);
    };
    let total = seals.iter().flatten().map(Vec::len).sum::<usize>();
    // The last view holds the output of its own ingot and of the ones it
    // depends on. Other ingots (several roots) need a view of everything.
    if output.generation_inputs().requests(&output).len() == total {
        return Ok(Some(output));
    }
    let mut everything = Vec::new();
    for requests in seals.iter().flatten() {
        everything.extend(requests.iter().cloned());
    }
    Ok(Some(prepared_view(db, &ViewBasis::Package, &everything)))
}

/// Replace an authored database with the prepared view of `roots` when they
/// request generation, so every later consumer reads the generated program.
pub fn prepare_ingots_in_place(
    db: &mut DriverDataBase,
    roots: &[url::Url],
) -> Result<(), GenerationError> {
    if let Some(prepared) = prepare_ingots(db, roots)? {
        *db = prepared;
    }
    Ok(())
}

/// What a first look at the text of an authored file finds.
struct FileSyntax {
    root: ast::Root,
    /// The file writes a request, a trait or inherent derive header, outside
    /// quotations.
    has_requests: bool,
    /// The file parses and has `with` blocks, one of which may open a provider.
    may_open_provider: bool,
}

/// What a first look at an authored file finds: its syntax, and whether a
/// `with` block of it needs a provider's output.
struct FileScan {
    root: ast::Root,
    has_scoped_opening: bool,
}

/// Look for generation syntax in the text of one authored file, before any
/// view is built. `None` means the file contains none outside quotations, so
/// nothing needs preparing.
fn scan_syntax(url: &url::Url, text: &str) -> Result<Option<FileSyntax>, GenerationError> {
    // Every request form spells `with`; other files need no parse.
    if !text.contains("with") {
        return Ok(None);
    }
    let (green, parse_errors) = parser::parse_source_file(text, parser::RecoveryMode::Recover);
    let root = ast::Root::cast(parser::SyntaxNode::new_root(green)).unwrap();
    let has_scoped_syntax = root
        .syntax()
        .descendants()
        .any(|node| ast::WithExpr::cast(node).is_some());
    let outside_quotation = |node: &parser::SyntaxNode| {
        !node
            .ancestors()
            .any(|node| ast::QuoteExpr::cast(node).is_some())
    };
    let has_requests = root
        .syntax()
        .descendants()
        .filter_map(ast::ImplTrait::cast)
        .any(|request| request.derive_provider().is_some() && outside_quotation(request.syntax()))
        || root
            .syntax()
            .descendants()
            .filter_map(ast::Impl::cast)
            .any(|request| {
                request.derive_provider().is_some() && outside_quotation(request.syntax())
            });
    if !has_requests {
        // A file with parse errors is not searched for provider openings.
        return Ok(
            (has_scoped_syntax && parse_errors.is_empty()).then_some(FileSyntax {
                root,
                has_requests,
                may_open_provider: true,
            }),
        );
    }
    if !parse_errors.is_empty() {
        return Err(GenerationError {
            file: url.clone(),
            range: parser::TextRange::empty(0.into()),
            cause: format!("authored parse errors: {parse_errors:?}"),
        });
    }
    Ok(Some(FileSyntax {
        root,
        has_requests,
        may_open_provider: has_scoped_syntax,
    }))
}

/// Starts the analysis of `module` in `db` with the growing-cycle search of
/// its ingot's type definitions. A field type whose array length calls a
/// const function and the body of that function can depend on each other
/// through that search, and salsa resolves the cycle only from the query
/// that was entered first. The search has a start value for it (no type grows
/// until the fields are known); the typed body of a function instance has
/// none. Generation reads bodies, written types and record fields before the
/// analysis passes run, so every database it reads enters the cycle here.
fn analyze_type_definitions_first<'db>(
    db: &'db DriverDataBase,
    module: hir::hir_def::TopLevelMod<'db>,
) {
    let _ = hir::semantic::ingot_growing_cycles(db, module.ingot(db));
}

/// Decide whether an authored file needs preparing: it writes a request, or a
/// `with` block of it opens a provider whose implementation preparation must
/// produce. The demands of a block are read in `db`, which must be the view
/// the file's requests are evaluated in: a block that names output a
/// dependency generated has a demand only where that output is visible.
fn scan_file(
    db: &DriverDataBase,
    file: File,
    syntax: FileSyntax,
    standalone_only: bool,
) -> Result<Option<FileScan>, GenerationError> {
    analyze_type_definitions_first(db, db.top_mod(file));
    // Ordinary lexical implementation choices do not require preparation or
    // the standalone generation adapter. Only actual provider demands do.
    let has_scoped_opening = syntax.may_open_provider
        && !hir::analysis::ty::trait_resolution::generation::preparation_demands(
            db,
            db.top_mod(file),
        )
        .is_empty();
    if !syntax.has_requests && !has_scoped_opening {
        return Ok(None);
    }
    if standalone_only
        && file
            .containing_ingot(db)
            .is_none_or(|ingot| ingot.kind(db) != IngotKind::StandAlone)
    {
        return Err(GenerationError {
            file: file.url(db).expect("indexed generation input"),
            range: parser::TextRange::empty(0.into()),
            cause: "bounded generation currently requires a standalone source file".into(),
        });
    }
    Ok(Some(FileScan {
        root: syntax.root,
        has_scoped_opening,
    }))
}

/// Collect and validate the explicit requests of one scanned file in the view the
/// providers run in.
fn file_requests(
    db: &DriverDataBase,
    file: File,
    scan: FileScan,
) -> Result<FileRequests, GenerationError> {
    let url = file.url(db).expect("indexed generation input");
    let error = |range, cause| GenerationError {
        file: url.clone(),
        range,
        cause,
    };
    let FileScan {
        root,
        has_scoped_opening,
    } = scan;
    let module = db.top_mod(file);
    analyze_type_definitions_first(db, module);
    let mut explicit_requests = Vec::new();
    for request_ast in root.syntax().descendants().filter_map(ast::ImplTrait::cast) {
        let Some(path) = request_ast.derive_provider() else {
            continue;
        };
        let range = request_ast.syntax().text_range();
        if request_ast
            .syntax()
            .ancestors()
            .any(|node| ast::QuoteExpr::cast(node).is_some())
        {
            continue;
        }
        // Lowering drops a header whose `cfg` does not hold; it is no request.
        if !hir::core::lower::syntax_item_cfg_holds(db, module, request_ast.syntax()) {
            continue;
        }
        // A header in any module's item list is an ordinary module member, so
        // it sees that module's private names. Block-local headers are a
        // pending interface, never ignored.
        if request_ast
            .syntax()
            .ancestors()
            .any(|node| ast::BlockExpr::cast(node).is_some())
        {
            return Err(error(
                range,
                "a derive header inside a function body is not supported; declare the header in a module".into(),
            ));
        }
        let request = hir::core::meta::authored_derive_request(db, module, &request_ast)
            .ok_or_else(|| {
                error(
                    range,
                    "cannot recover authored derive header identity".into(),
                )
            })?;
        header_provider_ty(
            db,
            request.scope(),
            request.derive_provider(db),
            path.syntax().text_range(),
            &error,
        )?;
        explicit_requests.push((request_ast, range, path.syntax().text_range()));
    }
    let mut inherent_requests = Vec::new();
    for request_ast in root.syntax().descendants().filter_map(ast::Impl::cast) {
        let Some(path) = request_ast.derive_provider() else {
            continue;
        };
        let range = request_ast.syntax().text_range();
        if request_ast
            .syntax()
            .ancestors()
            .any(|node| ast::QuoteExpr::cast(node).is_some())
        {
            continue;
        }
        // Lowering drops a header whose `cfg` does not hold; it is no request.
        if !hir::core::lower::syntax_item_cfg_holds(db, module, request_ast.syntax()) {
            continue;
        }
        // As for a trait header: a module member, never a block-local item.
        if request_ast
            .syntax()
            .ancestors()
            .any(|node| ast::BlockExpr::cast(node).is_some())
        {
            return Err(error(
                range,
                "a derive header inside a function body is not supported; declare the header in a module".into(),
            ));
        }
        let request = hir::core::meta::authored_inherent_derive_request(db, module, &request_ast)
            .ok_or_else(|| {
            error(
                range,
                "cannot recover authored derive header identity".into(),
            )
        })?;
        let provider = header_provider_ty(
            db,
            request.scope(),
            request.derive_provider(db),
            path.syntax().text_range(),
            &error,
        );
        // The header's own errors, such as a target or bound that names
        // nothing, are reported as for any implementation, together with a
        // provider that does not resolve, and not hidden behind it.
        let header_errors = inherent_header_errors(db, request);
        if !header_errors.is_empty() {
            let cause = match provider {
                Err(failure) => failure.cause,
                Ok(_) => "the inherent header has errors".into(),
            };
            return Err(diagnostic_error(db, &header_errors, error(range, cause)));
        }
        provider?;
        inherent_requests.push((request_ast, range, path.syntax().text_range()));
    }
    Ok(FileRequests {
        url: url.clone(),
        explicit_requests,
        inherent_requests,
        has_scoped_opening,
        root_range: root.syntax().text_range(),
    })
}

/// The errors that the analysis of an inherent header reports about the header
/// itself: its target type, its binders and its bounds. The header has no
/// members before preparation, so these are all its diagnostics.
fn inherent_header_errors(
    db: &DriverDataBase,
    request: hir::hir_def::Impl<'_>,
) -> Vec<common::diagnostics::CompleteDiagnostic> {
    use hir::diagnosable::Diagnosable;
    request
        .diags(db)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(db))
        .filter(|diagnostic| diagnostic.severity == common::diagnostics::Severity::Error)
        .collect()
}

/// The provider type that a derive header names: `P` in
/// `impl Trait for Type with (P)` or `impl Type with (P)`. `provider_path` is
/// the path as written and lowered with the header, including its arguments
/// (`Scaled<10>`); it resolves where the header is written, and the provider
/// must be visible there.
fn header_provider_ty<'db>(
    db: &'db DriverDataBase,
    scope: hir::hir_def::scope_graph::ScopeId<'db>,
    provider_path: Option<hir::hir_def::PathId<'db>>,
    path_range: parser::TextRange,
    error: &impl Fn(parser::TextRange, String) -> GenerationError,
) -> Result<TyId<'db>, GenerationError> {
    let provider_path =
        provider_path.ok_or_else(|| error(path_range, "invalid derive provider path".into()))?;
    let resolved = hir::analysis::name_resolution::resolve_path(
        db,
        provider_path,
        scope,
        hir::analysis::ty::trait_resolution::PredicateListId::empty_list(db),
        true,
    )
    .map_err(|_| error(path_range, "cannot resolve the derive provider type".into()))?;
    if let hir::analysis::name_resolution::PathRes::TyAlias(alias, _) = &resolved
        && !hir::analysis::name_resolution::is_scope_visible_from(db, alias.alias.scope(), scope)
    {
        return Err(error(
            path_range,
            "the derive provider alias is not visible".into(),
        ));
    }
    let (hir::analysis::name_resolution::PathRes::Ty(provider_ty)
    | hir::analysis::name_resolution::PathRes::TyAlias(_, provider_ty)) = resolved
    else {
        return Err(error(
            path_range,
            "the derive provider must name a type implementing Derive<G>".into(),
        ));
    };
    if let Some(provider_scope) = provider_ty.as_scope(db)
        && !hir::analysis::name_resolution::is_scope_visible_from(db, provider_scope, scope)
    {
        return Err(error(
            path_range,
            "the derive provider type is not visible".into(),
        ));
    }
    Ok(provider_ty)
}

/// Where a prepared view takes its authored inputs from.
enum ViewBasis {
    /// Only the one standalone file, over the builtin libraries.
    Standalone(File),
    /// Every authored input of the database, including dependencies.
    Package,
}

/// The most views one transaction builds. The providers of the derive headers
/// run before the first view is built, so every view holds their output. Every
/// round serves each request the previous round's output wants, so independent
/// requests share a round. A provider never waits for another's output and
/// generated code cannot open a provider, so a program needs the round that
/// serves its requests and the round that finds nothing pending; the limit is
/// a guard.
const MAX_PREPARATION_ROUNDS: usize = 128;

/// What one preparation transaction prepares: the authored modules, and how it
/// publishes its view.
struct Transaction<'a> {
    basis: ViewBasis,
    modules: &'a [url::Url],
    standalone_root: Option<parser::TextRange>,
    max_rounds: usize,
}

/// One preparation transaction over its modules. `sealed` is the output of the
/// ingots the modules depend on, already prepared and part of every view;
/// `authored` are the requests the modules write. The providers of the derive
/// headers of every file run first, and every view holds their output; each
/// round then discovers scoped demands in the output view and serves them.
/// Every provider, whichever
/// channel names it, is admitted and evaluated in the input view `db`, so the
/// order of the channels decides only which refusal is reported first. A
/// standalone transaction also admits the whole output once, as before; a
/// package view is returned with its ordinary diagnostics for consumers to
/// report. The result is the output view and the requests the transaction
/// added to `sealed`.
fn admit_requests(
    db: &DriverDataBase,
    transaction: &Transaction<'_>,
    sealed: Vec<common::generation::FrozenGenerationRequest>,
    authored: &[FileRequests],
) -> Result<
    (
        DriverDataBase,
        Vec<common::generation::FrozenGenerationRequest>,
    ),
    GenerationError,
> {
    let basis = &transaction.basis;
    let modules = transaction.modules;
    let (standalone_root, max_rounds) = (transaction.standalone_root, transaction.max_rounds);
    let sealed_len = sealed.len();
    let mut requests = sealed;
    let error_at = |file: &url::Url, range, cause| GenerationError {
        file: file.clone(),
        range,
        cause,
    };
    let fallback_file = &modules[0];
    let whole_input = standalone_root.unwrap_or_else(|| parser::TextRange::empty(0.into()));
    let mut completed = std::collections::HashSet::new();
    // Every authored request is admitted before any provider runs. Admission
    // reads only the input view `db`, which no provider's output changes, so
    // a request that admission refuses is refused before the compile-time code
    // of a scoped or another authored provider runs.
    let admitted = admit_authored_requests(db, authored)?;
    // The providers of the authored requests run in the input view `db`, like
    // scoped ones: none reads what generation produces in the modules being
    // prepared. Their output is part of the first view, so a written header
    // is a fact when the code that a scoped request generates is checked, as
    // a written implementation is.
    for request in &admitted {
        let mut plan = evaluate_provider_instance(
            db,
            request.file,
            request.provider,
            request.input,
            request.range,
        )?;
        plan.request_impl = Some(request.range);
        requests.push(plan);
    }
    // The first request of the last round, for the limit's message.
    let mut last_round_first = None;
    // Every retry starts from unchanged authored text and the cumulative plan.
    // A bounded transaction never recursively treats generated output as input.
    for _ in 0..max_rounds {
        let output = prepared_view(db, basis, &requests);
        let output_module = |url: &url::Url| {
            let file = output
                .workspace()
                .get(&output, url)
                .expect("prepared view keeps every authored file");
            (file, output.top_mod(file))
        };
        for url in modules {
            let (_, module) = output_module(url);
            let _ = module.scope_graph(&output);
            let lowering_errors = hir::lower::generation_lowering_errors(&output, module);
            if let Some(first) = lowering_errors.first() {
                return Err(GenerationError {
                    file: first.file.url(&output).unwrap_or_else(|| url.clone()),
                    range: first.primary_range,
                    cause: first.message.clone(),
                });
            }
            analyze_type_definitions_first(&output, module);
            check_no_hidden_generated_members(&output, module)?;
            check_no_nested_deciding_impls(&output, module)?;
        }
        // A generated header is not eligible merely because it was lowered.
        // Validate conformance before retrying any goal against the new plan.
        use hir::diagnosable::Diagnosable;
        for request in &requests[sealed_len..] {
            let Some(scoped) = request.scoped_impl.as_ref() else {
                continue;
            };
            let (_, module) = output_module(&request.target_file);
            let implementation = module
                .all_impl_traits(&output)
                .iter()
                .copied()
                .find(|implementation| {
                    implementation
                        .selection_name(&output)
                        .is_some_and(|name| name.data(&output) == &scoped.selection_name)
                })
                .ok_or_else(|| {
                    error_at(
                        &scoped.demand.opening.file,
                        scoped.demand.opening.binding_range,
                        "generated scoped implementation was not constructed".into(),
                    )
                })?;
            let mut complete = implementation
                .diags(&output)
                .into_iter()
                .map(|diagnostic| diagnostic.to_complete(&output))
                .collect::<Vec<_>>();
            for method in implementation.methods(&output) {
                complete.extend(
                    method
                        .diags(&output)
                        .into_iter()
                        .map(|diagnostic| diagnostic.to_complete(&output)),
                );
            }
            if complete
                .iter()
                .any(|diagnostic| diagnostic.severity == common::diagnostics::Severity::Error)
            {
                return Err(diagnostic_error(
                    &output,
                    &complete,
                    error_at(
                        &scoped.demand.opening.file,
                        scoped.demand.opening.binding_range,
                        "generated implementation admission failed".into(),
                    ),
                ));
            }
        }
        let pending = modules
            .iter()
            .flat_map(|url| {
                hir::analysis::ty::trait_resolution::generation::preparation_demands(
                    &output,
                    output_module(url).1,
                )
            })
            .collect::<Vec<_>>();
        if !pending.is_empty() {
            let mut new_requests = Vec::new();
            let mut batch = std::collections::HashSet::new();
            let mut examined = Vec::new();
            // Reject every invalid opening before any provider can run; the
            // order in which demands were found must never grant authority to
            // an earlier entry.
            let pending = pending
                .into_iter()
                .map(|pending| {
                    pending.demand.map_err(|cause| {
                        error_at(&pending.opening.file, pending.opening.binding_range, cause)
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
            // The prepared view only reports which implementations are wanted.
            // Every provider is admitted and run in the input view, the same one
            // that header providers run in, so no provider reads
            // what another provider produced and the demands of one round are
            // independent of each other.
            for demand in pending {
                let file = demand.opening.file.clone();
                let range = demand.opening.binding_range;
                if !batch.insert(demand.clone()) {
                    continue;
                }
                examined.push(demand.clone());
                if completed.contains(&demand) {
                    return Err(error_at(
                        &file,
                        range,
                        "generated implementation did not discharge its preparation dependency"
                            .into(),
                    ));
                }
                let request = hir::core::meta::rebind_generation_request(db, &demand)
                    .map_err(|cause| error_at(&file, range, cause))?;
                if !hir::analysis::ty::trait_resolution::generation::admission_demands(db, request)
                    .is_empty()
                {
                    return Err(error_at(
                        &file,
                        range,
                        format!(
                            "a body that decides whether the provider applies (a condition or the derive function) needs a generated implementation; {PROVIDER_VIEW_RULE}"
                        ),
                    ));
                }
                let opening = request.opening(db);
                let origin = opening.scope(db).item();
                let (key, input) = hir::core::meta::scoped_derive_provider(
                    db,
                    opening,
                    request.goal(db),
                    origin,
                    origin.span().into(),
                )
                .map_err(|cause| {
                    provider_error(
                        db,
                        cause,
                        "scoped derive target admission failed",
                        error_at(
                            &file,
                            range,
                            "scoped derive provider admission failed".into(),
                        ),
                    )
                })?;
                let opening_file = db
                    .workspace()
                    .get(db, &file)
                    .expect("the authored view keeps every authored file");
                let mut plan = evaluate_provider_instance(db, opening_file, key, input, range)?;
                completed.insert(demand.clone());
                plan.scoped_impl = Some(common::generation::FrozenScopedImpl {
                    selection_name: scoped_selection_name(&demand),
                    demand,
                });
                new_requests.push(plan);
            }
            last_round_first = examined
                .first()
                .map(|demand| (demand.opening.file.clone(), demand.opening.binding_range));
            requests.extend(new_requests);
            continue;
        }
        if let ViewBasis::Standalone(_) = basis {
            let (_, module) = output_module(fallback_file);
            let diagnostics = output.run_on_top_mod(module);
            if diagnostics.has_errors(&output) {
                let complete = diagnostics.finalize(&output);
                // Written errors are reported by the ordinary check of the prepared view.
                if complete.iter().any(|diagnostic| {
                    matches!(diagnostic.severity, common::diagnostics::Severity::Error)
                        && !in_written_code(&output, diagnostic)
                }) {
                    return Err(diagnostic_error(
                        &output,
                        &complete,
                        error_at(
                            fallback_file,
                            whole_input,
                            "generated output admission failed".into(),
                        ),
                    ));
                }
            }
            drop(diagnostics);
        }
        return Ok((output, requests.split_off(sealed_len)));
    }
    // A round rebuilds the whole view, so the limit bounds the work spent.
    let (file, range) = last_round_first.unwrap_or_else(|| (fallback_file.clone(), whole_input));
    Err(error_at(
        &file,
        range,
        format!(
            "scoped generation preparation limit exceeded: {max_rounds} rounds did not serve every scoped request; the first request of the last round is at this `with` binding"
        ),
    ))
}

/// An authored request that admission accepted: the provider instance of its
/// header, which has not run yet, the record the provider reads and where the
/// request is written.
struct AdmittedRequest<'db> {
    file: File,
    /// The range of the derive header.
    range: parser::TextRange,
    input: RecordInput<'db>,
    provider: hir::analysis::semantic::SemanticInstanceKey<'db>,
}

/// Admit every authored request of `authored` in the input view `db`, in the
/// order they are written: the header, target and provider application of
/// each derive header. No provider code runs.
fn admit_authored_requests<'db>(
    db: &'db DriverDataBase,
    authored: &[FileRequests],
) -> Result<Vec<AdmittedRequest<'db>>, GenerationError> {
    let mut admitted = Vec::new();
    for found in authored {
        let error = |range, cause| GenerationError {
            file: found.url.clone(),
            range,
            cause,
        };
        let authored_file = db
            .workspace()
            .get(db, &found.url)
            .expect("the authored view keeps every authored file");
        let authored_module = db.top_mod(authored_file);
        for (request_ast, range, path_range) in &found.explicit_requests {
            let request =
                hir::core::meta::authored_derive_request(db, authored_module, request_ast)
                    .ok_or_else(|| {
                        error(
                            *range,
                            "cannot recover authored derive header identity".into(),
                        )
                    })?;
            let provider_ty = header_provider_ty(
                db,
                request.scope(),
                request.derive_provider(db),
                *path_range,
                &error,
            )?;
            let (provider, input) =
                hir::core::meta::native_derive_provider(db, request, provider_ty).map_err(
                    |cause| {
                        provider_error(
                            db,
                            cause,
                            "derive target admission failed",
                            error(*range, String::new()),
                        )
                    },
                )?;
            admitted.push(AdmittedRequest {
                file: authored_file,
                range: *range,
                input,
                provider,
            });
        }
        for (request_ast, range, path_range) in &found.inherent_requests {
            let request =
                hir::core::meta::authored_inherent_derive_request(db, authored_module, request_ast)
                    .ok_or_else(|| {
                        error(
                            *range,
                            "cannot recover authored derive header identity".into(),
                        )
                    })?;
            let provider_ty = header_provider_ty(
                db,
                request.scope(),
                request.derive_provider(db),
                *path_range,
                &error,
            )?;
            let (provider, input) =
                hir::core::meta::native_inherent_derive_provider(db, request, provider_ty)
                    .map_err(|cause| {
                        provider_error(
                            db,
                            cause,
                            "derive target admission failed",
                            error(*range, String::new()),
                        )
                    })?;
            admitted.push(AdmittedRequest {
                file: authored_file,
                range: *range,
                input,
                provider,
            });
        }
    }
    Ok(admitted)
}

/// Refuses a generated member of an inherent header that its type already has
/// under that name, through a written inherent member or a trait member. An
/// inherent member outranks a trait member, so the generated one would
/// otherwise silently win or never be selected.
fn check_no_hidden_generated_members<'db>(
    output: &'db DriverDataBase,
    output_module: hir::hir_def::TopLevelMod<'db>,
) -> Result<(), GenerationError> {
    match hir::core::hidden_items::hidden_generated_members(output, output_module)
        .into_iter()
        .next()
    {
        Some(hidden) => Err(GenerationError {
            file: hidden.file.clone(),
            range: hidden.range,
            cause: hidden.diagnostic_message(),
        }),
        None => Ok(()),
    }
}

/// Refuses an implementation of a deciding trait, such as `Drop`, that a
/// provider wrote nested inside a generated function: the compiler decides
/// what runs where a value goes out of use by whether the type implements the
/// trait, so only an implementation of authored code may change that.
fn check_no_nested_deciding_impls<'db>(
    output: &'db DriverDataBase,
    output_module: hir::hir_def::TopLevelMod<'db>,
) -> Result<(), GenerationError> {
    match hir::core::hidden_items::nested_generated_deciding_impl_errors(output, output_module)
        .into_iter()
        .next()
    {
        Some((file, range, cause)) => Err(GenerationError { file, range, cause }),
        None => Ok(()),
    }
}

/// Whether the provider instance `key`, or anything its evaluation runs, waits
/// for a generated implementation. Such a provider cannot run: the
/// implementation it waits for is produced by another provider of the same
/// package.
fn provider_needs_generation<'db>(
    db: &'db DriverDataBase,
    key: hir::analysis::semantic::SemanticInstanceKey<'db>,
) -> bool {
    !hir::analysis::ty::trait_resolution::generation::evaluation_demands(db, key).is_empty()
}

fn scoped_selection_name(demand: &common::generation::ScopedDeriveDemand) -> String {
    // Length-prefixed identity is injective; the NUL prefix is unspellable in
    // authored identifiers. No implementation authority comes from this name.
    let mut name = String::from("\0scoped-derive");
    for component in [
        demand.opening.file.as_str().to_owned(),
        format!("{:?}", demand.opening.binding_range),
        demand.trait_ref.file.as_str().to_owned(),
        format!("{:?}", demand.trait_ref.range),
        demand.target.target_file.as_str().to_owned(),
        format!("{:?}", demand.target.target_range),
        // Each instance of a generic target is its own implementation.
        format!("{:?}", demand.target.args),
        format!("{:?}", demand.mode),
    ] {
        use std::fmt::Write;
        write!(&mut name, ":{}:{}", component.len(), component).unwrap();
    }
    name
}

/// The error of a refused provider application: its message, or the
/// diagnostics of the refused target (`target_label`) or provider application.
fn provider_error(
    db: &DriverDataBase,
    cause: hir::core::meta::NativeDeriveProviderError<'_>,
    target_label: &str,
    fallback: GenerationError,
) -> GenerationError {
    let (failure, label) = match cause {
        hir::core::meta::NativeDeriveProviderError::Message(message) => {
            return GenerationError {
                cause: message,
                ..fallback
            };
        }
        hir::core::meta::NativeDeriveProviderError::Provider(failure) => {
            (failure, "provider application not admitted")
        }
        hir::core::meta::NativeDeriveProviderError::Target(failure) => (failure, target_label),
    };
    let complete = failure
        .vouchers()
        .iter()
        .map(|voucher| voucher.to_complete(db))
        .collect::<Vec<_>>();
    diagnostic_error(
        db,
        &complete,
        GenerationError {
            cause: format!("{label}: {}", failure.description()),
            ..fallback
        },
    )
}

/// The authored text of `basis` as the one view every provider runs in, with
/// the `sealed` output of the ingots it depends on as ordinary code. Written
/// implementation headers, `impl Trait for T with (Provider)`, that no sealed
/// output serves are implementors in it whose members are unavailable. What
/// generation produces in the ingot being prepared is absent. It is a copy, so
/// the caller's authored view is unchanged.
fn input_view(
    db: &DriverDataBase,
    basis: &ViewBasis,
    sealed: &[common::generation::FrozenGenerationRequest],
) -> DriverDataBase {
    let mut view = prepared_view(db, basis, sealed);
    view.generation_inputs()
        .set_declared_headers(&mut view)
        .to(true);
    view
}

fn prepared_view(
    db: &DriverDataBase,
    basis: &ViewBasis,
    requests: &[common::generation::FrozenGenerationRequest],
) -> DriverDataBase {
    let mut output = match basis {
        ViewBasis::Standalone(file) => {
            let url = file.url(db).expect("indexed generation input");
            let mut output = DriverDataBase::default();
            output
                .workspace()
                .touch(&mut output, url.clone(), Some(file.text(db).clone()));
            output
                .compiler_options()
                .set_recovery_mode(&mut output)
                .to(db.compiler_options().recovery_mode(db));
            output
                .compilation_settings()
                .set_profile(&mut output)
                .to(db.compilation_settings().profile(db).clone());
            output
        }
        ViewBasis::Package => db.fork_inputs(),
    };
    output
        .compilation_settings()
        .set_target(&mut output)
        .to(db.compilation_settings().target(db));
    output
        .compilation_settings()
        .set_platform(&mut output)
        .to(db.compilation_settings().platform(db));
    output
        .generation_inputs()
        .set_requests(&mut output)
        .to(requests.to_vec());
    output
}

fn evaluate_provider_instance<'db>(
    db: &'db DriverDataBase,
    file: File,
    provider_key: hir::analysis::semantic::SemanticInstanceKey<'db>,
    input: RecordInput<'db>,
    range: parser::TextRange,
) -> Result<common::generation::FrozenGenerationRequest, GenerationError> {
    let BodyOwner::Func(provider, _) = provider_key.owner(db) else {
        return Err(GenerationError {
            file: file.url(db).unwrap(),
            range,
            cause: "provider is not a function instance".into(),
        });
    };
    let url = file.url(db).expect("indexed generation input");
    let error = |range, cause| GenerationError {
        file: url.clone(),
        range,
        cause,
    };
    // The function itself takes no binders; those of its implementation, as in
    // `impl<const K: u256> Derive<G> for Scaled<K>`, are fixed by the ground
    // provider application the instance was finalized for.
    if !provider.is_const(db) || provider.body(db).is_none() || provider.declares_generic_params(db)
    {
        return Err(error(
            range,
            "provider must be an authored nongeneric const function".into(),
        ));
    }
    let carrier = resolve_lib_type_path(db, provider.scope(), "core::meta::Record")
        .ok_or_else(|| error(range, "missing Record carrier".into()))?;
    let code = resolve_lib_type_path(db, provider.scope(), "core::meta::Code")
        .ok_or_else(|| error(range, "missing Code carrier".into()))?;
    let item = resolve_lib_type_path(db, provider.scope(), "core::meta::Item")
        .ok_or_else(|| error(range, "missing Item kind".into()))?;
    let arguments = provider.arg_tys(db);
    if arguments.len() != 1
        || arguments[0]
            .instantiate_identity()
            .as_view(db)
            .unwrap_or(arguments[0].instantiate_identity())
            != carrier
        || provider
            .return_ty(db)
            .as_view(db)
            .unwrap_or(provider.return_ty(db))
            != TyId::app(db, code, item)
    {
        return Err(error(
            range,
            format!(
                "provider signature must be const fn(Record) -> Code<Item>: args={:?}, result={}, expected={}",
                arguments
                    .iter()
                    .map(|a| a.instantiate_identity().pretty_print(db))
                    .collect::<Vec<_>>(),
                provider.return_ty(db).pretty_print(db),
                TyId::app(db, code, item).pretty_print(db)
            ),
        ));
    }
    if !checked_body_demands(db, provider_key.owner(db)).is_empty() {
        return Err(error(
            range,
            format!(
                "the provider body opens an implementation that only generation can supply; {PROVIDER_VIEW_RULE}"
            ),
        ));
    }
    let diagnostics = &check_func_body(db, provider).0;
    if !body_errors(db, provider).is_empty() {
        for diagnostic in diagnostics {
            if let hir::analysis::ty::diagnostics::FuncBodyDiag::Body(
                hir::analysis::ty::diagnostics::BodyDiag::UndefinedVariable(span, name, _),
            ) = diagnostic
                && let Some(span) = span.resolve(db)
            {
                return Err(GenerationError {
                    file: span.file.url(db).unwrap_or(url.clone()),
                    range: span.range,
                    cause: format!(
                        "authored provider references undefined variable `{}`; generated output is not authored input",
                        name.data(db)
                    ),
                });
            }
        }
        let complete = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.to_complete(db))
            .collect::<Vec<_>>();
        return Err(with_view_note(diagnostic_error(
            db,
            &complete,
            error(range, "authored provider body diagnostics".into()),
        )));
    }
    if provider_needs_generation(db, provider_key) {
        return Err(error(
            range,
            format!(
                "the provider runs code that opens an implementation only generation can supply; {PROVIDER_VIEW_RULE}"
            ),
        ));
    }
    let owner = provider_key.owner(db);
    let instance = hir::analysis::semantic::SemanticInstance::new(db, provider_key);
    let mut helper_diagnostics = Vec::new();
    let mut validation_diagnostics = Vec::new();
    let mut unresolved = Vec::new();
    {
        use hir::analysis::semantic::access::SemanticAccessCheckResult;
        let mut pending = std::collections::VecDeque::from([instance]);
        let mut seen = std::collections::HashSet::new();
        while let Some(current) = pending.pop_front() {
            if !seen.insert(current) {
                continue;
            }
            match hir::analysis::semantic::access::semantic_access_check_query(db, current) {
                SemanticAccessCheckResult::Ok { .. } => {
                    for callee in current.callees(db) {
                        pending.push_back(hir::analysis::semantic::SemanticInstance::new(
                            db, callee.key,
                        ));
                    }
                }
                SemanticAccessCheckResult::Err(diagnostic) => {
                    let diagnostic = diagnostic.diag(db);
                    let origin_owner = diagnostic.instance.key(db).owner(db);
                    let errors = match origin_owner {
                        BodyOwner::Func(function, _) => body_errors(db, function),
                        _ => Vec::new(),
                    };
                    if !errors.is_empty() {
                        helper_diagnostics.extend(errors);
                    } else {
                        validation_diagnostics.push(diagnostic.to_complete(db));
                    }
                }
                SemanticAccessCheckResult::Blocked(cause) => {
                    let origin_owner = current.key(db).owner(db);
                    let errors = match origin_owner {
                        BodyOwner::Func(function, _) => body_errors(db, function),
                        _ => Vec::new(),
                    };
                    if !errors.is_empty() {
                        helper_diagnostics.extend(errors);
                    } else {
                        unresolved.push(format!("Blocked: {cause:?}"));
                    }
                }
            }
        }
    }
    if !helper_diagnostics.is_empty() {
        return Err(diagnostic_error(
            db,
            &helper_diagnostics,
            error(range, "authored const helper diagnostics".into()),
        ));
    }
    if !validation_diagnostics.is_empty() {
        return Err(diagnostic_error(
            db,
            &validation_diagnostics,
            error(
                range,
                "authored provider ownership/capability validation failed".into(),
            ),
        ));
    }
    if !unresolved.is_empty() {
        // Pending and Blocked are not a ready provider and are never
        // replaced with a successful quotation or an empty output.
        return Err(error(
            range,
            format!(
                "authored provider validation unresolved: {}",
                unresolved.join("; ")
            ),
        ));
    }
    let value = match hir::analysis::semantic::ctfe::force_const_computation(
        db,
        hir::analysis::semantic::ctfe::const_computation_for_instance(
            db,
            provider_key,
            vec![
                hir::core::meta::record_input_argument(db, input, owner.scope())
                    .map_err(|cause| error(range, cause.into()))?,
            ],
        ),
        hir::analysis::semantic::ctfe::CtfeConfig::default(),
    ) {
        EvalOutcome::Ready(value) => value.value(),
        EvalOutcome::Failed(cause) => {
            let fallback = error(range, format!("provider evaluation failed: {cause:?}"));
            return Err(match &cause {
                EvalFailure::Ctfe(cause) => evaluation_error(db, owner, cause, fallback),
                _ => fallback,
            });
        }
        EvalOutcome::Blocked(cause) => {
            return Err(error(
                range,
                format!("provider evaluation blocked: {cause:?}"),
            ));
        }
    };
    let SemConstValue::ItemCode { code, .. } = value.value(db) else {
        return Err(error(
            range,
            "provider returned an ordinary value rather than checked item code".into(),
        ));
    };
    code.freeze_request(db, file)
        .map_err(|cause| error(range, cause.into()))
}

#[cfg(test)]
mod round_limit_tests {
    use super::*;

    const FILE: &str = "file:///tmp/fe-round-limit.fe";

    fn prepare_within(source: &str, max_rounds: usize) -> Result<DriverDataBase, GenerationError> {
        let mut db = DriverDataBase::default();
        let file =
            db.workspace()
                .touch(&mut db, url::Url::parse(FILE).unwrap(), Some(source.into()));
        prepare_file_within(&db, file, max_rounds)
            .map(|prepared| prepared.expect("the source requests generation"))
    }

    /// `targets` records, each served by the same provider in one block.
    fn breadth(targets: usize) -> String {
        let mut source = String::from(
            "use core::derive::Derive\nuse core::meta::{Code, Item, Record}\n\
             trait Value { const fn value() -> u256 }\n\
             struct Provider {}\n\
             impl Derive<Value> for Provider {\n\
             \x20   const fn derive(_ record: Record) -> Code<Item> {\n\
             \x20       quote { const fn value() -> u256 { 1 } }\n\
             \x20   }\n}\n",
        );
        for target in 0..targets {
            source += &format!("struct S{target} {{}}\n");
        }
        let sum = (0..targets)
            .map(|target| format!("S{target}::value()"))
            .collect::<Vec<_>>()
            .join(" + ");
        source + &format!("const fn run() -> u256 {{ with (Provider) {{ {sum} }} }}\n")
    }

    /// A round serves every ready request, so independent requests do not use
    /// a round each: the targets share one round, and one more finds nothing
    /// pending. One request per round would need `targets + 1` rounds.
    #[test]
    fn independent_requests_share_a_round() {
        let targets = 12;
        assert!(prepare_within(&breadth(targets), 2).is_ok());
        let source = breadth(targets);
        let error = prepare_within(&source, 1).err().unwrap();
        assert!(error.cause.contains("limit exceeded"), "{error}");
        assert_eq!(error.file.as_str(), FILE);
        let start = usize::from(error.range.start());
        let end = usize::from(error.range.end());
        assert_eq!(&source[start..end], "Provider", "{error}");
    }
}
