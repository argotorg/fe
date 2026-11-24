//! Generation for derive headers: finds the headers of a source file or of a
//! package with its dependencies, evaluates each provider at compile time
//! and admits the generated items into one prepared view, which checks, builds
//! and the language server read in place of the written file.
use crate::DriverDataBase;
use common::{InputDb, file::File, generation::FrozenGenerationRequest, ingot::IngotKind};
use hir::{
    analysis::{
        diagnostics::DiagnosticVoucher,
        semantic::{CtfeError, EvalFailure, EvalOutcome, SemConstValue, SemOrigin},
        ty::{
            corelib::resolve_lib_type_path,
            ty_check::{BodyOwner, check_func_body},
            ty_def::TyId,
        },
    },
    core::meta::RecordInput,
    hir_def::HirIngot,
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

/// The rule in a phrase short enough for the first line of a diagnostic.
const PROVIDER_VIEW_BRIEF: &str =
    "a provider cannot see what generation produces in its own package";

/// A provider diagnostic about something generation produces in the provider's
/// own package carries the rule that fixes the view. Other errors of a provider
/// body, such as a type mismatch, are not about the view and carry no note.
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

/// The names that the quotations of `ingot` write as functions, constants or
/// implemented traits: what generation of that package can add to it.
fn quoted_names(
    db: &DriverDataBase,
    ingot: common::ingot::Ingot<'_>,
) -> std::collections::HashSet<String> {
    let mut names = std::collections::HashSet::new();
    for (_, file) in ingot.files(db).iter() {
        if !matches!(file.kind(db), Some(common::file::IngotFileKind::Source)) {
            continue;
        }
        let (green, _) = parser::parse_source_file(file.text(db), parser::RecoveryMode::Recover);
        let root = ast::Root::cast(parser::SyntaxNode::new_root(green)).unwrap();
        for quote in root.syntax().descendants().filter_map(ast::QuoteExpr::cast) {
            for node in quote.syntax().descendants() {
                if let Some(function) = ast::Func::cast(node.clone()) {
                    names.extend(function.sig().name().map(|name| name.text().to_string()));
                } else if let Some(constant) = ast::Const::cast(node.clone()) {
                    names.extend(constant.name().map(|name| name.text().to_string()));
                } else if let Some(implementation) = ast::ImplTrait::cast(node) {
                    for token in implementation
                        .trait_ref()
                        .iter()
                        .flat_map(|trait_ref| trait_ref.syntax().descendants_with_tokens())
                        .filter_map(|element| element.into_token())
                        .filter(|token| token.kind() == parser::SyntaxKind::Ident)
                    {
                        names.insert(token.text().to_string());
                    }
                }
            }
        }
    }
    names
}

/// Whether a diagnostic of a provider body is about a name or an
/// implementation that generation of the provider's package supplies: a name
/// that some quotation writes, or an implementation of a trait whose name some
/// quotation writes. A type mismatch, or a name no quotation writes, is an
/// ordinary error of the provider.
fn about_generated(
    db: &DriverDataBase,
    diagnostic: &hir::analysis::ty::diagnostics::FuncBodyDiag<'_>,
    quoted: &std::collections::HashSet<String>,
) -> bool {
    use hir::analysis::{
        name_resolution::diagnostics::PathResDiag,
        ty::diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
    };
    let written = |name: &hir::hir_def::IdentId<'_>| quoted.contains(name.data(db).as_str());
    let path_diag = |diagnostic: &PathResDiag<'_>| match diagnostic {
        PathResDiag::NotFound(_, name) => written(name),
        PathResDiag::MethodNotFound { method_name, .. } => written(method_name),
        _ => false,
    };
    match diagnostic {
        FuncBodyDiag::Body(BodyDiag::UndefinedVariable(_, name, _)) => written(name),
        FuncBodyDiag::NameRes(diagnostic)
        | FuncBodyDiag::Ty(TyDiagCollection::PathRes(diagnostic)) => path_diag(diagnostic),
        FuncBodyDiag::Ty(TyDiagCollection::Satisfiability(
            TraitConstraintDiag::TraitBoundNotSat { primary_goal, .. },
        )) => primary_goal
            .def(db)
            .name(db)
            .to_opt()
            .is_some_and(|name| written(&name)),
        _ => false,
    }
}

/// Whether an error of the body of `function` is about something generation of
/// its package supplies.
fn body_about_generated(db: &DriverDataBase, function: hir::hir_def::Func<'_>) -> bool {
    let quoted = quoted_names(db, function.scope().ingot(db));
    check_func_body(db, function)
        .0
        .iter()
        .filter(|diagnostic| diagnostic.severity() == common::diagnostics::Severity::Error)
        .any(|diagnostic| about_generated(db, diagnostic, &quoted))
}

/// The first line of a provider diagnostic says what the provider cannot see
/// only when the error is about something generation supplies.
fn view_brief(generated: bool) -> String {
    if generated {
        format!(" ({PROVIDER_VIEW_BRIEF})")
    } else {
        String::new()
    }
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
            let generated = body_about_generated(db, function);
            let error = diagnostic_error(
                db,
                &complete,
                GenerationError {
                    cause: format!("authored const helper diagnostics{}", view_brief(generated)),
                    ..fallback
                },
            );
            return if generated {
                with_view_note(error)
            } else {
                error
            };
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
    let Some(root) = scan_syntax(file.text(db)) else {
        return Ok(None);
    };
    if file
        .containing_ingot(db)
        .is_none_or(|ingot| ingot.kind(db) != IngotKind::StandAlone)
    {
        return Err(error(
            parser::TextRange::empty(0.into()),
            "bounded generation currently requires a standalone source file".into(),
        ));
    }
    let basis = ViewBasis::Standalone(file);
    let input_db = input_view(db, &basis, &[]);
    let file = input_db
        .workspace()
        .get(&input_db, &url)
        .expect("the input view holds the authored file");
    let found = file_requests(&input_db, file, root)?;
    if found.is_empty() {
        return Ok(None);
    }
    let root_range = found.root_range;
    let (output, _) = admit_requests(
        &input_db,
        &basis,
        &[url],
        Some(root_range),
        Vec::new(),
        &[found],
    )?;
    Ok(Some(output))
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
    fn visit<'db>(
        db: &'db DriverDataBase,
        ingot: common::ingot::Ingot<'db>,
        index: &mut std::collections::HashMap<url::Url, usize>,
        in_progress: &mut std::collections::HashSet<url::Url>,
        plans: &mut Vec<IngotPlan>,
    ) -> Option<usize> {
        // The embedded libraries carry no requests. A package is not one
        // because its manifest says it is named `core` or `std`.
        if ingot.kind(db) == IngotKind::StandAlone
            || common::stdlib::is_embedded_builtin(ingot.base(db))
        {
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
/// file, with the same dependency frontier. Output diagnostics
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
    let mut seals = vec![Vec::new(); plans.len()];
    let mut last = None;
    for (index, plan) in plans.iter().enumerate() {
        let mut syntaxes = Vec::new();
        for url in &plan.files {
            let file = db
                .workspace()
                .get(db, url)
                .expect("a planned ingot file is in the database");
            if let Some(root) = scan_syntax(file.text(db)) {
                syntaxes.push((url.clone(), root));
            }
        }
        if syntaxes.is_empty() {
            continue;
        }
        let sealed: Vec<_> = plan
            .dependencies
            .iter()
            .flat_map(|&index| seals[index].iter().cloned())
            .collect();
        let basis = ViewBasis::Package;
        let input = input_view(db, &basis, &sealed);
        let found_requests = syntaxes
            .into_iter()
            .map(|(url, root)| {
                let file = input
                    .workspace()
                    .get(&input, &url)
                    .expect("the input view holds every authored file");
                file_requests(&input, file, root)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let (output, own) =
            admit_requests(&input, &basis, &plan.files, None, sealed, &found_requests)?;
        seals[index] = own;
        last = Some(output);
    }
    let total = seals.iter().map(Vec::len).sum::<usize>();
    if total == 0 {
        return Ok(None);
    }
    // The last view is sufficient when its dependency closure includes all
    // generated output. Multiple unrelated roots may need a combined view.
    if let Some(output) = last
        && output.generation_inputs().requests(&output).len() == total
    {
        return Ok(Some(output));
    }
    let everything: Vec<_> = seals.into_iter().flatten().collect();
    Ok(Some(prepared_view(db, &ViewBasis::Package, &everything)))
}

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
/// Look for generation syntax in the text of one authored file, before any
/// view is built. `None` means the file contains none outside quotations, or
/// has parse errors, so nothing is prepared for it.
fn scan_syntax(text: &str) -> Option<ast::Root> {
    // Every request form spells `with`; other files need no parse.
    if !text.contains("with") {
        return None;
    }
    let (green, parse_errors) = parser::parse_source_file(text, parser::RecoveryMode::Recover);
    let root = ast::Root::cast(parser::SyntaxNode::new_root(green)).unwrap();
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
        return None;
    }
    // A file with parse errors is not prepared: its requests are not known, and
    // the ordinary pipeline reports the parse errors with the file's other
    // diagnostics, as it does for a file without requests.
    if !parse_errors.is_empty() {
        return None;
    }
    Some(root)
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

/// Collect and validate the explicit requests of one scanned file in the view the
/// providers run in.
fn file_requests(
    db: &DriverDataBase,
    file: File,
    root: ast::Root,
) -> Result<FileRequests, GenerationError> {
    let url = file.url(db).expect("indexed generation input");
    let error = |range, cause| GenerationError {
        file: url.clone(),
        range,
        cause,
    };
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

fn check_preparation_module(db: &DriverDataBase, url: &url::Url) -> Result<(), GenerationError> {
    let file = db
        .workspace()
        .get(db, url)
        .expect("prepared view keeps every authored file");
    let module = db.top_mod(file);
    let _ = module.scope_graph(db);
    let errors = hir::lower::generation_lowering_errors(db, module);
    if let Some(first) = errors.first() {
        return Err(GenerationError {
            file: first.file.url(db).unwrap_or_else(|| url.clone()),
            range: first.primary_range,
            cause: first.message.clone(),
        });
    }
    analyze_type_definitions_first(db, module);
    Ok(())
}

/// Admit all written provider applications before evaluating any of them, then
/// admit their output together. Providers read only authored code and sealed
/// dependency output from the input view. The prepared view is returned with
/// ordinary diagnostics for written code.
fn admit_requests(
    db: &DriverDataBase,
    basis: &ViewBasis,
    modules: &[url::Url],
    standalone_root: Option<parser::TextRange>,
    mut requests: Vec<FrozenGenerationRequest>,
    authored: &[FileRequests],
) -> Result<(DriverDataBase, Vec<FrozenGenerationRequest>), GenerationError> {
    let sealed_len = requests.len();
    let admitted = admit_authored_requests(db, authored)?;
    let mut sites = Vec::with_capacity(admitted.len());
    for request in admitted {
        let mut plan = evaluate_provider_instance(
            db,
            request.file,
            request.provider,
            request.input,
            request.site.range,
        )?;
        plan.request_impl = Some(request.site.range);
        requests.push(plan);
        sites.push(request.site);
    }
    let output = prepared_view(db, basis, &requests);
    for url in modules {
        check_preparation_module(&output, url)?;
    }
    let requested = Requested {
        requests: &requests[sealed_len..],
        sites: &sites,
    };
    check_conservative_extension(&output, modules, &requested)?;
    if let Some(whole_input) = standalone_root {
        let url = &modules[0];
        let file = output
            .workspace()
            .get(&output, url)
            .expect("prepared view keeps the standalone file");
        let diagnostics = output.run_on_top_mod(output.top_mod(file));
        if diagnostics.has_errors(&output) {
            let complete = diagnostics.finalize(&output);
            if complete.iter().any(|diagnostic| {
                matches!(diagnostic.severity, common::diagnostics::Severity::Error)
                    && !in_written_code(&output, diagnostic)
            }) {
                return Err(diagnostic_error(
                    &output,
                    &complete,
                    GenerationError {
                        file: url.clone(),
                        range: whole_input,
                        cause: "generated output admission failed".into(),
                    },
                ));
            }
        }
    }
    Ok((output, requests.split_off(sealed_len)))
}

/// An authored request that admission accepted: the provider instance of its
/// header, which has not run yet, the record the provider reads and where the
/// request is written.
struct AdmittedRequest<'db> {
    file: File,
    site: RequestSite,
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
        let site = |range| RequestSite {
            file: found.url.clone(),
            range,
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
                site: site(*range),
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
                site: site(*range),
                input,
                provider,
            });
        }
    }
    Ok(admitted)
}

/// Where an author wrote a request: the derive header.
#[derive(Clone, Debug)]
struct RequestSite {
    file: url::Url,
    range: parser::TextRange,
}

/// The requests of the modules being prepared, with the place each one was
/// written, in the same order.
struct Requested<'a> {
    requests: &'a [common::generation::FrozenGenerationRequest],
    sites: &'a [RequestSite],
}

impl Requested<'_> {
    /// The request whose output holds the text written at `source`, a place in
    /// a quotation.
    fn site_of(&self, source: &(url::Url, parser::TextRange)) -> Option<&RequestSite> {
        fn holds(
            code: &common::generation::FrozenCode,
            source: &(url::Url, parser::TextRange),
        ) -> bool {
            use common::generation::{FrozenCode, FrozenHoleValue};
            let FrozenCode::Template {
                quote_file,
                quote_range,
                holes,
                ..
            } = code
            else {
                return false;
            };
            (quote_file == &source.0 && quote_range.contains_range(source.1))
                || holes.iter().any(|hole| match hole {
                    FrozenHoleValue::Code(nested) => holds(nested, source),
                    _ => false,
                })
        }
        self.requests
            .iter()
            .position(|request| holds(&request.code, source))
            .and_then(|index| self.sites.get(index))
    }

    /// The request written at `at`, the place of its header.
    fn site_written_at(&self, at: &(url::Url, parser::TextRange)) -> Option<&RequestSite> {
        self.sites
            .iter()
            .find(|site| site.file == at.0 && site.range == at.1)
    }
}

/// Inside the package that generates them, generated items only fill gaps. A
/// name, member or implementation that the package's own code or a dependency
/// already answers keeps that answer, and generated items are consulted only
/// where nothing else resolves. Resolution applies that rule itself (see
/// `Tier` in the name resolver). What it cannot do is make a generated item
/// that is hidden in the module it is published into useful, so such an item
/// is refused at its request, naming the item, what hides it, the request and
/// how to resolve it. `output` is the prepared view and `modules` the authored
/// files being prepared.
///
/// A generated member of an inherent implementation that its type already has
/// under that name is refused the same way, since an inherent member would
/// otherwise outrank a trait member without any name being involved.
fn check_conservative_extension(
    output: &DriverDataBase,
    modules: &[url::Url],
    requested: &Requested<'_>,
) -> Result<(), GenerationError> {
    check_no_hidden_generated_items(output, modules, requested)?;
    check_no_hidden_generated_members(output, modules, requested)?;
    check_no_changed_choices(output, modules, requested)
}

/// Refuses a generated implementation that would change a decision the
/// package's written code already answers: which of several implementations is
/// preferred, or whether one exists. The program keeps the written answer, so
/// the generated implementation would never be the one that runs; the error is
/// at the request, naming the decision and the implementation.
fn check_no_changed_choices(
    output: &DriverDataBase,
    modules: &[url::Url],
    requested: &Requested<'_>,
) -> Result<(), GenerationError> {
    use hir::analysis::name_resolution::changed_choices;
    for url in modules {
        let Some(output_file) = output.workspace().get(output, url) else {
            continue;
        };
        let Some(changed) = changed_choices(output, output.top_mod(output_file))
            .into_iter()
            .next()
        else {
            continue;
        };
        let generated = changed.generated.first();
        let site = generated.and_then(|generated| match &generated.target {
            hir::analysis::ty::ty_check::UseTarget::Generated { source, .. } => {
                source.as_ref().and_then(|source| requested.site_of(source))
            }
            _ => None,
        });
        let requested_at = site.map_or_else(String::new, |site| {
            format!(
                " requested at {}:{}..{}",
                site.file,
                u32::from(site.range.start()),
                u32::from(site.range.end())
            )
        });
        let what = generated.map_or_else(
            || "a generated item".to_string(),
            |generated| format!("the generated {}", generated.description),
        );
        let decided_at = changed.site.as_ref().map_or_else(String::new, |site| {
            format!(
                ", decided at `{}` ({}:{}..{})",
                site.text,
                site.file,
                u32::from(site.range.start()),
                u32::from(site.range.end())
            )
        });
        return Err(GenerationError {
            file: site.map_or_else(|| changed.file.clone(), |site| site.file.clone()),
            range: site.map_or_else(|| parser::TextRange::empty(0.into()), |site| site.range),
            cause: format!(
                "{what}{requested_at} is refused: it {verb} change {decision} for `{subject}`{decided_at}. {explanation} Inside the package that generates them, generated items only fill gaps that nothing written in the package answers. To resolve it, {remedy}",
                verb = if changed.by_definition {
                    "could"
                } else {
                    "would"
                },
                decision = changed.what,
                subject = changed.subject,
                explanation = changed.explanation,
                remedy = changed.remedy,
            ),
        });
    }
    Ok(())
}

fn describe_target(target: &hir::analysis::ty::ty_check::UseTarget) -> String {
    use hir::analysis::ty::ty_check::UseTarget;
    match target {
        UseTarget::Declared { file, range, name } => format!(
            "`{name}` declared at {file}:{}..{}",
            u32::from(range.start()),
            u32::from(range.end())
        ),
        UseTarget::Generated { name, ingot, .. } => {
            format!("the generated `{name}` of the ingot {ingot}")
        }
    }
}

fn describe_via(via: &hir::analysis::name_resolution::Via) -> String {
    use hir::analysis::name_resolution::{Located, Via};
    let place = |located: &Option<Located>| {
        located.as_ref().map_or(String::new(), |located| {
            format!(
                " `{}` at {}:{}..{}",
                located.text,
                located.file,
                u32::from(located.range.start()),
                u32::from(located.range.end())
            )
        })
    };
    match via {
        Via::Item => "a declaration of the module".to_string(),
        Via::NamedImport(located) => format!("the import{}", place(located)),
        Via::GlobImport(located) => format!("the glob import{}", place(located)),
        Via::Prelude => "the prelude".to_string(),
        Via::Ingot => "the name of an ingot".to_string(),
        Via::Builtin => "a builtin type".to_string(),
        Via::Enclosing(inner) => format!("the enclosing module, through {}", describe_via(inner)),
    }
}

/// Refuses a generated item that its module binds another item to the name
/// of: it would never be found there. The error is at the request that
/// generated the item.
fn check_no_hidden_generated_items(
    output: &DriverDataBase,
    modules: &[url::Url],
    requested: &Requested<'_>,
) -> Result<(), GenerationError> {
    use hir::analysis::name_resolution::{Via, hidden_generated_items};
    for url in modules {
        let Some(output_file) = output.workspace().get(output, url) else {
            continue;
        };
        if let Some(hidden) = hidden_generated_items(output, output.top_mod(output_file))
            .into_iter()
            .next()
        {
            let site = match &hidden.generated {
                hir::analysis::ty::ty_check::UseTarget::Generated { source, .. } => {
                    source.as_ref().and_then(|source| requested.site_of(source))
                }
                _ => None,
            };
            let requested_at = site.map_or_else(String::new, |site| {
                format!(
                    " requested at {}:{}..{}",
                    site.file,
                    u32::from(site.range.start()),
                    u32::from(site.range.end())
                )
            });
            let place = hidden.module.map_or_else(
                || format!("the root module of {}", hidden.file),
                |range| {
                    format!(
                        "the module at {}:{}..{}",
                        hidden.file,
                        u32::from(range.start()),
                        u32::from(range.end())
                    )
                },
            );
            let binding = &hidden.hidden_by;
            let hidden_by = binding.target.as_ref().map_or_else(
                || describe_via(&binding.via),
                |target| {
                    format!(
                        "{} through {}",
                        describe_target(target),
                        describe_via(&binding.via)
                    )
                },
            );
            let ambiguous = if binding.ambiguous {
                " (it is ambiguous there, and one candidate is described)"
            } else {
                ""
            };
            let imported = hidden.imported_by.as_ref().map_or_else(String::new, |import| {
                format!(
                    " The import `{}` at {}:{}..{} names the generated item, but imports nothing while a binding written in the package answers the name.",
                    import.text,
                    import.file,
                    u32::from(import.range.start()),
                    u32::from(import.range.end())
                )
            });
            let remedy = match &binding.via {
                Via::GlobImport(_) | Via::NamedImport(_) => format!(
                    "rename the generated item, or narrow the import so that it no longer binds `{}`",
                    hidden.name
                ),
                Via::Prelude => "rename the generated item".to_string(),
                _ => "rename the generated item or the item that hides it".to_string(),
            };
            return Err(GenerationError {
                file: site.map_or_else(|| hidden.file.clone(), |site| site.file.clone()),
                range: site.map_or_else(|| parser::TextRange::empty(0.into()), |site| site.range),
                cause: format!(
                    "the generated `{name}`{requested_at} would be hidden in {place}: the {domain} name `{name}` is already bound there to {hidden_by}{ambiguous}. Inside the package that generates them, generated items only fill gaps, so the existing binding keeps the name and the generated {generated_domain} `{name}` would never be found.{imported} To resolve it, {remedy}",
                    name = hidden.name,
                    domain = binding.domain,
                    generated_domain = hidden.domain,
                ),
            });
        }
    }
    Ok(())
}

/// Refuses a generated member of an inherent implementation that its type
/// already has under that name. A path or call that names it would find the
/// existing member, so the generated one would never be selected.
fn check_no_hidden_generated_members(
    output: &DriverDataBase,
    modules: &[url::Url],
    requested: &Requested<'_>,
) -> Result<(), GenerationError> {
    use hir::analysis::name_resolution::hidden_generated_members;
    for url in modules {
        let Some(output_file) = output.workspace().get(output, url) else {
            continue;
        };
        if let Some(hidden) = hidden_generated_members(output, output.top_mod(output_file))
            .into_iter()
            .next()
        {
            let site = match (&hidden.header, &hidden.generated) {
                (Some(header), _) => requested.site_written_at(header),
                (None, hir::analysis::ty::ty_check::UseTarget::Generated { source, .. }) => {
                    source.as_ref().and_then(|source| requested.site_of(source))
                }
                (None, _) => None,
            };
            let requested_at = site.map_or_else(String::new, |site| {
                format!(
                    " requested at {}:{}..{}",
                    site.file,
                    u32::from(site.range.start()),
                    u32::from(site.range.end())
                )
            });
            let existing = match &hidden.hidden_by.target {
                Some(target) => format!(
                    "{}, {}",
                    hidden.hidden_by.description,
                    describe_target(target)
                ),
                None => hidden.hidden_by.description.clone(),
            };
            return Err(GenerationError {
                file: site.map_or_else(|| hidden.file.clone(), |site| site.file.clone()),
                range: site.map_or_else(|| parser::TextRange::empty(0.into()), |site| site.range),
                cause: format!(
                    "the generated `{name}`{requested_at} would be hidden: `{ty}` already has {existing}, so `{ty}::{name}` keeps meaning that member. Inside the package that generates them, generated items only fill gaps, so the generated {kind} would never be selected; rename the generated {kind} or the existing member",
                    name = hidden.name,
                    ty = hidden.self_ty,
                    kind = hidden.kind,
                ),
            });
        }
    }
    Ok(())
}

/// The first body that the evaluation of the provider instance `key` runs and
/// that does not check without errors, with its errors. Evaluation runs the
/// bodies of the calls the instance makes and of the constants it reads, so a
/// body that is ill-typed in the view the provider runs in, for instance
/// because it needs an implementation that only another provider's output
/// supplies, would be run for a value that the prepared program does not have.
fn first_ill_typed_body<'db>(
    db: &'db DriverDataBase,
    key: hir::analysis::semantic::SemanticInstanceKey<'db>,
) -> Option<(BodyOwner<'db>, Vec<common::diagnostics::CompleteDiagnostic>)> {
    let mut pending = std::collections::VecDeque::from([key]);
    let mut seen = std::collections::HashSet::new();
    while let Some(key) = pending.pop_front() {
        if !seen.insert(key) {
            continue;
        }
        let owner = key.owner(db);
        let errors = hir::analysis::ty::ty_check::check_body(db, owner)
            .0
            .iter()
            .map(|diagnostic| diagnostic.to_complete(db))
            .filter(|diagnostic| diagnostic.severity == common::diagnostics::Severity::Error)
            .collect::<Vec<_>>();
        if !errors.is_empty() {
            return Some((owner, errors));
        }
        let instance = hir::analysis::semantic::SemanticInstance::new(db, key);
        // An instance past the instance limits is rejected as itself where
        // it is checked; its calls need not end, so they are not followed.
        if hir::analysis::semantic::exceeds_instance_limits(db, instance) {
            continue;
        }
        for callee in instance.callees(db) {
            pending.push_back(callee.key);
        }
        pending.extend(instance.implicit_dependencies(db));
    }
    None
}

/// Whether the provider instance `key`, or anything its evaluation runs, waits
/// for a generated implementation. Such a provider cannot run: the
/// implementation it waits for is produced by another provider of the same
/// package.
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
        // The file over the builtin sources of the database it is prepared in,
        // which may differ from the embedded ones (`fe doc --stdlib-path`).
        ViewBasis::Standalone(file) => {
            let url = file.url(db).expect("indexed generation input");
            db.fork_inputs_selecting(|candidate| {
                *candidate == url || common::stdlib::is_embedded_builtin(candidate)
            })
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
    let diagnostics = &check_func_body(db, provider).0;
    if !body_errors(db, provider).is_empty() {
        let quoted = quoted_names(db, provider.scope().ingot(db));
        for diagnostic in diagnostics {
            if let hir::analysis::ty::diagnostics::FuncBodyDiag::Body(
                hir::analysis::ty::diagnostics::BodyDiag::UndefinedVariable(span, name, _),
            ) = diagnostic
                && quoted.contains(name.data(db).as_str())
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
        let generated = diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.severity() == common::diagnostics::Severity::Error)
            .any(|diagnostic| about_generated(db, diagnostic, &quoted));
        let complete = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.to_complete(db))
            .collect::<Vec<_>>();
        let failure = diagnostic_error(
            db,
            &complete,
            error(
                range,
                format!(
                    "authored provider body diagnostics{}",
                    view_brief(generated)
                ),
            ),
        );
        return Err(if generated {
            with_view_note(failure)
        } else {
            failure
        });
    }
    if let Some((callee, errors)) = first_ill_typed_body(db, provider_key) {
        let name = match callee {
            BodyOwner::Func(function, _) => function
                .name(db)
                .to_opt()
                .map(|name| format!("`{}`", name.data(db))),
            _ => None,
        };
        return Err(diagnostic_error(
            db,
            &errors,
            error(
                range,
                format!(
                    "the provider evaluates {}, whose body does not check without errors in the view the provider runs in; a provider may only evaluate bodies that check there, since a body that needs what generation produces would give a value that the prepared program does not have; {PROVIDER_VIEW_RULE}",
                    name.unwrap_or_else(|| "a constant".to_string())
                ),
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
