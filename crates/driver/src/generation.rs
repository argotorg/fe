//! Generation for derive headers: finds the headers of a source file or of a
//! package with its dependencies, evaluates each provider at compile time
//! and admits the generated items into one prepared view, which checks, builds
//! and the language server read in place of the written file.
use crate::DriverDataBase;
use common::{
    InputDb,
    file::File,
    generation::{FrozenGenerationRequest, ScopedDeriveDemand},
    ingot::IngotKind,
    stdlib::is_embedded_builtin,
};
use hir::{
    analysis::{
        diagnostics::DiagnosticVoucher,
        semantic::{CtfeError, EvalFailure, EvalOutcome, SemConstValue, SemOrigin},
        ty::{
            corelib::resolve_lib_type_path,
            trait_resolution::generation::GenerationDemand,
            ty_check::{BodyOwner, check_func_body, checked_body_demands},
            ty_def::TyId,
        },
    },
    core::meta::RecordInput,
    hir_def::HirIngot,
    span::LazySpan,
};
use parser::ast::{self, prelude::*};
use salsa::Setter;

#[derive(Clone, Debug, PartialEq, Eq)]
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
struct FileRequests<'db> {
    url: url::Url,
    /// Each trait header `impl Trait for Type with (Provider)`, with its range
    /// and the range of its provider path.
    explicit_requests: Vec<(ast::ImplTrait, parser::TextRange, parser::TextRange)>,
    /// Each inherent header `impl Type with (Provider)`, with its range and
    /// the range of its provider path.
    inherent_requests: Vec<(ast::Impl, parser::TextRange, parser::TextRange)>,
    short_form_requests: Vec<(hir::hir_def::ImplTrait<'db>, parser::TextRange, u8)>,
    has_scoped_opening: bool,
    root_range: parser::TextRange,
}

impl FileRequests<'_> {
    fn is_empty(&self) -> bool {
        self.explicit_requests.is_empty()
            && self.inherent_requests.is_empty()
            && self.short_form_requests.is_empty()
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

/// Install the prepared view and return its handle for the source file.
pub fn prepare_file_in_place(db: &mut DriverDataBase, file: File) -> Result<File, GenerationError> {
    let Some(prepared) = prepare_file(db, file)? else {
        return Ok(file);
    };
    let url = file.url(db).expect("indexed generation input");
    let prepared_file = prepared
        .workspace()
        .get(&prepared, &url)
        .expect("prepared view keeps the source file");
    *db = prepared;
    Ok(prepared_file)
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
    let Some(syntax) = scan_syntax(db, file)? else {
        return Ok(None);
    };
    // A standalone file has no dependencies whose output could be read, so the
    // authored view shows its openings.
    let Some(scan) = scan_standalone_file(db, file, syntax)? else {
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
    match admit_requests(
        db,
        &Transaction {
            basis: ViewBasis::Standalone(file),
            modules: &[url],
            dependency_openings: &[],
            standalone_root: Some(root_range),
            max_rounds,
        },
        Vec::new(),
        &[found],
    )? {
        Preparation::Complete(output, _) => Ok(Some(output)),
        Preparation::NeedsDependencies(_) => {
            unreachable!("standalone preparation has no dependencies")
        }
    }
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
    let mut seals = vec![Vec::new(); plans.len()];
    let mut additions = vec![Vec::new(); plans.len()];
    let mut opening_files = vec![Vec::new(); plans.len()];
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
            if let Some(syntax) = scan_syntax(db, file)? {
                syntaxes.push((url.clone(), syntax));
            }
        }
        opening_files[index] = syntaxes
            .iter()
            .filter(|(_, syntax)| syntax.may_open_provider)
            .map(|(url, _)| url.clone())
            .collect();
        let dependency_openings: Vec<_> = plan
            .dependencies
            .iter()
            .flat_map(|&index| opening_files[index].iter().cloned())
            .collect();
        if syntaxes.is_empty() && dependency_openings.is_empty() {
            continue;
        }
        // The one view this ingot's providers are admitted and evaluated in:
        // authored text, the sealed output of its dependencies, and the
        // implementation headers an author wrote counted as facts. The `with`
        // blocks that need a provider are found in it too, since a block can
        // name what a dependency generated. Without a sealed dependency and
        // without a request of its own, the authored view shows the same
        // blocks and no copy is made until a block needs preparing.
        let transaction = Transaction {
            basis: ViewBasis::Package,
            modules: &plan.files,
            dependency_openings: &dependency_openings,
            standalone_root: None,
            max_rounds: MAX_PREPARATION_ROUNDS,
        };
        let mut retries = 0;
        loop {
            let sealed = sealed_dependencies(plan, &seals, &additions);
            let writes_headers = syntaxes.iter().any(|(_, syntax)| syntax.has_requests);
            let wants_view = !sealed.is_empty() || writes_headers;
            let input_db = wants_view.then(|| input_view(db, &ViewBasis::Package, &sealed));
            let scanned_in = input_db.as_ref().unwrap_or(db);
            let pending = if opening_files[index].is_empty() && dependency_openings.is_empty() {
                Vec::new()
            } else {
                package_preparation_demands(scanned_in, &plan.files, &dependency_openings)
            };
            let dependency_demands: Vec<_> = if pending.iter().all(|pending| pending.demand.is_ok())
            {
                pending
                    .iter()
                    .filter(|pending| dependency_openings.contains(&pending.opening.file))
                    .filter_map(|pending| pending.demand.as_ref().ok().cloned())
                    .collect()
            } else {
                Vec::new()
            };
            let waiting = if !dependency_demands.is_empty() {
                dependency_demands
            } else {
                if !writes_headers && pending.is_empty() {
                    break;
                }
                let input_db =
                    input_db.unwrap_or_else(|| input_view(db, &ViewBasis::Package, &sealed));
                let found_requests = syntaxes
                    .iter()
                    .filter(|(_, syntax)| syntax.has_requests)
                    .map(|(url, syntax)| {
                        let file = input_db
                            .workspace()
                            .get(&input_db, url)
                            .expect("the input view holds every authored file");
                        file_requests(
                            &input_db,
                            file,
                            FileScan {
                                root: syntax.root.clone(),
                                has_scoped_opening: false,
                            },
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                match admit_requests(&input_db, &transaction, sealed, &found_requests)? {
                    Preparation::Complete(output, own) => {
                        seals[index] = own;
                        last = Some(output);
                        break;
                    }
                    Preparation::NeedsDependencies(waiting) => waiting,
                }
            };
            if retries == MAX_PREPARATION_ROUNDS {
                return Err(scoped_request_error(
                    &waiting[0],
                    "scoped dependency preparation did not converge",
                ));
            }
            retries += 1;
            prepare_dependency_requests(db, &plans, plan, &seals, &mut additions, waiting)?;
        }
    }
    // No ingot needed preparing: consumers keep using the authored view.
    let total = seals.iter().chain(&additions).map(Vec::len).sum::<usize>();
    if total == 0 {
        return Ok(None);
    }
    // The last view holds the output of its own ingot and of the ones it
    // depends on. Other ingots (several roots) need a view of everything.
    if let Some(output) = last
        && output.generation_inputs().requests(&output).len() == total
    {
        return Ok(Some(output));
    }
    let mut everything = Vec::new();
    for (seal, extra) in seals.into_iter().zip(additions) {
        everything.extend(seal);
        everything.extend(extra);
    }
    Ok(Some(prepared_view(db, &ViewBasis::Package, &everything)))
}

fn sealed_dependencies(
    plan: &IngotPlan,
    seals: &[Vec<FrozenGenerationRequest>],
    additions: &[Vec<FrozenGenerationRequest>],
) -> Vec<FrozenGenerationRequest> {
    plan.dependencies
        .iter()
        .flat_map(|&index| seals[index].iter().chain(&additions[index]).cloned())
        .collect()
}

fn prepare_dependency_requests(
    db: &DriverDataBase,
    plans: &[IngotPlan],
    consumer: &IngotPlan,
    seals: &[Vec<FrozenGenerationRequest>],
    additions: &mut [Vec<FrozenGenerationRequest>],
    demands: Vec<ScopedDeriveDemand>,
) -> Result<(), GenerationError> {
    let mut batch = std::collections::HashSet::new();
    for demand in demands {
        if !batch.insert(demand.clone()) {
            continue;
        }
        let index = consumer
            .dependencies
            .iter()
            .copied()
            .find(|&index| {
                plans[index]
                    .files
                    .binary_search(&demand.opening.file)
                    .is_ok()
            })
            .ok_or_else(|| {
                scoped_request_error(
                    &demand,
                    "the provider is outside this package's dependencies",
                )
            })?;
        if seals[index].iter().chain(&additions[index]).any(|request| {
            request
                .scoped_impl
                .as_ref()
                .is_some_and(|scoped| scoped.demand == demand)
        }) {
            return Err(scoped_request_error(
                &demand,
                "generated dependency implementation did not discharge its preparation dependency",
            ));
        }

        let owner = &plans[index];
        let mut requests = sealed_dependencies(owner, seals, additions);
        let owner_ingot = db
            .workspace()
            .containing_ingot(db, demand.opening.file.clone())
            .expect("the planned opening belongs to an ingot");
        let owner_sources = std::iter::once(owner_ingot)
            .chain(owner_ingot.transitive_dependencies(db).iter().copied())
            .flat_map(|ingot| ingot.files(db).iter().map(|(url, _)| url))
            .collect();
        // Rebind the downstream demand in the owner's source view before it
        // can supply type identities to the provider. Its own generated
        // implementations are visible only for output admission.
        let input = input_view(db, &ViewBasis::DependencyOwner(&owner_sources), &requests);
        let added = evaluate_scoped_request(&input, demand)?;
        requests.extend(seals[index].iter().cloned());
        requests.extend(additions[index].iter().cloned());
        requests.push(added.clone());
        let output = prepared_view(&input, &ViewBasis::Package, &requests);
        for url in &owner.files {
            check_preparation_module(&output, url)?;
        }
        check_scoped_implementation(&output, &added)?;
        check_conservative_extension(
            &output,
            &owner.files,
            &Requested {
                requests: &[],
                sites: &[],
            },
        )?;
        additions[index].push(added);
    }
    Ok(())
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
fn scan_syntax(db: &DriverDataBase, file: File) -> Result<Option<FileSyntax>, GenerationError> {
    let text = file.text(db);
    // Skip the parse when none of the request forms can occur in this file.
    if !["with", "abi", "event", "msg"]
        .into_iter()
        .any(|spelling| text.contains(spelling))
    {
        return Ok(None);
    }
    let parsed = hir::lower::parse_file_impl(db, db.top_mod(file));
    let root = ast::Root::cast(parser::SyntaxNode::new_root(parsed.root.clone())).unwrap();
    let parse_errors = &parsed.errors;
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
    let has_short_forms = root.syntax().descendants().any(|node| {
        (ast::Msg::can_cast(node.kind())
            || ast::Struct::cast(node.clone()).is_some_and(|item| {
                item.attr_list().is_some_and(|attrs| {
                    ["abi", "event"]
                        .into_iter()
                        .any(|name| attrs.normal_attrs_named(name).next().is_some())
                })
            }))
            && outside_quotation(&node)
    });
    if !has_requests && !has_short_forms {
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
        let diagnostics: Vec<_> = parse_errors
            .iter()
            .map(|error| error.to_complete(db))
            .collect();
        return Err(diagnostic_error(
            db,
            &diagnostics,
            GenerationError {
                file: file.url(db).expect("indexed generation input"),
                range: parser::TextRange::empty(0.into()),
                cause: "authored parse errors".into(),
            },
        ));
    }
    Ok(Some(FileSyntax {
        root,
        has_requests: has_requests || has_short_forms,
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
fn scan_standalone_file(
    db: &DriverDataBase,
    file: File,
    syntax: FileSyntax,
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
    if file
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

fn package_preparation_demands(
    db: &DriverDataBase,
    files: &[url::Url],
    dependency_openings: &[url::Url],
) -> Vec<GenerationDemand> {
    let module = |url: &url::Url| {
        let file = db
            .workspace()
            .get(db, url)
            .expect("the view holds every authored file");
        db.top_mod(file)
    };
    let modules: Vec<_> = files.iter().map(module).collect();
    if let Some(&first) = modules.first() {
        analyze_type_definitions_first(db, first);
    }
    hir::analysis::ty::trait_resolution::generation::preparation_demands_with_dependencies(
        db,
        modules,
        dependency_openings.iter().map(module),
    )
}

/// Collect and validate the explicit requests of one scanned file in the view the
/// providers run in.
fn file_requests<'db>(
    db: &'db DriverDataBase,
    file: File,
    scan: FileScan,
) -> Result<FileRequests<'db>, GenerationError> {
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
    let short_form_requests = module
        .all_impl_traits(db)
        .iter()
        .copied()
        .filter(|header| header.scope().syntax_top_mod(db) == module)
        .filter_map(|header| {
            let (range, slot) = header.short_form_derive_site(db)?;
            (!hir::analysis::ty::abi_short_form_has_authored_errors(db, header))
                .then_some((header, range, slot))
        })
        .collect();
    Ok(FileRequests {
        url: url.clone(),
        explicit_requests,
        inherent_requests,
        short_form_requests,
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
enum ViewBasis<'a> {
    /// Only the one standalone file, over the builtin libraries.
    Standalone(File),
    /// Every authored input of the database, including dependencies.
    Package,
    /// Sources owned by one dependency and the ingots it depends on.
    DependencyOwner(&'a std::collections::HashSet<url::Url>),
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
    basis: ViewBasis<'a>,
    modules: &'a [url::Url],
    dependency_openings: &'a [url::Url],
    standalone_root: Option<parser::TextRange>,
    max_rounds: usize,
}

enum Preparation {
    Complete(DriverDataBase, Vec<FrozenGenerationRequest>),
    NeedsDependencies(Vec<ScopedDeriveDemand>),
}

fn scoped_request_error(demand: &ScopedDeriveDemand, cause: impl Into<String>) -> GenerationError {
    GenerationError {
        file: demand.opening.file.clone(),
        range: demand.opening.binding_range,
        cause: cause.into(),
    }
}

fn evaluate_scoped_request(
    db: &DriverDataBase,
    demand: ScopedDeriveDemand,
) -> Result<FrozenGenerationRequest, GenerationError> {
    let request = hir::core::meta::rebind_generation_request(db, &demand)
        .map_err(|cause| scoped_request_error(&demand, cause))?;
    let requirements =
        hir::analysis::ty::trait_resolution::generation::admission_demands(db, request);
    for pending in &requirements {
        if let Err(cause) = &pending.demand {
            return Err(GenerationError {
                file: pending.opening.file.clone(),
                range: pending.opening.binding_range,
                cause: cause.clone(),
            });
        }
    }
    if !requirements.is_empty() {
        return Err(scoped_request_error(
            &demand,
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
            scoped_request_error(&demand, "scoped derive provider admission failed"),
        )
    })?;
    let file = db
        .workspace()
        .get(db, &demand.opening.file)
        .expect("the authored view keeps every authored file");
    let mut output =
        evaluate_provider_instance(db, file, key, input, demand.opening.binding_range)?;
    output.scoped_impl = Some(common::generation::FrozenScopedImpl {
        selection_name: scoped_selection_name(&demand),
        demand,
    });
    Ok(output)
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

fn check_scoped_implementation(
    db: &DriverDataBase,
    request: &FrozenGenerationRequest,
) -> Result<(), GenerationError> {
    use hir::diagnosable::Diagnosable;
    let Some(scoped) = request.scoped_impl.as_ref() else {
        return Ok(());
    };
    let implementation = hir::lower::prepared_scoped_item(db, &scoped.demand).ok_or_else(|| {
        scoped_request_error(
            &scoped.demand,
            "generated scoped implementation was not constructed",
        )
    })?;
    let mut complete = implementation
        .diags(db)
        .into_iter()
        .map(|diagnostic| diagnostic.to_complete(db))
        .collect::<Vec<_>>();
    for method in implementation.methods(db) {
        complete.extend(
            method
                .diags(db)
                .into_iter()
                .map(|diagnostic| diagnostic.to_complete(db)),
        );
    }
    if complete
        .iter()
        .any(|diagnostic| diagnostic.severity == common::diagnostics::Severity::Error)
    {
        return Err(diagnostic_error(
            db,
            &complete,
            scoped_request_error(&scoped.demand, "generated implementation admission failed"),
        ));
    }
    Ok(())
}

/// One preparation transaction over its modules. `sealed` is the output of the
/// ingots the modules depend on, already prepared and part of every view;
/// `authored` are the requests the modules write. The providers of the derive
/// headers of every file run first, and every view holds their output; each
/// round then discovers scoped demands in the output view and serves them.
/// Every provider, whichever
/// channel names it, is admitted and evaluated in the input view `db`, so the
/// order of the channels affects only how refusals are reported. A
/// standalone transaction also admits the whole output once, as before; a
/// package view is returned with its ordinary diagnostics for consumers to
/// report. The result is the output view and the requests the transaction
/// added to `sealed`.
fn admit_requests(
    db: &DriverDataBase,
    transaction: &Transaction<'_>,
    sealed: Vec<common::generation::FrozenGenerationRequest>,
    authored: &[FileRequests<'_>],
) -> Result<Preparation, GenerationError> {
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
    // `own` holds the places of the modules' own derive headers in `requests`,
    // and `sites` where each one was written, in the same order.
    let mut sites = Vec::new();
    for request in &admitted {
        let mut plan = evaluate_provider_instance(
            db,
            request.file,
            request.provider,
            request.input,
            request.site.range,
        )?;
        plan.request_impl = Some(request.site.range);
        plan.request_slot = request.slot;
        requests.push(plan);
        sites.push(request.site.clone());
    }
    debug_assert_eq!(
        requests.len() - sealed_len,
        sites.len(),
        "every request has a site"
    );
    let own = sealed_len..requests.len();
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
            check_preparation_module(&output, url)?;
        }
        // A generated header is not eligible merely because it was lowered.
        // Validate conformance before retrying any goal against the new plan.
        for request in &requests[sealed_len..] {
            check_scoped_implementation(&output, request)?;
        }
        let pending =
            package_preparation_demands(&output, modules, transaction.dependency_openings);
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
            let dependency_demands: Vec<_> = pending
                .iter()
                .filter(|demand| {
                    transaction
                        .dependency_openings
                        .contains(&demand.opening.file)
                })
                .cloned()
                .collect();
            if !dependency_demands.is_empty() {
                return Ok(Preparation::NeedsDependencies(dependency_demands));
            }
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
                completed.insert(demand.clone());
                new_requests.push(evaluate_scoped_request(db, demand)?);
            }
            last_round_first = examined
                .first()
                .map(|demand| (demand.opening.file.clone(), demand.opening.binding_range));
            requests.extend(new_requests);
            continue;
        }
        let requested = Requested {
            requests: &requests[own.clone()],
            sites: &sites,
        };
        check_conservative_extension(&output, modules, &requested)?;
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
        return Ok(Preparation::Complete(
            output,
            requests.split_off(sealed_len),
        ));
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
    site: RequestSite,
    slot: Option<u8>,
    input: RecordInput<'db>,
    provider: hir::analysis::semantic::SemanticInstanceKey<'db>,
}

/// Admit every authored request of `authored` in the input view `db`, in the
/// order they are written: the header, target and provider application of
/// each derive header. No provider code runs.
fn admit_authored_requests<'db>(
    db: &'db DriverDataBase,
    authored: &[FileRequests<'db>],
) -> Result<Vec<AdmittedRequest<'db>>, GenerationError> {
    let mut admitted = Vec::new();
    let mut short_form_errors = Vec::new();
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
        let admit_trait = |request: hir::hir_def::ImplTrait<'db>, range, path_range, slot| {
            let provider_ty = header_provider_ty(
                db,
                request.scope(),
                request.derive_provider(db),
                path_range,
                &error,
            )?;
            let (provider, input) =
                hir::core::meta::native_derive_provider(db, request, provider_ty).map_err(
                    |cause| {
                        provider_error(
                            db,
                            cause,
                            "derive target admission failed",
                            error(range, String::new()),
                        )
                    },
                )?;
            Ok::<_, GenerationError>(AdmittedRequest {
                file: authored_file,
                site: site(range),
                slot,
                input,
                provider,
            })
        };
        for (request_ast, range, path_range) in &found.explicit_requests {
            let request =
                hir::core::meta::authored_derive_request(db, authored_module, request_ast)
                    .ok_or_else(|| {
                        error(
                            *range,
                            "cannot recover authored derive header identity".into(),
                        )
                    })?;
            admitted.push(admit_trait(request, *range, *path_range, None)?);
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
                slot: None,
                input,
                provider,
            });
        }
        for &(header, range, slot) in &found.short_form_requests {
            match admit_trait(header, range, range, Some(slot)) {
                Ok(request) => admitted.push(request),
                Err(error) => {
                    // Several slots can report the same invalid declaration.
                    // Keep that report once and still check independent forms.
                    if !short_form_errors.contains(&error) {
                        short_form_errors.push(error);
                    }
                }
            }
        }
    }
    let mut short_form_errors = short_form_errors.into_iter();
    if let Some(mut first) = short_form_errors.next() {
        use std::fmt::Write;
        for error in short_form_errors {
            write!(first.cause, "\n{error}").unwrap();
        }
        return Err(first);
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
            match code {
                FrozenCode::Template {
                    quote_file,
                    quote_range,
                    holes,
                    ..
                } => {
                    (quote_file == &source.0 && quote_range.contains_range(source.1))
                        || holes.iter().any(|hole| match hole {
                            FrozenHoleValue::Code(nested) => holds(nested, source),
                            _ => false,
                        })
                }
                FrozenCode::Construct { fields, .. } | FrozenCode::FieldStatements { fields } => {
                    fields.iter().any(|field| holds(field, source))
                }
                _ => false,
            }
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
        format!("{:?}", demand.trait_args),
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
    basis: &ViewBasis<'_>,
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
    basis: &ViewBasis<'_>,
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
        ViewBasis::DependencyOwner(sources) => db.fork_inputs_selecting(|url| {
            // Keep manifests and resolution settings. Source identities past
            // this boundary must be compiler-provided or belong to the owner's
            // resolved dependency closure.
            !url.path().ends_with(".fe") || is_embedded_builtin(url) || sources.contains(url)
        }),
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
    if provider_needs_generation(db, provider_key) {
        return Err(error(
            range,
            format!(
                "the provider runs code that opens an implementation only generation can supply; {PROVIDER_VIEW_RULE}"
            ),
        ));
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
             impl Derive<trait Value> for Provider {\n\
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
