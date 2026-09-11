//! Checked value function-body generation in an explicit, isolated stage.
//!
//! An ordinary Fe const function computes a designated `FunctionBody<T>` value.
//! Only its finished value crosses stages. The supplied source template owns
//! the complete signature and context; generation replaces its empty body.
//! The resulting standalone stage uses builtin core/std and default compiler
//! options. Custom dependencies and references into the provider stage are not
//! implicitly imported. No database-specific semantic IDs cross the boundary.

use std::{collections::HashSet, fmt, ops::Range};

mod value;

use common::{
    InputDb,
    diagnostics::Severity,
    file::File,
    ingot::IngotKind,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use hir::{
    analysis::{
        semantic::{SemConstValue, eval_body_owner_const_with_args, sem_const_ty},
        ty::{
            corelib::resolve_lib_type_path, normalize::normalize_ty,
            trait_resolution::PredicateListId, ty_check::BodyOwner, ty_def::TyId,
        },
    },
    hir_def::{Func, HirIngot, ItemKind, WhereClauseOwner},
    semantic::param_env,
    span::LazySpan,
};
use parser::{
    RecoveryMode, SyntaxNode,
    ast::{self, prelude::AstNode},
};
use url::Url;

use crate::DriverDataBase;

/// Source for a standalone output stage. The named top-level function must have
/// an explicit return type and an empty body. Supporting items and callers may
/// appear in the same source. Only the selected body range is replaced.
pub struct FunctionTemplate {
    pub url: Url,
    pub source: String,
    pub function_name: String,
}

/// A logical request name within an explicitly named stage. This is not a
/// source fingerprint, semantic item ID, or authority to reuse an old artifact.
/// A fresh session can reevaluate the same identity against revised sources.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct RequestIdentity {
    pub stage: String,
    pub key: String,
}

pub struct GenerationRequest {
    pub key: String,
    pub template: FunctionTemplate,
}

/// One evaluation of a stage. Every attempted key is reserved, including failed
/// requests. Duplicate keys fail before checking or execution and are never
/// silently replayed. Start a fresh session to reevaluate a stage after edits.
/// Stage/key names are supplied by the caller, not inferred from output text or
/// allocation order. Uniqueness is enforced within this session only. The
/// attempted-key ledger is not bounded by the output budget; callers control
/// admission and the lifetime of a session.
pub struct GenerationSession {
    stage: String,
    attempted: HashSet<String>,
    budget: GenerationBudget,
}

impl GenerationSession {
    pub fn new(stage: String, budget: GenerationBudget) -> Self {
        Self {
            stage,
            attempted: HashSet::new(),
            budget,
        }
    }

    pub fn budget(&self) -> &GenerationBudget {
        &self.budget
    }

    /// Fill a checked template from a bool/u256 value or a tuple/fixed array of
    /// those values. User nominal types and cross-stage references are excluded.
    pub fn generate_value_function(
        &mut self,
        db: &DriverDataBase,
        provider: Func<'_>,
        request: GenerationRequest,
    ) -> Result<GeneratedFunction, GenerationError> {
        let identity = RequestIdentity {
            stage: self.stage.clone(),
            key: request.key.clone(),
        };
        if !self.attempted.insert(request.key) {
            let mut error = GenerationError::new(
                GenerationErrorKind::DuplicateRequest,
                "generation request key was already attempted in this session",
            );
            error.request_identity = Some(identity);
            return Err(error);
        }
        generate_function(
            db,
            provider,
            request.template,
            &mut self.budget,
            Some(identity.clone()),
        )
        .map_err(|mut error| {
            error.request_identity = Some(identity);
            error
        })
    }
}

/// Request-local accounting, independent of query-cache execution counts.
///
/// Each emitted source is charged before target analysis, even if that analysis
/// rejects it. Exhaustion never returns a partial artifact. The source limit
/// also bounds each input template before parsing. These limits do not bound
/// provider type checking or all CTFE allocations; CTFE retains its own limits.
pub struct GenerationBudget {
    max_functions: usize,
    max_source_bytes: usize,
    used_functions: usize,
    used_bytes: usize,
}

impl GenerationBudget {
    pub fn new(max_functions: usize, max_source_bytes: usize) -> Self {
        Self {
            max_functions,
            max_source_bytes,
            used_functions: 0,
            used_bytes: 0,
        }
    }

    pub fn used_functions(&self) -> usize {
        self.used_functions
    }
    pub fn used_bytes(&self) -> usize {
        self.used_bytes
    }

    fn charge(&mut self, bytes: usize) -> Result<usize, GenerationError> {
        let total = self
            .used_bytes
            .checked_add(bytes)
            .filter(|total| *total <= self.max_source_bytes)
            .ok_or_else(|| {
                GenerationError::new(
                    GenerationErrorKind::Limit,
                    "generated source byte limit exceeded",
                )
            })?;
        if self.used_functions >= self.max_functions {
            return Err(GenerationError::new(
                GenerationErrorKind::Limit,
                "generated function limit exceeded",
            ));
        }
        let invocation = self.used_functions;
        self.used_functions += 1;
        self.used_bytes = total;
        Ok(invocation)
    }
}

/// Owned provenance for the explicit stage boundary. Spans are byte offsets.
/// Bytes before/after the template body are unchanged; later offsets shift by
/// the difference between the generated and template body lengths. This receipt
/// does not give reparsed HIR items a new compiler-generated origin or hygiene.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenerationProvenance {
    pub provider_url: Url,
    pub provider_source: String,
    pub provider_span: Range<usize>,
    pub template_url: Url,
    pub template_source: String,
    pub template_body: Range<usize>,
    pub generated_body: Range<usize>,
    /// Emission ordinal for budget accounting, not logical identity.
    pub invocation: usize,
    /// Present for explicit-session requests; absent for the scalar entry.
    pub request_identity: Option<RequestIdentity>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GenerationErrorKind {
    DuplicateRequest,
    Provider,
    Protocol,
    Execution,
    Template,
    Output,
    ReturnType,
    Limit,
}

/// Diagnostics are formatted while their owning database is alive. No File IDs
/// from a discarded output database escape through an error.
#[derive(Debug)]
pub struct GenerationError {
    pub kind: GenerationErrorKind,
    pub message: String,
    pub provenance: Option<Box<GenerationProvenance>>,
    pub request_identity: Option<RequestIdentity>,
}

impl GenerationError {
    fn new(kind: GenerationErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            provenance: None,
            request_identity: None,
        }
    }

    fn with_provenance(mut self, provenance: &GenerationProvenance) -> Self {
        self.provenance = Some(Box::new(provenance.clone()));
        self
    }
}

impl fmt::Display for GenerationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}
impl std::error::Error for GenerationError {}

/// A checked source artifact and the canonical database that owns its items.
/// Consumers can request ordinary semantic/code-generation queries on this
/// database. Mutating the stage requires a new generation request.
pub struct GeneratedFunction {
    db: DriverDataBase,
    file: File,
    function_name: String,
    provenance: GenerationProvenance,
}

impl GeneratedFunction {
    pub fn database(&self) -> &DriverDataBase {
        &self.db
    }
    pub fn file(&self) -> File {
        self.file
    }
    pub fn source(&self) -> &str {
        self.file.text(&self.db)
    }
    pub fn provenance(&self) -> &GenerationProvenance {
        &self.provenance
    }
    pub fn function(&self) -> Func<'_> {
        target_function(&self.db, self.file, &self.function_name)
            .expect("checked generation artifact has its selected function")
    }
}

fn diagnostics(db: &DriverDataBase, file: File) -> Option<String> {
    let ingot = db.top_mod(file).ingot(db);
    let hir = db.run_on_ingot(ingot);
    if hir.has_errors(db) {
        return Some(hir.format_diags(db));
    }
    let semantic = db.mir_diagnostics_for_ingot(ingot);
    semantic
        .iter()
        .any(|diag| diag.severity == Severity::Error)
        .then(|| db.format_complete_diagnostics(&semantic))
}

fn target_function<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Option<Func<'db>> {
    db.top_mod(file)
        .children_non_nested(db)
        .find_map(|item| match item {
            ItemKind::Func(func)
                if func.name(db).to_opt().is_some_and(|id| id.data(db) == name) =>
            {
                Some(func)
            }
            _ => None,
        })
}

fn template_body(template: &FunctionTemplate) -> Result<Range<usize>, GenerationError> {
    let error = |message| GenerationError::new(GenerationErrorKind::Template, message);
    if template.url.scheme() != "file" || !template.url.path().ends_with(".fe") {
        return Err(error(
            "template URL must identify a standalone .fe file".to_owned(),
        ));
    }
    let (green, errors) = parser::parse_source_file(&template.source, RecoveryMode::default());
    if !errors.is_empty() {
        let messages = errors
            .iter()
            .map(|err| format!("{} at bytes {:?}", err.msg(), err.range()))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(error(format!("{}: {messages}", template.url)));
    }
    let root = ast::Root::cast(SyntaxNode::new_root(green)).expect("parser produces a root");
    let mut targets = root
        .items()
        .into_iter()
        .flatten()
        .filter_map(|item| match item.kind() {
            Some(ast::ItemKind::Func(func))
                if func
                    .sig()
                    .name()
                    .is_some_and(|name| name.text() == template.function_name) =>
            {
                Some(func)
            }
            _ => None,
        });
    let func = targets
        .next()
        .ok_or_else(|| error("template must contain the named top-level function".to_owned()))?;
    if targets.next().is_some() {
        return Err(error("template target is ambiguous".to_owned()));
    }
    if func.sig().ret_ty().is_none() {
        return Err(error(
            "template function requires an explicit return type".to_owned(),
        ));
    }
    let body = func
        .body()
        .ok_or_else(|| error("template function requires an empty body".to_owned()))?;
    if body.stmts().next().is_some() || body.items().next().is_some() {
        return Err(error("template function body must be empty".to_owned()));
    }
    Ok(body.syntax().text_range().into())
}

/// Evaluate a checked, top-level nullary const provider in a standalone ingot
/// with only builtin core/std dependencies, returning the designated
/// `core::meta::FunctionBody<bool>` or `FunctionBody<u256>`, then fill and check
/// an explicit target template. Root generics, where clauses and effects are
/// outside this entry's contract. Generic/constrained helpers remain ordinary
/// Fe calls and their obligations are checked at those call sites.
pub fn generate_scalar_function(
    db: &DriverDataBase,
    provider: Func<'_>,
    template: FunctionTemplate,
    budget: &mut GenerationBudget,
) -> Result<GeneratedFunction, GenerationError> {
    generate_function(db, provider, template, budget, None)
}

fn generate_function(
    db: &DriverDataBase,
    provider: Func<'_>,
    template: FunctionTemplate,
    budget: &mut GenerationBudget,
    request_identity: Option<RequestIdentity>,
) -> Result<GeneratedFunction, GenerationError> {
    if budget.used_functions >= budget.max_functions
        || template.source.len() > budget.max_source_bytes
    {
        return Err(GenerationError::new(
            GenerationErrorKind::Limit,
            "generation request exceeds its function or source limit",
        ));
    }
    let body_range = template_body(&template)?;
    let span = provider.span().resolve(db).ok_or_else(|| {
        GenerationError::new(
            GenerationErrorKind::Protocol,
            "provider must have an authored source location",
        )
    })?;
    let file = span.file;
    let ingot = db.top_mod(file).ingot(db);
    if ingot.kind(db) != IngotKind::StandAlone
        || ingot
            .resolved_external_ingots(db)
            .iter()
            .any(|(_, dependency)| !matches!(dependency.kind(db), IngotKind::Core | IngotKind::Std))
    {
        return Err(GenerationError::new(
            GenerationErrorKind::Protocol,
            "provider stage must be standalone with only builtin core/std dependencies",
        ));
    }
    if let Some(message) = diagnostics(db, file) {
        return Err(GenerationError::new(GenerationErrorKind::Provider, message));
    }
    if !provider.is_const(db)
        || provider.is_unsafe(db)
        || !db
            .top_mod(file)
            .children_non_nested(db)
            .any(|item| item == ItemKind::Func(provider))
        || provider.params(db).next().is_some()
        || provider.has_effects(db)
        || WhereClauseOwner::Func(provider)
            .clause(db)
            .predicates(db)
            .next()
            .is_some()
        || !provider
            .as_callable(db)
            .is_some_and(|callable| callable.params(db).is_empty())
    {
        return Err(GenerationError::new(
            GenerationErrorKind::Protocol,
            "provider must be a safe top-level nullary const function without generics, requirements, or effects",
        ));
    }
    let root = db.builtin_core().root_mod(db);
    let designated = resolve_lib_type_path(db, root.scope(), "core::meta::FunctionBody")
        .ok_or_else(|| {
            GenerationError::new(
                GenerationErrorKind::Protocol,
                "builtin core does not define FunctionBody",
            )
        })?;
    let return_ty = normalize_ty(
        db,
        provider.return_ty(db),
        provider.scope(),
        PredicateListId::empty_list(db),
    );
    let (base, args) = return_ty.decompose_ty_app(db);
    if base != designated.base_ty(db)
        || args.len() != 1
        || (request_identity.is_none() && args[0] != TyId::bool(db) && args[0] != TyId::u256(db))
    {
        return Err(GenerationError::new(
            GenerationErrorKind::Protocol,
            if request_identity.is_some() {
                "provider must return core::meta::FunctionBody with a supported value type"
            } else {
                "provider must return core::meta::FunctionBody<bool> or FunctionBody<u256>"
            },
        ));
    }
    // Preserve structural type identity even for empty aggregates. Reading the
    // shape also bounds nesting and expanded value nodes before evaluation.
    let expected = value::read_type(db, args[0])?;
    let value = eval_body_owner_const_with_args(db, BodyOwner::Func(provider), vec![], vec![])
        .map_err(|err| {
            GenerationError::new(
                GenerationErrorKind::Execution,
                format!("provider evaluation failed: {err:?}"),
            )
        })?;
    let SemConstValue::Struct { ty, fields } = value.value(db) else {
        return Err(GenerationError::new(
            GenerationErrorKind::Protocol,
            "provider did not evaluate to a FunctionBody value",
        ));
    };
    if ty != return_ty || fields.len() != 1 || sem_const_ty(db, fields[0]) != args[0] {
        return Err(GenerationError::new(
            GenerationErrorKind::Protocol,
            "provider descriptor type or fields do not match its checked return type",
        ));
    }
    let remaining = budget.max_source_bytes.saturating_sub(budget.used_bytes);
    let surrounding = template.source.len() - body_range.len();
    let body_limit = remaining.checked_sub(surrounding).ok_or_else(|| {
        GenerationError::new(
            GenerationErrorKind::Limit,
            "generated source byte limit exceeded",
        )
    })?;
    let body = value::body(db, fields[0], &expected, body_limit)?;
    let mut source = template.source.clone();
    source.replace_range(body_range.clone(), &body);
    let invocation = budget.charge(source.len())?;
    let provenance = GenerationProvenance {
        provider_url: file.url(db).ok_or_else(|| {
            GenerationError::new(GenerationErrorKind::Protocol, "provider source has no URL")
        })?,
        provider_source: file.text(db).clone(),
        provider_span: span.range.into(),
        template_url: template.url.clone(),
        template_source: template.source,
        template_body: body_range.clone(),
        generated_body: body_range.start..body_range.start + body.len(),
        invocation,
        request_identity,
    };
    let mut target = DriverDataBase::default();
    target.initialize_builtin_core();
    target.initialize_builtin_std();
    let output = target
        .workspace()
        .touch(&mut target, template.url, Some(source));
    if let Some(message) = diagnostics(&target, output) {
        return Err(
            GenerationError::new(GenerationErrorKind::Output, message).with_provenance(&provenance)
        );
    }
    let func = target_function(&target, output, &template.function_name).ok_or_else(|| {
        GenerationError::new(
            GenerationErrorKind::Template,
            "selected target function is missing after lowering",
        )
        .with_provenance(&provenance)
    })?;
    let actual = normalize_ty(
        &target,
        func.return_ty(&target),
        func.scope(),
        param_env(&target, func.into()),
    );
    if value::read_type(&target, actual).ok().as_ref() != Some(&expected) {
        return Err(GenerationError::new(
            GenerationErrorKind::ReturnType,
            format!(
                "descriptor body has type {}, but target function returns {}",
                expected,
                actual.pretty_print(&target)
            ),
        )
        .with_provenance(&provenance));
    }
    Ok(GeneratedFunction {
        db: target,
        file: output,
        function_name: template.function_name,
        provenance,
    })
}
