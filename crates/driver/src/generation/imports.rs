//! Checked binding of one generated package function into a fresh consumer.
//!
//! The complete generated source is copied into a path dependency. A frozen
//! function carries owned interface and provenance data, never HIR identities
//! from its original database. Binding rechecks both packages and proves that
//! the emitted direct call resolves to the selected function in the new
//! database.

use std::{fmt, ops::Range, rc::Rc};

use common::{
    InputDb,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use hir::{
    analysis::ty::{
        normalize::normalize_ty, trait_resolution::PredicateListId, ty_check::check_func_body,
        ty_def::TyId,
    },
    hir_def::{CallableDef, Expr, Func, FuncParamMode, ItemKind, Partial, WhereClauseOwner},
    span::LazySpan,
};
use parser::{
    RecoveryMode, SyntaxNode,
    ast::{self, prelude::AstNode},
};
use url::Url;

use super::{
    FunctionTemplate, GeneratedFunction, GenerationBudget, GenerationProvenance, diagnostics,
    target_function, template_body,
};
use crate::DriverDataBase;

const IMPORT_ALIAS: &str = "__frozen_import";
const ARTIFACT_CONFIG_URL: &str = "file:///frozen-bind/artifact/fe.toml";
const ARTIFACT_SOURCE_URL: &str = "file:///frozen-bind/artifact/src/lib.fe";
const CONSUMER_CONFIG_URL: &str = "file:///frozen-bind/consumer/fe.toml";
const CONSUMER_SOURCE_URL: &str = "file:///frozen-bind/consumer/src/lib.fe";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ImportErrorKind {
    Export,
    Template,
    Output,
    Binding,
    Limit,
}

/// An import failure with database-independent diagnostics and, once emission
/// has happened, the complete owned receipt for the attempted binding.
#[derive(Debug)]
pub struct ImportError {
    pub kind: ImportErrorKind,
    pub message: String,
    pub receipt: Option<Box<FrozenImportReceipt>>,
}

impl ImportError {
    fn new(kind: ImportErrorKind, message: impl Into<String>) -> Self {
        Self {
            kind,
            message: message.into(),
            receipt: None,
        }
    }

    fn with_receipt(mut self, receipt: &FrozenImportReceipt) -> Self {
        self.receipt = Some(Box::new(receipt.clone()));
        self
    }
}

impl fmt::Display for ImportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ImportError {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScalarType {
    Bool,
    U256,
}

impl fmt::Display for ScalarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Bool => "bool",
            Self::U256 => "u256",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParamInterface {
    ty: ScalarType,
    label: Option<String>,
    mode: FuncParamMode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExportInterface {
    params: Vec<ParamInterface>,
    result: ScalarType,
    selected_span: Range<usize>,
    selected_source: String,
}

/// Owns a checked generated package so exported selections remain valid after
/// the caller drops its original handle.
#[derive(Clone)]
pub struct FrozenArtifact {
    artifact: Rc<GeneratedFunction>,
}

impl FrozenArtifact {
    pub fn new(artifact: GeneratedFunction) -> Self {
        Self {
            artifact: Rc::new(artifact),
        }
    }

    /// Select a public safe monomorphic const function with a bool/u256-only
    /// interface. The whole package remains visible when this export is bound.
    pub fn export(&self, name: &str) -> Result<FrozenFunction, ImportError> {
        let db = self.artifact.database();
        let file = self.artifact.file();
        if let Some(message) = diagnostics(db, file) {
            return Err(ImportError::new(ImportErrorKind::Export, message));
        }
        let func = target_function(db, file, name).ok_or_else(|| {
            ImportError::new(
                ImportErrorKind::Export,
                format!("missing frozen export function `{name}`"),
            )
        })?;
        if !func.vis(db).is_pub()
            || !func.is_const(db)
            || func.is_unsafe(db)
            || func.has_effects(db)
            || !func
                .as_callable(db)
                .is_some_and(|callable| callable.params(db).is_empty())
            || WhereClauseOwner::Func(func)
                .clause(db)
                .predicates(db)
                .next()
                .is_some()
            || !db
                .top_mod(file)
                .children_non_nested(db)
                .any(|item| item == ItemKind::Func(func))
        {
            return Err(ImportError::new(
                ImportErrorKind::Export,
                "frozen export must be a checked public safe top-level const function without generics, requirements, or effects",
            ));
        }
        let interface = export_interface(db, file, func)
            .map_err(|message| ImportError::new(ImportErrorKind::Export, message))?;
        Ok(FrozenFunction {
            artifact: Rc::clone(&self.artifact),
            name: name.to_owned(),
            interface,
        })
    }
}

/// An owned selection from a frozen artifact. Its semantic interface is
/// intentionally opaque and is revalidated after package materialization.
#[derive(Clone)]
pub struct FrozenFunction {
    artifact: Rc<GeneratedFunction>,
    name: String,
    interface: ExportInterface,
}

impl FrozenFunction {
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// Database-independent evidence for a completed or post-emission failed bind.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrozenImportReceipt {
    pub original_provenance: GenerationProvenance,
    pub original_source: String,
    pub selected_name: String,
    pub selected_source: String,
    pub selected_span: Range<usize>,
    pub materialized_source_url: Url,
    pub consumer_source_url: Url,
    pub consumer_source: String,
    pub template_url: Url,
    pub template_source: String,
    pub template_body: Range<usize>,
    pub emitted_call: Range<usize>,
    pub invocation: usize,
}

/// A checked consumer and the database that owns both its function and the
/// selected imported function used by its emitted call.
pub struct BoundFunction {
    db: DriverDataBase,
    file: File,
    function_name: String,
    receipt: FrozenImportReceipt,
}

impl BoundFunction {
    pub fn database(&self) -> &DriverDataBase {
        &self.db
    }

    pub fn file(&self) -> File {
        self.file
    }

    pub fn source(&self) -> &str {
        self.file.text(&self.db)
    }

    pub fn function(&self) -> Func<'_> {
        target_function(&self.db, self.file, &self.function_name)
            .expect("checked bound artifact has its selected function")
    }

    pub fn receipt(&self) -> &FrozenImportReceipt {
        &self.receipt
    }
}

/// Fill one empty function template with a direct call to a frozen export.
/// `parameters` maps each export argument, in declaration order, to a template
/// parameter index. Reordered and repeated indices are preserved for ordinary
/// Fe ownership checking.
pub fn bind_function(
    template: FunctionTemplate,
    target: &FrozenFunction,
    parameters: &[usize],
    budget: &mut GenerationBudget,
) -> Result<BoundFunction, ImportError> {
    let minimum_bytes = target
        .artifact
        .source()
        .len()
        .checked_add(template.source.len())
        .ok_or_else(source_limit)?;
    let remaining = budget.max_source_bytes.saturating_sub(budget.used_bytes);
    if budget.used_functions >= budget.max_functions {
        return Err(ImportError::new(
            ImportErrorKind::Limit,
            "frozen binding function limit exceeded",
        ));
    }
    if minimum_bytes > remaining {
        return Err(source_limit());
    }

    let body_range = template_body(&template)
        .map_err(|error| ImportError::new(ImportErrorKind::Template, error.message))?;
    let names = template_param_names(&template)?;
    if parameters.len() != target.interface.params.len() {
        return Err(ImportError::new(
            ImportErrorKind::Template,
            format!(
                "binding supplies {} parameter indices for {} frozen parameters",
                parameters.len(),
                target.interface.params.len()
            ),
        ));
    }

    let surrounding = template.source.len() - body_range.len();
    let call_limit = remaining
        .checked_sub(target.artifact.source().len())
        .and_then(|bytes| bytes.checked_sub(surrounding))
        .and_then(|bytes| bytes.checked_sub(4))
        .ok_or_else(source_limit)?;
    let mut call = String::new();
    push_bounded(&mut call, IMPORT_ALIAS, call_limit)?;
    push_bounded(&mut call, "::", call_limit)?;
    push_bounded(&mut call, &target.name, call_limit)?;
    push_bounded(&mut call, "(", call_limit)?;
    for (argument_index, (export_param, &index)) in
        target.interface.params.iter().zip(parameters).enumerate()
    {
        let name = names.get(index).ok_or_else(|| {
            ImportError::new(
                ImportErrorKind::Template,
                format!("binding parameter index {index} is out of range"),
            )
        })?;
        let name = name.as_deref().ok_or_else(|| {
            ImportError::new(
                ImportErrorKind::Template,
                format!("binding parameter index {index} has no usable value name"),
            )
        })?;
        if argument_index != 0 {
            push_bounded(&mut call, ", ", call_limit)?;
        }
        if let Some(label) = &export_param.label {
            push_bounded(&mut call, label, call_limit)?;
            push_bounded(&mut call, ": ", call_limit)?;
        }
        push_bounded(&mut call, name, call_limit)?;
    }
    push_bounded(&mut call, ")", call_limit)?;
    let body = format!("{{ {call} }}");
    let completed_len = template
        .source
        .len()
        .checked_sub(body_range.len())
        .and_then(|bytes| bytes.checked_add(body.len()))
        .ok_or_else(source_limit)?;
    let charged_bytes = target
        .artifact
        .source()
        .len()
        .checked_add(completed_len)
        .ok_or_else(source_limit)?;
    if charged_bytes > remaining {
        return Err(source_limit());
    }

    let mut source = template.source.clone();
    source.replace_range(body_range.clone(), &body);
    let emitted_call = body_range.start + 2..body_range.start + 2 + call.len();
    let invocation = budget
        .charge(charged_bytes)
        .map_err(|error| ImportError::new(ImportErrorKind::Limit, error.message))?;
    let materialized_source_url =
        Url::parse(ARTIFACT_SOURCE_URL).expect("fixed frozen import source URL is valid");
    let consumer_source_url =
        Url::parse(CONSUMER_SOURCE_URL).expect("fixed frozen consumer URL is valid");
    let receipt = FrozenImportReceipt {
        original_provenance: target.artifact.provenance().clone(),
        original_source: target.artifact.source().to_owned(),
        selected_name: target.name.clone(),
        selected_source: target.interface.selected_source.clone(),
        selected_span: target.interface.selected_span.clone(),
        materialized_source_url: materialized_source_url.clone(),
        consumer_source_url,
        consumer_source: source.clone(),
        template_url: template.url,
        template_source: template.source,
        template_body: body_range,
        emitted_call: emitted_call.clone(),
        invocation,
    };

    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    touch(
        &mut db,
        ARTIFACT_CONFIG_URL,
        "[ingot]\nname = \"frozen_artifact\"\nversion = \"0.0.0\"\n".to_owned(),
    )
    .map_err(|error| error.with_receipt(&receipt))?;
    touch(
        &mut db,
        CONSUMER_CONFIG_URL,
        format!(
            "[ingot]\nname = \"frozen_consumer\"\nversion = \"0.0.0\"\n\n[dependencies]\n{IMPORT_ALIAS} = {{ path = \"../artifact\" }}\n"
        ),
    )
    .map_err(|error| error.with_receipt(&receipt))?;
    let artifact_file = db.workspace().touch(
        &mut db,
        materialized_source_url,
        Some(receipt.original_source.clone()),
    );
    let file = touch(&mut db, CONSUMER_SOURCE_URL, source)
        .map_err(|error| error.with_receipt(&receipt))?;

    if let Some(message) = diagnostics(&db, artifact_file) {
        return Err(ImportError::new(ImportErrorKind::Output, message).with_receipt(&receipt));
    }
    if artifact_file.text(&db) != &receipt.original_source {
        return Err(ImportError::new(
            ImportErrorKind::Binding,
            "materialized frozen package changed source bytes",
        )
        .with_receipt(&receipt));
    }
    let imported = target_function(&db, artifact_file, &target.name).ok_or_else(|| {
        ImportError::new(
            ImportErrorKind::Binding,
            "materialized frozen package lost the selected function",
        )
        .with_receipt(&receipt)
    })?;
    let imported_interface = export_interface(&db, artifact_file, imported).map_err(|message| {
        ImportError::new(ImportErrorKind::Binding, message).with_receipt(&receipt)
    })?;
    if imported_interface != target.interface {
        return Err(ImportError::new(
            ImportErrorKind::Binding,
            "materialized frozen function changed its checked interface",
        )
        .with_receipt(&receipt));
    }

    if let Some(message) = diagnostics(&db, file) {
        return Err(ImportError::new(ImportErrorKind::Output, message).with_receipt(&receipt));
    }
    let bound = target_function(&db, file, &template.function_name).ok_or_else(|| {
        ImportError::new(
            ImportErrorKind::Binding,
            "selected consumer function is missing after lowering",
        )
        .with_receipt(&receipt)
    })?;
    validate_recipient_interface(&db, bound, &target.interface, parameters, &receipt)?;
    validate_call_binding(&db, file, bound, imported, emitted_call, &receipt)?;

    Ok(BoundFunction {
        db,
        file,
        function_name: template.function_name,
        receipt,
    })
}

fn source_limit() -> ImportError {
    ImportError::new(
        ImportErrorKind::Limit,
        "frozen binding source byte limit exceeded",
    )
}

fn push_bounded(output: &mut String, text: &str, limit: usize) -> Result<(), ImportError> {
    output
        .len()
        .checked_add(text.len())
        .filter(|length| *length <= limit)
        .ok_or_else(source_limit)?;
    output.push_str(text);
    Ok(())
}

fn touch(db: &mut DriverDataBase, url: &str, source: String) -> Result<File, ImportError> {
    let url = Url::parse(url).map_err(|error| {
        ImportError::new(
            ImportErrorKind::Binding,
            format!("invalid internal frozen import URL: {error}"),
        )
    })?;
    Ok(db.workspace().touch(db, url, Some(source)))
}

fn scalar_type(db: &DriverDataBase, ty: TyId<'_>) -> Result<ScalarType, String> {
    if ty == TyId::bool(db) {
        Ok(ScalarType::Bool)
    } else if ty == TyId::u256(db) {
        Ok(ScalarType::U256)
    } else {
        Err(format!(
            "frozen functions support only bool and u256, found `{}`",
            ty.pretty_print(db)
        ))
    }
}

fn param_scalar_type(
    db: &DriverDataBase,
    ty: TyId<'_>,
    mode: FuncParamMode,
) -> Result<ScalarType, String> {
    match mode {
        FuncParamMode::Own => scalar_type(db, ty),
        FuncParamMode::View => {
            let inner = ty.as_view(db).ok_or_else(|| {
                format!(
                    "frozen view parameters require an ordinary bool or u256 value, found `{}`",
                    ty.pretty_print(db)
                )
            })?;
            scalar_type(db, inner)
        }
    }
}

fn export_interface(
    db: &DriverDataBase,
    file: File,
    func: Func<'_>,
) -> Result<ExportInterface, String> {
    let arg_tys = func.arg_tys(db);
    let params = func.params(db).collect::<Vec<_>>();
    if arg_tys.len() != params.len() {
        return Err("frozen export parameter metadata is inconsistent".to_owned());
    }
    let mut interface_params = Vec::with_capacity(params.len());
    for (param, ty) in params.into_iter().zip(arg_tys) {
        let mode = param.mode(db);
        let ty = normalize_ty(
            db,
            *ty.skip_binder(),
            func.scope(),
            PredicateListId::empty_list(db),
        );
        interface_params.push(ParamInterface {
            ty: param_scalar_type(db, ty, mode)?,
            label: param.label(db).map(|label| label.data(db).to_string()),
            mode,
        });
    }
    let result = normalize_ty(
        db,
        func.return_ty(db),
        func.scope(),
        PredicateListId::empty_list(db),
    );
    let span = func
        .span()
        .resolve(db)
        .ok_or_else(|| "frozen export requires an authored source span".to_owned())?;
    if span.file != file {
        return Err("frozen export span belongs to a different source file".to_owned());
    }
    let selected_span: Range<usize> = span.range.into();
    let selected_source = file
        .text(db)
        .get(selected_span.clone())
        .ok_or_else(|| "frozen export span is outside its source file".to_owned())?
        .to_owned();
    Ok(ExportInterface {
        params: interface_params,
        result: scalar_type(db, result)?,
        selected_span,
        selected_source,
    })
}

fn template_param_names(template: &FunctionTemplate) -> Result<Vec<Option<String>>, ImportError> {
    let (green, errors) = parser::parse_source_file(&template.source, RecoveryMode::default());
    if !errors.is_empty() {
        return Err(ImportError::new(
            ImportErrorKind::Template,
            "template parameters cannot be read from invalid source",
        ));
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
    let func = targets.next().ok_or_else(|| {
        ImportError::new(
            ImportErrorKind::Template,
            "template must contain the named top-level function",
        )
    })?;
    let params = func.sig().params().ok_or_else(|| {
        ImportError::new(
            ImportErrorKind::Template,
            "template function requires a parameter list",
        )
    })?;
    Ok(params
        .into_iter()
        .map(|param| {
            param.name().and_then(|name| match name {
                ast::FuncParamName::Ident(token) => Some(token.text().to_owned()),
                ast::FuncParamName::SelfParam(_) | ast::FuncParamName::Underscore(_) => None,
            })
        })
        .collect())
}

fn validate_recipient_interface(
    db: &DriverDataBase,
    bound: Func<'_>,
    expected: &ExportInterface,
    parameters: &[usize],
    receipt: &FrozenImportReceipt,
) -> Result<(), ImportError> {
    let arg_tys = bound.arg_tys(db);
    let params = bound.params(db).collect::<Vec<_>>();
    if arg_tys.len() != params.len() {
        return Err(ImportError::new(
            ImportErrorKind::Binding,
            "consumer parameter metadata is inconsistent",
        )
        .with_receipt(receipt));
    }
    for (frozen, &index) in expected.params.iter().zip(parameters) {
        let param = params.get(index).ok_or_else(|| {
            ImportError::new(
                ImportErrorKind::Binding,
                "consumer parameter mapping changed after lowering",
            )
            .with_receipt(receipt)
        })?;
        let ty = arg_tys.get(index).ok_or_else(|| {
            ImportError::new(
                ImportErrorKind::Binding,
                "consumer parameter type mapping changed after lowering",
            )
            .with_receipt(receipt)
        })?;
        let normalized = normalize_ty(
            db,
            *ty.skip_binder(),
            bound.scope(),
            PredicateListId::empty_list(db),
        );
        let actual = param_scalar_type(db, normalized, param.mode(db)).map_err(|message| {
            ImportError::new(ImportErrorKind::Binding, message).with_receipt(receipt)
        })?;
        if actual != frozen.ty {
            return Err(ImportError::new(
                ImportErrorKind::Binding,
                format!(
                    "consumer parameter {index} has scalar type {actual}, but frozen argument requires {}",
                    frozen.ty
                ),
            )
            .with_receipt(receipt));
        }
    }
    let result = normalize_ty(
        db,
        bound.return_ty(db),
        bound.scope(),
        PredicateListId::empty_list(db),
    );
    let actual = scalar_type(db, result).map_err(|message| {
        ImportError::new(ImportErrorKind::Binding, message).with_receipt(receipt)
    })?;
    if actual != expected.result {
        return Err(ImportError::new(
            ImportErrorKind::Binding,
            format!(
                "consumer returns {actual}, but frozen function returns {}",
                expected.result
            ),
        )
        .with_receipt(receipt));
    }
    Ok(())
}

fn validate_call_binding(
    db: &DriverDataBase,
    file: File,
    bound: Func<'_>,
    imported: Func<'_>,
    call_range: Range<usize>,
    receipt: &FrozenImportReceipt,
) -> Result<(), ImportError> {
    let typed = &check_func_body(db, bound).1;
    let Some(body) = typed.body() else {
        return Err(ImportError::new(
            ImportErrorKind::Binding,
            "selected consumer function has no typed body",
        )
        .with_receipt(receipt));
    };
    let mut resolved = Vec::new();
    for expr in body.exprs(db).keys() {
        if !matches!(expr.data(db, body), Partial::Present(Expr::Call(..))) {
            continue;
        }
        let Some(span) = expr.span(body).resolve(db) else {
            continue;
        };
        let range: Range<usize> = span.range.into();
        if span.file == file && range == call_range {
            resolved.push(typed.callable_expr(expr).map(|call| call.callable_def()));
        }
    }
    if resolved.len() != 1 || resolved[0] != Some(CallableDef::Func(imported)) {
        return Err(ImportError::new(
            ImportErrorKind::Binding,
            "emitted call did not resolve to the selected frozen function",
        )
        .with_receipt(receipt));
    }
    Ok(())
}
