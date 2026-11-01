//! One authored input pass, then one ordinary output checking pass.
//! This bounded adapter supports explicit providers on concrete root records.
use crate::DriverDataBase;
use common::{InputDb, file::File, ingot::IngotKind};
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
    hir_def::CallableDef,
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

fn diagnostic_error(
    db: &DriverDataBase,
    diagnostics: &[common::diagnostics::CompleteDiagnostic],
    mut fallback: GenerationError,
) -> GenerationError {
    if let Some(span) = diagnostics
        .iter()
        .find_map(|diagnostic| diagnostic.primary_span())
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
            return diagnostic_error(
                db,
                &complete,
                GenerationError {
                    cause: "authored const helper diagnostics".into(),
                    ..fallback
                },
            );
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
    let (green, parse_errors) =
        parser::parse_source_file(file.text(db), parser::RecoveryMode::Recover);
    let root = ast::Root::cast(parser::SyntaxNode::new_root(green)).unwrap();
    if !root
        .syntax()
        .descendants()
        .filter_map(ast::ImplTrait::cast)
        .any(|request| {
            request.derive_provider().is_some()
                && !request
                    .syntax()
                    .ancestors()
                    .any(|node| ast::QuoteExpr::cast(node).is_some())
        })
        && !root
            .syntax()
            .descendants()
            .filter_map(ast::NormalAttr::cast)
            .any(|attr| {
                attr.is_named("generate")
                    && !attr
                        .syntax()
                        .ancestors()
                        .any(|node| ast::QuoteExpr::cast(node).is_some())
            })
    {
        return Ok(None);
    }
    if !parse_errors.is_empty() {
        return Err(error(
            parser::TextRange::empty(0.into()),
            format!("authored parse errors: {parse_errors:?}"),
        ));
    }
    if file
        .containing_ingot(db)
        .is_none_or(|ingot| ingot.kind(db) != IngotKind::StandAlone)
    {
        return Err(error(
            parser::TextRange::empty(0.into()),
            "bounded generation currently requires a standalone source file".into(),
        ));
    }
    let module = db.top_mod(file);
    let mut requests = Vec::new();
    let mut consumed = std::collections::HashSet::new();
    for item in root.items().into_iter().flatten() {
        let attributes = match item.kind() {
            Some(ast::ItemKind::Struct(record)) => record.attr_list(),
            Some(ast::ItemKind::Enum(enum_)) => enum_.attr_list(),
            _ => continue,
        };
        let Some(attributes) = attributes else {
            continue;
        };
        for attribute in attributes.normal_attrs_named("generate") {
            let range = attribute.syntax().text_range();
            consumed.insert(range);
            let args = attribute
                .args()
                .map(|args| args.into_iter().collect::<Vec<_>>())
                .unwrap_or_default();
            if args.len() != 1 || args[0].value().is_some() {
                return Err(error(
                    range,
                    "use #[generate(provider_path)] with one ordinary const function".into(),
                ));
            }
            let path = args[0]
                .key()
                .ok_or_else(|| error(range, "generation provider must be an item path".into()))?;
            let segments = path.segments().map(|segment| {
                if segment.generic_args().is_some() || segment.qualified_type().is_some() {
                    return Err(error(segment.syntax().text_range(),
                        "generation provider path does not support generic or qualified segments".into()));
                }
                segment.ident().map(|ident| ident.text().to_string()).ok_or_else(|| {
                    error(segment.syntax().text_range(), "generation provider path requires identifier segments".into())
                })
            }).collect::<Result<Vec<_>, _>>()?;
            let segments = segments.iter().map(String::as_str).collect::<Vec<_>>();
            let resolved = hir::analysis::name_resolution::resolve_path(
                db,
                hir::hir_def::PathId::from_segments(db, &segments),
                module.scope(),
                hir::analysis::ty::trait_resolution::PredicateListId::empty_list(db),
                true,
            )
            .map_err(|_| error(range, "cannot resolve authored generation provider".into()))?;
            let hir::analysis::name_resolution::PathRes::Func(provider_ty) = resolved else {
                return Err(error(
                    range,
                    "generation provider must resolve to an ordinary function".into(),
                ));
            };
            let hir::analysis::ty::ty_def::TyData::TyBase(hir::analysis::ty::ty_def::TyBase::Func(
                CallableDef::Func(provider),
            )) = provider_ty.data(db)
            else {
                return Err(error(
                    range,
                    "generation provider must resolve to an ordinary function".into(),
                ));
            };
            let provider = *provider;
            if !hir::analysis::name_resolution::is_scope_visible_from(
                db,
                provider.scope(),
                module.scope(),
            ) {
                return Err(error(
                    range,
                    "generation provider is not visible from the request".into(),
                ));
            }
            let input = RecordInput::for_authored_item(db, module, &item).map_err(|cause| {
                error(
                    range,
                    format!("cannot issue concrete authored record: {cause:?}"),
                )
            })?;
            let (target_item, target_span) = match input.declaration() {
                hir::analysis::ty::adt_def::AdtRef::Struct(record) => {
                    (hir::hir_def::ItemKind::Struct(record), record.span().into())
                }
                hir::analysis::ty::adt_def::AdtRef::Enum(enumeration) => (
                    hir::hir_def::ItemKind::Enum(enumeration),
                    enumeration.span().into(),
                ),
            };
            if !input.ty().has_param(db) {
                hir::analysis::ty::ty_check::admit_ground_type_application(
                    db,
                    input.ty(),
                    target_item,
                    target_span,
                )
                .map_err(|failure| {
                    let diagnostics = failure
                        .vouchers()
                        .iter()
                        .map(|voucher| voucher.to_complete(db))
                        .collect::<Vec<_>>();
                    diagnostic_error(
                        db,
                        &diagnostics,
                        error(
                            range,
                            format!(
                                "generation target admission failed: {}",
                                failure.description()
                            ),
                        ),
                    )
                })?;
            }
            requests.push(evaluate_provider(db, file, provider, input, range)?);
        }
    }
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
        // Nested declarations are a documented pending interface, never ignored.
        if !root
            .items()
            .into_iter()
            .flatten()
            .any(|item| item.syntax().text_range() == range)
        {
            return Err(error(
                range,
                "bounded native derive currently requires a root declaration".into(),
            ));
        }
        let request = hir::core::meta::authored_derive_request(db, module, &request_ast)
            .ok_or_else(|| {
                error(
                    range,
                    "cannot recover authored derive header identity".into(),
                )
            })?;
        let segments = path.segments().map(|segment| {
            if segment.generic_args().is_some() || segment.qualified_type().is_some() {
                return Err(error(segment.syntax().text_range(), "bounded native derive provider path does not support generic or qualified segments".into()));
            }
            segment.ident().map(|token| token.text().to_string())
                .ok_or_else(|| error(segment.syntax().text_range(), "invalid native derive provider path segment".into()))
        }).collect::<Result<Vec<_>, _>>()?;
        let segments = segments.iter().map(String::as_str).collect::<Vec<_>>();
        let resolved = hir::analysis::name_resolution::resolve_path(
            db,
            hir::hir_def::PathId::from_segments(db, &segments),
            request.scope(),
            hir::analysis::ty::trait_resolution::PredicateListId::empty_list(db),
            true,
        )
        .map_err(|_| {
            error(
                path.syntax().text_range(),
                "cannot resolve native derive provider type".into(),
            )
        })?;
        if let hir::analysis::name_resolution::PathRes::TyAlias(alias, _) = &resolved
            && !hir::analysis::name_resolution::is_scope_visible_from(
                db,
                alias.alias.scope(),
                request.scope(),
            )
        {
            return Err(error(
                path.syntax().text_range(),
                "native derive provider alias is not visible".into(),
            ));
        }
        let (hir::analysis::name_resolution::PathRes::Ty(provider_ty)
        | hir::analysis::name_resolution::PathRes::TyAlias(_, provider_ty)) = resolved
        else {
            return Err(error(
                path.syntax().text_range(),
                "native derive provider must name a type implementing Derive<G>".into(),
            ));
        };
        if let Some(scope) = provider_ty.as_scope(db)
            && !hir::analysis::name_resolution::is_scope_visible_from(db, scope, request.scope())
        {
            return Err(error(
                path.syntax().text_range(),
                "native derive provider type is not visible".into(),
            ));
        }
        let (provider, input) = hir::core::meta::native_derive_provider(db, request, provider_ty)
            .map_err(|cause| match cause {
            hir::core::meta::NativeDeriveProviderError::Message(message) => error(range, message),
            hir::core::meta::NativeDeriveProviderError::Provider(failure) => {
                let diagnostics = failure
                    .vouchers()
                    .iter()
                    .map(|voucher| voucher.to_complete(db))
                    .collect::<Vec<_>>();
                diagnostic_error(
                    db,
                    &diagnostics,
                    error(
                        range,
                        format!(
                            "provider application not admitted: {}",
                            failure.description()
                        ),
                    ),
                )
            }
            hir::core::meta::NativeDeriveProviderError::Target(failure) => {
                let diagnostics = failure
                    .vouchers()
                    .iter()
                    .map(|voucher| voucher.to_complete(db))
                    .collect::<Vec<_>>();
                diagnostic_error(
                    db,
                    &diagnostics,
                    error(
                        range,
                        format!(
                            "native derive target admission failed: {}",
                            failure.description()
                        ),
                    ),
                )
            }
        })?;
        let mut plan = evaluate_provider_instance(db, file, provider, input, range)?;
        plan.request_impl = Some(range);
        requests.push(plan);
    }
    for attribute in root
        .syntax()
        .descendants()
        .filter_map(ast::NormalAttr::cast)
    {
        if attribute.is_named("generate")
            && !attribute
                .syntax()
                .ancestors()
                .any(|node| ast::QuoteExpr::cast(node).is_some())
            && !consumed.contains(&attribute.syntax().text_range())
        {
            return Err(error(
                attribute.syntax().text_range(),
                "generation request requires an authored concrete root record".into(),
            ));
        }
    }
    if requests.is_empty() {
        return Err(error(
            parser::TextRange::empty(0.into()),
            "generation request requires an authored concrete root record".into(),
        ));
    }
    let mut output = DriverDataBase::default();
    let output_file =
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
        .compilation_settings()
        .set_target(&mut output)
        .to(db.compilation_settings().target(db));
    output
        .generation_inputs()
        .set_requests(&mut output)
        .to(requests);
    let output_module = output.top_mod(output_file);
    let _ = output_module.scope_graph(&output);
    let lowering_errors = hir::lower::generation_lowering_errors(&output, output_module);
    if let Some(first) = lowering_errors.first() {
        return Err(GenerationError {
            file: first.file.url(&output).unwrap_or(url),
            range: first.primary_range,
            cause: first.message.clone(),
        });
    }
    let diagnostics = output.run_on_top_mod(output_module);
    if diagnostics.has_errors(&output) {
        return Err(diagnostic_error(
            &output,
            &diagnostics.finalize(&output),
            error(
                root.syntax().text_range(),
                "generated output admission failed".into(),
            ),
        ));
    }
    drop(diagnostics);
    // Source providers remain in the output source view, but no provider has
    // been re-evaluated and no generated declaration is used as authored input.
    Ok(Some(output))
}

fn evaluate_provider<'db>(
    db: &'db DriverDataBase,
    file: File,
    provider: hir::hir_def::Func<'db>,
    input: RecordInput<'db>,
    range: parser::TextRange,
) -> Result<common::generation::FrozenGenerationRequest, GenerationError> {
    let key = hir::analysis::semantic::generated_callee_key(
        db,
        input.scope(),
        hir::analysis::ty::trait_resolution::PredicateListId::empty_list(db),
        provider,
        None,
        &[],
    )
    .map_err(|_| GenerationError {
        file: file.url(db).expect("indexed generation input"),
        range,
        cause: "provider function could not be admitted".into(),
    })?;
    evaluate_provider_instance(db, file, key, input, range)
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
    if !provider.is_const(db)
        || provider.body(db).is_none()
        || !CallableDef::Func(provider).params(db).is_empty()
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
        return Err(diagnostic_error(
            db,
            &complete,
            error(range, "authored provider body diagnostics".into()),
        ));
    }
    let owner = provider_key.owner(db);
    let instance = hir::analysis::semantic::SemanticInstance::new(db, provider_key);
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
                        validation_diagnostics.extend(errors);
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
                        validation_diagnostics.extend(errors);
                    } else {
                        unresolved.push(format!("Blocked: {cause:?}"));
                    }
                }
            }
        }
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
