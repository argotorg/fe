use std::{collections::BTreeSet, ops::Range};

use common::{
    InputDb,
    diagnostics::Severity,
    file::File,
    stdlib::{HasBuiltinCore, HasBuiltinStd},
};
use fe_driver::{
    DriverDataBase,
    generation::{GeneratedFunction, GenerationProvenance},
};
use hir::{
    analysis::{
        semantic::eval_body_owner_const_with_args,
        ty::{
            normalize::normalize_ty, trait_resolution::PredicateListId, ty_check::BodyOwner,
            ty_def::TyId,
        },
    },
    hir_def::{Func, FuncParamMode, ItemKind, WhereClauseOwner},
    span::LazySpan,
};
use url::Url;

const MAX_IMPORTS: usize = 16;
const MAX_SOURCE_BYTES: usize = 64 * 1024;

pub fn database() -> DriverDataBase {
    let mut db = DriverDataBase::default();
    db.initialize_builtin_core();
    db.initialize_builtin_std();
    db
}

pub fn named<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Func<'db> {
    find_named(db, file, name).unwrap_or_else(|| panic!("missing top-level function `{name}`"))
}

fn find_named<'db>(db: &'db DriverDataBase, file: File, name: &str) -> Option<Func<'db>> {
    db.top_mod(file)
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
}

pub fn evaluate(db: &DriverDataBase, file: File, name: &str) -> String {
    eval_body_owner_const_with_args(
        db,
        BodyOwner::Func(named(db, file, name)),
        Vec::new(),
        Vec::new(),
    )
    .unwrap_or_else(|error| panic!("failed to evaluate `{name}`: {error:?}"))
    .pretty_print(db)
}

pub fn check(db: &DriverDataBase, file: File) -> Result<(), String> {
    let ingot = db.top_mod(file).ingot(db);
    let hir = db.run_on_ingot(ingot);
    if hir.has_errors(db) {
        return Err(hir.format_diags(db));
    }
    let semantic = db.mir_diagnostics_for_ingot(ingot);
    if semantic
        .iter()
        .any(|diagnostic| diagnostic.severity == Severity::Error)
    {
        return Err(db.format_complete_diagnostics(&semantic));
    }
    Ok(())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScalarType {
    Bool,
    U256,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParamInterface {
    ty: ScalarType,
    label: Option<String>,
    mode: FuncParamMode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ExportInterface {
    is_const: bool,
    params: Vec<ParamInterface>,
    result: ScalarType,
    selected_span: Range<usize>,
    selected_source: String,
}

fn scalar_type(db: &DriverDataBase, ty: TyId<'_>) -> Result<ScalarType, String> {
    if ty == TyId::bool(db) {
        Ok(ScalarType::Bool)
    } else if ty == TyId::u256(db) {
        Ok(ScalarType::U256)
    } else {
        Err(format!(
            "frozen exports support only bool and u256, found `{}`",
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
        is_const: func.is_const(db),
        params: interface_params,
        result: scalar_type(db, result)?,
        selected_span,
        selected_source,
    })
}

pub struct FrozenExport<'a> {
    artifact: &'a GeneratedFunction,
    name: String,
    interface: ExportInterface,
}

impl<'a> FrozenExport<'a> {
    pub fn new(artifact: &'a GeneratedFunction, name: &str) -> Result<Self, String> {
        let db = artifact.database();
        let file = artifact.file();
        check(db, file)?;
        let func = find_named(db, file, name)
            .ok_or_else(|| format!("missing frozen export function `{name}`"))?;
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
            return Err(
                "frozen export must be a checked public safe top-level const function without generics, requirements, or effects"
                    .to_owned(),
            );
        }
        let interface = export_interface(db, file, func)?;
        Ok(Self {
            artifact,
            name: name.to_owned(),
            interface,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ImportReceipt {
    pub alias: String,
    pub export_name: String,
    pub original_source: String,
    pub original_provenance: GenerationProvenance,
    pub materialized_source_url: Url,
}

pub struct ImportStage {
    pub db: DriverDataBase,
    pub file: File,
    pub receipts: Vec<ImportReceipt>,
}

fn valid_alias(alias: &str) -> bool {
    let mut chars = alias.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    first.is_ascii()
        && (first == '_' || first.is_ascii_alphabetic())
        && chars.all(|ch| ch.is_ascii() && (ch == '_' || ch.is_ascii_alphanumeric()))
}

fn touch(db: &mut DriverDataBase, url: &str, source: String) -> Result<File, String> {
    let url = Url::parse(url).map_err(|error| format!("invalid frozen import URL: {error}"))?;
    Ok(db.workspace().touch(db, url, Some(source)))
}

/// Materializes whole checked packages as path dependencies for an executable
/// anchoring probe. This does not define an export whitelist or hygienic import
/// reference. Ordinary public siblings remain visible, and recipient aliases
/// may still be shadowed by source declarations.
pub fn import_stage(
    imports: &[(&str, &FrozenExport<'_>)],
    source: &str,
) -> Result<ImportStage, String> {
    if imports.len() > MAX_IMPORTS {
        return Err("frozen import count limit exceeded".to_owned());
    }
    let mut aliases = BTreeSet::new();
    let mut combined_bytes = source.len();
    for (alias, export) in imports {
        if !valid_alias(alias) || matches!(*alias, "core" | "std" | "ingot") {
            return Err(format!("invalid frozen import alias `{alias}`"));
        }
        if !aliases.insert(*alias) {
            return Err(format!("duplicate frozen import alias `{alias}`"));
        }
        combined_bytes = combined_bytes
            .checked_add(alias.len())
            .and_then(|bytes| bytes.checked_add(export.artifact.source().len()))
            .filter(|bytes| *bytes <= MAX_SOURCE_BYTES)
            .ok_or_else(|| "frozen import source byte limit exceeded".to_owned())?;
    }

    if combined_bytes > MAX_SOURCE_BYTES {
        return Err("frozen import source byte limit exceeded".to_owned());
    }
    let mut db = database();
    let mut dependency_config =
        String::from("[ingot]\nname = \"consumer\"\nversion = \"0.0.0\"\n\n[dependencies]\n");
    for (index, (alias, _)) in imports.iter().enumerate() {
        dependency_config.push_str(&format!("{alias} = {{ path = \"../artifact_{index}\" }}\n"));
    }
    touch(
        &mut db,
        "file:///frozen-imports/consumer/fe.toml",
        dependency_config,
    )?;

    let mut materialized = Vec::with_capacity(imports.len());
    for (index, (_, export)) in imports.iter().enumerate() {
        touch(
            &mut db,
            &format!("file:///frozen-imports/artifact_{index}/fe.toml"),
            format!("[ingot]\nname = \"artifact_{index}\"\nversion = \"0.0.0\"\n"),
        )?;
        let source_url = Url::parse(&format!(
            "file:///frozen-imports/artifact_{index}/src/lib.fe"
        ))
        .map_err(|error| format!("invalid materialized source URL: {error}"))?;
        let source_file = db.workspace().touch(
            &mut db,
            source_url.clone(),
            Some(export.artifact.source().to_owned()),
        );
        materialized.push((source_file, source_url));
    }
    let file = touch(
        &mut db,
        "file:///frozen-imports/consumer/src/lib.fe",
        source.to_owned(),
    )?;

    let mut receipts = Vec::with_capacity(imports.len());
    for ((alias, export), (source_file, source_url)) in imports.iter().zip(materialized) {
        check(&db, source_file)?;
        if source_file.text(&db) != export.artifact.source() {
            return Err(format!("frozen import `{alias}` changed source bytes"));
        }
        let imported = find_named(&db, source_file, &export.name)
            .ok_or_else(|| format!("frozen import `{alias}` lost its exported function"))?;
        let imported_interface = export_interface(&db, source_file, imported)?;
        if imported_interface != export.interface {
            return Err(format!(
                "frozen import `{alias}` changed its checked interface"
            ));
        }
        receipts.push(ImportReceipt {
            alias: (*alias).to_owned(),
            export_name: export.name.clone(),
            original_source: export.artifact.source().to_owned(),
            original_provenance: export.artifact.provenance().clone(),
            materialized_source_url: source_url,
        });
    }
    check(&db, file)?;
    Ok(ImportStage { db, file, receipts })
}
