use common::InputDb;
use driver::DriverDataBase;
use hir::lower::map_file_to_mod;
use trace_facts::{TraceBundle, TraceMetadata, TraceSnapshot};
use trace_query::TraceIntrospectionService;
use url::Url;

pub(crate) fn service_for_file(
    db: &DriverDataBase,
    uri: &Url,
    config: introspection_config::FeToolingConfig,
) -> Result<Option<TraceIntrospectionService>, String> {
    let Some(file) = db.workspace().get(db, uri) else {
        return Ok(None);
    };
    let top_mod = map_file_to_mod(db, file);
    let package = mir::build_runtime_package(db, top_mod)
        .map_err(|err| format!("runtime package lowering for trace: {err}"))?;
    let mut facts = mir::trace::emit_mir_facts(db, package);
    let bytecode = codegen::emit_module_sonatina_bytecode(db, top_mod, codegen::OptLevel::O1, None)
        .map_err(|err| format!("bytecode emission for trace: {err}"))?;
    let module_key = top_mod.name(db).data(db).to_string();
    for (contract_name, artifact) in bytecode {
        let owner_key =
            codegen::trace::bytecode_runtime_owner_key(uri.as_str(), &module_key, &contract_name);
        facts.extend(codegen::trace::emit_bytecode_instruction_facts(
            &owner_key,
            "function:runtime",
            &artifact.runtime,
        ));
    }

    let metadata = TraceMetadata::compiler_emitted(
        option_env!("FE_GIT_COMMIT").unwrap_or("unknown"),
        "evm/sonatina",
        vec!["fe-language-server".to_string(), "trace".to_string()],
        uri.to_string(),
        vec!["source=lsp-live".to_string()],
    );
    let snapshot = TraceSnapshot::new(TraceBundle::new(metadata, facts))
        .map_err(|err| format!("live trace validation failed: {err}"))?;
    Ok(Some(TraceIntrospectionService::with_config(
        snapshot, config,
    )))
}
