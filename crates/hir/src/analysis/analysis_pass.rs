use crate::analysis::{
    HirAnalysisDb,
    diagnostics::DiagnosticVoucher,
    ty::{adt_def::AdtRef, ty_lower::lower_hir_ty},
};
use crate::{
    AbiFieldContext, AbiFieldDiagnostic, AttrMisuseError, ErrorDiagnostic, EventError, ParserError,
    SelectorError,
    hir_def::{ModuleTree, TopLevelMod},
    lower::{parse_file_impl, scope_graph_impl, top_mod_ast},
    semantic::constraints_for,
    span::{DesugaredOrigin, HirOrigin},
};
use parser::ast::{self, prelude::*};

/// All analysis passes that run analysis on the HIR top level module
/// granularity should implement this trait.
pub trait ModuleAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>>;
}

#[derive(Default)]
pub struct AnalysisPassManager {
    module_passes: Vec<(&'static str, Box<dyn ModuleAnalysisPass>)>,
}

impl AnalysisPassManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module_pass(&mut self, name: &'static str, pass: Box<dyn ModuleAnalysisPass>) {
        self.module_passes.push((name, pass));
    }

    pub fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags = vec![];
        for (name, pass) in self.module_passes.iter_mut() {
            let t0 = std::time::Instant::now();
            diags.extend(pass.run_on_module(db, top_mod));
            let elapsed = t0.elapsed();
            if elapsed.as_micros() > 100 {
                tracing::debug!("[fe:timing]   pass {name}: {elapsed:?}");
            }
        }
        diags
    }

    pub fn run_on_module_tree<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        tree: &'db ModuleTree<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags = vec![];
        for module in tree.all_modules() {
            for (name, pass) in self.module_passes.iter_mut() {
                let t0 = std::time::Instant::now();
                diags.extend(pass.run_on_module(db, module));
                let elapsed = t0.elapsed();
                if elapsed.as_micros() > 100 {
                    tracing::debug!("[fe:timing]   pass {name}: {elapsed:?}");
                }
            }
        }
        diags
    }
}

#[derive(Clone, Copy)]
pub struct ParsingPass {}

impl ModuleAnalysisPass for ParsingPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        parse_file_impl::accumulated::<ParserError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects selector errors from msg block lowering.
pub struct MsgLowerPass {}

impl ModuleAnalysisPass for MsgLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<SelectorError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}

/// Analysis pass that collects event lowering errors from `#[event]` struct desugaring.
pub struct EventLowerPass {}

impl ModuleAnalysisPass for EventLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        let mut diags = scope_graph_impl::accumulated::<EventError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>();
        diags.extend(
            accumulated_abi_field_diagnostics(db, top_mod, AbiFieldContext::Event)
                .map(|d| Box::new(d) as _),
        );
        diags.extend(
            semantic_tuple_field_type_errors(db, top_mod, AbiFieldContext::Event)
                .map(|d| Box::new(d) as _),
        );
        diags
    }
}

// Syntactic non-path field types are rejected during event/error lowering so we
// can skip generated selector/TOPIC0 constants. This catches unsupported shapes
// that only become visible after type aliases are resolved.
fn accumulated_abi_field_diagnostics<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    context: AbiFieldContext,
) -> impl Iterator<Item = AbiFieldDiagnostic> {
    scope_graph_impl::accumulated::<AbiFieldDiagnostic>(db, top_mod)
        .into_iter()
        .filter(move |diag| diag.context == context)
        .cloned()
}

fn semantic_tuple_field_type_errors<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    context: AbiFieldContext,
) -> impl Iterator<Item = AbiFieldDiagnostic> {
    let mut diags = Vec::new();
    let mut seen_structs = Vec::new();

    for &impl_trait in top_mod.all_impl_traits(db) {
        let HirOrigin::Desugared(origin) = impl_trait.origin(db) else {
            continue;
        };
        let Some(struct_ptr) = abi_field_struct(origin, context) else {
            continue;
        };

        let Some(self_ty) = impl_trait.type_ref(db).to_opt() else {
            continue;
        };
        let self_ty = lower_hir_ty(
            db,
            self_ty,
            impl_trait.scope(),
            constraints_for(db, impl_trait.into()),
        );
        let Some(AdtRef::Struct(abi_struct)) = self_ty.adt_ref(db) else {
            continue;
        };
        if seen_structs.contains(&abi_struct) {
            continue;
        }
        seen_structs.push(abi_struct);

        let root = top_mod_ast(db, top_mod).syntax().clone();
        let ast_struct = struct_ptr
            .syntax_node_ptr()
            .try_to_node(&root)
            .and_then(ast::Struct::cast);

        let assumptions = constraints_for(db, abi_struct.into());
        let fields = abi_struct.hir_fields(db);
        for (field_idx, field) in fields.data(db).iter().enumerate() {
            let Some(field_ty) = field.type_ref().to_opt() else {
                continue;
            };

            let resolved_ty = lower_hir_ty(db, field_ty, abi_struct.scope(), assumptions);
            if !resolved_ty.is_tuple(db) {
                continue;
            }

            let primary_range = ast_struct
                .as_ref()
                .and_then(|ast_struct| ast_struct.fields())
                .and_then(|fields| fields.into_iter().nth(field_idx))
                .and_then(|field| field.ty())
                .map_or_else(
                    || parser::TextRange::empty(0.into()),
                    |ty| ty.syntax().text_range(),
                );
            diags.push(AbiFieldDiagnostic {
                context,
                ty: resolved_ty.pretty_print(db).to_string(),
                file: top_mod.file(db),
                primary_range,
                struct_name: abi_struct
                    .name(db)
                    .to_opt()
                    .map(|name| name.data(db).to_string()),
                field_name: field.name.to_opt().map(|name| name.data(db).to_string()),
            });
        }
    }

    diags.into_iter()
}

fn abi_field_struct(
    origin: &DesugaredOrigin,
    context: AbiFieldContext,
) -> Option<parser::ast::AstPtr<ast::Struct>> {
    match (origin, context) {
        (DesugaredOrigin::Event(event_origin), AbiFieldContext::Event) => {
            Some(event_origin.event_struct.clone())
        }
        (DesugaredOrigin::Error(error_origin), AbiFieldContext::Error) => {
            Some(error_origin.error_struct.clone())
        }
        _ => None,
    }
}

/// Analysis pass that collects error lowering diagnostics from `#[error]` struct desugaring.
pub struct ErrorLowerPass {}

impl ModuleAnalysisPass for ErrorLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        let mut diags = scope_graph_impl::accumulated::<ErrorDiagnostic>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>();
        diags.extend(
            accumulated_abi_field_diagnostics(db, top_mod, AbiFieldContext::Error)
                .map(|d| Box::new(d) as _),
        );
        diags.extend(
            semantic_tuple_field_type_errors(db, top_mod, AbiFieldContext::Error)
                .map(|d| Box::new(d) as _),
        );
        diags
    }
}

/// Analysis pass that collects generic attribute misuse diagnostics.
pub struct AttrMisusePass {}

impl ModuleAnalysisPass for AttrMisusePass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<AttrMisuseError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>()
    }
}
