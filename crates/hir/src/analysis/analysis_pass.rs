use crate::analysis::{
    HirAnalysisDb,
    diagnostics::DiagnosticVoucher,
    ty::{
        abi_struct_field_checks,
        abi_ty::{is_composite_event_ty, is_dynamic_event_ty},
        adt_def::AdtRef,
        corelib::{resolve_core_trait, resolve_lib_trait_path, resolve_lib_type_path},
        non_copy_array_abi_fields,
        trait_def::TraitInstId,
        trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
        ty_lower::lower_hir_ty,
    },
};
use crate::{
    AbiFieldContext, AbiFieldDiagnostic, AbiFieldDiagnosticKind, AbiStructDiagnostic,
    AbiStructDiagnosticKind, AttrMisuseError, ErrorDiagnostic, EventError, EventErrorKind,
    FieldModifierError, MsgDiagnostic, ParserError,
    hir_def::{ModuleTree, Partial, TopLevelMod, TypeKind},
    lower::{parse_file_impl, scope_graph_impl, top_mod_ast},
    semantic::constraints_for,
    span::{DesugaredOrigin, HirOrigin},
};
use common::{
    diagnostics::{DiagnosticPass, GlobalErrorCode},
    file::File,
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

/// Analysis pass that collects message diagnostics accumulated during lowering.
pub struct MsgLowerPass {}

impl ModuleAnalysisPass for MsgLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        scope_graph_impl::accumulated::<MsgDiagnostic>(db, top_mod)
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
            semantic_field_type_errors(db, top_mod, AbiFieldContext::Event)
                .map(|d| Box::new(d) as _),
        );
        diags.extend(semantic_indexed_dynamic_field_errors(db, top_mod).map(|d| Box::new(d) as _));
        diags.extend(
            non_copy_array_field_errors(
                db,
                top_mod,
                |origin| matches!(origin, DesugaredOrigin::Event(_)),
                GlobalErrorCode::new(DiagnosticPass::EventLower, 10),
            )
            .map(|d| Box::new(d) as _),
        );
        diags
    }
}

fn semantic_indexed_dynamic_field_errors<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> impl Iterator<Item = EventError> {
    let mut diags = Vec::new();
    let mut seen_structs = Vec::new();

    for &impl_trait in top_mod.all_impl_traits(db) {
        let HirOrigin::Desugared(DesugaredOrigin::Event(event_origin)) = impl_trait.origin(db)
        else {
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
        let Some(AdtRef::Struct(event_struct)) = self_ty.adt_ref(db) else {
            continue;
        };
        if seen_structs.contains(&event_struct) {
            continue;
        }
        seen_structs.push(event_struct);

        let root = top_mod_ast(db, top_mod).syntax().clone();
        let ast_struct = event_origin
            .event_struct
            .syntax_node_ptr()
            .try_to_node(&root)
            .and_then(ast::Struct::cast);
        let assumptions = constraints_for(db, event_struct.into());

        for (field_idx, field) in event_struct.hir_fields(db).data(db).iter().enumerate() {
            if !field.is_event_indexed {
                continue;
            }
            let Some(field_ty) = field.type_ref().to_opt() else {
                continue;
            };
            let resolved_ty = lower_hir_ty(db, field_ty, event_struct.scope(), assumptions);
            let ty = resolved_ty.pretty_print(db).to_string();
            let kind = if is_dynamic_event_ty(db, resolved_ty) {
                EventErrorKind::IndexedDynamicField { ty }
            } else if is_composite_event_ty(db, resolved_ty) && !resolved_ty.is_tuple(db) {
                // Tuples are already rejected as event fields.
                EventErrorKind::IndexedCompositeField { ty }
            } else {
                continue;
            };

            let ast_field = ast_struct
                .as_ref()
                .and_then(|ast_struct| ast_struct.fields())
                .and_then(|fields| fields.into_iter().nth(field_idx));
            let primary_range = ast_field.as_ref().map_or_else(
                || parser::TextRange::empty(0.into()),
                |field| {
                    field
                        .ty()
                        .map_or(field.syntax().text_range(), |ty| ty.syntax().text_range())
                },
            );
            diags.push(EventError {
                kind,
                file: top_mod.file(db),
                primary_range,
                struct_name: event_struct
                    .name(db)
                    .to_opt()
                    .map(|name| name.data(db).to_string()),
                field_name: field.name.to_opt().map(|name| name.data(db).to_string()),
            });
        }
    }

    diags.into_iter()
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

/// Tuple fields, and fields without a Solidity type name for the signature,
/// once type aliases are resolved. Fields that are not paths are reported
/// during lowering.
fn semantic_field_type_errors<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    context: AbiFieldContext,
) -> impl Iterator<Item = AbiFieldDiagnostic> {
    let mut diags = Vec::new();
    let mut seen_structs = Vec::new();
    let root = top_mod_ast(db, top_mod).syntax().clone();

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

        let scope = abi_struct.scope();
        let assumptions = constraints_for(db, abi_struct.into());
        let solve_cx = TraitSolveCx::new(db, scope).with_assumptions(assumptions);
        let traits = (
            resolve_lib_type_path(db, scope, "std::abi::Sol"),
            resolve_core_trait(db, scope, &["abi", "AbiSize"]),
            resolve_core_trait(db, scope, &["abi", "Encode"]),
            resolve_lib_trait_path(db, scope, "std::abi::SolCompat"),
        );
        let implements = |trait_, args| {
            !matches!(
                is_goal_satisfiable(db, solve_cx, TraitInstId::new_simple(db, trait_, args)),
                GoalSatisfiability::UnSat(_) | GoalSatisfiability::NeedsConfirmation { .. }
            )
        };
        for (field_idx, field) in abi_struct.hir_fields(db).data(db).iter().enumerate() {
            let Some(hir_ty) = field.type_ref().to_opt() else {
                continue;
            };
            if !matches!(hir_ty.data(db), TypeKind::Path(Partial::Present(_))) {
                continue;
            }

            let resolved_ty = lower_hir_ty(db, hir_ty, scope, assumptions);
            // Only a type with an ABI encoding lacks just its Solidity name;
            // the generated code reports types that cannot be encoded.
            let kind = if resolved_ty.is_tuple(db) {
                AbiFieldDiagnosticKind::Unsupported
            } else if !resolved_ty.has_invalid(db)
                && let (Some(sol_ty), Some(abi_size), Some(encode), Some(sol_compat)) = traits
                && implements(abi_size, vec![resolved_ty])
                && implements(encode, vec![resolved_ty, sol_ty])
                && !implements(sol_compat, vec![resolved_ty])
            {
                AbiFieldDiagnosticKind::MissingSolCompat
            } else {
                continue;
            };

            diags.push(AbiFieldDiagnostic {
                kind,
                context,
                ty: resolved_ty.pretty_print(db).to_string(),
                file: top_mod.file(db),
                primary_range: field_ty_range(&root, &struct_ptr, field_idx),
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
            semantic_field_type_errors(db, top_mod, AbiFieldContext::Error)
                .map(|d| Box::new(d) as _),
        );
        diags.extend(
            non_copy_array_field_errors(
                db,
                top_mod,
                |origin| matches!(origin, DesugaredOrigin::Error(_)),
                GlobalErrorCode::new(DiagnosticPass::ErrorLower, 6),
            )
            .map(|d| Box::new(d) as _),
        );
        diags
    }
}

/// Analysis pass that collects diagnostics of `#[abi]` struct desugaring.
pub struct AbiStructLowerPass {}

impl ModuleAnalysisPass for AbiStructLowerPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        let mut diags = scope_graph_impl::accumulated::<AbiStructDiagnostic>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>();
        diags.extend(
            non_copy_array_field_errors(
                db,
                top_mod,
                |origin| matches!(origin, DesugaredOrigin::AbiStruct(_)),
                GlobalErrorCode::new(DiagnosticPass::AbiStructLower, 3),
            )
            .map(|d| Box::new(d) as _),
        );
        let root = top_mod_ast(db, top_mod).syntax().clone();
        diags.extend(
            abi_struct_field_checks(db, top_mod)
                .unsupported
                .iter()
                .map(|field| {
                    Box::new(AbiStructDiagnostic {
                        kind: AbiStructDiagnosticKind::UnsupportedFieldType {
                            ty: field.field_ty.pretty_print(db).to_string(),
                            missing: field.missing.clone(),
                        },
                        file: top_mod.file(db),
                        primary_range: field_ty_range(&root, &field.ast_struct, field.field_idx),
                    }) as _
                }),
        );
        diags
    }
}

/// The range of the type of a struct's field, or an empty range if the
/// syntax is unavailable.
fn field_ty_range(
    root: &parser::SyntaxNode,
    ast_struct: &parser::ast::AstPtr<ast::Struct>,
    field_idx: usize,
) -> parser::TextRange {
    ast_struct
        .syntax_node_ptr()
        .try_to_node(root)
        .and_then(ast::Struct::cast)
        .and_then(|ast_struct| ast_struct.fields())
        .and_then(|fields| fields.into_iter().nth(field_idx))
        .and_then(|field| field.ty())
        .map_or_else(
            || parser::TextRange::empty(0.into()),
            |ty| ty.syntax().text_range(),
        )
}

/// A field of an `#[abi]`, `#[event]` or `#[error]` struct that holds a fixed
/// array of non-`Copy` elements, which the fixed-array ABI codecs cannot
/// encode. Each struct kind's lowering pass reports it under its own code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AbiArrayElemNotCopy {
    pub error_code: GlobalErrorCode,
    pub ty: String,
    pub elem_ty: String,
    /// Why `elem_ty` has no `Copy` answer, when deciding it reached a
    /// normalization limit.
    pub limit_reason: Option<String>,
    pub file: File,
    pub primary_range: parser::TextRange,
}

fn non_copy_array_field_errors<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    owns: fn(&DesugaredOrigin) -> bool,
    error_code: GlobalErrorCode,
) -> impl Iterator<Item = AbiArrayElemNotCopy> {
    let root = top_mod_ast(db, top_mod).syntax().clone();
    non_copy_array_abi_fields(db, top_mod)
        .iter()
        .filter(move |field| owns(&field.origin))
        .map(move |field| AbiArrayElemNotCopy {
            error_code: error_code.clone(),
            ty: field.field_ty.pretty_print(db).to_string(),
            elem_ty: field.elem_ty.pretty_print(db).to_string(),
            limit_reason: field.limit.map(|limit| limit.reason()),
            file: top_mod.file(db),
            primary_range: field_ty_range(&root, &field.ast_struct, field.field_idx),
        })
}

/// Analysis pass that collects generic attribute misuse diagnostics.
pub struct AttrMisusePass {}

impl ModuleAnalysisPass for AttrMisusePass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher>> {
        let mut diags = scope_graph_impl::accumulated::<AttrMisuseError>(db, top_mod)
            .into_iter()
            .map(|d| Box::new(d.clone()) as _)
            .collect::<Vec<_>>();
        diags.extend(
            scope_graph_impl::accumulated::<FieldModifierError>(db, top_mod)
                .into_iter()
                .map(|d| Box::new(d.clone()) as _),
        );
        diags
    }
}
