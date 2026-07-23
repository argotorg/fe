use rustc_hash::FxHashMap;

use parser::{
    SyntaxNode,
    ast::{self, AttrListOwner as _, prelude::*},
};

use crate::analysis::ty::abi_ty::{
    AbiTypeError, parse_function_signature, semantic_ty_to_abi_desc, suggested_fe_type_for_sol_type,
};
use crate::analysis::ty::adt_def::AdtRef;
use crate::analysis::ty::corelib::{resolve_core_trait, resolve_lib_type_path};
use crate::analysis::ty::diagnostics::FuncBodyDiag;
use crate::analysis::ty::trait_def::TraitInstId;
use crate::analysis::ty::trait_resolution::{
    GoalSatisfiability, TraitSolveCx, is_goal_satisfiable,
};
use crate::analysis::ty::ty_check::eval_msg_variant_selector;
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};
use crate::hir_def::{ItemKind, Mod, Struct, TopLevelMod};
use crate::lower::parse_file_impl;
use crate::semantic::get_variant_selector_info;
use crate::span::{DesugaredOrigin, HirOrigin, MsgDesugaredFocus};
use crate::{MsgDiagnostic, MsgDiagnosticKind};

pub struct MsgAnalysisPass;

impl ModuleAnalysisPass for MsgAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = vec![];
        let mut ty_diags: Vec<FuncBodyDiag<'db>> = vec![];

        for &msg_mod in top_mod
            .all_mods(db)
            .iter()
            .filter(|&&m| is_msg_desugared_mod(db, m))
        {
            diags.extend(check_msg_mod(db, top_mod, msg_mod, &mut ty_diags));
        }

        diags.extend(ty_diags.iter().map(|d| d.to_voucher()));
        diags
    }
}

fn is_msg_desugared_mod<'db>(db: &'db dyn HirAnalysisDb, mod_: Mod<'db>) -> bool {
    matches!(
        mod_.origin(db).clone(),
        HirOrigin::Desugared(DesugaredOrigin::Msg(_))
    )
}

fn check_msg_mod<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    msg_mod: Mod<'db>,
    ty_diags: &mut Vec<FuncBodyDiag<'db>>,
) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
    let file = top_mod.file(db);

    let mut seen: FxHashMap<u32, (parser::TextRange, String)> = FxHashMap::default();
    let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = vec![];

    for struct_ in msg_variant_structs(db, msg_mod) {
        let Some(name) = struct_.name(db).to_opt() else {
            continue;
        };
        let variant_name = name.data(db).to_string();

        let variant_ty = TyId::adt(db, AdtRef::from(struct_).as_adt(db));

        check_variant_field_abi_requirements(db, top_mod, struct_, &variant_name, &mut diags);
        check_variant_signature_types(db, top_mod, struct_, variant_ty, &variant_name, &mut diags);

        let Some(selector) = eval_msg_variant_selector(db, variant_ty, struct_.scope(), ty_diags)
        else {
            continue;
        };

        let range = msg_variant_focus_range(db, top_mod, struct_, MsgDesugaredFocus::Selector);

        if let Some((first_range, first_name)) = seen.get(&selector) {
            diags.push(Box::new(MsgDiagnostic {
                kind: MsgDiagnosticKind::Duplicate {
                    first_variant_name: first_name.clone(),
                    selector,
                },
                file,
                primary_range: range,
                secondary_range: Some(*first_range),
                variant_name: variant_name.clone(),
            }) as _);
        } else {
            seen.insert(selector, (range, variant_name));
        }
    }

    diags
}

fn check_variant_field_abi_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    struct_: Struct<'db>,
    variant_name: &str,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    let (Some(sol_ty), Some(abi_size_trait), Some(encode_trait), Some(decode_trait)) = (
        resolve_lib_type_path(db, struct_.scope(), "std::abi::Sol"),
        resolve_core_trait(db, struct_.scope(), &["abi", "AbiSize"]),
        resolve_core_trait(db, struct_.scope(), &["abi", "Encode"]),
        resolve_core_trait(db, struct_.scope(), &["abi", "Decode"]),
    ) else {
        return;
    };

    let solve_cx = TraitSolveCx::new(db, struct_.scope());
    for (idx, field_ty) in struct_
        .field_tys(db)
        .into_iter()
        .map(|ty| ty.instantiate_identity())
        .enumerate()
    {
        if field_ty.has_invalid(db) {
            continue;
        }

        let kind = match semantic_ty_to_abi_desc(db, field_ty) {
            Err(AbiTypeError::Recursive(_)) => continue,
            Err(AbiTypeError::Unsupported(reason)) => MsgDiagnosticKind::UnsupportedAbiField {
                ty: field_ty.pretty_print(db).to_string(),
                reason,
            },
            Ok(_) => {
                let mut traits = Vec::new();
                for (name, trait_, args) in [
                    ("AbiSize", abi_size_trait, vec![field_ty]),
                    ("Encode<Sol>", encode_trait, vec![field_ty, sol_ty]),
                    ("Decode<Sol>", decode_trait, vec![field_ty, sol_ty]),
                ] {
                    let goal = TraitInstId::new_simple(db, trait_, args);
                    if matches!(
                        is_goal_satisfiable(db, solve_cx, goal),
                        GoalSatisfiability::UnSat(_) | GoalSatisfiability::NeedsConfirmation { .. }
                    ) {
                        traits.push(name);
                    }
                }
                if traits.is_empty() {
                    continue;
                }
                MsgDiagnosticKind::MissingAbiTraits {
                    ty: field_ty.pretty_print(db).to_string(),
                    traits,
                }
            }
        };

        let primary_range = msg_variant_field(db, top_mod, struct_, idx)
            .map(|field| {
                field
                    .ty()
                    .map_or(field.syntax().text_range(), |ty| ty.syntax().text_range())
            })
            .unwrap_or_else(|| {
                msg_variant_focus_range(db, top_mod, struct_, MsgDesugaredFocus::Selector)
            });
        diags.push(Box::new(MsgDiagnostic {
            kind,
            file: top_mod.file(db),
            primary_range,
            secondary_range: None,
            variant_name: variant_name.to_string(),
        }));
    }
}

/// Checks the argument types declared in a variant's `sol("...")` selector
/// signature against the semantic ABI types of the variant's fields. Variants
/// whose selector is not a recoverable signature string (e.g. a plain integer
/// literal) are skipped; malformed signatures are reported at ABI emission.
fn check_variant_signature_types<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    struct_: Struct<'db>,
    variant_ty: TyId<'db>,
    variant_name: &str,
    diags: &mut Vec<Box<dyn DiagnosticVoucher + 'db>>,
) {
    let file = top_mod.file(db);

    let Some(signature) = get_variant_selector_info(db, variant_ty, struct_.scope()).signature
    else {
        return;
    };
    let Ok(parsed) = parse_function_signature(&signature) else {
        return;
    };

    let field_tys: Vec<_> = struct_
        .field_tys(db)
        .into_iter()
        .map(|ty| ty.instantiate_identity())
        .collect();

    if parsed.arg_types.len() != field_tys.len() {
        let range = msg_variant_focus_range(db, top_mod, struct_, MsgDesugaredFocus::Selector);
        diags.push(Box::new(MsgDiagnostic {
            kind: MsgDiagnosticKind::ArityMismatch {
                signature_arity: parsed.arg_types.len(),
                field_count: field_tys.len(),
            },
            file,
            primary_range: range,
            secondary_range: None,
            variant_name: variant_name.to_string(),
        }) as _);
        return;
    }

    let hir_fields = struct_.hir_fields(db).data(db);
    for (idx, (selector_ty, field_ty)) in parsed.arg_types.iter().zip(field_tys).enumerate() {
        let Ok(desc) = semantic_ty_to_abi_desc(db, field_ty) else {
            continue;
        };
        if &desc.canonical_type == selector_ty {
            continue;
        }

        let field_name = hir_fields
            .get(idx)
            .and_then(|field| field.name.to_opt())
            .map(|name| name.data(db).to_string())
            .unwrap_or_default();
        let range = msg_variant_field(db, top_mod, struct_, idx)
            .map(|field| field.syntax().text_range())
            .unwrap_or_else(|| {
                msg_variant_focus_range(db, top_mod, struct_, MsgDesugaredFocus::Selector)
            });
        diags.push(Box::new(MsgDiagnostic {
            kind: MsgDiagnosticKind::AbiTypeMismatch {
                selector_ty: selector_ty.clone(),
                field_name,
                field_abi_ty: desc.canonical_type,
                suggestion: suggested_fe_type_for_sol_type(selector_ty),
            },
            file,
            primary_range: range,
            secondary_range: None,
            variant_name: variant_name.to_string(),
        }) as _);
    }
}

fn msg_variant_field<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    struct_: Struct<'db>,
    field_idx: usize,
) -> Option<ast::RecordFieldDef> {
    let (msg_ptr, variant_idx) = msg_origin_for_variant_struct(db, struct_)?;
    let root = SyntaxNode::new_root(parse_file_impl(db, top_mod));
    let msg_node = msg_ptr.to_node(&root);
    let variant = msg_node.variants()?.into_iter().nth(variant_idx)?;
    variant.params()?.into_iter().nth(field_idx)
}

fn msg_variant_structs<'db>(
    db: &'db dyn HirAnalysisDb,
    msg_mod: Mod<'db>,
) -> impl Iterator<Item = Struct<'db>> + 'db {
    msg_mod
        .children_non_nested(db)
        .filter_map(|item| match item {
            ItemKind::Struct(s) => Some(s),
            _ => None,
        })
}

fn msg_variant_focus_range<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    struct_: Struct<'db>,
    focus: MsgDesugaredFocus,
) -> parser::TextRange {
    let Some((msg_ptr, variant_idx)) = msg_origin_for_variant_struct(db, struct_) else {
        return parser::TextRange::new(0.into(), 0.into());
    };

    let root = SyntaxNode::new_root(parse_file_impl(db, top_mod));
    let msg_node = msg_ptr.to_node(&root);

    if !matches!(focus, MsgDesugaredFocus::Selector) {
        return msg_node.syntax().text_range();
    }

    let Some(variant) = msg_node
        .variants()
        .and_then(|v| v.into_iter().nth(variant_idx))
    else {
        return msg_node.syntax().text_range();
    };

    if let Some(attr_list) = variant.attr_list() {
        for attr in attr_list {
            if let ast::AttrKind::Normal(normal) = attr.kind()
                && let Some(path) = normal.path()
                && path.text() == "selector"
            {
                return attr.syntax().text_range();
            }
        }
    }

    variant
        .name()
        .map_or_else(|| variant.syntax().text_range(), |name| name.text_range())
}

fn msg_origin_for_variant_struct<'db>(
    db: &'db dyn HirAnalysisDb,
    struct_: Struct<'db>,
) -> Option<(parser::ast::AstPtr<ast::Msg>, usize)> {
    let HirOrigin::Desugared(DesugaredOrigin::Msg(msg)) = struct_.origin(db).clone() else {
        return None;
    };
    Some((msg.msg.clone(), msg.variant_idx?))
}
