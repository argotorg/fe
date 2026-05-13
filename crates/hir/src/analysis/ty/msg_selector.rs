use rustc_hash::FxHashMap;

use parser::{
    SyntaxNode,
    ast::{self, AttrListOwner as _, prelude::*},
};

use crate::analysis::{
    HirAnalysisDb, analysis_pass::ModuleAnalysisPass, diagnostics::DiagnosticVoucher,
};
use crate::analysis::{
    name_resolution::{PathRes, resolve_path},
    semantic::{SemConstScalar, SemConstValue, eval_body_owner_const},
    ty::{
        corelib::resolve_core_trait,
        diagnostics::FuncBodyDiag,
        trait_def::{TraitInstId, assoc_const_body_for_trait_inst},
        trait_resolution::{PredicateListId, TraitSolveCx},
        ty_check::{BodyOwner, eval_msg_variant_selector},
        ty_def::TyId,
    },
};
use crate::hir_def::{IdentId, IntegerId, ItemKind, Mod, PathId, Struct, TopLevelMod};
use crate::lower::parse_file_impl;
use crate::span::{DesugaredOrigin, HirOrigin, MsgDesugaredFocus};
use crate::{SelectorError, SelectorErrorKind};
use common::{indexmap::IndexMap, ingot::IngotKind};
use num_traits::ToPrimitive;

pub struct MsgSelectorAnalysisPass;

impl ModuleAnalysisPass for MsgSelectorAnalysisPass {
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

    let mut seen: FxHashMap<IntegerId<'db>, (parser::TextRange, String, String)> =
        FxHashMap::default();
    let mut diags: Vec<Box<dyn DiagnosticVoucher + 'db>> = vec![];
    let abi_ty = resolve_msg_mod_abi_ty(db, msg_mod);
    let selector_size = abi_ty.and_then(|ty| eval_abi_selector_size(db, msg_mod.scope(), ty));

    for struct_ in msg_variant_structs(db, msg_mod) {
        let Some(name) = struct_.name(db).to_opt() else {
            continue;
        };

        let variant_ty = crate::analysis::ty::ty_def::TyId::adt(
            db,
            crate::analysis::ty::adt_def::AdtRef::from(struct_).as_adt(db),
        );
        let Some(abi_ty) = abi_ty else {
            continue;
        };
        let Some(selector) =
            eval_msg_variant_selector(db, variant_ty, abi_ty, struct_.scope(), ty_diags)
        else {
            continue;
        };
        let selector_text = selector_text(db, selector, selector_size);

        let range = msg_variant_focus_range(db, top_mod, struct_, MsgDesugaredFocus::Selector);

        if let Some((first_range, first_name, first_selector_text)) = seen.get(&selector) {
            diags.push(Box::new(SelectorError {
                kind: SelectorErrorKind::Duplicate {
                    first_variant_name: first_name.clone(),
                    selector: first_selector_text.clone(),
                },
                file,
                primary_range: range,
                secondary_range: Some(*first_range),
                variant_name: name.data(db).to_string(),
            }) as _);
        } else {
            seen.insert(selector, (range, name.data(db).to_string(), selector_text));
        }
    }

    diags
}

fn resolve_msg_mod_abi_ty<'db>(db: &'db dyn HirAnalysisDb, msg_mod: Mod<'db>) -> Option<TyId<'db>> {
    let assumptions = PredicateListId::empty_list(db);
    if let Some(path) = ItemKind::Mod(msg_mod)
        .attrs(db)
        .and_then(|attrs| attrs.abi_path(db))
        && let Some(ty) = resolve_ty_path(db, path, msg_mod.scope(), assumptions)
    {
        return Some(ty);
    }
    resolve_sol_abi_ty(db, msg_mod.scope(), assumptions)
}

fn resolve_ty_path<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    match resolve_path(db, path, scope, assumptions, false).ok()? {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => Some(ty),
        _ => None,
    }
}

fn resolve_sol_abi_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    let ingot = scope.ingot(db);
    let std_root = if ingot.kind(db) == IngotKind::Std {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, "std".to_string())
    };

    let sol_path = PathId::from_ident(db, std_root)
        .push_ident(db, IdentId::new(db, "abi".to_string()))
        .push_ident(db, IdentId::new(db, "Sol".to_string()));

    resolve_ty_path(db, sol_path, scope, assumptions)
}

fn eval_abi_selector_size<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: crate::hir_def::scope_graph::ScopeId<'db>,
    abi_ty: TyId<'db>,
) -> Option<usize> {
    let abi_trait = resolve_core_trait(db, scope, &["abi", "Abi"])?;
    let inst = TraitInstId::new(db, abi_trait, vec![abi_ty], IndexMap::new());
    let solve_cx = TraitSolveCx::new(db, scope);
    let selector_size_name = IdentId::new(db, "SELECTOR_SIZE".to_string());
    let body = assoc_const_body_for_trait_inst(db, solve_cx, inst, selector_size_name)?;
    match eval_body_owner_const(
        db,
        BodyOwner::AnonConstBody {
            body,
            expected: TyId::u256(db),
        },
        Vec::new(),
    ) {
        Ok(value) => match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => value.to_usize(),
            _ => None,
        },
        _ => None,
    }
}

fn selector_text<'db>(
    db: &'db dyn HirAnalysisDb,
    selector: IntegerId<'db>,
    selector_size: Option<usize>,
) -> String {
    let mut bytes = selector.data(db).to_bytes_be();
    if let Some(size) = selector_size
        && bytes.len() <= size
    {
        let mut padded = vec![0; size.saturating_sub(bytes.len())];
        padded.extend(bytes);
        bytes = padded;
    }

    let mut out = String::from("0x");
    for byte in bytes {
        out.push_str(&format!("{byte:02x}"));
    }
    out
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
