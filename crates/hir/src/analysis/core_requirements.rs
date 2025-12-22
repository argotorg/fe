use std::collections::HashSet;

use common::ingot::IngotKind;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{NameDomain, PathRes, resolve_ident_to_bucket, resolve_path},
        ty::trait_resolution::PredicateListId,
    },
    hir_def::{IdentId, PathId, Trait, scope_graph::ScopeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreRequirementKind {
    Trait,
    TraitMethod,
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreRequirement {
    pub path: &'static str,
    pub kind: CoreRequirementKind,
    pub method: Option<&'static str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingCoreRequirement {
    pub path: String,
    pub kind: CoreRequirementKind,
    pub detail: Option<String>,
}

impl std::fmt::Display for MissingCoreRequirement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            CoreRequirementKind::Trait => {
                write!(f, "missing required core trait `{}`", self.path)
            }
            CoreRequirementKind::TraitMethod => {
                let method = self.detail.as_deref().unwrap_or("unknown");
                write!(
                    f,
                    "missing required core trait method `{}` on `{}`",
                    method, self.path
                )
            }
            CoreRequirementKind::Type => {
                write!(f, "missing required core type `{}`", self.path)
            }
        }
    }
}

const CORE_OP_REQUIREMENTS: &[(&str, &str)] = &[
    ("core::ops::Neg", "neg"),
    ("core::ops::Not", "not"),
    ("core::ops::BitNot", "bit_not"),
    ("core::ops::Add", "add"),
    ("core::ops::Sub", "sub"),
    ("core::ops::Mul", "mul"),
    ("core::ops::Div", "div"),
    ("core::ops::Rem", "rem"),
    ("core::ops::Pow", "pow"),
    ("core::ops::Shl", "shl"),
    ("core::ops::Shr", "shr"),
    ("core::ops::BitAnd", "bitand"),
    ("core::ops::BitOr", "bitor"),
    ("core::ops::BitXor", "bitxor"),
    ("core::ops::Eq", "eq"),
    ("core::ops::Ord", "lt"),
    ("core::ops::Ord", "le"),
    ("core::ops::Ord", "gt"),
    ("core::ops::Ord", "ge"),
    ("core::ops::Index", "index"),
    ("core::ops::AddAssign", "add_assign"),
    ("core::ops::SubAssign", "sub_assign"),
    ("core::ops::MulAssign", "mul_assign"),
    ("core::ops::DivAssign", "div_assign"),
    ("core::ops::RemAssign", "rem_assign"),
    ("core::ops::PowAssign", "pow_assign"),
    ("core::ops::ShlAssign", "shl_assign"),
    ("core::ops::ShrAssign", "shr_assign"),
    ("core::ops::BitAndAssign", "bitand_assign"),
    ("core::ops::BitOrAssign", "bitor_assign"),
    ("core::ops::BitXorAssign", "bitxor_assign"),
];

const CORE_TRAIT_REQUIREMENTS: &[&str] = &[
    "core::effect_ref::EffectRef",
    "core::effect_ref::EffectRefMut",
    "core::message::MsgVariant",
    "core::abi::Decode",
];

const STD_TYPE_REQUIREMENTS: &[&str] = &[
    "std::evm::effects::MemPtr",
    "std::evm::effects::StorPtr",
    "std::evm::effects::CalldataPtr",
];

pub fn check_core_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ingot_kind: IngotKind,
) -> Vec<MissingCoreRequirement> {
    let mut missing = Vec::new();
    let mut missing_traits = HashSet::new();

    for &path in CORE_TRAIT_REQUIREMENTS {
        if resolve_trait_in_scope(db, scope, ingot_kind, path).is_none() {
            missing.push(MissingCoreRequirement {
                path: path.to_string(),
                kind: CoreRequirementKind::Trait,
                detail: None,
            });
        }
    }

    for &(path, method) in CORE_OP_REQUIREMENTS {
        match resolve_trait_in_scope(db, scope, ingot_kind, path) {
            Some(trait_def) => {
                let method_ident = IdentId::new(db, method.to_string());
                if !trait_def.method_defs(db).contains_key(&method_ident) {
                    missing.push(MissingCoreRequirement {
                        path: path.to_string(),
                        kind: CoreRequirementKind::TraitMethod,
                        detail: Some(method.to_string()),
                    });
                }
            }
            None => {
                if missing_traits.insert(path) {
                    missing.push(MissingCoreRequirement {
                        path: path.to_string(),
                        kind: CoreRequirementKind::Trait,
                        detail: None,
                    });
                }
            }
        }
    }

    missing
}

pub fn check_std_type_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ingot_kind: IngotKind,
) -> Vec<MissingCoreRequirement> {
    let mut missing = Vec::new();

    for &path in STD_TYPE_REQUIREMENTS {
        if resolve_type_in_scope(db, scope, ingot_kind, path).is_none() {
            missing.push(MissingCoreRequirement {
                path: path.to_string(),
                kind: CoreRequirementKind::Type,
                detail: None,
            });
        }
    }

    missing
}

fn resolve_trait_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ingot_kind: IngotKind,
    path: &str,
) -> Option<Trait<'db>> {
    let segments: Vec<_> = path.split("::").collect();
    let (root, trait_name) = (segments.first()?, segments.last()?);
    let (_, mut module_path) = build_root_path(db, ingot_kind, root)?;

    for segment in &segments[1..segments.len() - 1] {
        module_path = module_path.push_str(db, segment);
    }

    let assumptions = PredicateListId::empty_list(db);
    let Ok(PathRes::Mod(mod_scope)) = resolve_path(db, module_path, scope, assumptions, false)
    else {
        return None;
    };

    let trait_ident = IdentId::new(db, trait_name.to_string());
    let bucket = resolve_ident_to_bucket(db, PathId::from_ident(db, trait_ident), mod_scope);
    let nameres = match bucket.pick(NameDomain::TYPE) {
        Ok(nameres) => nameres,
        Err(_) => return None,
    };
    let trait_def = nameres.trait_()?;
    Some(trait_def)
}

fn resolve_type_in_scope<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ingot_kind: IngotKind,
    path: &str,
) -> Option<()> {
    let mut segments = path.split("::");
    let root = segments.next()?;
    let (_, mut path_id) = build_root_path(db, ingot_kind, root)?;

    for segment in segments {
        path_id = path_id.push_str(db, segment);
    }

    let assumptions = PredicateListId::empty_list(db);
    match resolve_path(db, path_id, scope, assumptions, true).ok()? {
        PathRes::Ty(_) | PathRes::TyAlias(_, _) => Some(()),
        _ => None,
    }
}

fn build_root_path<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot_kind: IngotKind,
    root: &str,
) -> Option<(IdentId<'db>, PathId<'db>)> {
    let ingot_root = (ingot_kind == IngotKind::Core && root == "core")
        || (ingot_kind == IngotKind::Std && root == "std");
    let root_ident = if ingot_root {
        IdentId::make_ingot(db)
    } else {
        IdentId::new(db, root.to_string())
    };
    let path = PathId::from_ident(db, root_ident);
    Some((root_ident, path))
}
