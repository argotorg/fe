use std::collections::HashSet;

use common::ingot::IngotKind;

use crate::{
    analysis::{
        HirAnalysisDb,
        analysis_pass::ModuleAnalysisPass,
        diagnostics::DiagnosticVoucher,
        name_resolution::{NameDomain, PathRes, resolve_ident_to_bucket, resolve_path},
        ty::trait_resolution::PredicateListId,
    },
    hir_def::{HirIngot, IdentId, PathId, TopLevelMod, Trait, scope_graph::ScopeId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreRequirementKind {
    Trait,
    TraitMethod,
    TraitMethodArgCount,
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreRequirementViolation {
    pub path: String,
    pub kind: CoreRequirementKind,
    pub method: Option<String>,
    pub expected_arg_count: Option<usize>,
    pub given_arg_count: Option<usize>,
}

impl CoreRequirementViolation {
    pub fn local_code(&self) -> u16 {
        match self.kind {
            CoreRequirementKind::Trait => 101,
            CoreRequirementKind::TraitMethod => 102,
            CoreRequirementKind::TraitMethodArgCount => 103,
            CoreRequirementKind::Type => 104,
        }
    }
}

impl std::fmt::Display for CoreRequirementViolation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            CoreRequirementKind::Trait => {
                write!(f, "missing required core trait `{}`", self.path)
            }
            CoreRequirementKind::TraitMethod => {
                let method = self.method.as_deref().unwrap_or("unknown");
                write!(
                    f,
                    "missing required core trait method `{}` on `{}`",
                    method, self.path
                )
            }
            CoreRequirementKind::TraitMethodArgCount => {
                let method = self.method.as_deref().unwrap_or("unknown");
                let expected = self.expected_arg_count.unwrap_or_default();
                let given = self.given_arg_count.unwrap_or_default();
                write!(
                    f,
                    "invalid required core trait method signature `{}` on `{}`: expected {} arguments, but {} given",
                    method, self.path, expected, given
                )
            }
            CoreRequirementKind::Type => {
                write!(f, "missing required core type `{}`", self.path)
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct RequiredTraitMethod {
    path: &'static str,
    method: &'static str,
    arg_count: usize,
}

const CORE_OP_REQUIREMENTS: &[RequiredTraitMethod] = &[
    RequiredTraitMethod {
        path: "core::ops::Neg",
        method: "neg",
        arg_count: 1,
    },
    RequiredTraitMethod {
        path: "core::ops::Not",
        method: "not",
        arg_count: 1,
    },
    RequiredTraitMethod {
        path: "core::ops::BitNot",
        method: "bit_not",
        arg_count: 1,
    },
    RequiredTraitMethod {
        path: "core::ops::Add",
        method: "add",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Sub",
        method: "sub",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Mul",
        method: "mul",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Div",
        method: "div",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Rem",
        method: "rem",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Pow",
        method: "pow",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Shl",
        method: "shl",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Shr",
        method: "shr",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::BitAnd",
        method: "bitand",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::BitOr",
        method: "bitor",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::BitXor",
        method: "bitxor",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Eq",
        method: "eq",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Ord",
        method: "lt",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Ord",
        method: "le",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Ord",
        method: "gt",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Ord",
        method: "ge",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::Index",
        method: "index",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::AddAssign",
        method: "add_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::SubAssign",
        method: "sub_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::MulAssign",
        method: "mul_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::DivAssign",
        method: "div_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::RemAssign",
        method: "rem_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::PowAssign",
        method: "pow_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::ShlAssign",
        method: "shl_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::ShrAssign",
        method: "shr_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::BitAndAssign",
        method: "bitand_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::BitOrAssign",
        method: "bitor_assign",
        arg_count: 2,
    },
    RequiredTraitMethod {
        path: "core::ops::BitXorAssign",
        method: "bitxor_assign",
        arg_count: 2,
    },
];

const CORE_TRAIT_REQUIREMENTS: &[&str] = &[
    "core::effect_ref::EffectRef",
    "core::effect_ref::EffectRefMut",
    "core::effect_ref::EffectHandle",
    "core::message::MsgVariant",
    "core::abi::Decode",
];

const STD_TYPE_REQUIREMENTS: &[&str] = &[
    "std::evm::effects::MemPtr",
    "std::evm::effects::StorPtr",
    "std::evm::effects::CalldataPtr",
];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreRequirementDiag<'db> {
    pub top_mod: TopLevelMod<'db>,
    pub violation: CoreRequirementViolation,
}

pub struct CoreRequirementsAnalysisPass;

impl ModuleAnalysisPass for CoreRequirementsAnalysisPass {
    fn run_on_module<'db>(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        top_mod: TopLevelMod<'db>,
    ) -> Vec<Box<dyn DiagnosticVoucher + 'db>> {
        if top_mod != top_mod.ingot(db).root_mod(db) {
            return Vec::new();
        }

        check_ingot_requirements(db, top_mod)
            .into_iter()
            .map(|diag| Box::new(diag) as _)
            .collect()
    }
}

pub fn check_ingot_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<CoreRequirementDiag<'db>> {
    let ingot = top_mod.ingot(db);
    let violations = match ingot.kind(db) {
        IngotKind::Core => check_core_requirements(db, top_mod.scope(), ingot.kind(db)),
        IngotKind::Std => check_std_type_requirements(db, top_mod.scope(), ingot.kind(db)),
        _ => Vec::new(),
    };

    violations
        .into_iter()
        .map(|violation| CoreRequirementDiag { top_mod, violation })
        .collect()
}

pub fn check_core_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ingot_kind: IngotKind,
) -> Vec<CoreRequirementViolation> {
    let mut violations = Vec::new();
    let mut missing_traits = HashSet::new();

    for &path in CORE_TRAIT_REQUIREMENTS {
        if resolve_trait_in_scope(db, scope, ingot_kind, path).is_none() {
            violations.push(CoreRequirementViolation {
                path: path.to_string(),
                kind: CoreRequirementKind::Trait,
                method: None,
                expected_arg_count: None,
                given_arg_count: None,
            });
        }
    }

    for req in CORE_OP_REQUIREMENTS {
        match resolve_trait_in_scope(db, scope, ingot_kind, req.path) {
            Some(trait_def) => {
                let method_ident = IdentId::new(db, req.method.to_string());
                let Some(method) = trait_def.method_defs(db).get(&method_ident).copied() else {
                    violations.push(CoreRequirementViolation {
                        path: req.path.to_string(),
                        kind: CoreRequirementKind::TraitMethod,
                        method: Some(req.method.to_string()),
                        expected_arg_count: None,
                        given_arg_count: None,
                    });
                    continue;
                };

                let given_arg_count = method.arg_tys(db).len();
                if given_arg_count != req.arg_count {
                    violations.push(CoreRequirementViolation {
                        path: req.path.to_string(),
                        kind: CoreRequirementKind::TraitMethodArgCount,
                        method: Some(req.method.to_string()),
                        expected_arg_count: Some(req.arg_count),
                        given_arg_count: Some(given_arg_count),
                    });
                }
            }
            None => {
                if missing_traits.insert(req.path) {
                    violations.push(CoreRequirementViolation {
                        path: req.path.to_string(),
                        kind: CoreRequirementKind::Trait,
                        method: None,
                        expected_arg_count: None,
                        given_arg_count: None,
                    });
                }
            }
        }
    }

    violations
}

pub fn check_std_type_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ingot_kind: IngotKind,
) -> Vec<CoreRequirementViolation> {
    let mut violations = Vec::new();

    for &path in STD_TYPE_REQUIREMENTS {
        if resolve_type_in_scope(db, scope, ingot_kind, path).is_none() {
            violations.push(CoreRequirementViolation {
                path: path.to_string(),
                kind: CoreRequirementKind::Type,
                method: None,
                expected_arg_count: None,
                given_arg_count: None,
            });
        }
    }

    violations
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
