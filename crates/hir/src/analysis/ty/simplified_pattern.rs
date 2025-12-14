//! Simplified pattern representation for pattern matching analysis
//!
//! This module contains the conversion logic from HIR patterns to a simplified
//! representation that's easier to work with during pattern analysis.

use crate::analysis::HirAnalysisDb;
use crate::analysis::name_resolution::{PathRes, ResolvedVariant, resolve_path};
use crate::analysis::ty::const_eval::{ConstValue, try_eval_const_ref};
use crate::analysis::ty::trait_resolution::PredicateListId;
use crate::analysis::ty::ty_check::ConstRef;
use crate::analysis::ty::ty_def::{InvalidCause, TyId};
use crate::core::hir_def::{
    Body as HirBody, IntegerId, LitKind, Partial, Pat as HirPat, PathId, VariantKind,
    scope_graph::ScopeId,
};
use crate::core::hir_def::{EnumVariant, FieldParent, IdentId, PatId};
use rustc_hash::FxHashMap;
use smallvec1::SmallVec;

use super::adt_def::AdtRef;

/// A simplified representation of a pattern for analysis
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SimplifiedPattern<'db> {
    pub kind: SimplifiedPatternKind<'db>,
    pub ty: TyId<'db>,
}

impl<'db> SimplifiedPattern<'db> {
    pub fn new(kind: SimplifiedPatternKind<'db>, ty: TyId<'db>) -> Self {
        Self { kind, ty }
    }

    pub fn wildcard(bind: Option<(IdentId<'db>, usize)>, ty: TyId<'db>) -> Self {
        Self::new(SimplifiedPatternKind::WildCard(bind), ty)
    }

    /// Pattern that could not be simplified due to earlier errors
    /// (e.g. ambiguous or invalid constructor). These are ignored in
    /// reachability/exhaustiveness checking.
    pub fn error(ty: TyId<'db>) -> Self {
        Self::new(SimplifiedPatternKind::Error, ty)
    }

    pub fn constructor(
        ctor: ConstructorKind<'db>,
        fields: Vec<SimplifiedPattern<'db>>,
        ty: TyId<'db>,
    ) -> Self {
        Self::new(
            SimplifiedPatternKind::Constructor { kind: ctor, fields },
            ty,
        )
    }

    pub fn is_wildcard(&self) -> bool {
        matches!(self.kind, SimplifiedPatternKind::WildCard(_))
    }

    pub fn from_hir_pat(
        db: &'db dyn HirAnalysisDb,
        pat: &HirPat<'db>,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        match pat {
            HirPat::Rest => {
                unreachable!("Rest pattern is only allowed within tuple and record patterns")
            }
            HirPat::WildCard => SimplifiedPattern::wildcard(None, expected_ty),

            HirPat::Lit(lit_partial) => {
                if let Partial::Present(lit_kind) = lit_partial {
                    let ctor = ConstructorKind::Literal(*lit_kind, expected_ty);
                    SimplifiedPattern::constructor(ctor, vec![], expected_ty)
                } else {
                    SimplifiedPattern::wildcard(None, expected_ty)
                }
            }

            HirPat::Path(path_partial, _) => {
                if let Some((ctor, ctor_ty)) =
                    Self::resolve_constructor(path_partial, db, scope, Some(expected_ty))
                {
                    SimplifiedPattern::constructor(ctor, vec![], ctor_ty)
                } else if let Some(lit) =
                    Self::resolve_literal_pat_from_path(path_partial, db, scope, expected_ty)
                {
                    let ctor = ConstructorKind::Literal(lit, expected_ty);
                    SimplifiedPattern::constructor(ctor, vec![], expected_ty)
                } else if let Partial::Present(path_id) = path_partial {
                    // Only a single-segment path can be a binding. If we have a qualified
                    // path (e.g. `Type::CONST`) and it didn't resolve to a constructor or const,
                    // treat it as an errored pattern so we don't incorrectly consider it a
                    // wildcard that makes later patterns unreachable.
                    if path_id.parent(db).is_some() {
                        SimplifiedPattern::error(expected_ty)
                    } else {
                        let binding_name = path_id.ident(db).to_opt().map(|ident| (ident, arm_idx));
                        SimplifiedPattern::wildcard(binding_name, expected_ty)
                    }
                } else {
                    SimplifiedPattern::wildcard(None, expected_ty)
                }
            }

            HirPat::Tuple(elements) => {
                let simplified = simplify_tuple_pattern_elements(
                    db,
                    body,
                    scope,
                    arm_idx,
                    elements,
                    &expected_ty.field_types(db),
                );
                SimplifiedPattern::constructor(
                    ConstructorKind::Type(expected_ty),
                    simplified,
                    expected_ty,
                )
            }

            HirPat::PathTuple(path_partial, elements) => {
                if let Some((ctor, ctor_ty)) =
                    Self::resolve_constructor(path_partial, db, scope, Some(expected_ty))
                {
                    let simplified = simplify_tuple_pattern_elements(
                        db,
                        body,
                        scope,
                        arm_idx,
                        elements,
                        &ctor.field_types(db),
                    );
                    SimplifiedPattern::constructor(ctor, simplified, ctor_ty)
                } else {
                    // Constructor couldn't be resolved (e.g. ambiguous name).
                    // Treat this as an errored pattern so that reachability
                    // analysis does not incorrectly mark later patterns as
                    // unreachable.
                    SimplifiedPattern::error(expected_ty)
                }
            }

            HirPat::Record(path_partial, fields) => {
                if let Some((ctor, ctor_ty)) =
                    Self::resolve_constructor(path_partial, db, scope, Some(expected_ty))
                {
                    let named: FxHashMap<_, _> = fields
                        .iter()
                        .filter_map(|f| Some((f.label(db, body)?, f.pat.data(db, body))))
                        .collect();

                    let mut canonicalized_fields = vec![];
                    for (name, field_ty) in ctor
                        .field_names(db)
                        .expect("Pat::Record constructor must have field names")
                        .iter()
                        .zip(ctor.field_types(db))
                    {
                        let p = match named.get(name) {
                            Some(Partial::Present(fp)) => {
                                Self::from_hir_pat(db, fp, body, scope, arm_idx, field_ty)
                            }
                            Some(Partial::Absent) => {
                                Self::wildcard(Some((*name, arm_idx)), field_ty)
                            }
                            None => Self::wildcard(None, field_ty),
                        };
                        canonicalized_fields.push(p);
                    }

                    Self::constructor(ctor, canonicalized_fields, ctor_ty)
                } else {
                    SimplifiedPattern::wildcard(None, expected_ty)
                }
            }

            HirPat::Or(left, right) => {
                let left_pat =
                    Self::from_partial_pat_id(*left, db, body, scope, arm_idx, expected_ty);
                let right_pat =
                    Self::from_partial_pat_id(*right, db, body, scope, arm_idx, expected_ty);
                SimplifiedPattern::new(
                    SimplifiedPatternKind::Or(vec![left_pat, right_pat]),
                    expected_ty,
                )
            }
        }
    }

    fn from_partial_pat_id(
        pat_id: PatId,
        db: &'db dyn HirAnalysisDb,
        body: HirBody<'db>,
        scope: ScopeId<'db>,
        arm_idx: usize,
        expected_ty: TyId<'db>,
    ) -> Self {
        match pat_id.data(db, body) {
            Partial::Present(pat_data) => {
                SimplifiedPattern::from_hir_pat(db, pat_data, body, scope, arm_idx, expected_ty)
            }
            Partial::Absent => SimplifiedPattern::wildcard(None, expected_ty),
        }
    }

    /// Unified constructor resolution from path
    fn resolve_constructor(
        path_partial: &Partial<PathId<'db>>,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        expected_ty: Option<TyId<'db>>,
    ) -> Option<(ConstructorKind<'db>, TyId<'db>)> {
        let Partial::Present(path_id) = path_partial else {
            return None;
        };

        match resolve_path(db, *path_id, scope, PredicateListId::empty_list(db), true) {
            Ok(PathRes::EnumVariant(variant)) => {
                let ty = expected_ty.unwrap_or(variant.ty);
                let ctor = ConstructorKind::Variant(variant.variant, ty);
                Some((ctor, ty))
            }
            Ok(PathRes::Ty(ty_id)) => {
                // For type paths, check if this is an imported enum variant
                if let Some(expected_ty) = expected_ty
                    && let Some(variant) =
                        Self::try_resolve_enum_variant_from_ty(path_id, db, expected_ty)
                {
                    let ctor = ConstructorKind::Variant(variant.variant, expected_ty);
                    return Some((ctor, expected_ty));
                }
                // Handle struct/tuple types
                let ctor = ConstructorKind::Type(ty_id);
                Some((ctor, ty_id))
            }
            _ => None,
        }
    }

    fn try_resolve_enum_variant_from_ty(
        path_id: &PathId<'db>,
        db: &'db dyn HirAnalysisDb,
        expected_ty: TyId<'db>,
    ) -> Option<ResolvedVariant<'db>> {
        // Check if the expected type is an enum and this path could be a variant
        let expected_enum = expected_ty.as_enum(db)?;
        let variants = expected_enum.variants(db);

        // Try to match the path against variant names
        let path_ident = path_id.ident(db).to_opt()?;
        let path_name = path_ident.data(db);

        for (idx, variant_def) in variants.enumerate() {
            if let Some(variant_name) = variant_def.name(db)
                && variant_name.data(db) == path_name
            {
                let variant = EnumVariant {
                    enum_: expected_enum,
                    idx: idx as u16,
                };

                return Some(ResolvedVariant {
                    ty: expected_ty,
                    variant,
                    path: *path_id,
                });
            }
        }

        None
    }

    fn resolve_literal_pat_from_path(
        path_partial: &Partial<PathId<'db>>,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        expected_ty: TyId<'db>,
    ) -> Option<LitKind<'db>> {
        let Partial::Present(path_id) = path_partial else {
            return None;
        };

        let assumptions = PredicateListId::empty_list(db);
        let resolved = resolve_path(db, *path_id, scope, assumptions, true).ok()?;

        match resolved {
            PathRes::Const(const_def, _) => {
                let cref = ConstRef::Const(const_def);
                match try_eval_const_ref(db, cref, expected_ty)? {
                    ConstValue::Int(int) => Some(LitKind::Int(IntegerId::new(db, int))),
                    ConstValue::Bool(flag) => Some(LitKind::Bool(flag)),
                }
            }
            PathRes::TraitConst(_recv_ty, inst, name) => {
                let cref = ConstRef::TraitConst { inst, name };
                match try_eval_const_ref(db, cref, expected_ty)? {
                    ConstValue::Int(int) => Some(LitKind::Int(IntegerId::new(db, int))),
                    ConstValue::Bool(flag) => Some(LitKind::Bool(flag)),
                }
            }
            _ => None,
        }
    }
}

fn simplify_tuple_pattern_elements<'db>(
    db: &'db dyn HirAnalysisDb,
    body: HirBody<'db>,
    scope: ScopeId<'db>,
    arm_idx: usize,
    elements: &[PatId],
    elem_tys: &[TyId<'db>],
) -> Vec<SimplifiedPattern<'db>> {
    let mut simplified = vec![];

    let mut elem_tys_iter = elem_tys.iter();
    for pat in elements {
        if pat.is_rest(db, body) {
            let remaining = elem_tys
                .len()
                .saturating_sub(elements.len().saturating_sub(1));
            for _ in 0..remaining {
                let ty = elem_tys_iter
                    .next()
                    .copied()
                    .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));
                simplified.push(SimplifiedPattern::new(
                    SimplifiedPatternKind::WildCard(None),
                    ty,
                ));
            }
        } else {
            let elem_ty = elem_tys_iter
                .next()
                .copied()
                .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other));
            simplified.push(SimplifiedPattern::from_hir_pat(
                db,
                pat.data(db, body).unwrap_ref(),
                body,
                scope,
                arm_idx,
                elem_ty,
            ));
        }
    }
    simplified
}

/// The kind of a simplified pattern
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimplifiedPatternKind<'db> {
    WildCard(Option<(IdentId<'db>, usize)>),
    Constructor {
        kind: ConstructorKind<'db>,
        fields: Vec<SimplifiedPattern<'db>>,
    },
    Or(Vec<SimplifiedPattern<'db>>),
    /// Represents a pattern we failed to resolve (e.g. ambiguous constructor).
    /// Used to suppress confusing reachability diagnostics when earlier phases
    /// already reported an error.
    Error,
}

impl<'db> SimplifiedPatternKind<'db> {
    pub(crate) fn collect_ctors(&self) -> Vec<ConstructorKind<'db>> {
        match self {
            Self::WildCard(_) | Self::Error => vec![],
            Self::Constructor { kind, .. } => vec![*kind],
            Self::Or(pats) => {
                let mut ctors = vec![];
                for pat in pats {
                    ctors.extend_from_slice(&pat.kind.collect_ctors());
                }
                ctors
            }
        }
    }

    pub fn ctor_with_wild_card_fields(
        db: &'db dyn HirAnalysisDb,
        kind: ConstructorKind<'db>,
    ) -> Self {
        let fields = kind
            .field_types(db)
            .into_iter()
            .map(|ty| SimplifiedPattern::wildcard(None, ty))
            .collect();
        Self::Constructor { kind, fields }
    }
}

/// Represents different kinds of constructors that can appear in patterns
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ConstructorKind<'db> {
    /// Enum variant - stores just the variant and type, not the path
    Variant(EnumVariant<'db>, TyId<'db>),
    Type(TyId<'db>),
    Literal(LitKind<'db>, TyId<'db>),
}

impl<'db> ConstructorKind<'db> {
    pub fn field_types(&self, db: &'db dyn HirAnalysisDb) -> Vec<TyId<'db>> {
        match self {
            Self::Variant(variant, ty) => {
                if let Some(adt_def) = ty.adt_def(db) {
                    let args = ty.generic_args(db);

                    adt_def
                        .fields(db)
                        .get(variant.idx as usize)
                        .map(|field_list| {
                            field_list
                                .iter_types(db)
                                .map(|binder| binder.instantiate(db, args))
                                .collect()
                        })
                        .unwrap_or_default()
                } else {
                    vec![]
                }
            }
            Self::Type(ty) => ty.field_types(db),
            Self::Literal(_, _) => vec![],
        }
    }

    pub fn field_names(&self, db: &'db dyn HirAnalysisDb) -> Option<SmallVec<[IdentId<'db>; 4]>> {
        let field_parent = match self {
            Self::Variant(v, _) if matches!(v.kind(db), VariantKind::Record(..)) => {
                Some(FieldParent::Variant(*v))
            }
            Self::Type(ty) => match ty.adt_def(db)?.adt_ref(db) {
                AdtRef::Struct(struct_def) => Some(FieldParent::Struct(struct_def)),
                _ => None,
            },
            _ => None,
        }?;
        Some(field_parent.fields(db).filter_map(|v| v.name(db)).collect())
    }

    pub fn arity(&self, db: &'db dyn HirAnalysisDb) -> usize {
        match self {
            Self::Variant(variant, _) => {
                // Get field count from the variant
                match variant.kind(db) {
                    VariantKind::Unit => 0,
                    VariantKind::Tuple(types) => types.data(db).len(),
                    VariantKind::Record(fields) => fields.data(db).len(),
                }
            }
            Self::Type(ty) => ty.field_count(db),
            Self::Literal(_, _) => 0,
        }
    }
}

pub fn ctor_variant_num<'db>(db: &'db dyn HirAnalysisDb, ctor: &ConstructorKind<'db>) -> usize {
    match ctor {
        ConstructorKind::Variant(variant, _) => variant.enum_.len_variants(db),
        ConstructorKind::Type(_) => 1,
        ConstructorKind::Literal(LitKind::Bool(_), _) => 2,
        ConstructorKind::Literal(LitKind::Int(_), _) => usize::MAX, // Infinite possibilities
        ConstructorKind::Literal(LitKind::String(_), _) => usize::MAX, // Infinite possibilities
    }
}

pub fn display_missing_pattern<'db>(
    db: &'db dyn HirAnalysisDb,
    pat: &SimplifiedPattern<'db>,
) -> String {
    match &pat.kind {
        SimplifiedPatternKind::WildCard(_) => "_".to_string(),

        SimplifiedPatternKind::Constructor { kind, fields, .. } => {
            match kind {
                ConstructorKind::Variant(variant, _) => {
                    // Get the actual variant name
                    let variant_name = variant
                        .name(db)
                        .map(|name| name.to_string())
                        .unwrap_or_else(|| "UnknownVariant".to_string());

                    // Get enum name for better context
                    let enum_name = match variant.enum_.name(db) {
                        Partial::Present(name) => name.data(db).to_string(),
                        Partial::Absent => "UnknownEnum".to_string(),
                    };

                    let full_name = format!("{enum_name}::{variant_name}");

                    match variant.kind(db) {
                        crate::core::hir_def::VariantKind::Unit => full_name,
                        crate::core::hir_def::VariantKind::Tuple(_) => {
                            if fields.is_empty() {
                                format!("{full_name}(..)")
                            } else {
                                let field_patterns: Vec<String> = fields
                                    .iter()
                                    .map(|f| display_missing_pattern(db, f))
                                    .collect();
                                format!("{}({})", full_name, field_patterns.join(", "))
                            }
                        }
                        crate::core::hir_def::VariantKind::Record(_) => {
                            if fields.is_empty() {
                                format!("{full_name} {{ .. }}")
                            } else {
                                // For record variants, we'd need field names which are complex to get
                                // For now, use the simpler pattern
                                format!("{full_name} {{ .. }}")
                            }
                        }
                    }
                }
                ConstructorKind::Type(ty) => {
                    if ty.is_tuple(db) {
                        if fields.is_empty() {
                            "()".to_string()
                        } else {
                            let parts: Vec<String> = fields
                                .iter()
                                .map(|f| display_missing_pattern(db, f))
                                .collect();
                            format!("({})", parts.join(", "))
                        }
                    } else {
                        // Try to get struct/type name
                        let type_name = ty.pretty_print(db);
                        format!("{type_name} {{ .. }}")
                    }
                }
                ConstructorKind::Literal(lit, _) => match lit {
                    LitKind::Bool(b) => b.to_string(),
                    LitKind::Int(i) => i.data(db).to_string(),
                    LitKind::String(s) => format!("\"{}\"", s.data(db)),
                },
            }
        }

        SimplifiedPatternKind::Or(patterns) => {
            if patterns.is_empty() {
                "_".to_string()
            } else if patterns.len() == 1 {
                display_missing_pattern(db, &patterns[0])
            } else {
                // For multiple patterns, show a few concrete examples
                let examples: Vec<String> = patterns
                    .iter()
                    .take(3)
                    .map(|p| display_missing_pattern(db, p))
                    .collect();

                if patterns.len() <= 3 {
                    examples.join(" | ")
                } else {
                    format!(
                        "{} | ... ({} more)",
                        examples.join(" | "),
                        patterns.len() - 3
                    )
                }
            }
        }

        // Errored patterns are represented generically.
        SimplifiedPatternKind::Error => "_".to_string(),
    }
}
