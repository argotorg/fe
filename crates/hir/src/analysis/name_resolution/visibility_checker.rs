use common::ingot::Ingot;

use crate::core::hir_def::{FieldParent, ItemKind, Use, scope_graph::ScopeId};

use crate::analysis::{
    HirAnalysisDb,
    ty::{
        adt_def::AdtRef,
        const_ty::ConstTyData,
        ty_def::{TyBase, TyData, TyId},
    },
};

pub(super) fn is_authored_module(db: &dyn HirAnalysisDb, scope: ScopeId) -> bool {
    matches!(scope, ScopeId::Item(ItemKind::Mod(_) | ItemKind::TopMod(_)))
        && !scope.is_in_published_item(db)
}

/// Compute the "defining scope" for visibility purposes.
///
/// For a private item, this is the scope within which the item is visible.
/// - For associated functions: the parent of the parent item (i.e. the module containing the impl/trait)
/// - For trait members: always visible (returns `None` to signal "always visible")
/// - For fields/variants: the parent of the containing type
/// - For other items: the direct parent scope
fn def_scope_for_vis<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Option<ScopeId<'db>> {
    match scope {
        ScopeId::Item(ItemKind::Func(func)) => {
            if func.is_associated_func(db) {
                scope
                    .parent_item(db)
                    .and_then(|item| ScopeId::Item(item).parent(db))
            } else {
                scope.parent(db)
            }
        }
        ScopeId::Item(_) => scope.parent(db),
        // Like associated functions: private consts are visible within the
        // module containing the impl, not just inside the impl block.
        ScopeId::ImplConst(..) | ScopeId::Field(..) | ScopeId::Variant(..) => {
            let parent_item = scope.item();
            ScopeId::Item(parent_item).parent(db)
        }
        _ => scope.parent(db),
    }
}

/// Return `true` if the given `scope` is visible from `from_scope`.
pub fn is_scope_visible_from(db: &dyn HirAnalysisDb, scope: ScopeId, from_scope: ScopeId) -> bool {
    is_scope_visible_to(db, scope, from_scope, from_scope.ingot(db))
}

/// Return `true` if `scope` is visible to code in the ingot `from_ingot` that
/// runs at `from_scope`. Lexical privacy is judged from `from_scope`; the
/// visibility that names an ingot is judged against `from_ingot`, which is the
/// ingot of `from_scope` unless a body was written by another package than the
/// one it runs in (see `Viewer`).
fn is_scope_visible_to(
    db: &dyn HirAnalysisDb,
    scope: ScopeId,
    from_scope: ScopeId,
    from_ingot: Ingot,
) -> bool {
    use crate::core::hir_def::item::Visibility;

    let vis = scope.data(db).vis;

    // Fast path: unrestricted `pub` is visible everywhere.
    if vis.is_pub() {
        return true;
    }

    // Trait members are always visible regardless of their declared visibility.
    if matches!(scope, ScopeId::Item(ItemKind::Func(_)))
        && matches!(scope.parent_item(db), Some(ItemKind::Trait(..)))
    {
        return true;
    }

    match vis {
        Visibility::Public => unreachable!(),

        Visibility::PubIngot => {
            // Visible within the same ingot.
            scope.ingot(db) == from_ingot
        }

        Visibility::PubSuper => {
            // Visible within the grandparent scope (parent of the defining module).
            let Some(def_scope) = def_scope_for_vis(db, scope) else {
                return false;
            };
            match def_scope.parent(db) {
                Some(parent_of_def) => from_scope.is_transitive_child_of(db, parent_of_def),
                // At the ingot root, pub(super) behaves like pub(ingot)
                // (matches Rust where pub(super) at crate root = pub(crate)).
                None => scope.ingot(db) == from_ingot,
            }
        }

        Visibility::Private => {
            // Visible within the defining scope only.
            let Some(def_scope) = def_scope_for_vis(db, scope) else {
                return false;
            };
            from_scope.is_transitive_child_of(db, def_scope)
        }
    }
}

/// Whether `target` is visible to the package that wrote the template that
/// `scope` is published from. A published body climbs its lexical parents into
/// the provider's quotation, so what the provider's own package may see,
/// beyond what the package that holds `scope` may, is judged against that
/// package. Inherent method selection, which knows the scope but not the
/// expression, admits what any writer of the code could use; the member check
/// of the type checker then decides for the expression's own writer (`Viewer`).
pub(super) fn is_scope_visible_to_lexical_writers(
    db: &dyn HirAnalysisDb,
    target: ScopeId,
    scope: ScopeId,
) -> bool {
    let own = scope.ingot(db);
    let target_ingot = target.ingot(db);
    if own == target_ingot {
        return false;
    }
    let mut current = scope.lex_parent(db);
    while let Some(ancestor) = current {
        if ancestor.ingot(db) == target_ingot {
            return is_scope_visible_to(db, target, scope, target_ingot);
        }
        current = ancestor.lex_parent(db);
    }
    false
}

/// Who looks at a name: the place the code runs, the package that wrote it and
/// the record that its header requests. Name resolution and the member checks
/// of the type checker decide visibility through this one value.
///
/// The names in a template resolve where the template is written, so the
/// members it reaches are judged by the package that wrote it too. Code that a
/// package wrote and uses itself sees what `is_scope_visible_from` says. The
/// code that a provider of another package wrote for a header may use the
/// public items of the package that asked, the fields and variants of the
/// record the header requests, and what the provider generated; the private
/// and `pub(ingot)` items of the asking package are not its to use. The
/// provider's own items are judged against the provider's package, so its
/// `pub(ingot)` items are visible to its generated code.
#[derive(Clone, Copy, Debug)]
pub struct Viewer<'db> {
    scope: ScopeId<'db>,
    writer: Ingot<'db>,
    requested: Option<AdtRef<'db>>,
}

impl<'db> Viewer<'db> {
    /// Code that runs at `scope` and was written in the package of `scope`.
    pub fn at(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        Self {
            scope,
            writer: scope.ingot(db),
            requested: None,
        }
    }

    /// Code that runs at `scope` and was written by the package `writer`, for
    /// the header that requests `requested`.
    pub fn written_by(
        scope: ScopeId<'db>,
        writer: Ingot<'db>,
        requested: Option<AdtRef<'db>>,
    ) -> Self {
        Self {
            scope,
            writer,
            requested,
        }
    }

    /// The place the code runs.
    pub fn scope(self) -> ScopeId<'db> {
        self.scope
    }

    /// Whether the code may use `target`, an item, a field, a variant or an
    /// inherent method.
    pub fn sees(self, db: &'db dyn HirAnalysisDb, target: ScopeId<'db>) -> bool {
        if self.writer == target.ingot(db) {
            // An item of the writer's package, judged against that package.
            return is_scope_visible_to(db, target, self.scope, self.writer);
        }
        if target.syntax_top_mod(db).ingot(db) == self.writer {
            // An item that the writer generated into the package it runs in.
            return is_scope_visible_from(db, target, self.scope);
        }
        if target.data(db).vis.is_pub()
            || matches!(target, ScopeId::Item(ItemKind::Func(_)))
                && matches!(target.parent_item(db), Some(ItemKind::Trait(..)))
        {
            return true;
        }
        let Some(requested) = self.requested else {
            return false;
        };
        match target {
            ScopeId::Field(FieldParent::Struct(struct_), _) => requested == AdtRef::Struct(struct_),
            ScopeId::Field(FieldParent::Variant(variant), _) | ScopeId::Variant(variant) => {
                requested == AdtRef::Enum(variant.enum_)
            }
            _ => false,
        }
    }
}

pub(crate) fn is_ty_visible_from(db: &dyn HirAnalysisDb, ty: TyId, from_scope: ScopeId) -> bool {
    match ty.base_ty(db).data(db) {
        TyData::TyBase(base) => match base {
            TyBase::Prim(_) | TyBase::Closure(_) | TyBase::Shape(_) => true,
            TyBase::Adt(adt) => is_scope_visible_from(db, adt.scope(db), from_scope),
            TyBase::Contract(c) => is_scope_visible_from(db, c.scope(), from_scope),
            TyBase::Func(func) => is_scope_visible_from(db, func.scope(), from_scope),
        },
        TyData::TyParam(param) => is_scope_visible_from(db, param.scope(db), from_scope),
        // Associated type projections (e.g., `T::Assoc`, `<T as Trait>::Assoc`) are
        // semantic types, not free type items. They should be usable wherever the
        // projection type itself is well-formed, regardless of the visibility of
        // the associated type declaration inside the trait. Treat them as visible.
        TyData::AssocTy(_assoc_ty) => true,

        TyData::ConstTy(const_ty) => match const_ty.data(db) {
            ConstTyData::TyVar(_, _) => true,
            ConstTyData::TyParam(param, _) => {
                is_scope_visible_from(db, param.scope(db), from_scope)
            }
            ConstTyData::Value(_) | ConstTyData::Description(_) | ConstTyData::Invalid(_) => true,
            ConstTyData::Abstract(_, _) | ConstTyData::Computation { .. } => true,
            ConstTyData::UnEvaluated { body, .. } => {
                is_scope_visible_from(db, body.scope(), from_scope)
            }
        },
        TyData::TyVar(_) | TyData::Never | TyData::Invalid(_) => true,
        TyData::QualifiedTy(trait_inst) => {
            is_scope_visible_from(db, trait_inst.def(db).scope(), from_scope)
        }
        TyData::ConstraintTerm(goal) => is_scope_visible_from(db, goal.def(db).scope(), from_scope),
        TyData::TraitFamily(family) => {
            is_scope_visible_from(db, family.def(db).scope(), from_scope)
        }
        TyData::TyApp(_, _) => unreachable!(),
    }
}

/// Return `true` if the given `use_` is visible from the `ref_scope`.
pub(super) fn is_use_visible(db: &dyn HirAnalysisDb, ref_scope: ScopeId, use_: Use) -> bool {
    let use_scope = ScopeId::from_item(use_.into());
    is_scope_visible_from(db, use_scope, ref_scope)
}
