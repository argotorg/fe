//! Projection return shapes.
//!
//! `ref` and `mut` are not types. A function whose return mentions them is a
//! projection, and its return is a *shape*: owned components, access
//! components, tuples of shapes, and the sum shapes `Option<shape>` and
//! `Result<E, shape>`. The type checker sees a projection call's *erased*
//! type; the shape records which parts of that type the call grants as
//! accesses. Semantic lowering represents an access component by a carrier.
use salsa::SalsaValue;

use super::{
    const_ty::{ConstBodyLowering, LoweringContext},
    corelib::resolve_lib_type_path,
    diagnostics::TyLowerDiag,
    fold::{TyFoldable, TyFolder},
    trait_resolution::{PredicateListId, TraitSolveCx},
    ty_def::{AssocTy, BorrowKind, InvalidCause, Kind, ShapeCon, ShapeMode, TyData, TyId},
    ty_error::{WrittenTyDiags, check_written_ty},
    ty_lower::{lower_hir_ty, lower_hir_ty_with_minter},
    visitor::{TyVisitable, TyVisitor, walk_assoc_ty},
};
use crate::{
    analysis::HirAnalysisDb,
    hir_def::{
        FuncParamMode, GenericArg, Partial, PathId, TypeId as HirTyId, TypeKind, TypeMode,
        scope_graph::ScopeId,
    },
    span::types::LazyTySpan,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub enum Shape<'db> {
    Owned(TyId<'db>),
    Access(BorrowKind, TyId<'db>),
    Tuple(Vec<Shape<'db>>),
    /// `Option<shape>` or `Result<E, shape>`. Only `variant` carries the
    /// payload shape, as type argument `arg` of `ty`, the erased enum type.
    Sum {
        ty: TyId<'db>,
        variant: u16,
        arg: usize,
        payload: Box<Shape<'db>>,
    },
    /// A shape whose structure is not known here, the shape term of an
    /// associated shape at a mode (`D::Step<ref>`) in generic code. Folding
    /// it decodes the term once it is known.
    Opaque(TyId<'db>),
}

/// A `#[view]` type exists only behind view and `ref` accesses: `ty` takes
/// one wrongly if it is a part of another type, or if it is one and the
/// position it is in would make it `whole`. Returns the view type and what
/// the position would make it.
fn view_misuse<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    whole: Option<&'static str>,
) -> Option<(TyId<'db>, &'static str)> {
    match ty.view_part(db) {
        Some(view) => Some((view, "part of another type")),
        None => whole
            .filter(|_| ty.is_view(db))
            .map(|position| (ty, position)),
    }
}

/// A `#[view]` type a parameter of `mode` and type `ty` takes wrongly: as an
/// `own` parameter, or as part of another type.
pub fn view_param_misuse<'db>(
    db: &'db dyn HirAnalysisDb,
    mode: FuncParamMode,
    ty: TyId<'db>,
) -> Option<(TyId<'db>, &'static str)> {
    let whole = (mode == FuncParamMode::Own).then_some("an `own` parameter");
    view_misuse(db, ty, whole)
}

impl<'db> Shape<'db> {
    /// A `#[view]` type this return takes wrongly: by value, or as part of
    /// another type.
    pub fn view_misuse(&self, db: &'db dyn HirAnalysisDb) -> Option<(TyId<'db>, &'static str)> {
        match self {
            Self::Owned(ty) => view_misuse(db, *ty, Some("returned by value")),
            Self::Access(_, ty) => view_misuse(db, *ty, None),
            Self::Tuple(elems) => elems.iter().find_map(|elem| elem.view_misuse(db)),
            Self::Sum { payload, .. } => payload.view_misuse(db),
            Self::Opaque(_) => None,
        }
    }

    /// A `#[view]` type this return yields by `mut`.
    pub fn mut_view(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        match self {
            Self::Owned(_) => None,
            Self::Access(kind, ty) => (*kind == BorrowKind::Mut && ty.is_view(db)).then_some(*ty),
            Self::Tuple(elems) => elems.iter().find_map(|elem| elem.mut_view(db)),
            Self::Sum { payload, .. } => payload.mut_view(db),
            Self::Opaque(_) => None,
        }
    }

    /// A state-only type this return moves out by value, with the storage
    /// collection it holds. A projection yields a storage place instead.
    pub fn state_only_value(&self, db: &'db dyn HirAnalysisDb) -> Option<(TyId<'db>, TyId<'db>)> {
        match self {
            Self::Owned(ty) => ty.pinned_part(db).map(|collection| (*ty, collection)),
            Self::Access(..) => None,
            Self::Tuple(elems) => elems.iter().find_map(|elem| elem.state_only_value(db)),
            Self::Sum { payload, .. } => payload.state_only_value(db),
            Self::Opaque(_) => None,
        }
    }

    /// The leaves of a product parameter's shape, in order: each part's
    /// access, or `None` for an owned part, and its type. An opaque part is
    /// one leaf, transported whole as a value of the shape at its mode, which
    /// its carrier keeps.
    pub fn leaves(&self, db: &'db dyn HirAnalysisDb) -> Vec<(Option<BorrowKind>, TyId<'db>)> {
        match self {
            Self::Owned(ty) => vec![(None, *ty)],
            Self::Access(kind, ty) => vec![(Some(*kind), *ty)],
            Self::Tuple(elems) => elems.iter().flat_map(|elem| elem.leaves(db)).collect(),
            Self::Sum { payload, .. } => payload.leaves(db),
            Self::Opaque(term) => vec![(None, TyId::shape_part(db, ShapeCon::Value, *term))],
        }
    }

    /// The number of leaves `leaves` gives.
    pub fn leaf_count(&self) -> usize {
        match self {
            Self::Owned(_) | Self::Access(..) | Self::Opaque(_) => 1,
            Self::Tuple(elems) => elems.iter().map(Self::leaf_count).sum(),
            Self::Sum { payload, .. } => payload.leaf_count(),
        }
    }

    /// The mode and type of each of a product parameter's parts, in order,
    /// as a parameter of its own would take it: an owned part is `own`, and
    /// an access part `mut` or a view.
    pub fn part_modes(&self, db: &'db dyn HirAnalysisDb) -> Vec<(FuncParamMode, TyId<'db>)> {
        self.leaves(db)
            .into_iter()
            .map(|(access, ty)| {
                let mode = match access {
                    None => FuncParamMode::Own,
                    Some(BorrowKind::Ref) => FuncParamMode::View,
                    Some(BorrowKind::Mut) => FuncParamMode::Mut,
                };
                (mode, ty)
            })
            .collect()
    }

    /// The shape of a parameter of `mode` and type `ty`.
    pub fn param(mode: FuncParamMode, ty: TyId<'db>) -> Self {
        match mode {
            FuncParamMode::Own => Self::Owned(ty),
            FuncParamMode::View => Self::Access(BorrowKind::Ref, ty),
            FuncParamMode::Mut => Self::Access(BorrowKind::Mut, ty),
        }
    }

    /// The mode a parameter of this shape takes its argument at: `own`, a
    /// view or `mut`. `None` for a product.
    pub fn param_mode(&self) -> Option<FuncParamMode> {
        match self {
            Self::Owned(_) => Some(FuncParamMode::Own),
            Self::Access(BorrowKind::Ref, _) => Some(FuncParamMode::View),
            Self::Access(BorrowKind::Mut, _) => Some(FuncParamMode::Mut),
            Self::Tuple(_) | Self::Sum { .. } | Self::Opaque(_) => None,
        }
    }

    /// Whether this shape grants an access, i.e. whether a function returning
    /// it is a projection.
    pub fn has_access(&self) -> bool {
        match self {
            Self::Owned(_) => false,
            Self::Access(..) => true,
            Self::Tuple(elems) => elems.iter().any(Self::has_access),
            Self::Sum { payload, .. } => payload.has_access(),
            // An associated shape a projection returns has an access in
            // every instance.
            Self::Opaque(_) => true,
        }
    }

    /// Whether this shape may grant an access of `kind`. An opaque shape's
    /// accesses are at most its mode.
    pub fn has_access_of(&self, db: &'db dyn HirAnalysisDb, kind: BorrowKind) -> bool {
        match self {
            Self::Owned(_) => false,
            Self::Access(given, _) => *given == kind,
            Self::Tuple(elems) => elems.iter().any(|elem| elem.has_access_of(db, kind)),
            Self::Sum { payload, .. } => payload.has_access_of(db, kind),
            Self::Opaque(term) => match term.data(db) {
                TyData::AssocTy(assoc) => assoc
                    .mode
                    .and_then(ShapeMode::kind)
                    .is_none_or(|mode| kind <= mode),
                _ => true,
            },
        }
    }

    /// The type a projection call has in the type checker.
    pub fn erased_ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self {
            Self::Owned(ty) | Self::Access(_, ty) | Self::Sum { ty, .. } => *ty,
            Self::Tuple(elems) => TyId::tuple_with_elems(
                db,
                &elems
                    .iter()
                    .map(|elem| elem.erased_ty(db))
                    .collect::<Vec<_>>(),
            ),
            // A value's type is the shape's at every mode, as an access's
            // type is its target's.
            Self::Opaque(term) => {
                let term = match term.data(db) {
                    TyData::AssocTy(assoc) => TyId::new(
                        db,
                        TyData::AssocTy(AssocTy {
                            mode: None,
                            ..*assoc
                        }),
                    ),
                    _ => *term,
                };
                TyId::shape_part(db, ShapeCon::Value, term)
            }
        }
    }

    /// The semantic-IR representation of the shape: access components are
    /// carriers.
    pub fn carrier_ty(&self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self {
            Self::Owned(ty) => *ty,
            Self::Access(kind, ty) => TyId::borrow_of(db, *kind, *ty),
            Self::Tuple(elems) => TyId::tuple_with_elems(
                db,
                &elems
                    .iter()
                    .map(|elem| elem.carrier_ty(db))
                    .collect::<Vec<_>>(),
            ),
            Self::Sum {
                ty, arg, payload, ..
            } => {
                let (base, args) = ty.decompose_ty_app(db);
                args.iter().enumerate().fold(base, |acc, (idx, &ty_arg)| {
                    let ty_arg = if idx == *arg {
                        payload.carrier_ty(db)
                    } else {
                        ty_arg
                    };
                    TyId::app(db, acc, ty_arg)
                })
            }
            Self::Opaque(term) => TyId::shape_part(db, ShapeCon::Carrier, *term),
        }
    }

    /// Whether a value granting `self` satisfies `required`: the same
    /// structure, with each access at least as strong. An opaque part is
    /// passed on whole, at its mode; its type is compared with the rest of
    /// the types.
    pub fn grants(&self, db: &'db dyn HirAnalysisDb, required: &Self) -> bool {
        match (self, required) {
            (Self::Owned(_), Self::Owned(_)) => true,
            (Self::Access(given, _), Self::Access(required, _)) => given >= required,
            (Self::Tuple(given), Self::Tuple(required)) => {
                given.len() == required.len()
                    && given
                        .iter()
                        .zip(required)
                        .all(|(given, required)| given.grants(db, required))
            }
            (
                Self::Sum {
                    variant, payload, ..
                },
                Self::Sum {
                    variant: required_variant,
                    payload: required_payload,
                    ..
                },
            ) => variant == required_variant && payload.grants(db, required_payload),
            (Self::Opaque(given), Self::Opaque(required)) => {
                opaque_mode(db, *given) == opaque_mode(db, *required)
            }
            _ => false,
        }
    }

    /// Whether `self` has `other`'s structure: an access where it has one,
    /// whatever the accesses' kinds.
    pub fn same_structure(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Owned(_), Self::Owned(_)) | (Self::Access(..), Self::Access(..)) => true,
            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.len() == rhs.len()
                    && lhs
                        .iter()
                        .zip(rhs)
                        .all(|(lhs, rhs)| lhs.same_structure(rhs))
            }
            (
                Self::Sum {
                    variant, payload, ..
                },
                Self::Sum {
                    variant: other_variant,
                    payload: other_payload,
                    ..
                },
            ) => variant == other_variant && payload.same_structure(other_payload),
            _ => false,
        }
    }

    /// Whether two shapes grant the same accesses, ignoring the types: an
    /// opaque part's are its mode's.
    pub fn same_modes(&self, db: &'db dyn HirAnalysisDb, other: &Self) -> bool {
        match (self, other) {
            (Self::Owned(_), Self::Owned(_)) => true,
            (Self::Access(lhs, _), Self::Access(rhs, _)) => lhs == rhs,
            (Self::Tuple(lhs), Self::Tuple(rhs)) => {
                lhs.len() == rhs.len()
                    && lhs
                        .iter()
                        .zip(rhs)
                        .all(|(lhs, rhs)| lhs.same_modes(db, rhs))
            }
            (
                Self::Sum {
                    variant, payload, ..
                },
                Self::Sum {
                    variant: other_variant,
                    payload: other_payload,
                    ..
                },
            ) => variant == other_variant && payload.same_modes(db, other_payload),
            (Self::Opaque(lhs), Self::Opaque(rhs)) => {
                opaque_mode(db, *lhs) == opaque_mode(db, *rhs)
            }
            _ => false,
        }
    }

    /// Prints a product parameter's shape, whose owned parts are written
    /// `own`: a part without a mode is a `ref` access.
    pub fn pretty_print_product(&self, db: &'db dyn HirAnalysisDb) -> String {
        match self {
            Self::Owned(ty) => format!("own {}", ty.pretty_print(db)),
            Self::Tuple(elems) => format!(
                "({})",
                elems
                    .iter()
                    .map(|elem| elem.pretty_print_product(db))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            _ => self.pretty_print(db),
        }
    }

    pub fn pretty_print(&self, db: &'db dyn HirAnalysisDb) -> String {
        match self {
            Self::Owned(ty) => ty.pretty_print(db).to_string(),
            Self::Access(BorrowKind::Ref, ty) => format!("ref {}", ty.pretty_print(db)),
            Self::Access(BorrowKind::Mut, ty) => format!("mut {}", ty.pretty_print(db)),
            Self::Tuple(elems) => format!(
                "({})",
                elems
                    .iter()
                    .map(|elem| elem.pretty_print(db))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Sum {
                ty, arg, payload, ..
            } => {
                let (base, args) = ty.decompose_ty_app(db);
                let args = args
                    .iter()
                    .enumerate()
                    .map(|(idx, ty_arg)| {
                        if idx == *arg {
                            payload.pretty_print(db)
                        } else {
                            ty_arg.pretty_print(db).to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}<{args}>", base.pretty_print(db))
            }
            Self::Opaque(term) => term.pretty_print(db).to_string(),
        }
    }

    /// The shape with `f` applied to its types; an opaque part's term is
    /// decoded again, so a normalizing `f` makes it known.
    pub fn map_tys(
        &self,
        db: &'db dyn HirAnalysisDb,
        f: &mut impl FnMut(TyId<'db>) -> TyId<'db>,
    ) -> Self {
        match self {
            Self::Owned(ty) => Self::Owned(f(*ty)),
            Self::Access(kind, ty) => Self::Access(*kind, f(*ty)),
            Self::Tuple(elems) => {
                Self::Tuple(elems.iter().map(|elem| elem.map_tys(db, f)).collect())
            }
            Self::Opaque(term) => f(*term).as_shape_part(db),
            Self::Sum {
                ty,
                variant,
                arg,
                payload,
            } => Self::Sum {
                ty: f(*ty),
                variant: *variant,
                arg: *arg,
                payload: Box::new(payload.map_tys(db, f)),
            },
        }
    }
}

impl<'db> TyVisitable<'db> for Shape<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: TyVisitor<'db> + ?Sized,
    {
        match self {
            Self::Owned(ty) | Self::Access(_, ty) | Self::Opaque(ty) => ty.visit_with(visitor),
            Self::Tuple(elems) => elems.iter().for_each(|elem| elem.visit_with(visitor)),
            Self::Sum { ty, payload, .. } => {
                ty.visit_with(visitor);
                payload.visit_with(visitor);
            }
        }
    }
}

impl<'db> TyFoldable<'db> for Shape<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        match self {
            Self::Owned(ty) => Self::Owned(ty.fold_with(db, folder)),
            Self::Access(kind, ty) => Self::Access(kind, ty.fold_with(db, folder)),
            Self::Tuple(elems) => Self::Tuple(
                elems
                    .into_iter()
                    .map(|elem| elem.fold_with(db, folder))
                    .collect(),
            ),
            Self::Sum {
                ty,
                variant,
                arg,
                payload,
            } => Self::Sum {
                ty: ty.fold_with(db, folder),
                variant,
                arg,
                payload: Box::new(payload.fold_with(db, folder)),
            },
            Self::Opaque(term) => term.fold_with(db, folder).as_shape_part(db),
        }
    }
}

/// The mode an opaque shape term is named at: an associated shape's.
fn opaque_mode<'db>(db: &'db dyn HirAnalysisDb, term: TyId<'db>) -> Option<ShapeMode> {
    match term.data(db) {
        TyData::AssocTy(assoc) => assoc.mode,
        _ => None,
    }
}

/// The access `mut?` is in an item used at `mode`. An item without a mode
/// has no `mut?` to resolve; one written there is taken as `ref`.
pub fn maybe_mut_kind(mode: Option<BorrowKind>) -> BorrowKind {
    mode.unwrap_or(BorrowKind::Ref)
}

/// Whether a written type mentions a mode anywhere.
pub fn mentions_mode<'db>(db: &'db dyn HirAnalysisDb, ty: HirTyId<'db>) -> bool {
    let mentions = |ty: &Partial<HirTyId<'db>>| ty.to_opt().is_some_and(|ty| mentions_mode(db, ty));
    match ty.data(db) {
        TypeKind::Mode(..) => true,
        TypeKind::Ptr(inner) | TypeKind::Array(inner, _) => mentions(inner),
        TypeKind::Tuple(elems) => elems.data(db).iter().any(mentions),
        TypeKind::Path(path) => path.to_opt().is_some_and(|path| {
            (0..path.len(db)).any(|idx| {
                path.segment(db, idx).is_some_and(|segment| {
                    segment
                        .generic_args(db)
                        .data(db)
                        .iter()
                        .any(|arg| matches!(arg, GenericArg::Type(arg) if mentions(&arg.ty)))
                })
            })
        }),
        TypeKind::Never => false,
    }
}

/// Lowers a written return type to its shape, with `mut?` and an
/// associated shape named without a mode at `mode`, the mode of the function
/// it returns from.
pub fn lower_return_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    mode: Option<BorrowKind>,
) -> Shape<'db> {
    let lower_opt = |ty: Partial<HirTyId<'db>>| {
        ty.to_opt().map_or_else(
            || TyId::invalid(db, InvalidCause::ParseError),
            |ty| lower_hir_ty(db, ty, scope, assumptions),
        )
    };
    match ty.data(db) {
        TypeKind::Mode(TypeMode::Mut, inner) => Shape::Access(BorrowKind::Mut, lower_opt(*inner)),
        TypeKind::Mode(TypeMode::MaybeMut, inner) => {
            Shape::Access(maybe_mut_kind(mode), lower_opt(*inner))
        }
        TypeKind::Mode(TypeMode::Ref, inner) => Shape::Access(BorrowKind::Ref, lower_opt(*inner)),
        TypeKind::Mode(TypeMode::Own, inner) => Shape::Owned(lower_opt(*inner)),
        // A tuple is a shape if a part is.
        TypeKind::Tuple(elems) => {
            let parts: Vec<_> = elems
                .data(db)
                .iter()
                .map(|elem| match elem.to_opt() {
                    Some(elem) => lower_return_shape(db, elem, scope, assumptions, mode),
                    None => Shape::Owned(TyId::invalid(db, InvalidCause::ParseError)),
                })
                .collect();
            if parts.iter().all(|part| matches!(part, Shape::Owned(_))) {
                Shape::Owned(lower_hir_ty(db, ty, scope, assumptions))
            } else {
                Shape::Tuple(parts)
            }
        }
        TypeKind::Path(Partial::Present(path)) if mentions_mode(db, ty) => {
            sum_shape(db, *path, scope, assumptions, mode)
                .unwrap_or_else(|| Shape::Owned(lower_hir_ty(db, ty, scope, assumptions)))
        }
        _ => {
            let ty = lower_hir_ty(db, ty, scope, assumptions);
            if is_associated_shape(db, ty) {
                Shape::Opaque(shape_at_item_mode(db, ty, mode))
            } else {
                Shape::Owned(ty)
            }
        }
    }
}

/// Whether a written parameter type may name an associated shape: an
/// associated item's path with a mode argument (`D::Step<ref>`), or one in a
/// `mut?` function (`maybe_mut`). Decided from the written type, before the
/// function's constraints, which may need its parameters' types.
pub fn may_name_shape<'db>(db: &'db dyn HirAnalysisDb, ty: HirTyId<'db>, maybe_mut: bool) -> bool {
    let TypeKind::Path(Partial::Present(path)) = ty.data(db) else {
        return false;
    };
    let mode_arg = matches!(
        path.generic_args(db).data(db).as_slice(),
        [GenericArg::Mode(_)]
    );
    path.len(db) >= 2 && (mode_arg || maybe_mut)
}

/// The opaque shape of a parameter written as an associated shape,
/// `_ s: D::Step<ref>`: a value of it, passed on whole, with a shape named
/// without a mode at `mode`, the mode of the function. `None` for any other
/// parameter.
pub fn lower_opaque_param<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    mode: Option<BorrowKind>,
) -> Option<Shape<'db>> {
    let ty = lower_hir_ty(db, ty, scope, assumptions);
    is_associated_shape(db, ty).then(|| Shape::Opaque(shape_at_item_mode(db, ty, mode)))
}

/// Whether `ty` is an associated shape, `D::Step`, whose structure an impl
/// gives.
fn is_associated_shape<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    matches!(ty.data(db), TyData::AssocTy(assoc) if assoc.is_shape(db))
}

/// The shape term `ty` in an item used at `mode`: an associated shape named
/// without a mode takes the item's; without one, it stays without.
fn shape_at_item_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    mode: Option<BorrowKind>,
) -> TyId<'db> {
    match (ty.data(db), mode) {
        (TyData::AssocTy(assoc), Some(kind)) if assoc.mode.is_none() => {
            ty.with_shape_mode(db, ShapeMode::from(kind))
        }
        _ => ty,
    }
}

/// The shape of a written parameter type that is a *product*, a tuple whose
/// parts carry modes (`(own usize, ref T)`), with `mut?` at `mode`, the mode
/// of the function. A part written without a mode is a `ref` access, unless
/// it is a product itself. `None` for any other parameter, which takes its
/// mode whole.
pub fn lower_param_product<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    mode: Option<BorrowKind>,
) -> Option<Shape<'db>> {
    let TypeKind::Tuple(elems) = ty.data(db) else {
        return None;
    };
    if !is_param_product(db, ty) {
        return None;
    }
    // A mode's payload is a value's type, of kind `*`.
    let lower_opt = |ty: Partial<HirTyId<'db>>| {
        ty.to_opt().map_or_else(
            || TyId::invalid(db, InvalidCause::ParseError),
            |ty| {
                let ty = lower_hir_ty(db, ty, scope, assumptions);
                if ty.is_star_kind(db) || ty.has_invalid(db) {
                    ty
                } else {
                    TyId::invalid(db, InvalidCause::NotFullyApplied)
                }
            },
        )
    };
    let part = |elem: HirTyId<'db>| match elem.data(db) {
        TypeKind::Mode(TypeMode::Own, inner) => Shape::Owned(lower_opt(*inner)),
        TypeKind::Mode(TypeMode::Mut, inner) => Shape::Access(BorrowKind::Mut, lower_opt(*inner)),
        TypeKind::Mode(TypeMode::MaybeMut, inner) => {
            Shape::Access(maybe_mut_kind(mode), lower_opt(*inner))
        }
        TypeKind::Mode(TypeMode::Ref, inner) => Shape::Access(BorrowKind::Ref, lower_opt(*inner)),
        _ => lower_param_product(db, elem, scope, assumptions, mode).unwrap_or_else(|| {
            let ty = lower_hir_ty(db, elem, scope, assumptions);
            // A part naming a shape, an associated one or a parameter, is
            // that shape, at the function's mode.
            if is_associated_shape(db, ty) || *ty.kind(db) == Kind::Shape {
                Shape::Opaque(shape_at_item_mode(db, ty, mode))
            } else {
                Shape::Access(BorrowKind::Ref, ty)
            }
        }),
    };
    Some(Shape::Tuple(
        elems
            .data(db)
            .iter()
            .map(|elem| match elem.to_opt() {
                Some(elem) => part(elem),
                None => Shape::Owned(TyId::invalid(db, InvalidCause::ParseError)),
            })
            .collect(),
    ))
}

/// Whether `value` names an associated shape without a mode, where nothing
/// gives it one: outside a `mut?` item's own signature, `D::Step` needs
/// `<ref>` or `<mut>`.
pub(crate) fn names_shape_without_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &impl TyVisitable<'db>,
) -> bool {
    names_assoc_ty(db, value, |assoc| {
        assoc.mode.is_none() && assoc.is_shape(db)
    })
}

/// Whether `value` names an associated type or shape `found` accepts.
pub(crate) fn names_assoc_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &impl TyVisitable<'db>,
    found: impl Fn(&AssocTy<'db>) -> bool,
) -> bool {
    assoc_tys(db, value).iter().any(found)
}

/// The associated types and shapes `value` names, nested ones included.
pub(crate) fn assoc_tys<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &impl TyVisitable<'db>,
) -> Vec<AssocTy<'db>> {
    struct Collector<'db> {
        db: &'db dyn HirAnalysisDb,
        found: Vec<AssocTy<'db>>,
    }
    impl<'db> TyVisitor<'db> for Collector<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_assoc_ty(&mut self, assoc_ty: &AssocTy<'db>) {
            self.found.push(*assoc_ty);
            walk_assoc_ty(self, assoc_ty);
        }
    }
    let mut collector = Collector {
        db,
        found: Vec::new(),
    };
    value.visit_with(&mut collector);
    collector.found
}

/// The parameter list a tuple type written for a shape argument is, when
/// it is one: a product written with its parts' modes (`(own A, B)`), or a
/// tuple naming a shape (`(A, D::Step<ref>)`), whose other parts are views.
/// `None` for a tuple of types alone, which the argument's kind decides.
pub fn lower_shape_list<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<TyId<'db>> {
    let TypeKind::Tuple(elems) = ty.data(db) else {
        return None;
    };
    if is_param_product(db, ty) {
        return Some(
            match lower_param_product(db, ty, scope, assumptions, None) {
                Some(Shape::Tuple(params)) => TyId::params_shape(db, &params),
                _ => TyId::invalid(db, InvalidCause::Other),
            },
        );
    }
    let elems: Vec<_> = elems
        .data(db)
        .iter()
        .map(|elem| {
            elem.to_opt().map_or_else(
                || TyId::invalid(db, InvalidCause::ParseError),
                |elem| lower_hir_ty(db, elem, scope, assumptions),
            )
        })
        .collect();
    elems
        .iter()
        .any(|elem| *elem.kind(db) == Kind::Shape)
        .then(|| {
            let params: Vec<_> = elems
                .into_iter()
                .map(|elem| {
                    if *elem.kind(db) == Kind::Shape {
                        elem
                    } else {
                        TyId::shape_part(db, ShapeCon::Access(BorrowKind::Ref), elem)
                    }
                })
                .collect();
            TyId::params_term(db, &params)
        })
}

/// The shape term an impl gives an associated shape (`shape Step = ..`), or
/// a trait its default: an owned value or an access, a product of parts, or
/// another shape. `mut?` and a shape named without a mode take the mode the
/// value is named at, and a part without a mode is a view.
pub(crate) fn lower_shape_value<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    lowering_cx: &LoweringContext<'db>,
) -> TyId<'db> {
    match ty.data(db) {
        TypeKind::Mode(mode, inner) => {
            let con = match mode {
                TypeMode::Own => ShapeCon::Owned,
                TypeMode::Ref => ShapeCon::Access(BorrowKind::Ref),
                TypeMode::Mut => ShapeCon::Access(BorrowKind::Mut),
                TypeMode::MaybeMut => ShapeCon::MaybeMut,
            };
            let inner = inner.to_opt().map_or_else(
                || TyId::invalid(db, InvalidCause::ParseError),
                |inner| lower_hir_ty_with_minter(db, inner, scope, assumptions, lowering_cx),
            );
            TyId::shape_part(db, con, inner)
        }
        // A product: its parts carry modes, or some of them are shapes,
        // `(D::Part, D::Part)`.
        TypeKind::Tuple(elems)
            if is_param_product(db, ty)
                || elems.data(db).iter().any(|elem| {
                    elem.to_opt().is_some_and(|elem| {
                        let elem =
                            lower_hir_ty_with_minter(db, elem, scope, assumptions, lowering_cx);
                        *elem.kind(db) == Kind::Shape || is_associated_shape(db, elem)
                    })
                }) =>
        {
            let parts: Vec<_> = elems
                .data(db)
                .iter()
                .map(|elem| match elem.to_opt() {
                    Some(elem) => lower_shape_value(db, elem, scope, assumptions, lowering_cx),
                    None => TyId::invalid(db, InvalidCause::ParseError),
                })
                .collect();
            TyId::params_term(db, &parts)
        }
        _ => {
            let ty = lower_hir_ty_with_minter(db, ty, scope, assumptions, lowering_cx);
            match ty.data(db) {
                TyData::AssocTy(assoc) if assoc.mode.is_none() && assoc.is_shape(db) => {
                    ty.with_shape_mode(db, ShapeMode::Inherit)
                }
                _ if *ty.kind(db) == Kind::Shape => ty,
                _ => TyId::shape_part(db, ShapeCon::Access(BorrowKind::Ref), ty),
            }
        }
    }
}

/// Checks a written associated shape value (see [`check_written_ty`]): each
/// part's type is checked as a type, under its mode if it has one.
pub(crate) fn check_shape_value<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
    solve_cx: Option<TraitSolveCx<'db>>,
) -> WrittenTyDiags<'db> {
    match ty.data(db) {
        TypeKind::Mode(_, inner) => inner
            .to_opt()
            .map_or_else(WrittenTyDiags::default, |inner| {
                check_written_ty(
                    db,
                    scope,
                    inner,
                    span.into_mode_type().inner(),
                    assumptions,
                    ConstBodyLowering::Eager,
                    solve_cx,
                )
            }),
        _ => check_param_product(db, ty, scope, span, assumptions, solve_cx),
    }
}

/// Whether a written parameter type is a product: a tuple whose parts carry
/// modes.
pub fn is_param_product<'db>(db: &'db dyn HirAnalysisDb, ty: HirTyId<'db>) -> bool {
    matches!(ty.data(db), TypeKind::Tuple(_)) && mentions_mode(db, ty)
}

/// Checks a written product parameter type (see [`check_written_ty`]): each
/// part's type is checked as a type, under its mode if it has one.
pub(crate) fn check_param_product<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
    solve_cx: Option<TraitSolveCx<'db>>,
) -> WrittenTyDiags<'db> {
    let check = |ty: HirTyId<'db>, span: LazyTySpan<'db>| {
        check_written_ty(
            db,
            scope,
            ty,
            span,
            assumptions,
            ConstBodyLowering::Eager,
            solve_cx,
        )
    };
    let TypeKind::Tuple(elems) = ty.data(db) else {
        return check(ty, span);
    };
    elems
        .data(db)
        .iter()
        .enumerate()
        .filter_map(|(idx, elem)| {
            Some((elem.to_opt()?, span.clone().into_tuple_type().elem_ty(idx)))
        })
        .map(|(elem, span)| match elem.data(db) {
            // A product's parts carry its modes.
            TypeKind::Mode(_, inner)
                if inner
                    .to_opt()
                    .is_some_and(|inner| is_param_product(db, inner)) =>
            {
                WrittenTyDiags {
                    errors: vec![
                        TyLowerDiag::ModeOnProduct {
                            span: span.into(),
                            shape: false,
                        }
                        .into(),
                    ],
                    ill_formed: Vec::new(),
                }
            }
            // A mode's payload is a value's type.
            TypeKind::Mode(_, inner) => inner
                .to_opt()
                .map(|inner| {
                    let span = span.into_mode_type().inner();
                    let checked = check(inner, span.clone());
                    if checked.errors.is_empty()
                        && !lower_hir_ty(db, inner, scope, assumptions).is_star_kind(db)
                    {
                        WrittenTyDiags {
                            errors: vec![TyLowerDiag::ExpectedStarKind(span.into()).into()],
                            ill_formed: Vec::new(),
                        }
                    } else {
                        checked
                    }
                })
                .unwrap_or_default(),
            _ if mentions_mode(db, elem) => {
                check_param_product(db, elem, scope, span, assumptions, solve_cx)
            }
            _ => check(elem, span),
        })
        .collect()
}

/// The sums whose payload variant can carry a shape or be taken by `?`.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum SumKind {
    /// `Option<T>`: `None`, `Some(T)`.
    Option,
    /// `Result<E, T>`: `Ok(T)`, `Err(E)`.
    Result,
}

impl SumKind {
    /// The variant that carries the payload: `Some` or `Ok`. The other
    /// variant is empty or carries the error.
    pub(crate) fn payload_variant(self) -> u16 {
        match self {
            Self::Option => 1,
            Self::Result => 0,
        }
    }

    /// The type argument that is the payload's type: `T` in both.
    pub(crate) fn payload_arg(self) -> usize {
        match self {
            Self::Option => 0,
            Self::Result => 1,
        }
    }
}

/// Whether `ty` is an `Option` or a `Result`.
pub(crate) fn sum_kind<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: TyId<'db>,
) -> Option<SumKind> {
    let is = |lib_path| {
        resolve_lib_type_path(db, scope, lib_path)
            .is_some_and(|lib| lib.base_ty(db) == ty.base_ty(db))
    };
    if is("core::option::Option") {
        Some(SumKind::Option)
    } else if is("core::result::Result") {
        Some(SumKind::Result)
    } else {
        None
    }
}

/// The variant and type argument of a sum type that carries a shape.
fn sum_shape_slot<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Option<(TyId<'db>, u16, usize)> {
    let ctor_hir = HirTyId::new(
        db,
        TypeKind::Path(Partial::Present(path.strip_generic_args(db))),
    );
    let ctor = lower_hir_ty(db, ctor_hir, scope, assumptions);
    let kind = sum_kind(db, scope, ctor)?;
    Some((ctor, kind.payload_variant(), kind.payload_arg()))
}

fn sum_type_args<'db>(db: &'db dyn HirAnalysisDb, path: PathId<'db>) -> Option<Vec<HirTyId<'db>>> {
    path.generic_args(db)
        .data(db)
        .iter()
        .map(|arg| match arg {
            GenericArg::Type(arg) => arg.ty.to_opt(),
            GenericArg::Const(_) | GenericArg::AssocType(_) | GenericArg::Mode(_) => None,
        })
        .collect()
}

fn sum_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    path: PathId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
    mode: Option<BorrowKind>,
) -> Option<Shape<'db>> {
    let (ctor, variant, arg) = sum_shape_slot(db, path, scope, assumptions)?;
    let args = sum_type_args(db, path)?;
    if args.len() != arg + 1 {
        return None;
    }
    let payload = lower_return_shape(db, args[arg], scope, assumptions, mode);
    let ty = args.iter().enumerate().fold(ctor, |acc, (idx, &ty_arg)| {
        let ty_arg = if idx == arg {
            payload.erased_ty(db)
        } else {
            lower_hir_ty(db, ty_arg, scope, assumptions)
        };
        TyId::app(db, acc, ty_arg)
    });
    Some(Shape::Sum {
        ty,
        variant,
        arg,
        payload: Box::new(payload),
    })
}

/// Checks a written return type (see [`check_written_ty`]). Modes are
/// accepted exactly where the shape grammar allows them; every other position
/// is checked as a type.
pub(crate) fn check_return_shape<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    span: LazyTySpan<'db>,
    assumptions: PredicateListId<'db>,
    solve_cx: Option<TraitSolveCx<'db>>,
) -> WrittenTyDiags<'db> {
    let check = |ty: HirTyId<'db>, span: LazyTySpan<'db>| {
        check_written_ty(
            db,
            scope,
            ty,
            span,
            assumptions,
            ConstBodyLowering::Eager,
            solve_cx,
        )
    };
    let shape = |ty: HirTyId<'db>, span: LazyTySpan<'db>| {
        check_return_shape(db, ty, scope, span, assumptions, solve_cx)
    };
    // A tuple is checked part by part: a part may name a shape.
    if !mentions_mode(db, ty) && !matches!(ty.data(db), TypeKind::Tuple(_)) {
        return check(ty, span);
    }
    match ty.data(db) {
        TypeKind::Mode(TypeMode::Mut | TypeMode::MaybeMut | TypeMode::Ref, inner) => inner
            .to_opt()
            .map(|inner| check(inner, span.into_mode_type().inner()))
            .unwrap_or_default(),
        TypeKind::Tuple(elems) => elems
            .data(db)
            .iter()
            .enumerate()
            .filter_map(|(idx, elem)| {
                Some(shape(
                    elem.to_opt()?,
                    span.clone().into_tuple_type().elem_ty(idx),
                ))
            })
            .collect(),
        TypeKind::Path(Partial::Present(path))
            if let Some((_, _, arg)) = sum_shape_slot(db, *path, scope, assumptions)
                && let Some(args) = sum_type_args(db, *path)
                && args.len() == arg + 1 =>
        {
            let segment = path.segment_index(db);
            args.iter()
                .enumerate()
                .map(|(idx, &ty_arg)| {
                    let span = span
                        .clone()
                        .into_path_type()
                        .path()
                        .segment(segment)
                        .generic_args()
                        .arg(idx)
                        .into_type_arg()
                        .ty();
                    if idx == arg {
                        shape(ty_arg, span)
                    } else {
                        check(ty_arg, span)
                    }
                })
                .collect()
        }
        _ => check(ty, span),
    }
}
