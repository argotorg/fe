//! What one operand of a `with` means: a named implementation, a qualified
//! opening of an ordinary implementation, a name that is neither, or a
//! refused operand. A block `with` and a module `with` both ask this one
//! function. They differ only in the names their scope holds: a block also
//! sees its local bindings (`OperandValues`), and the rest of the site is the
//! scope, the bounds and the choices already in force.
//!
//! The function reads no expression and no checker state. It returns the
//! diagnostics an operand earns and leaves the bookkeeping (what the operand
//! provides, which diagnostics are reported, what a refusal leaves open) to
//! its caller.

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        Decision, PathRes, Viewer,
        diagnostics::PathResDiag,
        path_resolver::{ImplNameRes, resolve_impl_name},
        resolve_path,
    },
    ty::{
        binder::Binder,
        const_ty::LoweringContext,
        diagnostics::{BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection},
        normalize::normalize_ty,
        trait_def::TraitInstId,
        trait_resolution::{
            ChosenImpl, ChosenImpls, PredicateListId, Selection, TraitSolveCx,
            generation::provider_hints,
        },
        ty_def::{BorrowKind, TyData, TyId},
    },
};
use crate::hir_def::{GenericParamOwner, PathKind};
use crate::span::DynLazySpan;
use crate::{hir_def::IdentId, hir_def::PathId, hir_def::scope_graph::ScopeId};

/// The kind of place an operand is written in. The classifier answers the same
/// question at every site; the kind says which classes the site can use.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum SiteKind {
    /// An operand of a `with` block: providers and values are meaningful, and
    /// local bindings are in scope.
    Block,
    /// An operand of a module's `with` declaration: implementations only.
    Module,
}

/// Where an operand is written: what its names resolve in and what is already
/// chosen around it.
pub(super) struct OperandSite<'db> {
    pub(super) kind: SiteKind,
    pub(super) scope: ScopeId<'db>,
    /// The bounds the operand's names are judged under.
    pub(super) assumptions: PredicateListId<'db>,
    /// The bounds the names resolve under before the body's instantiation.
    pub(super) written_assumptions: PredicateListId<'db>,
    /// Who reads the operand's names: the package that wrote it.
    pub(super) viewer: Viewer<'db>,
    /// The choices in force where the operand is read, earlier operands of
    /// the same list included.
    pub(super) outer: Option<ChosenImpls<'db>>,
    /// The operand, for the diagnostics about it.
    pub(super) span: DynLazySpan<'db>,
    /// The last segment of the operand's path, for the diagnostics about the
    /// name.
    pub(super) name_span: DynLazySpan<'db>,
}

impl<'db> OperandSite<'db> {
    fn solve_cx(&self, db: &'db dyn HirAnalysisDb) -> TraitSolveCx<'db> {
        TraitSolveCx::new(db, self.scope, crate::analysis::name_resolution::Tier::All)
            .with_assumptions(self.assumptions)
            .with_choices(self.outer)
    }
}

/// What a bare name resolves to where an operand is written.
pub(super) enum BareName<'db> {
    /// A local binding, with the span of its definition.
    Binding(DynLazySpan<'db>),
    /// An item or an imported name.
    Item(PathRes<'db>),
    /// The name is ambiguous or conflicts.
    Diag(FuncBodyDiag<'db>),
    /// Nothing is named so: an expression would bind it as a new variable.
    Unbound,
    /// The name does not resolve to anything usable.
    Invalid,
}

/// The names that only the writer's scope can answer: a block's local
/// bindings, and the instantiation of a default checked for an
/// implementation. A module sees items only.
pub(super) trait OperandValues<'db> {
    /// What the bare name `path` resolves to as an expression.
    fn bare_name(&self, path: PathId<'db>, span: DynLazySpan<'db>) -> BareName<'db>;
    /// Whether a local binding hides a type named `path`, as it does in any
    /// expression.
    fn shadows_type(&self, path: PathId<'db>) -> bool;
    fn instantiate_res(&self, res: PathRes<'db>) -> PathRes<'db>;
    fn instantiate_ty(&self, ty: TyId<'db>) -> TyId<'db>;
}

/// What an operand that names no implementation is.
pub(super) enum OperandClass<'db> {
    /// A type that a derive provider may serve, with the alias through which
    /// it was named, if any. Only a site that opens providers has this class.
    ProviderType(TyId<'db>, Option<ScopeId<'db>>),
    /// A value: a local binding, a function or a constant, which a block
    /// provides as an effect.
    Value,
    /// A name that is neither: a trait, a module, a type no provider serves,
    /// or a name that resolves to nothing.
    Other,
}

/// The meaning of an operand.
pub(super) enum OperandMeaning<'db> {
    /// A named implementation, pushed on the choices in force.
    Named { choice: ChosenImpls<'db> },
    /// An ordinary implementation opened by `<Type as Trait>`, pushed on the
    /// choices in force. Its conditions are proved once the whole list is
    /// read.
    Opening {
        choice: ChosenImpls<'db>,
        conditions: Vec<TraitInstId<'db>>,
    },
    /// Not an implementation: what the name is instead.
    NotAnImplementation(OperandClass<'db>),
    /// An operand that cannot stand: it contributes nothing. With
    /// `changed_choice`, `diags` is the one diagnostic of a generated
    /// implementation that written code did not choose, reported only for
    /// written code. With `leaves_open`, the meaning of the whole `with` is
    /// left open.
    Refused {
        diags: Vec<FuncBodyDiag<'db>>,
        changed_choice: bool,
        leaves_open: bool,
    },
}

impl<'db> OperandMeaning<'db> {
    /// The choices that an implementation operand pushed, with the conditions
    /// of an opening; any other meaning is returned.
    pub(super) fn admitted(self) -> Result<(ChosenImpls<'db>, Vec<TraitInstId<'db>>), Self> {
        match self {
            Self::Named { choice } => Ok((choice, Vec::new())),
            Self::Opening { choice, conditions } => Ok((choice, conditions)),
            other => Err(other),
        }
    }
}

fn refused<'db>(diag: impl Into<FuncBodyDiag<'db>>, leaves_open: bool) -> OperandMeaning<'db> {
    OperandMeaning::Refused {
        diags: vec![diag.into()],
        changed_choice: false,
        leaves_open,
    }
}

/// Whether `value`, what a path names, is a value that a `with` operand can
/// provide, rather than a type, trait, module or record variant.
pub(super) fn is_with_operand_value<'db>(db: &'db dyn HirAnalysisDb, value: &PathRes<'db>) -> bool {
    use crate::hir_def::VariantKind;
    match value {
        PathRes::Ty(ty) | PathRes::TyAlias(_, ty)
            if ty.const_ty_ty(db).is_none()
                && (!matches!(value, PathRes::Ty(_)) || ty.tuple_struct_ctor_ty(db).is_none()) =>
        {
            false
        }
        PathRes::Trait(_) | PathRes::Mod(_) => false,
        PathRes::EnumVariant(variant) if matches!(variant.kind(db), VariantKind::Record(_)) => {
            false
        }
        _ => true,
    }
}

/// The value that the operand `path` names here: a local binding or a visible
/// value item. The span is the one of its definition, when it has one.
fn operand_value<'db>(
    db: &'db dyn HirAnalysisDb,
    site: &OperandSite<'db>,
    values: &dyn OperandValues<'db>,
    path: PathId<'db>,
) -> Option<Option<DynLazySpan<'db>>> {
    if path.as_ident(db).is_some() {
        match values.bare_name(path, site.span.clone()) {
            BareName::Binding(span) => Some(Some(span)),
            BareName::Item(value) if is_with_operand_value(db, &value) => Some(value.name_span(db)),
            _ => None,
        }
    } else {
        match resolve_path(db, path, site.scope, site.assumptions, true) {
            Ok(value)
                if is_with_operand_value(db, &value)
                    && value.is_visible_as_written(db, path, site.viewer) =>
            {
                Some(value.name_span(db))
            }
            _ => None,
        }
    }
}

/// The derive provider type that the operand `path` names here, and the span
/// of its definition: a visible type that a `Derive` implementation can
/// serve.
fn provider_type<'db>(
    db: &'db dyn HirAnalysisDb,
    site: &OperandSite<'db>,
    path: PathId<'db>,
) -> Option<(TyId<'db>, Option<DynLazySpan<'db>>)> {
    let (ty, alias_scope) =
        match resolve_path(db, path, site.scope, site.assumptions, false).ok()? {
            PathRes::Ty(ty) => (ty, None),
            PathRes::TyAlias(alias, ty) => (ty, Some(alias.alias.scope())),
            _ => return None,
        };
    for scope in [alias_scope, ty.as_scope(db)].into_iter().flatten() {
        if !site.viewer.sees(db, scope) {
            return None;
        }
    }
    let ty = normalize_ty(db, ty, site.scope, site.assumptions);
    matches!(
        provider_hints(db, site.scope, site.assumptions, ty),
        Ok(Some(_))
    )
    .then(|| (ty, ty.as_scope(db).and_then(|scope| scope.name_span(db))))
}

/// What the operand `path` means, written at `site`. The caller reads the
/// operand's syntax: only a path that no key accompanies is classified.
pub(super) fn classify_with_operand<'db>(
    db: &'db dyn HirAnalysisDb,
    site: &OperandSite<'db>,
    path: PathId<'db>,
    values: &dyn OperandValues<'db>,
) -> OperandMeaning<'db> {
    if let Some(meaning) = named_implementation(db, site, path, values) {
        return meaning;
    }
    if let Some(meaning) = qualified_opening(db, site, path, values) {
        return meaning;
    }
    provider_or_value(db, site, path, values)
}

/// Two meanings for one operand: a named implementation and another name.
/// A generated implementation only fills gaps: a name that written code binds
/// keeps its meaning, and the implementation is refused where it is requested.
fn two_meanings<'db>(
    db: &'db dyn HirAnalysisDb,
    site: &OperandSite<'db>,
    implementation: crate::hir_def::ImplTrait<'db>,
    name: IdentId<'db>,
    other: Option<DynLazySpan<'db>>,
) -> OperandMeaning<'db> {
    if implementation
        .scope()
        .is_generated_in(db, site.scope.ingot(db))
    {
        return OperandMeaning::Refused {
            diags: vec![
                BodyDiag::GenerationChangesChoice {
                    primary: site.span.clone(),
                    decision: Decision::WithOperand,
                    trait_: None,
                    ty: None,
                    subject: implementation.ty(db).pretty_print(db).to_string(),
                    generated: Some(implementation.scope()),
                }
                .into(),
            ],
            changed_choice: true,
            leaves_open: true,
        };
    }
    let mut candidates = vec![(implementation.span().into(), false)];
    candidates.extend(other.map(|span| (span, false)));
    refused(
        PathResDiag::Ambiguous(site.span.clone(), name, candidates),
        true,
    )
}

/// The operand as a named implementation, when it names one. `None`: the
/// path names no implementation and the other meanings are considered.
fn named_implementation<'db>(
    db: &'db dyn HirAnalysisDb,
    site: &OperandSite<'db>,
    path: PathId<'db>,
    values: &dyn OperandValues<'db>,
) -> Option<OperandMeaning<'db>> {
    let implementation = match resolve_impl_name(db, path, site.scope) {
        ImplNameRes::Found(implementation) => {
            // A bare value and a named implementation are separate meanings,
            // not a priority between namespaces. Resolve the ordinary
            // expression name without checking or evaluating the value
            // before selecting either one.
            if let Some(name) = path.as_ident(db) {
                let value_span = match values.bare_name(path, site.span.clone()) {
                    BareName::Binding(span) => Some(span),
                    BareName::Item(value) => is_with_operand_value(db, &value)
                        .then(|| value.name_span(db))
                        .flatten(),
                    BareName::Diag(diagnostic) => return Some(refused(diagnostic, true)),
                    BareName::Unbound | BareName::Invalid => None,
                };
                if let Some(value_span) = value_span {
                    return Some(two_meanings(
                        db,
                        site,
                        implementation,
                        name,
                        Some(value_span),
                    ));
                }
            } else if let Ok(value) =
                resolve_path(db, path, site.scope, site.written_assumptions, true)
                    .map(|value| values.instantiate_res(value))
                && is_with_operand_value(db, &value)
                && value.is_visible_as_written(db, path, site.viewer)
            {
                // A qualified path is classified like a bare name: a value of
                // the same path is a second meaning, and neither is chosen
                // silently.
                let name = path
                    .ident(db)
                    .to_opt()
                    .expect("implementation path identifier");
                return Some(two_meanings(
                    db,
                    site,
                    implementation,
                    name,
                    value.name_span(db),
                ));
            }
            // A derive provider type of the same name is a third meaning of
            // the operand, and none is chosen silently.
            if let Some((_, provider_span)) = provider_type(db, site, path) {
                let name = path
                    .ident(db)
                    .to_opt()
                    .expect("implementation path identifier");
                return Some(two_meanings(db, site, implementation, name, provider_span));
            }
            implementation
        }
        // An implementation whose name is not visible here is not one of the
        // operand's meanings: a value of the same name that is visible here is
        // checked as any other operand.
        ImplNameRes::NotVisible { .. } if operand_value(db, site, values, path).is_some() => {
            return None;
        }
        ImplNameRes::NotVisible { name, definition } => {
            return Some(refused(
                PathResDiag::Invisible(site.name_span.clone(), name, definition),
                false,
            ));
        }
        // An ambiguous implementation name cannot become an effect operand
        // merely because selection failed.
        ImplNameRes::Ambiguous(candidates) => {
            return Some(refused(
                PathResDiag::Ambiguous(
                    site.name_span.clone(),
                    path.ident(db)
                        .to_opt()
                        .expect("implementation path identifier"),
                    candidates,
                ),
                true,
            ));
        }
        // A bare name that is neither an implementation nor a value is
        // reported once, as a `with` operand.
        ImplNameRes::NotFound
            if path.as_ident(db).is_some()
                && matches!(values.bare_name(path, site.span.clone()), BareName::Unbound) =>
        {
            return Some(refused(
                BodyDiag::UnknownWithOperand {
                    primary: site.span.clone(),
                    name: path.as_ident(db).expect("an identifier path"),
                },
                false,
            ));
        }
        ImplNameRes::NotFound => return None,
    };
    let implementation = crate::analysis::ty::trait_lower::lower_impl_trait(db, implementation)?;
    let written_args = path.generic_args(db);
    let choice = if written_args.is_empty(db) {
        ChosenImpls::push(db, site.outer, implementation)
    } else {
        let args: Vec<_> = crate::analysis::ty::ty_lower::lower_generic_arg_list(
            db,
            written_args,
            site.scope,
            site.written_assumptions,
            &LoweringContext::new(),
        )
        .into_iter()
        .map(|ty| values.instantiate_ty(ty))
        .collect();
        let owner = implementation.hir_impl_trait(db);
        let inherited = GenericParamOwner::from(owner)
            .parent(db)
            .map(|parent| {
                crate::analysis::ty::ty_lower::collect_generic_params(db, parent)
                    .params(db)
                    .to_vec()
            })
            .unwrap_or_default();
        let written = args.len();
        let args = match crate::analysis::ty::ty_lower::collect_generic_params(db, owner.into())
            .complete_args(
                db,
                &inherited,
                &args,
                crate::analysis::ty::generic_defaults::DefaultApplication::StructuralMetadata(
                    &LoweringContext::new(),
                ),
            ) {
            Ok(args) => args,
            Err(error) => {
                let diag = crate::analysis::ty::ty_error::emit_invalid_ty_error(
                    db,
                    TyId::invalid(db, error.cause),
                    site.span.clone(),
                );
                return Some(OperandMeaning::Refused {
                    diags: diag.into_iter().map(Into::into).collect(),
                    changed_choice: false,
                    leaves_open: false,
                });
            }
        };
        let expected = implementation.params(db).len() - inherited.len();
        if args.len() != expected {
            // Completion refuses too many arguments, so these are too few,
            // and no choice can stand for the operand: the block's meaning is
            // left open.
            return Some(refused(
                BodyDiag::TooFewWithGenericArgs {
                    primary: site.span.clone(),
                    def_span: owner.span().selection_name().into(),
                    given: written,
                    expected,
                },
                true,
            ));
        }
        let args = inherited.into_iter().chain(args).collect::<Vec<_>>();
        let applied = Binder::bind(owner.into(), implementation).instantiate(db, &args);
        ChosenImpls::push_applied(db, site.outer, implementation, applied)
    };
    Some(OperandMeaning::Named { choice })
}

/// The operand `<Type as Trait>`, an ordinary implementation opened by name
/// of its goal.
fn qualified_opening<'db>(
    db: &'db dyn HirAnalysisDb,
    site: &OperandSite<'db>,
    path: PathId<'db>,
    values: &dyn OperandValues<'db>,
) -> Option<OperandMeaning<'db>> {
    if path.len(db) != 1 || !matches!(path.kind(db), PathKind::QualifiedType { .. }) {
        return None;
    }
    let Ok(PathRes::Ty(ty)) = resolve_path(db, path, site.scope, site.written_assumptions, false)
        .map(|reso| values.instantiate_res(reso))
    else {
        return None;
    };
    let TyData::QualifiedTy(goal) = ty.data(db) else {
        return None;
    };
    match crate::analysis::ty::trait_def::resolve_ordinary_trait_impl_instance(
        db,
        site.solve_cx(db),
        *goal,
    ) {
        Selection::Unique(resolved) => {
            let choice = ChosenImpls::push_applied(
                db,
                site.outer,
                resolved.selected(),
                resolved.instantiated(),
            );
            // The opened implementation's conditions are proved once the
            // whole list is read.
            let mode = Some(if goal.is_mut(db) {
                BorrowKind::Mut
            } else {
                BorrowKind::Ref
            });
            let conditions = resolved
                .instantiated_constraints(db)
                .list(db)
                .iter()
                .map(|&condition| condition.at_mode(db, mode))
                .collect::<Vec<_>>();
            Some(OperandMeaning::Opening { choice, conditions })
        }
        Selection::Ambiguous(_) | Selection::NotFound | Selection::NeedsGeneration(_) => {
            Some(refused(
                TyDiagCollection::from(TraitConstraintDiag::TraitBoundNotSat {
                    span: site.span.clone(),
                    primary_goal: *goal,
                    unsat_subgoal: None,
                    required_by: None,
                }),
                false,
            ))
        }
    }
}

/// An operand that names no implementation: a type is a derive provider
/// candidate unless a value of the same name makes it ambiguous.
fn provider_or_value<'db>(
    db: &'db dyn HirAnalysisDb,
    site: &OperandSite<'db>,
    path: PathId<'db>,
    values: &dyn OperandValues<'db>,
) -> OperandMeaning<'db> {
    let none = OperandMeaning::NotAnImplementation(OperandClass::Other);
    let Ok(resolved) = resolve_path(db, path, site.scope, site.assumptions, false) else {
        return none;
    };
    let (provider_ty, alias_scope) = match resolved {
        PathRes::Ty(ty) => (ty, None),
        PathRes::TyAlias(alias, ty) => (ty, Some(alias.alias.scope())),
        _ => return none,
    };
    // A local binding shadows a type of the same name, as it does in any
    // expression. Otherwise an operand that names a value item and a derive
    // provider type has two meanings, and none is chosen silently. A value
    // alone is checked as any other operand.
    if values.shadows_type(path) {
        return none;
    }
    match operand_value(db, site, values, path) {
        Some(value_span) => {
            if let Some((_, provider_span)) = provider_type(db, site, path) {
                let name = path.ident(db).to_opt().expect("provider path identifier");
                let mut candidates = Vec::new();
                candidates.extend(provider_span.map(|span| (span, false)));
                candidates.extend(value_span.map(|span| (span, false)));
                return refused(
                    PathResDiag::Ambiguous(site.span.clone(), name, candidates),
                    true,
                );
            }
            OperandMeaning::NotAnImplementation(OperandClass::Value)
        }
        None if site.kind == SiteKind::Block => OperandMeaning::NotAnImplementation(
            OperandClass::ProviderType(provider_ty, alias_scope),
        ),
        None => none,
    }
}

/// An earlier operand of a list that answers the same goal as a later one: the
/// earlier operand's span, the goal, and whether the overlap is only possible
/// because some projections remain unknown.
pub(super) type Duplicate<'db> = (DynLazySpan<'db>, TraitInstId<'db>, bool);

/// The operands of one `with` list that were admitted, in order. A block and a
/// module read a list the same way: an operand that answers the goal of an
/// earlier one is refused (8-0160), and the conditions of an opened
/// implementation are proved once the whole list is read.
#[derive(Default)]
pub(super) struct OperandList<'db> {
    /// Each admitted operand's implementation, with its span: the enclosing
    /// frame is never compared.
    listed: Vec<(ChosenImpl<'db>, DynLazySpan<'db>)>,
    /// The conditions of each opening, with the opening's position in the
    /// choices and its operand's span.
    openings: Vec<(usize, Vec<TraitInstId<'db>>, DynLazySpan<'db>)>,
}

impl<'db> OperandList<'db> {
    pub(super) fn new() -> Self {
        Self::default()
    }

    /// Records an operand admitted before the one being read.
    pub(super) fn earlier(&mut self, entry: ChosenImpl<'db>, span: DynLazySpan<'db>) {
        self.listed.push((entry, span));
    }

    /// Admits `entry`, the implementation of the operand at `span`, whose
    /// position in the choices is `at`, unless an earlier operand answers the
    /// same goal under `cx`.
    pub(super) fn admit(
        &mut self,
        db: &'db dyn HirAnalysisDb,
        cx: TraitSolveCx<'db>,
        entry: ChosenImpl<'db>,
        at: usize,
        conditions: Vec<TraitInstId<'db>>,
        span: DynLazySpan<'db>,
    ) -> Result<(), Duplicate<'db>> {
        if let Some(duplicate) = self.listed.iter().find_map(|(earlier, earlier_span)| {
            crate::analysis::ty::trait_resolution::common_choice_goal(db, cx, *earlier, entry)
                .map(|(goal, possible)| (earlier_span.clone(), goal, possible))
        }) {
            return Err(duplicate);
        }
        self.opening(at, conditions, span.clone());
        self.listed.push((entry, span));
        Ok(())
    }

    /// Records the conditions of the opening at position `at`.
    pub(super) fn opening(
        &mut self,
        at: usize,
        conditions: Vec<TraitInstId<'db>>,
        span: DynLazySpan<'db>,
    ) {
        if !conditions.is_empty() {
            self.openings.push((at, conditions, span));
        }
    }

    /// The goals that the openings owe, each with the choices it is proved
    /// under and the opening's operand: `choices`, the whole list, but the
    /// opening itself, whatever the order of the operands. An opening whose
    /// conditions can never hold is refused whether or not the body uses it.
    pub(super) fn obligations(
        self,
        db: &'db dyn HirAnalysisDb,
        choices: ChosenImpls<'db>,
    ) -> Vec<(TraitInstId<'db>, Option<ChosenImpls<'db>>, DynLazySpan<'db>)> {
        let mut goals = Vec::new();
        for (index, conditions, span) in self.openings {
            let context = choices.retain(db, |peer, _| peer != index);
            for goal in conditions {
                goals.push((goal, context, span.clone()));
            }
        }
        goals
    }
}
