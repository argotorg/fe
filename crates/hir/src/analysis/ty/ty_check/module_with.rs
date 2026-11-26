//! A module's `with` declaration: the outermost choice frame of every body
//! written in the module and its child modules. Its operands go through the
//! operand classifier that a block `with` uses (`with_operand`), at the
//! module's scope, so a name means the same in both forms. A module adds no
//! values or providers: those are written in a block.
//!
//! The frame is built from names and deferred lowering only (`module_choices`).
//! Each operand is an entry that knows the trait it can answer. Its
//! implementation is chosen by `declared_choice` on first use, and only for a
//! goal of that trait, so no value checked under a frame can feed the frame.
//! The diagnostics of the declaration are `module_with_check`, which nothing
//! else reads.

use salsa::SalsaValue;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{
            ExpectedPathKind, PathRes, Tier, Viewer,
            diagnostics::PathResDiag,
            path_resolver::{
                ImplNameRes, PathResErrorKind, resolve_impl_name, resolve_path,
                resolve_path_with_minter,
            },
        },
        ty::{
            const_ty::LoweringContext,
            diagnostics::{
                BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
            },
            trait_def::TraitInstId,
            trait_lower::lower_impl_trait_header,
            trait_resolution::{
                ChoiceEntry, ChosenImpl, ChosenImpls, DeclaredOperand, PredicateListId,
                TraitSolveCx, goal_holds,
            },
            ty_def::{TyData, TyId},
        },
    },
    hir_def::{ItemKind, PathId, PathKind, Trait, WithItem, scope_graph::ScopeId},
    span::{DynLazySpan, path::LazyPathSpan},
};

use super::BodyOwner;
use super::with_operand::{
    BareName, OperandClass, OperandList, OperandMeaning, OperandSite, OperandValues, SiteKind,
    classify_with_operand, type_kind_name,
};

/// The frame of a module: its parent's, extended by its own declaration.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub(crate) struct ModuleChoices<'db> {
    pub choices: Option<ChosenImpls<'db>>,
}

#[salsa::interned]
struct ModuleKey<'db> {
    #[returns(copy)]
    module: ItemKind<'db>,
}

fn module_key<'db>(db: &'db dyn HirAnalysisDb, module: ScopeId<'db>) -> ModuleKey<'db> {
    let ScopeId::Item(item @ (ItemKind::Mod(_) | ItemKind::TopMod(_))) = module else {
        unreachable!("a module scope is the scope of a module item");
    };
    ModuleKey::new(db, item)
}

/// The frame of `module`, a module scope: the choices of the `with`
/// declarations of the module and of its parent modules, outermost first.
pub(crate) fn module_choices<'db>(
    db: &'db dyn HirAnalysisDb,
    module: ScopeId<'db>,
) -> &'db ModuleChoices<'db> {
    module_choices_query(db, module_key(db, module))
}

/// The module's `with` declaration, if it has one.
fn declaration_of<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Option<WithItem<'db>> {
    scope.child_items(db).find_map(|child| match child {
        ItemKind::With(declaration) => Some(declaration),
        _ => None,
    })
}

/// The names a module's operand sees: items only.
struct ModuleOperands<'db> {
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path_span: LazyPathSpan<'db>,
}

impl<'db> OperandValues<'db> for ModuleOperands<'db> {
    fn bare_name(&self, path: PathId<'db>, _span: DynLazySpan<'db>) -> BareName<'db> {
        match resolve_path(
            self.db,
            path,
            self.scope,
            PredicateListId::empty_list(self.db),
            true,
        ) {
            Ok(res) => BareName::Item(res),
            Err(error) => match &error.kind {
                PathResErrorKind::NotFound { .. } => BareName::Unbound,
                PathResErrorKind::Ambiguous(_) => error
                    .into_diag(
                        self.db,
                        path,
                        self.path_span.clone(),
                        ExpectedPathKind::Value,
                    )
                    .map_or(BareName::Invalid, |diag| BareName::Diag(diag.into())),
                _ => BareName::Invalid,
            },
        }
    }

    fn shadows_type(&self, _path: PathId<'db>) -> bool {
        false
    }

    fn instantiate_res(&self, res: PathRes<'db>) -> PathRes<'db> {
        res
    }

    fn instantiate_ty(&self, ty: TyId<'db>) -> TyId<'db> {
        ty
    }
}

/// The trait that the operand `path` can answer, known from names and deferred
/// lowering alone: a named implementation's trait, or the trait of an opening
/// `<Type as Trait>`. `None`: the operand names neither, and the declaration
/// reports it (`module_with_check`).
fn operand_trait<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: PathId<'db>,
) -> Option<Trait<'db>> {
    if let ImplNameRes::Found(implementation) = resolve_impl_name(db, path, scope) {
        return lower_impl_trait_header(db, implementation)
            .map(|header| header.trait_inst(db).def(db));
    }
    if path.len(db) != 1 || !matches!(path.kind(db), PathKind::QualifiedType { .. }) {
        return None;
    }
    let Ok(PathRes::Ty(ty)) = resolve_path_with_minter(
        db,
        path,
        scope,
        PredicateListId::empty_list(db),
        false,
        &LoweringContext::deferred(),
    ) else {
        return None;
    };
    match ty.data(db) {
        TyData::QualifiedTy(goal) => Some(goal.def(db)),
        _ => None,
    }
}

/// The frame of a module is built from names and deferred lowering only. It
/// never evaluates a constant, searches for an implementation or checks a
/// body: no value checked under a frame can feed the frame itself.
#[salsa::tracked(returns(ref))]
fn module_choices_query<'db>(
    db: &'db dyn HirAnalysisDb,
    key: ModuleKey<'db>,
) -> ModuleChoices<'db> {
    let scope = ScopeId::from_item(key.module(db));
    let outer = scope
        .parent_module(db)
        .and_then(|parent| module_choices(db, parent).choices);
    let Some(declaration) = declaration_of(db, scope) else {
        return ModuleChoices { choices: outer };
    };
    let mut current = outer;
    for (index, operand) in declaration.operands(db).iter().enumerate() {
        // An operand that the parser dropped, or that names no implementation,
        // adds no entry: `module_with_check` reports it.
        let Some(trait_) = operand
            .to_opt()
            .and_then(|path| operand_trait(db, scope, path))
        else {
            continue;
        };
        let operand = DeclaredOperand::new(db, declaration, index as u32, trait_);
        current = Some(ChosenImpls::push_declared(db, current, operand));
    }
    ModuleChoices { choices: current }
}

/// What a declared operand chose, when a goal of its trait asked.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub(crate) struct DeclaredChoice<'db> {
    /// `None`: the operand contributes nothing, because it is refused or
    /// repeats an earlier operand's answer.
    pub choice: Option<ChosenImpl<'db>>,
    /// An opening's conditions, proved by `module_with_check`.
    pub conditions: Vec<TraitInstId<'db>>,
    /// What the classifier reported for the operand.
    pub diags: Vec<FuncBodyDiag<'db>>,
    /// The earlier operand that answers the same goal (8-0160), with the
    /// goal and whether the overlap is only possible.
    pub duplicate_of: Option<(DynLazySpan<'db>, TraitInstId<'db>, bool)>,
}

/// The choices of `frame` that come before operand `index` of `with`.
fn frame_before<'db>(
    db: &'db dyn HirAnalysisDb,
    frame: Option<ChosenImpls<'db>>,
    with: WithItem<'db>,
    index: usize,
) -> Option<ChosenImpls<'db>> {
    frame.and_then(|frame| {
        frame.retain(db, |_, entry| {
            !matches!(entry, ChoiceEntry::Declared(operand)
                if operand.with(db) == with && operand.index(db) as usize >= index)
        })
    })
}

/// What operand `index` of `with`, whose path is `path`, means where the
/// module writes it, with `outer` already chosen.
fn classify_operand<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    with: WithItem<'db>,
    index: usize,
    path: PathId<'db>,
    outer: Option<ChosenImpls<'db>>,
) -> OperandMeaning<'db> {
    let operand_span = with.span().operands().operand(index);
    let site = OperandSite {
        scope,
        assumptions: PredicateListId::empty_list(db),
        written_assumptions: PredicateListId::empty_list(db),
        viewer: Viewer::written_in(db, with.scope()),
        outer,
        span: operand_span.clone().into(),
        name_span: operand_span
            .clone()
            .segment(path.segment_index(db))
            .ident()
            .into(),
    };
    let values = ModuleOperands {
        db,
        scope,
        path_span: operand_span,
    };
    classify_with_operand(db, &site, path, &values)
}

/// What a refused operand reports. An operand that is admitted reports
/// nothing here.
fn refusal_diags<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    with: WithItem<'db>,
    index: usize,
    path: PathId<'db>,
    meaning: OperandMeaning<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let operand_span = with.span().operands().operand(index);
    match meaning {
        OperandMeaning::Named { .. } | OperandMeaning::Opening { .. } => Vec::new(),
        // A refused operand contributes nothing; the rest of the declaration
        // is still read. A generated implementation changes what written code
        // chose; a declaration that generated code wrote changes nothing.
        OperandMeaning::Refused {
            diags,
            changed_choice,
            ..
        } => {
            if !changed_choice || !scope.is_within_generated(db, scope.ingot(db)) {
                diags
            } else {
                Vec::new()
            }
        }
        OperandMeaning::NotAnImplementation(class) => {
            if SiteKind::Module.accepts(&class) {
                return Vec::new();
            }
            let name = path.ident(db).to_opt();
            let kind = match &class {
                OperandClass::ProviderType(ty, alias) => {
                    Some(match type_kind_name(db, scope, *ty) {
                        "type" if alias.is_some() => "type alias",
                        kind => kind,
                    })
                }
                OperandClass::Other(Some(res)) => Some(res.kind_name()),
                // A value, or a name that resolved to nothing as a type: it
                // is named as an expression would name it.
                OperandClass::Value | OperandClass::Other(None) => None,
            };
            if let Some(kind) = kind {
                // It names something, and it is not an implementation.
                return name
                    .map(|name| PathResDiag::ExpectedImpl(operand_span.into(), name, kind).into())
                    .into_iter()
                    .collect();
            }
            let empty = PredicateListId::empty_list(db);
            match resolve_path(db, path, scope, empty, true)
                .or_else(|_| resolve_path(db, path, scope, empty, false))
            {
                Ok(res) => name
                    .map(|name| {
                        PathResDiag::ExpectedImpl(operand_span.into(), name, res.kind_name()).into()
                    })
                    .into_iter()
                    .collect(),
                // It names nothing: the path is reported as it is anywhere.
                Err(error) => error
                    .into_diag(db, path, operand_span, ExpectedPathKind::Value)
                    .map(Into::into)
                    .into_iter()
                    .collect(),
            }
        }
    }
}

/// The implementation operand `operand` chooses. It is read only when a goal
/// of the operand's trait asks (the solver, the enumerations that name a
/// trait, `module_with_check`), never while a frame is built.
#[salsa::tracked(returns(ref), cycle_result = declared_choice_cycle)]
pub(crate) fn declared_choice<'db>(
    db: &'db dyn HirAnalysisDb,
    operand: DeclaredOperand<'db>,
) -> DeclaredChoice<'db> {
    let with = operand.with(db);
    let index = operand.index(db) as usize;
    let scope = module_of(db, with.scope());
    let frame = module_choices(db, scope).choices;
    let outer = frame_before(db, frame, with, index);
    let path = with.operands(db)[index]
        .to_opt()
        .expect("an operand with an entry has a path");
    let none = |diags| DeclaredChoice {
        choice: None,
        conditions: Vec::new(),
        diags,
        duplicate_of: None,
    };
    let (choice, conditions) =
        match classify_operand(db, scope, with, index, path, outer).admitted() {
            Ok(admitted) => admitted,
            Err(refused) => return none(refusal_diags(db, scope, with, index, path, refused)),
        };
    let entry = choice
        .items(db)
        .last()
        .and_then(|entry| entry.implementation())
        .expect("a pushed choice is the last item");

    // An operand that answers the same goal as an earlier one is not chosen.
    let mut list = OperandList::new();
    for earlier in frame.into_iter().flat_map(|frame| frame.items(db).iter()) {
        if let ChoiceEntry::Declared(earlier) = earlier
            && earlier.with(db) == with
            && (earlier.index(db) as usize) < index
            && let Some(earlier_choice) = declared_choice(db, *earlier).choice
        {
            let span = with.span().operands().operand(earlier.index(db) as usize);
            list.earlier(earlier_choice, span.into());
        }
    }
    let cx = TraitSolveCx::new(db, scope, Tier::All).with_choices(outer);
    let span = with.span().operands().operand(index).into();
    if let Err(duplicate) = list.admit(db, cx, entry, 0, conditions.clone(), span) {
        return DeclaredChoice {
            duplicate_of: Some(duplicate),
            ..none(Vec::new())
        };
    }
    DeclaredChoice {
        choice: Some(entry),
        conditions,
        diags: Vec::new(),
        duplicate_of: None,
    }
}

// A declaration whose operand needs a value that the operand itself decides
// is refused where the cycle meets it, never iterated.
fn declared_choice_cycle<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    operand: DeclaredOperand<'db>,
) -> DeclaredChoice<'db> {
    let span = operand
        .with(db)
        .span()
        .operands()
        .operand(operand.index(db) as usize);
    DeclaredChoice {
        choice: None,
        conditions: Vec::new(),
        diags: vec![FuncBodyDiag::Ty(
            TyLowerDiag::TypeLoweringCycle(span.into()).into(),
        )],
        duplicate_of: None,
    }
}

/// The diagnostics of the module's own `with` declaration: what each refused
/// operand reports, the operands that repeat an earlier answer, and the
/// conditions of the openings. Only `ModuleWithAnalysisPass` reads it.
pub(crate) fn module_with_check<'db>(
    db: &'db dyn HirAnalysisDb,
    module: ScopeId<'db>,
) -> &'db Vec<FuncBodyDiag<'db>> {
    module_with_check_query(db, module_key(db, module))
}

#[salsa::tracked(returns(ref))]
fn module_with_check_query<'db>(
    db: &'db dyn HirAnalysisDb,
    key: ModuleKey<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let scope = ScopeId::from_item(key.module(db));
    let Some(with) = declaration_of(db, scope) else {
        return Vec::new();
    };
    let frame = module_choices(db, scope).choices;
    let mut diags: Vec<FuncBodyDiag<'db>> = Vec::new();
    let mut list = OperandList::new();

    for (index, operand) in with.operands(db).iter().enumerate() {
        let Some(path) = operand.to_opt() else {
            // The parser reported it.
            continue;
        };
        let span: DynLazySpan<'db> = with.span().operands().operand(index).into();
        let entry = frame.and_then(|frame| {
            frame
                .items(db)
                .iter()
                .enumerate()
                .find_map(|(position, entry)| match entry {
                    ChoiceEntry::Declared(operand)
                        if operand.with(db) == with && operand.index(db) as usize == index =>
                    {
                        Some((position, *operand))
                    }
                    _ => None,
                })
        });
        let Some((position, operand)) = entry else {
            // No entry: the operand names no implementation, and is
            // classified as a block would.
            let outer = frame_before(db, frame, with, index);
            let meaning = classify_operand(db, scope, with, index, path, outer);
            diags.extend(refusal_diags(db, scope, with, index, path, meaning));
            continue;
        };
        let chosen = declared_choice(db, operand);
        diags.extend(chosen.diags.iter().cloned());
        if let Some((first, goal, possible)) = &chosen.duplicate_of {
            diags.push(
                BodyDiag::DuplicateWithChoice {
                    primary: span.clone(),
                    first: first.clone(),
                    goal: *goal,
                    possible: *possible,
                }
                .into(),
            );
        }
        if chosen.choice.is_some() {
            list.opening(position, chosen.conditions.clone(), span);
        }
    }

    // An opening's conditions are proved once the whole list is read, with the
    // outer frame and every other operand of the declaration, without the
    // opening itself, whatever the order of the operands.
    if let Some(list_choices) = frame {
        for (goal, context, span) in list.obligations(db, list_choices) {
            let cx = TraitSolveCx::new(db, scope, Tier::All).with_choices(context);
            if !goal_holds(db, cx, goal) {
                diags.push(
                    TyDiagCollection::from(TraitConstraintDiag::TraitBoundNotSat {
                        span,
                        primary_goal: goal,
                        unsat_subgoal: None,
                        required_by: None,
                    })
                    .into(),
                );
            }
        }
    }
    diags
}

/// The module that `scope` is in: `scope` itself for a module.
pub(crate) fn module_of<'db>(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> ScopeId<'db> {
    match scope {
        ScopeId::Item(ItemKind::Mod(_) | ItemKind::TopMod(_)) => scope,
        _ => scope
            .parent_module(db)
            .expect("every scope is inside a module"),
    }
}

/// The choices that a check or generated call at this scope starts from: the
/// frame of the module that holds it.
pub(crate) fn scope_choices<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
) -> Option<ChosenImpls<'db>> {
    module_choices(db, module_of(db, scope)).choices
}

/// Whether `frame` has an entry that could answer a goal of `trait_`: an
/// implementation of it, or a provider that may offer it.
pub(crate) fn frame_may_answer<'db>(
    db: &'db dyn HirAnalysisDb,
    frame: Option<ChosenImpls<'db>>,
    trait_: Trait<'db>,
) -> bool {
    frame.is_some_and(|frame| {
        frame.items(db).iter().any(|entry| match entry {
            ChoiceEntry::Impl(choice) => choice.implementation().trait_(db).def(db) == trait_,
            ChoiceEntry::Provider(opening) => opening.may_offer(db, trait_),
            ChoiceEntry::Declared(operand) => operand.trait_(db) == trait_,
        })
    })
}

/// The frame a body starts with: the frame of the module that writes it. An
/// anonymous constant starts with it when it is a value of its module: a
/// static assertion's condition, the initializer of an associated constant (a
/// trait's default included), the value of a constant, or a `where` condition
/// on a function. The default of a trait's constant belongs to the module of
/// the trait. One written inside a type (a const argument, an array length, a
/// parameter default) or in the `where` clause of a type, an implementation or
/// a trait is a part of the type, which is never decided by a choice.
pub(crate) fn body_frame<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<ChosenImpls<'db>> {
    match owner {
        BodyOwner::AnonConstBody { body, .. } => {
            let value_of_module = matches!(
                body.scope().parent_item(db),
                Some(ItemKind::StaticAssert(_))
            ) || owner.is_assoc_const_initializer(db)
                || owner.is_func_const_predicate(db)
                || matches!(
                    body.scope().parent_item(db),
                    Some(ItemKind::Const(constant)) if constant.body(db).to_opt() == Some(body)
                );
            value_of_module
                .then(|| scope_choices(db, body.scope()))
                .flatten()
        }
        BodyOwner::InheritedConst { body, .. } => scope_choices(db, body.scope()),
        _ => scope_choices(db, owner.scope()),
    }
}
