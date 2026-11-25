//! A module's `with` declaration: the outermost choice frame of every body
//! written in the module and its child modules. Its operands go through the
//! operand classifier that a block `with` uses (`with_operand`), at the
//! module's scope, so a name means the same in both forms. A module adds no
//! values or providers: those are written in a block.

use salsa::SalsaValue;

use crate::{
    analysis::{
        HirAnalysisDb,
        name_resolution::{
            ExpectedPathKind, PathRes, Tier,
            diagnostics::PathResDiag,
            path_resolver::{PathResErrorKind, resolve_path},
        },
        ty::{
            diagnostics::{
                BodyDiag, FuncBodyDiag, TraitConstraintDiag, TyDiagCollection, TyLowerDiag,
            },
            trait_def::TraitInstId,
            trait_resolution::{
                ChoiceEntry, ChosenImpl, ChosenImpls, PredicateListId, TraitSolveCx, goal_holds,
            },
            ty_def::TyId,
        },
    },
    hir_def::{ItemKind, PathId, Trait, scope_graph::ScopeId},
    span::{DynLazySpan, path::LazyPathSpan},
};

use super::BodyOwner;
use super::with_operand::{
    BareName, OperandMeaning, OperandSite, OperandValues, classify_with_operand, earlier_answer,
    opening_contexts,
};

/// The frame of a module and the diagnostics of its own declaration.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub(crate) struct ModuleChoices<'db> {
    /// The module's frame: its parent's, extended by its own declaration.
    pub choices: Option<ChosenImpls<'db>>,
    /// The diagnostics of the module's own declaration.
    pub diags: Vec<FuncBodyDiag<'db>>,
}

#[salsa::interned]
struct ModuleKey<'db> {
    #[returns(copy)]
    module: ItemKind<'db>,
}

/// The frame of `module`, a module scope: the choices of the `with`
/// declarations of the module and of its parent modules, outermost first.
pub(crate) fn module_choices<'db>(
    db: &'db dyn HirAnalysisDb,
    module: ScopeId<'db>,
) -> &'db ModuleChoices<'db> {
    let ScopeId::Item(item @ (ItemKind::Mod(_) | ItemKind::TopMod(_))) = module else {
        unreachable!("a module scope is the scope of a module item");
    };
    module_choices_query(db, ModuleKey::new(db, item))
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

#[salsa::tracked(cycle_initial = module_choices_cycle_initial)]
fn module_choices_query<'db>(
    db: &'db dyn HirAnalysisDb,
    key: ModuleKey<'db>,
) -> ModuleChoices<'db> {
    let scope = ScopeId::from_item(key.module(db));
    let outer = scope
        .parent_module(db)
        .and_then(|parent| module_choices(db, parent).choices);
    let Some(declaration) = scope.child_items(db).find_map(|child| match child {
        ItemKind::With(declaration) => Some(declaration),
        _ => None,
    }) else {
        return ModuleChoices {
            choices: outer,
            diags: Vec::new(),
        };
    };

    let ingot = scope.ingot(db);
    let mut diags: Vec<FuncBodyDiag<'db>> = Vec::new();
    let mut current = outer;
    // This declaration's own entries, each with its operand's span: the
    // parent's frame is never compared.
    let mut listed: Vec<(ChosenImpl<'db>, DynLazySpan<'db>)> = Vec::new();
    // The conditions of each opening, with the opening's position in the
    // choices and its operand's span.
    let mut openings: Vec<(usize, Vec<TraitInstId<'db>>, DynLazySpan<'db>)> = Vec::new();

    for (idx, operand) in declaration.operands(db).iter().enumerate() {
        let Some(path) = operand.to_opt() else {
            // The parser reported it.
            continue;
        };
        let operand_span = declaration.span().operands().operand(idx);
        let span: DynLazySpan<'db> = operand_span.clone().into();
        let site = OperandSite {
            scope,
            assumptions: PredicateListId::empty_list(db),
            written_assumptions: PredicateListId::empty_list(db),
            viewer: crate::analysis::name_resolution::Viewer::written_in(db, declaration.scope()),
            outer: current,
            span: span.clone(),
            name_span: operand_span
                .clone()
                .segment(path.segment_index(db))
                .ident()
                .into(),
        };
        let values = ModuleOperands {
            db,
            scope,
            path_span: operand_span.clone(),
        };
        let (choice, conditions) = match classify_with_operand(db, &site, path, &values) {
            OperandMeaning::Named { choice } => (choice, None),
            OperandMeaning::Opening { choice, conditions } => (choice, Some(conditions)),
            // A refused operand contributes nothing; the rest of the
            // declaration is still read.
            OperandMeaning::Refused {
                diags: refused,
                changed_choice,
                ..
            } => {
                // A generated implementation changes what written code
                // chose; a declaration that generated code wrote changes
                // nothing.
                if !changed_choice || !scope.is_within_generated(db, ingot) {
                    diags.extend(refused);
                }
                continue;
            }
            OperandMeaning::NotAnImplementation { .. } => {
                let empty = PredicateListId::empty_list(db);
                match resolve_path(db, path, scope, empty, true)
                    .or_else(|_| resolve_path(db, path, scope, empty, false))
                {
                    // It names something, and it is not an implementation.
                    Ok(res) => {
                        if let Some(name) = path.ident(db).to_opt() {
                            diags.push(
                                PathResDiag::ExpectedImpl(span, name, res.kind_name()).into(),
                            );
                        }
                    }
                    // It names nothing: the path is reported as it is anywhere.
                    Err(error) => diags.extend(
                        error
                            .into_diag(db, path, operand_span, ExpectedPathKind::Value)
                            .map(Into::into),
                    ),
                }
                continue;
            }
        };

        let entry = choice
            .items(db)
            .last()
            .and_then(|entry| entry.implementation())
            .expect("a pushed choice is the last item");
        let cx = TraitSolveCx::new(db, scope, Tier::All).with_choices(current);
        if let Some((first, goal, possible)) = earlier_answer(db, cx, &listed, entry) {
            diags.push(
                BodyDiag::DuplicateWithChoice {
                    primary: span,
                    first,
                    goal,
                    possible,
                }
                .into(),
            );
            continue;
        }
        if let Some(conditions) = conditions {
            openings.push((choice.items(db).len() - 1, conditions, span.clone()));
        }
        listed.push((entry, span));
        current = Some(choice);
    }

    // An opening's conditions are proved once the whole list is read, with the
    // outer frame and every other operand of the declaration, without the
    // opening itself, whatever the order of the operands.
    if let Some(list_choices) = current {
        for (goal, context, span) in opening_contexts(db, list_choices, openings) {
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

    ModuleChoices {
        choices: current,
        diags,
    }
}

// A declaration whose resolution needs a value that its own frame decides
// starts the cycle with a refusal, never a provisional frame.
fn module_choices_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _: salsa::Id,
    key: ModuleKey<'db>,
) -> ModuleChoices<'db> {
    let scope = ScopeId::from_item(key.module(db));
    let span = scope
        .child_items(db)
        .find_map(|child| match child {
            ItemKind::With(declaration) => Some(declaration.span().into()),
            _ => None,
        })
        .unwrap_or_else(DynLazySpan::invalid);
    ModuleChoices {
        choices: None,
        diags: vec![FuncBodyDiag::Ty(
            TyLowerDiag::TypeLoweringCycle(span).into(),
        )],
    }
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
