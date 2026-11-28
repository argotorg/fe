//! The text of a refusal of the agreement check. A refusal names each route to
//! a goal by the bound it starts from and the steps after it (`RouteRef`), and
//! the implementation each route gives.

use super::{
    evidence::{Cut, Refusal, RouteRef, route_goals},
    trait_def::{ImplementorId, ImplementorOrigin, TraitInstId},
    trait_resolution::{
        BoundStep, COMPONENT_DEPTH_LIMIT, PredicateListId,
        normalize_trait_inst_preserving_validity, same_identity,
    },
};
use crate::{analysis::HirAnalysisDb, hir_def::scope_graph::ScopeId};

/// The header of an implementation as written, `impl<T: Marker> Foo for T`:
/// its parameters with the bounds on them, then the other bounds in a `where`
/// clause. The trait is printed without the associated types the
/// implementation defines.
pub(crate) fn implementation_header<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
) -> String {
    let params = implementor.params(db);
    let bounds = implementor.constraints(db).list(db);
    let generics = params
        .iter()
        .map(|&param| {
            let name = param.pretty_print(db).to_string();
            let own = bounds
                .iter()
                .filter(|bound| bound.self_ty(db) == param)
                .map(|bound| bound.pretty_print(db, false))
                .collect::<Vec<_>>();
            if own.is_empty() {
                name
            } else {
                format!("{name}: {}", own.join(" + "))
            }
        })
        .collect::<Vec<_>>();
    let generics = if generics.is_empty() {
        String::new()
    } else {
        format!("<{}>", generics.join(", "))
    };
    let others = bounds
        .iter()
        .filter(|bound| !params.contains(&bound.self_ty(db)))
        .map(|bound| bound.pretty_print(db, true))
        .collect::<Vec<_>>();
    let clause = if others.is_empty() {
        String::new()
    } else {
        format!(" where {}", others.join(", "))
    };
    let header = implementor.trait_(db).without_assoc_bindings(db);
    format!(
        "impl{generics} {} for {}{clause}",
        header.pretty_print(db, false),
        header.self_ty(db).pretty_print(db)
    )
}

/// How an implementation is named in a message: its name, or its header.
pub(crate) fn implementation_label<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: ImplementorId<'db>,
) -> String {
    match implementor.origin(db) {
        ImplementorOrigin::Hir(implementation) => match implementation.selection_name(db) {
            Some(name) => format!("`{}`", name.data(db)),
            None => format!("`{}`", implementation_header(db, implementor)),
        },
        _ => format!(
            "a compiler-provided implementation of `{}`",
            implementor.trait_(db).pretty_print(db, true)
        ),
    }
}

/// A route as printed: the bound it starts from, as the declaration states it,
/// and the steps after it.
fn route_text<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    route: &RouteRef<'db>,
) -> String {
    let root = format!("`{}`", route.declared.pretty_print(db, true));
    let Some(path) = route.path else {
        return if route.written {
            format!("{root}, as chosen here")
        } else {
            root
        };
    };
    let goals = route_goals(db, scope, route);
    let steps = path
        .steps(db)
        .iter()
        .enumerate()
        .map(|(position, step)| {
            let step_text = match step {
                BoundStep::AssocBound { assoc, .. } => {
                    format!("the bound on `{}`", assoc.data(db))
                }
                BoundStep::Super(index) => {
                    let name = goals
                        .get(position)
                        .copied()
                        .flatten()
                        .and_then(|child| child.def(db).name(db).to_opt())
                        .map_or_else(|| format!("#{index}"), |name| name.data(db).to_string());
                    format!("its supertrait `{name}`")
                }
            };
            // From the second step on, the goal the step starts from.
            match position.checked_sub(1).and_then(|before| goals.get(before)) {
                Some(Some(from)) => format!("{step_text} of `{}`", from.pretty_print(db, true)),
                _ => step_text,
            }
        })
        .collect::<Vec<_>>()
        .join(", then ");
    format!("{root} ({steps})")
}

fn implementation_of<'db>(
    db: &'db dyn HirAnalysisDb,
    implementor: Option<ImplementorId<'db>>,
) -> String {
    implementor.map_or_else(
        || "a dictionary of the caller".to_string(),
        |implementor| implementation_label(db, implementor),
    )
}

/// Whether the route passes through a goal that has the identity of `goal`
/// before it arrives at its last step.
fn passes_through<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    route: &RouteRef<'db>,
    goal: TraitInstId<'db>,
) -> bool {
    let goals = route_goals(db, scope, route);
    goals
        .iter()
        .take(goals.len().saturating_sub(1))
        .flatten()
        .any(|&through| same_identity(db, scope, through, goal))
}

/// `goal` with its projections normalized through ordinary implementations in
/// `scope`, as a message names it.
pub(crate) fn normalized_goal<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    goal: TraitInstId<'db>,
) -> TraitInstId<'db> {
    normalize_trait_inst_preserving_validity(db, goal, scope, PredicateListId::empty_list(db))
}

/// The message of a refusal of `goal`, in the coordinates of the instance or
/// use that `scope` is the normalization scope of.
pub(crate) fn refusal_label<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    goal: TraitInstId<'db>,
    reason: Refusal<'db>,
) -> String {
    let printed = goal.pretty_print(db, true);
    let holds = |named: Option<ImplementorId<'db>>| {
        named.map_or_else(String::new, |named| {
            format!(
                ", and that dictionary holds {}",
                implementation_label(db, named)
            )
        })
    };
    // The clause that ends a route that is not followed: the named
    // implementation a dictionary on it holds, or, where the route is refused
    // only because a module-level `with` is visible, that a frame can choose.
    let reached = |named: Option<ImplementorId<'db>>, by_frame: bool, place: &str| {
        if by_frame {
            format!(", and a module-level `with` can choose an implementation {place}")
        } else {
            holds(named)
        }
    };
    match reason {
        Refusal::Differs { a, b } => {
            // The route written at the use comes first, then the routes in the
            // order of their text, so the message does not depend on the order
            // of the bounds.
            let mut routes =
                [a, b].map(|route| (!route.written, route_text(db, scope, &route), route));
            routes.sort_by(|x, y| (x.0, &x.1).cmp(&(y.0, &y.1)));
            let [(_, first_text, first), (_, second_text, second)] = routes;
            // A route written at the use ends a clause: "as chosen here,".
            let comma = if first.written { "," } else { "" };
            let through = if passes_through(db, scope, &first, goal)
                || passes_through(db, scope, &second, goal)
            {
                format!(", which passes through `{printed}` itself")
            } else {
                String::new()
            };
            format!(
                "`{printed}` receives {} through {first_text}{comma} and {} through {second_text}{through}",
                implementation_of(db, first.selects),
                implementation_of(db, second.selects),
            )
        }
        Refusal::NoComponent { route: Some(route) } => {
            // A route written at the use ends a clause: "as chosen here,".
            let comma = if route.written { "," } else { "" };
            format!(
                "`{printed}`: the dictionary given through {}{comma} has no such component",
                route_text(db, scope, &route)
            )
        }
        Refusal::NoComponent { route: None } => format!("`{printed}`: no dictionary supplies it"),
        Refusal::Unfollowed {
            route,
            cut: Cut::Depth,
            named,
            by_frame,
        } => {
            let limit = match COMPONENT_DEPTH_LIMIT {
                4 => "four".to_string(),
                other => other.to_string(),
            };
            format!(
                "`{printed}` lies below the {limit} associated-type levels that are followed through {}{}",
                route_text(db, scope, &route),
                reached(named, by_frame, "below that point")
            )
        }
        Refusal::Unfollowed {
            route,
            cut: Cut::Cycle,
            named,
            by_frame,
        } => format!(
            "`{printed}` is reached through {} by a derivation that returns to a dictionary still being formed{}",
            route_text(db, scope, &route),
            reached(named, by_frame, "on it")
        ),
    }
}
