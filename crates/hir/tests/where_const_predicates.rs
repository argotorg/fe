//! HIR-level tests for `where` clause const predicates (FCO-M5 S1):
//! lowering into `WhereClauseId::const_predicates` as anonymous bodies, and
//! visitor traversal of those bodies via `walk_where_clause`.

use fe_hir::{
    hir_def::{Body, BodyKind, Partial, TopLevelMod, WhereClauseId, WhereClauseOwner},
    test_db::HirAnalysisTestDb,
    visitor::{Visitor, VisitorCtxt, prelude::*},
};

const SOURCE: &str = r#"
trait Sized {
    const SIZE: u256
}

fn check_big<T>() where T: Sized, T::SIZE >= 50 {
}

fn check_mixed<T>() where T: Sized, T::SIZE < 500, { T::SIZE > 0 }
{
}

struct Wrap<T> where T: Sized, T::SIZE <= 32 {
    inner: T
}

enum Opt<T> where T: Sized, T::SIZE <= 32 {
    Some(T),
    None,
}

trait BigOnly<T> where T: Sized, T::SIZE >= 50 {
    fn big(self) -> u256
}

impl<T> Sized for Wrap<T> where T: Sized, T::SIZE <= 32 {
    const SIZE: u256 = 1
}
"#;

fn top_mod(db: &HirAnalysisTestDb, file: fe_hir::File) -> TopLevelMod<'_> {
    let (top_mod, _) = db.top_mod(file);
    top_mod
}

fn new_file(db: &mut HirAnalysisTestDb) -> fe_hir::File {
    db.new_stand_alone("where_const_predicates.fe".into(), SOURCE)
}

fn owner_clauses<'db>(
    db: &'db HirAnalysisTestDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<(String, WhereClauseId<'db>)> {
    top_mod
        .scope_graph(db)
        .items_dfs(db)
        .filter_map(|item| {
            let owner = WhereClauseOwner::from_item_opt(item)?;
            let clause = owner.where_clause(db);
            if clause.data(db).is_empty() && clause.const_predicates(db).is_empty() {
                return None;
            }
            let name = item
                .name(db)
                .map(|n| n.data(db).to_string())
                .unwrap_or_else(|| item.kind_name().to_string());
            Some((name, clause))
        })
        .collect()
}

#[test]
fn const_predicates_lowered_for_every_owner() {
    let mut db = HirAnalysisTestDb::default();
    let file = new_file(&mut db);
    let top_mod = top_mod(&db, file);

    let counts: Vec<_> = owner_clauses(&db, top_mod)
        .into_iter()
        .map(|(name, clause)| {
            (
                name,
                clause.data(&db).len(),
                clause.const_predicates(&db).len(),
            )
        })
        .collect();

    assert_eq!(
        counts,
        vec![
            ("check_big".to_string(), 1, 1),
            ("check_mixed".to_string(), 1, 2),
            ("Wrap".to_string(), 1, 1),
            ("Opt".to_string(), 1, 1),
            ("BigOnly".to_string(), 1, 1),
            // The `impl<T> Sized for Wrap<T>` block (impl-trait items are
            // unnamed; `kind_name` is the fallback).
            ("impl trait".to_string(), 1, 1),
        ],
    );
}

/// Every lowered const predicate is an anonymous body whose root expression
/// is present (the predicate expression itself).
#[test]
fn const_predicate_bodies_are_anonymous_with_present_expr() {
    let mut db = HirAnalysisTestDb::default();
    let file = new_file(&mut db);
    let top_mod = top_mod(&db, file);

    let mut bodies = 0;
    for (_, clause) in owner_clauses(&db, top_mod) {
        for body in clause.const_predicates(&db) {
            bodies += 1;
            assert_eq!(body.body_kind(&db), BodyKind::Anonymous);
            assert!(matches!(
                body.expr(&db).data(&db, *body),
                Partial::Present(_)
            ));
        }
    }
    assert_eq!(bodies, 7);
}

/// `walk_where_clause` walks const predicate bodies: a visitor that records
/// every visited body must see the predicate bodies of every owner kind
/// (new work in FCO-M5 S1 — effort2 never wired this).
#[test]
fn visitor_walks_const_predicate_bodies() {
    struct Collector<'db> {
        bodies: Vec<Body<'db>>,
        exprs: usize,
    }

    impl<'db> Visitor<'db> for Collector<'db> {
        fn visit_body(&mut self, ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, body: Body<'db>) {
            self.bodies.push(body);
            walk_body(self, ctxt, body);
        }

        fn visit_expr(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
            expr: fe_hir::hir_def::ExprId,
            _expr_data: &fe_hir::hir_def::Expr<'db>,
        ) {
            self.exprs += 1;
            walk_expr(self, ctxt, expr);
        }
    }

    let mut db = HirAnalysisTestDb::default();
    let file = new_file(&mut db);
    let top_mod = top_mod(&db, file);

    let mut collector = Collector {
        bodies: Vec::new(),
        exprs: 0,
    };
    let mut ctxt = VisitorCtxt::with_top_mod(&db, top_mod);
    collector.visit_top_mod(&mut ctxt, top_mod);

    let expected: Vec<Body> = owner_clauses(&db, top_mod)
        .into_iter()
        .flat_map(|(_, clause)| clause.const_predicates(&db).to_vec())
        .collect();
    assert_eq!(expected.len(), 7);

    for body in &expected {
        assert!(
            collector.bodies.contains(body),
            "visitor missed a const predicate body"
        );
    }
    // The walk also descended into predicate expressions (e.g. `T::SIZE >= 50`).
    assert!(collector.exprs > 0);
}
