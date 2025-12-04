//! Pull-based HIR traversal.
//!
//! This module provides an iterator-based approach to traversing the HIR.
//! Each HIR node type implements `HirChildren` to enumerate its direct children.
//! The `HirDfs` iterator performs depth-first traversal by iterating children.

use crate::{
    HirDb,
    hir_def::{
        Body, Expr, ExprId, Partial, Pat, PatId, PathId, Stmt, StmtId, scope_graph::ScopeId,
    },
};

/// A node in the HIR tree, with its span and scope context.
#[derive(Debug, Clone)]
pub enum HirNode<'db> {
    Expr {
        body: Body<'db>,
        id: ExprId,
        scope: ScopeId<'db>,
    },
    Stmt {
        body: Body<'db>,
        id: StmtId,
        scope: ScopeId<'db>,
    },
    Pat {
        body: Body<'db>,
        id: PatId,
        scope: ScopeId<'db>,
    },
    Path {
        path: PathId<'db>,
        scope: ScopeId<'db>,
        kind: PathContext,
    },
}

/// Context for where a path appears.
#[derive(Debug, Clone, Copy)]
pub enum PathContext {
    Expr(ExprId),
    Pat(PatId),
    RecordInit(ExprId),
}

impl<'db> HirNode<'db> {
    /// Returns children of this node for traversal.
    pub fn children(&self, db: &'db dyn HirDb) -> Vec<HirNode<'db>> {
        match self {
            HirNode::Expr { body, id, scope } => expr_children(db, *body, *id, *scope),
            HirNode::Stmt { body, id, scope } => stmt_children(db, *body, *id, *scope),
            HirNode::Pat { body, id, scope } => pat_children(db, *body, *id, *scope),
            HirNode::Path { .. } => {
                // Paths are leaves (for now - could expand to path segments)
                vec![]
            }
        }
    }
}

fn expr_children<'db>(
    db: &'db dyn HirDb,
    body: Body<'db>,
    expr_id: ExprId,
    scope: ScopeId<'db>,
) -> Vec<HirNode<'db>> {
    let Partial::Present(expr) = expr_id.data(db, body) else {
        return vec![];
    };

    let mut children = Vec::new();

    // Check if this is a block - if so, update scope
    let child_scope = if matches!(expr, Expr::Block(_)) {
        ScopeId::Block(body, expr_id)
    } else {
        scope
    };

    match expr {
        Expr::Lit(_) => {}

        Expr::Block(stmts) => {
            for stmt_id in stmts {
                children.push(HirNode::Stmt {
                    body,
                    id: *stmt_id,
                    scope: child_scope,
                });
            }
        }

        Expr::Bin(lhs, rhs, _) => {
            children.push(HirNode::Expr {
                body,
                id: *lhs,
                scope: child_scope,
            });
            children.push(HirNode::Expr {
                body,
                id: *rhs,
                scope: child_scope,
            });
        }

        Expr::Un(inner, _) => {
            children.push(HirNode::Expr {
                body,
                id: *inner,
                scope: child_scope,
            });
        }

        Expr::Call(callee, args) => {
            children.push(HirNode::Expr {
                body,
                id: *callee,
                scope: child_scope,
            });
            for arg in args {
                children.push(HirNode::Expr {
                    body,
                    id: arg.expr,
                    scope: child_scope,
                });
            }
        }

        Expr::MethodCall(receiver, _, _, args) => {
            children.push(HirNode::Expr {
                body,
                id: *receiver,
                scope: child_scope,
            });
            for arg in args {
                children.push(HirNode::Expr {
                    body,
                    id: arg.expr,
                    scope: child_scope,
                });
            }
        }

        Expr::Path(path) => {
            if let Some(path) = path.to_opt() {
                children.push(HirNode::Path {
                    path,
                    scope: child_scope,
                    kind: PathContext::Expr(expr_id),
                });
            }
        }

        Expr::RecordInit(path, fields) => {
            if let Some(path) = path.to_opt() {
                children.push(HirNode::Path {
                    path,
                    scope: child_scope,
                    kind: PathContext::RecordInit(expr_id),
                });
            }
            for field in fields {
                children.push(HirNode::Expr {
                    body,
                    id: field.expr,
                    scope: child_scope,
                });
            }
        }

        Expr::Field(receiver, _) => {
            children.push(HirNode::Expr {
                body,
                id: *receiver,
                scope: child_scope,
            });
        }

        Expr::Tuple(elems) | Expr::Array(elems) => {
            for elem in elems {
                children.push(HirNode::Expr {
                    body,
                    id: *elem,
                    scope: child_scope,
                });
            }
        }

        Expr::ArrayRep(val, _rep_body) => {
            children.push(HirNode::Expr {
                body,
                id: *val,
                scope: child_scope,
            });
            // rep_body is a separate Body, would need separate traversal
        }

        Expr::If(cond, then_branch, else_branch) => {
            children.push(HirNode::Expr {
                body,
                id: *cond,
                scope: child_scope,
            });
            children.push(HirNode::Expr {
                body,
                id: *then_branch,
                scope: child_scope,
            });
            if let Some(else_expr) = else_branch {
                children.push(HirNode::Expr {
                    body,
                    id: *else_expr,
                    scope: child_scope,
                });
            }
        }

        Expr::Match(scrutinee, arms) => {
            children.push(HirNode::Expr {
                body,
                id: *scrutinee,
                scope: child_scope,
            });
            if let Partial::Present(arms) = arms {
                for arm in arms {
                    children.push(HirNode::Pat {
                        body,
                        id: arm.pat,
                        scope: child_scope,
                    });
                    children.push(HirNode::Expr {
                        body,
                        id: arm.body,
                        scope: child_scope,
                    });
                }
            }
        }

        Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
            children.push(HirNode::Expr {
                body,
                id: *lhs,
                scope: child_scope,
            });
            children.push(HirNode::Expr {
                body,
                id: *rhs,
                scope: child_scope,
            });
        }

        Expr::With(bindings, body_expr) => {
            for binding in bindings {
                children.push(HirNode::Expr {
                    body,
                    id: binding.value,
                    scope: child_scope,
                });
            }
            children.push(HirNode::Expr {
                body,
                id: *body_expr,
                scope: child_scope,
            });
        }
    }

    children
}

fn stmt_children<'db>(
    db: &'db dyn HirDb,
    body: Body<'db>,
    stmt_id: StmtId,
    scope: ScopeId<'db>,
) -> Vec<HirNode<'db>> {
    let Partial::Present(stmt) = stmt_id.data(db, body) else {
        return vec![];
    };

    let mut children = Vec::new();

    match stmt {
        Stmt::Let(pat, _ty, expr) => {
            children.push(HirNode::Pat {
                body,
                id: *pat,
                scope,
            });
            // TODO: handle type paths
            if let Some(expr_id) = expr {
                children.push(HirNode::Expr {
                    body,
                    id: *expr_id,
                    scope,
                });
            }
        }

        Stmt::For(pat, iter_expr, body_expr) => {
            children.push(HirNode::Pat {
                body,
                id: *pat,
                scope,
            });
            children.push(HirNode::Expr {
                body,
                id: *iter_expr,
                scope,
            });
            children.push(HirNode::Expr {
                body,
                id: *body_expr,
                scope,
            });
        }

        Stmt::While(cond, body_expr) => {
            children.push(HirNode::Expr {
                body,
                id: *cond,
                scope,
            });
            children.push(HirNode::Expr {
                body,
                id: *body_expr,
                scope,
            });
        }

        Stmt::Expr(expr_id) => {
            children.push(HirNode::Expr {
                body,
                id: *expr_id,
                scope,
            });
        }

        Stmt::Return(Some(expr_id)) => {
            children.push(HirNode::Expr {
                body,
                id: *expr_id,
                scope,
            });
        }

        Stmt::Return(None) | Stmt::Continue | Stmt::Break => {}
    }

    children
}

fn pat_children<'db>(
    db: &'db dyn HirDb,
    body: Body<'db>,
    pat_id: PatId,
    scope: ScopeId<'db>,
) -> Vec<HirNode<'db>> {
    let Partial::Present(pat) = pat_id.data(db, body) else {
        return vec![];
    };

    let mut children = Vec::new();

    match pat {
        Pat::Lit(_) | Pat::WildCard | Pat::Rest => {}

        Pat::Tuple(elems) => {
            for elem in elems {
                children.push(HirNode::Pat {
                    body,
                    id: *elem,
                    scope,
                });
            }
        }

        Pat::Path(path, _) => {
            if let Some(path) = path.to_opt() {
                children.push(HirNode::Path {
                    path,
                    scope,
                    kind: PathContext::Pat(pat_id),
                });
            }
        }

        Pat::PathTuple(path, elems) => {
            if let Some(path) = path.to_opt() {
                children.push(HirNode::Path {
                    path,
                    scope,
                    kind: PathContext::Pat(pat_id),
                });
            }
            for elem in elems {
                children.push(HirNode::Pat {
                    body,
                    id: *elem,
                    scope,
                });
            }
        }

        Pat::Record(path, fields) => {
            if let Some(path) = path.to_opt() {
                children.push(HirNode::Path {
                    path,
                    scope,
                    kind: PathContext::Pat(pat_id),
                });
            }
            for field in fields {
                children.push(HirNode::Pat {
                    body,
                    id: field.pat,
                    scope,
                });
            }
        }

        Pat::Or(lhs, rhs) => {
            children.push(HirNode::Pat {
                body,
                id: *lhs,
                scope,
            });
            children.push(HirNode::Pat {
                body,
                id: *rhs,
                scope,
            });
        }
    }

    children
}

/// Depth-first iterator over HIR nodes in a body.
pub struct BodyDfs<'db> {
    db: &'db dyn HirDb,
    stack: Vec<HirNode<'db>>,
}

impl<'db> BodyDfs<'db> {
    pub fn new(db: &'db dyn HirDb, body: Body<'db>) -> Self {
        let root = HirNode::Expr {
            body,
            id: body.expr(db),
            scope: body.scope(),
        };
        BodyDfs {
            db,
            stack: vec![root],
        }
    }
}

impl<'db> Iterator for BodyDfs<'db> {
    type Item = HirNode<'db>;

    fn next(&mut self) -> Option<HirNode<'db>> {
        let node = self.stack.pop()?;

        // Push children in reverse order so we visit them in order
        let children = node.children(self.db);
        self.stack.extend(children.into_iter().rev());

        Some(node)
    }
}

/// Convenience: iterate all paths in a body.
pub fn body_paths<'db>(
    db: &'db dyn HirDb,
    body: Body<'db>,
) -> impl Iterator<Item = (PathId<'db>, ScopeId<'db>, PathContext)> + 'db {
    BodyDfs::new(db, body).filter_map(|node| {
        if let HirNode::Path { path, scope, kind } = node {
            Some((path, scope, kind))
        } else {
            None
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{hir_def::ItemKind, test_db::TestDb};

    #[test]
    fn test_body_dfs_finds_paths() {
        let mut db = TestDb::default();

        let text = r#"
            fn foo() {
                let x: i32 = bar()
                baz(x)
            }
        "#;

        let file = db.standalone_file(text);
        let func = db.expect_item::<crate::hir_def::Func>(file);
        let body = func.body(&db).unwrap();

        let paths: Vec<_> = body_paths(&db, body).collect();

        // Should find: bar, baz, x (and possibly i32 if we handled types)
        // For now we only handle expression paths, so: bar, baz, x
        assert!(
            paths.len() >= 2,
            "Expected at least 2 paths, got {}",
            paths.len()
        );

        // Check that we got path IDs
        for (path, scope, _kind) in &paths {
            assert!(path.ident(&db).to_opt().is_some() || path.parent(&db).is_some());
            // scope should be valid
            let _ = scope.kind_name();
        }
    }

    #[test]
    fn test_body_dfs_finds_pattern_paths() {
        let mut db = TestDb::default();

        let text = r#"
            enum Foo { Bar, Baz }
            fn test(x: Foo) {
                match x {
                    Foo::Bar => {}
                    Foo::Baz => {}
                }
            }
        "#;

        let file = db.standalone_file(text);

        // Find the test function (second item after enum)
        let scope_graph = db.parse_source(file);
        let items: Vec<_> = scope_graph
            .items_dfs(&db)
            .filter(|item| matches!(item, ItemKind::Func(_)))
            .collect();

        let func = match items.first() {
            Some(ItemKind::Func(f)) => *f,
            _ => panic!("Expected a function"),
        };

        let body = func.body(&db).unwrap();
        let paths: Vec<_> = body_paths(&db, body).collect();

        // Should find: x (in match), Foo::Bar, Foo::Baz
        assert!(
            paths.len() >= 2,
            "Expected at least 2 paths from match arms, got {}",
            paths.len()
        );
    }
}
