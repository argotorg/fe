use parser::ast::{
    self, AstPtr, AttrListOwner as _, ItemModifierOwner as _, prelude::AstNode as _,
};

use crate::{
    HirDb,
    hir_def::{
        Attr, AttrListId, Body, BodyKind, CallArg, Contract, ContractRecv, ContractRecvArm,
        ContractRecvArmListId, ContractRecvListId, EffectParam, EffectParamListId, Expr, ExprId,
        FieldDef, FieldDefListId, Func, FuncParam, FuncParamListId, FuncParamName, GenericArg,
        GenericArgListId, GenericParamListId, IdentId, IntegerId, ItemModifier, LitKind, MatchArm,
        NormalAttr, Partial, Pat, PatId, PathId, PathKind, Stmt, StmtId, TrackedItemVariant,
        TypeGenericArg, TypeId, TypeKind, Use, Visibility, WhereClauseId,
        use_tree::{UsePathId, UsePathSegment},
    },
    lower::{FileLowerCtxt, body::BodyCtxt, item::lower_uses_clause_opt},
    span::{
        ContractInitDesugared, ContractLoweringDesugared, ContractLoweringDesugaredFocus,
        DesugaredOrigin, HirOrigin,
    },
};

/// Builder for synthesizing HIR expressions during contract lowering.
///
/// Wraps a `BodyCtxt` and desugared origin to provide convenient methods for
/// building common HIR patterns like path expressions, let bindings, and calls.
struct HirBuilder<'a, 'ctxt, 'db> {
    body_ctxt: &'a mut BodyCtxt<'ctxt, 'db>,
    origin: DesugaredOrigin,
}

#[allow(dead_code)]
impl<'a, 'ctxt, 'db> HirBuilder<'a, 'ctxt, 'db> {
    fn new(body_ctxt: &'a mut BodyCtxt<'ctxt, 'db>, origin: impl Into<DesugaredOrigin>) -> Self {
        Self {
            body_ctxt,
            origin: origin.into(),
        }
    }

    fn db(&self) -> &'db dyn HirDb {
        self.body_ctxt.f_ctxt.db()
    }

    fn origin<T: parser::ast::prelude::AstNode<Language = parser::FeLang>>(&self) -> HirOrigin<T> {
        HirOrigin::desugared(self.origin.clone())
    }
    /// Creates a path from string segments, e.g. `&["std", "evm", "calldata", "CallData"]`.
    fn path(&self, segments: &[&str]) -> PathId<'db> {
        PathId::from_segments(self.db(), segments)
    }

    /// Creates an identifier.
    fn ident(&self, name: &str) -> IdentId<'db> {
        IdentId::new(self.db(), name.to_string())
    }

    /// Creates a path expression from segments.
    fn path_expr(&mut self, segments: &[&str]) -> ExprId {
        let path = self.path(segments);
        self.body_ctxt
            .push_expr(Expr::Path(Partial::Present(path)), self.origin())
    }

    /// Creates an expression referencing a local variable by name.
    fn var_expr(&mut self, name: &str) -> ExprId {
        let ident = self.ident(name);
        self.body_ctxt.push_expr(
            Expr::Path(Partial::Present(PathId::from_ident(self.db(), ident))),
            self.origin(),
        )
    }

    /// Creates an integer literal expression.
    fn int_lit(&mut self, n: usize) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::Lit(LitKind::Int(IntegerId::from_usize(self.db(), n))),
            self.origin(),
        )
    }

    /// Creates a function call expression with unlabeled arguments.
    fn call(&mut self, callee: ExprId, args: Vec<ExprId>) -> ExprId {
        let call_args = args
            .into_iter()
            .map(|expr| CallArg { label: None, expr })
            .collect();
        self.body_ctxt
            .push_expr(Expr::Call(callee, call_args), self.origin())
    }

    /// Creates a function call expression with labeled arguments.
    fn call_labeled(&mut self, callee: ExprId, args: Vec<(Option<&str>, ExprId)>) -> ExprId {
        let call_args = args
            .into_iter()
            .map(|(label, expr)| CallArg {
                label: label.map(|l| self.ident(l)),
                expr,
            })
            .collect();
        self.body_ctxt
            .push_expr(Expr::Call(callee, call_args), self.origin())
    }

    /// Creates a method call expression.
    fn method_call(&mut self, receiver: ExprId, method: &str, args: Vec<ExprId>) -> ExprId {
        let call_args = args
            .into_iter()
            .map(|expr| CallArg { label: None, expr })
            .collect();
        self.body_ctxt.push_expr(
            Expr::MethodCall(
                receiver,
                Partial::Present(self.ident(method)),
                GenericArgListId::none(self.db()),
                call_args,
            ),
            self.origin(),
        )
    }

    /// Creates a binding pattern for a variable.
    fn bind_pat(&mut self, name: &str, is_mut: bool) -> PatId {
        let ident = self.ident(name);
        self.body_ctxt.push_pat(
            Pat::Path(
                Partial::Present(PathId::from_ident(self.db(), ident)),
                is_mut,
            ),
            self.origin(),
        )
    }

    /// Creates a wildcard pattern.
    fn wildcard_pat(&mut self) -> PatId {
        self.body_ctxt.push_pat(Pat::WildCard, self.origin())
    }

    /// Creates a let statement binding an expression to a name.
    fn let_stmt(&mut self, stmts: &mut Vec<StmtId>, name: &str, is_mut: bool, value: ExprId) {
        let pat = self.bind_pat(name, is_mut);
        stmts.push(
            self.body_ctxt
                .push_stmt(Stmt::Let(pat, None, Some(value)), self.origin()),
        );
    }

    /// Creates an expression statement.
    fn expr_stmt(&mut self, stmts: &mut Vec<StmtId>, expr: ExprId) {
        stmts.push(self.body_ctxt.push_stmt(Stmt::Expr(expr), self.origin()));
    }

    /// Creates a return statement.
    fn return_stmt(&mut self, stmts: &mut Vec<StmtId>, expr: Option<ExprId>) {
        stmts.push(self.body_ctxt.push_stmt(Stmt::Return(expr), self.origin()));
    }

    /// Creates a block expression from statements.
    fn block(&mut self, stmts: Vec<StmtId>) -> ExprId {
        self.body_ctxt.f_ctxt.enter_block_scope();
        let block = self.body_ctxt.push_expr(Expr::Block(stmts), self.origin());
        self.body_ctxt.f_ctxt.leave_block_scope(block);
        block
    }

    /// Creates a tuple expression.
    fn tuple(&mut self, elems: Vec<ExprId>) -> ExprId {
        self.body_ctxt.push_expr(Expr::Tuple(elems), self.origin())
    }

    /// Creates a record initialization expression.
    fn record_init(
        &mut self,
        path: PathId<'db>,
        fields: Vec<crate::hir_def::Field<'db>>,
    ) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::RecordInit(Partial::Present(path), fields),
            self.origin(),
        )
    }

    /// Creates an if-else expression.
    fn if_else(&mut self, cond: ExprId, then_expr: ExprId, else_expr: ExprId) -> ExprId {
        self.body_ctxt
            .push_expr(Expr::If(cond, then_expr, Some(else_expr)), self.origin())
    }

    /// Creates an if expression with no else branch.
    fn if_(&mut self, cond: ExprId, then_expr: ExprId) -> ExprId {
        self.body_ctxt
            .push_expr(Expr::If(cond, then_expr, None), self.origin())
    }

    /// Creates an equality comparison expression.
    fn eq(&mut self, lhs: ExprId, rhs: ExprId) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::Bin(
                lhs,
                rhs,
                crate::hir_def::BinOp::Comp(crate::hir_def::CompBinOp::Eq),
            ),
            self.origin(),
        )
    }

    /// Creates a less-than comparison expression.
    fn lt(&mut self, lhs: ExprId, rhs: ExprId) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::Bin(
                lhs,
                rhs,
                crate::hir_def::BinOp::Comp(crate::hir_def::CompBinOp::Lt),
            ),
            self.origin(),
        )
    }

    /// Creates an addition expression.
    fn add(&mut self, lhs: ExprId, rhs: ExprId) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::Bin(
                lhs,
                rhs,
                crate::hir_def::BinOp::Arith(crate::hir_def::ArithBinOp::Add),
            ),
            self.origin(),
        )
    }

    /// Creates a field access expression by name.
    fn field_access(&mut self, base: ExprId, field: &str) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::Field(
                base,
                Partial::Present(crate::hir_def::FieldIndex::Ident(self.ident(field))),
            ),
            self.origin(),
        )
    }

    /// Creates a field access expression by index.
    fn field_index(&mut self, base: ExprId, idx: usize) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::Field(
                base,
                Partial::Present(crate::hir_def::FieldIndex::Index(IntegerId::from_usize(
                    self.db(),
                    idx,
                ))),
            ),
            self.origin(),
        )
    }

    /// Creates a `revert(0, 0)` call.
    fn revert_call(&mut self) -> ExprId {
        let evm = self.var_expr("evm");
        let z0 = self.int_lit(0);
        let z1 = self.int_lit(0);
        self.method_call(evm, "revert", vec![z0, z1])
    }

    /// Creates `return_data(ptr, len)` call.
    fn return_data_call(&mut self, ptr: ExprId, len: ExprId) -> ExprId {
        let evm = self.var_expr("evm");
        self.method_call(evm, "return_data", vec![ptr, len])
    }

    /// Creates an Ok pattern: `Result::Ok(inner_pat)`.
    fn ok_pat(&mut self, inner: PatId) -> PatId {
        let ok_path = self.path(&["Result", "Ok"]);
        self.body_ctxt.push_pat(
            Pat::PathTuple(Partial::Present(ok_path), vec![inner]),
            self.origin(),
        )
    }

    /// Creates an Err pattern: `Result::Err(inner_pat)`.
    fn err_pat(&mut self, inner: PatId) -> PatId {
        let err_path = self.path(&["Result", "Err"]);
        self.body_ctxt.push_pat(
            Pat::PathTuple(Partial::Present(err_path), vec![inner]),
            self.origin(),
        )
    }

    /// Creates a tuple pattern.
    fn tuple_pat(&mut self, elems: Vec<PatId>) -> PatId {
        self.body_ctxt.push_pat(Pat::Tuple(elems), self.origin())
    }

    /// Creates a match expression with Ok/Err arms that reverts on error.
    fn match_or_revert(&mut self, scrutinee: ExprId, ok_body: ExprId) -> ExprId {
        let ok_wild = self.wildcard_pat();
        let ok_pat = self.ok_pat(ok_wild);
        let err_wild = self.wildcard_pat();
        let err_pat = self.err_pat(err_wild);
        let revert = self.revert_call();

        self.body_ctxt.push_expr(
            Expr::Match(
                scrutinee,
                Partial::Present(vec![
                    MatchArm {
                        pat: ok_pat,
                        body: ok_body,
                    },
                    MatchArm {
                        pat: err_pat,
                        body: revert,
                    },
                ]),
            ),
            self.origin(),
        )
    }

    /// Creates a match expression extracting value from Ok or reverting on Err.
    fn match_ok_value_or_revert(&mut self, scrutinee: ExprId, binding: &str) -> ExprId {
        let binding_pat = self.bind_pat(binding, false);
        let ok_pat = self.ok_pat(binding_pat);
        let ok_body = self.var_expr(binding);

        let err_wild = self.wildcard_pat();
        let err_pat = self.err_pat(err_wild);
        let revert = self.revert_call();

        self.body_ctxt.push_expr(
            Expr::Match(
                scrutinee,
                Partial::Present(vec![
                    MatchArm {
                        pat: ok_pat,
                        body: ok_body,
                    },
                    MatchArm {
                        pat: err_pat,
                        body: revert,
                    },
                ]),
            ),
            self.origin(),
        )
    }

    /// Creates a with-expression wrapping a call with effect bindings.
    fn with_expr(&mut self, bindings: Vec<IdentId<'db>>, inner: ExprId) -> ExprId {
        if bindings.is_empty() {
            return inner;
        }
        let with_bindings = bindings
            .into_iter()
            .map(|binding| {
                let value = self.body_ctxt.push_expr(
                    Expr::Path(Partial::Present(PathId::from_ident(self.db(), binding))),
                    self.origin(),
                );
                crate::hir_def::WithBinding {
                    key_path: None,
                    value,
                }
            })
            .collect();
        self.body_ctxt
            .push_expr(Expr::With(with_bindings, inner), self.origin())
    }

    /// Creates a path expression with generic type arguments appended.
    /// E.g., `path_with_generic(&["core", "effect_ref"], "StorPtr", ty)` creates
    /// `core::effect_ref::StorPtr<ty>`.
    /// If base is empty, creates just `StorPtr<ty>`.
    fn path_with_generic(&self, base: &[&str], name: &str, ty: TypeId<'db>) -> PathId<'db> {
        let db = self.db();
        let generic_args = GenericArgListId::new(
            db,
            vec![GenericArg::Type(TypeGenericArg {
                ty: Partial::Present(ty),
            })],
            true,
        );
        if base.is_empty() {
            PathId::new(
                db,
                PathKind::Ident {
                    ident: Partial::Present(IdentId::new(db, name.to_string())),
                    generic_args,
                },
                None,
            )
        } else {
            let base_path = PathId::from_segments(db, base);
            base_path.push(
                db,
                PathKind::Ident {
                    ident: Partial::Present(IdentId::new(db, name.to_string())),
                    generic_args,
                },
            )
        }
    }

    /// Creates a type from a path.
    fn path_ty(&self, segments: &[&str]) -> TypeId<'db> {
        TypeId::new(
            self.db(),
            TypeKind::Path(Partial::Present(self.path(segments))),
        )
    }

    /// Creates `size_of<Sol::Selector>()` expression.
    fn sol_selector_size(&mut self) -> ExprId {
        let sol_selector_ty = self.path_ty(&["Sol", "Selector"]);
        let size_of_path = self.path_with_generic(&[], "size_of", sol_selector_ty);
        let size_of_fn = self.path_id_expr(size_of_path);
        self.call(size_of_fn, vec![])
    }

    /// Creates `size_of<T>()` expression for a given type.
    fn size_of_ty(&mut self, ty: TypeId<'db>) -> ExprId {
        let size_of_path = self.path_with_generic(&[], "size_of", ty);
        let size_of_fn = self.path_id_expr(size_of_path);
        self.call(size_of_fn, vec![])
    }

    /// Creates `encoded_size<T>()` expression for a given type.
    /// Returns the Solidity ABI-encoded size of the type.
    fn encoded_size_of_ty(&mut self, ty: TypeId<'db>) -> ExprId {
        let encoded_size_path = self.path_with_generic(&[], "encoded_size", ty);
        let encoded_size_fn = self.path_id_expr(encoded_size_path);
        self.call(encoded_size_fn, vec![])
    }

    /// Creates a let statement with an explicit type annotation.
    fn let_stmt_typed(
        &mut self,
        stmts: &mut Vec<StmtId>,
        name: &str,
        is_mut: bool,
        ty: TypeId<'db>,
        value: ExprId,
    ) {
        let pat = self.bind_pat(name, is_mut);
        stmts.push(
            self.body_ctxt
                .push_stmt(Stmt::Let(pat, Some(ty), Some(value)), self.origin()),
        );
    }

    /// Creates a match expression with custom Ok/Err handling.
    fn match_result(
        &mut self,
        scrutinee: ExprId,
        ok_pat: PatId,
        ok_body: ExprId,
        err_pat: PatId,
        err_body: ExprId,
    ) -> ExprId {
        self.body_ctxt.push_expr(
            Expr::Match(
                scrutinee,
                Partial::Present(vec![
                    MatchArm {
                        pat: ok_pat,
                        body: ok_body,
                    },
                    MatchArm {
                        pat: err_pat,
                        body: err_body,
                    },
                ]),
            ),
            self.origin(),
        )
    }

    /// Creates a path expression from a PathId.
    fn path_id_expr(&mut self, path: PathId<'db>) -> ExprId {
        self.body_ctxt
            .push_expr(Expr::Path(Partial::Present(path)), self.origin())
    }
}

impl<'db> ContractRecvArmListId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, recv_idx: usize, ast: ast::RecvArmList) -> Self {
        let arms = ast
            .into_iter()
            .enumerate()
            .map(|(idx, arm)| ContractRecvArm::lower_ast(ctxt, recv_idx, idx, arm))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), arms)
    }
}

impl<'db> ContractRecvArm<'db> {
    fn lower_ast(
        ctxt: &mut FileLowerCtxt<'db>,
        recv_idx: usize,
        arm_idx: usize,
        ast: ast::RecvArm,
    ) -> Self {
        let variant = TrackedItemVariant::ContractRecvArm {
            recv_idx: recv_idx as u32,
            arm_idx: arm_idx as u32,
        };
        let id = ctxt.joined_id(variant);
        let mut body_ctxt = BodyCtxt::new(ctxt, id);

        let pat = Pat::lower_ast_opt(&mut body_ctxt, ast.pat());
        let body_ast = ast
            .body()
            .map(|b| ast::Expr::cast(b.syntax().clone()).unwrap());
        let body_expr = Expr::push_to_body_opt(&mut body_ctxt, body_ast.clone());
        let body = body_ctxt.build(body_ast.as_ref(), body_expr, BodyKind::FuncBody);
        let ret_ty = ast.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));
        let effects = lower_uses_clause_opt(ctxt, ast.uses_clause());

        ContractRecvArm {
            pat,
            ret_ty,
            effects,
            body,
        }
    }
}

impl<'db> Contract<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Contract) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Contract(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let fields = lower_contract_fields_opt(ctxt, ast.fields());
        // Contract-level uses clause
        let effects = lower_uses_clause_opt(ctxt, ast.uses_clause());

        if let Some(init_ast) = ast.init_block() {
            lower_contract_init_as_func(ctxt, init_ast);
        }

        // Recv blocks (message handlers)
        let recvs = {
            let mut data = Vec::new();
            for (recv_idx, r) in ast.recvs().enumerate() {
                let msg_path = r.path().map(|p| PathId::lower_ast(ctxt, p));
                let arms = r
                    .arms()
                    .map(|arms| ContractRecvArmListId::lower_ast(ctxt, recv_idx, arms))
                    .unwrap_or_else(|| ContractRecvArmListId::new(ctxt.db(), vec![]));
                data.push(ContractRecv { msg_path, arms });
            }
            ContractRecvListId::new(ctxt.db(), data)
        };
        let origin = HirOrigin::raw(&ast);

        let contract = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            fields,
            effects,
            recvs,
            ctxt.top_mod(),
            origin,
        );
        let contract = ctxt.leave_item_scope(contract);

        // Synthesize low-level contract entrypoints + recv arm handlers at the parent scope.
        lower_contract_entrypoints_and_handlers(ctxt, contract, &ast);

        contract
    }
}

fn lower_contract_fields_opt<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    ast: Option<ast::ContractFields>,
) -> FieldDefListId<'db> {
    match ast {
        Some(cf) => {
            let fields = cf
                .into_iter()
                .map(|field| FieldDef::lower_ast(ctxt, field))
                .collect::<Vec<_>>();
            FieldDefListId::new(ctxt.db(), fields)
        }
        None => FieldDefListId::new(ctxt.db(), Vec::new()),
    }
}

fn lower_contract_entrypoints_and_handlers<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    contract: Contract<'db>,
    contract_ast: &ast::Contract,
) {
    let db = ctxt.db();
    let contract_ptr = AstPtr::new(contract_ast);
    let init_ast = contract_ast.init_block();

    let Some(contract_name) = contract.name(db).to_opt() else {
        return;
    };

    let has_fields = !contract.fields(db).data(db).is_empty();
    let has_init = contract_ast.init_block().is_some();
    let has_recv = contract_ast.recvs().next().is_some();
    if !has_fields && !has_init && !has_recv {
        return;
    }

    // Create the module `mod __ContractName { ... }` to contain all generated functions.
    let mod_name_str = format!("__{}", contract_name.data(db));
    let mod_name = Partial::Present(IdentId::new(db, mod_name_str));
    let mod_id = ctxt.joined_id(TrackedItemVariant::Mod(mod_name));
    ctxt.enter_item_scope(mod_id, true);

    // Insert `use super::*` to access parent scope items
    ctxt.insert_synthetic_super_use();

    // Insert use statements for core/std types used in generated code
    insert_contract_use_statements(ctxt);

    // Synthesize per-arm handler functions so the runtime can call them.
    for (recv_idx, recv_ast) in contract_ast.recvs().enumerate() {
        let msg_path = recv_ast.path().map(|p| PathId::lower_ast(ctxt, p));

        let Some(arms_ast) = recv_ast.arms() else {
            continue;
        };

        for (arm_idx, arm_ast) in arms_ast.into_iter().enumerate() {
            lower_contract_recv_arm_handler_func(
                ctxt,
                contract,
                contract_ptr.clone(),
                msg_path,
                recv_idx,
                arm_idx,
                arm_ast,
            );
        }
    }

    // Synthesize init handler (user init block) so the init entrypoint can call it.
    if let Some(init_ast) = init_ast.clone() {
        lower_contract_init_handler_func(ctxt, contract, contract_ptr.clone(), init_ast);
    }

    // Synthesize init/runtime entrypoints.
    lower_contract_init_entrypoint_func(
        ctxt,
        contract,
        contract_ptr.clone(),
        contract_name,
        init_ast,
    );
    lower_contract_runtime_entrypoint_func(ctxt, contract, contract_name, contract_ast);

    // Create and close the module
    let desugared = ContractLoweringDesugared {
        contract: contract_ptr,
        recv_idx: None,
        arm_idx: None,
        focus: ContractLoweringDesugaredFocus::Contract,
    };
    let origin = HirOrigin::desugared(desugared);
    let mod_ = crate::hir_def::Mod::new(
        db,
        mod_id,
        mod_name,
        AttrListId::new(db, vec![]),
        Visibility::Private,
        ctxt.top_mod(),
        origin,
    );
    ctxt.leave_item_scope(mod_);
}

/// Inserts use statements for common core/std types used in generated contract code.
fn insert_contract_use_statements(ctxt: &mut FileLowerCtxt<'_>) {
    let db = ctxt.db();

    // Helper to insert a single use statement
    let mut insert_use = |segments: &[&str]| {
        let segs: Vec<_> = segments
            .iter()
            .map(|s| Partial::Present(UsePathSegment::Ident(IdentId::new(db, s.to_string()))))
            .collect();
        let path = Partial::Present(UsePathId::new(db, segs));
        let id = ctxt.joined_id(TrackedItemVariant::Use(path));
        ctxt.enter_item_scope(id, false);
        let top_mod = ctxt.top_mod();
        let origin = HirOrigin::synthetic();
        let use_ = Use::new(db, id, path, None, Visibility::Private, top_mod, origin);
        ctxt.leave_item_scope(use_);
    };

    // Core types
    insert_use(&["core", "size_of"]);

    // Std types
    insert_use(&["std", "evm", "effects", "Evm"]);
    insert_use(&["std", "evm", "effects", "RawOps"]);
    insert_use(&["std", "evm", "effects", "RawStorage"]);
    insert_use(&["std", "evm", "effects", "StorPtr"]);
    insert_use(&["std", "evm", "contract_field_slot"]);
    insert_use(&["std", "evm", "calldata", "CallData"]);
    insert_use(&["std", "abi", "Sol"]);
    insert_use(&["std", "abi", "sol", "SolDecoder"]);
    insert_use(&["std", "abi", "sol", "SolEncoder"]);
    insert_use(&["std", "abi", "sol", "encoded_size"]);
}

/// Converts a path to an underscore-separated name string.
/// E.g., `Erc20::Transfer` becomes `Erc20_Transfer`.
fn path_to_name<'db>(db: &'db dyn HirDb, path: PathId<'db>) -> String {
    let this = match path.kind(db) {
        PathKind::Ident { ident, .. } => ident
            .to_opt()
            .map_or("_".to_string(), |id| id.data(db).to_string()),
        PathKind::QualifiedType { .. } => "_".to_string(),
    };

    if let Some(parent) = path.parent(db) {
        path_to_name(db, parent) + "_" + &this
    } else {
        this
    }
}

fn mk_contract_attr<'db>(db: &'db dyn HirDb, name: &str, contract_name: IdentId<'db>) -> Attr<'db> {
    let path = PathId::from_ident(db, IdentId::new(db, name.to_string()));
    // In attribute syntax like `#[attr(Foo)]`, Foo is the "key" (a path), not a value.
    // The value is only present when there's `= something` after the key.
    Attr::Normal(NormalAttr {
        path: Partial::Present(path),
        args: vec![crate::hir_def::attr::AttrArg {
            key: Partial::Present(PathId::from_ident(db, contract_name)),
            value: None,
        }],
    })
}

fn contract_field_type_paths<'db>(
    db: &'db dyn HirDb,
    contract: Contract<'db>,
) -> rustc_hash::FxHashMap<IdentId<'db>, PathId<'db>> {
    let mut map = rustc_hash::FxHashMap::default();
    for field in contract.fields(db).data(db) {
        let (Some(name), Some(ty)) = (field.name.to_opt(), field.type_ref.to_opt()) else {
            continue;
        };
        if let TypeKind::Path(Partial::Present(path)) = ty.data(db) {
            map.insert(name, *path);
        }
    }
    map
}

fn contract_effect_type_paths<'db>(
    db: &'db dyn HirDb,
    contract: Contract<'db>,
) -> rustc_hash::FxHashMap<IdentId<'db>, PathId<'db>> {
    let mut map = rustc_hash::FxHashMap::default();
    for eff in contract.effects(db).data(db) {
        let Some(key_path) = eff.key_path.to_opt() else {
            continue;
        };
        let Some(binding) = eff.name.or_else(|| key_path.as_ident(db)) else {
            continue;
        };
        map.insert(binding, key_path);
    }
    map
}

fn lower_contract_typed_uses_clause<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    contract: Contract<'db>,
    uses: Option<ast::UsesClause>,
) -> EffectParamListId<'db> {
    use crate::hir_def::EffectParam;

    let db = ctxt.db();
    let raw = lower_uses_clause_opt(ctxt, uses);
    let field_types = contract_field_type_paths(db, contract);
    let effect_types = contract_effect_type_paths(db, contract);

    let mut typed = Vec::new();
    for eff in raw.data(db) {
        let Some(raw_key_path) = eff.key_path.to_opt() else {
            continue;
        };
        let Some(binding) = eff.name.or_else(|| raw_key_path.as_ident(db)) else {
            continue;
        };

        let ty_path = field_types
            .get(&binding)
            .copied()
            .or_else(|| effect_types.get(&binding).copied())
            .or_else(|| {
                // If this uses param was explicitly written as `name: Type`, keep its type.
                eff.name.is_some().then_some(raw_key_path)
            })
            .unwrap_or(raw_key_path);

        typed.push(EffectParam {
            name: Some(binding),
            key_path: Partial::Present(ty_path),
            is_mut: eff.is_mut,
        });
    }

    EffectParamListId::new(db, typed)
}

fn lower_contract_recv_arm_handler_func<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    contract: Contract<'db>,
    contract_ptr: AstPtr<ast::Contract>,
    msg_path: Option<PathId<'db>>,
    recv_idx: usize,
    arm_idx: usize,
    arm_ast: ast::RecvArm,
) -> Func<'db> {
    let db = ctxt.db();
    let desugared = ContractLoweringDesugared {
        contract: contract_ptr,
        recv_idx: Some(recv_idx),
        arm_idx: Some(arm_idx),
        focus: ContractLoweringDesugaredFocus::RecvArm,
    };

    // Determine the msg variant type for the arm from its pattern.
    let variant_ty_path = match arm_ast.pat().and_then(|p| match p.kind() {
        ast::PatKind::Path(path_pat) => path_pat.path(),
        ast::PatKind::PathTuple(path_tup) => path_tup.path(),
        ast::PatKind::Record(record) => record.path(),
        _ => None,
    }) {
        Some(path_ast) => {
            let variant_path = PathId::lower_ast(ctxt, path_ast);
            if let Some(msg_path) = msg_path
                && variant_path.is_bare_ident(db)
                && let Some(ident) = variant_path.as_ident(db)
            {
                msg_path.push_ident(db, ident)
            } else {
                variant_path
            }
        }
        None => PathId::from_str(db, "_"),
    };

    let variant_name = path_to_name(db, variant_ty_path);
    let fn_name_str = format!("recv_{}_{}_{}", variant_name, recv_idx, arm_idx);
    let fn_name = Partial::Present(IdentId::new(db, fn_name_str));
    let id = ctxt.joined_id(TrackedItemVariant::Func(fn_name));
    ctxt.enter_item_scope(id, false);

    let args_ty_path = variant_ty_path.push_str(db, "Args");
    let args_ty = TypeId::new(db, TypeKind::Path(Partial::Present(args_ty_path)));
    let args_param_name = FuncParamName::Ident(IdentId::new(db, "args".to_string()));
    let params = FuncParamListId::new(
        db,
        vec![FuncParam {
            is_mut: false,
            label: None,
            name: Partial::Present(args_param_name),
            ty: Partial::Present(args_ty),
            self_ty_fallback: false,
        }],
    );

    let ret_ty = arm_ast
        .ret_ty()
        .and_then(|t| TypeId::lower_ast_partial(ctxt, Some(t)).to_opt());

    let uses = lower_contract_typed_uses_clause(ctxt, contract, arm_ast.uses_clause());

    // Body: destructure `msg` into bindings used by the user arm body, then
    // return the arm body expression.
    let mut body_ctxt = BodyCtxt::new(ctxt, ctxt.joined_id(TrackedItemVariant::FuncBody));
    let mut stmts = Vec::new();
    body_ctxt.f_ctxt.enter_block_scope();

    let args_ident = IdentId::new(db, "args".to_string());
    if let Some(pat_ast) = arm_ast.pat()
        && let ast::PatKind::Record(record_pat) = pat_ast.kind()
        && let Some(fields) = record_pat.fields()
    {
        let mut arg_idx = 0usize;
        for field in fields {
            let field_pat_ast = field.pat();
            if let Some(field_pat_ast) = &field_pat_ast
                && let ast::PatKind::Rest(_) = field_pat_ast.kind()
            {
                break;
            }

            let field_pat = if let Some(field_pat_ast) = field_pat_ast {
                Pat::lower_ast(&mut body_ctxt, field_pat_ast)
            } else if let Some(label_tok) = field.name() {
                let label = IdentId::new(db, label_tok.text().to_string());
                body_ctxt.push_pat(
                    Pat::Path(Partial::Present(PathId::from_ident(db, label)), false),
                    HirOrigin::desugared(desugared.clone()),
                )
            } else {
                continue;
            };

            let args_expr = body_ctxt.push_expr(
                Expr::Path(Partial::Present(PathId::from_ident(db, args_ident))),
                HirOrigin::desugared(desugared.clone()),
            );
            let arg_expr = body_ctxt.push_expr(
                Expr::Field(
                    args_expr,
                    Partial::Present(crate::hir_def::FieldIndex::Index(IntegerId::from_usize(
                        db, arg_idx,
                    ))),
                ),
                HirOrigin::desugared(desugared.clone()),
            );

            stmts.push(body_ctxt.push_stmt(
                Stmt::Let(field_pat, None, Some(arg_expr)),
                HirOrigin::desugared(desugared.clone()),
            ));
            arg_idx += 1;
        }
    }

    if let Some(body_ast) = arm_ast.body() {
        let expr_ast = ast::Expr::cast(body_ast.syntax().clone()).unwrap();
        let inner_expr = Expr::lower_ast(&mut body_ctxt, expr_ast);
        stmts.push(body_ctxt.push_stmt(
            Stmt::Return(Some(inner_expr)),
            HirOrigin::desugared(desugared.clone()),
        ));
    } else {
        stmts
            .push(body_ctxt.push_stmt(Stmt::Return(None), HirOrigin::desugared(desugared.clone())));
    }
    let root_expr =
        body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared.clone()));
    body_ctxt.f_ctxt.leave_block_scope(root_expr);
    let body = body_ctxt.build(None, root_expr, BodyKind::FuncBody);

    let handler = Func::new(
        db,
        id,
        fn_name,
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        Partial::Present(params),
        uses,
        ret_ty,
        ItemModifier::None,
        Some(body),
        ctxt.top_mod(),
        HirOrigin::desugared(desugared),
    );

    ctxt.leave_item_scope(handler)
}

fn lower_contract_init_entrypoint_func<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    contract: Contract<'db>,
    contract_ptr: AstPtr<ast::Contract>,
    contract_name: IdentId<'db>,
    init_ast: Option<ast::ContractInit>,
) -> Func<'db> {
    let db = ctxt.db();
    let desugared = ContractLoweringDesugared {
        contract: contract_ptr,
        recv_idx: None,
        arm_idx: None,
        focus: ContractLoweringDesugaredFocus::InitBlock,
    };

    let init_fn_name = Partial::Present(IdentId::new(db, "init".to_string()));
    let init_fn_id = ctxt.joined_id(TrackedItemVariant::Func(init_fn_name));
    ctxt.enter_item_scope(init_fn_id, false);

    let attrs = AttrListId::new(
        db,
        vec![mk_contract_attr(db, "contract_init", contract_name)],
    );

    let mut body_ctxt = BodyCtxt::new(ctxt, ctxt.joined_id(TrackedItemVariant::FuncBody));
    let mut stmts = Vec::new();
    let runtime_fn_name = "runtime".to_string();

    {
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());

        if let Some(init_ast) = init_ast {
            // Instantiate contract fields as storage pointers.
            for (idx, field) in contract.fields(db).data(db).iter().enumerate() {
                let (Some(field_name), Some(field_ty)) =
                    (field.name.to_opt(), field.type_ref.to_opt())
                else {
                    continue;
                };

                // let mut field = evm.stor_ptr(contract_field_slot(idx))
                let slot_idx = b.int_lit(idx);
                let field_slot = b.path_expr(&["contract_field_slot"]);
                let slot_expr = b.call(field_slot, vec![slot_idx]);

                let evm = b.var_expr("evm");
                let ptr_init = b.method_call(evm, "stor_ptr", vec![slot_expr]);
                let stor_ptr_path = b.path_with_generic(&[], "StorPtr", field_ty);
                let stor_ptr_ty = TypeId::new(db, TypeKind::Path(Partial::Present(stor_ptr_path)));

                let bind_pat = b.body_ctxt.push_pat(
                    Pat::Path(Partial::Present(PathId::from_ident(db, field_name)), true),
                    b.origin(),
                );
                stmts.push(b.body_ctxt.push_stmt(
                    Stmt::Let(bind_pat, Some(stor_ptr_ty), Some(ptr_init)),
                    b.origin(),
                ));
            }

            // Decode init args from calldata and call user init logic.
            // let __calldata = CallData {}
            let call_data_path = b.path(&["CallData"]);
            let call_data_init = b.record_init(call_data_path, vec![]);
            b.let_stmt(&mut stmts, "__calldata", false, call_data_init);

            // let mut __d = SolDecoder<CallData>::new(__calldata)
            let call_data_ident = b.ident("__calldata");
            let call_data_ty = TypeId::new(
                db,
                TypeKind::Path(Partial::Present(PathId::from_str(db, "CallData"))),
            );
            let sol_decoder_path = b
                .path_with_generic(&[], "SolDecoder", call_data_ty)
                .push_str(db, "new");
            let sol_decoder = b.path_id_expr(sol_decoder_path);
            let call_data_expr = b.body_ctxt.push_expr(
                Expr::Path(Partial::Present(PathId::from_ident(db, call_data_ident))),
                b.origin(),
            );
            let d_init = b.call(sol_decoder, vec![call_data_expr]);
            b.let_stmt(&mut stmts, "__d", true, d_init);

            // if calldatasize() < sum(encoded_size<ParamTy>()) { revert(0, 0) }
            let required_size = {
                let mut total: Option<ExprId> = None;
                if let Some(params_ast) = init_ast.params() {
                    let params_hir = FuncParamListId::lower_ast(b.body_ctxt.f_ctxt, params_ast);
                    for param in params_hir.data(db) {
                        let Some(param_ty) = param.ty.to_opt() else {
                            continue;
                        };
                        let sz = b.encoded_size_of_ty(param_ty);
                        total = Some(match total {
                            None => sz,
                            Some(acc) => b.add(acc, sz),
                        });
                    }
                }
                total.unwrap_or_else(|| b.int_lit(0))
            };

            let evm = b.var_expr("evm");
            let calldata_size = b.method_call(evm, "calldatasize", vec![]);
            let is_too_short = b.lt(calldata_size, required_size);
            let revert_expr = b.revert_call();
            let revert_stmt = b.body_ctxt.push_stmt(Stmt::Expr(revert_expr), b.origin());
            let then_block = b.block(vec![revert_stmt]);
            let guard = b.if_(is_too_short, then_block);
            b.expr_stmt(&mut stmts, guard);

            // Decode each param and bind it as a local.
            let mut arg_names: Vec<IdentId<'db>> = Vec::new();
            if let Some(params_ast) = init_ast.params() {
                let params_hir = FuncParamListId::lower_ast(b.body_ctxt.f_ctxt, params_ast);
                for param in params_hir.data(db) {
                    let Some(param_ty) = param.ty.to_opt() else {
                        continue;
                    };
                    let FuncParamName::Ident(param_name) = param
                        .name
                        .to_opt()
                        .unwrap_or_else(|| FuncParamName::Ident(IdentId::new(db, "_".to_string())))
                    else {
                        continue;
                    };

                    let decode_path = match param_ty.data(db) {
                        TypeKind::Path(path) => path
                            .to_opt()
                            .unwrap_or_else(|| PathId::from_str(db, "_"))
                            .push_str(db, "decode"),
                        _ => PathId::from_str(db, "_"),
                    };
                    let decode_fn = b.path_id_expr(decode_path);
                    let d_expr = b.var_expr("__d");
                    let decoded = b.call(decode_fn, vec![d_expr]);
                    b.let_stmt_typed(&mut stmts, param_name.data(db), false, param_ty, decoded);
                    arg_names.push(param_name);
                }
            }

            // Call init_user(...) under the init block's `uses` bindings.
            let raw_uses = lower_uses_clause_opt(b.body_ctxt.f_ctxt, init_ast.uses_clause());
            let with_names: Vec<_> = raw_uses
                .data(db)
                .iter()
                .filter_map(|eff| {
                    eff.name
                        .or_else(|| eff.key_path.to_opt().and_then(|p| p.as_ident(db)))
                })
                .collect();

            let init_user_ident = IdentId::new(db, "init_contract".to_string());
            let init_user = b.body_ctxt.push_expr(
                Expr::Path(Partial::Present(PathId::from_ident(db, init_user_ident))),
                b.origin(),
            );
            let call_args: Vec<_> = arg_names
                .iter()
                .map(|name| b.var_expr(name.data(db)))
                .collect();
            let call = b.call(init_user, call_args);
            let wrapped = b.with_expr(with_names, call);
            b.expr_stmt(&mut stmts, wrapped);
        }

        // let __len = code_region_len(runtime)
        let runtime_expr = b.var_expr(&runtime_fn_name);
        let evm = b.var_expr("evm");
        let len_call = b.method_call(evm, "code_region_len", vec![runtime_expr]);
        b.let_stmt(&mut stmts, "__len", false, len_call);

        // let __offset = code_region_offset(runtime)
        let runtime_expr2 = b.var_expr(&runtime_fn_name);
        let evm = b.var_expr("evm");
        let offset_call = b.method_call(evm, "code_region_offset", vec![runtime_expr2]);
        b.let_stmt(&mut stmts, "__offset", false, offset_call);

        // codecopy(dest: 0, __offset, __len)
        let dest0 = b.int_lit(0);
        let offset_expr = b.var_expr("__offset");
        let len_expr = b.var_expr("__len");
        let evm = b.var_expr("evm");
        let codecopy_call = b.method_call(evm, "codecopy", vec![dest0, offset_expr, len_expr]);
        b.expr_stmt(&mut stmts, codecopy_call);

        // return_data(0, __len)
        let len_expr2 = b.var_expr("__len");
        let dest0b = b.int_lit(0);
        let ret_call = b.return_data_call(dest0b, len_expr2);
        b.expr_stmt(&mut stmts, ret_call);
    }

    body_ctxt.f_ctxt.enter_block_scope();
    let root_expr =
        body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared.clone()));
    body_ctxt.f_ctxt.leave_block_scope(root_expr);
    let body = body_ctxt.build(None, root_expr, BodyKind::FuncBody);

    let entrypoint_effects = {
        let mut effects = contract.effects(db).data(db).clone();
        let evm_ident = IdentId::new(db, "evm".to_string());
        let has_evm = effects.iter().any(|effect| {
            effect.name.is_some_and(|name| name == evm_ident)
                || effect.key_path.to_opt().is_some_and(|path| {
                    path.ident(db)
                        .to_opt()
                        .is_some_and(|id| id.data(db) == "Evm")
                })
        });
        if !has_evm {
            effects.push(EffectParam {
                name: Some(evm_ident),
                key_path: Partial::Present(PathId::from_str(db, "Evm")),
                is_mut: true,
            });
        }
        EffectParamListId::new(db, effects)
    };

    let init_fn = Func::new(
        db,
        init_fn_id,
        init_fn_name,
        attrs,
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        Partial::Present(FuncParamListId::new(db, vec![])),
        entrypoint_effects,
        None,
        ItemModifier::None,
        Some(body),
        ctxt.top_mod(),
        HirOrigin::desugared(desugared),
    );

    ctxt.leave_item_scope(init_fn)
}

fn lower_contract_init_handler_func<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    contract: Contract<'db>,
    contract_ptr: AstPtr<ast::Contract>,
    init_ast: ast::ContractInit,
) -> Func<'db> {
    let db = ctxt.db();
    let desugared = ContractLoweringDesugared {
        contract: contract_ptr,
        recv_idx: None,
        arm_idx: None,
        focus: ContractLoweringDesugaredFocus::InitBlock,
    };

    let fn_name = Partial::Present(IdentId::new(db, "init_contract".to_string()));
    let id = ctxt.joined_id(TrackedItemVariant::Func(fn_name));
    ctxt.enter_item_scope(id, false);

    let params = init_ast
        .params()
        .map(|p| FuncParamListId::lower_ast(ctxt, p))
        .unwrap_or_else(|| FuncParamListId::new(db, vec![]));
    let uses = lower_contract_typed_uses_clause(ctxt, contract, init_ast.uses_clause());

    let body_ast = init_ast
        .body()
        .map(|b| ast::Expr::cast(b.syntax().clone()).unwrap());

    let mut body_ctxt = BodyCtxt::new(ctxt, ctxt.joined_id(TrackedItemVariant::FuncBody));
    let mut stmts = Vec::new();
    body_ctxt.f_ctxt.enter_block_scope();

    if let Some(body_ast) = body_ast {
        let inner_expr = Expr::lower_ast(&mut body_ctxt, body_ast);
        stmts.push(body_ctxt.push_stmt(
            Stmt::Return(Some(inner_expr)),
            HirOrigin::desugared(desugared.clone()),
        ));
    } else {
        stmts
            .push(body_ctxt.push_stmt(Stmt::Return(None), HirOrigin::desugared(desugared.clone())));
    }

    let root_expr =
        body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared.clone()));
    body_ctxt.f_ctxt.leave_block_scope(root_expr);
    let body = body_ctxt.build(None, root_expr, BodyKind::FuncBody);

    let init_user = Func::new(
        db,
        id,
        fn_name,
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        Partial::Present(params),
        uses,
        None,
        ItemModifier::None,
        Some(body),
        ctxt.top_mod(),
        HirOrigin::desugared(desugared),
    );

    ctxt.leave_item_scope(init_user)
}

fn lower_contract_runtime_entrypoint_func<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    contract: Contract<'db>,
    contract_name: IdentId<'db>,
    contract_ast: &ast::Contract,
) -> Func<'db> {
    let db = ctxt.db();
    let contract_ptr = AstPtr::new(contract_ast);
    let desugared = ContractLoweringDesugared {
        contract: contract_ptr.clone(),
        recv_idx: None,
        arm_idx: None,
        focus: ContractLoweringDesugaredFocus::Contract,
    };

    let runtime_fn_name = Partial::Present(IdentId::new(db, "runtime".to_string()));
    let runtime_fn_id = ctxt.joined_id(TrackedItemVariant::Func(runtime_fn_name));
    ctxt.enter_item_scope(runtime_fn_id, false);

    let attrs = AttrListId::new(
        db,
        vec![mk_contract_attr(db, "contract_runtime", contract_name)],
    );

    let mut body_ctxt = BodyCtxt::new(ctxt, ctxt.joined_id(TrackedItemVariant::FuncBody));
    let mut stmts = Vec::new();

    // Instantiate contract fields as storage pointers.
    {
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());
        for (idx, field) in contract.fields(db).data(db).iter().enumerate() {
            let (Some(field_name), Some(field_ty)) = (field.name.to_opt(), field.type_ref.to_opt())
            else {
                continue;
            };

            // let field = evm.stor_ptr(contract_field_slot(idx))
            let slot_idx = b.int_lit(idx);
            let field_slot = b.path_expr(&["contract_field_slot"]);
            let slot_expr = b.call(field_slot, vec![slot_idx]);

            let evm = b.var_expr("evm");
            let ptr_init = b.method_call(evm, "stor_ptr", vec![slot_expr]);
            let stor_ptr_path = b.path_with_generic(&[], "StorPtr", field_ty);
            let stor_ptr_ty = TypeId::new(db, TypeKind::Path(Partial::Present(stor_ptr_path)));

            let bind_pat = b.body_ctxt.push_pat(
                Pat::Path(Partial::Present(PathId::from_ident(db, field_name)), true),
                HirOrigin::desugared(desugared.clone()),
            );
            stmts.push(b.body_ctxt.push_stmt(
                Stmt::Let(bind_pat, Some(stor_ptr_ty), Some(ptr_init)),
                HirOrigin::desugared(desugared.clone()),
            ));
        }
    }

    // let __calldata = CallData {}
    // if calldatasize() < size_of<Sol::Selector>() { revert(0, 0) }
    // let __selector = Sol::selector_from_prefix(calldataload(0))
    {
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());

        let call_data_path = b.path(&["CallData"]);
        let call_data_init = b.record_init(call_data_path, vec![]);
        b.let_stmt(&mut stmts, "__calldata", false, call_data_init);

        let evm = b.var_expr("evm");
        let calldata_size = b.method_call(evm, "calldatasize", vec![]);
        let selector_size = b.sol_selector_size();
        let is_too_short = b.lt(calldata_size, selector_size);
        let revert_expr = b.revert_call();
        let revert_stmt = b.body_ctxt.push_stmt(
            Stmt::Expr(revert_expr),
            HirOrigin::desugared(desugared.clone()),
        );
        let then_block = b.block(vec![revert_stmt]);
        let guard = b.if_(is_too_short, then_block);
        b.expr_stmt(&mut stmts, guard);

        let zero = b.int_lit(0);
        let evm = b.var_expr("evm");
        let word0 = b.method_call(evm, "calldataload", vec![zero]);
        let sol_selector = b.path_expr(&["Sol", "selector_from_prefix"]);
        let selector_call = b.call(sol_selector, vec![word0]);
        b.let_stmt(&mut stmts, "__selector", false, selector_call);
    }

    // Dispatch for all recv arms.
    let call_data_ident = IdentId::new(db, "__calldata".to_string());
    let mut dispatch_arms: Vec<(ContractLoweringDesugared, PathId<'db>, ExprId)> = Vec::new();
    for (recv_idx, recv_ast) in contract_ast.recvs().enumerate() {
        let msg_path = recv_ast
            .path()
            .map(|p| PathId::lower_ast(body_ctxt.f_ctxt, p));
        let Some(arms_ast) = recv_ast.arms() else {
            continue;
        };

        for (arm_idx, arm_ast) in arms_ast.into_iter().enumerate() {
            let arm_desugared = ContractLoweringDesugared {
                contract: contract_ptr.clone(),
                recv_idx: Some(recv_idx),
                arm_idx: Some(arm_idx),
                focus: ContractLoweringDesugaredFocus::RecvArm,
            };

            let variant_ty_path = match arm_ast.pat().and_then(|p| match p.kind() {
                ast::PatKind::Path(path_pat) => path_pat.path(),
                ast::PatKind::PathTuple(path_tup) => path_tup.path(),
                ast::PatKind::Record(record) => record.path(),
                _ => None,
            }) {
                Some(path_ast) => {
                    let variant_path = PathId::lower_ast(body_ctxt.f_ctxt, path_ast);
                    if let Some(msg_path) = msg_path
                        && variant_path.is_bare_ident(db)
                        && let Some(ident) = variant_path.as_ident(db)
                    {
                        msg_path.push_ident(db, ident)
                    } else {
                        variant_path
                    }
                }
                None => continue,
            };

            let variant_name = path_to_name(db, variant_ty_path);
            let handler_name = IdentId::new(
                db,
                format!("recv_{}_{}_{}", variant_name, recv_idx, arm_idx),
            );

            let selector_const_path = variant_ty_path.push_str(db, "SELECTOR");

            let raw_uses = lower_uses_clause_opt(body_ctxt.f_ctxt, arm_ast.uses_clause());
            let with_names: Vec<_> = raw_uses
                .data(db)
                .iter()
                .filter_map(|eff| {
                    eff.name
                        .or_else(|| eff.key_path.to_opt().and_then(|p| p.as_ident(db)))
                })
                .collect();

            let arm_body = lower_contract_runtime_dispatch_arm_body(
                &mut body_ctxt,
                call_data_ident,
                handler_name,
                variant_ty_path,
                arm_desugared.clone(),
                arm_ast,
                with_names,
            );

            dispatch_arms.push((arm_desugared, selector_const_path, arm_body));
        }
    }

    // Build dispatch match: match __selector { X::SELECTOR => arm, Y::SELECTOR => arm, _ => revert }
    let dispatch_expr = {
        let mut match_arms: Vec<MatchArm> = Vec::new();

        for (arm_desugared, selector_const_path, arm_body) in dispatch_arms {
            let b = HirBuilder::new(&mut body_ctxt, arm_desugared);
            let pat = b.body_ctxt.push_pat(
                Pat::Path(Partial::Present(selector_const_path), false),
                b.origin(),
            );
            match_arms.push(MatchArm {
                pat,
                body: arm_body,
            });
        }

        // Add wildcard arm that reverts
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());
        let wildcard_pat = b.wildcard_pat();
        let revert_body = b.revert_call();
        match_arms.push(MatchArm {
            pat: wildcard_pat,
            body: revert_body,
        });

        let selector_var = b.var_expr("__selector");
        b.body_ctxt.push_expr(
            Expr::Match(selector_var, Partial::Present(match_arms)),
            b.origin(),
        )
    };

    {
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());
        b.expr_stmt(&mut stmts, dispatch_expr);
    }

    body_ctxt.f_ctxt.enter_block_scope();
    let root_expr =
        body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared.clone()));
    body_ctxt.f_ctxt.leave_block_scope(root_expr);
    let body = body_ctxt.build(None, root_expr, BodyKind::FuncBody);

    let entrypoint_effects = {
        let mut effects = contract.effects(db).data(db).clone();
        let evm_ident = IdentId::new(db, "evm".to_string());
        let has_evm = effects.iter().any(|effect| {
            effect.name.is_some_and(|name| name == evm_ident)
                || effect.key_path.to_opt().is_some_and(|path| {
                    path.ident(db)
                        .to_opt()
                        .is_some_and(|id| id.data(db) == "Evm")
                })
        });
        if !has_evm {
            effects.push(EffectParam {
                name: Some(evm_ident),
                key_path: Partial::Present(PathId::from_str(db, "Evm")),
                is_mut: true,
            });
        }
        EffectParamListId::new(db, effects)
    };

    let runtime_fn = Func::new(
        db,
        runtime_fn_id,
        runtime_fn_name,
        attrs,
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        Partial::Present(FuncParamListId::new(db, vec![])),
        entrypoint_effects,
        None,
        ItemModifier::None,
        Some(body),
        ctxt.top_mod(),
        HirOrigin::desugared(desugared),
    );

    ctxt.leave_item_scope(runtime_fn)
}

fn lower_contract_runtime_dispatch_arm_body<'ctxt, 'db>(
    body_ctxt: &mut BodyCtxt<'ctxt, 'db>,
    call_data_ident: IdentId<'db>,
    handler_name: IdentId<'db>,
    variant_ty_path: PathId<'db>,
    desugared: ContractLoweringDesugared,
    arm_ast: ast::RecvArm,
    with_names: Vec<IdentId<'db>>,
) -> ExprId {
    let db = body_ctxt.f_ctxt.db();
    let mut stmts: Vec<StmtId> = Vec::new();

    // Build common types used for decoding (use imported CallData type)
    let call_data_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_str(db, "CallData"))),
    );

    {
        let mut b = HirBuilder::new(body_ctxt, desugared.clone());

        // let mut __d = SolDecoder<CallData>::with_base(__calldata, size_of<Sol::Selector>())
        let call_data_expr = b.body_ctxt.push_expr(
            Expr::Path(Partial::Present(PathId::from_ident(db, call_data_ident))),
            HirOrigin::desugared(desugared.clone()),
        );
        let sol_decoder_path = b
            .path_with_generic(&[], "SolDecoder", call_data_ty)
            .push_str(db, "with_base");
        let sol_decoder = b.path_id_expr(sol_decoder_path);
        let selector_size = b.sol_selector_size();
        let d_init = b.call(sol_decoder, vec![call_data_expr, selector_size]);
        b.let_stmt(&mut stmts, "__d", true, d_init);
    }

    let args_ty_path = variant_ty_path.push_str(db, "Args");
    let args_ty = TypeId::new(db, TypeKind::Path(Partial::Present(args_ty_path)));

    {
        let mut b = HirBuilder::new(body_ctxt, desugared.clone());

        // if calldatasize() < size_of<Sol::Selector>() + encoded_size<Args>() { revert(0, 0) }
        let evm = b.var_expr("evm");
        let calldata_size = b.method_call(evm, "calldatasize", vec![]);
        let selector_size = b.sol_selector_size();
        let args_size = b.encoded_size_of_ty(args_ty);
        let required_size = b.add(selector_size, args_size);

        let is_too_short = b.lt(calldata_size, required_size);
        let revert_expr = b.revert_call();
        let revert_stmt = b.body_ctxt.push_stmt(
            Stmt::Expr(revert_expr),
            HirOrigin::desugared(desugared.clone()),
        );
        let then_block = b.block(vec![revert_stmt]);
        let guard = b.if_(is_too_short, then_block);
        b.expr_stmt(&mut stmts, guard);

        // let __args: <Variant>::Args = <Variant>::Args::decode(__d)
        let d_expr = b.var_expr("__d");
        let decode_fn = b.path_id_expr(args_ty_path.push_str(db, "decode"));
        let args_value = b.call(decode_fn, vec![d_expr]);
        b.let_stmt_typed(&mut stmts, "__args", false, args_ty, args_value);
    }

    // Build and wrap handler call with effects
    let handler_call = {
        let mut b = HirBuilder::new(body_ctxt, desugared.clone());
        let handler = b.body_ctxt.push_expr(
            Expr::Path(Partial::Present(PathId::from_ident(db, handler_name))),
            HirOrigin::desugared(desugared.clone()),
        );
        let args_expr = b.var_expr("__args");
        let call = b.call(handler, vec![args_expr]);
        b.with_expr(with_names, call)
    };

    // Handle unit return case (no return type annotation)
    if arm_ast.ret_ty().is_none() {
        let mut b = HirBuilder::new(body_ctxt, desugared.clone());
        b.expr_stmt(&mut stmts, handler_call);
        let z0 = b.int_lit(0);
        let z1 = b.int_lit(0);
        let ret_call = b.return_data_call(z0, z1);
        b.expr_stmt(&mut stmts, ret_call);

        body_ctxt.f_ctxt.enter_block_scope();
        let block = body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared));
        body_ctxt.f_ctxt.leave_block_scope(block);
        return block;
    }

    // Non-unit return: ABI-encode using SolEncoder
    {
        let mut b = HirBuilder::new(body_ctxt, desugared.clone());

        // let __result = handler_call
        b.let_stmt(&mut stmts, "__result", false, handler_call);

        // let mut __enc = SolEncoder::new()
        let sol_encoder = b.path_expr(&["SolEncoder", "new"]);
        let enc_init = b.call(sol_encoder, vec![]);
        b.let_stmt(&mut stmts, "__enc", true, enc_init);

        // __enc.reserve_head(32)
        let enc_expr = b.var_expr("__enc");
        let head_32 = b.int_lit(32);
        let reserve_head = b.method_call(enc_expr, "reserve_head", vec![head_32]);
        b.expr_stmt(&mut stmts, reserve_head);

        // __result.encode(__enc)
        let result_expr = b.var_expr("__result");
        let enc_expr2 = b.var_expr("__enc");
        let encode_call = b.method_call(result_expr, "encode", vec![enc_expr2]);
        b.expr_stmt(&mut stmts, encode_call);

        // let __out = __enc.finish()
        let enc_expr3 = b.var_expr("__enc");
        let finish_call = b.method_call(enc_expr3, "finish", vec![]);
        b.let_stmt(&mut stmts, "__out", false, finish_call);

        // return_data(__ptr, __out_len)
        let out_expr = b.var_expr("__out");
        let ptr_expr = b.field_index(out_expr, 0);
        let out_expr2 = b.var_expr("__out");
        let len_expr = b.field_index(out_expr2, 1);
        let ret_call = b.return_data_call(ptr_expr, len_expr);
        b.expr_stmt(&mut stmts, ret_call);
    }

    body_ctxt.f_ctxt.enter_block_scope();
    let block = body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared));
    body_ctxt.f_ctxt.leave_block_scope(block);
    block
}

fn lower_contract_init_as_func<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    init_ast: ast::ContractInit,
) -> Func<'db> {
    let db = ctxt.db();

    let init_name = Partial::Present(IdentId::new(db, "init".to_string()));
    let id = ctxt.joined_id(TrackedItemVariant::Func(init_name));
    ctxt.enter_item_scope(id, false);

    let params = init_ast
        .params()
        .map(|p| FuncParamListId::lower_ast(ctxt, p))
        .into();
    let body = init_ast
        .body()
        .map(|b| Body::lower_ast(ctxt, ast::Expr::cast(b.syntax().clone()).unwrap()));

    let init_fn = Func::new(
        db,
        id,
        init_name,
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        params,
        lower_uses_clause_opt(ctxt, init_ast.uses_clause()),
        None,
        ItemModifier::None,
        body,
        ctxt.top_mod(),
        HirOrigin::desugared(ContractInitDesugared {
            init: AstPtr::new(&init_ast),
            focus: Default::default(),
        }),
    );
    ctxt.leave_item_scope(init_fn)
}
