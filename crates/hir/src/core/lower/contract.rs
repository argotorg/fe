use parser::ast::{
    self, AstPtr, AttrListOwner as _, ItemModifierOwner as _, prelude::AstNode as _,
};

use crate::{
    HirDb,
    hir_def::{
        AssocConstDef, AssocTyDef, Attr, AttrListId, Body, BodyKind, CallArg, Contract,
        ContractRecv, ContractRecvArm, ContractRecvArmListId, ContractRecvListId, EffectParam,
        EffectParamListId, Expr, ExprId, FieldDef, FieldDefListId, Func, FuncParam,
        FuncParamListId, FuncParamName, GenericArg, GenericArgListId, GenericParamListId, IdentId,
        ImplTrait, IntegerId, ItemModifier, LitKind, MatchArm, NormalAttr, Partial, Pat, PatId,
        PathId, PathKind, Stmt, StmtId, TrackedItemVariant, TraitRefId, TupleTypeId, TypeAlias,
        TypeGenericArg, TypeId, TypeKind, Use, Visibility, WhereClauseId, WithBinding,
        use_tree::{UsePathId, UsePathSegment},
    },
    lower::{FileLowerCtxt, body::BodyCtxt, item::lower_uses_clause_opt},
    span::{
        ContractInitDesugared, ContractLoweringDesugared, ContractLoweringDesugaredFocus,
        DesugaredOrigin, HirOrigin,
    },
};

const DEFAULT_TARGET_ALIAS: &str = "__Target";
const DEFAULT_ROOT_EFFECT_ALIAS: &str = "__RootEffect";
const DEFAULT_DEFAULT_ABI_ALIAS: &str = "__DefaultAbi";

/// Builder for synthesizing HIR expressions during contract lowering.
///
/// Wraps a `BodyCtxt` and desugared origin to provide convenient methods for
/// building common HIR patterns like path expressions, let bindings, and calls.
struct HirBuilder<'a, 'ctxt, 'db> {
    body_ctxt: &'a mut BodyCtxt<'ctxt, 'db>,
    origin: DesugaredOrigin,
}

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

    /// Creates a method call expression with generic arguments.
    fn method_call_generic(
        &mut self,
        receiver: ExprId,
        method: &str,
        generic_args: GenericArgListId<'db>,
        args: Vec<ExprId>,
    ) -> ExprId {
        let call_args = args
            .into_iter()
            .map(|expr| CallArg { label: None, expr })
            .collect();
        self.body_ctxt.push_expr(
            Expr::MethodCall(
                receiver,
                Partial::Present(self.ident(method)),
                generic_args,
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

    /// Creates a `root.abort()` call.
    fn abort_call(&mut self) -> ExprId {
        let root = self.var_expr("root");
        self.method_call(root, "abort", vec![])
    }

    /// Creates a `root.return_unit()` call.
    fn return_unit_call(&mut self) -> ExprId {
        let root = self.var_expr("root");
        self.method_call(root, "return_unit", vec![])
    }

    /// Creates a with-expression wrapping a call with effect bindings.
    fn with_expr(&mut self, bindings: Vec<WithBinding<'db>>, inner: ExprId) -> ExprId {
        if bindings.is_empty() {
            return inner;
        }
        self.body_ctxt
            .push_expr(Expr::With(bindings, inner), self.origin())
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
    let init_ast_for_metadata = init_ast.clone();

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

    // Synthesize `impl std::evm::Contract for <ContractName> { .. }` metadata for typed CREATE.
    lower_contract_deploy_metadata_impl(
        ctxt,
        contract_ptr.clone(),
        contract_name,
        init_ast_for_metadata,
    );

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

fn lower_contract_deploy_metadata_impl<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    contract_ptr: AstPtr<ast::Contract>,
    contract_name: IdentId<'db>,
    init_ast: Option<ast::ContractInit>,
) {
    let db = ctxt.db();
    let desugared = ContractLoweringDesugared {
        contract: contract_ptr,
        recv_idx: None,
        arm_idx: None,
        focus: ContractLoweringDesugaredFocus::Contract,
    };

    let contract_trait_path = PathId::from_segments(db, &["std", "evm", "Contract"]);
    let trait_ref = TraitRefId::new(db, Partial::Present(contract_trait_path));

    let self_ty_path = PathId::from_ident(db, contract_name);
    let ty = TypeId::new(db, TypeKind::Path(Partial::Present(self_ty_path)));

    let id = ctxt.joined_id(TrackedItemVariant::ImplTrait(
        Partial::Present(trait_ref),
        Partial::Present(ty),
    ));
    ctxt.enter_item_scope(id, false);

    // Associated type: `type InitArgs = (T0, T1, ...)` from the contract init params.
    let init_args_name = IdentId::new(db, "InitArgs".to_string());
    let init_args_ty = {
        let elems = init_ast
            .and_then(|init| init.params())
            .map(|params_ast| {
                let params_hir = FuncParamListId::lower_ast(ctxt, params_ast);
                params_hir
                    .data(db)
                    .iter()
                    .filter_map(|param| param.ty.to_opt())
                    .map(Partial::Present)
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        TypeId::new(db, TypeKind::Tuple(TupleTypeId::new(db, elems)))
    };
    let types = vec![AssocTyDef {
        attributes: AttrListId::new(db, vec![]),
        name: Partial::Present(init_args_name),
        type_ref: Partial::Present(init_args_ty),
    }];

    let consts: Vec<AssocConstDef<'db>> = vec![];

    // Methods: `init_code_offset/len` forward to `std::evm::code_region_offset/len(init)`.
    lower_contract_deploy_metadata_method(
        ctxt,
        desugared.clone(),
        "init_code_offset",
        "code_region_offset",
    );
    lower_contract_deploy_metadata_method(
        ctxt,
        desugared.clone(),
        "init_code_len",
        "code_region_len",
    );

    let impl_trait = ImplTrait::new(
        db,
        id,
        Partial::Present(trait_ref),
        Partial::Present(ty),
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        types,
        consts,
        ctxt.top_mod(),
        HirOrigin::desugared(desugared),
    );
    ctxt.leave_item_scope(impl_trait);
}

fn lower_contract_deploy_metadata_method<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    desugared: ContractLoweringDesugared,
    method_name: &str,
    intrinsic_name: &str,
) -> Func<'db> {
    let db = ctxt.db();

    let fn_name = Partial::Present(IdentId::new(db, method_name.to_string()));
    let fn_id = ctxt.joined_id(TrackedItemVariant::Func(fn_name));
    ctxt.enter_item_scope(fn_id, false);

    let mut body_ctxt = BodyCtxt::new(ctxt, ctxt.joined_id(TrackedItemVariant::FuncBody));
    let expr_origin = HirOrigin::desugared(desugared.clone());
    let stmt_origin = HirOrigin::desugared(desugared.clone());

    // std::evm::<intrinsic_name>(init)
    let callee_path = PathId::from_segments(db, &["std", "evm", intrinsic_name]);
    let callee = body_ctxt.push_expr(
        Expr::Path(Partial::Present(callee_path)),
        expr_origin.clone(),
    );
    let init_path = PathId::from_str(db, "init");
    let init_fn = body_ctxt.push_expr(Expr::Path(Partial::Present(init_path)), expr_origin.clone());
    let call = body_ctxt.push_expr(
        Expr::Call(
            callee,
            vec![CallArg {
                label: None,
                expr: init_fn,
            }],
        ),
        expr_origin.clone(),
    );

    // return <call>
    let stmts = vec![body_ctxt.push_stmt(Stmt::Return(Some(call)), stmt_origin)];

    body_ctxt.f_ctxt.enter_block_scope();
    let root_expr = body_ctxt.push_expr(Expr::Block(stmts), expr_origin);
    body_ctxt.f_ctxt.leave_block_scope(root_expr);
    let body = body_ctxt.build(None, root_expr, BodyKind::FuncBody);

    let ret_ty = Some(TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_str(db, "u256"))),
    ));
    let func = Func::new(
        db,
        fn_id,
        fn_name,
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        Partial::Present(FuncParamListId::new(db, vec![])),
        EffectParamListId::new(db, vec![]),
        ret_ty,
        ItemModifier::None,
        Some(body),
        ctxt.top_mod(),
        HirOrigin::desugared(desugared),
    );

    ctxt.leave_item_scope(func)
}

/// Inserts use statements for common core/std types used in generated contract code.
fn insert_contract_type_alias<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    alias: &str,
    rhs_ty: TypeId<'db>,
) {
    let db = ctxt.db();
    let name = Partial::Present(IdentId::new(db, alias.to_string()));
    let id = ctxt.joined_id(TrackedItemVariant::TypeAlias(name));
    ctxt.enter_item_scope(id, false);
    let top_mod = ctxt.top_mod();
    let origin = HirOrigin::synthetic();
    let alias_item = TypeAlias::new(
        db,
        id,
        name,
        AttrListId::new(db, vec![]),
        Visibility::Private,
        GenericParamListId::new(db, vec![]),
        Partial::Present(rhs_ty),
        top_mod,
        origin,
    );
    ctxt.leave_item_scope(alias_item);
}

fn insert_contract_use_statements<'db>(ctxt: &mut FileLowerCtxt<'db>) {
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
        let attrs = AttrListId::new(db, vec![]);
        let use_ = Use::new(
            db,
            id,
            attrs,
            path,
            None,
            Visibility::Private,
            top_mod,
            origin,
        );
        ctxt.leave_item_scope(use_);
    };

    // Core traits used by generated entrypoints.
    insert_use(&["core", "contracts", "ContractHost"]);
    insert_use(&["core", "contracts", "Target"]);

    // Core ABI traits used by generated entrypoints.
    insert_use(&["core", "abi", "Abi"]);
    insert_use(&["core", "abi", "AbiEncoder"]);
    insert_use(&["core", "abi", "AbiSize"]);
    insert_use(&["core", "abi", "ByteInput"]);
    insert_use(&["core", "abi", "prefix"]);
    insert_use(&["core", "abi", "Encode"]);

    // Default target (hardcoded for now).
    insert_use(&["std", "evm", "EvmTarget"]);

    // Type aliases to keep generated code concise:
    //   type __Target = EvmTarget
    //   type __RootEffect = __Target::RootEffect
    //   type __DefaultAbi = __Target::DefaultAbi
    let evm_target_ty = TypeId::new(
        db,
        TypeKind::Path(Partial::Present(PathId::from_str(db, "EvmTarget"))),
    );
    insert_contract_type_alias(ctxt, DEFAULT_TARGET_ALIAS, evm_target_ty);

    let target_root_path = PathId::from_str(db, DEFAULT_TARGET_ALIAS).push_str(db, "RootEffect");
    let target_root_ty = TypeId::new(db, TypeKind::Path(Partial::Present(target_root_path)));
    insert_contract_type_alias(ctxt, DEFAULT_ROOT_EFFECT_ALIAS, target_root_ty);

    let target_abi_path = PathId::from_str(db, DEFAULT_TARGET_ALIAS).push_str(db, "DefaultAbi");
    let target_abi_ty = TypeId::new(db, TypeKind::Path(Partial::Present(target_abi_path)));
    insert_contract_type_alias(ctxt, DEFAULT_DEFAULT_ABI_ALIAS, target_abi_ty);
}

fn default_target_assoc_path<'db>(db: &'db dyn HirDb, assoc: &str) -> PathId<'db> {
    match assoc {
        "RootEffect" => PathId::from_str(db, DEFAULT_ROOT_EFFECT_ALIAS),
        "DefaultAbi" => PathId::from_str(db, DEFAULT_DEFAULT_ABI_ALIAS),
        _ => PathId::from_str(db, assoc),
    }
}

fn default_target_assoc_ty<'db>(db: &'db dyn HirDb, assoc: &str) -> TypeId<'db> {
    TypeId::new(
        db,
        TypeKind::Path(Partial::Present(default_target_assoc_path(db, assoc))),
    )
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

fn entrypoint_with_bindings_from_uses<'ctxt, 'db>(
    b: &mut HirBuilder<'_, 'ctxt, 'db>,
    contract: Contract<'db>,
    raw_uses: EffectParamListId<'db>,
) -> Vec<WithBinding<'db>> {
    let db = b.db();
    let field_types = contract_field_type_paths(db, contract);

    let mut bindings_out = Vec::new();

    for eff in raw_uses.data(db) {
        let Some(raw_key_path) = eff.key_path.to_opt() else {
            continue;
        };
        let Some(binding) = eff.name.or_else(|| raw_key_path.as_ident(db)) else {
            continue;
        };

        if field_types.contains_key(&binding) {
            let value = b.var_expr(binding.data(db));
            bindings_out.push(WithBinding {
                key_path: Some(Partial::Present(raw_key_path)),
                value,
            });
        } else {
            let value = b.var_expr("root");
            bindings_out.push(WithBinding {
                key_path: Some(Partial::Present(raw_key_path)),
                value,
            });
        }
    }

    bindings_out
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
            // Instantiate contract fields as target-specific field pointers.
            for (idx, field) in contract.fields(db).data(db).iter().enumerate() {
                let (Some(field_name), Some(field_ty)) =
                    (field.name.to_opt(), field.type_ref.to_opt())
                else {
                    continue;
                };

                // let mut field = <<T::RootEffect as ContractHost>::init_field<FieldTy>>(root, contract_field_slot(idx))
                let root = b.var_expr("root");
                let field_idx = b.int_lit(idx);
                let contract_field_slot = b.path_expr(&["core", "contract_field_slot"]);
                let slot = b.call(contract_field_slot, vec![field_idx]);
                let field_generic_args = GenericArgListId::new(
                    db,
                    vec![GenericArg::Type(TypeGenericArg {
                        ty: Partial::Present(field_ty),
                    })],
                    true,
                );
                let ptr_init =
                    b.method_call_generic(root, "init_field", field_generic_args, vec![slot]);

                let bind_pat = b.body_ctxt.push_pat(
                    Pat::Path(Partial::Present(PathId::from_ident(db, field_name)), true),
                    b.origin(),
                );
                stmts.push(
                    b.body_ctxt
                        .push_stmt(Stmt::Let(bind_pat, None, Some(ptr_init)), b.origin()),
                );
            }

            // Decode init args and call user init logic.
            // This uses the target's `ContractHost::init_input` implementation (e.g. EVM reads
            // constructor args appended to the init bytecode).
            let root = b.var_expr("root");
            let runtime_expr = b.var_expr(&runtime_fn_name);
            let init_input = b.method_call(root, "init_input", vec![runtime_expr]);
            let decoder_new = b.path_expr(&[DEFAULT_DEFAULT_ABI_ALIAS, "decoder_new"]);
            let d_init = b.call(decoder_new, vec![init_input]);
            b.let_stmt(&mut stmts, "__d", true, d_init);

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
            let typed_uses = lower_contract_typed_uses_clause(
                b.body_ctxt.f_ctxt,
                contract,
                init_ast.uses_clause(),
            );
            let with_bindings = entrypoint_with_bindings_from_uses(&mut b, contract, typed_uses);

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
            let wrapped = b.with_expr(with_bindings, call);
            b.expr_stmt(&mut stmts, wrapped);
        }

        // Finalize deployment by returning the runtime code.
        let runtime_expr = b.var_expr(&runtime_fn_name);
        let root = b.var_expr("root");
        let create_call = b.method_call(root, "create_contract", vec![runtime_expr]);
        b.expr_stmt(&mut stmts, create_call);
    }

    body_ctxt.f_ctxt.enter_block_scope();
    let root_expr =
        body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared.clone()));
    body_ctxt.f_ctxt.leave_block_scope(root_expr);
    let body = body_ctxt.build(None, root_expr, BodyKind::FuncBody);

    let entrypoint_effects = {
        let root_ident = IdentId::new(db, "root".to_string());
        EffectParamListId::new(
            db,
            vec![EffectParam {
                name: Some(root_ident),
                key_path: Partial::Present(default_target_assoc_path(db, "RootEffect")),
                is_mut: true,
            }],
        )
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

    // Instantiate contract fields as target-specific field pointers.
    {
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());
        for (idx, field) in contract.fields(db).data(db).iter().enumerate() {
            let (Some(field_name), Some(field_ty)) = (field.name.to_opt(), field.type_ref.to_opt())
            else {
                continue;
            };

            // let mut field = <<T::RootEffect as ContractHost>::field<FieldTy>>(root, contract_field_slot(idx))
            let root = b.var_expr("root");
            let field_idx = b.int_lit(idx);
            let contract_field_slot = b.path_expr(&["core", "contract_field_slot"]);
            let slot = b.call(contract_field_slot, vec![field_idx]);
            let field_generic_args = GenericArgListId::new(
                db,
                vec![GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(field_ty),
                })],
                true,
            );
            let ptr_init = b.method_call_generic(root, "field", field_generic_args, vec![slot]);

            let bind_pat = b.body_ctxt.push_pat(
                Pat::Path(Partial::Present(PathId::from_ident(db, field_name)), true),
                HirOrigin::desugared(desugared.clone()),
            );
            stmts.push(b.body_ctxt.push_stmt(
                Stmt::Let(bind_pat, None, Some(ptr_init)),
                HirOrigin::desugared(desugared.clone()),
            ));
        }
    }

    // let __selector = root.runtime_selector<T::DefaultAbi>()
    {
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());

        let root = b.var_expr("root");
        let abi_ty = default_target_assoc_ty(db, "DefaultAbi");
        let selector_generic_args = GenericArgListId::new(
            db,
            vec![GenericArg::Type(TypeGenericArg {
                ty: Partial::Present(abi_ty),
            })],
            true,
        );

        let selector_call =
            b.method_call_generic(root, "runtime_selector", selector_generic_args, vec![]);
        b.let_stmt(&mut stmts, "__selector", false, selector_call);
    }

    // Initialize a runtime decoder once and move it into the selected arm.
    // let mut __d = root.runtime_decoder<T::DefaultAbi>()
    {
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());

        let root = b.var_expr("root");
        let abi_ty = default_target_assoc_ty(db, "DefaultAbi");
        let decoder_generic_args = GenericArgListId::new(
            db,
            vec![GenericArg::Type(TypeGenericArg {
                ty: Partial::Present(abi_ty),
            })],
            true,
        );
        let d_init = b.method_call_generic(root, "runtime_decoder", decoder_generic_args, vec![]);
        b.let_stmt(&mut stmts, "__d", true, d_init);
    }

    // Dispatch for all recv arms.
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

            let raw_uses =
                lower_contract_typed_uses_clause(body_ctxt.f_ctxt, contract, arm_ast.uses_clause());

            let arm_body = lower_contract_runtime_dispatch_arm_body(
                &mut body_ctxt,
                contract,
                handler_name,
                variant_ty_path,
                arm_desugared.clone(),
                arm_ast,
                raw_uses,
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

        // Add wildcard arm that aborts
        let mut b = HirBuilder::new(&mut body_ctxt, desugared.clone());
        let wildcard_pat = b.wildcard_pat();
        let revert_body = b.abort_call();
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
        let root_ident = IdentId::new(db, "root".to_string());
        EffectParamListId::new(
            db,
            vec![EffectParam {
                name: Some(root_ident),
                key_path: Partial::Present(default_target_assoc_path(db, "RootEffect")),
                is_mut: true,
            }],
        )
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

#[allow(clippy::too_many_arguments)] // TODO refactor
fn lower_contract_runtime_dispatch_arm_body<'ctxt, 'db>(
    body_ctxt: &mut BodyCtxt<'ctxt, 'db>,
    contract: Contract<'db>,
    handler_name: IdentId<'db>,
    variant_ty_path: PathId<'db>,
    desugared: ContractLoweringDesugared,
    arm_ast: ast::RecvArm,
    raw_uses: EffectParamListId<'db>,
) -> ExprId {
    let db = body_ctxt.f_ctxt.db();
    let mut stmts: Vec<StmtId> = Vec::new();

    let args_ty_path = variant_ty_path.push_str(db, "Args");
    let args_ty = TypeId::new(db, TypeKind::Path(Partial::Present(args_ty_path)));

    {
        let mut b = HirBuilder::new(body_ctxt, desugared.clone());

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
        let with_bindings = entrypoint_with_bindings_from_uses(&mut b, contract, raw_uses);
        b.with_expr(with_bindings, call)
    };

    // Handle unit return case (no return type annotation)
    if arm_ast.ret_ty().is_none() {
        let mut b = HirBuilder::new(body_ctxt, desugared.clone());
        b.expr_stmt(&mut stmts, handler_call);
        let ret_call = b.return_unit_call();
        b.expr_stmt(&mut stmts, ret_call);

        body_ctxt.f_ctxt.enter_block_scope();
        let block = body_ctxt.push_expr(Expr::Block(stmts), HirOrigin::desugared(desugared));
        body_ctxt.f_ctxt.leave_block_scope(block);
        return block;
    }

    // Non-unit return: ABI-encode and return using the default ABI.
    {
        let Some(ret_ty) = arm_ast
            .ret_ty()
            .map(|ty| TypeId::lower_ast(body_ctxt.f_ctxt, ty))
        else {
            return body_ctxt.push_missing_expr();
        };

        let mut b = HirBuilder::new(body_ctxt, desugared.clone());

        // let __result = handler_call
        b.let_stmt(&mut stmts, "__result", false, handler_call);

        // <<T::RootEffect as ContractHost>::return_value<T::DefaultAbi, RetTy>(root, __result)
        let abi_ty = default_target_assoc_ty(db, "DefaultAbi");
        let return_generic_args = GenericArgListId::new(
            db,
            vec![
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(abi_ty),
                }),
                GenericArg::Type(TypeGenericArg {
                    ty: Partial::Present(ret_ty),
                }),
            ],
            true,
        );
        let root = b.var_expr("root");
        let result_expr = b.var_expr("__result");
        let return_call =
            b.method_call_generic(root, "return_value", return_generic_args, vec![result_expr]);
        b.expr_stmt(&mut stmts, return_call);
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
